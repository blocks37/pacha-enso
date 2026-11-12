# Modelo de riesgo de inundación y huaico ------------------------------------
# Funciones puras y vectorizadas: reciben tablas, devuelven tablas.
# Mismo modelo que la app de Streamlit, con las constantes en config.yml.

#' Índice ENSO entre 0 y 1: promedio del ONI y de Niño 1+2 saturados.
#' Solo la fase cálida amplifica la lluvia en la costa norte.
indice_enso <- function(oni, nino12, cfg_riesgo) {
  a <- acotar(oni / cfg_riesgo$oni_saturacion)
  b <- acotar(nino12 / cfg_riesgo$nino12_saturacion)
  rowMeans(cbind(a, b), na.rm = TRUE)
}

#' Lleva la lluvia a 0–1 contra el clima propio del distrito:
#' 0 = mediana de los días con lluvia, 1 = percentil 99.
normalizar_lluvia <- function(pp_mm, p50_mm, p99_mm) {
  rango <- pmax(p99_mm - p50_mm, 1e-6)
  acotar((pp_mm - p50_mm) / rango)
}

#' Amenaza dominante según la altitud del distrito.
amenaza_distrito <- function(altitud, cfg_riesgo) {
  ifelse(!is.na(altitud) & altitud >= cfg_riesgo$pendiente_huaico_m, "huaico", "inundacion")
}

#' Exposición: 1 en zonas bajas para inundación. Para huaico, el desnivel dentro del
#' distrito (qué tan empinado es): el altiplano es alto pero plano y casi no tiene
#' huaicos; los valles interandinos sí. Sin dato de relieve se usa la altitud.
indice_exposicion <- function(altitud, amenaza, cfg_riesgo, relieve = NULL) {
  baja <- cfg_riesgo$elevacion_baja_m
  rango <- unlist(cfg_riesgo$relieve_huaico_m %||% c(300, 1800))
  por_altitud <- acotar((altitud - 1000) / 2500)
  por_relieve <- if (is.null(relieve)) NA_real_ else acotar((relieve - rango[1]) / (rango[2] - rango[1]))
  ifelse(
    amenaza == "inundacion",
    acotar(1 - (altitud - baja) / 450),        # 1 bajo 50 m, 0 sobre 500 m
    dplyr::coalesce(por_relieve, por_altitud)
  )
}

#' Cuánto amplifica El Niño la lluvia de cada distrito (0–1). El Niño costero trae
#' lluvia a la vertiente del Pacífico del norte; en el sur andino y la Amazonía
#' suele traer déficit (eso lo mide la sequía), así que ahí no amplifica.
alcance_enso <- function(codigo_cuenca, lat, costero, cfg_riesgo) {
  lim <- unlist(cfg_riesgo$enso_latitud %||% c(-12, -8))
  pacifico <- dplyr::coalesce(substr(codigo_cuenca, 1, 1) == "1", dplyr::coalesce(costero, FALSE))
  ifelse(pacifico, acotar((lat - lim[1]) / (lim[2] - lim[1])), 0)
}

#' Calcula el riesgo para cada fila de `df`.
#'
#' `df` necesita: lluvia_idx, mar_idx, cauce_idx, exposicion_idx, enso_idx, amenaza.
#' Devuelve `df` con `riesgo` (0–1) y `nivel`.
calcular_riesgo <- function(df, cfg_riesgo) {
  w <- cfg_riesgo$pesos
  peso <- function(componente) {
    ifelse(df$amenaza == "huaico", w$huaico[[componente]], w$inundacion[[componente]])
  }
  lluvia_ajustada <- acotar(df$lluvia_idx * (1 + cfg_riesgo$enso_amplificacion * df$enso_idx))

  riesgo <- peso("lluvia") * lluvia_ajustada +
    peso("mar") * df$mar_idx +
    peso("exposicion") * df$exposicion_idx +
    peso("cauce") * df$cauce_idx

  # Sin lluvia (ni en el distrito ni en la cuenca de sus ríos), el resto de
  # componentes no basta para salir de «bajo».
  techo_sin_lluvia <- cfg_riesgo$umbrales$vigilancia - 0.01
  lluvia_cuenca <- if ("cuenca_idx" %in% names(df)) dplyr::coalesce(df$cuenca_idx, 0) else 0
  riesgo <- ifelse(df$lluvia_idx <= 0 & lluvia_cuenca <= 0, pmin(riesgo, techo_sin_lluvia), riesgo)

  df$riesgo <- round(acotar(riesgo), 3)
  df$nivel <- nivel_riesgo(df$riesgo, df$amenaza, cfg_riesgo$umbrales)
  df
}

nivel_riesgo <- function(riesgo, amenaza, umbrales) {
  dplyr::case_when(
    riesgo >= umbrales$critico ~ "Crítico",
    riesgo >= umbrales$alto ~ "Alto",
    riesgo >= umbrales$alerta ~ "Alerta",
    riesgo >= umbrales$vigilancia & amenaza == "huaico" ~ "Huaico medio",
    riesgo >= umbrales$vigilancia ~ "Vigilancia",
    TRUE ~ "Bajo"
  )
}

#' Peor escenario por distrito dentro de una ventana de fechas.
peor_escenario <- function(riesgo_df) {
  if (!"lluvia_mm" %in% names(riesgo_df)) riesgo_df$lluvia_mm <- 0
  riesgo_df |>
    dplyr::group_by(ubigeo) |>
    dplyr::arrange(dplyr::desc(riesgo), dplyr::desc(lluvia_mm), fecha) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(riesgo), dplyr::desc(lluvia_mm))   # a igual riesgo, primero el más lluvioso
}

#' Arma la tabla de riesgo a partir del pronóstico diario y las entradas del ENFEN.
#'
#' @param pronostico ubigeo, fecha, pp_mm
#' @param distritos  dim_distrito
#' @param clima      ubigeo, p50_mm, p99_mm
#' @param enso       lista con oni y nino12 (último valor)
#' @param enfen      lista con mar_sobre_normal (lógico) y rios (carácter)
#' @param cuenca     ubigeo, fecha, cuenca_mm, cuenca_idx (lluvia en la cuenca de sus ríos); opcional
construir_riesgo <- function(pronostico, distritos, clima, enso, enfen, cfg_riesgo, cuenca = NULL) {
  enso_idx <- indice_enso(enso$oni, enso$nino12, cfg_riesgo)
  claves_enfen <- if (length(enfen$claves)) unlist(enfen$claves) else clave_rio(enfen$rios %||% character())

  if (!"relieve" %in% names(distritos)) distritos$relieve <- NA_real_
  if (!"codigo_cuenca" %in% names(distritos)) distritos$codigo_cuenca <- NA_character_
  if (is.null(cuenca)) cuenca <- tibble::tibble(ubigeo = character(), fecha = as.Date(character()),
                                                cuenca_mm = numeric(), cuenca_idx = numeric())
  pronostico |>
    dplyr::inner_join(distritos, by = "ubigeo") |>
    dplyr::inner_join(clima, by = "ubigeo") |>
    dplyr::left_join(cuenca, by = c("ubigeo", "fecha")) |>
    dplyr::mutate(
      amenaza = amenaza_distrito(altitud, cfg_riesgo),
      lluvia_mm = pp_mm,
      lluvia_idx = normalizar_lluvia(pp_mm, p50_mm, p99_mm),
      mar_idx = ifelse(dplyr::coalesce(costero, FALSE) & isTRUE(enfen$mar_sobre_normal), 0.5, 0),
      cuenca_idx = dplyr::coalesce(cuenca_idx, 0),
      # Río crecido: el ENFEN lo reporta o llueve fuerte en la cuenca aguas arriba
      cauce_idx = pmax(cauce_en_alerta(claves_rio, claves_enfen), cuenca_idx),
      exposicion_idx = dplyr::coalesce(indice_exposicion(altitud, amenaza, cfg_riesgo, relieve), 0),
      enso_idx = round(enso_idx * alcance_enso(codigo_cuenca, lat, costero, cfg_riesgo), 3)
    ) |>
    calcular_riesgo(cfg_riesgo) |>
    dplyr::select(ubigeo, fecha, amenaza, lluvia_mm, lluvia_idx, mar_idx,
                  cauce_idx, exposicion_idx, enso_idx, cuenca_mm, cuenca_idx, riesgo, nivel)
}

#' Aporte de cada componente al riesgo, en puntos porcentuales (para gráficos).
componentes_riesgo <- function(df, cfg_riesgo) {
  w <- cfg_riesgo$pesos
  peso <- function(componente) {
    ifelse(df$amenaza == "huaico", w$huaico[[componente]], w$inundacion[[componente]])
  }
  con_enso <- acotar(df$lluvia_idx * (1 + cfg_riesgo$enso_amplificacion * df$enso_idx))
  tibble::tibble(
    ubigeo = df$ubigeo,
    distrito = df$distrito,
    lluvia = 100 * peso("lluvia") * acotar(df$lluvia_idx),
    enso = 100 * peso("lluvia") * (con_enso - acotar(df$lluvia_idx)),   # lo que suma El Niño
    cauce = 100 * peso("cauce") * df$cauce_idx,
    mar = 100 * peso("mar") * df$mar_idx,
    exposicion = 100 * peso("exposicion") * df$exposicion_idx
  )
}
