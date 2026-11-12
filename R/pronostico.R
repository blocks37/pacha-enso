# Pronóstico de lluvia mensual por años análogos del ENSO ---------------------------
#
# Idea: «si hoy hay un El Niño fuerte, los próximos meses se parecerán a los mismos
# meses de otros años con El Niño fuerte». Para cada distrito y mes futuro se toma
# la mediana de la lluvia de ese mes en los años análogos (según el ONI más reciente).
# Es simple, usa solo PISCO y no hereda los sesgos de los modelos globales en los Andes.

categoria_oni <- function(oni) {
  cut(oni, c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf),
      labels = c("Niña fuerte", "Niña", "Neutro", "Niño", "Niño fuerte"), right = FALSE)
}

#' Años cuya ONI, en el mismo mes del año que `mes_oni`, cae en la misma categoría.
#' Si hay menos de `min_analogos`, se amplía a la fase (Niño / Neutro / Niña).
anios_analogos <- function(oni, mes_oni, valor_oni, anios_validos, min_analogos = 3) {
  m <- as.integer(format(mes_oni, "%m"))
  cand <- dplyr::filter(oni, as.integer(format(fecha, "%m")) == m,
                        as.integer(format(fecha, "%Y")) %in% anios_validos)
  anio <- as.integer(format(cand$fecha, "%Y"))
  elegidos <- anio[categoria_oni(cand$valor) == categoria_oni(valor_oni)]
  if (length(elegidos) < min_analogos) elegidos <- anio[fase_enso(cand$valor) == fase_enso(valor_oni)]
  sort(unique(elegidos))
}

#' Lluvia pronosticada para `meses` (Date, primer día de cada mes).
#' @param precip ubigeo, mes, pp_mm (solo observado)
#' @param oni    fecha, valor (ONI mensual)
#' @return lista con `datos` (ubigeo, mes, pp_mm, fuente, pronostico) y `analogos`
pronostico_analogos <- function(precip, oni, meses, min_analogos = 3) {
  if (nrow(oni) == 0) stop("No hay datos del ONI (revise el error de NOAA · ONI más arriba)")
  ultimo <- oni[which.max(oni$fecha), ]
  base_mes <- format(ultimo$fecha, "%m")
  desfases <- meses_entre(ultimo$fecha, meses)
  fin_obs <- max(precip$mes)
  inicio_obs <- min(precip$mes)

  # Un año sirve de análogo si hay lluvia observada para todos los meses equivalentes.
  anios <- unique(as.integer(format(oni$fecha, "%Y")))
  inicio_anio <- as.Date(sprintf("%d-%s-01", anios, base_mes))
  completos <- sumar_meses(inicio_anio, min(desfases)) >= inicio_obs &
    sumar_meses(inicio_anio, max(desfases)) <= fin_obs
  analogos <- anios_analogos(oni, ultimo$fecha, ultimo$valor, anios[completos], min_analogos)
  if (length(analogos) == 0) stop("No hay años análogos para el ONI actual")
  log_info("Años análogos (ONI ", ultimo$valor, "): ", paste(analogos, collapse = ", "))

  equivalencias <- tidyr::expand_grid(anio = analogos, k = seq_along(meses)) |>
    dplyr::transmute(
      mes_objetivo = meses[k],
      mes = sumar_meses(as.Date(sprintf("%d-%s-01", anio, base_mes)), desfases[k])
    )
  datos <- precip |>
    dplyr::select(ubigeo, mes, pp_mm) |>
    dplyr::inner_join(equivalencias, by = "mes", relationship = "many-to-many") |>
    dplyr::group_by(ubigeo, mes = mes_objetivo) |>
    dplyr::summarise(pp_mm = round(stats::median(pp_mm, na.rm = TRUE), 1), .groups = "drop") |>
    dplyr::mutate(fuente = paste("Análogos ENSO", paste(analogos, collapse = ", ")), pronostico = TRUE)
  list(datos = datos, analogos = analogos)
}
