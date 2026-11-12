# Datos compartidos por todas las vistas -----------------------------------------
# La app vigila el archivo ACTUAL cada 15 s. Cuando la actualización publica una
# base nueva, todas las vistas se recalculan solas y aparece un aviso.

datos_compartidos <- function(db, cfg, session) {
  carpeta <- cfg$rutas$db
  version <- shiny::reactivePoll(
    15000, session,
    checkFunc = function() db_actual(carpeta) %||% "",
    valueFunc = function() db_actual(carpeta) %||% ""
  )

  primera <- TRUE
  shiny::observeEvent(version(), {
    if (!primera) {
      shiny::showNotification(
        shiny::tagList(bsicons::bs_icon("arrow-repeat"), " Llegaron datos nuevos: la app ya los muestra."),
        type = "message", duration = 8)
    }
    primera <<- FALSE
  })

  leer <- function(f) shiny::reactive({ version(); f() })

  distritos <- leer(function() q_distritos(db))
  fuentes <- leer(function() q_fuentes(db))
  enso <- leer(function() q_enso(db))
  corrida <- leer(function() q_ultima_corrida(db))
  riesgo14 <- leer(function() q_riesgo(db, 14))
  pronostico <- leer(function() q_pronostico(db))
  clima <- leer(function() q_clima(db))
  ultimo_obs <- leer(function() q_sequia_ultimo_obs(db))
  sequia_reciente <- leer(function() {
    u <- q_sequia_ultimo_obs(db)
    if (length(u) == 0 || is.na(u)) return(tibble::tibble())
    q_sequia_meses(db, sumar_meses(u, -11))
  })

  # Mes que se muestra por defecto en sequía: el mes en curso (estimado con el
  # pronóstico por años análogos) si existe; si no, el último medido.
  mes_actual <- shiny::reactive({
    m <- inicio_mes(Sys.Date())
    meses <- unique(sequia_reciente()$mes)
    if (m %in% meses) m else ultimo_obs()
  })

  list(
    db = db,
    version = version,
    mes_actual = mes_actual,
    distritos = distritos,
    fuentes = fuentes,
    enso = enso,
    corrida = corrida,
    riesgo14 = riesgo14,
    pronostico = pronostico,
    clima = clima,
    ultimo_obs = ultimo_obs,
    sequia_reciente = sequia_reciente,
    enfen = shiny::reactive({
      d <- fuentes()$detalle[fuentes()$fuente == "enfen"]
      if (length(d) && !is.na(d)) jsonlite::fromJSON(d) else NULL
    })
  )
}

#' Peor día de cada distrito dentro de los próximos `dias`, con nombres.
peor_riesgo <- function(riesgo, distritos, dias) {
  if (nrow(riesgo) == 0) return(riesgo)
  riesgo |>
    dplyr::filter(fecha < min(fecha) + dias) |>
    peor_escenario() |>
    dplyr::left_join(dplyr::select(distritos, ubigeo, distrito, provincia, departamento, cod_dep,
                                   macrorregion, lat, lon), by = "ubigeo")
}

#' Filtra por región: "pais", "norte", "centro", "sur", "oriente", "dep:CC",
#' "prov:CCPP" o "dis:UBIGEO". Provincia y distrito se leen del UBIGEO (CC PP DD).
filtrar_region <- function(df, region) {
  if (is.null(region) || region == "pais" || nrow(df) == 0) return(df)
  if (startsWith(region, "dep:")) return(dplyr::filter(df, substr(ubigeo, 1, 2) == sub("^dep:", "", region)))
  if (startsWith(region, "prov:")) return(dplyr::filter(df, substr(ubigeo, 1, 4) == sub("^prov:", "", region)))
  if (startsWith(region, "dis:")) return(dplyr::filter(df, ubigeo == sub("^dis:", "", region)))
  dplyr::filter(df, macrorregion == region)
}

nombre_region <- function(region, distritos) {
  if (is.null(region) || region == "pais") return("todo el Perú")
  if (startsWith(region, "dep:")) return(distritos$departamento[distritos$cod_dep == sub("^dep:", "", region)][1])
  if (startsWith(region, "prov:")) {
    f <- distritos[substr(distritos$ubigeo, 1, 4) == sub("^prov:", "", region), ][1, ]
    return(sprintf("la provincia de %s (%s)", f$provincia, f$departamento))
  }
  if (startsWith(region, "dis:")) {
    f <- distritos[distritos$ubigeo == sub("^dis:", "", region), ][1, ]
    return(sprintf("el distrito de %s (%s, %s)", f$distrito, f$provincia, f$departamento))
  }
  paste("la macrorregión", region)
}

#' Nombre corto para el encabezado: «Todo el Perú», «Puno», «Prov. Lampa · Puno»…
nombre_region_titulo <- function(region, distritos) {
  if (is.null(region) || region == "pais") return("Todo el Perú")
  if (startsWith(region, "dep:")) return(nombre_region(region, distritos))
  if (startsWith(region, "prov:")) {
    f <- distritos[substr(distritos$ubigeo, 1, 4) == sub("^prov:", "", region), ][1, ]
    return(sprintf("Provincia %s · %s", f$provincia, f$departamento))
  }
  if (startsWith(region, "dis:")) {
    f <- distritos[distritos$ubigeo == sub("^dis:", "", region), ][1, ]
    return(sprintf("%s · %s, %s", f$distrito, f$provincia, f$departamento))
  }
  paste("Macrorregión", region)
}

#' Sequía de un mes, con nombres de distrito.
sequia_del_mes <- function(sequia, distritos, mes) {
  sequia |>
    dplyr::filter(.data$mes == .env$mes) |>
    dplyr::left_join(dplyr::select(distritos, ubigeo, distrito, provincia, departamento, cod_dep,
                                   macrorregion, lat, lon), by = "ubigeo")
}

en_sequia <- function(categoria) categoria %in% c("Sequía moderada", "Sequía severa", "Sequía extrema")
