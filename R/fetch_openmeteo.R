# Open-Meteo: pronóstico diario (ECMWF IFS por defecto; ver config.yml) y elevación --
#
# Se usa ECMWF IFS 0,25° y no GFS: en noviembre de 2026 GFS daba 3–4 veces más lluvia
# que ECMWF en el altiplano (Lampa 109 vs 28 mm en 14 días) y casi nada en la costa
# norte con El Niño costero (Tumbes 60 vs 254 mm). GFS tiene un sesgo húmedo conocido
# sobre los Andes.
# Sin clave de API. Plan gratuito: 600 llamadas/min y 10 000/día. Para no pasarse,
# los 1 891 distritos se agrupan en puntos de una grilla de 0,25° (~805 puntos)
# y se pide en lotes con pausa.

consultar_openmeteo <- function(url, params, cfg_http) {
  peticion(url, cfg_http) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = cfg_http$reintentos, max_seconds = cfg_http$max_espera_s %||% 120,
                     retry_on_failure = TRUE, backoff = function(i) 20 * i) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
}

#' Asigna cada distrito a un punto de grilla de `res` grados.
puntos_grilla <- function(distritos, res) {
  distritos |>
    dplyr::transmute(ubigeo,
                     lat = round(round(lat / res) * res, 4),
                     lon = round(round(lon / res) * res, 4))
}

#' Ejecuta `fun(lote)` por lotes de puntos únicos, con pausa entre lotes.
por_lotes <- function(puntos, cfg_http, fun, etiqueta = "Open-Meteo") {
  lotes <- split(puntos, ceiling(seq_len(nrow(puntos)) / cfg_http$lote_coordenadas))
  purrr::imap_dfr(lotes, function(lote, i) {
    i <- as.integer(i)
    avanzar(etiqueta, sprintf("lote %d de %d (%d puntos)", i, length(lotes), nrow(lote)), i, length(lotes))
    if (i > 1) Sys.sleep(cfg_http$pausa_s)
    fun(lote)
  })
}

a_numerico <- function(valores) {
  vapply(valores, function(z) if (is.null(z)) NA_real_ else as.numeric(z), numeric(1))
}

#' Pasa la respuesta JSON (uno o varios puntos) a tabla larga.
respuesta_a_tabla <- function(resp, lote, variable, bloque = "daily") {
  if (!is.null(resp[[bloque]])) resp <- list(resp)   # un solo punto
  purrr::map2_dfr(resp, seq_len(nrow(lote)), function(r, i) {
    datos <- r[[bloque]]
    tibble::tibble(lat = lote$lat[i], lon = lote$lon[i],
                   fecha = as.Date(unlist(datos$time)),
                   valor = a_numerico(datos[[variable]]))
  })
}

coordenadas <- function(lote) {
  list(latitude = paste(lote$lat, collapse = ","), longitude = paste(lote$lon, collapse = ","))
}

#' GFS y ECMWF IFS corren 4 veces al día (00, 06, 12, 18 UTC) y Open-Meteo las
#' publica unas horas después: la versión es la última corrida que ya debería estar.
version_gfs <- function(ahora = Sys.time(), etiqueta = "GFS", retraso_h = 5) {
  t <- as.POSIXct(ahora, tz = "UTC") - retraso_h * 3600
  corrida <- as.POSIXct(floor(as.numeric(t) / 21600) * 21600, origin = "1970-01-01", tz = "UTC")
  paste(etiqueta, format(corrida, "%Y-%m-%d %H UTC"))
}

#' Las 4 esquinas de la celda de grilla que contiene a cada distrito, con pesos
#' bilineales (más peso a la esquina más cercana). Así un solo punto de grilla con
#' un valor extremo no define él solo la lluvia del distrito.
esquinas_grilla <- function(distritos, res) {
  x0 <- floor(distritos$lon / res) * res
  y0 <- floor(distritos$lat / res) * res
  fx <- (distritos$lon - x0) / res
  fy <- (distritos$lat - y0) / res
  dplyr::bind_rows(
    tibble::tibble(ubigeo = distritos$ubigeo, lon = x0,       lat = y0,       peso = (1 - fx) * (1 - fy)),
    tibble::tibble(ubigeo = distritos$ubigeo, lon = x0 + res, lat = y0,       peso = fx * (1 - fy)),
    tibble::tibble(ubigeo = distritos$ubigeo, lon = x0,       lat = y0 + res, peso = (1 - fx) * fy),
    tibble::tibble(ubigeo = distritos$ubigeo, lon = x0 + res, lat = y0 + res, peso = fx * fy)
  ) |>
    dplyr::mutate(lat = round(lat, 4), lon = round(lon, 4)) |>
    dplyr::filter(peso > 0.01)
}

#' Lluvia diaria pronosticada por distrito para los próximos `dias`
#' (interpolación bilineal de la grilla del modelo; config gfs$interpolacion).
fetch_gfs <- function(distritos, cfg) {
  bilineal <- !identical(cfg$gfs$interpolacion, "cercano")
  asignacion <- if (bilineal) esquinas_grilla(distritos, cfg$gfs$resolucion) else
    dplyr::mutate(puntos_grilla(distritos, cfg$gfs$resolucion), peso = 1)
  unicos <- dplyr::distinct(asignacion, lat, lon)
  log_info("Pronóstico ", cfg$gfs$modelo, ": ", nrow(unicos), " puntos")
  datos <- por_lotes(unicos, cfg$http, etiqueta = paste("Descargando pronóstico", cfg$gfs$etiqueta %||% "GFS"), fun = function(lote) {
    resp <- consultar_openmeteo(cfg$gfs$url, c(coordenadas(lote), list(
      daily = "precipitation_sum",
      models = cfg$gfs$modelo,
      forecast_days = cfg$gfs$dias,
      timezone = cfg$zona_horaria
    )), cfg$http)
    respuesta_a_tabla(resp, lote, "precipitation_sum")
  })
  datos <- dplyr::mutate(datos, lat = round(lat, 4), lon = round(lon, 4))
  asignacion |>
    dplyr::inner_join(datos, by = c("lat", "lon"), relationship = "many-to-many") |>
    dplyr::filter(!is.na(valor)) |>
    dplyr::group_by(ubigeo, fecha) |>
    dplyr::summarise(pp_mm = sum(peso * pmax(valor, 0)) / sum(peso), .groups = "drop") |>   # ECMWF da a veces −0,1 mm
    dplyr::transmute(ubigeo, fecha, pp_mm = round(pp_mm, 1), modelo = cfg$gfs$modelo)
}

#' Altitud del punto interior de cada distrito (Copernicus DEM 90 m vía Open-Meteo).
fetch_elevacion <- function(distritos, cfg) {
  lotes <- split(distritos, ceiling(seq_len(nrow(distritos)) / 100))
  purrr::imap_dfr(lotes, function(lote, i) {
    avanzar("Consultando altitudes", sprintf("lote %s de %d", i, length(lotes)), as.integer(i), length(lotes))
    Sys.sleep(1)
    resp <- consultar_openmeteo(cfg$elevacion_url, coordenadas(lote), cfg$http)
    tibble::tibble(ubigeo = lote$ubigeo, altitud = a_numerico(resp$elevation))
  })
}
