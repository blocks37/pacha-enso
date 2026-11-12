# NOAA CPC: ONI trimestral y anomalías semanales de Niño 1+2 y 3.4 --------------

#' Petición HTTP con límites duros: si un sitio no responde, falla en ~2 min como
#' máximo (conexión 20 s, transferencia detenida 30 s, total `timeout_s`, reintentos
#' acotados a `max_espera_s` aunque el servidor pida esperar más con Retry-After).
peticion <- function(url, cfg_http, timeout_s = cfg_http$timeout_s %||% 60) {
  httr2::request(url) |>
    httr2::req_user_agent("enso-peru (R; monitoreo de riesgo climatico)") |>
    httr2::req_options(connecttimeout = 20, low_speed_limit = 50, low_speed_time = 30) |>
    httr2::req_timeout(timeout_s) |>
    httr2::req_retry(max_tries = cfg_http$reintentos, max_seconds = cfg_http$max_espera_s %||% 120,
                     retry_on_failure = TRUE, backoff = function(i) 5 * i)
}

leer_texto <- function(url, cfg_http) {
  peticion(url, cfg_http) |> httr2::req_perform() |> httr2::resp_body_string()
}

#' Huella barata de un archivo web: fecha de modificación o ETag; si el servidor
#' no las da, se descarga y se usa un hash del contenido.
version_http <- function(url, cfg_http) {
  resp <- tryCatch(
    peticion(url, cfg_http) |> httr2::req_method("HEAD") |> httr2::req_perform(),
    error = function(e) NULL
  )
  if (!is.null(resp)) {
    marca <- httr2::resp_header(resp, "last-modified") %||% httr2::resp_header(resp, "etag")
    if (!is.null(marca)) return(marca)
  }
  rlang::hash(leer_texto(url, cfg_http))
}

#' ONI: anomalía trimestral móvil de Niño 3.4. Cada trimestre se fecha en su mes central.
fetch_oni <- function(url, cfg_http) {
  tabla <- utils::read.table(text = leer_texto(url, cfg_http), header = TRUE, stringsAsFactors = FALSE)
  trimestres <- c("DJF", "JFM", "FMA", "MAM", "AMJ", "MJJ",
                  "JJA", "JAS", "ASO", "SON", "OND", "NDJ")
  tibble::tibble(
    fecha = as.Date(sprintf("%d-%02d-01", tabla$YR, match(tabla$SEAS, trimestres))),
    indice = "oni",
    valor = as.numeric(tabla$ANOM),
    fuente = "NOAA CPC"
  ) |>
    dplyr::filter(!is.na(fecha), !is.na(valor))
}

#' Anomalías semanales (base 1991–2020) de las regiones Niño.
#' El archivo pega valores sin espacio («23.4-0.4»), así que se leen con una
#' expresión regular en lugar de anchos fijos.
fetch_nino_semanal <- function(url, cfg_http) {
  lineas <- strsplit(leer_texto(url, cfg_http), "\n", fixed = TRUE)[[1]]
  lineas <- grep("^\\s*\\d{2}[A-Z]{3}\\d{4}", lineas, value = TRUE)
  clave <- trimws(substr(lineas, 1, 10))                 # p. ej. 04NOV2026
  fechas <- as.Date(sprintf("%s-%02d-%s", substr(clave, 6, 9),
                            match(substr(clave, 3, 5), toupper(month.abb)),
                            substr(clave, 1, 2)))
  resto <- substring(lineas, 11)
  numeros <- regmatches(resto, gregexpr("-?\\d+\\.\\d", resto))
  validos <- lengths(numeros) == 8
  m <- do.call(rbind, lapply(numeros[validos], as.numeric))
  # Columnas: SST y SSTA para Niño 1+2, 3, 3.4 y 4.
  dplyr::bind_rows(
    tibble::tibble(fecha = fechas[validos], indice = "nino12", valor = m[, 2]),
    tibble::tibble(fecha = fechas[validos], indice = "nino34", valor = m[, 6])
  ) |>
    dplyr::mutate(fuente = "NOAA CPC") |>
    dplyr::filter(!is.na(fecha))
}
