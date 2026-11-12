# CHIRPS mensual: llena los meses que PISCO todavía no cubre ----------------------
#
# PISCO se publica con meses de retraso. CHIRPS v3.0 (Climate Hazards Center) sale
# cada mes, unas 4–5 semanas después de terminado (la v2.0 ya no se publica). Se
# usa el recorte de Latinoamérica (~4 MB por mes), se recorta Perú, se lleva a la
# grilla de PISCO (0,1°) y se corrige su sesgo con un factor por distrito y época
# del año calculado en los últimos años en que ambos se traslapan.

EXTENSION_PERU <- c(-81.4, -64.0, -19.0, 2.0)   # bordes de la grilla PISCO

ruta_chirps <- function(mes, cfg) {
  file.path(cfg$rutas$raw, "chirps", sprintf("chirps_peru_%s.tif", format(as.Date(mes), "%Y_%m")))
}

#' URL de un mes. CHIRPS v3.0 Latinoamérica: «chirps-v3.0.AAAA.MM.tif» (sin comprimir);
#' la v2.0 global venía como «.tif.gz» y también se acepta.
url_chirps <- function(mes, cfg) {
  archivo <- cfg$chirps$archivo %||% "chirps-v3.0.%s.tif"
  paste0(sub("/+$", "", cfg$chirps$url), "/", sprintf(archivo, format(as.Date(mes), "%Y.%m")))
}

existe_url <- function(url, cfg_http) {
  resp <- tryCatch(
    peticion(url, cfg_http) |> httr2::req_method("HEAD") |>
      httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform(),
    error = function(e) NULL
  )
  !is.null(resp) && httr2::resp_status(resp) == 200
}

#' Último mes publicado: prueba desde el mes pasado hacia atrás.
chirps_ultimo_mes <- function(cfg) {
  mes <- inicio_mes(Sys.Date())
  for (k in seq_len(cfg$chirps$meses_atras)) {
    mes <- sumar_meses(mes, -1)
    if (existe_url(url_chirps(mes, cfg), cfg$http)) return(mes)
  }
  stop("No se encontró CHIRPS de los últimos ", cfg$chirps$meses_atras, " meses")
}

#' Recorta Perú, marca los «sin dato» y pasa de 0,05° a la grilla de PISCO (0,1°).
#' `fuente` es un archivo local o una dirección «/vsicurl/https://…» (lee solo el recorte).
recortar_chirps <- function(fuente) {
  r <- terra::crop(terra::rast(fuente), terra::ext(EXTENSION_PERU))
  r <- terra::classify(r, cbind(-Inf, -1, NA))             # −9999 = sin dato
  terra::aggregate(r, fact = 2, fun = "mean", na.rm = TRUE)   # 0,05° → 0,1°
}

#' Descarga un mes (si no está en caché), recorta Perú y lo lleva a 0,1°.
chirps_mes <- function(mes, cfg) {
  destino <- ruta_chirps(mes, cfg)
  if (file.exists(destino)) return(destino)
  dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE)
  url <- url_chirps(mes, cfg)
  comprimido <- grepl("\\.gz$", url)
  bajado <- tempfile(fileext = if (comprimido) ".tif.gz" else ".tif")
  tif <- sub("\\.gz$", "", bajado)
  on.exit(unlink(c(bajado, tif)), add = TRUE)
  peticion(url, cfg$http, timeout_s = 900) |> httr2::req_perform(path = bajado)
  if (comprimido) R.utils::gunzip(bajado, destname = tif, overwrite = TRUE)
  terra::writeRaster(recortar_chirps(tif), destino, overwrite = TRUE)
  destino
}

#' Lluvia mensual por distrito desde CHIRPS para un conjunto de meses.
chirps_distrital <- function(meses, cfg, pesos = leer_pesos(cfg), para = "") {
  purrr::imap_dfr(as.list(meses), function(mes, i) {
    en_cache <- file.exists(ruta_chirps(mes, cfg))
    avanzar(if (en_cache) "Leyendo CHIRPS (en caché)" else "Descargando CHIRPS",
            sprintf("%s · %d de %d%s", format(mes, "%Y-%m"), i, length(meses), para), i, length(meses))
    log_info("CHIRPS ", format(mes, "%Y-%m"), if (en_cache) " (caché)" else " (descarga)")
    r <- terra::rast(chirps_mes(mes, cfg))
    if (!en_cache) Sys.sleep(cfg$chirps$pausa_s %||% 1)   # el servidor limita pedidos seguidos (HTTP 429)
    m <- agregar_distritos(valores_en_pesos(r, pesos), pesos)
    tibble::tibble(ubigeo = rownames(m), mes = as.Date(mes), pp_mm = as.vector(m[, 1]))
  })
}

#' Factor PISCO/CHIRPS por distrito y mes del año, con ventana de 3 meses para
#' que no dependa de pocos datos. Se limita a 0,5–2 y vale 1 donde casi no llueve.
factor_ajuste <- function(pisco, chirps) {
  dplyr::inner_join(pisco, chirps, by = c("ubigeo", "mes"), suffix = c("_p", "_c")) |>
    dplyr::mutate(m = as.integer(format(mes, "%m"))) |>
    tidyr::crossing(desplazamiento = -1:1) |>
    dplyr::mutate(m = (m - 1 + desplazamiento) %% 12 + 1) |>
    dplyr::group_by(ubigeo, m) |>
    dplyr::summarise(suma_p = sum(pp_mm_p, na.rm = TRUE), suma_c = sum(pp_mm_c, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(factor = ifelse(suma_c < 5, 1, pmin(pmax(suma_p / suma_c, 0.5), 2))) |>
    dplyr::select(ubigeo, m, factor)
}

#' Meses posteriores a PISCO, con CHIRPS corregido.
#'
#' Primero los meses de CHIRPS mensual (definitivo, sale ~5 semanas después). Luego,
#' si `hasta_dia` existe, los meses ya terminados que solo están en CHIRPS diario
#' preliminar (sale a los 2–3 días) y el acumulado del mes en curso. A todo se le
#' aplica el mismo factor PISCO/CHIRPS por distrito y época del año.
#' Devuelve list(mensual, en_curso): `en_curso` = lluvia del mes actual hasta `hasta_dia`.
chirps_completar <- function(pisco, cfg, hasta, hasta_dia = NA) {
  fin_pisco <- max(pisco$mes)
  pesos <- leer_pesos(cfg)
  nuevos <- NULL
  if (hasta > fin_pisco) {
    faltan <- seq(sumar_meses(fin_pisco, 1), hasta, by = "month")
    nuevos <- chirps_distrital(faltan, cfg, pesos, para = " · meses después de PISCO") |>
      dplyr::mutate(fuente = "CHIRPS ajustado")
  }
  diario <- NULL
  if (!is.na(hasta_dia)) {
    desde <- sumar_meses(max(hasta, fin_pisco), 1)
    diario <- tryCatch(chirps_diario_meses(desde, as.Date(hasta_dia), cfg, pesos),
                       error = function(e) { log_error("CHIRPS diario: ", conditionMessage(e)); NULL })
    if (!is.null(diario) && nrow(diario$mensual)) {
      nuevos <- dplyr::bind_rows(nuevos, dplyr::mutate(diario$mensual, fuente = "CHIRPS diario ajustado"))
    }
  }
  if (is.null(nuevos) && is.null(diario$en_curso)) return(list(mensual = NULL, en_curso = NULL))

  ajustar <- function(d) d
  anios <- cfg$chirps$anios_ajuste
  if (anios > 0) {
    traslape <- seq(sumar_meses(fin_pisco, -12 * anios + 1), fin_pisco, by = "month")
    factores <- factor_ajuste(dplyr::filter(pisco, mes %in% traslape),
                              chirps_distrital(traslape, cfg, pesos, para = " · traslape para corregir sesgo"))
    ajustar <- function(d) {
      if (is.null(d) || nrow(d) == 0) return(d)
      d |>
        dplyr::mutate(m = as.integer(format(mes, "%m"))) |>
        dplyr::left_join(factores, by = c("ubigeo", "m")) |>
        dplyr::mutate(pp_mm = pp_mm * dplyr::coalesce(factor, 1)) |>
        dplyr::select(-m, -factor)
    }
  }
  list(
    mensual = if (!is.null(nuevos)) dplyr::mutate(ajustar(nuevos), pp_mm = round(pp_mm, 1)),
    en_curso = if (!is.null(diario$en_curso)) dplyr::mutate(ajustar(diario$en_curso), pp_mm = round(pp_mm, 1))
  )
}

# CHIRPS diario preliminar ---------------------------------------------------------------
#
# El CHIRPS mensual definitivo sale ~5 semanas después de cada mes; el diario
# preliminar (satélite, CHIRPS v3.0) sale a los 2–3 días. Con él se cierran los meses
# que el mensual todavía no trae y se conoce la lluvia del mes en curso. Se lee solo
# el recorte de Perú de cada archivo global («/vsicurl/»): unos pocos MB por día, y
# cada día queda en caché (data/raw/chirps_diario/).

ruta_chirps_diario <- function(dia, cfg) {
  file.path(cfg$rutas$raw, "chirps_diario", sprintf("chirps_peru_%s.tif", format(as.Date(dia), "%Y_%m_%d")))
}

#' Direcciones candidatas de un día: «{Y}», «{m}», «{d}» se reemplazan por la fecha.
urls_chirps_diario <- function(dia, cfg) {
  dia <- as.Date(dia)
  base <- sub("/+$", "", cfg$chirps$diario$url)
  vapply(unlist(cfg$chirps$diario$archivos), function(p) {
    p <- gsub("{Y}", format(dia, "%Y"), p, fixed = TRUE)
    p <- gsub("{m}", format(dia, "%m"), p, fixed = TRUE)
    paste0(base, "/", gsub("{d}", format(dia, "%d"), p, fixed = TRUE))
  }, character(1), USE.NAMES = FALSE)
}

#' Último día publicado: prueba desde ayer hacia atrás (o el último en caché).
chirps_diario_ultimo_dia <- function(cfg) {
  if (is.null(cfg$chirps$diario$url)) stop("CHIRPS diario no está configurado (config.yml › chirps › diario)")
  dia <- Sys.Date() - 1
  for (k in seq_len(cfg$chirps$diario$dias_buscar %||% 12)) {
    if (file.exists(ruta_chirps_diario(dia, cfg))) return(dia)
    for (url in urls_chirps_diario(dia, cfg)) if (existe_url(url, cfg$http)) return(dia)
    Sys.sleep(0.3)
    dia <- dia - 1
  }
  stop("No se encontró CHIRPS diario de los últimos ", cfg$chirps$diario$dias_buscar %||% 12, " días")
}

#' Un día recortado a Perú (de la caché o descargado). NULL si no está publicado.
chirps_dia <- function(dia, cfg) {
  destino <- ruta_chirps_diario(dia, cfg)
  if (file.exists(destino)) return(destino)
  dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE)
  # Se escribe a un temporal y se renombra: un corte a medias no deja un archivo roto en caché
  temporal <- sub("\\.tif$", ".parcial.tif", destino)
  guardar <- function(fuente) {
    terra::writeRaster(recortar_chirps(fuente), temporal, overwrite = TRUE)
    file.rename(temporal, destino)
  }
  for (url in urls_chirps_diario(dia, cfg)) {
    if (!existe_url(url, cfg$http)) next
    # 1) Solo el recorte, sin bajar el archivo global
    if (isTRUE(tryCatch(guardar(paste0("/vsicurl/", url)), error = function(e) FALSE))) return(destino)
    # 2) Si GDAL no puede leerlo a distancia, se descarga entero y se recorta
    tif <- tempfile(fileext = ".tif")
    ok <- tryCatch({
      peticion(url, cfg$http, timeout_s = 900) |> httr2::req_perform(path = tif)
      guardar(tif)
    }, error = function(e) { log_aviso("CHIRPS diario ", dia, ": ", conditionMessage(e)); FALSE })
    unlink(c(tif, temporal))
    if (isTRUE(ok)) return(destino)
  }
  NULL
}

#' Lluvia por distrito de los días entre `desde` (primer día de un mes) y `hasta_dia`.
#' Devuelve list(mensual = meses completos, en_curso = acumulado del último mes
#' incompleto, con columna `hasta` = último día con dato).
chirps_diario_meses <- function(desde, hasta_dia, cfg, pesos = leer_pesos(cfg)) {
  desde <- inicio_mes(desde)
  if (hasta_dia < desde) return(NULL)
  dias <- seq(desde, hasta_dia, by = "day")
  # El servidor del CHC responde 429 si se le pide muy seguido: que GDAL reintente con calma
  Sys.setenv(GDAL_HTTP_MAX_RETRY = "4", GDAL_HTTP_RETRY_DELAY = "20", GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
  capas <- list()
  for (i in seq_along(dias)) {
    en_cache <- file.exists(ruta_chirps_diario(dias[i], cfg))
    avanzar(if (en_cache) "Leyendo CHIRPS diario (en caché)" else "Descargando CHIRPS diario",
            sprintf("%s · %d de %d", format(dias[i]), i, length(dias)), i, length(dias))
    ruta <- chirps_dia(dias[i], cfg)
    if (is.null(ruta)) { log_aviso("CHIRPS diario: falta el ", dias[i]); next }
    if (!en_cache) Sys.sleep(cfg$chirps$pausa_s %||% 1)
    capas[[format(dias[i])]] <- ruta
  }
  if (length(capas) == 0) return(NULL)
  log_info("CHIRPS diario: ", length(capas), " días, ", names(capas)[1], " a ", utils::tail(names(capas), 1))
  r <- terra::rast(unlist(capas))
  m <- agregar_distritos(valores_en_pesos(r, pesos), pesos)        # distritos × días
  fechas <- as.Date(names(capas))
  meses <- inicio_mes(fechas)
  por_mes <- lapply(unique(meses), function(mes) {
    cols <- which(meses == mes)
    fin_mes <- sumar_meses(mes, 1) - 1
    completo <- length(cols) == as.integer(fin_mes - mes) + 1
    tibble::tibble(ubigeo = rownames(m), mes = mes, pp_mm = rowSums(m[, cols, drop = FALSE], na.rm = TRUE),
                   completo = completo, hasta = max(fechas[cols]), dias = length(cols))
  })
  todo <- dplyr::bind_rows(por_mes)
  mensual <- dplyr::filter(todo, completo) |> dplyr::select(ubigeo, mes, pp_mm)
  # Solo el último mes puede ir incompleto; si falta un día en medio de un mes pasado, se descarta
  ultimo <- max(todo$mes)
  en_curso <- dplyr::filter(todo, !completo, mes == ultimo) |> dplyr::select(ubigeo, mes, pp_mm, hasta, dias)
  list(mensual = mensual, en_curso = if (nrow(en_curso)) en_curso)
}
