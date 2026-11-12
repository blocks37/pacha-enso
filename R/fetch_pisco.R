# PISCO (SENAMHI) y agregación de grillas a distritos -----------------------------
#
# Cada distrito se calcula como el promedio ponderado de las celdas de 0,1° que lo
# cubren. Los pesos (data/geo/pesos_pisco.csv: ubigeo, lon, lat, peso) son la
# fracción del distrito que cae en cada celda y se calcularon una vez a partir de
# los límites INEI 2025. Así, actualizar PISCO es solo leer el NetCDF y multiplicar.

leer_pesos <- function(cfg) {
  readr::read_csv(file.path(cfg$rutas$geo, "pesos_pisco.csv"),
                  col_types = readr::cols(ubigeo = "c", .default = "d"))
}

#' Abre un NetCDF de PISCO como SpatRaster con una capa por paso de tiempo.
leer_raster_nc <- function(ruta) {
  if (!file.exists(ruta)) stop("No existe ", ruta, ". Copie ahí el NetCDF de PISCO.")
  r <- terra::rast(ruta)
  if (terra::nlyr(r) <= 1) {                    # el archivo trae varias variables
    s <- terra::sds(ruta)
    r <- s[[which.max(vapply(seq_along(s), function(i) terra::nlyr(s[[i]]), numeric(1)))]]
  }
  r
}

#' Promedio ponderado por distrito de una matriz celda × tiempo.
#' @param valores matriz (filas alineadas con `pesos`, columnas = pasos de tiempo)
agregar_distritos <- function(valores, pesos) {
  valores[!is.finite(valores) | valores < -1e20] <- NA
  ok <- !is.na(valores)
  num <- rowsum(ifelse(ok, valores, 0) * pesos$peso, pesos$ubigeo)
  den <- rowsum(ok * pesos$peso, pesos$ubigeo)
  m <- num / den
  m[den == 0] <- NA
  m
}

#' Valores de un raster en las celdas de los pesos (misma grilla de 0,1°).
valores_en_pesos <- function(r, pesos) {
  celdas <- terra::cellFromXY(r, as.matrix(pesos[, c("lon", "lat")]))
  if (anyNA(celdas)) stop("La grilla del raster no cubre todos los distritos.")
  terra::values(r, mat = TRUE)[celdas, , drop = FALSE]
}

#' Lluvia mensual por distrito desde el NetCDF mensual de PISCO.
pisco_mensual_distrital <- function(cfg, pesos = leer_pesos(cfg)) {
  r <- leer_raster_nc(cfg$pisco$mensual)
  if (!isTRUE(all.equal(terra::res(r), c(0.1, 0.1), tolerance = 1e-4))) {
    stop("PISCO no viene en grilla de 0,1°: regenere los pesos con data-raw/preparar_limites.R")
  }
  m <- agregar_distritos(valores_en_pesos(r, pesos), pesos)
  meses <- fechas_mensuales(r, cfg$pisco$inicio)
  tibble::tibble(
    ubigeo = rep(rownames(m), times = ncol(m)),
    mes = rep(meses, each = nrow(m)),
    pp_mm = round(as.vector(m), 1)
  ) |>
    dplyr::filter(!is.na(pp_mm))
}

#' Fechas de las capas: usa el eje de tiempo si trae fechas; si no, cuenta meses desde `inicio`.
fechas_mensuales <- function(r, inicio) {
  t <- tryCatch(terra::time(r), error = function(e) NULL)
  if (inherits(t, c("Date", "POSIXt")) && !anyNA(t)) return(inicio_mes(as.Date(t)))
  seq(as.Date(inicio), by = "month", length.out = terra::nlyr(r))
}

#' Huella del archivo local: si SENAMHI publica una versión nueva y se reemplaza
#' el archivo, la huella cambia y se recalcula todo lo que depende de él.
version_archivo <- function(ruta) {
  if (!file.exists(ruta)) stop("No existe ", ruta)
  info <- file.info(ruta)
  paste(format(info$mtime, "%Y%m%d%H%M%S"), info$size, sep = "-")
}

# PISCO diario ----------------------------------------------------------------------------
#
# El NetCDF diario (PISCOp v3, 1981–2025, ~1,5 GB) se procesa una sola vez y deja dos
# tablas pequeñas en data/geo/:
#   clima_diaria_pisco.csv   ubigeo, p50_mm, p99_mm   umbrales exactos de lluvia diaria
#   dsc_mensual_pisco.csv    ubigeo, mes, dsc          racha seca más larga de cada mes
# La actualización lo hace sola cuando encuentra el archivo (o uno nuevo); la app
# nunca vuelve a leer el archivo grande.

ruta_marca_pisco_diario <- function(cfg) file.path(dirname(cfg$pisco$clima_diaria), "pisco_diario.version")

#' Huella del PISCO diario: cambia si se reemplaza el archivo o si faltan sus resultados.
version_pisco_diario <- function(cfg) {
  nc <- cfg$pisco$diario %||% ""
  if (!nzchar(nc) || !file.exists(nc)) return("sin archivo")
  listo <- file.exists(cfg$pisco$clima_diaria) && file.exists(cfg$pisco$dsc_mensual)
  paste(version_archivo(nc), if (listo) "procesado" else "pendiente")
}

#' Procesa el PISCO diario si hace falta (archivo nuevo o resultados ausentes).
pisco_diario_al_dia <- function(cfg) {
  nc <- cfg$pisco$diario %||% ""
  if (!nzchar(nc) || !file.exists(nc)) {
    # En el servidor no hace falta el archivo grande si ya vienen sus resultados
    return(list(mensaje = if (file.exists(cfg$pisco$clima_diaria)) "Umbrales y rachas secas ya procesados (data/geo)"
                          else "Sin archivo diario: umbrales aproximados desde PISCO mensual"))
  }
  marca <- ruta_marca_pisco_diario(cfg)
  huella <- version_archivo(nc)
  hecho <- file.exists(marca) && identical(readLines(marca, n = 1, warn = FALSE), huella) &&
    file.exists(cfg$pisco$clima_diaria) && file.exists(cfg$pisco$dsc_mensual)
  if (!hecho) {
    log_info("Procesando PISCO diario (solo esta vez; tarda 10–40 minutos)…")
    procesar_pisco_diario(nc, cfg)
    writeLines(huella, marca)
  }
  clima <- readr::read_csv(cfg$pisco$clima_diaria, col_types = "cdd")
  list(mensaje = sprintf("%s distritos · umbral extremo (P99) mediano %s mm/día%s",
                         format(nrow(clima), big.mark = " "), fmt_num(stats::median(clima$p99_mm, na.rm = TRUE)),
                         if (hecho) "" else " · recién procesado"))
}

#' Lee el NetCDF diario año por año (para no llenar la memoria) y guarda las dos tablas.
procesar_pisco_diario <- function(nc, cfg = cargar_config(), inicio = cfg$pisco$inicio) {
  ref <- unlist(cfg$climatologia)
  r <- leer_raster_nc(nc)
  if (!isTRUE(all.equal(terra::res(r), c(0.1, 0.1), tolerance = 1e-4))) {
    stop("El NetCDF diario no está en la grilla de 0,1° de los pesos.")
  }
  n <- terra::nlyr(r)
  # El eje de tiempo de PISCOp v3 es solo un índice (Z = 1, 2, 3…): se cuentan días desde `inicio`
  t <- tryCatch(terra::time(r), error = function(e) NULL)
  fechas <- if (inherits(t, c("Date", "POSIXt")) && !anyNA(t) && format(min(t), "%Y") == format(as.Date(inicio), "%Y"))
    as.Date(t) else seq(as.Date(inicio), by = "day", length.out = n)
  log_info("PISCO diario: ", n, " días, de ", fechas[1], " a ", fechas[n])

  pesos <- leer_pesos(cfg)
  celdas <- terra::cellFromXY(r, as.matrix(pesos[, c("lon", "lat")]))
  if (anyNA(celdas)) stop("La grilla del PISCO diario no cubre todos los distritos.")
  anio_de <- as.integer(format(fechas, "%Y"))
  anios <- unique(anio_de)
  umbral_seco <- cfg$sequia$umbral_dia_seco_mm %||% 1

  dsc <- vector("list", length(anios)); referencia <- list()
  for (k in seq_along(anios)) {
    a <- anios[k]
    avanzar("Procesando PISCO diario (solo la primera vez)", sprintf("año %d · %d de %d", a, k, length(anios)),
            k, length(anios))
    idx <- which(anio_de == a)
    v <- terra::values(r[[idx]], mat = TRUE)[celdas, , drop = FALSE]
    m <- agregar_distritos(v, pesos)                          # distritos × días
    rm(v)
    meses <- inicio_mes(fechas[idx])
    dsc[[k]] <- dplyr::bind_rows(lapply(unique(meses), function(mes) {
      cols <- which(meses == mes)
      tibble::tibble(ubigeo = rownames(m), mes = mes,
                     dsc = apply(m[, cols, drop = FALSE], 1, dias_secos_consecutivos, umbral = umbral_seco))
    }))
    if (a >= ref[1] && a <= ref[2]) referencia[[as.character(a)]] <- m
  }

  avanzar("Procesando PISCO diario (solo la primera vez)", "umbrales de lluvia diaria por distrito")
  todo <- do.call(cbind, referencia)
  cuantil <- function(x, p) {
    x <- x[!is.na(x) & x >= 1]                                 # solo días con lluvia
    if (length(x) < 20) return(NA_real_)
    stats::quantile(x, p, names = FALSE, type = 8)
  }
  clima <- tibble::tibble(
    ubigeo = rownames(todo),
    p50_mm = round(apply(todo, 1, cuantil, p = 0.50), 1),
    p99_mm = round(apply(todo, 1, cuantil, p = 0.99), 1)
  ) |>
    dplyr::mutate(p50_mm = dplyr::coalesce(pmax(p50_mm, 1), 1), p99_mm = dplyr::coalesce(pmax(p99_mm, 8), 8))

  dir.create(dirname(cfg$pisco$clima_diaria), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(clima, cfg$pisco$clima_diaria)
  readr::write_csv(dplyr::bind_rows(dsc), cfg$pisco$dsc_mensual)
  log_info("PISCO diario listo: ", nrow(clima), " distritos")
  invisible(clima)
}

# Umbrales de lluvia diaria por distrito ------------------------------------------------

#' Mediana y percentil 99 de los días con lluvia. Si ya se procesó el PISCO diario
#' se usa esa tabla; si no, se aproxima desde la lluvia mensual suponiendo lluvias
#' diarias con distribución exponencial.
clima_diaria <- function(cfg, precip) {
  if (file.exists(cfg$pisco$clima_diaria)) {
    return(readr::read_csv(cfg$pisco$clima_diaria, col_types = "cdd") |>
             dplyr::mutate(metodo = "PISCO diario"))
  }
  ref <- unlist(cfg$climatologia)
  precip |>
    dplyr::filter(en_referencia(mes, ref), !pronostico) |>
    dplyr::mutate(m = as.integer(format(mes, "%m"))) |>
    dplyr::group_by(ubigeo, m) |>
    dplyr::summarise(media = mean(pp_mm, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(ubigeo) |>
    dplyr::summarise(mes_lluvioso = max(media), .groups = "drop") |>
    dplyr::mutate(
      dias_lluvia = pmin(pmax(mes_lluvioso / 8, 3), 20),    # días con lluvia en el mes más lluvioso
      intensidad = mes_lluvioso / dias_lluvia,               # lluvia media de un día lluvioso
      p50_mm = round(pmax(1, log(2) * intensidad), 1),       # mediana exponencial
      p99_mm = round(pmax(8, log(100) * intensidad), 1),     # percentil 99 exponencial
      metodo = "aproximado desde PISCO mensual"
    ) |>
    dplyr::select(ubigeo, p50_mm, p99_mm, metodo)
}

#' Racha seca mensual precalculada desde PISCO diario (opcional).
dsc_pisco <- function(cfg) {
  if (!file.exists(cfg$pisco$dsc_mensual)) return(NULL)
  readr::read_csv(cfg$pisco$dsc_mensual, col_types = readr::cols(ubigeo = "c", mes = "D", dsc = "i"))
}
