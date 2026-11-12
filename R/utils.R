# Utilidades generales ------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Recorta valores a un intervalo (por defecto 0–1).
acotar <- function(x, min = 0, max = 1) pmin(pmax(x, min), max)

#' Lee config.yml respetando R_CONFIG_ACTIVE.
cargar_config <- function(archivo = "config.yml") config::get(file = archivo)

#' Primer día del mes de una fecha.
inicio_mes <- function(fecha) as.Date(format(as.Date(fecha), "%Y-%m-01"))

#' Suma (o resta) meses a fechas que son primero de mes. Vectorizado.
sumar_meses <- function(fecha, k) {
  fecha <- as.Date(fecha)
  total <- as.integer(format(fecha, "%Y")) * 12 + as.integer(format(fecha, "%m")) - 1 + k
  as.Date(sprintf("%d-%02d-01", total %/% 12, total %% 12 + 1))
}

#' Meses entre `desde` y cada fecha de `hasta`.
meses_entre <- function(desde, hasta) {
  a <- function(f) as.integer(format(as.Date(f), "%Y")) * 12 + as.integer(format(as.Date(f), "%m"))
  a(hasta) - a(desde)
}

en_referencia <- function(fecha, ref) {
  anio <- as.integer(format(as.Date(fecha), "%Y"))
  anio >= ref[1] & anio <= ref[2]
}

#' Clasifica el ONI en fases del ENSO (umbral ±0,5 °C).
fase_enso <- function(oni) {
  factor(
    dplyr::case_when(oni >= 0.5 ~ "El Niño", oni <= -0.5 ~ "La Niña", TRUE ~ "Neutro"),
    levels = c("La Niña", "Neutro", "El Niño")
  )
}

# Formato en español --------------------------------------------------------

MESES_ES <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
MESES_LARGO <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                 "agosto", "septiembre", "octubre", "noviembre", "diciembre")

fmt_num <- function(x, digitos = 1) {
  ifelse(is.na(x), "—",
         formatC(x, format = "f", digits = digitos, decimal.mark = ",", big.mark = " "))
}

fmt_signo <- function(x, digitos = 1, unidad = "") {
  ifelse(is.na(x), "—", paste0(ifelse(x > 0, "+", ifelse(x < 0, "−", "")), fmt_num(abs(x), digitos), unidad))
}

fmt_pct <- function(x, digitos = 0) ifelse(is.na(x), "—", paste0(fmt_num(100 * x, digitos), " %"))

fmt_fecha <- function(x) {
  x <- as.Date(x)
  ifelse(is.na(x), "—",
         paste(as.integer(format(x, "%d")), MESES_ES[as.integer(format(x, "%m"))], format(x, "%Y")))
}

#' «oct 26»: etiqueta corta de mes que no depende del idioma del servidor.
fmt_mes <- function(x) {
  x <- as.Date(x)
  paste(MESES_ES[as.integer(format(x, "%m"))], format(x, "%y"))
}

fmt_mes_largo <- function(x) {
  x <- as.Date(x)
  paste(MESES_LARGO[as.integer(format(x, "%m"))], format(x, "%Y"))
}

#' «hace 3 h», «hace 2 días».
fmt_hace <- function(t, ahora = Sys.time()) {
  if (length(t) == 0 || !is.finite(as.numeric(t))) return("nunca")
  min <- as.numeric(difftime(ahora, t, units = "mins"))
  if (min < 2) return("hace un momento")
  if (min < 60) return(sprintf("hace %d min", round(min)))
  if (min < 48 * 60) return(sprintf("hace %d h", round(min / 60)))
  sprintf("hace %d días", round(min / 1440))
}

# Registro ------------------------------------------------------------------
# Escribe en la consola y, si options(enso.log = "ruta") está definido, en ese archivo.

escribir_log <- function(nivel, msg) {
  linea <- sprintf("%s [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), nivel, msg)
  message(linea)
  archivo <- getOption("enso.log")
  if (!is.null(archivo)) cat(linea, "\n", file = archivo, append = TRUE, sep = "")
  invisible(linea)
}

log_info <- function(...) escribir_log("INFO", paste0(...))
log_aviso <- function(...) escribir_log("AVISO", paste0(...))
log_error <- function(...) escribir_log("ERROR", paste0(...))

# Paquetes ----------------------------------------------------------------------------

PAQUETES_ACTUALIZACION <- c("rlang", "vctrs", "tibble", "dplyr", "tidyr", "purrr", "readr", "stringr",
                            "stringi", "jsonlite", "httr2", "xml2", "rvest", "R.utils", "DBI", "duckdb",
                            "sf", "terra", "SPEI", "trend", "pointblank", "ps", "config")

#' Carga de una vez todos los paquetes que usa la actualización. Si falta uno o
#' alguno exige una versión más nueva de otro («namespace 'dplyr' 1.1.4 is already
#' loaded, but >= 1.2.0 is required»), falla en segundos y dice cómo arreglarlo,
#' en vez de fallar al final, después de media hora de cálculo.
verificar_paquetes <- function(paquetes = PAQUETES_ACTUALIZACION, avisar_pdftools = TRUE) {
  problemas <- character()
  for (p in paquetes) {
    msg <- tryCatch({ loadNamespace(p); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(msg)) problemas <- c(problemas, paste0(p, ": ", msg))
  }
  if (length(problemas)) {
    stop("Hay paquetes de R que faltan o están desactualizados. Cierre RStudio por completo y haga doble clic en ",
         "reparar_paquetes.bat (o reinicie R y corra source(\"install.R\")). Detalle: ",
         paste(problemas, collapse = " | "), call. = FALSE)
  }
  if (avisar_pdftools && !requireNamespace("pdftools", quietly = TRUE)) {
    log_aviso("Falta el paquete pdftools: no se leerán los ríos del comunicado del ENFEN (corra source(\"install.R\"))")
  }
  invisible(TRUE)
}

# Acceso ------------------------------------------------------------------------------

#' ¿Quien abre la app es el administrador? La plataforma es de solo lectura para el
#' público: nadie que la visite descarga ni recalcula nada. El panel de actualización
#' aparece solo (1) en el propio computador (localhost) si acceso$admin_local es true,
#' o (2) entrando con ?admin=CLAVE, donde CLAVE es la variable de entorno
#' ENSO_ADMIN_CLAVE (o acceso$admin_clave en config.yml). Sin clave configurada, (2) no existe.
es_admin <- function(session, cfg) {
  cd <- shiny::isolate(shiny::reactiveValuesToList(session$clientData))
  clave <- Sys.getenv("ENSO_ADMIN_CLAVE", cfg$acceso$admin_clave %||% "")
  q <- shiny::parseQueryString(cd$url_search %||% "")
  if (nzchar(clave) && identical(q$admin, clave)) return(TRUE)
  isTRUE(cfg$acceso$admin_local) && (cd$url_hostname %||% "") %in% c("127.0.0.1", "localhost", "::1", "[::1]")
}

#' ¿Está a la vista la pestaña `valor` de la barra principal (input$nav)?
#' Los observadores que pintan mapas de pestañas ocultas esperan a que se abran:
#' así la primera carga solo calcula el Panorama.
pestana_activa <- function(session, valor) {
  identical(session$rootScope()$input$nav %||% "panorama", valor)
}
