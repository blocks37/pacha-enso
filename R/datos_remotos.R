# Datos publicados en GitHub ---------------------------------------------------------------
#
# En la web (Posit Connect Cloud) la app no calcula nada: la tarea «actualizar-datos» de
# GitHub Actions corre actualizar.R cada 6 h y publica la base en la release «datos» del
# repositorio (pacha_db.tgz + VERSION). La app revisa VERSION cada 30 min y, si cambió,
# descarga la base nueva y cambia el puntero ACTUAL: las sesiones abiertas pasan a la
# base nueva solas (datos_compartidos() vigila ACTUAL).
#
# En la PC del administrador no se usa: allí la carpeta data/db ya tiene su propia base
# (sin el archivo .remota), y nunca se reemplaza por la de GitHub.

#' Dirección de la release de datos, o NULL si no hay repositorio configurado.
url_datos_remotos <- function(cfg) {
  repo <- Sys.getenv("ENSO_REPO_DATOS", cfg$datos_remotos$repo %||% "")
  if (!nzchar(repo) || !grepl("^[^/ ]+/[^/ ]+$", repo)) return(NULL)
  sprintf("https://github.com/%s/releases/download/%s", repo, cfg$datos_remotos$etiqueta %||% "datos")
}

#' ¿Esta copia de la app se alimenta de la base publicada en GitHub?
usa_datos_remotos <- function(cfg) {
  !is.null(url_datos_remotos(cfg)) &&
    (is.null(db_actual(cfg$rutas$db)) || file.exists(file.path(cfg$rutas$db, ".remota")))
}

bajar_archivo <- function(url, destino) {
  isTRUE(tryCatch(utils::download.file(url, destino, quiet = TRUE, mode = "wb") == 0,
                  error = function(e) FALSE, warning = function(w) FALSE))
}

#' Descarga la base publicada si es más nueva que la local. Devuelve TRUE si cambió.
sincronizar_datos_remotos <- function(cfg) {
  url <- url_datos_remotos(cfg)
  if (is.null(url)) return(invisible(FALSE))
  carpeta <- cfg$rutas$db
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  marca <- file.path(carpeta, ".remota")

  version <- tempfile()
  on.exit(unlink(version), add = TRUE)
  if (!bajar_archivo(paste0(url, "/VERSION"), version)) {
    message("[datos] No se pudo leer ", url, "/VERSION (¿todavía no corre la tarea de GitHub?)")
    return(invisible(FALSE))
  }
  remota <- trimws(readLines(version, n = 1, warn = FALSE))
  local <- if (file.exists(marca)) trimws(readLines(marca, n = 1, warn = FALSE)) else ""
  if (identical(remota, local) && !is.null(db_actual(carpeta))) return(invisible(FALSE))

  options(timeout = max(900, getOption("timeout")))
  tgz <- tempfile(fileext = ".tgz")
  temporal <- tempfile()
  on.exit(unlink(c(tgz, temporal), recursive = TRUE), add = TRUE)
  if (!bajar_archivo(paste0(url, "/pacha_db.tgz"), tgz)) {
    message("[datos] Falló la descarga de pacha_db.tgz")
    return(invisible(FALSE))
  }
  dir.create(temporal)
  utils::untar(tgz, exdir = temporal)
  nueva <- trimws(readLines(file.path(temporal, "ACTUAL"), n = 1, warn = FALSE))
  if (!file.exists(file.path(temporal, nueva))) {
    message("[datos] El paquete descargado no trae la base ", nueva)
    return(invisible(FALSE))
  }
  # Se copia con nombre propio de esta versión: las sesiones que leen la anterior no se cortan
  destino <- sub("\\.duckdb$", paste0("_", format(Sys.time(), "%H%M%S"), ".duckdb"), nueva)
  file.copy(file.path(temporal, nueva), file.path(carpeta, destino), overwrite = TRUE)
  for (f in c("ULTIMA_REVISION", "METODO_SEQUIA")) {
    if (file.exists(file.path(temporal, f))) file.copy(file.path(temporal, f), file.path(carpeta, f), overwrite = TRUE)
  }
  writeLines(remota, marca)
  writeLines(destino, file.path(carpeta, "ACTUAL.tmp"))
  file.rename(file.path(carpeta, "ACTUAL.tmp"), file.path(carpeta, "ACTUAL"))
  # Bases viejas: se borran (en Linux, quien aún las tenga abiertas sigue leyendo sin problema)
  viejas <- setdiff(list.files(carpeta, pattern = "\\.duckdb$"), destino)
  unlink(file.path(carpeta, viejas))
  message("[datos] Base actualizada desde GitHub: ", remota)
  invisible(TRUE)
}

#' Revisa la base publicada al arrancar (sin hacer esperar a nadie) y luego cada
#' `cada_min` minutos, mientras la app esté despierta.
programar_datos_remotos <- function(cfg) {
  if (!usa_datos_remotos(cfg)) return(invisible(FALSE))
  cada <- 60 * (cfg$datos_remotos$cada_min %||% 30)
  revisar <- function() {
    try(sincronizar_datos_remotos(cfg), silent = TRUE)
    later::later(revisar, cada)
  }
  later::later(revisar, 1)
  invisible(TRUE)
}
