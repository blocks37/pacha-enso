# Progreso visible de la actualización ------------------------------------------------
#
# Mientras corre, actualizar.R escribe en data/db/PROGRESO qué está haciendo
# («Descargando CHIRPS 2024-03», 15 de 70…) y toca el candado .actualizando como
# «latido». La app lee ese archivo cada 2 s y muestra una barra de avance; con el
# número de proceso (PID) del candado sabe si sigue vivo y puede detenerlo.

#' Anota el paso actual. `i` y `n` son opcionales (avance dentro del paso).
avanzar <- function(paso, detalle = "", i = NA, n = NA) {
  ruta <- getOption("enso.progreso")
  if (is.null(ruta)) return(invisible())
  inicio <- getOption("enso.inicio", Sys.time())
  info <- list(pid = Sys.getpid(), inicio = format(inicio, "%Y-%m-%d %H:%M:%S"),
               hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               paso = paso, detalle = detalle, i = i, n = n)
  tmp <- paste0(ruta, ".tmp")
  try({
    writeLines(as.character(jsonlite::toJSON(info, auto_unbox = TRUE, na = "null")), tmp, useBytes = TRUE)
    file.rename(tmp, ruta)
    candado <- getOption("enso.candado")
    if (!is.null(candado) && file.exists(candado)) Sys.setFileTime(candado, Sys.time())
  }, silent = TRUE)
  invisible()
}

leer_progreso <- function(carpeta) {
  ruta <- file.path(carpeta, "PROGRESO")
  if (!file.exists(ruta)) return(NULL)
  p <- tryCatch(jsonlite::fromJSON(readLines(ruta, warn = FALSE, encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(p)) return(NULL)
  p$hora <- as.POSIXct(p$hora); p$inicio <- as.POSIXct(p$inicio)
  p
}

# Candado con número de proceso -------------------------------------------------------

ruta_candado <- function(carpeta) file.path(carpeta, ".actualizando")

pid_candado <- function(carpeta) {
  ruta <- ruta_candado(carpeta)
  if (!file.exists(ruta)) return(NA_integer_)
  suppressWarnings(as.integer(readLines(ruta, n = 1, warn = FALSE)))
}

proceso_vivo <- function(pid) {
  if (is.na(pid)) return(FALSE)
  if (pid == Sys.getpid()) return(TRUE)
  if (!requireNamespace("ps", quietly = TRUE)) return(TRUE)   # sin ps no se puede saber: asumir vivo
  tryCatch(ps::ps_is_running(ps::ps_handle(pid)), error = function(e) FALSE)
}

#' TRUE si hay una actualización corriendo de verdad. Si el candado quedó de un
#' proceso que ya murió (se cerró R, se apagó la PC), lo borra.
actualizacion_en_curso <- function(carpeta) {
  pid <- pid_candado(carpeta)
  if (is.na(pid)) return(FALSE)
  if (proceso_vivo(pid)) return(TRUE)
  unlink(ruta_candado(carpeta))
  FALSE
}

tomar_candado <- function(carpeta) {
  if (actualizacion_en_curso(carpeta)) return(NULL)
  ruta <- ruta_candado(carpeta)
  writeLines(as.character(Sys.getpid()), ruta)
  options(enso.candado = ruta, enso.progreso = file.path(carpeta, "PROGRESO"), enso.inicio = Sys.time())
  ruta
}

#' Detiene la actualización en curso (botón «Detener» de la app).
detener_actualizacion <- function(carpeta) {
  pid <- pid_candado(carpeta)
  if (!is.na(pid) && proceso_vivo(pid) && requireNamespace("ps", quietly = TRUE)) {
    try(ps::ps_kill(ps::ps_handle(pid)), silent = TRUE)
  }
  unlink(ruta_candado(carpeta))
  writeLines(as.character(jsonlite::toJSON(list(paso = "Detenida por el usuario", detalle = "",
                                                hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                inicio = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                                           auto_unbox = TRUE)), file.path(carpeta, "PROGRESO"))
  invisible(TRUE)
}

# Panel para la app ---------------------------------------------------------------------

#' Tarjeta con el paso actual, barra de avance, tiempo y últimas líneas del registro.
panel_progreso <- function(cfg, id_detener = NULL) {
  carpeta <- cfg$rutas$db
  corriendo <- actualizacion_en_curso(carpeta)
  p <- leer_progreso(carpeta)
  log <- if (file.exists(cfg$rutas$log)) utils::tail(readLines(cfg$rutas$log, warn = FALSE, encoding = "UTF-8"), 12) else character()

  cabecera <- if (corriendo && !is.null(p)) {
    quieto <- as.numeric(difftime(Sys.time(), p$hora, units = "mins"))
    pct <- if (!is.null(p$i) && !is.null(p$n) && !is.na(p$n) && p$n > 0) round(100 * p$i / p$n) else NA
    shiny::div(
      class = "progreso",
      shiny::div(class = "progreso-paso", shiny::span(class = "spinner-border spinner-border-sm"),
                 shiny::strong(p$paso), shiny::span(class = "nota", p$detalle %||% "")),
      if (!is.na(pct)) shiny::div(class = "progress", role = "progressbar", `aria-valuenow` = pct,
                                  `aria-valuemin` = 0, `aria-valuemax` = 100,
                                  shiny::div(class = "progress-bar", style = sprintf("width:%d%%", pct), paste0(pct, " %"))),
      shiny::div(class = "nota", sprintf("En marcha desde hace %s · último avance %s",
                                         sub("^hace ", "", fmt_hace(p$inicio)), fmt_hace(p$hora))),
      if (quieto > 10) shiny::div(class = "alert alert-warning mt-2 mb-0",
                                  "Lleva más de 10 minutos sin avanzar en este paso. Si no cambia, deténgala y revise el registro."),
      if (!is.null(id_detener)) shiny::actionButton(id_detener, "Detener", icon = bsicons::bs_icon("stop-circle"),
                                                    class = "btn-sm btn-outline-danger mt-2")
    )
  } else if (corriendo) {
    shiny::div(
      class = "progreso",
      shiny::div(class = "progreso-paso", shiny::span(class = "spinner-border spinner-border-sm"),
                 shiny::strong("En curso"), shiny::span(class = "nota", sprintf("proceso %s, sin detalle de avance", pid_candado(carpeta)))),
      if (!is.null(id_detener)) shiny::actionButton(id_detener, "Detener", icon = bsicons::bs_icon("stop-circle"),
                                                    class = "btn-sm btn-outline-danger mt-2")
    )
  } else if (!is.null(p) && p$paso %in% c("Terminada", "Sin novedades")) {
    shiny::div(class = "nota", "Última actualización: ", tolower(p$paso), " · ", fmt_hace(p$hora))
  } else if (!is.null(p) && grepl("install\\.R|is required|is already loaded|no hay paquete|there is no package",
                                  p$detalle %||% "")) {
    shiny::div(class = "alert alert-warning mt-2",
               shiny::strong("Hay que actualizar paquetes de R. "),
               "Cierre RStudio por completo, haga doble clic en ", shiny::code("reparar_paquetes.bat"),
               " (carpeta del proyecto) y espere «Listo»; luego abra la app y vuelva a lanzar la actualización.",
               shiny::div(class = "nota mt-1", p$detalle))
  } else if (!is.null(p)) {
    shiny::div(class = "alert alert-warning mt-2",
               shiny::strong("La última actualización no terminó. "),
               sprintf("Se quedó en «%s%s» %s. Vuelva a lanzarla; si se repite, envíe el registro.",
                       p$paso, if (nzchar(p$detalle %||% "")) paste(" ·", p$detalle) else "", fmt_hace(p$hora)))
  }
  shiny::tagList(cabecera,
                 if (length(log)) shiny::tags$pre(class = "registro", paste(log, collapse = "\n")))
}

#' Estado del proceso de actualización que solo cambia cuando hay algo nuevo:
#' archivo PROGRESO, registro, candado o base publicada (y cada 30 s mientras corre,
#' para el «hace X min»). Evita redibujar el panel cada 2 s (eso lo hacía parpadear).
vigilar_actualizacion <- function(cfg, session) {
  carpeta <- cfg$rutas$db
  huella <- function() {
    corriendo <- actualizacion_en_curso(carpeta)
    paste(file.mtime(file.path(carpeta, "PROGRESO")), file.mtime(cfg$rutas$log),
          file.mtime(file.path(carpeta, "ACTUAL")), corriendo,
          if (corriendo) floor(as.numeric(Sys.time()) / 30))
  }
  shiny::reactivePoll(2000, session, huella, function() list(corriendo = actualizacion_en_curso(carpeta)))
}
