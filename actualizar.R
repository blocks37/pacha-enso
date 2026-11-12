# Actualiza los datos de ENSO Perú.
#
#   Desde RStudio (con el proyecto abierto):   source("actualizar.R")
#   Desde la consola de Windows:               Rscript actualizar.R
#   Recalcular todo aunque no haya novedades:  Rscript actualizar.R --forzar
#
# La primera vez tarda (descarga CHIRPS, consulta altitudes y calcula el SPI de
# 1 891 distritos). Después solo baja lo nuevo y suele tomar 2–5 minutos.

local({
  # Si se ejecuta con Rscript o desde el Programador de tareas, trabajar en la carpeta del script.
  archivo <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(archivo) == 1) setwd(dirname(normalizePath(archivo)))
})

invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source, encoding = "UTF-8"))

cfg <- cargar_config()
forzar <- "--forzar" %in% commandArgs(TRUE) || identical(Sys.getenv("ENSO_FORZAR"), "1")

resultado <- tryCatch(
  actualizar_todo(cfg, forzar = forzar),
  error = function(e) {
    options(enso.log = cfg$rutas$log)
    log_error("La actualización se detuvo: ", conditionMessage(e))
    avanzar("Error", conditionMessage(e))
    unlink(file.path(cfg$rutas$db, ".actualizando"))
    if (!interactive()) quit(status = 1)
  }
)
