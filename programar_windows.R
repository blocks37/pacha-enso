# Programa la actualización automática en Windows (Programador de tareas).
# Ejecútelo una sola vez con el proyecto abierto:  source("programar_windows.R")
#
# Crea la tarea «ENSO_Peru_actualizar», que corre actualizar.R cada 6 horas mientras
# la computadora esté encendida. Para quitarla:
#   taskscheduleR::taskscheduler_delete("ENSO_Peru_actualizar")

if (.Platform$OS.type != "windows") stop("Este script es para Windows. En Linux/Mac use cron (ver README).")
if (!requireNamespace("taskscheduleR", quietly = TRUE)) install.packages("taskscheduleR")

cfg <- config::get(file = "config.yml")
tarea <- "ENSO_Peru_actualizar"
script <- normalizePath("actualizar.R")

existentes <- taskscheduleR::taskscheduler_ls()
if (tarea %in% existentes$TaskName) taskscheduleR::taskscheduler_delete(tarea)

taskscheduleR::taskscheduler_create(
  taskname = tarea,
  rscript = script,
  schedule = "HOURLY",
  modifier = cfg$actualizacion$cada_horas,               # cada 6 horas
  starttime = format(Sys.time() + 120, "%H:%M")           # la primera, en 2 minutos
)

message("Listo: la tarea «", tarea, "» correrá cada ", cfg$actualizacion$cada_horas,
        " horas. Su registro queda en ", sub("\\.R$", ".log", script),
        " y en ", cfg$rutas$log, ".")
