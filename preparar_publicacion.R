# Prepara el proyecto para publicarlo en la web: GitHub (datos) + Posit Connect Cloud (app).
# Ejecútelo con el proyecto abierto en RStudio:   source("preparar_publicacion.R")
# Vuelva a ejecutarlo cada vez que instale o actualice paquetes de R.
#
# Hace cuatro cosas:
#   1. Anota en config.yml el repositorio de GitHub de donde la app baja los datos.
#   2. Copia PISCO mensual a insumos/ (la tarea de GitHub lo necesita para la sequía).
#   3. Copia la tarea automática (plantillas/actualizar-datos.yml) a .github/workflows/.
#   4. Escribe manifest.json: la lista de archivos y paquetes que Connect Cloud instala.

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

# 1. Repositorio ----------------------------------------------------------------------
cfg_txt <- readLines("config.yml", encoding = "UTF-8", warn = FALSE)
linea <- grep('^    repo: "', cfg_txt)[1]
actual <- sub('^    repo: "([^"]*)".*', "\\1", cfg_txt[linea])
repo <- actual
if (!nzchar(repo) || interactive()) {
  r <- trimws(readline(sprintf("Repositorio de GitHub (usuario/nombre)%s: ",
                               if (nzchar(actual)) paste0(" [", actual, "]") else "")))
  if (nzchar(r)) repo <- sub("^https?://github.com/", "", sub("\\.git$", "", sub("/+$", "", r)))
}
if (!grepl("^[^/ ]+/[^/ ]+$", repo)) stop("Escriba el repositorio como usuario/nombre, p. ej. juanperez/pacha-enso")
cfg_txt[linea] <- sub('repo: "[^"]*"', sprintf('repo: "%s"', repo), cfg_txt[linea])
writeLines(cfg_txt, "config.yml", useBytes = TRUE)
message("✓ config.yml › datos_remotos › repo = ", repo)

# 2. PISCO mensual para la tarea de GitHub --------------------------------------------
dir.create("insumos", showWarnings = FALSE)
if (file.exists("data/raw/PISCOp_m.nc")) {
  file.copy("data/raw/PISCOp_m.nc", "insumos/PISCOp_m.nc", overwrite = TRUE)
  message("✓ insumos/PISCOp_m.nc (", round(file.size("insumos/PISCOp_m.nc") / 1e6), " MB)")
} else {
  warning("No se encontró data/raw/PISCOp_m.nc: la tarea de GitHub no podrá calcular la sequía.")
}

# 3. Tarea automática de GitHub -----------------------------------------------------------
# La tarea vive en .github/workflows/; aquí se copia desde plantillas/ (revísela si quiere).
dir.create(".github/workflows", showWarnings = FALSE, recursive = TRUE)
file.copy("plantillas/actualizar-datos.yml", ".github/workflows/pipeline.yml", overwrite = TRUE)
message("✓ .github/workflows/pipeline.yml (actualización cada 6 h en GitHub)")

# 4. manifest.json para Connect Cloud ---------------------------------------------------
# Solo lo que la app necesita para mostrarse (los datos los baja de GitHub al arrancar).
archivos <- c(
  "app.R", "config.yml",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("www", full.names = TRUE, recursive = TRUE),
  setdiff(list.files("data/geo", full.names = TRUE), "data/geo/dsc_mensual_pisco.csv")
)
rsconnect::writeManifest(appDir = ".", appFiles = archivos, appPrimaryDoc = "app.R")
message("✓ manifest.json (", length(archivos), " archivos)")

message("\nListo. Ahora suba los cambios a GitHub (GitHub Desktop › Commit › Push) y siga el README,",
        " sección «Publicar en la web: GitHub + Connect Cloud».")
