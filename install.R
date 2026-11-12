# Instala las dependencias que falten y actualiza las que quedaron viejas.
#
# Forma más segura en Windows: cierre RStudio por completo y haga doble clic en
# reparar_paquetes.bat (corre este mismo script sin ningún otro R abierto).
# Desde RStudio: Session › Restart R (Ctrl+Shift+F10) y luego source("install.R").
#
# Por qué: Windows no deja reemplazar un paquete cuya DLL está en uso por algún R
# abierto (la app, otra sesión, una tarea en segundo plano). Si eso pasa, R deja la
# versión vieja y otros paquetes ya actualizados no cargan (p. ej. vctrs 0.7.3 pide
# rlang >= 1.1.7 y quedó rlang 1.1.6 → «package tibble not available»).
#
# Para fijar versiones después: renv::init()

paquetes <- c(
  # App
  "shiny", "bslib", "bsicons", "leaflet", "echarts4r", "reactable", "htmltools", "htmlwidgets",
  # Datos
  "DBI", "duckdb", "dplyr", "tidyr", "tibble", "purrr", "readr", "stringr", "stringi", "jsonlite", "rlang",
  # Fuentes y actualización en segundo plano
  "httr2", "xml2", "rvest", "pdftools", "R.utils", "callr", "ps",
  # Espacial
  "sf", "terra",
  # Clima y estadística
  "SPEI", "trend",
  # Calidad y configuración
  "pointblank", "config", "testthat"
)
windows <- .Platform$OS.type == "windows"
if (windows) paquetes <- c(paquetes, "taskscheduleR")

repos <- getOption("repos")
if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) repos <- c(CRAN = "https://cloud.r-project.org")

#' Paquetes instalados con una versión menor a la que exige otro paquete de la
#' lista o de sus dependencias. install.packages() no los actualiza por su cuenta.
versiones_insuficientes <- function(paquetes) {
  ip <- utils::installed.packages(fields = c("Depends", "Imports", "LinkingTo"), noCache = TRUE)
  ip <- ip[!duplicated(ip[, "Package"]), , drop = FALSE]   # el primero de .libPaths() es el que se carga
  rownames(ip) <- ip[, "Package"]
  usados <- unique(c(paquetes, unlist(tools::package_dependencies(
    intersect(paquetes, rownames(ip)), db = ip, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE))))
  usados <- intersect(usados, rownames(ip))
  req <- gsub("\\s+", " ", paste(ip[usados, "Depends"], ip[usados, "Imports"], ip[usados, "LinkingTo"], sep = ","))
  req <- unlist(regmatches(req, gregexpr("[A-Za-z][A-Za-z0-9.]*\\s*\\(\\s*>=\\s*[0-9.-]+\\s*\\)", req)))
  if (length(req) == 0) return(character())
  pkg <- trimws(sub("\\(.*", "", req))
  minimo <- sub(".*>=\\s*([0-9.-]+).*", "\\1", req)
  ok <- pkg %in% rownames(ip)
  bajo <- package_version(ip[pkg[ok], "Version"]) < package_version(minimo[ok])
  sort(unique(pkg[ok][bajo]))
}

#' Otros procesos de R abiertos en Windows (pueden tener DLLs de paquetes en uso).
otros_r <- function() {
  if (!windows) return(character())
  t <- tryCatch(suppressWarnings(system2("tasklist", c("/FO", "CSV", "/NH"), stdout = TRUE)), error = function(e) character())
  if (length(t) == 0) return(character())
  campos <- strsplit(gsub('"', "", t), ",")
  nombre <- vapply(campos, function(x) x[1], "")
  pid <- suppressWarnings(as.integer(vapply(campos, function(x) x[2], "")))
  es_r <- tolower(nombre) %in% c("rterm.exe", "rgui.exe", "rsession.exe", "rsession-utf8.exe")
  sprintf("%s (PID %d)", nombre, pid)[es_r & !is.na(pid) & pid != Sys.getpid()]
}

#' Carga los paquetes en un R nuevo y devuelve los errores (vacío si todo carga).
probar_carga <- function(paquetes) {
  script <- tempfile(fileext = ".R")
  writeLines(c(
    sprintf("paquetes <- c(%s)", paste0('"', paquetes, '"', collapse = ", ")),
    "for (p in paquetes) {",
    "  m <- tryCatch({ loadNamespace(p); '' }, error = function(e) conditionMessage(e))",
    "  if (nzchar(m)) cat('ERROR ', p, ': ', gsub('\\\\s+', ' ', m), '\\n', sep = '')",
    "}"
  ), script)
  rscript <- file.path(R.home("bin"), if (windows) "Rscript.exe" else "Rscript")
  salida <- suppressWarnings(system2(rscript, shQuote(script), stdout = TRUE, stderr = TRUE))
  grep("^ERROR ", salida, value = TRUE)
}

faltan <- setdiff(paquetes, rownames(utils::installed.packages(noCache = TRUE)))
viejos <- versiones_insuficientes(paquetes)
pendientes <- unique(c(faltan, viejos))

if (length(pendientes) == 0) {
  message("Todo instalado y con versiones compatibles.")
} else {
  if (length(faltan)) message("Faltan: ", paste(faltan, collapse = ", "))
  if (length(viejos)) message("Versión vieja (otro paquete pide una más nueva): ", paste(viejos, collapse = ", "))
  if (windows) {
    cargados <- intersect(pendientes, loadedNamespaces())
    if (length(cargados)) {
      stop("Estos paquetes están cargados en esta sesión y no se pueden reemplazar: ", paste(cargados, collapse = ", "),
           ". Cierre RStudio y haga doble clic en reparar_paquetes.bat.", call. = FALSE)
    }
    otros <- otros_r()
    if (length(otros)) {
      stop("Hay otros R abiertos que pueden bloquear la actualización: ", paste(otros, collapse = ", "),
           ". Ciérrelos (la app, otras ventanas de RStudio) o reinicie el computador, y vuelva a intentar.",
           call. = FALSE)
    }
  }
  utils::install.packages(pendientes, repos = repos)
}

# Comprobación final en un R limpio: ¿cargan todos?
message("\nComprobando que todo cargue…")
errores <- probar_carga(unique(c("rlang", "vctrs", "tibble", paquetes)))
quedan <- versiones_insuficientes(paquetes)
if (length(errores) || length(quedan)) {
  if (length(quedan)) message("Siguen con versión vieja: ", paste(quedan, collapse = ", "))
  if (length(errores)) message(paste(errores, collapse = "\n"))
  message("\nNo quedó bien. Cierre RStudio por completo (y la app si está abierta), haga doble clic en ",
          "reparar_paquetes.bat y espere el mensaje «Listo».")
} else {
  message("Listo: todos los paquetes cargan. Ya puede abrir la app con shiny::runApp().")
}
