# Publicar la base desde la PC del administrador -------------------------------------------
#
# La PC sí llega a SENAMHI (ENFEN) y al servidor de CHIRPS, que rechazan a los servidores
# de GitHub. Por eso la actualización principal corre en la PC (Programador de tareas,
# cada 6 h) y, si la base cambió, la sube a la release «datos» de GitHub. La web
# (Connect Cloud) la toma de ahí. La tarea de GitHub queda de respaldo: solo actualiza
# si la PC lleva más de un día sin publicar.
#
# Requiere GitHub CLI (gh) con la sesión iniciada una vez:  gh auth login

#' Ruta del programa gh (también lo busca donde lo deja el instalador de Windows).
ruta_gh <- function() {
  gh <- Sys.which("gh")
  if (nzchar(gh)) return(unname(gh))
  candidatos <- c(file.path(Sys.getenv("ProgramFiles"), "GitHub CLI", "gh.exe"),
                  file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "GitHub CLI", "gh.exe"))
  candidatos <- candidatos[file.exists(candidatos)]
  if (length(candidatos)) candidatos[1] else NA_character_
}

#' Nombre de la base vigente + parte de su md5 (la misma huella que usa la tarea de GitHub).
huella_base <- function(carpeta) {
  actual <- db_actual(carpeta)
  if (is.null(actual)) return(NA_character_)
  paste(basename(actual), substr(unname(tools::md5sum(actual)), 1, 12))
}

#' Sube la base a la release si cambió desde la última vez. Devuelve TRUE si subió.
publicar_base_github <- function(cfg) {
  pub <- cfg$publicar_github
  if (!isTRUE(pub$activo) || identical(Sys.getenv("GITHUB_ACTIONS"), "true")) return(invisible(FALSE))
  repo <- cfg$datos_remotos$repo %||% ""
  if (!nzchar(repo)) { log_aviso("Publicar en GitHub: falta datos_remotos › repo en config.yml"); return(invisible(FALSE)) }
  gh <- ruta_gh()
  if (is.na(gh)) {
    log_aviso("Publicar en GitHub: no se encontró GitHub CLI (gh). Instálelo y ejecute «gh auth login» una vez.")
    return(invisible(FALSE))
  }
  carpeta <- cfg$rutas$db
  huella <- huella_base(carpeta)
  marca <- file.path(carpeta, "PUBLICADO")
  if (is.na(huella)) return(invisible(FALSE))
  if (file.exists(marca) && identical(readLines(marca, n = 1, warn = FALSE), huella)) {
    log_info("Publicar en GitHub: la web ya tiene esta base.")
    return(invisible(FALSE))
  }

  avanzar("Publicando la base en GitHub", repo)
  temporal <- tempfile("pacha_publicar_")
  dir.create(temporal)
  on.exit(unlink(temporal, recursive = TRUE), add = TRUE)
  tgz <- file.path(temporal, "pacha_db.tgz")
  version <- file.path(temporal, "VERSION")
  archivos <- c("ACTUAL", basename(db_actual(carpeta)),
                intersect(c("METODO_SEQUIA", "ULTIMA_REVISION"), list.files(carpeta)))
  previo <- setwd(carpeta)
  ok_tar <- tryCatch({ utils::tar(tgz, files = archivos, compression = "gzip", tar = "internal"); TRUE },
                     error = function(e) { log_error("Publicar en GitHub: no se pudo empaquetar: ", conditionMessage(e)); FALSE })
  setwd(previo)
  if (!ok_tar) return(invisible(FALSE))
  writeLines(huella, version)

  subir <- function(archivo) {
    salida <- suppressWarnings(system2(gh, c("release", "upload", cfg$datos_remotos$etiqueta %||% "datos",
                                             shQuote(archivo), "--clobber", "-R", repo), stdout = TRUE, stderr = TRUE))
    estado <- attr(salida, "status") %||% 0
    if (estado != 0) log_error("Publicar en GitHub: ", paste(salida, collapse = " "))
    estado == 0
  }
  # Primero la base y después VERSION: la web nunca ve una versión sin su base
  if (subir(tgz) && subir(version)) {
    writeLines(huella, marca)
    log_info(sprintf("Publicar en GitHub: base subida (%s MB) · %s", round(file.size(tgz) / 1e6), huella))
    return(invisible(TRUE))
  }
  invisible(FALSE)
}
