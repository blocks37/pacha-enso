# Ríos y quebradas (ANA) ----------------------------------------------------------
#
# data/geo/rios_distrito.csv viene del shapefile «Ríos y quebradas» de la ANA
# (1 225 tramos categorizados): para cada distrito, qué ríos y quebradas lo
# cruzan, cuántos km y de qué cuenca son. Se usa para saber qué distritos
# afecta un río que el ENFEN reporta con caudal sobre lo normal.

#' Clave comparable de un nombre: sin tildes, minúsculas, solo letras.
#' «Chancay - Lambayeque» y «Chancay-Lambayeque» dan la misma clave.
clave_rio <- function(x) {
  x <- stringi::stri_trans_general(as.character(unlist(x)), "Latin-ASCII")
  gsub("[^a-z]", "", tolower(x))
}

#' Resumen por distrito: río principal (el de más km), hasta 3 ríos para mostrar,
#' claves de río y de cuenca para cruzar con el ENFEN, y km de río.
resumen_rios <- function(ruta) {
  vacio <- tibble::tibble(ubigeo = character(), rio = character(), rios = character(),
                          claves_rio = character(), km_rio = numeric())
  if (!file.exists(ruta)) return(vacio)
  r <- readr::read_csv(ruta, col_types = readr::cols(ubigeo = "c", km = "d", .default = "c"))
  if (nrow(r) == 0) return(vacio)
  r |>
    dplyr::arrange(ubigeo, tipo != "Río", dplyr::desc(km)) |>
    dplyr::group_by(ubigeo) |>
    dplyr::summarise(
      rio = rio[1],
      rios = paste(utils::head(unique(paste(ifelse(tipo == "Río", "río", "qda."), rio)), 3), collapse = ", "),
      claves_rio = paste(unique(c(clave_rio(rio[tipo == "Río"]), clave_rio(cuenca[tipo == "Río"]))), collapse = "|"),
      km_rio = round(sum(km[tipo == "Río"]), 1),
      .groups = "drop"
    )
}

#' 1 si algún río (o la cuenca de un río) del distrito figura entre los que el
#' ENFEN reporta con caudal sobre lo normal.
cauce_en_alerta <- function(claves_rio, claves_enfen) {
  claves_enfen <- unique(claves_enfen[nzchar(claves_enfen)])
  if (length(claves_enfen) == 0) return(rep(0, length(claves_rio)))
  vapply(strsplit(dplyr::coalesce(claves_rio, ""), "|", fixed = TRUE),
         function(k) as.numeric(any(k %in% claves_enfen)), numeric(1))
}

#' Ríos para dibujar en el mapa (líneas simplificadas).
cargar_rios <- function(cfg) {
  ruta <- file.path(cfg$rutas$geo, "rios.geojson")
  if (file.exists(ruta)) sf::read_sf(ruta, quiet = TRUE) else NULL
}
