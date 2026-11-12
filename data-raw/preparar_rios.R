# Regenera data/geo/rios_distrito.csv y data/geo/rios.geojson desde el shapefile
# «Ríos y quebradas» de la ANA. Solo hace falta si cambia el shapefile o los límites.
#
#   invisible(lapply(list.files("R", full.names = TRUE), source, encoding = "UTF-8"))
#   source("data-raw/preparar_rios.R")
#   preparar_rios("data/raw/rios/Rios_quebradas_ANA_geogpsperu_SuyoPomalia.shp",
#                 "data/raw/limites/Limite Distrital INEI 2025 CPV.shp")

preparar_rios <- function(shp_rios, shp_distritos, cfg = cargar_config(), tolerancia_m = 200) {
  sf::sf_use_s2(FALSE)
  quitar <- function(x, prefijos) {
    x <- stringr::str_squish(x)
    for (p in prefijos) x <- sub(paste0("^", p, "\\s+"), "", x, ignore.case = TRUE)
    x
  }
  rios <- sf::read_sf(shp_rios) |>
    sf::st_transform(4326) |>
    dplyr::transmute(rio = quitar(NOMBRE_CA, c("Río", "Rio", "Quebrada")), tipo = TIPO_CA,
                     cuenca = quitar(NOMBRE_UH, c("Cuenca", "Intercuenca")), codigo_uh = CODIGO_UH)
  dist <- sf::read_sf(shp_distritos) |> sf::st_transform(4326) |> dplyr::select(ubigeo = UBIGEO)

  # Tramos de río dentro de cada distrito y su largo en km (UTM 18S)
  tramos <- sf::st_intersection(rios, dist)
  tramos$km <- as.numeric(sf::st_length(sf::st_transform(tramos, 32718))) / 1000
  tabla <- tramos |>
    sf::st_drop_geometry() |>
    dplyr::group_by(ubigeo, rio, tipo, cuenca, codigo_uh) |>
    dplyr::summarise(km = round(sum(km), 2), .groups = "drop") |>
    dplyr::filter(km >= 0.2) |>
    dplyr::arrange(ubigeo, dplyr::desc(km))
  readr::write_csv(tabla, file.path(cfg$rutas$geo, "rios_distrito.csv"))

  simples <- sf::st_transform(rios, 32718) |>
    sf::st_simplify(dTolerance = tolerancia_m) |>
    sf::st_transform(4326) |>
    dplyr::select(nombre = rio, tipo)
  sf::write_sf(simples, file.path(cfg$rutas$geo, "rios.geojson"), delete_dsn = TRUE)
  message(dplyr::n_distinct(tabla$ubigeo), " distritos con ríos o quebradas.")
  invisible(tabla)
}
