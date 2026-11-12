# Regenera las tablas fijas de distritos a partir de los límites del INEI.
# Solo hace falta si cambian los límites (el proyecto ya trae las del INEI 2025)
# o si PISCO cambia de grilla.
#
#   invisible(lapply(list.files("R", full.names = TRUE), source, encoding = "UTF-8"))
#   source("data-raw/preparar_limites.R")
#   preparar_limites(
#     distritos = "data/raw/limites/Limite Distrital INEI 2025 CPV.shp",
#     departamentos = "data/raw/limites/Departamental INEI 2023 geogpsperu SuyoPomalia.shp",
#     provincias = "data/raw/limites/Provincial INEI 2023 geogpsperu SuyoPomalia.shp"
#   )
#
# Escribe en data/geo/: dim_distrito.csv, pesos_pisco.csv y los GeoJSON simplificados.

TILDES <- c("Ancash" = "Áncash", "Apurimac" = "Apurímac", "Huanuco" = "Huánuco",
            "Junin" = "Junín", "San Martin" = "San Martín")

nombre_propio <- function(x) {
  x <- stringr::str_squish(stringr::str_to_title(x))
  x <- gsub("(?<= )(De|Del|La|Las|Los|Y|El)(?= )", "\\L\\1", x, perl = TRUE)
  dplyr::coalesce(unname(TILDES[x]), x)
}

preparar_limites <- function(distritos, departamentos, provincias, cfg = cargar_config(),
                             tolerancia_m = 300) {
  sf::sf_use_s2(FALSE)
  geo <- cfg$rutas$geo
  dist <- sf::read_sf(distritos) |>
    sf::st_transform(4326) |>
    dplyr::transmute(ubigeo = UBIGEO, distrito = nombre_propio(DISTRITO), provincia = nombre_propio(PROVINCIA),
                     departamento = nombre_propio(DEPARTAMEN), cod_dep = CCDD, cod_prov = paste0(CCDD, CCPP))

  punto <- sf::st_coordinates(sf::st_point_on_surface(dist))
  dist$lon <- round(punto[, 1], 4)
  dist$lat <- round(punto[, 2], 4)
  dist$area_km2 <- round(as.numeric(sf::st_area(sf::st_transform(dist, 32718))) / 1e6, 1)
  dist$macrorregion <- dplyr::case_when(
    dist$cod_dep %in% c("01", "06", "13", "14", "20", "24") ~ "norte",
    dist$cod_dep %in% c("03", "04", "05", "08", "09", "11", "17", "18", "21", "23") ~ "sur",
    dist$cod_dep %in% c("16", "22", "25") ~ "oriente",
    TRUE ~ "centro"
  )

  # Costero: en cada latitud entre Tacna y Tumbes, el distrito más al oeste toca el mar.
  latitudes <- seq(-18.40, -3.41, by = 0.01)
  lineas <- sf::st_sfc(lapply(latitudes, function(y) sf::st_linestring(rbind(c(-82, y), c(-68, y)))), crs = 4326)
  cortes <- sf::st_intersection(sf::st_sf(id = seq_along(lineas), geometry = lineas), dist[, "ubigeo"])
  cortes$xmin <- vapply(sf::st_geometry(cortes), function(g) sf::st_bbox(g)[["xmin"]], numeric(1))
  costeros <- cortes |> sf::st_drop_geometry() |> dplyr::group_by(id) |>
    dplyr::slice_min(xmin, n = 1) |> dplyr::filter(xmin < -70.3) |> dplyr::pull(ubigeo) |> unique()
  dist$costero <- dist$ubigeo %in% costeros

  # Pesos de la grilla PISCO: fracción del distrito en cada celda de 0,1°
  r <- leer_raster_nc(cfg$pisco$mensual)[[1]]
  ex <- terra::extract(r, terra::vect(dist), cells = TRUE, exact = TRUE, ID = TRUE)
  names(ex)[2] <- "valor"
  ex$ubigeo <- dist$ubigeo[ex$ID]
  xy <- terra::xyFromCell(r, ex$cell)
  pesos <- tibble::tibble(ubigeo = ex$ubigeo, lon = round(xy[, 1], 2), lat = round(xy[, 2], 2),
                          fraccion = ex$fraction, valido = !is.na(ex$valor)) |>
    dplyr::filter(valido) |>
    dplyr::group_by(ubigeo) |>
    dplyr::mutate(peso = round(fraccion / sum(fraccion), 5)) |>
    dplyr::ungroup() |>
    dplyr::select(ubigeo, lon, lat, peso)
  faltan <- setdiff(dist$ubigeo, pesos$ubigeo)
  if (length(faltan)) warning("Distritos sin celda PISCO válida: ", paste(faltan, collapse = ", "))

  simplificar <- function(x) {
    sf::st_transform(x, 32718) |> sf::st_simplify(dTolerance = tolerancia_m, preserveTopology = TRUE) |>
      sf::st_transform(4326)
  }
  readr::write_csv(sf::st_drop_geometry(dist), file.path(geo, "dim_distrito.csv"))
  readr::write_csv(pesos, file.path(geo, "pesos_pisco.csv"))
  sf::write_sf(simplificar(dist[, c("ubigeo", "distrito")]), file.path(geo, "distritos.geojson"), delete_dsn = TRUE)
  dep <- sf::read_sf(departamentos) |> sf::st_transform(4326) |>
    dplyr::transmute(cod_dep = CCDD, departamento = nombre_propio(DEPARTAMEN))
  sf::write_sf(simplificar(dep), file.path(geo, "departamentos.geojson"), delete_dsn = TRUE)
  prov <- sf::read_sf(provincias) |> sf::st_transform(4326) |>
    dplyr::transmute(cod_prov = paste0(CCDD, CCPP), provincia = nombre_propio(PROVINCIA))
  sf::write_sf(simplificar(prov), file.path(geo, "provincias.geojson"), delete_dsn = TRUE)
  invisible(dist)
}
