# Regenera data/geo/cuencas.csv, cuencas_distrito.csv y cuencas.geojson desde las
# Unidades Hidrográficas de la ANA (archivo UH.shp). Solo si cambian los límites.
#
#   invisible(lapply(list.files("R", full.names = TRUE), source, encoding = "UTF-8"))
#   source("data-raw/preparar_cuencas.R")
#   preparar_cuencas("data/raw/cuencas/UH.shp", "data/raw/limites/Limite Distrital INEI 2025 CPV.shp")

#' Pfafstetter: B está aguas arriba de A si, en el primer dígito distinto k,
#' B[k] > A[k] y todos los dígitos de A desde k son impares (A está en el cauce principal).
aguas_arriba_de <- function(a, codigos) {
  es_arriba <- function(b) {
    if (identical(a, b)) return(TRUE)
    x <- strsplit(a, "")[[1]]; y <- strsplit(b, "")[[1]]; n <- min(length(x), length(y))
    k <- which(x[seq_len(n)] != y[seq_len(n)])[1]
    if (is.na(k)) return(startsWith(b, a))
    y[k] > x[k] && all(as.integer(x[k:length(x)]) %% 2 == 1)
  }
  # En el Pacífico y el Titicaca cada cuenca desagua por su cuenta: solo el Amazonas (4) forma una red.
  if (substr(a, 1, 1) != "4") return(a)
  candidatos <- codigos[substr(codigos, 1, 1) == "4"]
  candidatos[vapply(candidatos, es_arriba, logical(1))]
}

preparar_cuencas <- function(shp_uh, shp_distritos, cfg = cargar_config(), tolerancia_m = 400) {
  sf::sf_use_s2(FALSE)
  uh <- sf::read_sf(shp_uh, options = "ENCODING=LATIN1") |> sf::st_transform(4326) |>
    dplyr::transmute(codigo = CODIGO, nombre = stringr::str_squish(NOMBRE), area_km2 = round(AREA_KM2, 1),
                     region = dplyr::case_when(substr(CODIGO, 1, 1) == "1" ~ "Pacífico",
                                               substr(CODIGO, 1, 1) == "4" ~ "Amazonas", TRUE ~ "Titicaca"))
  dist <- sf::read_sf(shp_distritos) |> sf::st_transform(32718) |> dplyr::select(ubigeo = UBIGEO)
  dist$area <- as.numeric(sf::st_area(dist))

  cortes <- sf::st_intersection(dist, sf::st_transform(uh[, "codigo"], 32718))
  fr <- tibble::tibble(ubigeo = cortes$ubigeo, codigo = cortes$codigo,
                       fraccion = round(as.numeric(sf::st_area(cortes)) / cortes$area, 4)) |>
    dplyr::filter(fraccion >= 0.01)

  tabla <- sf::st_drop_geometry(uh) |>
    dplyr::mutate(aguas_arriba = vapply(codigo, function(a) paste(sort(aguas_arriba_de(a, codigo)), collapse = "|"), ""))
  readr::write_csv(tabla, file.path(cfg$rutas$geo, "cuencas.csv"))
  readr::write_csv(fr, file.path(cfg$rutas$geo, "cuencas_distrito.csv"))
  simples <- sf::st_transform(uh, 32718) |> sf::st_simplify(dTolerance = tolerancia_m, preserveTopology = TRUE) |>
    sf::st_transform(4326) |> dplyr::select(codigo, nombre)
  sf::write_sf(simples, file.path(cfg$rutas$geo, "cuencas.geojson"), delete_dsn = TRUE)
  invisible(tabla)
}
