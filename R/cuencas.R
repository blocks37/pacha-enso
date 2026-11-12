# Cuencas (Unidades Hidrográficas de la ANA) -------------------------------------------
#
# Un río crece por la lluvia de TODA su cuenca aguas arriba, no solo por la que cae
# en el distrito. Para cada río que cruza un distrito se calcula la lluvia
# pronosticada promedio de su cuenca (y, en el Amazonas, de todas las unidades
# aguas arriba, con la numeración Pfafstetter) y se compara con lo normal.
#
# data/geo/cuencas.csv            codigo, nombre, region, area_km2, aguas_arriba (códigos separados por |)
# data/geo/cuencas_distrito.csv   ubigeo, codigo, fraccion (parte del distrito dentro de la unidad)
# data/geo/rios_distrito.csv      ... codigo_uh: unidad hidrográfica de cada río

leer_cuencas <- function(cfg) {
  geo <- cfg$rutas$geo
  if (!file.exists(file.path(geo, "cuencas.csv"))) return(NULL)
  list(
    unidades = readr::read_csv(file.path(geo, "cuencas.csv"), col_types = readr::cols(area_km2 = "d", .default = "c")),
    distritos = readr::read_csv(file.path(geo, "cuencas_distrito.csv"),
                                col_types = readr::cols(fraccion = "d", .default = "c"))
  )
}

#' Cuenca principal de cada distrito (la que contiene la mayor parte de su área).
cuenca_principal <- function(cuencas) {
  if (is.null(cuencas)) return(tibble::tibble(ubigeo = character(), codigo_cuenca = character(), cuenca = character()))
  cuencas$distritos |>
    dplyr::group_by(ubigeo) |>
    dplyr::slice_max(fraccion, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::left_join(dplyr::select(cuencas$unidades, codigo, nombre), by = "codigo") |>
    dplyr::transmute(ubigeo, codigo_cuenca = codigo, cuenca = nombre)
}

#' Peso de cada distrito en la cuenca de cada río: área del distrito × fracción
#' dentro de la unidad del río o de sus unidades aguas arriba.
miembros_cuenca <- function(cuencas, distritos, codigos) {
  conjuntos <- cuencas$unidades |>
    dplyr::filter(codigo %in% codigos) |>
    dplyr::select(codigo_rio = codigo, aguas_arriba) |>
    tidyr::separate_rows(aguas_arriba, sep = "\\|") |>
    dplyr::rename(codigo = aguas_arriba)
  conjuntos |>
    dplyr::inner_join(cuencas$distritos, by = "codigo", relationship = "many-to-many") |>
    dplyr::inner_join(dplyr::select(distritos, ubigeo, area_km2), by = "ubigeo") |>
    dplyr::group_by(codigo_rio, ubigeo) |>
    dplyr::summarise(peso = sum(area_km2 * fraccion), .groups = "drop")
}

#' Factor de reducción areal: el promedio de una cuenca grande casi nunca alcanza
#' los extremos de un punto. 1 en cuencas pequeñas, ~0,7 en 10 000 km², mínimo 0,5.
reduccion_areal <- function(area_km2) acotar(1 - 0.1 * log10(pmax(area_km2, 10) / 10), 0.5, 1)

#' Índice de lluvia de cuenca (0–1) por río y día: promedio de 3 días de la lluvia
#' pronosticada en la cuenca, medido contra su clima (mediana y P99 ponderados).
lluvia_cuenca <- function(gfs, clima, miembros) {
  if (nrow(miembros) == 0) return(tibble::tibble(codigo_rio = character(), fecha = as.Date(character()),
                                                 cuenca_mm = numeric(), cuenca_idx = numeric()))
  umbrales <- miembros |>
    dplyr::inner_join(clima, by = "ubigeo") |>
    dplyr::group_by(codigo_rio) |>
    dplyr::summarise(area = sum(peso), p50 = sum(peso * p50_mm) / sum(peso), p99 = sum(peso * p99_mm) / sum(peso),
                     .groups = "drop") |>
    dplyr::mutate(arf = reduccion_areal(area), p50 = p50 * arf, p99 = p99 * arf)

  miembros |>
    dplyr::inner_join(dplyr::select(gfs, ubigeo, fecha, pp_mm), by = "ubigeo", relationship = "many-to-many") |>
    dplyr::group_by(codigo_rio, fecha) |>
    dplyr::summarise(mm = sum(peso * pp_mm, na.rm = TRUE) / sum(peso[!is.na(pp_mm)]), .groups = "drop") |>
    dplyr::arrange(codigo_rio, fecha) |>
    dplyr::group_by(codigo_rio) |>
    dplyr::mutate(cuenca_mm = (mm + dplyr::coalesce(dplyr::lag(mm), mm) + dplyr::coalesce(dplyr::lag(mm, 2), mm)) / 3) |>
    dplyr::ungroup() |>
    dplyr::inner_join(umbrales, by = "codigo_rio") |>
    dplyr::transmute(codigo_rio, fecha, cuenca_mm = round(cuenca_mm, 1),
                     cuenca_idx = round(normalizar_lluvia(cuenca_mm, p50, p99), 3),
                     p50 = round(p50, 1), p99 = round(p99, 1), area = round(area))
}

#' Para cada distrito y día, el índice de cuenca más alto entre los ríos que lo cruzan.
cuenca_por_distrito <- function(gfs, clima, distritos, cuencas, ruta_rios) {
  vacio <- tibble::tibble(ubigeo = character(), fecha = as.Date(character()), cuenca_mm = numeric(), cuenca_idx = numeric())
  if (is.null(cuencas) || !file.exists(ruta_rios)) return(vacio)
  rios <- readr::read_csv(ruta_rios, col_types = readr::cols(km = "d", .default = "c")) |>
    dplyr::filter(tipo == "Río", !is.na(codigo_uh)) |>
    dplyr::distinct(ubigeo, codigo_rio = codigo_uh)
  if (nrow(rios) == 0) return(vacio)
  miembros <- miembros_cuenca(cuencas, distritos, unique(rios$codigo_rio))
  lc <- lluvia_cuenca(gfs, clima, miembros)
  rios |>
    dplyr::inner_join(lc, by = "codigo_rio", relationship = "many-to-many") |>
    dplyr::group_by(ubigeo, fecha) |>
    dplyr::slice_max(cuenca_idx, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(ubigeo, fecha, cuenca_mm, cuenca_idx)
}

#' Distritos cuya mayor parte está dentro de una unidad (para las series de Explorar).
distritos_de_cuenca <- function(cuencas, codigo, minimo = 0.5) {
  cuencas$distritos |>
    dplyr::filter(.data$codigo == .env$codigo, fraccion >= minimo) |>
    dplyr::pull(ubigeo)
}
