# Mapas -----------------------------------------------------------------------------
# Los polígonos se envían al navegador una sola vez. Después, cambiar de mes, de
# amenaza o de región solo cambia colores y etiquetas (www/app.js), así el mapa
# responde al instante y no se reinicia.

#' Lee los límites simplificados (GeoJSON) una vez por proceso.
cargar_geo <- function(cfg) {
  leer <- function(nombre) sf::read_sf(file.path(cfg$rutas$geo, paste0(nombre, ".geojson")), quiet = TRUE)
  cuencas_sf <- if (file.exists(file.path(cfg$rutas$geo, "cuencas.geojson"))) leer("cuencas") else NULL
  list(distritos = leer("distritos"), departamentos = leer("departamentos"), rios = cargar_rios(cfg),
       cuencas_sf = cuencas_sf, cuencas = leer_cuencas(cfg))
}

PERU_LIMITES <- c(-81.4, -18.4, -68.6, -0.03)   # oeste, sur, este, norte

mapa_base <- function() {
  leaflet::leaflet(options = leaflet::leafletOptions(
    preferCanvas = TRUE, zoomSnap = 0.25, minZoom = 4,
    # No se puede salir mucho del Perú: siempre queda a la vista
    maxBounds = list(c(-26, -92), c(6, -58)), maxBoundsViscosity = 0.8
  )) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas, group = "Claro") |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Relieve") |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satélite") |>
    leaflet::addLayersControl(baseGroups = c("Claro", "Relieve", "Satélite"),
                              options = leaflet::layersControlOptions(collapsed = TRUE)) |>
    leaflet::fitBounds(PERU_LIMITES[1], PERU_LIMITES[2], PERU_LIMITES[3], PERU_LIMITES[4])
}

# Geometrías para el navegador --------------------------------------------------------------
# Los polígonos NO viajan por la conexión de Shiny: el navegador los descarga como
# archivos estáticos (www/geo/*.geojson, en caché del navegador) y www/app.js arma las
# capas. Así el mapa abre rápido y no se corta en servidores que limitan el tamaño de
# los mensajes (Connect Cloud). Los archivos son copia de data/geo/.
GEO_WEB <- c(distritos = "geo/distritos.geojson", cuencas = "geo/cuencas.geojson",
             rios = "geo/rios.geojson", departamentos = "geo/departamentos.geojson")

url_geo <- function(nombre) {
  ruta <- GEO_WEB[[nombre]]
  local <- file.path("www", ruta)
  paste0(ruta, "?v=", if (file.exists(local)) as.integer(file.mtime(local)) else 0)
}

#' Mapa con polígonos grises listos para colorear. `capas` = lista de
#' list(geo = "distritos" | "cuencas", id, nombre, grupo, borde); la primera se
#' muestra, las otras quedan ocultas. `rios`, `cuencas` y `contornos` (TRUE/FALSE o
#' cualquier objeto no nulo) agregan esas capas de referencia.
mapa_poligonos <- function(capas, rios = NULL, cuencas = NULL, contornos = NULL) {
  m <- mapa_base()
  geo <- list()
  for (i in seq_along(capas)) {
    capa <- capas[[i]]
    geo[[length(geo) + 1]] <- list(tipo = "forma", url = url_geo(capa$geo), id = capa$id, nombre = capa$nombre,
                                   grupo = capa$grupo, peso = capa$borde %||% 0.5, oculto = i > 1)
  }
  if (!is.null(contornos) && !isFALSE(contornos)) {
    geo[[length(geo) + 1]] <- list(tipo = "contorno", url = url_geo("departamentos"))
  }
  extras <- character()
  if (!is.null(cuencas) && !isFALSE(cuencas)) {
    extras <- c(extras, "Cuencas (ANA)")
    geo[[length(geo) + 1]] <- list(tipo = "cuencas", url = url_geo("cuencas"), grupo = "Cuencas (ANA)")
    m <- leaflet::hideGroup(m, "Cuencas (ANA)")
  }
  if (!is.null(rios) && !isFALSE(rios)) {
    extras <- c(extras, "Ríos y quebradas (ANA)")
    geo[[length(geo) + 1]] <- list(tipo = "rios", url = url_geo("rios"), grupo = "Ríos y quebradas (ANA)")
  }
  if (length(extras)) {
    m <- leaflet::addLayersControl(m, baseGroups = c("Claro", "Relieve", "Satélite"), overlayGroups = extras,
                                   options = leaflet::layersControlOptions(collapsed = TRUE))
  }
  htmlwidgets::onRender(m, "function(el, x, datos) { ensoCapasGeo(el.id, this, datos); }",
                        data = list(capas = geo))
}

#' Cambia color, opacidad y etiqueta de los polígonos de un mapa ya dibujado.
#' Funciona aunque el mapa esté en una pestaña oculta (se aplica al mostrarse).
#' `bordes`: color del contorno por polígono ("" = contorno normal); se usa para
#' resaltar en rojo los distritos con riesgo crítico (≥ 85 %).
colorear_mapa <- function(session, id, ids, colores, etiquetas, opacidad = 0.85, bordes = NULL) {
  msg <- list(
    id = session$ns(id), ids = as.list(ids), colores = as.list(colores),
    etiquetas = as.list(etiquetas), opacidad = as.list(rep_len(opacidad, length(ids)))
  )
  if (!is.null(bordes)) msg$bordes <- as.list(dplyr::coalesce(rep_len(bordes, length(ids)), ""))
  session$sendCustomMessage("colorear-mapa", msg)
}

#' Color de alarma (rojo): solo para lo crítico.
color_alarma <- function(cfg) cfg$colores$alarma %||% "#E7000B"

#' Contorno rojo para los distritos con riesgo ≥ umbral crítico.
bordes_criticos <- function(riesgo, cfg, dentro = TRUE) {
  ifelse(dentro & dplyr::coalesce(riesgo, 0) >= cfg$riesgo$umbrales$critico, cfg$colores$borde_critico %||% "#7F1D1D", "")
}

#' Encuadra el mapa en un conjunto de distritos (o todo el Perú si está vacío).
#' El encuadre queda guardado en el navegador (www/app.js) y se vuelve a aplicar
#' cada vez que la pestaña se muestra, así el mapa nunca queda descentrado.
encuadrar_mapa <- function(proxy, distritos) {
  margen <- 0.15
  b <- if (nrow(distritos) == 0 || nrow(distritos) > 1500) PERU_LIMITES else
    c(min(distritos$lon) - margen, min(distritos$lat) - margen, max(distritos$lon) + margen, max(distritos$lat) + margen)
  proxy$session$sendCustomMessage("encuadrar-mapa", list(id = proxy$id, b = as.list(b)))
  invisible(proxy)
}

#' Muestra un grupo y oculta otro (p. ej. departamentos ↔ distritos).
mostrar_grupo <- function(proxy, mostrar, ocultar) {
  proxy |> leaflet::showGroup(mostrar) |> leaflet::hideGroup(ocultar)
}

#' Focos de alerta: los distritos críticos (≥ 85 %) ya llevan contorno rojo; los focos
#' que laten marcan solo los `maximo` PRINCIPALES (mayor riesgo y lluvia), saltando los
#' que estén a menos de `separacion` grados de uno ya elegido, para que se lean sobre el
#' mapa. Con 0 filas, borra los focos.
focos_mapa <- function(session, id, d, cfg, maximo = 12, separacion = 0.4) {
  if (!"valor" %in% names(d)) d$valor <- 0
  if (!"lluvia_mm" %in% names(d)) d$lluvia_mm <- 0
  d <- d[!is.na(d$valor) & d$valor >= cfg$riesgo$umbrales$critico & !is.na(d$lat), , drop = FALSE]
  d <- d[order(-d$valor, -dplyr::coalesce(d$lluvia_mm, 0)), , drop = FALSE]
  elegidos <- integer()
  for (i in seq_len(nrow(d))) {
    if (length(elegidos) >= maximo) break
    lejos <- all(sqrt((d$lat[i] - d$lat[elegidos])^2 + (d$lon[i] - d$lon[elegidos])^2) >= separacion)
    if (lejos) elegidos <- c(elegidos, i)
  }
  d <- d[elegidos, , drop = FALSE]
  session$sendCustomMessage("focos-mapa", list(
    id = session$ns(id), ubigeo = as.list(d$ubigeo), lat = as.list(d$lat), lon = as.list(d$lon),
    # Punto oscuro: se distingue sobre el rojo del nivel crítico
    color = as.list(rep("#171717", nrow(d))), etiqueta = as.list(d$etiqueta),
    critico = as.list(rep(TRUE, nrow(d)))
  ))
}

#' Mapa animado de la anomalía de temperatura del mar (NASA GIBS, GHRSST MUR 0,25°),
#' un cuadro por semana en las últimas `semanas`. La animación corre en el navegador
#' (www/app.js, ensoAnimarMar); aquí solo se arma el mapa y la lista de fechas.
mapa_mar <- function(cfg, semanas = 16) {
  ultima <- Sys.Date() - 3                                  # GIBS publica con 1–2 días de retraso
  fechas <- format(rev(seq(ultima, by = "-7 days", length.out = semanas)))
  capa <- cfg$mar$capa %||% "GHRSST_L4_MUR25_Sea_Surface_Temperature_Anomalies"
  nivel <- cfg$mar$nivel %||% "GoogleMapsCompatible_Level6"
  plantilla <- sprintf("https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/%s/default/{fecha}/%s/{z}/{y}/{x}.png", capa, nivel)
  cajas <- data.frame(
    nombre = c("Niño 1+2", "Niño 3.4"),
    oeste = c(-90, -170), este = c(-80, -120), sur = c(-10, -5), norte = c(0, 5)
  )
  leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2, maxZoom = 6, zoomSnap = 0.25,
                                                     worldCopyJump = FALSE)) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
    leaflet::addRectangles(lng1 = cajas$oeste, lat1 = cajas$sur, lng2 = cajas$este, lat2 = cajas$norte,
                           fill = FALSE, color = "#0B1F3A", weight = 1.5, dashArray = "5 4", opacity = 0.9,
                           label = cajas$nombre,
                           labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE,
                                                                className = "etiqueta-caja")) |>
    leaflet::fitBounds(-180, -25, -68, 15) |>
    htmlwidgets::onRender(
      "function (el, x, datos) { ensoAnimarMar(this, datos); }",
      data = list(fechas = as.list(fechas), plantilla = plantilla, intervalo = 900,
                  atribucion = "NASA GIBS · GHRSST MUR (JPL)")
    )
}
