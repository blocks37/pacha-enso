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

#' Mapa con polígonos grises listos para colorear. `capas` = lista de
#' list(sf, id, nombre, grupo); la primera se muestra, las otras quedan ocultas.
mapa_poligonos <- function(capas, rios = NULL, cuencas = NULL, contornos = NULL) {
  m <- mapa_base()
  for (i in seq_along(capas)) {
    capa <- capas[[i]]
    m <- leaflet::addPolygons(
      m, data = capa$sf, layerId = capa$sf[[capa$id]], group = capa$grupo,
      fillColor = "#E5E7EB", fillOpacity = 0.85, color = "#FFFFFF", weight = capa$borde %||% 0.5,
      label = lapply(capa$sf[[capa$nombre]], htmltools::htmlEscape),
      labelOptions = leaflet::labelOptions(sticky = TRUE, direction = "auto", className = "etiqueta-mapa"),
      highlightOptions = leaflet::highlightOptions(weight = 2, color = "#0F172A", bringToFront = TRUE)
    )
    if (i > 1) m <- leaflet::hideGroup(m, capa$grupo)
  }
  if (!is.null(contornos)) {   # límites departamentales como referencia, sin relleno ni clic
    m <- leaflet::addPolygons(m, data = contornos, fill = FALSE, color = "#0B3A6F", weight = 1.1, opacity = 0.55,
                              options = leaflet::pathOptions(interactive = FALSE))
  }
  extras <- character()
  if (!is.null(cuencas)) {
    extras <- c(extras, "Cuencas (ANA)")
    m <- leaflet::addPolygons(m, data = cuencas, group = "Cuencas (ANA)", fill = FALSE, color = "#1E3A8A",
                              weight = 1.2, opacity = 0.6, dashArray = "4 3",
                              options = leaflet::pathOptions(interactive = FALSE)) |>
      leaflet::hideGroup("Cuencas (ANA)")
  }
  if (!is.null(rios)) {
    extras <- c(extras, "Ríos y quebradas (ANA)")
    m <- leaflet::addPolylines(m, data = rios, group = "Ríos y quebradas (ANA)", color = "#0284C7",
                               weight = ifelse(rios$tipo == "Río", 1.6, 0.8), opacity = 0.75,
                               options = leaflet::pathOptions(interactive = FALSE))
  }
  if (length(extras)) {
    m <- leaflet::addLayersControl(m, baseGroups = c("Claro", "Relieve", "Satélite"), overlayGroups = extras,
                                   options = leaflet::layersControlOptions(collapsed = TRUE))
  }
  htmlwidgets::onRender(m, "function(el) { ensoRegistrarMapa(el.id, this); }")
}

#' Cambia color, opacidad y etiqueta de los polígonos de un mapa ya dibujado.
#' Funciona aunque el mapa esté en una pestaña oculta (se aplica al mostrarse).
colorear_mapa <- function(session, id, ids, colores, etiquetas, opacidad = 0.85) {
  session$sendCustomMessage("colorear-mapa", list(
    id = session$ns(id), ids = as.list(ids), colores = as.list(colores),
    etiquetas = as.list(etiquetas), opacidad = as.list(rep_len(opacidad, length(ids)))
  ))
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

#' Focos de alerta: pocos puntos que laten, solo en distritos críticos. Se toman de
#' mayor a menor riesgo (y lluvia) saltando los que estén a menos de `separacion`
#' grados de un foco ya elegido, hasta `maximo`. Con 0 filas, borra los focos.
focos_mapa <- function(session, id, d, cfg, maximo = 8, separacion = 0.55) {
  if (!"valor" %in% names(d)) d$valor <- 0
  if (!"lluvia_mm" %in% names(d)) d$lluvia_mm <- 0
  d <- d[!is.na(d$nivel) & d$nivel == "Crítico" & !is.na(d$lat), , drop = FALSE]
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
    color = as.list(rep(cfg$colores$nino, nrow(d))), etiqueta = as.list(d$etiqueta),
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
