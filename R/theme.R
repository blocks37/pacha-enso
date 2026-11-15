# Tema visual, paletas y componentes reutilizables -----------------------------

tema_app <- function(cfg) {
  col <- cfg$colores
  # Sistema sobrio (www/estilos.css): tinta casi negra, superficies blancas, bordes finos;
  # el color queda para los datos y el rojo solo para lo crítico.
  bslib::bs_theme(
    version = 5,
    bg = "#F5F5F5", fg = "#0A0A0A",
    primary = "#0A0A0A", secondary = "#F5F5F5", info = col$acento, warning = "#D97706",
    danger = col$alarma %||% "#E7000B", success = "#16A34A",
    base_font = bslib::font_collection(bslib::font_google("Geist", wght = c(400, 500, 600), local = FALSE),
                                       "Inter", "system-ui", "sans-serif"),
    heading_font = bslib::font_collection(bslib::font_google("Geist", wght = c(500, 600), local = FALSE),
                                          "Inter", "system-ui", "sans-serif"),
    code_font = bslib::font_collection(bslib::font_google("Geist Mono", local = FALSE), "monospace"),
    "font-size-base" = "0.875rem",
    "navbar-bg" = "#FFFFFF",
    "border-color" = "#E5E5E5",
    "border-radius" = "0.625rem",
    "card-border-radius" = "24px",
    "card-border-color" = "#E5E5E5",
    "card-cap-bg" = "transparent",
    "card-bg" = "#FFFFFF",
    "dropdown-bg" = "#FFFFFF",
    "modal-content-bg" = "#FFFFFF",
    "btn-border-radius" = "18px", "btn-border-radius-sm" = "18px",
    "input-border-radius" = "18px",
    "link-color" = "#0A0A0A",
    "secondary-color" = "#737373"
  )
}

# Colores por nivel -------------------------------------------------------------

NIVELES_RIESGO <- c("Bajo", "Vigilancia", "Huaico medio", "Alerta", "Alto", "Crítico")

color_nivel <- function(nivel, cfg) {
  inund <- unlist(cfg$colores$inundacion)
  paleta <- c("Vigilancia" = inund[1], "Huaico medio" = inund[2], "Alerta" = inund[2],
              "Alto" = inund[3], "Crítico" = inund[4], "Bajo" = cfg$colores$neutro)
  unname(dplyr::coalesce(paleta[nivel], cfg$colores$neutro))
}

color_sequia <- function(categoria, cfg) {
  s <- unlist(cfg$colores$sequia)
  paleta <- c("Anormalmente seco" = s[1], "Sequía moderada" = s[2], "Sequía severa" = s[3],
              "Sequía extrema" = s[4], "Normal o húmedo" = cfg$colores$neutro)
  paleta[CATEGORIA_ARIDO] <- cfg$colores$arido
  unname(dplyr::coalesce(paleta[categoria], cfg$colores$neutro))
}

#' Color continuo para SPI: ámbar si es seco, azul si es húmedo.
color_spi <- function(spi, cfg) {
  s <- unlist(cfg$colores$sequia)
  dplyr::case_when(
    is.na(spi) ~ cfg$colores$neutro,
    spi <= -2 ~ s[4], spi <= -1.5 ~ s[3], spi <= -1 ~ s[2], spi <= -0.5 ~ s[1],
    spi < 0.5 ~ "#EBEBEB", spi < 1 ~ "#DBEAFE", spi < 1.5 ~ "#93C5FD", TRUE ~ "#3B82F6"
  )
}

#' Texto legible sobre un color de fondo.
color_texto <- function(fondo) {
  rgb <- grDevices::col2rgb(fondo) / 255
  lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  ifelse(lum > 0.5, "#0A0A0A", "#FFFFFF")
}

# Componentes -------------------------------------------------------------------

#' Tarjeta compacta: título, valor grande, detalle y (opcional) una mini serie.
tarjeta <- function(titulo, valor, detalle = NULL, tono = "neutro", spark = NULL, pie = NULL) {
  shiny::div(
    class = paste("tarjeta", paste0("tono-", tono)),
    shiny::div(class = "tarjeta-titulo", titulo),
    shiny::div(class = "tarjeta-cuerpo",
               shiny::div(class = "tarjeta-valor", valor),
               if (!is.null(spark)) shiny::div(class = "tarjeta-spark", spark)),
    if (!is.null(detalle)) shiny::div(class = "tarjeta-detalle", detalle),
    pie
  )
}

fila_tarjetas <- function(...) shiny::div(class = "fila-tarjetas", ...)

#' Mini serie en SVG (sin JavaScript), con línea de cero y punto final.
sparkline <- function(y, color = "#1D4ED8", ancho = 120, alto = 36) {
  y <- y[is.finite(y)]
  if (length(y) < 2) return(NULL)
  r <- range(c(y, 0)); if (diff(r) == 0) r <- r + c(-1, 1)
  x <- seq(2, ancho - 4, length.out = length(y))
  py <- 3 + (alto - 6) * (1 - (y - r[1]) / diff(r))
  y0 <- 3 + (alto - 6) * (1 - (0 - r[1]) / diff(r))
  shiny::HTML(sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 %d %d" aria-hidden="true">
       <line x1="0" x2="%d" y1="%.1f" y2="%.1f" stroke="#E5E5E5" stroke-dasharray="3 3"/>
       <polyline points="%s" fill="none" stroke="%s" stroke-width="1.5" stroke-linejoin="round" stroke-linecap="round"/>
       <circle cx="%.1f" cy="%.1f" r="3" fill="%s"/></svg>',
    ancho, alto, ancho, alto, ancho, y0, y0,
    paste(sprintf("%.1f,%.1f", x, py), collapse = " "), color,
    utils::tail(x, 1), utils::tail(py, 1), color))
}

insignia <- function(texto, fondo) {
  shiny::span(class = "insignia", style = sprintf("background:%s;color:%s", fondo, color_texto(fondo)), texto)
}

# Celdas de reactable como texto HTML (colDef(html = TRUE)). Devolver etiquetas de
# htmltools desde `cell` obliga al navegador a «hidratarlas» con React, y un valor
# raro (vector con nombres, NA) rompe ahí con «reading '0'»: ese error aborta todo el
# lote de mensajes de Shiny y deja sin dibujar mapas, filtros y gráficos.
html_texto <- function(x) {
  x <- unname(as.character(x))
  x[is.na(x)] <- "—"
  htmltools::htmlEscape(x)
}
celda_insignia <- function(texto, fondo) {
  fondo <- rep_len(unname(as.character(fondo)), length(texto))
  fondo[is.na(fondo) | !nzchar(fondo)] <- "#EBEBEB"
  sprintf('<span class="insignia" style="background:%s;color:%s">%s</span>', fondo, color_texto(fondo), html_texto(texto))
}
celda_doble <- function(titulo, nota) {
  sprintf('<div class="celda-titulo">%s</div><div class="nota">%s</div>', html_texto(titulo), html_texto(nota))
}

punto_estado <- function(estado) {
  if (length(estado) == 0 || is.na(estado)) estado <- ""
  clase <- switch(estado, "ok" = "ok", "sin cambios" = "ok", "error" = "error", "pendiente")
  shiny::span(class = paste("punto", clase))
}

#' Grupo de botones tipo «píldora» (radio con estilo).
segmentado <- function(id, opciones, seleccion = opciones[[1]], etiqueta = NULL) {
  shiny::div(class = "segmentado",
             shiny::radioButtons(id, etiqueta, choices = opciones, selected = seleccion, inline = TRUE))
}

etiqueta_demo <- function(texto) if (!is.null(texto)) shiny::span(class = "etiqueta-aviso", texto)

leyenda_html <- function(colores, etiquetas, critico = FALSE) {
  shiny::div(class = "leyenda",
             purrr::map2(colores, etiquetas, function(c, e) {
               shiny::span(class = "leyenda-item", shiny::span(class = "leyenda-color", style = paste0("background:", c)), e)
             }),
             if (critico) shiny::span(class = "leyenda-item leyenda-critico",
                                      shiny::span(class = "leyenda-anillo"), "los puntos marcan los focos principales (≥ 85 %)"))
}

boton_csv <- function(id) {
  shiny::downloadButton(id, NULL, class = "btn-sm btn-light", icon = bsicons::bs_icon("download"),
                        title = "Descargar CSV")
}

csv_handler <- function(nombre, datos) {
  shiny::downloadHandler(
    filename = function() sprintf("%s_%s.csv", nombre, format(Sys.Date())),
    content = function(file) readr::write_excel_csv(datos(), file)
  )
}

#' Encabezado de cada vista: título fijo, línea de contexto (fecha, región) y un
#' resumen breve debajo, en lugar de una sola frase larga.
titulo_vista <- function(id_texto, titulo = NULL, icono = NULL, contexto_id = NULL) {
  shiny::div(
    class = "titulo-vista",
    shiny::div(
      class = "titulo-bloque",
      if (!is.null(titulo)) shiny::h1(if (!is.null(icono)) bsicons::bs_icon(icono), titulo,
                                      if (!is.null(contexto_id)) shiny::span(class = "titulo-contexto",
                                                                             shiny::textOutput(contexto_id, inline = TRUE))),
      shiny::p(class = "titulo-resumen", shiny::textOutput(id_texto, inline = TRUE))
    )
  )
}

vacio <- function(mensaje, icono = "inbox") {
  shiny::div(class = "estado-vacio", bsicons::bs_icon(icono, size = "2rem"), shiny::p(mensaje))
}

# Gráficos comunes ----------------------------------------------------------------

FUENTE_GRAFICOS <- "Geist, Inter, system-ui, sans-serif"

# Tema de ECharts: ejes y textos en gris, cuadrícula de líneas finas, leyenda discreta,
# tooltip blanco con borde fino. Se registra una vez por gráfico (e_estilo).
TEMA_ECHARTS <- jsonlite::toJSON(list(
  textStyle = list(fontFamily = FUENTE_GRAFICOS, color = "#0A0A0A"),
  legend = list(textStyle = list(color = "#737373", fontSize = 12), itemWidth = 12, itemHeight = 8, icon = "roundRect"),
  categoryAxis = list(axisLine = list(show = TRUE, lineStyle = list(color = "#E5E5E5")),
                      axisTick = list(show = FALSE), axisLabel = list(color = "#737373", fontSize = 11),
                      splitLine = list(show = FALSE), nameTextStyle = list(color = "#737373")),
  valueAxis = list(axisLine = list(show = FALSE), axisTick = list(show = FALSE),
                   axisLabel = list(color = "#737373", fontSize = 11),
                   splitLine = list(show = TRUE, lineStyle = list(color = "#F0F0F0")),
                   nameTextStyle = list(color = "#737373", fontSize = 11)),
  tooltip = list(backgroundColor = "#FFFFFF", borderColor = "#E5E5E5", borderWidth = 1,
                 textStyle = list(color = "#0A0A0A", fontSize = 12), extraCssText = "border-radius:12px;box-shadow:0 10px 30px -8px rgba(0,0,0,.18);"),
  dataZoom = list(borderColor = "transparent", fillerColor = "rgba(10,10,10,0.06)", handleColor = "#0A0A0A",
                  backgroundColor = "#FAFAFA", dataBackgroundColor = "#D4D4D4", textStyle = list(color = "#737373"))
), auto_unbox = TRUE) |> as.character()

e_estilo <- function(e) {
  e |>
    echarts4r::e_theme_custom(TEMA_ECHARTS, name = "pacha") |>
    echarts4r::e_text_style(fontFamily = FUENTE_GRAFICOS)
}

e_base <- function(e) {
  e |>
    e_estilo() |>
    echarts4r::e_tooltip(trigger = "axis", confine = TRUE)
}

#' Mezcla un color con blanco (f = 0 deja el color, f = 1 da blanco).
aclarar <- function(color, f = 0.5) {
  rgb <- grDevices::col2rgb(color)[, 1]
  grDevices::rgb(t(round(rgb + (255 - rgb) * f)), maxColorValue = 255)
}

con_alfa <- function(color, alfa) {
  rgb <- grDevices::col2rgb(color)[, 1]
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alfa)
}

#' Antes eran degradados; el diseño actual usa colores planos. Se conservan los
#' nombres para no tocar cada gráfico: devuelven el color principal.
degradado_v <- function(arriba, abajo) arriba
degradado_h <- function(color) color

grafico_oni <- function(oni, cfg) {
  col <- cfg$colores
  oni |>
    dplyr::mutate(fecha = format(fecha, "%Y-%m"), valor = round(valor, 2),
                  calido = ifelse(valor > 0, valor, 0), frio = ifelse(valor < 0, valor, 0)) |>
    echarts4r::e_charts(fecha) |>
    echarts4r::e_area(calido, name = "Fase cálida", symbol = "none", lineStyle = list(width = 0),
                      areaStyle = list(color = con_alfa(col$nino, 0.75)),
                      tooltip = list(show = FALSE)) |>
    echarts4r::e_area(frio, name = "Fase fría", symbol = "none", lineStyle = list(width = 0),
                      areaStyle = list(color = con_alfa(col$nina, 0.75)),
                      tooltip = list(show = FALSE)) |>
    echarts4r::e_line(valor, name = "ONI (°C)", symbol = "none",
                      lineStyle = list(width = 1, color = "#0A0A0A"), itemStyle = list(color = "#0A0A0A")) |>
    echarts4r::e_mark_line(serie = "ONI (°C)", data = list(yAxis = 0.5), symbol = "none",
                           lineStyle = list(type = "dashed", color = col$nino, opacity = 0.6),
                           label = list(formatter = "El Niño", color = col$nino)) |>
    echarts4r::e_mark_line(serie = "ONI (°C)", data = list(yAxis = -0.5), symbol = "none",
                           lineStyle = list(type = "dashed", color = col$nina, opacity = 0.6),
                           label = list(formatter = "La Niña", color = col$nina)) |>
    echarts4r::e_datazoom(type = "slider", start = 65, height = 18, borderColor = "transparent") |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_grid(left = 36, right = 56, top = 16, bottom = 56) |>
    e_base()
}

# Animación: ¿cómo va El Niño? -------------------------------------------------------------
#
# Umbrales de magnitud del ENFEN para la anomalía de Niño 1+2 (ICEN): débil 0,4;
# moderado 1,0; fuerte 1,7; extraordinario 3,0 °C. Para Niño 3.4 se usan los de la
# NOAA: débil 0,5; moderado 1,0; fuerte 1,5; muy fuerte 2,0 °C.

BANDAS_ENFEN <- list(c(0.4, 1.0, "Débil"), c(1.0, 1.7, "Moderado"), c(1.7, 3.0, "Fuerte"), c(3.0, 9, "Extraordinario"))

#' Anomalías semanales de Niño 1+2 y Niño 3.4 que se van dibujando (animación de ~6 s).
grafico_evolucion_enso <- function(enso, cfg, semanas = 104) {
  col <- cfg$colores
  d <- enso |>
    dplyr::filter(indice %in% c("nino12", "nino34")) |>
    dplyr::group_by(indice) |>
    dplyr::slice_tail(n = semanas) |>
    dplyr::ungroup() |>
    dplyr::mutate(valor = round(valor, 2)) |>
    tidyr::pivot_wider(names_from = indice, values_from = valor) |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(fecha = format(fecha, "%d %b %Y"))
  bandas <- lapply(seq_along(BANDAS_ENFEN), function(i) {
    b <- BANDAS_ENFEN[[i]]
    list(list(yAxis = as.numeric(b[1]), name = b[3],
              itemStyle = list(color = con_alfa("#737373", 0.03 + 0.03 * i)),
              label = list(position = "insideLeft", color = "#737373", fontSize = 10)),
         list(yAxis = as.numeric(b[2])))
  })
  etiqueta_final <- htmlwidgets::JS(
    "function (p) { var v = Number(Array.isArray(p.value) ? p.value[1] : p.value);",
    "  if (!isFinite(v)) return '';",
    "  return p.seriesName + '  ' + (v > 0 ? '+' : '') + v.toFixed(1) + ' °C'; }")
  d |>
    echarts4r::e_charts(fecha) |>
    echarts4r::e_line(nino12, name = "Niño 1+2 (costa)", symbol = "none", smooth = TRUE,
                      lineStyle = list(width = 2.25, color = col$nino),
                      itemStyle = list(color = col$nino),
                      areaStyle = list(color = con_alfa(col$nino, 0.08)),
                      endLabel = list(show = TRUE, formatter = etiqueta_final, color = col$nino, fontWeight = 600),
                      markArea = list(silent = TRUE, data = bandas)) |>
    echarts4r::e_line(nino34, name = "Niño 3.4 (Pacífico central)", symbol = "none", smooth = TRUE,
                      lineStyle = list(width = 2, color = col$acento),
                      itemStyle = list(color = col$acento),
                      endLabel = list(show = TRUE, formatter = etiqueta_final, color = col$acento, fontWeight = 600)) |>
    echarts4r::e_y_axis(name = "Anomalía (°C)") |>
    echarts4r::e_legend(top = 0) |>
    echarts4r::e_grid(left = 48, right = 150, top = 36, bottom = 28) |>
    echarts4r::e_animation(duration = 6000, easing = "linear") |>
    e_base()
}

#' Trayectoria de las últimas semanas en el plano Niño 3.4 (x) vs Niño 1+2 (y):
#' arriba a la derecha = El Niño en todo el Pacífico; arriba a la izquierda = Niño
#' costero. Cuatro tramos de color (del más antiguo al más reciente) y un marcador
#' en la semana actual. Sin agrupar datos: cada tramo es una columna propia.
grafico_trayectoria_enso <- function(enso, cfg, semanas = 52) {
  col <- cfg$colores
  d <- enso |>
    dplyr::filter(indice %in% c("nino12", "nino34")) |>
    dplyr::select(fecha, indice, valor) |>
    tidyr::pivot_wider(names_from = indice, values_from = valor, values_fn = mean) |>
    dplyr::filter(!is.na(nino12), !is.na(nino34)) |>
    dplyr::arrange(fecha) |>
    utils::tail(semanas)
  if (nrow(d) < 5) return(NULL)
  n <- nrow(d)
  cortes <- unique(round(seq(1, n, length.out = 5)))
  d <- d |> dplyr::mutate(fila = dplyr::row_number(), x = round(nino34, 2), semana = format(fecha, "%d/%m/%Y"))
  # Cada tramo comparte su último punto con el siguiente para que la línea no se corte
  tramo <- function(k) ifelse(d$fila >= cortes[k] & d$fila <= cortes[k + 1], round(d$nino12, 2), NA)
  for (k in seq_len(length(cortes) - 1)) d[[paste0("t", k)]] <- tramo(k)
  ultimo <- d[n, ]
  lim <- max(3, ceiling(max(abs(c(d$nino12, d$nino34)), na.rm = TRUE)))
  nombres <- c("Hace un año", "Hace 9 meses", "Hace 6 meses", "Últimos 3 meses")
  colores <- c(aclarar(col$celeste, 0.25), col$acento, col$sol, col$nino)
  zona <- function(x0, y0, x1, y1, nombre, color, pos)
    list(list(xAxis = x0, yAxis = y0, name = nombre, itemStyle = list(color = color),
              label = list(position = pos, color = "#737373", fontSize = 10)),
         list(xAxis = x1, yAxis = y1))

  e <- d |> echarts4r::e_charts(x)
  for (k in seq_len(length(cortes) - 1)) {
    extra <- list()
    if (k == 1) extra$markArea <- list(silent = TRUE, data = list(
      zona(0.5, 0.4, lim, lim, "El Niño en todo el Pacífico", con_alfa(col$nino, 0.07), "insideTopRight"),
      zona(-lim, 0.4, 0.5, lim, "Niño costero", con_alfa(col$sol, 0.10), "insideTopLeft"),
      zona(-lim, -lim, -0.5, -0.4, "La Niña", con_alfa(col$nina, 0.07), "insideBottomLeft")))
    if (k == length(cortes) - 1) extra$markPoint <- list(
      symbol = "pin", symbolSize = 42, itemStyle = list(color = "#0A0A0A"),
      label = list(formatter = "hoy", color = "#FFFFFF", fontWeight = 600, fontSize = 10),
      data = list(list(coord = c(ultimo$x, round(ultimo$nino12, 2)))))
    e <- do.call(echarts4r::e_line_, c(list(e, paste0("t", k), name = nombres[k], bind = "semana",
                                           symbol = "circle", symbolSize = 5, smooth = 0.3, connectNulls = FALSE,
                                           lineStyle = list(width = 2.5, color = colores[k]),
                                           itemStyle = list(color = colores[k])), extra))
  }
  e |>
    echarts4r::e_x_axis(type = "value", name = "Niño 3.4 (°C)", nameLocation = "middle", nameGap = 26,
                        min = -lim, max = lim) |>
    echarts4r::e_y_axis(name = "Niño 1+2 (°C)", min = -lim, max = lim) |>
    echarts4r::e_tooltip(trigger = "item", formatter = htmlwidgets::JS(
      "function (p) { if (!Array.isArray(p.value)) return p.name || '';",
      "  return '<b>' + p.name + '</b><br>Niño 3.4: ' + Number(p.value[0]).toFixed(2) + ' °C<br>Niño 1+2: ' + Number(p.value[1]).toFixed(2) + ' °C'; }")) |>
    echarts4r::e_legend(bottom = 0, itemWidth = 14, textStyle = list(fontSize = 11)) |>
    echarts4r::e_grid(left = 52, right = 20, top = 20, bottom = 64) |>
    echarts4r::e_animation(duration = 6000, easing = "linear") |>
    e_estilo()
}

# Aviso y firma --------------------------------------------------------------------------

#' Franja fija bajo los filtros: la plataforma es referencial.
aviso_referencial <- function() {
  shiny::div(
    class = "aviso-referencial", role = "note",
    bsicons::bs_icon("info-circle-fill"),
    shiny::span(
      shiny::tags$b("Plataforma referencial."),
      " Los niveles de riesgo y los pronósticos son estimaciones automáticas con fines informativos.",
      " No reemplazan los avisos oficiales del SENAMHI, ENFEN, INDECI ni CENEPRED: ante una emergencia, siga las indicaciones de las autoridades."
    )
  )
}

#' Bolita flotante con el enlace al perfil de LinkedIn del autor (config.yml › autor).
#' Muestra la foto www/perfil.jpg (config.yml › autor › foto) con una insignia de
#' LinkedIn. El navegador la pide en cada visita: cambiar o agregar la foto no exige
#' reiniciar la app. Si no existe, queda solo el logo de LinkedIn.
firma_autor <- function(cfg) {
  a <- cfg$autor
  if (is.null(a$linkedin) || !nzchar(a$linkedin)) return(NULL)
  nombre <- a$nombre %||% "el autor"
  bolita <- shiny::span(
    class = "firma-bolita con-foto",
    shiny::tags$img(src = a$foto %||% "perfil.jpg", alt = nombre,
                    onerror = "this.parentNode.classList.remove('con-foto'); this.remove();"),
    shiny::span(class = "firma-insignia", bsicons::bs_icon("linkedin")),
    shiny::span(class = "firma-icono", bsicons::bs_icon("linkedin"))
  )
  shiny::tags$a(
    class = "firma-autor", href = a$linkedin, target = "_blank", rel = "noopener",
    title = paste("Perfil de LinkedIn de", nombre), `aria-label` = paste("Perfil de LinkedIn de", nombre),
    shiny::span(class = "firma-texto", shiny::tags$small("Desarrollado por"), shiny::tags$b(nombre)),
    bolita
  )
}
