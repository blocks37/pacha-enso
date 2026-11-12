# Sequía -----------------------------------------------------------------------------
# Un control de mes (con «play») recorre los últimos 12 meses observados y los
# pronosticados; el mapa, los indicadores y el ranking cambian con él.

mod_sequia_ui <- function(id, cfg) {
  ns <- shiny::NS(id)
  shiny::tagList(
    titulo_vista(ns("titular"), "Sequía", "brightness-high", ns("contexto")),
    shiny::div(class = "barra-vista",
               segmentado(ns("indice"), c("SPI-1" = "spi1", "SPI-3" = "spi3", "SPI-6" = "spi6", "SPI-12" = "spi12"), "spi3"),
               shiny::div(class = "control-mes",
                          shiny::uiOutput(ns("control_mes"), inline = TRUE),
                          shiny::uiOutput(ns("mes_txt"), inline = TRUE))),
    shiny::uiOutput(ns("kpis")),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(
        full_screen = TRUE, class = "card-mapa",
        bslib::card_header(class = "cabecera-flex", shiny::textOutput(ns("mapa_titulo"), inline = TRUE),
                           shiny::span(class = "nota", "clic en un distrito para ver su ficha")),
        leaflet::leafletOutput(ns("mapa"), height = 520),
        bslib::card_footer(leyenda_html(
          c(rev(unlist(cfg$colores$sequia)), "#F1F5F9", "#93C5FD", "#3B82F6", cfg$colores$arido),
          c("Extrema", "Severa", "Moderada", "Anormal", "Normal", "Húmedo", "Muy húmedo", "Árido")))
      ),
      bslib::card(
        bslib::card_header(class = "cabecera-flex", "Distritos más secos", boton_csv(ns("csv"))),
        reactable::reactableOutput(ns("ranking"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(class = "cabecera-flex", "Departamentos mes a mes",
                         shiny::span(class = "nota", "promedio de sus distritos · * = pronóstico")),
      echarts4r::echarts4rOutput(ns("calor"), height = 440)
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(class = "cabecera-flex", shiny::textOutput(ns("serie_titulo"), inline = TRUE),
                         shiny::span(class = "nota", "bandas: moderada, severa y extrema")),
      echarts4r::echarts4rOutput(ns("serie"), height = 320)
    )
  )
}

mod_sequia_server <- function(id, datos, filtros, abrir_ficha, geo, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    region <- filtros$region
    meses <- shiny::reactive(sort(unique(datos$sequia_reciente()$mes)))

    # El control se dibuja desde el servidor con los meses reales (no depende de que
    # llegue una actualización aparte) y arranca en el último mes observado.
    inicial <- shiny::reactive({   # arranca en el mes en curso; con ▶ o el control se revisan los anteriores
      m <- meses()
      dplyr::coalesce(match(datos$mes_actual(), m), match(datos$ultimo_obs(), m), length(m))
    })
    output$control_mes <- shiny::renderUI({
      m <- meses(); shiny::req(length(m) > 1)
      shiny::sliderInput(session$ns("mes_i"), NULL, min = 1, max = length(m), value = inicial(), step = 1,
                         ticks = FALSE, width = "300px",
                         animate = shiny::animationOptions(interval = 1400, loop = FALSE,
                                                           playButton = bsicons::bs_icon("play-circle-fill", size = "1.4rem"),
                                                           pauseButton = bsicons::bs_icon("pause-circle-fill", size = "1.4rem")))
    })

    mes <- shiny::reactive({
      m <- meses(); shiny::req(length(m) > 0)
      i <- input$mes_i %||% inicial()
      m[min(max(i, 1), length(m))]
    })
    es_pronostico <- shiny::reactive(mes() > datos$ultimo_obs())
    indice <- shiny::reactive(input$indice %||% "spi3")
    etiqueta_indice <- shiny::reactive(toupper(sub("spi", "SPI-", indice())))

    del_mes <- shiny::reactive({
      shiny::req(nrow(datos$sequia_reciente()) > 0)
      s <- sequia_del_mes(datos$sequia_reciente(), datos$distritos(), mes())
      s$valor <- s[[indice()]]
      s$arido <- dplyr::coalesce(s$categoria == CATEGORIA_ARIDO, FALSE)
      s$cat_indice <- ifelse(s$arido, CATEGORIA_ARIDO, as.character(categoria_sequia(s$valor)))
      s
    })
    en_region <- shiny::reactive(filtrar_region(del_mes(), region()))

    output$mes_txt <- shiny::renderUI({
      shiny::span(class = "mes-actual", fmt_mes_largo(mes()),
                  if (es_pronostico()) shiny::span(class = "etiqueta-aviso", "pronóstico"))
    })

    output$titular <- shiny::renderText({
      s <- dplyr::filter(en_region(), !arido)
      shiny::req(nrow(s) > 0)
      n <- sum(s$valor <= -1, na.rm = TRUE)
      peor <- s[which.min(s$valor), ]
      sprintf("%s distritos en sequía (%s ≤ −1)%s · más afectado: %s (%s), %s %s",
              format(n, big.mark = " "), etiqueta_indice(), if (es_pronostico()) " según el pronóstico por años análogos" else "",
              peor$distrito, peor$departamento, etiqueta_indice(), fmt_num(peor$valor, 1))
    })
    output$contexto <- shiny::renderText(sprintf("%s · %s%s", nombre_region_titulo(region(), datos$distritos()),
                                                 fmt_mes_largo(mes()), if (es_pronostico()) " (pronóstico)" else ""))

    output$kpis <- shiny::renderUI({
      s <- dplyr::filter(en_region(), !arido)
      d <- dplyr::select(datos$distritos(), ubigeo, area_km2)
      s <- dplyr::left_join(s, d, by = "ubigeo")
      area <- sum(s$area_km2[s$valor <= -1], na.rm = TRUE) / sum(s$area_km2, na.rm = TRUE)
      fut <- filtrar_region(sequia_del_mes(datos$sequia_reciente(), datos$distritos(),
                                           sumar_meses(datos$ultimo_obs(), 3)), region())
      fut <- dplyr::filter(fut, categoria != CATEGORIA_ARIDO)
      fila_tarjetas(
        tarjeta(paste("Distritos con", etiqueta_indice(), "≤ −1"), sum(s$valor <= -1, na.rm = TRUE),
                sprintf("de %d con lluvia suficiente para medir", nrow(s)), tono = "seco"),
        tarjeta("Superficie en sequía", fmt_pct(area), "del territorio evaluado"),
        tarjeta("Sequía extrema", sum(s$valor <= -2, na.rm = TRUE), paste(etiqueta_indice(), "≤ −2"), tono = "seco"),
        tarjeta(paste("En 3 meses (", fmt_mes(sumar_meses(datos$ultimo_obs(), 3)), ")"),
                if (nrow(fut)) sum(fut$spi3 <= -1, na.rm = TRUE) else "—",
                "distritos con SPI-3 ≤ −1 según años análogos")
      )
    })

    # Mapa ------------------------------------------------------------------------
    output$mapa_titulo <- shiny::renderText(paste(etiqueta_indice(), "·", fmt_mes_largo(mes())))
    output$mapa <- leaflet::renderLeaflet({
      mapa_poligonos(list(list(geo = "distritos", id = "ubigeo", nombre = "distrito", grupo = "dis", borde = 0.3)))
    }) |> shiny::bindCache("mapa-sequia")

    shiny::observe({
      shiny::req(pestana_activa(session, "sequia"))
      s <- del_mes()
      color <- ifelse(s$arido, cfg$colores$arido, color_spi(s$valor, cfg))
      etiqueta <- sprintf("<b>%s</b> · %s<br>%s %s · %s%s", s$distrito, s$departamento, etiqueta_indice(),
                          fmt_num(s$valor, 2), s$cat_indice, ifelse(s$pronostico, "<br><i>pronóstico</i>", ""))
      dentro <- s$ubigeo %in% en_region()$ubigeo
      colorear_mapa(session, "mapa", s$ubigeo, color, etiqueta, ifelse(dentro, 0.85, 0.15))
    })
    shiny::observeEvent(region(), {
      encuadrar_mapa(leaflet::leafletProxy("mapa", session), filtrar_region(datos$distritos(), region()))
    })
    shiny::observeEvent(input$mapa_shape_click, abrir_ficha(input$mapa_shape_click$id))

    # Ranking -----------------------------------------------------------------------
    ranking <- shiny::reactive({
      en_region() |>
        dplyr::filter(!arido, valor <= -0.5) |>
        dplyr::arrange(valor) |>
        dplyr::transmute(ubigeo, distrito, departamento, valor = round(valor, 2),
                         spi12 = round(spi12, 2), categoria = cat_indice)
    })
    output$ranking <- reactable::renderReactable({
      d <- ranking()
      if (nrow(d) == 0) return(NULL)
      d <- utils::head(d, 300) |> dplyr::mutate(categoria = celda_insignia(categoria, color_sequia(categoria, cfg)))
      tabla_clicable(
        d, session$ns("fila"), paginar = TRUE, defaultPageSize = 12, searchable = TRUE,
        language = reactable::reactableLang(searchPlaceholder = "Buscar…", pageInfo = "{rowStart}–{rowEnd} de {rows}",
                                            pagePrevious = "Anterior", pageNext = "Siguiente"),
        columns = list(
          ubigeo = reactable::colDef(show = FALSE),
          distrito = reactable::colDef("Distrito", minWidth = 120),
          departamento = reactable::colDef("Depto."),
          valor = reactable::colDef(etiqueta_indice(), align = "right", width = 72),
          spi12 = reactable::colDef("SPI-12", align = "right", width = 72),
          categoria = reactable::colDef("Categoría", minWidth = 140,
                                        html = TRUE)
        )
      )
    })
    shiny::observeEvent(input$fila, abrir_ficha(input$fila))
    output$csv <- csv_handler("sequia_distritos", ranking)

    # Mapa de calor por departamento -----------------------------------------------------
    output$calor <- echarts4r::renderEcharts4r({
      s <- datos$sequia_reciente() |>
        dplyr::filter(categoria != CATEGORIA_ARIDO) |>
        dplyr::left_join(dplyr::select(datos$distritos(), ubigeo, departamento, cod_dep, macrorregion), by = "ubigeo") |>
        filtrar_region(if (startsWith(region(), "dep:")) "pais" else region())
      shiny::validate(shiny::need(nrow(s) > 0, "Sin datos de sequía."))
      ind <- indice()
      m_sel <- mes()
      dep <- s |>
        dplyr::group_by(departamento, mes, pronostico) |>
        dplyr::summarise(valor = round(mean(.data[[ind]], na.rm = TRUE), 2), .groups = "drop")
      orden <- dep |> dplyr::filter(mes == m_sel) |> dplyr::arrange(dplyr::desc(valor)) |> dplyr::pull(departamento)
      col <- cfg$colores$sequia
      dep |>
        dplyr::mutate(departamento = factor(departamento, levels = orden),
                      etiqueta = paste0(fmt_mes(mes), ifelse(pronostico, "*", ""))) |>
        dplyr::arrange(mes, departamento) |>
        dplyr::mutate(departamento = as.character(departamento)) |>
        echarts4r::e_charts(etiqueta) |>
        echarts4r::e_heatmap(departamento, valor, itemStyle = list(borderColor = "#FFFFFF", borderWidth = 2),
                             label = list(show = TRUE, fontSize = 10)) |>
        echarts4r::e_visual_map(
          valor, type = "piecewise", orient = "horizontal", left = "center", bottom = 0,
          pieces = list(
            list(max = -2, color = col[[4]], label = "Extrema"),
            list(min = -2, max = -1.5, color = col[[3]], label = "Severa"),
            list(min = -1.5, max = -1, color = col[[2]], label = "Moderada"),
            list(min = -1, max = -0.5, color = col[[1]], label = "Anormal"),
            list(min = -0.5, max = 1, color = "#F1F5F9", label = "Normal"),
            list(min = 1, color = "#93C5FD", label = "Húmedo")
          )
        ) |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS(
          "function(p){ return p.value[1] + ' · ' + p.value[0] + '<br><b>' + p.value[2] + '</b>'; }")) |>
        echarts4r::e_text_style(fontFamily = "IBM Plex Sans") |>
        echarts4r::e_grid(left = 120, right = 16, top = 8, bottom = 56)
    })

    # Serie histórica de la región ---------------------------------------------------------
    output$serie_titulo <- shiny::renderText(paste("SPI-3 y SPI-12 de", nombre_region(region(), datos$distritos()),
                                                   "desde 1981"))
    output$serie <- echarts4r::renderEcharts4r({
      ub <- filtrar_region(datos$distritos(), region()) |>
        dplyr::filter(dplyr::coalesce(pp_anual, Inf) >= cfg$sequia$pp_anual_minima) |>
        dplyr::pull(ubigeo)
      shiny::validate(shiny::need(length(ub) > 0, "Región sin distritos evaluables."))
      grafico_spi(q_sequia_serie(datos$db, ub), cfg, inicio_zoom = 70)
    })
  })
}

#' Serie de SPI con bandas de categoría; los meses pronosticados en línea punteada.
grafico_spi <- function(serie, cfg, inicio_zoom = 0) {
  col <- cfg$colores$sequia
  banda <- function(desde, hasta, color) list(list(yAxis = desde, itemStyle = list(color = color, opacity = 0.25)),
                                              list(yAxis = hasta))
  serie |>
    dplyr::mutate(mes_txt = format(mes, "%Y-%m"),
                  spi3_obs = ifelse(pronostico, NA, round(spi3, 2)),
                  spi3_pron = ifelse(pronostico | dplyr::lead(pronostico, default = FALSE), round(spi3, 2), NA),
                  spi12 = round(spi12, 2)) |>
    echarts4r::e_charts(mes_txt) |>
    echarts4r::e_line(spi3_obs, name = "SPI-3", symbol = "none", lineStyle = list(width = 2), connectNulls = FALSE,
                      areaStyle = list(color = degradado_v(con_alfa(col[[2]], 0.05), con_alfa(col[[4]], 0.35)))) |>
    echarts4r::e_line(spi3_pron, name = "SPI-3 pronóstico", symbol = "circle", symbolSize = 4,
                      lineStyle = list(width = 1.6, type = "dashed")) |>
    echarts4r::e_line(spi12, name = "SPI-12", symbol = "none", lineStyle = list(width = 1.2)) |>
    echarts4r::e_mark_area(serie = "SPI-3", data = banda(-1, -1.5, col[[2]])) |>
    echarts4r::e_mark_area(serie = "SPI-3", data = banda(-1.5, -2, col[[3]])) |>
    echarts4r::e_mark_area(serie = "SPI-3", data = banda(-2, -4, col[[4]])) |>
    echarts4r::e_color(c(col[[4]], col[[3]], "#475569")) |>
    echarts4r::e_y_axis(min = -3.5, max = 3.5) |>
    echarts4r::e_datazoom(type = "slider", start = inicio_zoom, height = 18) |>
    echarts4r::e_legend(top = 0) |>
    echarts4r::e_grid(left = 40, right = 16, top = 32, bottom = 56) |>
    e_base()
}
