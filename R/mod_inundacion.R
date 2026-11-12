# Inundación y huaico ---------------------------------------------------------------

mod_inundacion_ui <- function(id, cfg) {
  ns <- shiny::NS(id)
  shiny::tagList(
    titulo_vista(ns("titular"), "Inundación y huaico", "cloud-rain-heavy", ns("contexto")),
    shiny::div(class = "barra-vista",
               segmentado(ns("dias"), c("Próximos 3 días" = 3, "7 días" = 7, "14 días" = 14), 7),
               shiny::div(class = "control-umbral",
                          shiny::span("Mostrar desde"),
                          shiny::sliderInput(ns("umbral"), NULL, min = 0.2, max = 0.9, value = 0.4, step = 0.05,
                                             width = "180px", ticks = FALSE),
                          shiny::textOutput(ns("umbral_txt"), inline = TRUE))),
    shiny::uiOutput(ns("kpis")),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(
        full_screen = TRUE, class = "card-mapa",
        bslib::card_header(class = "cabecera-flex", "Peor día de cada distrito",
                           shiny::span(class = "nota", "clic en un distrito para ver su ficha")),
        leaflet::leafletOutput(ns("mapa"), height = 520),
        bslib::card_footer(leyenda_html(c(unlist(cfg$colores$inundacion), cfg$colores$neutro),
                                        c("Vigilancia", "Alerta", "Alto", "Crítico", "Bajo")))
      ),
      bslib::navset_card_underline(
        full_screen = TRUE,
        title = "¿Qué lo explica?",
        bslib::nav_panel("Componentes", echarts4r::echarts4rOutput(ns("componentes"), height = 500)),
        bslib::nav_panel("Día a día", echarts4r::echarts4rOutput(ns("calor"), height = 500)),
        bslib::nav_panel("Historial", echarts4r::echarts4rOutput(ns("historial"), height = 500))
      )
    ),
    bslib::card(
      bslib::card_header(class = "cabecera-flex", "Distritos en vigilancia o más", boton_csv(ns("csv"))),
      reactable::reactableOutput(ns("tabla"))
    )
  )
}

mod_inundacion_server <- function(id, datos, filtros, abrir_ficha, geo, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    region <- filtros$region
    dias <- shiny::reactive(as.integer(input$dias %||% 7))

    peor_todo <- shiny::reactive(peor_riesgo(datos$riesgo14(), datos$distritos(), dias()))
    peor <- shiny::reactive(filtrar_region(peor_todo(), region()))
    sobre <- shiny::reactive(dplyr::filter(peor(), riesgo >= input$umbral))
    top <- shiny::reactive(utils::head(sobre(), 15))

    output$umbral_txt <- shiny::renderText(fmt_pct(input$umbral))

    output$titular <- shiny::renderText({
      s <- sobre()
      if (nrow(s) == 0) {
        return(sprintf("Ningún distrito supera el umbral de %s en los próximos %d días.", fmt_pct(input$umbral), dias()))
      }
      sprintf("%s distritos superan el umbral de %s · mayor riesgo: %s (%s), %s el %s · lluvia pronosticada %s mm",
              format(nrow(s), big.mark = " "), fmt_pct(input$umbral), s$distrito[1], s$departamento[1],
              fmt_pct(s$riesgo[1]), fmt_fecha(s$fecha[1]), fmt_num(s$lluvia_mm[1]))
    })
    output$contexto <- shiny::renderText(sprintf("%s · próximos %d días", nombre_region_titulo(region(), datos$distritos()), dias()))

    output$kpis <- shiny::renderUI({
      p <- peor(); s <- sobre()
      corrida <- datos$corrida()
      fila_tarjetas(
        tarjeta("Distritos sobre el umbral", nrow(s), sprintf("de %d evaluados", nrow(p)), tono = "agua"),
        tarjeta("Riesgo máximo", fmt_pct(max(p$riesgo, 0)),
                if (nrow(p)) paste(p$distrito[1], "·", fmt_fecha(p$fecha[1]))),
        tarjeta("Lluvia diaria máxima", paste(fmt_num(max(p$lluvia_mm, 0, na.rm = TRUE)), "mm"),
                if (nrow(p)) paste("en", p$distrito[which.max(p$lluvia_mm)])),
        tarjeta("Pronóstico", fmt_hace(corrida$inicio), corrida$notas %||% "ECMWF vía Open-Meteo")
      )
    })

    # Mapa ---------------------------------------------------------------------
    output$mapa <- leaflet::renderLeaflet({
      mapa_poligonos(list(list(sf = geo$distritos, id = "ubigeo", nombre = "distrito", grupo = "dis", borde = 0.3)),
                     rios = geo$rios, cuencas = geo$cuencas_sf)
    })

    shiny::observe({
      d <- datos$distritos() |>
        dplyr::left_join(dplyr::select(peor_todo(), ubigeo, riesgo, nivel, fecha, amenaza, lluvia_mm, cuenca_mm), by = "ubigeo") |>
        dplyr::mutate(
          nivel = dplyr::coalesce(nivel, "Bajo"),
          color = ifelse(dplyr::coalesce(riesgo, 0) >= input$umbral, color_nivel(nivel, cfg), cfg$colores$neutro),
          etiqueta = sprintf("<b>%s</b> · %s<br>%s %s (%s)<br>Lluvia %s mm el %s%s", distrito, departamento,
                             ifelse(amenaza %in% "huaico", "Huaico", "Inundación"),
                             fmt_pct(riesgo), nivel, fmt_num(lluvia_mm), fmt_fecha(fecha),
                             ifelse(is.na(rios), "", paste0("<br><span style='color:#0369A1'>", rios,
                                    ifelse(is.na(cuenca_mm), "", paste0(" · cuenca ", fmt_num(cuenca_mm), " mm/día")),
                                    "</span>")))
        )
      dentro <- d$ubigeo %in% filtrar_region(d, region())$ubigeo
      colorear_mapa(session, "mapa", d$ubigeo, d$color, d$etiqueta, ifelse(dentro, 0.85, 0.15))
      # Focos que laten: distritos en nivel alto o crítico dentro de la región y del umbral
      focos <- dplyr::filter(d, dentro, dplyr::coalesce(riesgo, 0) >= input$umbral) |>
        dplyr::mutate(valor = riesgo)
      focos_mapa(session, "mapa", focos, cfg)
    })

    shiny::observeEvent(region(), {
      encuadrar_mapa(leaflet::leafletProxy("mapa", session), filtrar_region(datos$distritos(), region()))
    })
    shiny::observeEvent(input$mapa_shape_click, abrir_ficha(input$mapa_shape_click$id))

    # Gráficos -------------------------------------------------------------------
    output$componentes <- echarts4r::renderEcharts4r({
      shiny::validate(shiny::need(nrow(top()) > 0, "Ningún distrito supera el umbral."))
      col <- cfg$colores$componentes
      t <- top()
      componentes_riesgo(t, cfg$riesgo) |>
        dplyr::mutate(dplyr::across(c(lluvia, enso, cauce, mar, exposicion), ~ round(.x, 1)),
                      distrito = make.unique(distrito, sep = " "),
                      # Al final de cada barra: riesgo, lluvia y día (distingue distritos con igual riesgo)
                      texto = sprintf("%s · %s mm · %s", fmt_pct(t$riesgo), fmt_num(t$lluvia_mm), fmt_fecha(t$fecha))) |>
        dplyr::arrange(lluvia + enso + cauce + mar + exposicion) |>
        echarts4r::e_charts(distrito) |>
        echarts4r::e_bar(lluvia, name = "Lluvia pronosticada", stack = "r", itemStyle = list(color = degradado_h(col$lluvia))) |>
        echarts4r::e_bar(enso, name = "Efecto El Niño", stack = "r", itemStyle = list(color = degradado_h(col$enso %||% "#E4572E"))) |>
        echarts4r::e_bar(cauce, name = "Río crecido (ENFEN o cuenca)", stack = "r", itemStyle = list(color = degradado_h(col$cauce))) |>
        echarts4r::e_bar(mar, name = "Mar alto", stack = "r", itemStyle = list(color = degradado_h(col$mar))) |>
        echarts4r::e_bar(exposicion, name = "Exposición (relieve / zona baja)", stack = "r", bind = texto,
                         itemStyle = list(color = degradado_h(col$exposicion), borderRadius = c(0, 6, 6, 0)),
                         label = list(show = TRUE, position = "right", formatter = "{b}", color = "#475569", fontSize = 11)) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_color(c(col$lluvia, col$enso %||% "#E4572E", col$cauce, col$mar, col$exposicion)) |>
        echarts4r::e_x_axis(max = 100, axisLabel = list(formatter = "{value} %")) |>
        echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_y_axis(axisLabel = list(width = 118, overflow = "truncate")) |>
        echarts4r::e_grid(left = 130, right = 170, top = 8, bottom = 48) |>
        e_base()
    })

    output$calor <- echarts4r::renderEcharts4r({
      shiny::validate(shiny::need(nrow(top()) > 0, "Ningún distrito supera el umbral."))
      inund <- cfg$colores$inundacion
      u <- cfg$riesgo$umbrales
      datos$riesgo14() |>
        dplyr::filter(ubigeo %in% top()$ubigeo, fecha < min(fecha) + dias()) |>
        dplyr::left_join(dplyr::select(top(), ubigeo, distrito), by = "ubigeo") |>
        dplyr::mutate(dia = sprintf("%s %s", format(fecha, "%d"), MESES_ES[as.integer(format(fecha, "%m"))]),
                      riesgo = round(100 * riesgo)) |>
        dplyr::arrange(fecha) |>
        echarts4r::e_charts(dia) |>
        echarts4r::e_heatmap(distrito, riesgo, itemStyle = list(borderColor = "#FFFFFF", borderWidth = 2)) |>
        echarts4r::e_visual_map(
          riesgo, type = "piecewise", orient = "horizontal", left = "center", bottom = 0,
          pieces = list(
            list(min = 100 * u$critico, color = inund[[4]], label = "Crítico"),
            list(min = 100 * u$alto, max = 100 * u$critico, color = inund[[3]], label = "Alto"),
            list(min = 100 * u$alerta, max = 100 * u$alto, color = inund[[2]], label = "Alerta"),
            list(min = 100 * u$vigilancia, max = 100 * u$alerta, color = inund[[1]], label = "Vigilancia"),
            list(max = 100 * u$vigilancia, color = "#F1F5F9", label = "Bajo")
          )
        ) |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS(
          "function(p){ return p.value[1] + '<br>' + p.value[0] + ': <b>' + p.value[2] + ' %</b>'; }")) |>
        echarts4r::e_text_style(fontFamily = "IBM Plex Sans") |>
        echarts4r::e_grid(left = 120, right = 16, top = 8, bottom = 64)
    })

    output$historial <- echarts4r::renderEcharts4r({
      shiny::validate(shiny::need(nrow(top()) > 0, "Ningún distrito supera el umbral."))
      h <- q_riesgo_historial(datos$db, utils::head(top()$ubigeo, 6))
      shiny::validate(shiny::need(dplyr::n_distinct(h$inicio) > 1,
                                  "El historial aparece desde la segunda actualización."))
      h |>
        dplyr::left_join(dplyr::select(top(), ubigeo, distrito), by = "ubigeo") |>
        dplyr::mutate(corrida = format(inicio, "%d/%m %Hh"), riesgo_max = round(100 * riesgo_max)) |>
        dplyr::group_by(distrito) |>
        echarts4r::e_charts(corrida) |>
        echarts4r::e_line(riesgo_max, symbolSize = 6, smooth = TRUE, lineStyle = list(width = 2.5)) |>
        echarts4r::e_y_axis(min = 0, max = 100, axisLabel = list(formatter = "{value} %")) |>
        echarts4r::e_legend(bottom = 0, type = "scroll") |>
        echarts4r::e_grid(left = 48, right = 16, top = 16, bottom = 48) |>
        e_base()
    })

    # Tabla -------------------------------------------------------------------------
    detalle <- shiny::reactive({
      dplyr::filter(peor(), riesgo >= cfg$riesgo$umbrales$vigilancia) |>
        dplyr::left_join(dplyr::select(datos$distritos(), ubigeo, rio_principal = rio), by = "ubigeo") |>
        dplyr::transmute(ubigeo, distrito, provincia, departamento,
                         amenaza = ifelse(amenaza == "huaico", "Huaico", "Inundación"),
                         fecha, lluvia_mm = round(lluvia_mm, 1), rio_principal, rio = cauce_idx > 0, mar = mar_idx > 0,
                         riesgo = round(riesgo, 3), nivel)
    })

    output$tabla <- reactable::renderReactable({
      d <- detalle()
      if (nrow(d) == 0) return(NULL)
      tabla_clicable(
        d, session$ns("fila"),
        searchable = TRUE, defaultPageSize = 10, paginar = TRUE,
        defaultSorted = list(riesgo = "desc"),
        language = reactable::reactableLang(searchPlaceholder = "Buscar…", pageInfo = "{rowStart}–{rowEnd} de {rows}",
                                            pagePrevious = "Anterior", pageNext = "Siguiente", noData = "Sin datos"),
        columns = list(
          ubigeo = reactable::colDef("UBIGEO", width = 80, class = "num"),
          distrito = reactable::colDef("Distrito", minWidth = 120),
          provincia = reactable::colDef("Provincia"),
          departamento = reactable::colDef("Departamento"),
          amenaza = reactable::colDef("Amenaza", width = 96),
          fecha = reactable::colDef("Día pico", cell = function(v) fmt_fecha(v), width = 100),
          lluvia_mm = reactable::colDef("Lluvia mm", align = "right", width = 90),
          rio_principal = reactable::colDef("Río principal", cell = function(v) if (is.na(v)) "—" else v),
          rio = reactable::colDef("Crecido", cell = function(v) if (isTRUE(v)) "▲" else "", width = 72, align = "center"),
          mar = reactable::colDef("Mar", cell = function(v) if (isTRUE(v)) "▲" else "", width = 56, align = "center"),
          riesgo = reactable::colDef("Riesgo", cell = function(v) fmt_pct(v), align = "right", width = 80),
          nivel = reactable::colDef("Nivel", html = TRUE, cell = function(v) celda_insignia(v, color_nivel(v, cfg)), width = 110)
        )
      )
    })
    shiny::observeEvent(input$fila, abrir_ficha(input$fila))
    output$csv <- csv_handler("riesgo_inundacion", detalle)
  })
}
