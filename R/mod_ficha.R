# Ficha de distrito ---------------------------------------------------------------
# Se abre al hacer clic en un distrito (mapa, tablas o buscador): resume en una
# ventana su riesgo de inundación, su sequía y su lluvia frente a lo normal.

mod_ficha_server <- function(id, seleccion, datos, cfg, ir_a_serie) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ub <- shiny::reactive({ shiny::req(seleccion()); seleccion()$ubigeo })

    info <- shiny::reactive(dplyr::filter(datos$distritos(), ubigeo == ub()))
    riesgo <- shiny::reactive(dplyr::filter(datos$riesgo14(), ubigeo == ub()) |> dplyr::arrange(fecha))
    sequia <- shiny::reactive({ datos$version(); u <- ub(); q_sequia_serie(datos$db, u) })
    lluvia <- shiny::reactive({ datos$version(); u <- ub(); q_precip_serie(datos$db, u) })

    shiny::observeEvent(seleccion(), {
      d <- info()
      shiny::req(nrow(d) == 1)
      shiny::showModal(shiny::modalDialog(
        title = shiny::div(class = "ficha-titulo",
                           shiny::h2(d$distrito),
                           shiny::span(class = "nota", sprintf("%s, %s · UBIGEO %s", d$provincia, d$departamento, d$ubigeo))),
        size = "xl", easyClose = TRUE,
        footer = shiny::tagList(
          shiny::actionButton(ns("ver_serie"), "Ver serie completa", icon = bsicons::bs_icon("graph-up"),
                              class = "btn-primary"),
          shiny::modalButton("Cerrar")
        ),
        shiny::uiOutput(ns("resumen")),
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
          bslib::card(bslib::card_header("Próximos 14 días: lluvia y riesgo"),
                      echarts4r::echarts4rOutput(ns("dias"), height = 260)),
          bslib::card(bslib::card_header("Sequía: SPI-3 de los últimos 3 años y pronóstico"),
                      echarts4r::echarts4rOutput(ns("spi"), height = 260))
        ),
        bslib::card(bslib::card_header("Lluvia de los últimos 24 meses frente a lo normal"),
                    echarts4r::echarts4rOutput(ns("lluvia"), height = 240))
      ))
    })

    output$resumen <- shiny::renderUI({
      d <- info(); r <- riesgo(); s <- sequia()
      obs <- dplyr::filter(s, !pronostico)
      ult <- utils::tail(obs, 1)
      fut <- utils::tail(dplyr::filter(s, pronostico), 1)
      peor <- if (nrow(r)) r[which.max(r$riesgo), ] else NULL
      arido <- isTRUE(d$pp_anual < cfg$sequia$pp_anual_minima)
      fila_tarjetas(
        tarjeta(if (isTRUE(peor$amenaza == "huaico")) "Riesgo de huaico" else "Riesgo de inundación",
                if (is.null(peor)) "—" else fmt_pct(peor$riesgo),
                if (!is.null(peor)) sprintf("%s · peor día %s (%s mm)", peor$nivel, fmt_fecha(peor$fecha), fmt_num(peor$lluvia_mm)),
                tono = if (isTRUE(peor$riesgo >= cfg$riesgo$umbrales$alerta)) "agua" else "neutro"),
        tarjeta(paste("SPI-3 ·", fmt_mes(ult$mes)),
                if (arido) "No aplica" else if (isTRUE(is.na(ult$spi3))) "—" else fmt_num(ult$spi3, 2),
                if (arido) "zona árida: llueve menos de 100 mm al año"
                else if (isTRUE(is.na(ult$spi3))) "estación seca: con tan poca lluvia normal el SPI no se calcula"
                else as.character(categoria_sequia(ult$spi3)),
                tono = if (!arido && isTRUE(ult$spi3 <= -1)) "seco" else "neutro"),
        tarjeta(paste("SPI-3 pronosticado ·", fmt_mes(fut$mes)), if (arido || nrow(fut) == 0) "—" else fmt_num(fut$spi3, 2),
                "según años análogos del ENSO"),
        tarjeta(paste("Datos del distrito", if (!is.na(d$cuenca)) paste("·", d$cuenca) else ""),
                paste(fmt_num(d$altitud, 0), "m s. n. m."),
                sprintf("%s km² · %s mm/año%s%s", fmt_num(d$area_km2, 0), fmt_num(d$pp_anual, 0),
                        if (isTRUE(d$costero)) " · costero" else "",
                        if (!is.na(d$rios)) paste0(" · ", d$rios, " (", fmt_num(d$km_rio, 0), " km)") else ""))
      )
    })

    output$dias <- echarts4r::renderEcharts4r({
      r <- riesgo()
      shiny::validate(shiny::need(nrow(r) > 0, "Sin pronóstico para este distrito."))
      r |>
        dplyr::mutate(dia = sprintf("%s %s", format(fecha, "%d"), MESES_ES[as.integer(format(fecha, "%m"))]),
                      lluvia_mm = round(lluvia_mm, 1), cuenca_mm = round(cuenca_mm, 1), riesgo = round(100 * riesgo)) |>
        echarts4r::e_charts(dia) |>
        echarts4r::e_bar(lluvia_mm, name = "Lluvia en el distrito (mm)", color = cfg$colores$celeste,
                         itemStyle = list(color = degradado_v(cfg$colores$celeste, cfg$colores$acento), borderRadius = c(5, 5, 0, 0))) |>
        echarts4r::e_bar(cuenca_mm, name = "Lluvia en la cuenca (mm, 3 días)", color = cfg$colores$marino,
                         itemStyle = list(color = degradado_v(cfg$colores$acento, cfg$colores$profundo), borderRadius = c(5, 5, 0, 0))) |>
        echarts4r::e_line(riesgo, name = "Riesgo (%)", y_index = 1, color = cfg$colores$nino, symbolSize = 7, smooth = TRUE,
                          lineStyle = list(width = 2.5)) |>
        echarts4r::e_y_axis(name = "mm") |>
        echarts4r::e_y_axis(index = 1, min = 0, max = 100, name = "%") |>
        echarts4r::e_legend(top = 0) |>
        echarts4r::e_grid(left = 40, right = 40, top = 36, bottom = 28) |>
        e_base()
    })

    output$spi <- echarts4r::renderEcharts4r({
      s <- dplyr::filter(sequia(), mes >= sumar_meses(max(mes), -42))
      shiny::validate(shiny::need(nrow(s) > 0, "Sin datos de sequía."))
      col <- color_spi(s$spi3, cfg)
      s |>
        dplyr::mutate(mes_txt = paste0(fmt_mes(mes), ifelse(pronostico, "*", "")), spi3 = round(spi3, 2),
                      color = ifelse(pronostico, paste0(col, "99"), col)) |>
        echarts4r::e_charts(mes_txt) |>
        echarts4r::e_bar(spi3, name = "SPI-3") |>
        echarts4r::e_add_nested("itemStyle", color) |>
        echarts4r::e_mark_line(data = list(yAxis = -1), symbol = "none", lineStyle = list(type = "dashed", color = "#B45309"),
                               label = list(formatter = "sequía")) |>
        echarts4r::e_y_axis(min = -3, max = 3) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_grid(left = 36, right = 16, top = 16, bottom = 28) |>
        e_base()
    })

    output$lluvia <- echarts4r::renderEcharts4r({
      l <- lluvia()
      obs <- dplyr::filter(l, !pronostico)
      shiny::validate(shiny::need(nrow(obs) > 36, "Sin serie de lluvia."))
      clima <- climatologia_mensual(obs, unlist(cfg$climatologia))
      l |>
        dplyr::filter(mes > sumar_meses(max(obs$mes), -24)) |>
        dplyr::mutate(m = as.integer(format(mes, "%m"))) |>
        dplyr::left_join(clima, by = "m") |>
        dplyr::mutate(mes_txt = paste0(fmt_mes(mes), ifelse(pronostico, "*", "")),
                      obs = ifelse(pronostico, NA, round(pp_mm, 1)), pron = ifelse(pronostico, round(pp_mm, 1), NA),
                      p10 = round(p10, 1), p50 = round(p50, 1), p90 = round(p90, 1)) |>
        echarts4r::e_charts(mes_txt) |>
        echarts4r::e_band2(p10, p90, name = "Rango normal", itemStyle = list(color = "#DBEAFE", borderWidth = 0)) |>
        echarts4r::e_line(p50, name = "Mediana", symbol = "none", color = "#A3A3A3", lineStyle = list(type = "dashed")) |>
        echarts4r::e_bar(obs, name = "Observada", color = cfg$colores$acento, stack = "x",
                         itemStyle = list(color = degradado_v(cfg$colores$celeste, cfg$colores$marino))) |>
        echarts4r::e_bar(pron, name = "Pronóstico", color = cfg$colores$sol, stack = "x",
                         itemStyle = list(color = degradado_v(aclarar(cfg$colores$sol, 0.3), cfg$colores$sol))) |>
        echarts4r::e_y_axis(name = "mm") |>
        echarts4r::e_legend(top = 0) |>
        echarts4r::e_grid(left = 44, right = 16, top = 36, bottom = 28) |>
        e_base()
    })

    shiny::observeEvent(input$ver_serie, {
      shiny::removeModal()
      ir_a_serie(ub())
    })
  })
}
