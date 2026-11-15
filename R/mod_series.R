# Explorar: series y estadísticas de cualquier departamento o distrito -----------

mod_series_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 290, open = list(desktop = "open", mobile = "closed"),
      segmentado(ns("modo"), c("Departamento" = "dep", "Cuenca" = "cuenca"), "dep", etiqueta = "Ver por"),
      shiny::conditionalPanel(
        "input.modo == 'dep'", ns = ns,
        shiny::selectizeInput(ns("dep"), "Departamento", choices = NULL),
        shiny::selectizeInput(ns("distrito"), "Distrito (opcional)", choices = NULL,
                              options = list(placeholder = "Todo el departamento"))
      ),
      shiny::conditionalPanel(
        "input.modo == 'cuenca'", ns = ns,
        shiny::selectizeInput(ns("cuenca"), "Cuenca o unidad hidrográfica (ANA)", choices = NULL,
                              options = list(placeholder = "Escriba el nombre…"))
      ),
      shiny::selectInput(ns("temporada"), "Temporada para las estadísticas",
                         choices = c("Dic – Feb (lluvias)" = "12,1,2", "Ene – Mar" = "1,2,3",
                                     "Oct – Dic (siembra)" = "10,11,12", "Jun – Ago" = "6,7,8",
                                     "Todo el año" = "1,2,3,4,5,6,7,8,9,10,11,12")),
      shiny::selectInput(ns("indice_enso"), "Índice ENSO",
                         choices = c("Niño 3.4 (central)" = "nino34", "Niño 1+2 (costa)" = "nino12", "ONI" = "oni")),
      boton_csv(ns("csv")),
      shiny::p(class = "nota", "Lluvia: PISCO v3 (SENAMHI) promediada por área; meses recientes con CHIRPS",
               "mensual y diario corregido a PISCO. Climatología 1991–2020.")
    ),
    titulo_vista(ns("titular"), "Explorar series", "graph-up", ns("contexto")),
    shiny::uiOutput(ns("kpis")),
    bslib::navset_card_underline(
      full_screen = TRUE,
      bslib::nav_panel("Lluvia mensual", echarts4r::echarts4rOutput(ns("serie"), height = 360)),
      bslib::nav_panel("Índice de sequía", echarts4r::echarts4rOutput(ns("spi"), height = 360)),
      bslib::nav_panel("Ciclo anual", echarts4r::echarts4rOutput(ns("ciclo"), height = 360))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(4, 4, 4)),
      bslib::card(bslib::card_header("Lluvia de la temporada según el ENSO"),
                  echarts4r::echarts4rOutput(ns("compuestos"), height = 250)),
      bslib::card(bslib::card_header("¿Cuánto depende del ENSO?"),
                  echarts4r::echarts4rOutput(ns("correlacion"), height = 250),
                  shiny::p(class = "nota", "Correlación de Spearman entre la anomalía de lluvia y el índice,",
                           "con el índice 0 a 3 meses antes.")),
      bslib::card(bslib::card_header("¿Cada cuánto un déficit así?"),
                  reactable::reactableOutput(ns("retorno")),
                  shiny::p(class = "nota", "Ajuste Gumbel a los déficits de la temporada, 1981 en adelante."))
    )
  )
}

mod_series_server <- function(id, datos, cfg, ir_a, cuencas = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ref <- unlist(cfg$climatologia)
    modo <- shiny::reactive(input$modo %||% "dep")

    # Cuencas con al menos un distrito mayormente dentro, agrupadas por vertiente
    if (!is.null(cuencas)) {
      con_distritos <- unique(cuencas$distritos$codigo[cuencas$distritos$fraccion >= 0.5])
      u <- dplyr::filter(cuencas$unidades, codigo %in% con_distritos) |> dplyr::arrange(region, nombre)
      shiny::updateSelectizeInput(session, "cuenca", selected = "1378",   # Piura
                                  choices = lapply(split(stats::setNames(u$codigo, u$nombre), u$region), as.list))
    }

    shiny::observe({
      d <- datos$distritos()
      deps <- dplyr::distinct(d, cod_dep, departamento) |> dplyr::arrange(departamento)
      actual <- shiny::isolate(input$dep)
      shiny::updateSelectizeInput(session, "dep", choices = stats::setNames(deps$cod_dep, deps$departamento),
                                  selected = if (nzchar(actual %||% "")) actual else "21")   # Puno
    })

    pendiente <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$dep, {
      d <- dplyr::filter(datos$distritos(), cod_dep == input$dep)
      sel <- pendiente() %||% ""
      shiny::updateSelectizeInput(session, "distrito", server = TRUE, selected = sel,
                                  choices = c("Todo el departamento" = "", stats::setNames(d$ubigeo, d$distrito)))
      pendiente(NULL)
    })

    # Desde la ficha: «Ver serie completa»
    shiny::observeEvent(ir_a(), {
      ub <- ir_a()$ubigeo
      shiny::updateRadioButtons(session, "modo", selected = "dep")
      dep <- substr(ub, 1, 2)
      if (identical(input$dep, dep)) {
        # El selector de distritos es del lado del servidor: hay que reenviar las opciones
        d <- dplyr::filter(datos$distritos(), cod_dep == dep)
        shiny::updateSelectizeInput(session, "distrito", server = TRUE, selected = ub,
                                    choices = c("Todo el departamento" = "", stats::setNames(d$ubigeo, d$distrito)))
      } else {
        pendiente(ub)
        shiny::updateSelectizeInput(session, "dep", selected = dep)
      }
    })

    ubigeos <- shiny::reactive({
      if (modo() == "cuenca") {
        shiny::req(input$cuenca, !is.null(cuencas))
        u <- distritos_de_cuenca(cuencas, input$cuenca)
        if (length(u) == 0) u <- distritos_de_cuenca(cuencas, input$cuenca, minimo = 0.1)
        return(u)
      }
      shiny::req(input$dep)
      if (nzchar(input$distrito %||% "")) input$distrito
      else dplyr::filter(datos$distritos(), cod_dep == input$dep)$ubigeo
    })
    nombre <- shiny::reactive({
      d <- datos$distritos()
      if (modo() == "cuenca") {
        return(paste0(cuencas$unidades$nombre[cuencas$unidades$codigo == input$cuenca], " (", length(ubigeos()), " distritos)"))
      }
      if (nzchar(input$distrito %||% "")) {
        f <- d[d$ubigeo == input$distrito, ]
        paste0(f$distrito, " (", f$departamento, ")")
      } else d$departamento[d$cod_dep == input$dep][1]
    })
    meses <- shiny::reactive(as.integer(strsplit(input$temporada, ",")[[1]]))

    # Ojo: un req() dentro del argumento de una consulta SQL se vuelve error visible
    serie_completa <- shiny::reactive({ datos$version(); u <- ubigeos(); q_precip_serie(datos$db, u) })
    serie <- shiny::reactive(dplyr::filter(serie_completa(), !pronostico))
    enso_mensual <- shiny::reactive(enso_a_mensual(datos$enso(), input$indice_enso))
    oni_mensual <- shiny::reactive(enso_a_mensual(datos$enso(), "oni"))

    stats <- shiny::reactive({
      s <- serie()
      shiny::req(nrow(s) > 24)
      list(
        anomalias = serie_con_anomalias(s, ref),
        percentil = percentil_ultimo_mes(s, ref),
        tendencia = tendencia_anual(s),
        correlacion = correlacion_enso(s, enso_mensual(), meses(), ref = ref),
        compuestos = compuestos_enso(s, oni_mensual(), meses(), ref),
        ultimo = s[which.max(s$mes), ]
      )
    })

    output$titular <- shiny::renderText({
      st <- stats()
      ult <- utils::tail(st$anomalias, 1)
      r0 <- st$correlacion$r[st$correlacion$desfase == 0]
      sprintf("%s: lluvia %s respecto de lo normal · tendencia %s mm por década · correlación con el ENSO r = %s",
              fmt_mes_largo(st$ultimo$mes), fmt_signo(ult$anomalia_pct, 0, " %"),
              fmt_signo(st$tendencia$pendiente_decada, 0), fmt_num(r0, 2))
    })
    output$contexto <- shiny::renderText(nombre())

    output$kpis <- shiny::renderUI({
      st <- stats()
      ult <- utils::tail(st$anomalias, 1)
      tend <- st$tendencia
      signif <- !is.na(tend$p_valor) && tend$p_valor < 0.05
      fila_tarjetas(
        tarjeta(paste("Percentil de", fmt_mes(st$ultimo$mes)),
                if (is.na(st$percentil)) "—" else paste0("P", round(st$percentil)),
                "frente al mismo mes en 1991–2020",
                tono = if (isTRUE(st$percentil < 20)) "seco" else if (isTRUE(st$percentil > 80)) "agua" else "neutro"),
        tarjeta("Anomalía estandarizada", paste(fmt_signo(ult$anomalia_std), "σ"),
                paste(fmt_num(ult$pp_mm), "mm frente a", fmt_num(ult$media), "mm de media"),
                spark = sparkline(utils::tail(st$anomalias$anomalia_std, 24), "#1E3A8A")),
        tarjeta("Tendencia anual (Sen)", paste(fmt_signo(tend$pendiente_decada, 0), "mm/década"),
                sprintf("%s · Mann-Kendall p = %s · %d años",
                        if (signif) "significativa" else "no significativa", fmt_num(tend$p_valor, 2), tend$n_anios)),
        tarjeta("Racha seca máxima", if (is.na(ult$dsc)) "—" else paste(ult$dsc, "días"),
                if (is.na(ult$dsc)) "sin dato diario para este mes" else "días seguidos con < 1 mm")
      )
    })

    output$serie <- echarts4r::renderEcharts4r({
      st <- stats()
      ind <- input$indice_enso
      pron <- dplyr::filter(serie_completa(), pronostico) |> dplyr::select(mes, pp_pron = pp_mm)
      st$anomalias |>
        dplyr::bind_rows(pron) |>
        dplyr::left_join(enso_mensual(), by = "mes") |>
        dplyr::mutate(mes_txt = format(mes, "%Y-%m"), pp_mm = round(pp_mm, 1), pp_pron = round(pp_pron, 1),
                      p10 = round(p10, 1), p90 = round(p90, 1), valor = round(valor, 2)) |>
        echarts4r::e_charts(mes_txt) |>
        echarts4r::e_band2(p10, p90, name = "Rango normal (P10–P90)",
                           itemStyle = list(color = "#DCEFFB", borderWidth = 0)) |>
        echarts4r::e_line(pp_mm, name = "Lluvia observada", symbol = "none", color = cfg$colores$acento,
                          lineStyle = list(width = 2),
                          areaStyle = list(color = con_alfa(cfg$colores$acento, 0.08))) |>
        echarts4r::e_line(pp_pron, name = "Pronóstico (análogos)", symbol = "circle", symbolSize = 5,
                          color = cfg$colores$sol, lineStyle = list(type = "dashed", width = 2)) |>
        echarts4r::e_line(valor, name = paste(toupper(ind), "°C"), y_index = 1, symbol = "none",
                          color = cfg$colores$nino, lineStyle = list(type = "dotted", width = 1.2)) |>
        echarts4r::e_y_axis(name = "mm") |>
        echarts4r::e_y_axis(index = 1, name = "°C") |>
        echarts4r::e_datazoom(type = "slider", start = 85, height = 18) |>
        echarts4r::e_legend(top = 0) |>
        echarts4r::e_grid(left = 48, right = 48, top = 40, bottom = 56) |>
        e_base()
    })

    output$spi <- echarts4r::renderEcharts4r({
      datos$version()
      u <- ubigeos()
      grafico_spi(q_sequia_serie(datos$db, u), cfg, inicio_zoom = 60)
    })

    output$ciclo <- echarts4r::renderEcharts4r({
      st <- stats()
      ultimo_anio <- st$anomalias |> dplyr::filter(mes > sumar_meses(st$ultimo$mes, -12))
      climatologia_mensual(serie(), ref) |>
        dplyr::left_join(dplyr::transmute(ultimo_anio, m = as.integer(format(mes, "%m")), ultimos = round(pp_mm, 1)),
                         by = "m") |>
        dplyr::mutate(mes = MESES_ES[m], p10 = round(p10, 1), p50 = round(p50, 1), p90 = round(p90, 1)) |>
        echarts4r::e_charts(mes) |>
        echarts4r::e_band2(p10, p90, name = "Rango normal", itemStyle = list(color = "#DCEFFB", borderWidth = 0)) |>
        echarts4r::e_line(p50, name = "Mediana 1991–2020", symbol = "none", color = "#737373") |>
        echarts4r::e_bar(ultimos, name = "Últimos 12 meses", color = cfg$colores$acento, barWidth = "40%",
                         itemStyle = list(color = degradado_v(cfg$colores$celeste, cfg$colores$marino), borderRadius = c(6, 6, 0, 0))) |>
        echarts4r::e_y_axis(name = "mm") |>
        echarts4r::e_legend(top = 0) |>
        echarts4r::e_grid(left = 48, right = 16, top = 40, bottom = 32) |>
        e_base()
    })

    output$compuestos <- echarts4r::renderEcharts4r({
      colores <- c("La Niña" = cfg$colores$nina, "Neutro" = "#A3A3A3", "El Niño" = cfg$colores$nino)
      stats()$compuestos |>
        dplyr::mutate(fase = as.character(fase), anomalia_media = round(anomalia_media), color = colores[fase]) |>
        echarts4r::e_charts(fase) |>
        echarts4r::e_bar(anomalia_media, name = "Anomalía media") |>
        echarts4r::e_add_nested("itemStyle", color) |>
        echarts4r::e_labels(position = "top", formatter = htmlwidgets::JS("function(p){ return p.value[1] + ' %'; }")) |>
        echarts4r::e_y_axis(axisLabel = list(formatter = "{value} %")) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_grid(left = 48, right = 8, top = 24, bottom = 32) |>
        e_base()
    })

    output$correlacion <- echarts4r::renderEcharts4r({
      stats()$correlacion |>
        dplyr::mutate(desfase = paste(desfase, "m antes"), r = round(r, 2)) |>
        echarts4r::e_charts(desfase) |>
        echarts4r::e_bar(r, name = "r") |>
        echarts4r::e_color("#737373") |>
        echarts4r::e_y_axis(min = -1, max = 1) |>
        echarts4r::e_labels(position = "top", formatter = htmlwidgets::JS("function(p){ return p.value[1]; }")) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_grid(left = 40, right = 8, top = 24, bottom = 32) |>
        e_base()
    })

    output$retorno <- reactable::renderReactable({
      s <- serie()
      tabla <- tibble::tibble(Deficit = c("≥ 20 %", "≥ 30 %", "≥ 50 %"), umbral = c(20, 30, 50)) |>
        dplyr::mutate(Retorno = vapply(umbral, function(u) {
          t <- periodo_retorno_deficit(s, u, meses(), ref)
          if (is.na(t)) "—" else if (t > 500) "Más de 1 en 500 años" else sprintf("1 en %s años", fmt_num(t, 0))
        }, character(1))) |>
        dplyr::select(`Déficit de la temporada` = Deficit, `Periodo de retorno` = Retorno)
      reactable::reactable(tabla, compact = TRUE, borderless = TRUE, pagination = FALSE)
    })

    output$csv <- csv_handler("serie_region", shiny::reactive(stats()$anomalias))
  })
}
