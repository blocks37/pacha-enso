# Panorama nacional: estado del ENSO, mapa por amenaza y lo más urgente ----------

mod_panorama_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    titulo_vista(ns("titular"), "Panorama nacional", "globe-americas", ns("contexto")),
    shiny::uiOutput(ns("enso")),
    # Fila 1: riesgo en tierra y El Niño en el mar, lado a lado
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, xl = c(6, 6)),
      bslib::card(
        full_screen = TRUE, class = "card-mapa",
        bslib::card_header(
          class = "cabecera-flex",
          shiny::span("Mapa de riesgo"),
          segmentado(ns("amenaza"), c("Inundación" = "inundacion", "Sequía" = "sequia")),
          shiny::span(class = "nota", "por distrito · en rojo, riesgo ≥ 85 %")
        ),
        leaflet::leafletOutput(ns("mapa"), height = 540),
        bslib::card_footer(shiny::uiOutput(ns("leyenda")))
      ),
      bslib::card(
        class = "card-animacion card-mapa", full_screen = TRUE,
        bslib::card_header(
          class = "cabecera-flex",
          shiny::span(bsicons::bs_icon("water"), " El Niño en el mar"),
          shiny::span(class = "nota", "temperatura del mar respecto de lo normal · NASA GHRSST MUR · últimas 16 semanas")
        ),
        leaflet::leafletOutput(ns("mar"), height = 540),
        bslib::card_footer(shiny::div(
          class = "leyenda-mar",
          shiny::span("Más frío"), shiny::span(class = "barra-anomalia"), shiny::span("Más cálido que lo normal"),
          shiny::span(class = "nota ms-2", "Recuadros: Niño 1+2 (frente a la costa) y Niño 3.4 (Pacífico central).")
        ))
      )
    ),
    # Fila 2: ¿cómo va El Niño? (dos gráficos, cada uno en su tarjeta)
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(
        bslib::card_header(
          class = "cabecera-flex",
          shiny::span("¿Cómo va El Niño? · semana a semana"),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-reproducir",
                             onclick = sprintf("ensoReproducir(['%s', '%s'])", ns("evolucion"), ns("trayectoria")),
                             bsicons::bs_icon("play-fill"), " Reproducir")
        ),
        echarts4r::echarts4rOutput(ns("evolucion"), height = 340),
        shiny::p(class = "nota mb-0", "Anomalía de la temperatura del mar (NOAA, semanal). Bandas: magnitud del",
                 "Niño costero según el ENFEN (débil, moderado, fuerte, extraordinario).")
      ),
      bslib::card(
        bslib::card_header(class = "cabecera-flex", "Costa frente al Pacífico central",
                           shiny::span(class = "nota", "último año")),
        echarts4r::echarts4rOutput(ns("trayectoria"), height = 340),
        shiny::p(class = "nota mb-0", "Niño 1+2 (costa) contra Niño 3.4 (Pacífico central). El marcador «hoy» es esta semana.")
      )
    ),
    # Fila 3: lo más urgente | conteos + ONI histórico
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      bslib::card(
        bslib::card_header(class = "cabecera-flex", "Lo más urgente",
                           shiny::span(class = "nota", "clic para ver la ficha")),
        reactable::reactableOutput(ns("urgente"))
      ),
      shiny::div(
        class = "columna-lateral",
        shiny::uiOutput(ns("conteos")),
        bslib::card(
          bslib::card_header(class = "cabecera-flex", "Índice Oceánico de El Niño (ONI), 1950 – hoy",
                             shiny::span(class = "nota", "rojo: El Niño · azul: La Niña · arrastre para acercar")),
          echarts4r::echarts4rOutput(ns("oni"), height = 300)
        )
      )
    )
  )
}

mod_panorama_server <- function(id, datos, filtros, abrir_ficha, geo, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    u <- cfg$riesgo$umbrales

    riesgo <- shiny::reactive(peor_riesgo(datos$riesgo14(), datos$distritos(), 14))
    sequia_mes <- shiny::reactive({
      s <- datos$sequia_reciente()
      if (nrow(s) == 0) return(s)
      sequia_del_mes(s, datos$distritos(), datos$mes_actual())
    })
    region <- filtros$region

    ultimo <- function(indice, n) {
      e <- dplyr::filter(datos$enso(), .data$indice == .env$indice)
      utils::tail(e, n)
    }

    output$titular <- shiny::renderText({
      oni <- utils::tail(ultimo("oni", 1)$valor, 1)
      r <- filtrar_region(riesgo(), region())
      s <- filtrar_region(sequia_mes(), region())
      sprintf("%s (ONI %s °C) · %s distritos en alerta de inundación o huaico en los próximos 14 días · %s en sequía (SPI-3, %s)",
              as.character(fase_enso(oni %||% 0)), fmt_signo(oni, 2),
              format(sum(r$riesgo >= u$alerta), big.mark = " "), format(sum(en_sequia(s$categoria)), big.mark = " "),
              paste0(fmt_mes_largo(datos$mes_actual()), if (datos$mes_actual() > datos$ultimo_obs()) ", estimado" else ""))
    })
    output$contexto <- shiny::renderText(paste(nombre_region_titulo(region(), datos$distritos()), "·",
                                               fmt_fecha(Sys.Date())))

    output$enso <- shiny::renderUI({
      oni <- ultimo("oni", 36); n12 <- ultimo("nino12", 52); n34 <- ultimo("nino34", 52)
      enfen <- datos$enfen()
      f_enfen <- datos$fuentes() |> dplyr::filter(fuente == "enfen")
      ult <- function(d) utils::tail(d$valor, 1)
      fila_tarjetas(
        tarjeta("ONI · NOAA", paste(fmt_signo(ult(oni), 2), "°C"),
                sprintf("%s · %s", as.character(fase_enso(ult(oni) %||% 0)), fmt_mes(max(oni$fecha))),
                tono = if (isTRUE(ult(oni) >= 0.5)) "calido" else "neutro",
                spark = sparkline(oni$valor, cfg$colores$nino)),
        tarjeta("Niño 1+2 · costa", paste(fmt_signo(ult(n12)), "°C"),
                "Calienta el mar frente al norte: más lluvia en la costa",
                tono = if (isTRUE(ult(n12) >= 1)) "calido" else "neutro",
                spark = sparkline(n12$valor, cfg$colores$nino)),
        tarjeta("Niño 3.4 · Pacífico central", paste(fmt_signo(ult(n34)), "°C"),
                "Ligado a menos lluvia en el sur andino",
                tono = if (isTRUE(ult(n34) >= 1)) "seco" else "neutro",
                spark = sparkline(n34$valor, cfg$colores$sol)),
        tarjeta("ENFEN", enfen$estado %||% "Sin datos",
                if (length(enfen$rios)) paste("Ríos sobre lo normal:", paste(unlist(enfen$rios), collapse = ", "))
                else "Comunicado oficial del Perú",
                tono = if (grepl("Alerta", enfen$estado %||% "")) "alerta" else "neutro",
                pie = shiny::div(class = "tarjeta-pie",
                                 if (nzchar(enfen$comunicado %||% "")) paste0("Comunicado ", enfen$comunicado, " · "),
                                 "revisado ", fmt_hace(f_enfen$actualizado)))
      )
    })

    output$conteos <- shiny::renderUI({
      r <- filtrar_region(riesgo(), region())
      s <- filtrar_region(sequia_mes(), region())
      en_curso <- datos$mes_actual() > datos$ultimo_obs()
      medido <- character()
      if (en_curso) {
        # ¿El mes en curso ya trae lluvia medida (CHIRPS diario)?
        f <- tryCatch(q(datos$db, "SELECT fuente FROM precip_mensual WHERE mes = ? LIMIT 1",
                        params = list(datos$mes_actual()))$fuente, error = function(e) character())
        medido <- regmatches(f, regexpr("al [0-9]{2}/[0-9]{2}", f))
      }
      fila_tarjetas(
        tarjeta("Inundación y huaico", format(sum(r$riesgo >= u$alerta), big.mark = " "),
                sprintf("distritos en alerta o más · 14 días · %s críticos", sum(r$riesgo >= u$critico)), tono = "agua"),
        tarjeta("Sequía", format(sum(en_sequia(s$categoria)), big.mark = " "),
                sprintf("distritos con SPI-3 ≤ −1 · %s%s", fmt_mes_largo(if (en_curso) datos$mes_actual() else datos$ultimo_obs()),
                        if (!en_curso) "" else if (length(medido)) paste0(" (medido ", medido, ")") else " (estimado)"),
                tono = "seco")
      )
    })

    # Mapa ----------------------------------------------------------------------
    output$mapa <- leaflet::renderLeaflet({
      mapa_poligonos(list(list(geo = "distritos", id = "ubigeo", nombre = "distrito", grupo = "dis", borde = 0.3)),
                     contornos = TRUE)
    }) |> shiny::bindCache("mapa-panorama")     # el mapa base no cambia: se arma una vez para todas las visitas
    output$mar <- leaflet::renderLeaflet(mapa_mar(cfg)) |> shiny::bindCache("mapa-mar", Sys.Date())

    por_distrito <- shiny::reactive({
      d <- datos$distritos()
      if (input$amenaza == "inundacion") {
        r <- riesgo()
        d |>
          dplyr::left_join(dplyr::select(r, ubigeo, riesgo, nivel, fecha, amenaza), by = "ubigeo") |>
          dplyr::mutate(
            nivel = dplyr::coalesce(nivel, "Bajo"),
            valor = dplyr::coalesce(riesgo, 0),
            color = color_nivel(nivel, cfg),
            etiqueta = sprintf("<b>%s</b> · %s<br>%s: %s (%s)<br>Día más crítico: %s",
                               distrito, departamento, ifelse(amenaza %in% "huaico", "Huaico", "Inundación"),
                               fmt_pct(riesgo), nivel, fmt_fecha(fecha))
          )
      } else {
        s <- sequia_mes()
        d |>
          dplyr::left_join(dplyr::select(s, ubigeo, spi3, categoria), by = "ubigeo") |>
          dplyr::mutate(
            valor = -dplyr::coalesce(spi3, 0),
            color = color_sequia(dplyr::coalesce(categoria, "Normal o húmedo"), cfg),
            etiqueta = sprintf("<b>%s</b> · %s<br>SPI-3 %s · %s", distrito, departamento,
                               fmt_num(spi3, 2), dplyr::coalesce(categoria, "sin dato"))
          )
      }
    })

    shiny::observe({
      d <- por_distrito()
      dentro <- d$ubigeo %in% filtrar_region(d, region())$ubigeo
      inund <- input$amenaza == "inundacion"
      colorear_mapa(session, "mapa", d$ubigeo, d$color, d$etiqueta, ifelse(dentro, 0.85, 0.2),
                    bordes = if (inund) bordes_criticos(d$valor, cfg, dentro) else "")

      focos <- if (inund) dplyr::filter(d, dentro, valor >= u$critico) else d[0, ]
      focos_mapa(session, "mapa", focos, cfg)
    })

    shiny::observeEvent(region(), {
      encuadrar_mapa(leaflet::leafletProxy("mapa", session), filtrar_region(datos$distritos(), region()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$mapa_shape_click, {
      abrir_ficha(input$mapa_shape_click$id)
    })

    output$leyenda <- shiny::renderUI({
      if (input$amenaza == "inundacion") {
        leyenda_html(c(unlist(cfg$colores$inundacion), cfg$colores$neutro),
                     c("Vigilancia 40 %", "Alerta 50 %", "Alto 70 %", "Crítico 85 %", "Bajo"), critico = TRUE)
      } else {
        leyenda_html(c(unlist(cfg$colores$sequia), cfg$colores$neutro, cfg$colores$arido),
                     c("Anormal", "Moderada", "Severa", "Extrema", "Normal", "Árido"))
      }
    })

    # Lo más urgente --------------------------------------------------------------
    output$urgente <- reactable::renderReactable({
      r <- filtrar_region(riesgo(), region()) |>
        dplyr::filter(riesgo >= u$vigilancia) |>
        dplyr::transmute(ubigeo, distrito, departamento,
                         amenaza = ifelse(amenaza == "huaico", "Huaico", "Inundación"),
                         valor = fmt_pct(riesgo), orden = riesgo, color = color_nivel(nivel, cfg))
      s <- filtrar_region(sequia_mes(), region()) |>
        dplyr::filter(en_sequia(categoria)) |>
        dplyr::transmute(ubigeo, distrito, departamento, amenaza = "Sequía",
                         valor = paste("SPI", fmt_num(spi3)), orden = dplyr::coalesce(riesgo, 0),
                         color = color_sequia(categoria, cfg))
      # Mitad y mitad: si no, una amenaza tapa a la otra
      tabla <- dplyr::bind_rows(utils::head(dplyr::arrange(r, dplyr::desc(orden)), 4),
                                utils::head(dplyr::arrange(s, dplyr::desc(orden)), 4)) |>
        dplyr::arrange(dplyr::desc(orden))
      if (nrow(tabla) == 0) return(NULL)
      tabla_clicable(
        dplyr::select(tabla, ubigeo, distrito, departamento, amenaza, valor, color),
        session$ns("fila"),
        columns = list(
          ubigeo = reactable::colDef(show = FALSE), color = reactable::colDef(show = FALSE),
          departamento = reactable::colDef(show = FALSE),
          distrito = reactable::colDef("Distrito", minWidth = 130, html = TRUE,
                                       cell = function(v, i) celda_doble(v, tabla$departamento[i])),
          amenaza = reactable::colDef("Amenaza", width = 92),
          valor = reactable::colDef("Índice", width = 92, align = "right", html = TRUE,
                                    cell = function(v, i) celda_insignia(v, tabla$color[i]))
        )
      )
    })
    shiny::observeEvent(input$fila, abrir_ficha(input$fila))

    output$oni <- echarts4r::renderEcharts4r({
      datos$enso() |> dplyr::filter(indice == "oni") |> grafico_oni(cfg)
    })
    output$evolucion <- echarts4r::renderEcharts4r({
      e <- datos$enso()
      shiny::validate(shiny::need(any(e$indice == "nino12"), "Sin datos semanales de Niño 1+2 todavía."))
      grafico_evolucion_enso(e, cfg)
    })
    output$trayectoria <- echarts4r::renderEcharts4r({
      e <- datos$enso()
      shiny::validate(shiny::need(any(e$indice == "nino34"), "Sin datos semanales todavía."))
      grafico_trayectoria_enso(e, cfg)
    })
  })
}

color_porcentaje_sequia <- function(p, cfg) {
  s <- unlist(cfg$colores$sequia)
  dplyr::case_when(p >= 0.75 ~ s[4], p >= 0.5 ~ s[3], p >= 0.25 ~ s[2], p >= 0.1 ~ s[1], TRUE ~ cfg$colores$neutro)
}

#' reactable con filas que, al hacer clic, envían su ubigeo a Shiny.
tabla_clicable <- function(datos, input_id, ..., paginar = FALSE, clave = "ubigeo") {
  reactable::reactable(
    datos, compact = TRUE, borderless = TRUE, highlight = TRUE, pagination = paginar,
    rowStyle = list(cursor = "pointer"),
    onClick = reactable::JS(sprintf(
      "function(rowInfo) { var r = rowInfo.values || rowInfo.row; Shiny.setInputValue('%s', r['%s'], {priority: 'event'}); }",
      input_id, clave)),
    ...
  )
}
