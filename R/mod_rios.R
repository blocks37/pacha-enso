# Ríos: cuáles pueden crecer en los próximos 14 días -------------------------------------
#
# Un río crece por la lluvia de toda su cuenca, no solo por la del distrito. Para cada
# río principal de una Unidad Hidrográfica de la ANA (y, en el Amazonas, con las
# unidades aguas arriba) se promedia la lluvia pronosticada (ECMWF) en su cuenca, en
# ventanas de 3 días, y se compara con lo normal de esa cuenca (mediana y percentil 99
# de un día lluvioso, con reducción areal). Se suma lo que reporta el ENFEN.

NIVELES_CRECIDA <- c("Muy alta", "Alta", "Moderada", "Leve", "Normal")

nivel_crecida <- function(idx, enfen = FALSE) {
  n <- dplyr::case_when(idx >= 0.85 ~ "Muy alta", idx >= 0.7 ~ "Alta", idx >= 0.5 ~ "Moderada",
                        idx >= 0.3 ~ "Leve", TRUE ~ "Normal")
  # Si el ENFEN reporta caudal sobre lo normal, al menos «Alta»
  ifelse(enfen & n %in% c("Normal", "Leve", "Moderada"), "Alta", n)
}

color_crecida <- function(nivel, cfg) {
  inund <- unlist(cfg$colores$inundacion)
  paleta <- c("Leve" = inund[1], "Moderada" = inund[2], "Alta" = inund[3], "Muy alta" = inund[4],
              "Normal" = cfg$colores$neutro)
  unname(dplyr::coalesce(paleta[nivel], cfg$colores$neutro))
}

#' Río principal de cada Unidad Hidrográfica (el «Río» con más km) y sus distritos.
catalogo_rios <- function(ruta_rios, cuencas) {
  if (is.null(cuencas) || !file.exists(ruta_rios)) return(NULL)
  r <- readr::read_csv(ruta_rios, col_types = readr::cols(km = "d", .default = "c")) |>
    dplyr::filter(!is.na(codigo_uh))
  principal <- r |>
    dplyr::filter(tipo == "Río") |>
    dplyr::group_by(codigo_uh, rio) |>
    dplyr::summarise(km = sum(km), .groups = "drop") |>
    dplyr::group_by(codigo_uh) |>
    dplyr::slice_max(km, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  distritos <- r |>
    dplyr::group_by(codigo_uh) |>
    dplyr::summarise(ubigeos = list(unique(ubigeo)), n_distritos = dplyr::n_distinct(ubigeo), .groups = "drop")
  principal |>
    dplyr::inner_join(distritos, by = "codigo_uh") |>
    dplyr::left_join(dplyr::select(cuencas$unidades, codigo_uh = codigo, cuenca = nombre, vertiente = region),
                     by = "codigo_uh") |>
    dplyr::mutate(cuenca = dplyr::coalesce(cuenca, rio),
                  claves = paste(clave_rio(rio), clave_rio(sub("^(Cuenca|Intercuenca) ", "", cuenca)), sep = "|"))
}

#' Lluvia de cuenca por río y día, y resumen del pico de los próximos días.
crecida_rios <- function(pron, clima, distritos, cuencas, catalogo, claves_enfen, peor) {
  vacio <- list(diaria = tibble::tibble(), resumen = tibble::tibble())
  if (is.null(catalogo) || nrow(pron) == 0 || nrow(clima) == 0) return(vacio)
  miembros <- miembros_cuenca(cuencas, distritos, catalogo$codigo_uh)
  diaria <- lluvia_cuenca(dplyr::select(pron, ubigeo, fecha, pp_mm), clima, miembros) |>
    dplyr::rename(codigo_uh = codigo_rio)
  if (nrow(diaria) == 0) return(vacio)
  pico <- diaria |>
    dplyr::group_by(codigo_uh) |>
    dplyr::arrange(dplyr::desc(cuenca_idx), dplyr::desc(cuenca_mm), .by_group = TRUE) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::transmute(codigo_uh, fecha_pico = fecha, mm_pico = cuenca_mm, idx = cuenca_idx, p50, p99, area)
  en_alerta <- function(u) as.numeric(sum(peor$riesgo[match(u, peor$ubigeo)] >= 0.5, na.rm = TRUE))
  resumen <- catalogo |>
    dplyr::inner_join(pico, by = "codigo_uh") |>
    dplyr::mutate(
      enfen = cauce_en_alerta(claves, claves_enfen) > 0,
      nivel = nivel_crecida(idx, enfen),
      orden = pmax(idx, ifelse(enfen, 0.7, 0)) + mm_pico / 1e4,       # desempate por milímetros
      distritos_alerta = vapply(ubigeos, en_alerta, numeric(1)),
      veces_p99 = round(mm_pico / pmax(p99, 1), 2)
    ) |>
    dplyr::arrange(dplyr::desc(orden))
  list(diaria = diaria, resumen = resumen)
}

mod_rios_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    titulo_vista(ns("titular"), "Ríos y crecidas", "water", ns("contexto")),
    shiny::uiOutput(ns("kpis")),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, xl = c(5, 7)),
      bslib::card(
        full_screen = TRUE, class = "card-mapa",
        bslib::card_header(class = "cabecera-flex", "Cuencas según la crecida esperada",
                           shiny::span(class = "nota", "clic en una cuenca para ver su río")),
        leaflet::leafletOutput(ns("mapa"), height = 560),
        bslib::card_footer(leyenda_html(c(unlist(list("#0B2E5C", "#0179C7", "#6EC3EE", "#CDEBFA", "#E6EBF1"))),
                                        c("Muy alta", "Alta", "Moderada", "Leve", "Normal")))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(class = "cabecera-flex", "Ríos con más crecida esperada · próximos 14 días",
                           boton_csv(ns("csv"))),
        reactable::reactableOutput(ns("tabla"))
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, xl = c(7, 5)),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(class = "cabecera-flex", "Lluvia en la cuenca, día a día",
                           shiny::span(class = "nota", "mm/día, promedio de 3 días · los 15 ríos con más crecida")),
        echarts4r::echarts4rOutput(ns("calor"), height = 460)
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(class = "cabecera-flex", shiny::textOutput(ns("titulo_rio"), inline = TRUE),
                           shiny::span(class = "nota", "lluvia de la cuenca contra lo normal")),
        echarts4r::echarts4rOutput(ns("rio"), height = 460)
      )
    ),
    shiny::p(class = "nota", "Ríos principales de las Unidades Hidrográficas de la ANA. Crecida = lluvia pronosticada en",
             "toda la cuenca (en el Amazonas, con las cuencas aguas arriba) medida contra su propio clima: Leve ≥ 0,3;",
             "Moderada ≥ 0,5; Alta ≥ 0,7; Muy alta ≥ 0,85 (1 = día extremo, percentil 99). Si el ENFEN reporta caudal",
             "sobre lo normal, el río queda al menos en «Alta». Es un indicador de lluvia, no un caudal medido:",
             "para caudales observados, ver los avisos hidrológicos del SENAMHI y la ANA.")
  )
}

mod_rios_server <- function(id, datos, filtros, abrir_ficha, geo, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    region <- filtros$region
    catalogo <- catalogo_rios(cfg$rutas$rios, geo$cuencas)

    todo <- shiny::reactive({
      enfen <- datos$enfen()
      claves <- if (length(enfen$claves)) unlist(enfen$claves) else clave_rio(unlist(enfen$rios) %||% character())
      crecida_rios(datos$pronostico(), datos$clima(), datos$distritos(), geo$cuencas, catalogo, claves,
                   peor_riesgo(datos$riesgo14(), datos$distritos(), 14))
    })
    # Ríos que tocan la región elegida
    resumen <- shiny::reactive({
      r <- todo()$resumen
      shiny::req(nrow(r) > 0)
      if (identical(region(), "pais")) return(r)
      dentro <- filtrar_region(datos$distritos(), region())$ubigeo
      r[vapply(r$ubigeos, function(u) any(u %in% dentro), logical(1)), ]
    })
    elegido <- shiny::reactiveVal(NULL)
    rio_actual <- shiny::reactive({
      r <- resumen()
      if (!is.null(elegido()) && elegido() %in% r$codigo_uh) elegido() else r$codigo_uh[1]
    })

    output$contexto <- shiny::renderText(sprintf("%s · próximos 14 días", nombre_region_titulo(region(), datos$distritos())))
    output$titular <- shiny::renderText({
      r <- resumen()
      n <- sum(r$nivel %in% c("Muy alta", "Alta"))
      sprintf("%d ríos con crecida alta o muy alta · %d reportados por el ENFEN con caudal sobre lo normal · mayor: %s (%s), %s mm/día el %s",
              n, sum(r$enfen), r$rio[1], r$cuenca[1], fmt_num(r$mm_pico[1]), fmt_fecha(r$fecha_pico[1]))
    })

    output$kpis <- shiny::renderUI({
      r <- resumen()
      fila_tarjetas(
        tarjeta("Crecida alta o muy alta", sum(r$nivel %in% c("Muy alta", "Alta")),
                sprintf("de %d ríos evaluados", nrow(r)), tono = "agua"),
        tarjeta("Reportados por el ENFEN", sum(r$enfen),
                if (any(r$enfen)) paste(r$rio[r$enfen], collapse = ", ") else "ninguno con caudal sobre lo normal",
                tono = if (any(r$enfen)) "alerta" else "neutro"),
        tarjeta("Mayor lluvia de cuenca", paste(fmt_num(max(r$mm_pico)), "mm/día"),
                paste("río", r$rio[which.max(r$mm_pico)], "·", fmt_fecha(r$fecha_pico[which.max(r$mm_pico)]))),
        tarjeta("Distritos ribereños en alerta", sum(r$distritos_alerta[r$nivel %in% c("Muy alta", "Alta")]),
                "a orillas de ríos con crecida alta o muy alta")
      )
    })

    # Mapa de cuencas ---------------------------------------------------------------------
    output$mapa <- leaflet::renderLeaflet({
      shiny::req(!is.null(geo$cuencas_sf))
      mapa_poligonos(list(list(sf = geo$cuencas_sf, id = "codigo", nombre = "nombre", grupo = "cue", borde = 0.6)),
                     rios = geo$rios, contornos = geo$departamentos)
    }) |> shiny::bindCache("mapa-rios")
    shiny::observe({
      shiny::req(pestana_activa(session, "rios"))
      r <- todo()$resumen
      shiny::req(nrow(r) > 0)
      cue <- tibble::tibble(codigo = geo$cuencas_sf$codigo) |>
        dplyr::left_join(dplyr::select(r, codigo = codigo_uh, rio, nivel, mm_pico, fecha_pico, enfen, veces_p99), by = "codigo") |>
        dplyr::mutate(nivel = dplyr::coalesce(nivel, "Normal"),
                      etiqueta = ifelse(is.na(rio), NA_character_, sprintf(
                        "<b>Río %s</b><br>Crecida %s%s<br>Lluvia de cuenca %s mm/día el %s (%s × P99)",
                        rio, tolower(nivel), ifelse(enfen, " · ENFEN: caudal sobre lo normal", ""),
                        fmt_num(mm_pico), fmt_fecha(fecha_pico), fmt_num(veces_p99, 1))))
      dentro <- if (identical(region(), "pais")) rep(TRUE, nrow(cue)) else cue$codigo %in% resumen()$codigo_uh
      colorear_mapa(session, "mapa", cue$codigo, color_crecida(cue$nivel, cfg), cue$etiqueta, ifelse(dentro, 0.8, 0.15))
    })
    shiny::observeEvent(input$mapa_shape_click, elegido(input$mapa_shape_click$id))
    shiny::observeEvent(region(), {
      encuadrar_mapa(leaflet::leafletProxy("mapa", session), filtrar_region(datos$distritos(), region()))
    }, ignoreInit = TRUE)

    # Tabla ---------------------------------------------------------------------------------
    tabla <- shiny::reactive({
      resumen() |>
        dplyr::transmute(codigo_uh, rio, cuenca, vertiente, nivel, idx = round(idx, 2), mm_pico, veces_p99,
                         fecha_pico, enfen, distritos_alerta, n_distritos)
    })
    output$tabla <- reactable::renderReactable({
      t <- tabla()
      reactable::reactable(
        t, compact = TRUE, borderless = TRUE, highlight = TRUE, searchable = TRUE, defaultPageSize = 12,
        rowStyle = list(cursor = "pointer"),
        onClick = reactable::JS(sprintf(
          "function(rowInfo) { Shiny.setInputValue('%s', rowInfo.row.codigo_uh, {priority: 'event'}); }", session$ns("fila"))),
        columns = list(
          codigo_uh = reactable::colDef(show = FALSE),
          rio = reactable::colDef("Río", minWidth = 120, html = TRUE, cell = function(v, i) celda_doble(v, t$cuenca[i])),
          cuenca = reactable::colDef(show = FALSE),
          vertiente = reactable::colDef("Vertiente", width = 92),
          nivel = reactable::colDef("Crecida", width = 100, html = TRUE, cell = function(v) celda_insignia(v, color_crecida(v, cfg))),
          idx = reactable::colDef("Índice", width = 70, align = "right"),
          mm_pico = reactable::colDef("mm/día", width = 76, align = "right", format = reactable::colFormat(digits = 1)),
          veces_p99 = reactable::colDef("× P99", width = 64, align = "right"),
          fecha_pico = reactable::colDef("Día pico", width = 96, cell = function(v) unname(fmt_fecha(v))),
          enfen = reactable::colDef("ENFEN", width = 66, align = "center", cell = function(v) if (isTRUE(v)) "▲" else ""),
          distritos_alerta = reactable::colDef("Distritos en alerta", width = 88, align = "right"),
          n_distritos = reactable::colDef("Distritos ribereños", width = 92, align = "right")
        )
      )
    })
    shiny::observeEvent(input$fila, elegido(input$fila))
    output$csv <- csv_handler("rios_crecida", shiny::reactive(dplyr::select(tabla(), -codigo_uh)))

    # Mapa de calor de los 15 primeros -----------------------------------------------------
    output$calor <- echarts4r::renderEcharts4r({
      r <- utils::head(resumen(), 15)
      inund <- unlist(cfg$colores$inundacion)
      todo()$diaria |>
        dplyr::filter(codigo_uh %in% r$codigo_uh) |>
        dplyr::left_join(dplyr::select(r, codigo_uh, rio), by = "codigo_uh") |>
        dplyr::mutate(rio = factor(rio, levels = rev(unique(r$rio))),
                      dia = sprintf("%s %s", format(fecha, "%d"), MESES_ES[as.integer(format(fecha, "%m"))])) |>
        dplyr::arrange(fecha, rio) |>
        echarts4r::e_charts(dia) |>
        echarts4r::e_heatmap(rio, cuenca_mm, itemStyle = list(borderColor = "#FFFFFF", borderWidth = 2)) |>
        echarts4r::e_visual_map(cuenca_mm, orient = "horizontal", left = "center", bottom = 0, calculable = TRUE,
                                inRange = list(color = c("#F4F8FC", inund[1], inund[2], inund[3], inund[4]))) |>
        echarts4r::e_y_axis(axisLabel = list(width = 110, overflow = "truncate")) |>
        echarts4r::e_grid(left = 120, right = 16, top = 8, bottom = 64) |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS(
          "function (p) { return '<b>' + p.value[1] + '</b><br>' + p.value[0] + ': ' + Number(p.value[2]).toFixed(1) + ' mm/día'; }")) |>
        echarts4r::e_text_style(fontFamily = "IBM Plex Sans")
    })

    # Detalle del río elegido -----------------------------------------------------------------
    output$titulo_rio <- shiny::renderText({
      r <- resumen()[resumen()$codigo_uh == rio_actual(), ]
      sprintf("Río %s · %s", r$rio[1], r$cuenca[1])
    })
    output$rio <- echarts4r::renderEcharts4r({
      d <- dplyr::filter(todo()$diaria, codigo_uh == rio_actual())
      shiny::req(nrow(d) > 0)
      col <- cfg$colores
      d |>
        dplyr::mutate(dia = sprintf("%s %s", format(fecha, "%d"), MESES_ES[as.integer(format(fecha, "%m"))])) |>
        echarts4r::e_charts(dia) |>
        echarts4r::e_bar(cuenca_mm, name = "Lluvia en la cuenca (mm/día, 3 días)",
                         itemStyle = list(color = degradado_v(col$celeste, col$marino), borderRadius = c(6, 6, 0, 0))) |>
        echarts4r::e_line(p50, name = "Día lluvioso normal (mediana)", symbol = "none",
                          lineStyle = list(type = "dashed", color = "#64748B"), itemStyle = list(color = "#64748B")) |>
        echarts4r::e_line(p99, name = "Día extremo (percentil 99)", symbol = "none",
                          lineStyle = list(type = "dashed", color = col$nino, width = 2), itemStyle = list(color = col$nino)) |>
        echarts4r::e_y_axis(name = "mm/día") |>
        echarts4r::e_legend(top = 0) |>
        echarts4r::e_grid(left = 48, right = 16, top = 44, bottom = 32) |>
        e_base()
    })
  })
}
