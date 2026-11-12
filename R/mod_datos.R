# Datos y método: de dónde viene cada dato, cuándo llegó y cómo se calcula ------------

mod_datos_ui <- function(id, cfg) {
  ns <- shiny::NS(id)
  shiny::tagList(
    titulo_vista(ns("titular"), "Datos y método", "database"),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),
      bslib::card(
        bslib::card_header(class = "cabecera-flex", "Fuentes de datos",
                           shiny::span(class = "nota", "de dónde viene cada dato y cuándo llegó lo último")),
        reactable::reactableOutput(ns("fuentes"))
      ),
      bslib::card(
        bslib::card_header("¿Qué tan actuales son los datos?"),
        shiny::uiOutput(ns("frescura"))
      )
    ),
    # Solo el administrador ve este panel (ver es_admin() en R/utils.R)
    shiny::uiOutput(ns("panel_admin")),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Cómo se calcula el riesgo de inundación y huaico", icon = bsicons::bs_icon("cloud-rain-heavy"),
        shiny::p("riesgo = w_lluvia · lluvia × (1 + 0,15 · ENSO) + w_mar · mar + w_exposición · exposición + w_cauce · río"),
        reactable::reactableOutput(ns("pesos")),
        shiny::tags$ul(
          shiny::tags$li("Lluvia: pronóstico diario ECMWF IFS 0,25° (14 días, vía Open-Meteo), interpolado en cada distrito desde los 4 puntos de grilla que lo rodean, medido contra el clima del propio distrito: 0 = día lluvioso típico (mediana) y 1 = día extremo (percentil 99). Se usa ECMWF y no GFS porque GFS exagera la lluvia en los Andes (3–4 veces en el altiplano) y subestima la del Niño costero en la costa norte."),
          shiny::tags$li("ENSO: promedio del ONI (saturado en 2 °C) y de Niño 1+2 (saturado en 4 °C). Solo amplifica la lluvia en la vertiente del Pacífico al norte de ~10° S (pleno al norte de 8° S, nada al sur de 12° S): en el sur andino y la Amazonía El Niño suele traer déficit, que se mide en Sequía."),
          shiny::tags$li("Mar: 0,5 en distritos costeros si el ENFEN indica nivel del mar sobre lo normal. Río: se usa el mayor de dos indicadores para los ríos que cruzan el distrito (ríos y quebradas de la ANA): 1 si el ENFEN reporta ese río o su cuenca con caudal sobre lo normal, y la lluvia pronosticada en toda su cuenca aguas arriba (promedio de 3 días; Unidades Hidrográficas de la ANA, numeración Pfafstetter en el Amazonas) medida contra lo normal de esa cuenca."),
          shiny::tags$li("Huaico en vez de inundación sobre 1 500 m s. n. m. Exposición a huaico = desnivel dentro del distrito (percentil 90 − percentil 10 de la altitud en ~15 puntos; mosaicos de elevación Terrarium): 0 con 300 m o menos, 1 con 1 500 m o más. Así el altiplano, alto pero plano, no cuenta como zona de huaicos. Sin lluvia, el resto de componentes no basta para salir de «bajo»."),
          shiny::tags$li("Mapa por departamentos: color según el percentil 75 de sus distritos (el cuarto más afectado), no el máximo, para que un solo distrito no pinte todo el departamento.")
        ),
        shiny::uiOutput(ns("nota_clima"))
      ),
      bslib::accordion_panel(
        "Cómo se calcula la sequía", icon = bsicons::bs_icon("brightness-high"),
        shiny::tags$ul(
          shiny::tags$li("Lluvia mensual por distrito: PISCO v3 (SENAMHI, 1981–2025) promediado por área con los límites INEI 2025; los meses que PISCO aún no cubre, con CHIRPS v3 (mensual definitivo y, para las últimas semanas, diario preliminar), corregido a PISCO por distrito y época del año. Umbrales de lluvia extrema (P50 y P99 de los días con lluvia, 1991–2020) y rachas secas: PISCO diario."),
          shiny::tags$li("SPI-1, 3, 6 y 12 (distribución gamma, paquete SPEI) ajustado a 1991–2020. Categorías OMM: extrema ≤ −2; severa ≤ −1,5; moderada ≤ −1; anormalmente seco ≤ −0,5."),
          shiny::tags$li("Pronóstico de 6 meses: mediana de la lluvia de los años con un ENSO parecido al actual (años análogos según el ONI)."),
          shiny::tags$li("En distritos con menos de 100 mm al año (desierto costero) el SPI no se interpreta. Tampoco en la estación seca: si lo normal en la ventana del índice es menos de 8 mm por mes (p. ej. junio–agosto en la sierra sur), un mes sin lluvia no es sequía y el SPI queda en blanco."),
          shiny::tags$li("Riesgo compuesto = 0,45 · SPI-3 + 0,25 · SPEI-6 + 0,15 · racha seca + 0,15 · Niño 3.4 (los componentes que falten reparten su peso).")
        ),
        shiny::uiOutput(ns("nota_analogos"))
      ),
      bslib::accordion_panel(
        "Limitaciones", icon = bsicons::bs_icon("exclamation-triangle"),
        shiny::p("Modelo indicativo. Pesos y umbrales aún no se calibran con emergencias registradas por INDECI ni con pérdidas",
                 "agrícolas del MIDAGRI. No reemplaza los comunicados del ENFEN ni los avisos del SENAMHI, INDECI y CENEPRED.")
      )
    )
  )
}

ENLACES_FUENTES <- list(
  oni = "https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php",
  nino_semanal = "https://www.cpc.ncep.noaa.gov/data/indices/",
  enfen = "https://www.senamhi.gob.pe/?p=fenomeno-el-nino",
  pisco = "https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/",
  pisco_diario = "https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/",
  chirps = "https://www.chc.ucsb.edu/data/chirps3",
  chirps_diario = "https://www.chc.ucsb.edu/data/chirps3",
  gfs = "https://open-meteo.com/en/docs/ecmwf-api"
)

mod_datos_server <- function(id, datos, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    es_administrador <- es_admin(session, cfg)

    output$panel_admin <- shiny::renderUI({
      if (!es_administrador) return(NULL)
      bslib::card(
        class = "card-admin",
        bslib::card_header(class = "cabecera-flex",
                           shiny::span(bsicons::bs_icon("shield-lock"), " Administración · actualizar datos"),
                           shiny::span(class = "nota", "solo visible para el administrador")),
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
          shiny::div(
            if (isTRUE(cfg$actualizacion$desde_app)) shiny::uiOutput(session$ns("controles"), class = "sin-atenuar"),
            shiny::p(class = "nota mt-2", sprintf(
              "Lo normal es no tocar nada: la tarea programada corre cada %d horas (Windows: programar_windows.R; servidor: cron con Rscript actualizar.R).",
              cfg$actualizacion$cada_horas))
          ),
          shiny::uiOutput(session$ns("estado_proceso"), class = "sin-atenuar")
        )
      )
    })
    revision <- shiny::reactivePoll(10000, session,
                                    function() file.mtime(file.path(cfg$rutas$db, "ULTIMA_REVISION")),
                                    function() leer_revision(cfg$rutas$db))
    lanzado <- shiny::reactiveVal(0)

    output$titular <- shiny::renderText({
      r <- revision()
      f <- datos$fuentes()
      errores <- sum(f$estado == "error", na.rm = TRUE)
      sprintf("Última revisión de las fuentes %s · %s", if (is.null(r)) "—" else fmt_hace(r$hora),
              if (errores == 0) "todas responden" else sprintf("%d con problemas (ver detalle)", errores))
    })

    output$fuentes <- reactable::renderReactable({
      f <- datos$fuentes()
      catalogo <- tibble::tibble(fuente = names(FUENTES),
                                 nombre = vapply(FUENTES, `[[`, "", "nombre"),
                                 que = vapply(FUENTES, `[[`, "", "que"),
                                 frecuencia = vapply(FUENTES, `[[`, "", "frecuencia"),
                                 enlace = vapply(names(FUENTES), function(k) ENLACES_FUENTES[[k]] %||% "", ""))
      tabla <- dplyr::left_join(catalogo, f, by = "fuente")
      # Celdas como HTML simple (un valor vacío dentro de una etiqueta rompía la tabla en el navegador)
      esc <- function(x) htmltools::htmlEscape(dplyr::coalesce(as.character(x), ""))
      clase <- dplyr::case_when(tabla$estado %in% c("ok", "sin cambios") ~ "ok", tabla$estado %in% "error" ~ "error", TRUE ~ "pendiente")
      vista <- tibble::tibble(
        estado = sprintf('<span class="punto %s" title="%s"></span>', clase,
                         c(ok = "responde", error = "con problemas", pendiente = "sin revisar")[clase]),
        fuente = sprintf('<div class="celda-titulo">%s</div><div class="nota">%s</div>',
                         ifelse(nzchar(tabla$enlace), sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>',
                                                               tabla$enlace, esc(tabla$nombre)), esc(tabla$nombre)),
                         esc(tabla$que)),
        frecuencia = esc(tabla$frecuencia),
        ultimo = vapply(as.list(tabla$actualizado), function(x) if (length(x) == 0 || is.na(x)) "—" else
          sprintf("%s<div class=\"nota\">%s</div>", fmt_fecha(as.Date(x)), fmt_hace(x)), ""),
        detalle = sprintf('<span class="nota">%s</span>', esc(ifelse(clase == "error" & !es_administrador,
                                                                      "Sin respuesta en la última revisión; se usa el último dato bueno.",
                                                                      dplyr::coalesce(tabla$mensaje, "—"))))
      )
      reactable::reactable(
        vista, compact = TRUE, borderless = TRUE, pagination = FALSE, sortable = FALSE,
        columns = list(
          estado = reactable::colDef("", width = 30, html = TRUE),
          fuente = reactable::colDef("Fuente", minWidth = 190, html = TRUE),
          frecuencia = reactable::colDef("Se publica", width = 130),
          ultimo = reactable::colDef("Último dato nuevo", width = 130, html = TRUE),
          detalle = reactable::colDef("Qué trajo", minWidth = 200, html = TRUE)
        )
      )
    })

    output$frescura <- shiny::renderUI({
      f <- datos$fuentes()
      ultimo <- suppressWarnings(max(f$actualizado, na.rm = TRUE))
      revisado <- suppressWarnings(max(f$revisado, na.rm = TRUE))
      shiny::tagList(
        shiny::div(class = "frescura-grande", bsicons::bs_icon("arrow-repeat"),
                   shiny::span("Se actualiza sola cada ", shiny::strong(cfg$actualizacion$cada_horas, " horas"))),
        shiny::tags$ul(class = "lista-frescura",
          shiny::tags$li("Última revisión de las fuentes: ", shiny::strong(fmt_hace(revisado))),
          shiny::tags$li("Último dato nuevo: ", shiny::strong(fmt_hace(ultimo))),
          shiny::tags$li("Pronóstico de lluvia: se renueva 4 veces al día (ECMWF)."),
          shiny::tags$li("Temperatura del mar y ENFEN: semanal y quincenal."),
          shiny::tags$li("Lluvia observada (sequía): CHIRPS diario preliminar (2–3 días de retraso) cierra cada mes apenas termina; el mes en curso combina lo ya medido con lo estimado.")),
        shiny::p(class = "nota", "Usted no necesita hacer nada: el servidor busca datos nuevos por su cuenta y",
                 "esta página muestra siempre lo último calculado.")
      )
    })

    output$pesos <- reactable::renderReactable({
      w <- cfg$riesgo$pesos
      tabla <- tibble::tibble(
        Amenaza = c("Inundación", "Huaico"),
        Lluvia = c(w$inundacion$lluvia, w$huaico$lluvia), Mar = c(w$inundacion$mar, w$huaico$mar),
        `Exposición` = c(w$inundacion$exposicion, w$huaico$exposicion), `Río` = c(w$inundacion$cauce, w$huaico$cauce)
      )
      reactable::reactable(tabla, compact = TRUE, borderless = TRUE,
                           defaultColDef = reactable::colDef(cell = function(v) if (is.numeric(v)) fmt_pct(v) else v))
    })

    output$nota_clima <- shiny::renderUI({
      datos$version()
      m <- q(datos$db, "SELECT metodo, count(*) AS n FROM clima_diaria GROUP BY metodo")
      if (nrow(m) == 0) return(NULL)
      shiny::p(class = "nota", "Umbrales de lluvia diaria: ", paste(m$metodo, collapse = ", "),
               if (any(grepl("aproximado", m$metodo))) ". Copie el PISCO diario en data/raw/PISCOp_d.nc: la próxima actualización calcula los umbrales exactos.")
    })

    output$nota_analogos <- shiny::renderUI({
      datos$version()
      a <- q(datos$db, "SELECT DISTINCT fuente FROM precip_mensual WHERE pronostico")
      if (nrow(a) == 0) return(NULL)
      shiny::p(class = "nota", "Pronóstico vigente: ", a$fuente[1], ".")
    })

    # Actualización desde la app -------------------------------------------------------
    shiny::observeEvent(input$actualizar, {
      if (!es_administrador) return()
      if (actualizacion_en_curso(cfg$rutas$db)) {
        shiny::showNotification("Ya hay una actualización en curso.", type = "warning")
        return()
      }
      lanzado(as.numeric(Sys.time()))
      lanzar_actualizacion(getwd(), forzar = isTRUE(input$forzar))
      shiny::showNotification("Buscando datos nuevos… puede seguir usando la app.", type = "message")
    })

    # Solo el administrador vigila el proceso (cada 2 s): el público no lo necesita y en
    # la web ese sondeo mantendría la app ocupada mientras haya una pestaña abierta.
    proceso <- if (es_administrador) vigilar_actualizacion(cfg, session) else shiny::reactive(list(corriendo = FALSE))
    corriendo <- shiny::reactive(proceso()$corriendo)
    shiny::observeEvent(corriendo(), {
      if (!corriendo() && lanzado() > 0) shiny::showNotification("La búsqueda de datos nuevos terminó.", type = "message")
    }, ignoreInit = TRUE)

    output$controles <- shiny::renderUI({
      if (as.numeric(Sys.time()) - lanzado() < 20) {
        shiny::invalidateLater(5000)
        return(NULL)
      }
      if (corriendo()) return(NULL)
      shiny::tagList(
        shiny::actionButton(session$ns("actualizar"), "Buscar datos nuevos ahora",
                            icon = bsicons::bs_icon("arrow-repeat"), class = "btn-primary w-100"),
        shiny::checkboxInput(session$ns("forzar"), "Recalcular todo aunque no haya novedades", FALSE)
      )
    })

    output$estado_proceso <- shiny::renderUI({
      if (!es_administrador) return(NULL)
      proceso()
      panel_progreso(cfg, id_detener = session$ns("detener"))
    })
    shiny::observeEvent(input$detener, {
      if (!es_administrador) return()
      detener_actualizacion(cfg$rutas$db)
      shiny::showNotification("Actualización detenida.", type = "warning")
    })
  })
}
