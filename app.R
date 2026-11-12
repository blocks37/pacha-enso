# PACHA ENSO · Monitor de riesgo climático del Perú
# App Shiny de solo lectura: muestra lo que actualizar.R deja en data/db/.
# Shiny carga automáticamente todos los archivos de R/.

# Antes que nada: si un paquete quedó roto (p. ej. rlang viejo tras una actualización
# a medias), avisar con la solución en vez de fallar más adentro.
verificar_paquetes(c("rlang", "vctrs", "tibble", "dplyr", "tidyr", "readr", "stringr", "jsonlite",
                     "DBI", "duckdb", "sf", "leaflet", "echarts4r", "reactable", "bsicons"),
                   avisar_pdftools = FALSE)

library(shiny)
library(bslib)

cfg <- cargar_config()
geo <- cargar_geo(cfg)
# En la web: la base la calcula GitHub Actions y la app solo la descarga (R/datos_remotos.R).
# Se guarda en el disco local del contenedor (más rápido que la carpeta del proyecto).
if (usa_datos_remotos(cfg)) cfg$rutas$db <- file.path(dirname(tempdir()), "pacha_enso_db")
programar_datos_remotos(cfg)
# Una sola conexión de lectura para todas las visitas (se reabre sola si llega una base nueva)
db <- db_lector(cfg$rutas$db)

cabecera <- tags$head(
  tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
  # «?v=fecha»: el navegador no se queda con una copia vieja de los estilos al actualizar la app
  tags$link(rel = "stylesheet", href = paste0("estilos.css?v=", as.integer(file.mtime("www/estilos.css")))),
  tags$script(src = paste0("app.js?v=", as.integer(file.mtime("www/app.js"))))
)

marca <- tags$span(
  class = "marca",
  tags$span(class = "marca-emblema", tags$img(src = "emblema.png", alt = "")),
  tags$span(class = "marca-texto", tags$b("PACHA"), tags$span("ENSO")),
  tags$span(class = "marca-lema d-none d-xl-inline", "Clima · Océano · Territorio")
)

# Pantalla de bienvenida cuando todavía no se ha descargado nada ------------------------
ui_sin_datos <- page_fillable(
  theme = tema_app(cfg), title = "PACHA ENSO", cabecera,
  div(class = "bienvenida",
      tags$img(src = "logo.png", class = "logo-bienvenida", alt = "PACHA ENSO · Clima, océano, territorio"),
      firma_autor(cfg),
      card(
        uiOutput("instrucciones"),
        uiOutput("aviso_pisco"),
        uiOutput("boton", class = "sin-atenuar"),
        uiOutput("progreso", class = "sin-atenuar")
      ),
      aviso_referencial())
)

server_sin_datos <- function(input, output, session) {
  admin <- es_admin(session, cfg)
  output$instrucciones <- renderUI({
    if (!admin) {
      return(tagList(
        card_header(tags$span(bsicons::bs_icon("hourglass-split"), " La plataforma se está preparando")),
        p("Estamos calculando los primeros resultados. Vuelva en unos minutos: esta página se recarga sola cuando estén listos.")
      ))
    }
    tagList(
      card_header(tags$span(bsicons::bs_icon("rocket-takeoff"), " Primera puesta en marcha (administración)")),
      p("Todavía no hay datos. La primera actualización descarga y calcula todo (unos 20–40 minutos)."),
      tags$ol(
        tags$li("Copie el NetCDF de PISCO mensual en ", tags$code(cfg$pisco$mensual), "."),
        tags$li("Pulse el botón. Puede cerrar esta ventana: el proceso sigue solo."),
        tags$li("Cuando termine, esta página se recarga con los datos.")
      )
    )
  })
  output$aviso_pisco <- renderUI({
    if (admin && !file.exists(cfg$pisco$mensual)) {
      div(class = "alert alert-warning", "No se encuentra ", tags$code(cfg$pisco$mensual),
          ". Sin PISCO no se puede calcular la sequía.")
    }
  })
  lanzado <- reactiveVal(0)   # hora del último clic: evita lanzar dos veces mientras arranca
  proceso <- vigilar_actualizacion(cfg, session)
  corriendo <- reactive(proceso()$corriendo)
  output$boton <- renderUI({
    if (!admin) return(NULL)
    arrancando <- as.numeric(Sys.time()) - lanzado() < 20
    if (arrancando) invalidateLater(5000)
    if (!corriendo() && !arrancando) actionButton("arrancar", "Descargar y calcular",
                                                  icon = bsicons::bs_icon("cloud-download"), class = "btn-primary")
    else if (!corriendo()) div(class = "en-curso", span(class = "spinner-border spinner-border-sm"), " Iniciando…")
  })
  observeEvent(input$arrancar, {
    if (admin && !actualizacion_en_curso(cfg$rutas$db)) {
      lanzado(as.numeric(Sys.time()))
      lanzar_actualizacion(getwd())
    }
  })
  observeEvent(input$detener, if (admin) detener_actualizacion(cfg$rutas$db))
  output$progreso <- renderUI({
    proceso()
    if (!is.null(db_actual(cfg$rutas$db))) session$reload()
    if (admin) panel_progreso(cfg, id_detener = "detener")
  })
}

# App completa ------------------------------------------------------------------------------
ui_app <- page_navbar(
  id = "nav",
  title = marca,
  window_title = "PACHA ENSO · Monitor de riesgo climático del Perú",
  theme = tema_app(cfg), lang = "es", fillable = FALSE,
  header = tagList(cabecera, mod_filtros_ui("filtros"), firma_autor(cfg)),
  footer = tags$footer(class = "pie-pagina", aviso_referencial()),
  nav_panel("Panorama", value = "panorama", mod_panorama_ui("panorama"), icon = bsicons::bs_icon("grid-1x2")),
  nav_panel("Inundación y huaico", value = "inundacion", mod_inundacion_ui("inundacion", cfg),
            icon = bsicons::bs_icon("cloud-rain-heavy")),
  nav_panel("Ríos", value = "rios", mod_rios_ui("rios"), icon = bsicons::bs_icon("water")),
  nav_panel("Sequía", value = "sequia", mod_sequia_ui("sequia", cfg), icon = bsicons::bs_icon("brightness-high")),
  nav_panel("Explorar", value = "explorar", mod_series_ui("series"), icon = bsicons::bs_icon("graph-up")),
  nav_panel("Datos y método", value = "datos", mod_datos_ui("datos", cfg), icon = bsicons::bs_icon("database")),
  nav_spacer(),
  nav_item(uiOutput("frescura"))
)

server_app <- function(input, output, session) {
  datos <- datos_compartidos(db, cfg, session)

  # Ficha de distrito: se abre desde cualquier vista
  seleccion <- reactiveVal(NULL)
  abrir_ficha <- function(ubigeo) {
    if (length(ubigeo) == 1 && !is.na(ubigeo) && nzchar(ubigeo)) seleccion(list(ubigeo = ubigeo, t = as.numeric(Sys.time())))
  }
  ir_a <- reactiveVal(NULL)
  ir_a_serie <- function(ubigeo) {
    ir_a(list(ubigeo = ubigeo, t = as.numeric(Sys.time())))
    nav_select("nav", "explorar")
  }

  filtros <- mod_filtros_server("filtros", datos, abrir_ficha)
  mod_panorama_server("panorama", datos, filtros, abrir_ficha, geo, cfg)
  mod_inundacion_server("inundacion", datos, filtros, abrir_ficha, geo, cfg)
  mod_rios_server("rios", datos, filtros, abrir_ficha, geo, cfg)
  mod_sequia_server("sequia", datos, filtros, abrir_ficha, geo, cfg)
  mod_series_server("series", datos, cfg, ir_a, cuencas = geo$cuencas)
  mod_datos_server("datos", datos, cfg)
  mod_ficha_server("ficha", seleccion, datos, cfg, ir_a_serie)

  # Indicador de frescura en la barra: verde < 12 h, ámbar < 2 días, rojo si más
  output$frescura <- renderUI({
    invalidateLater(60000)
    f <- datos$fuentes()
    ultimo <- suppressWarnings(max(f$actualizado, na.rm = TRUE))
    horas <- as.numeric(difftime(Sys.time(), ultimo, units = "hours"))
    clase <- if (!is.finite(horas)) "rojo" else if (horas < 12) "verde" else if (horas < 48) "ambar" else "rojo"
    errores <- sum(f$estado == "error", na.rm = TRUE)
    actionLink("ir_datos", class = paste("frescura", clase), title = "Ver estado de las fuentes",
               label = tagList(span(class = "punto-vivo"), paste("Datos", fmt_hace(ultimo)),
                               if (errores > 0) span(class = "insignia-error", errores)))
  })
  observeEvent(input$ir_datos, nav_select("nav", "datos"))
}

# Se decide en cada visita: así, cuando termina la primera actualización, al
# recargar la página aparece la app completa sin reiniciar R.
hay_datos <- function() !is.null(db_actual(cfg$rutas$db))
shinyApp(
  ui = function(req) if (hay_datos()) ui_app else ui_sin_datos,
  server = function(input, output, session) {
    if (hay_datos()) server_app(input, output, session) else server_sin_datos(input, output, session)
  }
)
