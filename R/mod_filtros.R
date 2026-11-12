# Barra superior: región (macrorregión o departamento → provincia → distrito) y buscador ----
# Un solo filtro global, la región, que se arma en cascada. Cada vista maneja su
# propio horizonte de tiempo.

mod_filtros_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "barra-filtros",
    shiny::div(class = "filtro-region",
               bsicons::bs_icon("geo-alt"),
               shiny::selectizeInput(ns("region"), NULL, choices = c("Todo el Perú" = "pais"), width = "100%"),
               shiny::selectizeInput(ns("provincia"), NULL, choices = NULL, width = "100%",
                                     options = list(placeholder = "Provincia")),
               shiny::selectizeInput(ns("distrito"), NULL, choices = NULL, width = "100%",
                                     options = list(placeholder = "Distrito"))),
    shiny::div(class = "atajos-region",
               shiny::actionLink(ns("r_pais"), "Perú"),
               shiny::actionLink(ns("r_norte"), "Norte"),
               shiny::actionLink(ns("r_centro"), "Centro"),
               shiny::actionLink(ns("r_sur"), "Sur"),
               shiny::actionLink(ns("r_oriente"), "Oriente")),
    shiny::div(class = "filtro-buscar",
               bsicons::bs_icon("search"),
               shiny::selectizeInput(ns("buscar"), NULL, choices = NULL, width = "100%",
                                     options = list(placeholder = "Ficha de un distrito o UBIGEO…",
                                                    maxOptions = 30)))
  )
}

#' @return lista con `region()` (reactivo: "pais", macrorregión, "dep:CC",
#'   "prov:CCPP" o "dis:UBIGEO") y `fijar_region(valor)`.
mod_filtros_server <- function(id, datos, abrir_ficha) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      d <- datos$distritos()
      deps <- dplyr::distinct(d, cod_dep, departamento) |> dplyr::arrange(departamento)
      shiny::updateSelectizeInput(session, "region", selected = shiny::isolate(input$region) %||% "pais", choices = list(
        "Perú" = c("Todo el Perú" = "pais"),
        "Macrorregión" = c("Norte" = "norte", "Centro" = "centro", "Sur" = "sur", "Oriente" = "oriente"),
        "Departamento" = stats::setNames(paste0("dep:", deps$cod_dep), deps$departamento)
      ))
      opciones <- stats::setNames(d$ubigeo, sprintf("%s · %s, %s (%s)", d$distrito, d$provincia, d$departamento, d$ubigeo))
      shiny::updateSelectizeInput(session, "buscar", choices = opciones, selected = character(0), server = TRUE)
    })

    # Departamento elegido (o NULL si se eligió el país o una macrorregión)
    dep <- shiny::reactive({
      r <- input$region %||% ""
      if (startsWith(r, "dep:")) sub("^dep:", "", r) else NULL
    })

    # Provincias del departamento; al cambiar de departamento se limpian provincia y distrito
    shiny::observeEvent(input$region, {
      d <- datos$distritos()
      if (is.null(dep())) {
        shiny::updateSelectizeInput(session, "provincia", choices = character(0), selected = character(0),
                                    options = list(placeholder = "Provincia"))
        shiny::updateSelectizeInput(session, "distrito", choices = character(0), selected = character(0),
                                    options = list(placeholder = "Distrito"))
        return()
      }
      p <- d[substr(d$ubigeo, 1, 2) == dep(), ] |>
        dplyr::distinct(cod = substr(ubigeo, 1, 4), provincia) |> dplyr::arrange(provincia)
      shiny::updateSelectizeInput(session, "provincia", selected = character(0),
                                  choices = c("Todas las provincias" = "", stats::setNames(p$cod, p$provincia)),
                                  options = list(placeholder = "Todas las provincias"))
    }, ignoreInit = FALSE)

    # Distritos de la provincia (o de todo el departamento si no hay provincia)
    shiny::observeEvent(list(input$region, input$provincia), {
      if (is.null(dep())) return()
      d <- datos$distritos()
      prefijo <- if (nzchar(input$provincia %||% "")) input$provincia else dep()
      x <- d[startsWith(d$ubigeo, prefijo), ] |> dplyr::arrange(distrito)
      shiny::updateSelectizeInput(session, "distrito", selected = character(0),
                                  choices = c("Todos los distritos" = "", stats::setNames(x$ubigeo, x$distrito)),
                                  options = list(placeholder = "Todos los distritos"))
    })

    for (r in c("pais", "norte", "centro", "sur", "oriente")) local({
      valor <- r
      shiny::observeEvent(input[[paste0("r_", valor)]], shiny::updateSelectizeInput(session, "region", selected = valor))
    })

    shiny::observeEvent(input$buscar, {
      if (nzchar(input$buscar %||% "")) {
        abrir_ficha(input$buscar)
        shiny::updateSelectizeInput(session, "buscar", selected = character(0))
      }
    })

    region <- shiny::reactive({
      if (!is.null(dep()) && nzchar(input$distrito %||% "")) return(paste0("dis:", input$distrito))
      if (!is.null(dep()) && nzchar(input$provincia %||% "")) return(paste0("prov:", input$provincia))
      if (nzchar(input$region %||% "")) input$region else "pais"
    })

    list(
      region = shiny::debounce(region, 150),   # evita tres cálculos seguidos al cambiar en cascada
      fijar_region = function(valor) shiny::updateSelectizeInput(session, "region", selected = valor)
    )
  })
}
