# ENFEN: estado del sistema de alerta, ríos con caudal alto y nivel del mar ---
#
# El ENFEN no publica una API. Fuente principal: SENAMHI. Su API JSON de la página de
# El Niño da el estado vigente y la fecha («Alerta de El Niño Costero», «30 de
# octubre del 2026»); la página enlaza el PDF del último comunicado oficial, y de ese
# PDF (paquete pdftools) se leen los ríos con caudal sobre lo normal. Respaldo: la página del ENFEN en IMARPE, que desde algunas
# redes no resuelve («Could not resolve host: enfen.imarpe.gob.pe»).
# Si el formato cambia, la función falla con un mensaje claro y la app sigue con lo
# último que tenía.

# Ríos de la vertiente del Pacífico que el ENFEN suele nombrar en sus comunicados.
RIOS_COSTA_NORTE <- c("Tumbes", "Zarumilla", "Chira", "Piura", "La Leche", "Motupe",
                      "Chancay-Lambayeque", "Zaña", "Jequetepeque", "Chicama", "Moche", "Virú",
                      "Chao", "Santa", "Lacramarca", "Nepeña", "Casma", "Culebras", "Huarmey",
                      "Fortaleza", "Pativilca", "Supe", "Huaura", "Chancay-Huaral", "Chillón",
                      "Rímac", "Lurín", "Mala", "Cañete", "San Juan", "Pisco", "Ica", "Acarí",
                      "Yauca", "Ocoña", "Camaná", "Majes", "Chili", "Tambo", "Moquegua",
                      "Locumba", "Sama", "Caplina")

ESTADOS_ENFEN <- c(
  "Alerta de El Niño Costero", "Alerta de La Niña Costera",
  "Vigilancia de El Niño Costero", "Vigilancia de La Niña Costera",
  "No activo"
)

# «superiores a lo normal», «por encima de lo normal», «sobre su normal»…, buscado
# en el texto sin espacios para que no importe cómo cortó las palabras el PDF.
SOBRE_NORMAL <- "(sobre|encimade|superior(es)?a|mayor(es)?a)(lo|su|sus)(valores?)?normal"

normalizar_texto <- function(x) stringr::str_squish(tolower(stringi::stri_trans_general(x, "Latin-ASCII")))

#' Estado del ENFEN mencionado en un texto (el primero de ESTADOS_ENFEN que aparezca).
estado_enfen <- function(texto) {
  t <- normalizar_texto(paste(texto, collapse = " "))
  hallado <- ESTADOS_ENFEN[vapply(normalizar_texto(ESTADOS_ENFEN), grepl, logical(1), x = t, fixed = TRUE)]
  if (length(hallado) == 0) stop("No se reconoce el estado del ENFEN: ¿cambió el formato de la página?")
  hallado[1]
}

#' Ríos con caudal sobre lo normal y nivel del mar, leídos oración por oración.
lecturas_enfen <- function(texto) {
  oraciones <- unlist(strsplit(stringr::str_squish(paste(texto, collapse = " ")), "(?<=[.!?])\\s+", perl = TRUE))
  n <- normalizar_texto(oraciones)
  junto <- gsub("[^a-z0-9]", "", n)
  sobre <- grepl(SOBRE_NORMAL, junto)
  caudal <- n[sobre & grepl("caudal", junto, fixed = TRUE)]
  # Por palabra completa: «Ica» no debe coincidir con «Pacífico».
  patron <- function(r) paste0("\\b", gsub("[- ]+", "[- ]*", normalizar_texto(r)), "\\b")
  rios <- RIOS_COSTA_NORTE[vapply(RIOS_COSTA_NORTE, function(r) any(grepl(patron(r), caudal, perl = TRUE)), logical(1))]
  list(rios = rios, mar_sobre_normal = any(sobre & grepl("niveldelmar|nivelmediodelmar", junto)))
}

#' «N° 19-2026» a partir del texto del comunicado.
numero_comunicado <- function(texto) {
  m <- stringr::str_match(gsub("\\s+", "", paste(texto, collapse = " ")),
                          stringr::regex("COMUNICADOOFICIALENFENN\\D{0,3}(\\d{1,2})-(\\d{4})", ignore_case = TRUE))
  if (is.na(m[1, 1])) NA_character_ else sprintf("N° %s-%s", m[1, 2], m[1, 3])
}

# SENAMHI ---------------------------------------------------------------------------

#' Página de El Niño de SENAMHI. El estado y la fecha los pinta JavaScript (Vue) desde
#' una API JSON; la página en sí trae «{{ page.conditionDescription }}». Por eso el
#' estado se lee de la API y de la página solo los enlaces a los PDF del comunicado.
pagina_senamhi <- function(url, cfg_http, url_api = NULL) {
  api <- NULL
  if (!is.null(url_api)) {
    api <- tryCatch(jsonlite::fromJSON(leer_texto(url_api, cfg_http))$data, error = function(e) {
      log_aviso("ENFEN: la API de SENAMHI no respondió (", conditionMessage(e), ")")
      NULL
    })
  }
  analizar_pagina_senamhi(leer_texto(url, cfg_http), url, api)
}

#' @param api lista con conditionDescription y date (API de SENAMHI), o NULL
analizar_pagina_senamhi <- function(contenido, url, api = NULL) {
  html <- rvest::read_html(contenido)
  if (length(api$conditionDescription) == 1 && nzchar(api$conditionDescription)) {
    estado <- estado_enfen(api$conditionDescription)
    fecha <- if (length(api$date) == 1) api$date else NA_character_
  } else {
    # Por si algún día vuelve a venir escrito en la página (sin plantillas {{ }})
    titulos <- rvest::html_text2(rvest::html_elements(html, "h3, h4"))
    titulos <- titulos[!grepl("{{", titulos, fixed = TRUE)]
    condicion <- grep("Condiciones actuales", titulos, value = TRUE, ignore.case = TRUE)
    if (length(condicion) == 0) stop("SENAMHI no trae el estado del ENFEN (ni en la API ni en la página)")
    estado <- estado_enfen(condicion[1])
    fecha <- stringr::str_match(paste(titulos, collapse = " "), "Actualizado al ([^)]+)")[1, 2]
  }

  enlaces <- rvest::html_elements(html, "a")
  href <- rvest::html_attr(enlaces, "href")
  rotulo <- paste(rvest::html_attr(enlaces, "title"), rvest::html_text2(enlaces))
  pdf <- href[!is.na(href) & grepl("\\.pdf", href, ignore.case = TRUE) &
                grepl("Comunicado Oficial", rotulo, ignore.case = TRUE)]
  list(estado = estado, fecha = fecha,
       pdf = if (length(pdf)) url_absoluta(pdf[1], url) else NA_character_)
}

#' Enlace relativo («../../load/file/x.pdf») a dirección completa.
url_absoluta <- function(href, base) {
  if (grepl("^https?://", href)) return(href)
  raiz <- sub("^(https?://[^/?#]+).*", "\\1", base)
  if (startsWith(href, "/")) return(paste0(raiz, href))
  carpeta <- sub("[^/]*$", "", sub("[?#].*", "", sub("^https?://[^/?#]+", "", base)))
  pila <- character()
  for (parte in strsplit(paste0(carpeta, href), "/")[[1]]) {
    if (parte == "..") pila <- utils::head(pila, -1)
    else if (!parte %in% c(".", "")) pila <- c(pila, parte)
  }
  paste0(raiz, "/", paste(pila, collapse = "/"))
}

#' Texto del PDF del comunicado; NA (con aviso) si no se puede leer.
texto_pdf <- function(url, cfg_http) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    log_aviso("ENFEN: falta el paquete pdftools; se lee el estado pero no los ríos del comunicado")
    return(NA_character_)
  }
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  tryCatch({
    peticion(url, cfg_http, timeout_s = 120) |> httr2::req_perform(path = tmp)
    paste(pdftools::pdf_text(tmp), collapse = "\n")
  }, error = function(e) {
    log_aviso("ENFEN: no se pudo leer el PDF del comunicado (", conditionMessage(e), ")")
    NA_character_
  })
}

enfen_desde_senamhi <- function(url, cfg_http, url_api = NULL) {
  p <- pagina_senamhi(url, cfg_http, url_api)
  texto <- if (!is.na(p$pdf)) texto_pdf(p$pdf, cfg_http) else NA_character_
  lect <- if (!is.na(texto)) lecturas_enfen(texto) else list(rios = character(), mar_sobre_normal = FALSE)
  list(estado = p$estado, rios = lect$rios, mar_sobre_normal = lect$mar_sobre_normal,
       comunicado = if (!is.na(texto)) numero_comunicado(texto) else NA_character_,
       fecha = p$fecha, origen = if (!is.na(p$pdf)) p$pdf else url)
}

# IMARPE (respaldo) -------------------------------------------------------------------

texto_enfen <- function(url, cfg_http) {
  leer_texto(url, cfg_http) |> rvest::read_html() |> rvest::html_text2()
}

enfen_desde_imarpe <- function(url, cfg_http) {
  texto <- texto_enfen(url, cfg_http)
  lect <- lecturas_enfen(texto)
  list(estado = estado_enfen(texto), rios = lect$rios, mar_sobre_normal = lect$mar_sobre_normal,
       comunicado = numero_comunicado(texto), fecha = NA_character_, origen = url)
}

# Lo que usa la actualización ------------------------------------------------------------

#' Prueba SENAMHI y, si falla, el respaldo de IMARPE.
con_respaldo <- function(cfg_fuentes, principal, respaldo) {
  res <- tryCatch(principal(cfg_fuentes$enfen), error = function(e) e)
  if (!inherits(res, "error")) return(res)
  if (is.null(cfg_fuentes$enfen_respaldo)) stop(res)
  log_aviso("ENFEN: ", cfg_fuentes$enfen, " no respondió (", conditionMessage(res), "); se prueba ", cfg_fuentes$enfen_respaldo)
  respaldo(cfg_fuentes$enfen_respaldo)
}

#' Huella barata: estado + fecha + nombre del PDF (sin descargarlo).
version_enfen <- function(cfg_fuentes, cfg_http) {
  con_respaldo(
    cfg_fuentes,
    function(url) {
      p <- pagina_senamhi(url, cfg_http, cfg_fuentes$enfen_api)
      partes <- c(p$estado, p$fecha, if (is.na(p$pdf)) NA_character_ else basename(p$pdf))
      paste(partes[!is.na(partes)], collapse = " · ")
    },
    function(url) rlang::hash(texto_enfen(url, cfg_http))
  )
}

#' @return lista con estado, rios, mar_sobre_normal, comunicado, fecha y origen
#'   (los textos que falten quedan como "" para que el JSON no guarde «NA»).
fetch_enfen <- function(cfg_fuentes, cfg_http) {
  e <- con_respaldo(cfg_fuentes,
                    function(url) enfen_desde_senamhi(url, cfg_http, cfg_fuentes$enfen_api),
                    function(url) enfen_desde_imarpe(url, cfg_http))
  for (k in c("comunicado", "fecha", "origen")) if (length(e[[k]]) == 0 || is.na(e[[k]])) e[[k]] <- ""
  e
}
