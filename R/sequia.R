# Índices de sequía -----------------------------------------------------------
# SPI con el paquete SPEI (gamma), con parámetros ajustados solo al periodo de
# referencia: los meses pronosticados se miden con la misma vara. Se calcula para
# todos los distritos a la vez (una matriz meses × distritos).

CATEGORIAS_SEQUIA <- c("Sequía extrema", "Sequía severa", "Sequía moderada",
                       "Anormalmente seco", "Normal o húmedo")
CATEGORIA_ARIDO <- "Árido (no aplica)"

#' Categoría OMM para SPI/SPEI: −2,0 es extrema; −1,5 severa; −1,0 moderada.
categoria_sequia <- function(x) {
  cut(x, breaks = c(-Inf, -2, -1.5, -1, -0.5, Inf), labels = CATEGORIAS_SEQUIA, right = TRUE)
}

#' Racha más larga de días secos (lluvia < umbral) en un vector diario.
dias_secos_consecutivos <- function(pp_diaria, umbral = 1) {
  seco <- !is.na(pp_diaria) & pp_diaria < umbral
  if (!any(seco)) return(0L)
  rachas <- rle(seco)
  as.integer(max(rachas$lengths[rachas$values]))
}

#' Pasa una tabla larga (ubigeo, mes, valor) a matriz meses × distritos sin huecos de meses.
a_matriz <- function(df, columna) {
  meses <- seq(min(df$mes), max(df$mes), by = "month")
  ub <- sort(unique(df$ubigeo))
  m <- matrix(NA, length(meses), length(ub), dimnames = list(format(meses), ub))
  m[cbind(match(df$mes, meses), match(df$ubigeo, ub))] <- df[[columna]]
  m
}

#' Rellena huecos con la mediana del mismo mes del año en el periodo de referencia.
imputar_mediana <- function(m, meses, ref) {
  mm <- as.integer(format(meses, "%m"))
  en_ref <- en_referencia(meses, ref)
  for (k in 1:12) {
    filas <- which(mm == k)
    med <- apply(m[filas[en_ref[filas]], , drop = FALSE], 2, stats::median, na.rm = TRUE)
    sub <- m[filas, , drop = FALSE]
    huecos <- which(is.na(sub), arr.ind = TRUE)
    if (nrow(huecos) > 0) sub[huecos] <- med[huecos[, 2]]
    m[filas, ] <- sub
  }
  m
}

#' SPI de todas las columnas de una matriz mensual.
spi_matriz <- function(m, meses, escala, ref) {
  inicio <- c(as.integer(format(meses[1], "%Y")), as.integer(format(meses[1], "%m")))
  serie <- stats::ts(m, start = inicio, frequency = 12)
  utils::capture.output(ajuste <- suppressWarnings(suppressMessages(SPEI::spi(
    serie, scale = escala, na.rm = TRUE,
    ref.start = c(ref[1], 1), ref.end = c(ref[2], 12)
  ))))
  x <- matrix(as.numeric(ajuste$fitted), nrow = nrow(m), dimnames = dimnames(m))
  # Con muchos meses en 0 mm (costa árida) la gamma da ±Inf o NaN. Se acota a ±3,5
  # (más allá no hay diferencia práctica: −2 ya es «extrema») y NaN pasa a NA.
  x[is.nan(x)] <- NA
  acotar(x, -3.5, 3.5)
}

#' Meses en que el SPI no se interpreta: la lluvia normal de esa ventana (k meses
#' que terminan en ese mes del año) es casi nula. Con 0 mm en un mes que casi nunca
#' llueve, la gamma da −3 o menos, una «sequía extrema» que no existe.
#' Umbral: cfg_sequia$pp_minima_ventana_mm (mm por mes de ventana; 8 por defecto).
estacion_seca <- function(pp, meses, k, ref, cfg_sequia) {
  minimo <- (cfg_sequia$pp_minima_ventana_mm %||% 8) * k
  acum <- apply(pp, 2, function(v) as.numeric(stats::filter(v, rep(1, k), sides = 1)))
  acum <- matrix(acum, nrow = nrow(pp), dimnames = dimnames(pp))
  mm <- as.integer(format(meses, "%m"))
  en_ref <- en_referencia(meses, ref)
  clima <- matrix(vapply(1:12, function(m) colMeans(acum[mm == m & en_ref, , drop = FALSE], na.rm = TRUE),
                         numeric(ncol(pp))), nrow = 12, byrow = TRUE)   # 12 × distritos
  seca <- clima < minimo
  seca[is.na(seca)] <- FALSE
  seca[mm, , drop = FALSE]                   # meses × distritos
}

#' Índices de sequía por distrito y mes.
#'
#' @param precip ubigeo, mes, pp_mm, pronostico, dsc (dsc puede faltar)
#' @return lista: `indices` (tabla larga) y `pp_anual` (lluvia anual media de referencia por distrito)
indices_sequia <- function(precip, cfg_sequia, ref = c(1991, 2020)) {
  pp <- a_matriz(precip, "pp_mm")
  meses <- as.Date(rownames(pp))
  pron <- a_matriz(dplyr::mutate(precip, pronostico = as.numeric(pronostico)), "pronostico")
  ultimo_obs <- max(precip$mes[!precip$pronostico])
  pron[is.na(pron)] <- as.numeric(meses > ultimo_obs)[row(pron)[is.na(pron)]]
  pp <- imputar_mediana(pp, meses, ref)

  ub <- colnames(pp)
  largo <- function(m) as.vector(m)
  salida <- tibble::tibble(
    ubigeo = rep(ub, each = length(meses)),
    mes = rep(meses, times = length(ub)),
    pronostico = largo(pron) > 0
  )
  for (k in cfg_sequia$escalas_spi) {
    log_info("SPI-", k, " para ", length(ub), " distritos")
    avanzar("Calculando sequía (SPI)", sprintf("SPI-%d para %d distritos", k, length(ub)),
            match(k, cfg_sequia$escalas_spi), length(cfg_sequia$escalas_spi))
    x <- spi_matriz(pp, meses, k, ref)
    x[estacion_seca(pp, meses, k, ref, cfg_sequia)] <- NA
    salida[[paste0("spi", k)]] <- round(largo(x), 2)
  }
  salida[[paste0("spei", cfg_sequia$escala_spei)]] <- NA_real_   # requiere ET0 (PISCOeo_pm)

  dsc <- if ("dsc" %in% names(precip) && any(!is.na(precip$dsc))) {
    largo(a_matriz(precip, "dsc"))
  } else NA_integer_
  salida$dsc <- as.integer(dsc)

  pp_anual <- colSums(pp[en_referencia(meses, ref), , drop = FALSE]) / (ref[2] - ref[1] + 1)
  list(indices = salida, pp_anual = tibble::tibble(ubigeo = ub, pp_anual = round(pp_anual)))
}

#' Riesgo compuesto de sequía (0–1). Reparte el peso de los componentes que falten.
riesgo_sequia <- function(spi3, spei6, dsc, nino34, cfg_sequia) {
  f_indice <- function(x) acotar((-0.5 - x) / 1.5)  # −0,5 → 0 ; −2,0 → 1
  w <- cfg_sequia$pesos
  partes <- cbind(
    w$spi3 * f_indice(spi3), w$spei6 * f_indice(spei6),
    w$dsc * acotar(dsc / cfg_sequia$dsc_max), w$nino34 * acotar(nino34 / cfg_sequia$nino34_saturacion)
  )
  presentes <- cbind(w$spi3 * !is.na(spi3), w$spei6 * !is.na(spei6),
                     w$dsc * !is.na(dsc), w$nino34 * !is.na(nino34))
  round(rowSums(partes, na.rm = TRUE) / pmax(rowSums(presentes), 1e-6), 3)
}

#' Tabla final de sequía: riesgo, categoría y máscara de distritos áridos.
construir_sequia <- function(resultado, nino34_mensual, cfg_sequia) {
  aridos <- resultado$pp_anual$ubigeo[resultado$pp_anual$pp_anual < cfg_sequia$pp_anual_minima]
  resultado$indices |>
    dplyr::left_join(nino34_mensual, by = "mes") |>
    dplyr::mutate(
      riesgo = riesgo_sequia(spi3, spei6, dsc, nino34, cfg_sequia),
      categoria = as.character(categoria_sequia(spi3)),
      arido = ubigeo %in% aridos,
      riesgo = ifelse(arido, NA_real_, riesgo),
      categoria = ifelse(arido, CATEGORIA_ARIDO, categoria)
    ) |>
    dplyr::select(-nino34, -arido)
}
