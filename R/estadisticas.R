# Estadísticas por región -----------------------------------------------------
# Todas reciben una serie mensual (mes, pp_mm) y devuelven tablas pequeñas
# que la app muestra o deja descargar.

#' Percentiles 10, 50 y 90 de cada mes del año en el periodo de referencia.
climatologia_mensual <- function(serie, ref = c(1991, 2020)) {
  serie |>
    dplyr::filter(en_referencia(mes, ref)) |>
    dplyr::mutate(m = as.integer(format(mes, "%m"))) |>
    dplyr::group_by(m) |>
    dplyr::summarise(
      p10 = stats::quantile(pp_mm, 0.10, na.rm = TRUE),
      p50 = stats::quantile(pp_mm, 0.50, na.rm = TRUE),
      p90 = stats::quantile(pp_mm, 0.90, na.rm = TRUE),
      media = mean(pp_mm, na.rm = TRUE),
      sd = stats::sd(pp_mm, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Serie con su banda normal, anomalía en % y anomalía estandarizada.
serie_con_anomalias <- function(serie, ref = c(1991, 2020)) {
  clima <- climatologia_mensual(serie, ref)
  serie |>
    dplyr::mutate(m = as.integer(format(mes, "%m"))) |>
    dplyr::left_join(clima, by = "m") |>
    dplyr::mutate(
      anomalia_pct = 100 * (pp_mm - media) / pmax(media, 0.1),
      anomalia_std = (pp_mm - media) / pmax(sd, 0.1)
    )
}

#' Percentil (0–100) del último mes respecto a los mismos meses de referencia.
percentil_ultimo_mes <- function(serie, ref = c(1991, 2020)) {
  serie <- dplyr::filter(serie, !is.na(pp_mm))
  ultimo <- serie[which.max(serie$mes), ]
  m <- format(ultimo$mes, "%m")
  hist <- serie$pp_mm[en_referencia(serie$mes, ref) & format(serie$mes, "%m") == m]
  if (length(hist) < 10) return(NA_real_)
  100 * stats::ecdf(hist)(ultimo$pp_mm)
}

#' Tendencia de la lluvia anual: Mann-Kendall + pendiente de Sen (mm por década).
tendencia_anual <- function(serie) {
  anual <- serie |>
    dplyr::mutate(anio = as.integer(format(mes, "%Y"))) |>
    dplyr::group_by(anio) |>
    dplyr::filter(dplyr::n() == 12) |>              # solo años completos
    dplyr::summarise(pp = sum(pp_mm), .groups = "drop")
  if (nrow(anual) < 10) {
    return(tibble::tibble(pendiente_decada = NA_real_, p_valor = NA_real_, n_anios = nrow(anual)))
  }
  mk <- trend::mk.test(anual$pp)
  sen <- trend::sens.slope(anual$pp)
  tibble::tibble(
    pendiente_decada = 10 * unname(sen$estimates),
    p_valor = unname(mk$p.value),
    n_anios = nrow(anual)
  )
}

#' Correlación entre la anomalía de lluvia y un índice ENSO con desfases de 0 a 3 meses.
#' Un desfase k compara la lluvia del mes t con el índice del mes t − k.
correlacion_enso <- function(serie, enso_mensual, meses = c(12, 1, 2), desfases = 0:3,
                             ref = c(1991, 2020)) {
  anom <- serie_con_anomalias(serie, ref) |>
    dplyr::select(mes, anomalia_std)
  purrr::map_dfr(desfases, function(k) {
    indice <- enso_mensual |>
      dplyr::mutate(mes = sumar_meses(mes, k)) |>
      dplyr::rename(indice = valor)
    datos <- anom |>
      dplyr::inner_join(indice, by = "mes") |>
      dplyr::filter(as.integer(format(mes, "%m")) %in% meses) |>
      tidyr::drop_na()
    if (nrow(datos) < 10) return(tibble::tibble(desfase = k, r = NA_real_, p_valor = NA_real_, n = nrow(datos)))
    prueba <- stats::cor.test(datos$anomalia_std, datos$indice, method = "spearman", exact = FALSE)
    tibble::tibble(desfase = k, r = unname(prueba$estimate), p_valor = prueba$p.value, n = nrow(datos))
  })
}

#' Lluvia de la temporada por fase del ENSO (compuestos).
compuestos_enso <- function(serie, oni_mensual, meses = c(12, 1, 2), ref = c(1991, 2020)) {
  serie_con_anomalias(serie, ref) |>
    dplyr::inner_join(dplyr::rename(oni_mensual, oni = valor), by = "mes") |>
    dplyr::filter(as.integer(format(mes, "%m")) %in% meses) |>
    dplyr::mutate(fase = fase_enso(oni)) |>
    dplyr::group_by(fase) |>
    dplyr::summarise(
      n = dplyr::n(),
      anomalia_media = mean(anomalia_pct, na.rm = TRUE),
      p25 = stats::quantile(anomalia_pct, 0.25, na.rm = TRUE),
      p75 = stats::quantile(anomalia_pct, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Periodo de retorno (años) de un déficit de temporada, con ajuste Gumbel de mínimos
#' por el método de momentos. `deficit_pct` es positivo: 30 = 30 % bajo lo normal.
periodo_retorno_deficit <- function(serie, deficit_pct, meses = c(12, 1, 2), ref = c(1991, 2020)) {
  temporada <- serie_con_anomalias(serie, ref) |>
    dplyr::filter(as.integer(format(mes, "%m")) %in% meses) |>
    dplyr::mutate(anio = as.integer(format(mes, "%Y")) + (as.integer(format(mes, "%m")) == 12)) |>
    dplyr::group_by(anio) |>
    dplyr::filter(dplyr::n() == length(meses)) |>
    dplyr::summarise(anom = mean(anomalia_pct), .groups = "drop")
  if (nrow(temporada) < 15) return(NA_real_)
  x <- -temporada$anom                      # déficit como valor positivo
  beta <- stats::sd(x) * sqrt(6) / pi
  mu <- mean(x) - 0.5772 * beta
  prob_exceder <- -expm1(-exp(-(deficit_pct - mu) / beta))   # 1 − F(x), estable para colas
  1 / prob_exceder
}

#' Meses en sequía (SPI ≤ umbral) por década.
frecuencia_sequia <- function(sequia_df, columna = "spi3", umbral = -1) {
  sequia_df |>
    dplyr::filter(!pronostico) |>
    dplyr::mutate(decada = 10 * (as.integer(format(mes, "%Y")) %/% 10)) |>
    dplyr::group_by(decada) |>
    dplyr::summarise(meses_sequia = sum(.data[[columna]] <= umbral, na.rm = TRUE),
                     .groups = "drop")
}

#' Mensualiza un índice ENSO (promedio de las observaciones de cada mes).
enso_a_mensual <- function(enso_df, nombre) {
  enso_df |>
    dplyr::filter(indice == nombre) |>
    dplyr::mutate(mes = inicio_mes(fecha)) |>
    dplyr::group_by(mes) |>
    dplyr::summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop")
}
