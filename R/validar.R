# Contratos de datos: si una fuente cambia de formato, el paso falla antes de
# escribir en la base y la app conserva lo último bueno de esa fuente.

validar_enso <- function(df) {
  df |>
    pointblank::col_vals_not_null(c(fecha, indice, valor)) |>
    pointblank::col_vals_between(valor, -6, 8) |>
    pointblank::col_vals_in_set(indice, c("oni", "nino12", "nino34"))
}

validar_lluvia <- function(df) {
  df |>
    pointblank::col_vals_not_null(c(ubigeo, pp_mm)) |>
    pointblank::col_vals_between(pp_mm, 0, 3000)     # mm por día o por mes
}

validar_riesgo <- function(df) {
  df |>
    pointblank::col_vals_not_null(c(ubigeo, fecha, riesgo, nivel)) |>
    pointblank::col_vals_between(c(riesgo, lluvia_idx, mar_idx, cauce_idx, exposicion_idx), 0, 1) |>
    pointblank::rows_distinct(c(ubigeo, fecha))
}

validar_sequia <- function(df) {
  df |>
    pointblank::col_vals_between(c(spi1, spi3, spi6, spi12), -10, 10, na_pass = TRUE) |>
    pointblank::col_vals_between(riesgo, 0, 1, na_pass = TRUE) |>
    pointblank::rows_distinct(c(ubigeo, mes))
}
