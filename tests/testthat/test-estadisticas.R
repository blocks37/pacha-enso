serie_prueba <- function(tendencia = 0, semilla = 7) {
  set.seed(semilla)
  mes <- seq(as.Date("1981-01-01"), as.Date("2024-12-01"), by = "month")
  anio <- as.integer(format(mes, "%Y")) - 1981
  base <- 50 + 40 * cos(2 * pi * (as.integer(format(mes, "%m")) - 2) / 12)
  tibble::tibble(mes = mes, pp_mm = pmax(0, base + tendencia * anio / 12 + stats::rnorm(length(mes), 0, 8)))
}

test_that("la climatología tiene 12 meses con p10 ≤ p50 ≤ p90", {
  cl <- climatologia_mensual(serie_prueba())
  expect_equal(nrow(cl), 12)
  expect_true(all(cl$p10 <= cl$p50 & cl$p50 <= cl$p90))
})

test_that("el percentil del último mes está entre 0 y 100", {
  p <- percentil_ultimo_mes(serie_prueba())
  expect_gte(p, 0)
  expect_lte(p, 100)
})

test_that("Mann-Kendall detecta una tendencia impuesta", {
  skip_if_not_installed("trend")
  t_sube <- tendencia_anual(serie_prueba(tendencia = 3))
  t_plana <- tendencia_anual(serie_prueba(tendencia = 0))
  expect_gt(t_sube$pendiente_decada, 0)
  expect_lt(t_sube$p_valor, 0.05)
  expect_gt(t_plana$p_valor, t_sube$p_valor)
})

test_that("la correlación recupera una relación conocida con el ENSO", {
  s <- serie_prueba()
  enso <- tibble::tibble(mes = s$mes, valor = stats::rnorm(nrow(s)))
  s$pp_mm <- pmax(0, s$pp_mm - 15 * enso$valor)          # más ENSO, menos lluvia
  r <- correlacion_enso(s, enso, meses = 1:12, desfases = 0:1)
  expect_lt(r$r[r$desfase == 0], -0.5)
  expect_gt(r$r[r$desfase == 1], r$r[r$desfase == 0])
})

test_that("el periodo de retorno crece con el déficit", {
  s <- serie_prueba()
  t20 <- periodo_retorno_deficit(s, 20, meses = c(12, 1, 2))
  t50 <- periodo_retorno_deficit(s, 50, meses = c(12, 1, 2))
  expect_gt(t50, t20)
})
