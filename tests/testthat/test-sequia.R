test_that("las categorías siguen los cortes de la OMM", {
  cat <- as.character(categoria_sequia(c(-2.5, -2, -1.7, -1.2, -0.7, 0.3)))
  expect_equal(cat, c("Sequía extrema", "Sequía extrema", "Sequía severa",
                      "Sequía moderada", "Anormalmente seco", "Normal o húmedo"))
})

test_that("la racha seca cuenta días seguidos bajo el umbral", {
  expect_equal(dias_secos_consecutivos(c(0, 0, 5, 0, 0, 0, 2)), 3L)
  expect_equal(dias_secos_consecutivos(c(3, 4, 5)), 0L)
  expect_equal(dias_secos_consecutivos(c(NA, 0, 0)), 2L)
})

serie_sintetica <- function(n_distritos = 3, semilla = 1) {
  set.seed(semilla)
  mes <- seq(as.Date("1981-01-01"), as.Date("2025-12-01"), by = "month")
  purrr::map_dfr(seq_len(n_distritos), function(i) {
    media <- (60 + 40 * cos(2 * pi * seq_along(mes) / 12)) * i
    tibble::tibble(ubigeo = sprintf("%06d", i), mes = mes,
                   pp_mm = stats::rgamma(length(mes), shape = 2, rate = 2 / media),
                   pronostico = FALSE, dsc = NA_integer_)
  })
}

test_that("el SPI tiene media ≈ 0 y desvío ≈ 1 en el periodo de referencia", {
  skip_if_not_installed("SPEI")
  res <- indices_sequia(serie_sintetica(), cfg$sequia, ref = c(1991, 2020))
  ref <- dplyr::filter(res$indices, format(mes, "%Y") >= "1991", format(mes, "%Y") <= "2020")
  expect_lt(abs(mean(ref$spi3, na.rm = TRUE)), 0.15)
  expect_lt(abs(stats::sd(ref$spi3, na.rm = TRUE) - 1), 0.15)
  expect_equal(nrow(res$pp_anual), 3)
})

test_that("los meses sin dato se rellenan y quedan marcados como pronóstico", {
  skip_if_not_installed("SPEI")
  s <- serie_sintetica(1)
  s <- dplyr::filter(s, mes <= as.Date("2025-10-01"))
  futuro <- tibble::tibble(ubigeo = "000001", mes = as.Date("2026-01-01"), pp_mm = 50, pronostico = TRUE, dsc = NA_integer_)
  res <- indices_sequia(dplyr::bind_rows(s, futuro), cfg$sequia)$indices
  nov <- dplyr::filter(res, mes == as.Date("2025-11-01"))
  expect_true(nov$pronostico)
  expect_false(is.na(nov$spi3))
})

test_that("el riesgo de sequía crece con el déficit y reparte pesos faltantes", {
  expect_equal(riesgo_sequia(0, 0, 0, 0, cfg$sequia), 0)
  expect_equal(riesgo_sequia(-2.5, -2.5, 30, 2.5, cfg$sequia), 1)
  expect_equal(riesgo_sequia(-2.5, NA, 30, 2.5, cfg$sequia), 1)
})

test_that("los distritos áridos no reciben categoría de sequía", {
  res <- list(
    indices = tibble::tibble(ubigeo = c("a", "b"), mes = as.Date("2025-01-01"), pronostico = FALSE,
                             spi1 = -2, spi3 = -2, spi6 = -2, spi12 = -2, spei6 = NA_real_, dsc = NA_integer_),
    pp_anual = tibble::tibble(ubigeo = c("a", "b"), pp_anual = c(20, 600))
  )
  s <- construir_sequia(res, tibble::tibble(mes = as.Date("2025-01-01"), nino34 = 1), cfg$sequia)
  expect_equal(s$categoria, c(CATEGORIA_ARIDO, "Sequía extrema"))
  expect_true(is.na(s$riesgo[1]))
})

test_that("el SPI no se interpreta en la estación seca", {
  meses <- seq(as.Date("1991-01-01"), as.Date("2020-12-01"), by = "month")
  mm <- as.integer(format(meses, "%m"))
  pp <- matrix(ifelse(mm %in% 6:8, 1, 80), ncol = 1, dimnames = list(as.character(meses), "a"))   # jun–ago casi sin lluvia
  seca <- estacion_seca(pp, meses, 1, c(1991, 2020), list(pp_minima_ventana_mm = 8))
  expect_true(all(seca[mm %in% 6:8, 1]))
  expect_false(any(seca[mm %in% c(1, 12), 1]))
})
