test_that("el índice ENSO se satura y no amplifica en La Niña", {
  expect_equal(indice_enso(2.46, 5.6, cfg$riesgo), 1)
  expect_equal(indice_enso(1, 2, cfg$riesgo), 0.5)
  expect_equal(indice_enso(-1.5, -2, cfg$riesgo), 0)
})

test_that("la lluvia se normaliza contra el clima local", {
  expect_equal(normalizar_lluvia(5, 5, 45), 0)
  expect_equal(normalizar_lluvia(25, 5, 45), 0.5)
  expect_equal(normalizar_lluvia(80, 5, 45), 1)
  expect_equal(normalizar_lluvia(0, 5, 45), 0)
})

fila <- function(...) {
  base <- list(amenaza = "inundacion", lluvia_idx = 1, mar_idx = 0.5, cauce_idx = 1,
               exposicion_idx = 1, enso_idx = 1)
  tibble::as_tibble(utils::modifyList(base, list(...)))
}

test_that("el peor caso de inundación llega a crítico", {
  r <- calcular_riesgo(fila(), cfg$riesgo)
  # 0,55·1 + 0,15·0,5 + 0,15·1 + 0,15·1 = 0,925
  expect_equal(r$riesgo, 0.925)
  expect_equal(r$nivel, "Crítico")
})

test_that("sin lluvia el riesgo nunca sale de «bajo»", {
  r <- calcular_riesgo(fila(lluvia_idx = 0), cfg$riesgo)
  expect_lt(r$riesgo, cfg$riesgo$umbrales$vigilancia)
  expect_equal(r$nivel, "Bajo")
})

test_that("el huaico usa sus propios pesos y su nivel medio", {
  r <- calcular_riesgo(fila(amenaza = "huaico", lluvia_idx = 0.5, enso_idx = 0, exposicion_idx = 0.5,
                            mar_idx = 1, cauce_idx = 1), cfg$riesgo)
  # 0,70·0,5 + 0,20·0,5 = 0,45 (mar y caudal pesan 0 en huaico)
  expect_equal(r$riesgo, 0.45)
  expect_equal(r$nivel, "Huaico medio")
})

test_that("peor_escenario deja una fila por distrito, la de mayor riesgo", {
  df <- tibble::tibble(ubigeo = c("a", "a", "b"), fecha = as.Date("2026-11-01") + 0:2,
                       riesgo = c(0.2, 0.7, 0.5))
  p <- peor_escenario(df)
  expect_equal(nrow(p), 2)
  expect_equal(p$riesgo, c(0.7, 0.5))
})

test_that("a igual riesgo, el distrito más lluvioso va primero", {
  df <- tibble::tibble(ubigeo = c("a", "b", "c"), fecha = as.Date("2026-11-12"),
                       riesgo = c(0.9, 0.9, 0.5), lluvia_mm = c(20, 60, 90))
  expect_equal(peor_escenario(df)$ubigeo, c("b", "a", "c"))
})

test_that("El Niño solo amplifica en la vertiente del Pacífico norte", {
  a <- alcance_enso(codigo_cuenca = c("1378", "1378", "1378", "4997", "0172", NA),
                    lat = c(-5, -10, -14, -5, -15, -4), costero = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
                    cfg_riesgo = cfg$riesgo)
  expect_equal(a, c(1, 0.5, 0, 0, 0, 1))   # Piura sí; Lima a medias; Arequipa, Amazonía y Titicaca no
})

test_that("la exposición a huaico depende del relieve, no de la altitud", {
  e <- indice_exposicion(altitud = c(3900, 3300, 3900), amenaza = rep("huaico", 3), cfg_riesgo = cfg$riesgo,
                         relieve = c(300, 1500, NA))
  expect_equal(e, c(0, 1, acotar((3900 - 1000) / 2500)))   # altiplano plano = 0; sin dato, por altitud
})

test_that("el aporte de El Niño se separa de la lluvia en los componentes", {
  df <- tibble::tibble(ubigeo = "a", distrito = "A", amenaza = "inundacion", lluvia_idx = 0.5,
                       enso_idx = 1, cauce_idx = 0, mar_idx = 0, exposicion_idx = 0)
  k <- componentes_riesgo(df, cfg$riesgo)
  expect_equal(k$lluvia, 100 * 0.55 * 0.5)
  expect_equal(round(k$enso, 3), round(100 * 0.55 * 0.5 * 0.15, 3))
})
