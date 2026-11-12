test_that("sumar_meses y meses_entre cruzan años", {
  expect_equal(sumar_meses(as.Date("2025-11-01"), 3), as.Date("2026-02-01"))
  expect_equal(sumar_meses(as.Date("2026-01-01"), -1), as.Date("2025-12-01"))
  expect_equal(meses_entre(as.Date("2025-10-01"), as.Date(c("2025-11-01", "2026-04-01"))), c(1L, 6L))
})

test_that("agregar_distritos promedia por peso e ignora celdas sin dato", {
  pesos <- tibble::tibble(ubigeo = c("a", "a", "b"), lon = 0, lat = 0, peso = c(0.75, 0.25, 1))
  valores <- matrix(c(10, 20, 5,      # mes 1
                      NA, 40, 6), ncol = 2)
  m <- agregar_distritos(valores, pesos)
  expect_equal(unname(m["a", 1]), 12.5)
  expect_equal(unname(m["a", 2]), 40)       # la celda vacía no cuenta
  expect_equal(unname(m["b", ]), c(5, 6))
})

test_that("la versión de GFS cambia cada 6 horas con 5 h de retraso", {
  expect_equal(version_gfs(as.POSIXct("2026-11-12 10:59", tz = "UTC")), "GFS 2026-11-12 00 UTC")
  expect_equal(version_gfs(as.POSIXct("2026-11-12 11:01", tz = "UTC")), "GFS 2026-11-12 06 UTC")
  expect_equal(version_gfs(as.POSIXct("2026-11-12 13:59", tz = "UTC"), etiqueta = "ECMWF", retraso_h = 8),
               "ECMWF 2026-11-12 00 UTC")
})

test_that("el factor PISCO/CHIRPS se limita a 0,5–2", {
  pisco <- tibble::tibble(ubigeo = "a", mes = seq(as.Date("2024-01-01"), by = "month", length.out = 12), pp_mm = 300)
  chirps <- dplyr::mutate(pisco, pp_mm = 100)
  f <- factor_ajuste(pisco, chirps)
  expect_equal(nrow(f), 12)
  expect_true(all(f$factor == 2))
})

test_that("los años análogos repiten la lluvia de años con el mismo ENSO", {
  meses <- seq(as.Date("1981-01-01"), as.Date("2025-12-01"), by = "month")
  anio <- as.integer(format(meses, "%Y"))
  oni <- tibble::tibble(fecha = meses, valor = ifelse(anio %in% c(1982, 1997, 2015), 2.2, 0))
  precip <- tibble::tibble(ubigeo = "a", mes = meses,
                           pp_mm = ifelse(anio %in% c(1983, 1998, 2016), 10, 100))   # el año siguiente al Niño es seco
  oni <- dplyr::bind_rows(oni, tibble::tibble(fecha = as.Date("2026-10-01"), valor = 2.4))
  p <- pronostico_analogos(precip, oni, seq(as.Date("2026-11-01"), by = "month", length.out = 4))
  expect_equal(p$analogos, c(1982, 1997, 2015))
  expect_equal(p$datos$pp_mm, c(100, 100, 10, 10))   # nov–dic del año Niño, ene–feb del siguiente
  expect_true(all(p$datos$pronostico))
})

test_that("los ríos del ENFEN se cruzan con los distritos por río o por cuenca", {
  expect_equal(clave_rio(c("Chancay - Lambayeque", "Zaña", "La Leche")), c("chancaylambayeque", "zana", "laleche"))
  claves <- c("tumbes|tumbes", "chancay|reque|chancaylambayeque", "chancay|chancayhuaral", NA)
  enfen <- clave_rio(c("Tumbes", "Chancay-Lambayeque"))
  expect_equal(cauce_en_alerta(claves, enfen), c(1, 1, 0, 0))   # Huaral no se confunde con Lambayeque
  expect_equal(cauce_en_alerta(claves, character()), c(0, 0, 0, 0))
})

test_that("el resumen de ríos elige el río más largo como principal", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(ubigeo = c("1", "1", "1"), rio = c("Chira", "Chipillico", "Seca"),
                                  tipo = c("Río", "Río", "Quebrada"), cuenca = c("Chira", "Chira", "Chira"),
                                  km = c(22, 10, 30)), tmp)
  r <- resumen_rios(tmp)
  expect_equal(r$rio, "Chira")
  expect_equal(r$km_rio, 32)
  expect_true(grepl("chira", r$claves_rio))
})

test_that("la lluvia de cuenca promedia aguas arriba y se compara con su clima", {
  cuencas <- list(
    unidades = tibble::tibble(codigo = c("4997", "4998", "4999", "1378"), nombre = c("Bajo", "Trib", "Alto", "Piura"),
                              region = c("Amazonas", "Amazonas", "Amazonas", "Pacífico"), area_km2 = c(10, 10, 10, 10),
                              aguas_arriba = c("4997|4998|4999", "4998", "4999", "1378")),
    distritos = tibble::tibble(ubigeo = c("a", "b", "c", "d"), codigo = c("4997", "4998", "4999", "1378"), fraccion = 1)
  )
  distritos <- tibble::tibble(ubigeo = c("a", "b", "c", "d"), area_km2 = c(100, 100, 200, 100))
  m <- miembros_cuenca(cuencas, distritos, c("4997", "1378"))
  expect_setequal(m$ubigeo[m$codigo_rio == "4997"], c("a", "b", "c"))       # incluye lo de aguas arriba
  expect_equal(m$ubigeo[m$codigo_rio == "1378"], "d")                        # en la costa, solo su cuenca

  gfs <- tibble::tibble(ubigeo = rep(c("a", "b", "c", "d"), each = 3), fecha = rep(as.Date("2026-11-12") + 0:2, 4),
                        pp_mm = c(0, 0, 0, 0, 0, 0, 40, 40, 40, 5, 5, 5))
  clima <- tibble::tibble(ubigeo = c("a", "b", "c", "d"), p50_mm = 5, p99_mm = 45)
  lc <- lluvia_cuenca(gfs, clima, m)
  bajo <- dplyr::filter(lc, codigo_rio == "4997", fecha == as.Date("2026-11-14"))
  expect_equal(bajo$cuenca_mm, 20)       # 40 mm en «c», que pesa la mitad del área
  expect_gt(bajo$cuenca_idx, 0)          # aunque en el distrito «a» no llueve nada
  expect_equal(reduccion_areal(c(10, 1e4, 1e8)), c(1, 0.7, 0.5))
})

test_that("la lluvia en la cuenca evita el techo de «sin lluvia»", {
  base <- tibble::tibble(amenaza = "inundacion", lluvia_idx = 0, mar_idx = 0, cauce_idx = 1,
                         exposicion_idx = 1, enso_idx = 0)
  sin <- calcular_riesgo(dplyr::mutate(base, cuenca_idx = 0, cauce_idx = 0), cfg$riesgo)
  con <- calcular_riesgo(dplyr::mutate(base, cuenca_idx = 1), cfg$riesgo)
  expect_lt(sin$riesgo, cfg$riesgo$umbrales$vigilancia)
  expect_equal(con$riesgo, 0.30)         # 0,15 río + 0,15 exposición
})

test_that("la lluvia del distrito interpola las 4 esquinas de la grilla", {
  e <- esquinas_grilla(tibble::tibble(ubigeo = "a", lat = -5.0625, lon = -80.6875), 0.25)
  expect_equal(nrow(e), 4)
  expect_equal(sum(e$peso), 1)
  expect_equal(e$peso[e$lat == -5 & e$lon == -80.75], 0.75 * 0.75)   # esquina más cercana: 75 % en cada eje
})

test_that("las direcciones del CHIRPS diario se arman con la fecha", {
  cfg <- list(chirps = list(diario = list(url = "https://x.org/prelim/sat/",
                                          archivos = list("{Y}/chirps-v3.0.prelim.{Y}.{m}.{d}.tif"))))
  expect_equal(urls_chirps_diario(as.Date("2026-11-03"), cfg),
               "https://x.org/prelim/sat/2026/chirps-v3.0.prelim.2026.11.03.tif")
})

test_that("sin PISCO diario la huella lo dice y no falla", {
  cfg <- list(pisco = list(diario = tempfile(fileext = ".nc"), clima_diaria = tempfile(), dsc_mensual = tempfile()))
  expect_equal(version_pisco_diario(cfg), "sin archivo")
})
