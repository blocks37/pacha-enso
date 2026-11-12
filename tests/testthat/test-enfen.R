# Textos tomados de la página de El Niño de SENAMHI y del Comunicado Oficial ENFEN
# N° 19-2026 (30 de octubre de 2026), tal como los entrega pdftools (con cortes raros).

pagina <- '<html><body>
  <h2 class="senamhi-titulo">Fenómeno el Niño</h2>
  <h3>Condiciones actuales de El Niño/La Niña: <span style="color: rgb(255, 0, 49);">Alerta de El Niño Costero</span></h3>
  <h4>(Actualizado al 30 de octubre del 2026)</h4>
  <p>EL NIÑO COSTERO CONTINUARÍA HASTA FINES DE OTOÑO DE 2027</p>
  <p>Definiciones: No activo, Vigilancia de La Niña Costera…</p>
  <a href="../../load/file/02262SENA-77.pdf">Pronóstico Estacional Noviembre - Enero 2027</a>
  <h3 style="font-size: 1rem;">Último Comunicado Oficial del ENFEN</h3>
  <a href="../../load/file/02204SENA-225.pdf" title="ENFEN - Comunicado Oficial"><img src="x.gif"> Descargar PDF</a>
</body></html>'

comunicado <- paste(
  "COMUNICADO OFICIAL ENFEN N°19-2026",
  "30 octubre 2026",
  "Alerta de El Niño Costero 1",
  "EL NIÑO COSTERO CONTINUARÍA HASTA FINES DE OTOÑO DE 2027",
  "Los caudales registrarían valores superiores a l o normal8 en los ríos de la costa norte,",
  "principalmente Tumbes, Chira, Piura, La Leche y Jequetepeque. Esta tendencia se acentuaría a partir de",
  "diciembre hacia la costa central, afectando las cuencas de los ríos Chillón, Rímac y Lurín. En contraste,",
  "en la sierra sur oriental y sierra centro oriental los caudales serían de normal a inferior. Por otro lado,",
  "los ríos tributarios del Lago Titicaca continuarían con caudales por debajo de lo normal.",
  "En el Pacífico, Ica y Pisco no se mencionan con caudales.",
  sep = "\n")

test_that("la página de SENAMHI da el estado, la fecha y el PDF del comunicado", {
  p <- analizar_pagina_senamhi(pagina, "https://www.senamhi.gob.pe/?p=fenomeno-el-nino")
  expect_equal(p$estado, "Alerta de El Niño Costero")       # no la de las definiciones
  expect_equal(p$fecha, "30 de octubre del 2026")
  expect_equal(p$pdf, "https://www.senamhi.gob.pe/load/file/02204SENA-225.pdf")
})

test_that("con la página real (plantilla Vue) el estado sale de la API", {
  plantilla <- sub("<span[^<]*>Alerta de El Niño Costero</span>",
                   "<span :style=\"{ color: page.conditionColor }\">{{ page.conditionDescription }}</span>", pagina)
  plantilla <- sub("(Actualizado al 30 de octubre del 2026)", "(Actualizado al {{ page.date }})", plantilla, fixed = TRUE)
  api <- jsonlite::fromJSON('{"status":true,"data":{"conditionColor":"#ff0031",
    "conditionDescription":"Alerta de El Niño Costero","date":"30 de octubre del 2026"}}')$data
  p <- analizar_pagina_senamhi(plantilla, "https://www.senamhi.gob.pe/?p=fenomeno-el-nino", api)
  expect_equal(p$estado, "Alerta de El Niño Costero")
  expect_equal(p$fecha, "30 de octubre del 2026")
  expect_match(p$pdf, "02204SENA-225.pdf$")
  expect_error(analizar_pagina_senamhi(plantilla, "https://www.senamhi.gob.pe/"), "no trae el estado")
})

test_that("el SPI se acota a ±3,5 y los NaN quedan como NA", {
  meses <- seq(as.Date("1991-01-01"), as.Date("2020-12-01"), by = "month")
  m <- matrix(c(rep(0, length(meses) - 1), 50), ncol = 1, dimnames = list(as.character(meses), "a"))
  x <- spi_matriz(m, meses, 1, c(1991, 2020))
  expect_true(all(is.na(x) | (x >= -3.5 & x <= 3.5)))
})

test_that("del comunicado salen los ríos con caudal sobre lo normal", {
  l <- lecturas_enfen(comunicado)
  expect_equal(l$rios, c("Tumbes", "Chira", "Piura", "La Leche", "Jequetepeque"))
  expect_false(l$mar_sobre_normal)
  expect_equal(numero_comunicado(comunicado), "N° 19-2026")
  expect_equal(numero_comunicado("COMUNICADO OFICIAL ENFEN Nº 1 9 - 2026"), "N° 19-2026")
})

test_that("el nivel del mar y otras formas de decir «sobre lo normal»", {
  t <- "El nivel medio del mar se mantuvo por encima de lo normal. El caudal del río Chancay - Lambayeque estuvo sobre su normal."
  l <- lecturas_enfen(t)
  expect_true(l$mar_sobre_normal)
  expect_equal(l$rios, "Chancay-Lambayeque")
  expect_equal(estado_enfen("Sistema de alerta: NO ACTIVO"), "No activo")
  expect_error(estado_enfen("texto sin estado"), "No se reconoce")
})

test_that("CHIRPS usa la v3.0 de Latinoamérica sin comprimir", {
  expect_equal(url_chirps(as.Date("2026-09-01"), cfg),
               "https://data.chc.ucsb.edu/products/CHIRPS/v3.0/monthly/latam/tifs/chirps-v3.0.2026.09.tif")
  cfg_v2 <- cfg
  cfg_v2$chirps$url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/"
  cfg_v2$chirps$archivo <- "chirps-v2.0.%s.tif.gz"
  expect_match(url_chirps(as.Date("2024-03-01"), cfg_v2), "tifs/chirps-v2.0.2024.03.tif.gz$")
})
