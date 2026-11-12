# Relieve (desnivel) de cada distrito ------------------------------------------------------
#
# Para el huaico importa qué tan empinado es el terreno, no qué tan alto: el altiplano
# está a 3 800 m pero es plano. Se toma la altitud de ~15 puntos repartidos dentro de
# cada distrito (data/geo/puntos_relieve.csv) en los mosaicos públicos de elevación
# Terrarium (AWS Open Data, SRTM/ALOS, ~300 m por píxel a zoom 9; sin clave ni cupo) y
# relieve = percentil 90 − percentil 10 de esas altitudes, en metros.
# Se calcula una sola vez (≈260 mosaicos, 1–2 min) y queda en data/geo/relieve_distrito.csv.
#
# Valores de control (noviembre 2026): Lima 86 m · Piura 35 · Ayaviri 511 ·
# Huari 915 · Huancayo 1 218.

calcular_relieve <- function(cfg, z = 9) {
  pts <- readr::read_csv(file.path(cfg$rutas$geo, "puntos_relieve.csv"),
                         col_types = readr::cols(ubigeo = "c", lat = "d", lon = "d"))
  n <- 2^z
  fx <- (pts$lon + 180) / 360 * n
  lr <- pts$lat * pi / 180
  fy <- (1 - log(tan(lr) + 1 / cos(lr)) / pi) / 2 * n
  pts$tx <- floor(fx)
  pts$ty <- floor(fy)
  pts$px <- pmin(255, floor((fx %% 1) * 256))
  pts$py <- pmin(255, floor((fy %% 1) * 256))
  pts$elev <- NA_real_
  pts$elev_inv <- NA_real_

  mosaicos <- unique(pts[c("tx", "ty")])
  url_base <- cfg$relieve_url %||% "https://s3.amazonaws.com/elevation-tiles-prod/terrarium"
  for (k in seq_len(nrow(mosaicos))) {
    avanzar("Calculando el relieve de los distritos", sprintf("mosaico %d de %d (solo la primera vez)", k, nrow(mosaicos)),
            k, nrow(mosaicos))
    tmp <- tempfile(fileext = ".png")
    ok <- tryCatch({
      peticion(sprintf("%s/%d/%d/%d.png", url_base, z, mosaicos$tx[k], mosaicos$ty[k]), cfg$http) |>
        httr2::req_perform(path = tmp)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) next
    v <- terra::values(terra::rast(tmp))            # 65 536 × 3 (rojo, verde, azul), por filas
    sel <- which(pts$tx == mosaicos$tx[k] & pts$ty == mosaicos$ty[k])
    elev <- function(fila) { i <- fila * 256 + pts$px[sel] + 1; v[i, 1] * 256 + v[i, 2] + v[i, 3] / 256 - 32768 }
    pts$elev[sel] <- elev(pts$py[sel])
    pts$elev_inv[sel] <- elev(255 - pts$py[sel])
  }
  # Un PNG sin coordenadas puede leerse con las filas invertidas (terra/GDAL). Los
  # puntos están todos en tierra: la orientación correcta es la que casi no da
  # altitudes bajo el nivel del mar (con las filas invertidas, Lima salía en −137 m).
  bajo_mar <- function(x) mean(x < -20, na.rm = TRUE)
  if (bajo_mar(pts$elev_inv) < bajo_mar(pts$elev)) pts$elev <- pts$elev_inv
  log_info("Relieve: ", round(100 * bajo_mar(pts$elev), 2), " % de puntos bajo el mar (debería ser ~0)")

  pts |>
    dplyr::filter(!is.na(elev)) |>
    dplyr::group_by(ubigeo) |>
    dplyr::summarise(alt_min = round(min(elev)), alt_max = round(max(elev)),
                     relieve = round(stats::quantile(elev, 0.9, names = FALSE) -
                                       stats::quantile(elev, 0.1, names = FALSE)),
                     n = dplyr::n(), .groups = "drop")
}
