# Actualización automática ----------------------------------------------------------
#
# 1. Pregunta a cada fuente «¿tienes algo nuevo?» (huella: fecha de modificación,
#    hash o número de corrida). Es rápido y no descarga datos.
# 2. Si nada cambió, termina. Si algo cambió, crea una versión nueva de la base,
#    descarga solo lo nuevo, valida, recalcula lo que depende de eso y publica.
# 3. Si una fuente falla, se anota el error y se sigue con las demás: la app
#    conserva lo último bueno de esa fuente.

FUENTES <- list(
  oni = list(
    nombre = "NOAA · ONI", que = "Índice Oceánico de El Niño", frecuencia = "Mensual",
    version = function(cfg) version_http(cfg$fuentes$oni, cfg$http)
  ),
  nino_semanal = list(
    nombre = "NOAA · Niño semanal", que = "Temperatura del mar Niño 1+2 y 3.4", frecuencia = "Semanal",
    version = function(cfg) version_http(cfg$fuentes$nino_semanal, cfg$http)
  ),
  enfen = list(
    nombre = "ENFEN · vía SENAMHI", que = "Estado de alerta, ríos y nivel del mar", frecuencia = "Quincenal",
    version = function(cfg) version_enfen(cfg$fuentes, cfg$http)
  ),
  pisco = list(
    nombre = "SENAMHI · PISCO", que = "Lluvia mensual histórica por distrito", frecuencia = "Cuando SENAMHI publica",
    version = function(cfg) version_archivo(cfg$pisco$mensual)
  ),
  pisco_diario = list(
    nombre = "SENAMHI · PISCO diario", que = "Umbrales de lluvia extrema y rachas secas por distrito",
    frecuencia = "Cuando SENAMHI publica",
    version = function(cfg) version_pisco_diario(cfg)
  ),
  chirps = list(
    nombre = "CHIRPS mensual", que = "Lluvia de los meses que PISCO aún no cubre", frecuencia = "Mensual",
    version = function(cfg) format(chirps_ultimo_mes(cfg), "%Y-%m")
  ),
  chirps_diario = list(
    nombre = "CHIRPS diario", que = "Lluvia de las últimas semanas (preliminar, 2–3 días de retraso)",
    frecuencia = "Diaria",
    version = function(cfg) format(chirps_diario_ultimo_dia(cfg))
  ),
  gfs = list(
    nombre = "ECMWF IFS · Open-Meteo", que = "Lluvia diaria pronosticada, 14 días", frecuencia = "Cada 6 horas",
    version = function(cfg) version_gfs(etiqueta = cfg$gfs$etiqueta %||% "GFS", retraso_h = cfg$gfs$retraso_h %||% 5)
  )
)

# Si cambia la forma de calcular (sequía o riesgo), la próxima actualización
# recalcula todo sola, sin marcar «Recalcular todo». Cambiar este texto al tocar el método.
METODO_SEQUIA <- "2026-11-12 · SPI sin estación seca · ECMWF interpolado bilineal · relieve"
leer_metodo_sequia <- function(cfg) {
  ruta <- file.path(cfg$rutas$db, "METODO_SEQUIA")
  if (file.exists(ruta)) readLines(ruta, n = 1, warn = FALSE, encoding = "UTF-8") else ""
}
escribir_metodo_sequia <- function(cfg) writeLines(METODO_SEQUIA, file.path(cfg$rutas$db, "METODO_SEQUIA"), useBytes = TRUE)

#' Punto de entrada. `forzar = TRUE` recalcula todo aunque nada haya cambiado.
actualizar_todo <- function(cfg = cargar_config(), forzar = FALSE) {
  dir.create(cfg$rutas$db, showWarnings = FALSE, recursive = TRUE)
  options(enso.log = cfg$rutas$log)
  candado <- tomar_candado(cfg$rutas$db)
  if (is.null(candado)) {
    log_aviso("Ya hay otra actualización en curso; no se hace nada.")
    return(invisible(NULL))
  }
  on.exit(unlink(candado), add = TRUE)
  log_info("===== Actualización iniciada =====")
  avanzar("Revisando paquetes de R", "")
  verificar_paquetes()

  vigente <- db_actual(cfg$rutas$db)
  previo <- estado_previo(vigente)
  metodo_nuevo <- !identical(leer_metodo_sequia(cfg), METODO_SEQUIA)
  if (metodo_nuevo && !is.null(vigente)) {
    log_info("Cambió el método de cálculo: se recalcula todo esta vez.")
    forzar <- TRUE
  }

  # 1. ¿Qué hay de nuevo? --------------------------------------------------------
  versiones <- lapply(seq_along(FUENTES), function(k) {
    id <- names(FUENTES)[k]
    log_info("Revisando ", FUENTES[[id]]$nombre, "…")
    avanzar("Revisando si hay datos nuevos", FUENTES[[id]]$nombre, k, length(FUENTES))
    v <- tryCatch(list(valor = FUENTES[[id]]$version(cfg), error = NULL),
                  error = function(e) list(valor = NULL, error = conditionMessage(e)))
    anterior <- previo$version[previo$fuente == id]
    estado_ant <- previo$estado[previo$fuente == id]
    v$cambio <- is.null(v$error) && (forzar || length(anterior) == 0 || !identical(v$valor, anterior) ||
                                       identical(estado_ant, "error"))
    log_info(sprintf("  %-14s %s", id, if (!is.null(v$error)) paste("ERROR:", v$error)
                     else if (v$cambio) paste("nuevo:", v$valor) else "sin cambios"))
    v
  })
  names(versiones) <- names(FUENTES)
  escribir_revision(cfg$rutas$db, versiones)

  if (!any(vapply(versiones, `[[`, logical(1), "cambio"))) {
    log_info("Sin novedades. La base vigente sigue igual.")
    avanzar("Sin novedades", "la base vigente sigue igual")
    return(invisible(versiones))
  }

  # 2. Versión nueva de la base ----------------------------------------------------
  db_publicar(cfg$rutas$db, conservar = cfg$actualizacion$bases_a_conservar, function(con) {
    avanzar("Preparando distritos", "límites INEI, ríos y altitud")
    distritos <- preparar_distritos(cfg, con)
    db_reemplazar(con, "dim_distrito", distritos)
    cambio <- function(id) isTRUE(versiones[[id]]$cambio)
    resultado <- list()

    paso <- function(id, expr) {
      res <- tryCatch(list(ok = TRUE, valor = force(expr)),
                      error = function(e) list(ok = FALSE, mensaje = conditionMessage(e)))
      if (!res$ok) log_error(id, ": ", res$mensaje)
      registrar_fuente(con, id, versiones[[id]], res)
      resultado[[id]] <<- res$ok
      if (res$ok) res$valor else NULL
    }

    # ENSO
    if (cambio("oni")) paso("oni", {
      avanzar("Descargando", "NOAA · ONI")
      d <- validar_enso(fetch_oni(cfg$fuentes$oni, cfg$http))
      db_upsert(con, "enso_indice", d, c("fecha", "indice"))
      list(mensaje = sprintf("ONI hasta %s: %s °C", fmt_mes(max(d$fecha)), fmt_signo(d$valor[which.max(d$fecha)], 2)))
    })
    if (cambio("nino_semanal")) paso("nino_semanal", {
      avanzar("Descargando", "NOAA · Niño semanal")
      d <- validar_enso(fetch_nino_semanal(cfg$fuentes$nino_semanal, cfg$http))
      db_upsert(con, "enso_indice", d, c("fecha", "indice"))
      list(mensaje = sprintf("Semana del %s", fmt_fecha(max(d$fecha))))
    })
    if (cambio("enfen")) paso("enfen", {
      avanzar("Descargando", "ENFEN · comunicado oficial")
      e <- fetch_enfen(cfg$fuentes, cfg$http)
      e$claves <- clave_rio(e$rios)
      log_info("ENFEN: ", e$estado, if (nzchar(e$comunicado)) paste0(" (comunicado ", e$comunicado, ")"),
               if (length(e$rios)) paste0(" · ríos sobre lo normal: ", paste(e$rios, collapse = ", ")))
      list(mensaje = paste(c(e$estado, e$comunicado, e$fecha)[nzchar(c(e$estado, e$comunicado, e$fecha))], collapse = " · "),
           detalle = as.character(jsonlite::toJSON(e, auto_unbox = TRUE)))
    })

    # PISCO diario: umbrales exactos y rachas secas (se procesa una sola vez)
    if (cambio("pisco_diario")) paso("pisco_diario", {
      avanzar("Revisando PISCO diario", basename(cfg$pisco$diario %||% ""))
      pisco_diario_al_dia(cfg)
    })

    # Lluvia mensual y sequía
    if (cambio("pisco") || cambio("pisco_diario") || cambio("chirps") || cambio("chirps_diario") ||
        cambio("oni") || tabla_vacia(con, "sequia")) {
      actualizar_lluvia_y_sequia(con, cfg, versiones, paso)
    }

    # Pronóstico diario y riesgo de inundación
    if (cambio("gfs") || cambio("oni") || cambio("nino_semanal") || cambio("enfen") ||
        cambio("pisco") || cambio("pisco_diario") || tabla_vacia(con, "riesgo")) {
      actualizar_riesgo(con, cfg, distritos, cambio("gfs"), paso)
    }

    # Fuentes sin cambios: solo se anota que se revisaron
    for (id in names(FUENTES)) {
      if (!cambio(id) && is.null(resultado[[id]])) registrar_fuente(con, id, versiones[[id]], NULL)
    }
    avanzar("Guardando", "publicando la versión nueva de la base")
    resultado
  })
  escribir_metodo_sequia(cfg)
  log_info("===== Actualización terminada =====")
  avanzar("Terminada", format(Sys.time(), "%d/%m %H:%M"))
  invisible(versiones)
}

# Pasos ------------------------------------------------------------------------------

actualizar_lluvia_y_sequia <- function(con, cfg, versiones, paso) {
  ref <- unlist(cfg$climatologia)
  # Si la fuente cambió se registra como novedad; si no, solo se usan sus datos.
  usar <- function(id, expr) {
    if (isTRUE(versiones[[id]]$cambio)) paso(id, expr)
    else tryCatch(force(expr), error = function(e) { log_error(id, ": ", conditionMessage(e)); NULL })
  }

  pisco <- usar("pisco", {
    avanzar("Leyendo PISCO", basename(cfg$pisco$mensual))
    p <- pisco_mensual_distrital(cfg)
    list(datos = p, mensaje = sprintf("%s a %s · %d distritos", fmt_mes(min(p$mes)), fmt_mes(max(p$mes)),
                                      dplyr::n_distinct(p$ubigeo)))
  })$datos
  if (is.null(pisco)) {
    pisco <- q_con(con, "SELECT ubigeo, mes, pp_mm FROM precip_mensual WHERE fuente = 'PISCO'")
    if (nrow(pisco) == 0) return(log_error("Sin PISCO no se puede calcular la sequía."))
  }

  # CHIRPS: mensual definitivo y, después, diario preliminar (cierra los meses recientes
  # y da la lluvia del mes en curso)
  chirps <- NULL; en_curso <- NULL
  if (is.null(versiones$chirps$error)) {
    hasta <- as.Date(paste0(versiones$chirps$valor, "-01"))
    hasta_dia <- if (is.null(versiones$chirps_diario$error) && !is.null(versiones$chirps_diario$valor))
      as.Date(versiones$chirps_diario$valor) else NA
    res <- usar("chirps", {
      d <- chirps_completar(pisco, cfg, hasta, hasta_dia)
      m <- d$mensual
      list(datos = d, mensaje = if (is.null(m) || nrow(m) == 0) "PISCO ya cubre hasta el último mes"
           else sprintf("%s a %s", fmt_mes(min(m$mes)), fmt_mes(max(m$mes))))
    })$datos
    chirps <- res$mensual; en_curso <- res$en_curso
    if (isTRUE(versiones$chirps_diario$cambio)) paso("chirps_diario", {
      dias <- dplyr::filter(chirps %||% tibble::tibble(fuente = character()), fuente == "CHIRPS diario ajustado")
      partes <- c(
        if (nrow(dias)) paste("cierra", paste(fmt_mes(sort(unique(dias$mes))), collapse = ", ")),
        if (!is.null(en_curso)) sprintf("%s medido al %s", fmt_mes(en_curso$mes[1]), fmt_fecha(max(en_curso$hasta)))
      )
      list(mensaje = if (length(partes)) paste(partes, collapse = " · ") else "sin días nuevos")
    })
  }
  if (is.null(chirps)) {
    chirps <- q_con(con, "SELECT ubigeo, mes, pp_mm, fuente FROM precip_mensual
                          WHERE fuente LIKE 'CHIRPS%' AND NOT pronostico")
    chirps <- dplyr::filter(chirps, mes > max(pisco$mes))
  }

  observado <- dplyr::bind_rows(dplyr::mutate(pisco, fuente = "PISCO"), chirps) |>
    dplyr::mutate(pronostico = FALSE)
  dsc <- dsc_pisco(cfg)
  observado <- if (is.null(dsc)) dplyr::mutate(observado, dsc = NA_integer_)
               else dplyr::left_join(observado, dsc, by = c("ubigeo", "mes"))

  # Pronóstico por análogos ENSO desde el mes siguiente al último observado
  avanzar("Pronóstico de lluvia", "años análogos del ENSO")
  oni <- q_con(con, "SELECT fecha, valor FROM enso_indice WHERE indice = 'oni' ORDER BY fecha")
  futuros <- seq(sumar_meses(max(observado$mes), 1),
                 sumar_meses(inicio_mes(Sys.Date()), cfg$pronostico$meses), by = "month")
  pron <- tryCatch(pronostico_analogos(observado, oni, futuros, cfg$pronostico$min_analogos),
                   error = function(e) { log_error("Pronóstico por análogos: ", conditionMessage(e)); NULL })
  # Mes en curso: lo ya medido por CHIRPS diario + lo que falta del mes según los análogos
  if (!is.null(pron) && !is.null(en_curso) && nrow(en_curso)) {
    hasta_ec <- max(en_curso$hasta)
    dias_mes <- as.integer(sumar_meses(en_curso$mes[1], 1) - en_curso$mes[1])
    resto <- max(0, dias_mes - max(en_curso$dias)) / dias_mes         # días sin medir, por estimar
    pron$datos <- pron$datos |>
      dplyr::left_join(dplyr::select(en_curso, ubigeo, mes, medido = pp_mm), by = c("ubigeo", "mes")) |>
      dplyr::mutate(
        fuente = ifelse(is.na(medido), fuente, sprintf("CHIRPS diario al %s + análogos ENSO", format(hasta_ec, "%d/%m"))),
        pp_mm = ifelse(is.na(medido), pp_mm, round(medido + pp_mm * resto, 1))
      ) |>
      dplyr::select(-medido)
    log_info(sprintf("Mes en curso: lluvia medida hasta el %s (falta %.0f %% del mes, estimado con análogos)",
                     format(hasta_ec), 100 * resto))
  }
  precip <- dplyr::bind_rows(observado, if (!is.null(pron)) dplyr::mutate(pron$datos, dsc = NA_integer_))
  avanzar("Guardando lluvia mensual", sprintf("%s filas", format(nrow(precip), big.mark = " ")))
  db_reemplazar(con, "precip_mensual", precip)

  log_info("Calculando SPI (puede tardar unos minutos)…")
  res <- indices_sequia(precip, cfg$sequia, ref)
  # Niño 3.4 mensual; para los meses sin dato se repite el último valor (persistencia)
  nino34 <- q_con(con, "SELECT fecha, indice, valor FROM enso_indice WHERE indice = 'nino34'") |>
    enso_a_mensual("nino34")
  ultimo_n34 <- if (nrow(nino34)) utils::tail(nino34$valor, 1) else NA_real_
  nino34 <- dplyr::bind_rows(
    nino34,
    tibble::tibble(mes = setdiff_fechas(unique(precip$mes), nino34$mes), valor = ultimo_n34)
  ) |>
    dplyr::rename(nino34 = valor)
  sequia <- tryCatch({
    d <- validar_sequia(construir_sequia(res, nino34, cfg$sequia))
    db_reemplazar(con, "sequia", d)
    d
  }, error = function(e) {
    log_error("Sequía: ", conditionMessage(e), " (se conserva la sequía anterior; el resto sigue)")
    NULL
  })

  DBI::dbExecute(con, "UPDATE dim_distrito SET pp_anual = NULL")
  duckdb::duckdb_register(con, "tmp_pp", res$pp_anual)
  DBI::dbExecute(con, "UPDATE dim_distrito SET pp_anual = tmp_pp.pp_anual FROM tmp_pp
                       WHERE dim_distrito.ubigeo = tmp_pp.ubigeo")
  duckdb::duckdb_unregister(con, "tmp_pp")
  db_reemplazar(con, "clima_diaria", clima_diaria(cfg, precip))
  if (!is.null(sequia)) log_info("Sequía lista: ", nrow(sequia), " filas")
}

actualizar_riesgo <- function(con, cfg, distritos, traer_gfs, paso) {
  gfs <- if (traer_gfs) {
    paso("gfs", {
      d <- fetch_gfs(distritos, cfg) |> dplyr::filter(!is.na(pp_mm)) |> validar_lluvia()
      list(datos = d, mensaje = sprintf("%s a %s · %d distritos", fmt_fecha(min(d$fecha)),
                                        fmt_fecha(max(d$fecha)), dplyr::n_distinct(d$ubigeo)))
    })$datos
  }
  if (is.null(gfs)) {   # se reutiliza el último pronóstico guardado
    gfs <- q_con(con, "SELECT ubigeo, fecha, pp_mm, modelo FROM pronostico
                       WHERE run_id = (SELECT max(run_id) FROM pronostico)")
    if (nrow(gfs) == 0) return(log_error("Sin pronóstico de lluvia no se puede calcular el riesgo."))
  }

  enso <- q_con(con, "SELECT indice, arg_max(valor, fecha) AS valor FROM enso_indice GROUP BY indice")
  enso_ultimo <- list(oni = enso$valor[enso$indice == "oni"] %||% 0,
                      nino12 = enso$valor[enso$indice == "nino12"] %||% 0)
  detalle <- q_con(con, "SELECT detalle FROM fuente_estado WHERE fuente = 'enfen'")$detalle
  enfen <- if (length(detalle) && !is.na(detalle)) jsonlite::fromJSON(detalle)
           else list(rios = character(), mar_sobre_normal = FALSE)
  clima <- q_con(con, "SELECT ubigeo, p50_mm, p99_mm FROM clima_diaria")
  if (nrow(clima) == 0) return(log_error("Falta la climatología diaria (se calcula con PISCO)."))

  avanzar("Lluvia en las cuencas", "promedio aguas arriba de cada río")
  cuenca <- tryCatch(cuenca_por_distrito(gfs, clima, distritos, leer_cuencas(cfg), cfg$rutas$rios),
                     error = function(e) { log_error("Lluvia de cuenca: ", conditionMessage(e)); NULL })
  avanzar("Calculando riesgo de inundación", sprintf("%d distritos", dplyr::n_distinct(gfs$ubigeo)))
  run_id <- format(Sys.time(), "%Y%m%d%H%M%S")
  riesgo <- construir_riesgo(dplyr::select(gfs, ubigeo, fecha, pp_mm), distritos, clima,
                             enso_ultimo, enfen, cfg$riesgo, cuenca) |>
    validar_riesgo() |>
    dplyr::mutate(run_id = run_id)
  resumen <- riesgo |>
    dplyr::group_by(run_id, ubigeo) |>
    dplyr::summarise(fecha_pico = fecha[which.max(riesgo)], riesgo_max = max(riesgo), .groups = "drop")

  db_upsert(con, "corrida", tibble::tibble(
    run_id = run_id, inicio = Sys.time(), fin = Sys.time(), version_modelo = cfg$version_modelo,
    estado = "ok", notas = sprintf("ONI %s · %d distritos", fmt_signo(enso_ultimo$oni, 2), dplyr::n_distinct(riesgo$ubigeo))
  ), "run_id")
  db_upsert(con, "pronostico", dplyr::mutate(dplyr::select(gfs, ubigeo, fecha, pp_mm), run_id = run_id, modelo = "gfs"),
            c("run_id", "ubigeo", "fecha", "modelo"))
  db_upsert(con, "riesgo", riesgo, c("run_id", "ubigeo", "fecha"))
  db_upsert(con, "riesgo_resumen", resumen, c("run_id", "ubigeo"))

  # Solo las últimas corridas guardan el detalle diario; el máximo se guarda siempre.
  n <- as.integer(cfg$actualizacion$corridas_detalle)
  for (tabla in c("riesgo", "pronostico")) {
    DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE run_id NOT IN
      (SELECT run_id FROM corrida ORDER BY run_id DESC LIMIT %d)", tabla, n))
  }
  log_info("Riesgo listo: corrida ", run_id)
}

# Distritos ----------------------------------------------------------------------------

#' Tabla de distritos: límites INEI + ríos + altitud (se consulta una sola vez y se guarda).
preparar_distritos <- function(cfg, con) {
  geo <- cfg$rutas$geo
  d <- readr::read_csv(file.path(geo, "dim_distrito.csv"),
                       col_types = readr::cols(ubigeo = "c", cod_dep = "c", cod_prov = "c", costero = "l"))
  d <- d |>
    dplyr::left_join(resumen_rios(cfg$rutas$rios), by = "ubigeo") |>
    dplyr::left_join(cuenca_principal(leer_cuencas(cfg)), by = "ubigeo")

  cache <- file.path(geo, "altitud.csv")
  alt <- if (file.exists(cache)) readr::read_csv(cache, col_types = "cd") else tibble::tibble(ubigeo = character(), altitud = numeric())
  faltan <- dplyr::filter(d, !ubigeo %in% alt$ubigeo[!is.na(alt$altitud)])
  if (nrow(faltan) > 0) {
    log_info("Consultando la altitud de ", nrow(faltan), " distritos (solo la primera vez)…")
    nuevas <- tryCatch(fetch_elevacion(faltan, cfg), error = function(e) {
      log_error("Altitud: ", conditionMessage(e)); NULL
    })
    if (!is.null(nuevas)) {
      alt <- dplyr::bind_rows(dplyr::filter(alt, !ubigeo %in% nuevas$ubigeo), nuevas)
      readr::write_csv(alt, cache)
    }
  }
  # Desnivel dentro del distrito (data-raw/relieve_open_meteo.html): mide qué tan
  # empinado es; mejor indicador de huaico que la altitud (el altiplano es alto y plano).
  ruta_relieve <- file.path(geo, "relieve_distrito.csv")   # (relieve.csv: versión con filas invertidas, ya no se usa)
  if (!file.exists(ruta_relieve) && file.exists(file.path(geo, "puntos_relieve.csv"))) {
    log_info("Calculando el relieve de los distritos (solo la primera vez)…")
    r <- tryCatch(calcular_relieve(cfg), error = function(e) { log_error("Relieve: ", conditionMessage(e)); NULL })
    if (!is.null(r) && nrow(r) > 1500) readr::write_csv(r, ruta_relieve)
  }
  if (file.exists(ruta_relieve)) {
    d <- dplyr::left_join(d, readr::read_csv(ruta_relieve, col_types = readr::cols(ubigeo = "c", .default = "d")) |>
                            dplyr::select(ubigeo, relieve), by = "ubigeo")
  }
  pp <- tryCatch(q_con(con, "SELECT ubigeo, pp_anual FROM dim_distrito"), error = function(e) NULL)
  d |>
    dplyr::left_join(alt, by = "ubigeo") |>
    dplyr::left_join(pp %||% tibble::tibble(ubigeo = character(), pp_anual = numeric()), by = "ubigeo")
}

# Estado de las fuentes -------------------------------------------------------------------

estado_previo <- function(ruta) {
  vacio <- tibble::tibble(fuente = character(), version = character(), estado = character())
  if (is.null(ruta)) return(vacio)
  con <- db_conectar(ruta, read_only = TRUE)
  on.exit(db_desconectar(con))
  tryCatch(q_con(con, "SELECT fuente, version, estado FROM fuente_estado"), error = function(e) vacio)
}

#' Anota en fuente_estado el resultado de revisar/actualizar una fuente.
#' `res` NULL = no hubo cambios; lista(ok, valor, mensaje) = se intentó actualizar.
registrar_fuente <- function(con, id, version, res) {
  ahora <- Sys.time()
  ant <- q_con(con, sprintf("SELECT * FROM fuente_estado WHERE fuente = '%s'", id))
  fila <- if (nrow(ant)) ant else tibble::tibble(fuente = id, version = NA_character_, revisado = ahora,
                                                 actualizado = as.POSIXct(NA), estado = NA_character_,
                                                 mensaje = NA_character_, detalle = NA_character_)
  fila$revisado <- ahora
  if (!is.null(version$error)) {
    fila$estado <- "error"; fila$mensaje <- paste("No responde:", version$error)
  } else if (is.null(res)) {
    if (!identical(fila$estado, "error")) fila$estado <- "sin cambios"
  } else if (isTRUE(res$ok)) {
    fila$version <- version$valor; fila$actualizado <- ahora; fila$estado <- "ok"
    fila$mensaje <- res$valor$mensaje %||% NA_character_
    if (!is.null(res$valor$detalle)) fila$detalle <- res$valor$detalle
  } else {
    fila$estado <- "error"; fila$mensaje <- res$mensaje
  }
  db_upsert(con, "fuente_estado", fila, "fuente")
}

#' Archivo pequeño con la hora de la última revisión (aunque no haya habido novedades).
escribir_revision <- function(carpeta, versiones) {
  errores <- names(Filter(function(v) !is.null(v$error), versiones))
  nuevos <- names(Filter(function(v) isTRUE(v$cambio), versiones))
  writeLines(c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               paste(nuevos, collapse = ","), paste(errores, collapse = ",")),
             file.path(carpeta, "ULTIMA_REVISION"))
}

leer_revision <- function(carpeta) {
  ruta <- file.path(carpeta, "ULTIMA_REVISION")
  if (!file.exists(ruta)) return(NULL)
  l <- readLines(ruta, warn = FALSE)
  list(hora = as.POSIXct(l[1]), nuevos = strsplit(l[2] %||% "", ",")[[1]],
       errores = strsplit(l[3] %||% "", ",")[[1]])
}

# Ayudas -------------------------------------------------------------------------------------

q_con <- function(con, sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))

tabla_vacia <- function(con, tabla) {
  DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", tabla))$n == 0
}

setdiff_fechas <- function(a, b) as.Date(setdiff(as.numeric(a), as.numeric(b)), origin = "1970-01-01")

#' Lanza actualizar.R como un programa aparte (Rscript), totalmente independiente
#' de la app: sigue aunque se cierre la app o RStudio. Su avance se ve en PROGRESO.
lanzar_actualizacion <- function(dir = getwd(), forzar = FALSE) {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  args <- c(shQuote(normalizePath(file.path(dir, "actualizar.R"))), if (forzar) "--forzar")
  system2(rscript, args, wait = FALSE, stdout = FALSE, stderr = FALSE)
  invisible(TRUE)
}
