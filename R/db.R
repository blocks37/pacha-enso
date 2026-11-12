# Base de datos DuckDB versionada ------------------------------------------------
#
# data/db/
#   enso_20261112060000.duckdb   ← una base completa por cada actualización
#   enso_20261112120000.duckdb
#   ACTUAL                       ← texto con el nombre de la base vigente
#
# La actualización copia la base vigente, escribe en la copia y al final cambia
# ACTUAL. La app siempre lee la base que dice ACTUAL. Como nunca escriben el mismo
# archivo, no hay bloqueos en Windows aunque la app esté abierta.

ESQUEMA <- c(
  dim_distrito = "
    CREATE TABLE IF NOT EXISTS dim_distrito (
      ubigeo VARCHAR PRIMARY KEY, distrito VARCHAR, provincia VARCHAR, departamento VARCHAR,
      cod_dep VARCHAR, cod_prov VARCHAR, macrorregion VARCHAR, lat DOUBLE, lon DOUBLE,
      area_km2 DOUBLE, altitud DOUBLE, costero BOOLEAN, rio VARCHAR, pp_anual DOUBLE,
      rios VARCHAR, claves_rio VARCHAR, km_rio DOUBLE, codigo_cuenca VARCHAR, cuenca VARCHAR,
      relieve DOUBLE
    )",
  fuente_estado = "
    CREATE TABLE IF NOT EXISTS fuente_estado (
      fuente VARCHAR PRIMARY KEY,
      version VARCHAR,              -- huella de la última versión descargada
      revisado TIMESTAMP,           -- última vez que se preguntó si había algo nuevo
      actualizado TIMESTAMP,        -- última vez que trajo algo nuevo
      estado VARCHAR,               -- ok, sin cambios, error
      mensaje VARCHAR,
      detalle VARCHAR               -- JSON con datos propios de la fuente (p. ej. estado ENFEN)
    )",
  enso_indice = "
    CREATE TABLE IF NOT EXISTS enso_indice (
      fecha DATE, indice VARCHAR, valor DOUBLE, fuente VARCHAR, PRIMARY KEY (fecha, indice)
    )",
  precip_mensual = "
    CREATE TABLE IF NOT EXISTS precip_mensual (
      ubigeo VARCHAR, mes DATE, pp_mm DOUBLE, dsc INTEGER,
      fuente VARCHAR,               -- PISCO, CHIRPS (ajustado), análogos ENSO
      pronostico BOOLEAN,
      PRIMARY KEY (ubigeo, mes)
    )",
  clima_diaria = "
    CREATE TABLE IF NOT EXISTS clima_diaria (
      ubigeo VARCHAR PRIMARY KEY, p50_mm DOUBLE, p99_mm DOUBLE, metodo VARCHAR
    )",
  corrida = "
    CREATE TABLE IF NOT EXISTS corrida (
      run_id VARCHAR PRIMARY KEY, inicio TIMESTAMP, fin TIMESTAMP,
      version_modelo VARCHAR, estado VARCHAR, notas VARCHAR
    )",
  pronostico = "
    CREATE TABLE IF NOT EXISTS pronostico (
      run_id VARCHAR, ubigeo VARCHAR, fecha DATE, modelo VARCHAR, pp_mm DOUBLE,
      PRIMARY KEY (run_id, ubigeo, fecha, modelo)
    )",
  riesgo = "
    CREATE TABLE IF NOT EXISTS riesgo (
      run_id VARCHAR, ubigeo VARCHAR, fecha DATE, amenaza VARCHAR,
      lluvia_mm DOUBLE, lluvia_idx DOUBLE, mar_idx DOUBLE, cauce_idx DOUBLE,
      exposicion_idx DOUBLE, enso_idx DOUBLE, cuenca_mm DOUBLE, cuenca_idx DOUBLE, riesgo DOUBLE, nivel VARCHAR,
      PRIMARY KEY (run_id, ubigeo, fecha)
    )",
  riesgo_resumen = "
    CREATE TABLE IF NOT EXISTS riesgo_resumen (
      run_id VARCHAR, ubigeo VARCHAR, riesgo_max DOUBLE, fecha_pico DATE,
      PRIMARY KEY (run_id, ubigeo)
    )",
  sequia = "
    CREATE TABLE IF NOT EXISTS sequia (
      ubigeo VARCHAR, mes DATE, pronostico BOOLEAN,
      spi1 DOUBLE, spi3 DOUBLE, spi6 DOUBLE, spi12 DOUBLE, spei6 DOUBLE,
      dsc INTEGER, riesgo DOUBLE, categoria VARCHAR,
      PRIMARY KEY (ubigeo, mes)
    )"
)

db_conectar <- function(path, read_only = FALSE) {
  DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)
}

db_desconectar <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
}

# Columnas agregadas después de la primera versión: se añaden a bases existentes.
MIGRACIONES <- c(
  "ALTER TABLE dim_distrito ADD COLUMN IF NOT EXISTS rios VARCHAR",
  "ALTER TABLE dim_distrito ADD COLUMN IF NOT EXISTS claves_rio VARCHAR",
  "ALTER TABLE dim_distrito ADD COLUMN IF NOT EXISTS km_rio DOUBLE",
  "ALTER TABLE dim_distrito ADD COLUMN IF NOT EXISTS codigo_cuenca VARCHAR",
  "ALTER TABLE dim_distrito ADD COLUMN IF NOT EXISTS cuenca VARCHAR",
  "ALTER TABLE dim_distrito ADD COLUMN IF NOT EXISTS relieve DOUBLE",
  "ALTER TABLE riesgo ADD COLUMN IF NOT EXISTS cuenca_mm DOUBLE",
  "ALTER TABLE riesgo ADD COLUMN IF NOT EXISTS cuenca_idx DOUBLE"
)

db_inicializar <- function(con) {
  for (sql in c(ESQUEMA, MIGRACIONES)) DBI::dbExecute(con, sql)
  invisible(con)
}

#' Ruta de la base vigente, o NULL si todavía no hay ninguna.
db_actual <- function(carpeta) {
  puntero <- file.path(carpeta, "ACTUAL")
  if (!file.exists(puntero)) return(NULL)
  nombre <- trimws(readLines(puntero, n = 1, warn = FALSE))
  ruta <- file.path(carpeta, nombre)
  if (length(nombre) == 0 || !file.exists(ruta)) NULL else ruta
}

#' Inserta o reemplaza filas según su clave primaria.
db_upsert <- function(con, tabla, df, claves) {
  if (is.null(df) || nrow(df) == 0) return(invisible(0L))
  tmp <- paste0("tmp_", tabla)
  duckdb::duckdb_register(con, tmp, df)
  on.exit(duckdb::duckdb_unregister(con, tmp), add = TRUE)
  cond <- paste(sprintf("t.%1$s = s.%1$s", claves), collapse = " AND ")
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, sprintf("DELETE FROM %s t USING %s s WHERE %s", tabla, tmp, cond))
    DBI::dbExecute(con, sprintf("INSERT INTO %s BY NAME SELECT * FROM %s", tabla, tmp))
  })
  invisible(nrow(df))
}

#' Reemplaza una tabla completa (para tablas derivadas que se recalculan enteras).
db_reemplazar <- function(con, tabla, df) {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, sprintf("DELETE FROM %s", tabla))
    if (nrow(df) > 0) {
      duckdb::duckdb_register(con, "tmp_reemplazo", df)
      on.exit(duckdb::duckdb_unregister(con, "tmp_reemplazo"), add = TRUE)
      DBI::dbExecute(con, sprintf("INSERT INTO %s BY NAME SELECT * FROM tmp_reemplazo", tabla))
    }
  })
  invisible(nrow(df))
}

#' Crea una versión nueva de la base: copia la vigente, aplica `escribir(con)`
#' y, si todo salió bien, la publica en ACTUAL. Devuelve lo que devuelva `escribir`.
db_publicar <- function(carpeta, escribir, conservar = 3) {
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  nueva <- file.path(carpeta, sprintf("enso_%s.duckdb", format(Sys.time(), "%Y%m%d%H%M%S")))
  vigente <- db_actual(carpeta)
  if (!is.null(vigente)) file.copy(vigente, nueva, overwrite = TRUE)

  con <- db_conectar(nueva)
  publicado <- FALSE
  on.exit({
    db_desconectar(con)
    if (!publicado) unlink(nueva)
  })
  db_inicializar(con)
  resultado <- escribir(con)
  DBI::dbExecute(con, "CHECKPOINT")
  db_desconectar(con)

  escribir_puntero(carpeta, basename(nueva))
  publicado <- TRUE
  limpiar_bases(carpeta, conservar)
  resultado
}

escribir_puntero <- function(carpeta, nombre) {
  tmp <- file.path(carpeta, "ACTUAL.tmp")
  writeLines(nombre, tmp)
  for (intento in 1:10) {                       # en Windows puede estar leyéndose un instante
    if (file.rename(tmp, file.path(carpeta, "ACTUAL"))) return(invisible(TRUE))
    Sys.sleep(1)
  }
  stop("No se pudo actualizar el archivo ACTUAL")
}

#' Borra versiones viejas; si alguna está abierta por la app, se borrará la próxima vez.
limpiar_bases <- function(carpeta, conservar = 3) {
  bases <- sort(list.files(carpeta, pattern = "^enso_\\d+\\.duckdb$", full.names = TRUE), decreasing = TRUE)
  viejas <- utils::tail(bases, -conservar)
  for (b in viejas) suppressWarnings(unlink(c(b, paste0(b, ".wal"))))
}

# Lectura desde la app --------------------------------------------------------------

#' Devuelve una función que entrega una conexión de solo lectura a la base vigente
#' y la reabre cuando ACTUAL cambia.
db_lector <- function(carpeta) {
  estado <- new.env(parent = emptyenv())
  function() {
    ruta <- db_actual(carpeta)
    if (is.null(ruta)) stop("Todavía no hay datos: ejecute actualizar.R")
    if (!identical(ruta, estado$ruta) || is.null(estado$con) || !DBI::dbIsValid(estado$con)) {
      if (!is.null(estado$con)) try(db_desconectar(estado$con), silent = TRUE)
      estado$con <- db_conectar(ruta, read_only = TRUE)
      estado$ruta <- ruta
    }
    estado$con
  }
}

q <- function(db, sql, params = NULL) {
  tibble::as_tibble(DBI::dbGetQuery(db(), sql, params = params))
}

en_lista <- function(x) paste(sprintf("'%s'", gsub("'", "", x)), collapse = ", ")

q_distritos <- function(db) q(db, "SELECT * FROM dim_distrito ORDER BY departamento, provincia, distrito")

q_fuentes <- function(db) q(db, "SELECT * FROM fuente_estado ORDER BY fuente")

#' Pronóstico diario de lluvia de la última corrida (para la lluvia de cuenca de cada río).
q_pronostico <- function(db) {
  q(db, "SELECT ubigeo, fecha, pp_mm FROM pronostico WHERE run_id = (SELECT max(run_id) FROM pronostico)")
}

q_clima <- function(db) q(db, "SELECT ubigeo, p50_mm, p99_mm FROM clima_diaria")

q_ultima_corrida <- function(db) {
  q(db, "SELECT * FROM corrida WHERE estado <> 'error' ORDER BY run_id DESC LIMIT 1")
}

q_enso <- function(db, desde = as.Date("1950-01-01")) {
  q(db, "SELECT fecha, indice, valor FROM enso_indice WHERE fecha >= ? ORDER BY fecha", list(desde))
}

#' Riesgo diario de la última corrida para los próximos `dias`.
q_riesgo <- function(db, dias = 14) {
  q(db, "
    WITH u AS (SELECT max(run_id) AS run_id FROM corrida WHERE estado <> 'error')
    SELECT r.* FROM riesgo r, u
    WHERE r.run_id = u.run_id
      AND r.fecha < (SELECT min(fecha) FROM riesgo r2 WHERE r2.run_id = u.run_id) + CAST(? AS INTEGER)",
    list(as.integer(dias)))
}

#' Máximo de riesgo por corrida (historial persistente).
q_riesgo_historial <- function(db, ubigeos) {
  if (length(ubigeos) == 0) return(tibble::tibble())
  q(db, sprintf("
    SELECT c.inicio, r.ubigeo, r.riesgo_max FROM riesgo_resumen r JOIN corrida c USING (run_id)
    WHERE r.ubigeo IN (%s) ORDER BY c.inicio", en_lista(ubigeos)))
}

#' Sequía de los meses indicados (todas las columnas).
q_sequia_meses <- function(db, desde, hasta = as.Date("2100-01-01")) {
  q(db, "SELECT * FROM sequia WHERE mes BETWEEN ? AND ? ORDER BY mes", list(as.Date(desde), as.Date(hasta)))
}

q_sequia_ultimo_obs <- function(db) {
  q(db, "SELECT max(mes) AS mes FROM sequia WHERE NOT pronostico")$mes
}

#' Serie de sequía promediada para un conjunto de distritos.
q_sequia_serie <- function(db, ubigeos) {
  q(db, sprintf("
    SELECT mes, bool_or(pronostico) AS pronostico, avg(spi3) AS spi3, avg(spi6) AS spi6,
           avg(spi12) AS spi12, avg(spei6) AS spei6
    FROM sequia WHERE ubigeo IN (%s) GROUP BY mes ORDER BY mes", en_lista(ubigeos)))
}

#' Lluvia mensual promediada (por área) para un conjunto de distritos.
q_precip_serie <- function(db, ubigeos) {
  q(db, sprintf("
    SELECT p.mes, sum(p.pp_mm * d.area_km2) / sum(d.area_km2) AS pp_mm, max(p.dsc) AS dsc,
           bool_or(p.pronostico) AS pronostico, min(p.fuente) AS fuente
    FROM precip_mensual p JOIN dim_distrito d USING (ubigeo)
    WHERE p.ubigeo IN (%s) GROUP BY p.mes ORDER BY p.mes", en_lista(ubigeos)))
}
