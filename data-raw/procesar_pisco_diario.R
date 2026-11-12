# Procesa PISCO DIARIO a mano (normalmente no hace falta: la actualización lo hace
# sola la primera vez que encuentra data/raw/PISCOp_d.nc, o cuando se reemplaza).
#
# Deja dos tablas pequeñas en data/geo/:
#   clima_diaria_pisco.csv   ubigeo, p50_mm, p99_mm   (umbrales del riesgo de inundación)
#   dsc_mensual_pisco.csv    ubigeo, mes, dsc          (racha seca de cada mes)
#
# Uso, con el proyecto abierto:
#   invisible(lapply(list.files("R", full.names = TRUE), source, encoding = "UTF-8"))
#   procesar_pisco_diario("data/raw/PISCOp_d.nc")
#
# La función está en R/fetch_pisco.R. Luego lance la actualización con «Recalcular todo».
