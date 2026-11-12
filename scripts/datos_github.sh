#!/usr/bin/env bash
# Guarda y recupera la base de PACHA ENSO en la release «datos» del propio repositorio.
# Lo usa la tarea de GitHub Actions (.github/workflows/pipeline.yml):
#   bash scripts/datos_github.sh bajar    antes de actualizar.R
#   bash scripts/datos_github.sh subir    después: publica solo si la base cambió
set -euo pipefail
ETIQUETA="${ENSO_ETIQUETA_DATOS:-datos}"
DB=data/db
mkdir -p "$DB" data/raw

# Nombre de la base vigente + parte de su md5: cambia cuando cambia la base
huella() {
  [ -f "$DB/ACTUAL" ] || { echo ""; return; }
  local actual; actual=$(tr -d '[:space:]' < "$DB/ACTUAL")
  [ -f "$DB/$actual" ] || { echo ""; return; }
  echo "$actual $(md5sum "$DB/$actual" | cut -c1-12)"
}

case "${1:-}" in
  bajar)
    if ! gh release view "$ETIQUETA" >/dev/null 2>&1; then
      gh release create "$ETIQUETA" --title "Datos de PACHA ENSO" \
        --notes "Base publicada automáticamente cada 6 h por la tarea «actualizar-datos». La app la descarga de aquí. No la borre."
    fi
    if gh release download "$ETIQUETA" -p pacha_db.tgz -D /tmp --clobber 2>/dev/null; then
      tar -xzf /tmp/pacha_db.tgz -C "$DB" && rm -f /tmp/pacha_db.tgz
      echo "Base anterior: $(huella)"
    else
      echo "Todavía no hay base publicada: se calcula desde cero (30-60 min)."
    fi
    if [ ! -f data/raw/PISCOp_m.nc ]; then
      if [ -f insumos/PISCOp_m.nc ]; then
        cp insumos/PISCOp_m.nc data/raw/PISCOp_m.nc
      else
        echo "::error::Falta insumos/PISCOp_m.nc en el repositorio"; exit 1
      fi
    fi
    # La versión de PISCO se reconoce por fecha y tamaño del archivo. Cada corrida lo copia
    # de nuevo, así que la fecha se fija según su contenido: solo cambia si cambia el archivo.
    suma=$(md5sum data/raw/PISCOp_m.nc | cut -c1-7)
    touch -d "@$(( 16#$suma + 1000000000 ))" data/raw/PISCOp_m.nc
    # Caché inicial de CHIRPS (meses y días ya descargados en la PC del administrador):
    # evita volver a pedir al servidor del CHC lo que ya se tiene
    for d in chirps chirps_diario; do
      if [ -d "insumos/cache_chirps/$d" ]; then
        mkdir -p "data/raw/$d"
        cp -n insumos/cache_chirps/$d/*.tif "data/raw/$d/" 2>/dev/null || true
      fi
    done
    huella > /tmp/huella_antes
    ;;
  subir)
    ahora=$(huella)
    if [ -z "$ahora" ]; then echo "::warning::No hay base para publicar"; exit 0; fi
    if [ "$ahora" = "$(cat /tmp/huella_antes 2>/dev/null || true)" ]; then echo "Sin cambios: no se publica."; exit 0; fi
    actual=$(tr -d '[:space:]' < "$DB/ACTUAL")
    extras=""
    for f in METODO_SEQUIA ULTIMA_REVISION; do [ -f "$DB/$f" ] && extras="$extras $f"; done
    tar -czf /tmp/pacha_db.tgz -C "$DB" ACTUAL "$actual" $extras
    echo "$ahora" > /tmp/VERSION
    # Primero la base y después VERSION: la app nunca ve una versión sin su base
    gh release upload "$ETIQUETA" /tmp/pacha_db.tgz --clobber
    gh release upload "$ETIQUETA" /tmp/VERSION --clobber
    echo "Publicada: $ahora ($(du -h /tmp/pacha_db.tgz | cut -f1))"
    ;;
  *)
    echo "uso: $0 bajar|subir"; exit 2 ;;
esac
