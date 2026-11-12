#!/usr/bin/env bash
# Baja o sube los datos a un almacenamiento persistente (S3, GCS, R2…).
# BUCKET_URL: p. ej. s3://mi-bucket/enso  (requiere aws CLI configurada)
set -euo pipefail
accion="${1:?uso: sincronizar.sh bajar|subir}"
mkdir -p data/db data/raw

case "$accion" in
  bajar)
    aws s3 sync "$BUCKET_URL/raw" data/raw
    aws s3 sync "$BUCKET_URL/db" data/db || echo "Sin base previa: se creará una nueva." ;;
  subir)
    aws s3 sync data/raw "$BUCKET_URL/raw"
    aws s3 sync data/db "$BUCKET_URL/db" --delete ;;
  *) echo "Acción desconocida: $accion" >&2; exit 1 ;;
esac
