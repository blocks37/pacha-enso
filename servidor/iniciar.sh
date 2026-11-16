#!/bin/sh
# Arranque del contenedor: la actualización automática corre en segundo plano y la
# app queda al frente. Ambas viven en el mismo contenedor para que el candado de
# «actualización en curso» (número de proceso) valga para las dos.
mkdir -p /srv/enso/db /srv/enso/raw
CADA_HORAS="${ENSO_CADA_HORAS:-6}"

# ENSO_CADA_HORAS=0: este servidor no calcula; la app baja la base que publica la PC en GitHub
[ "$CADA_HORAS" = "0" ] || (

  sleep 20   # que la app arranque primero
  while true; do
    echo "== $(date '+%Y-%m-%d %H:%M') actualización programada ==" >> /srv/enso/cron.log
    Rscript /app/actualizar.R >> /srv/enso/cron.log 2>&1
    # el registro no crece sin límite: se quedan las últimas 5000 líneas
    tail -n 5000 /srv/enso/cron.log > /srv/enso/cron.tmp && mv /srv/enso/cron.tmp /srv/enso/cron.log
    sleep $((CADA_HORAS * 3600))
  done
) &

exec Rscript -e "shiny::runApp('/app', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"
