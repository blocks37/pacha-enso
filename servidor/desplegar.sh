#!/usr/bin/env bash
# Aplica en el servidor lo último de GitHub (lo llama cron cada 10 min; ver nube.sh).
set -euo pipefail
cd /opt/pacha-enso
git fetch --quiet origin main
if [ "$(git rev-parse HEAD)" != "$(git rev-parse origin/main)" ]; then
  echo "== $(date) Nueva versión $(git rev-parse --short origin/main)"
  git reset --hard --quiet origin/main
  docker compose up -d --build
  docker image prune -f >/dev/null
fi
