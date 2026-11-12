#!/usr/bin/env bash
# Instala o actualiza PACHA ENSO en un servidor Ubuntu (Oracle Cloud Always Free u otro).
# Lo ejecuta publicar.bat por usted; a mano: sudo bash /opt/pacha-enso/servidor/instalar.sh
set -euo pipefail
APP=/opt/pacha-enso
DATOS=/srv/enso

echo "== 1/4 Docker"
if ! command -v docker >/dev/null 2>&1; then
  curl -fsSL https://get.docker.com | sh
fi
systemctl enable --now docker >/dev/null 2>&1 || true

echo "== 2/4 Puertos 80 y 443"
# Las imágenes Ubuntu de Oracle traen un cortafuegos que solo deja pasar SSH.
for p in 80 443; do
  iptables -C INPUT -p tcp --dport "$p" -j ACCEPT 2>/dev/null || iptables -I INPUT 1 -p tcp --dport "$p" -j ACCEPT
done
if command -v netfilter-persistent >/dev/null 2>&1; then netfilter-persistent save >/dev/null 2>&1 || true; fi

echo "== 3/4 Datos"
mkdir -p "$DATOS/db" "$DATOS/raw"
for f in PISCOp_m.nc PISCOp_d.nc; do
  if [ -f "/tmp/$f" ]; then mv -f "/tmp/$f" "$DATOS/raw/$f"; fi
done
# Base ya calculada en la PC (opcional): evita esperar la primera actualización
if [ -f /tmp/pacha_db.tgz ] && [ ! -f "$DATOS/db/ACTUAL" ]; then
  tar -xzf /tmp/pacha_db.tgz -C "$DATOS/db" && rm -f /tmp/pacha_db.tgz
fi

echo "== 4/4 App"
cd "$APP"
if [ ! -f .env ]; then
  IP=$(curl -fsS https://api.ipify.org 2>/dev/null || hostname -I | awk '{print $1}')
  {
    echo "DOMINIO=${IP//./-}.sslip.io"
    echo "ENSO_ADMIN_CLAVE=$(openssl rand -hex 10)"
    echo "ENSO_CADA_HORAS=6"
  } > .env
  chmod 600 .env
fi
docker compose up -d --build

. ./.env
echo
echo "Listo. Dirección pública:   https://$DOMINIO"
echo "Panel de administración:    https://$DOMINIO/?admin=$ENSO_ADMIN_CLAVE"
echo "(guarde esa clave; está en $APP/.env)"
echo "Ver la actividad:           cd $APP && sudo docker compose logs -f app"
