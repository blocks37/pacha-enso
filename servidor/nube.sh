#!/usr/bin/env bash
# PACHA ENSO en Oracle Cloud (Always Free), sin SSH ni archivos que subir.
#
# Se pega este bloque en «Script de inicialización» (cloud-init) al crear la máquina:
#
#   #!/bin/bash
#   until git clone --depth 1 https://github.com/blocks37/pacha-enso /opt/pacha-enso; do sleep 20; done
#   bash /opt/pacha-enso/servidor/nube.sh > /var/log/pacha-enso.log 2>&1
#
# Qué hace:
#   - instala Docker, abre los puertos 80 y 443 y arranca la app con HTTPS (Caddy);
#   - la app NO calcula: baja la base que la PC publica en GitHub (release «datos»);
#   - cada 10 min revisa GitHub: si hay cambios en el código, los aplica solo
#     (igual que Connect Cloud al hacer push desde GitHub Desktop).
# Avance:  sudo tail -f /var/log/pacha-enso.log      Dirección y clave: /opt/pacha-enso/LEAME_SERVIDOR.txt
set -euo pipefail
APP=/opt/pacha-enso

echo "== $(date) Inicio"
# Memoria de intercambio: la primera compilación de paquetes de R es pesada
if ! swapon --show | grep -q /swapfile; then
  fallocate -l 4G /swapfile && chmod 600 /swapfile && mkswap /swapfile && swapon /swapfile
  echo "/swapfile none swap sw 0 0" >> /etc/fstab
fi

if ! command -v docker >/dev/null 2>&1; then curl -fsSL https://get.docker.com | sh; fi
systemctl enable --now docker

# Las imágenes Ubuntu de Oracle traen un cortafuegos que solo deja pasar SSH
for p in 80 443; do
  iptables -C INPUT -p tcp --dport "$p" -j ACCEPT 2>/dev/null || iptables -I INPUT 1 -p tcp --dport "$p" -j ACCEPT
done
DEBIAN_FRONTEND=noninteractive apt-get install -y iptables-persistent >/dev/null 2>&1 || true
netfilter-persistent save >/dev/null 2>&1 || true

mkdir -p /srv/enso
cd "$APP"
if [ ! -f .env ]; then
  IP=$(curl -fsS https://api.ipify.org || hostname -I | awk '{print $1}')
  {
    echo "DOMINIO=${IP//./-}.sslip.io"
    echo "ENSO_ADMIN_CLAVE=$(openssl rand -hex 10)"
    echo "ENSO_CADA_HORAS=0"          # 0 = el servidor no calcula; usa la base de GitHub
  } > .env
  chmod 600 .env
fi

echo "== $(date) Construyendo la app (la primera vez tarda 20-60 min)"
docker compose up -d --build

# Despliegue automático desde GitHub cada 10 minutos
cat > /etc/cron.d/pacha-enso <<CRON
*/10 * * * * root bash $APP/servidor/desplegar.sh >> /var/log/pacha-enso-despliegue.log 2>&1
CRON

. ./.env
cat > "$APP/LEAME_SERVIDOR.txt" <<TXT
PACHA ENSO en este servidor
  Dirección pública:        https://$DOMINIO
  Panel de administración:  https://$DOMINIO/?admin=$ENSO_ADMIN_CLAVE
TXT
chmod 600 "$APP/LEAME_SERVIDOR.txt"
echo "== $(date) Listo: https://$DOMINIO"
