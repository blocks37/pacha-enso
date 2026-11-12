FROM rocker/r-ver:4.4.2
# Imagen multiplataforma: sirve igual en Intel/AMD y en ARM (Oracle Ampere).

# Librerías del sistema para sf/terra (GDAL, GEOS, PROJ), httr2 y pdftools (poppler).
RUN apt-get update && apt-get install -y --no-install-recommends \
      libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
      libcurl4-openssl-dev libssl-dev libxml2-dev libpoppler-cpp-dev \
    && rm -rf /var/lib/apt/lists/*

# rocker/r-ver ya apunta a binarios de Posit Package Manager: la instalación es rápida.
WORKDIR /app

COPY install.R .
RUN Rscript install.R

COPY . .

EXPOSE 3838
ENV R_CONFIG_ACTIVE=produccion TZ=America/Lima
# Arranca la app y, en segundo plano, la actualización automática cada 6 h
CMD ["sh", "/app/servidor/iniciar.sh"]
