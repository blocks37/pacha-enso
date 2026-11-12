# PACHA ENSO · Monitor de riesgo climático del Perú (R Shiny) · v2.2

Riesgo de **inundación y huaico** (14 días) y **sequía** (SPI, 12 meses observados + 6 pronosticados) para los **1 891 distritos** del Perú, con límites INEI 2025 y lluvia PISCO v3 de SENAMHI.

## Puesta en marcha

```r
# 1. Dependencias (una vez). Lo más seguro en Windows: cierre RStudio y haga doble
#    clic en reparar_paquetes.bat. Desde RStudio: Session › Restart R y luego
source("install.R")
#    Instala lo que falte, actualiza lo que quedó viejo y comprueba en un R limpio
#    que todo cargue (p. ej. vctrs 0.7.3 pide rlang >= 1.1.7).

# 2. Copie su PISCO mensual en data/raw/PISCOp_m.nc

# 3. Primera actualización (20–40 min: descarga CHIRPS, altitudes y calcula el SPI)
source("actualizar.R")

# 4. La app
shiny::runApp()

# 5. Actualización automática cada 6 horas (Windows, una vez)
source("programar_windows.R")
```

Si abre la app sin datos, muestra una pantalla con el botón **Descargar y calcular** que hace el paso 3 por usted.

## Cómo se actualiza (versión corta)

1. Cada 6 horas, el Programador de tareas de Windows ejecuta `actualizar.R`.
2. El script pregunta a cada fuente si tiene algo nuevo, sin descargar nada: fecha del archivo en NOAA, número de corrida del pronóstico (ECMWF), último mes de CHIRPS, fecha de su archivo PISCO.
3. Si nada cambió, termina en segundos.
4. Si algo cambió, copia la base vigente, descarga solo lo nuevo, lo valida, recalcula lo que depende de eso y publica la copia como la nueva vigente (`data/db/ACTUAL`).
5. La app revisa `ACTUAL` cada 15 segundos. Cuando cambia, se refresca sola y avisa.
6. Si una fuente falla, se anota en **Datos y método** y la app sigue con lo último bueno.

| Fuente | Qué trae | Cada cuánto cambia | Qué recalcula |
| --- | --- | --- | --- |
| NOAA CPC · ONI | Índice de El Niño | mensual | riesgo, pronóstico de lluvia, sequía |
| NOAA CPC · Niño semanal | Mar en Niño 1+2 y 3.4 | semanal | riesgo |
| ENFEN (vía página de El Niño de SENAMHI; respaldo enfen.imarpe.gob.pe) | Alerta; ríos y mar leídos del PDF del comunicado (pdftools) | quincenal | riesgo |
| ECMWF IFS 0,25° (Open-Meteo) | Lluvia diaria 14 días | cada 6 h | riesgo |
| CHIRPS v3.0 Latinoamérica (la v2.0 ya no se publica) | Lluvia de los meses que PISCO no cubre | mensual | sequía |
| CHIRPS v3.0 diario preliminar | Lluvia de las últimas semanas | diaria | sequía (mes reciente y en curso) |
| PISCO diario (su archivo) | Umbrales de lluvia extrema y rachas secas | una vez | inundación, ríos |
| PISCO (su archivo) | Lluvia mensual 1981–2025 | cuando SENAMHI publica | todo |

Para PISCO no hay descarga automática: cuando SENAMHI publique una versión nueva, reemplace `data/raw/PISCOp_m.nc` y la siguiente actualización lo detecta sola.

## PISCO diario y CHIRPS diario

**PISCO diario** (`data/raw/PISCOp_d.nc`, PISCOp v3 1981–2025, ~1,5 GB). Si el archivo está ahí, la siguiente actualización lo procesa **una sola vez** (10–40 min) y deja dos tablas pequeñas en `data/geo/`:

- `clima_diaria_pisco.csv`: mediana (P50) y percentil 99 de los días con lluvia de cada distrito, 1991–2020. Son los umbrales del riesgo de inundación y de la lluvia de cuenca de los ríos (sin el diario se aproximan desde el mensual).
- `dsc_mensual_pisco.csv`: racha seca más larga de cada mes.

La app nunca vuelve a leer el archivo grande. Si SENAMHI publica uno nuevo, reemplácelo: la actualización lo detecta y lo vuelve a procesar.

**CHIRPS diario preliminar** (v3.0, sale 2–3 días después). El CHIRPS mensual definitivo llega ~5 semanas tarde; con el diario la sequía se calcula con el mes que acaba de terminar y el mes en curso combina la lluvia ya medida con la estimada por años análogos. Se lee solo el recorte de Perú de cada día y queda en caché en `data/raw/chirps_diario/`. Si el servidor no responde, se sigue con el mensual.

## Estructura

| Archivo | Qué hace |
| --- | --- |
| `app.R` | Arranque, navegación, ficha y frescura de datos |
| `actualizar.R` | Punto de entrada de la actualización (manual o programada) |
| `programar_windows.R` | Crea la tarea programada de Windows |
| `config.yml` | Rutas, pesos, umbrales, colores, fuentes |
| `R/actualizar.R` | Qué fuente cambió, qué recalcular, registro de estado |
| `R/db.R` | Base DuckDB versionada (una por actualización + `ACTUAL`) |
| `R/fetch_*.R` | NOAA, ENFEN, PISCO, CHIRPS, Open-Meteo |
| `R/pronostico.R` | Pronóstico de lluvia por años análogos del ENSO |
| `R/riesgo.R`, `R/sequia.R`, `R/estadisticas.R` | Modelos y estadísticas (funciones puras, con pruebas) |
| `R/mapas.R`, `www/app.js` | Mapas que cambian de color sin recargarse |
| `R/mod_*.R` | Una vista por archivo + la ficha de distrito |
| `data/geo/` | Límites INEI simplificados, distritos y pesos de la grilla PISCO |
| `R/datos_remotos.R` | En la web, baja la base que publica GitHub Actions |
| `data-raw/` | Scripts de una sola vez: PISCO diario, regenerar límites, ríos |

## Pruebas

```r
testthat::test_dir("tests/testthat")
```

## Pendientes conocidos

- **Calibración:** pesos y umbrales con emergencias de INDECI (2017, 2023) y pérdidas del MIDAGRI.
- **SPEI:** falta la evapotranspiración (PISCOeo_pm). Mientras tanto, el riesgo de sequía reparte ese peso entre los demás componentes.
- **Ríos:** `data-raw/rios_distritos.csv` cubre 44 distritos de la costa norte; habría que completarlo con la ANA.
- **Límites de Open-Meteo:** el plan gratuito permite 10 000 llamadas al día. El pronóstico usa unas 805 por corrida (4 corridas al día, unas 3 200).

Modelo indicativo: no reemplaza los comunicados del ENFEN ni los avisos del SENAMHI, INDECI y CENEPRED.

## Público y administración

La plataforma es **de solo lectura** para quien la visita: nadie que entre a la página descarga ni recalcula nada. Los datos se actualizan solos en el servidor y la app muestra siempre lo último calculado.

- **Actualización automática**
  - Windows: `source("programar_windows.R")` (Programador de tareas, cada 6 h).
  - Servidor con Docker (`compose.yaml`): ya viene incluida; no hace falta cron.
  - Servidor Linux sin Docker, con `crontab -e`: `0 */6 * * * cd /srv/enso-peru && Rscript actualizar.R >> /srv/enso/cron.log 2>&1`
- **Panel de administración** (botón «Buscar datos nuevos ahora», progreso y registro). Se ve:
  - en el propio computador (`http://127.0.0.1:…`), si `acceso$admin_local` es `true`;
  - en la web, entrando con `?admin=CLAVE`, con `CLAVE` definida en la variable de entorno `ENSO_ADMIN_CLAVE`.

## Publicar en la web: GitHub + Connect Cloud (gratis, sin tarjeta)

- **GitHub Actions** calcula los datos cada 6 h en los servidores de GitHub, gratis en repositorios públicos, y publica la base en la release «datos» del repositorio.
- **Posit Connect Cloud** muestra la app y baja esa base cada 30 min.

Su PC no tiene que quedar prendida.

**Una sola vez:**

1. **Cuentas**: cree una cuenta en [github.com](https://github.com/signup) e instale [GitHub Desktop](https://desktop.github.com/). Decida el nombre del repositorio, por ejemplo `pacha-enso`.
2. **Preparar**: en RStudio, con el proyecto abierto, ejecute `source("preparar_publicacion.R")`. Cuando se lo pida, escriba `suusuario/pacha-enso`. Deja anotado el repositorio en `config.yml`, copia PISCO mensual a `insumos/` y escribe `manifest.json`.
3. **Subir el código**: en GitHub Desktop, *File › Add local repository* › elija la carpeta `enso-peru` (si dice que no es un repositorio, *create a repository*). Luego *Publish repository* con el nombre elegido y **desmarque «Keep this code private»**. Las bases (`data/db`) y los datos crudos no se suben (`.gitignore`).
4. **Primera actualización**: en github.com, en su repositorio, *Actions* › *actualizar-datos* › *Run workflow*. La primera vez tarda 40–60 min. Al terminar aparece la release **datos** con `pacha_db.tgz` y `VERSION`. Desde ahí corre sola cada 6 h.
5. **Publicar la app**: entre a [connect.posit.cloud](https://connect.posit.cloud) con su cuenta de GitHub › *Publish* › *Shiny* › su repositorio, rama `main`, archivo `app.R` › *Publish*. Active la publicación automática al hacer *push*. Esa dirección es la que se comparte.

**Después:**

- **Cambios en el código**: commit y *Push* en GitHub Desktop; Connect Cloud vuelve a publicar solo.
- **Paquetes de R nuevos**: si instala o actualiza paquetes, vuelva a ejecutar `preparar_publicacion.R` antes del push.
- **PISCO nuevo**: reemplácelo en `data/raw/`, ejecute `preparar_publicacion.R` y haga push.
- **Si falla una actualización**: GitHub le avisa por correo. El detalle está en *Actions*.

**A tener en cuenta:**

- Si nadie entra durante un tiempo, Connect Cloud duerme la app y la siguiente visita tarda unos segundos en abrirla.
- GitHub pausa las tareas de repositorios sin cambios en 60 días. La tarea hace un commit vacío antes de que eso pase.

## Alternativa: servidor propio (Oracle Cloud Always Free)

Una máquina virtual ARM de Oracle (hasta 2 OCPU y 12 GB de RAM, 200 GB de disco) es gratuita sin plazo y basta de sobra. Ahí corren dos contenedores: la app con su actualización cada 6 h, y Caddy, que pone HTTPS solo.

1. **Cuenta**: cree una cuenta en Oracle Cloud (piden tarjeta para verificar identidad; no se cobra mientras use solo recursos «Always Free»). La *región principal* no se puede cambiar después: elija **Chile Central (Santiago)** o **Brazil East (São Paulo)**, las más cercanas a Perú.
2. **Máquina**: *Compute › Instances › Create instance*
   - Imagen: **Canonical Ubuntu 24.04**. Forma: **VM.Standard.A1.Flex**, 2 OCPU y 12 GB.
   - *Add SSH keys › Generate a key pair*: descargue la clave privada (`ssh-key-….key`).
   - Si sale «Out of host capacity», pruebe otro *availability domain* u otro momento del día.
3. **Puertos**: en la red de la máquina (*Subnet › Security list › Add ingress rules*) abra TCP **80** y **443** desde `0.0.0.0/0`.
4. **Publicar**: abra `servidor/publicar.bat` con el Bloc de notas, escriba la IP pública y la ruta de la clave, guarde y haga doble clic. Sube la app, PISCO mensual y la base ya calculada, instala Docker y arranca todo. La primera vez tarda 30–60 min (compila los paquetes de R para ARM).
5. Al final muestra la **dirección pública** (`https://IP-con-guiones.sslip.io`) y la del **panel de administración** (`…/?admin=CLAVE`). La clave queda en `/opt/pacha-enso/.env` del servidor.

Para subir cambios del código, vuelva a ejecutar `publicar.bat`: los datos en `/srv/enso` se conservan.

**Para que Oracle no la apague**: en cuentas solo gratuitas Oracle puede recuperar máquinas «inactivas» (uso de CPU, red y memoria bajo 20 % durante 7 días). Pasar la cuenta a *Pay As You Go* evita ese riesgo y sigue sin costo si solo usa recursos Always Free; ponga además un presupuesto con alerta en 1 USD (*Billing › Budgets*).

**Dominio propio** (opcional): apunte un registro A a la IP y cambie `DOMINIO=` en `/opt/pacha-enso/.env`; luego `sudo docker compose up -d` en esa carpeta.
