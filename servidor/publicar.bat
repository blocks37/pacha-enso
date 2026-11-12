@echo off
setlocal
rem ==========================================================================
rem  Publica PACHA ENSO en su servidor (Oracle Cloud u otro Ubuntu).
rem  Edite estas dos líneas y haga doble clic. Sirve también para subir
rem  cambios después: vuelva a ejecutarlo.
rem ==========================================================================
set IP=000.000.000.000
set CLAVE=%USERPROFILE%\Downloads\ssh-key.key
rem ==========================================================================

if "%IP%"=="000.000.000.000" (
  echo Primero abra este archivo con el Bloc de notas y escriba la IP publica del servidor.
  pause & exit /b 1
)
cd /d "%~dp0\.."
set DESTINO=ubuntu@%IP%
set SSH=-i "%CLAVE%" -o StrictHostKeyChecking=accept-new

rem Windows rechaza claves con permisos abiertos: dejarla solo para este usuario
icacls "%CLAVE%" /inheritance:r /grant:r "%USERNAME%:R" >nul

echo [1/4] Empaquetando la app...
tar -czf "%TEMP%\pacha.tgz" app.R actualizar.R install.R config.yml Dockerfile .dockerignore compose.yaml R www data/geo servidor
if errorlevel 1 ( echo No se pudo empaquetar. & pause & exit /b 1 )

echo [2/4] Subiendo la app...
scp %SSH% "%TEMP%\pacha.tgz" %DESTINO%:/tmp/pacha.tgz || goto :error

echo [3/4] Subiendo PISCO mensual y la base ya calculada (solo la primera vez)...
ssh %SSH% %DESTINO% "test -f /srv/enso/raw/PISCOp_m.nc" || scp %SSH% data/raw/PISCOp_m.nc %DESTINO%:/tmp/PISCOp_m.nc
ssh %SSH% %DESTINO% "test -f /srv/enso/db/ACTUAL"
if errorlevel 1 (
  for /f "usebackq delims=" %%a in ("data\db\ACTUAL") do set BASE=%%a
  tar -czf "%TEMP%\pacha_db.tgz" -C data/db ACTUAL !BASE!
  scp %SSH% "%TEMP%\pacha_db.tgz" %DESTINO%:/tmp/pacha_db.tgz
)

echo [4/4] Instalando en el servidor (la primera vez puede tardar 30-60 min)...
ssh %SSH% %DESTINO% "sudo mkdir -p /opt/pacha-enso && sudo tar -xzf /tmp/pacha.tgz -C /opt/pacha-enso && sudo bash /opt/pacha-enso/servidor/instalar.sh" || goto :error
echo.
echo Publicado. Arriba aparecen la direccion publica y la del panel de administracion.
pause
exit /b 0

:error
echo.
echo Algo fallo. Revise la IP, la clave y que el servidor este encendido.
pause
exit /b 1
