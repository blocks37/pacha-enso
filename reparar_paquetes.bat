@echo off
rem ENSO Peru - repara y actualiza los paquetes de R.
rem Antes: cierre RStudio y la app. Despues: doble clic en este archivo.
chcp 65001 >nul
cd /d "%~dp0"
set "RSCRIPT="
for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
if not defined RSCRIPT (
  where Rscript >nul 2>nul && set "RSCRIPT=Rscript"
)
if not defined RSCRIPT (
  echo No se encontro R. Abra R y corra: source^("install.R"^)
  pause
  exit /b 1
)
tasklist /FI "IMAGENAME eq rstudio.exe" 2>nul | find /I "rstudio.exe" >nul && (
  echo RStudio sigue abierto. Cierrelo por completo y vuelva a hacer doble clic en este archivo.
  pause
  exit /b 1
)
echo Usando "%RSCRIPT%"
echo.
"%RSCRIPT%" install.R
echo.
pause
