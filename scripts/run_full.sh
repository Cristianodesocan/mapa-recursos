#!/bin/bash
# Pipeline completo en secuencia. Logs por fase en logs/<fecha>/
set -u
cd "$(dirname "$0")/.."

TS=$(date +%Y%m%d_%H%M%S)
LOG_DIR="logs/$TS"
mkdir -p "$LOG_DIR"

# Cargar credenciales desde .env sin shell eval (parentesis-safe).
if [ -f .env ]; then
  export MAPBOX_ACCESS_TOKEN="$(grep '^MAPBOX_ACCESS_TOKEN=' .env | cut -d= -f2-)"
  export MAPA_RECURSOS_UA="$(grep '^MAPA_RECURSOS_UA=' .env | cut -d= -f2-)"
  export SUPABASE_URL="$(grep '^SUPABASE_URL=' .env | cut -d= -f2-)"
  export SUPABASE_SERVICE_ROLE_KEY="$(grep '^SUPABASE_SERVICE_ROLE_KEY=' .env | cut -d= -f2-)"
  export SUPABASE_SCHEMA="$(grep '^SUPABASE_SCHEMA=' .env | cut -d= -f2-)"
  export SUPABASE_TABLE="$(grep '^SUPABASE_TABLE=' .env | cut -d= -f2-)"
fi

step() { echo "::: [$(date +%H:%M:%S)] $1 :::"; }
run() {
  local name="$1"; shift
  step "START $name"
  "$@" > "$LOG_DIR/$name.log" 2>&1
  local rc=$?
  if [ $rc -ne 0 ]; then
    step "FAIL  $name (rc=$rc) - ver $LOG_DIR/$name.log"
    exit $rc
  fi
  step "OK    $name"
}

run "1_scraper"      Rscript pipeline/01_extraccion/scrape_registro_asociaciones.R
run "2_transform"    Rscript pipeline/02_transformacion/transform_recursos.R   --root "$(pwd)"
run "3_geocode"      Rscript pipeline/03_geocodificacion/geocode_recursos.R    --root "$(pwd)" --geocoder cascade
run "4_analisis"     Rscript pipeline/04_analisis/qa_recursos.R                --root "$(pwd)"
run "6_export_web"   Rscript pipeline/06_export_web/build_web_data.R           --root "$(pwd)"
run "5_supabase"     Rscript pipeline/05_supabase/upload_supabase.R            --root "$(pwd)"

step "TODO OK"
echo ""
echo "Log dir: $LOG_DIR"
echo "Resumen:"
grep -E "Registros recuperados|Dataset normalizado|Geocodificados|QA|Carga completada" "$LOG_DIR"/*.log || true
