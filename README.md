# Mapa de Recursos del Tercer Sector en Canarias

Pipeline de extracción, normalización, geocodificación, QA y carga a Supabase
de las entidades del tercer sector (violencia machista, dependencia,
discapacidad, infancia, juventud, mayores, migraciones, vivienda, etc.) en
Canarias. Incluye visualización web (Leaflet + Supabase REST) servida por
GitHub Pages.

Mantenido por **ODESOCAN — Observatorio Social de Canarias**.

## Estructura

```
.
├── .github/workflows/         GitHub Actions (pipeline + Pages)
├── R/                         Utilidades R compartidas
│   ├── utils_pdf.R            CLI args, logging, escritura segura, etc.
│   ├── utils_scraper.R        Scraping (httr2 + retries)
│   └── canarias_geo.R         Lookups municipio↔isla, bbox por isla, etc.
├── pipeline/
│   ├── 01_extraccion/         PDFs (residencias, VG/discapacidad) + scraper
│   ├── 02_transformacion/     Normalización + categoría temática
│   ├── 03_geocodificacion/    Mapbox + Nominatim en cascada
│   ├── 04_analisis/           QA y dataset canónico (xlsx + csv)
│   └── 05_supabase/           Upsert PostgREST a recursos.entidades
├── data/                      Tablas de referencia (islas, municipios, CP)
├── sources/  (= fuentes/)     PDFs origen
├── salidas/                   Salidas regenerables (gitignored)
├── cache/                     Cache de geocoding y scraper (gitignored)
├── logs/                      Logs por ejecución (gitignored)
├── supabase/migrations/       Migraciones SQL
├── web/                       index.html publicable en GitHub Pages
├── scripts/                   Orquestador y wrappers shell
└── .env.example               Plantilla de variables de entorno
```

## Quickstart local

1. Instalar R ≥ 4.3 y dependencias del sistema (Linux/macOS):
   `gdal`, `proj`, `sqlite3`, `libxml2`, `libssl-dev`.
2. Copiar credenciales:
   ```bash
   cp .env.example .env
   # editar .env con las claves reales
   ```
3. Instalar paquetes R:
   ```bash
   Rscript scripts/run_pipeline.R --install-deps
   ```
4. Ejecutar pipeline completo:
   ```bash
   Rscript scripts/run_pipeline.R
   # o para subir a Supabase:
   Rscript scripts/run_pipeline.R --to supabase
   ```
   Wrappers shell equivalentes:
   ```bash
   bash scripts/run_full.sh    # pipeline completo
   bash scripts/run_resume.sh  # reanuda desde transformación
   ```

## Etapas

| Stage | Script | Output |
|---|---|---|
| 01 extraccion | `pipeline/01_extraccion/extract_*.R`, `scrape_*.R` | `salidas/01_extraccion/*.csv` |
| 02 transformacion | `pipeline/02_transformacion/transform_recursos.R` | `salidas/02_transformacion/recursos_normalizados.csv` |
| 03 geocodificacion | `pipeline/03_geocodificacion/geocode_recursos.R` | `salidas/03_geocodificacion/recursos_geocodificados.{csv,geojson}` |
| 04 analisis | `pipeline/04_analisis/qa_recursos.R` | `salidas/04_analisis/recursos_canonicos.csv`, `directorio_recursos.xlsx`, etc. |
| 05 supabase | `pipeline/05_supabase/upload_supabase.R` | Upsert a `recursos.entidades` |

## Visualización

`web/index.html` es un mapa Leaflet que carga los recursos en runtime desde
Supabase REST (clave publishable, sólo lectura). No requiere rebuild cuando
los datos cambian: la web siempre refleja el estado actual de la BBDD.

Despliegue: GitHub Pages con `web/` como root del sitio.

## CI/CD

- **`.github/workflows/pipeline.yml`** — corre el pipeline completo con cron
  semanal y permite disparo manual. Sube cambios a Supabase via secretos.
  Cachea paquetes R y `cache/geocoding_cache.csv` entre runs.
- **`.github/workflows/pages.yml`** — publica `web/` en GitHub Pages cada vez
  que cambia algo en `web/` en `main`.

Secretos requeridos en el repo:
- `SUPABASE_URL`
- `SUPABASE_SERVICE_ROLE_KEY`
- `MAPBOX_ACCESS_TOKEN`

## Schema Supabase

`supabase/migrations/0001_create_schema_recursos.sql` crea el schema
`recursos`, la tabla `entidades` (con geometría PostGIS generada), índices,
RLS de lectura pública y trigger de `updated_at`. Aplícala desde el dashboard
o con la Supabase CLI.

## Licencia

Datos: CC BY-SA 4.0 (atribución a ODESOCAN). Código: MIT.
