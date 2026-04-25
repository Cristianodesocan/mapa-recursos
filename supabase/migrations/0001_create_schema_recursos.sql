-- =====================================================================
-- Migracion 0001: Schema "recursos" para Mapa de Recursos (ODESOCAN)
-- =====================================================================
-- - Una fila por entidad (id_recurso = hash xxhash64 estable)
-- - Geometria PostGIS generada desde lat/lon
-- - RLS: lectura publica (anon + authenticated), escritura solo service_role
-- - Indices: isla, municipio, categoria, fuente_tipo, geom (GIST),
--   cif (parcial), entidad (trigram para busqueda fuzzy)
-- =====================================================================

-- Supabase: postgis vive en schema "general" y pg_trgm en "extensions".
-- Ajustamos search_path para que tipos y operadores resuelvan sin calificar.
SET search_path TO public, general, extensions;

CREATE SCHEMA IF NOT EXISTS recursos;

-- ---------- Tabla principal ----------
CREATE TABLE IF NOT EXISTS recursos.entidades (
  id_recurso            text        PRIMARY KEY,

  -- Procedencia
  fuente_tipo           text        NOT NULL,
  fuente_archivo        text,
  fuente_url            text,
  identificador_fuente  text,
  pagina                integer,

  -- Clasificacion
  categoria_principal   text,
  subcategoria          text,
  area                  text,
  ambito                text,

  -- Ubicacion administrativa
  comunidad_autonoma    text,
  provincia             text,
  isla                  text,
  municipio             text,
  codigo_postal         text,

  -- Identidad de la entidad
  entidad               text        NOT NULL,
  cif                   text,
  descripcion           text,
  direccion             text,

  -- Contacto
  telefono              text,
  email                 text,
  web                   text,
  horario               text,

  -- Metadata especifica
  plazas                integer,
  recurso_igualdad      text,
  vigente               boolean,

  -- Calidad
  estado_registro       text,

  -- Geocoding
  lat                   double precision,
  lon                   double precision,
  geom                  general.geography(Point, 4326) GENERATED ALWAYS AS (
    CASE
      WHEN lat IS NOT NULL AND lon IS NOT NULL
      THEN general.ST_SetSRID(general.ST_MakePoint(lon, lat), 4326)::general.geography
      ELSE NULL
    END
  ) STORED,
  geocode_status        text,
  geocode_source        text,
  match_level           text,
  validated_in_island   boolean,

  -- Timestamps
  created_at            timestamptz NOT NULL DEFAULT now(),
  updated_at            timestamptz NOT NULL DEFAULT now()
);

COMMENT ON TABLE  recursos.entidades            IS 'Recursos sociales en Canarias: residencias, VG, discapacidad, registro de asociaciones';
COMMENT ON COLUMN recursos.entidades.id_recurso IS 'Hash xxhash64 estable de (fuente_tipo, identificador_fuente, entidad, direccion); apto para UPSERT';
COMMENT ON COLUMN recursos.entidades.geom       IS 'Geografia (lon, lat) generada automaticamente cuando hay coordenadas';

-- ---------- Indices ----------
CREATE INDEX IF NOT EXISTS idx_entidades_isla         ON recursos.entidades (isla);
CREATE INDEX IF NOT EXISTS idx_entidades_municipio    ON recursos.entidades (municipio);
CREATE INDEX IF NOT EXISTS idx_entidades_categoria    ON recursos.entidades (categoria_principal);
CREATE INDEX IF NOT EXISTS idx_entidades_fuente_tipo  ON recursos.entidades (fuente_tipo);
CREATE INDEX IF NOT EXISTS idx_entidades_geom         ON recursos.entidades USING GIST (geom);
CREATE INDEX IF NOT EXISTS idx_entidades_cif          ON recursos.entidades (cif) WHERE cif IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_entidades_entidad_trgm ON recursos.entidades USING GIN (entidad extensions.gin_trgm_ops);

-- ---------- Trigger de updated_at ----------
-- search_path = '' (vacio) para evitar shadowing attacks (advisor 0011).
CREATE OR REPLACE FUNCTION recursos.set_updated_at()
RETURNS trigger
LANGUAGE plpgsql
SECURITY INVOKER
SET search_path = ''
AS $$
BEGIN
  NEW.updated_at := now();
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_entidades_updated_at ON recursos.entidades;
CREATE TRIGGER trg_entidades_updated_at
  BEFORE UPDATE ON recursos.entidades
  FOR EACH ROW EXECUTE FUNCTION recursos.set_updated_at();

-- ---------- Permisos al schema ----------
GRANT USAGE ON SCHEMA recursos TO anon, authenticated, service_role;
GRANT SELECT ON ALL TABLES IN SCHEMA recursos TO anon, authenticated;
GRANT ALL    ON ALL TABLES IN SCHEMA recursos TO service_role;
ALTER DEFAULT PRIVILEGES IN SCHEMA recursos
  GRANT SELECT ON TABLES TO anon, authenticated;
ALTER DEFAULT PRIVILEGES IN SCHEMA recursos
  GRANT ALL ON TABLES TO service_role;

-- ---------- Row Level Security ----------
ALTER TABLE recursos.entidades ENABLE ROW LEVEL SECURITY;

-- Lectura publica: todos pueden SELECT
DROP POLICY IF EXISTS "lectura_publica_entidades" ON recursos.entidades;
CREATE POLICY "lectura_publica_entidades"
  ON recursos.entidades
  FOR SELECT
  TO anon, authenticated
  USING (true);

-- Escritura: solo service_role (bypassea RLS automaticamente).
-- No creamos policies de INSERT/UPDATE/DELETE para anon/authenticated:
-- al estar RLS activo y sin policy, esas operaciones quedan bloqueadas.

-- =====================================================================
-- IMPORTANTE: Para que el schema "recursos" sea accesible via PostgREST
-- (REST API y supabase-js), exponerlo en el Dashboard:
--   Project Settings > API > "Exposed schemas" -> agregar "recursos"
-- (No se puede hacer 100% por SQL; lo gestiona el API gateway de Supabase.)
-- =====================================================================
