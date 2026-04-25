## Mapeador de temática y tipo de entidad.
## Separa dos dimensiones que antes se confundian:
##   - categoria_principal: tematica (discapacidad, mayores, educacion, ...)
##   - tipo_entidad:       forma/tipo (residencia, asociacion, servicio_especializado)
## La tematica se infiere combinando (actividad, area/seccion, descripcion,
## entidad) con regex sobre texto normalizado ASCII en mayusculas.
## Las reglas mas especificas van primero para evitar colisiones.

suppressPackageStartupMessages({
  library(stringr)
  library(stringi)
})

CATEGORIAS_TEMATICAS_ORDER <- c(
  "Violencia machista",
  "Igualdad y mujer",
  "LGTBI+",
  "Dependencia",
  "Discapacidad",
  "Personas mayores",
  "Infancia",
  "Juventud",
  "Sanidad",
  "Adicciones",
  "Migraciones",
  "Minorias etnicas",
  "Vivienda y exclusion",
  "Educacion",
  "Patrimonio",
  "Cultura",
  "Deportes",
  "Medio ambiente",
  "Animales",
  "Cooperacion y desarrollo",
  "Religion",
  "Profesional",
  "Vecinal",
  "Otros"
)

CATEGORIAS_TEMATICAS_PATTERNS <- list(
  "Violencia machista"        = "VIOLENCIA (DE |MACHIST|GENERO)|MALTRATO|AGRESION SEXUAL|FEMICIDI",
  "Igualdad y mujer"          = "\\bMUJER(ES)?\\b|\\bIGUALDAD\\b|FEMINIS|GENERO",
  "LGTBI+"                    = "LGBT|LGTB|TRANSGENER|HOMOSEXUAL|BISEXUAL|DIVERSIDAD SEXUAL|ORIENTACION SEXUAL",
  "Dependencia"               = "DEPENDENCI|CUIDADOR(AS|ES)?|CUIDADO\\b",
  "Discapacidad"              = "DISCAPACI|MINUSVALI|SORDERA|SORDOS?\\b|CIEGOS?\\b|AUTIS|ASPERG|SINDROME DE DOWN|PARALISIS CEREBRAL|ESCLEROSIS|ALZHEIMER|PARKINSON|DIVERSIDAD FUNCIONAL|INVIDENTE",
  "Personas mayores"          = "\\bMAYORES?\\b|TERCERA EDAD|ANCIAN|JUBILAD|PENSIONIST|GERIATR",
  "Infancia"                  = "INFANCIA|INFANTIL|MENORES?\\b|NINOS?\\b|NINAS?\\b",
  "Juventud"                  = "JUVENTUD|JOVEN(ES)?\\b|ADOLESCEN|DE EX.?ALUMNOS|UNIVERSITAR",
  "Sanidad"                   = "\\bSALUD\\b|SANITAR|ENFERM(OS|EDAD|ERIA)|PACIENTES|HOSPITAL|CANCER|SIDA|VIH|ENFERMEDADES RARAS|CLINIC",
  "Adicciones"                = "ADICCION|DROGODEPEN|DROGA|ALCOHOL|LUDOPATI|TABAQUIS",
  "Migraciones"               = "MIGRA|INMIGRA|REFUGIAD|EXTRANJER|INTERCULTURAL",
  "Minorias etnicas"          = "GITAN|ETNIA|RACIAL|RACISMO|AFRO(DESCEN)?|INDIGEN",
  "Vivienda y exclusion"      = "EXCLUSION|POBREZA|SIN HOGAR|SIN TECHO|VIVIENDA|DESAHUCIO",
  "Educacion"                 = "EDUCAC|ESCOLAR|ESCUELA|ACADEMIC|AMPA|PADRES DE ALUMNOS|MADRES Y PADRES|PROTECTORES DE CENTROS",
  "Patrimonio"                = "PATRIMONIO|HISTORIC|ARQUEOL|MUSEO|FOLKLOR|TRADICION|CASAS REGIONALES",
  "Cultura"                   = "CULTURA|CULTURAL|ARTIST|MUSIC|TEATRO|CINE|LITERAT|DANZA|PLASTIC",
  "Deportes"                  = "DEPORT|RECREAT",
  "Medio ambiente"            = "AMBIENT|ECOLOG|NATURA|FAUNA|FLORA|CONSERVACION|RECICL",
  "Animales"                  = "\\bANIMAL(ES)?\\b|PROTECCION ANIMAL|PERR(OS|UN)|\\bGATO(S)?\\b|FELIN|CANIN",
  "Cooperacion y desarrollo"  = "COOPERACION|DESARROLLO|ONG[D]?|HUMANITAR|SOLIDARIDAD INTERNACIONAL",
  "Religion"                  = "RELIGIOS|CATOLIC|EVANGELI|CRISTIAN|ISLAM|JUDI|BUDIS|HERMANDAD|COFRADI",
  "Profesional"               = "PROFESIONAL|COLEGIAL|GREMIAL|EMPRESARIAL|COMERCIAN|AUTONOM|SINDICA",
  "Vecinal"                   = "VECINAL|VECINOS|COMUNIDAD DE PROPIETARIOS|URBANISM"
)

.normalize_match_text <- function(...) {
  parts <- c(...)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (!length(parts)) return("")
  text <- paste(parts, collapse = " ")
  text <- stringi::stri_trans_general(text, "Latin-ASCII")
  str_to_upper(text)
}

infer_categoria_tematica <- function(actividad = NA_character_,
                                     area = NA_character_,
                                     descripcion = NA_character_,
                                     entidad = NA_character_,
                                     subcategoria = NA_character_,
                                     default = "Otros") {
  text <- .normalize_match_text(actividad, area, descripcion, entidad, subcategoria)
  if (!nchar(text)) return(default)
  for (cat in CATEGORIAS_TEMATICAS_ORDER) {
    pattern <- CATEGORIAS_TEMATICAS_PATTERNS[[cat]]
    if (is.null(pattern)) next
    if (str_detect(text, regex(pattern, ignore_case = FALSE))) {
      return(cat)
    }
  }
  default
}

## tipo_entidad: forma juridica / tipo funcional del recurso.
## Valores: "residencia", "servicio_especializado", "asociacion".
infer_tipo_entidad <- function(fuente_tipo) {
  switch(as.character(fuente_tipo),
    pdf_residencias         = "residencia",
    pdf_vg_discapacidad     = "servicio_especializado",
    registro_asociaciones   = "asociacion",
    NA_character_
  )
}
