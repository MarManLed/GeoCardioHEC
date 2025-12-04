#######################################################################################
# 1. Librerias
#######################################################################################
library(Hmisc)
library(odbc) 
library(dplyr)
library(lubridate)
library(stringr)
library(stringi)
library(sf)
library(dplyr)
library(stringr)
library(geoAr)
library(mgcv)

#######################################################################################
# 2. Funciones custom
#######################################################################################
.clean_side_one <- function(s) {
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  
  s <- toupper(s)
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- stringr::str_replace_all(s, "\\s+", " ")
  s <- stringr::str_trim(s)
  
  # --- Normalización de N°, Nº, N 1400, #1400, etc. ---
  # Quitar "Nº / N° / NO / N 1400" sin tocar la N final de TUCUMAN, etc.
  s <- stringr::str_replace_all(
    s,
    "(?<![A-Z0-9])N[º°O]?\\s*(\\d+)",
    " \\1"
  )
  
  # Quitar N suelta tipo " ... N ..."
  s <- stringr::str_replace_all(s, "\\bN[º°O]?\\b", " ")
  
  # Quitar "# 1400", "#1400", etc.
  s <- stringr::str_replace_all(
    s,
    "(?<!\\S)#\\s*(\\d+)",
    " \\1"
  )
  
  s <- stringr::str_squish(s)
  
  # --- Normalizar CALLE / AVENIDA ---
  s <- stringr::str_replace(s, "\\bCALLE\\s+CALLE\\b", "CALLE ")
  s <- stringr::str_replace(s, "\\bAV\\.?\\s+AV\\.?\\b", "AV ")
  s <- stringr::str_replace_all(s, "\\bAV\\.?\\b", "AVENIDA")
  
  # --- Heurística para calles numéricas ---
  tokens <- stringr::str_split(s, " +")[[1]]
  
  if (length(tokens) > 0 && stringr::str_detect(tokens[1], "^\\d+$")) {
    
    # 1) Caso especial: "56 A 4822 BERAZATEGUI" -> CALLE 56 4822 BERAZATEGUI
    if (length(tokens) >= 3 &&
        stringr::str_detect(tokens[2], "^[A-Z]$") &&
        stringr::str_detect(tokens[3], "^\\d+$")) {
      
      # Dropeamos la letra intermedia tipo "A"
      rest_tokens <- c(tokens[1], tokens[3:length(tokens)])
      s <- paste("CALLE", paste(rest_tokens, collapse = " "))
      
    } else {
      remaining <- if (length(tokens) >= 2) tokens[2:length(tokens)] else character(0)
      remaining_has_digits <- any(stringr::str_detect(remaining, "^\\d+$"))
      
      if (!remaining_has_digits) {
        # 2) No hay otro número puro después:
        #    ej: "149 A", "35 BERAZATEGUI" -> CALLE 149 / CALLE 35 BERAZATEGUI
        
        if (length(tokens) >= 2 && stringr::str_detect(tokens[2], "^[A-Z]$")) {
          # "149 A" -> CALLE 149
          rest_tokens <- if (length(tokens) >= 3) tokens[3:length(tokens)] else character(0)
        } else {
          # "35 BERAZATEGUI" -> CALLE 35 BERAZATEGUI
          rest_tokens <- remaining
        }
        
        s <- paste("CALLE", tokens[1], paste(rest_tokens, collapse = " "))
        
      } else if (length(tokens) >= 2 && stringr::str_detect(tokens[2], "^\\d+$")) {
        # 3) Patrón "898 2472 QUILMES" -> CALLE 898 2472 QUILMES
        rest_tokens <- remaining
        s <- paste("CALLE", tokens[1], paste(rest_tokens, collapse = " "))
      }
      # Si es "9 JULIO 2403 MONTE GRANDE":
      #   tokens[1] = "9" (digito)
      #   remaining = c("JULIO","2403","MONTE","GRANDE")
      #   remaining_has_digits = TRUE
      #   PERO tokens[2] no es digito -> no entra al caso 3 => se deja tal cual.
    }
    
    s <- stringr::str_squish(s)
  }
  
  if (!nzchar(s)) NA_character_ else s
}
normalizar_interseccion_one <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  x <- toupper(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  x <- str_replace_all(x, "\\bN[º°O]?\\b", " ")
  x <- str_replace_all(x, "(?<=\\S)#(?=\\S)|\\s#\\s", " ")
  x <- str_squish(x)
  
  if (str_detect(x, "\\bY\\b")) {
    parts <- str_split(x, "\\bY\\b", n = 2, simplify = TRUE)
    left  <- .clean_side_one(parts[,1])
    right <- .clean_side_one(parts[,2])
    if (!is.na(left) && nchar(left) >= 3 && !is.na(right) && nchar(right) >= 3) {
      out <- paste0(left, " y ", right)
    } else {
      out <- .clean_side_one(x)
    }
  } else {
    out <- .clean_side_one(x)
  }
  
  out <- str_replace_all(out, "\\s+", " ")
  out <- str_trim(out)
  if (!nzchar(out)) NA_character_ else out
}
normalizar_direccion2 <- function(x_vec) {
  x_vec <- as.character(x_vec)
  vapply(x_vec, normalizar_interseccion_one, FUN.VALUE = character(1))
}
LOCALIDADES_OK <- c(
  "FLORENCIO VARELA", "FCIO VARELA", "(F.VARELA)","FLORENCIO VARELA.", 
  "BERAZATEGUI",
  "LOMAS DE ZAMORA",
  "ENSENADA",
  "QUILMES",
  "LA PLATA",
  "ALMIRANTE BROWN"
)

detectar_localidad_cola <- function(x) {
  x <- as.character(x)
  
  # Matchea: "prefijo que termina en dígito" + espacio + "cola"
  # El prefijo es GREEDY (.+\\d), así que se lleva el ÚLTIMO número
  m <- stringr::str_match(x, "^(.+\\d)\\s+(.+)$")
  
  # m[,3] es lo que viene después del último número -> localidad candidata
  loc <- m[, 3]
  loc <- stringr::str_trim(loc)
  
  # vacíos a NA
  loc[loc == ""] <- NA_character_
  loc
}
extraer_localidad <- function(dir_norm, domic,
                              default_loc = "FLORENCIO VARELA") {
  dir_norm  <- as.character(dir_norm)
  domic     <- as.character(domic)
  
  # localidad desde la cola de la dirección
  loc_from_dir <- detectar_localidad_cola(dir_norm)
  
  # localidad desde 'domic' usando LOCALIDADES_OK
  rx_loc <- regex(paste0("\\b(", paste(LOCALIDADES_OK, collapse="|"), ")\\b"),
                  ignore_case = TRUE)
  loc_from_domic <- str_to_upper(str_extract(domic, rx_loc))
  
  # lógica de prioridad:
  # 1) lo que viene al final de la dirección (RAFAEL CALZADA)
  # 2) localidad explícita en 'domic' (FCIO VARELA, BERAZATEGUI, etc.)
  # 3) default FLORENCIO VARELA
  out <- ifelse(
    !is.na(loc_from_dir) & nzchar(loc_from_dir), loc_from_dir,
    ifelse(!is.na(loc_from_domic) & nzchar(loc_from_domic), loc_from_domic, default_loc)
  )
  out
}
normalizar_localidad_nombre <- function(loc) {
  loc_raw <- as.character(loc)
  
  loc_clean <- loc_raw %>%
    toupper() %>%
    stri_trans_general("Latin-ASCII") %>%     # Saca acentos
    str_replace_all("[\\.,\\-]", " ") %>%     # ., - a espacio
    str_replace_all("\\s+", " ") %>%          # espacios múltiples -> uno
    str_trim() %>%
    # fuera paréntesis externos
    str_replace_all("^\\(|\\)$", "") %>%
    # sacar textos de provincia / país
    str_replace_all("\\b(BS\\.?\\s*AS|BUENOS AIRES)\\b", "") %>%
    str_squish()
  
  # Mapeo a nombres canónicos
  canon <- case_when(
    # FLORENCIO VARELA
    str_detect(loc_clean, "FLORENCIO VARELA|F\\. ?VARELA|F VARELA|FCIO ?VARELA|FCIOVARELA|FCO\\. ?VARELA|FLORECIO VARELA|FLOENCIO VARELA|FLORENCIA VARELA|FLORENCIO VRELA|Bº .*FLORENCIO VARELA|HUDSON FLORENCIO VARELA|ZEBALLOS FLORENCIO VARELA|BOSQUE FLORENCIO VARELA|VARELA$") ~
      "FLORENCIO VARELA",
    
    # BERAZATEGUI
    str_detect(loc_clean, "BERAZATEGUI|BERZATEGUI|BGUI") ~ "BERAZATEGUI",
    
    # QUILMES OESTE (podés decidir si lo querés separado)
    str_detect(loc_clean, "QUILMES OESTE") ~ "QUILMES OESTE",
    
    # QUILMES
    str_detect(loc_clean, "QUILMES|QUIMLES|QUILES") ~ "QUILMES",
    
    # ALMIRANTE BROWN
    str_detect(loc_clean, "ALMIRANTE BROWN|ALTE ?BROWN|ALMIGRANTE BROWN|ALTE\\. ?BROWN|ALMIRANTE BRAWN|ALMIRANTE BROW|ALMIRANTE BRWN|ALTE ROWN") ~
      "ALMIRANTE BROWN",
    
    # LANUS
    str_detect(loc_clean, "LANUS") ~ "LANUS",
    
    # AVELLANEDA (incluyo Wilde / Villa Domínico / Gerli como Avellaneda)
    str_detect(loc_clean, "AVELLANEDA|WILDE|VILLA DOMINICO|SARANDI|GERLI") ~ "AVELLANEDA",
    
    # LOMAS DE ZAMORA (incluyo Ing. Budge, etc.)
    str_detect(loc_clean, "LOMAS DE ZAMORA|ING ?BUDGE|INQ ?BUDGE|LOMAS CASAS") ~ "LOMAS DE ZAMORA",
    
    # SAN FRANCISCO SOLANO
    str_detect(loc_clean, "SAN FRANCISCO SOLANO|SAN FCO SOLANO|SAN FRANCISCA SOLANO|SAN FRANCISCO SOLANA") ~ "SAN FRANCISCO SOLANO",
    str_detect(loc_clean, "^SOLANO($| )|IS SOLANO|BIS SOLANO|NRO\\. ?\\d+ SOLANO") ~ "SAN FRANCISCO SOLANO",
    
    # RAFAEL CALZADA
    str_detect(loc_clean, "RAFAEL CALZADA|R CALZADA|R\\. CALZADA|RAFAEL CALZDA|RAFEL CALZADA|RAFAELCALZADA") ~ "RAFAEL CALZADA",
    
    # BURZACO
    str_detect(loc_clean, "BURZACO|BUZACO") ~ "BURZACO",
    
    # CLAYPOLE
    str_detect(loc_clean, "CLAYPOLE|CLYPOLE") ~ "CLAYPOLE",
    
    # LONGCHAMPS
    str_detect(loc_clean, "LONGCHAMPS|LONGCHAMS|LONGCHANG|LONCHAMGS|LONCHAMPS|LONG CHAMPS") ~ "LONGCHAMPS",
    
    # MONTE GRANDE
    str_detect(loc_clean, "MONTE GRANDE") ~ "MONTE GRANDE",
    
    # BERNAL
    str_detect(loc_clean, "BERNAL") ~ "BERNAL",
    
    # RANELAGH
    str_detect(loc_clean, "RANELAGH|RANELANG|RANELANGH") ~ "RANELAGH",
    
    # HUDSON
    str_detect(loc_clean, "HUDSON") ~ "HUDSON",
    
    # GUERNICA
    str_detect(loc_clean, "GUERNICA") ~ "GUERNICA",
    
    # SAN VICENTE
    str_detect(loc_clean, "SAN VICENTE") ~ "SAN VICENTE",
    
    # EZEIZA
    str_detect(loc_clean, "EZEIZA|TRISTAN SUAREZ") ~ "EZEIZA",
    
    # LA PLATA
    str_detect(loc_clean, "LA PLATA") ~ "LA PLATA",
    
    # ENSENADA
    str_detect(loc_clean, "ENSENADA") ~ "ENSENADA",
    
    # Si no matchea nada, devolvemos la versión limpia
    TRUE ~ loc_clean
  )
  
  canon
}
completar_localidad <- function(x, domic,
                                default_loc = "FLORENCIO VARELA",
                                prov = "BUENOS AIRES",
                                pais = "ARGENTINA") {
  x     <- as.character(x)
  domic <- as.character(domic)
  
  # ---- 1) Separar "base" (calle + número) y "cola" (posible localidad) en x ----
  # Ej: "TUCUMAN 1401 RAFAEL CALZADA" 
  #  -> base = "TUCUMAN 1401"; cola = "RAFAEL CALZADA"
  m <- stringr::str_match(x, "^(.+\\d)\\s+(.+)$")
  base_x   <- stringr::str_trim(m[, 2])
  cola_x   <- stringr::str_trim(m[, 3])
  
  # Si no matchea patrón "algo+numero localidad", dejamos base = x y cola = NA
  base_x[is.na(base_x)] <- x[is.na(base_x)]
  cola_x[cola_x == ""]  <- NA_character_
  
  # ---- 2) Localidad candidata desde x (cola) y domic, ambas depuradas ----
  loc_from_x_raw    <- cola_x
  loc_from_x_clean  <- normalizar_localidad_nombre(loc_from_x_raw)
  loc_from_x_clean[loc_from_x_clean == ""] <- NA_character_
  
  # Desde domic: tomamos lo que venga después del último número y lo depuramos
  loc_from_domic_raw   <- detectar_localidad_cola(domic)
  loc_from_domic_clean <- normalizar_localidad_nombre(loc_from_domic_raw)
  loc_from_domic_clean[loc_from_domic_clean == ""] <- NA_character_
  
  # ---- 3) Localidad final canónica ----
  localidad_final <- ifelse(
    !is.na(loc_from_x_clean) & nzchar(loc_from_x_clean),
    loc_from_x_clean,
    ifelse(
      !is.na(loc_from_domic_clean) & nzchar(loc_from_domic_clean),
      loc_from_domic_clean,
      default_loc
    )
  )
  
  # ---- 4) Construir la dirección completa ----
  is_empty_x <- is.na(x) | !nzchar(x)
  has_base   <- !is.na(base_x) & nzchar(base_x)
  
  res <- character(length(x))
  
  # 4a) Cuando no hay dirección (x vacía/NA) -> solo localidad + prov + país
  res[is_empty_x] <- paste0(localidad_final[is_empty_x], ", ", prov, ", ", pais)
  
  # 4b) Cuando hay dirección
  idx <- which(!is_empty_x)
  if (length(idx) > 0) {
    # de esos, los que tienen base separada de localidad
    idx_base <- idx[has_base[idx]]
    if (length(idx_base) > 0) {
      res[idx_base] <- paste0(
        base_x[idx_base], ", ",
        localidad_final[idx_base], ", ",
        prov, ", ", pais
      )
    }
    
    # los que no tienen número para separar: usamos x tal cual
    idx_nobase <- setdiff(idx, idx_base)
    if (length(idx_nobase) > 0) {
      res[idx_nobase] <- paste0(
        x[idx_nobase], ", ",
        localidad_final[idx_nobase], ", ",
        prov, ", ", pais
      )
    }
  }
  
  # ---- 5) Fallback por longitud muy corta ----
  too_short <- !is.na(res) & nchar(res) < 10
  res[too_short] <- paste0(localidad_final[too_short], ", ", prov, ", ", pais)
  
  res
}

#######################################################################################
### 3. Carga de mapa de radios/sectores censales (del INDEC) para Florencio Varela.
#######################################################################################

pba_dept <- get_geo(geo = "BUENOS AIRES", level = "departamento")  
pba_dept$coddepto_censo <- as.character(pba_dept$coddepto_censo)
targets <- c("FLORENCIO VARELA","BERAZATEGUI","LOMAS DE ZAMORA",
             "ENSENADA","QUILMES","LA PLATA","ALMIRANTE BROWN")
CodesPBA <- radios_ar %>% 
  filter(NOMDEPTO %in% targets)
deptos <- unique(CodesPBA$DEPTO)
fv <- pba_dept %>% 
  filter(coddepto_censo %in% deptos)
url_zip <- "https://catalogo.datos.gba.gob.ar/dataset/33b080d2-e369-4076-acd4-511db0e9bffb/resource/151d80d2-87c1-4981-9bea-9aab38a82ec9/download/radios-censales-2022-geojson.zip"
zipf <- tempfile(fileext = ".zip")
download.file(url_zip, zipf, mode = "wb", quiet = TRUE)
files_in_zip <- unzip(zipf, list = TRUE)$Name
geojson_inside <- files_in_zip[grepl("\\.geojson$", files_in_zip, ignore.case = TRUE)][1]
stopifnot(length(geojson_inside) == 1)
radios_ar <- st_read(paste0("/vsizip/", zipf, "/", geojson_inside), quiet = FALSE)
radios_ar <- st_make_valid(radios_ar) %>% st_transform(st_crs(fv))
radios_fv <- st_intersection(radios_ar, fv)

#######################################################################################
### 4. Carga de variable población por radio censal. Archivo obtenido de 
#######################################################################################

censo_radio <- read.xlsx("path/Pop_Radio.xlsx")
censo_radio <- censo_radio %>%
  rename(codgeo_raw = Código, poblacion_total = C1) %>%
  mutate(
    codgeo = str_pad(as.character(codgeo_raw), width = 9, pad = "0")  
  )
#######################################################################################
#######################################################################################
### 5. Carga, análisis, depuración y normalización de la base de datos de cardiología. 
#######################################################################################

MDBfile <- shQuote("path/Base de datos SCA El Cruce V.14-07-25 ESTA - copia.mdb")
pacientes <- mdb.get(MDBfile, tables = "Tablapacientes")
IAM_data <- mdb.get(MDBfile, tables = "Tablainternaciones")

#######################################################################################
### 6. Geocodificación de domicilios de pacientes (lon/lat). 
#######################################################################################

pac <- pacientes %>%
  mutate(
    domicilio_raw  = as.character(domic),
    domicilio_geo  = normalizar_direccion2(domicilio_raw),
    localidad      = extraer_localidad(domicilio_geo, domicilio_raw),
    domicilio_geo2 = completar_localidad(domicilio_geo, domicilio_raw)
  )

LOCALIDADES_OK <- c("FLORENCIO VARELA","BERAZATEGUI", "LOMAS DE ZAMORA",
  "ENSENADA", "QUILMES","LA PLATA","EZPELETA","ALMIRANTE BROWN","LANUS",
  "AVELLANEDA","SAN FRANCISCO SOLANO","RAFAEL CALZADA","BURZACO","CLAYPOLE",
  "MONTE GRANDE","LONGCHAMPS","BERNAL","RANELAGH","HUDSON","EZEIZA","GUERNICA","SAN VICENTE"
)

pac.1 <- pac %>%
  mutate(
    localidad_raw   = localidad,
    localidad_clean = normalizar_localidad_nombre(localidad_raw)
  ) %>%
  filter(
    localidad_clean %in% LOCALIDADES_OK
  )

SinNumero_domicilio <- pac.1 %>% 
  dplyr::filter(grepl("[0-9]", domic) == FALSE)

Pac_1 <- pac.1  %>% 
  anti_join(SinNumero_domicilio, by = "idpte")

options(tidygeocoder.progress_bar = TRUE)
Sys.setenv("TIDYGEOCODER_PROGRESS_BAR" = "TRUE")

set.seed(42)

geo_sf.1 <- geocode(
  Pac_1,
  address     = domicilio_geo2,  
  method      = "osm",
  lat         = latitude,
  long        = longitude,
  full_results = FALSE,
  limit       = 1,                             
  custom_query = list(countrycodes = "AR")      
)

geo_sf <- geo_sf.1 %>% 
  filter(!(is.na(latitude)))

pts <- st_as_sf(geo_sf, coords = c("longitude","latitude"), crs = 4326)

pts <- st_transform(pts, st_crs(radios_fv))

#######################################################################################
### 6.1 Determinación de pacientes no localizables
#######################################################################################

### sin numero ni domicilio n = 635

SinNumero_domicilio <- pac.1 %>% 
  dplyr::filter(grepl("[0-9]", domic) == FALSE)

### sin numero  n = 296

NA_domicilio <- pac.1 %>% 
  dplyr::filter(domic == "")

### sin domicilio  n = 339

No_Domicilio_1 <-  SinNumero_domicilio %>% 
  anti_join(NA_domicilio, by = "idpte")

### filtro de pacientes sin direcciones o mal escritas n = 1450

### direcciones no localizadas n = 388

geo_sf.NA <- geo_sf.1 %>% 
  filter(is.na(latitude))

## N de direcciones de pacientes detectadas = 1062 sobre un total de 1450 analizables

#######################################################################################
#### 6.2 Vinculación con FID censales y con datos de población por FID censal 
#######################################################################################

radios_fv_censo <- radios_fv %>%
  left_join(censo_radio %>% select(codgeo, poblacion_total),
            by = c("LINK" = "codgeo"))

pts_tag <- st_join(pts, radios_fv_censo[, c("fid", "poblacion_total")], join = st_within)

# Snap opcional: puntos fuera del partido -> al radio más cercano si están a < 100 m
fuera <- which(is.na(pts_tag$fid))
if(length(fuera) > 0){
  nearest <- st_nearest_feature(pts_tag[fuera,], radios_fv_censo)
  dist_near <- st_distance(pts_tag[fuera,], st_geometry(radios_fv_censo)[nearest], by_element = TRUE)
  use_snap <- as.numeric(dist_near) < 1000  # umbral en metros (ajustable)
  pts_tag$fid[fuera[use_snap]] <- radios_fv_censo$fid[nearest[use_snap]]
}

# Resultado tabular final para mergear
cardio_con_fid <- pts_tag %>%
  st_drop_geometry() %>%
  select(idpte, domicilio_raw, domicilio_geo, fid, poblacion_total)

#######################################################################################
#### 6.3 Incidencia de IAM por 100k por radio censal
#######################################################################################

# 1) Agregar por radio: casos y población (robusto a duplicados)
agg_radio <- cardio_con_fid %>%
  filter(!is.na(fid)) %>%
  group_by(fid) %>%
  summarise(
    casos_iam = n(),
    # tomar el único valor no-NA de población por radio
    poblacion = {
      vals <- unique(na.omit(poblacion_total))
      if (length(vals) > 1) warning("Más de un 'poblacion_total' en fid=", first(fid), "; uso el primero.")
      if (length(vals) == 0) NA_integer_ else vals[1]
    },
    .groups = "drop"
  ) %>%
  mutate(
    inc_acum     = ifelse(!is.na(poblacion) & poblacion > 0, casos_iam / poblacion, NA_real_),
    inc_x100k    = inc_acum * 1e5
  )

radios_ml <- radios_fv %>% 
  left_join(agg_radio, by = "fid")

#######################################################################################
#### 6.4 Tiempo de demora desde inicio de dolor hasta ingreso al hospital por radio censal
#######################################################################################

options(tz = "America/Argentina/Buenos_Aires")

IAM_data_times <- IAM_data %>%
  mutate(
    feindol  = str_remove_all(feindol,  "[()]"),
    feingoc  = str_remove_all(feingoc,  "[()]"),
    t_dolor  = suppressWarnings(mdy_hms(feindol, tz="America/Argentina/Buenos_Aires")),
    t_dolor  = ifelse(is.na(t_dolor),
                      dmy_hms(feindol, tz="America/Argentina/Buenos_Aires"),
                      t_dolor) %>% as.POSIXct(origin="1970-01-01", tz="America/Argentina/Buenos_Aires"),
    t_ingreso= suppressWarnings(mdy_hms(feingoc, tz="America/Argentina/Buenos_Aires")),
    t_ingreso= ifelse(is.na(t_ingreso),
                      dmy_hms(feingoc, tz="America/Argentina/Buenos_Aires"),
                      t_ingreso) %>% as.POSIXct(origin="1970-01-01", tz="America/Argentina/Buenos_Aires"),
    demora_min   = as.numeric(difftime(t_ingreso, t_dolor, units = "mins")),
    demora_min   = ifelse(is.na(t_dolor) | is.na(t_ingreso) | demora_min < 0, NA, demora_min),
    demora_hora  = demora_min / 60
  )

IAM_data_times.1 <- IAM_data_times[, c(1, 36, 44, 180:183)]

IAM_data_times.1 <- IAM_data_times.1 %>% 
  filter(!is.na(demora_hora))

IAM_data_times.1 <- IAM_data_times.1[, c(1,7)]

IAM_data_times.1$idpte <- as.numeric(IAM_data_times.1$idpte)

cardio_con_fid <- cardio_con_fid %>% 
  left_join(IAM_data_times.1, by = "idpte")

time_radio <- cardio_con_fid %>%
  filter(!is.na(fid)) %>%
  filter(!is.na(demora_hora)) %>%
  group_by(fid) %>%
  summarise(
    casos_tiempo = n(),
    mean_tiempo = mean(demora_hora, na.rm = TRUE)
  )

radios_ml <- radios_ml %>% 
  left_join(time_radio, by = "fid")

#######################################################################################
#### 6.5 Entrenamiento de un GAM “Generalized Additive Model” (s(x,y)) espacial
#######################################################################################
radios_fv2 <- radios_ml
radios_fv2 <- radios_fv2 %>% 
  filter(!(is.na(inc_x100k)))

cent <- st_point_on_surface(radios_fv2)
xy   <- st_coordinates(cent)
df   <- cbind(st_drop_geometry(radios_fv2), x = xy[,1], y = xy[,2])

fit_gam_1 <- gam(inc_x100k ~ s(x, y, k = 150), data = df)
radios_fv2$pred_gam_IAM <- predict(fit_gam_1, type = "response")

####

radios_fv2_2 <- radios_ml
radios_fv2_2 <- radios_fv2_2 %>% 
  filter(!(is.na(mean_tiempo)))

cent <- st_point_on_surface(radios_fv2_2)
xy   <- st_coordinates(cent)
df   <- cbind(st_drop_geometry(radios_fv2_2), x = xy[,1], y = xy[,2])

fit_gam_2 <- gam(mean_tiempo ~ s(x, y, k = 150), data = df)
radios_fv2_2$pred_gam_demora <- predict(fit_gam_2, type = "response")

radios_fv2 <- st_drop_geometry(radios_fv2) %>% 
  left_join(st_drop_geometry(radios_fv2_2[, c(1, 21)]), by = "fid")
#######################################################################################
