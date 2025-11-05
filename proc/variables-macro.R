rm(list=ls())

## Cargar librerias ----

library(pacman)
p_load(tidyverse,
       readxl,
       janitor,
       devtools,
       stringr,
       stringi,
       psych)

devtools::install_github("vdeminstitute/vdemdata")


## Cargar datos ----

load("input/orig/base_shiny_completa.RData") 

gdp <- read_xls("input/orig/gdp-ppa.xls", range= "A4:BQ270") |> clean_names()
# educ <- read_xls("input/orig/ed-secundaria.xls", range="A4:BP270") |> clean_names()
migra <- read_xlsx("input/orig/migration.xlsx", sheet = "Table 3")
wgi <- read_xlsx("input/orig/wgi.xlsx")

df <- clean_names(df)


vdem <- vdemdata::vdem

## Estandarizar nombres paises ----

iso_dict <- tibble(
  pais = c(
    "Argentina","Bahamas","Belize","Bolivia","Brazil","Canada","Chile","Colombia",
    "Costa Rica","Dominican Republic","Ecuador","El Salvador","Grenada","Guatemala",
    "Guyana","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay",
    "Peru","Suriname","Trinidad & Tobago","United States","Uruguay","Venezuela"
  ),
  codigo_pais = c(
    "ARG","BHS","BLZ","BOL","BRA","CAN","CHL","COL",
    "CRI","DOM","ECU","SLV","GRD","GTM","GUY","HTI","HND","JAM","MEX","NIC","PAN","PRY","PER","SUR","TTO","USA","URY","VEN")
) |>
  mutate(
    region = case_when(
      codigo_pais %in% c("ARG","BOL","BRA","CHL","COL","ECU","PRY","PER","URY","VEN") ~ "América del Sur",
      codigo_pais %in% c("BLZ","CRI","SLV","GTM","HND","MEX","NIC","PAN")                                   ~ "Centroamérica",
      codigo_pais %in% c("BHS","DOM","GRD", "GUY", "HTI","JAM","SUR","TTO")                                         ~ "El Caribe",
      codigo_pais %in% c("CAN","USA")                                                           ~ "América del Norte",
      TRUE ~ NA_character_
    ),
    region = factor(region, levels = c("América del Norte","Centroamérica","El Caribe","América del Sur"))
  )


## Pivotear base macro ----

datos_wide <- df |>
  pivot_wider(
    id_cols= c(ola, pais),
    names_from = variable,
    values_from = valor)


## Lista de paises ----

codigos_pais <- c(
  "ABW", "ARG", "ATG", "BHS", "BLZ", "BMU", "BOL", "BRA", "BRB", "CAN",
  "CHL", "COL", "CRI", "CUB", "CUW", "CYM", "DMA", "DOM", "ECU", "GRD",
  "GTM", "GUY", "HND", "HTI", "JAM", "KNA", "LCA", "MAF", "MEX", "NIC",
  "PAN", "PER", "PRI", "PRY", "SLV", "SUR", "SXM", "TCA", "TTO", "URY",
  "USA", "VCT", "VEN", "VGB", "VIR"
)


## Base gdp ----

gdp_proc <- gdp |>
  filter(country_code %in% codigos_pais) |>
  select(1:3, 49:67) |>
  rename_with(~ sub("^x(?=(\\d|año))", "", .x, perl = TRUE))

# Detectar columnas de años
gdp_long <- gdp_proc %>%
  pivot_longer(
    # selecciona todas las columnas que "parecen" años
    matches("^(X)?\\s*\\d{4}\\s*$"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  # deja solo los dígitos del nombre de columna (quita X y espacios)
  mutate(year = as.integer(gsub("\\D", "", year))) %>%
  arrange(country_code, year, indicator_name) 


nas <- gdp_long |> filter(is.na(value))

# Imputación
gdp_long <- gdp_long %>%
  group_by(country_name, value) %>%
  arrange(year) %>%
  mutate(
    value = if_else(
      is.na(value),
      case_when(
        !is.na(lag(value)) & !is.na(lead(value)) ~ (lag(value) + lead(value)) / 2,
        !is.na(lag(value)) ~ lag(value),
        !is.na(lead(value)) ~ lead(value),
        TRUE ~ NA_real_
      ),
      value
    )
  ) %>%
  ungroup()

# Filtrar años

olas <- unique(datos_wide$ola)

gdp_long <- gdp_long |>
  filter(year %in% olas) |>
  mutate(log_pib_ppa = log(value)) |>
  select(ola=year,
         codigo_pais=country_code,
         "PIB per cápita (PPA)"= value,
         "(Log) PIB per cápita (PPA)"= log_pib_ppa)

 ## Base educación ----
# 
# educa_proc <- educ |>
#   filter(country_code %in% codigos_pais) |>
#   select(1:3, 49:67) |>
#   rename_with(~ sub("^x(?=(\\d|año))", "", .x, perl = TRUE))
# 
# # Detectar columnas de años
# educ_long <- educa_proc %>%
#   pivot_longer(
#     # selecciona todas las columnas que "parecen" años
#     matches("^(X)?\\s*\\d{4}\\s*$"),
#     names_to  = "year",
#     values_to = "value"
#   ) %>%
#   # deja solo los dígitos del nombre de columna (quita X y espacios)
#   mutate(year = as.integer(gsub("\\D", "", year))) %>%
#   arrange(country_code, year, indicator_name)
# 
# # Imputación con lógica extendida
# educ_long_imp <- educ_long  %>%
#   group_by(country_code) %>%
#   arrange(year) %>%
#   mutate(
#     value = if_else(
#       is.na(value),
#       case_when(
#         !is.na(lag(value)) & !is.na(lead(value)) ~ (lag(value) + lead(value)) / 2,
#         !is.na(lag(value)) ~ lag(value),
#         !is.na(lead(value)) ~ lead(value),
#         TRUE ~ NA_real_
#       ),
#       value  # si ya tiene value, se mantiene
#     )
#   ) %>%
#   ungroup()
# 
# # Filtrar años
# 
# olas <- unique(datos_wide$ola)
# 
# educ_long <- educ_long |>
#   filter(year %in% olas) |>
#   select(ola=year,
#          codigo_pais=country_code,
#          "Pob. con educación secundaria completa"= value)

## Migración  ----

# 1) Tu set objetivo
targets <- c(
  "Argentina","Bahamas","Belize","Bolivia","Brazil","Canada","Chile","Colombia",
  "Costa Rica","Dominican Republic","Ecuador","El Salvador","Grenada","Guatemala",
  "Guyana","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay",
  "Peru","Suriname","Trinidad & Tobago","United States","Uruguay","Venezuela"
)

# 2) Normalizador más robusto (maneja (..), &, THE/bahamas, comas finales, etc.)
normalize_name <- function(x) {
  x |>
    str_replace_all("\\*", "") |>                 # quita asteriscos
    str_replace_all("\\s*\\(.*?\\)", "") |>       # quita paréntesis y su contenido
    str_replace_all("&", " and ") |>              # unifica & -> " and "
    str_replace_all(",\\s*the$", "") |>           # "Bahamas, The" -> "Bahamas"
    str_replace_all("^the\\s+", "") |>            # "The Bahamas" -> "Bahamas"
    str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |> # sin tildes
    str_replace_all("[^a-z ]+", " ") |>           # solo letras/espacios
    str_squish()
}

# 3) Diccionario canónico normalizado
dict <- tibble(
  country_std = targets,
  norm_target = normalize_name(targets)           # p.ej. "trinidad and tobago"
)

# 4) Sinónimos habituales 
synonyms <- tribble(
  ~norm_raw,                               ~norm_target,
  "united states of america",              "united states",
  "bolivia plurinational state of",        "bolivia",
  "venezuela bolivarian republic of",      "venezuela",
  "trinidad tobago",                       "trinidad and tobago",
  "mexico united mexican states",          "mexico",
  "bahamas",                               "bahamas"           # cubre "bahamas the" tras normalizar
)

# Helper para aplicar sinónimos sobre una columna normalizada
apply_synonyms <- function(norm_vec) {
  out <- norm_vec
  # reemplazos exactos
  out <- coalesce(left_join(tibble(norm_raw = out),
                            synonyms, by = "norm_raw")$norm_target, out)
  # reemplazos por patrón/cola (ej. "bolivia ..." -> "bolivia")
  out <- case_when(
    str_detect(out, "^united states( .*)?$") ~ "united states",
    str_detect(out, "^bolivia( .*)?$")       ~ "bolivia",
    str_detect(out, "^venezuela( .*)?$")     ~ "venezuela",
    str_detect(out, "^trinidad( and)? tobago$") ~ "trinidad and tobago",
    TRUE ~ out
  )
  out
}

# migra_std: normaliza y mapea a target
migra_std <- migra %>%
  mutate(
    country_raw = `Region, development group, country or area`,
    norm_raw = normalize_name(country_raw),
    norm_raw = apply_synonyms(norm_raw)
  ) %>%
  left_join(dict, by = c("norm_raw" = "norm_target")) %>%
  mutate(country_std = country_std)

no_match <- migra_std %>%
  filter(is.na(country_std)) %>%
  distinct(country_raw) %>%
  arrange(country_raw)

migra <- migra_std %>% filter(!is.na(country_std))

# 1) Normaliza 'pais' proveniente de migra_logistico
# 2) Mapea a country_std usando dict
# 3) Recién ahí cruza con iso_dict (idealmente también normalizado)

# iso_dict normalizado para cruces robustos
iso_dict_norm <- iso_dict %>%
  mutate(norm_iso = normalize_name(pais)) %>%
  left_join(dict, by = c("norm_iso" = "norm_target")) %>%
  # Si tus 'pais' en iso_dict ya son iguales a country_std, este paso solo refuerza
  mutate(pais_std = coalesce(country_std, pais)) %>%
  select(pais_std, codigo_pais, region)

## Imputacion

# === Configuración ===
extrapolar <- FALSE   # TRUE si quieres extrapolar fuera del primer/último año observado
epsilon    <- 1e-6    # para evitar logit(0) o logit(1)

# 1) Detectar columnas de años y llaves (excluye columna índice '...1')
year_cols <- names(migra)[grepl("^\\d{4}$", names(migra))]
id_cols   <- setdiff(names(migra), c(year_cols, "...1"))

# 2) Pasar a largo y asegurar numérico (porcentaje 0–100)
migra_long <- migra %>%
  select(all_of(c(id_cols, year_cols))) %>%
  pivot_longer(all_of(year_cols), names_to = "anio", values_to = "pct") %>%
  mutate(
    anio = as.integer(anio),
    pct  = suppressWarnings(as.numeric(pct))
  )

# 3) Rango GLOBAL de años (evita errores en grupos con pocos datos)
full_years <- seq(min(migra_long$anio, na.rm = TRUE),
                  max(migra_long$anio, na.rm = TRUE), by = 1)

# 4) Completar años e interpolar en escala logit
migra_logistico <- migra_long %>%
  group_by(across(all_of(id_cols))) %>%
  complete(anio = full_years) %>%
  arrange(anio, .by_group = TRUE) %>%
  mutate(
    observado = !is.na(pct),
    # Proporción en (0,1), recortada para evitar ±Inf en el logit
    p01 = pct / 100,
    p01 = ifelse(is.na(p01), NA_real_, pmin(pmax(p01, epsilon), 1 - epsilon)),
    logit_obs = log(p01 / (1 - p01)),
    
    # Interpolación lineal en la escala logit
    logit_hat = if (sum(observado) >= 2) {
      approx(x = anio[observado],
             y = logit_obs[observado],
             xout = anio,
             method = "linear",
             rule = if (extrapolar) 2 else 1)$y
    } else logit_obs,   # con 0–1 puntos: deja observado y NA en el resto
    
    # Volver a porcentaje 0–100
    pct_logistico = 100 * (1 / (1 + exp(-logit_hat))),
    imputado = is.na(pct) & !is.na(pct_logistico)
  ) %>%
  ungroup()

## Limpieza

migra_log <- migra_logistico %>%
  transmute(
    ola  = anio,
    pais_raw = `Region, development group, country or area`,
    norm_pais = normalize_name(pais_raw),
    norm_pais = apply_synonyms(norm_pais),
    `% pob. migrante` = pct_logistico
  ) %>%
  # mapea a estándar canónico
  left_join(dict, by = c("norm_pais" = "norm_target")) %>%
  rename(pais_std = country_std) %>%
  filter(ola >= 2004, ola <= 2022, !is.na(pais_std)) %>%
  # ahora sí: ISO por país estándar
  left_join(iso_dict_norm, by = "pais_std") %>%
  # deja nombres finales amigables
  rename(pais = pais_std, codigo_pais = codigo_pais) |>
  filter(pais_raw!="United States Virgin Islands*") |>
  select(ola,
         pais,
         codigo_pais,
         region,
         `% pob. migrante`)

# Opcional: ver qué quedó sin ISO (si algo)
# faltantes_iso <- migra_log %>% filter(is.na(codigo_pais)) %>% distinct(pais)

# V-Dem

vdem_proc <- vdem |>
  filter(year>=2004 & year<=2022 & country_text_id %in% codigos_pais) |>
  select(codigo_pais= country_text_id,
         ola=year,
         "Indice V-Dem"= v2x_polyarchy)

# Imputación con lógica extendida
vdem_proc <- vdem_proc %>%
  group_by(codigo_pais, `Indice V-Dem`) %>%
  arrange(ola) %>%
  mutate(
    `Indice V-Dem` = if_else(
      is.na(`Indice V-Dem`),
      case_when(
        !is.na(lag(`Indice V-Dem`)) & !is.na(lead(`Indice V-Dem`)) ~ (lag(`Indice V-Dem`) + lead(`Indice V-Dem`)) / 2,
        !is.na(lag(`Indice V-Dem`)) ~ lag(`Indice V-Dem`),
        !is.na(lead(`Indice V-Dem`)) ~ lead(`Indice V-Dem`),
        TRUE ~ NA_real_
      ),
      `Indice V-Dem`  # si ya tiene `Indice V-Dem`, se mantiene
    )
  ) %>%
  ungroup()


# WGI

wgi_proc <- wgi |>
  filter(year>=2004 & year<=2022 & code %in% codigos_pais) |>
  mutate(estimate= as.numeric(estimate)) |>
  group_by(code, year, indicator) |>
  summarise(valores= mean(estimate), .groups = "drop") |>
  pivot_wider(names_from= indicator, values_from=valores) 

items <- setdiff(names(wgi_proc), c("year","code"))

a_wgi <- alpha(wgi_proc[3:8])

wgi_proc <- wgi_proc |>
  mutate(wgi=rowMeans(pick(all_of(items)))) |>
  select(codigo_pais= code,
         ola= year,
         wgi)


# añadir codigos iso a la base datos principal
datos_wide <- datos_wide |>
  left_join(iso_dict, by="pais")


# merge

# Los objetos que quieres conservar (ajusta los nombres si hace falta)
keep <- c("datos_wide", "gdp_long",  "migra_log", "vdem_proc", "wgi_proc")

# Borra todo lo demás del entorno global
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

datos_wide <- datos_wide |>
  left_join(gdp_long, by=c("codigo_pais", "ola")) |>
  # left_join(educ_long, by=c("codigo_pais", "ola")) |>
  left_join(migra_log, by=c("codigo_pais", "ola")) |>
  left_join(vdem_proc, by=c("codigo_pais", "ola")) |>
  left_join(wgi_proc, by=c("codigo_pais", "ola")) |>
  select(ola,
         pais=pais.x,
         codigo_pais,
         region = region.x,
         everything(),
         -pais.y,
         -region.y
         )

# Limpiar

resumen <- datos_wide |>
  dplyr::filter(!is.na(`Cohesión general`), !is.na(ola)) |>
  dplyr::distinct(pais, ola) |>                      # evita duplicados país-año
  dplyr::group_by(pais) |>
  dplyr::summarise(
    n_olas = n(),
    `olas disponibles` = paste(sort(as.integer(ola)), collapse = "; "),
    .groups = "drop"
  ) |>
  dplyr::filter(n_olas >= 5) |>
  arrange(desc(n_olas), pais) |>
  select(pais, n_olas, "olas disponibles")


datos_wide <- datos_wide |>
  dplyr::filter(!is.na(`Cohesión general`), !is.na(ola)) |>
  semi_join(resumen, by="pais") |>
  select(1:12, 21,24:28)

  

# exportar

save(datos_wide, file="input/proc/datos-completos.rdata")



