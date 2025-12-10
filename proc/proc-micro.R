rm(list=ls())

## Librerías ----

library(pacman)
p_load(tidyverse,
       haven,
       sjlabelled,
       srvyr,
       survey,
       forcats,
       car,
       janitor,
       scales)



## Funciones ----

# Selecciona por nombres con tolerancia (no falla si faltan)
sel_vars <- function(df, vars) df %>% dplyr::select(any_of(vars))

# Función para estandarizar
standarize_data <- function(df, exclude_cols = NULL) {
  cols <- setdiff(names(df)[sapply(df, is.numeric)], exclude_cols)
  for(col in cols) {
    old_min <- min(df[[col]], na.rm = TRUE)
    old_max <- max(df[[col]], na.rm = TRUE)
    new_min <- 0
    new_max <- 10
    df[[col]] <- ifelse(!is.na(df[[col]]), ((df[[col]] - old_min) / (old_max - old_min)) * 
                          (new_max - new_min) + new_min, NA)
  }
  return(df)
}
# Promedio condicionado a completitud mínima (n_req no-NA)
row_mean_if_complete <- function(df, cols, n_req) {
  m <- df %>% dplyr::select(all_of(cols))
  ok <- rowSums(!is.na(m)) >= n_req
  out <- rowMeans(m, na.rm = TRUE)
  out[!ok] <- NA_real_
  out
}


## Procesar bases ----

## Variables
vars_hor <- c("it1", "aoj11", "vic1ext", "vic1" )
vars_ver <- c("b13", "b21", "b31", "ing4", "pn4")
control  <- c("q1", "q1tb", "q1tc_r",
              "q2", "q2y",
              "q3", "q3c", "q3cn",
              "l1", "l1b", "l1n",
              "ed", "edr", "edre",
              "q10", "q10new_12", "q10new_14", "q10new_16", "q10new_18", "q10inc", "q10new")
vars <- c("wave", "year", "pais", "wt", vars_hor, vars_ver, control)

## Carga de archivos

# 1) Grand Merge 
# load(file = "input/orig/GrandMerge.RData") 
# 
# datos <- datos |> mutate(pais_year = paste0(pais,year))
# subset <- datos |> select(pais, year, starts_with("q10")) |> filter(year==2010)

load(file="input/orig/lapop3.RData")
lapop <- lapop |> mutate(pais_year = paste0(pais,year))

# # 2) LAPOP 2004–2008
load(file = "input/orig/LAPOP_2004-2008.RData") # <- 'datos0418'

datos0418 <- datos0418 |> mutate(pais_year = paste0(pais,year))



# 3) Faltantes
dta <- list.files(path = "input/orig/lapop-faltantes/", pattern = "\\.dta$", full.names = TRUE)
merge_faltante <- lapply(dta, function(archivo) {
  message(archivo)
  df <- read_dta(archivo) %>% sel_vars(vars)
  # wave desde el nombre del archivo (mantengo tu lógica de substr)
  wv <- suppressWarnings(as.numeric(substr(basename(archivo),
                                           nchar(basename(archivo)) - 7,
                                           nchar(basename(archivo)) - 4)))
  df %>% mutate(wave = wv, year = wave)
}) %>% bind_rows()
rm(dta)

# 4) 2021 
dta <- list.files(path = "input/orig/LAPOP2021/", pattern = "\\.dta$", full.names = TRUE)
data2021 <- lapply(dta, function(archivo) {
  message(archivo)
  read_dta(archivo) %>% sel_vars(vars)
}) %>% bind_rows()
rm(dta)
data2021 <- data2021 %>% mutate(year = wave)

# 5) gm 23 + faltantes 2023
# gm23 <- read_dta("input/orig/gm23.dta")
# save(gm23, file="input/proc/gm23.rdata")

load("input/proc/gm23.rdata")

data2023 <- gm23 %>%
  filter(year == 2023) %>%
  sel_vars(vars) %>%
  mutate(year = wave)

dta_2023 <- list.files(path = "input/orig/LAPOP2023/", pattern = "\\.dta$", full.names = TRUE)
data2023_f <- lapply(dta_2023, function(archivo) {
  message(archivo)
  read_dta(archivo) %>% sel_vars(vars)
}) %>% bind_rows()
rm(dta_2023)
data2023_f <- data2023_f %>% mutate(year = wave)


# Procesa algunos datos faltantes

datos_subset  <- lapop |>
  anti_join(datos0418,  by="pais_year")

datos0418 <- sel_vars(datos0418, vars)

# Recodificar años
datos0418$wave <- NA_real_
datosselc <- datos0418 %>%
  sel_vars(vars) %>%
  remove_all_labels() %>%
  mutate(
    wave = case_when(
      year %in% c(2006, 2007) ~ 2006,
      year %in% c(2008, 2009) ~ 2008,
      year %in% c(2016,2017) ~ 2016,
      year %in% c(2018, 2019) ~ 2018,
      year %in% c(2004, 2010, 2012, 2014) ~ as.numeric(year),
      TRUE ~ NA_real_
    )
  )

datos_subset$wave <- NA_real_
datos_subset <- datos_subset %>%
  sel_vars(vars) %>%
  remove_all_labels() %>%
  mutate(
    wave = case_when(
      year %in% c(2006, 2007) ~ 2006,
      year %in% c(2008, 2009) ~ 2008,
      year %in% c(2016,2017) ~ 2016,
      year %in% c(2018, 2019) ~ 2018,
      year %in% c(2004, 2010, 2012, 2014) ~ as.numeric(year),
      TRUE ~ NA_real_
    )
  )

# Merge
datos <- bind_rows(
  datosselc,
  datos_subset,
  merge_faltante,
  data2021,
  data2023,
  data2023_f
)

# Copiar etiquetas
datos <- copy_labels(datos, datos0418)
datos_label <- to_label(datos)
datos$pais  <- datos_label$pais

## Limpieza y estandarización ----

# Limpieza

keep <- c("datos", "vars", "vars_hor", "vars_ver", "control",
  "sel_vars", "standarize_data", "row_mean_if_complete")

rm(list = setdiff(ls(), keep))

## recodificar vic1

datos <- datos |> 
  mutate(vic1ext = coalesce(vic1, vic1ext))

# Invierte dirección si es necesario
datos <- datos %>%
  mutate(
    it1  = 5 - it1,
    aoj11= 5 - aoj11,
    pn4  = 5 - pn4
  )

# Aplica estandarización

exclude_cols <- c("wave", "pais", "wt", control, "year")

datos <- standarize_data(datos, exclude_cols) %>% select(pais, wave, 
                                                         everything(),
                                                         -year)

## Índices ----
# Subdimensiones (exigen completitud mínima: 1 ítem para it1; 2 para las de 2 ítems; 2 para democracia_ind)
datos <- datos %>%
  mutate(
    confianza_it_ind     = row_mean_if_complete(cur_data(), c("it1"), 1L),
    seguridad_ind        = row_mean_if_complete(cur_data(), c("aoj11","vic1ext"), 2L),
    confianza_inst_ind   = row_mean_if_complete(cur_data(), c("b13","b21","b31"), 2L),  # exige ≥2 de 3
    democracia_ind       = row_mean_if_complete(cur_data(), c("ing4","pn4"), 2L)        # exige ambos
  )

# Dimensiones: requieren todas sus subpartes presentes
datos <- datos %>%
  mutate(
    cohesion_horizontal_ind = if_else(
      !if_any(c(confianza_it_ind, seguridad_ind), is.na),
      (confianza_it_ind + seguridad_ind)/2, NA_real_
    ),
    cohesion_vertical_ind = if_else(
      !if_any(c(confianza_inst_ind, democracia_ind), is.na),
      (confianza_inst_ind + democracia_ind)/2, NA_real_
    )
  )

# Cohesión general: requiere ambas dimensiones presentes
datos <- datos %>%
  mutate(
    cohesion_general_ind = if_else(
      !if_any(c(cohesion_horizontal_ind, cohesion_vertical_ind), is.na),
      (cohesion_horizontal_ind + cohesion_vertical_ind)/2, NA_real_
    )
  )


# Solo para chequear
subset <- datos |> select(pais, wave, it1, confianza_it_ind,
                         aoj11, vic1ext, seguridad_ind,
                         cohesion_horizontal_ind,
                         b13, b21, b31, confianza_inst_ind,
                         ing4, pn4, democracia_ind,
                         cohesion_vertical_ind,
                         cohesion_general_ind)

tabla <- subset |>
  count(
    pais = haven::as_factor(pais),
    ola  = haven::as_factor(wave)
  ) |>
  pivot_wider(
    names_from  = ola,
    values_from = n,
    values_fill = 0,
    names_sort = TRUE
  ) |>
  adorn_totals("row") |>  # Agrega la fila "Total" al final
  adorn_totals("col")     # Agrega la columna "Total" a la derecha

tabla

## Recodifica variables micro ----

## Recodifica variables

datos <- datos |>
  mutate(
    sexo = coalesce(q1, q1tb, q1tc_r),
    sexo = ifelse(is.na(sexo), NA, ifelse(sexo == 1, 1, 0)),
    sexo = factor(sexo, levels = c(0,1), labels = c("Female", "Male")),
    
    edad = ifelse(is.na(q2), wave - q2y, q2),
    
    pos_politica = coalesce(l1, l1b, l1n),
    pos_politica = as.numeric(pos_politica),
    pos_politica = car::recode(pos_politica, "1:4=1; 5:6=2; 7:10=3"),
    pos_politica = ifelse(is.na(pos_politica), 99, pos_politica),
    pos_politica = factor(pos_politica,
                          levels = c(1,2,3,99),
                          labels = c("Left","Center","Right","Not declared")),
    
    ed_r = case_when(
      ed <= 6            ~ 1,
      ed > 6  & ed <=12  ~ 2,
      ed > 12            ~ 3,
      TRUE               ~ NA_real_
    ),
    
    edre_r = case_when(
      edre <= 2           ~ 1,
      edre > 2 & edre <=4 ~ 2,
      edre > 4            ~ 3,
      TRUE                ~ NA_real_
    ),
    
    edr_r = ifelse(edr == 0, 1, edr),
    
    nivel_educ = coalesce(ed_r, edre_r, edr_r),
    nivel_educ = factor(nivel_educ,
                        levels = c(1,2,3),
                        labels = c("Primary","Secondary","Tertiary"))
  )

# Variable ingresos

### Recodificar q10inc sin los codigos por pais

datos <- datos |>
  mutate(q10inc_r = q10inc %% 100)

## incorporar q10new 16 y 18

datos <- datos |> 
  mutate(
    q10new_16 = if_else(wave == 2016, coalesce(q10new_16, q10new), q10new_16),
    q10new_18 = if_else(wave == 2018, coalesce(q10new_18, q10new), q10new_18),
    q10new_12 = if_else(wave == 2012, coalesce(q10new_12, q10new), q10new_12),
    q10new_14 = if_else(wave == 2014, coalesce(q10new_14, q10new), q10new_14),
  )

  
  
datos <- datos |>
  mutate(income_ori = case_when(wave <= 2010 ~ as.numeric(q10),
                                wave == 2012 ~ as.numeric(q10new_12),
                                wave == 2014 ~ as.numeric(q10new_14),
                                wave == 2016 ~ as.numeric(q10new_16),
                                wave == 2018 ~ as.numeric(q10new_18),
                                wave == 2023 ~ as.numeric(q10inc_r)),
         income_ori = ifelse(income_ori==0, NA, income_ori)) |>
  group_by(pais, wave) |>
  mutate(decile=ntile(income_ori, 10),
         income = ifelse(wave>=2012, decile, income_ori),
         income_decile=income)

nas <- datos |>
  # filter(wave %in% c(2016, 2018)) |>
  filter(wave !=2021) |>
  group_by(pais, wave) |>
  summarise(
    n_total = n(),
    n_na = sum(is.na(income_decile)),
    n_non_na = sum(!is.na(income_decile)),
    pct_na = mean(is.na(income_decile)) * 100
  )

## Limpiar el entorno, seleccionar variables y cargar datos macro ----

rm(list = setdiff(ls(),c("datos", "vars_hor", "vars_ver")))
load("input/proc/datos-completos.rdata")

datos <- datos |>
  select(pais, ola=wave, wt,
         sexo, edad, pos_politica, nivel_educ, income_decile, 
         all_of(vars_hor),
         all_of(vars_ver),
         ends_with("ind"))

save(datos, file = "input/proc/datos-micro.rdata")

datos_wide <- datos_wide |> mutate(ola= ifelse(ola==2022, 2023, ola))

datos_merge <- left_join(datos, datos_wide,
                         by = c("pais", "ola")) |>
  clean_names()


macro <- datos_merge %>%
  mutate(
    edu_terciaria = if_else(nivel_educ == "Tertiary", 1, 0)
  ) %>%
  as_survey_design(
    ids = 1,         # <- sin fórmula
    weights =wt,    # <- sin fórmula
    strata = NULL,
    nest = TRUE
  ) %>%
  group_by(pais, ola) %>%
  summarise(
    edu_terciaria = 100 * survey_mean(edu_terciaria, na.rm = TRUE)) |>
  select(pais, ola, edu_terciaria) |>
  mutate(edu_terciaria = ifelse(edu_terciaria==0, NA, edu_terciaria))


datos_merge <- left_join(datos_merge, macro,
                         by = c("pais", "ola"))

save(datos_merge, file = "input/proc/micro-macro-merge.rdata")


# Filtrar paises y casos perdidos ----

load("input/proc/datos-completos.rdata")

resumen <- datos_wide |>
  dplyr::filter(!is.na("Cohesión general"), !is.na(ola)) |>
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


datos_merge <- datos_merge |>
  dplyr::filter(!is.na(cohesion_general_ind)) |>
  semi_join(resumen, by="pais") |>
  filter(ola!=2021)

save(datos_merge, file = "input/proc/micro-macro-merge.rdata")
