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
vars_hor <- c("it1", "aoj11", "vic1ext")
vars_ver <- c("b13", "b21", "b31", "ing4", "pn4")
control  <- c("q1", "q1tb", "q1tc_r",
              "q2", "q2y",
              "q3", "q3c", "q3cn",
              "l1", "l1b", "l1n",
              "ed", "edr", "edre")
vars <- c("wave", "year", "pais", "wt", vars_hor, vars_ver, control)

## Carga de archivos

# 1) Grand Merge 
load(file = "input/orig/GrandMerge.RData")  

# 2) LAPOP 2004–2008
load(file = "input/orig/LAPOP_2004-2008.RData") # <- 'datos0418'

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
gm23 <- read_dta("input/orig/gm23.dta")
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
datos0418 <- sel_vars(datos0418, vars)
datos1618  <- datos0418 %>% filter(wave %in% c(2016, 2018)) %>% mutate(wave = as.numeric(wave))

# Recodificar años
datos$wave <- NA_real_
datosselc <- datos %>%
  sel_vars(vars) %>%
  remove_all_labels() %>%
  mutate(
    wave = case_when(
      year %in% c(2006, 2007) ~ 2006,
      year %in% c(2008, 2009) ~ 2008,
      year %in% c(2004, 2010, 2012, 2014) ~ as.numeric(year),
      TRUE ~ NA_real_
    )
  )

# Merge
datos <- bind_rows(
  datosselc,
  datos1618,
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

subset <- datos |> select(it1, confianza_it_ind,
                         aoj11, vic1ext, seguridad_ind,
                         cohesion_horizontal_ind,
                         b13, b21, b31, confianza_inst_ind,
                         ing4, pn4, democracia_ind,
                         cohesion_vertical_ind,
                         cohesion_general_ind)


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


## Limpiar el entorno, seleccionar variables y cargar datos macro ----

rm(list = setdiff(ls(),c("datos", "vars_hor", "vars_ver")))
load("input/proc/datos-completos.rdata")

datos <- datos |>
  select(pais, ola=wave, wt,
         sexo, edad, pos_politica, nivel_educ,
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
  dplyr::filter(!is.na(cohesion_general_ind), !is.na(ola)) |>
  semi_join(resumen, by="pais") |>
  filter(ola!=2021)

save(datos_merge, file = "input/proc/micro-macro-merge.rdata")
