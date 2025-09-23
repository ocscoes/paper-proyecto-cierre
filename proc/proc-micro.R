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
       janitor)
       


## Funciones ----

# 1. Función de estandarización de las escalas.
## Descripción:
## Esta función reescala las variables numéricas a un rango 0-10.
## Excluye automáticamente variables que no sean de caracter numérico.
# Argumentos:
## df: Espera un dataframe.
## exclude_cols: Se espera un vector con el nombre (character) de las variables que no se procesarán.

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

# Función para calcular el promedio


mean_na <- function(...) {
  mean_result <- mean(c(...), na.rm = TRUE)
  if (is.nan(mean_result)) {
    mean_result <- NA
  }
  return(mean_result)
}


## Procesar bases ----

vars_hor <- c("it1", "aoj11", "vic1ext")
vars_ver <- c("b13", "b21", "b31",
              "ing4", "pn4")
control <- c("q1", "q1tb", "q1tc_r",
             "q2", "q2y",
             "q3", "q3c", "q3cn",
             "l1", "l1b", "l1n",
             "ed", "edr", "edre")
vars <- c("wave","year","pais","wt",vars_hor,vars_ver, control)

# A continuación se realiza la carga de archivos LAPOP.
load(file = "input/orig/GrandMerge.RData") 
load(file="input/orig/LAPOP_2004-2008.RData")# datos0418
dta <- list.files(path = "input/orig/lapop-faltantes/", pattern = ".dta")
dta <- paste0(file = "input/orig/lapop-faltantes/",dta)
merge_faltante <- lapply(dta, function(archivo){
  print(archivo)
  df <- read_dta(archivo)
  df <- df %>% select_if(names(df) %in% vars) %>% mutate(
    wave = substr(archivo,nchar(archivo)-7,nchar(archivo)-4)
  ) 
  return(df)
})
rm(dta)
merge_faltante <- bind_rows(merge_faltante)

dta<- list.files(path = "input/orig/LAPOP2021/", pattern = ".dta")
dta <- paste0(file = "input/orig/LAPOP2021/",dta)

gm23 <- read_dta("input/orig/gm23.dta")
data2023 <- gm23 |> filter(year==2023)
data2023 <- data2023 %>% select_if(names(data2023) %in% vars)

dta_2023<- list.files(path = "input/orig/LAPOP2023/", pattern = ".dta")
dta_2023 <- paste0(file = "input/orig/LAPOP2023/", dta_2023)

data2021 <- lapply(dta, function(archivo){
  print(archivo)
  df <- read_dta(archivo)
  df <- df %>% select_if(names(df) %in% vars)
  return(df)
})
data2021 <- bind_rows(data2021) ; rm(dta)

data2023_f <- lapply(dta_2023, function(archivo){
  print(archivo)
  df <- read_dta(archivo)
  df <- df %>% select_if(names(df) %in% vars)
  return(df)
})
data2023_f <- bind_rows(data2023_f) ; rm(dta_2023)

datos0418 <- datos0418 %>% select_if(names(datos0418) %in% vars)
datos1618 <-  datos0418  %>% filter(wave==2016 | wave==2018)
datos1618$wave <- as.numeric(datos1618$wave)
datos$wave <- NA
datosselc <- datos %>% select_if(names(datos) %in% vars)

# Los casos se recodifican de tal modo wave (olas): 
# 2004(2004), 2006(2006 y 2007),  2008(2008 y 2009), 2010(2010), 2012(2012), 2014(2014), 2016(16,17), 2018(18,19)
datosselc$wave <- ifelse(datosselc$year==2006 | datosselc$year==2007, 2006, NA)
datosselc$wave <- ifelse(datosselc$year==2004 , 2004, datosselc$wave)
datosselc$wave <- ifelse(datosselc$year==2008 | datosselc$year==2009, 2008, datosselc$wave)
datosselc$wave <- ifelse(datosselc$year==2010, 2010, datosselc$wave)
datosselc$wave <- ifelse(datosselc$year==2012, 2012, datosselc$wave)
datosselc$wave <- ifelse(datosselc$year==2014, 2014, datosselc$wave)

datosselc <- remove_all_labels(datosselc) # Las etiquetas no son compatibles
merge_faltante <- mutate_all(merge_faltante, as.numeric)
merge_faltante$year <- merge_faltante$wave

data2021 <- mutate_all(data2021, as.numeric)
data2021$year <- data2021$wave

data2023 <- data2023 %>% 
  mutate_all(as.numeric)
data2023$year <- data2023$wave

# Merge 2004-2014 + 2018 + faltantes varias del grand merge + 2021
datos <- bind_rows(datosselc,datos1618,merge_faltante,data2021, data2023, data2023_f) # logra una base longitudinal con la pr4 incluida. 
datos <- copy_labels(datos, datos0418)
# rm(list = setdiff(ls(),c("datos","vars","vars_hor","vars_ver","mean_result")))
datos_label <- to_label(datos)
datos$pais<-datos_label$pais

# Se invierte sentido de respuestas para ajustar escalas (valores mayores ~ mayor cohesión).
datos <- datos %>%
  mutate(it1 = 5 - it1,
         aoj11 = 5 - aoj11,
         pn4 = 5 -pn4
  )

# Especifica las columnas para excluir
exclude_cols = c("wave", "pais","wt", control)

# Estandariza los datos
datos <- standarize_data(datos, exclude_cols) %>% select(pais, wave, 
                                                        everything(),
                                                        -year)


# religion

subset <- datos |>
  filter(wave<2010)

## Calcular indices ----

## Subdimensiones

datos <- datos %>%
  mutate(
    confianza_it_ind    = it1,
    seguridad_ind        = rowMeans(across(c(aoj11, vic1ext)), na.rm = TRUE),
    confianza_inst_ind   = rowMeans(across(c(b13, b21, b31)), na.rm = TRUE),
    democracia_ind       = rowMeans(across(c(ing4, pn4)), na.rm =TRUE)
  )

calidad_year <- datos %>%
  as.data.frame() %>%
  select(-pais, -wave) %>%                                # sin c()
  mutate(across(everything(), ~ !is.na(.))) %>%           # TRUE si no es NA
  mutate(
    confianza_it_ind   = ifelse(rowSums(cbind(it1)) > 0, FALSE, TRUE),
    seguridad_ind      = ifelse(rowSums(cbind(aoj11, vic1ext)) > 1, FALSE, TRUE),
    confianza_inst_ind = ifelse(rowSums(cbind(b13, b21, b31)) > 1, FALSE, TRUE),
    democracia_ind     = ifelse(rowSums(cbind(ing4, pn4)) > 1, FALSE, TRUE)
  ) %>%
  select(all_of(c("confianza_it_ind",
                  "seguridad_ind",
                  "confianza_inst_ind",
                  "democracia_ind"))) %>%                 # ← ahora sí se cierra
  mutate(
    pais = datos$pais,
    wave = datos$wave
  ) %>%
  select(pais, wave, everything())



for(i in 1:nrow(datos)){
  # print(i)
  for(x in names(calidad_year)[3:6]){
    if(calidad_year[i,x]){
      datos[i,x] <- NA
    }
  }
}

## Dimensiones

datos <- datos %>%
  mutate(
    cohesion_horizontal_ind = rowMeans(across(c(confianza_it_ind, seguridad_ind)), na.rm = FALSE),
    cohesion_vertical_ind  = rowMeans(across(c(confianza_inst_ind, democracia_ind)), na.rm = FALSE)
  )

calidad_year <- datos %>%
  as.data.frame %>%
  select(-c(pais, wave)) %>%
  mutate(across(everything(), ~ !is.na(.))) %>%
  as.data.frame() %>%
  mutate(
    cohesion_horizontal_ind   = ifelse(rowSums(cbind(confianza_it_ind, seguridad_ind)) > 1, FALSE, TRUE),
    cohesion_vertical_ind = ifelse(rowSums(cbind(confianza_inst_ind, democracia_ind)) > 1, FALSE, TRUE)) %>%
  select(c(cohesion_horizontal_ind,
           cohesion_vertical_ind)) %>%
  mutate(
    pais = datos$pais,
    wave = datos$wave
  ) %>%
  select(pais,wave,everything())


for(i in 1:nrow(datos)){
  # print(i)
  for(x in names(calidad_year)[3:4]){
    if(calidad_year[i,x]){
      datos[i,x] <- NA
    }
  }
}

# Cohesión General

datos <- datos %>%
  mutate(
    cohesion_general_ind = rowMeans(across(c(cohesion_horizontal_ind, cohesion_vertical_ind)), na.rm = FALSE)
  )


calidad_year <- datos %>%
  as.data.frame %>%
  select(-c(pais, wave)) %>%
  mutate(across(everything(), ~ !is.na(.))) %>%
  as.data.frame() %>%
  mutate(
    cohesion_general_ind = ifelse(rowSums(cbind(cohesion_horizontal_ind, cohesion_vertical_ind)) > 1, FALSE, TRUE)
  ) %>%
  select(c(cohesion_general_ind)) %>%
  mutate(
    pais = datos$pais,
    wave = datos$wave
  ) %>%
  select(pais,wave,everything())

columnas_objetivo <- c("cohesion_general_ind")  # Agrega más si lo deseas
for(i in 1:nrow(datos)) {
  if (isTRUE(calidad_year$cohesion_general_ind[i])) {
    datos[i, columnas_objetivo] <- NA
  }
}



## Recodifica variables

datos <- datos |>
  mutate(
    sexo = coalesce(q1, q1tb, q1tc_r),
    sexo = ifelse(is.na(sexo), NA, ifelse(sexo == 1, 1, 0)),
    sexo = factor(sexo, levels = c(0,1), labels = c("Female", "Male")),
    
    edad = ifelse(is.na(q2), wave - q2y, q2),
    
    religion = coalesce(q3c, q3cn, q3),
    religion = case_when(
      religion %in% c(4, 11) ~ "Sin religión",
      religion == 1          ~ "Católica",
      religion == 2          ~ "Protestante",
      religion == 5          ~ "Evangélica",
      is.na(religion)        ~ NA_character_,
      TRUE                   ~ "Otra religión"
    ),
    
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



vars_creadas <- c("sexo","edad","religion","pos_politica","nivel_educ")

resumen_na_wave <- datos %>%
  group_by(wave) %>%
  summarise(
    n = n(),
    across(
      all_of(vars_creadas),
      list(
        n_na   = ~ sum(is.na(.)),
        prop_na = ~ mean(is.na(.))   # equivalente a sum(is.na(.))/n() por grupo
      ),
      .names = "{col}_{fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    -c(wave, n),
    names_to = c("variable", ".value"),
    names_sep = "_(?=(n_na|prop_na)$)"
  ) %>%
  arrange(wave, desc(prop_na))

resumen_na_wave

## Limpiar el entorno, seleccionar variables y cargar datos macro

rm(list = setdiff(ls(),c("datos", "vars_hor", "vars_ver")))
load("input/proc/datos-completos.rdata")

datos <- datos |>
  select(pais, ola=wave, wt,
         sexo, edad, religion, pos_politica, nivel_educ,
         all_of(vars_hor),
         all_of(vars_ver),
         ends_with("ind"))

save(datos, file = "input/proc/datos-micro.rdata")

# datos <- datos |> rename(ola=wave)

datos_wide <- datos_wide |> mutate(ola= ifelse(ola==2022, 2023, ola))

datos_merge <- left_join(datos, datos_wide,
                         by = c("pais", "ola")) |>
  clean_names()


macro <- datos_merge %>%
  mutate(
    pob_catolica  = if_else(religion == "Católica", 1, 0),
    edu_terciaria = if_else(nivel_educ == 3,          1, 0),
    sin_politica = if_else(pos_politica==99, 1, 0)
  ) %>%
  as_survey_design(
    ids = 1,         # <- sin fórmula
    weights =wt,    # <- sin fórmula
    strata = NULL,
    nest = TRUE
  ) %>%
  group_by(pais, ola) %>%
  summarise(
    pob_catolica  = 100 * survey_mean(pob_catolica,  na.rm = TRUE),
    edu_terciaria = 100 * survey_mean(edu_terciaria, na.rm = TRUE),
    sin_politica  = 100 * survey_mean(sin_politica, na.rm=T)) |>
  select(pais, ola, pob_catolica, edu_terciaria, sin_politica) |>
  mutate(pob_catolica = ifelse(pob_catolica==0, NA, pob_catolica),
         edu_terciaria = ifelse(edu_terciaria==0, NA, edu_terciaria),
         sin_politica = ifelse(sin_politica == 0, NA, sin_politica))


datos_merge <- left_join(datos_merge, macro,
                         by = c("pais", "ola")) |>
  mutate(pib_be = mean(log_pib_per_capita_ppa, na.rm=T),
         gini_be = mean(gini_index, na.rm=T),
         mig_be = mean(percent_pob_migrante, na.rm=T),
         dem_be = mean(indice_v_dem, na.rm=T),
         wgi_be = mean(wgi, na.rm=T),
         pib_we = log_pib_per_capita_ppa - pib_be,
         gini_we = gini_index - gini_be,
         mig_we = percent_pob_migrante - mig_be,
         dem_we = indice_v_dem - dem_be,
         wgi_we = wgi- wgi_be)
  


save(datos_merge, file = "input/proc/micro-macro-merge.rdata")


# Filtrar paises

load("input/proc/datos-completos.rdata")
load("input/proc/micro-macro-merge.rdata")

antes <- datos_merge |> group_by(pais) |> summarise(f= n())

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


datos_merge_b <- datos_merge |>
  dplyr::filter(!is.na(cohesion_general_ind), !is.na(ola)) |>
  semi_join(resumen, by="pais") 

despues <- datos_merge_b |> group_by(pais) |> summarise(f= n())

save(datos_merge_b, file = "input/proc/micro-macro-merge.rdata")
