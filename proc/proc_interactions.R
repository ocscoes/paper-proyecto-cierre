load(here::here("input/proc/datos-fit.rdata"))
library(lme4)
library(texreg)
library(dplyr)
library(correlation)
isced_labels <- c(
  "Early education/none",
  "Primary",
  "Lower secondary",
  "Upper secondary",
  "Postsecondary non-terciary",
  "Terciary (short-cycle)",
  "Universitary"
)

datos_fit$isced11 <- car::recode(datos_fit$escolaridad, 
                                 "0 = 0;1:6 = 1;7:9 = 2;10:12 = 3;13:14 = 4;15:16 = 5;17:18 = 6")

datos_fit$isced11 <- factor(
  datos_fit$isced11,
  levels = 0:6,
  labels = isced_labels
)
datos_fit <- 
  datos_fit %>% 
  select(cohesion_general_ind, cohesion_vertical_ind, cohesion_horizontal_ind,
         nivel_educ, sexo, edad_c, ola_s, pos_politica,ola,
         income_decile,
         pib_we, gini_we, dem_we, wgi_we, mig_we,fho_we,
         pib_be, gini_be, dem_be, wgi_be, mig_be,fho_be, 
         country_wave, pais, wt) %>% na.omit() %>% 
  filter(!pais %in% c("Canada","United States"))
datos_fit$ola <- as.numeric(haven::as_factor(datos_fit$ola))

coef_names <- list(
  ola = "Time",
  gini_be = "Gini (BE)",
  gini_we = "Gini (WE)",
  pib_be = "GDP (BE)",
  pib_we = "GDP (WE)",
  dem_be = "Democracy (BE)",
  dem_we = "Democracy (WE)",
  wgi_be = "Governace (BE)",
  wgi_we = "Governace (WE)",
  mig_be = "Migration (BE)",
  mig_we = "Migration (WE)"
)

hz_1 <- lmer(cohesion_horizontal_ind ~ 1 + income_decile + nivel_educ + sexo + edad_c + I(edad_c^2) + pos_politica + ola +  gini_be*ola+gini_we*ola+pib_be+pib_we+dem_be+dem_we+wgi_be+wgi_we+mig_be+mig_we+(1 | country_wave) + (gini_we+ola| pais), data= datos_fit, weights= wt)
hz_5 <- lmer(cohesion_horizontal_ind ~ 1 + income_decile + nivel_educ + sexo + edad_c + I(edad_c^2) + pos_politica + ola +  gini_be+gini_we+pib_be*ola+pib_we*ola+dem_be+dem_we+wgi_be+wgi_we+mig_be+mig_we+(1 | country_wave) + (pib_we+ola| pais), data= datos_fit, weights= wt)
hz_2 <- lmer(cohesion_horizontal_ind ~ 1 + income_decile + nivel_educ + sexo + edad_c + I(edad_c^2) + pos_politica + ola +  gini_be+gini_we+pib_be+pib_we+dem_be*ola+dem_we*ola+wgi_be+wgi_we +mig_be+mig_we+(1 | country_wave) + (dem_we+ola| pais), data= datos_fit, weights= wt)
hz_3 <- lmer(cohesion_horizontal_ind ~ 1 + income_decile + nivel_educ + sexo + edad_c + I(edad_c^2) + pos_politica + ola +  gini_be+gini_we+pib_be+pib_we+dem_be+dem_we+wgi_be*ola+wgi_we*ola+mig_be+mig_we+(1 | country_wave) + (wgi_we+ola| pais), data= datos_fit, weights= wt)
hz_4 <- lmer(cohesion_horizontal_ind ~ 1 + income_decile + nivel_educ + sexo + edad_c + I(edad_c^2) + pos_politica + ola +  gini_be+gini_we+pib_be+pib_we+dem_be+dem_we+wgi_be+wgi_we+mig_be*ola+mig_we*ola+(1 | country_wave) + (mig_we+ola| pais), data= datos_fit, weights= wt)

hz_ola <- list(hz_1,hz_5,hz_2,hz_3,hz_4)
save(hz_ola,file = "hz_ola.RData")

screenreg(hz_ola,digits=3)
htmlreg(hz_ola,digits=3)
vt_1 <- update(hz_1,cohesion_vertical_ind ~ . )
vt_5 <- update(hz_5,cohesion_vertical_ind ~ . )
vt_2 <- update(hz_2,cohesion_vertical_ind ~ . )  
vt_3 <- update(hz_3,cohesion_vertical_ind ~ . ) 
vt_4 <- update(hz_4,cohesion_vertical_ind ~ . )
vt_ola <- list(vt_1,vt_5,vt_2,vt_3,vt_4)
save(vt_ola,file = "vt_ola.RData")

screenreg(vt_ola,digits=3)


