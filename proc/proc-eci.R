rm(list=ls())

# Librerías

library(pacman)
p_load(tidyverse,
       janitor,
       ggbreak,
       patchwork,
       ggimage)


# Datos

load(file = "data/base_shiny_completa.rdata")

df_bivariado <- df %>% filter(País!="Bahamas" & País !="Grenada" & País != "Suriname",
                              !is.nan(Valor))

df_bivariado <- clean_names(df_bivariado)

datos_wide <- df_bivariado %>%
  filter(variable=="Cohesión vertical" | variable== "Cohesión horizontal" | variable=="Cohesión general" |
           variable=="PIB per cápita (nominal)" | variable=="Gini index") |>
  pivot_wider(
    names_from = variable,
    values_from = valor
  ) |> clean_names()

# Calculo promedio

df_prom_la <- datos_wide |>
  group_by(ola) |>
  summarise(
    cohesion_horizontal = mean(cohesion_horizontal, na.rm=T),
    cohesion_vertical = mean(cohesion_vertical, na.rm=T),
    cohesion_general = mean(cohesion_general, na.rm=T),
    .groups = "drop"
  ) |>  
  pivot_longer(
    cols = c(cohesion_horizontal, cohesion_vertical, cohesion_general),
    names_to = "variable",
    values_to = "valor"
  ) |> filter(ola>2004)

# 3. Gráfico de líneas
promedio <- ggplot(df_prom_la, aes(x = ola, y = valor, color = variable, group= variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    x = "Ola",
    y = "Promedio",
    color = "Variable",
    title = "América Latina")   +
  scale_color_discrete(
    labels = c(
      "cohesion_horizontal" = "Cohesión Horizontal",
      "cohesion_vertical"   = "Cohesión Vertical",
      "cohesion_general"    = "Cohesión General")
  ) +
  scale_y_continuous(limits = c(3,7.5)) + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 12),
      legend.title = element_blank(),
      legend.position = "bottom")

promedio


## Gráfico Chile

df_prom_cl <- datos_wide |>
  group_by(ola, pais) |>
  summarise(
    cohesion_horizontal = mean(cohesion_horizontal, na.rm=T),
    cohesion_vertical = mean(cohesion_vertical, na.rm=T),
    cohesion_general = mean(cohesion_general, na.rm=T),
    .groups = "drop"
  ) |>  
  pivot_longer(
    cols = c(cohesion_horizontal, cohesion_vertical, cohesion_general),
    names_to = "variable",
    values_to = "valor"
  ) |> filter(pais=="Chile" & ola>2004)

# 3. Gráfico de líneas
cl <- ggplot(df_prom_cl, aes(x = ola, y = valor, color = variable, group= variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    x = "Ola",
    y = NULL,
    color = "Variable",
    title= "Chile")   +
  scale_color_discrete(
    labels = c(
      "cohesion_horizontal" = "Cohesión Horizontal",
      "cohesion_vertical"   = "Cohesión Vertical",
      "cohesion_general"    = "Cohesión General")
  ) +
  scale_y_continuous(limits = c(3,7.5)) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_blank())

cl

promedio + cl + theme(legend.position = "none") +
  patchwork::plot_layout(guides = "collect", axes= "collect") & 
  theme(legend.position = "bottom")


## Gini

# 1) Promedio general por ola (todos los países)
gini_prom_ola <- datos_wide %>%
  group_by(ola) %>%
  summarise(gini_prom = mean(gini_index, na.rm = TRUE), .groups = "drop")

# 2) Serie de Chile por ola (por si hay múltiples filas por ola, se promedia)
gini_chile_ola <- datos_wide %>%
  filter(str_to_lower(pais) == "chile") %>%
  group_by(ola) %>%
  summarise(gini_chile = mean(gini_index, na.rm = TRUE), .groups = "drop")

# 3) Unir y pasar a formato largo para graficar
df_plot <- gini_prom_ola %>%
  full_join(gini_chile_ola, by = "ola") %>%
  pivot_longer(
    cols = c(gini_prom, gini_chile),
    names_to = "serie",
    values_to = "gini"
  ) %>%
  mutate(
    serie = recode(serie,
                   gini_prom  = "Promedio países",
                   gini_chile = "Chile")
  )

# 4) Gráfico
ggplot(df_plot, aes(x = ola, y = gini, color = serie, linetype = serie)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Índice Gini por ola: Chile vs promedio de países",
    x = "Ola",
    y = "Índice Gini",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")


# Gráfico de banderas

# Función para obtener URL de la bandera
get_flag_url <- function(country_name) {
  country_codes <- list(
    Argentina = "ar", Belize = "bz", Bolivia = "bo", Brazil = "br",
    Canada = "ca", Chile = "cl", Colombia = "co", `Costa Rica` = "cr",
    `Dominican Republic` = "do", Ecuador = "ec", `El Salvador` = "sv",
    Guatemala = "gt", Guyana = "gy", Haiti = "ht", Honduras = "hn",
    Jamaica = "jm", Mexico = "mx", Nicaragua = "ni", Panama = "pa",
    Paraguay = "py", Peru = "pe", `Trinidad & Tobago` = "tt",
    `United States` = "us", Uruguay = "uy", Venezuela = "ve"
  )
  code <- country_codes[[country_name]]
  if (is.null(code)) return(NA)
  paste0("https://flagcdn.com/w40/", code, ".png")
}

# Filtrar datos y preparar para graficar
df_2022 <- datos_wide %>%
  filter(ola == 2022) %>%
  mutate(flag_url = sapply(pais, get_flag_url))

# # Calcular promedios
# mean_h <- mean(df_2022$cohesion_horizontal, na.rm = TRUE)
# mean_v <- mean(df_2022$cohesion_vertical, na.rm = TRUE)

cor.test(df_2022$gini_index, df_2022$cohesion_vertical) # -0.35
cor.test(df_2022$gini_index, df_2022$cohesion_general) #-0.22




# Graficar
dat_text_vert <- data.frame(
  label = c("r = -0.35"),
  gini_index = c(52.5),
  cohesion_vertical = c(5.5)) 

gini_vertical <- ggplot(df_2022, aes(x = gini_index, y = cohesion_vertical)) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  geom_image(aes(image = flag_url), size = 0.07, asp = 1.5) +
  scale_y_continuous(n.breaks = 6) + 
  scale_x_continuous(breaks = seq(35,55,5)) +
  labs(
    x = "Índice Gini",
    y = "Cohesión vertical"
  ) +
  geom_text(data = dat_text_vert,
            mapping = aes(x = gini_index, y = cohesion_vertical, label = label),
            size = rel(5)) +
  theme_minimal()

gini_vertical

# general

dat_text_geral <- data.frame(
  label = c("r = -0.22"),
  gini_index = c(52.5),
  cohesion_general = c(6.25)) 

gini_general <- ggplot(df_2022, aes(x = gini_index, y = cohesion_general)) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  geom_image(aes(image = flag_url), size = 0.07, asp = 1.5) +
  scale_y_continuous(n.breaks = 6) + 
  scale_x_continuous(breaks = seq(35,55,5)) +
  labs(
    x = "Índice Gini",
    y = "Cohesión Social"
  ) +
  geom_text(data = dat_text_geral,
            mapping = aes(x = gini_index, y = cohesion_general, label = label),
            size = rel(5)) +
  theme_minimal()

gini_general

gini_general + gini_vertical + 
  patchwork::plot_layout(guides = "collect", axes= "collect")
