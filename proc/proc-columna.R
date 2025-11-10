rm(list = ls())

library(pacman)
p_load(tidyverse, janitor, ggimage, corrplot)

load("input/proc/datos-completos.rdata")

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
df_2023 <- datos_wide %>%
  filter(ola == 2022) %>%
  mutate(flag_url = sapply(pais, get_flag_url)) |> clean_names()

# # Calcular promedios
# mean_h <- mean(df_2022$cohesion_horizontal, na.rm = TRUE)
# mean_v <- mean(df_2022$cohesion_vertical, na.rm = TRUE)

# PIB PPA ----

graf1 <- ggplot(df_2023, aes(x = confianza_interpersonal, y = log(pib_per_capita_ppa))) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  geom_image(aes(image = flag_url), size = 0.07, asp = 1.5) +
  labs(
    x = "Log PIB per Capita (PPA)",
    y = "Confianza en las Instituciones"
  ) +
  # geom_text(data = dat_text_vert,
  #           mapping = aes(x = gini_index, y = cohesion_vertical, label = label),
  #           size = rel(5)) +
  theme_minimal()

graf1

# GINI ----

graf2 <- ggplot(df_2023, aes(x = gini_index, y = confianza_en_las_instituciones)) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  geom_image(aes(image = flag_url), size = 0.07, asp = 1.5) +
  labs(
    x = "Gini Index",
    y = "Confianza en las Instituciones"
  ) +
  # geom_text(data = dat_text_vert,
  #           mapping = aes(x = gini_index, y = cohesion_vertical, label = label),
  #           size = rel(5)) +
  theme_minimal()

graf2

# Migraion ----

graf3 <- ggplot(df_2023, aes(x = percent_pob_migrante, y = confianza_en_las_instituciones)) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  geom_image(aes(image = flag_url), size = 0.07, asp = 1.5) +
  labs(
    x = "% Pob. Migrante",
    y = "Confianza en las Instituciones"
  ) +
  # geom_text(data = dat_text_vert,
  #           mapping = aes(x = gini_index, y = cohesion_vertical, label = label),
  #           size = rel(5)) +
  theme_minimal()

graf3

# Democracia ----

graf4 <- ggplot(df_2023, aes(x = indice_v_dem, y = confianza_en_las_instituciones)) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  geom_image(aes(image = flag_url), size = 0.07, asp = 1.5) +
  labs(
    x = "Indice V-dem",
    y = "Confianza en las Instituciones"
  ) +
  # geom_text(data = dat_text_vert,
  #           mapping = aes(x = gini_index, y = cohesion_vertical, label = label),
  #           size = rel(5)) +
  theme_minimal()

graf4

# Gobernanza ----
graf5 <- ggplot(df_2023, aes(x = wgi, y = confianza_en_las_instituciones)) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  geom_image(aes(image = flag_url), size = 0.07, asp = 1.5) +
  labs(
    x = "Indice WGI",
    y = "Confianza en las Instituciones"
  ) +
  # geom_text(data = dat_text_vert,
  #           mapping = aes(x = gini_index, y = cohesion_vertical, label = label),
  #           size = rel(5)) +
  theme_minimal()

graf5


# Matriz de Correlación----

df <- df_2023 |>
  select(confianza_en_las_instituciones,
         seguridad_publica,
         confianza_interpersonal,
         gini_index,
         log_pib_per_capita_ppa,
         percent_pob_migrante,
         indice_v_dem,
         wgi)


M <- cor(df[sapply(df, is.numeric)], use = "pairwise.complete.obs")

corrplot(M, method = "color", type = "upper",
         addCoef.col = "black", number.cex = .7)
