rm(list=ls())

library(tidyverse)
library(janitor)
library(sjlabelled)
library(readr)

load("input/orig/hief.RData")
load("input/proc/micro-macro-merge.rdata")

olas <- unique(as_numeric(datos_merge$ola))



pais = c(
  "Argentina","Bahamas","Belize","Bolivia","Brazil","Canada","Chile","Colombia",
  "Costa Rica","Dominican Republic","Ecuador","El Salvador","Grenada","Guatemala",
  "Guyana","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay",
  "Peru","Suriname","Trinidad & Tobago","United States","Uruguay","Venezuela"
)

# HIEF ----

data <- x |>
  clean_names() |>
  filter(country %in% pais &
           year %in% olas) |>
  distinct(country, year) |>
  mutate(disponible = TRUE)

tab <- data |>
  group_by(country) |>
  summarise(disponible = n()) |>
  mutate(total_olas_paises = sum(disponible))

# Matriz

matriz <- data |>
  pivot_wider(names_from = year, values_from = disponible)

olas_f <- setdiff(as.character(olas), names(matriz))

for (col in olas_f) {
  matriz[[col]] <- FALSE
}

paises_f <- setdiff(pais, matriz$country)
paises_f

# QoG -----

qog <- read_delim("input/orig/qog.csv", delim= ";")

codigo_pais = c(
  "ARG","BHS","BLZ","BOL","BRA","CAN","CHL","COL",
  "CRI","DOM","ECU","SLV","GRD","GTM","GUY","HTI","HND","JAM","MEX","NIC","PAN","PRY","PER","SUR","TTO","USA","URY","VEN")

data_qog <- qog |>
  clean_names() |>
  filter(ccodealp %in% codigo_pais &
           year %in% olas) |>
  mutate(disponible = TRUE)

tab <- data_qog |>
  group_by(cname) |>
  summarise(disponible = n()) |>
  mutate(total_olas_paises = sum(disponible))

matriz <- data_qog |>
  select(year, ccodealp, fe_etfra, disponible) |>
  pivot_wider(names_from = year, values_from = disponible)

paises_f <- setdiff(codigo_pais, matriz$ccodealp)
paises_f
