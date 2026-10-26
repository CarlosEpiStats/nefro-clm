# Setup ----
library(tidyverse)
library(plotly)
library(here)
library(readxl)

source(here("R", "utils_helpers.R"))
source(here("R", "fct_plots.R"))


read_barometro <- function(range) {
  file <- here("R", "data", "BS_1998-2025_Principales_variables_CCAA.xlsx")
  read_excel(
    file,
    sheet = "P 16",
    range = range,
    col_types = c("text", rep("numeric", 27)),
    na = c("", "-", "..")
  ) |>
    rename(region = "...1") |>
    pivot_longer(cols = -region, names_to = "year", values_to = "value")
}

# Load data ----
datos_lista_espera_mejorado <- read_barometro("A5:AB25") |>
  mutate(var = "Mejorado")
datos_lista_espera_empeorado <- read_barometro("A29:AB49") |>
  mutate(var = "Empeorado")
datos_lista_espera_igual <- read_barometro("A52:AB72") |>
  mutate(var = "Igual")
datos_lista_espera_ns <- read_barometro("A75:AB95") |>
  mutate(var = "No sabe")

datos_barometro <- bind_rows(
  datos_lista_espera_mejorado,
  datos_lista_espera_empeorado,
  datos_lista_espera_igual,
  datos_lista_espera_ns
) |>
  filter(!is.na(value)) |>
  mutate(
    year = as.integer(year),
    tipo_region = case_when(
      region == "Castilla-La Mancha" ~ "Castilla-La Mancha",
      region == "TOTAL" ~ "Total Estado",
      TRUE ~ "Resto de regiones"
    ),
    value_round = format(round(value, 2), nsmall = 2),
    text = str_glue(
      "Región: {region}<br>Año: {year}<br>Respuesta: {var}<br>Porcentaje: {value_round}%"
    )
  )


# Trasplantes ----
## Gráfico de líneas ----
plot_barometro_lines <- function(data) {
  data |>
    plot_nefro_lines(
      y_var = "value",
      y_label = "Porcentaje de encuestados",
      text_var = "text"
    )
}

plot_barometro_mejorado_lineas <- datos_barometro |>
  filter(var == "Mejorado") |>
  plot_barometro_lines()

plot_barometro_empeorado_lineas <- datos_barometro |>
  filter(var == "Empeorado") |>
  plot_barometro_lines()

plot_barometro_igual_lineas <- datos_barometro |>
  filter(var == "Igual") |>
  plot_barometro_lines()

plot_barometro_nose_lineas <- datos_barometro |>
  filter(var == "No sabe") |>
  plot_barometro_lines()

# Guardar objetos ----
save_data(plot_barometro_mejorado_lineas)
save_data(plot_barometro_empeorado_lineas)
save_data(plot_barometro_igual_lineas)
save_data(plot_barometro_nose_lineas)
