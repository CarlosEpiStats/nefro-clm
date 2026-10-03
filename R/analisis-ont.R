# Setup ----
library(tidyverse)
library(plotly)
library(here)

source(here("R", "utils_helpers.R"))


# Load data ----
datos_ont <- read_csv(
  here("R", "data", "data-ont.csv"),
  show_col_types = FALSE
) |>
  mutate(
    region = str_replace_all(region, "UNIDAD", "\\.")
  )

datos_lista_espera <- read_csv(
  here("R", "data", "data-lista-espera.csv"),
  show_col_types = FALSE
) |>
  mutate(
    region = str_replace_all(region, "Í", "I"),
    region = str_replace_all(region, "Á", "A"),
    region = str_replace_all(region, "Ó", "O"),
    region = str_replace_all(region, "-", " "),
    region = str_replace_all(region, "  ", " ")
  )

datos <- datos_ont |>
  full_join(datos_lista_espera, join_by(year, region))

# Calcular la tasa total de trasplantes renales por millón de población
datos_grafico <- datos |>
  mutate(
    tasa_total = tr_dv + tr_me + tr_a,
    tipo_region = case_when(
      region == "CASTILLA LA MANCHA" ~ "Castilla-La Mancha",
      region == "TOTAL ESTADO" ~ "Total Estado",
      TRUE ~ "Resto de regiones"
    ),
    tasa_round = format(round(tasa_total, 2), nsmall = 2),
    text_trasplantes = str_glue(
      "Región: {region}<br>Año: {year}<br>Tasa: {tasa_round} trasplantes por millón"
    ),
    pcnt = tx_pmp / le_pmp,
    espera_round = format(round(le_pmp, 2), nsmall = 2),
    text_espera = str_glue(
      "Región: {region}<br>Año: {year}<br>Lista de espera: {espera_round} pacientes por millón"
    )
  )

# Trasplantes ----
## Gráfico de líneas ----
plot_trasplantes_lineas <- plot_nefro_lines(
  y_var = "tasa_total",
  y_label = "Trasplantes por millón de población",
  text_var = "text_trasplantes"
)

## Gráficos de barras ----
plot_trasplantes_2020 <- plot_nefro_bars(2020)
plot_trasplantes_2021 <- plot_nefro_bars(2021)
plot_trasplantes_2022 <- plot_nefro_bars(2022)
plot_trasplantes_2023 <- plot_nefro_bars(2023)
plot_trasplantes_2024 <- plot_nefro_bars(2024)
plot_trasplantes_2025 <- plot_nefro_bars(2025)

# Lista de espera ----
plot_espera_lineas <- plot_nefro_lines(
  y_var = "le_pmp",
  y_label = "Lista de espera estimada por millón de población",
  text_var = "text_espera"
)
plot_espera_2020 <- plot_nefro_bars(2020, "espera")
plot_espera_2021 <- plot_nefro_bars(2021, "espera")
plot_espera_2022 <- plot_nefro_bars(2022, "espera")
plot_espera_2023 <- plot_nefro_bars(2023, "espera")
plot_espera_2024 <- plot_nefro_bars(2024, "espera")
plot_espera_2025 <- plot_nefro_bars(2025, "espera")


# Guardar objetos ----

save_data(plot_trasplantes_lineas)
save_data(plot_trasplantes_2020)
save_data(plot_trasplantes_2021)
save_data(plot_trasplantes_2022)
save_data(plot_trasplantes_2023)
save_data(plot_trasplantes_2024)
save_data(plot_trasplantes_2025)
save_data(plot_espera_lineas)
save_data(plot_espera_2020)
save_data(plot_espera_2021)
save_data(plot_espera_2022)
save_data(plot_espera_2023)
save_data(plot_espera_2024)
save_data(plot_espera_2025)
