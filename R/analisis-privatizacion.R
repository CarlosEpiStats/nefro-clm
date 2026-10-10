# Setup ----
library(tidyverse)
library(plotly)
library(here)
library(janitor)

source(here("R", "utils_helpers.R"))
source(here("R", "fct_plots.R"))

# Load data ----
datos_privatizacion <- read_csv(
  here("R", "data", "data-privatizacion-total.csv"),
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(
    tipo_region = case_when(
      ccaa == "C la Mancha" ~ "Castilla-La Mancha",
      ccaa == "Media CCAA" ~ "Total Estado",
      TRUE ~ "Resto de regiones"
    ),
    year = 2026
  )

datos_privat_evolucion <- read_csv(
  here("R", "data", "data-privatizacion-evolucion.csv"),
  show_col_types = FALSE
) |>
  pivot_longer(
    cols = -CCAA,
    names_to = "year",
    values_to = "value"
  )

media_privat_evolucion <- datos_privat_evolucion |>
  summarise(
    value = mean(value),
    .by = "year"
  ) |>
  mutate(CCAA = "Media CCAA")

datos_privat_evol_todo <- datos_privat_evolucion |>
  bind_rows(media_privat_evolucion) |>
  mutate(
    tipo_region = case_when(
      CCAA == "Castilla la Mancha" ~ "Castilla-La Mancha",
      CCAA == "Media CCAA" ~ "Total Estado",
      TRUE ~ "Resto de regiones"
    ),
    year = as.integer(year),
    text = str_glue("Región: {CCAA}\nAño: {year}\nValor: {value}")
  ) |>
  rename(region = CCAA)

datos_bolsillo <- read_csv(
  here("R", "data", "data-privatizacion-gasto-bolsillo.csv"),
  show_col_types = FALSE
) |>
  pivot_longer(
    cols = -CCAA,
    names_to = "year",
    values_to = "value"
  ) |>
  rename(region = CCAA) |>
  mutate(
    tipo_region = case_when(
      region == "C la Mancha" ~ "Castilla-La Mancha",
      region == "Media CCAA" ~ "Total Estado",
      TRUE ~ "Resto de regiones"
    ),
    year = as.integer(year),
    text = str_glue("Región: {region}\nAño: {year}\nValor: {value}")
  )

datos_seguro <- read_csv(
  here("R", "data", "data-privatizacion-seguro-privado.csv"),
  show_col_types = FALSE
) |>
  pivot_longer(
    cols = -CCAA,
    names_to = "year",
    values_to = "value"
  ) |>
  rename(region = CCAA) |>
  mutate(
    tipo_region = case_when(
      region == "C la Mancha" ~ "Castilla-La Mancha",
      region == "Media CCAA" ~ "Total Estado",
      TRUE ~ "Resto de regiones"
    ),
    year = as.integer(year),
    text = str_glue("Región: {region}\nAño: {year}\nValor: {value}")
  )


# Gráficos de barras ----

plot_privat_mutuas <- plot_privat_bars(
  datos_privatizacion,
  var = "mutuas",
  axis_label = "Afiliados a Mutualidades del Estado que\neligen atención privada (%)"
)
plot_privat_seg_priv <- plot_privat_bars(
  datos_privatizacion,
  var = "seg_priv",
  axis_label = "Gasto sanitario en seguros privados (€/hab./año)"
)
plot_privat_gs_bols <- plot_privat_bars(
  datos_privatizacion,
  var = "gs_bols_ine_2025",
  axis_label = "Gasto sanitario de bolsillo per cápita (€/hab./año)"
)
plot_privat_camas_privadas <- plot_privat_bars(
  datos_privatizacion,
  var = "camas_privadas",
  axis_label = "Camas hospitalarias privadas (%)"
)
plot_privat_at_privada <- plot_privat_bars(
  datos_privatizacion,
  var = "at_privada",
  axis_label = "Equipamiento de alta tecnología en\nhospitales privados (%)"
)
plot_privat_gs_conciertos <- plot_privat_bars(
  datos_privatizacion,
  var = "gs_conciertos_privados",
  axis_label = "Gasto sanitario destinado a conciertos privados (%)"
)
plot_privat_esp_priv <- plot_privat_bars(
  datos_privatizacion,
  var = "esp_priv",
  axis_label = "Personas que acudieron a consultas de\nespecialistas privados (%)"
)
plot_privat_med_cab_priv <- plot_privat_bars(
  datos_privatizacion,
  var = "med_cab_priv",
  axis_label = "Personas que acudieron a consultas del\nmédico de cabecera privado (%)"
)
plot_privat_global <- plot_privat_bars(
  datos_privatizacion,
  var = "indice_global",
  axis_label = "Índice de privatización global"
)

# Gráficos de líneas ----
plot_privat_evol_global <- plot_nefro_lines(
  datos_privat_evol_todo,
  y_var = "value",
  y_label = "Índice de privatización global",
  text_var = "text"
)

plot_privat_evol_bolsillo <- plot_nefro_lines(
  datos_bolsillo,
  y_var = "value",
  y_label = "Gasto sanitario de bolsillo per cápita (€/hab./año)",
  text_var = "text"
)

plot_privat_evol_seguro <- plot_nefro_lines(
  datos_seguro,
  y_var = "value",
  y_label = "Gasto sanitario en seguros privados (€/hab./año)",
  text_var = "text"
)

# Guardar objetos ----

save_data(plot_privat_mutuas)
save_data(plot_privat_seg_priv)
save_data(plot_privat_gs_bols)
save_data(plot_privat_camas_privadas)
save_data(plot_privat_at_privada)
save_data(plot_privat_gs_conciertos)
save_data(plot_privat_esp_priv)
save_data(plot_privat_med_cab_priv)
save_data(plot_privat_global)
save_data(plot_privat_evol_global)
save_data(plot_privat_evol_bolsillo)
save_data(plot_privat_evol_seguro)
