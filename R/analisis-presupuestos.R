# Setup ----
library(tidyverse)
library(plotly)
library(here)
library(scales)

source(here("R", "utils_helpers.R"))
source(here("R", "fct_plots.R"))


# Load data ----
presupuestos <- read_data("presupuestos")

# Clean ----
ggplot_presupuestos <- presupuestos |>
  # quitar columnas redundantes
  mutate(
    text = str_glue(
      "{clasificacion}",
      "<br>Año: {year}",
      "<br>Presupuestado: {format(definitivo, big.mark = ' ', decimal.mark = ',')} euros",
      "<br>Ejecutado: {format(obligado, big.mark = ' ', decimal.mark = ',')} euros"
    )
  ) |>
  select(
    clasificacion,
    year,
    Superávit = definitivo,
    Déficit = obligado
  ) |>
  pivot_longer(
    cols = c(Superávit, Déficit),
    names_to = "var",
    values_to = "value"
  ) |>
  ggplot(
    aes(
      x = value,
      y = fct_rev(factor(year)),
      fill = var
    )
  ) +
  geom_col(position = "identity", alpha = 0.5) +
  facet_wrap(vars(clasificacion), scales = "free_x") +
  scale_x_continuous(
    labels = label_number(big.mark = " ", decimal.mark = ",", scale = 1 / 1000)
  ) +
  labs(
    x = "Candidad (miles de euros)",
    y = "Año",
    fill = NULL
  ) +
  theme_nefro()

plot_presupuestos <- make_plotly(ggplot_presupuestos)

# Save ----
save_data(plot_presupuestos)
