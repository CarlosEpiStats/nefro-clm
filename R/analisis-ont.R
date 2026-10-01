# Setup ----
library(tidyverse)
library(plotly)
library(here)

# Custom functions ----
theme_nefro <- function() {
  theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

make_plotly <- function(p) {
  p |>
    ggplotly(tooltip = "text") |>
    layout(
      hoverlabel = list(
        bgcolor = "white",
        font = list(color = "black")
      ),
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE)
    ) |>
    config(
      displayModeBar = FALSE
    )
}

values_nefro <- function() {
  c(
    "Resto de regiones" = "#BDBDBD",
    "Castilla-La Mancha" = "#DC143C",
    "Total Estado" = "#000000"
  )
}

plot_transplant_year <- function(year_val) {
  p <- datos_grafico |>
    dplyr::filter(year == year_val) |>
    ggplot(
      aes(
        x = tasa_total,
        y = fct_reorder(region, tasa_total),
        fill = tipo_region,
        text = hovertext
      )
    ) +
    geom_col() +
    scale_fill_manual(values = values_nefro()) +
    labs(
      x = "Trasplantes por millón de población",
      y = NULL,
      fill = NULL
    ) +
    theme_nefro()
  p |> make_plotly()
}

save_data <- function(object) {
  file_name <- deparse(substitute(object))
  saveRDS(
    object,
    here("R", "data", paste0(file_name, ".rds"))
  )
}

# Load data ----
datos <- read_csv(
  here("R", "data", "data-ont.csv"),
  show_col_types = FALSE
)

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
    hovertext = str_glue(
      "Región: {region}<br>Año: {year}<br>Tasa: {tasa_round} trasplantes por millón"
    )
  )

# Gráfico de líneas ----
# Gráfico base con ggplot2
grafico_lineas <- ggplot(
  datos_grafico,
  aes(
    x = year,
    y = tasa_total,
    group = region,
    color = tipo_region,
    text = hovertext
  )
) +
  # Resto de regiones
  geom_line(
    data = filter(datos_grafico, tipo_region == "Resto de regiones"),
    linewidth = 0.7,
    alpha = 0.55
  ) +
  geom_point(
    data = filter(datos_grafico, tipo_region == "Resto de regiones"),
    size = 1.5,
    alpha = 0.55
  ) +
  # Total Estado
  geom_line(
    data = filter(datos_grafico, tipo_region == "Total Estado"),
    linewidth = 1.3,
    linetype = "dashed"
  ) +
  geom_point(
    data = filter(datos_grafico, tipo_region == "Total Estado"),
    size = 2
  ) +
  # Castilla-La Mancha, dibujada en último lugar
  geom_line(
    data = filter(datos_grafico, tipo_region == "Castilla-La Mancha"),
    linewidth = 1.8,
    alpha = 1
  ) +
  geom_point(
    data = filter(datos_grafico, tipo_region == "Castilla-La Mancha"),
    size = 2.5,
    alpha = 1
  ) +
  scale_color_manual(values = values_nefro()) +
  scale_x_continuous(
    breaks = sort(unique(datos_grafico$year))
  ) +
  labs(
    x = "Año",
    y = "Trasplantes por millón de población",
    color = NULL
  ) +
  theme_nefro()

# Convertir el gráfico a interactivo
plot_trasplantes_lineas <- grafico |> make_plotly()


# Gráficos de barras ----
plot_trasplantes_2020 <- plot_transplant_year(2020)
plot_trasplantes_2021 <- plot_transplant_year(2021)
plot_trasplantes_2022 <- plot_transplant_year(2022)
plot_trasplantes_2023 <- plot_transplant_year(2023)
plot_trasplantes_2024 <- plot_transplant_year(2024)
plot_trasplantes_2025 <- plot_transplant_year(2025)

# Guardar objetos ----

save_data(plot_trasplantes_lineas)
save_data(plot_trasplantes_2020)
save_data(plot_trasplantes_2021)
save_data(plot_trasplantes_2022)
save_data(plot_trasplantes_2023)
save_data(plot_trasplantes_2024)
save_data(plot_trasplantes_2025)
