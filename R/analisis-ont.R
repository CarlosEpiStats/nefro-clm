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

plot_nefro_bars <- function(year_val, variable = c("trasplantes", "espera")) {
  variable <- match.arg(variable)
  if (variable == "trasplantes") {
    var <- "tasa_total"
    axis_label <- "Trasplantes por millón de población"
    text_var <- "text_trasplantes"
  } else if (variable == "espera") {
    var <- "le_pmp"
    axis_label <- "Lista de espera estimada por millón de población"
    text_var <- "text_espera"
  }
  p <- datos_grafico |>
    dplyr::filter(year == year_val) |>
    ggplot(
      aes(
        x = .data[[var]],
        y = fct_reorder(region, .data[[var]]),
        fill = tipo_region,
        text = .data[[text_var]]
      )
    ) +
    geom_col() +
    scale_fill_manual(values = values_nefro()) +
    labs(
      x = axis_label,
      y = NULL,
      fill = NULL
    ) +
    theme_nefro()
  p |> make_plotly()
}


plot_nefro_lines <- function(y_var, y_label, text_var) {
  data <- datos_grafico
  plot <- ggplot(
    data,
    aes(
      x = year,
      y = .data[[y_var]],
      group = region,
      color = tipo_region,
      text = .data[[text_var]]
    )
  ) +
    # Resto de regiones
    geom_line(
      data = filter(data, tipo_region == "Resto de regiones"),
      linewidth = 0.7,
      alpha = 0.55
    ) +
    geom_point(
      data = filter(data, tipo_region == "Resto de regiones"),
      size = 1.5,
      alpha = 0.55
    ) +
    # Total Estado
    geom_line(
      data = filter(data, tipo_region == "Total Estado"),
      linewidth = 1.3,
      linetype = "dashed"
    ) +
    geom_point(
      data = filter(data, tipo_region == "Total Estado"),
      size = 2
    ) +
    # Castilla-La Mancha, dibujada en último lugar
    geom_line(
      data = filter(data, tipo_region == "Castilla-La Mancha"),
      linewidth = 1.8,
      alpha = 1
    ) +
    geom_point(
      data = filter(data, tipo_region == "Castilla-La Mancha"),
      size = 2.5,
      alpha = 1
    ) +
    scale_color_manual(values = values_nefro()) +
    scale_x_continuous(
      breaks = sort(unique(data$year))
    ) +
    labs(
      x = "Año",
      y = y_label,
      color = NULL
    ) +
    theme_nefro()

  plot |> make_plotly()
}

save_data <- function(object) {
  file_name <- deparse(substitute(object))
  saveRDS(
    object,
    here("R", "data", paste0(file_name, ".rds"))
  )
}

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
