library(here)
library(tidyverse)
library(bslib)
library(htmltools)
library(plotly)

#' Save object
save_data <- function(object) {
  file_name <- deparse(substitute(object))
  saveRDS(
    object,
    here("R", "data", paste0(file_name, ".rds"))
  )
}

#' Read saved object
read_data <- function(filename) {
  readRDS(
    here("R", "data", paste0(filename, ".rds"))
  )
}

#' Create bslib card with default header and footer
card_with_info <- function(mode = c("trasplantes", "espera"), year, ...) {
  mode <- match.arg(mode)
  if (mode == "trasplantes") {
    header <- str_glue(
      "Tasa de trasplantes renales por millón de población, {year}."
    )
  } else if (mode == "espera") {
    header <- str_glue(
      "Tasa de espera estimada por millón de población, {year}."
    )
  }
  card(
    full_screen = TRUE,
    card_header(class = "bg-primary", header),
    card_body(...),
    card_footer(
      class = "bg-light",
      a(
        "Fuente: Informes de la Organización Nacional de Trasplantes",
        href = "sources.html#sec-informes-ont"
      )
    )
  )
}


# Custom plot functions ----
#' Custom theme
theme_nefro <- function() {
  theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

#' Convert ggplot to plotly
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

#' Color scale
values_nefro <- function() {
  c(
    "Resto de regiones" = "#BDBDBD",
    "Castilla-La Mancha" = "#DC143C",
    "Total Estado" = "#000000"
  )
}

#' Bar plot
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

#' Line plot
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
