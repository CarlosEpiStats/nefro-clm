# Setup ----
library(tidyverse)
library(plotly)

# Load data ----
datos <- read_csv(
  file.path("R", "data", "data-ont.csv"),
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
    )
  )

# Gráfico base con ggplot2
grafico <- ggplot(
  datos_grafico,
  aes(
    x = year,
    y = tasa_total,
    group = region,
    text = str_glue(
      "Región: {region},<br>Año: {year},<br>Tasa: {round(tasa_total, 2)} trasplantes por millón"
    )
  )
) +
  # Resto de regiones
  geom_line(
    data = filter(datos_grafico, tipo_region == "Resto de regiones"),
    color = "grey75",
    linewidth = 0.7,
    alpha = 0.8
  ) +
  # Castilla-La Mancha
  geom_line(
    data = filter(datos_grafico, tipo_region == "Castilla-La Mancha"),
    color = "#DC143C",
    linewidth = 1.4
  ) +
  # Total Estado
  geom_line(
    data = filter(datos_grafico, tipo_region == "Total Estado"),
    color = "black",
    linewidth = 1.3,
    linetype = "dashed"
  ) +
  geom_point(
    aes(color = tipo_region),
    size = 1.8,
    alpha = 0.9
  ) +
  scale_color_manual(
    values = c(
      "Resto de regiones" = "grey75",
      "Castilla-La Mancha" = "#DC143C",
      "Total Estado" = "black"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(datos_grafico$year))
  ) +
  labs(
    title = "Tasa de trasplantes renales por millón de población",
    x = "Año",
    y = "Trasplantes por millón de población",
    color = NULL,
    caption = "Fuente: ONT. Actividad de donación y trasplante renal España. Años 2020-2025.<br>Elaboración propia"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Convertir el gráfico a interactivo
grafico_interactivo <- ggplotly(
  grafico,
  tooltip = "text"
) |>
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(color = "black")
    ),
    annotations = list(
      list(
        text = "Fuente: elaboración propia a partir de los informes de Actividad de donación y trasplante renal en España, Organización Nacional de Transplantes.",
        x = 0,
        y = -0.2,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE,
        align = "left",
        font = list(
          size = 10,
          color = "grey40"
        )
      )
    ),
    margin = list(
      b = 150
    )
  )
grafico_interactivo
