# Setup ----
library(tidyverse)
library(plotly)

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
    )
  )

# Gráfico base con ggplot2
grafico <- ggplot(
  datos_grafico,
  aes(
    x = year,
    y = tasa_total,
    group = region,
    color = tipo_region,
    text = paste0(
      "Región: ",
      region,
      "<br>Año: ",
      year,
      "<br>Tasa: ",
      round(tasa_total, 2),
      " trasplantes por millón"
    )
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
  scale_color_manual(
    values = c(
      "Resto de regiones" = "#BDBDBD",
      "Castilla-La Mancha" = "#DC143C",
      "Total Estado" = "#000000"
    ),
    breaks = c(
      "Castilla-La Mancha",
      "Total Estado",
      "Resto de regiones"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(datos_grafico$year))
  ) +
  labs(
    title = "Tasa de trasplantes renales por millón de población",
    x = "Año",
    y = "Trasplantes por millón de población",
    color = NULL
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

saveRDS(
  grafico_interactivo,
  here("R", "data", "grafico-ont.rds")
)
