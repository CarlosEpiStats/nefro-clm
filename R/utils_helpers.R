library(here)
library(tidyverse)
library(bslib)
library(htmltools)

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
card_with_info <- function(
  mode = c("trasplantes", "espera", "presupuestos"),
  year,
  ...
) {
  mode <- match.arg(mode)
  if (mode == "trasplantes") {
    header <- str_glue(
      "Tasa de trasplantes renales por millón de población, {year}."
    )
    footer <- a(
      "Fuente: Informes de la Organización Nacional de Trasplantes",
      href = "sources.html#sec-informes-ont"
    )
  } else if (mode == "espera") {
    header <- str_glue(
      "Tasa de espera estimada por millón de población, {year}."
    )
    footer <- a(
      "Fuente: Informes de la Organización Nacional de Trasplantes",
      href = "sources.html#sec-informes-ont"
    )
  } else if (mode == "presupuestos") {
    header <- str_glue(
      "Presupuesto de hemodiálisis en Castilla-La Mancha, {year}."
    )
    footer <- a(
      "Fuente: Ejecución Presupuestaria Mensual de las Comunidades Autónomas",
      href = "sources.html#sec-presupuestos"
    )
  }
  card(
    full_screen = TRUE,
    card_header(class = "bg-primary", header),
    card_body(...),
    card_footer(
      class = "bg-light",
      footer
    )
  )
}

#' Formatear decimales
format_big <- function(x) {
  format(
    round(x),
    big.mark = ' ',
    decimal.mark = ','
  )
}
