library(rvest)
library(httr2)
library(xml2)
library(here)
library(tidyverse)

# Functions ----
is_pdf_link <- function(x) {
  grepl("\\.pdf($|\\?|#)", x, ignore.case = TRUE)
}

filter_pdf_links <- function(x) {
  x[is_pdf_link(x)]
}

download_pdf <- function(pdf_url) {
  full_url <- url_absolute(pdf_url, url)
  # Nombres como 2026.pdf, 2026.pdf...

  year <- str_extract(pdf_url, "([0-9]{4})[-._]", 1)
  filename <- paste0(year, ".pdf")
  destination <- here(res_dir, filename)
  version <- 1

  while (file.exists(destination)) {
    version <- version + 1
    filename <- paste0(year, "_", version, ".pdf")
    destination <- here(res_dir, filename)
  }

  download.file(
    url = full_url,
    destfile = destination,
    mode = "web"
  )
}

# Paths ----
url <- "https://intervencion.castillalamancha.es/transparencia-economico-financiera/ejecucion-presupuestaria-indicad"
res_dir <- here("documentos", "presupuestos")


# Download ----
page <- request(url) |>
  req_user_agent("Mozilla/5.0") |>
  req_perform() |>
  resp_body_html()

pdf_links <- page |>
  html_elements("a[href]") |>
  html_attr("href") |>
  na.omit() |>
  filter_pdf_links()

# Sacar los pdf de diciembre 2026
pdf_diciembre <- pdf_links[grepl("diciembre", pdf_links, ignore.case = TRUE)] |>
  rev()

walk(
  pdf_diciembre,
  download_pdf
)
