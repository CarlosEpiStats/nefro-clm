library(tabulapdf)
library(here)
library(tidyverse)
library(janitor)
library(cli)

source(here("R", "utils_helpers.R"))

#' find keyword in pdf pages
find_pages <- function(pdf_file, keyword, pages) {
  text <- extract_text(
    file = pdf_file,
    pages = pages
  )
  pages[grepl(keyword, text)]
}

#' convert Spanish text format to number
parse_number_es <- function(x) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  parse_number(
    x,
    locale = locale(decimal_mark = ",", grouping_mark = ".")
  )
}

#' full workflow
extract_table <- function(file, keyword = "diálisis") {
  year <- as.integer(str_extract(file, "[0-9]{4}"))

  pdf_file <- here(
    "documentos",
    "presupuestos",
    file
  )

  cli_h1("Extracting table from {.file {file}}")
  cli_alert_info("Counting pages...")

  n_pages <- get_n_pages(pdf_file)

  cli_alert_success("Found {n_pages} pages")
  cli_alert_info("Searching for {.val {keyword}}")

  matching_pages <- find_pages(
    pdf_file,
    keyword,
    pages = seq_len(n_pages)
  )

  if (length(matching_pages) == 0) {
    cli_alert_warning("No pages found containing {.val {keyword}}")
    return(NULL)
  }

  cli_alert_success(
    "Matching page{?s}: {paste(matching_pages, collapse = ', ')}"
  )

  selected_page <- matching_pages[[1]]

  cli_alert_info("Extracting table from page {selected_page}...")

  table_result <- suppressMessages(
    extract_tables(
      file = pdf_file,
      pages = selected_page,
      method = "stream",
      output = "tibble"
    )
  )

  cli_alert_success("Table extracted")
  cli_alert_info("Cleaning and filtering rows...")

  tabla <- table_result[[1]] |>
    clean_names() |>
    filter(
      str_detect(
        clasificacion_economica,
        keyword
      )
    ) |>
    mutate(
      across(
        -clasificacion_economica,
        parse_number_es
      ),
      year = year
    )

  cli_alert_success("Finished: {nrow(tabla)} rows returned")

  tabla
}

# Run ----
keyword <- "diálisis"
files <- list.files(here("documentos", "presupuestos"))

res <- map(files, extract_table)
presupuestos <- bind_rows(res) |>
  mutate(
    # 2026_08 pilla mal el nombre de la columna
    c_inicial = coalesce(c_inicial, c_inicial_definitivo),
    definitivo = coalesce(definitivo, c_inicial_definitivo),
    # etiquetas más fáciles
    clasificacion = fct(str_replace_all(
      clasificacion_economica,
      "2530[BC]  ",
      ""
    )),
  ) |>
  relocate(clasificacion) |>
  select(-c(c_inicial_definitivo, x6, clasificacion_economica))

# Save ----
save_data(presupuestos)
