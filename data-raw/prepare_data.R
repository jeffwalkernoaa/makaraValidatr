## Code to prepare package data
## This script converts CSV files from inst/extdata/ to .rda files in data/

library(readr)
library(dplyr)
library(tibble)

# Column Definitions ------------------------------------------------------

column_definitions <- read_csv(
  "inst/extdata/column_definitions.csv",
  col_types = cols(
    .default = col_character(),
    index = col_integer(),
    min = col_double(),
    required = col_logical(),
    required_ncei = col_logical(),
    max = col_double()
  )
) |>
  mutate(
    across(where(is.logical), \(x) coalesce(x, FALSE))
  ) |>
  nest_by(table) |>
  deframe()

usethis::use_data(column_definitions, overwrite = TRUE)


# Reference Tables --------------------------------------------------------

reference_tables <- read_csv(
  "inst/extdata/reference_tables.csv",
  col_types = cols(
    .default = col_character()
  )
) |>
  nest_by(table) |>
  deframe()

usethis::use_data(reference_tables, overwrite = TRUE)


# Manual Rules ------------------------------------------------------------

manual_rules <- read_csv(
  "inst/extdata/manual_rules.csv",
  col_types = cols(
    .default = col_character()
  )
) |>
  nest_by(table) |>
  deframe()

usethis::use_data(manual_rules, overwrite = TRUE)
