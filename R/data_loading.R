#' Load column definitions from CSV file
#'
#' Loads and parses column definitions from a CSV file or uses package data.
#' Returns a nested list by table name.
#'
#' @param file Path to column definitions CSV file. If NULL, uses package data.
#'
#' @return A named list of data frames, one per table, containing column
#'   definitions
#'
#' @importFrom readr read_csv cols col_character col_integer col_double col_logical
#' @importFrom dplyr mutate across coalesce nest_by
#' @importFrom tibble deframe
#' @export
load_column_definitions <- function(file = NULL) {
  if (is.null(file)) {
    # Use package data if available, otherwise load from inst/extdata
    if (exists("column_definitions", envir = asNamespace("makaraValidatr"))) {
      return(get("column_definitions", envir = asNamespace("makaraValidatr")))
    }
    file <- system.file("extdata", "column_definitions.csv", package = "makaraValidatr")
  }

  x <- read_csv(
    file,
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
}

#' Load reference tables from CSV file
#'
#' Loads reference lookup tables from a CSV file or uses package data.
#' Returns a nested list by table name.
#'
#' @param file Path to reference tables CSV file. If NULL, uses package data.
#'
#' @return A named list of data frames, one per reference table, containing
#'   valid codes and names
#'
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr nest_by
#' @importFrom tibble deframe
#' @export
load_reference_tables <- function(file = NULL) {
  if (is.null(file)) {
    # Use package data if available, otherwise load from inst/extdata
    if (exists("reference_tables", envir = asNamespace("makaraValidatr"))) {
      return(get("reference_tables", envir = asNamespace("makaraValidatr")))
    }
    file <- system.file("extdata", "reference_tables.csv", package = "makaraValidatr")
  }

  x <- read_csv(
    file,
    col_types = cols(
      .default = col_character()
    )
  ) |>
    nest_by(table) |>
    deframe()
}

#' Load manual validation rules from CSV file
#'
#' Loads custom validation rules from a CSV file or uses package data.
#' Returns a nested list by table name.
#'
#' @param file Path to manual rules CSV file. If NULL, uses package data.
#'
#' @return A named list of data frames, one per table, containing custom
#'   validation rules
#'
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr nest_by
#' @importFrom tibble deframe
#' @export
load_manual_rules <- function(file = NULL) {
  if (is.null(file)) {
    # Use package data if available, otherwise load from inst/extdata
    if (exists("manual_rules", envir = asNamespace("makaraValidatr"))) {
      return(get("manual_rules", envir = asNamespace("makaraValidatr")))
    }
    file <- system.file("extdata", "manual_rules.csv", package = "makaraValidatr")
  }

  read_csv(
    file,
    col_types = cols(
      .default = col_character()
    )
  ) |>
    nest_by(table) |>
    deframe()
}
