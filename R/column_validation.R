#' Validate columns in a data frame
#'
#' Checks for required columns and unexpected columns in a data frame.
#' Returns a tibble with validation errors for missing required columns
#' and warnings for unexpected columns.
#'
#' @param data A data frame to validate
#' @param columns A data frame with column definitions containing at least:
#'   \code{name}, \code{required}, and \code{required_unless} columns
#'
#' @return A tibble with columns: row, col, value, rule, error
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows transmute
#' @importFrom glue glue
#' @importFrom logger log_error log_warn log_info
#' @export
validate_columns <- function(data, columns) {
  all_cols <- columns[["name"]]
  required_cols <- columns[["name"]][
    columns$required & is.na(columns$required_unless)
  ]
  optional_cols <- setdiff(all_cols, required_cols)

  missing_required_cols <- tibble(
    col = setdiff(required_cols, colnames(data)),
    error = glue("Missing required column '{col}'"),
    rule = "required"
  )
  if (nrow(missing_required_cols) > 0) {
    log_error(
      "Missing required columns: {paste(missing_required_cols$col, collapse = ', ')}"
    )
  }

  unexpected_cols <- tibble(
    col = setdiff(colnames(data), all_cols),
    error = glue("Found unknown column '{col}'"),
    rule = "unknown"
  )
  if (nrow(unexpected_cols) > 0) {
    log_warn(
      "Unexpected columns found: {paste(unexpected_cols$col, collapse = ', ')}"
    )
  }

  missing_optional_cols <- setdiff(optional_cols, colnames(data))
  if (length(missing_optional_cols) > 0) {
    log_info(
      "Missing optional columns: {paste(missing_optional_cols, collapse = ', ')}"
    )
  }

  bind_rows(
    missing_required_cols,
    unexpected_cols
  ) |>
    transmute(
      row = NA_integer_,
      col,
      value = NA_character_,
      rule,
      error
    )
}

#' Create column type specifications for reading CSV files
#'
#' Generates a list of column type specifications for use with
#' \code{readr::read_csv()}. Automatically detects integer and float columns
#' based on column definitions, defaulting to character for all others.
#'
#' @param data A data frame with existing column names
#' @param columns A data frame with column definitions containing at least:
#'   \code{name} and \code{type} columns
#'
#' @return A list of column type specifications suitable for
#'   \code{readr::read_csv()}
#'
#' @importFrom readr col_integer col_double
#' @export
create_col_types <- function(data, columns) {
  col_types <- list(.default = "c")
  for (i in seq_len(nrow(columns))) {
    column <- columns[i, ]
    if (!column$name %in% colnames(data)) {
      next
    }

    if (column$type == "integer") {
      col_types[[column$name]] <- col_integer()
    } else if (column$type == "float") {
      col_types[[column$name]] <- col_double()
    }
  }
  col_types
}
