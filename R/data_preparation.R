#' Add JSON format validation columns
#'
#' For each JSON column in the data, adds a corresponding boolean column
#' indicating whether the JSON is valid. The new column is named
#' \code{<column_name>_is_json_format}.
#'
#' @param data A data frame to process
#' @param json_columns Character vector of column names containing JSON data
#'
#' @return Data frame with additional JSON validation columns
#'
#' @examples
#' \dontrun{
#' data <- data.frame(metadata = c('{"key": "value"}', 'invalid'))
#' prepare_data_json(data, "metadata")
#' }
#'
#' @export
prepare_data_json <- function(data, json_columns) {
  for (col in json_columns) {
    if (col %in% colnames(data)) {
      data[[paste0(col, "_is_json_format")]] <- sapply(
        data[[col]],
        is_valid_json_format
      )
    }
  }
  data
}

#' Apply default values to missing data
#'
#' Replaces NA or empty string values with default values specified in the
#' column definitions.
#'
#' @param data A data frame to process
#' @param columns A data frame with column definitions containing \code{name}
#'   and \code{default} columns
#'
#' @return Data frame with default values applied
#'
#' @export
prepare_data_defaults <- function(data, columns) {
  column_defaults <- columns[!is.na(columns$default), c("name", "default")]
  for (i in seq_len(nrow(column_defaults))) {
    col_name <- column_defaults$name[i]
    default_value <- column_defaults$default[i]
    if (col_name %in% colnames(data)) {
      data[[col_name]][
        is.na(data[[col_name]]) | data[[col_name]] == ""
      ] <- default_value
    }
  }
  data
}

#' Prepare data for validation
#'
#' Applies default values and adds validation helper columns to prepare
#' data for validation. Also adds a row number column at the beginning.
#'
#' @param data A data frame to prepare
#' @param columns A data frame with column definitions
#'
#' @return Prepared data frame ready for validation
#'
#' @importFrom dplyr mutate row_number everything
#' @export
prepare_data <- function(data, columns) {
  # Apply default values
  data <- prepare_data_defaults(data, columns)

  # Validate JSON columns
  json_columns <- columns[["name"]][columns$type == "json"]
  if (length(json_columns) > 0) {
    data <- prepare_data_json(data, json_columns)
  }

  data |>
    mutate(
      row = row_number(),
      .before = everything()
    )
}
