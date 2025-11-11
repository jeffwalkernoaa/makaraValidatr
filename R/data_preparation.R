#' Add JSON format validation columns
#'
#' For each JSON column in the data, adds a corresponding boolean column
#' indicating whether the JSON is valid. The new column is named
#' \code{<column_name>_valid_json}.
#'
#' @param data A data frame to process
#' @param columns A data frame of column definitions containing code list info
#' with \code{name} column
#'
#' @return Data frame with additional JSON validation columns
#'
#' @examples
#' \dontrun{
#' data <- data.frame(metadata = c('{"key": "value"}', 'invalid'))
#' prepare_data_json(data, list(name = "metadata"))
#' }
#'
#' @export
prepare_data_json <- function(data, columns = NULL) {
  if (is.null(columns) || nrow(columns) == 0) {
    return(data)
  }
  for (i in seq_len(nrow(columns))) {
    col <- columns$name[i]
    if (col %in% colnames(data)) {
      data[[paste0(col, "_valid_json")]] <- sapply(
        data[[col]],
        is_valid_json_format
      )
    }
  }
  data
}

#' Add code list validation columns
#'
#' For each code list column in the data, adds a corresponding boolean column
#' indicating whether the list contains valied codes. The new column is named
#' \code{<column_name>_valid_code_list}.
#'
#' @param data A data frame to process
#' @param columns A data frame of column definitions containing code list info
#' with \code{name}, \code{reference_table} and \code{transaction_table} columns
#' @param reference_tables A named list of reference tables, each containing a 
#' data frame with a \code{code} column
#'
#' @return Data frame with additional code list validation columns
#'
#' @examples
#' \dontrun{
#' data <- data.frame(tab_code = c("x,y", "z"))
#' prepare_data_code_list(
#'   data,
#'   list(name = "tab_code", reference_table = "tab"),
#'   list(tab = c("x", "y"))
#' )
#' }
#'
#' @export
prepare_data_code_list <- function(data, columns, reference_tables = list()) {
  if (is.null(columns) || nrow(columns) == 0) {
    return(data)
  }
  for (i in seq_len(nrow(columns))) {
    col_name <- columns$name[i]
    col_reference_table <- columns$reference_table[i]
    codes <- reference_tables[[col_reference_table]][["code"]]
    if (is.null(codes)) {
      data[[paste0(col_name, "_valid_code_list")]] <- TRUE
    } else {
      data[[paste0(col_name, "_valid_code_list")]] <- sapply(
        data[[col_name]],
        function(x) is_valid_code_list(x, codes)
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
#' @param reference_tables A named list of reference tables for code list validation
#'
#' @return Prepared data frame ready for validation
#'
#' @importFrom dplyr mutate row_number everything
#' @export
prepare_data <- function(data, columns, reference_tables) {
  # Apply default values
  data <- prepare_data_defaults(data, columns)

  # Validate JSON columns
  json_columns <- columns[columns$type == "json", ]
  data <- prepare_data_json(data, json_columns)

  # Code lists
  code_list_columns <- columns[columns$type == "code_list" & !is.na(columns$reference_table), ]
  data <- prepare_data_code_list(data, code_list_columns, reference_tables = reference_tables)

  data |>
    mutate(
      row = row_number(),
      .before = everything()
    )
}
