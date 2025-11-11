#' Check if string is valid JSON format
#'
#' Validates whether a string contains properly formatted JSON.
#' Empty strings and NA values are considered valid.
#'
#' @param value Character string to validate
#'
#' @return Logical value: TRUE if valid JSON or empty/NA, FALSE otherwise
#'
#' @examples
#' is_valid_json_format('{"key": "value"}')  # TRUE
#' is_valid_json_format('invalid json')      # FALSE
#' is_valid_json_format(NA)                  # TRUE
#' is_valid_json_format("")                  # TRUE
#'
#' @importFrom jsonlite fromJSON
#' @export
is_valid_json_format <- function(value) {
  if (is.na(value) || trimws(value) == "") {
    return(TRUE)
  }
  tryCatch(
    {
      fromJSON(value)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Check if string is valid code list
#'
#' Validates whether a string contains a comma-separated
#' list of valid codes from a predefined list.
#' Empty strings and NA values are considered valid.
#'
#' @param value Character string to validate as comma-separates list of values
#' @param codes Character vector of valid code values
#'
#' @return Logical value: TRUE if valid code list or empty/NA, FALSE otherwise
#'
#' @examples
#' is_valid_code_list('x',    c('x', 'y')) # TRUE
#' is_valid_code_list('x,y',  c('x', 'y')) # TRUE
#' is_valid_code_list('x, y', c('x', 'y')) # TRUE
#' is_valid_code_list('z',    c('x', 'y')) # FALSE
#' is_valid_code_list('x,z',  c('x', 'y')) # FALSE
#' is_valid_code_list(NA, c('x', 'y'))     # TRUE
#' is_valid_code_list("", c('x', 'y'))     # TRUE
#'
#' @export
is_valid_code_list <- function(value, codes) {
  if (is.na(value) || trimws(value) == "") {
    return(TRUE)
  }
  split_values <- trimws(unlist(strsplit(value, ",")))
  all(split_values %in% codes)
}
