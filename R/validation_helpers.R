#' Check if string is valid JSON format
#'
#' Validates whether a string contains properly formatted JSON.
#' Empty strings and NA values are considered valid.
#'
#' @param json_str Character string to validate
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
is_valid_json_format <- function(json_str) {
  if (is.na(json_str) || stringr::str_trim(json_str) == "") {
    return(TRUE)
  }
  tryCatch(
    {
      fromJSON(json_str)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}
