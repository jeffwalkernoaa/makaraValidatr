#' Format validation patterns
#'
#' Named vector of regular expressions for validating different data formats.
#'
#' @format A named character vector with the following elements:
#' \describe{
#'   \item{code}{Uppercase alphanumeric codes with underscores and hyphens}
#'   \item{code_list}{Comma-separated list of codes, spaces allowed between codes}
#'   \item{timestamp}{ISO 8601 timestamp with timezone}
#'   \item{date}{ISO 8601 date (YYYY-MM-DD)}
#'   \item{timezone}{UTC timezone offset}
#'   \item{integer}{Integer numbers}
#'   \item{float}{Floating point numbers}
#'   \item{bool}{Boolean values (TRUE/FALSE)}
#'   \item{url}{HTTP/HTTPS URLs}
#'   \item{uri}{Generic URI with scheme}
#' }
#' @export
formats <- c(
  code = "^[A-Z0-9_-]+$",
  code_list = "^[A-Z0-9_, -]+$",
  timestamp = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?([+-]\\d{4}(:\\d{2})?|Z)$",
  date = "^\\d{4}-\\d{2}-\\d{2}$",
  timezone = "^UTC([+-]([0-9]|0[0-9]|1[0-4]):?([0-5][0-9])?)?$",
  integer = "^-?\\d+$",
  float = "^-?\\d*(\\.\\d+)?$",
  bool = "^(TRUE|FALSE)$",
  url = "^https?://",
  uri = "^[a-zA-Z][a-zA-Z0-9+.-]*://"
)

#' Minimum valid date
#'
#' The earliest date considered valid for date fields in Makara database.
#'
#' @format A character string in ISO 8601 format (YYYY-MM-DD)
#' @export
MIN_DATE <- "1980-01-01"

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Package initialization
  invisible()
}
