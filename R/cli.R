#' Command-line interface for validate_submission
#'
#' Provides a command-line interface for validating Makara database
#' submission files. This function is designed to be called from
#' Rscript or other command-line contexts.
#'
#' @param args Character vector of command-line arguments. If NULL,
#'   uses \code{commandArgs(trailingOnly = TRUE)}.
#'
#' @return Does not return; calls \code{quit()} with appropriate status code
#'
#' @examples
#' \dontrun{
#' # From R console:
#' validate_submission_cli(c("/path/to/data"))
#'
#' # From command line:
#' # Rscript -e 'makaraValidatr::validate_submission_cli()' /path/to/data
#' }
#'
#' @importFrom logger log_info log_error
#' @export
validate_submission_cli <- function(args = NULL) {
  if (!requireNamespace("optparse", quietly = TRUE)) {
    stop("Package 'optparse' is required for CLI. Install with: install.packages('optparse')")
  }

  require("makaraValidatr")

  options(warn = 1) # Print warnings as they occur

  if (is.null(args)) {
    args <- commandArgs(trailingOnly = TRUE)
  }

  # Define command line options
  option_list <- list(
    optparse::make_option(
      c("-n", "--ncei"),
      action = "store_true",
      default = FALSE,
      help = "Include NCEI validation rules"
    ),
    optparse::make_option(
      c("-t", "--tables"),
      type = "character",
      default = NULL,
      help = "Comma-separated list of specific tables to validate (default: all tables)",
      metavar = "table1,table2,..."
    ),
    optparse::make_option(
      c("-o", "--output"),
      type = "character",
      default = NULL,
      help = "Output file to save validation results",
      metavar = "output_file"
    )
  )

  # Create parser
  parser <- optparse::OptionParser(
    usage = "%prog [options] data_directory",
    description = "Validates CSV files for Makara Database using validate package",
    option_list = option_list
  )

  # Parse arguments
  parsed <- optparse::parse_args(parser, args = args, positional_arguments = 1)

  # Check if positional argument was provided
  if (length(parsed$args) != 1) {
    optparse::print_help(parser)
    cat("\nError: Please provide the data directory path\n")
    quit(status = 1)
  }

  data_dir <- parsed$args[1]

  if (!dir.exists(data_dir)) {
    cat("Error: Directory does not exist:", data_dir, "\n")
    quit(status = 1)
  }

  # Parse tables if specified
  tables_to_validate <- NULL
  if (!is.null(parsed$options$tables)) {
    tables_to_validate <- trimws(strsplit(parsed$options$tables, ",")[[1]])
  }

  # Run validation
  success <- validate_submission(
    data_dir,
    tables = tables_to_validate,
    ncei = parsed$options$ncei,
    output_file = parsed$options$output
  )

  if (success) {
    log_info("\u2713 All validations passed!")
    quit(status = 0)
  } else {
    log_error("\u2717 Some validations failed. Please check the output above.")
    quit(status = 1)
  }
}
