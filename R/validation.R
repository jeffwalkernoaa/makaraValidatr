#' Extract failed rows from validation results
#'
#' Processes validation results to extract detailed information about
#' rows that failed validation, including the specific values that
#' caused the failure.
#'
#' @param results Validation results object from \code{validate::confront()}
#' @param prepared_data The data frame that was validated
#' @param rules Validator object containing the rules
#' @param rules_data Data frame with rule definitions (name, label, expression)
#'
#' @return A tibble with columns: row, col, values, error
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter mutate select transmute left_join
#' @importFrom purrr map map_chr
#' @importFrom tidyr unnest
#' @export
extract_failed_rows <- function(results, prepared_data, rules, rules_data) {

  if (all(validate::summary(results)$fails == 0)) {
    return(NULL)
  }

  validate::summary(results) |>
    as_tibble() |>
    filter(!error, fails > 0) |>
    mutate(
      violating = map(name, function(name) {
        rows <- validate::violating(prepared_data, rules[name])
        vars <- intersect(validate::variables(rules[name]), colnames(prepared_data))

        rows |>
          mutate(
            col = paste0(vars, collapse = ","),
            values = map_chr(row, function(row) {
              pairs <- map_chr(vars, function(var) {
                value <- prepared_data[[var]][[row]]
                if (is.na(value)) return(paste0(var, "=NA"))
                paste0(var, "='", value, "'")
              })
              paste0(pairs, collapse = ", ")
            })
          ) |>
          select(
            row,
            col,
            values
          )
      }),
    ) |>
    left_join(
      select(rules_data, name, label),
      by = "name"
    ) |>
    select(name, label, expression, violating) |>
    unnest(violating) |>
    transmute(
      row,
      col,
      values,
      error = label
    )
}

#' Validate a single table
#'
#' Reads a CSV file, validates columns and data according to the specified
#' rules, and returns validation results.
#'
#' @param data_dir Directory containing CSV files
#' @param table Name of the table to validate (without .csv extension)
#' @param columns Data frame with column definitions for this table
#' @param reference_tables Named list of reference table data frames
#' @param manual_rules Optional data frame with custom validation rules
#' @param ncei Logical, whether to apply NCEI-specific rules
#'
#' @return A list with elements:
#'   \describe{
#'     \item{results}{Validation results object from validate package}
#'     \item{failed}{Tibble of failed validations with details}
#'   }
#'
#' @importFrom readr read_csv cols col_character problems
#' @importFrom dplyr bind_rows select
#' @importFrom logger log_info log_error log_warn
#' @importFrom validate validator confront
#' @export
validate_table <- function(data_dir, table, columns, reference_tables, manual_rules = NULL, ncei = FALSE) {
  tryCatch(
    {
      file_path <- file.path(data_dir, paste0(table, ".csv"))

      # Read just the first row to get column names
      data_head <- read_csv(
        file_path,
        col_types = cols(.default = col_character()),
        n_max = 1
      )
      col_types <- create_col_types(data_head, columns)

      log_info("Loading {table} from {file_path}")
      data <- read_csv(
        file_path,
        col_types = col_types
      )

      read_problems <- problems(data)
      if (nrow(read_problems) > 0) {
        log_error(
          "Found {nrow(read_problems)} problems while reading {file_path}"
        )
        print(read_problems)
        stop("Error reading CSV file")
      }

      log_info("Loaded {nrow(data)} rows and {ncol(data)} columns")

      log_info("Validating columns...")
      failed_cols <- validate_columns(data, columns)
      if (nrow(failed_cols) > 0) {
        log_info("Column validation found {nrow(failed_cols)} issues")
      } else {
        log_info("No column issues found")
      }

      # Prepare derived columns
      log_info("Preparing data...")
      prepared_data <- prepare_data(data, columns)

      # Generate validation rules
      log_info("Generating data validation rules...")
      rules_data <- bind_rows(generate_rules(columns, ncei = ncei), manual_rules)
      log_info("Generated {nrow(rules_data)} validation rules")

      rules <- validator(.data = rules_data)

      # Run validation using validate package
      log_info("Validating data...")
      results <- confront(prepared_data, rules, ref = reference_tables)

      # Extract failed rows
      failed_rows <- extract_failed_rows(results, prepared_data, rules, rules_data)
      if (!is.null(failed_rows) && nrow(failed_rows) > 0) {
        log_info("Data validation Found {nrow(failed_rows)} issues")
      } else {
        log_info("No data validation errors found")
      }

      # Combine column and data validation failures
      failed <- bind_rows(failed_cols, failed_rows)
      if (nrow(failed) > 0) {
        failed$table <- table
        failed <- select(failed, table, row, col, values, error)
      }

      return(
        list(
          results = results,
          failed = failed
        )
      )
    },
    error = function(e) {
      log_error("Uncaught error occurred: {e$message}")
      stop(e)
    }
  )
}

#' Validate submission files
#'
#' Main validation function that validates one or more tables in a data
#' directory. Checks column structure and data values according to
#' Makara database specifications.
#'
#' @param data_dir Directory containing CSV files to validate
#' @param tables Character vector of table names to validate. If NULL,
#'   validates all standard Makara tables.
#' @param ncei Logical, whether to apply NCEI-specific validation rules
#' @param output_file Optional path to save detailed validation results as CSV
#' @param columns Optional custom column definitions (overrides package data)
#' @param reference_tables Optional custom reference tables (overrides package data)
#' @param manual_rules Optional custom validation rules (overrides package data)
#' @param verbose Logical, whether to print detailed logging output
#' @param stop_on_error Logical, whether to stop on first validation error
#'
#' @return Logical value: TRUE if all validations passed, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' # Validate all tables in a directory
#' validate_submission("/path/to/data")
#'
#' # Validate specific tables only
#' validate_submission("/path/to/data", tables = c("deployments", "recordings"))
#'
#' # Save detailed results to CSV
#' validate_submission("/path/to/data", output_file = "validation_results.csv")
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom readr write_csv
#' @importFrom logger log_info log_warn log_error
#' @export
validate_submission <- function(data_dir,
                                tables = NULL,
                                ncei = FALSE,
                                output_file = NULL,
                                columns = NULL,
                                reference_tables = NULL,
                                manual_rules = NULL,
                                verbose = TRUE,
                                stop_on_error = FALSE) {
  results <- list()
  overall_success <- TRUE

  # Load data files
  if (is.null(columns)) {
    columns <- load_column_definitions()
  }
  if (is.null(manual_rules)) {
    manual_rules <- load_manual_rules()
  }
  if (is.null(reference_tables)) {
    reference_tables <- load_reference_tables()
  }

  # Determine which tables to validate
  if (is.null(tables)) {
    tables <- names(columns)
  }

  for (table in tables) {
    file_path <- file.path(data_dir, paste0(table, ".csv"))

    table_columns <- columns[[table]]
    stopifnot(!is.null(table_columns) && nrow(table_columns) > 0)

    table_manual_rules <- NULL
    if (table %in% names(manual_rules)) {
      table_manual_rules <- manual_rules[[table]]
    }

    log_info("=== Processing {table} ===")
    if (file.exists(file_path)) {
      results[[table]] <- validate_table(
        data_dir,
        table,
        table_columns,
        reference_tables,
        manual_rules = table_manual_rules,
        ncei = ncei
      )

      if (stop_on_error && nrow(results[[table]]$failed) > 0) {
        log_error("Stopping on first error as requested")
        overall_success <- FALSE
        break
      }
    } else {
      log_warn("Skipping, file not found: {file_path}")
    }
    log_info(" ")
  }

  # Print summary
  log_info("=== VALIDATION SUMMARY ===")
  for (table in names(results)) {
    result <- results[[table]]$results

    if (all(validate::summary(result)$fails == 0)) {
      log_info("\u2713 {table}: PASSED")
    } else {
      log_info("\u2717 {table}: FAILED")
      overall_success <- FALSE

      # Print detailed error information using validate package
      summary_result <- validate::summary(result)
      failed_rules <- summary_result[summary_result$fails > 0, ]
      for (i in seq_len(nrow(failed_rules))) {
        rule_info <- failed_rules[i, ]
        log_info(
          "  - Rule {rule_info$name} failed: {rule_info$fails} out of {rule_info$items} rows"
        )
        log_info("    Expression: {rule_info$expression}")
      }
    }
  }

  if (!is.null(output_file)) {
    log_info("Writing detailed results to {output_file}")
    all_failed <- bind_rows(
      lapply(names(results), function(table) {
        results[[table]]$failed
      })
    )
    write_csv(all_failed, output_file, na = "")
  }

  return(overall_success)
}
