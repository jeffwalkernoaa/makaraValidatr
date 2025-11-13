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
#' @param columns Data frame with column definitions for the table
#' @param ref Named list of reference table data frames and formats
#'
#' @return A tibble with columns: row, col, value, rule, error
#'
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr filter mutate select transmute left_join
#' @importFrom purrr map map_chr
#' @importFrom tidyr unnest
#' @export
extract_data_errors <- function(results, prepared_data, rules, rules_data, columns, ref) {
  if (all(validate::summary(results)$fails == 0)) {
    return(NULL)
  }

  validate::summary(results) |>
    as_tibble() |>
    filter(!error, fails > 0) |>
    mutate(
      rule = map(name, function (name) {
        rule <- rules_data[rules_data$name == name, ]
        if (nrow(rule) > 1) {
          stop("Multiple rules found with the same name: ", name)
        } else if (nrow(rule) == 0) {
          stop("No rule found with name: ", name)
        }
        as.list(rule)
      }),
      violating = map(rule, function(rule) {
        error <- rule$label
        rows <- validate::violating(prepared_data, rules[rule$name], ref = ref)
        cols <- intersect(validate::variables(rules[rule$name]), colnames(prepared_data))
        if (length(cols) == 1 && stringr::str_ends(cols, "_valid_code_list")) {
          col <- stringr::str_remove(cols, "_valid_code_list")
          reference_table <- columns$reference_table[columns$name == col]
          valid_codes <- ref[[reference_table]][["code"]]

          violating_rows <- rows |>
            mutate(
              value = map_chr(row, function(row) {
                prepared_data[[col]][[row]]
              }),
              invalid_values = map(value, function(value) {
                if (is.na(value)) return(NA)
                value_split <- trimws(unlist(strsplit(value, ",")))
                invalid_values <- value_split[!value_split %in% valid_codes]
                if (length(invalid_values) == 0) {
                  return(NA)
                }
                paste(invalid_values, collapse = ",")
              }),
              col = col,
              error = glue("'{col}' contains unknown codes ('{invalid_values}') for reference table '{reference_table}'")
            ) |>
            transmute(
              row,
              col,
              value = glue("'{value}'"),
              rule = rule$name,
              error
            )
          return(violating_rows)
        } else if (length(cols) == 1 && stringr::str_ends(cols, "_valid_json")) {
          col <- stringr::str_remove(cols, "_valid_json")
          violating_rows <- rows |>
            mutate(
              value = map_chr(row, function(row) {
                prepared_data[[col]][[row]]
              })
            ) |>
            transmute(
              row,
              col = col,
              value = glue("'{value}'"),
              rule = rule$name,
              error = error
            )
          return(violating_rows)
        }
        rows |>
          mutate(
            col = paste0(cols, collapse = ","),
            value = map_chr(row, function(row) {
              if (length(cols) > 1) {
                pairs <- map_chr(cols, function(col) {
                  value <- prepared_data[[col]][[row]]
                  if (is.na(value)) return(paste0(col, "=NA"))
                  paste0(col, "='", value, "'")
                })
                return(paste0(pairs, collapse = ", "))
              }
              value <- prepared_data[[cols]][[row]]
              return(paste0("'", value, "'"))
            }),
            rule = rule$name,
            error = stringr::str_replace(error, 'unknown code', glue("unknown code {value}"))
          ) |>
          select(row, col, value, rule, error)

      }),
    ) |>
    select(violating) |> 
    unnest(violating) |>
    transmute(
      row,
      col,
      value,
      rule,
      error
    )
}

#' Extract errors from validation results
#'
#' Processes validation results to extract any errors thrown by validator
#'
#' @param results Validation results object from \code{validate::confront()}
#'
#' @return A tibble with columns: rule, error
#'
#' @importFrom tidyr unnest
#' @importFrom tibble enframe
#' @export
extract_validation_errors <- function(results) {
  validate::errors(results) |>
    enframe(name = "rule", value = "error") |>
    unnest(error)
}

#' Extract parse errors from data reading
#'
#' Processes readr parsing problems to create a structured tibble of parse errors
#'
#' @param data Data frame that was read with readr
#' @param columns Data frame with column definitions for this table
#'
#' @return A tibble with columns: row, col, value, rule, error
#'
#' @export
extract_parse_errors <- function(data, columns) {
  read_problems <- readr::problems(data)

  if (nrow(read_problems) == 0) {
    return(tibble::tibble(
      row = integer(0),
      col = character(0),
      value = character(0),
      rule = character(0),
      error = character(0)
    ))
  }

  tibble::tibble(
    name = colnames(data),
    col = 1:ncol(data)
  ) |>
    dplyr::inner_join(read_problems, by = c("col")) |>
    dplyr::left_join(
      dplyr::select(columns, name, type),
      by = "name"
    ) |>
    dplyr::transmute(
      row = row,
      col = name,
      value = glue::glue("'{actual}'"),
      rule = glue::glue("parse.{col}"),
      error = glue::glue("'{col}' failed to parse as '{type}'")
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
#' @importFrom dplyr bind_rows select arrange count
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
        na = "",
        n_max = 1
      )
      col_types <- create_col_types(data_head, columns)

      log_info("Loading {table} from {file_path}")
      data <- read_csv(
        file_path,
        col_types = col_types,
        na = ""
      )

      file_errors <- NULL
      if (any(data == "NA", na.rm = TRUE)) {
        log_error("Found literal 'NA' strings in CSV file, missing values should be empty instead")
        file_errors <- tibble(
          row = NA_integer_,
          col = NA_character_,
          value = NA_character_,
          rule = "na_string",
          error = "file contains literal 'NA' strings (missing values should be empty instead)"
        )
      }

      log_info("Checking for parsing errors...")
      parse_errors <- extract_parse_errors(data, columns)
      if (nrow(parse_errors) > 0) {
        log_error(
          "Found {nrow(parse_errors)} parsing errors"
        )
      } else {
        log_info("No parsing errors found")
      }

      log_info("Loaded {nrow(data)} rows and {ncol(data)} columns")

      log_info("Validating columns...")
      column_errors <- validate_columns(data, columns)
      if (nrow(column_errors) > 0) {
        log_error("Found {nrow(column_errors)} column errors")
      } else {
        log_info("No column errors found")
      }

      # Prepare derived columns
      log_info("Preparing data for validation...")
      prepared_data <- prepare_data(data, columns, reference_tables)

      # Generate validation rules
      log_info("Generating validation rules...")
      rules_data <- bind_rows(generate_rules(columns, ncei = ncei), manual_rules)

      rules <- validator(.data = rules_data)

      # Run validation using validate package
      log_info("Validating data...")
      ref <- as.list(reference_tables)
      ref[["formats"]] <- formats
      results <- confront(prepared_data, rules, ref = ref)

      validation_errors <- extract_validation_errors(results)
      if (nrow(validation_errors) > 0) { 
        log_warn("Validation warnings (can usually be safely ignored):")
        for (i in seq_len(nrow(validation_errors))) {
          log_warn("  - {validation_errors$error[i]} (rule={validation_errors$rule[i]})")
          if (i > 5) {
            log_warn("  - ...and {nrow(validation_errors) - 5} more")
            break
          }
        }
        # stop("Unexpected errors occurred during validation")
      }

      # Extract failed rows
      data_errors <- extract_data_errors(results, prepared_data, rules, rules_data, columns, ref)
      if (!is.null(data_errors) && nrow(data_errors) > 0) {
        log_warn("Found {nrow(data_errors)} data validation errors")
      } else {
        log_info("No data validation errors found")
      }

      # Combine column and data validation failures
      errors <- bind_rows(
        file = file_errors,
        column = column_errors,
        parse = parse_errors,
        data = data_errors,
        .id = "type"
      ) |> 
        mutate(
          type = factor(type, levels = c("file", "column", "parse", "data"))
        ) |> 
        arrange(type, rule, row)

      if (nrow(errors) > 0) {
        errors$table <- table
        errors <- select(errors, table, type, row, col, value, rule, error)
      }

      errors
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
#' @param output_file Optional filename or path to save detailed validation results as CSV
#' @param columns Optional custom column definitions (overrides package data)
#' @param reference_tables_file Optional path to reference tables CSV file.
#'   If provided, loads reference tables from this file instead of package data.
#' @param reference_tables Optional custom reference tables (overrides package data).
#'   If both reference_tables_file and reference_tables are provided, reference_tables takes precedence.
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
#'
#' # Use custom reference tables from a downloaded file
#' validate_submission("/path/to/data", reference_tables_file = "reference_tables.csv")
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom readr write_csv
#' @importFrom logger log_info log_warn log_error log_threshold
#' @export
validate_submission <- function(data_dir,
                                tables = NULL,
                                ncei = FALSE,
                                output_file = "validation-errors.csv",
                                columns = NULL,
                                reference_tables_file = NULL,
                                reference_tables = NULL,
                                manual_rules = NULL,
                                verbose = TRUE,
                                stop_on_error = FALSE) {
  results <- list()
  overall_success <- TRUE
  
  # Turn off logger if verbose=FALSE
  if(isFALSE(verbose)) {
    old_thresh <- log_threshold()
    on.exit(log_threshold(old_thresh))
    log_threshold(level='OFF')
  }
  
  # Load data files
  if (is.null(columns)) {
    columns <- load_column_definitions()
  }
  if (is.null(manual_rules)) {
    manual_rules <- load_manual_rules()
  }
  if (is.null(reference_tables)) {
    if (!is.null(reference_tables_file)) {
      log_info("Loading reference tables from: {reference_tables_file}")
      reference_tables <- load_reference_tables(file = reference_tables_file)
    } else {
      reference_tables <- load_reference_tables()
    }
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

    log_info("=== Validating {table} ===")
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
      log_info("Skipping, file not found: {file_path}")
    }
    log_info(" ")
  }

  # Print summary
  log_info("=== VALIDATION SUMMARY ===")
  for (table in names(results)) {
    errors <- results[[table]]

    if (is.null(errors) || nrow(errors) == 0) {
      log_info("\u2713 {table}: PASSED")
    } else {
      log_info("\u2717 {table}: FAILED")

      error_counts <- count(errors, type, rule, error)

      for (i in seq_len(nrow(error_counts))) {
        error <- error_counts[i, ]
        log_info(
          "  - {error$error} (n={error$n}, rule={error$type}.{error$rule})"
        )
      }
    }
  }

  all_errors <- bind_rows(results)
  if (!is.null(output_file)) {
    if (output_file == basename((output_file))) {
      output_file <- file.path(data_dir, output_file)
    }
    log_info(" ")
    log_info("Writing validation errors to '{output_file}'")

    write_csv(all_errors, output_file, na = "")
  }

  return(is.null(all_errors) || nrow(all_errors) == 0)
}
