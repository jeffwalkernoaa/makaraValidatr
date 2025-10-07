#!/usr/bin/env Rscript

# Submission Validation Script for Makara Database
# Validates CSV files using validate package
# Usage: Rscript validate_submission.R /path/to/templates/directory

# Load required libraries
suppressMessages({
  library(validate)
  library(readr)
  library(tibble)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(logger)
  library(lubridate)
  library(glue)
  library(jsonlite)
})

ALL_TABLES <- c(
  "projects",
  "sites",
  "devices",
  "deployments",
  "recordings",
  "recording_intervals",
  "analyses",
  "detections",
  "tracks",
  "track_positions"
)

MIN_DATE <- "1980-01-01"

# validation rules -------------------------------------------------------

formats <- c(
  code = "^[A-Z0-9_-]+$",
  code_list = "^[A-Z0-9_,-]+$",
  timestamp = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?([+-]\\d{4}(:\\d{2})?|Z)$",
  date = "^\\d{4}-\\d{2}-\\d{2}$",
  timezone = "^UTC([+-]([0-9]|0[0-9]|1[0-4]):?([0-5][0-9])?)?$",
  integer = "^-?\\d+$",
  float = "^-?\\d*(\\.\\d+)?$",
  bool = "^(TRUE|FALSE)$",
  url = "^https?://",
  uri = "^[a-zA-Z][a-zA-Z0-9+.-]*://"
)

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


# column functions -------------------------------------------------------

validate_columns <- function(data, columns) {
  all_cols <- columns[["name"]]
  required_cols <- columns[["name"]][
    columns$required & is.na(columns$required_condition)
  ]
  optional_cols <- setdiff(all_cols, required_cols)

  missing_required_cols <- tibble(
    col = setdiff(required_cols, colnames(data)),
    error = glue("Missing required column '{col}'"),
    rule = glue("required.{col}")
  )
  if (nrow(missing_required_cols) > 0) {
    log_error(
      "Missing required columns: {paste(missing_required_cols$col, collapse = ', ')}"
    )
  }

  unexpected_cols <- tibble(
    col = setdiff(colnames(data), all_cols),
    error = glue("Found unknown column '{col}'"),
    rule = glue("unknown_column")
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
      values = NA_character_,
      error
      # rule,
      # expression = NA_character_
    )
}

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


# prepare dataset --------------------------------------------------------

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


# load files -------------------------------------------------------------

load_column_definitions <- function(file = "column_definitions.csv") {
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
# columns <- load_column_definitions()

load_reference_tables <- function(file = "reference_tables.csv") {
  x <- read_csv(
    file,
    col_types = cols(
      .default = col_character()
    )
  ) |>
    nest_by(table) |>
    deframe()
}
# reference_tables <- load_reference_tables()

load_manual_rules <- function(file = "manual_rules.csv") {
  read_csv(
    file,
    col_types = cols(
      .default = col_character()
    )
  ) |> 
    nest_by(table) |>
    deframe()
}
# all_manual_rules <- load_manual_rules()

# rule generators --------------------------------------------------------

generate_required_rules <- function(columns, ncei = FALSE) {
  # rules_always_required_is_not_missing <- columns |> 
  #   filter(required, is.na(required_condition)) |>
  #   transmute(
  #     rule = glue("'{name}' %in% colnames(.)"),
  #     name = glue("col_exists.{name}"),
  #     label = glue("'{name}' is missing")
  #   )
  rules_always_required_is_complete <- columns |> 
    filter(required, is.na(required_condition)) |>
    transmute(
      rule = glue("!is.na({name})"),
      name = glue("is_complete.{name}"),
      label = glue("'{name}' has missing values"),
    )
  rules_conditionally_required_is_complete <- columns |> 
    filter(required, !is.na(required_condition)) |>
    transmute(
      rule = glue("{required_condition} & !is.na({name})"),
      label = glue("'{name}' has missing values"),
      name = glue("is_complete.{name}"),
    )
  rules_ncei <- NULL
  if (ncei) {
    # rules_ncei_required_is_not_missing <- columns |> 
    #   filter(required, ncei) |>
    #   transmute(
    #     rule = glue("'{name}' %in% colnames(.)"),
    #     name = glue("col_exists_ncei.{name}"),
    #     label = glue("'{name}' is missing for NCEI")
    #   )
    rules_ncei_required_is_complete <- columns |> 
      filter(required, ncei) |>
      transmute(
        rule = glue("!is.na({name})"),
        label = glue("'{name}' has missing values for NCEI"),
        name = glue("is_complete_ncei.{name}"),
      )
    rules_ncei <- bind_rows(
      # rules_ncei_required_is_not_missing,
      rules_ncei_required_is_complete
    )
  }

  bind_rows(
    # rules_always_required_is_not_missing,
    rules_always_required_is_complete,
    rules_conditionally_required_is_complete,
    rules_ncei
  )
}
# generate_required_rules(all_columns$recordings)
# generate_required_rules(all_columns$recordings, ncei = TRUE)

generate_format_rules <- function(columns) {
  columns |> 
    filter(type %in% c(names(formats), "json")) |>
    transmute(
      rule = case_when(
        type %in% names(formats) ~ glue(
          "if (!is.na({name})) grepl(formats[[\"{type}\"]],{name})"
        ),
        type == "json" ~ glue("{name}_is_json_format == TRUE"),
        TRUE ~ NA_character_
      ),
      label = glue("'{name}' is not valid '{type}' format"),
      name = glue("format.{name}"),
    )
}
# generate_format_rules(all_columns$deployments)

generate_numeric_range_rules <- function(columns) {
  columns |> 
    filter(type %in% c("integer", "float")) |>
    filter(!is.na(min) | !is.na(max)) |>
    transmute(
      rule = glue(
        "is.na({name}) | in_range({name}, {coalesce(min, -Inf)}, {coalesce(max, Inf)})"
      ),
      label = glue("'{name}' is not within the valid range [{coalesce(min, -Inf)}, {coalesce(max, Inf)}]"),
      name = glue("in_range.{name}"),
    )
}
# generate_numeric_range_rules(all_columns$deployments)

generate_unique_rules <- function(columns) {
  columns |> 
    filter(!is.na(unique_by)) |>
    transmute(
      rule = glue("is_unique({name}, by = paste0({unique_by}))"),
      label = glue("'{name}' is not unique within '{unique_by}'"),
      name = glue("is_unique.{name}"),
    )
}
# generate_unique_rules(all_columns$recordings)

generate_timestamp_rules <- function(columns) {
  rules_before_now <- columns |> 
    filter(type == "timestamp") |>
    transmute(
      rule = glue("is.na({name}) | ymd_hms({name}, quiet = TRUE) <= now()"),
      label = glue("'{name}' is after the current date/time"),
      name = glue("before_now.{name}"),
    )
  
  rules_start_before_end_datetime <- columns |> 
    filter(type == "timestamp", grepl("_start_datetime", name)) |> 
    transmute(
      end_col = sub("_start_datetime", "_end_datetime", name),
      rule = glue("is.na({name}) | is.na({end_col}) | ymd_hms({name}, quiet = TRUE) <= ymd_hms({end_col}, quiet = TRUE)"),
      label = glue("'{name}' is after corresponding end datetime field '{end_col}'"),
      name = glue("start_before_end_datetime.{name}"),
    ) |>
    select(-end_col)

  bind_rows(
    rules_before_now,
    rules_start_before_end_datetime
  )
}
# generate_timestamp_rules(all_columns$recordings)

generate_date_rules <- function(columns) {
  rules_before_today <- columns |> 
    filter(type == "date") |>
    transmute(
      rule = glue("is.na({name}) | ymd({name}, quiet = TRUE) <= as.Date(now())"),
      label = glue("'{name}' is after today's date"),
      name = glue("before_today.{name}"),
    )
  
  rules_after_min_date <- columns |> 
    filter(type == "date") |>
    transmute(
      rule = glue("is.na({name}) | ymd({name}, quiet = TRUE) >= ymd('{MIN_DATE}')"),
      label = glue("'{name}' is after minimum valid date '{MIN_DATE}'"),
      name = glue("{name}_is_after_min_date"),
    )
  
  rules_start_before_end_date <- columns |>
    filter(type == "date", grepl("_start_date", name)) |> 
    transmute(
      end_col = sub("_start_date", "_end_date", name),
      rule = glue("is.na({name}) | is.na({end_col}) | ymd({name}, quiet = TRUE) <= ymd({end_col}, quiet = TRUE)"),
      label = glue("'{name}' is after corresponding end date field '{end_col}'"),
      name = glue("start_before_end_date.{name}"),
    ) |>
    select(-end_col)
  
  bind_rows(
    rules_before_today,
    rules_after_min_date,
    rules_start_before_end_date
  )
}
# generate_date_rules(all_columns$devices)

generate_frequency_range_rules <- function(columns) {
  columns |> 
    filter(grepl("min_frequency", name)) |>
    transmute(
      max_col = sub("min_frequency", "max_frequency", name),
      rule = glue(
        "is.na({name}) | is.na({max_col}) | {name} <= {max_col}"
      ),
      label = glue("'{name}' is greater than corresponding max frequency field '{max_col}'"),
      name = glue("min_less_than_max_freq.{name}"),
    ) |> 
    select(-max_col)
}
# generate_frequency_range_rules(all_columns$analyses)

generate_reference_table_rules <- function(columns) {
  rules_code_list <- columns |> 
    filter(type == "code_list", !is.na(reference_table)) |>
    transmute(
      rule = glue(
        "is.na({name}) | all(trimws(unique(unlist(strsplit({name}, split = ',', fixed = TRUE)))) %in% {reference_table})"
      ),
      label = glue("'{name}' has unknown codes for reference table '{reference_table}'"),
      name = glue("ref.{name}"),
    )
  
  rules_codes <- columns |> 
    filter(type == "code", !is.na(reference_table)) |>
    transmute(
      rule = glue(
        "is.na({name}) | {name} %in% {reference_table}"
      ),
      label = glue("'{name}' has unknown code for reference table '{reference_table}'"),
      name = glue("ref.{name}"),
    )
  
  bind_rows(
    rules_code_list,
    rules_codes
  )
}
# generate_reference_table_rules(all_columns$analyses)

generate_rules <- function(columns, ncei = FALSE) {
  bind_rows(
    generate_required_rules(columns, ncei),
    generate_format_rules(columns),
    generate_unique_rules(columns),
    generate_reference_table_rules(columns),
    generate_timestamp_rules(columns),
    generate_date_rules(columns),
    generate_numeric_range_rules(columns),
    generate_frequency_range_rules(columns)
  )
}
# generate_rules(all_columns$recordings)


# validate table ---------------------------------------------------------

extract_failed_rows <- function (results, prepared_data, rules, rules_data) {
  if (all(summary(results)$fails == 0)) {
    return(NULL)
  }

  summary(results) |> 
    as_tibble() |> 
    filter(!error, fails > 0) |> 
    mutate(
      violating = map(name, function (name) {
        rows <- violating(prepared_data, rules[name])
        vars <- intersect(variables(rules[name]), colnames(prepared_data))

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
      # rule = name,
      # expression = expression
    )
}

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
      results <- confront(prepared_data, rules, ref = reference_tables) # na.value = TRUE
      browser()
      # Extract failed rows
      failed_rows <- extract_failed_rows(results, prepared_data, rules, rules_data)
      if (!is.null(failed_rows) && nrow(failed_rows) > 0) {
        log_info("Data validation Found {nrow(failed_rows)} issues")
      } else {
        log_info("No data validation errors found")
      }

      # Dropped error check because rule has error when column not found (even if optional)
      # if (any(summary(results)$error)) {
      #   log_error("Unexpected errors occurred during validation")
      #   print(errors(results))
      #   stop("Validation errors")
      # }

      # if (any(summary(results)$fails > 0)) {
      #   log_error(
      #     "Validation failed for {table}: {sum(summary(results)$fails > 0)} failed rule(s)"
      #   )
      # } else {
      #   log_info("Validation passed for {table}")
      # }

      # Combine column and data validation failures
      failed <- bind_rows(failed_cols, failed_rows)
      if (nrow(failed) > 0) {
        failed$table <- table
        failed <- select(failed, table, row, col, values, error)
      }

      # TODO: add separate rules for block checks (required columns exist)
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

validate_submission <- function(data_dir, tables = NULL, ncei = FALSE, output_file = NULL) {
  results <- list()
  overall_success <- TRUE

  all_columns <- load_column_definitions()
  all_manual_rules <- load_manual_rules()
  reference_tables <- load_reference_tables()

  if (is.null(tables)) {
    tables <- ALL_TABLES
  }

  for (table in tables) {
    file_path <- file.path(data_dir, paste0(table, ".csv"))

    columns <- all_columns[[table]]
    stopifnot(!is.null(columns) && nrow(columns) > 0)

    manual_rules <- NULL
    if (table %in% names(manual_rules)) {
      manual_rules <- all_manual_rules[[table]]
    }

    log_info("=== Processing {table} ===")
    if (file.exists(file_path)) {
      results[[table]] <- validate_table(
        data_dir,
        table,
        columns,
        reference_tables,
        manual_rules = manual_rules,
        ncei = ncei
      )
    } else {
      log_warn("Skipping, file not found: {file_path}")
    }
    log_info(" ")
  }

  # Print summary
  log_info("=== VALIDATION SUMMARY ===")
  for (table in names(results)) {
    result <- results[[table]]$results

    if (all(summary(result)$fails == 0)) {
      log_info("✓ {table}: PASSED")
    } else {
      log_info("✗ {table}: FAILED")
      overall_success <- FALSE

      # Print detailed error information using validate package
      summary_result <- summary(result)
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


# run --------------------------------------------------------------------

options(warn = 1) # Print warnings as they occur

library(optparse)

# Define command line options
option_list <- list(
  make_option(c("-n", "--ncei"), 
              action = "store_true", 
              default = FALSE,
              help = "Include NCEI validation rules"),
  make_option(c("-t", "--tables"), 
              type = "character", 
              default = NULL,
              help = "Comma-separated list of specific tables to validate (default: all tables)",
              metavar = "table1,table2,..."),
  make_option(c("-o", "--output"), 
              type = "character", 
              default = NULL,
              help = "Output file to save validation results",
              metavar = "output_file")
)

# Create parser
parser <- OptionParser(
  usage = "%prog [options] data_directory",
  description = "Validates CSV files for Makara Database using validate package",
  option_list = option_list
)

# Parse arguments
args <- parse_args(parser, positional_arguments = 1)

# Check if positional argument was provided
if (length(args$args) != 1) {
  print_help(parser)
  cat("\nError: Please provide the data directory path\n")
  quit(status = 1)
}

data_dir <- args$args[1]

if (!dir.exists(data_dir)) {
  cat("Error: Directory does not exist:", data_dir, "\n")
  quit(status = 1)
}

# Parse tables if specified
tables_to_validate <- NULL
if (!is.null(args$options$tables)) {
  tables_to_validate <- trimws(strsplit(args$options$tables, ",")[[1]])
}

# Update the validate_submission call to use the parsed options
success <- validate_submission(
  data_dir,
  tables = tables_to_validate,
  ncei = args$options$ncei,
  output_file = args$options$output
)

if (success) {
  log_info("✓ All validations passed!")
  quit(status = 0)
} else {
  log_error("✗ Some validations failed. Please check the output above.")
  quit(status = 1)
}
