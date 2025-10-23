#' Generate validation rules for required fields
#'
#' Creates rules to check that required fields are not missing (NA).
#' Handles both unconditionally required fields and conditionally required fields.
#'
#' @param columns Data frame with column definitions
#' @param ncei Logical, whether to include NCEI-specific required field rules
#'
#' @return Data frame with columns: rule, name, label
#'
#' @importFrom dplyr filter transmute bind_rows
#' @importFrom glue glue
#' @export
generate_required_rules <- function(columns, ncei = FALSE) {
  rules_always_required_is_complete <- columns |>
    filter(required, is.na(required_unless)) |>
    transmute(
      rule = glue("!is.na({name})"),
      label = glue("'{name}' has missing values"),
      name = glue("is_complete.{name}"),
    )
  rules_conditionally_required_is_complete <- columns |>
    filter(required, !is.na(required_unless)) |>
    transmute(
      rule = glue("{required_unless} | !is.na({name})"),
      label = glue("'{name}' has missing values"),
      name = glue("is_complete.{name}"),
    )
  rules_ncei <- NULL
  if (ncei) {
    rules_ncei_required_is_complete <- columns |>
      filter(required, ncei) |>
      transmute(
        rule = glue("!is.na({name})"),
        label = glue("'{name}' has missing values for NCEI"),
        name = glue("is_complete_ncei.{name}"),
      )
    rules_ncei <- bind_rows(
      rules_ncei_required_is_complete
    )
  }

  bind_rows(
    rules_always_required_is_complete,
    rules_conditionally_required_is_complete,
    rules_ncei
  )
}

#' Generate validation rules for data formats
#'
#' Creates rules to check that fields match expected format patterns
#' (e.g., date, timestamp, code, URL, JSON).
#'
#' @param columns Data frame with column definitions
#'
#' @return Data frame with columns: rule, name, label
#'
#' @importFrom dplyr filter transmute case_when
#' @importFrom glue glue
#' @export
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

#' Generate validation rules for numeric ranges
#'
#' Creates rules to check that numeric fields (integer or float) fall
#' within specified min/max ranges.
#'
#' @param columns Data frame with column definitions
#'
#' @return Data frame with columns: rule, name, label
#'
#' @importFrom dplyr filter transmute coalesce
#' @importFrom glue glue
#' @export
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

#' Generate validation rules for uniqueness constraints
#'
#' Creates rules to check that fields are unique within specified groupings.
#'
#' @param columns Data frame with column definitions
#'
#' @return Data frame with columns: rule, name, label
#'
#' @importFrom dplyr filter transmute
#' @importFrom glue glue
#' @export
generate_unique_rules <- function(columns) {
  columns |>
    filter(!is.na(unique_by)) |>
    transmute(
      rule = glue("is_unique({name}, by = paste0({unique_by}))"),
      label = glue("'{name}' is not unique within '{unique_by}'"),
      name = glue("is_unique.{name}"),
    )
}

#' Generate validation rules for timestamps
#'
#' Creates rules to check that timestamps are:
#' - Not in the future
#' - Start timestamps are before corresponding end timestamps
#'
#' @param columns Data frame with column definitions
#'
#' @return Data frame with columns: rule, name, label
#'
#' @importFrom dplyr filter transmute select bind_rows
#' @importFrom glue glue
#' @importFrom lubridate ymd_hms now
#' @export
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

#' Generate validation rules for dates
#'
#' Creates rules to check that dates are:
#' - Not in the future
#' - After the minimum valid date
#' - Start dates are before corresponding end dates
#'
#' @param columns Data frame with column definitions
#'
#' @return Data frame with columns: rule, name, label
#'
#' @importFrom dplyr filter transmute select bind_rows
#' @importFrom glue glue
#' @importFrom lubridate ymd now
#' @export
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

#' Generate validation rules for frequency ranges
#'
#' Creates rules to check that minimum frequency values are less than or
#' equal to corresponding maximum frequency values.
#'
#' @param columns Data frame with column definitions
#'
#' @return Data frame with columns: rule, name, label
#'
#' @importFrom dplyr filter transmute select
#' @importFrom glue glue
#' @export
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

#' Generate validation rules for reference table lookups
#'
#' Creates rules to check that code and code_list fields contain only
#' values that exist in specified reference tables.
#'
#' @param columns Data frame with column definitions
#'
#' @return Data frame with columns: rule, name, label
#'
#' @importFrom dplyr filter transmute bind_rows
#' @importFrom glue glue
#' @export
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

#' Generate all validation rules
#'
#' Main function that calls all rule generators and combines their results
#' into a single data frame of validation rules.
#'
#' @param columns Data frame with column definitions
#' @param ncei Logical, whether to include NCEI-specific rules
#'
#' @return Data frame with columns: rule, name, label
#'
#' @importFrom dplyr bind_rows
#' @export
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
