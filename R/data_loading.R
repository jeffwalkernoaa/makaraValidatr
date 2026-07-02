#' Load column definitions
#'
#' Loads and parses column definitions from the NOAA column definition CSV
#' endpoint or a local CSV file. Returns a nested list by table name.
#'
#' @param file Optional path to a local column definitions CSV file. If `NULL`,
#'   column definitions are fetched from `url`.
#' @param url URL for the column definitions CSV endpoint.
#' @param cache Logical, whether to cache the fetched CSV and use the cache
#'   as a fallback if the remote endpoint cannot be reached.
#' @param cache_file Optional path for the cached CSV file. If `NULL`, uses
#'   the package cache directory.
#'
#' @return A named list of data frames, one per table, containing column
#'   definitions
#'
#' @importFrom readr read_csv cols col_character col_integer col_double col_logical
#' @importFrom dplyr mutate across coalesce nest_by
#' @importFrom tibble deframe
#' @export
load_column_definitions <- function(file = NULL,
                                    url = COLUMN_DEFINITIONS_URL,
                                    cache = TRUE,
                                    cache_file = NULL) {
  if (!is.null(file)) {
    return(.load_column_definitions_csv(file))
  }

  if (is.null(cache_file)) {
    cache_file <- .column_definitions_cache_file()
  }

  remote_definitions <- tryCatch(
    {
      fetched <- .read_resource_text(url)
      column_definitions <- .parse_column_definitions_csv(fetched, source = url)
      if (isTRUE(cache)) {
        .write_resource_cache(fetched, cache_file)
      }
      column_definitions
    },
    error = function(e) e
  )

  if (!inherits(remote_definitions, "error")) {
    return(remote_definitions)
  }

  if (isTRUE(cache) && file.exists(cache_file)) {
    warning(
      "Could not load column definitions from ",
      url,
      ": ",
      conditionMessage(remote_definitions),
      ". Using cached column definitions from ",
      cache_file,
      ".",
      call. = FALSE
    )
    cached <- .read_resource_text(cache_file)
    return(.parse_column_definitions_csv(cached, source = cache_file))
  }

  stop(
    "Could not load column definitions from ",
    url,
    ": ",
    conditionMessage(remote_definitions),
    ". No cached column definitions were available at ",
    cache_file,
    ".",
    call. = FALSE
  )
}

.load_column_definitions_csv <- function(file) {
  .parse_column_definitions_csv(file, source = file, literal = FALSE)
}

.parse_column_definitions_csv <- function(csv, source = "column definitions CSV", literal = TRUE) {
  input <- if (isTRUE(literal)) I(csv) else csv

  parsed <- tryCatch(
    read_csv(
      input,
      col_types = .column_definitions_col_types()
    ),
    error = function(e) {
      stop(
        "Failed to parse column definitions CSV from ",
        source,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  required_columns <- c(
    "index",
    "table",
    "name",
    "type",
    "default",
    "min",
    "max",
    "unique_by",
    "required",
    "required_unless",
    "required_ncei",
    "reference_table",
    "notes"
  )
  missing_columns <- setdiff(required_columns, names(parsed))
  if (length(missing_columns) > 0) {
    stop(
      "Column definitions CSV from ",
      source,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (any(is.na(parsed$table)) || any(!nzchar(parsed$table))) {
    stop(
      "Column definitions CSV from ",
      source,
      " contains missing or empty table names.",
      call. = FALSE
    )
  }

  parsed |>
    mutate(
      across(where(is.logical), \(x) coalesce(x, FALSE))
    ) |>
    nest_by(table) |>
    deframe()
}

.column_definitions_col_types <- function() {
  cols(
    .default = col_character(),
    index = col_integer(),
    min = col_double(),
    required = col_logical(),
    required_ncei = col_logical(),
    max = col_double()
  )
}

.column_definitions_cache_file <- function() {
  file.path(
    tools::R_user_dir("makaraValidatr", "cache"),
    "column_definitions.csv"
  )
}

#' Load reference tables
#'
#' Loads reference lookup tables from the NOAA reference table JSON endpoint,
#' a local JSON file, or a local CSV file. Returns a nested list by table name.
#'
#' @param file Optional path to a local reference tables CSV or JSON file.
#'   If `NULL`, reference tables are fetched from `url`.
#' @param url URL for the reference tables JSON endpoint.
#' @param cache Logical, whether to cache the fetched JSON and use the cache
#'   as a fallback if the remote endpoint cannot be reached.
#' @param cache_file Optional path for the cached JSON file. If `NULL`, uses
#'   the package cache directory.
#'
#' @return A named list of data frames, one per reference table, containing
#'   valid codes and names
#'
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr nest_by
#' @importFrom tibble deframe
#' @importFrom jsonlite fromJSON
#' @export
load_reference_tables <- function(file = NULL,
                                  url = REFERENCE_TABLES_URL,
                                  cache = TRUE,
                                  cache_file = NULL) {
  if (!is.null(file)) {
    return(.load_reference_tables_file(file))
  }

  if (is.null(cache_file)) {
    cache_file <- .reference_tables_cache_file()
  }

  remote_tables <- tryCatch(
    {
      fetched <- .read_resource_text(url)
      reference_tables <- .parse_reference_tables_json(fetched, source = url)
      if (isTRUE(cache)) {
        .write_resource_cache(fetched, cache_file)
      }
      reference_tables
    },
    error = function(e) e
  )

  if (!inherits(remote_tables, "error")) {
    return(remote_tables)
  }

  if (isTRUE(cache) && file.exists(cache_file)) {
    warning(
      "Could not load reference tables from ",
      url,
      ": ",
      conditionMessage(remote_tables),
      ". Using cached reference tables from ",
      cache_file,
      ".",
      call. = FALSE
    )
    cached <- .read_resource_text(cache_file)
    return(.parse_reference_tables_json(cached, source = cache_file))
  }

  stop(
    "Could not load reference tables from ",
    url,
    ": ",
    conditionMessage(remote_tables),
    ". No cached reference tables were available at ",
    cache_file,
    ".",
    call. = FALSE
  )
}

.load_reference_tables_file <- function(file) {
  ext <- tolower(tools::file_ext(file))

  if (identical(ext, "json")) {
    json_text <- .read_resource_text(file)
    return(.parse_reference_tables_json(json_text, source = file))
  }

  .load_reference_tables_csv(file)
}

.load_reference_tables_csv <- function(file) {
  read_csv(
    file,
    col_types = cols(
      .default = col_character()
    )
  ) |>
    nest_by(table) |>
    deframe()
}

.parse_reference_tables_json <- function(json_text, source = "reference tables JSON") {
  parsed <- tryCatch(
    fromJSON(json_text, simplifyDataFrame = TRUE),
    error = function(e) {
      stop(
        "Failed to parse reference tables JSON from ",
        source,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (!is.data.frame(parsed) || !all(c("table", "data") %in% names(parsed))) {
    stop(
      "Reference tables JSON from ",
      source,
      " must be an array of objects with 'table' and 'data' fields.",
      call. = FALSE
    )
  }

  table_names <- parsed$table
  if (!is.character(table_names) || any(is.na(table_names)) || any(!nzchar(table_names))) {
    stop(
      "Reference tables JSON from ",
      source,
      " contains missing or empty table names.",
      call. = FALSE
    )
  }

  duplicates <- unique(table_names[duplicated(table_names)])
  if (length(duplicates) > 0) {
    stop(
      "Reference tables JSON from ",
      source,
      " contains duplicate table names: ",
      paste(duplicates, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  tables <- parsed$data
  if (!is.list(tables) || length(tables) != length(table_names)) {
    stop(
      "Reference tables JSON from ",
      source,
      " has invalid 'data' values.",
      call. = FALSE
    )
  }

  for (i in seq_along(tables)) {
    if (!is.data.frame(tables[[i]])) {
      stop(
        "Reference table '",
        table_names[[i]],
        "' from ",
        source,
        " must contain tabular data.",
        call. = FALSE
      )
    }

    missing_columns <- setdiff(c("code", "name"), names(tables[[i]]))
    if (length(missing_columns) > 0) {
      stop(
        "Reference table '",
        table_names[[i]],
        "' from ",
        source,
        " is missing required columns: ",
        paste(missing_columns, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    tables[[i]] <- as.data.frame(tables[[i]], stringsAsFactors = FALSE)
  }

  stats::setNames(tables, table_names)
}

.reference_tables_cache_file <- function() {
  file.path(
    tools::R_user_dir("makaraValidatr", "cache"),
    "reference_tables.json"
  )
}

.read_resource_text <- function(path_or_url) {
  lines <- suppressWarnings(readLines(path_or_url, warn = FALSE, encoding = "UTF-8"))
  paste(lines, collapse = "\n")
}

.write_resource_cache <- function(text, cache_file) {
  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  writeLines(text, cache_file, useBytes = TRUE)
}

#' Load manual validation rules
#'
#' Loads custom validation rules from the NOAA manual rules CSV endpoint or
#' a local CSV file. Returns a nested list by table name.
#'
#' @param file Optional path to a local manual rules CSV file. If `NULL`,
#'   manual rules are fetched from `url`.
#' @param url URL for the manual rules CSV endpoint.
#' @param cache Logical, whether to cache the fetched CSV and use the cache
#'   as a fallback if the remote endpoint cannot be reached.
#' @param cache_file Optional path for the cached CSV file. If `NULL`, uses
#'   the package cache directory.
#'
#' @return A named list of data frames, one per table, containing custom
#'   validation rules
#'
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr nest_by
#' @importFrom tibble deframe
#' @export
load_manual_rules <- function(file = NULL,
                              url = MANUAL_RULES_URL,
                              cache = TRUE,
                              cache_file = NULL) {
  if (!is.null(file)) {
    return(.load_manual_rules_csv(file))
  }

  if (is.null(cache_file)) {
    cache_file <- .manual_rules_cache_file()
  }

  remote_rules <- tryCatch(
    {
      fetched <- .read_resource_text(url)
      manual_rules <- .parse_manual_rules_csv(fetched, source = url)
      if (isTRUE(cache)) {
        .write_resource_cache(fetched, cache_file)
      }
      manual_rules
    },
    error = function(e) e
  )

  if (!inherits(remote_rules, "error")) {
    return(remote_rules)
  }

  if (isTRUE(cache) && file.exists(cache_file)) {
    warning(
      "Could not load manual rules from ",
      url,
      ": ",
      conditionMessage(remote_rules),
      ". Using cached manual rules from ",
      cache_file,
      ".",
      call. = FALSE
    )
    cached <- .read_resource_text(cache_file)
    return(.parse_manual_rules_csv(cached, source = cache_file))
  }

  stop(
    "Could not load manual rules from ",
    url,
    ": ",
    conditionMessage(remote_rules),
    ". No cached manual rules were available at ",
    cache_file,
    ".",
    call. = FALSE
  )
}

.load_manual_rules_csv <- function(file) {
  .parse_manual_rules_csv(file, source = file, literal = FALSE)
}

.parse_manual_rules_csv <- function(csv, source = "manual rules CSV", literal = TRUE) {
  input <- if (isTRUE(literal)) I(csv) else csv

  parsed <- tryCatch(
    read_csv(
      input,
      col_types = cols(
        .default = col_character()
      )
    ),
    error = function(e) {
      stop(
        "Failed to parse manual rules CSV from ",
        source,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  required_columns <- c("table", "name", "label", "rule")
  missing_columns <- setdiff(required_columns, names(parsed))
  if (length(missing_columns) > 0) {
    stop(
      "Manual rules CSV from ",
      source,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (any(is.na(parsed$table)) || any(!nzchar(parsed$table))) {
    stop(
      "Manual rules CSV from ",
      source,
      " contains missing or empty table names.",
      call. = FALSE
    )
  }

  parsed |>
    nest_by(table) |>
    deframe()
}

.manual_rules_cache_file <- function() {
  file.path(
    tools::R_user_dir("makaraValidatr", "cache"),
    "manual_rules.csv"
  )
}
