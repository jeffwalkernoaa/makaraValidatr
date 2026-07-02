#' Load column definitions from CSV file
#'
#' Loads and parses column definitions from a CSV file or uses package data.
#' Returns a nested list by table name.
#'
#' @param file Path to column definitions CSV file. If NULL, uses package data.
#'
#' @return A named list of data frames, one per table, containing column
#'   definitions
#'
#' @importFrom readr read_csv cols col_character col_integer col_double col_logical
#' @importFrom dplyr mutate across coalesce nest_by
#' @importFrom tibble deframe
#' @export
load_column_definitions <- function(file = NULL) {
  if (is.null(file)) {
    # Use package data if available, otherwise load from inst/extdata
    if (exists("column_definitions", envir = asNamespace("makaraValidatr"))) {
      return(get("column_definitions", envir = asNamespace("makaraValidatr")))
    }
    file <- system.file("extdata", "column_definitions.csv", package = "makaraValidatr")
  }

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
      fetched <- .read_reference_tables_text(url)
      reference_tables <- .parse_reference_tables_json(fetched, source = url)
      if (isTRUE(cache)) {
        .write_reference_tables_cache(fetched, cache_file)
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
    cached <- .read_reference_tables_text(cache_file)
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
    json_text <- .read_reference_tables_text(file)
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

.read_reference_tables_text <- function(path_or_url) {
  lines <- suppressWarnings(readLines(path_or_url, warn = FALSE, encoding = "UTF-8"))
  paste(lines, collapse = "\n")
}

.write_reference_tables_cache <- function(json_text, cache_file) {
  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  writeLines(json_text, cache_file, useBytes = TRUE)
}

#' Load manual validation rules from CSV file
#'
#' Loads custom validation rules from a CSV file or uses package data.
#' Returns a nested list by table name.
#'
#' @param file Path to manual rules CSV file. If NULL, uses package data.
#'
#' @return A named list of data frames, one per table, containing custom
#'   validation rules
#'
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr nest_by
#' @importFrom tibble deframe
#' @export
load_manual_rules <- function(file = NULL) {
  if (is.null(file)) {
    # Use package data if available, otherwise load from inst/extdata
    if (exists("manual_rules", envir = asNamespace("makaraValidatr"))) {
      return(get("manual_rules", envir = asNamespace("makaraValidatr")))
    }
    file <- system.file("extdata", "manual_rules.csv", package = "makaraValidatr")
  }

  read_csv(
    file,
    col_types = cols(
      .default = col_character()
    )
  ) |>
    nest_by(table) |>
    deframe()
}
