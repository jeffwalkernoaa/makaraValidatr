#' Split an organization-prefixed code into org and code parts
#'
#' Vectorized port of the Django `split_code()` helper
#' (`makara/makara/utils.py`). Reference columns in CSV templates may prefix
#' a code with its owner organization (`"SEFSC:UXS_MHI"`); unprefixed codes
#' belong to `default_org`.
#'
#' @param values Character vector of raw code values (possibly `ORG:CODE`)
#' @param default_org Character vector of default organization codes, used
#'   when `values` carries no `ORG:` prefix. Recycled against `values`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{org}{Organization code (`NA` if malformed or input was NA/blank)}
#'     \item{code}{Bare code (`NA` if malformed or input was NA/blank)}
#'     \item{malformed_reason}{`NA` if valid, else a description of why the
#'       value could not be split}
#'   }
#'
#' @examples
#' split_org_code(c("UXS_MHI", "SEFSC:UXS_MHI", NA, "A:B:C"), "NEFSC")
#'
#' @importFrom tibble tibble
#' @export
split_org_code <- function(values, default_org) {
  n <- max(length(values), length(default_org))
  values <- rep_len(values, n)
  default_org <- rep_len(default_org, n)

  trimmed <- trimws(values)
  is_blank <- is.na(trimmed) | !nzchar(trimmed)

  org <- rep(NA_character_, n)
  code <- rep(NA_character_, n)
  malformed_reason <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    if (is_blank[i]) {
      next
    }

    value <- trimmed[i]
    colon_count <- lengths(regmatches(value, gregexpr(":", value, fixed = TRUE)))

    if (colon_count == 0) {
      org[i] <- default_org[i]
      code[i] <- value
      next
    }

    if (colon_count > 1) {
      malformed_reason[i] <- glue(
        "expected at most one ':' (ORG:CODE), got '{value}'"
      )
      next
    }

    parts <- trimws(strsplit(value, ":", fixed = TRUE)[[1]])
    # strsplit drops a trailing empty element (e.g. "ORG:" -> "ORG"), so
    # pad back to length 2 to detect an empty code part.
    if (length(parts) < 2) {
      parts <- c(parts, "")
    }
    org_part <- parts[1]
    code_part <- parts[2]

    if (!nzchar(org_part)) {
      malformed_reason[i] <- glue(
        "organization prefix is empty in '{value}'"
      )
      next
    }
    if (!nzchar(code_part)) {
      malformed_reason[i] <- glue(
        "code after ':' is empty in '{value}'"
      )
      next
    }

    org[i] <- org_part
    code[i] <- code_part
  }

  tibble(org = org, code = code, malformed_reason = malformed_reason)
}

#' Identify columns with relation annotations
#'
#' @param columns Data frame of column definitions for a single table
#'
#' @return Data frame, the subset of `columns` with a non-blank `relation`
#'
#' @importFrom dplyr filter
#' @noRd
.relation_columns <- function(columns) {
  columns |>
    filter(!is.na(relation) & nzchar(relation))
}

#' Derive the FK scope for a relation
#'
#' Scope is derived from the parent table's `unique_by` arity, with a
#' documented special case for `relation == "analyses"` (plan §1).
#'
#' @param parent_columns Data frame of column definitions for the parent
#'   (referenced) table
#' @param relation Character, name of the parent table
#'
#' @return A list with elements `scope` (one of `"org"`, `"deployment"`,
#'   `"analyses"`) and `key_col` (the parent's single unique_by key column)
#'
#' @importFrom dplyr filter pull
#' @noRd
.relation_scope <- function(parent_columns, relation) {
  key_rows <- parent_columns |>
    filter(!is.na(unique_by) & nzchar(unique_by))

  if (nrow(key_rows) != 1) {
    stop(
      "Relation target '",
      relation,
      "' must have exactly one column with a non-blank 'unique_by' ",
      "(found ",
      nrow(key_rows),
      ").",
      call. = FALSE
    )
  }

  key_col <- key_rows$name[1]
  arity <- length(strsplit(key_rows$unique_by[1], ",")[[1]])

  scope <- if (identical(relation, "analyses")) {
    "analyses"
  } else if (arity == 1) {
    "org"
  } else if (arity == 2) {
    "deployment"
  } else {
    stop(
      "Relation target '",
      relation,
      "' has an unsupported unique_by arity (",
      arity,
      ").",
      call. = FALSE
    )
  }

  list(scope = scope, key_col = key_col)
}

#' Collect distinct organization codes referenced by a submission
#'
#' Gathers every organization code that appears (directly, or via an
#' `ORG:` prefix) anywhere relation columns or their context columns are
#' used, so the DB layer can bound its queries.
#'
#' @param data_list Named list of data frames, one per submission table
#'   already loaded (with at least `organization_code` and, where present,
#'   `deployment_code`)
#' @param relation_columns Optional data frame of relation-annotated columns
#'   (`table`, `name`, `type`) whose values are also scanned for `ORG:`
#'   prefixes (including items of comma-separated code lists)
#'
#' @return Character vector of distinct, non-NA organization codes
#'
#' @noRd
.collect_org_codes <- function(data_list, relation_columns = NULL) {
  org_codes <- character(0)

  for (table in names(data_list)) {
    data <- data_list[[table]]
    if (is.null(data) || nrow(data) == 0) {
      next
    }

    if ("organization_code" %in% names(data)) {
      org_codes <- c(org_codes, data$organization_code)
    }

    if ("deployment_code" %in% names(data) && "organization_code" %in% names(data)) {
      split <- split_org_code(data$deployment_code, data$organization_code)
      org_codes <- c(org_codes, split$org)
    }
  }

  # ORG: prefixes carried by the relation values themselves (e.g. a
  # cross-org project_code or a prefixed item in deployment_device_codes)
  # must also bound the DB queries.
  if (!is.null(relation_columns) && nrow(relation_columns) > 0) {
    for (i in seq_len(nrow(relation_columns))) {
      data <- data_list[[relation_columns$table[i]]]
      col <- relation_columns$name[i]
      if (is.null(data) || !col %in% names(data)) {
        next
      }
      values <- data[[col]]
      values <- values[!is.na(values) & nzchar(trimws(values))]
      if (length(values) == 0) {
        next
      }
      items <- trimws(unlist(strsplit(values, ",")))
      items <- items[nzchar(items)]
      split <- split_org_code(items, NA_character_)
      org_codes <- c(org_codes, split$org)
    }
  }

  unique(org_codes[!is.na(org_codes) & nzchar(org_codes)])
}

#' Build submission-internal relation keys for a parent table
#'
#' Constructs the set of keys (as `"|"`-joined strings, matching
#' `.combine_keys()`/`check_relation_column()`) available from the
#' submission's own CSV for a parent table, based on the derived scope.
#'
#' @param data Data frame, the parent table's own submission data (already
#'   loaded; may be `NULL` if the parent table's CSV was not present)
#' @param scope One of `"org"`, `"deployment"`, `"analyses"`
#' @param key_col Character, the parent's key column name
#'
#' @return Character vector of keys (see `check_relation_column()` for the
#'   key format per scope)
#'
#' @noRd
.build_submission_keys <- function(data, scope, key_col) {
  if (is.null(data) || nrow(data) == 0 || !key_col %in% names(data)) {
    return(character(0))
  }

  if (scope == "org") {
    keys <- paste(data$organization_code, data[[key_col]], sep = "|")
  } else if (scope == "deployment") {
    dep_split <- split_org_code(data$deployment_code, data$organization_code)
    keys <- paste(dep_split$org, dep_split$code, data[[key_col]], sep = "|")
  } else if (scope == "analyses") {
    dep_split <- split_org_code(data$deployment_code, data$organization_code)
    keys <- paste(
      dep_split$org, dep_split$code, data$organization_code, data[[key_col]],
      sep = "|"
    )
  } else {
    stop("Unknown relation scope: ", scope, call. = FALSE)
  }

  valid <- !is.na(data[[key_col]]) & nzchar(data[[key_col]])
  unique(keys[valid])
}

#' Combine submission-internal and database-sourced relation keys
#'
#' @param submission_keys Character vector of keys derived from the
#'   submission's own CSV data
#' @param db_keys Character vector of keys fetched from the database
#'
#' @return Character vector, the union of both key sets
#'
#' @noRd
.combine_keys <- function(submission_keys, db_keys) {
  unique(c(submission_keys, db_keys))
}

#' Check a single relation column against a combined key set
#'
#' Implements the FK-resolution algorithm mirroring the Django import-export
#' widgets (`makara/makara/resources.py`), for one annotated column of one
#' submission table.
#'
#' @param data Data frame, the child table's submission data, including a
#'   `row` column aligned with `validate_table()`'s row numbering
#' @param table Character, the child table name
#' @param col_def A single-row data frame/list with the column definition
#'   for the relation column (`name`, `type`, `relation`, `org_prefix_allowed`)
#' @param scope One of `"org"`, `"deployment"`, `"analyses"`
#' @param keys Character vector of valid keys (submission + DB, combined via
#'   `.combine_keys()`)
#'
#' @return A tibble with columns: `row`, `col`, `value`, `rule`, `error`
#'   (zero rows if no violations)
#'
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @noRd
check_relation_column <- function(data, table, col_def, scope, keys) {
  col <- col_def$name
  relation <- col_def$relation
  is_code_list <- identical(col_def$type, "code_list")

  empty_result <- tibble(
    row = integer(0), col = character(0), value = character(0),
    rule = character(0), error = character(0)
  )

  if (nrow(data) == 0 || !col %in% names(data)) {
    return(empty_result)
  }

  raw_values <- data[[col]]
  row_nums <- data$row
  org_codes <- data$organization_code
  has_deployment_context <- "deployment_code" %in% names(data)
  deployment_codes <- if (has_deployment_context) data$deployment_code else NA_character_

  rows_out <- list()

  for (i in seq_along(raw_values)) {
    value <- raw_values[i]
    if (is.na(value) || !nzchar(trimws(value))) {
      next
    }

    row_org <- org_codes[i]

    # Resolve deployment context once per row for deployment/analyses scope.
    dep_org <- NA_character_
    dep_code <- NA_character_
    dep_malformed <- NA_character_
    if (scope %in% c("deployment", "analyses")) {
      dep_split <- split_org_code(deployment_codes[i], row_org)
      dep_org <- dep_split$org
      dep_code <- dep_split$code
      dep_malformed <- dep_split$malformed_reason
    }

    if (!is.na(dep_malformed)) {
      rows_out[[length(rows_out) + 1]] <- tibble(
        row = row_nums[i],
        col = "deployment_code",
        value = glue("'{deployment_codes[i]}'"),
        rule = glue("relation.{col}"),
        error = glue(
          "'deployment_code' is malformed ({dep_malformed})"
        )
      )
      next
    }

    items <- if (is_code_list) {
      trimws(unlist(strsplit(value, ",")))
    } else {
      trimws(value)
    }
    items <- items[nzchar(items)]
    if (length(items) == 0) {
      next
    }

    unknown_items <- character(0)
    unknown_orgs <- character(0)
    malformed_hit <- FALSE

    for (item in items) {
      if (scope == "org") {
        split <- split_org_code(item, row_org)
        if (!is.na(split$malformed_reason)) {
          rows_out[[length(rows_out) + 1]] <- tibble(
            row = row_nums[i],
            col = col,
            value = glue("'{item}'"),
            rule = glue("relation.{col}"),
            error = glue("'{col}' is malformed ({split$malformed_reason})")
          )
          malformed_hit <- TRUE
          next
        }
        key <- paste(split$org, split$code, sep = "|")
        item_org <- split$org
        item_code <- split$code
      } else if (scope == "deployment") {
        if (grepl(":", item, fixed = TRUE)) {
          rows_out[[length(rows_out) + 1]] <- tibble(
            row = row_nums[i],
            col = col,
            value = glue("'{item}'"),
            rule = glue("relation.{col}"),
            error = glue(
              "'{col}' must not carry an organization prefix (deployment-scoped)"
            )
          )
          malformed_hit <- TRUE
          next
        }
        key <- paste(dep_org, dep_code, item, sep = "|")
        item_code <- item
      } else if (scope == "analyses") {
        if (grepl(":", item, fixed = TRUE)) {
          rows_out[[length(rows_out) + 1]] <- tibble(
            row = row_nums[i],
            col = col,
            value = glue("'{item}'"),
            rule = glue("relation.{col}"),
            error = glue(
              "'{col}' must not carry an organization prefix (deployment-scoped)"
            )
          )
          malformed_hit <- TRUE
          next
        }
        key <- paste(dep_org, dep_code, row_org, item, sep = "|")
        item_code <- item
      } else {
        stop("Unknown relation scope: ", scope, call. = FALSE)
      }

      if (!key %in% keys) {
        unknown_items <- c(unknown_items, item_code)
        if (scope == "org") {
          unknown_orgs <- c(unknown_orgs, item_org)
        }
      }
    }

    if (malformed_hit || length(unknown_items) == 0) {
      next
    }

    error_message <- if (is_code_list) {
      codes_joined <- paste(unknown_items, collapse = "', '")
      if (scope == "org") {
        glue(
          "'{col}' contains unknown {relation} codes ('{codes_joined}') ",
          "in organization '{row_org}'"
        )
      } else if (scope == "deployment") {
        glue(
          "'{col}' contains unknown {relation} codes ('{codes_joined}') ",
          "in deployment '{dep_code}' of organization '{dep_org}'"
        )
      } else {
        glue(
          "'{col}' contains unknown {relation} codes ('{codes_joined}') ",
          "for deployment '{dep_code}' of organization '{dep_org}' ",
          "from organization '{row_org}'"
        )
      }
    } else {
      code <- unknown_items[1]
      if (scope == "org") {
        code_org <- unknown_orgs[1]
        glue(
          "'{col}' references unknown {relation} code '{code}' ",
          "in organization '{code_org}'"
        )
      } else if (scope == "deployment") {
        glue(
          "'{col}' references unknown {relation} code '{code}' ",
          "in deployment '{dep_code}' of organization '{dep_org}'"
        )
      } else {
        glue(
          "'{col}' references unknown {relation} analysis code '{code}' ",
          "for deployment '{dep_code}' of organization '{dep_org}' ",
          "from organization '{row_org}'"
        )
      }
    }

    rows_out[[length(rows_out) + 1]] <- tibble(
      row = row_nums[i],
      col = col,
      value = glue("'{value}'"),
      rule = glue("relation.{col}"),
      error = error_message
    )
  }

  if (length(rows_out) == 0) {
    return(empty_result)
  }

  dplyr::bind_rows(rows_out)
}

#' Read the columns needed for relation checking from a submission CSV
#'
#' Reads only `organization_code`, `deployment_code` (if present in the
#' column definitions for `table`), and `needed_cols` from
#' `<data_dir>/<table>.csv`, using `readr::cols_only()` to bound memory for
#' large files (e.g. detections). Drops fully-empty rows and assigns `row`
#' the same way `validate_table()` does, so row numbers align across output.
#'
#' @param data_dir Directory containing CSV files
#' @param table Table name (without `.csv`)
#' @param table_columns Column definitions for `table`
#' @param needed_cols Character vector of additional columns to read
#'
#' @return A tibble with a `row` column, or `NULL` if the CSV does not exist
#'
#' @importFrom readr read_csv cols_only col_character
#' @importFrom dplyr filter if_all everything mutate row_number
#' @noRd
.read_relation_csv <- function(data_dir, table, table_columns, needed_cols = character(0)) {
  file_path <- file.path(data_dir, paste0(table, ".csv"))
  if (!file.exists(file_path)) {
    return(NULL)
  }

  all_names <- table_columns$name
  context_cols <- intersect(c("organization_code", "deployment_code"), all_names)
  read_cols <- unique(c(context_cols, intersect(needed_cols, all_names)))

  col_spec <- do.call(
    cols_only,
    stats::setNames(rep(list(col_character()), length(read_cols)), read_cols)
  )

  data <- read_csv(file_path, col_types = col_spec, na = "")

  data <- dplyr::filter(data, !dplyr::if_all(dplyr::everything(), is.na))

  dplyr::mutate(data, row = dplyr::row_number())
}

#' Validate foreign-key relations across a Makara submission
#'
#' For each annotated relation column (`relation` in column_definitions) in
#' each submitted table, confirms that every referenced code resolves either
#' within the submission itself or in the existing Makara PostgreSQL
#' database. Mirrors the scoping semantics of the Django import-export
#' widgets (org-scoped, deployment-scoped, and the analyses composite-key
#' special case).
#'
#' @param data_dir Directory containing CSV files to validate
#' @param tables Character vector of table names to check. If `NULL`, checks
#'   all tables with at least one relation-annotated column.
#' @param columns Optional custom column definitions (overrides
#'   `column_definitions_file` and remote loading)
#' @param column_definitions_file Optional path to a local column
#'   definitions CSV file. If provided and `columns` is `NULL`, loads column
#'   definitions from this file instead of the NOAA column definition CSV
#'   endpoint.
#' @param conn Optional DBI connection to the Makara database. If `NULL`, a
#'   connection is created via `makara_db_connect()` and closed on exit;
#'   an injected connection is left open for the caller to manage.
#' @param verbose Logical, whether to print detailed logging output
#'
#' @return A tibble with columns: `table`, `type` (`"relation"`), `row`,
#'   `col`, `value`, `rule`, `error`. Zero rows if no violations were found.
#'
#' @examples
#' \dontrun{
#' validate_relations("/path/to/data")
#' validate_relations("/path/to/data", tables = c("deployments", "recordings"))
#' }
#'
#' @importFrom dplyr bind_rows filter
#' @importFrom logger log_info log_warn log_error log_threshold
#' @export
validate_relations <- function(data_dir,
                               tables = NULL,
                               columns = NULL,
                               column_definitions_file = NULL,
                               conn = NULL,
                               verbose = TRUE) {
  if (isFALSE(verbose)) {
    old_thresh <- logger::log_threshold()
    on.exit(logger::log_threshold(old_thresh), add = TRUE)
    logger::log_threshold(level = "OFF")
  }

  if (is.null(columns)) {
    if (!is.null(column_definitions_file)) {
      log_info("Loading column definitions from: {column_definitions_file}")
      columns <- load_column_definitions(file = column_definitions_file)
    } else {
      columns <- load_column_definitions()
    }
  }

  # Which child tables have relation-annotated columns?
  relation_tables <- Filter(
    function(table) nrow(.relation_columns(columns[[table]])) > 0,
    names(columns)
  )

  if (is.null(tables)) {
    tables <- relation_tables
  } else {
    tables <- intersect(tables, relation_tables)
  }

  empty_result <- tibble::tibble(
    table = character(0), type = character(0), row = integer(0),
    col = character(0), value = character(0), rule = character(0),
    error = character(0)
  )

  if (length(tables) == 0) {
    return(empty_result)
  }

  owns_conn <- is.null(conn)
  if (owns_conn) {
    conn <- makara_db_connect()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  # Every parent table referenced by any relation column across the tables
  # being checked, plus every child table itself (for self-refs and to load
  # submission-internal data).
  all_relations <- lapply(tables, function(table) {
    rel_cols <- .relation_columns(columns[[table]])
    if (nrow(rel_cols) == 0) {
      return(NULL)
    }
    rel_cols$table <- table
    rel_cols
  })
  all_relations <- dplyr::bind_rows(Filter(Negate(is.null), all_relations))

  parent_tables <- unique(all_relations$relation)
  tables_to_read <- unique(c(tables, parent_tables))

  # Read submission data for every table involved (child + parent), pulling
  # only the columns needed for relation checking.
  data_list <- list()
  for (table in tables_to_read) {
    table_columns <- columns[[table]]
    if (is.null(table_columns)) {
      next
    }
    needed_cols <- unique(c(
      .relation_columns(table_columns)$name,
      table_columns$name[!is.na(table_columns$unique_by) & nzchar(table_columns$unique_by)]
    ))
    data_list[[table]] <- .read_relation_csv(data_dir, table, table_columns, needed_cols)
  }

  org_codes <- .collect_org_codes(data_list, all_relations)

  all_errors <- list()

  for (i in seq_len(nrow(all_relations))) {
    rel <- all_relations[i, ]
    table <- rel$table
    col_def <- rel

    data <- data_list[[table]]
    if (is.null(data) || nrow(data) == 0) {
      next
    }

    parent_table <- rel$relation
    parent_columns <- columns[[parent_table]]
    if (is.null(parent_columns)) {
      stop(
        "Relation on '", table, ".", col_def$name, "' references table '",
        parent_table, "', but no column definitions were provided for it.",
        call. = FALSE
      )
    }
    scope_info <- .relation_scope(parent_columns, parent_table)
    scope <- scope_info$scope
    key_col <- scope_info$key_col

    parent_data <- data_list[[parent_table]]
    submission_keys <- .build_submission_keys(parent_data, scope, key_col)

    log_info("Checking relation '{table}.{col_def$name}' -> '{parent_table}'...")
    db_keys_df <- .fetch_db_relation_keys(conn, parent_table, org_codes)
    db_keys <- if (nrow(db_keys_df) == 0) {
      character(0)
    } else {
      do.call(paste, c(as.list(db_keys_df), sep = "|"))
    }

    keys <- .combine_keys(submission_keys, db_keys)

    result <- check_relation_column(data, table, col_def, scope, keys)
    if (nrow(result) > 0) {
      log_warn("Found {nrow(result)} relation errors for '{table}.{col_def$name}'")
      result$table <- table
      all_errors[[length(all_errors) + 1]] <- result
    }
  }

  if (length(all_errors) == 0) {
    return(empty_result)
  }

  errors <- dplyr::bind_rows(all_errors)
  errors$type <- "relation"
  errors <- dplyr::select(errors, table, type, row, col, value, rule, error)
  dplyr::arrange(errors, table, rule, row)
}
