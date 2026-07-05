#' Connect to the Makara PostgreSQL database
#'
#' Opens a DBI connection to the Makara database using RPostgres, defaulting
#' to the same environment variables the Django app and `r/quality-ncei.R`
#' use (`DB_NAME`, `DB_HOST`, `DB_PORT`, `DB_USER`, `DB_PASSWORD`).
#'
#' @param dbname Database name
#' @param host Database host
#' @param port Database port
#' @param user Database user
#' @param password Database password
#'
#' @return A DBI connection object. The caller is responsible for closing it
#'   with `DBI::dbDisconnect()`.
#'
#' @examples
#' \dontrun{
#' conn <- makara_db_connect()
#' on.exit(DBI::dbDisconnect(conn))
#' }
#'
#' @export
makara_db_connect <- function(dbname = Sys.getenv("DB_NAME", "makara"),
                              host = Sys.getenv("DB_HOST", "localhost"),
                              port = Sys.getenv("DB_PORT", "5432"),
                              user = Sys.getenv("DB_USER", "makara"),
                              password = Sys.getenv("DB_PASSWORD", "")) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop(
      "Package 'DBI' is required to connect to the Makara database. ",
      "Install with: install.packages('DBI')",
      call. = FALSE
    )
  }
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop(
      "Package 'RPostgres' is required to connect to the Makara database. ",
      "Install with: install.packages('RPostgres')",
      call. = FALSE
    )
  }

  tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = dbname,
      host = host,
      port = port,
      user = user,
      password = password
    ),
    error = function(e) {
      stop(
        "Could not connect to the Makara database at ",
        user, "@", host, ":", port, "/", dbname,
        ": ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Parent tables reachable directly by organization_code
#'
#' @noRd
.org_scoped_relation_tables <- function() {
  c("projects", "sites", "devices", "deployments", "citations")
}

#' Parent tables reachable via a join through deployments
#'
#' @noRd
.deployment_scoped_relation_tables <- function() {
  c("recordings", "tracks", "sensor_datasets")
}

#' Fetch distinct relation keys for a parent table, filtered by organization
#'
#' Queries only the columns needed to build relation keys (never pulls full
#' rows), restricted to the organizations actually referenced by the
#' submission being validated. Mirrors the scoping semantics of the Django
#' import-export widgets (`makara/makara/resources.py`): org-scoped parents
#' are looked up directly by `organization_code`; deployment-scoped parents
#' (recordings, tracks, sensor_datasets) are resolved through a join to
#' `deployments` on the deployment's own organization/code, not the child
#' row's own `organization_code`; `analyses` is a documented special case
#' keyed by `(deployment_org, deployment_code, analysis_org, analysis_code)`.
#'
#' @param conn A DBI connection
#' @param parent_table Character, one of `"projects"`, `"sites"`,
#'   `"devices"`, `"deployments"`, `"citations"`, `"recordings"`,
#'   `"tracks"`, `"sensor_datasets"`, `"analyses"`
#' @param org_codes Character vector of organization codes to filter to
#'
#' @return A tibble with columns depending on scope:
#'   \describe{
#'     \item{org-scoped}{`org`, `code`}
#'     \item{deployment-scoped}{`dep_org`, `dep_code`, `code`}
#'     \item{analyses}{`dep_org`, `dep_code`, `analysis_org`, `code`}
#'   }
#'
#' @importFrom glue glue_sql
#' @importFrom tibble as_tibble
#' @noRd
.fetch_db_relation_keys <- function(conn, parent_table, org_codes) {
  empty_org <- tibble::tibble(org = character(0), code = character(0))
  empty_dep <- tibble::tibble(
    dep_org = character(0), dep_code = character(0), code = character(0)
  )
  empty_analyses <- tibble::tibble(
    dep_org = character(0), dep_code = character(0),
    analysis_org = character(0), code = character(0)
  )

  if (length(org_codes) == 0) {
    if (parent_table %in% .org_scoped_relation_tables()) {
      return(empty_org)
    } else if (parent_table %in% .deployment_scoped_relation_tables()) {
      return(empty_dep)
    } else if (identical(parent_table, "analyses")) {
      return(empty_analyses)
    }
    stop("Unsupported relation parent table: ", parent_table, call. = FALSE)
  }

  if (parent_table %in% .org_scoped_relation_tables()) {
    key_col <- .relation_key_column(parent_table)
    query <- glue::glue_sql(
      "SELECT DISTINCT organization_code AS org, {`key_col`} AS code
       FROM {`parent_table`}
       WHERE organization_code IN ({org_codes*})",
      .con = conn
    )
    result <- DBI::dbGetQuery(conn, query)
    names(result) <- c("org", "code")
    return(tibble::as_tibble(result))
  }

  if (parent_table %in% .deployment_scoped_relation_tables()) {
    key_col <- .relation_key_column(parent_table)
    query <- glue::glue_sql(
      "SELECT DISTINCT d.organization_code AS dep_org,
              d.deployment_code AS dep_code,
              x.{`key_col`} AS code
       FROM {`parent_table`} AS x
       JOIN deployments AS d ON x.deployment_id = d.id
       WHERE d.organization_code IN ({org_codes*})",
      .con = conn
    )
    result <- DBI::dbGetQuery(conn, query)
    names(result) <- c("dep_org", "dep_code", "code")
    return(tibble::as_tibble(result))
  }

  if (identical(parent_table, "analyses")) {
    query <- glue::glue_sql(
      "SELECT DISTINCT d.organization_code AS dep_org,
              d.deployment_code AS dep_code,
              a.organization_code AS analysis_org,
              a.analysis_code AS code
       FROM analyses AS a
       JOIN deployments AS d ON a.deployment_id = d.id
       WHERE d.organization_code IN ({org_codes*})
          OR a.organization_code IN ({org_codes*})",
      .con = conn
    )
    result <- DBI::dbGetQuery(conn, query)
    names(result) <- c("dep_org", "dep_code", "analysis_org", "code")
    return(tibble::as_tibble(result))
  }

  stop("Unsupported relation parent table: ", parent_table, call. = FALSE)
}

#' Look up the DB key column name for a relation parent table
#'
#' @param parent_table Character, parent table name
#'
#' @return Character, the corresponding DB column holding the unique code
#'
#' @noRd
.relation_key_column <- function(parent_table) {
  key_columns <- c(
    projects = "project_code",
    sites = "site_code",
    devices = "device_code",
    deployments = "deployment_code",
    citations = "citation_code",
    recordings = "recording_code",
    tracks = "track_code",
    sensor_datasets = "sensor_dataset_code"
  )

  if (!parent_table %in% names(key_columns)) {
    stop("Unsupported relation parent table: ", parent_table, call. = FALSE)
  }

  key_columns[[parent_table]]
}
