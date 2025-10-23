#' Column definitions for Makara database tables
#'
#' A nested list containing column definitions for all tables in the
#' Makara passive acoustics monitoring database. Each element is a
#' data frame defining columns for one table.
#'
#' @format A named list with 10 elements (one per table):
#' \describe{
#'   \item{projects}{Column definitions for projects table}
#'   \item{sites}{Column definitions for sites table}
#'   \item{devices}{Column definitions for devices table}
#'   \item{deployments}{Column definitions for deployments table}
#'   \item{recordings}{Column definitions for recordings table}
#'   \item{recording_intervals}{Column definitions for recording_intervals table}
#'   \item{analyses}{Column definitions for analyses table}
#'   \item{detections}{Column definitions for detections table}
#'   \item{tracks}{Column definitions for tracks table}
#'   \item{track_positions}{Column definitions for track_positions table}
#' }
#'
#' Each data frame contains columns describing field properties:
#' \describe{
#'   \item{table}{Table name}
#'   \item{index}{Column order index}
#'   \item{name}{Column name}
#'   \item{type}{Data type (e.g., code, integer, float, date, timestamp, json)}
#'   \item{required}{Whether the column is required}
#'   \item{required_unless}{Conditional expression to ignore required constraint}
#'   \item{required_ncei}{Whether required for NCEI submissions}
#'   \item{min}{Minimum value for numeric columns}
#'   \item{max}{Maximum value for numeric columns}
#'   \item{default}{Default value if missing}
#'   \item{reference_table}{Name of reference table for validation}
#'   \item{unique_by}{Fields that determine uniqueness constraint}
#'   \item{description}{Column description}
#' }
#'
#' @source Generated from inst/extdata/column_definitions.csv
"column_definitions"

#' Reference tables for controlled vocabularies
#'
#' A nested list containing reference lookup tables for validating
#' controlled vocabulary fields in the Makara database.
#'
#' @format A named list with multiple elements (one per reference table).
#'   Each element is a data frame with columns:
#' \describe{
#'   \item{table}{Reference table name}
#'   \item{code}{Valid code value}
#'   \item{name}{Human-readable name for the code}
#' }
#'
#' Reference tables include: analysis_granularity_types, analysis_processing_types,
#' analysis_quality_types, behaviors, call_types, and many others.
#'
#' @source Generated from inst/extdata/reference_tables.csv
"reference_tables"

#' Manual validation rules
#'
#' A nested list containing custom validation rules that supplement
#' the automatically generated rules.
#'
#' @format A named list with elements for tables that have custom rules.
#'   Each element is a data frame with columns:
#' \describe{
#'   \item{table}{Table name}
#'   \item{rule}{Validation rule expression}
#'   \item{name}{Rule identifier}
#'   \item{label}{Human-readable error message}
#' }
#'
#' @source Generated from inst/extdata/manual_rules.csv
"manual_rules"
