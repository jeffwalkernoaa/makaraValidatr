# makaraValidatr

Data validation tools for Makara passive acoustics monitoring database.

## Overview

`makaraValidatr` provides tools for validating data in the Makara passive acoustics monitoring database. It includes functions for checking data types, ranges, and consistency, and is designed for validating template files prior to submission to the Makara database.

The validation rules reflect those listed in the Makara Data Submission Guide, which can be found on the Passive Acoustic Reporting System (PARS) website: https://www.fisheries.noaa.gov/resource/document/passive-acoustic-reporting-system-templates.

The types of checks include:

1. Required fields are present, and there are no unexpected columns
2. Data type and format is correct for each field
3. Data ranges are within acceptable limits
4. Fields referring to reference tables contain only valid codes
5. Optionally, fields referring to other Makara tables (foreign keys) resolve to a real row, either within the submission itself or in the Makara database (see [Foreign-Key Relation Validation](#foreign-key-relation-validation))

Output from the validation process provides a summary of errors for each rule and table. Detailed results of all errors can also be saved to a CSV file.

## Installation

### Prerequisites

- R >= 4.1.0

### Install from GitHub

This package is not available on CRAN, so it must be installed from GitHub using the `remotes` package.

```r
remotes::install_github("jeffwalkernoaa/makaraValidatr")
```

## Usage

### Basic Example

```r
library(makaraValidatr)

# Validate all tables in a directory containing templates (e.g. deployments.csv, recordings.csv, etc.)
validate_submission("/path/to/templates")

# Validate specific tables only
validate_submission("/path/to/templates", tables = c("deployments", "recordings"))

# Save detailed results of all errors to CSV
validate_submission("/path/to/templates", output_file = "validation_results.csv")

# Apply NCEI-specific validation rules
validate_submission("/path/to/templates", ncei = TRUE)
```

### Remote Validation Metadata

By default, the package fetches the latest validation metadata from NOAA:

- Column definitions: https://passiveacoustics.fisheries.noaa.gov/data/makara/templates/column_definitions.csv
- Manual rules: https://passiveacoustics.fisheries.noaa.gov/data/makara/templates/manual_rules.csv
- Reference tables: https://passiveacoustics.fisheries.noaa.gov/data/reference_tables.json

The downloaded CSV and JSON files are cached in the user cache directory. If a NOAA endpoint cannot be reached later, the corresponding loader warns and uses the cached file when available.

You can also use local CSV or JSON files when you need to validate against specific metadata snapshots:

```r
library(makaraValidatr)

# Use custom column definitions from a local CSV file
validate_submission(
  "/path/to/templates",
  column_definitions_file = "/path/to/column_definitions.csv"
)

# Use custom reference tables from a local CSV or JSON file
validate_submission(
  "/path/to/templates",
  reference_tables_file = "/path/to/reference_tables.json"
)

# Use custom manual rules from a local CSV file
validate_submission(
  "/path/to/templates",
  manual_rules_file = "/path/to/manual_rules.csv"
)

# Or load reference tables manually first, then pass them to the validation function
ref_tables <- load_reference_tables(file = "/path/to/reference_tables.csv")
validate_submission(
  "/path/to/templates",
  reference_tables = ref_tables
)
```

## Foreign-Key Relation Validation

In addition to the checks above, `makaraValidatr` can validate that foreign-key
references between tables actually resolve — for example, that a
`deployments.csv` row's `project_code` points at a project that exists
somewhere (either elsewhere in the submission or already in the Makara
database). This requires a connection to the Makara PostgreSQL database, so
it is off by default.

```r
library(makaraValidatr)

# Validate a submission including foreign-key relations
# (connects to the Makara database using DB_* environment variables)
validate_submission("/path/to/templates", relations = TRUE)

# Run relation validation on its own
validate_relations("/path/to/templates")

# Validate specific tables only
validate_relations("/path/to/templates", tables = c("deployments", "recordings"))

# Reuse an existing DBI connection instead of opening a new one
conn <- makara_db_connect()
validate_submission("/path/to/templates", relations = TRUE, conn = conn)
DBI::dbDisconnect(conn)
```

From the command line, pass `-r`/`--relations`:

```sh
Rscript -e 'makaraValidatr::validate_submission_cli()' /path/to/templates --relations
```

### Database connection

`makara_db_connect()` (and `validate_relations()`/`validate_submission(relations = TRUE)`
when no `conn` is supplied) connects using the `DBI`/`RPostgres` packages and
the same environment variables as the Django app, with these defaults:

| Variable | Default |
| --- | --- |
| `DB_NAME` | `makara` |
| `DB_HOST` | `localhost` |
| `DB_PORT` | `5432` |
| `DB_USER` | `makara` |
| `DB_PASSWORD` | *(empty)* |

Install the additional dependencies to use this feature:

```r
install.packages(c("DBI", "RPostgres"))
```

Foreign-key values may carry an owner-organization prefix (`ORG:CODE`) for
columns where this is allowed (declared via `org_prefix_allowed` in the
column definitions), mirroring the Makara Django admin's CSV import/export
conventions.

## Dependencies

Required packages:
- validate (>= 1.1.0)
- readr (>= 2.0.0)
- tibble (>= 3.0.0)
- dplyr (>= 1.0.0)
- purrr (>= 1.0.0)
- tidyr (>= 1.0.0)
- glue (>= 1.6.0)
- jsonlite (>= 1.8.0)
- logger (>= 0.2.0)
- stringr (>= 1.4.0)
- lubridate (>= 1.9.0)

Optional (only required for foreign-key relation validation):
- DBI
- RPostgres

## License

Apache License (>= 2). See [LICENSE](LICENSE) file for details.

## Author

Jeff Walker (jeffrey.walker@noaa.gov)
