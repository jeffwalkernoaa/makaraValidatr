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

### Using Custom Reference Tables

The package includes built-in reference tables, but you can also use updated reference tables from an external file. This is useful when reference codes are updated between package releases.

NOAA staff can download the latest reference tables from Google Drive at: https://drive.google.com/file/d/1JyrU5g43YdGTgJ7-XpMurGYsDGHibnOT/view?usp=drive_link

Once this file is downloaded, you can load and use the custom reference tables in the validation process as shown below:

```r
library(makaraValidatr)

# Use custom reference tables from a downloaded file (single step)
validate_submission(
  "/path/to/templates",
  reference_tables_file = "/path/to/reference_tables.csv"
)

# Or load reference tables manually first, then pass them to the validation function
ref_tables <- load_reference_tables(file = "/path/to/reference_tables.csv")
validate_submission(
  "/path/to/templates",
  reference_tables = ref_tables
)
```

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

## License

Apache License (>= 2). See [LICENSE](LICENSE) file for details.

## Author

Jeff Walker (jeffrey.walker@noaa.gov)
