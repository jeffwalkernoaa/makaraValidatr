# makaraValidatr

Data validation tools for Makara passive acoustics monitoring database.

## Overview

`makaraValidatr` provides tools for validating data in the Makara passive acoustics monitoring database. It includes functions for checking data types, ranges, consistency, and (optionally) relations, and is designed for validating template files prior to submission to the Makara database.

The validation rules reflect those listed in the Makara Data Submission Guide, which is currently under development.

The types of checks include:

1. Required fields are present, and there are no unexpected columns
2. Data type and format is correct for each field
3. Data ranges are within acceptable limits
4. Fields referring to reference tables contain only valid codes
5. Optionally, fields referring to other Makara tables (foreign keys) resolve to a real row, either within the submission itself or in the Makara database (see [Relations Validation](#relations-validation))

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

### Basic Validation

Use the `validate_submission()` function to validate each template included in a submission.

This function simply needs a path on your computer containing the template files (e.g., `deployments.csv`, `recordings.csv`, `analyses.csv`, etc.). 

Note that this function assumes the files use the standardized template filename convention (`<table>.csv`). It cannot accomodate custom filenames (e.g., `my-deployments.csv`). Any files not matching an expected template will be ignored by this package.

```r
library(makaraValidatr)

# Validate all tables in a directory containing templates (e.g. deployments.csv, recordings.csv, etc.)
validate_submission("/path/to/templates")

# Validate specific tables only, in this case only deployments.csv and recordings.csv
validate_submission("/path/to/templates", tables = c("deployments", "recordings"))

# Apply NCEI-specific validation rules
validate_submission("/path/to/templates", ncei = TRUE)

# Change the name and location of the validation results (default is validation_results.csv within the same folder as the templates)
validate_submission("/path/to/templates", output_file = "/path/to/custom_validation_results.csv")
```

### Relations Validation

By default, the `validate_submission()` function **does not** validate relations between tables. In other words, it does not verify that the `deployment_code` values in the `recordings` table all match either a new or existing deployment as this requires access to the Makara database in order to check whether a given deployment, recording, device, etc. already exists. Instead, it simply checks that the values in any columns referencing other tables are propertly fomatted as valid codes (but does not check that those codes exist).

However, users with access to the Makara BigQuery dataset can perform this addition validation by setting the `relations = TRUE` argument in `validate_submissions()`. 

Access to the Makara BigQuery dataset requires an active `@noaa.gov` email and can be requested by contacting the Makara team at `passive.acoustics@noaa.gov`

Once you have been granted access, the `validate_submission()` function can be used to perform additional valdiations of the table relations to ensure all referened codes between each table are either included in the current submission (new records) or already exist in Makara (existing records).  

To do this, follow these steps:

```r
library(makaraValidatr)

# STEP 0: install {bigrquery}
install.packages("bigrquery")

# STEP 1: Authenticate BigQuery

# PAM-WW ONLY: if you are on a pam-ww workstation, run this line first (otherwise skip to the next line)
gargle::cred_funs_add(credentials_gce = NULL) # pam-ww only!

# Replace the email field with your email address
# This should open a web browser and ask you to authorize access to your Google account
bigrquery::bq_auth(
  email = "<username>@noaa.gov", 
  scopes = c(
    "https://www.googleapis.com/auth/userinfo.email", 
    "https://www.googleapis.com/auth/bigquery", 
    "https://www.googleapis.com/auth/cloud-platform"
  )
)


# STEP 2: Set Environmental Variables
# makaraValidatr uses the `BQ_PROJECT` and `BQ_DATASET` variables to connect to the Makara dataset in BigQuery
Sys.setenv(BQ_PROJECT = "ggn-nmfs-pacm-dev-1", BQ_DATASET = "makara")

# STEP 3: Run Validation with Relations
# Run validate_submission() as before but with the relations=TRUE argument
validate_submission("/path/to/templates", relations = TRUE)
# The output should contain a "Validating Relations" section at the end, confirming it was able to access BigQuery
```

## Remote Validation Metadata

By default, the package fetches the latest validation metadata from the NOAA Passive Acoustics Server. From these files, the package automatically generates the associated validation rules.

- Column definitions: https://passiveacoustics.fisheries.noaa.gov/data/makara/templates/column_definitions.csv
- Manual rules: https://passiveacoustics.fisheries.noaa.gov/data/makara/templates/manual_rules.csv
- Reference tables: https://passiveacoustics.fisheries.noaa.gov/data/reference_tables.json

The downloaded CSV and JSON files are cached in the user cache directory. If the NOAA Passive Acoustics server cannot be reached later, the package uses the cached file when available.

## License

Apache License (>= 2). See [LICENSE](LICENSE) file for details.

## Author

Jeff Walker (jeffrey.walker@noaa.gov)
