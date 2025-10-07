# Plan: Converting validate_submission.R to R Package

## Overview
Convert the standalone validation script into a well-structured R package that provides data validation functionality for the Makara passive acoustics monitoring database.

## Current State Analysis

### Script Structure
The current script ([script/validate_submission.R](script/validate_submission.R)) contains:
- ~740 lines of code
- Format definitions and validation helpers
- Column validation functions
- Data preparation functions
- File loading functions
- Rule generation functions (7 different types)
- Table validation logic
- Command-line interface using `optparse`

### Data Files
Three CSV files containing metadata:
- `column_definitions.csv`: 196 rows defining columns for 11 tables (deployments, recordings, analyses, detections, devices, projects, sites, recording_intervals, tracks, track_positions, sensor_datasets, sensor_values)
- `manual_rules.csv`: 3 custom validation rules
- `reference_tables.csv`: 2,476 rows of reference data for controlled vocabularies

## Package Structure Plan

### 1. Data Files (`data/` and `data-raw/`)

**Move CSV files to `inst/extdata/`:**
- `inst/extdata/column_definitions.csv`
- `inst/extdata/manual_rules.csv`
- `inst/extdata/reference_tables.csv`

**Create R data objects in `data/`:**
- `data/column_definitions.rda` - Nested list by table
- `data/manual_rules.rda` - Nested list by table
- `data/reference_tables.rda` - Nested list by table
- `data/makara_tables.rda` - Character vector of all table names

**Create data preparation script:**
- `data-raw/prepare_data.R` - Script to convert CSV files to .rda format

### 2. R Functions (`R/`)

Organize functions into logical modules:

#### `R/constants.R`
- `formats` - Named vector of regex patterns
- `MIN_DATE` - Minimum valid date constant
- `.onLoad()` - Package initialization

#### `R/data.R`
- Documentation for exported data objects
- `@format` and `@source` tags for datasets

#### `R/validation_helpers.R`
- `is_valid_json_format()` - JSON validation helper

#### `R/column_validation.R`
- `validate_columns()` - Check for required/unexpected columns
- `create_col_types()` - Generate readr column specifications

#### `R/data_preparation.R`
- `prepare_data_json()` - Add JSON validation columns
- `prepare_data_defaults()` - Apply default values
- `prepare_data()` - Main preparation wrapper

#### `R/data_loading.R`
- `load_column_definitions()` - Load and parse column definitions
- `load_reference_tables()` - Load reference lookup tables
- `load_manual_rules()` - Load custom validation rules
- Helper function to load from package data

#### `R/rule_generators.R`
- `generate_required_rules()` - Rules for required fields
- `generate_format_rules()` - Rules for data formats
- `generate_numeric_range_rules()` - Rules for numeric ranges
- `generate_unique_rules()` - Rules for uniqueness constraints
- `generate_timestamp_rules()` - Rules for timestamps
- `generate_date_rules()` - Rules for dates
- `generate_frequency_range_rules()` - Rules for frequency fields
- `generate_reference_table_rules()` - Rules for controlled vocabularies
- `generate_rules()` - Main wrapper function

#### `R/validation.R`
- `extract_failed_rows()` - Extract validation failures
- `validate_table()` - Validate a single table
- `validate_submission()` - Main validation function (exported)

#### `R/cli.R` (optional)
- `validate_submission_cli()` - Command-line interface wrapper
- Keep optparse logic separate from core validation

### 3. Documentation

#### Roxygen2 Documentation
Add to all exported functions:
- `@title` and `@description`
- `@param` for all parameters
- `@return` describing return values
- `@examples` with realistic examples
- `@export` for public functions
- `@importFrom` for imported functions

#### Package-level Documentation
- `R/makaraValidatr-package.R` - Package overview
- Link to vignettes and key functions

#### Vignettes (`vignettes/`)
- `vignettes/introduction.Rmd` - Getting started guide
- `vignettes/validation-rules.Rmd` - Explanation of validation rules
- `vignettes/custom-rules.Rmd` - How to extend validation
- `vignettes/command-line.Rmd` - Using the CLI interface

### 4. Tests (`tests/`)

#### Test Structure
```
tests/
├── testthat.R
└── testthat/
    ├── test-column_validation.R
    ├── test-data_preparation.R
    ├── test-rule_generators.R
    ├── test-validation.R
    ├── test-validation_helpers.R
    └── fixtures/
        ├── valid_deployments.csv
        ├── invalid_deployments.csv
        └── ... (test data files)
```

#### Test Coverage
- Unit tests for each rule generator
- Integration tests for full validation workflow
- Edge cases: empty files, missing columns, invalid formats
- Test data fixtures for each table type

### 5. Dependencies (`DESCRIPTION`)

**Imports:**
- validate (>= 1.1.0)
- readr (>= 2.0.0)
- tibble (>= 3.0.0)
- dplyr (>= 1.0.0)
- purrr (>= 1.0.0)
- tidyr (>= 1.0.0)
- lubridate (>= 1.9.0)
- glue (>= 1.6.0)
- jsonlite (>= 1.8.0)
- logger (>= 0.2.0)

**Suggests:**
- optparse (>= 1.7.0) - For CLI interface
- testthat (>= 3.0.0)
- knitr
- rmarkdown

### 6. Command-Line Interface

**Create executable script:**
- `inst/scripts/validate_submission` - Thin wrapper calling `validate_submission_cli()`
- Document installation: `Rscript -e 'makaraValidatr::validate_submission_cli()'`

**Alternative approach:**
- Create separate binary package or keep script in `inst/exec/`

### 7. Additional Files

#### `README.md`
- Installation instructions
- Quick start example
- Link to documentation
- Badge for build status (if using CI/CD)

#### `NEWS.md`
- Version history
- Start with version 0.1.0

#### `.Rbuildignore`
Add patterns for:
- `^script$` - Old script directory
- `^data-raw$` - Data preparation scripts
- `^\.Rproj\.user$`
- `^PLAN\.md$`

#### `.gitignore`
Standard R package patterns

## Implementation Steps

### Phase 1: Data Setup
1. Create `inst/extdata/` directory and move CSV files
2. Create `data-raw/prepare_data.R` script
3. Generate `.rda` files in `data/`
4. Document data objects in `R/data.R`

### Phase 2: Core Functions
1. Create `R/constants.R` with format patterns
2. Migrate helper functions to `R/validation_helpers.R`
3. Migrate column functions to `R/column_validation.R`
4. Migrate preparation functions to `R/data_preparation.R`
5. Migrate loading functions to `R/data_loading.R`

### Phase 3: Rule Generation
1. Migrate all `generate_*_rules()` to `R/rule_generators.R`
2. Add comprehensive documentation
3. Ensure all reference table names are parameterized

### Phase 4: Validation Logic
1. Migrate validation functions to `R/validation.R`
2. Refactor `validate_submission()` to use package data by default
3. Add progress indicators and better logging
4. Make `validate_table()` more modular

### Phase 5: CLI Interface
1. Create `R/cli.R` with CLI wrapper
2. Move optparse logic from main script
3. Create executable in `inst/scripts/`
4. Document CLI usage in vignette

### Phase 6: Documentation
1. Add roxygen2 comments to all functions
2. Create package-level documentation
3. Write vignettes (at least introduction)
4. Create comprehensive `README.md`

### Phase 7: Testing
1. Set up testthat infrastructure
2. Create test fixtures
3. Write unit tests for each module
4. Write integration tests
5. Aim for >80% code coverage

### Phase 8: Polish
1. Run `devtools::check()` and fix all issues
2. Verify examples run correctly
3. Check documentation completeness
4. Update `DESCRIPTION` metadata
5. Create `NEWS.md`

## API Design Considerations

### Main User-Facing Function

```r
validate_submission(
  data_dir,
  tables = NULL,           # NULL = all tables
  ncei = FALSE,            # Include NCEI-specific rules
  output_file = NULL,      # Where to save detailed results
  columns = NULL,          # Override default column definitions
  reference_tables = NULL, # Override default reference tables
  manual_rules = NULL,     # Additional custom rules
  verbose = TRUE,          # Control logging output
  stop_on_error = FALSE    # Whether to stop on first error
)
```

### Return Value
Return a structured S3 object:
```r
structure(
  list(
    success = TRUE/FALSE,
    tables = list(
      table_name = list(
        results = <validation object>,
        failed = <tibble of failures>
      )
    ),
    summary = <tibble of per-table results>
  ),
  class = "makara_validation_result"
)
```

### Print Method
Create `print.makara_validation_result()` for nice console output.

## Breaking Changes from Original Script

### Minimal Changes
- Function names remain the same
- Core logic preserved
- Same CSV input format

### Enhancements
- Data shipped with package (no need to specify paths)
- Can be used programmatically (not just CLI)
- Better error messages and logging
- Structured return values
- Extensibility through custom rules

### Backwards Compatibility
- Keep CLI interface identical
- Support same command-line flags
- Same exit codes for scripts

## Future Enhancements (Post v1.0)

1. **Validation profiles**: Pre-defined rule sets for different submission types
2. **Validation reports**: HTML/PDF reports with visualizations
3. **Auto-fixing**: Suggest or apply automatic corrections
4. **Incremental validation**: Validate only changed records
5. **Database integration**: Validate directly against PostgreSQL
6. **Shiny app**: Interactive validation interface
7. **Foreign key validation**: Cross-table relationship checks
8. **Performance optimization**: Parallel processing for large datasets

## Success Criteria

Package is ready for release when:
- [ ] All functions documented with roxygen2
- [ ] `devtools::check()` passes with 0 errors, 0 warnings, 0 notes
- [ ] Test coverage >80%
- [ ] At least one vignette completed
- [ ] README with working examples
- [ ] Can validate all example datasets successfully
- [ ] CLI interface works identically to original script
- [ ] Installation works via `devtools::install_github()`

## Estimated Timeline

- Phase 1: Data Setup - 1 hour
- Phase 2: Core Functions - 2 hours
- Phase 3: Rule Generation - 1 hour
- Phase 4: Validation Logic - 2 hours
- Phase 5: CLI Interface - 1 hour
- Phase 6: Documentation - 3 hours
- Phase 7: Testing - 4 hours
- Phase 8: Polish - 2 hours

**Total: ~16 hours of focused development**
