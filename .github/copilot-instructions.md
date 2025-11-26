# GitHub Copilot Instructions for WHEP Package

## Workflow and Style

- Follow the workflow: https://lbm364dl.github.io/follow-the-workflow/
- Follow tidyverse style guide: https://style.tidyverse.org/
- In the documentation section of functions, ensure these tags exist: `@description`, `@param` (for each function parameter), `@return`, `@export`, `@examples`
- Maximum line width is 80 characters
- There must be one space after `#'`
- If you start the text for an annotation in that same line, then the next lines should be indented two spaces to easily know it's for that same section
- Finish all doc sentences with full stop
- Use `snake_case` for column namings
- Always make sure that all rules in this document and all lintrs have passed after a change in the code

- Documentation for data objects saved in `data/*.rda` should be placed in `data-raw/` as a
  script named after the object (for example `data-raw/livestock_coefs.R`) following the
  pattern used by `data-raw/whep_inputs.R` (read source into an object and call
  `usethis::use_data()`).

## Tests

- All functions should have a set of tests, made with the testthat package
- Tests are located in their own script (one per script in R folder) in `tests/testthat`, and named following the convention: `"test_scriptname.R"`
- Tests should have a meaningful set of tests, designed to test functionality and edge cases. Avoid unnecessary tests
- Use dplyr as much as possible, including:
  - Make sample tibbles with `tibble::tribble()`
  - Pipes to pass datasets to functions
  - `dplyr::pull()` to extract vectors from dataframes
  - To test grouping behaviour, filter on group followed by group values test
- Use pointblank functions to test behaviour as much as possible (e.g. `expect_col_exists()`, `expect_col_vals_in_set()`, `expect_col_vals_not_null()`, `expect_col_vals_equal()`)
- Take `test_gapfilling.R` as example to make tests
- Make helper fixtures if they contribute to reduce the amount of code in the tests files
