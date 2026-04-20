# CLAUDE.md — WHEP Package

## Workflow and Style

- Follow the workflow: <https://lbm364dl.github.io/follow-the-workflow/>
- Follow tidyverse style guide: <https://style.tidyverse.org/>
- Maximum line width is 80 characters.
- For code formatting, **always** run `air format .` before committing. Install the binary if not on PATH. Do not try to format manually.
- Extract complex logic into private/helper functions (prefixed with `.`) early. These helpers should be stateless and receive all necessary context via arguments.
- Functions should be short. Ideally no more than 25 lines. Split large functions into smaller ones with meaningful names.
- Main exported functions should come first in the file. Private helpers come at the end.
- Use `snake_case` for column names in tibbles. Names must be readable and self-explanatory — no cryptic abbreviations like `NEm`, `Bo`, `VS`, `GE`. Prefer descriptive names like `ne_maintenance`, `methane_potential`, `volatile_solids`, `gross_energy`.
- Avoid carrying redundant name+code column pairs (e.g. `area` + `area_code`, `item_prod` + `item_prod_code`) through intermediate computations. Integer codes are sufficient as join/group keys inside functions. Human-readable name columns should only be added at the final output stage via a dedicated lookup join. Do not add name columns to new functions until the lookup helpers exist.
- Always make sure all rules in this document and all linters have passed after a change in the code.
- Don't use imported functions without namespace prefix (e.g. use `dplyr::filter()` instead of just `filter()`). Do not use `@importFrom` in roxygen2 documentation.
- This is a tidy data project. Exported functions must accept and return `tibble` from tidyverse. Private helpers (`.` prefix) may use `data.table` internally for performance, but must convert back to tibble before returning. Never use bare `data.frame`.
- For argument validation use `rlang` functions (e.g. `rlang::has_name()` instead of `%in% names()`).
- For error messages use `cli::cli_abort()` instead of `stop()`. Same for warnings (`cli::cli_warn()`) and info.
- Escaped characters in regex must be double-escaped in R strings (e.g. `\\.` not `\.`).
- When defining functions that expect column names as arguments, expect symbolic names (unquoted) and use `{{ }}` (curly-curly) inside.
- There must not be functions inside functions. All functions at top level; helpers as private `.` functions.
- Always use native R pipes (`|>`). Make functions look like piped expressions.
- Avoid for loops. Use vectorised operations, `purrr`, or `dplyr`/`tidyr`. Exception: data.table private helpers may use for loops when iterating over a small fixed set (e.g. column names).
- Avoid function signatures with > 5 arguments. Group related arguments into named lists.
- Use `tibble::tribble()` for small inline tibbles.
- Use `stringr` functions instead of base R for strings.
- Use `.by` argument name for grouping.

## Documentation

- Use roxygen2. Only document exported functions. Private functions (`.` prefix) can remain undocumented.
- Finish all doc sentences with full stop.
- First line = title (no `@title` tag). Short, verb in imperative form.
- Then `@description`, `@param` for each parameter, `@return`, `@export`, `@examples`.
- One space after `#'`. Indent continuation lines by two spaces.
- Variable and function names must not exceed 30 characters.
- **Never** use `\dontrun{}` or `\donttest{}` in examples. All examples must run during `R CMD check`. For functions that depend on external/remote data or take too long to run live, use the `example = FALSE` parameter pattern: add an `example` argument that, when `TRUE`, returns a small hardcoded `tibble::tribble()` from a private helper in `R/toy_examples.R` (run the real function first, then sample ~10 rows for the fixture). The `@examples` block then just calls `my_function(example = TRUE)`. See `build_primary_production()` and `R/toy_examples.R` for reference. For functions that run fast on small input, just provide a self-contained inline example directly.

## Tests

- All functions should have tests with `testthat`.
- One test file per R script: `tests/testthat/test_scriptname.R`.
- Access exported objects (functions and datasets) via `whep::name` in tests, not bare names. For dynamic access in loops, use `getExportedValue("whep", nm)`. Never use `:::` or `getFromNamespace()` for exported objects.
- Use `tibble::tribble()`, pipes, `dplyr::pull()`, pointblank assertions.
- Take `test_gapfilling.R` as example.
- Make helper fixtures to reduce code repetition.

## References & Citations

- **NEVER guess or hallucinate reference titles, authors, years, or DOIs.** Always verify from the actual source (web search, PDF, or DOI lookup) before writing any bibliographic information.
- If you cannot verify a reference, say so explicitly rather than guessing.

## CI Checks

The PR must pass these GitHub Actions checks:

1. **R-CMD-check** (5 platforms): `rcmdcheck::rcmdcheck()` with no errors, warnings, or notes.
   - All NSE variables must be declared in `utils::globalVariables()` in `R/utils.R`.
   - All `stats::` functions must use explicit prefix (e.g., `stats::median()`).
   - Run locally: `Rscript -e "rcmdcheck::rcmdcheck(build_args='--no-build-vignettes', args=c('--no-tests','--ignore-vignettes'), error_on='error')"`

2. **lint** (`lintr`): Must pass with these linters disabled: `object_usage_linter`, `line_length_linter`, `indentation_linter`, `commas_linter` (conflicts with `air` formatting).
   - Run locally: `Rscript -e "lintr::lint_package(linters=lintr::linters_with_defaults(object_usage_linter=NULL, line_length_linter=NULL, indentation_linter=NULL, commas_linter=NULL))"`

3. **format-suggest** (`air`): Code must be formatted with `air format .`
   - **This is mandatory, not optional.** Do not attempt to manually match air style -- always run the binary.
   - If `air` is not on PATH, install it: download from `https://github.com/posit-dev/air/releases/latest/download/air-x86_64-pc-windows-msvc.zip` (Windows) or use `posit-dev/setup-air` (CI). Then run `air format .` on the repo root.
   - After running air, also run `devtools::document()` to update `man/` files.
   - Air formats **all** `.R` files: `R/`, `tests/`, `data-raw/`, etc. Not just the files you edited.

4. **pkgdown**: The documentation site must build without errors.
   - **Every documented topic** (functions AND datasets with roxygen docs) must appear in `_pkgdown.yml` under the `reference:` section. This includes all `.Rd` files in `man/`.
   - When adding new exported functions or documented datasets, add them to the appropriate section in `_pkgdown.yml` (or create a new section).
   - To verify: compare `ls man/*.Rd` topics against `_pkgdown.yml` contents. Every `.Rd` file (except `whep-package.Rd`) must have a matching entry.
   - Run locally: `Rscript -e "pkgdown::build_reference_index()"`

5. **Tests**: `devtools::test()` — 2 pre-existing failures in `test_commodity_balance_sheet.R` are expected (pin format, `skip_on_ci`).

## Before committing

```bash
# 1. Format (MANDATORY — do not skip, do not do manually)
air format .
```

```r
# 2. Document
devtools::document()

# 3. Check
rcmdcheck::rcmdcheck(
  build_args = "--no-build-vignettes",
  args = c("--no-tests", "--ignore-vignettes"),
  error_on = "error"
)

# 4. Test
devtools::test()
```

```bash
# 5. Verify pkgdown — every man/*.Rd must be in _pkgdown.yml
# (compare outputs; empty = OK)
comm -23 \
  <(ls man/*.Rd | sed 's|man/||;s|\.Rd||' | grep -v whep-package | sort) \
  <(grep '^  - ' _pkgdown.yml | sed 's/^  - //' | sort)
```

## Data pipeline

- **Primary production**: `build_primary_production()` — FAOSTAT + LUH2 extension (1850–2023).
- **CBS**: `build_commodity_balances()` — long format output with `source` and `fao_flag` columns.
- **Processing coefficients**: `build_processing_coefs()` — cascades from CBS.
- **Source labels**: use dataset-specific names (`FAOSTAT_prod`, `FAOSTAT_FBS_New`, etc.).
- **New data sources**: register via `whep_inputs.csv` + pins system; prepare with `inst/scripts/prepare_upload.R`.

## Package data updates

When modifying CSV files in `inst/extdata/harmonization/`:

1. Edit the CSV.
2. Run `Rscript data-raw/harmonization_tables.R` to rebuild `.rda` files.
3. Run `Rscript data-raw/table_mappings.R` if `regions.csv` or `items_*.csv` changed.
4. Run `Rscript data-raw/whep_inputs.R` if `whep_inputs.csv` changed.
