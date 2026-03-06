# Contributing to whep

Thank you for considering contributing to `whep`! This document
outlines the process and guidelines for contributing.

## Code of Conduct

Please note that this package is released with a
[Contributor Code of Conduct](https://ropensci.org/code-of-conduct/).
By contributing to this project, you agree to abide by its terms.

## How to Contribute

### Reporting Bugs

Open an issue on GitHub describing the bug, including a minimal
reproducible example if possible.

### Suggesting Features

Open an issue on GitHub describing the feature you'd like to see.

### Pull Requests

1. **Open an issue first** to discuss the change you'd like to make.
2. **Create a branch** from `main` with a descriptive name
   (e.g., `yourname/add-feature`).
3. **Write your code** following the style guidelines below.
4. **Write tests** for your new functionality using `testthat` and
   `pointblank`.
5. **Document** any new exported functions with `roxygen2`.
6. **Run checks** locally with `devtools::check()` before submitting.
7. **Submit a pull request** and request review.

## Development Workflow

We manage dependencies with `renv`. After cloning the repository:

```r
renv::restore()
```

Format your code with [Air](https://posit-dev.github.io/air/)
if available, or follow the
[Tidyverse style guide](https://style.tidyverse.org/).

Run tests:

```r
devtools::test()
```

Build and check:

```r
devtools::check()
```

## Style Guidelines

- Follow the [Tidyverse style guide](https://style.tidyverse.org/).
- Maximum line width: 80 characters.
- Use `snake_case` for column names in tibbles.
- Prefix private/helper functions with a dot (e.g., `.helper_fn()`).
- Always namespace external functions (e.g., `dplyr::filter()`).
- Use pipes (`|>`) for readability.
- Use `cli::cli_abort()` and `cli::cli_warn()` for error and
  warning messages.
- Use `rlang` for argument validation.
- Use tidy evaluation (`{{ }}`) for column name arguments.
- Use `roxygen2` for documentation of exported functions.

## Testing

- Use `testthat` for tests, with `pointblank` assertions where
  appropriate.
- Create sample data with `tibble::tribble()`.
- Tests are located in `tests/testthat/` and named
  `test_<scriptname>.R`.
- Aim for meaningful coverage of functionality and edge cases.

## Learning Resources

If you are new to R package development, we recommend our free
online guide:
[Follow the Workflow](https://lbm364dl.github.io/follow-the-workflow/).
