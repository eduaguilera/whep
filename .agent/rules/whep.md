---
trigger: always_on
---

## Workflow and Style

- Follow the workflow: https://lbm364dl.github.io/follow-the-workflow/
- Follow tidyverse style guide: https://style.tidyverse.org/
- Maximum line width is 80 characters
- For code formatting, use `air format` if this is available in the user's computer
- Extract complex logic into private/helper functions (prefixed with .) early. These helpers should be stateless and receive all necessary context via arguments.
- Functions should be short. Ideally no more than 25 lines. There might be exceptions if the code is easy to follow, but try to keep functions short and modular. If necessary, split large functions into smaller ones, with meaningful names. 
- Main exported functions should come first in the file. The private helpers should come at the end, after all exported functions.
- Use `snake_case` for column namings in tibbles
- Always make sure that all rules in this document and all lintrs have passed after a change in the code
- Don't use imported functions without namespace prefix (e.g. use `dplyr::filter()` instead of just `filter()`). This also means in the package functions you must not use `@importFrom` in the roxygen2 documentation.
- This is a tidy data project. Don't use `data.frame` or `data.table`. Always use `tibble` from tidyverse.
- For argument validation try to use `rlang` functions as much as possible instead of base R. For example, instead of `time_col %in% names(data)` use `rlang::has_name(data, time_col)`.
- For error messages, try to use `cli::cli_abort()` instead of `stop()`. For example, instead of `stop("Time column '", time_col, "' not found in data")` use `cli::cli_abort("Time column '{time_col}' not found in data")`. This also applies to warnings with `cli::cli_warn()`, info messages, etc... Try to use well formatted cli messages instead of base R messages.
- If the code uses regex, keep in mind that escaped characters must be double-escaped in R strings. For example, to match a dot (`.`) you must use `\\.` in the R string, so `\.` is not enough.
- When defining functions that expect column names as arguments, expect symbolic names (unquoted) instead of strings (quoted). For example, use `function(data, time_col)` instead of `function(data, time_col_name)`. Inside the function then, use `{{ time_col }}` to refer to the column.
- There must not be functions inside functions. All functions must be defined at the top level. This means that if you need a helper function, define it as a private function (with a name starting with a dot) at the end of the script, instead of including its definition inside the function where it's used.
- Always strive to use pipes, native R pipes (`|>`). But in general, use piped expressions as much as possible to improve readability. If you can, make functions ideally look like this:
```
result <- data |>
  dplyr::filter(...) |>
  dplyr::mutate(...) |>
  some_other_function(...) |>
  dplyr::summarise(...) |>
  some_final_function(...)
```
- Make as many functions as necessary to make code look like the above example. Avoid long intermediate expressions that assign to variables, unless necessary for readability.
- For the same purpose, avoid things like for loops. Try to use vectorised operations, use `purrr` if necessary or just plain functions from `dplyr` or `tidyr` to operate on entire columns or datasets at once. Explicit loops should be the last resort.
- Avoid function signatures with excessive arguments (e.g., > 5). Instead, group related arguments into named lists (context objects).
- For column name arguments, strictly use Tidy Evaluation with curly-curly `{{ }}` syntax. To pass these arguments down to helper functions, 'tunnel' them by using `{{ }}` in the function call itself.
- Use `tibble::tribble()` when creating small tibbles in the code if it makes them more readable.
- Try to use stringr package functions instead of base R if you work with strings.
- Use `.by` argument name for grouping arguments, as is done in tidyverse.

## Documentation

- Use roxygen2 for function documentation
- It's enough to document only exported functions. The private ones can remain undocumented. The private functions' names should start with a dot (`.`).
- Finish all doc sentences with full stop
- The first line must be the documentation title. No need to use `@title` tag. Keep it short and start with a verb in imperative form.
- Next part should be the description, starting with `@description` tag. This part can be multiple lines. It can become long if necessary, but don't fill it with useless sentences that say nothing.
- Next part should be the parameters, each starting with `@param` tag. Each parameter should have its own `@param` tag.
- Next part should be the return value, starting with `@return` tag. Describe what the function returns.
- Next part should be the `@export` tag.
- Last part should be the examples, starting with `@examples` tag. Provide meaningful examples that show how to use the function. Avoid unnecessary examples.
- In examples, the last line should show the output of the function, so that when the user runs the example, they can see what the function returns.
- There must be one space after `#'` for each roxygen2 line
- If you start the text for one tag in that same line, then the next lines should be indented two spaces to easily know it's for that same section. For example, if each parameter description starts in the same line as `@param`, then the next lines should be indented two spaces.
- Don't use variable and function names longer than 30 characters. Otherwise lintr fails.

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