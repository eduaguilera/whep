# Harmonize advanced cases with interpolation for 1:N groups

Harmonize data containing `"simple"` and `"1:n"` mappings. `"simple"`
covers both 1:1 and N:1 relationships (values are summed). For `"1:n"`
groups (one original item splits into several harmonized items) this
function computes value shares across the full year range, interpolates
missing shares, and applies them to split values.

**Important for 1:n mappings**: For each original item that splits into
multiple harmonized items (e.g., "wheatrice" into "wheat" and "rice"),
provide **one row per target `item_code_harm`**. Each row should have
the same `item`, `year`, and `value`, differing only in
`item_code_harm`. For example, to disaggregate "wheatrice":

- Row 1: `item` = "wheatrice", `item_code_harm` = 1

- Row 2: `item` = "wheatrice", `item_code_harm` = 2

Do not provide a single row; the function will not create duplicates
automatically.

## Usage

``` r
harmonize_interpolate(data, ...)
```

## Arguments

- data:

  A data frame containing at least columns:

  - `item`: String, original item name.

  - `item_code_harm`: Numeric, code for harmonized item.

  - `year`: Numeric, year of observation.

  - `value`: Numeric, value of observation.

  - `type`: String, `"simple"` or `"1:n"`.

- ...:

  Additional grouping columns provided as bare names.

## Value

A tibble with columns:

- `item_code`: Numeric, code for harmonized item.

- `year`: Numeric, year of observation.

- `value`: Numeric, summed value of observation.

- and any additional grouping columns.

## Examples

``` r
# Simple-only data (no 1:n rows)
df_simple <- tibble::tribble(
  ~item,      ~item_code_harm, ~year, ~value, ~type,
  "wheat",    1,               2000,   5,     "simple",
  "barley",   2,               2000,   3,     "simple",
  "oats",     2,               2000,   2,     "simple"
)
harmonize_interpolate(df_simple)
#> ℹ Only simple harmonization detected, returning simple harmonizations only.
#> # A tibble: 2 × 3
#>   item_code  year value
#>       <dbl> <dbl> <dbl>
#> 1         1  2000     5
#> 2         2  2000     5

# Mixed simple + 1:n data
df_mixed <- tibble::tribble(
  ~item,       ~item_code_harm, ~year, ~value, ~type,
  "wheatrice", 1,               2000,  20,     "1:n",
  "wheatrice", 2,               2000,  20,     "1:n",
  "wheat",     1,               2000,   8,     "simple",
  "rice",      2,               2000,  12,     "simple"
)
harmonize_interpolate(df_mixed)
#> # A tibble: 2 × 3
#>   item_code  year value
#>       <dbl> <dbl> <dbl>
#> 1         1  2000    16
#> 2         2  2000    24

# Multiple years with share interpolation
# Shares are known in 2000 and 2002; 2001 is interpolated.
df_years <- tibble::tribble(
  ~item,       ~item_code_harm, ~year, ~value, ~type,
  "wheat",     1,               2000,   6,     "simple",
  "rice",      2,               2000,   4,     "simple",
  "wheatrice", 1,               2001,  10,     "1:n",
  "wheatrice", 2,               2001,  10,     "1:n",
  "wheat",     1,               2002,   8,     "simple",
  "rice",      2,               2002,   2,     "simple"
)
harmonize_interpolate(df_years)
#> # A tibble: 6 × 3
#>   item_code  year value
#>       <dbl> <dbl> <dbl>
#> 1         1  2001     7
#> 2         2  2001     3
#> 3         1  2000     6
#> 4         2  2000     4
#> 5         1  2002     8
#> 6         2  2002     2

# With extra grouping columns
df_grouped <- tibble::tribble(
  ~item,       ~item_code_harm, ~year, ~value, ~type,    ~country,
  "wheat",     1,               2000,   6,     "simple", "usa",
  "rice",      2,               2000,   4,     "simple", "usa",
  "wheatrice", 1,               2001,  10,     "1:n",    "usa",
  "wheatrice", 2,               2001,  10,     "1:n",    "usa",
  "wheat",     1,               2002,   8,     "simple", "usa",
  "rice",      2,               2002,   2,     "simple", "usa",
  "wheat",     1,               2002,   8,     "simple", "germany"
)
harmonize_interpolate(df_grouped, country)
#> # A tibble: 7 × 4
#>   item_code  year country value
#>       <dbl> <dbl> <chr>   <dbl>
#> 1         1  2001 usa         7
#> 2         2  2001 usa         3
#> 3         1  2000 usa         6
#> 4         2  2000 usa         4
#> 5         1  2002 usa         8
#> 6         2  2002 usa         2
#> 7         1  2002 germany     8
```
