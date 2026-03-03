# Harmonize rows labeled "simple" by summing values

Sum `value` for rows where `type == "simple"`. This covers both 1:1 and
N:1 item mappings, since in both cases the values are simply summed. The
results are grouped by `item_code_harm`, `year` and any additional
grouping columns supplied via `...`.

## Usage

``` r
harmonize_simple(data, ...)
```

## Arguments

- data:

  A data frame containing at least columns:

  - `item_code_harm`: Numeric, code for harmonized item.

  - `year`: Numeric, year of observation.

  - `value`: Numeric, value of observation.

  - `type`: String, harmonization type. Only `"simple"` rows are used.

- ...:

  Additional grouping columns supplied as bare names.

## Value

A tibble with columns:

- `item_code_harm`: Numeric, code for harmonized item.

- `year`: Numeric, year of observation.

- `value`: Numeric, summed value of observation.

- and any additional grouping columns.

## Examples

``` r
# 1:1 mapping: one original item -> one harmonized code
df_one_to_one <- tibble::tribble(
  ~item_code_harm, ~year, ~value, ~type,
  1,               2000,  10,     "simple",
  2,               2000,   3,     "simple",
  1,               2001,  12,     "simple",
  2,               2001,   5,     "simple"
)
harmonize_simple(df_one_to_one)
#> # A tibble: 4 × 3
#>   item_code_harm  year value
#>            <dbl> <dbl> <dbl>
#> 1              1  2000    10
#> 2              2  2000     3
#> 3              1  2001    12
#> 4              2  2001     5

# N:1 mapping: multiple items map to the same code
df_many_to_one <- tibble::tribble(
  ~item_code_harm, ~year, ~value, ~type,
  1,               2000,   4,     "simple",
  1,               2000,   6,     "simple",
  2,               2000,   3,     "simple"
)
harmonize_simple(df_many_to_one)
#> # A tibble: 2 × 3
#>   item_code_harm  year value
#>            <dbl> <dbl> <dbl>
#> 1              1  2000    10
#> 2              2  2000     3

# With an extra grouping column (e.g. country)
df_grouped <- tibble::tribble(
  ~item_code_harm, ~year, ~value, ~type,    ~country,
  1,               2000,   4,     "simple", "usa",
  1,               2000,   6,     "simple", "usa",
  1,               2000,   9,     "simple", "germany",
  2,               2000,   3,     "simple", "usa"
)
harmonize_simple(df_grouped, country)
#> # A tibble: 3 × 4
#>   item_code_harm  year country value
#>            <dbl> <dbl> <chr>   <dbl>
#> 1              1  2000 usa        10
#> 2              1  2000 germany     9
#> 3              2  2000 usa         3

# Rows with type != "simple" are ignored
df_mixed <- tibble::tribble(
  ~item_code_harm, ~year, ~value, ~type,
  1,               2000,  10,     "simple",
  1,               2000,  99,     "1:n",
  2,               2000,   3,     "simple"
)
harmonize_simple(df_mixed)
#> # A tibble: 2 × 3
#>   item_code_harm  year value
#>            <dbl> <dbl> <dbl>
#> 1              1  2000    10
#> 2              2  2000     3
```
