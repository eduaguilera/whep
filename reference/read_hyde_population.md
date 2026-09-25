# Read gridded HYDE population onto WHEP's grid.

Reads a HYDE baseline-scenario population count (native 5-arcmin ESRI
ASCII grid, people per native cell) for one or more calendar years and,
by default, aggregates it to WHEP's 0.5-degree grid by summing the 6x6
fine cells inside each 0.5-degree block, since population count is an
extensive quantity. Each requested year is read from its own
`"{year}AD_pop.zip"` archive, which holds the total (`popc`), urban
(`urbc`) and rural (`rurc`) counts on the same grid.

## Usage

``` r
read_hyde_population(
  hyde_dir = NULL,
  years = NULL,
  variable = c("total", "urban", "rural"),
  aggregate = TRUE,
  example = FALSE
)
```

## Arguments

- hyde_dir:

  Path to the directory holding the HYDE `"{year}AD_pop.zip"` archives.
  Defaults to `Sys.getenv("WHEP_HYDE_DIR")`.

- years:

  Integer vector of calendar years to read (`AD`, so `>= 1`). Required:
  each year is a real unzip-and-parse of a ~150MB archive, so there is
  no default range.

- variable:

  Which population count to read: `"total"` (default, HYDE `popc`),
  `"urban"` (`urbc`) or `"rural"` (`rurc`). The output column is named
  after it.

- aggregate:

  If `TRUE` (default), sum the fine cells to WHEP's 0.5-degree grid. If
  `FALSE`, return the native 5-arcmin cells, keyed by their own centres,
  for a consumer whose target grid is not 0.5 degrees.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `year` and one count column named after
`variable`: `total_pop`, `urban_pop` or `rural_pop` (people in the cell
that year). Cells HYDE marks as no-data are absent, never zero.

## Examples

``` r
read_hyde_population(example = TRUE)
#> # A tibble: 1 × 4
#>     lon   lat  year total_pop
#>   <dbl> <dbl> <int>     <dbl>
#> 1 -0.25 -0.25  2020     12000
read_hyde_population(variable = "urban", example = TRUE)
#> # A tibble: 1 × 4
#>     lon   lat  year urban_pop
#>   <dbl> <dbl> <int>     <dbl>
#> 1 -0.25 -0.25  2020     12000
```
