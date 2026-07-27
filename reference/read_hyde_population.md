# Read gridded HYDE urban population onto WHEP's grid.

Reads the HYDE baseline-scenario urban population count (`urbc`, native
5-arcmin ESRI ASCII grid, total people per native cell) for one or more
calendar years and aggregates it to WHEP's 0.5-degree grid by summing
the 6x6 fine cells inside each 0.5-degree block, since population count
is an extensive quantity. Each requested year is read from its own
`"{year}AD_pop.zip"` archive.

## Usage

``` r
read_hyde_population(hyde_dir = NULL, years = NULL, example = FALSE)
```

## Arguments

- hyde_dir:

  Path to the directory holding the HYDE `"{year}AD_pop.zip"` archives.
  Defaults to `Sys.getenv("WHEP_HYDE_DIR")`.

- years:

  Integer vector of calendar years to read (`AD`, so `>= 1`). Required:
  each year is a real unzip-and-parse of a ~150MB archive, so there is
  no default range.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `year`, `urban_pop` (total urban population
in the 0.5-degree cell that year).

## Examples

``` r
read_hyde_population(example = TRUE)
#> # A tibble: 1 × 4
#>     lon   lat  year urban_pop
#>   <dbl> <dbl> <int>     <dbl>
#> 1 -0.25 -0.25  2020     12000
```
