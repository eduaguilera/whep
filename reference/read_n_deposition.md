# Read a HaNi atmospheric nitrogen deposition species onto WHEP's grid.

Reads one HaNi NHx or NOy deposition NetCDF (native 5-arcmin grid, total
grams N deposited per native cell per year) and aggregates it to WHEP's
0.5-degree grid by summing the 6x6 fine cells inside each 0.5-degree
block, since the source quantity is an extensive mass. Returns the
summed mass per 0.5-degree cell; converting to a per-hectare rate needs
the true cell area and is done downstream by
[`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md).

## Usage

``` r
read_n_deposition(
  species = c("nhx", "noy"),
  hani_dir = NULL,
  years = NULL,
  example = FALSE
)
```

## Arguments

- species:

  Which HaNi species to read, `"nhx"` or `"noy"`.

- hani_dir:

  Path to the directory holding `ndep_nhx.nc` and `ndep_noy.nc`.
  Defaults to `Sys.getenv("WHEP_HANI_DIR")`.

- years:

  Optional integer vector of calendar years to keep. `NULL` reads every
  year present in the file.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `year`, `value_g` (total grams N deposited
in the 0.5-degree cell that year).

## Examples

``` r
read_n_deposition(example = TRUE)
#> # A tibble: 1 × 4
#>     lon   lat  year  value_g
#>   <dbl> <dbl> <int>    <dbl>
#> 1 -0.25 -0.25  2020 30800000
```
