# Build gridded atmospheric nitrogen deposition inputs.

Combines HaNi NHx and NOy deposition into a total nitrogen deposition
rate per WHEP grid cell, using the true latitude-dependent 0.5-degree
cell area (from the cell-polity crosswalk) to convert the deposited mass
into a per-hectare rate, and the crosswalk's polity land-area share to
derive the absolute mass a polity receives.

## Usage

``` r
build_n_deposition(years = NULL, data = list(), example = FALSE)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year the inputs cover.

- data:

  Optional named list of pre-loaded inputs: `nhx` and `noy` (each `lon`,
  `lat`, `year`, `value_g`, falling back to
  [`read_n_deposition()`](https://eduaguilera.github.io/whep/reference/read_n_deposition.md)
  when absent) and `cell_polity` (`lon`, `lat`, `area_code`,
  `polity_frac`, `cell_area_ha`, required).

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `area_code`, `year`, `deposition_kgn_ha`,
`deposition_n_t` and `method_deposition`.

## Examples

``` r
build_n_deposition(example = TRUE)
#> # A tibble: 1 × 7
#>     lon   lat area_code  year deposition_kgn_ha deposition_n_t method_deposition
#>   <dbl> <dbl>     <int> <int>             <dbl>          <dbl> <chr>            
#> 1 -0.25 -0.25         1  2020                15           46.2 hani             
```
