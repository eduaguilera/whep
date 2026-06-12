# Build commodity balance sheets

Construct commodity balance sheets (CBS) from raw FAOSTAT data. This is
a convenience wrapper that chains the three pipeline steps:

1.  `.read_cbs()` — read & reformat FAOSTAT CBS data.

2.  `.fix_cbs()` — processing calibration, trade imputation, destiny
    filling, and final balancing.

3.  `.qc_cbs()` — flag data-quality anomalies.

## Usage

``` r
build_commodity_balances(
  primary_all,
  start_year = 1850,
  end_year = 2023,
  smooth_carry_forward = FALSE,
  example = FALSE,
  .fixed_data = NULL
)
```

## Arguments

- primary_all:

  A tibble of primary production, as returned by
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md).

- start_year:

  Integer. First year to include. Default `1850`.

- end_year:

  Integer. Last year to include. Default `2023`.

- smooth_carry_forward:

  Logical. If `TRUE`, carry-forward tails are replaced with a linear
  trend. Default `FALSE`.

- example:

  Logical. If `TRUE`, return a small hardcoded example tibble instead of
  reading remote data. Default `FALSE`.

- .fixed_data:

  Optional tibble with the same structure as the output of the internal
  `.read_cbs() |> .fix_cbs()` steps. When supplied, `primary_all` is
  ignored and the pipeline skips directly to `.qc_cbs()`. Default
  `NULL`.

## Value

A tibble in long format with columns: `year`, legacy numeric
`area_code`, numeric `polity_area_code`, `reporting_polity_code`,
`reporting_polity_name`, `reporting_polity_has_geometry`,
`item_cbs_code`, `element` (e.g. `"production"`, `"import"`, `"food"`),
`value`, `source`, and `fao_flag`.

## Examples

``` r
build_commodity_balances(example = TRUE)
#> # A tibble: 10 × 11
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name 
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                 
#>  1  2010       120              120 LAO-1954-2025         Laos                  
#>  2  1981       222              222 TUN-1881-2025         Tunisia               
#>  3  1906       203              203 ESP-1800-2025         Spain                 
#>  4  1899       175              175 GNB-1886-1974         Guinea-Bissau (1886-1…
#>  5  2018        48               48 CRI-1800-2025         Costa Rica            
#>  6  1871        10               10 AUS-1800-1901         Australian Colonies (…
#>  7  1938       226              226 UGA-1926-1962         Uganda (1926-1962)    
#>  8  1924        11               11 AUT-1919-2025         Austria               
#>  9  1928        96               96 HKG-1842-2025         Hong Kong             
#> 10  1879       236              236 VEN-1821-2025         Venezuela             
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>, item_cbs_code <dbl>,
#> #   element <chr>, value <dbl>, source <chr>, fao_flag <chr>
```
