# Build primary production dataset

Construct the full primary production dataset from raw FAOSTAT inputs.
This is a convenience wrapper that chains the three pipeline steps:

1.  `.read_production()` — read & reformat FAOSTAT data.

2.  `.fix_production()` — apply Global-ported corrections.

3.  `.qc_production()` — flag data-quality anomalies.

## Usage

``` r
build_primary_production(
  start_year = 1850,
  end_year = 2023,
  smooth_carry_forward = FALSE,
  example = FALSE
)
```

## Arguments

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

## Value

A tibble with the same columns as
[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md):
`year`, `area_code` (numeric FAOSTAT), `item_prod_code`,
`item_cbs_code`, `live_anim_code`, `unit`, `value`. Names can be
recovered via
[`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md),
[`add_item_prod_name()`](https://eduaguilera.github.io/whep/reference/add_item_prod_name.md),
etc.

## Examples

``` r
build_primary_production(example = TRUE)
#> # A tibble: 10 × 8
#>     year area_code item_prod_code item_cbs_code live_anim_code unit        value
#>    <dbl>     <dbl> <chr>                  <dbl> <chr>          <chr>       <dbl>
#>  1  1912       165 772                      772 NA             tonnes    3.25e+2
#>  2  2012       112 982                     2848 976            t_head    2.68e-2
#>  3  1943        41 515                     2617 NA             t_ha      6   e-1
#>  4  1979        45 977                     2732 976            tonnes    3.3 e+1
#>  5  1910       141 1098                    2736 1096           t_LU      1.86e-3
#>  6  1867        90 976                      976 NA             heads     1.12e+5
#>  7  1939        15 157                     2537 NA             ha        4.59e+4
#>  8  1935       211 270                     2558 NA             ha        4.02e+3
#>  9  1937         9 772                      772 NA             ha        7.86e+5
#> 10  2000         9 571                     2625 NA             ha        2.36e+2
#> # ℹ 1 more variable: source <chr>
```
