# MANNER process-based ammonia-volatilisation factors.

Long-form lookup of the discrete factor tables of the MANNER ammonia
volatilisation model. Each row gives one factor, identified by its table
(`category`), the lookup key (`key`) and, where the table is
two-dimensional, a second key (`sub_key`). The tables are: the
per-fertiliser maximum ammonia-loss ceiling (`max_nh3`); the soil-pH
factor for synthetic fertiliser (`ph`, sub-keyed by pH band); the
incorporation / land-use factor (`incorporation`); the manure
application-technique factor (`technique`); the wind-speed factor
(`windspeed`); and the combined rainfall-by-wetness factor
(`rainfall_wet`). Continuous fertiliser-rate, temperature and rainfall
response surfaces are computed in the MANNER function and are not part
of this table.

## Usage

``` r
manner_params
```

## Format

A tibble with columns:

- category:

  Factor table: one of `"max_nh3"`, `"ph"`, `"incorporation"`,
  `"technique"`, `"windspeed"`, `"rainfall_wet"`.

- key:

  Primary lookup key (fertiliser, technique, wind-speed class, or
  rainfall-wetness class).

- sub_key:

  Secondary lookup key (pH band for the `ph` table); `NA` for
  one-dimensional tables.

- factor:

  Numeric multiplicative factor.

## Source

Nicholson, F. A., Bhogal, A., Chadwick, D., Gill, E., Gooday, R. D.,
Lord, E., Misselbrook, T., Rollett, A. J., Sagoo, E., Smith, K. A.,
Thorman, R. E., Williams, J. R. & Chambers, B. J. (2013). An enhanced
software tool to support better use of manure nutrients: MANNER-NPK.
*Soil Use and Management*, 29(4), 473-484.
[doi:10.1111/sum.12078](https://doi.org/10.1111/sum.12078) . Underlying
mass-flow ammonia framework: Webb, J. & Misselbrook, T. H. (2004). A
mass-flow model of ammonia emissions from UK livestock production.
*Atmospheric Environment*, 38(14), 2163-2176.
[doi:10.1016/j.atmosenv.2004.01.023](https://doi.org/10.1016/j.atmosenv.2004.01.023)
. Values transcribed from the Spain historical MANNER implementation
(`MANNER_model.R`).

## Examples

``` r
manner_params
#> # A tibble: 33 × 4
#>    category key   sub_key  factor
#>    <chr>    <chr> <chr>     <dbl>
#>  1 max_nh3  Urea  NA         0.45
#>  2 max_nh3  AN    NA         0.04
#>  3 max_nh3  CAN   NA         0.04
#>  4 max_nh3  AS    NA         0.45
#>  5 ph       Urea  pH<7       1   
#>  6 ph       Urea  other pH   1   
#>  7 ph       AN    pH<7       1   
#>  8 ph       AN    other pH   1   
#>  9 ph       CAN   pH<7       1   
#> 10 ph       CAN   other pH   1   
#> # ℹ 23 more rows
```
