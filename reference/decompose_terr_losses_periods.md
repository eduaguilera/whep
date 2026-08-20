# Decompose territorial N losses by reference period (chained)

Runs the same four compartments as
[`decompose_terr_losses()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses.md),
but comparing each reference period (each averaged across its ten years)
against the immediately preceding one — 1860-1870 -\> 1920-1930 -\>
1960-1970 -\> 2010-2020 — plus one extra transition spanning the full
analysis window, 1860-1870 straight to 2010-2020 (the total change),
instead of chaining year on year. This is the periodised table
recommended alongside the main chained figure in the decomposition
proposal (section 12), summarizing the four historical phases rather
than following the full 160-year trajectory.

## Usage

``` r
decompose_terr_losses_periods(
  n_prov_destiny = NULL,
  raw = NULL,
  example = FALSE
)
```

## Arguments

- n_prov_destiny:

  Nitrogen flows tibble from
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md),
  shared across all four compartments. If `NULL`, loaded automatically.

- raw:

  Named list overriding any of the raw inputs shared across compartments
  (`npp_ygpit`, `codes_coefs`, `intake_ygiac`, `n_excretion_ygs`,
  `stock_prod_ygps`, `livestock_units`, `population_yg`). Missing
  elements are loaded automatically.

- example:

  If `TRUE`, return a small hardcoded output without downloading remote
  data. Default is `FALSE`.

## Value

A named list with tibbles `detail` (per-compartment LMDI output, with
`compartment` and `mechanism` columns added), `by_compartment`, and
`by_mechanism` (each with `period` — one of "1865-1925", "1925-1965",
"1965-2015", "Total (1865-2015)", the mean year of each reference window
(1865 = mean of 1860-1870, and so on) — plus `contribution_mgn`,
`period_years`, and `contribution_per_yr_mgn`, the per-year-normalized
value used for plotting).

## Examples

``` r
decompose_terr_losses_periods(example = TRUE)
#> $detail
#> # A tibble: 8 × 6
#>   period    factor_label           component_type additive compartment mechanism
#>   <chr>     <chr>                  <chr>             <dbl> <chr>       <chr>    
#> 1 1860-1920 Size                   factor           33253. cropland    Size     
#> 2 1860-1920 Intensity              factor           16268. cropland    Intensif…
#> 3 1860-1920 Inefficiency           factor          -53899. cropland    Ineffici…
#> 4 1860-1920 Cropland N surplus     target           -4379. cropland    Total    
#> 5 1860-1920 Size                   factor          -15150. semi_natur… Size     
#> 6 1860-1920 Intensity              factor            9236. semi_natur… Intensif…
#> 7 1860-1920 Inefficiency           factor           53368. semi_natur… Ineffici…
#> 8 1860-1920 Semi-natural N surplus target           47454. semi_natur… Total    
#> 
#> $by_compartment
#> # A tibble: 8 × 5
#>   period compartment  contribution_mgn period_years contribution_per_yr_mgn
#>    <dbl> <chr>                   <dbl>        <dbl>                   <dbl>
#> 1      1 cropland               -4379.           60                   -73.0
#> 2      2 cropland              395464            40                  9887. 
#> 3      1 semi_natural           47454.           60                   791. 
#> 4      2 semi_natural          163739            40                  4093. 
#> 5      1 manure                 -1649.           60                   -27.5
#> 6      2 manure                 33043.           40                   826. 
#> 7      1 urban                  62451.           60                  1041. 
#> 8      2 urban                  33567            40                   839. 
#> 
#> $by_mechanism
#> # A tibble: 6 × 5
#>   period mechanism       contribution_mgn period_years contribution_per_yr_mgn
#>    <dbl> <chr>                      <dbl>        <dbl>                   <dbl>
#> 1      1 Size                      80496.           60                  1342. 
#> 2      1 Intensification           25861.           60                   431. 
#> 3      1 Inefficiency              -2601.           60                   -43.3
#> 4      2 Size                      39875.           40                   997. 
#> 5      2 Intensification          364865            40                  9122. 
#> 6      2 Inefficiency             221223            40                  5531. 
#> 
```
