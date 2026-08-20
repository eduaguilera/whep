# Decompose total territorial N losses into compartments and mechanisms

Runs the cropland
([`decompose_cropland_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_cropland_surplus.md)),
semi-natural
([`decompose_semi_natural_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_semi_natural_surplus.md)),
manure
([`decompose_manure_losses()`](https://eduaguilera.github.io/whep/reference/decompose_manure_losses.md)),
and urban
([`decompose_urban_losses()`](https://eduaguilera.github.io/whep/reference/decompose_urban_losses.md))
decompositions, then combines them into two cumulative, year-on-year
contribution series:

- `by_compartment`: change in total territorial N losses attributed to
  each of the four compartments.

- `by_mechanism`: the same total change regrouped across compartments
  into scale, intensification, and efficiency (population and per-capita
  excretion are grouped under scale, since together they represent total
  human N throughput). Cropland, semi-natural, and manure no longer
  carry a spatial/destiny/species-mix factor (see their own simplified
  decompositions), so no factor currently maps to a "specialization"
  mechanism — that signal now lives only in
  [`decompose_specialization_cov()`](https://eduaguilera.github.io/whep/reference/decompose_specialization_cov.md)
  and
  [`decompose_crop_livestock_conn()`](https://eduaguilera.github.io/whep/reference/decompose_crop_livestock_conn.md).

## Usage

``` r
decompose_terr_losses(n_prov_destiny = NULL, raw = NULL, example = FALSE)
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
`by_mechanism` (each with `t0`, `contribution_mgn`, and
`cumulative_mgn`).

## Examples

``` r
decompose_terr_losses(example = TRUE)
#> $detail
#> # A tibble: 10 × 7
#>    period    factor_label    component_type additive compartment mechanism    t0
#>    <chr>     <chr>           <chr>             <dbl> <chr>       <chr>     <dbl>
#>  1 1860-1861 Size            factor            1004. cropland    Size       1860
#>  2 1860-1861 Intensity       factor            -171. cropland    Intensif…  1860
#>  3 1860-1861 Inefficiency    factor          -17200. cropland    Ineffici…  1860
#>  4 1860-1861 Cropland N sur… target          -16367. cropland    Total      1860
#>  5 1861-1862 Size            factor            1082. cropland    Size       1861
#>  6 1861-1862 Intensity       factor           13085. cropland    Intensif…  1861
#>  7 1861-1862 Inefficiency    factor           32979. cropland    Ineffici…  1861
#>  8 1861-1862 Cropland N sur… target           47146  cropland    Total      1861
#>  9 1860-1861 Size            factor            -287. semi_natur… Size       1860
#> 10 1860-1861 Intensity       factor           -3265. semi_natur… Intensif…  1860
#> 
#> $by_compartment
#> # A tibble: 8 × 4
#>      t0 compartment  contribution_mgn cumulative_mgn
#>   <dbl> <chr>                   <dbl>          <dbl>
#> 1  1860 cropland             -16367.       -16367.  
#> 2  1860 semi_natural         -13936.       -13936.  
#> 3  1860 manure                   74.8          74.8 
#> 4  1860 urban                   -12.6         -12.6 
#> 5  1861 cropland              47146         30779.  
#> 6  1861 semi_natural          99112.        85176   
#> 7  1861 manure                  -71.2           3.61
#> 8  1861 urban                   -12.6         -25.2 
#> 
#> $by_mechanism
#> # A tibble: 6 × 4
#>      t0 mechanism       contribution_mgn cumulative_mgn
#>   <dbl> <chr>                      <dbl>          <dbl>
#> 1  1860 Size                        722.           722.
#> 2  1860 Intensification           -3434.         -3434.
#> 3  1860 Inefficiency             -27529.        -27529.
#> 4  1861 Size                        686.          1408.
#> 5  1861 Intensification           43339.         39905.
#> 6  1861 Inefficiency             102149          74620.
#> 
```
