# Compute the national cropland destiny mix over time

Computes, per year, the share of Spain's cropland output (by mass N)
going to each destiny — domestic food, feed, exported food, non-food —
from
[`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md)'s
national commodity balance sheets. Deliberately *not*
[`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md)'s
provincial data summed up: the provincial `"export"` destiny does not
distinguish inter-provincial trade from true international export (per
[`decompose_crop_livestock_conn()`](https://eduaguilera.github.io/whep/reference/decompose_crop_livestock_conn.md)'s
same caveat on `"Outside"` imports), which would overstate the export
share here.
[`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md)
instead recomputes export/import directly from the national
production-vs-consumption balance per item, so trade between two Spanish
provinces nets out rather than counting as export. This is the
supplementary diagnostic recommended in the decomposition proposal
(section 14, "attach to existing figures"), to show the local-food -\>
feed + export transition directly, since
[`decompose_cropland_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_cropland_surplus.md)
no longer carries a destiny factor.

## Usage

``` r
decompose_destiny_mix(n_nat_destiny = NULL, example = FALSE)
```

## Arguments

- n_nat_destiny:

  National nitrogen flows tibble from
  [`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md).
  If `NULL`, computed automatically (slow).

- example:

  If `TRUE`, return a small hardcoded output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with columns `year`, `destiny_grp` (one of `"domestic_food"`,
`"feed"`, `"exported"`, `"non_food"`), `output_mg`, and `share` (that
destiny's share of the year's total cropland output).

## Examples

``` r
decompose_destiny_mix(example = TRUE)
#> # A tibble: 8 × 4
#>    year destiny_grp   output_mg  share
#>   <dbl> <chr>             <dbl>  <dbl>
#> 1  1960 exported         54087. 0.155 
#> 2  1960 feed            188775  0.540 
#> 3  1960 domestic_food   101884  0.292 
#> 4  1960 non_food          4554. 0.013 
#> 5  2000 feed            426311  0.556 
#> 6  2000 domestic_food   119629  0.156 
#> 7  2000 exported        205771  0.269 
#> 8  2000 non_food         14476. 0.0189
```
