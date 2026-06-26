# Build a consumption land footprint by physical trade balance.

End-to-end land-balance footprint for one year on real WHEP data:
assemble production (primary crop production plus grass dry-matter
derived from grassland area times yield), the bilateral trade network,
and the crop-plus-grassland direct-land extension, then trace land to
consumers with
[`compute_footprint_balance()`](https://eduaguilera.github.io/whep/reference/compute_footprint_balance.md).
This is the independent, non-Leontief estimator for stress-testing the
multi-regional input-output footprint via
[`compare_footprint_methods()`](https://eduaguilera.github.io/whep/reference/compare_footprint_methods.md).

Grass items (`item_cbs_code` 3000 and 3002) are barely traded, so their
land stays with the producing country: the balance, unlike the
input-output model, does not route grass through the grass-to-livestock
chain. Any `production`, `trade` or `extension` supplied explicitly is
used as-is instead of being built, which is useful for reusing cached
inputs or for testing.

## Usage

``` r
build_land_balance_footprint(
  year,
  production = NULL,
  trade = NULL,
  extension = NULL,
  example = FALSE
)
```

## Arguments

- year:

  Year to build the footprint for.

- production:

  Optional production tibble (`area_code`, `item_cbs_code`, `value`);
  built when `NULL`.

- trade:

  Optional trade tibble (`from_code`, `to_code`, `item_cbs_code`,
  `value`); built when `NULL`.

- extension:

  Optional direct-land tibble (`area_code`, `item_cbs_code`, `value`);
  built when `NULL`.

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble as returned by
[`compute_footprint_balance()`](https://eduaguilera.github.io/whep/reference/compute_footprint_balance.md).

## Examples

``` r
build_land_balance_footprint(example = TRUE)
#> # A tibble: 10 × 4
#>    area_code item_cbs_code        value method      
#>        <int>         <int>        <dbl> <chr>       
#>  1        10          3000 328899491    land_balance
#>  2        41          3000 391524410    land_balance
#>  3       101          2615    109730    land_balance
#>  4       114           772       962    land_balance
#>  5       122          2520      3865    land_balance
#>  6       126          2617      6491    land_balance
#>  7       137          2613        40.6  land_balance
#>  8       179          2514      5009    land_balance
#>  9       188          2535         1.83 land_balance
#> 10       236          2537      1304    land_balance
```
