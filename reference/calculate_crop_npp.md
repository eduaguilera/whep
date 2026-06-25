# Estimate total crop net primary production.

Assembles total crop net primary production (product plus residue plus
root dry matter) by running
[`calculate_crop_residues()`](https://eduaguilera.github.io/whep/reference/calculate_crop_residues.md)
then
[`calculate_crop_roots()`](https://eduaguilera.github.io/whep/reference/calculate_crop_roots.md).

## Usage

``` r
calculate_crop_npp(
  x,
  residue_method = "ensemble",
  root_method = "ensemble",
  weights = list(w_ipcc = 0.5, w_ref = 0.5)
)
```

## Arguments

- x:

  A tibble with `item_prod_code`, `production_t` and `area_ha`, plus any
  optional adjustment columns used by the residue and root steps.

- residue_method:

  Residue method passed to
  [`calculate_crop_residues()`](https://eduaguilera.github.io/whep/reference/calculate_crop_residues.md).

- root_method:

  Root method passed to
  [`calculate_crop_roots()`](https://eduaguilera.github.io/whep/reference/calculate_crop_roots.md).

- weights:

  Named list of ensemble weights; `w_ipcc` for residues and `w_ref` for
  roots (each 0-1, default 0.5).

## Value

The input tibble with `product_dm_t`, `yield_dm_t_ha`, `residue_dm_t`,
`root_dm_t`, `crop_npp_dm_t`, `method_residue` and `method_root`.

## Examples

``` r
calculate_crop_npp(
  tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
)
#> # A tibble: 1 × 10
#>   item_prod_code production_t area_ha product_dm_t yield_dm_t_ha residue_dm_t
#>   <chr>                 <dbl>   <dbl>        <dbl>         <dbl>        <dbl>
#> 1 15                      100      40         87.9          2.20         136.
#> # ℹ 4 more variables: method_residue <chr>, root_dm_t <dbl>, method_root <chr>,
#> #   crop_npp_dm_t <dbl>
```
