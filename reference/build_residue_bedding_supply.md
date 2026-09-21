# Build the bedding straw supply reaching the managed manure chain.

Converts the bedding destiny of crop residues
([`calculate_residue_destinies()`](https://eduaguilera.github.io/whep/reference/calculate_residue_destinies.md)'s
`residue_bedding_dm_t`) into the dry matter, carbon and nitrogen that
[`add_manure_bedding()`](https://eduaguilera.github.io/whep/reference/add_manure_bedding.md)
adds to the housed manure streams. Composition comes from the same
`bio_coefs` residue coefficients
[`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md)
uses for the residue that stays on the field (`residue_c_kgdm`,
`residue_n_kgdm`, joined on `item_prod_code`), so the straw that goes
through the yard and the straw that does not are described by one table.

The supply is aggregated over crops, because bedding is not traceable to
the crop it came from once it is in the heap: the manure chain keys on
`year x territory` (and `sub_territory` when the input carries one).

## Usage

``` r
build_residue_bedding_supply(x)
```

## Arguments

- x:

  A tibble from
  [`calculate_residue_destinies()`](https://eduaguilera.github.io/whep/reference/calculate_residue_destinies.md)
  with `item_prod_code`, `residue_bedding_dm_t`, `year` and `territory`,
  and optionally `sub_territory`.

## Value

A tibble with one row per `year x territory` (x `sub_territory`) and
columns `bedding_dm_t`, `bedding_c_t` and `bedding_n_t`.

## Examples

``` r
tibble::tibble(
  year = 2020L, territory = "203", item_prod_code = "15",
  residue_bedding_dm_t = 1000
) |>
  build_residue_bedding_supply()
#> # A tibble: 1 × 5
#>    year territory bedding_dm_t bedding_c_t bedding_n_t
#>   <int> <chr>            <dbl>       <dbl>       <dbl>
#> 1  2020 203               1000        458.        5.92
```
