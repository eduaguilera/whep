# Per-capita food protein from the Spain N destiny flows

Converts the `population_food` nitrogen from
[`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md)
/
[`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md)
into per-capita protein supply (nitrogen times 6.25), for comparison
against FAOSTAT
([`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)).

`population_food` is already edible-basis: `.split_food_inedible_loss()`
(applied inside both builders) moves the inedible fraction of every food
item into its own `population_food_inedible` destiny, so the two bases
this function offers are a plain choice of which destinies to sum, not a
second application of `Edible_portion`. Applying `Edible_portion` again
here would double-discount.

## Usage

``` r
build_food_protein_destiny(
  destiny_df,
  population,
  protein_basis = c("edible_portion", "whole_commodity")
)
```

## Arguments

- destiny_df:

  Output of
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md)
  (per province) or
  [`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md)
  (national, `province_name` is `"Spain"`).

- population:

  A tibble with `year`, `province_name`, `population` (head count) –
  e.g. `whep_read_file("population_yg")` renamed from `Year`,
  `Province_name`, `Pop_Mpeop_yg` (which is in millions, so needs
  `* 1e6`), or
  [`read_population()`](https://eduaguilera.github.io/whep/reference/read_population.md)
  filtered to Spain for the national case.

- protein_basis:

  `"edible_portion"` (default) sums only `population_food`, matching
  [`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)'s
  default and its best agreement with FAOSTAT. `"whole_commodity"` adds
  `population_food_inedible` back in, reconstructing the pre-split
  total.

## Value

A tibble keyed by `year`, `province_name` with `protein_g_cap_day` and
`method_protein_basis`.

## Examples

``` r
# The head count is a round illustrative value, not a historical figure;
# the year and province are picked to match the toy destiny fixture.
population <- tibble::tibble(
  year = 1862,
  province_name = "Huesca",
  population = 250000
)
build_food_protein_destiny(
  create_n_prov_destiny(example = TRUE),
  population
)
#> # A tibble: 1 × 4
#>    year province_name protein_g_cap_day method_protein_basis
#>   <dbl> <chr>                     <dbl> <chr>               
#> 1  1862 Huesca                  0.00579 edible_portion      
build_food_protein_destiny(
  create_n_prov_destiny(example = TRUE),
  population,
  protein_basis = "whole_commodity"
)
#> # A tibble: 1 × 4
#>    year province_name protein_g_cap_day method_protein_basis
#>   <dbl> <chr>                     <dbl> <chr>               
#> 1  1862 Huesca                  0.00643 whole_commodity     
```
