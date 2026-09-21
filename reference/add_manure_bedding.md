# Add bedding carbon and nitrogen to the housed manure streams.

Places the bedding supply from
[`build_residue_bedding_supply()`](https://eduaguilera.github.io/whep/reference/build_residue_bedding_supply.md)
onto the litter-using, non-grazing streams of a
[`split_manure_management()`](https://eduaguilera.github.io/whep/reference/split_manure_management.md)
result, in proportion to the nitrogen each of those streams already
carries. Grazing deposition (`Pasture/Range/Paddock`) never receives
bedding, which is what keeps its manure the fresh excreta it is.

The result is the input rows plus `dm_bedding`, `n_bedding` and
`c_bedding`, which
[`apply_management_losses()`](https://eduaguilera.github.io/whep/reference/apply_management_losses.md)
then adds to the manure applied to land.

## Usage

``` r
add_manure_bedding(split, bedding, options = list())
```

## Arguments

- split:

  A tibble from
  [`split_manure_management()`](https://eduaguilera.github.io/whep/reference/split_manure_management.md).

- bedding:

  A tibble from
  [`build_residue_bedding_supply()`](https://eduaguilera.github.io/whep/reference/build_residue_bedding_supply.md),
  keyed on the `year x territory` (x `sub_territory`) columns it shares
  with `split`.

- options:

  A named list. `mms_bedding` selects the litter-using systems:
  `"ipcc_2019"` (default) or `"with_daily_spread"`; see the section
  above.

## Value

The `split` rows with `dm_bedding`, `n_bedding`, `c_bedding` and
`method_bedding_mms`.

## Which systems use litter

IPCC 2019 Refinement Vol. 4 Ch. 10 p. 10.94 defines `NbeddingMS` as the
bedding nitrogen "to be applied for solid storage and deep bedding MMS
if known organic bedding usage". WHEP's `"Poultry Manure"` is the
deep-litter system (it takes its EF3 from the IPCC
`"Poultry Manure - Deep Litter"` row), so the IPCC pair is
`"Solid Storage"` and `"Poultry Manure"` and that is the default.
`"with_daily_spread"` additionally beds `"Daily Spread"`, the manure
scraped from a barn and spread the same day, which is bedded in practice
but which IPCC does not name; it is selectable rather than default for
that reason. Liquid and lagoon systems are excluded under both, since
straw cannot be pumped.

Moving bedding onto `"Daily Spread"` as well spreads the same mass over
more nitrogen, so it lowers the bedding placed per tonne of
solid-storage manure and raises the applied C:N of the daily-spread
stream instead; it moves no total.

## Examples

``` r
excretion <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~livestock_category,
  ~n_excretion, ~c_excretion, ~vs_excretion,
  2020L, "203", NA, "Cattle_milk", 100, 1000, 210
)
bedding <- tibble::tibble(
  year = 2020L, territory = "203",
  bedding_dm_t = 50, bedding_c_t = 22, bedding_n_t = 0.3
)
add_manure_bedding(split_manure_management(excretion), bedding)
#> # A tibble: 5 × 17
#>    year territory sub_territory livestock_category species_gen loss_category
#>   <int> <chr>     <lgl>         <chr>              <chr>       <chr>        
#> 1  2020 203       NA            Cattle_milk        Cattle      Dairy Cattle 
#> 2  2020 203       NA            Cattle_milk        Cattle      Dairy Cattle 
#> 3  2020 203       NA            Cattle_milk        Cattle      Dairy Cattle 
#> 4  2020 203       NA            Cattle_milk        Cattle      Dairy Cattle 
#> 5  2020 203       NA            Cattle_milk        Cattle      Dairy Cattle 
#> # ℹ 11 more variables: cn_species <chr>, mms_type <chr>, stream <chr>,
#> #   n_stream <dbl>, c_stream <dbl>, vs_stream <dbl>, method_mms <chr>,
#> #   dm_bedding <dbl>, c_bedding <dbl>, n_bedding <dbl>,
#> #   method_bedding_mms <chr>
```
