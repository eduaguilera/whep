# Apply IPCC manure-management losses to the collected manure streams.

Nets the nitrogen surviving manure management onto the field, applying
the IPCC 2019 management-loss fractions to the collected/housed streams
from
[`split_manure_management()`](https://eduaguilera.github.io/whep/reference/split_manure_management.md):
`applied_n = n_stream * (1 - FracLossMS)` where
`FracLossMS = FracGasMS + FracLeachMS + EF3 + FracN2MS`. The grazing
(pasture/range/paddock) stream is deposited in situ and keeps its full
nitrogen (its in-situ soil losses belong to the soil stage). Indirect
N2O is reported as a labelled sub-flux of the already-removed
volatilized and leached nitrogen (the same N is not removed twice).
Carbon applied to the field is `applied_n` times the post-storage manure
C:N (the solid/liquid/excreta value for the stream's management system),
so the applied C:N reflects storage, not fresh excreta; the carbon and
volatile-solids storage losses follow from that.

## Usage

``` r
apply_management_losses(split, options = list())
```

## Arguments

- split:

  A tibble from
  [`split_manure_management()`](https://eduaguilera.github.io/whep/reference/split_manure_management.md).

- options:

  A named list. `method` selects the loss method (`"ipcc_2019_tier2"`).

## Value

The input rows with `manure_type`, `applied_n`, `applied_c`,
`applied_vs`, `n_volatilized`, `n_leached`, `n2o_direct_n`, `n2_n`,
`n2o_indirect_n`, `c_lost`, `vs_destroyed` and `method_losses`.

## Examples

``` r
excretion <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~livestock_category,
  ~n_excretion, ~c_excretion, ~vs_excretion,
  2020L, "ES", NA, "Cattle_milk", 100, 1900, 60
)
apply_management_losses(split_manure_management(excretion))
#> # A tibble: 4 × 19
#>    year territory sub_territory livestock_category species_gen mms_type         
#>   <int> <chr>     <lgl>         <chr>              <chr>       <chr>            
#> 1  2020 ES        NA            Cattle_milk        Cattle      Pasture/Range/Pa…
#> 2  2020 ES        NA            Cattle_milk        Cattle      Solid Storage    
#> 3  2020 ES        NA            Cattle_milk        Cattle      Liquid/Slurry    
#> 4  2020 ES        NA            Cattle_milk        Cattle      Daily Spread     
#> # ℹ 13 more variables: manure_type <chr>, stream <chr>, applied_n <dbl>,
#> #   applied_c <dbl>, applied_vs <dbl>, n_volatilized <dbl>, n_leached <dbl>,
#> #   n2o_direct_n <dbl>, n2_n <dbl>, n2o_indirect_n <dbl>, c_lost <dbl>,
#> #   vs_destroyed <dbl>, method_losses <chr>
```
