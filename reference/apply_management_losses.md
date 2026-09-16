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
Carbon applied to the field is `c_stream * (1 - c_loss_fraction)`, the
stream's carbon less what its management system mineralises in storage,
and the volatile solids are scaled by the same ratio. The applied C:N is
therefore a RESULT of that loss rather than an imposed cap. Loss
fractions and their sources are in
`inst/extdata/manure/manure_storage_c_loss.csv`: nothing for grazing or
daily spread, which have no storage stage; 0.420 of initial carbon for
solid storage and 0.424 for poultry litter (Pardo et al. 2015,
[doi:10.1111/gcb.12806](https://doi.org/10.1111/gcb.12806) ); 0.110
(cattle) and 0.128 (pigs) for slurry, derived from Kupper et al. 2020
([doi:10.1016/j.agee.2020.106963](https://doi.org/10.1016/j.agee.2020.106963)
) using this package's own 0.47 kg C per kg volatile solids.

Until whep#1006 this was a cap,
`pmin(c_stream, applied_n * post-storage C:N)`, which produced a loss
only as a side effect of holding the applied C:N down. That made the
loss depend on the excreted composition rather than on the storage
system: when excreted carbon moved, the reported loss for cattle solid
storage fell from 40.3% to 6.9% with no coefficient changing.

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
  2020L, "203", NA, "Cattle_milk", 100, 1900, 60
)
apply_management_losses(split_manure_management(excretion))
#> # A tibble: 4 × 19
#>    year territory sub_territory livestock_category species_gen mms_type         
#>   <int> <chr>     <lgl>         <chr>              <chr>       <chr>            
#> 1  2020 203       NA            Cattle_milk        Cattle      Pasture/Range/Pa…
#> 2  2020 203       NA            Cattle_milk        Cattle      Solid Storage    
#> 3  2020 203       NA            Cattle_milk        Cattle      Liquid/Slurry    
#> 4  2020 203       NA            Cattle_milk        Cattle      Daily Spread     
#> # ℹ 13 more variables: manure_type <chr>, stream <chr>, applied_n <dbl>,
#> #   applied_c <dbl>, applied_vs <dbl>, n_volatilized <dbl>, n_leached <dbl>,
#> #   n2o_direct_n <dbl>, n2_n <dbl>, n2o_indirect_n <dbl>, c_lost <dbl>,
#> #   vs_destroyed <dbl>, method_losses <chr>
```
