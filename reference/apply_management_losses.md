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
  [`split_manure_management()`](https://eduaguilera.github.io/whep/reference/split_manure_management.md),
  optionally with the `n_bedding` and `c_bedding` columns
  [`add_manure_bedding()`](https://eduaguilera.github.io/whep/reference/add_manure_bedding.md)
  adds.

- options:

  A named list. `method` selects the loss method (`"ipcc_2019_tier2"`).
  `bedding_c_loss` selects how bedding carbon is treated in storage:
  `"same_as_excreta"` (default) or `"none"`; see the Bedding section.

## Value

The input rows with `manure_type`, `applied_n`, `applied_c`,
`applied_vs`, `n_volatilized`, `n_leached`, `n2o_direct_n`, `n2_n`,
`n2o_indirect_n`, `c_lost`, `vs_destroyed`, `n_bedding`, `c_bedding`,
`method_losses` and `method_bedding_c`.

## Bedding

When the rows carry `n_bedding` and `c_bedding` (from
[`add_manure_bedding()`](https://eduaguilera.github.io/whep/reference/add_manure_bedding.md)),
the straw bedded under housed animals is part of the manure that reaches
the field, and the applied C:N is the C:N of the bedded farmyard manure
rather than of the excreta alone. Without those columns nothing changes:
they default to zero and every number is the excreta-only one.

Nitrogen follows IPCC 2019 Refinement Vol. 4 Ch. 10 Eq. 10.34 (p. 10.94)
exactly: `NbeddingMS` sits **outside** the `(1 - FracLossMS)` term,
because "mineralization of nitrogen compounds in beddings occurs more
slowly compared to manure and the concentration of ammonia fraction in
organic beddings is negligible", so "both volatilization and leaching
losses during storage of bedding are assumed to be zero" (p. 10.93).
Bedding nitrogen therefore raises `applied_n` one-for-one and raises
none of the loss side-streams.

Carbon has no IPCC rule at all – manure CO2 is out of scope there – so
`bedding_c_loss` selects it. `"same_as_excreta"` (default) applies the
stream's own storage carbon-loss fraction to the bedding carbon too: the
0.420 for solid storage comes from Pardo et al. 2015
([doi:10.1111/gcb.12806](https://doi.org/10.1111/gcb.12806) , Table 2),
a systematic review of whole manure heaps, and a heap in solid storage
in practice already contains its litter, so the measured loss is a
whole-heap loss. `"none"` keeps every gram of bedding carbon, mirroring
the zero storage loss IPCC gives bedding nitrogen; it is the upper bound
on applied carbon and raises the applied carbon of a bedded
solid-storage stream by `0.420 / (1 - 0.420) = 72%` of the bedding
carbon relative to the default.

Volatile solids stay excreta-only under both, so the Tier 2 methane
engine in
[`build_livestock_ghg_extension()`](https://eduaguilera.github.io/whep/reference/build_livestock_ghg_extension.md)
is untouched. IPCC 2019 Ch. 10 does ask for bedding to be combined with
volatile solids when estimating manure methane; that is a separate
change to a separate engine and is not made here.

## Examples

``` r
excretion <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~livestock_category,
  ~n_excretion, ~c_excretion, ~vs_excretion,
  2020L, "203", NA, "Cattle_milk", 100, 1900, 60
)
apply_management_losses(split_manure_management(excretion))
#> # A tibble: 5 × 22
#>    year territory sub_territory livestock_category species_gen mms_type         
#>   <int> <chr>     <lgl>         <chr>              <chr>       <chr>            
#> 1  2020 203       NA            Cattle_milk        Cattle      Anaerobic Lagoon 
#> 2  2020 203       NA            Cattle_milk        Cattle      Daily Spread     
#> 3  2020 203       NA            Cattle_milk        Cattle      Liquid/Slurry    
#> 4  2020 203       NA            Cattle_milk        Cattle      Pasture/Range/Pa…
#> 5  2020 203       NA            Cattle_milk        Cattle      Solid Storage    
#> # ℹ 16 more variables: manure_type <chr>, stream <chr>, applied_n <dbl>,
#> #   applied_c <dbl>, applied_vs <dbl>, n_volatilized <dbl>, n_leached <dbl>,
#> #   n2o_direct_n <dbl>, n2_n <dbl>, n2o_indirect_n <dbl>, c_lost <dbl>,
#> #   vs_destroyed <dbl>, n_bedding <dbl>, c_bedding <dbl>, method_losses <chr>,
#> #   method_bedding_c <chr>
```
