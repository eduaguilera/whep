# Estimate nitrate leaching, topsoil denitrification and indirect N2O.

Two methods for partitioning a nitrogen surplus into leached nitrate and
topsoil-denitrified nitrogen. `"meisinger_drainage"` (the default) is
the full cascade of the source implementation: bins annual drainage and
soil organic matter share, looks up a topsoil denitrification share from
[meisinger_denitrification](https://eduaguilera.github.io/whep/reference/meisinger_denitrification.md),
applies subsoil NO3 reduction
([subsoil_no3_reduction](https://eduaguilera.github.io/whep/reference/subsoil_no3_reduction.md))
and a carbon-to-nitrogen leaching attenuation, then re-derives
`denitrification_n_t` as the residual of `n_surplus_t` minus the
computed `no3_n_t` (the raw denitrification share is only an
intermediate; see Details). `"ipcc_fracleach"` is the IPCC 2019 Tier 1
leached fraction, keyed on `moisture_regime` and `water_regime`, with
every factor read from
[ipcc_2019_soil_n_ef](https://eduaguilera.github.io/whep/reference/ipcc_2019_soil_n_ef.md):
`FracLEACH-(H)` is 0.24 on wet rows and on dry irrigated rows and 0 on
dry rainfed rows (IPCC 2019 Refinement Vol 4 Ch 11 Section 11.2.2.2 p.
11.25: 0.24 "for wet climates or in dry climate regions where irrigation
(other than drip irrigation) is used"), and `EF5` is 0.011 (Table 11.3
(Updated) p. 11.26). All irrigation is treated as non-drip, since no
layer says which is drip: an upper-bound assumption for dry irrigated
land. On wet and dry irrigated rows the rest of the surplus is booked as
`denitrification_n_t`, as in the Meisinger cascade. On dry rainfed rows,
where FracLEACH is 0, it is booked as `retained_profile_n_t`, nitrogen
left in the soil profile, and `denitrification_n_t` is 0: FracLEACH says
that surplus does not leach, not that it denitrifies, and no method here
estimates its fate. Each row is stamped `"ipcc_fracleach_wet"`,
`"ipcc_fracleach_dry_irrigated"` or
`"ipcc_fracleach_dry_rainfed_retained"`. Eq. 11.10 and Table 11.3 define
`FracLEACH-(H)` per kg of N added
(`F_SN + F_ON + F_PRP + F_CR + F_SOM`), so it multiplies
`n_input_for_n2o_t`, the same input set that prices direct N2O in
[`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md),
not the surplus. `"fracleach_surplus"` applies the same fractions to
`n_surplus_t` instead, the form before whep#1535: a surplus-based
deviation from the IPCC definition, kept selectable for comparison,
stamped `"fracleach_surplus_wet"` and so on. Under every method
`no3_n_t + denitrification_n_t + retained_profile_n_t` equals
`n_surplus_t`.

## Usage

``` r
calculate_n_leaching(
  x,
  drainage_mm = NULL,
  method = c("meisinger_drainage", "ipcc_fracleach", "fracleach_surplus"),
  example = FALSE
)
```

## Source

IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Vol 4, Ch 11, Section 11.2.2.2, p. 11.25
(`FracLEACH-(H)`) and Table 11.3 (Updated), p. 11.26 (`EF5`).

## Arguments

- x:

  A tibble with `n_surplus_t`. `method = "meisinger_drainage"` requires
  `fert_type`, `climate`, `irrig_cat`, `land_use`, `cn_input` (may be
  `NA`), `tillage` (checked only for synthetic rows) and `som_share`;
  `method = "ipcc_fracleach"` requires `moisture_regime` (`"wet"` or
  `"dry"`), on dry rows `water_regime` (`"rainfed"` or `"irrigated"`),
  and `n_input_for_n2o_t`, the N added in IPCC Eq. 11.10;
  `"fracleach_surplus"` requires the first two.

- drainage_mm:

  A numeric vector aligned to `x`'s rows giving annual drainage (mm), or
  a single string naming a column of `x` to use instead. Kept as a
  separate argument (rather than a static `x` column) because in the
  full pipeline it flows in from Module A's gridded water balance.

- method:

  `"meisinger_drainage"` (default), `"ipcc_fracleach"` or
  `"fracleach_surplus"`.

- example:

  If `TRUE`, return a small fixture instead of computing from `x`.
  Defaults to `FALSE`.

## Value

`x` with `no3_n_t`, `denitrification_n_t`, `retained_profile_n_t` (0
under `"meisinger_drainage"`, which books every non-leached tonne as
denitrified), `n2o_indirect_no3_n_t` and `method_leaching` appended.

## Details

For `method = "meisinger_drainage"`, `denitrification_n_t` is computed
twice: first as `n_surplus_t * denit_share` (the raw Meisinger share) to
derive `no3_n_t`, then overwritten as `n_surplus_t - no3_n_t`, as in the
source implementation. The RETURNED `denitrification_n_t` is this
second, residual value, not the raw share product; this is a deliberate
two-step sequence in the source, not a redundant computation to simplify
away. Drainage and soil organic matter bins are matched with the
source's strictly-open `s_min < s < s_max` filter: a value exactly on a
shared bin edge, or outside the covered range, matches no bin and aborts
via the unmatched-row check (the source drops it), rather than being
pulled into an adjacent or ceiling bin.

Manure/organic rows (`fert_cat == "Manure"`, i.e. every `fert_type`
other than `"Synthetic"`) always join the Meisinger table's
`tillage == "Not_specified"` row regardless of `x$tillage`. Synthetic
rows join on `x$tillage`, but only `"Tillage"` exists in the source's
synthetic block: a synthetic row with `tillage == "No_tillage"` aborts.

For `method = "ipcc_fracleach"`, a missing or invalid `moisture_regime`
aborts as in
[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md),
and a dry row without a `water_regime` aborts
(`whep_missing_water_regime` for an absent column,
`whep_water_regime_value` for a missing or unknown value, naming the
count): dry irrigated and dry rainfed land leach 0.24 and 0 of their
surplus, so a dry row of unknown regime has no defensible fraction. Wet
rows need no `water_regime`. `"fracleach_surplus"` checks the same.

Under `"ipcc_fracleach"` leaching is capped at `n_surplus_t`, so it
never exceeds the nitrogen left after crop uptake. IPCC Tier 1 has no
such cap; rows where it binds carry the suffix `"_capped_surplus"` on
their stamp (e.g. `"ipcc_fracleach_wet_capped_surplus"`). A missing
`n_input_for_n2o_t` column, or a missing value in it, aborts
(`whep_missing_n_added`, `whep_n_added_na`): read as zero it would leach
nothing while the partition still closed.

## Examples

``` r
calculate_n_leaching(example = TRUE)
#> # A tibble: 1 × 8
#>   n_surplus_t moisture_regime n_input_for_n2o_t no3_n_t denitrification_n_t
#>         <dbl> <chr>                       <dbl>   <dbl>               <dbl>
#> 1          60 wet                           100      24                  36
#> # ℹ 3 more variables: retained_profile_n_t <dbl>, n2o_indirect_no3_n_t <dbl>,
#> #   method_leaching <chr>
```
