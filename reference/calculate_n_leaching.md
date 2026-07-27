# Estimate nitrate leaching, topsoil denitrification and indirect N2O.

Two methods for partitioning a nitrogen surplus into leached nitrate and
topsoil-denitrified nitrogen. `"meisinger_drainage"` (the default,
`n_fun.r:932-988`) is the full Spain_Hist cascade: bins annual drainage
and soil organic matter share, looks up a topsoil denitrification share
from
[meisinger_denitrification](https://eduaguilera.github.io/whep/reference/meisinger_denitrification.md),
applies subsoil NO3 reduction
([subsoil_no3_reduction](https://eduaguilera.github.io/whep/reference/subsoil_no3_reduction.md))
and a carbon-to-nitrogen leaching attenuation, then re-derives
`denitrification_n_t` as the residual of `n_surplus_t` minus the
computed `no3_n_t` (the raw denitrification share is only an
intermediate; see Details). `"ipcc_fracleach"` is a much simpler global
fallback using the flat `FracLEACH = 0.24` constant already documented
in
[`build_crop_soil_n2o_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_soil_n2o_extension.md).

## Usage

``` r
calculate_n_leaching(
  x,
  drainage_mm = NULL,
  method = c("meisinger_drainage", "ipcc_fracleach"),
  example = FALSE
)
```

## Arguments

- x:

  A tibble with `n_surplus_t`, `fert_type`, `climate`, `irrig_cat`,
  `land_use`, `cn_input` (may be `NA`), `tillage` (checked only for
  synthetic rows) and `som_share`.

- drainage_mm:

  A numeric vector aligned to `x`'s rows giving annual drainage (mm), or
  a single string naming a column of `x` to use instead. Kept as a
  separate argument (rather than a static `x` column) because in the
  full pipeline it flows in from Module A's gridded water balance.

- method:

  `"meisinger_drainage"` (default) or `"ipcc_fracleach"`.

- example:

  If `TRUE`, return a small fixture instead of computing from `x`.
  Defaults to `FALSE`.

## Value

`x` with `no3_n_t`, `denitrification_n_t`, `n2o_indirect_no3_n_t` and
`method_leaching` appended.

## Details

For `method = "meisinger_drainage"`, `denitrification_n_t` is computed
twice: first as `n_surplus_t * denit_share` (the raw Meisinger share) to
derive `no3_n_t`, then overwritten as `n_surplus_t - no3_n_t` (verified
`n_fun.r:983`). The RETURNED `denitrification_n_t` is this second,
residual value, not the raw share product; this is a deliberate two-step
sequence in the source, not a redundant computation to simplify away.
Drainage and soil organic matter bins are matched with the source's
strictly-open `s_min < s < s_max` filter (`n_fun.r:939,942`): a value
exactly on a shared bin edge, or outside the covered range, matches no
bin and aborts via the unmatched-row check (the source drops it), rather
than being pulled into an adjacent or ceiling bin.

Manure/organic rows (`fert_cat == "Manure"`, i.e. every `fert_type`
other than `"Synthetic"`) always join the Meisinger table's
`tillage == "Not_specified"` row regardless of `x$tillage`. Synthetic
rows join on `x$tillage`, but only `"Tillage"` exists in the source's
synthetic block: a synthetic row with `tillage == "No_tillage"` aborts.

## Examples

``` r
calculate_n_leaching(example = TRUE)
#> # A tibble: 1 × 5
#>   n_surplus_t no3_n_t denitrification_n_t n2o_indirect_no3_n_t method_leaching
#>         <dbl>   <dbl>               <dbl>                <dbl> <chr>          
#> 1         100      24                  76                0.264 ipcc_fracleach 
```
