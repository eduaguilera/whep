# Estimate ammonia-N volatilisation from applied nitrogen.

Three independent methods for the fraction of applied nitrogen
volatilised as ammonia. `"ipcc"` (IPCC 2019 Tier 1, `n_fun.r:914-930`)
needs only `fert_type` and applies a single global fraction. `"manner"`
(the default) dispatches each row through the process-based
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)
MANNER model (Task C4), which requires far more driver detail (see
Details); this asymmetry in input requirements is intentional, not an
oversight. `"manner_default"` dispatches each row through
[`calculate_manner_nh3_default()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3_default.md)
instead, the same process-based organic-manure model but with
`technique`/`incorporation_delay_h` filled in from a documented
gross-assumption blend rather than required as driver columns.

## Usage

``` r
calculate_nh3(x, method = "manner", example = FALSE)
```

## Arguments

- x:

  A tibble with `n_input_t` (numeric, tonnes N) and `fert_type`.
  `method = "manner"` additionally requires `manner_fertiliser` and the
  driver columns listed in Details. `method = "manner_default"`
  additionally requires `manner_fertiliser` and the driver columns
  listed in Details, but NOT `technique`/`incorporation_delay_h`.

- method:

  `"manner"` (default, process-based, per-row), `"ipcc"` (Tier 1, global
  fraction) or `"manner_default"` (process-based organic path with a
  gross-assumption technique/incorporation-delay blend, no
  `technique`/`incorporation_delay_h` columns required).

- example:

  If `TRUE`, return a small fixture instead of computing from `x`.
  Defaults to `FALSE`.

## Value

`x` with `nh3_n_t` and `method_nh3` appended.

## Details

`method = "manner"` requires `x` to already carry a `manner_fertiliser`
column holding the exact
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)
`fertiliser` key (`"Urea"`, `"AN"`, `"CAN"`, `"AS"`, `"cattle_slurry"`,
`"pig_slurry"`, `"FYM"`, `"poultry_manure"` or `"urban"`) plus every
driver column that key's path needs: `soil_ph`, `rate_kg_ha`,
`rainfall_mm`, `irrigated`, `temp_c`, `temp_c_annual_mean` for the
synthetic path; `rainfall_mm`, `irrigated`, `windspeed_ms`, `technique`,
`system`, `temp_c`, `incorporation_delay_h`, `species` (unless
`manner_fertiliser == "urban"`) for the organic path. This function does
not infer `manner_fertiliser` from `fert_type` (e.g. which synthetic
sub-type `"Synthetic"` maps to is not determined by `fert_type` alone)
and does not silently fall back to `"ipcc"` or invent driver values: a
missing required column aborts naming exactly which column is absent.
Each row is dispatched to
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)
individually (MANNER's dispatch is inherently per-row categorical, not
vectorizable across the coefficient joins); this row-iteration is
isolated to a small private helper.

`method = "manner_default"` requires the same `manner_fertiliser` column
(restricted to the organic-manure keys, since the gross default only
covers that path) plus `rainfall_mm`, `irrigated`, `windspeed_ms`,
`system`, `temp_c` and `species` (unless
`manner_fertiliser == "urban"`). It does NOT require `technique` or
`incorporation_delay_h`: those are filled in from
[manner_default_technique_mix](https://eduaguilera.github.io/whep/reference/manner_default_technique_mix.md)
(see
[`calculate_manner_nh3_default()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3_default.md)'s
Details for the gross-assumption reasoning), never invented per-row.

## Examples

``` r
calculate_nh3(example = TRUE)
#> # A tibble: 1 × 4
#>   n_input_t fert_type nh3_n_t method_nh3
#>       <dbl> <chr>       <dbl> <chr>     
#> 1        10 Synthetic     1.1 ipcc      
```
