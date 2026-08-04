# Build per-capita food supply for the nourishment axis.

Assembles per-capita protein and dietary-energy supply, the state
variable for the SJOS-N nourishment ("just") axis. Protein is the SJOS-N
nourishment axis; dietary energy is a secondary cross-check. The default
`"whep_native"` method multiplies the WHEP commodity-balance food
element (tonnes fresh matter, per `year`, `area_code`, `item_cbs_code`)
by the per-item nutrition coefficients in
[`whep::biomass_coefs`](https://eduaguilera.github.io/whep/reference/biomass_coefs.md)
and divides by national population. Protein per kilogram fresh matter
follows the coalesce chain `Edible_N_kgFM`, then `N_kgN_kgFM`, then
`Product_kgN_kgDM * Product_kgDM_kgFM` (kg N per kg fresh matter), times
6.25 (nitrogen-to-protein factor). Energy per kilogram fresh matter
follows `GE_product_edible_portion_MJ_kgFM`, then `GE_product_MJ_kgFM`
(MJ per kg fresh matter), converted to kilocalories via `MJ / 0.004184`.
The energy term is GROSS (combustion) energy, not Atwater metabolisable
energy, and so is only a secondary cross-check for SJOS-N; Atwater
factors could refine it (O-B). Food items with no protein coefficient
after the coalesce chain are excluded with a warning naming the count
and a few examples (the residual gap-fill, O-B), never silently dropped.
The `"faostat_fbs"` method returns the injected FAOSTAT Food Balance
Sheet per-capita supply unchanged, as a cross-check / sensitivity.

## Usage

``` r
build_food_supply(
  method = c("whep_native", "faostat_fbs"),
  data = list(),
  example = FALSE
)
```

## Arguments

- method:

  Supply source: `"whep_native"` (default, commodity-balance food tonnes
  times
  [`whep::biomass_coefs`](https://eduaguilera.github.io/whep/reference/biomass_coefs.md)
  divided by population) or `"faostat_fbs"` (the injected FAOSTAT FBS
  per-capita supply).

- data:

  Named list of injected inputs. For `"whep_native"`: `cbs_food`
  (`year`, `area_code`, `item_cbs_code`, `food_t`) and `population`
  (`year`, `area_code`, `population`) are required, and `biomass_coefs`
  / `items_full` override the packaged
  [`whep::biomass_coefs`](https://eduaguilera.github.io/whep/reference/biomass_coefs.md)
  /
  [`whep::items_full`](https://eduaguilera.github.io/whep/reference/items_full.md).
  For `"faostat_fbs"`: `fbs_supply` (`year`, `area_code`,
  `protein_g_cap_day`, `energy_kcal_cap_day`, `population`) is required.

- example:

  If `TRUE`, return a small fixture instead of computing. Defaults to
  `FALSE`.

## Value

A tibble keyed by `year`, `area_code` with `protein_g_cap_day`,
`energy_kcal_cap_day`, `population` and `method_food_supply`, plus the
polity columns below.

## Polity columns

Every area-keyed output carries the polity its `area_code` resolves to
in that row's year:

- `polity_area_code`: The numeric key rows are AGGREGATED on, for the
  matrix workflows. It is a bucket, not an identity: use
  `reporting_polity_code` to say which territory a row belongs to.

- `reporting_polity_code`: The polity itself, e.g. `ESP-1846-1914`. It
  is year-aware, so the same `area_code` resolves to different polities
  in different years, which is the point of the crosswalk.

- `reporting_polity_name`: Its name. It can differ from the area's own
  name where the area folds into an aggregate.

- `reporting_polity_has_geometry`: Whether the polity has a polygon in
  the WHEP polity database, for callers that need to map or intersect
  it. `FALSE` is a documented gap upstream, not an error.

Rows whose `area_code` resolves to no polity keep the columns with `NA`
rather than being dropped, so a gap is visible instead of silent.

Rows before the back-cast anchor year resolve to the polity live in that
anchor year rather than to the polity live in the row's own year,
because WHEP's pre-anchor series are back-cast onto the anchor-year
territory. See
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the reasoning.

## Examples

``` r
build_food_supply(example = TRUE)
#> # A tibble: 3 × 10
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010        10               10 AUS-1901-2025         Australia            
#> 2  2010        32               32 CMR-1961-2025         Cameroon             
#> 3  2011        10               10 AUS-1901-2025         Australia            
#> # ℹ 5 more variables: reporting_polity_has_geometry <lgl>,
#> #   protein_g_cap_day <dbl>, energy_kcal_cap_day <dbl>, population <dbl>,
#> #   method_food_supply <chr>
```
