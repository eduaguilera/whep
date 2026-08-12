# Build per-capita food supply for the nourishment axis.

Assembles per-capita protein and dietary-energy supply, the state
variable for the SJOS-N nourishment ("just") axis. Protein is the SJOS-N
nourishment axis; dietary energy is a secondary cross-check. The default
`"whep_native"` method multiplies the WHEP commodity-balance food
element (tonnes fresh matter, per `year`, `area_code`, `item_cbs_code`)
by the per-item nutrition coefficients in
[`whep::biomass_coefs`](https://eduaguilera.github.io/whep/reference/biomass_coefs.md)
and divides by national population. Protein per kilogram fresh matter is
nitrogen times 6.25 (nitrogen-to-protein factor), on the basis selected
by `protein_basis`. The nitrogen density is `N_kgN_kgFM` where
available, otherwise `Product_kgN_kgDM * Product_kgDM_kgFM`.
`Edible_N_kgFM` is not read: it is empty in every coefficient row,
upstream as well as in the packaged data, so the edible basis is derived
from `Edible_portion` instead of stored redundantly. Energy per kilogram
fresh matter follows `GE_product_edible_portion_MJ_kgFM`, then
`GE_product_MJ_kgFM` (MJ per kg fresh matter), converted to kilocalories
via `MJ / 0.004184`. The energy term is GROSS (combustion) energy, not
Atwater metabolisable energy, and so is only a secondary cross-check for
SJOS-N; Atwater factors could refine it (O-B). Food items with no
protein coefficient after the coalesce chain are excluded with a warning
naming the count and a few examples (the residual gap-fill, O-B), never
silently dropped. The `"faostat_fbs"` method returns the injected
FAOSTAT Food Balance Sheet per-capita supply unchanged, as a cross-check
/ sensitivity.

An area with food but no `population` row has no denominator, so it is
absent from the output rather than wrong in it. Those areas are named in
a warning with the share of food protein that leaves with them; on the
real `gdp-population` pin they are 15 areas headed by Bhutan and Comoros
(#543). `options(whep.warn_missing_population = FALSE)` silences it.

## Usage

``` r
build_food_supply(
  method = c("whep_native", "faostat_fbs"),
  data = list(),
  protein_basis = c("edible_portion", "whole_commodity", "product_nitrogen"),
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

- protein_basis:

  How the inedible fraction is treated when converting nitrogen density
  to protein, for `"whep_native"` only: `"edible_portion"` (default)
  scales the nitrogen density by `Edible_portion`, which is correct when
  `food_t` is commodity mass while the density applies to the edible
  part, and agrees best with FAOSTAT FBS; `"whole_commodity"` applies no
  edible scaling, the behaviour before this argument existed, kept for
  continuity and sensitivity analysis; `"product_nitrogen"` uses the
  agronomic `Product_kgN_kgDM` for both the edible and inedible
  fractions, scaled by `Edible_portion`, ignoring `N_kgN_kgFM`. A
  missing `Edible_portion` counts as 1.

- example:

  If `TRUE`, return a small fixture instead of computing. Defaults to
  `FALSE`.

## Value

A tibble keyed by `year`, `area_code` with `protein_g_cap_day`,
`energy_kcal_cap_day`, `population`, `method_food_supply` and
`method_protein_basis` (`NA` for `"faostat_fbs"`), plus the polity
columns below.

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
for the reasoning. Where that polity is not live in the row's own year –
41.5% of the pre-1961 `(area, year)` cells –
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
says so as `mapping_status == "backcast_anchor"`, and
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports it as `gap_kind == "backcast_anchor"`. These columns do not say
so either way.

A row whose year no mapped period covers is resolved to the NEAREST
period of the same area instead, so `reporting_polity_code` can name a
polity that did not exist in that row's year – FAOSTAT bucket 206 "Sudan
(former)" keeps reporting after `SUD-1956-2011` ends, and its post-2011
rows carry that code. These columns do not say so:
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
reports such a row as `mapping_status == "out_of_span"`, and that column
is dropped here so that adding it does not change the schema of every
area-keyed output at once.
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports the stand-in rows of a built table, and
`options(whep.polity_mapping_status = "flag")` (or `"status"`) carries
the signal on the outputs themselves. Both are opt-in; the default is no
extra column.

## Examples

``` r
build_food_supply(example = TRUE)
#> # A tibble: 3 × 11
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010        10               10 AUS-1901-2025         Australia            
#> 2  2010        32               32 CMR-1961-2025         Cameroon             
#> 3  2011        10               10 AUS-1901-2025         Australia            
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>,
#> #   protein_g_cap_day <dbl>, energy_kcal_cap_day <dbl>, population <dbl>,
#> #   method_food_supply <chr>, method_protein_basis <chr>
```
