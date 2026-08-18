# Build the supply-to-intake loss wedge.

Returns the share of retail-level protein supply that does not become
intake, per country and year, and the divisor `1 / (1 - omega)` that
turns a requirement into a supply floor. It replaces part of the
unsourced 1.35 multiplier the SJOS-N axis used to carry (whep#753).

Rates come from Gustavsson et al. (2011), *Global food losses and food
waste*, FAO, Annex 4, which tabulates loss percentages for seven
commodity groups in seven world regions at five food-chain steps. Only
the two steps at or after the retail shelf are composed, because FBS
food availability is already measured there:
`omega_group = 1 - (1 - d/2) * (1 - c/2)`, with `d` and `c` the
distribution and consumption rates. Country-year `omega` is the
protein-weighted mean of the group values over the supplied food basket,
so it varies with a country's diet composition while the underlying
rates do not.

`method = "gustavsson_half_min"` (default) takes the minimum of each
rate across the seven regions and halves it. It is a **deliberate lower
bound on unavoidable loss, not an estimate of achievable loss**, for two
reasons that the halving does not repair. The consumption-step minimum
is sub-Saharan Africa in every commodity group, and those are scarcity
figures rather than efficiency figures; the distribution-step minima, by
contrast, are genuine best practice. On WHEP's 2010 basket it gives
`omega` near 2.5%, which is below FAOSTAT's retail-only median of 2.83%
on energy (item 21059) even though this wedge additionally spans the
household step – a different basis and a different statistic, so not a
contradiction, but confirmation that the construction is conservative.

`"gustavsson_min"` drops the halving and roughly doubles the wedge (near
4.9% on the same basket); `"none"` sets it to zero and makes every floor
an explicit lower bound. All are alternatives, never fallbacks: the
choice is stamped in `method_loss_wedge`.

`"gustavsson_regional_actual"` is the sensitivity arm, and it is **not
an unavoidable-loss estimate at all**: it drops both the minimum and the
halving and gives each country its Annex 1 region's own observed rates.
On the 2010 world basket that is 14.2% against the default's 2.5%, a
floor divisor of 1.166, spanning 4.1% to 21.4% across countries. Its
country structure is contested – Gustavsson's rich-high gradient is the
opposite sign to UNEP's Food Waste Index – so it quantifies that
disagreement rather than resolving it.

Annex 1 lists 152 countries, covering 99.0% of 2010 world food protein.
Areas outside it take the unweighted mean rate across the seven regions
by default (`coverage = "global_mean"`), stamped as such in
`method_region`; `"annex1_only"` returns nothing for them instead. Annex
1's "China" does not disambiguate, so both WHEP codes take
Industrialized Asia: `CHN` (area 41, "China, mainland") and the
aggregate area 351, which carries no `iso3c` and is what the FBS pin
reports food on. Keying on `iso3c` alone would drop area 351 and with it
a fifth of world food protein.

Items that Gustavsson's Annex 2 does not place in a commodity group
carry no rate rather than borrowing a neighbour's. They are dropped from
the weighting, which gives them the basket's mean wedge implicitly, and
their share is reported in `protein_grouped_share` so the choice stays
visible. On the 2010 world basket they are 5.0% of food protein, eggs
alone being 3.7%; assigning eggs to meat or to dairy instead moves
`omega` by less than 0.1 percentage points either way.

## Usage

``` r
build_loss_wedge(
  data = list(),
  method = c("gustavsson_half_min", "gustavsson_min", "gustavsson_regional_actual",
    "none"),
  coverage = c("global_mean", "annex1_only"),
  protein_basis = c("edible_portion", "whole_commodity", "product_nitrogen")
)
```

## Arguments

- data:

  Named list of injected inputs. Supply the basket either as
  `protein_supply` (`year`, `area_code`, `item_cbs_code`, `protein_t`)
  or as `cbs_food` (`year`, `area_code`, `item_cbs_code`, `food_t`),
  which is converted through the same nutrition lookup
  [`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
  uses. `biomass_coefs`, `items_full`, `food_loss_wedge`,
  `food_loss_item_groups` and `food_loss_regions` override the packaged
  tables.

- method:

  `"gustavsson_half_min"` (default), `"gustavsson_min"`,
  `"gustavsson_regional_actual"` or `"none"`.

- coverage:

  How areas outside Annex 1's 152 countries are handled by
  `"gustavsson_regional_actual"`: `"global_mean"` (default) or
  `"annex1_only"`. Ignored by the region-invariant methods.

- protein_basis:

  Passed to the nutrition lookup when the basket is given as `cbs_food`;
  see
  [`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md).
  Defaults to `"edible_portion"`, matching the supply the floor is
  compared against.

## Value

A tibble keyed by `year`, `area_code` with `omega`, `floor_divisor`,
`protein_grouped_share`, `method_loss_wedge` and `method_region`, plus
the polity columns below.

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
build_loss_wedge(
  data = list(
    protein_supply = tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~protein_t,
      2010L, 10L,        2511L,          100,
      2010L, 10L,        2605L,          100
    )
  )
)
#> # A tibble: 1 × 11
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010        10               10 AUS-1901-2025         Australia            
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>, omega <dbl>,
#> #   protein_grouped_share <dbl>, method_region <chr>, floor_divisor <dbl>,
#> #   method_loss_wedge <chr>
```
