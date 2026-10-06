# Build the irrigated:rainfed yield ratio per cell, crop and year.

Gives each cell-crop-year the ratio `R` of its irrigated yield to its
rainfed yield, the weight that splits a crop's production, synthetic
nitrogen and harvest removals between its two regimes (issue \#1233).
`R` combines three sources:

1.  **Anchor.** The SPAM2010 v2.0 ratio of irrigated to all-rainfed
    yield for the item's SPAM crop in the country (see
    [`read_spam_yields()`](https://eduaguilera.github.io/whep/reference/read_spam_yields.md)
    and
    [regime_yield_crop_mapping](https://eduaguilera.github.io/whep/reference/regime_yield_crop_mapping.md)).
    Each yield is the country's production over its harvested area,
    summed over its SPAM cells, so micro-stands do not weigh more than
    large ones. The anchor is floored at 1.

2.  **Level.** The anchor is carried to year `t` by the country's
    synthetic nitrogen per hectare of cropland relative to 2010,
    `R_level = 1 + (R_anchor - 1) * n_t / n_2010`, so the gap closes
    towards 1 before synthetic fertiliser and widens with it.

3.  **Anomaly**, in two parts. The **spatial** part is the cell's LPJmL
    irrigated:rainfed ratio over 1994-2023 divided by the country's, so
    drier places get a larger gap. The **temporal** part is the cell's
    ratio in year `t` divided by its own 1994-2023 ratio, so worse years
    get a larger gap.

The anomalies scale only the excess of the ratio over 1, so `R` goes to
1 wherever the level does, whatever the anomaly. The long-term ratio is
`R_lt = 1 + min(9, (R_level - 1) * spatial)`: it is capped at 10, i.e.
its excess over 1 at 9. The ratio of the year is
`R = 1 + (R_lt - 1) * temporal`; the temporal part is not capped, so
only a bad year takes `R` past 10.

This `R` is returned as `ratio_unbounded`, for diagnosis only: a
single-year near-zero LPJmL rainfed yield can make it absurdly large.
Only the `ratio` of
[`split_regime_yield()`](https://eduaguilera.github.io/whep/reference/split_regime_yield.md),
which applies it to a cell's production and areas under an
irrigated-yield ceiling and a rainfed-yield floor, is fit to weight
anything.

## Usage

``` r
build_regime_yield_ratio(cells, run_dir = NULL, data = NULL, example = FALSE)
```

## Source

Yu, Q. et al. (2020). A cultivated planet in 2010 – Part 2: The global
gridded agricultural-production maps. Earth System Science Data 12,
3545-3572.
[doi:10.5194/essd-12-3545-2020](https://doi.org/10.5194/essd-12-3545-2020)
. LPJmL 6.1.1 band harvests as in
[`read_lpjml_regime_yield()`](https://eduaguilera.github.io/whep/reference/read_lpjml_regime_yield.md).
FAOSTAT Fertilizers by Nutrient, Land Use and Crops and livestock
products. Smil, V. (2001) *Enriching the Earth*, MIT Press.

## Arguments

- cells:

  A tibble of the cell-crop-years to build, with `lon`, `lat`
  (0.5-degree cell centres), `area_code` (a WHEP area code; it is
  resolved to its polity bucket for the national inputs),
  `item_prod_code` and `year`. Other columns are dropped; duplicated
  keys are collapsed. The country's LPJmL 1994-2023 ratio pools the
  cells of `cells` with that `area_code` (any crop), so pass every cell
  of a country, as the gridded land use holds them, or the spatial part
  is measured against a partial country.

- run_dir:

  LPJmL run directory for the anomaly. `NULL` (default) uses
  `WHEP_LPJML_RUN_DIR`, as
  [`read_lpjml_regime_yield()`](https://eduaguilera.github.io/whep/reference/read_lpjml_regime_yield.md)
  does.

- data:

  Optional named list of inputs used instead of reading them, chiefly
  for testing and for reusing work across calls:

  - `spam`:
    [`read_spam_yields()`](https://eduaguilera.github.io/whep/reference/read_spam_yields.md)
    output (SPAM2010).

  - `fertilizer`: the `faostat-fertilizer-nutrients` table.

  - `cropland`:
    [`get_arable_permanent_land()`](https://eduaguilera.github.io/whep/reference/get_arable_permanent_land.md)
    output (`area_code`, `year`, `cropland_ha`).

  - `luh2`: the `luh2-areas` table (`ISO3`, `Year`, `Land_Use`,
    `Area_Mha`), for the successors' back-cast cropland.

  - `faostat_production`: the raw `faostat-production` table
    (`Area Code`, `Item Code`, `Element`, `Year`, `Value`), for the
    Linum and Hemp dominance.

  - `lpjml`: LPJmL crop yields for the years of `cells`, at the crop
    grain (`lon`, `lat`, `year`, `lpjml_crop`, `yield_rainfed`,
    `yield_irrigated`, `method_regime_yield`).

  - `lpjml_window`: the same for the normaliser years, with the stand
    fractions `stand_frac_rainfed`, `stand_frac_irrigated`. Only rows in
    1994-2023 are used.

  - `lpjml_grid`: the cells (`lon`, `lat`) the LPJmL run's output
    covers.

- example:

  If `TRUE`, return a small fixture instead of building the ratio.
  Defaults to `FALSE`.

## Value

A tibble with one row per cell-crop-year of `cells`:

- `lon`, `lat`, `area_code`, `item_prod_code`, `year`: the key.

- `ratio_spam`: the SPAM2010 ratio as computed, before the floor.

- `ratio_anchor`: `max(1, ratio_spam)`.

- `ratio_level`: the anchor carried to `year` by synthetic N.

- `ratio_spatial`: the cell's 1994-2023 LPJmL ratio over the country's.

- `ratio_long_term`: `1 + min(9, (ratio_level - 1) * ratio_spatial)`.

- `ratio_temporal`: the cell-year LPJmL ratio over the cell's 1994-2023
  ratio.

- `ratio_anomaly`: `ratio_spatial * ratio_temporal`, for reference.

- `ratio_unbounded`: `1 + (ratio_long_term - 1) * ratio_temporal`; `NA`
  where the level is. Not fit to weight anything: pass it to
  [`split_regime_yield()`](https://eduaguilera.github.io/whep/reference/split_regime_yield.md),
  whose `ratio` is the bounded one.

- `spam_crop_used`: the SPAM crop code(s) the anchor came from (for
  Linum and Hemp, the product chosen for the country).

- `method_ratio_anchor`: `"spam_country"`, `"spam_global"`,
  `"spam_composite_country"`, `"spam_composite_global"`,
  `"spam_dominance_country"`, `"spam_dominance_global"`,
  `"spam_dominance_world"` or `"spam_none"` (no SPAM ratio at all).

- `method_ratio_trend`: where `n_t` came from: `"faostat"` or
  `"smil_backcast"` (the country's own), either suffixed
  `"_predecessor"`, `"_sibling_interval"`, `"_aggregate"` or
  `"_shared_polity"` (from the polity reporting for it, by the lineage
  step that found it); or `"pre_synthetic_n"`,
  `"no_n_reported_pre1961"`, `"n_2010_zero"`, `"no_n_t"`,
  `"no_cropland"` (a reporting polity was found but its cropland is
  missing) or `"no_n_2010"`.

- `method_ratio_n_2010`: `"own"`, `"successors"`, `"none"` or
  `"not_needed"` (before 1913).

- `method_ratio_cropland`: the cropland `n_t` is divided by: `"own"`,
  `"successors_luh2_backcast"`, `"none"` or `"not_needed"`.

- `method_dominance`: `"dominance_raw_faostat"` for Linum and Hemp,
  `"not_applicable"` otherwise.

- `method_ratio_spatial`: `"lpjml"`, `"no_lpjml_cell"` or
  `"no_cell_normal"`.

- `method_ratio_temporal`: `"lpjml"`, `"lpjml_recycled_climate"` (years
  before 1901), `"no_lpjml_cell"`, `"no_cell_ratio"` or
  `"no_cell_normal"`.

- `method_regime_yield`: the adjustments applied, joined by `";"`:
  `"anchor_floor"` (SPAM ratio below 1), `"level_cap"` (long-term ratio
  above 10 before the cap), or `"none"`.

Plus the polity columns below.

## Details

Four implementation choices:

- The 1994-2023 normalisers pool every stand, weighted by stand area
  (stand fraction times the cell's geometric area; the land fraction is
  not applied), rather than averaging cell ratios.

- Linum and Hemp dominance pools each country's 1961-2023 production; a
  tie goes to the fibre.

- The floor at 1 applies to the finished composite, not to its members.

- The yield bounds of
  [`split_regime_yield()`](https://eduaguilera.github.io/whep/reference/split_regime_yield.md)
  pool every production row of 1961-2023 with positive tonnes and area,
  whatever its `source`.

The USSR's and Czechoslovakia's cropland before 1961 is their
successors' combined LUH2 cropland (annual plus perennial crop types),
rescaled to the predecessor's own FAOSTAT cropland in 1961 – the same
splicing rule
[`get_arable_permanent_land()`](https://eduaguilera.github.io/whep/reference/get_arable_permanent_land.md)
applies to a single country, which cannot be applied to these two
because their codes have no LUH2 country.

## Anchor

How `spam_crop` is read is fixed by `spam_basis` in
[regime_yield_crop_mapping](https://eduaguilera.github.io/whep/reference/regime_yield_crop_mapping.md):

- A single crop, or crops joined by `+` with basis `"direct"` (millet,
  coffee): harvested area and production are summed over the crops and
  the ratio is taken of the sums.

- `"composite_weighted"` (the forage crops): the mean of the member
  crops' ratios weighted by each member's SPAM harvested area (irrigated
  plus rainfed) in the country. A member with no irrigated or no rainfed
  yield there is dropped and the weights of the others renormalised;
  with no member left, the global composite is used.

- `"product_dominance"` (Linum 772, Hemp 776): `ooil` where the
  country's seed production (linseed 333, hempseed 336) exceeds its
  fibre production (flax 771 "Flax, raw or retted", true hemp 777),
  otherwise `ofib`, both summed over 1961-2023. A country with neither
  product takes the world's dominant product and its global ratio. The
  four products are read from the raw `faostat-production` pin
  (`method_dominance` `"dominance_raw_faostat"`), the same item codes
  WHEP's primary production books them on, so ranking two products needs
  no 1961-2023 production build.

A country with no irrigated or no rainfed yield for the crop in SPAM
takes the crop's global ratio (the ratio of the world's sums). Countries
are matched by ISO3 code onto WHEP's polity buckets, never by name.

## Level

`n_t` is the country's synthetic N over its cropland in year `t`:
FAOSTAT's agricultural use of nitrogen (the
`faostat-fertilizer-nutrients` pin) from 1961, back-cast to 1913 with
the Smil (2001) global series scaled by the country's 1961-1965 share
([smil_2001_synthetic_n_global](https://eduaguilera.github.io/whep/reference/smil_2001_synthetic_n_global.md)),
and zero before 1913, when there was no synthetic nitrogen. The share's
divisor is the Smil series interpolated over 1961-1965 (15.6 Mt), the
same divisor the spatialization scripts' `prepare_nitrogen_inputs()`
uses (issue \#1303). The cropland is
[`get_arable_permanent_land()`](https://eduaguilera.github.io/whep/reference/get_arable_permanent_land.md)
(FAOSTAT from 1961, LUH2 back-cast before).

A country that reports no N in year `t` takes the N per hectare of the
polity that reported fertiliser for its territory that year, found by
walking the `predecessor` edges of
[polities](https://eduaguilera.github.io/whep/reference/polities.md)
with
[`resolve_polity_lineage()`](https://eduaguilera.github.io/whep/reference/resolve_polity_lineage.md):
Russia before 1992 takes the USSR's N over the USSR's cropland. A
historical polity with no 2010 value of its own (the USSR) takes its
successors' combined 2010 N over their combined cropland. Before 1961, a
polity with a back-cast N but no back-cast cropland (the USSR and
Czechoslovakia, whose codes have no LUH2 country) takes its successors'
combined LUH2 cropland, rescaled to its own FAOSTAT cropland of 1961
(`method_ratio_cropland` `"successors_luh2_backcast"`); and a territory
for which nothing reported N in 1961-1965 had no synthetic N before
1961, so its level is 1 (`"no_n_reported_pre1961"`).
`method_ratio_trend` names the path (`"faostat"`,
`"faostat_predecessor"`, ...) and `method_ratio_n_2010` the 2010 basis.
A country with no synthetic nitrogen in 2010 keeps `R_level = 1`. What
no path reaches gets no ratio (`NA`) rather than a guessed one.

## Anomaly

Each item takes the LPJmL crop functional type of
[regime_yield_crop_mapping](https://eduaguilera.github.io/whep/reference/regime_yield_crop_mapping.md)
(`lpjml_cft`), including the `"others"` stand. The cell ratio is the
irrigated over the rainfed per-stand yield of
[`read_lpjml_regime_yield()`](https://eduaguilera.github.io/whep/reference/read_lpjml_regime_yield.md).
The cell's and the country's 1994-2023 ratios are each a ratio of pooled
yields: each regime's yield is the sum of yield times stand area over
the sum of stand area, over the cell's (or the country's cells') stands
and the window years, the LPJmL counterpart of the SPAM anchor. Both
parts are 1 in a cell the LPJmL run does not cover at all (its land mask
misses some coastal and island cells), stamped `"no_lpjml_cell"`. The
spatial part is 1 where the cell has no 1994-2023 ratio; the temporal
part is 1 where the cell has no ratio that year (a regime without a
stand, or no rainfed harvest) or no 1994-2023 ratio. `ratio_anomaly` is
their product, the cell-year ratio over the country's wherever all three
ratios exist.

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
build_regime_yield_ratio(example = TRUE)
#> # A tibble: 10 × 26
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <int>     <int>            <int> <chr>                 <chr>                
#>  1  2010         1                1 ARM-1991-2025         Armenia              
#>  2  2010       110              110 JPN-1952-2025         Japan                
#>  3  2010        41               41 CHN-1950-2025         China (PRC)          
#>  4  2010       203              203 ESP-1800-2025         Spain                
#>  5  2010        41               41 CHN-1950-2025         China (PRC)          
#>  6  2010       138              138 MEX-1848-2025         Mexico               
#>  7  2010       216              216 THA-1909-2025         Thailand             
#>  8  2010       201              201 SOM-1960-2025         Somalia              
#>  9  2010        41               41 CHN-1950-2025         China (PRC)          
#> 10  2010       159              159 NGA-1961-2025         Nigeria              
#> # ℹ 21 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_prod_code <int>, ratio_spam <dbl>, ratio_anchor <dbl>,
#> #   ratio_level <dbl>, ratio_spatial <dbl>, ratio_long_term <dbl>,
#> #   ratio_temporal <dbl>, ratio_anomaly <dbl>, ratio_unbounded <dbl>,
#> #   spam_crop_used <chr>, method_ratio_anchor <chr>, method_ratio_trend <chr>,
#> #   method_ratio_n_2010 <chr>, method_ratio_cropland <chr>,
#> #   method_dominance <chr>, method_ratio_spatial <chr>, …
```
