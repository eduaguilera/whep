# Build the full nitrogen balance: inputs, outputs, losses and NUE.

Assembles
[`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
long-format nitrogen inputs into the aggregate `N_input_*` sums, adds
the output side (crop production, residue use/burning, grazed weeds,
soil organic-matter sequestration), runs the nitrogen-loss cascade
([`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md),
[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md),
[`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md),
[`calculate_indirect_n2o_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_indirect_n2o_nh3.md)),
closes the balance (`N_input_full - N_output_full`), applies the
N-limitation cap on SOM sequestration, and derives
nutrient-use-efficiency (NUE) indicators plus a GWP/CO2e indicator for
the nitrous-oxide streams. Ported from the balance and indicator
equations of an earlier regional historical reconstruction.

## Usage

``` r
build_nitrogen_balance(
  methods = list(nh3 = "manner", n2o = "ipcc2019", n2o_indirect = "ipcc2019", leaching =
    "meisinger_drainage"),
  resolution = c("grid", "polity"),
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  gwp = c("ar6", "ar5", "ar4"),
  example = FALSE
)
```

## Arguments

- methods:

  A named list of method choices: `nh3` (forwarded to
  [`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md),
  default `"manner"`), `n2o` (forwarded to
  [`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md),
  default `"ipcc2019"`, the IPCC 2019 Tier 1 wet/dry factors of Table
  11.1 (Updated) p. 11.13; `"ipcc2019_atl_med"`, `"aguilera"` and
  `"ipcc2006"` are selectable), `n2o_indirect` (forwarded to
  [`calculate_indirect_n2o_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_indirect_n2o_nh3.md),
  default `"ipcc2019"`, the wet/dry EF4 of Table 11.3 (Updated) p.
  11.26, or `"ipcc2019_atl_med"`), `leaching` (forwarded to
  [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md),
  default `"meisinger_drainage"`, or `"ipcc_fracleach"`, the wet/dry
  FracLEACH-(H) of Section 11.2.2.2 p. 11.25) and `regime`, how each
  grid row is split into a rainfed and an irrigated part (see Details):
  `"yield_split"` (default), `"area_split"` or `"none"`. The IPCC 2019
  wet/dry methods read `moisture_regime` from the driver tables in
  `data`.

- resolution:

  `"grid"` (default) or `"polity"`, as in
  [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md);
  `"polity"` sums every term (and re-derives every indicator) over
  cells.

- polity_validity:

  What to do with a row whose `(area_code, year)` resolves to a polity
  that did not exist in that year (the cell-polity crosswalk has no year
  dimension, so an early-20th-century cell is labelled with its
  present-day territory). `"keep"` (default) keeps every row, which is
  the historical behaviour, and warns naming the rows, years and area
  codes involved. `"flag"` keeps them and adds the per-row logical
  `reporting_polity_out_of_span`, marking exactly which rows are
  stand-ins. `"drop"` removes them. All three warn; only `"drop"`
  changes the numbers. See
  [`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md),
  which reports the same rows for an already-built table.

- data:

  Named list of pre-loaded upstream inputs. `n_inputs`
  ([`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
  output) is used directly when supplied, else
  [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
  is called with this SAME `data` list (so its nested readers, e.g.
  `bnf_input`/`npp_n_input`/`livestock_intake`, propagate through). Also
  required:

  - `npp_n_input`: shared with
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
    `"recycling"` term (see
    [`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md));
    used here for `prod_n_t`.

  - `ag_land_support`: the
    [`build_ag_land_support()`](https://eduaguilera.github.io/whep/reference/build_ag_land_support.md)
    table, supplying `area_ha` (the per-hectare boundary denominator) on
    a physical-land basis. Derived natively when absent and derivable,
    and shared with
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
    so both see one table; when neither supplied nor derivable,
    `area_ha` falls back to the harvested area the NPP input carries.

  - `residue_destiny_input`:
    [`calculate_residue_destinies()`](https://eduaguilera.github.io/whep/reference/calculate_residue_destinies.md)'s
    required input (`item_prod_code`, `residue_dm_t`, plus whatever the
    chosen `residue_destiny_method` needs; the polity's
    `region_krausmann` and `region_un_sub` are taken from `area_code`
    when absent), for
    `used_residue_n_t`/`bedding_residue_n_t`/`burnt_residue_n_t`.
    `residue_destiny_method` selects the method (default
    `"recovery_regional"`), `residue_destiny_recovery` its recovery-rate
    variant (default `"wirsenius"`, the package default),
    `residue_bedding` the bedding rule (default `"wirsenius"`, 12.4% of
    the recovered cereal straw) and, under
    `residue_bedding = "fraction"`, `residue_bedding_fraction` the share
    of the recovered non-feed residue used as bedding (default `0`; see
    [`calculate_residue_destinies()`](https://eduaguilera.github.io/whep/reference/calculate_residue_destinies.md)).
    The same arguments split the residue of `npp_n_input` for the
    `"recycling"` input of
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md),
    which returns to the soil only the residue left on the field
    (`recycling_residue_basis`, default `"residue_soil_returned"`;
    `"total_residue"` recycles the gross residue and is recorded in
    `method_recycling_n`). An `npp_n_input` that already carries
    `residue_soil_dm_t` keeps it: the recycling then uses the caller's
    destinies while the removal outputs use `residue_destiny_input` and
    these arguments, so the soil-returned and removed residue add up to
    the gross residue only when both come from the package's own destiny
    split. The bedded straw also comes back as a manure input, through
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md),
    read from the same split as `bedding_residue_n_t`.
    `residue_feed_share` selects that split's `feed_share` (default
    `"wirsenius"`).

  - `regime_areas`, `regime_ratio`, `regime_production`,
    `regime_shares`: the rainfed/irrigated split's inputs (see Details),
    each read or built when absent: the rainfed and irrigated hectares
    per cell and production item (`lon`, `lat`, `area_code`,
    `item_prod_code`, `year`, `rainfed_ha`, `irrigated_ha`; built with
    [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)),
    the
    [`build_regime_yield_ratio()`](https://eduaguilera.github.io/whep/reference/build_regime_yield_ratio.md)
    table (built on the NPP rows; pass the table
    [`build_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_carbon_inputs.md)
    reads to make both chains apply one ratio), and the
    [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
    table the yield bounds of
    [`split_regime_yield()`](https://eduaguilera.github.io/whep/reference/split_regime_yield.md)
    are pooled from. `regime_shares` replaces all three with pre-built
    shares per grid key (`lon`, `lat`, `area_code`, `item_cbs_code`,
    `year`, `irrigated_area_share`, `irrigated_yield_share`), which
    carry no production item: the residue terms then follow the
    production share.

  - `livestock_intake`: shared with
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
    manure term; its `"grass"` `feed_quality` rows drive
    `grazed_weeds_n_t`.

  - `carbon_balance`: shared with
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
    `"som_ mineralization"` term; its `son_change_kgn_ha` drives
    `som_ sequestration_n_t`.

  - `n_balance_drivers` and `n_balance_leaching_drivers`: the loss
    drivers. At grid resolution each one left `NULL` is built by
    [`build_n_loss_drivers()`](https://eduaguilera.github.io/whep/reference/build_n_loss_drivers.md)
    on the split balance's own rows (the IPCC 2019 wet/dry class and the
    ATL/MED drivers mapped from it; see its table of which methods it
    supplies), and `method_drivers` says so. That needs the
    rainfed/irrigated split (`methods$regime` other than `"none"`);
    `leaching = "meisinger_drainage"` and the MANNER `nh3` methods read
    drivers it does not supply and are refused before any input is
    assembled unless their table is supplied. At polity resolution both
    must be supplied.

  - `n_loss_driver_inputs`: optional list passed as `data` to
    [`build_n_loss_drivers()`](https://eduaguilera.github.io/whep/reference/build_n_loss_drivers.md)
    when it runs (`climate`, `crop_depth`, `lpjml_years`, `lpjml_grid`,
    `regime_areas`, `moisture_regime`); the balance's own `country_grid`
    is shared with it.

  - `n_balance_drivers`: additional per-`(key, fert_type)` driver
    columns
    [`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md)/[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md)/
    [`calculate_indirect_n2o_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_indirect_n2o_nh3.md)
    need beyond `n_input_t`/`fert_type` (`moisture_regime` for the IPCC
    2019 wet/dry methods, `climate` and `irrig_type` for the ATL/MED
    ones, plus every MANNER driver column when
    `methods$nh3 == "manner"`), joined by the balance key AND
    `fert_type` (the Title-case vocabulary of the loss functions:
    `"Synthetic"`, `"Solid"`, `"Liquid"`, `"Excreta"`, `"Human"`,
    `"Recycling"`, `"SOM"`). The excreta rows' species mix,
    `excreta_cpp_share`, comes from
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md),
    N-weighted over the rows pooled into each loss row. A second set of
    balance-key-only driver columns (`moisture_regime`, `fert_type`,
    `climate`, `irrig_cat`, `tillage`, `som_share`, `cn_input`,
    `land_use`) for
    [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)
    is read from `n_balance_leaching_drivers`, which must hold at most
    one row per balance key (a many-to-one join aborts on duplicate keys
    rather than fanning the rows out and misaligning `drainage_mm`).
    Missing required drivers abort inside the called `calculate_*()`
    function, naming the exact column; for the MANNER `nh3` methods the
    check runs on `n_balance_drivers` before any input is assembled,
    because no function in the package supplies those drivers (only
    `windspeed_ms` has a reader,
    [`read_lpjml_wind()`](https://eduaguilera.github.io/whep/reference/read_lpjml_wind.md),
    not wired in).

  - `drainage_mm`: annual drainage (mm) for
    [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md),
    as a numeric vector aligned to the balance-key rows, or already
    present as a `drainage_mm` column via `n_balance_leaching_drivers`.
    At grid resolution the rainfed/irrigated split doubles the rows, so
    an aligned vector only fits with `methods$regime = "none"`;
    otherwise supply drainage as a column of
    `n_balance_leaching_drivers`.

- gwp:

  100-year global warming potential standard for N2O, `"ar6"` (default),
  `"ar5"` or `"ar4"`, matching
  [`build_crop_soil_n2o_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_soil_n2o_extension.md).

- example:

  If `TRUE`, return a small fixture instead of assembling real data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `year`/`area_code`/`item_cbs_code` (plus `lon`/`lat`
at `resolution = "grid"`, and `water_regime`, `"rainfed"` or
`"irrigated"`, at grid resolution unless `methods$regime = "none"`) with
`area_ha` (each crop's harvested hectares in the cell, summed over cells
at `resolution = "polity"`; used downstream to convert tonnes N to a
per-hectare rate), the input aggregates (`n_input_full_t`,
`n_input_full_nosom_t`, `n_input_std_t`, `n_input_som_t`,
`n_input_for_n2o_t`), the output aggregates (`n_output_residues_t`,
`n_output_som_t`, `n_output_useful_t`, `n_output_std_t`,
`n_output_full_t`), the loss terms (`nh3_n_t`, `n2o_direct_n_t`,
`no3_n_t`, `denitrification_n_t`, `retained_profile_n_t`,
`n2o_indirect_no3_n_t`, `n2o_indirect_nh3_n_t`;
`no3_n_t + denitrification_n_t + retained_profile_n_t` is the surplus,
see
[`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)),
the (post-cap) `som_sequestration_n_t`, `n_balance_t`, `surplus_t`,
`surplus_share`, the five NUE ratios (`nue_std`, `nue_residues`,
`nue_som`, `nue_useful`, `nue_full`), `total_gwp_co2e_kg`, and the
`method_nh3`/
`method_soil_n2o`/`method_n2o_indirect_nh3`/`method_leaching` provenance
columns (the per-row stamps of the loss functions; a balance row pooling
loss rows priced differently lists them joined by `"|"`, and a row with
no N2O-relevant input, which emits no direct or NH3-indirect N2O,
carries the method chosen suffixed `"_no_loss_row"`, e.g.
`"ipcc2019_no_loss_row"`), `method_drivers` (`"whep_built"`,
`"supplied"` or `"supplied|whep_built"`), and, when the leaching drivers
carry them (as built ones do), the row's wet/dry `moisture_regime`, how
it was obtained (`method_moisture_regime`) and `climate_fill_km`,
`method_regime` and `method_regime_split` when the split ran
(`"yield_ratio"`; `"area_no_production"` where the crop's production
could not be expressed in fresh weight and the area share was used for
everything; `"regime_shares"` for shares supplied as
`data$regime_shares`; or `"no_regime_share"` for a row booked wholly
rainfed because the regime layer does not cover it), and
`method_residue_regime`, how the row's residue terms were split, the
rules of its production items joined by `"|"`: `"part_yield"`
(re-evaluated at a rainfed part's yield),
`"part_yield_irrigation_terms"` (re-evaluated at an irrigated part's
yield, with the model's irrigation terms), `"crop_density"` (the item's
per-hectare values on the part's area: no part yield, or `"area_split"`)
or `"yield_share"` (the production share, for shares supplied without
production items), `NA` on a row with no residue term, plus the polity
columns below. When the supplied `n_inputs` carry them, the
`method_recycling_n`, `method_synthetic`, `method_deposition`,
`method_deposition_scope`, `method_human_population`,
`method_human_kgn_cap`, `method_unsupported`, `method_manure` and
`method_unattributed` stamps from
[`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
are carried through as well, so a balance names the input conventions
that produced it. Gains `reporting_polity_out_of_span` when
`polity_validity = "flag"`.

## Details

**Rainfed/irrigated split.** At grid resolution each crop row is split
into a rainfed and an irrigated part before the losses are computed, so
regime-specific loss drivers can attach. Two shares per row drive it:
the irrigated share of the crop's cell area, from the spatialization
chain's rainfed/irrigated hectares
([`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)),
and the irrigated share of its production, from
[`split_regime_yield()`](https://eduaguilera.github.io/whep/reference/split_regime_yield.md)
on the ratio of
[`build_regime_yield_ratio()`](https://eduaguilera.github.io/whep/reference/build_regime_yield_ratio.md).
Synthetic N and production N split by the production share, on the
assumption that N availability per unit of yield is the same in both
regimes; every other input, the harvested area, grazed weeds and SOM
sequestration split by the area share. The residue terms – the residue
and root N returned to soil (`"recycling"`) and the used, bedding and
burnt residue N – are evaluated per part: for each production item of
the row, the parts' yields keep its production
(`Y_r = Y / (1 + s (R - 1))`, `Y_i = R Y_r`), and each part carries the
item's residue and root N per hectare times the response of
[`calculate_crop_residues()`](https://eduaguilera.github.io/whep/reference/calculate_crop_residues.md)
and
[`calculate_crop_roots()`](https://eduaguilera.github.io/whep/reference/calculate_crop_roots.md)
from the item's mean yield to the part's yield, in the part's water
regime, so the model's irrigation terms (a lower residue ratio for the
crop groups that respond, a lower root:shoot ratio) apply to the
irrigated part. On the default chains, whose NPP input is built with the
polity's `region_hanpp` and without `n_input_kg_ha`, the item's residue
and root per hectare are the model's at its mean yield, so each part's
are the model's at the part's own yield; with any other NPP input they
are the item's own density scaled by the model's response. Residue and
root are affine in yield, so a high-yield hectare carries less than its
yield share. Summing the two parts reproduces the unsplit balance for
every additive column except the residue terms and the totals that
contain them, which sum to the parts' evaluation instead: with the
irrigation terms, the residue and root N of irrigated land fall. The
bedded straw the manure input brings back is the parts' bedding: when
the balance builds its own inputs and beds any straw, each production
item's bedding is scaled by its factor over the two parts before it
reaches the manure engine, so the manure columns, too, differ from an
unsplit run by the bedding the irrigation terms take off, and the bedded
N that leaves the field is still the bedded N that returns. A non-zero
residue term at a key the per-item evaluation does not cover means the
balance's inputs and the NPP-N table disagree, and aborts.
`"area_split"` uses the area share for everything, residue terms
included, and `"none"` returns the unsplit balance. Loss-driver tables
without a `water_regime` column apply to both parts.

`polity_validity` is forwarded to
[`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
– which forwards it in turn to
[`build_ag_land_support()`](https://eduaguilera.github.io/whep/reference/build_ag_land_support.md),
[`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md),
[`build_human_n()`](https://eduaguilera.github.io/whep/reference/build_human_n.md)
and
[`spatialize_country_n_to_crops()`](https://eduaguilera.github.io/whep/reference/spatialize_country_n_to_crops.md)
– and then applied to the balance rows themselves, so one choice governs
the whole build (whep#727). A `data$n_inputs` table supplied directly is
left alone: it was built by its own call with its own choice, and the
balance rows derived from it still get this argument's fate at the tail.

**Grazing excreta.** The `"excreta"` input is the N grazing animals
deposit on pasture, range and paddock, and its N2O is in
`n2o_direct_n_t`, `n2o_indirect_nh3_n_t` and `total_gwp_co2e_kg`, as the
field loss it is.
[`calculate_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/calculate_livestock_emissions.md)
prices the same emission and reports it as `manure_n2o_direct_pasture`
and `manure_n2o_indirect_pasture`, and that is the chain WHEP's GHG
footprint takes it from. So these balance columns are not to be added to
livestock manure N2O: subtract the pasture columns from one of the two
first (whep#1524).

## Loss drivers

Which driver columns each loss method reads, and whether
[`build_n_loss_drivers()`](https://eduaguilera.github.io/whep/reference/build_n_loss_drivers.md)
supplies them. At grid resolution that function builds each driver table
left `NULL`; at polity resolution both tables must be supplied.

|  |  |  |  |
|----|----|----|----|
| Method | Driver columns | Supplied | Status |
| `nh3`: `"ipcc"` | `fert_type` | yes | built |
| `nh3`: `"manner"`, `"manner_default"` | MANNER's | no | note 1 |
| `n2o`: `"ipcc2019"` | `moisture_regime` | yes | built |
| `n2o`: `"ipcc2019_atl_med"` | `climate` | yes | built |
| `n2o`: `"aguilera"`, `"ipcc2006"` | `climate`, `irrig_type` | yes | built |
| `n2o_indirect`: `"ipcc2019"` | `moisture_regime` | yes | built |
| `n2o_indirect`: `"ipcc2019_atl_med"` | `climate`, `irrig_type` | yes | built |
| `leaching`: `"ipcc_fracleach"` | `moisture_regime`, `water_regime` | yes | built |
| `leaching`: `"meisinger_drainage"` | Meisinger's | no | note 2 |

- Note 1: MANNER reads `manner_fertiliser`, `soil_ph`, `rate_kg_ha`,
  `rainfall_mm`, `irrigated`, `temp_c`, `temp_c_annual_mean`,
  `windspeed_ms`, `technique`, `system`, `incorporation_delay_h` and
  `species` (see
  [`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md)).
  Drivers must be supplied; global drivers planned.

- Note 2: Meisinger reads `fert_type`, `climate`, `irrig_cat`,
  `land_use`, `cn_input`, `tillage`, `som_share` and `drainage_mm` (see
  [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)).
  Drivers must be supplied; global drivers planned.

`build_nitrogen_balance()` refuses a method of note 1 or note 2 before
assembling any input unless its driver table is supplied.

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
build_nitrogen_balance(example = TRUE)
#> # A tibble: 1 × 45
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2020        10               10 AUS-1901-2025         Australia            
#> # ℹ 40 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_cbs_code <int>, area_ha <dbl>, n_input_full_t <dbl>,
#> #   n_input_full_nosom_t <dbl>, n_input_std_t <dbl>, n_input_som_t <dbl>,
#> #   n_input_for_n2o_t <dbl>, prod_n_t <dbl>, used_residue_n_t <dbl>,
#> #   bedding_residue_n_t <dbl>, burnt_residue_n_t <dbl>, grazed_weeds_n_t <dbl>,
#> #   som_sequestration_n_t <dbl>, n_output_residues_t <dbl>,
#> #   n_output_som_t <dbl>, n_output_useful_t <dbl>, n_output_std_t <dbl>, …
```
