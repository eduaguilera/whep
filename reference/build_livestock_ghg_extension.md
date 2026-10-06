# Build the livestock greenhouse-gas emissions extension.

Aggregate per-animal IPCC livestock emissions into a footprint extension
keyed by `(year, area_code, item_cbs_code)`, expressed in kilograms of
carbon-dioxide equivalent (CO2e). This bridges the cohort-level
emissions pipeline
([`calculate_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/calculate_livestock_emissions.md))
to the input-output grain used by
[`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
and
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md),
exactly like
[`build_grassland_land_extension()`](https://eduaguilera.github.io/whep/reference/build_grassland_land_extension.md)
does for land.

Live-animal head counts come from
[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md),
are bridged to IPCC species with
[`prepare_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/prepare_livestock_emissions.md),
and the resulting enteric and manure emissions are converted to CO2e and
summed back to the live-animal commodity sector (`item_cbs_code`, e.g.
961 for non-dairy cattle), which is itself a sector in
[`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md).

Two IPCC tiers are available, selected with `tier`:

- `1` (default): Tier 1 regional emission factors (IPCC 2019). It needs
  only species, country and head counts, so it is complete for every
  country in
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md).
  It covers enteric and manure **methane** and manure **N2O** (direct
  and indirect, from default per-head nitrogen excretion rates).

- `2`: Tier 2 cohort energy balance (IPCC 2019). It derives enteric CH4
  and manure N2O from a per-animal energy and nitrogen balance, for
  finer resolution, but requires cohort weight and diet inputs. It
  covers cattle, buffalo, sheep and goats; swine, poultry, horses, mules
  and asses and camels, for which the IPCC suggests Tier 1, take Tier 1
  by default (see `tier2_uncovered` under `options`), so their sectors
  show `"IPCC_2019_Tier1"` in `method_manure_ch4`. Rows that still
  cannot be resolved (missing diet data) are dropped with a warning
  rather than entering the footprint as `NA`. Its per-head enteric and
  manure emissions now sit in the same range as the Tier 1 regional
  factors. Tier 1 remains the default because it is complete for every
  country in
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md),
  whereas Tier 2 needs cohort and diet inputs.

The CO2e conversion uses 100-year global warming potentials selected
with `gwp`:

- `"ar6"` (default): IPCC AR6 (2021) Table 7.15, biogenic CH4 = 27, N2O
  = 273.

- `"ar5"`: IPCC AR5 (2013), CH4 = 28, N2O = 265 (no climate-carbon
  feedback).

- `"ar4"`: IPCC AR4 (2007), CH4 = 25, N2O = 298.

`options` is handed to
[`calculate_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/calculate_livestock_emissions.md)
unchanged, so the manure engine's method levers (which half of the
manure-management table is read and how it is keyed, and which methane
conversion factor table is read at which climate zone) are selectable
from here too; passing none takes the engine's own defaults, so this
entry point publishes what the engine does. `mms_shares` and
`mms_region` bite at both tiers; the `tier = 1` default here is
unaffected by `mcf_source` and by the climate options, because Tier 1
manure CH4 comes from regional emission factors rather than a
climate-zone MCF and both reach the MCF on the Tier 2 path only.
Whichever choice each row took is recorded in `method_mms` and
`method_manure_ch4`, which the extension carries into its own output.

## Usage

``` r
build_livestock_ghg_extension(
  tier = 1,
  gwp = c("ar6", "ar5", "ar4"),
  method_diet = c("per_cell_feed", "national_feed", "uniform_medium"),
  options = list(),
  data = list(),
  example = FALSE
)
```

## Arguments

- tier:

  IPCC tier, `1` (default) or `2`.

- gwp:

  100-year global warming potential standard, `"ar6"` (default), `"ar5"`
  or `"ar4"`.

- method_diet:

  How Tier 2 resolves each herd's `diet_quality`, which sets DE% and so
  gross energy, enteric CH4, volatile solids and nitrogen excretion at
  once. `"per_cell_feed"` (default) derives it from the feed mix of the
  cell the herd is in; `"national_feed"` from the country's own mix;
  `"uniform_medium"` assumes the IPCC `"Medium"` diet for every herd.
  The gridded rung is the default because WHEP resolves a diet per cell
  and a diet varies within a country, so a national mix is a coarsening
  and an assumed Medium is coarser still. Both remain selectable. The
  assumption is never chosen implicitly, and the rung used is recorded
  in `method_ghg`. Ignored at Tier 1, whose emission factors carry no
  diet dimension.

- options:

  A named list of manure-engine options. All but five defaults reproduce
  the behaviour in force before whep#949. The exceptions are
  `mcf_source`, which moved from the shipped table to the 2019
  Refinement in whep#1022 and does move Tier 2 manure CH4, `mms_shares`,
  which moved from the unsourced placeholder table to the GLEAM 2.0
  ingest in whep#958 and does move both tiers' manure N2O, `pasture_bo`,
  which since whep#1137 pairs the 2019 pasture MCF with its published
  `Bo` and moves Tier 2 manure CH4, and `tier2_uncovered`, which since
  whep#1028 gives species with no Tier 2 method their Tier 1 values
  instead of `NA`, and `indirect_n2o_source`, which since whep#1245
  reads the 2019 Refinement's leaching factors and moves both tiers'
  indirect manure N2O.

  `indirect_n2o_source` selects the edition of
  [indirect_n2o_ef](https://eduaguilera.github.io/whep/reference/indirect_n2o_ef.md)
  the indirect manure N2O reads:

  - `"ipcc_2019"` (default): EF5 0.011 and FracLEACH-(H) 0.24, from Vol
    4, Ch 11, Table 11.3 (Updated), p. 11.26 of the 2019 Refinement, the
    current IPCC guidance and the EF5 the nitrogen balance already uses.

  - `"ipcc_2006"`: EF5 0.0075 and FracLEACH-(H) 0.30, from Table
    11.3, p. 11.24 of the 2006 Guidelines. These are the values WHEP
    shipped before whep#1245 under a 2019 citation, kept selectable so
    earlier figures stay reproducible.

  EF4 (0.010) and FracGasMS (0.20) are the same under both. Relative to
  `"ipcc_2006"` the default raises the leaching term by
  `0.24 * 0.011 / (0.30 * 0.0075) = 1.173` and leaves the volatilisation
  term alone. `method_manure_n2o` records the edition used
  (`indirect_ipcc_2019` or `indirect_ipcc_2006`).

  `mms_shares` selects which half of
  [regional_mms_distribution](https://eduaguilera.github.io/whep/reference/regional_mms_distribution.md)
  the split is read from: `"gleam_2_0"` (default) is the GLEAM 2.0
  Supplement S1 Tab. 4.2-4.11 ingest, `"placeholder"` the unsourced
  table it replaced in whep#958. The placeholder stays selectable so the
  values WHEP published before that ingest remain reproducible and the
  sensitivity to it stays measurable; it is not a defensible alternative
  estimate.

  `mms_region` selects how the manure-management split in
  [regional_mms_distribution](https://eduaguilera.github.io/whep/reference/regional_mms_distribution.md)
  is keyed:

  - `"as_available"` (default): a row uses its own region when the frame
    already carries a `region` column, and the `region == "Global"`
    split otherwise. Tier 1 resolves a region for the (sourced) per-head
    N-excretion table and so takes the region-specific split; Tier 2
    carries no region and so takes the Global one.

  - `"resolve"`: the IPCC region is resolved from `iso3`, `area_code` or
    `polity_area_code` where it is missing, which makes the table's
    region-specific rows live on the Tier 2 path too. Opt-in because it
    changes which rows of the table apply, not because the rows are
    doubtful: since whep#958 they are the GLEAM 2.0 ingest.

  - `"global"`: every row takes the `region == "Global"` split, whatever
    region column it carries.

  `mcf_source` selects which methane conversion factor table the Tier 2
  manure CH4 weighting reads:

  - `"ipcc_2019"` (default): the matching `edition` rows of
    [climate_mcf_ipcc](https://eduaguilera.github.io/whep/reference/climate_mcf_ipcc.md),
    read off Table 10.17 (Updated) of the 2019 Refinement, which is the
    current IPCC guidance.

  - `"ipcc_2006"`: the matching `edition` rows of
    [climate_mcf_ipcc](https://eduaguilera.github.io/whep/reference/climate_mcf_ipcc.md),
    read off Table 10.17 of the 2006 Guidelines.

  - `"as_shipped"`:
    [climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md),
    whose live rows are predominantly the 2006 Guidelines Table 10.17
    but with six cells that match no published IPCC value (whep#601,
    whep#1022). Kept selectable so an older run can be reproduced; it is
    no longer the default, because values whose provenance could not be
    established should not be what ships.

  Neither edition publishes one number per Cool/Temperate/Warm zone for
  every system, so both as-published tables apply a stated collapse
  rule; see
  [climate_mcf_ipcc](https://eduaguilera.github.io/whep/reference/climate_mcf_ipcc.md).
  Both rules are WHEP's, not the IPCC's, and the default makes them
  live. `method_manure_ch4` records the table used.

  `pasture_bo` selects the methane potential (`Bo`) the Tier 2 manure
  CH4 prices the pasture/range/paddock stream at. The 2019 Refinement
  publishes its single 0.47 percent pasture MCF as half of a pair that
  "must always be used in conjunction with a B0 value of 0.19" (Vol 4,
  Ch 10, Table 10.17 (Updated) footnote 2, p. 10.70; Section 10.4.2, p.
  10.66); the pair is the `paired_bo_m3_kg_vs` column of
  [climate_mcf_ipcc](https://eduaguilera.github.io/whep/reference/climate_mcf_ipcc.md)
  (whep#1137).

  - `"paired"` (default): a stream whose MCF row carries a paired `Bo`
    is priced at it, every other stream at the animal-category `Bo` of
    [ipcc_tier2_bo_values](https://eduaguilera.github.io/whep/reference/ipcc_tier2_bo_values.md).
    Only `mcf_source = "ipcc_2019"` publishes a pair, so under the other
    two sources this changes nothing.

  - `"species"`: every stream takes the animal-category `Bo`, the
    behaviour before whep#1137. Under `"ipcc_2019"` that is the hybrid
    the Refinement rejects; kept so the earlier figures stay
    reproducible and the sensitivity to the pairing stays measurable.

  `method_manure_ch4` records per row which applied (`pasture_bo_paired`
  or `pasture_bo_species`) wherever the row has manure on a stream that
  carries a published pair.

  `climate_source` selects where the climate zone the methane conversion
  factors in the MCF table are read at comes from. A `climate_zone` a
  row already carries is always used and stamped `climate_from_data`;
  the option governs only the rows left without one, whether that is a
  hole in a supplied column or a wholly absent column.

  - `"assumed"` (default): fill with `assumed_climate_zone`.

  - `"from_data"`: abort instead of assuming.

  `assumed_climate_zone` is the zone `"assumed"` fills in: `"Cool"`,
  `"Temperate"` (default) or `"Warm"`. It is an assumption, not a
  measurement; `method_manure_ch4` records per row which of the sources
  applied, and this argument exists so the sensitivity to the assumption
  can be measured (whep#949).

  `tier2_uncovered` says what the Tier 2 path does with a species WHEP
  has no Tier 2 method for. The energy balance needs the maintenance and
  activity coefficients of Tables 10.4 and 10.5, which
  [ipcc_tier2_energy_coefs](https://eduaguilera.github.io/whep/reference/ipcc_tier2_energy_coefs.md)
  ships for cattle, buffalo, sheep and goats only; the 2019 Refinement
  itself suggests Tier 1 for camels, horses, mules and asses and swine
  and has no enteric method for poultry (Vol. 4 Ch. 10, Table 10.9
  (Updated)), and its Tier 2 manure equations for swine and poultry need
  a country-specific dry-matter intake (Equation 10.32A) that WHEP does
  not hold. Although it sits among the manure-engine options, it governs
  the enteric path too.

  - `"tier1"` (default): those species take the Tier 1 enteric CH4,
    manure CH4 and manure N2O, written into the Tier 2 output columns
    and stamped `"IPCC_2019_Tier1"` in `method_enteric`,
    `method_manure_ch4` and `method_manure_n2o`, with a message naming
    them. This is the IPCC's own suggested method for them, so a Tier 2
    inventory keeps the whole herd rather than silently covering fewer
    animals than Tier 1.

  - `"leave_na"`: they keep `NA` emissions, the behaviour before
    whep#1028, with a warning naming them. Kept so a ruminant-only Tier
    2 figure stays reproducible.

  - `"abort"`: any such species aborts, naming it.

- data:

  Optional named list of pre-loaded inputs to avoid remote reads:
  `primary_prod` (the
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  output) and, for Tier 2 with either feed-derived diet, `feed_intake`
  (the
  [`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md)
  output). `primary_prod` falls back to its reader when absent;
  `feed_intake` does not, because
  [`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md)
  rebuilds the whole feed allocation and would silently turn this
  extension into an hours-long build. Supply it, or choose
  `method_diet = "uniform_medium"`.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(livestock emissions in kilograms CO2e), `method_ghg` (the chosen tier
and GWP standard, e.g. `"IPCC_2019_Tier1_AR6"`), `method_mms` and
`method_manure_ch4` (the manure-engine choices the summed rows took,
`NA` when nothing reached the manure engine), plus the polity columns
below.

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
build_livestock_ghg_extension(example = TRUE)
#> # A tibble: 6 × 11
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  1986        10               10 AUS-1901-2025         Australia            
#> 2  1986        10               10 AUS-1901-2025         Australia            
#> 3  1986        10               10 AUS-1901-2025         Australia            
#> 4  1986       100              100 IND-1949-2025         India                
#> 5  1987        10               10 AUS-1901-2025         Australia            
#> 6  1987       100              100 IND-1949-2025         India                
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>, item_cbs_code <int>,
#> #   impact_u <dbl>, method_ghg <chr>, method_mms <chr>, method_manure_ch4 <chr>
```
