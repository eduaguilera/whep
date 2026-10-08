# Build per-cell livestock greenhouse-gas emissions.

Run the IPCC 2019 livestock emission model on a gridded herd, one
0.5-degree cell at a time, and return enteric CH4, manure CH4 and manure
N2O in kilotonnes per cell, year and species.

Unlike a spatial disaggregation of a national emission total, this
resolves the drivers that actually vary within a country: the
manure-management climate zone and the ambient temperature come from the
cell
([`build_cell_climate_zone()`](https://eduaguilera.github.io/whep/reference/build_cell_climate_zone.md)),
and the diet quality comes from the cell's own feed mix when one is
available. A national total spread over cells cannot show any of that,
because every cell of a country then carries the same implied climate
and diet.

**Both grains are reported.** The gridded sum is the primary output. The
same model is also run once per country on the head-weighted national
mean temperature and the national diet, and each cell additionally
carries that national estimate rescaled onto it (`*_national_kt`) plus
the per-country ratio between the two (`divergence_*`). The global
difference between the two grains is small while the per-country
difference is not, so the per-country ratio is emitted per row rather
than summarised away.

## Usage

``` r
build_gridded_livestock_emissions(
  gridded_livestock = NULL,
  method_diet = c("per_cell_feed", "national_feed", "uniform_medium"),
  method_species = c("national_head_share", "refuse"),
  method_climate_gap = c("nearest_cell", "drop", "refuse"),
  tier = 2,
  options = list(),
  data = list(),
  example = FALSE
)
```

## Arguments

- gridded_livestock:

  A tibble of gridded head counts (required unless `example = TRUE`),
  for example from
  [`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md),
  with columns `lon`, `lat`, `year`, `area_code`, `heads`, and either
  `species` (an IPCC species label such as `"Cattle, dairy"`) or
  `species_group` (a spatializer group label). Groups that name more
  than one IPCC species (`"sheep_goats"`, `"equines"`, `"poultry"`,
  `"other"`) are split as `method_species` says. Polity columns and cell
  identifiers are preserved when present.

- method_diet:

  How each row's `diet_quality` is resolved, in decreasing rigour:

  - `"per_cell_feed"` (default): from the cell's own feed mix, falling
    back per row to that country's national mix where a cell has no
    classifiable feed. Needs `data$feed_intake` at cell grain (a
    `sub_territory` column), for example from
    [`build_feed_intake_local()`](https://eduaguilera.github.io/whep/reference/build_feed_intake_local.md).

  - `"national_feed"`: from the country's feed mix. Needs
    `data$feed_intake`, for example from
    [`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md).

  - `"uniform_medium"`: every row gets the IPCC `"Medium"` diet. This is
    an assumption, not a measurement, and is never selected implicitly.

  Whatever is requested, the value actually used is recorded per row in
  `method_diet`. A row that no requested method resolves aborts.

- method_species:

  How a `species_group` that names more than one IPCC species is split
  into its species:

  - `"national_head_share"` (default): each cell's group head count is
    divided among the group's members in the proportions the country
    itself reports, from the national head counts by live-animal item
    (`data$species_heads`). The members are read from the same
    `livestock_mapping.csv` the spatializer summed them with, so the
    split inverts that grouping at national level: a country's gridded
    species mix equals its reported one, and the group's gridded head
    total is conserved exactly. It assumes the species mix is the same
    in every cell of a country, which is also what the spatializer
    assumed, because it gave all members of a group one spatial proxy.
    Countries are matched on `area_code`; a cell whose `area_code` the
    head table does not carry is matched on `polity_area_code` instead
    (for example Sudan and South Sudan, reported together as polity
    206), and `method_species` says which. A cell matched by neither is
    returned with `NA` emissions and a warning, never split on an
    invented ratio.

  - `"refuse"`: abort on any aggregate group, the behaviour before
    whep#1126.

  The rung used is recorded per row in `method_species`.

- method_climate_gap:

  What happens to a cell that has no row in the climate table, which is
  where the climate land mask disagrees with the livestock grid
  (coastlines, reclaimed land, small islands):

  - `"nearest_cell"` (default): the cell takes the mean annual
    temperature of its nearest climate cell in the same year
    (great-circle distance between cell centres; equidistant cells are
    averaged), and its zone is classified from that.
    `method_climate_zone` reads `"nearest_cell; <the donor's method>"`.
    Only cells up to three grid steps away are searched; a gap with none
    that close aborts, because it means the climate table does not cover
    the herd.

  - `"drop"`: such rows are removed, with a warning naming their head
    count. Their emissions are then absent from every total.

  - `"refuse"`: abort, the behaviour before whep#1126.

  No option gives a gap cell a default zone.

- tier:

  IPCC tier, `2` (default) or `1`. Tier 2 is the default here because
  the per-cell drivers only enter the Tier 2 energy and
  manure-management equations; Tier 1 emission factors carry no climate,
  temperature or diet dimension, so a Tier 1 grid differs from a
  disaggregated national total only by rounding.

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

  Optional named list of pre-loaded inputs: `cell_climate` (a
  [`build_cell_climate_zone()`](https://eduaguilera.github.io/whep/reference/build_cell_climate_zone.md)
  output), `feed_intake` (a feed-intake table) and `species_heads`
  (national head counts by live-animal item, with `year`, `area_code`,
  `item_cbs_code` and `value` or `heads`, optionally `polity_area_code`
  and `unit`; rows with a `unit` other than `"heads"` are ignored,
  except `"t_head"` rows, which give the milk yield, see section "Milk
  yield"). `cell_climate` falls back to
  [`build_cell_climate_zone()`](https://eduaguilera.github.io/whep/reference/build_cell_climate_zone.md),
  which reads CRU from `WHEP_CRU_DIR`. `species_heads` falls back to
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md),
  the source the spatializer's country table is built from, and is read
  only when an aggregate group is present. `feed_intake` has no
  fallback: the readers that produce it rebuild the whole feed
  allocation, so it is supplied or the diet method is
  `"uniform_medium"`.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with one row per `year`, `area_code`, `lon`, `lat` and
`species`:

- `heads`: Head count in the cell.

- `enteric_ch4_kt`, `manure_ch4_kt`, `manure_n2o_kt`: Gridded emissions
  (kilotonnes), the primary output.

- `enteric_ch4_national_kt`, `manure_ch4_national_kt`,
  `manure_n2o_national_kt`: The national-grain estimate rescaled onto
  the cell, so summing these over a country reproduces the
  national-grain run.

- `divergence_enteric_ch4`, `divergence_manure_ch4`,
  `divergence_manure_n2o`: Per-country ratio of the gridded total to the
  national-grain total. `1` means the two grains agree.

- `mean_annual_temp_c`, `climate_zone`, `diet_quality`: The resolved
  per-cell drivers.

- `species_group`: The spatializer group, when the input carried one.

- `method_species`: How the row's `species` was resolved: `"supplied"`,
  `"one_to_one"` (the group is a single species),
  `"national_head_share"`, `"polity_bucket_head_share"` or
  `"unsplit_no_national_mix"` (emissions `NA`, see `method_species`).

- `method_climate_zone`: How the cell's climate zone was resolved, from
  [`build_cell_climate_zone()`](https://eduaguilera.github.io/whep/reference/build_cell_climate_zone.md),
  prefixed `"nearest_cell; "` when it was taken from the nearest climate
  cell (see `method_climate_gap`).

- `method_diet`, `method_enteric`, `method_manure_ch4`,
  `method_manure_n2o`: Method tracking.

plus the polity columns below.

## Milk yield

Tier 2 lactation energy (IPCC 2019 Eq 10.8) needs a milk yield per dairy
cow. It is taken, in this order, from a `milk_yield_kg_day` column on
`gridded_livestock`, or from the realised national yield of the `t_head`
rows of `data$species_heads` (milk tonnes per head and year, converted
to kilograms per day as
[`prepare_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/prepare_livestock_emissions.md)
does). A dairy row that neither supplies aborts: the yield is never
filled with zero, which would book every dairy cow as giving no milk.

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
build_gridded_livestock_emissions(example = TRUE)
#> # A tibble: 3 × 29
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  1961       114              114 KEN-1926-1963         Kenya (1926-1963)    
#> 2  1961       114              114 KEN-1926-1963         Kenya (1926-1963)    
#> 3  1961       114              114 KEN-1926-1963         Kenya (1926-1963)    
#> # ℹ 24 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, species <chr>, heads <dbl>, mean_annual_temp_c <dbl>,
#> #   climate_zone <chr>, diet_quality <chr>, enteric_ch4_kt <dbl>,
#> #   manure_ch4_kt <dbl>, manure_n2o_kt <dbl>, divergence_enteric_ch4 <dbl>,
#> #   divergence_manure_ch4 <dbl>, divergence_manure_n2o <dbl>,
#> #   enteric_ch4_national_kt <dbl>, manure_ch4_national_kt <dbl>,
#> #   manure_n2o_national_kt <dbl>, species_group <chr>, method_species <chr>, …
```
