# Calculate all livestock emissions.

Main dispatcher that runs the full IPCC 2019 livestock emissions
pipeline: energy demand (Tier 2), enteric CH4, manure CH4, and manure
N2O.

Selects tier automatically: Tier 2 when cohort-level data (weight, diet)
are available; Tier 1 otherwise.

## Usage

``` r
calculate_livestock_emissions(data, tier = NULL, options = list())
```

## Arguments

- data:

  Dataframe with at minimum `species` and `heads`. For Tier 2, also
  needs `cohort`, `weight` (or `iso3`), `diet_quality`, and production
  columns.

- tier:

  Integer 1 or 2. If `NULL` (default), auto-selects based on data
  completeness.

- options:

  A named list of manure-engine options. All but one default reproduce
  the behaviour in force before whep#949; the exception is `mcf_source`,
  which moved from the shipped table to the 2019 Refinement in whep#1022
  and does move Tier 2 manure CH4.

  `mms_region` selects how the manure-management split in
  [regional_mms_distribution](https://eduaguilera.github.io/whep/reference/regional_mms_distribution.md)
  is keyed:

  - `"as_available"` (default): a row uses its own region when the frame
    already carries a `region` column, and the `region == "Global"`
    split otherwise. Tier 1 resolves a region for the (sourced) per-head
    N-excretion table and so takes the region-specific split; Tier 2
    carries no region and so takes the Global one.

  - `"resolve"`: the IPCC region is resolved from `iso3`, `area_code` or
    `polity_area_code` where it is missing, which makes the table's four
    region-specific `(region, species)` pairs live on the Tier 2 path
    too. Those four pairs are an unsourced placeholder (whep#921), which
    is why this is opt-in rather than the default.

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

  The default carries one known incompleteness: the Refinement pairs its
  single 0.47 percent pasture MCF with a mandatory `Bo` of 0.19, and
  this engine applies one per-species `Bo` to every stream, so the pair
  cannot be honoured here. See the corresponding section of
  [climate_mcf_ipcc](https://eduaguilera.github.io/whep/reference/climate_mcf_ipcc.md).

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

## Value

Dataframe with all emission columns, method tracking, and original data
columns preserved.

## Examples

``` r
tibble::tibble(
  species = "Dairy Cattle",
  cohort = "Adult Female",
  heads = 1000,
  weight = 600,
  diet_quality = "High",
  milk_yield_kg_day = 20
) |>
  calculate_livestock_emissions() |>
  dplyr::select(species, cohort, heads,
    enteric_ch4_tier2, manure_ch4_tier2,
    manure_n2o_total)
#> # A tibble: 1 × 6
#>   species      cohort  heads enteric_ch4_tier2 manure_ch4_tier2 manure_n2o_total
#>   <chr>        <chr>   <dbl>             <dbl>            <dbl>            <dbl>
#> 1 Dairy Cattle Adult …  1000           126919.           18470.            2189.
```
