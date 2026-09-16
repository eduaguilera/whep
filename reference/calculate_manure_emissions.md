# Calculate manure emissions (CH4 + N2O).

Wrapper that selects Tier 1 or 2 for manure CH4 and computes N2O (Tier 2
only; skipped for Tier 1).

## Usage

``` r
calculate_manure_emissions(data, tier = NULL, options = list())
```

## Arguments

- data:

  Dataframe with `species`, `heads`. For Tier 2, also needs `cohort`,
  `weight`, and `diet_quality`. For Tier 1, `iso3` is used to select
  regional emission factors.

- tier:

  Integer 1 or 2. If `NULL` (default), auto-selects based on data
  completeness.

- options:

  A named list of manure-engine options. All but two defaults reproduce
  the behaviour in force before whep#949. The exceptions are
  `mcf_source`, which moved from the shipped table to the 2019
  Refinement in whep#1022 and does move Tier 2 manure CH4, and
  `mms_shares`, which moved from the unsourced placeholder table to the
  GLEAM 2.0 ingest in whep#958 and does move both tiers' manure N2O.

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

Dataframe with all input columns preserved, plus:

- `method_manure_ch4`: tracking label.

- `method_mms`: which half of
  [regional_mms_distribution](https://eduaguilera.github.io/whep/reference/regional_mms_distribution.md)
  was read and how it was keyed, `"<shares>/<keying>"` (e.g.
  `"gleam_2_0/region_specific"`).

- Tier 1: `manure_ef_kgch4`, `manure_ch4_tier1`.

- Tier 2: `volatile_solids`, `methane_potential`, `weighted_mcf`,
  `manure_ch4_per_head`, `manure_ch4_tier2`.

- N2O (both tiers): `method_manure_n2o`, `n_excretion`,
  `manure_n2o_direct`, `manure_n2o_indirect`, `manure_n2o_total`. Tier 1
  uses default per-head excretion rates; Tier 2 uses the energy/nitrogen
  balance.

## Examples

``` r
tibble::tibble(
  species = "Cattle", heads = 1000, iso3 = "DEU"
) |>
  calculate_manure_emissions(tier = 1)
#> # A tibble: 1 × 15
#>   species heads iso3  species_gen method_manure_ch4 manure_ef_kgch4
#>   <chr>   <dbl> <chr> <chr>       <chr>                       <dbl>
#> 1 Cattle   1000 DEU   Cattle      IPCC_2019_Tier1                 1
#> # ℹ 9 more variables: manure_ch4_tier1 <dbl>, method_manure_n2o <chr>,
#> #   manure_category <chr>, region <chr>, n_excretion <dbl>, method_mms <chr>,
#> #   manure_n2o_direct <dbl>, manure_n2o_indirect <dbl>, manure_n2o_total <dbl>
```
