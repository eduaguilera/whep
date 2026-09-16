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

  A named list of manure-engine options. Every default reproduces the
  behaviour in force before whep#949, so passing none leaves published
  values unchanged.

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

  `climate_source` selects where the climate zone the methane conversion
  factors in
  [climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md)
  are read at comes from. A `climate_zone` a row already carries is
  always used and stamped `climate_from_data`; the option governs only
  the rows left without one, whether that is a hole in a supplied column
  or a wholly absent column.

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

- `method_mms`: which manure-management split was used
  (`"regional_default"` or `"region_specific"`).

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
