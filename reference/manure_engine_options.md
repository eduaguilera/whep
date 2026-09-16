# Manure engine options

Shared description of the `options` list the IPCC manure engine takes,
documented once and inherited by the functions that accept it.

## Arguments

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
