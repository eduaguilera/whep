# Manure engine options

Shared description of the `options` list the IPCC manure engine takes,
documented once and inherited by the functions that accept it.

## Arguments

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
