# Regional MMS distribution.

Fraction of manure managed in each manure-management system (MMS) by
region and species, in two selectable halves: the GLEAM 2.0 ingest that
the manure engines read by default, and the unsourced placeholder it
replaced.

## Usage

``` r
regional_mms_distribution
```

## Format

A tibble with `source`, `region`, `species`, `mms_type`, `fraction` and
`reference`. `source` is either "gleam_2_0" or "placeholder", and is the
value the manure engines' MMS-shares option takes (see
[manure_engine_options](https://eduaguilera.github.io/whep/reference/manure_engine_options.md)).
`reference` cites each row. `fraction` sums to one within every
`(source, region, species)`. `region` is the IPCC label vocabulary
`.add_ipcc_region()` emits, plus a "Global" fallback.

## Source

`source == "gleam_2_0"` is ingested from Tables 4.2-4.11 of Supplement
S1 to FAO. 2018. *GLEAM Model description, Version 2.0, Revision 5*,
July 2018, data reference year 2010 (workbook
<https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_2.0_Supplement_S1.xlsx>,
219715 bytes, md5 `72fd2ea477dfe8b30cd3657b2baa4af1`, re-downloaded from
FAO and verified 2026-09-09; FAO issues no DOI). Those ten tables are
the only published regional MMS shares in any GLEAM release: version 3.0
dropped the family, and its Supplement S1 carries MMS *definitions*
only. The read and the crosswalk are in
`data-raw/livestock_coefficients.R`.

The published grain is finer than this table's on three axes at once, so
the ingest is a crosswalk and each of its four choices moves numbers
(whep#958). Stated here, and at the point of use in the builder:

1.  **Species collapse.** GLEAM publishes one table per production
    system; this table is keyed on `species_gen`. Systems are averaged
    **unweighted** within a species and region, counting only the
    systems the source publishes a column for. A herd- or
    manure-weighted mean would be better and is **not available from the
    source**: Supplement S1 publishes herd *parameters* (Tab. 2.4-2.21)
    and no regional animal numbers or system shares anywhere. Tab. 4.4
    (feedlot cattle) is not read: GLEAM models the feedlot as a
    sub-system of the beef herd, so reading it would give a minority
    system a third of all cattle manure. The equal-weight collapse is
    coarsest for poultry, where the flat backyard assumption (50 percent
    pasture, 50 percent daily spread, identical in all ten regions) gets
    a third of the weight.

2.  **Region collapse.** GLEAM's 10 regions are mapped onto the IPCC
    labels `.add_ipcc_region()` emits. Only one pair collides – the
    Russian Federation and Eastern Europe both map to `"Eastern Europe"`
    – and those two published columns are averaged unweighted.

3.  **The `Global` row.** GLEAM publishes no global row, and most WHEP
    output resolves to `"Global"` (whep#678), so the row carrying the
    most weight is the one with no published value. It is the unweighted
    mean over the 10 GLEAM regions of the species-collapsed
    distributions, counting a region only where the source publishes
    one, and taken over the 10 published regions rather than the 9 IPCC
    labels so that the Russian Federation and Eastern Europe keep one
    vote each. **This row is WHEP-derived, not FAO-published**, and its
    `reference` says so.

4.  **MMS vocabulary.** GLEAM names 14 systems; WHEP's manure chain
    serves six labels and only six, in all four tables it reads by MMS
    name. Pasture, daily spread, solid storage, liquid slurry, uncovered
    anaerobic lagoon and poultry manure with litter map by identity.
    Drylot maps to `"Solid Storage"`, following IPCC's own combined
    "solid storage and dry lot" category, which
    [ipcc_2019_n2o_ef_direct](https://eduaguilera.github.io/whep/reference/ipcc_2019_n2o_ef_direct.md)
    carries at the same EF3; a separate `"Dry Lot"` label would give a
    *lower* methane conversion factor (2.5 against 4.0 percent,
    Temperate). Pit storage maps to `"Poultry Manure"` for chickens (a
    layer deep pit is IPCC's poultry manure without litter, held at the
    same EF3 as the with-litter row) and to `"Liquid/Slurry"` otherwise
    – **assumed, unverified**, as is intensive-windrow composting to
    `"Solid Storage"`. Burned for fuel and anaerobic digester are
    **excluded** and the rest renormalised: neither has a servable WHEP
    label, both being in
    [climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md)
    only at the unjoinable `climate_zone == "All"` and in
    `manure_loss_fractions.csv` not at all. Excluding them says the
    excreted nitrogen passes through the systems that remain, which
    overstates those systems; it removes 18.5 percent of published
    buffalo shares (dung burnt for fuel), 5.2 percent of swine and 4.1
    percent of cattle, and none of the others.

GLEAM 2.0 publishes no manure-management table for horses, camels or
mules and asses. Those four rows are the placeholder's, retained so the
engine still resolves a split for them, and flagged unsourced in
`reference`.

`source == "placeholder"` is the table WHEP shipped before whep#958: a
hardcoded `tribble()` annotated "GLEAM 3.0 / FAO statistics
(simplified)", round to the nearest 5 percentage points, matching no
table of any GLEAM release (whep#921). It is kept selectable so the
values published before the ingest stay reproducible and the sensitivity
to the ingest stays measurable. It is **not** a defensible alternative
estimate.

This object is result-affecting through `.resolve_mms_shares()`: it
weights the Tier 2 manure CH4 methane conversion factor
([climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md))
and the manure direct-N2O emission factor
([ipcc_2019_n2o_ef_direct](https://eduaguilera.github.io/whep/reference/ipcc_2019_n2o_ef_direct.md))
at both tiers.

## Examples

``` r
regional_mms_distribution
#> # A tibble: 231 × 6
#>    source    region         species mms_type              fraction reference    
#>    <chr>     <chr>          <chr>   <chr>                    <dbl> <chr>        
#>  1 gleam_2_0 Asia           Buffalo Daily Spread            0.0942 GLEAM 2.0 Su…
#>  2 gleam_2_0 Asia           Buffalo Liquid/Slurry           0.391  GLEAM 2.0 Su…
#>  3 gleam_2_0 Asia           Buffalo Pasture/Range/Paddock   0.141  GLEAM 2.0 Su…
#>  4 gleam_2_0 Asia           Buffalo Solid Storage           0.373  GLEAM 2.0 Su…
#>  5 gleam_2_0 Eastern Europe Buffalo Daily Spread            0.195  GLEAM 2.0 Su…
#>  6 gleam_2_0 Eastern Europe Buffalo Liquid/Slurry           0.0540 GLEAM 2.0 Su…
#>  7 gleam_2_0 Eastern Europe Buffalo Pasture/Range/Paddock   0.207  GLEAM 2.0 Su…
#>  8 gleam_2_0 Eastern Europe Buffalo Solid Storage           0.543  GLEAM 2.0 Su…
#>  9 gleam_2_0 Global         Buffalo Daily Spread            0.216  GLEAM 2.0 Su…
#> 10 gleam_2_0 Global         Buffalo Liquid/Slurry           0.214  GLEAM 2.0 Su…
#> # ℹ 221 more rows
```
