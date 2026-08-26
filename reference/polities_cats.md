# Polity categories and regional classifications

Reference table for countries and political entities (polities) with
identifiers from multiple data sources and assignments to various
regional groupings used in the literature and international databases.

## Usage

``` r
polities_cats
```

## Format

A tibble where each row corresponds to one polity (country or
territory). It contains the following columns:

- `legacy_polity_prefix`: Legacy current polity prefix, usually ISO
  3166-1 alpha-3 (e.g., `"AFG"`, `"ALB"`). **Not a
  [polities](https://eduaguilera.github.io/whep/reference/polities.md)
  code** and not a join key to any polity table: it was called
  `polity_code` until whep#687, where the name promised an identity none
  of its values holds. Read `reporting_polity_code` for the polity.

- `polity_name`: Current polity, country, or territory name.

- `V1`: Internal row index from the source table.

- `code`: Numeric FAOSTAT country code.

- `polity_area_code`: Numeric WHEP reporting area code used in matrix
  workflows.

- `reporting_polity_code`: Current periodized WHEP polity code for
  `code`.

- `reporting_polity_name`: Current WHEP polity name for `code`.

- `reporting_polity_has_geometry`: Logical flag indicating whether the
  current reporting polity has a polygon.

- `iso3c`: ISO 3166-1 alpha-3 code (character; may duplicate
  `legacy_polity_prefix` or differ for aggregates).

- `FAOSTAT_name`: Country name as used in FAOSTAT.

- `EU27`: Logical flag; `TRUE` if the polity is a member of the EU27.

- `name`: Country name used in other external databases.

- `eia`: Country name or code used by the US Energy Information
  Administration (EIA).

- `iea`: Country identifier used by the International Energy Agency
  (IEA).

- `water_code`: Numeric code used in water statistics datasets.

- `water_area`: Country/area name used in water statistics.

- `baci`: Numeric BACI trade database country code.

- `fish`: Numeric code used in fisheries datasets.

- `region_code`: Numeric regional grouping code.

- `cbs`: Logical flag; `TRUE` if the polity is included in the CBS
  dataset.

- `fabio_code`: Numeric country code used in the FABIO database.

- `ADB_Region`: Asian Development Bank regional classification.

- `region`: General world region (e.g., `"South Asia"`,
  `"Eastern Europe"`).

- `uISO3c`: Numeric Unicode / UN M49 country code.

- `Lassaletta`: Country grouping used in Lassaletta et al. nitrogen flow
  studies.

- `region_krausmann`: Regional grouping from Krausmann et al. biomass
  flow accounting.

- `region_HANPP`: Regional grouping used in human appropriation of net
  primary production (HANPP) studies.

- `region_krausmann2`: Alternative Krausmann regional grouping.

- `region_UN_sub`: UN sub-regional classification (M49 sub-region).

- `region_UN`: UN macro-regional classification (M49 region).

- `region_ILO1`: ILO primary regional grouping.

- `region_ILO2`: ILO secondary regional grouping.

- `region_ILO3`: ILO tertiary regional grouping.

- `region_IEA`: IEA regional grouping.

- `region_IPCC`: IPCC regional grouping used in climate assessments.

- `region_labour`: Labour-focused regional grouping.

- `region_labour_agg`: Aggregated labour-focused regional grouping.

- `region_labour_mech`: Labour mechanisation regional grouping. Two
  cells hold a sub-region name rather than a mechanisation class; see
  [regions_full](https://eduaguilera.github.io/whep/reference/regions_full.md).

## Source

Compiled from [FAOSTAT](https://www.fao.org/faostat/), UN M49, ILO, IEA,
and other international statistical sources.

## Note

Derived from
[regions_full](https://eduaguilera.github.io/whep/reference/regions_full.md)
rather than vendored separately: the 198-code membership is read from
`harmonization/polities_cats.csv` and every column value comes from
`regions_full`, so the two tables cannot disagree except where this one
deliberately folds an area into a rest-of-world aggregate. Two areas are
folded, both because they had no commodity balance sheet when the table
was compiled: Bhutan under `RASI` and Comoros under `RAFR`, each with
`cbs` `FALSE` and `fabio_code` `999`.

## Which regional groupings WHEP reads

The grouping columns are not all inputs to this package. Six have a
consumer in the tree, measured over `R/`, `data-raw/`, `tests/`,
`vignettes/` and `inst/`: `region` (carried into
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
at build time), `region_krausmann` (the residue recovery rate, and the
IPCC excreta regions of `prepare_spatialize_all.R`), `region_HANPP` (the
modern-variety adoption share), `region_UN_sub` (the residue feed-use
fraction, since whep#405), `ADB_Region`
([build_primary_production](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
area keys) and `EU27` (the EU aggregate of the FABIO comparison).
`region_code` carries no information `region` does not – the two are a
1:1 relabelling – so it needs no consumer of its own.

The rest – `Lassaletta`, `region_krausmann2`, `region_UN`,
`region_ILO1`, `region_ILO2`, `region_ILO3`, `region_IEA`,
`region_IPCC`, `region_labour`, `region_labour_agg`,
`region_labour_mech` – are published third-party taxonomies shipped for
downstream analysis and read by nothing in the package. They are shipped
as reference, and carry no promise of being re-validated against their
upstream taxonomy on release, so a consumer should check the gap it
inherits before keying anything by one (whep#386).

The gap the present-day taxonomies share is dissolved states. Over the
202 `cbs` reporters, `region_ILO1`, `region_ILO2`, `region_ILO3`,
`region_IEA` and `region_IPCC` are each `NA` for exactly the four
federations WHEP still books commodity balances for – Czechoslovakia
(51), Serbia and Montenegro (186), the USSR (228) and the Yugoslav SFR
(248) – and complete everywhere else; `region_UN` labels three of the
four and leaves only Czechoslovakia `NA`. `ROW` (999) carries an
explicit `"RoW"` value in all of them rather than `NA`. Grouping by one
of these without deciding what to do with the federations silently drops
the pre-succession record. `region_UN_sub`, which shares the gap and
does have a consumer, is pinned against it in
`test_region_classifications.R`.

A `region_test` column with two values (`"Europe"`, `"Other"`) and no
consumer was dropped in whep#386.

## Examples

``` r
head(polities_cats)
#> # A tibble: 6 × 38
#>   legacy_polity_prefix polity_name       V1  code iso3c FAOSTAT_name EU27  name 
#>   <chr>                <chr>          <dbl> <int> <chr> <chr>        <lgl> <chr>
#> 1 AFG                  Afghanistan        2     2 AFG   Afghanistan  FALSE Afgh…
#> 2 ALB                  Albania            3     3 ALB   Albania      FALSE Alba…
#> 3 DZA                  Algeria            4     4 DZA   Algeria      FALSE Alge…
#> 4 AGO                  Angola             7     7 AGO   Angola       FALSE Ango…
#> 5 ATG                  Antigua and B…     8     8 ATG   Antigua and… FALSE Anti…
#> 6 ARG                  Argentina          9     9 ARG   Argentina    FALSE Arge…
#> # ℹ 30 more variables: eia <chr>, iea <chr>, water_code <dbl>,
#> #   water_area <chr>, baci <dbl>, fish <dbl>, region_code <dbl>, cbs <lgl>,
#> #   fabio_code <dbl>, ADB_Region <chr>, region <chr>, uISO3c <dbl>,
#> #   Lassaletta <chr>, region_krausmann <chr>, region_HANPP <chr>,
#> #   region_krausmann2 <chr>, region_UN_sub <chr>, region_UN <chr>,
#> #   region_ILO1 <chr>, region_ILO2 <chr>, region_ILO3 <chr>, region_IEA <chr>,
#> #   region_IPCC <chr>, region_labour <chr>, region_labour_agg <chr>, …
```
