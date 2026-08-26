# Full polity and region reference table

Extended reference table covering all polities and aggregate regions,
including countries, territories, and statistical composites that appear
in international databases but may lack standard ISO codes.

## Usage

``` r
regions_full
```

## Format

A tibble where each row corresponds to one polity or aggregate region.
It contains the following columns (same definitions as `polities_cats`,
minus the five trailing `0...36`–`0...40` artefact columns):

- `legacy_polity_prefix`: Legacy current polity prefix, kept for
  compatibility with older code that expected ISO3-like values. **Not a
  [polities](https://eduaguilera.github.io/whep/reference/polities.md)
  code**: it was called `polity_code` until whep#687, where the name
  promised an identity none of its 271 non-`NA` values holds, so a join
  to
  [polities](https://eduaguilera.github.io/whep/reference/polities.md)
  or
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
  on it returned nothing. Read `reporting_polity_code` for the polity.

- `polity_name`: Current polity, country, territory, or aggregate name.

- `V1`: Internal row index.

- `code`: Numeric FAOSTAT country/region code.

- `polity_area_code`: Numeric WHEP reporting area code used in matrix
  workflows.

- `reporting_polity_code`: Current periodized WHEP polity code for
  `code`.

- `reporting_polity_name`: Current WHEP polity name for `code`.

- `reporting_polity_has_geometry`: Logical flag indicating whether the
  current reporting polity has a polygon.

- `iso3c`: ISO 3166-1 alpha-3 code (`NA` for aggregates).

- `FAOSTAT_name`: Name used in FAOSTAT (may be `"#N/A"` for aggregates).

- `EU27`: Logical EU27 membership flag.

- `name`: Name used in external databases.

- `eia`: EIA country identifier.

- `iea`: IEA country identifier.

- `water_code`: Water statistics numeric code.

- `water_area`: Name used in water statistics.

- `baci`: BACI trade database country code.

- `fish`: Fisheries dataset numeric code.

- `region_code`: Numeric regional code.

- `cbs`: Logical CBS dataset membership flag; `TRUE` if the area has a
  commodity balance sheet of its own. 202 areas.

- `fabio_code`: FABIO database numeric code, and the value
  `polity_area_code` is derived from, so it is the fold instruction as
  well as a fact about FABIO. It is the area's own `code` for a `cbs`
  reporter and 999 (Rest of World) otherwise, with seven exceptions: 62
  -\> 238, 276 -\> 206 and 277 -\> 206 are successor-state folds, and
  153, 154, 209 and 212 are `cbs` reporters folded into 999 anyway.
  Those four are a contradiction inside this table – FABIO's own
  published region list enumerates all four as regions of their own –
  left standing because correcting it would move published values. See
  [folded_reporting_areas](https://eduaguilera.github.io/whep/reference/folded_reporting_areas.md)
  and issue 556.

- `ADB_Region`: Asian Development Bank region.

- `region`: General world region.

- `uISO3c`: UN M49 numeric code.

- `Lassaletta`: Lassaletta et al. nitrogen study grouping.

- `region_krausmann`: Krausmann regional grouping.

- `region_HANPP`: HANPP study regional grouping.

- `region_krausmann2`: Alternative Krausmann grouping.

- `region_UN_sub`: UN M49 sub-region.

- `region_UN`: UN M49 macro-region.

- `region_ILO1`: ILO primary region.

- `region_ILO2`: ILO secondary region.

- `region_ILO3`: ILO tertiary region.

- `region_IEA`: IEA region.

- `region_IPCC`: IPCC region.

- `region_labour`: Labour-focused region.

- `region_labour_agg`: Aggregated labour region, one of `"SAA"`,
  `"LACA"`, `"Europe"`, `"AUS"`, `"SE-Asia"`, `"MENA"`, `"FSU"`,
  `"NAME"` or `"RoW"`. Northern Mariana Islands (code 163) instead holds
  `"Micronesia"`, its own `region_UN_sub` value.

- `region_labour_mech`: Labour mechanisation region, `"mech"` or
  `"no_mech"`. Two cells hold a sub-region name instead – Angola
  (code 7) `"Middle Africa"` and Northern Mariana Islands (163)
  `"Micronesia"`, each its own `region_labour`-family value – which
  looks like a column shift in the source spreadsheet. At code 163 the
  shift is two columns wide, since `region_labour_agg` is damaged in the
  same row; Angola's `region_labour_agg` is intact. Nothing here reads
  either column, so nothing computes on the bad cells; which class each
  belongs in is not recoverable from anything shipped with the package,
  and no public taxonomy defines this mechanised/not-mechanised split,
  so they are pinned in `test_region_classifications.R` rather than
  guessed at (whep#855). Group agreement is suggestive but not
  deductive: the other eight `region_labour == "Middle Africa"` rows are
  all `"no_mech"` and the other eight `region_UN_sub == "Micronesia"`
  rows are all `"mech"`, yet the column is not a function of either
  grouping – `"Pacific"`, `"FSU"` and `"South America - South Cone"`
  each split across both classes.

## Source

Compiled from [FAOSTAT](https://www.fao.org/faostat/), UN M49, ILO, IEA,
and other international statistical sources.

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

## See also

[polities_cats](https://eduaguilera.github.io/whep/reference/polities_cats.md)
for the subset restricted to sovereign countries.

## Examples

``` r
head(regions_full)
#> # A tibble: 6 × 38
#>   legacy_polity_prefix polity_name      V1  code iso3c FAOSTAT_name EU27  name  
#>   <chr>                <chr>         <dbl> <int> <chr> <chr>        <lgl> <chr> 
#> 1 ROW                  Rest of World    30    30 ATA   NA           FALSE Antar…
#> 2 NA                   NA              259   351 NA    China        FALSE China 
#> 3 ROW                  Rest of World   149   152 NTZ   NA           FALSE Neutr…
#> 4 ROW                  Rest of World   245   254 OXY   NA           FALSE Other…
#> 5 ROW                  Rest of World   260   999 ROW   NA           FALSE RoW   
#> 6 ROW                  Rest of World   244   252 UXY   NA           FALSE Unspe…
#> # ℹ 30 more variables: eia <chr>, iea <chr>, water_code <dbl>,
#> #   water_area <chr>, baci <dbl>, fish <dbl>, region_code <dbl>, cbs <lgl>,
#> #   fabio_code <dbl>, ADB_Region <chr>, region <chr>, uISO3c <dbl>,
#> #   Lassaletta <chr>, region_krausmann <chr>, region_HANPP <chr>,
#> #   region_krausmann2 <chr>, region_UN_sub <chr>, region_UN <chr>,
#> #   region_ILO1 <chr>, region_ILO2 <chr>, region_ILO3 <chr>, region_IEA <chr>,
#> #   region_IPCC <chr>, region_labour <chr>, region_labour_agg <chr>, …
```
