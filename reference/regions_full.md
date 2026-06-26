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

- `polity_code`: Legacy current polity prefix. This is kept for
  compatibility with older code that expected ISO3-like values.

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

- `cbs`: Logical CBS dataset membership flag.

- `fabio_code`: FABIO database numeric code.

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

- `region_labour_agg`: Aggregated labour region.

- `region_labour_mech`: Labour mechanisation region.

- `region_test`: Experimental regional grouping.

## Source

Compiled from [FAOSTAT](https://www.fao.org/faostat/), UN M49, ILO, IEA,
and other international statistical sources.

## See also

[polities_cats](https://eduaguilera.github.io/whep/reference/polities_cats.md)
for the subset restricted to sovereign countries.

## Examples

``` r
head(regions_full)
#> # A tibble: 6 × 39
#>   polity_code polity_name    V1  code iso3c FAOSTAT_name EU27  name  eia   iea  
#>   <chr>       <chr>       <dbl> <int> <chr> <chr>        <lgl> <chr> <chr> <chr>
#> 1 ROW         Rest of Wo…    30    30 ATA   NA           FALSE Anta… Anta… NA   
#> 2 NA          NA            259   351 NA    China        FALSE China NA    NA   
#> 3 ROW         Rest of Wo…   149   152 NTZ   NA           FALSE Neut… NA    NA   
#> 4 ROW         Rest of Wo…   245   254 OXY   NA           FALSE Othe… NA    NA   
#> 5 ROW         Rest of Wo…   260   999 ROW   NA           FALSE RoW   NA    NA   
#> 6 ROW         Rest of Wo…   244   252 UXY   NA           FALSE Unsp… NA    NA   
#> # ℹ 29 more variables: water_code <dbl>, water_area <chr>, baci <dbl>,
#> #   fish <dbl>, region_code <dbl>, cbs <lgl>, fabio_code <dbl>,
#> #   ADB_Region <chr>, region <chr>, uISO3c <dbl>, Lassaletta <chr>,
#> #   region_krausmann <chr>, region_HANPP <chr>, region_krausmann2 <chr>,
#> #   region_UN_sub <chr>, region_UN <chr>, region_ILO1 <chr>, region_ILO2 <chr>,
#> #   region_ILO3 <chr>, region_IEA <chr>, region_IPCC <chr>,
#> #   region_labour <chr>, region_labour_agg <chr>, region_labour_mech <chr>, …
```
