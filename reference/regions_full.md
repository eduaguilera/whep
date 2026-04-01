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

- `polity_code`: Primary polity identifier (ISO 3166-1 alpha-3 or `NA`
  for non-sovereign aggregates).

- `polity_name`: Polity name (`NA` for aggregates not matched to a
  standard polity).

- `V1`: Internal row index.

- `code`: Numeric FAOSTAT country/region code.

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
