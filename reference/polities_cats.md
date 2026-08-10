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

- `region_labour_mech`: Labour mechanisation regional grouping.

- `region_test`: Experimental/test regional grouping (may be
  incomplete).

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

## Examples

``` r
head(polities_cats)
#> # A tibble: 6 × 39
#>   legacy_polity_prefix polity_name       V1  code iso3c FAOSTAT_name EU27  name 
#>   <chr>                <chr>          <dbl> <int> <chr> <chr>        <lgl> <chr>
#> 1 AFG                  Afghanistan        2     2 AFG   Afghanistan  FALSE Afgh…
#> 2 ALB                  Albania            3     3 ALB   Albania      FALSE Alba…
#> 3 DZA                  Algeria            4     4 DZA   Algeria      FALSE Alge…
#> 4 AGO                  Angola             7     7 AGO   Angola       FALSE Ango…
#> 5 ATG                  Antigua and B…     8     8 ATG   Antigua and… FALSE Anti…
#> 6 ARG                  Argentina          9     9 ARG   Argentina    FALSE Arge…
#> # ℹ 31 more variables: eia <chr>, iea <chr>, water_code <dbl>,
#> #   water_area <chr>, baci <dbl>, fish <dbl>, region_code <dbl>, cbs <lgl>,
#> #   fabio_code <dbl>, ADB_Region <chr>, region <chr>, uISO3c <dbl>,
#> #   Lassaletta <chr>, region_krausmann <chr>, region_HANPP <chr>,
#> #   region_krausmann2 <chr>, region_UN_sub <chr>, region_UN <chr>,
#> #   region_ILO1 <chr>, region_ILO2 <chr>, region_ILO3 <chr>, region_IEA <chr>,
#> #   region_IPCC <chr>, region_labour <chr>, region_labour_agg <chr>, …
```
