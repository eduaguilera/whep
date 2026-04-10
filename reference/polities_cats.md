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

- `polity_code`: ISO 3166-1 alpha-3 country code used as the primary
  identifier (e.g., `"AFG"`, `"ALB"`).

- `polity_name`: Common country or territory name.

- `V1`: Internal row index from the source table.

- `code`: Numeric FAOSTAT country code.

- `iso3c`: ISO 3166-1 alpha-3 code (character; may duplicate
  `polity_code` or differ for aggregates).

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

Five trailing columns containing only Excel `#REF!` errors in the source
CSV are dropped at load time and are not part of this dataset.

## Examples

``` r
head(polities_cats)
#> # A tibble: 6 × 35
#>   polity_code polity_name    V1  code iso3c FAOSTAT_name EU27  name  eia   iea  
#>   <chr>       <chr>       <dbl> <dbl> <chr> <chr>        <lgl> <chr> <chr> <chr>
#> 1 AFG         Afghanistan     2     2 AFG   Afghanistan  FALSE Afgh… Afgh… 0    
#> 2 ALB         Albania         3     3 ALB   Albania      FALSE Alba… Alba… Alba…
#> 3 DZA         Algeria         4     4 DZA   Algeria      FALSE Alge… Alge… Alge…
#> 4 AGO         Angola          7     7 AGO   Angola       FALSE Ango… Ango… Ango…
#> 5 ATG         Antigua an…     8     8 ATG   Antigua and… FALSE Anti… Anti… 0    
#> 6 ARG         Argentina       9     9 ARG   Argentina    FALSE Arge… Arge… Arge…
#> # ℹ 25 more variables: water_code <dbl>, water_area <chr>, baci <dbl>,
#> #   fish <dbl>, region_code <dbl>, cbs <lgl>, fabio_code <dbl>,
#> #   ADB_Region <chr>, region <chr>, uISO3c <dbl>, Lassaletta <chr>,
#> #   region_krausmann <chr>, region_HANPP <chr>, region_krausmann2 <chr>,
#> #   region_UN_sub <chr>, region_UN <chr>, region_ILO1 <chr>, region_ILO2 <chr>,
#> #   region_ILO3 <chr>, region_IEA <chr>, region_IPCC <chr>,
#> #   region_labour <chr>, region_labour_agg <chr>, region_labour_mech <chr>, …
```
