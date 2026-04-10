# GLEAM geographic hierarchy.

Maps countries (ISO3) to GLEAM regions, FAOSTAT regions, and
classification indicators.

## Usage

``` r
gleam_geographic_hierarchy
```

## Format

A tibble with columns:

- iso3:

  ISO3 country code.

- country:

  Country name.

- continent:

  Continent.

- faostat_region:

  FAOSTAT regional grouping.

- gleam_region:

  GLEAM regional grouping.

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Tables S.A1-S.A2.
[doi:10.1088/1748-9326/aad4d8](https://doi.org/10.1088/1748-9326/aad4d8)

## Examples

``` r
gleam_geographic_hierarchy
#> # A tibble: 204 × 7
#>    iso3  country             continent faostat_region   gleam_region  eu27  oecd
#>    <chr> <chr>               <chr>     <chr>            <chr>        <int> <int>
#>  1 AFG   Afghanistan         Asia      Southern Asia    South Asia       0     0
#>  2 ALB   Albania             Europe    Southern Europe  Western Eur…     0     0
#>  3 DZA   Algeria             Africa    Northern Africa  West Asia &…     0     0
#>  4 AGO   Angola              Africa    Middle Africa    Sub-Saharan…     0     0
#>  5 ATG   Antigua and Barbuda Americas  Caribbean        Central & S…     0     0
#>  6 ARG   Argentina           Americas  South America    Central & S…     0     0
#>  7 ARM   Armenia             Asia      Western Asia     West Asia &…     0     0
#>  8 AUS   Australia           Oceania   Australia and N… Oceania          0     1
#>  9 AUT   Austria             Europe    Western Europe   Western Eur…     1     1
#> 10 AZE   Azerbaijan          Asia      Western Asia     West Asia &…     0     0
#> # ℹ 194 more rows
```
