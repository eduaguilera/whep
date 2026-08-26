# GLEAM geographic hierarchy.

Maps countries (ISO3) to GLEAM regions, FAOSTAT regions, and
classification indicators.

This is GLEAM's own registry of the countries that exist today, so
[`polity_identity_conventions()`](https://eduaguilera.github.io/whep/reference/polity_identity_conventions.md)
types it `"present_day_polity"` and it carries the polity its `iso3`
resolves to in the present day, as
[regions_full](https://eduaguilera.github.io/whep/reference/regions_full.md)
does. All 204 rows now resolve: ATF, SGS and WLF used to keep `NA` for
want of any WHEP polity, and upstream supplied one for each. A gap would
still stay visible rather than being guessed at. The 204 GLEAM regions
themselves are unchanged.

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

- eu27:

  1 for an EU-27 member, 0 otherwise.

- oecd:

  1 for an OECD member, 0 otherwise.

- reporting_polity_code:

  The
  [polities](https://eduaguilera.github.io/whep/reference/polities.md)
  code `iso3` resolves to in the present day, `NA` where WHEP has no
  polity for the territory.

- reporting_polity_name:

  The name of that polity.

## Source

FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO workbook; no DOI is
issued for it), Tables S.A1-S.A2:
<https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx>

## Examples

``` r
gleam_geographic_hierarchy
#> # A tibble: 204 × 9
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
#> # ℹ 2 more variables: reporting_polity_code <chr>, reporting_polity_name <chr>
```
