# Regional MMS distribution.

Fraction of manure managed in each MMS type by region and species.

## Usage

``` r
regional_mms_distribution
```

## Format

A tibble with `region`, `species`, `mms_type`, `fraction`.

## Source

Unverified. The values are a hardcoded `tribble()` in
`generate_ipcc_tier2_params()` in `data-raw/livestock_coefficients.R`,
annotated there "GLEAM 3.0 / FAO statistics (simplified)"; they are
round to the nearest 5 percentage points and match no table of the
committed GLEAM 3.0 Supplement S1 workbook, which carries no MMS shares.
Unlike `gleam_mms_shares` this object **is** result-affecting: it is the
MMS split `.resolve_mms_shares()` hands to the Tier 2 manure CH4 (MCF
weighting) and direct N2O (EF3) engines. The published GLEAM source is
Supplement S1, Tables 4.2-4.11 of FAO. 2018. *GLEAM Model description,
Version 2.0, Revision 5* (see `gleam_mms_shares`); re-ingesting it moves
manure numbers and is tracked separately. Treat as unverified until
then.

## Examples

``` r
regional_mms_distribution
#> # A tibble: 33 × 4
#>    region         species mms_type              fraction
#>    <chr>          <chr>   <chr>                    <dbl>
#>  1 North America  Cattle  Liquid/Slurry             0.4 
#>  2 North America  Cattle  Solid Storage             0.3 
#>  3 North America  Cattle  Pasture/Range/Paddock     0.25
#>  4 North America  Cattle  Daily Spread              0.05
#>  5 Western Europe Cattle  Liquid/Slurry             0.35
#>  6 Western Europe Cattle  Solid Storage             0.45
#>  7 Western Europe Cattle  Pasture/Range/Paddock     0.15
#>  8 Western Europe Cattle  Daily Spread              0.05
#>  9 Latin America  Cattle  Pasture/Range/Paddock     0.7 
#> 10 Latin America  Cattle  Solid Storage             0.15
#> # ℹ 23 more rows
```
