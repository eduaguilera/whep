# MANNER organic-manure incorporation-delay factor.

Multiplicative ammonia-volatilisation factor for organic manure as a
function of the delay between surface application and soil
incorporation, by manure type. Used by
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)'s
organic-manure path, distinct from
[manner_params](https://eduaguilera.github.io/whep/reference/manner_params.md)'s
`incorporation` table (a land-use factor reused by the
synthetic-fertiliser path; see that dataset's description). The delay
bins are monotonically increasing; a supplied delay is assigned to the
first (shortest) bin whose `delay_hours` is greater than or equal to it,
and a missing or infinite delay maps to `"No incorporation"` (factor 1,
no volatilisation reduction).

## Usage

``` r
manner_incorporation_factor
```

## Format

A tibble with columns:

- manure_type:

  Organic manure type: `"cattle_slurry"`, `"pig_slurry"`, `"FYM"` or
  `"poultry_manure"`.

- delay_bin:

  Incorporation-delay bin label (e.g. `"<2 h"`, `"6-12 days"`,
  `"No incorporation"`).

- delay_hours:

  Upper bound of the delay bin in hours; `NA` for `"No incorporation"`.

- factor:

  Numeric multiplicative incorporation factor.

## Source

WHEP project-internal coefficient workbook (not a public DOI): Spain
historical MANNER implementation, `NH3_model.xlsx`, sheet "manures".

## Examples

``` r
manner_incorporation_factor
#> # A tibble: 40 × 4
#>    manure_type    delay_bin delay_hours factor
#>    <chr>          <chr>           <dbl>  <dbl>
#>  1 pig_slurry     <2 h                2 0.147 
#>  2 cattle_slurry  <2 h                2 0.211 
#>  3 FYM            <2 h                2 0.118 
#>  4 poultry_manure <2 h                2 0.0472
#>  5 pig_slurry     2-4 h               4 0.256 
#>  6 cattle_slurry  2-4 h               4 0.348 
#>  7 FYM            2-4 h               4 0.212 
#>  8 poultry_manure 2-4 h               4 0.0901
#>  9 pig_slurry     4-6 h               6 0.341 
#> 10 cattle_slurry  4-6 h               6 0.444 
#> # ℹ 30 more rows
```
