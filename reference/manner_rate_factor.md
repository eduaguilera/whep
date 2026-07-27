# MANNER synthetic-fertiliser application-rate factor.

Multiplicative ammonia-volatilisation factor for synthetic fertiliser as
a function of the nitrogen application rate, keyed by fertiliser, soil
type (calcareous / non-calcareous) and application-rate bin (kg N/ha).
Used by
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)'s
synthetic-fertiliser path. AN and CAN are 1 for every soil type and rate
bin; Urea follows the same rate-response curve on both soil types; AS
follows the Urea curve on calcareous soils and is 1 on non-calcareous
soils.

## Usage

``` r
manner_rate_factor
```

## Format

A tibble with columns:

- fertiliser:

  Synthetic fertiliser: `"Urea"`, `"AN"`, `"CAN"` or `"AS"`.

- soil_type:

  `"calcareous"` or `"non-calcareous"`.

- rate_bin:

  Application-rate bin (kg N/ha): one of `"0-30"`, `"30-60"`, `"60-90"`,
  `"90-120"`, `"120-150"`, `"150-180"`, `"180-200"`, `">200"`.

- factor:

  Numeric multiplicative rate factor.

## Source

WHEP project-internal coefficient workbook (not a public DOI): Spain
historical MANNER implementation, `NH3_model.xlsx`, sheet "synthetic
fertilisers".

## Examples

``` r
manner_rate_factor
#> # A tibble: 64 × 4
#>    fertiliser soil_type      rate_bin factor
#>    <chr>      <chr>          <chr>     <dbl>
#>  1 AN         calcareous     0-30        1  
#>  2 CAN        calcareous     0-30        1  
#>  3 Urea       calcareous     0-30        0.4
#>  4 AN         non-calcareous 0-30        1  
#>  5 CAN        non-calcareous 0-30        1  
#>  6 Urea       non-calcareous 0-30        0.4
#>  7 AS         calcareous     0-30        0.4
#>  8 AS         non-calcareous 0-30        1  
#>  9 AN         calcareous     30-60       1  
#> 10 CAN        calcareous     30-60       1  
#> # ℹ 54 more rows
```
