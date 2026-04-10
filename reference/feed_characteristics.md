# Feed characteristics by diet quality.

DE%, NDF%, GE content, and crude protein percentage for High/Medium/Low
diet quality levels.

## Usage

``` r
feed_characteristics
```

## Format

A tibble with `diet_quality`, `de_percent`, `ndf_percent`,
`ge_content_mj_kg_dm`, `cp_percent`.

## Source

IPCC 2019, Vol 4, Ch 10.

## Examples

``` r
feed_characteristics
#> # A tibble: 3 × 5
#>   diet_quality de_percent ndf_percent ge_content_mj_kg_dm cp_percent
#>   <chr>             <dbl>       <dbl>               <dbl>      <dbl>
#> 1 High                 75          35                18.4         16
#> 2 Medium               65          50                18.4         12
#> 3 Low                  55          65                18.4          8
```
