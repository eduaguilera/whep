# Soil organic matter content bins.

Half-open bins that map a soil organic matter share (fraction) to a soil
organic matter class. A share `s` is assigned to the class whose
interval satisfies `s_min < s <= s_max`; the top (`High`) bin has no
finite ceiling in the source and is treated as open-ended (any share
above its `s_min` is `"High"`), so `s_max = 1` here is a wide ceiling,
not a literal source value. The classes key the Meisinger
denitrification matrix
([meisinger_denitrification](https://eduaguilera.github.io/whep/reference/meisinger_denitrification.md)).

## Usage

``` r
som_ranges
```

## Format

A tibble with columns:

- som_content:

  Soil organic matter class: `"High"`, `"Medium"` or `"Low"`.

- som_min:

  Lower bound of the soil organic matter share interval (fraction).

- som_max:

  Upper bound of the soil organic matter share interval (fraction); the
  `"High"` row's value is a wide ceiling, not a literal source bound
  (see Description).

## Source

Spain historical nitrogen coefficient workbook (`N_coefficients.xlsx`,
sheet `SOM_ranges`), companion to the Meisinger & Randall (1991)
denitrification matrix.
[doi:10.2136/1991.managingnitrogen.c5](https://doi.org/10.2136/1991.managingnitrogen.c5)
.

## Examples

``` r
som_ranges
#> # A tibble: 3 × 3
#>   som_content som_min som_max
#>   <chr>         <dbl>   <dbl>
#> 1 High           0.05    1   
#> 2 Medium         0.02    0.05
#> 3 Low            0       0.02
```
