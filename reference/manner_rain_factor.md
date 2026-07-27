# MANNER synthetic-fertiliser rainfall factor.

Multiplicative ammonia-volatilisation factor for synthetic fertiliser as
a function of soil pH class and the same rainfall/wetness classification
used by the organic-manure `rainfall_wet` table in
[manner_params](https://eduaguilera.github.io/whep/reference/manner_params.md).
Used by
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)'s
synthetic-fertiliser path. AN and CAN are 1 for every pH class and
rainfall class; AS is 1 for every `pH<7` row.

## Usage

``` r
manner_rain_factor
```

## Format

A tibble with columns:

- fertiliser:

  Synthetic fertiliser: `"Urea"`, `"AN"`, `"CAN"` or `"AS"`.

- ph_class:

  Soil pH class: `"pH<7"` or `"other pH"`.

- rainfall_class:

  Combined rainfall-level/wetness class (e.g. `"noraindry"`,
  `"heavyrainwet"`), the same 9 classes as
  [manner_params](https://eduaguilera.github.io/whep/reference/manner_params.md)'s
  `rainfall_wet` table.

- factor:

  Numeric multiplicative rainfall factor.

## Source

WHEP project-internal coefficient workbook (not a public DOI): Spain
historical MANNER implementation, `NH3_model.xlsx`, sheet "synthetic
fertilisers".

## Examples

``` r
manner_rain_factor
#> # A tibble: 72 × 4
#>    fertiliser ph_class rainfall_class factor
#>    <chr>      <chr>    <chr>           <dbl>
#>  1 Urea       pH<7     noraindry        0.7 
#>  2 Urea       other pH noraindry        0.7 
#>  3 AN         pH<7     noraindry        1   
#>  4 AN         other pH noraindry        1   
#>  5 CAN        pH<7     noraindry        1   
#>  6 CAN        other pH noraindry        1   
#>  7 AS         pH<7     noraindry        1   
#>  8 AS         other pH noraindry        1   
#>  9 Urea       pH<7     norainmoderate   0.95
#> 10 Urea       other pH norainmoderate   0.95
#> # ℹ 62 more rows
```
