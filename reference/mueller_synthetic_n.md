# Synthetic nitrogen application rates by crop and country

Country- and crop-process-level synthetic nitrogen application rates (kg
N ha\\^{-1}\\), derived from Mueller et al. (2012). Used as reference
crop-specific N rates in the WHEP nitrogen pipeline.

## Usage

``` r
mueller_synthetic_n
```

## Format

A tibble with one row per crop-process-country combination containing:

- `proc_code`: Internal process code (e.g. `"p001"`).

- `crop_process`: Descriptive crop process name (e.g.
  `"Rice production"`).

- `crop_original`: Crop name as in the source dataset.

- `unit`: Unit of the rate value (always `"kgN/ha"`).

- `iso3c`: ISO 3166-1 alpha-3 country code.

- `rate_value`: Nitrogen application rate (kg N ha\\^{-1}\\).

## Source

Mueller, N. D. et al. (2012). Closing yield gaps through nutrient and
water management. *Nature*, 490(7419), 254–257.
[doi:10.1038/nature11420](https://doi.org/10.1038/nature11420)

## Examples

``` r
head(mueller_synthetic_n)
#> # A tibble: 6 × 6
#>   proc_code crop_process    crop_original unit   iso3c rate_value
#>   <chr>     <chr>           <chr>         <chr>  <chr>      <dbl>
#> 1 p001      Rice production rice          kgN/ha ARM       18.4  
#> 2 p001      Rice production rice          kgN/ha AFG        2.21 
#> 3 p001      Rice production rice          kgN/ha ALB       50.4  
#> 4 p001      Rice production rice          kgN/ha DZA       14.6  
#> 5 p001      Rice production rice          kgN/ha AGO        0.893
#> 6 p001      Rice production rice          kgN/ha ARG       36.0  
```
