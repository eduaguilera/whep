# Compute the crop-livestock connectivity index per province

Computes, per province and year, two indicators of local crop-livestock
integration described in the decomposition proposal (section 7c) as the
"specialization of greatest interest" — regional crop-livestock
disconnection:

- **Local feed self-sufficiency**: the share of feed consumed by
  livestock in a province that was itself grown in that same province
  (rather than sourced from anywhere else, whether another Spanish
  province or abroad — `n_prov_destiny` does not distinguish
  inter-provincial trade from international imports, so both count as
  "not self-sufficient" here).

- **Manure-recycling ratio**: the share of a province's total cropland
  and semi-natural N inputs that comes from its own livestock manure,
  rather than synthetic fertilizer, deposition, fixation, or urban
  waste.

A well-connected (mixed) province has high values on both; a
disconnected (specialized crop-only or livestock-only) province has low
values on both, since its livestock has nowhere local to send manure
and/or its cropland has no local manure to draw on.

## Usage

``` r
decompose_crop_livestock_conn(n_prov_destiny = NULL, example = FALSE)
```

## Arguments

- n_prov_destiny:

  Nitrogen flows tibble from
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md).
  If `NULL`, loaded automatically.

- example:

  If `TRUE`, return a small hardcoded output without downloading remote
  data. Default is `FALSE`.

## Value

A named list with tibbles `by_province` (columns `year`,
`province_name`, `self_sufficiency`, `recycling_ratio`) and `national`
(the unweighted across-province average of both indicators, by `year`).

## Examples

``` r
decompose_crop_livestock_conn(example = TRUE)
#> $by_province
#> # A tibble: 3 × 4
#>    year province_name self_sufficiency recycling_ratio
#>   <dbl> <chr>                    <dbl>           <dbl>
#> 1  2000 A_Coruna                 0.483          0.374 
#> 2  2000 Albacete                 0.644          0.0821
#> 3  2000 Alicante                 0.318          0.07  
#> 
#> $national
#> # A tibble: 3 × 3
#>    year self_sufficiency recycling_ratio
#>   <dbl>            <dbl>           <dbl>
#> 1  1900            0.773           0.252
#> 2  1950            0.764           0.382
#> 3  2000            0.465           0.225
#> 
```
