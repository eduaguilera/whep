# Country-level mechanization levels for feed materials.

Mechanization level by country for each feed material, for ruminant and
monogastric species.

## Usage

``` r
gleam_mechanization_levels
```

## Format

A tibble in long format with columns:

- country:

  Country name.

- continent:

  Continent.

- region:

  GLEAM region.

- feed_material:

  Feed material code in lowercase.

- mechanization_level:

  Numeric mechanization level.

- species_group:

  `"ruminant"` or `"monogastric"`.

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Tables S.6.3 and S.6.4.

## Examples

``` r
gleam_mechanization_levels
#> # A tibble: 13,332 × 6
#>    country     continent region  feed_material mechanization_level species_group
#>    <chr>       <chr>     <chr>   <chr>                       <dbl> <chr>        
#>  1 Afghanistan Asia      South … grassf                       18.4 ruminant     
#>  2 Afghanistan Asia      South … grassh                       18.4 ruminant     
#>  3 Afghanistan Asia      South … grassh2                      18.4 ruminant     
#>  4 Afghanistan Asia      South … grasslegf                    18.4 ruminant     
#>  5 Afghanistan Asia      South … grasslegh                    18.4 ruminant     
#>  6 Afghanistan Asia      South … fddrsil                      18.4 ruminant     
#>  7 Afghanistan Asia      South … rstraw                       30   ruminant     
#>  8 Afghanistan Asia      South … wstraw                       12.5 ruminant     
#>  9 Afghanistan Asia      South … bstraw                       14.3 ruminant     
#> 10 Afghanistan Asia      South … zstover                       0   ruminant     
#> # ℹ 13,322 more rows
```
