# GLEAM feed use efficiency.

Regional feed use efficiency (FUE) values for forages and crop residues
of ruminant species.

## Usage

``` r
gleam_feed_composition
```

## Format

A tibble with columns:

- feed_group:

  Feed material group (1-6 or 9-15).

- feed_type:

  Feed type (mixed, grassland, or all).

- gleam_region:

  GLEAM geographic region.

- feed_use_efficiency:

  FUE value (0-1 fraction).

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Table S.3.2.

## Examples

``` r
gleam_feed_composition
#> # A tibble: 21 × 4
#>    feed_group         feed_type gleam_region         feed_use_efficiency
#>    <chr>              <chr>     <chr>                              <dbl>
#>  1 Feed materials 1-6 mixed     Asia                                0.7 
#>  2 Feed materials 1-6 grassland Asia                                0.49
#>  3 Feed materials 1-6 mixed     Africa                              0.56
#>  4 Feed materials 1-6 grassland Africa                              0.56
#>  5 Feed materials 1-6 mixed     LAC                                 0.49
#>  6 Feed materials 1-6 grassland LAC                                 0.42
#>  7 Feed materials 1-6 mixed     North America                       0.56
#>  8 Feed materials 1-6 grassland North America                       0.56
#>  9 Feed materials 1-6 mixed     South Eastern Europe                0.49
#> 10 Feed materials 1-6 grassland South Eastern Europe                0.49
#> # ℹ 11 more rows
```
