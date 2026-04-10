# GLEAM feed conversion ratios for monogastrics.

Nutritional values for feed materials of monogastric species (chicken
and pigs).

## Usage

``` r
gleam_feed_conversion_ratios
```

## Format

A tibble with columns:

- number:

  Feed material number.

- material:

  Feed material code.

- gross_energy_j_kg:

  Gross energy (J per kg).

- n_content_g_kg:

  Nitrogen content (g per kg DM).

- me_chicken_j_kg:

  Metabolisable energy for chicken (J per kg).

- me_pigs_j_kg:

  Metabolisable energy for pigs (J per kg).

- digestibility_pct:

  Digestibility (percent).

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Table S.3.4.

## Examples

``` r
gleam_feed_conversion_ratios
#> # A tibble: 41 × 7
#>    number material gross_energy_j_kg n_content_g_kg me_chicken_j_kg me_pigs_j_kg
#>     <int> <chr>                <dbl>          <dbl>           <dbl>        <dbl>
#>  1      1 SWILL                18450          35             13000        10500 
#>  2      2 GRASSF               17800          NA                 0        10556.
#>  3      3 PULSES               18850          39.6           11319.       14759.
#>  4      4 PSTRAW               18450           8.89              0         8889.
#>  5      5 CASSAVA              16900           4.53          13148.       13580.
#>  6      6 WHEAT                18500          20.0           14506.       15044.
#>  7      7 MAIZE                18880          15.1           15839.       16447.
#>  8      8 BARLEY               18460          18.7           13112.       13680.
#>  9      9 MILLET               18680          19.7           13533.       13714.
#> 10     10 RICE                 17700          13.8           12551.       13398.
#> # ℹ 31 more rows
#> # ℹ 1 more variable: digestibility_pct <dbl>
```
