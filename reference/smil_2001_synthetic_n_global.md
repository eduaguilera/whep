# Smil (2001) global synthetic nitrogen production, 1913-2000

Global synthetic-nitrogen production anchors from Smil (2001) "Enriching
the Earth", Tables 5.2 and 5.3, cross-checked with Smil (2002) Ambio
31:126-131. Anchor years span 1913 (first commercial Haber-Bosch plant
at BASF Oppau) to 2000. Used by `prepare_nitrogen_inputs()` to backcast
country-level synthetic N for the pre-FAOSTAT period (years before
1961): the temporal shape is taken from this global series and
downscaled to each country using its 1961-1965 share of global FAOSTAT
synthetic N.

Pre-1913 values are treated as zero by the consumer and are not stored
here.

## Usage

``` r
smil_2001_synthetic_n_global
```

## Format

A tibble with one row per anchor year:

- `year`: Integer anchor year (1913, 1920, 1925, ..., 2000).

- `global_kt_n`: Global synthetic-N production in kt N.

## Source

Smil, V. (2001) *Enriching the Earth: Fritz Haber, Carl Bosch, and the
Transformation of World Food Production*, MIT Press. Tables 5.2 and 5.3.

## Examples

``` r
head(smil_2001_synthetic_n_global)
#> # A tibble: 6 × 2
#>    year global_kt_n
#>   <int>       <dbl>
#> 1  1913           7
#> 2  1920         155
#> 3  1925         430
#> 4  1930        1900
#> 5  1935        1700
#> 6  1940        3000
```
