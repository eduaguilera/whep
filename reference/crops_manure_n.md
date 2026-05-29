# Manure nitrogen application by crop and country

Country- and crop-level estimates of manure nitrogen applied to
cropland, from West et al. (2014). Used as a reference for spatializing
manure N inputs in the WHEP pipeline.

## Usage

``` r
crops_manure_n
```

## Format

A tibble with one row per crop-country combination containing:

- `Crop_name`: Crop name (character).

- `ISO`: ISO 3166-1 alpha-3 country code.

- `Continent`: Three-letter continent code (e.g. `"AFR"`, `"ASI"`).

- `Manure_N_Mg`: Manure nitrogen applied in megagrams (Mg).

## Source

West, P. C. et al. (2014). Leverage points for improving global food
security and the environment. *Science*, 345(6194), 325–328.
[doi:10.1126/science.1246067](https://doi.org/10.1126/science.1246067)

## Examples

``` r
head(crops_manure_n)
#> # A tibble: 6 × 4
#>   Crop_name ISO   Continent Manure_N_Mg
#>   <chr>     <chr> <chr>           <dbl>
#> 1 abaca     AFG   ASI                 0
#> 2 abaca     AGO   AFR                 0
#> 3 abaca     ALB   EUR                 0
#> 4 abaca     ARE   ASI                 0
#> 5 abaca     ARG   LAM                 0
#> 6 abaca     ARM   ASI                 0
```
