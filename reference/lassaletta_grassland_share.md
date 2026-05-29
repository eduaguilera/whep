# Grassland share of synthetic nitrogen by country and year

Country-level time series of the share of synthetic nitrogen applied to
grassland (versus cropland). Used to split national N totals between
land use types in the WHEP nitrogen pipeline.

## Usage

``` r
lassaletta_grassland_share
```

## Format

A tibble with one row per country-year combination containing:

- `Country`: Country name.

- `year`: Year (numeric).

- `grass_share`: Share of synthetic N applied to grassland (0–1).

## Source

Lassaletta et al. nitrogen flow dataset. See pipeline documentation for
full citation.

## Examples

``` r
head(lassaletta_grassland_share)
#> # A tibble: 6 × 3
#>   Country      year grass_share
#>   <chr>       <dbl>       <dbl>
#> 1 Afghanistan  1961           0
#> 2 Afghanistan  1962           0
#> 3 Afghanistan  1963           0
#> 4 Afghanistan  1964           0
#> 5 Afghanistan  1965           0
#> 6 Afghanistan  1966           0
```
