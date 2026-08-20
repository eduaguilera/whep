# Calculate Finn Cycling Index for each province and year

Calculates the Finn Cycling Index (FCI) for the GRAFS nitrogen flow
network following Finn (1976) and Allesina & Ulanowicz (2004). The index
measures the fraction of total system throughput that is cycled through
internal compartments (Cropland, semi-natural agroecosystems, Livestock,
and People).

## Usage

``` r
create_finn_indicator(n_prov_destiny = NULL, example = FALSE)
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

A tibble with columns `year`, `province_name`, and `finn_index`.

## Examples

``` r
create_finn_indicator(example = TRUE)
#> # A tibble: 8 × 3
#>    year province_name finn_index
#>   <dbl> <chr>              <dbl>
#> 1  1900 A_Coruna          0.0942
#> 2  1900 Albacete          0.0765
#> 3  1900 Alicante          0.0476
#> 4  1900 Almeria           0.115 
#> 5  1900 Araba             0.112 
#> 6  1900 Asturias          0.137 
#> 7  1900 Avila             0.194 
#> 8  1900 Badajoz           0.113 
```
