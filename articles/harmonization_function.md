# Harmonization Function Guide

``` r
library(whep)
# Don't ask for credentials (read public files)
googlesheets4::gs4_deauth()
```

The WHEP package provides a function
[`harmonize_interpolate()`](https://eduaguilera.github.io/whep/reference/harmonize_interpolate.md)
to harmonize data with simple (1:1, N:1) and 1:n mappings. This vignette
demonstrates how to use this function effectively.
