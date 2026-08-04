# Coello (2025) crop-specific synthetic nitrogen application rates

Corrected average synthetic-nitrogen application rates (kg N
ha\\^{-1}\\) by calendar year, FAOSTAT area and CBS crop item, derived
from Coello et al. (2025). Coello's 13 crop groups are crosswalked to
FAOSTAT CBS items via `inst/extdata/coello_mapping.csv` and
[items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md);
native years 1961-2019 are carried forward to 2023 with
[fill_linear](https://eduaguilera.github.io/whep/reference/fill_linear.md).
Used by the package synthetic-fertiliser path (`method = "coello"`) to
differentiate the FAOSTAT national synthetic-N total across crops while
conserving that national total. Rates are the source values clamped to
non-negative; implausible outliers above 1000 kg N/ha (Coello
model-extrapolation artifacts in a few small areas) are treated as
missing, so those crop-area-years follow the missing-rate fallback
(temporal fill where available, else the area-share weight) rather than
skewing the split. The downstream rate-weighted share normalises within
each country-year, so the national total is conserved regardless.

## Usage

``` r
coello_synthetic_n
```

## Format

A tibble with one row per year-area-crop:

- `year`: Integer calendar year (1961-2023).

- `area_code`: Integer FAOSTAT area code.

- `item_cbs_code`: Integer CBS crop item code.

- `kg_n_ha`: Synthetic-N application rate (kg N ha\\^{-1}\\).

## Source

Coello, D. et al. (2025) A global gridded crop-specific fertilization
dataset from 1961 to 2019. *Scientific Data* 12:40.
[doi:10.1038/s41597-024-04215-x](https://doi.org/10.1038/s41597-024-04215-x)

## Examples

``` r
head(coello_synthetic_n)
#> # A tibble: 6 × 4
#>    year area_code item_cbs_code kg_n_ha
#>   <int>     <int>         <int>   <dbl>
#> 1  1992         1           677    37.0
#> 2  1993         1           677    46.1
#> 3  1994         1           677    32.9
#> 4  1995         1           677    28.9
#> 5  1996         1           677    27.2
#> 6  1997         1           677    32.3
```
