# Soil carbon-to-nitrogen ratios for organic-matter balances.

Soil carbon-to-nitrogen ratios used to convert a soil organic carbon
stock change into net nitrogen mineralization (when carbon is lost) or
net nitrogen sequestration (when carbon accumulates), by cropland class
and management system.

## Usage

``` r
soil_cn_ratios
```

## Format

A tibble with columns:

- cropland_class:

  Land class: `"Cropland"` or `"NonCropland"`.

- management:

  Management system: `"Conventional"` or `"Organic"`. **Only the
  `"Conventional"` rows are selectable by this package.**
  [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
  has no management dimension – it is a global gridded balance – so an
  argument for this column could only be set world-wide, and "run the
  whole world as organic" is not a run anyone should perform. The
  `"Organic"` rows therefore ship as *reference values for downstream
  consumers*, not as inputs this package can be asked to use (issue
  809).

- cn_ratio:

  Soil organic-matter carbon-to-nitrogen ratio.

- cn_mineralization:

  Carbon-to-nitrogen ratio applied when soil organic carbon is
  mineralized (net carbon loss).

- cn_sequestration:

  Carbon-to-nitrogen ratio applied when soil organic carbon is
  sequestered (net carbon gain).

## Source

Soil carbon-to-nitrogen ratios from the Spain historical SOC pipeline
coefficient set, consistent with the RothC framework of Coleman, K. &
Jenkinson, D. S. (1996).
[doi:10.1007/978-3-642-61094-3_17](https://doi.org/10.1007/978-3-642-61094-3_17)
.

## Examples

``` r
soil_cn_ratios
#> # A tibble: 4 × 5
#>   cropland_class management   cn_ratio cn_mineralization cn_sequestration
#>   <chr>          <chr>           <dbl>             <dbl>            <dbl>
#> 1 Cropland       Conventional       10                 8               11
#> 2 Cropland       Organic            10                 9               13
#> 3 NonCropland    Conventional       15                11               15
#> 4 NonCropland    Organic            15                11               15
```
