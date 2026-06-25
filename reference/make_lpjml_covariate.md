# Build an LPJmL/WHEP spatial covariate function

Creates a `function(centroids_sf, year)` suitable for APIs that accept a
spatial covariate density, such as
[`build_constant_territory_series()`](https://eduaguilera.github.io/whep/reference/build_constant_territory_series.md).
The covariate is built from prepared WHEP/LPJmL spatialization inputs,
either from a local `input_dir` or from pinned WHEP spatialization
inputs when `input_dir = NULL`.

## Usage

``` r
make_lpjml_covariate(
  input_dir = NULL,
  years = NULL,
  weighting = c("total_cropland", "crop_pattern"),
  item_prod_code = NULL,
  cft_mapping = whep::cft_mapping
)
```

## Arguments

- input_dir:

  Directory holding prepared WHEP/LPJmL input parquets. If `NULL`, the
  function reads pinned spatialization inputs via
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md).

- years:

  Integer vector of years to retain. If `NULL`, all years present in the
  required input are used.

- weighting:

  Either `"total_cropland"`, `"crop_pattern"`, or a custom
  `function(centroids_sf, year)`. A custom function is returned
  unchanged.

- item_prod_code:

  FAOSTAT/WHEP production item code. Required when
  `weighting = "crop_pattern"`.

- cft_mapping:

  Mapping from WHEP item codes to CFT/LUH2 types. Defaults to
  [cft_mapping](https://eduaguilera.github.io/whep/reference/cft_mapping.md).

## Value

A function `function(centroids_sf, year)` returning non-negative density
values aligned to `centroids_sf`.

## Examples

``` r
# A custom covariate function is returned unchanged, ready to plug into
# build_constant_territory_series(). Here a uniform (area-weighting) density:
uniform <- function(centroids_sf, year) rep(1, nrow(centroids_sf))
covariate <- make_lpjml_covariate(weighting = uniform)
identical(covariate, uniform)
#> [1] TRUE

# The "total_cropland" and "crop_pattern" modes instead read prepared
# WHEP/LPJmL spatialization parquets from `input_dir` (or pinned inputs).
```
