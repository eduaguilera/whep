# Build grazable grass availability.

Multi-method wrapper for the grass forage supply ceiling that feeds
allocation. The default `lpjml` method reads pinned LPJmL-derived
managed-grassland net primary production/availability unless custom
artifact data, a custom artifact path, or `run_dir` points to local
inputs; `coefficient` applies a per-area grass-yield coefficient and is
not yet implemented (it needs a `grass_yield_coef` dataset).

## Usage

``` r
build_grass_availability(method = c("lpjml", "coefficient"), ...)
```

## Arguments

- method:

  Grass-availability method, `"lpjml"` or `"coefficient"`.

- ...:

  Passed to the selected method's builder, e.g.
  [`build_grass_availability_lpjml()`](https://eduaguilera.github.io/whep/reference/build_grass_availability_lpjml.md).

## Value

A tibble of grass availability with a `method_grass` column recording
the method used.

## Examples

``` r
build_grass_availability(method = "lpjml", example = TRUE)
#> # A tibble: 3 × 7
#>      lon    lat  year grass_npp_gc_m2 grass_avail_dm_t_ha grass_avail_dm_t
#>    <dbl>  <dbl> <int>           <dbl>               <dbl>            <dbl>
#> 1   9.25  47.8   2000            612.                6.26            16980
#> 2 -55.2  -12.2   2000            488.                4.99            15640
#> 3  35.8   -1.25  2000            422.                4.31            13110
#> # ℹ 1 more variable: method_grass <chr>
```
