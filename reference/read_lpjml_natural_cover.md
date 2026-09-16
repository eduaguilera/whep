# Read natural-land soil cover from an LPJmL run.

Per-cell, per-year vegetated cover of the natural stand, from LPJmL's
foliar projective cover (`fpc.nc`). Band 1 of that file is the natural
stand FRACTION – how much of the cell is natural – and bands 2 to 15 are
the fourteen natural PFTs' projective cover WITHIN that stand. The cover
of natural land is therefore the sum of bands 2 to 15, capped at 1, and
never band 1, which is an area share and a different quantity.

Supply the result as `data$natural_cover` to
[`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
to replace
[soc_soil_cover_curve](https://eduaguilera.github.io/whep/reference/soc_soil_cover_curve.md)'s
constant 0.85 for the natural class.

## Usage

``` r
read_lpjml_natural_cover(
  run_dir = NULL,
  years = NULL,
  first_year = NULL,
  example = FALSE
)
```

## Source

Schaphoff, S. et al. (2018). LPJmL4 - a dynamic global vegetation model
with managed land - Part 1: Model description. *Geoscientific Model
Development*, 11, 1343-1375.
[doi:10.5194/gmd-11-1343-2018](https://doi.org/10.5194/gmd-11-1343-2018)
.

## Arguments

- run_dir:

  Path to the LPJmL run output directory. `NULL` (default) uses
  `WHEP_LPJML_RUN_DIR`.

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the file covers.

- first_year:

  Calendar year of the file's first time step. `NULL` (default) reads it
  from the file's own `time` axis.

- example:

  If `TRUE`, return a small fixture instead of reading a run. Defaults
  to `FALSE`.

## Value

A tibble with `lon`, `lat`, `year`, `natural_stand_frac` and
`natural_cover` (both fractions in 0-1).

## Why it matters where it does

The constant is close on the global mean – measured cover runs 0.858 in
1901 to 0.884 in 2023 – and wrong in the distribution: the median
natural cell is fully covered at 1.000 while the 5th percentile is bare
at roughly zero. The RothC plant-retainment term is
`0.6 + 0.4 * (1 - cover)`, so a near-bare cell moves from 0.66 to 1.00,
decomposes half again as fast, and loses about a third of its
equilibrium carbon. Those arid cells are exactly where the model sits
furthest from observation. The effect on the global mean is about 1.8%;
per cell it spans 0.66 to 1.10 times.

Managed grassland has no FPC band – `fpc.nc` covers the natural stand
only – so it necessarily stays on the curve.

## Examples

``` r
read_lpjml_natural_cover(example = TRUE)
#> # A tibble: 2 × 5
#>     lon   lat  year natural_stand_frac natural_cover
#>   <dbl> <dbl> <int>              <dbl>         <dbl>
#> 1 -0.25  5.25  2010               0.98          0.99
#> 2 12.2  18.8   2010               0.95          0.04
```
