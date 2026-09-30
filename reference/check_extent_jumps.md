# Flag implausible year-on-year jumps in a per-unit extent

Run
[`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md)
on `extent_ha` per unit series, the check the plan's seam section asks
for before an extent modulates a share. WHEP repairs LUH2's isolated
single-year cropland collapses nationally (`.fix_luh2_crop_collapse()`,
`R/build_production.R:627-695`); the same collapse reaches a unit series
unrepaired, where it would propagate into every earlier back-cast year
through the growth chain.

The scan covers whatever years `extent` carries.
[`backcast_admin_shares()`](https://eduaguilera.github.io/whep/reference/backcast_admin_shares.md)
calls it on the back-cast window alone – `t0` and everything before it,
the years the modulation actually reads – so a jump after the seam is
not a reason to refuse a back-cast.

## Usage

``` r
check_extent_jumps(
  extent,
  ratio_bounds = c(0.55, 1.6),
  min_value = 0,
  verbose = FALSE
)
```

## Arguments

- extent:

  A per-unit extent table as returned by
  [`aggregate_unit_extent()`](https://eduaguilera.github.io/whep/reference/aggregate_unit_extent.md).
  Every column that is not `year`, `extent_ha` or `extent_basis`
  identifies the series.

- ratio_bounds:

  Length-2 numeric `c(low, high)` plausible band for the ratio of
  consecutive years, passed to
  [`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md),
  whose own default it inherits.

- min_value:

  Minimum extent both members of a pair must exceed to be flagged,
  passed to
  [`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md).

- verbose:

  Logical, passed to
  [`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md).
  Default `FALSE`, so a guard inside a back-cast stays quiet.

## Value

The
[`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md)
flags tibble: the series key columns, `year`, `prev_value`, `value`,
`ratio` and `allowlisted`.

## Examples

``` r
extent <- tibble::tibble(
  area_code = 900L,
  level_polity_code = "A1",
  level = 1L,
  year = 1900:1903,
  extent_ha = c(1000, 1010, 5, 1020),
  extent_basis = "cropland_ha"
)
check_extent_jumps(extent)
#> # A tibble: 2 × 8
#>   area_code level_polity_code level  year prev_value value     ratio allowlisted
#>       <int> <chr>             <int> <int>      <dbl> <dbl>     <dbl> <lgl>      
#> 1       900 A1                    1  1902       1010     5   0.00495 FALSE      
#> 2       900 A1                    1  1903          5  1020 204       FALSE      
```
