# Derive the binding critical-nitrogen threshold per cell.

Identifies, for every 0.5-degree cell, the impact threshold whose
critical nitrogen surplus is lowest among the three threshold-specific
surfaces of Schulte-Uebbing et al. (2022): atmospheric deposition
(`"de"`), groundwater nitrate (`"gw"`) and surface-water eutrophication
(`"sw"`). That threshold is the one that binds. The archive's own
threshold-exceedance map
([`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
with `var = "threshold_exceedance"`) is a different quantity: it records
which thresholds the 2010 surplus exceeds.

`binding_threshold` is `"deposition"`, `"groundwater"`,
`"surface_water"`, a two-way tie, `"yield_potential_cap"` or
`"non_agricultural_floor"`. A two-way tie is recorded explicitly by
joining both tied thresholds with `+` in that fixed order, for example
`"groundwater+surface_water"`. Ties are exact equalities of the
deposited values: on the real archive no surface lies within 1e-6 kg
N/ha of the cell minimum without equalling it, so a tolerance would
change nothing. A cell missing from any of the three surfaces gets `NA`.

A cell where all three critical surpluses are equal has no environmental
threshold that binds on its own. Schulte-Uebbing et al. (2022, Nature
610, Methods) set such a threshold-independent value by one of two
rules, and the label names which one:

- `"non_agricultural_floor"`: all three thresholds are exceeded (each
  threshold-specific exceedance of the critical surplus is positive) or
  the tied critical surplus is negative. The source's "Aggregation to
  regional and planetary boundaries" section states: "Where N losses
  from non-agricultural sources alone exceeded thresholds, critical N
  inputs from fertilizer and manure were set to zero." The critical
  input is then the fixation and deposition left over, the same for
  every threshold.

- `"yield_potential_cap"`: every other tie. Step 4 of the source's
  Methods reads: "for areas with no threshold exceedance, cut off
  critical inputs and surplus at a maximum value, set to the input level
  required to obtain crop yield potentials", with
  `Nin(crit,max) = Nup(Yp) / NUE(act)`, where `Nup(Yp)` is crop nitrogen
  uptake at potential yield and `NUE(act)` the current nitrogen use
  efficiency, "capped ... at 0.8".

A tie whose critical surplus is not negative and whose exceedance is
missing on any threshold cannot be assigned to either rule and gets
`NA`. On the real archive the three critical inputs are identical in
every tied cell. The two labels count 293 floor and 8,895 cap cells of
28,573 for `"ara"`, 293 and 9,138 of 28,881 for `"all"`, and 0 and 1,727
of 11,740 for `"igl"`. The split matches the archive's
threshold-exceedance map exactly: every floor cell carries code 8 and
every cap cell code 1. The 293 floor cells include 3 with a negative
critical surplus (minimum -21.78 kg N/ha), and their median critical
input is 38.7 kg N/ha against 90.7 for the `"all"` cap cells.

When the deposited minimum-of-all-media surface (`"mi"`) is supplied,
`binding_matches_mi` reports whether it equals the lowest of the three
threshold-specific surpluses. Where it does not, the relation between
`"mi"` and the three surfaces is undetermined: `"mi"` differs from
`min(de, gw, sw)` there and the source text does not explain why. On the
real archive this happens in 1,623 of 28,573 cells for `"ara"` (maximum
gap 159.5 kg N/ha), 1,540 of 28,881 for `"all"` (159.5 kg N/ha) and
1,480 of 11,740 for `"igl"` (479.1 kg N/ha), with `"mi"` above the
minimum in some cells and below it in others. `binding_threshold` still
names the argmin of the three surfaces in those cells.

## Usage

``` r
build_critical_n_binding(
  critical = NULL,
  exceedance = NULL,
  land_use = c("all", "ara", "igl"),
  dir = NULL,
  example = FALSE
)
```

## Arguments

- critical:

  Optional named list of
  [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
  critical-surplus layers (`var = "critical_n_surplus"`) with elements
  `de`, `gw` and `sw`, and optionally `mi`, each stamped with its own
  threshold and with `land_use`. When `NULL` (default) the four layers
  are read from the archive with
  [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md),
  together with `exceedance`.

- exceedance:

  Named list of
  [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
  exceedance layers (`var = "exceedance"`) with elements `de`, `gw` and
  `sw`, used only to tell the two kinds of three-way tie apart. Required
  when `critical` is supplied; read from the archive when both are
  `NULL` (default).

- land_use:

  Land-use scope: `"all"`, `"ara"` or `"igl"`, as in
  [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md).
  Supplied layers must carry this scope.

- dir:

  Optional archive directory passed to
  [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
  when `critical` is `NULL`.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with one row per cell: `cell_id`, `lon`, `lat`,
`critical_land_use`, the three threshold-specific critical surpluses
`critical_de_kgn_ha`, `critical_gw_kgn_ha`, `critical_sw_kgn_ha` (kg N
per hectare per year), their exceedances `exceedance_de_kgn_ha`,
`exceedance_gw_kgn_ha`, `exceedance_sw_kgn_ha`, the minimum critical
surplus `binding_critical_kgn_ha`, the `binding_threshold` label, the
deposited `critical_mi_kgn_ha` and the logical `binding_matches_mi`
(both `NA` when `mi` is not supplied).

## Examples

``` r
build_critical_n_binding(example = TRUE)
#> # A tibble: 5 × 14
#>   cell_id   lon   lat critical_land_use critical_de_kgn_ha critical_gw_kgn_ha
#>     <int> <dbl> <dbl> <chr>                          <dbl>              <dbl>
#> 1  128521  0.25  0.75 ara                               50                 45
#> 2  128522  0.75  0.75 ara                               90                 90
#> 3  128523  1.25  0.75 ara                                4                  4
#> 4  129241  0.25  0.25 ara                               12                 40
#> 5  129242  0.75  0.25 ara                               60                 18
#> # ℹ 8 more variables: critical_sw_kgn_ha <dbl>, exceedance_de_kgn_ha <dbl>,
#> #   exceedance_gw_kgn_ha <dbl>, exceedance_sw_kgn_ha <dbl>,
#> #   binding_critical_kgn_ha <dbl>, binding_threshold <chr>,
#> #   critical_mi_kgn_ha <dbl>, binding_matches_mi <lgl>
```
