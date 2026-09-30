# Aggregate a gridded land quantity onto administrative units

Sum a per-cell land quantity onto the compartments of a granted-depth
country grid, giving the per-unit extent `E_u(t)` the seam back-cast
modulates shares with:

\$\$E_u(t) = \sum_c E_c(t) \times f(c, u)\$\$

where `f(c, u)` is `cell_area_frac`, the unit's share of the physical
cell on the land basis. The gridded tables are the ones already in
memory for the spatialization engine, so no LUH2 archive is re-read
here:
[`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md)
is never called, and the identity between the extent that modulates a
share and the extent that weights cells within a unit holds by
construction rather than by assertion. What the construction cannot
prove is which LUH2 vintage the caller's tables came from; the prep-side
provenance stamp on those tables carries that.

## Usage

``` r
aggregate_unit_extent(
  gridded,
  country_grid,
  quantity,
  extent_by = NULL,
  container_frac = NULL,
  residual_code = NULL,
  tolerance = 1e-08
)
```

## Arguments

- gridded:

  A tibble of per-cell land quantities keyed on `lon`, `lat`, `year`,
  plus any extra key columns named in `extent_by`, plus the numeric
  columns named by `quantity`.

- country_grid:

  A tibble of compartments: `lon`, `lat`, `area_code`,
  `level_polity_code`, `level`, `cell_area_frac`. One row per
  `(lon, lat, area_code, level_polity_code)`; an interval-grained layer
  must be filtered to one geometry first.

- quantity:

  Either a character vector of `gridded` columns summed with weight 1,
  or a lookup table with a `quantity` column of column names, an
  optional numeric `weight` column (default 1) and the `extent_by` key
  columns the lookup is keyed on. Several rows per key sum several
  columns.

- extent_by:

  Character vector of extra key columns of the output. Each must be a
  column of `gridded` (carried through the aggregation) or a key column
  of a `quantity` lookup (which selects columns per key), never both.

- container_frac:

  Optional tibble `lon`, `lat`, `area_code`, `cell_area_frac` giving the
  container's own share of each cell on the same land basis. Required by
  `residual_code`, ignored without it.

- residual_code:

  Optional `level_polity_code` for a residual pseudo-unit, whose extent
  is `E_res(t) = E_container(t) - sum_u E_u(t)`. `NULL` (default) emits
  no residual row, which is the right shape unless the allocation runs
  under the residual policy.

- tolerance:

  Relative tolerance below which a negative residual extent counts as
  floating-point noise and is clamped to zero. A larger negative
  residual aborts: it means the unit fractions exceed the container's.

## Value

A tibble: `area_code`, `level_polity_code`, `level`, the `extent_by`
columns, `year`, `extent_ha` and `extent_basis` (the `weight * column`
terms behind the row, `+`-joined in a stable order). One row per unit,
key and year for which at least one compartment of that unit met a
gridded cell.

## Mirroring the engine's land quantity

`R/spatialize.R:337-405` builds the crop weight from
`cropland_ha = i.cropland_ha * cell_area_frac`; a type-aware run
replaces that with `type_ha * cell_area_frac` joined on
`(lon, lat, luh2_type)`, zeroes cells with no row for the crop's type,
and restores total cropland for a whole `(area_code, item_prod_code)`
group whose type potential `sum(harvest_fraction * cropland_ha)` is not
positive. `R/spatialize_livestock.R:358-405` builds the livestock weight
from `pasture_ha + rangeland_ha` (`pasture`), `rangeland_ha`
(`rangeland`), `cropland_ha` (`cropland`) or
`0.5 * (pasture_ha + rangeland_ha) + 0.5 * cropland_ha` (`mixed`), also
times `cell_area_frac`.

This function mirrors the `cell_area_frac` multiplication and the
compartment sum. Which land column stands for a given item or species
group is the caller's choice, declared through `quantity`: a character
vector applies one weighted column set to every row, a lookup table
applies a different set per key. The engine's whole-group fallback to
total cropland turns on `harvest_fraction`, a within-unit weight rather
than a land extent, so the caller evaluates it and passes the resulting
per-item choice in the lookup; the choice comes back on `extent_basis`,
so the land quantity behind each row is recorded rather than inferred.

## Territory basis

`country_grid` is used exactly as given, unfiltered by edge validity.
That is the t0-geometry convention of the plan's seam section: shares
and `E_u` are defined on the t0 unit set with t0 geometry held fixed, so
a unit whose containment edge starts after a back-cast year still
contributes its t0 cells at that year. Pass the layer already filtered
to the t0 reference year (`.filter_country_grid_year()`) when the t0
geometry differs from the delivered layer.

## Examples

``` r
grid <- tibble::tibble(
  lon = c(10.25, 10.75, 10.75),
  lat = 40.25,
  area_code = 900L,
  level_polity_code = c("A1", "A1", "A2"),
  level = 1L,
  cell_area_frac = c(1, 0.3, 0.5)
)
cells <- tibble::tibble(
  lon = c(10.25, 10.75),
  lat = 40.25,
  year = 1900L,
  cropland_ha = c(1000, 1200)
)
# A1 = 1000 * 1 + 1200 * 0.3 = 1360; A2 = 1200 * 0.5 = 600.
aggregate_unit_extent(cells, grid, "cropland_ha")
#> # A tibble: 2 × 6
#>   area_code level_polity_code level  year extent_ha extent_basis
#>       <int> <chr>             <int> <int>     <dbl> <chr>       
#> 1       900 A1                    1  1900      1360 cropland_ha 
#> 2       900 A2                    1  1900       600 cropland_ha 
```
