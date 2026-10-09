# Read the cell-to-polity country grid at a containment depth

The spatialization engines allocate a national total into the
compartments of a *country grid*: one row per physical 0.5-degree cell
and territorial unit, carrying that unit's share of the cell.
`level = 0L` is today's grid, one row per cell and reporting
`area_code`. `level >= 1L` resolves the polycell support one containment
step deeper, so a cell held by a province arrives as a province-keyed
row instead of being folded into its country.

Under `grid_vintage = "snapshot_2015"` level 0 is served by the
unchanged `.read_polycell_country_grid()` path, so such a run is
bit-for-bit what it was. Deeper levels bypass that path entirely – in
particular they never reach `.carbon_support_to_area_code()`, whose fold
is what deletes a province.

## Usage

``` r
read_level_country_grid(
  level = 0L,
  support = NULL,
  containment = NULL,
  reference_year = NULL,
  grid_vintage = c("year_aware", "snapshot_2015"),
  containers = NULL,
  double_claim = c("co_presence", "measured")
)
```

## Arguments

- level:

  Containment depth, a non-negative whole number. `0L` (default) is
  today's cell-to-`area_code` grid.

- support:

  Polycell support table in the
  [`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
  grain, overriding
  [`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md).
  Not read at `level = 0L` with `grid_vintage = "snapshot_2015"`, which
  resolves the support itself.

- containment:

  Containment edge table in the
  [polity_containment](https://eduaguilera.github.io/whep/reference/polity_containment.md)
  schema, overriding the packaged one. Only read at `level >= 1L`.

- reference_year:

  Optional year to snapshot the grid at, applied with the package's own
  validity predicate. `NULL` (default) keeps the interval grain, which
  is what the engines' per-year filter expects. Not read at `level = 0L`
  with `grid_vintage = "snapshot_2015"`, whose reference year is fixed
  at `.carbon_support_year()`.

- grid_vintage:

  Which vintage of the cell-to-polity support a level-0 grid is read at,
  `"year_aware"` (default) or `"snapshot_2015"`. See *Which vintage of
  the support level 0 is read at*, which states what each one allocates
  into. Not read at `level >= 1L`, which is year-aware by construction;
  snapshot a depth with `reference_year`.

- containers:

  Optional integer vector of container reporting `area_code`s the depth
  read is scoped to – the containers a run grants a depth. `NULL`
  (default) reads every admitted edge. Only read at `level >= 1L`. See
  *Which containers a depth is read for*.

- double_claim:

  Which clashes the double-claim gate refuses, one of `"co_presence"`
  (default) or `"measured"`. See *Which double claims are refused*. Only
  read at `level >= 1L`.

## Value

A `tibble` with `lon`, `lat`, `area_code` (integer, the container's
reporting code), `level_polity_code` (character, `NA` at level 0),
`level` (integer), `cell_area_frac`, `polycell_id`, `start_year`,
`end_year`, `cell_area_ha` and `land_area_ha`. At level 0 with
`grid_vintage = "snapshot_2015"` the six columns
`.carbon_cell_support()` returns are passed through unchanged; with
`"year_aware"` those six arrive alongside `start_year` and `end_year`.

## Which vintage of the support level 0 is read at

The support is a table of cell-to-polity claims with validity intervals,
so "which polity holds this cell" has an answer per year. Two answers
are selectable and neither is ever a fallback for the other:

- `"snapshot_2015"`:

  One reference year, `.carbon_support_year()`, folded onto reporting
  `area_code`s – the carbon path's DA-28 / whep#549 choice that
  migrating the territorial EXTENT does not migrate the ATTRIBUTION.
  Every year of a run is allocated into the SAME present-day geography.

- `"year_aware"`:

  The support in its own interval grain, keyed and folded per epoch, so
  `.filter_country_grid_year()` selects the polities valid in each
  simulation year – the same treatment a granted depth gets. A cell held
  by a polity that has no reporting code in that year, or by no polity
  at all, is then absent for that year.

The two are different geographies, not two precisions of one, and the
difference is large before about 1990: on the `20260825T102349Z-1a0eb`
support the year-aware read covers 41,597 cells and 7,942 Mha of land at
1851 against the snapshot's 66,709 cells and 12,926 Mha. Of that year's
LUH2 cropland, 41.6% is taken away from the country the snapshot gives
it to and 21.5% is given to a country the snapshot does not – the two
directions are reported separately, because a metric that scores only
the losses cannot see a share that absorbs one country's land into
another. 101.8 Mha of the loss falls in cells no polity claims in 1851
and is attributed to nobody. At 2015 the two vintages agree exactly,
which is the check that fails if the year-aware share is renormalised.

Those figures are the `20260825T102349Z-1a0eb` support's, not the
`20260827T190201Z-f82a2` one's: that later pin books every hectare of
inland water and ice as land (whep#1010), which moves the cell count and
the land, though not the argument.
[`run_spatialize()`](https://eduaguilera.github.io/whep/reference/run_spatialize.md)
records the resolved value in `run_metadata.yaml` and in a
`method_grid_vintage` column on every output it writes.

`"snapshot_2015"` is still the default, against whep#1000 T31(j)'s
stated preference, because WHEP's national tables are on a
**constant-territory** basis and the support is on a
**historical-polity** one. In 1961 `country_areas` reports the Russian
Federation, Kazakhstan and Ukraine where the year-aware grid offers only
the USSR, so 17.2% of the world's harvested area has no cell to land in
and is dropped whole; `validation/spatialize_grid_vintage.R` measures it
per year. Reconciling the two bases is a lineage step on the national
side, not a grid setting.

## Which rows count as a level

A polity is at depth *d* when the containment edge
([polity_containment](https://eduaguilera.github.io/whep/reference/polity_containment.md))
places it *d* steps inside a container that is not itself contained. An
edge is admitted only when its container resolves, through
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md),
to a single reporting `area_code` **and** is not of `polity_type`
`"aggregate"`. An aggregate's reporting code is a matrix *bucket*: `999`
alone pools 62 territories, so filing a province under one would
attribute it to Rest of World rather than to a country. Dropped edges
are counted and named, never skipped in silence.

## How the share of the cell is measured

`cell_area_frac` is the unit's share of the **physical cell on the land
basis** – `land_area_ha / cell_land_ha`, the same basis
`.carbon_attach_land_share()` uses at level 0, with the cell's whole
measured land as the denominator. Everything the fraction later splits
(LUH2 class areas, crop-pattern hectares) is already land-only, so
dividing by the whole cell would subtract the water twice. Because the
support is read in its interval grain, the denominator is evaluated per
interval-start year: the cell's land partition genuinely differs between
epochs, and taking one denominator across all of them would count a cell
once per epoch.

Two support shapes are recognised, and which one is in hand is
**detected, never assumed**:

- replacement:

  The support carries the units *instead of* their container (the shape
  a level-tagged pin delivers). `land_area_ha` is already the unit's
  absolute land, so the share is taken directly. A container row
  surviving beside its own members in one cell is refused: both claim
  the same ground, and choosing one silently is exactly the double count
  this epic exists to remove.

- nested:

  The support carries a within-container share column
  (`container_area_frac`, `container_frac` or `parent_frac`) alongside
  the container's own row. The unit's share of the cell is then the
  composition `f_unit x f_container`, with `f_container` measured from
  the container's own row on the same land basis.

## Which double claims are refused

A support that carries a container's own row beside its units' rows in
one cell claims that ground twice: `cell_land_ha` is the sum over the
cell, so every unit's share is divided by a total counting the same
ground twice, and nothing downstream can see it because the container's
national total still reconciles. The two rules are alternatives, never a
fallback.

- `"co_presence"`:

  The default, and fail-closed: a container row found beside its own
  units is refused, full stop. It is the strict rule because this table
  carries no geometry – whether the two polygons overlap cannot be read
  off it.

- `"measured"`:

  Refuses only a container the cell's own area REFUTES: at least one
  shared cell-epoch whose whole claimed territory exceeds
  `cell_area_ha`. A cell cannot hold more territory than it has, so an
  excess is proof; but the proof is ONE-SIDED, which is why this is the
  weaker rule and not the default. A cell half of which is ocean can
  hold a duplicate under its own area and show no excess. Every clash it
  passes over is warned about by container and cell-epoch count.

On the `20260827T190201Z-f82a2` support the measurement separates the
two cleanly: over the ten containers the shipped edge table admits at
depth 1, 3,285 of 3,965 clashing cell-epochs over-claim by 529.03 Mha in
total, and the whole of that sits in eight containers whose units are
genuinely nested – Alaska inside the USA alone over-claims 150 Mha over
1,156 of its 1,372 shared cells. The two that over-claim nothing are
Ryukyu inside Japan 1895-1945 (8 cells, 0 Mha) and Singapore inside
Malaysia (1 cell, 0 Mha), whose ground in the shared cell is disjoint.
The decision is taken per CONTAINER rather than per cell for that
reason: a nested container's coastal cells hide their own duplicate,
while its interior cells prove the nesting outright.

## Which containers a depth is read for

Without `containers`, every edge
[polity_containment](https://eduaguilera.github.io/whep/reference/polity_containment.md)
admits at this depth is read, whoever the run is for. On the shipped
edge table that is ten containers with support rows – Alaska inside the
USA, three 1949 Indonesian units, Manchuria inside three Chinese epochs,
Serbia inside three Yugoslav ones, Singapore inside Malaysia – and each
of them is carried in the polycell support BESIDE its own units, so
`.level_check_no_double_claim()` refuses the read. The refusal is right:
continuing would divide every unit's share by a cell land total that
counts the same ground twice. It is simply not the Japanese run's
business.

`containers` scopes the edge set to the containers asked for, and that
is all it does. **Scoping is not suppressing**: the double-claim gate,
the aggregate-container refusal and the support-empty abort all still
run, on the scoped edges, so a granted container whose own row survives
beside its units aborts exactly as before. A container named here that
no edge places at this depth aborts with class
`whep_level_container_not_admitted` rather than returning a grid that
silently lacks it.

## See also

[`build_allocation_layer()`](https://eduaguilera.github.io/whep/reference/build_allocation_layer.md),
[`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md).

## Examples

``` r
# Offline: a two-cell support for one Japanese prefecture, with the
# containment edge that puts it inside Japan. No pin and no network.
support <- tibble::tribble(
  ~polycell_id, ~cell_id, ~lon, ~lat, ~polity_code,
  "JPN-23-1871-2025@1", 1L, 137.25, 35.25, "JPN-23-1871-2025",
  "JPN-23-1871-2025@2", 2L, 137.75, 35.25, "JPN-23-1871-2025"
) |>
  dplyr::mutate(
    area_code = 110L,
    start_year = 1952L,
    end_year = 2025L,
    cell_area_ha = 3000,
    land_area_ha = c(1200, 2000)
  )
containment <- tibble::tibble(
  member_code = "JPN-23-1871-2025",
  container_code = "JPN-1952-2025",
  start_year = 1952L,
  end_year = 2025L,
  basis = "prefecture inside JPN-1952-2025 for those years"
)
read_level_country_grid(
  level = 1L,
  support = support,
  containment = containment
)
#> No `containers` scope: every admitted containment edge is read (1 container).
#> ℹ country_grid: polycell support at level 1, 2 compartments over 2 cells in 1 container.
#> # A tibble: 2 × 11
#>     lon   lat area_code level_polity_code level cell_area_frac polycell_id      
#>   <dbl> <dbl>     <int> <chr>             <int>          <dbl> <chr>            
#> 1  137.  35.2       110 JPN-23-1871-2025      1              1 JPN-23-1871-2025…
#> 2  138.  35.2       110 JPN-23-1871-2025      1              1 JPN-23-1871-2025…
#> # ℹ 4 more variables: start_year <int>, end_year <int>, cell_area_ha <dbl>,
#> #   land_area_ha <dbl>

# The same read scoped to the containers a run grants a depth. Japan's
# reporting code is 110, so the edge above is kept and any other
# container's is left at level 0.
read_level_country_grid(
  level = 1L,
  support = support,
  containment = containment,
  containers = 110L
)
#> ℹ country_grid: polycell support at level 1, 2 compartments over 2 cells in 1 container.
#> # A tibble: 2 × 11
#>     lon   lat area_code level_polity_code level cell_area_frac polycell_id      
#>   <dbl> <dbl>     <int> <chr>             <int>          <dbl> <chr>            
#> 1  137.  35.2       110 JPN-23-1871-2025      1              1 JPN-23-1871-2025…
#> 2  138.  35.2       110 JPN-23-1871-2025      1              1 JPN-23-1871-2025…
#> # ℹ 4 more variables: start_year <int>, end_year <int>, cell_area_ha <dbl>,
#> #   land_area_ha <dbl>

# The same support read at level 0. The default vintage is year-aware,
# which takes this support and keeps its validity intervals; the 2015
# snapshot resolves the support itself and refuses one.
read_level_country_grid(
  level = 0L,
  support = support,
  grid_vintage = "year_aware"
)
#> ℹ country_grid: polycell support, year-aware, 2 compartments over 2 cells in 1 epoch.
#> # A tibble: 2 × 8
#>     lon   lat area_code cell_area_ha land_area_ha cell_area_frac start_year
#>   <dbl> <dbl>     <int>        <dbl>        <dbl>          <dbl>      <int>
#> 1  137.  35.2       110         3000         1200              1       1952
#> 2  138.  35.2       110         3000         2000              1       1952
#> # ℹ 1 more variable: end_year <int>
```
