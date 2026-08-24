# Build the polycell spatial support table

Produce WHEP's canonical spatial support unit, the **polycell**: a
0.5-degree grid cell intersected with a polity over that polity's
validity interval. Each row carries the polity's territory in the cell
decomposed into three separately addressable categories,
`polity_area_ha = land_area_ha + inland_water_ha + ice_area_ha`, so that
aggregating polycells to a polity changes no absolute value and no
quantity crosses a border it does not belong to.

Areas are geodesic, from
[`sf::st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.html)
on unprojected WGS84 with spherical (`s2`) geometry, matching WHEP's own
spherical convention; `cell_area_ha` keeps the package formula so it
stays bit-identical to
[`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md).
Territory is the union of live real polities; land claimed by no live
polity is emitted in the `"unassigned"` attribute rather than
renormalised away.

The default grain is **interval-keyed**: one row per polycell per
interval over which every area is constant, carrying `start_year` and
`end_year`. Supply `years` to expand to one row per polycell-year, which
is what
[`expand_polycell_years()`](https://eduaguilera.github.io/whep/reference/expand_polycell_years.md)
does on demand. No area varies within an interval, so the interval grain
is the form to store.

## Usage

``` r
build_polycell_support(
  years = NULL,
  geometries = NULL,
  water = NULL,
  ice = NULL,
  data = list()
)
```

## Arguments

- years:

  Optional integer vector of calendar years. `NULL` (default) returns
  the interval-keyed grain; a vector expands to one row per
  polycell-year and adds a `year` column.

- geometries:

  An `sf` table of polity geometries with at least `polity_code`,
  `start_year` and `end_year`; defaults to
  [`get_polity_geometries()`](https://eduaguilera.github.io/whep/reference/get_polity_geometries.md).
  `start_year` is inclusive; `end_year` is **exclusive at a succession**
  and **inclusive at the open end**, the convention `polities` is
  documented under, and neither bound is ever parsed out of
  `polity_code`. The intervals of one polity must partition time: two
  that overlap are an error rather than a shape the producer reconciles,
  and abort with class `whep_pcs_overlapping_interval`. Optional
  `wiki_status`, `polity_type`, `polygon_status` and `area_code` columns
  are honoured.

- water:

  Optional per-cell `tibble` of inland water with `lon`, `lat` and
  `water_frac`, a fraction of the **whole** cell, as
  [`read_glwd_water()`](https://eduaguilera.github.io/whep/reference/read_glwd_water.md)
  returns it.

- ice:

  Optional `sf` polygon layer of glaciated area, as
  [`read_glaciated_areas()`](https://eduaguilera.github.io/whep/reference/read_glaciated_areas.md)
  returns it, subtracted per polycell by exact geodesic intersection.

- data:

  Optional named list of auxiliary layers: `luh2` the validation layer
  (`lon`, `lat`, `terrestrial_ha`, e.g.
  [`read_luh2_terrestrial()`](https://eduaguilera.github.io/whep/reference/read_luh2_terrestrial.md));
  `crosswalk` the deployed
  [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
  table; `producer_crosswalk` a freshly built
  `build_cell_polity_fraction()` table; and `crosswalk_year`, the year
  whose polycells the crosswalk's present-day geometry describes
  (default 2015). The two crosswalks are read **only** by the DA-12
  footprint reconciliation: no crosswalk column is carried into the
  output, and no crosswalk row the intersection did not reproduce is
  appended to it.

## Value

A `tibble` whose columns are a superset of `polycell_id`, `cell_id`,
`lon`, `lat`, `polity_code`, `area_code`, `start_year`, `end_year`,
`cell_area_ha`, `polity_area_ha`, `land_area_ha`, `inland_water_ha`,
`ice_area_ha`, `geometry_source`, `polygon_status`, `split_method`,
`coverage_status`, `area_engine` and `luh2_vintage`, plus `year` when
`years` is supplied. `area_engine` is `"s2"` except on the pieces the
spherical engine cannot read back, which are measured with
[`terra::expanse()`](https://rspatial.github.io/terra/reference/expanse.html)
rather than dropped. Diagnostics ride as attributes: `"unassigned"` (the
validation-layer disagreement, in both directions: `unassigned_land_ha`
where the polities claim less than the layer and `over_claimed_land_ha`
where they claim more), `"coverage"` (every live polity interval and why
it did or did not produce polycells), `"overlap"` (cells holding more
territory than the cell, because two polities were handed the same
polygon), `"long_edges"` (polity edges the source stores as one long
segment along a parallel, which s2 draws as a bulging great circle),
`"terra_measured"` (polycells whose area came from `terra`),
`"water_excess"` (inland water clamped to the polycell's territory),
`"water_unmatched"` (cells the water layer and the polycells do not
share), `"footprints"` and `"footprint_diff"` (the deployed crosswalk,
the current producer and the polycell footprint, reconciled at
`data$crosswalk_year`).

`"overlap"`, `"terra_measured"`, `"water_excess"`, `"water_unmatched"`
and `"unassigned"` are **interval-grain**, like the table itself: they
carry `start_year` and `end_year`, and one cell contributes a row per
interval. Summing them without first filtering to the interval covering
the year of interest counts the same cell once per epoch. On the shipped
polities that is the difference between 1,343 clamped polycells over all
epochs and 94 in 2015.

Every row is a real polycell: `polity_code`, `polycell_id` and the area
columns are populated on all of them, so `sum(land_area_ha)` over the
output is the land the intersection measured. The DA-13 transition,
which padded the table with the crosswalk rows the intersection did not
reproduce and carried `polity_frac` alongside, ended with C9; the
footprint diagnostics below are where that disagreement is now reported.

**Identity is `polity_code`, and only `polity_code`.** `area_code` rides
along as a label and is not a key: `polity_area_crosswalk` folds 505
polity codes into 201 reporting buckets, 113 of which hold more than one
polity and one of which (206) holds Sudan and South Sudan at the same
time. A table whose whole purpose is correct territorial attribution
cannot be keyed on a bucket that merges two countries, so this one is
not, and no `reporting_polity_code` or `polity_area_code` is derived
here. A consumer joining to a reporting-vocabulary output converts at
its own boundary, and **that conversion is where the lossy fold
happens** – deliberately visible at the consumer rather than hidden in
the support.
[`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md)
refuses an unconverted support instead of converting one silently.

## Land definitions in play

Four definitions of "land" are live in this pipeline and they disagree
by up to 10%, so a global area only means something next to the
definition it was measured on. At 2015:

|                                         |             |
|-----------------------------------------|-------------|
| Definition                              | Global area |
| Whole 0.5-degree cells holding any land | 14.3195 Gha |
| HaNi's own land mask                    | 13.5977 Gha |
| Union of the live polity polygons       | 13.4267 Gha |
| LUH2 terrestrial, `(1 - icwtr) * carea` | 12.9931 Gha |

`polity_area_ha` carries the third row's territory – the polity
polygons, decomposed into land, inland water and ice – but **summing it
does not give the third row**. The union is unique ground, which is what
makes it comparable with the other three; a sum counts shared ground
once per claiming polity. At 2015 `sum(polity_area_ha)` is 13.4599 Gha,
exceeding the union by the 0.0332 Gha two live polities both claim.
Quote the union for a land definition and the sum for attributed
territory, and never read the difference between them as a leak. The
first row is the convention this table replaces – a per-hectare rate
multiplied by `cell_area_ha` – and it over-counts by 11.0%. The fourth
is the DA-5 validation layer: its disagreement with the polygons is
emitted in the `"unassigned"` attribute and never silently reconciled,
and the polygons exceeding it by about 2.2% is what
`inland_water_ha + ice_area_ha` has to account for. The second belongs
to the deposition source and governs a different quantity – see
[`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md),
where WHEP's territory decides *placement* while HaNi's mask decides the
*total*.

Only the first and fourth rows are constants of the inputs;
`inst/scripts/diagnose_polycell_support.R` re-derives both. The polygon
row moves with the polity vintage, so read it back off the table in hand
rather than quoting it – `inst/scripts/reconcile_polity_areas.R`
measures it.

A fifth land mask is present but deliberately absent from the ladder,
because nothing is measured on it: the GLWD water layer carries the CRU
mask (67,420 cells against LUH2's 64,493 terrestrial), so cells one
carries and the other does not are reported in `"water_unmatched"`
rather than dropped by an inner join.

## What does not vary historically

`ice_area_ha` comes from `ne_10m_glaciated_areas` (see
[`read_glaciated_areas()`](https://eduaguilera.github.io/whep/reference/read_glaciated_areas.md)),
a coarse **present-day snapshot**, so it is the same number in 1850 as
in 2015: a historical run carries today's ice extent, and land that lay
under ice in 1850 is credited to `land_area_ha`. This is accepted only
because ice is a **reporting category and not a driver** – nothing in
the package divides by `ice_area_ha` or drives a flux with it. If ice
ever becomes a driver, the source has to be reopened rather than the
caveat restated.

Polity geometry is likewise constant within an interval, and the GLWD
file carries a single time step. That is why the default grain is
interval-keyed: no area column varies by year, so a per-year grain would
repeat identical rows about 173 times.

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE)) {
  build_polycell_support(
    years = 2015L,
    geometries = polycell_example_geometries()
  )
}
#> # A tibble: 6 × 21
#>   polycell_id         cell_id   lon   lat polity_code area_code  year start_year
#>   <chr>                 <int> <dbl> <dbl> <chr>           <int> <int>      <int>
#> 1 AAA-2000-2020@3802…  380269  10.2  44.8 AAA-2000-2…        11  2015       2000
#> 2 AAA-2000-2020@3802…  380270  10.2  45.2 AAA-2000-2…        11  2015       2000
#> 3 AAA-2000-2020@3812…  381269  10.8  44.8 AAA-2000-2…        11  2015       2000
#> 4 AAA-2000-2020@3812…  381270  10.8  45.2 AAA-2000-2…        11  2015       2000
#> 5 AAA-2000-2020@3822…  382269  11.2  44.8 AAA-2000-2…        11  2015       2000
#> 6 AAA-2000-2020@3822…  382270  11.2  45.2 AAA-2000-2…        11  2015       2000
#> # ℹ 13 more variables: end_year <int>, cell_area_ha <dbl>,
#> #   polity_area_ha <dbl>, land_area_ha <dbl>, inland_water_ha <dbl>,
#> #   ice_area_ha <dbl>, geometry_source <chr>, polygon_status <chr>,
#> #   split_method <chr>, coverage_status <chr>, area_engine <chr>,
#> #   luh2_vintage <chr>, water_excess_ha <dbl>
```
