# Assemble WHEP's cell-polity crosswalk with true grid-cell area.

Reads the cell-polity fraction table (`lon`, `lat`, `area_code`,
`polity_frac`) and adds `cell_area_ha`, computed from latitude with the
same 0.5-degree cell-area formula used across the package (see
[`build_grass_availability_lpjml()`](https://eduaguilera.github.io/whep/reference/build_grass_availability_lpjml.md)).
This assembles the `data$cell_polity` contract that every Module C
function (e.g.
[`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md),
[`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md),
[`get_soc_climate_drivers()`](https://eduaguilera.github.io/whep/reference/get_soc_climate_drivers.md))
expects as a required input.

## Usage

``` r
build_cell_polity(
  polity_fraction_path = NULL,
  area_key = c("grid", "polity_area"),
  version = NULL,
  example = FALSE
)
```

## Arguments

- polity_fraction_path:

  Optional path to a local parquet, overriding
  `Sys.getenv("WHEP_POLITY_FRACTION_PATH")` and the pin.

- area_key:

  Which area code the output is keyed on: `"grid"` (default, the table's
  own reporting-area codes) or `"polity_area"` (the
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
  bucket national tables are aggregated on).

- version:

  Pin version, passed to
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md).
  `NULL` takes the version frozen in
  [whep_inputs](https://eduaguilera.github.io/whep/reference/whep_inputs.md).

- example:

  If `TRUE`, return a small fixture instead of reading the pin, so the
  example runs offline.

## Value

A tibble with `lon`, `lat`, `area_code`, `polity_frac` and
`cell_area_ha`.

## Which area code the grid is keyed on

The parquet is rasterized from present-day polygons through
`inst/extdata/regions.csv`, so its `area_code` is a raw reporting-area
code and **not** necessarily a
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
`polity_area_code`: the bucket every polity-keyed national table in whep
is aggregated on. Grid codes that are not a bucket cannot join to
national data at all: the join is silently empty on both sides.

`area_key` selects which of the two the output carries. It is not a
fallback: `"grid"` is the default, reproduces the parquet's own codes
bit-for-bit, and warns naming the codes that cannot resolve;
`"polity_area"` resolves each code to its bucket through
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
and re-sums `polity_frac` within `(lon, lat, area_code)`, so a cell
straddling two areas of the same bucket stays one row per bucket and
each cell's fractions still sum to 1. It respects
`options(whep.unfold_rest_of_world = TRUE)` (see
[`folded_reporting_areas()`](https://eduaguilera.github.io/whep/reference/folded_reporting_areas.md)),
so the grid and the national tables agree about where a Rest-of-World
member's rows belong.

Under `"polity_area"` the raw reporting code is **carried, not
replaced**: the output gains `grid_area_code` holding the parquet's own
code, joined with `+` where a cell's areas collapse into one bucket. So
the fold this performs is recoverable at the join rather than baked into
the grid — a derived key silently overwriting the raw one it came from
is what whep#582 reports from the output side, and the same fold is what
dropped Sudan's 40.8 M goats and doubled its sugar cane in the published
production series (whep#563).

The output deliberately does **not** gain `polity_code` /
`reporting_polity_*`. A bucket is not a polity: `999` holds up to 17
territories at once and `206` holds Sudan and South Sudan together, so
no polity code string is recoverable from this year-less grid. Carrying
that identity needs the cell x polity x validity-interval unit tracked
by epic whep#458, not a column added here.

## Where the table comes from

WHEP produces this crosswalk itself, in section 1b of
`inst/scripts/prepare_spatialize_all.R`, from Natural Earth polygons and
`inst/extdata/regions.csv`. It is therefore published as the
`spatialize-cell-polity-fraction` pin alongside the nine other artefacts
of that script, and the pin is what this function reads by default: no
user has to run the producer, or hold the Natural Earth shapefile, to
get the table WHEP's own runs use. `WHEP_POLITY_FRACTION_PATH` and
`polity_fraction_path` are **overrides** for a local development build,
in the shape
[`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md)
already uses.

## Vintage of the area vocabulary

The table's `area_code` values must all exist in the `regions.csv` the
installed package carries, because that is the table its producer
rasterizes through. A copy built through an older vintage keyed Ethiopia
`62` and Sudan (former) `206` where today's lookup uses `238` and `276`,
so adopting it deleted both countries from every consumer (whep#694).
Such a table is **refused** with class `whep_stale_cell_polity_grid`
rather than read, and the message names the producer re-run that
rebuilds it. The check guards the override, which is the only route that
can now go stale, but it runs on the pin too so that pinning an older
`version` cannot reintroduce the deletion either.

## Examples

``` r
build_cell_polity(example = TRUE)
#> # A tibble: 6 × 5
#>     lon   lat area_code polity_frac cell_area_ha
#>   <dbl> <dbl>     <int>       <dbl>        <dbl>
#> 1 -0.25  10.2        81      1           304174.
#> 2 -0.25  10.8        81      0.917       303682.
#> 3 -0.25  10.8       217      0.0833      303682.
#> 4 -0.25  11.2       233      0.778       303167.
#> 5 -0.25  11.2        81      0.194       303167.
#> 6 -0.25  11.2       217      0.0278      303167.
```
