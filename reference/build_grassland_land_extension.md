# Build the native grassland land extension.

Produce a grassland land extension keyed by
`(year, area_code, item_cbs_code)`, replacing the grassland rows that
used to come from the external `land_fp` pin.

Two area sources are available, selected with `source`:

- `"luh2"` (default): permanent and temporary grassland area (item_cbs
  3000 and 3002, LUH2 pasture and rangeland) taken from
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md).
  This shares the gridded LUH2 land-use basis used by the crop land
  extensions and by livestock spatialisation. Rotational fallow
  (item_cbs 3003) is excluded because the `cropgrids_fallow` crop
  extension already attributes fallow to crops, so counting it here too
  would double count it.

- `"faostat_pasture"`: FAOSTAT "Permanent meadows and pastures" area
  (Land Use item 6655), the statistics-based basis comparable to most
  published footprint studies.

Two metrics are available, selected with `grassland_metric`:

- `"occupation"` (default): the full grassland area is charged as
  occupied land.

- `"active_grazing"`: grassland is capped at the area implied by actual
  grazing intake (the `"grass"` feed in
  [`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md))
  divided by a usable grass yield, so ungrazed or marginal rangeland is
  not charged.

## Usage

``` r
build_grassland_land_extension(
  source = c("luh2", "faostat_pasture"),
  grassland_metric = c("occupation", "active_grazing"),
  usable_grass_yield_dm_t_ha = 2.06,
  data = list(),
  example = FALSE
)
```

## Arguments

- source:

  Grassland area source, `"luh2"` (default) or `"faostat_pasture"`.

- grassland_metric:

  Grassland land metric, `"occupation"` (default) or `"active_grazing"`.

- usable_grass_yield_dm_t_ha:

  Usable grass yield in dry-matter tonnes per hectare, used only by
  `"active_grazing"`. Defaults to `2.06`.

- data:

  Optional named list of pre-loaded inputs to avoid remote reads:
  `primary_prod` (for `source = "luh2"`), `landuse` (the
  `faostat-landuse` pin, for `source = "faostat_pasture"`) and
  `feed_intake` (for `grassland_metric = "active_grazing"`). Each falls
  back to its reader
  ([`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md),
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md),
  [`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md))
  when absent.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(grassland area in hectares) and `method_grassland` (the chosen metric),
plus the polity columns below.

## Polity columns

Every area-keyed output carries the polity its `area_code` resolves to
in that row's year:

- `polity_area_code`: The numeric key rows are AGGREGATED on, for the
  matrix workflows. It is a bucket, not an identity: use
  `reporting_polity_code` to say which territory a row belongs to.

- `reporting_polity_code`: The polity itself, e.g. `ESP-1846-1914`. It
  is year-aware, so the same `area_code` resolves to different polities
  in different years, which is the point of the crosswalk.

- `reporting_polity_name`: Its name. It can differ from the area's own
  name where the area folds into an aggregate.

- `reporting_polity_has_geometry`: Whether the polity has a polygon in
  the WHEP polity database, for callers that need to map or intersect
  it. `FALSE` is a documented gap upstream, not an error.

Rows whose `area_code` resolves to no polity keep the columns with `NA`
rather than being dropped, so a gap is visible instead of silent.

Rows before the back-cast anchor year resolve to the polity live in that
anchor year rather than to the polity live in the row's own year,
because WHEP's pre-anchor series are back-cast onto the anchor-year
territory. See
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the reasoning.

## Examples

``` r
build_grassland_land_extension(example = TRUE)
#> # A tibble: 4 × 9
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  1986        10               10 AUS-1901-2025         Australia            
#> 2  1986       100              100 IND-1949-2025         India                
#> 3  1987        10               10 AUS-1901-2025         Australia            
#> 4  1987       100              100 IND-1949-2025         India                
#> # ℹ 4 more variables: reporting_polity_has_geometry <lgl>, item_cbs_code <int>,
#> #   impact_u <dbl>, method_grassland <chr>
```
