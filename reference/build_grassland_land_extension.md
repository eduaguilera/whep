# Build the native grassland land extension.

Produce a grassland land extension keyed by
`(year, area_code, item_cbs_code)`, replacing the grassland rows that
used to come from the external `land_fp` pin.

Two area sources are available, selected with `source`:

- `"luh2"` (default): permanent grassland area (item_cbs 3000, LUH2
  pasture and rangeland) taken from
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
(grassland area in hectares) and `method_grassland` (the chosen metric).

## Examples

``` r
build_grassland_land_extension(example = TRUE)
#> # A tibble: 4 × 5
#>    year area_code item_cbs_code impact_u method_grassland
#>   <int>     <int>         <int>    <dbl> <chr>           
#> 1  1986        10          3000 25000000 occupation      
#> 2  1986       100          3000  8000000 occupation      
#> 3  1987        10          3000 25000000 occupation      
#> 4  1987       100          3000  8100000 occupation      
```
