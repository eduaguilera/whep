# Build a hectare-year (land-occupation) crop land extension.

Per-crop land *occupation* in hectare-years (the LCA `m2*year`
convention): the land-time each crop's production ties up,
\\occupation_i = harvested_i \times L_i/12 + fallow_i\\.

The first term is **active growing occupation** — harvested area times
mean cycle length \\L_i\\ (months, from MIRCA2000). Because it uses
harvested area, a field double-cropped twice contributes both cycles,
and a long-cycle perennial contributes close to a full year. The second
term is the **rotational fallow** attributed to the crop, which occupies
land the whole year while it rests.

This is "active" occupation: land is charged only while a crop is
growing on it or resting in its rotation, so the national total falls
below physical cropland area (which also counts off-season idle). It is
**distinct from, and complementary to**, the physical-area extensions
([`build_cropgrids_land_extension()`](https://eduaguilera.github.io/whep/reference/build_cropgrids_land_extension.md)):
those measure the field area each crop holds; this measures how much
land-*time* each crop's activity occupies. Short single-cropped crops
and intensively double-cropped staples occupy less land-time per hectare
than long-cycle and perennial crops.

## Usage

``` r
build_hayr_land_extension(
  harvested = NULL,
  fallow = NULL,
  season = NULL,
  base = c("cropgrids_fallow", "cropgrids")
)
```

## Arguments

- harvested:

  Tibble of harvested area with columns `year`, `area_code`,
  `item_cbs_code`, `harvested_ha`. If `NULL`, built from
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  (and reused to build the fallow term).

- fallow:

  Tibble of attributed rotational fallow with columns `year`,
  `area_code`, `item_cbs_code`, `fallow_ha`. If `NULL`: for
  `base = "cropgrids_fallow"` it is the difference between the
  fallow-inclusive and cropped CROPGRIDS physical extensions; for
  `base = "cropgrids"` it is zero (growing occupation only).

- season:

  Tibble of mean crop cycle length with columns `item_cbs_code`,
  `season_months` (strictly positive, unique keys). If `NULL`, the
  packaged MIRCA2000 season table is used. Crops with no season are
  given the median cycle length.

- base:

  `"cropgrids_fallow"` (default, include rotational fallow) or
  `"cropgrids"` (growing occupation only). Recorded in `method_land` as
  `<base>_hayr`.

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(land occupation in hectare-years), and `method_land`.

## Examples

``` r
harvested <- tibble::tribble(
  ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
  2000L, 1L, 2807L, 200, # rice, double-cropped (two harvests per field)
  2000L, 1L, 2511L, 100 # wheat, single-cropped
)
season <- tibble::tribble(
  ~item_cbs_code, ~season_months,
  2807L, 5,
  2511L, 8
)
fallow <- tibble::tribble(
  ~year, ~area_code, ~item_cbs_code, ~fallow_ha,
  2000L, 1L, 2511L, 20
)
build_hayr_land_extension(harvested, fallow, season)
#> # A tibble: 2 × 5
#>    year area_code item_cbs_code impact_u method_land          
#>   <int>     <int>         <int>    <dbl> <chr>                
#> 1  2000         1          2807     83.3 cropgrids_fallow_hayr
#> 2  2000         1          2511     86.7 cropgrids_fallow_hayr
```
