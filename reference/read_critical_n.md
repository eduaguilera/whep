# Read a Schulte-Uebbing gridded critical-nitrogen layer.

Reads one 0.5-degree gridded critical-nitrogen layer from the
Schulte-Uebbing et al. (2022) archive (doi:10.5281/zenodo.6395016) onto
WHEP's grid: the critical nitrogen surplus, the critical nitrogen input,
the exceedance of the critical surplus, the three medium-specific
critical losses (ammonia emission, groundwater leaching, surface-water
load), or the pre-computed binding-threshold map. Values are in kg N per
hectare per year (a categorical 1-8 impact code for
`binding_threshold`). The critical surplus, input and exceedance are
selectable by `threshold` (minimum of all media, surface water,
groundwater or deposition) and `land_use` (all agricultural land, arable
only, or intensively managed grassland); the three critical losses and
the binding threshold ignore `threshold`. The archive directory comes
from `dir`, else the `WHEP_CRITICAL_N_DIR` environment variable, else a
local cache that is populated by downloading the archive from Zenodo on
first use (see `dir`).

## Usage

``` r
read_critical_n(
  var = c("critical_n_surplus", "critical_n_input", "exceedance", "crit_nh3_emission",
    "crit_leaching_gw", "crit_load_sw", "binding_threshold"),
  threshold = c("mi", "sw", "gw", "de"),
  land_use = c("all", "ara", "igl"),
  dir = NULL,
  data = NULL,
  example = FALSE,
  verify_source = TRUE
)
```

## Arguments

- var:

  Which critical-nitrogen layer to read: one of `"critical_n_surplus"`,
  `"critical_n_input"`, `"exceedance"`, `"crit_nh3_emission"`,
  `"crit_leaching_gw"`, `"crit_load_sw"` or `"binding_threshold"`.

- threshold:

  Impact threshold selecting the critical value: `"mi"` (minimum across
  media, the collapsed boundary), `"sw"` (surface-water eutrophication),
  `"gw"` (groundwater nitrate) or `"de"` (atmospheric or terrestrial
  deposition). Ignored by the critical-loss and binding-threshold
  layers.

- land_use:

  Land-use scope: `"all"` (arable plus intensively managed grassland),
  `"ara"` (arable only) or `"igl"` (intensively managed grassland).
  Ignored by the critical-loss layers (`crit_nh3_emission`,
  `crit_leaching_gw`, `crit_load_sw`), which have a single
  land-use-agnostic file; used by the binding threshold and the
  surplus/input/exceedance layers.

- dir:

  Optional path to the archive directory, overriding
  `WHEP_CRITICAL_N_DIR`. Defaults to `NULL`, in which case the archive
  is resolved as: `dir`, then `WHEP_CRITICAL_N_DIR`, then a local cache
  under `rappdirs::user_cache_dir("whep")`. When the cache is empty the
  18.4 MB CC-BY-4.0 Zenodo archive is downloaded, verified against its
  published MD5 and unpacked there on first use, so a plain
  `read_critical_n()` call works with nothing configured. Unpacking
  needs a 7-Zip extractor: the `archive` package (system libarchive) or
  a `7z` binary on `PATH`. With neither, the call aborts after the
  download, naming the command to run.

- data:

  Optional pre-read tibble (`lon`, `lat`, `value`) returned directly
  instead of reading the archive, for tests and injection. Defaults to
  `NULL`.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

- verify_source:

  If `TRUE` (default), real archive reads verify the selected critical
  raster and its source-area/IMAGE support rasters against the package's
  versioned content manifest before parsing. Ignored for `data` and
  `example` injection.

## Value

A tibble with `lon`, `lat` (0.5-degree cell centres), `value` (kg N per
hectare per year; a categorical impact code for `binding_threshold`) and
retained layer provenance: `critical_var`, `critical_threshold`,
`critical_land_use`, `critical_year` and `critical_source`, canonical
integer `cell_id`/row/column keys, deposited `source_area_ha`,
IMAGE-region membership, DOI/version and archive checksum. NODATA cells
are dropped.

## Examples

``` r
read_critical_n(example = TRUE)
#> # A tibble: 6 × 16
#>     lon   lat value critical_var       critical_threshold critical_land_use
#>   <dbl> <dbl> <dbl> <chr>              <chr>              <chr>            
#> 1 -0.75  51.8     9 critical_n_surplus mi                 all              
#> 2 -0.25  51.8    84 critical_n_surplus mi                 all              
#> 3  0.25  51.8    12 critical_n_surplus mi                 all              
#> 4 -0.75  51.2   120 critical_n_surplus mi                 all              
#> 5 -0.25  51.2    47 critical_n_surplus mi                 all              
#> 6  0.25  51.2    63 critical_n_surplus mi                 all              
#> # ℹ 10 more variables: critical_year <int>, critical_source <chr>,
#> #   cell_id <int>, source_row <int>, source_col <int>, source_area_ha <dbl>,
#> #   image_region <int>, critical_source_doi <chr>,
#> #   critical_source_version <chr>, archive_md5 <chr>
```
