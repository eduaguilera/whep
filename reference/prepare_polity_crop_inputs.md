# Prepare crop inputs on source reporting polities

Resolve original source observations before geographic aggregation, then
prepare physical crop inputs for polity-native allocation. Source codes
are provenance; all territorial matching uses the resolved polity code.
This does not relabel the legacy prepared country-area pin, redistribute
predecessor observations, or reconstruct historical crop composition.

## Usage

``` r
prepare_polity_crop_inputs(
  data,
  method = c("mirca", "luh2"),
  output_dir = NULL,
  source_format = c("source_records", "faostat")
)
```

## Arguments

- data:

  Named list containing `production`, `mapping`, `support`, `luh2`,
  `type_mapping`, `crop_patterns` and `input_revisions`. Production
  requires `source`, character `source_area_code`, integer `year`,
  `item_prod_code`, `harvested_area_ha` and `territorial_basis` equal to
  `"source_reported"`. This basis describes the source territory, not
  whether its numeric value was measured or estimated; retain source
  observation flags as additional columns. Already aggregated or
  backcast production must not be presented as original source
  observations. Optional numeric `area_code` and `polity_area_code`
  metadata are retained as `input_area_code` and
  `input_polity_area_code` in prepared outputs; they cannot become
  geography keys. The source-specific mapping requires `source`,
  `source_area_code`, `start_year`, `end_year` (exclusive) and
  `polity_code`. Its territorial correctness must be established by the
  source adapter; a unique match alone is not documentary evidence.
  Support is a complete partition from
  [`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md).
  LUH2 cells require `year`, `lon`, `lat`, `luh2_type`, `crop_ha` and
  `irrig_ha`, including supplied zeros for every required type in every
  cell. The type mapping requires `item_prod_code` and `luh2_type`. Crop
  patterns have the
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  schema. `input_revisions` supplies nonempty revision identifiers for
  `production`, `mapping`, `support`, `luh2`, `crop_patterns` and, when
  used, `mirca`. MIRCA requires physical `lon`, `lat`, `item_prod_code`,
  `irrig_ha` and `rainfed_ha`, before any polity join. Patterns are
  applied to each year's reporting footprint; this is a spatial-pattern
  estimate, not an annual MIRCA observation.

- method:

  Irrigation estimation method. `"mirca"` uses the ratio of weighted
  MIRCA irrigated to total hectares per polity and crop, then scales the
  crop targets to the polity's LUH2 irrigated area. `"luh2"` distributes
  each type's LUH2 irrigation among crops in proportion to their
  harvested areas. Both retain the legacy per-crop harvested-area cap;
  capping can leave physical irrigation unallocated. Missing inputs
  never trigger a switch of method. The chosen method is recorded.

- output_dir:

  Optional directory in which to write native parquet inputs and their
  checksum manifest. Default `NULL` returns the inputs without writing
  them. Files use distinct native country-area/grid names.

- source_format:

  Either `"source_records"` for the schema above, or `"faostat"` to
  extract harvested hectares from an unaggregated FAOSTAT production
  table. The latter requires `Area Code`, `Year`, `Item Code`,
  `Element`, `Unit`, `Value` and `Flag`; only `Area harvested` records
  in `ha` for the supplied crop-type mapping are selected. No geographic
  rows are silently excluded: restrict the raw input to the intended
  reporting scope explicitly, and unmapped territories fail. Supply the
  authoritative whep-polities FAOSTAT map with `area_code`,
  `year_start`, `year_end` (inclusive) and `polity_code`. The adapter
  converts inclusive endpoints to the native exclusive contract and
  retains FAOSTAT observation flags.

## Value

A named list of prepared tables and a provenance manifest.

## Examples

``` r
# A synthetic source observation with a supplied physical irrigation zero.
inputs <- list(
  production = tibble::tibble(
    source = "example", source_area_code = "region", year = 1900L,
    item_prod_code = 15L, harvested_area_ha = 30,
    territorial_basis = "source_reported"
  ),
  mapping = tibble::tibble(
    source = "example", source_area_code = "region",
    start_year = 1800L, end_year = 1950L, polity_code = "AAA-1800-1950"
  ),
  support = tibble::tibble(
    lon = 0.25, lat = 0.25, polity_code = "AAA-1800-1950",
    start_year = 1800L, end_year = 1950L,
    cell_area_ha = 100, land_area_ha = 100
  ),
  luh2 = tibble::tibble(
    year = 1900L, lon = 0.25, lat = 0.25,
    luh2_type = "c3ann", crop_ha = 100, irrig_ha = 0
  ),
  type_mapping = tibble::tibble(item_prod_code = 15L, luh2_type = "c3ann"),
  crop_patterns = tibble::tibble(
    lon = 0.25, lat = 0.25, item_prod_code = 15L, harvest_fraction = 1
  ),
  input_revisions = list(
    production = "synthetic", mapping = "synthetic", support = "synthetic",
    luh2 = "synthetic", crop_patterns = "synthetic"
  )
)
prepare_polity_crop_inputs(inputs, method = "luh2")
#> $country_areas
#> # A tibble: 1 × 12
#>   source  source_area_code  year item_prod_code harvested_area_ha
#>   <chr>   <chr>            <int>          <int>             <dbl>
#> 1 example region            1900             15                30
#> # ℹ 7 more variables: territorial_basis <chr>, start_year <int>,
#> #   end_year <int>, polity_code <chr>, luh2_type <chr>,
#> #   irrigated_area_ha <dbl>, method_irrigation <chr>
#> 
#> $country_grid
#> # A tibble: 1 × 8
#>     lon   lat polity_code   start_year end_year cell_area_ha land_area_ha
#>   <dbl> <dbl> <chr>              <int>    <int>        <dbl>        <dbl>
#> 1  0.25  0.25 AAA-1800-1950       1800     1950          100          100
#> # ℹ 1 more variable: cell_area_frac <dbl>
#> 
#> $crop_patterns
#> # A tibble: 1 × 4
#>     lon   lat item_prod_code harvest_fraction
#>   <dbl> <dbl>          <int>            <dbl>
#> 1  0.25  0.25             15                1
#> 
#> $gridded_cropland
#> # A tibble: 1 × 5
#>    year   lon   lat cropland_ha irrigated_ha
#>   <int> <dbl> <dbl>       <dbl>        <dbl>
#> 1  1900  0.25  0.25         100            0
#> 
#> $type_cropland
#> # A tibble: 1 × 6
#>    year   lon   lat luh2_type type_ha type_irrig_ha
#>   <int> <dbl> <dbl> <chr>       <dbl>         <dbl>
#> 1  1900  0.25  0.25 c3ann         100             0
#> 
#> $manifest
#> $manifest$schema_version
#> [1] 1
#> 
#> $manifest$geographic_key
#> [1] "polity"
#> 
#> $manifest$territorial_basis
#> [1] "source_reported"
#> 
#> $manifest$method_irrigation
#> [1] "luh2"
#> 
#> $manifest$input_revisions
#> $manifest$input_revisions$production
#> [1] "synthetic"
#> 
#> $manifest$input_revisions$mapping
#> [1] "synthetic"
#> 
#> $manifest$input_revisions$support
#> [1] "synthetic"
#> 
#> $manifest$input_revisions$luh2
#> [1] "synthetic"
#> 
#> $manifest$input_revisions$crop_patterns
#> [1] "synthetic"
#> 
#> 
#> $manifest$source_format
#> [1] "source_records"
#> 
#> 
```
