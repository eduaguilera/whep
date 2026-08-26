# Build primary production dataset

Construct the full primary production dataset from raw FAOSTAT inputs.
This is a convenience wrapper that chains the pipeline steps:

1.  `.read_production()` — read & reformat FAOSTAT data.

2.  `.fix_production()` — apply Global-ported corrections.

3.  `.dedup_production()` — keep one value per key across sources.

4.  `.qc_production()` — flag data-quality anomalies on the surviving
    (deduplicated) values.

## Usage

``` r
build_primary_production(
  start_year = 1850,
  end_year = 2023,
  smooth_carry_forward = FALSE,
  example = FALSE,
  show_duplicates = FALSE,
  historical_data = NULL,
  federation_land = c("none", "successor_union"),
  land_method = c("present_day", "historical_polity"),
  .raw_data = NULL
)
```

## Arguments

- start_year:

  Integer. First year to include. Default `1850`.

- end_year:

  Integer. Last year to include. Default `2023`.

- smooth_carry_forward:

  Logical. If `TRUE`, carry-forward tails are replaced with a linear
  trend. Default `FALSE`.

- example:

  Logical. If `TRUE`, return a small hardcoded example tibble instead of
  reading remote data. Default `FALSE`.

- show_duplicates:

  Logical. If `TRUE`, return only the rows that have competing sources
  in wide format (one column per source) for diagnostic comparison.
  Default `FALSE`.

- historical_data:

  Optional harmonized historical production rows to add before the LUH2
  historical extension. May be a data frame or a path to a parquet/csv
  file. Required semantic columns are `year`, `item_prod_code`, `unit`,
  `value`, and one of `area_code` or `polity_area_code`. Names such as
  `item_prod_name`, `item_cbs_name`, and `source` are used when present;
  WHEP item and area tables fill canonical names where possible.
  Observed historical rows are retained, and LUH2 proxy filling can use
  them as anchors. **Rice supplied here is assumed to be on a paddy
  (rough-rice) basis** and is multiplied by the paddy-to-milled
  extraction rate, because WHEP's rice item is milled equivalent
  throughout; pre-divide by that rate if the series is already milled.
  Default `NULL`.

- federation_land:

  Character. How the pre-1962 LUH2 back-cast reaches an area whose
  territory is a dissolved federation. LUH2 land use is keyed on
  present-day ISO3, so 15 Belgium-Luxembourg, 51 Czechoslovakia, 228
  USSR and 248 Yugoslav SFR have no land record of their own.

  - `"none"` (default, current published behaviour) leaves them
    unmatched; their pre-1962 production is not back-cast at all and the
    build warns.

  - `"successor_union"` rebuilds each federation's land series as the
    sum of its successor states' LUH2 land, resolved from the
    `successor` relation published in
    [polities](https://eduaguilera.github.io/whep/reference/polities.md).
    This back-casts 14.3% more of the 1961-62 production tonnage and
    therefore moves published pre-1962 values.

- land_method:

  Character. Which borders the pre-1962 `ha` half of
  `tonnes = ha * t_ha` is measured on. The yield half is historical
  either way and is untouched by this argument.

  - `"present_day"` (default, current published behaviour) reads the
    `luh2-areas` pin, which is LUH2 land pre-aggregated to present-day
    ISO3, so a row labelled with the 1961 entity is measured on the
    borders that entity has today.

  - `"historical_polity"` measures it with
    [`build_historical_land_areas()`](https://eduaguilera.github.io/whep/reference/build_historical_land_areas.md):
    gridded LUH2 summed inside the polygon of the polity `area_code`
    resolved to in that year. It moves published pre-1962 values, needs
    `sf` and `terra`, and reads gridded LUH2 for every back-cast year,
    so it is minutes of extra work.

- .raw_data:

  Optional tibble with the same structure as the output of the internal
  `.read_production()` step. When supplied, the remote-data read is
  skipped entirely and the pipeline starts from `.fix_production()`.
  Columns required: `year`, `area`, `area_code`, `item_prod`,
  `item_prod_code`, `item_cbs`, `item_cbs_code`, `live_anim`,
  `live_anim_code`, `unit`, `value`, `source`. Default `NULL`.

## Value

A tibble with the same columns as
[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md):
`year`, legacy numeric `area_code`, numeric `polity_area_code`,
`reporting_polity_code`, `reporting_polity_name`,
`reporting_polity_has_geometry`, `item_prod_code`, `item_cbs_code`,
`live_anim_code`, `unit`, `value`, and `source`. Item names can be
recovered via
[`add_item_prod_name()`](https://eduaguilera.github.io/whep/reference/add_item_prod_name.md)
and related helpers. When `show_duplicates = TRUE`, returns a wide
tibble with one column per source showing the competing values.

## Examples

``` r
build_primary_production(example = TRUE)
#> # A tibble: 10 × 12
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                
#>  1  1912       165              165 PAK-1949-1971         Pakistan (1949-1971) 
#>  2  2012       112              112 JOR-1946-2025         Jordan               
#>  3  1943        41               41 CHN-1950-2025         China (PRC)          
#>  4  1979        45               45 COM-1975-2025         Comoros              
#>  5  1910       141              141 MNG-1921-2025         Mongolia             
#>  6  1867        90               90 GIN-1958-2025         Guinea               
#>  7  1939        15               15 BLX-1850-1999         Belgium-Luxembourg   
#>  8  1935       211              211 CHE-1800-2025         Switzerland          
#>  9  1937         9                9 ARG-1902-2025         Argentina            
#> 10  2000         9                9 ARG-1902-2025         Argentina            
#> # ℹ 7 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_prod_code <chr>, item_cbs_code <dbl>, live_anim_code <chr>,
#> #   unit <chr>, value <dbl>, source <chr>
```
