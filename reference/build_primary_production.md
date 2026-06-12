# Build primary production dataset

Construct the full primary production dataset from raw FAOSTAT inputs.
This is a convenience wrapper that chains the three pipeline steps:

1.  `.read_production()` — read & reformat FAOSTAT data.

2.  `.fix_production()` — apply Global-ported corrections.

3.  `.qc_production()` — flag data-quality anomalies.

## Usage

``` r
build_primary_production(
  start_year = 1850,
  end_year = 2023,
  smooth_carry_forward = FALSE,
  example = FALSE,
  show_duplicates = FALSE,
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
#>  1  1912       165              165 PAK-1800-1947         Pakistan (territory w…
#>  2  2012       112              112 JOR-1946-2025         Jordan                
#>  3  1943        41               41 CHN-1921-1945         China (1921-1945)     
#>  4  1979        45               45 COM-1975-2025         Comoros               
#>  5  1910       141              141 MNG-1911-1921         Bogd Khanate of Mongo…
#>  6  1867        90               90 GIN-1894-1958         Guinea (1894-1958)    
#>  7  1939        15               15 BLX-1850-1999         Belgium-Luxembourg    
#>  8  1935       211              211 CHE-1800-2025         Switzerland           
#>  9  1937         9                9 ARG-1800-2025         Argentina             
#> 10  2000         9                9 ARG-1800-2025         Argentina             
#> # ℹ 7 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_prod_code <chr>, item_cbs_code <dbl>, live_anim_code <chr>,
#> #   unit <chr>, value <dbl>, source <chr>
```
