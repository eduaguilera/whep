# Read United States state or county agricultural statistics from NASS

Reads one USDA NASS Quick Stats bulk dump from disk and returns the
requested series as source-native admin-statistics rows: one row per
reporting unit, item, indicator and year, with values converted to WHEP
units and every non-numeric NASS value code preserved.

The dump is streamed a chunk of lines at a time and filtered chunk by
chunk, so the ~8 GB flat file is never materialised. Only the fifteen
columns this reader uses are parsed, and a gzipped dump is read without
being extracted first.

Identifiers stay exactly as NASS published them: no item is mapped to a
WHEP `item_prod_code` and no unit is resolved to a polity. Both happen
downstream, from the source-native keys returned here.

## Usage

``` r
read_admin_stats_nass(
  domain = c("crops", "animals"),
  nass_dir = NULL,
  short_desc = NULL,
  reference_period = NULL,
  agg_level = c("STATE", "COUNTY"),
  example = FALSE
)
```

## Source

USDA National Agricultural Statistics Service, Quick Stats bulk
downloads (<https://www.nass.usda.gov/datasets/>), files
`qs.crops_YYYYMMDD.txt.gz` and `qs.animals_products_YYYYMMDD.txt.gz`.
Licence CC0 1.0 Universal. Verified 2026-09-02.

## Arguments

- domain:

  Which NASS dump to read, `"crops"` or `"animals"`.

- nass_dir:

  Directory holding the Quick Stats dumps, as
  `inst/scripts/download/download_nass.R` fills it. Defaults to
  `Sys.getenv("WHEP_NASS_DIR")`.

- short_desc:

  Character vector of NASS `SHORT_DESC` series to keep. `NULL` uses the
  documented default for `domain`.

- reference_period:

  Character vector of `REFERENCE_PERIOD_DESC` values to keep. `NULL`
  uses `"YEAR"` for crops and `"FIRST OF JAN"` for animals.

- agg_level:

  Aggregation level to return, `"STATE"` (grain `"admin1"`) or
  `"COUNTY"` (grain `"admin2"`). County rows are for within-unit
  validation, which the state constraint leaves untouched.

- example:

  If `TRUE`, return a small fixture instead of reading a dump. Defaults
  to `FALSE`.

## Value

A tibble with one row per unit, item, indicator and year:

- `source`: always `"USDA_NASS"`.

- `source_native_unit_id`, `source_native_unit_name`: the state FIPS
  code and `STATE_NAME` verbatim at `"STATE"` grain; the five-digit
  state plus county FIPS code and `"<state>, <county>"` at `"COUNTY"`
  grain. FIPS `"98"` is NASS's own residual unit, `"OTHER STATES"`, an
  observed value covering the states too few to publish separately.

- `source_native_item_code`: always `NA`; the dumps carry no item code.

- `source_native_item_name`: `SHORT_DESC` verbatim, the string that keys
  the series.

- `indicator_used`: `"area_harvested"`, `"area_planted_or_sown"`,
  `"production"` or `"yield"` for crop rows, `NA` for head counts.

- `quantity`: `"area"`, `"production"`, `"yield"` or `"heads"`.

- `year`: calendar year.

- `value`: the published value in WHEP units, `NA` where NASS published
  a code instead of a number.

- `value_unit`: `"ha"`, `"tonnes"` or `"heads"`.

- `value_flag`: `"residual"` on an OTHER STATES row, the NASS value code
  verbatim (`"(D)"`, `"(S)"`, `"(Z)"`, `"(NA)"`, `"(X)"`) on a row with
  no published number, both joined by `"; "` where both apply, and `NA`
  on a clean row.

- `grain`: `"admin1"` for state rows, `"admin2"` for county rows.

- `nuts_version`: always `NA`; the United States has no NUTS geography.

- `source_version`: the dump's own date stamp, `"YYYYMMDD"`, taken from
  its file name.

- `recorded_at`: ISO 8601 UTC time the dump was read.

## Series selection

NASS keys a series on `SHORT_DESC`, which folds the commodity, its
class, the production practice and the statistic into one string
(`"CORN, GRAIN - ACRES HARVESTED"`). `short_desc` is therefore the item
selector, and its default covers the field crops and livestock species
the subnational spatialization needs. The authoritative item-to-series
table is built separately; pass `short_desc` to read anything else.

Rows are kept only where `SOURCE_DESC` is `"SURVEY"` (not the
quinquennial census), `DOMAIN_DESC` is `"TOTAL"` (not a breakdown by
farm size, sales or NAICS class), and `PRODN_PRACTICE_DESC` is
`"ALL PRODUCTION PRACTICES"` (not the irrigated or organic split).

## Reference periods

Crop area is annual (`FREQ_DESC == "ANNUAL"`) and its final estimate
carries `REFERENCE_PERIOD_DESC == "YEAR"`; the in-season forecasts
(`"YEAR - AUG FORECAST"` and siblings) are excluded by that default.
Livestock inventories are `FREQ_DESC == "POINT IN TIME"` and the default
reference period is `"FIRST OF JAN"`.

One species does not follow that default: NASS publishes the annual hog
inventory as of **1 December**, so `"HOGS - INVENTORY"` has no
`"FIRST OF JAN"` rows at all and returns nothing under the default. That
is deliberately not patched here – which hog reference period stands for
the annual inventory is a vocabulary decision, not a reading one – but
it is never silent: any requested `short_desc` that matches no row warns
by name, and `reference_period` selects a different one.

## Examples

``` r
read_admin_stats_nass(example = TRUE)
#> # A tibble: 10 × 15
#>    source    source_native_unit_id source_native_unit_n…¹ source_native_item_c…²
#>    <chr>     <chr>                 <chr>                  <chr>                 
#>  1 USDA_NASS 55                    WISCONSIN              NA                    
#>  2 USDA_NASS 04                    ARIZONA                NA                    
#>  3 USDA_NASS 04                    ARIZONA                NA                    
#>  4 USDA_NASS 06                    CALIFORNIA             NA                    
#>  5 USDA_NASS 26                    MICHIGAN               NA                    
#>  6 USDA_NASS 22                    LOUISIANA              NA                    
#>  7 USDA_NASS 05                    ARKANSAS               NA                    
#>  8 USDA_NASS 34                    NEW JERSEY             NA                    
#>  9 USDA_NASS 45                    SOUTH CAROLINA         NA                    
#> 10 USDA_NASS 98                    OTHER STATES           NA                    
#> # ℹ abbreviated names: ¹​source_native_unit_name, ²​source_native_item_code
#> # ℹ 11 more variables: source_native_item_name <chr>, indicator_used <chr>,
#> #   quantity <chr>, year <int>, value <dbl>, value_unit <chr>,
#> #   value_flag <chr>, grain <chr>, nuts_version <chr>, source_version <chr>,
#> #   recorded_at <chr>
```
