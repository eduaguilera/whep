# Build local (per-cell) feed intake, chunked by year.

Runs the `redistribute_feed` local path (0.5-degree cell grain) one year
at a time, so the per-cell allocation stays within memory and the full
multi-year run is restartable. By default it sources pinned
LPJmL-derived grass availability and pinned gridded livestock inputs.
Pass `run_dir`, `grass_availability`, `grass_availability_path`, or
`input_dir` to use custom local inputs instead.

## Usage

``` r
build_feed_intake_local(
  years = NULL,
  out_dir = NULL,
  demand_tier = c("ipcc", "fcr"),
  feed_mode = c("historical", "scenario"),
  overwrite = FALSE,
  example = FALSE,
  run_dir = NULL,
  input_dir = NULL,
  grass_availability = NULL,
  grass_availability_path = NULL
)
```

## Arguments

- years:

  Integer vector of years to build. Default `NULL` builds every year
  present in the production data.

- out_dir:

  Directory to write per-year `feed_intake_local_<year>` parquet files
  to. If `NULL`, the bound result is returned in memory (only practical
  for a few years).

- demand_tier:

  Demand-estimation tier, `"ipcc"` (default) or `"fcr"`.

- feed_mode:

  Whether to distribute surplus feed availability. `"historical"`
  (default) suppresses the surplus-distribution pass: the CBS feed
  element is treated as realised consumption, so leftover availability
  is not dumped onto variable-demand livestock (which would inflate
  non-grass intake). `"scenario"` distributes the surplus.

- overwrite:

  Re-run years whose output file already exists. Default `FALSE` skips
  them so the batch is restartable.

- example:

  If `TRUE`, return a small example output without sourcing the remote
  and gridded data. Default is `FALSE`.

- run_dir:

  Optional path to a finished local LPJmL output directory holding
  `pft_npp.nc` and `cftfrac.nc`. If `NULL`, pinned grass availability is
  used unless `grass_availability` or `grass_availability_path` is
  supplied.

- input_dir:

  Optional directory holding locally prepared spatialization inputs. If
  `NULL`, pinned gridded livestock/spatial inputs are used.

- grass_availability:

  Optional already-derived grass availability tibble/data frame passed
  to
  [`build_grass_availability_lpjml()`](https://eduaguilera.github.io/whep/reference/build_grass_availability_lpjml.md).

- grass_availability_path:

  Optional path to an already-derived grass availability artifact passed
  to
  [`build_grass_availability_lpjml()`](https://eduaguilera.github.io/whep/reference/build_grass_availability_lpjml.md).

## Value

When `out_dir` is `NULL`, a tibble in the
[`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md)
contract plus a `sub_territory` (0.5-degree cell) column. Otherwise,
invisibly, the written file paths.

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
for the reasoning. Where that polity is not live in the row's own year –
41.5% of the pre-1961 `(area, year)` cells –
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
says so as `mapping_status == "backcast_anchor"`, and
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports it as `gap_kind == "backcast_anchor"`. These columns do not say
so either way.

A row whose year no mapped period covers is resolved to the NEAREST
period of the same area instead, so `reporting_polity_code` can name a
polity that did not exist in that row's year – FAOSTAT bucket 206 "Sudan
(former)" keeps reporting after `SUD-1956-2011` ends, and its post-2011
rows carry that code. These columns do not say so:
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
reports such a row as `mapping_status == "out_of_span"`, and that column
is dropped here so that adding it does not change the schema of every
area-keyed output at once.
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports the stand-in rows of a built table, and
`options(whep.polity_mapping_status = "flag")` (or `"status"`) carries
the signal on the outputs themselves. Both are opt-in; the default is no
extra column.

## Examples

``` r
build_feed_intake_local(example = TRUE)
#> # A tibble: 5 × 15
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000       203              203 ESP-1800-2025         Spain                
#> 2  2000       203              203 ESP-1800-2025         Spain                
#> 3  2000       203              203 ESP-1800-2025         Spain                
#> 4  2000       203              203 ESP-1800-2025         Spain                
#> 5  2000       203              203 ESP-1800-2025         Spain                
#> # ℹ 10 more variables: reporting_polity_has_geometry <lgl>,
#> #   sub_territory <chr>, live_anim_code <int>, item_cbs_code <int>,
#> #   feed_type <chr>, supply <dbl>, intake <dbl>, intake_dry_matter <dbl>,
#> #   loss <dbl>, loss_share <dbl>
```
