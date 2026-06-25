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

## Examples

``` r
build_feed_intake_local(example = TRUE)
#> # A tibble: 5 × 11
#>    year area_code sub_territory live_anim_code item_cbs_code feed_type  supply
#>   <int>     <int> <chr>                  <int>         <int> <chr>       <dbl>
#> 1  2000       724 -3.75_40.25              960          3000 grass        1250
#> 2  2000       724 -3.75_40.25              960          2591 crops          11
#> 3  2000       724 -3.25_40.25              961          3000 grass         900
#> 4  2000       724 -3.25_40.25              976          3500 scavenging      0
#> 5  2000       724 -3.25_40.75             1049          2591 crops          22
#> # ℹ 4 more variables: intake <dbl>, intake_dry_matter <dbl>, loss <dbl>,
#> #   loss_share <dbl>
```
