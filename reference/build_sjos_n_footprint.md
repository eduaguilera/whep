# Build the embodied-nitrogen trade footprint.

Trace a per-crop nitrogen category through the FABIO footprint framework
and return the consumption-side embodied nitrogen split into domestic
and traded flows. The `category` selects which nitrogen mass is carried
as the footprint intensity (see
[`build_n_exceedance_extension()`](https://eduaguilera.github.io/whep/reference/build_n_exceedance_extension.md)):
`"exceedance"` (default), `"within_boundary"`, or `"production"` (the
crop's harvest removal: harvested product plus used residue plus grazed
forage).

The pipeline is
[`build_n_exceedance_extension()`](https://eduaguilera.github.io/whep/reference/build_n_exceedance_extension.md)
to assemble the extension,
[`build_footprint()`](https://eduaguilera.github.io/whep/reference/build_footprint.md)
to trace it through the multi-regional input-output model, then a
domestic-versus-traded relabelling of the tidy flows: a flow is
`"Domestic consumption"` when the nitrogen is emitted and consumed in
the same area (`origin_area == target_area`) and `"Traded"` otherwise.
Two outputs are returned, mirroring Global's `FP_all_N` and
`FP_food_all_N`: `fp_all` is the embodied nitrogen across all
final-demand categories, `fp_food` is the subset consumed as food
(`target_fd == "food"`). Signed crop attributions are traced as separate
positive and negative linear extensions and recombined. Explicit
undefined-attribution residuals are rejected by
[`build_n_exceedance_extension()`](https://eduaguilera.github.io/whep/reference/build_n_exceedance_extension.md)
before tracing.

## Usage

``` r
build_sjos_n_footprint(
  exceedance = NULL,
  io = NULL,
  category = c("exceedance", "within_boundary", "production"),
  years = NULL,
  data = list(),
  example = FALSE
)
```

## Arguments

- exceedance:

  A
  [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md)
  country-resolution output passed straight to
  [`build_n_exceedance_extension()`](https://eduaguilera.github.io/whep/reference/build_n_exceedance_extension.md).
  Not needed when `example = TRUE`.

- io:

  Optional pre-built
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
  result reused across extensions. When `NULL` (default),
  [`build_footprint()`](https://eduaguilera.github.io/whep/reference/build_footprint.md)
  builds it for `years` from the package inputs (the real-data path, an
  integration wiring step).

- category:

  Which per-crop nitrogen mass to trace: `"exceedance"` (default),
  `"within_boundary"`, or `"production"`. Validated with
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html).

- years:

  Years to trace. Defaults to the years present in the extension;
  ignored when `io` is supplied.

- data:

  Optional named list of injected inputs. `data$fp_flows` supplies
  pre-traced tidy footprint flows (as from
  [`build_footprint()`](https://eduaguilera.github.io/whep/reference/build_footprint.md))
  directly, bypassing the model build, for testing the split logic in
  isolation. `data$origin_classes` may supply producer classifications
  keyed by `year`, `area_code`, `item_cbs_code` (for example
  [`classify_sjos_n()`](https://eduaguilera.github.io/whep/reference/classify_sjos_n.md)
  output). `data$target_classes` may supply consumer classifications,
  one row per country-year keyed by `year` and `area_code`, carrying
  `nourish` (for example
  [`normalize_nourishment()`](https://eduaguilera.github.io/whep/reference/normalize_nourishment.md)
  output) and optionally a country-year `boundary_side` and
  `sjos_class`. They join on `target_area` and `year` as
  `target_nourish`, `target_boundary_side` and `target_sjos_class`. Its
  `area_code` must be in the same code space as `target_area`, which is
  the IO model's `fd_labels$area_code`: the commodity balances'
  `polity_area_code` bucket (see
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)),
  not the source FAOSTAT area. A consumer bucket with no row in the
  table is reported as unclassified; a table keyed in another code space
  can match the wrong country wherever the two numberings share a code,
  so key it on the bucket. This function classifies nothing: a
  country-year boundary class is the caller's to classify after
  aggregation, and a table with more than one class per country-year (a
  crop-level
  [`classify_sjos_n()`](https://eduaguilera.github.io/whep/reference/classify_sjos_n.md)
  output, for instance) aborts rather than duplicating flows.

- example:

  If `TRUE`, return a small hardcoded fixture instead of running the
  pipeline. Defaults to `FALSE`.

## Value

A named list with two tibbles:

- `fp_all`: embodied nitrogen by `year`, producer `origin_area` /
  `origin_item`, consumer `target_area` / `target_item`, `target_fd`,
  `origin` (`"Domestic consumption"` or `"Traded"`), `item_cbs_code` (an
  alias of `target_item`) and `impact_u` (tonnes N), stamped with the
  traced `category`, optional producer classes and, when
  `data$target_classes` is supplied, the `target_*` consumer classes.

- `fp_food`: `fp_all` restricted to food consumption
  (`target_fd == "food"`).

- `target_class_diag`: only when `data$target_classes` is supplied. One
  row per output table (`table`, `"fp_all"` or `"fp_food"`) and every
  `year` in `fp_all`, zero-filled where `fp_food` has no flows that
  year: the flow and consumer-area counts, `impact_u` (tonnes N), and
  how many flows, consumer areas and tonnes N went to a country-year
  with no `nourish` class (`n_flows_unclassified`,
  `n_target_areas_unclassified`, `impact_u_unclassified`). Those flows
  keep `NA` in `target_nourish` and stay in the tables; a warning of
  class `whep_sjos_fp_unclassified_target` names the consumer areas.

## Examples

``` r
build_sjos_n_footprint(example = TRUE)
#> $fp_all
#> # A tibble: 5 × 10
#>    year origin_area origin_item target_area target_item target_fd  origin       
#>   <int>       <int>       <int>       <int>       <int> <chr>      <chr>        
#> 1  2000           1          10           1          10 food       Domestic con…
#> 2  2000           1          20           1          20 other_uses Domestic con…
#> 3  2000           1          10           2          10 food       Traded       
#> 4  2000           2          10           2          10 food       Domestic con…
#> 5  2000           2          20           2          20 food       Domestic con…
#> # ℹ 3 more variables: impact_u <dbl>, item_cbs_code <int>, category <chr>
#> 
#> $fp_food
#> # A tibble: 4 × 10
#>    year origin_area origin_item target_area target_item target_fd origin        
#>   <int>       <int>       <int>       <int>       <int> <chr>     <chr>         
#> 1  2000           1          10           1          10 food      Domestic cons…
#> 2  2000           1          10           2          10 food      Traded        
#> 3  2000           2          10           2          10 food      Domestic cons…
#> 4  2000           2          20           2          20 food      Domestic cons…
#> # ℹ 3 more variables: impact_u <dbl>, item_cbs_code <int>, category <chr>
#> 
```
