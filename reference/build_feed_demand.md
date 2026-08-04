# Build livestock feed demand.

Estimate the dry-matter feed demand of each livestock category: the
first stage of
[`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md),
exposed on its own. Demand is national, per
`(year, area_code, livestock_category)`, and is computed before any
matching against feed supply, so it can be audited or reused (for
example in land or nitrogen footprints) independently of the allocation.

## Usage

``` r
build_feed_demand(
  demand_tier = c("ipcc", "fcr"),
  by = c("category", "feed_type"),
  example = FALSE
)
```

## Arguments

- demand_tier:

  Demand-estimation tier. `"ipcc"` (default) uses the IPCC Tier-2 energy
  model for the ruminant species, Bouwman feed-conversion ratios for
  pigs and poultry, and Krausmann per-head intake for draft and other
  species. `"fcr"` uses the Bouwman / Krausmann magnitude for every
  species. The method actually used for each row is recorded in
  `method_demand`.

- by:

  Output grain. `"category"` (default) returns the per-livestock
  category demand. `"feed_type"` splits it across feed types and returns
  the `feed_demand` table that
  [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
  consumes, so the two compose:
  `build_feed_demand(by = "feed_type") |> redistribute_feed(feed_avail)`.

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

With `by = "category"`, a tibble with one row per
`(year, area_code, livestock_category)`:

- `year`: The year of the demand.

- `area_code`: The country code. For code details see e.g.
  [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md).

- `livestock_category`: The feed-demand grouping of livestock (e.g.
  `Cattle_milk`, `Cattle_meat`, `Pigs`, `Poultry`).

- `demand_dm_t`: Dry-matter feed demand in tonnes.

- `method_demand`: The demand method(s) used, e.g. `ipcc_tier2_energy`,
  `bouwman_fcr` or `krausmann_per_head` (a `+`-joined set for a mixed
  category whose animals used different methods).

plus the polity columns below.

With `by = "feed_type"`, the demand split across feed types as the
[`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
`feed_demand` contract: `year`, `territory`, `sub_territory`,
`livestock_category`, `item_cbs_code`, `feed_group`, `feed_quality`,
`demand_dm_t`, `fixed_demand`. That grain is keyed by `territory`, not
`area_code`, so it carries no polity columns.

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
for the reasoning.

## Examples

``` r
build_feed_demand(example = TRUE)
#> # A tibble: 8 × 9
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000        79               79 DEU-1990-2025         Germany              
#> 2  2000        79               79 DEU-1990-2025         Germany              
#> 3  2000        79               79 DEU-1990-2025         Germany              
#> 4  2000        79               79 DEU-1990-2025         Germany              
#> 5  2000        79               79 DEU-1990-2025         Germany              
#> 6  2000        79               79 DEU-1990-2025         Germany              
#> 7  2000        79               79 DEU-1990-2025         Germany              
#> 8  2000        79               79 DEU-1990-2025         Germany              
#> # ℹ 4 more variables: reporting_polity_has_geometry <lgl>,
#> #   livestock_category <chr>, demand_dm_t <dbl>, method_demand <chr>
build_feed_demand(example = TRUE, by = "feed_type")
#> # A tibble: 5 × 9
#>    year territory sub_territory livestock_category item_cbs_code feed_group
#>   <int> <chr>     <chr>         <chr>                      <int> <chr>     
#> 1  2000 79        NA            Cattle_milk                   NA NA        
#> 2  2000 79        NA            Cattle_milk                   NA NA        
#> 3  2000 79        NA            Cattle_milk                   NA NA        
#> 4  2000 79        NA            Pigs                          NA NA        
#> 5  2000 79        NA            Pigs                          NA NA        
#> # ℹ 3 more variables: feed_quality <chr>, demand_dm_t <dbl>, fixed_demand <lgl>
```
