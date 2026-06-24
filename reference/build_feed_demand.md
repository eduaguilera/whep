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

With `by = "feed_type"`, the demand split across feed types as the
[`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
`feed_demand` contract: `year`, `territory`, `sub_territory`,
`livestock_category`, `item_cbs_code`, `feed_group`, `feed_quality`,
`demand_dm_t`, `fixed_demand`.

## Examples

``` r
build_feed_demand(example = TRUE)
#> # A tibble: 8 × 5
#>    year area_code livestock_category demand_dm_t method_demand     
#>   <int>     <int> <chr>                    <dbl> <chr>             
#> 1  2000        79 Cattle_milk            5800000 ipcc_tier2_energy 
#> 2  2000        79 Cattle_meat            9400000 ipcc_tier2_energy 
#> 3  2000        79 Sheep                  1100000 ipcc_tier2_energy 
#> 4  2000        79 Goats                   200000 ipcc_tier2_energy 
#> 5  2000        79 Pigs                   8700000 bouwman_fcr       
#> 6  2000        79 Poultry                3900000 bouwman_fcr       
#> 7  2000        79 Horses                  150000 krausmann_per_head
#> 8  2000        79 Other                    30000 krausmann_per_head
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
