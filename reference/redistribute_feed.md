# Redistribute available feed supply among livestock demand.

Matches livestock feed demand to available feed items through a
hierarchical allocation that follows the remaining-share principle to
avoid exceeding availability. The redistribution path adapts to the
`fixed_demand` column in the demand table.

## Usage

``` r
redistribute_feed(feed_demand, feed_avail, options = list())
```

## Arguments

- feed_demand:

  A tibble of feed demand with columns `year`, `territory`,
  `sub_territory`, `livestock_category`, `item_cbs_code`, `feed_group`,
  `feed_quality`, `demand_dm_t`, and a logical `fixed_demand`.

- feed_avail:

  A tibble of feed availability with columns `year`, `sub_territory`,
  `item_cbs_code`, `feed_group`, `feed_quality`, `avail_dm_t`, and
  `feed_scale`.

- options:

  A named list of allocation options. See `.redistribute_feed_options()`
  for the available entries and their defaults. Supply
  `grass_availability` (a tibble with `year`, `territory` or
  `area_code`, and `grass_avail_dm_t`) to bound the otherwise-unlimited
  pasture grass at that supply per polity-year. The grass deficit then
  cascades: pasture grass is capped at the ceiling, the deficit is
  redistributed to leftover non-grass availability in the polity (added
  as `7_grass_deficit_substitute` intake, limited by that leftover), and
  the residual stays as biologically-feasible underfeeding
  (`scaling_factor < 1`). Supply `maintenance_share` (a scalar fraction
  or a tibble with `livestock_category` and `maintenance_share`) to also
  diagnose polities pushed below maintenance; the over-stocked demand
  rows are attached to the result as the `grass_deficit_diagnosis`
  attribute. Set `distribute_surplus = FALSE` to suppress the
  surplus-distribution pass that pushes leftover CBS availability onto
  variable-demand livestock (correct for historical analyses where the
  CBS feed element is the realised consumption; keep `TRUE`, the
  default, for unconstrained scenario projections).

## Value

A tibble of realised intake per demand row. When `maintenance_share` is
supplied alongside `grass_availability`, a `grass_deficit_diagnosis`
attribute lists demand rows underfed below maintenance.

## Examples

``` r
# Two variable-demand categories in one territory competing for a single
# feed item. Availability (150) falls short of demand (200), so the
# remaining-share principle underfeeds both by the same factor rather than
# letting either exceed what is there.
feed_demand <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~livestock_category,
  ~item_cbs_code, ~feed_group, ~feed_quality, ~demand_dm_t, ~fixed_demand,
  2000L, "79", "79", "Cattle_milk",
  NA_integer_, NA_character_, "high_quality", 120, FALSE,
  2000L, "79", "79", "Pigs",
  NA_integer_, NA_character_, "high_quality", 80, FALSE
)

feed_avail <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~item_cbs_code, ~feed_group,
  ~feed_quality, ~avail_dm_t, ~feed_scale,
  2000L, "79", "79", 2514L, "cereals", "high_quality", 150, "national"
)

redistribute_feed(feed_demand, feed_avail)
#> # A tibble: 2 × 14
#>    year territory sub_territory livestock_category item_cbs_code feed_group
#>   <int> <chr>     <chr>         <chr>                      <int> <chr>     
#> 1  2000 79        79            Cattle_milk                 2514 cereals   
#> 2  2000 79        79            Pigs                        2514 cereals   
#> # ℹ 8 more variables: feed_quality <chr>, demand_dm_t <dbl>, intake_dm_t <dbl>,
#> #   scaling_factor <dbl>, hierarchy_level <chr>, requested_item <int>,
#> #   source_compartment <chr>, fixed_demand <lgl>
```
