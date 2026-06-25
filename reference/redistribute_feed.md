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
