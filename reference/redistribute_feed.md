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
  for the available entries and their defaults.

## Value

A tibble of realised intake per demand row.
