# Allocate grazing land forward to livestock products.

Push grazing land forward along the feed chain instead of tracing it
backward through a Leontief inverse. Grass is not traded, so the
multi-regional input-output footprint leaks the grazing land that meat
and dairy consumption actually drives (the feed-conversion columns make
the system non-productive). This function hands every tonne of grazing
land forward in two mass-conserving steps:

1.  pool each country's grazing land and split it across the grazing
    animals that fed there, in proportion to their forage intake;

2.  split each animal's share across its output products in proportion
    to production mass, landing the land on the meat and milk items that
    are actually traded.

The result is a direct-land extension keyed by livestock output item,
ready to route to consumers with
[`compute_footprint_balance()`](https://eduaguilera.github.io/whep/reference/compute_footprint_balance.md).
Because each step redistributes a total without creating or destroying
it, the country-level land total is conserved whenever an animal fed and
produced an output there.

## Usage

``` r
allocate_grazing_to_products(
  grass_land,
  grazer_intake,
  livestock_production,
  products = c("all", "meat_milk")
)
```

## Arguments

- grass_land:

  Tibble with `year`, `area_code` and `value` (grazing land, e.g.
  hectares). Multiple rows per country (for example one per grass item)
  are pooled.

- grazer_intake:

  Tibble with `year`, `area_code`, `live_anim_code` and `value` (the
  intake by which to split the land across animals), as from
  [`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md).
  Using grazer forage intake (grass plus roughage residues) rather than
  the `"grass"` feed type alone keeps extensive-grazing countries, whose
  forage the feed model classes as residues, from dropping out.

- livestock_production:

  Tibble with `year`, `area_code`, `live_anim_code`, `item_cbs_code` and
  `value` (output production, tonnes).

- products:

  Which output items absorb the land. `"all"` (default) spreads it
  across every livestock output by mass; `"meat_milk"` restricts it to
  meat and dairy items, so all grazing land lands on meat and dairy
  consumers per the forward-allocation goal.

## Value

A tibble with `year`, `area_code`, `item_cbs_code` (a livestock output
item), `value` (grazing land allocated to it) and `method_allocation`
(the chosen `products`).

## Examples

``` r
grass_land <- tibble::tibble(
  year = 2010L, area_code = 1L, value = 100
)
grazer_intake <- tibble::tibble(
  year = 2010L,
  area_code = 1L,
  live_anim_code = c(960L, 976L),
  value = c(75, 25)
)
livestock_production <- tibble::tibble(
  year = 2010L,
  area_code = 1L,
  live_anim_code = c(960L, 960L, 976L),
  item_cbs_code = c(2848L, 2731L, 2732L),
  value = c(90, 10, 5)
)
allocate_grazing_to_products(
  grass_land, grazer_intake, livestock_production
)
#> # A tibble: 3 × 5
#>    year area_code item_cbs_code value method_allocation
#>   <int>     <int>         <int> <dbl> <chr>            
#> 1  2010         1          2848  67.5 all              
#> 2  2010         1          2731   7.5 all              
#> 3  2010         1          2732  25   all              
```
