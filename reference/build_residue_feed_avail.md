# Build residue feed availability for feed allocation.

Turns the feed destiny of crop residues into the `feed_avail` contract
consumed by
[`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md):
maps each crop to its residue commodity item, applies a
feed-availability loss, and aggregates to year / territory / residue
item.

## Usage

``` r
build_residue_feed_avail(x, loss_fraction = 0.15, feed_scale = "national")
```

## Arguments

- x:

  A tibble with `item_prod_code`, `year`, `sub_territory` and
  `residue_feed_dm_t` (from
  [`calculate_residue_destinies()`](https://eduaguilera.github.io/whep/reference/calculate_residue_destinies.md)).

- loss_fraction:

  Fraction of the feed residue lost before intake (default 0.15).

- feed_scale:

  Value for the `feed_scale` column (default `"national"`).

## Value

A tibble with the
[`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
`feed_avail` columns: `year`, `sub_territory`, `item_cbs_code`,
`feed_group`, `feed_quality` (`"residues"`), `avail_dm_t` and
`feed_scale`.

## Examples

``` r
tibble::tibble(
  item_prod_code = "15", year = 2000, sub_territory = "ESP",
  residue_feed_dm_t = 50
) |>
  build_residue_feed_avail()
#> # A tibble: 1 × 7
#>    year sub_territory item_cbs_code avail_dm_t feed_group feed_quality
#>   <dbl> <chr>                 <int>      <dbl> <chr>      <chr>       
#> 1  2000 ESP                    2105       42.5 residues   residues    
#> # ℹ 1 more variable: feed_scale <chr>
```
