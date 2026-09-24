# Build residue feed availability for feed allocation (deprecated).

**Deprecated, and not WHEP's residue feed availability.** Nothing in the
package calls it, and it warns (class
`whep_residue_feed_avail_deprecated`) on every call. The pipeline's
residue feed comes from the commodity balance instead:
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
books the recovered residue's feed share as the `feed` element of Straw
(2105) and Other crop residues (2106), and the feed allocator converts
that to available dry matter with the same `0.9` factor it applies to
every CBS feed item. This function applies a different loss
(`loss_fraction`, default `0.15`: assumed, unverified, no source on
record) to a different residue estimate, so the two do not agree
(whep#1138).

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
  `sub_territory` names the territory the residue belongs to.

- loss_fraction:

  Fraction of the feed residue lost before intake (default 0.15;
  assumed, unverified).

- feed_scale:

  Value for the `feed_scale` column (default `"national"`).

## Value

A tibble with the
[`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
`feed_avail` columns: `year`, `territory`, `sub_territory`,
`item_cbs_code`, `feed_group`, `feed_quality` (`"residues"`),
`avail_dm_t` and `feed_scale`. `territory` repeats `sub_territory`,
because
[`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
keys national-scale availability on `territory` alone.

## Examples

``` r
tibble::tibble(
  item_prod_code = "15", year = 2000, sub_territory = "ESP",
  residue_feed_dm_t = 50
) |>
  build_residue_feed_avail()
#> Warning: `build_residue_feed_avail()` is deprecated (whep#1138).
#> ℹ WHEP's residue feed availability is the commodity balance's feed element for
#>   items 2105 and 2106, which the feed allocator converts with a 0.9 factor, not
#>   this function's `loss_fraction`.
#> # A tibble: 1 × 8
#>    year territory sub_territory item_cbs_code avail_dm_t feed_group feed_quality
#>   <dbl> <chr>     <chr>                 <int>      <dbl> <chr>      <chr>       
#> 1  2000 ESP       ESP                    2105       42.5 residues   residues    
#> # ℹ 1 more variable: feed_scale <chr>
```
