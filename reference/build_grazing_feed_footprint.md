# Build a grazing-land footprint by forward feed-allocation.

End-to-end grazing-land footprint for one year on real WHEP data.
Grazing land (the grassland extension) is pushed forward onto livestock
meat and milk via
[`allocate_grazing_to_products()`](https://eduaguilera.github.io/whep/reference/allocate_grazing_to_products.md),
then routed to consuming countries through the bilateral meat-trade
network with
[`compute_footprint_balance()`](https://eduaguilera.github.io/whep/reference/compute_footprint_balance.md).
This attributes grazing land to the meat and dairy consumption that
drives it – the chain the Leontief footprint leaks because grass is
non-productive and not traded.

The inputs are assembled from
[`build_grassland_land_extension()`](https://eduaguilera.github.io/whep/reference/build_grassland_land_extension.md),
[`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md),
[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
and
[`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md).
Supply any of them through `data` to reuse cached inputs or to test
without remote reads.

`intake_basis` selects the intake that splits grazing land across
animals. The default `"grazer_forage"` uses every grazing animal's grass
plus roughage-residue intake, because the feed model classes the forage
of major extensive-grazing countries (for example Australia, the United
States and Argentina) as residues rather than grass; keying on the
`"grass"` feed type alone would silently strip that land. `"grass"`
reproduces the narrower grass-feed basis for sensitivity analysis. Land
in countries with no grazer intake at all cannot be attributed and is
reported via a warning.

## Usage

``` r
build_grazing_feed_footprint(
  year,
  products = c("all", "meat_milk"),
  intake_basis = c("grazer_forage", "grass"),
  data = list(),
  example = FALSE
)
```

## Arguments

- year:

  Year to build the footprint for.

- products:

  Co-product split passed to
  [`allocate_grazing_to_products()`](https://eduaguilera.github.io/whep/reference/allocate_grazing_to_products.md).
  `"all"` (default) or `"meat_milk"`.

- intake_basis:

  Intake used to split land across animals, `"grazer_forage"` (default)
  or `"grass"`.

- data:

  Optional named list of pre-built inputs, any of `grass_land`,
  `grazer_intake`, `livestock_production` and `trade`. Each falls back
  to its builder when absent.

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with `area_code` (consuming country), `item_cbs_code` (meat or
milk item), `value` (embodied grazing land) and `method`
(`"grazing_feed_allocation"`).

## Examples

``` r
build_grazing_feed_footprint(example = TRUE)
#> # A tibble: 10 × 4
#>    area_code item_cbs_code     value method                 
#>        <int>         <int>     <dbl> <chr>                  
#>  1        10          2848 184625300 grazing_feed_allocation
#>  2        10          2731  71204900 grazing_feed_allocation
#>  3        41          2731  38950100 grazing_feed_allocation
#>  4        33          2848  24310700 grazing_feed_allocation
#>  5        33          2732   9875400 grazing_feed_allocation
#>  6       100          2731   6420300 grazing_feed_allocation
#>  7        79          2848   3155800 grazing_feed_allocation
#>  8       179          2735    812600 grazing_feed_allocation
#>  9       122          2740     49120 grazing_feed_allocation
#> 10       137          2732     18430 grazing_feed_allocation
```
