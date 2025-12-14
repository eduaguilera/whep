# Livestock feed intake

Get amount of items used for feeding livestock.

## Usage

``` r
get_feed_intake(version = NULL)
```

## Arguments

- version:

  File version to use as input. See
  [whep_inputs](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  for details.

## Value

A tibble with the feed intake data. It contains the following columns:

- `year`: The year in which the recorded event occurred.

- `area_code`: The code of the country where the data is from. For code
  details see e.g.
  [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md).

- `live_anim_code`: Commodity balance sheet code for the type of
  livestock that is fed. For code details see e.g.
  [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md).

- `item_cbs_code`: The code of the item that is used for feeding the
  animal. For code details see e.g.
  [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md).

- `feed_type`: The type of item that is being fed. It can be one of:

  - `animals`: Livestock product, e.g. `Bovine Meat`, `Butter, Ghee`,
    etc.

  - `crops`: Crop product, e.g. `Vegetables, Other`, `Oats`, etc.

  - `residues`: Crop residue, e.g. `Straw`, `Fodder legumes`, etc.

  - `grass`: Grass, e.g. `Grassland`, `Temporary grassland`, etc.

  - `scavenging`: Other residues. Single `Scavenging` item.

- `supply`: The computed amount in tonnes of this item that should be
  fed to this animal, when sharing the total item `feed` use from the
  Commodity Balance Sheet among all livestock.

- `intake`: The actual amount in tonnes that the animal needs, which can
  be less than the theoretical used amount from `supply`.

- `intake_dry_matter`: The amount specified by `intake` but only
  considering dry matter, so it should be less than `intake`.

- `loss`: The amount that is not used for feed. This is
  `supply - intake`.

- `loss_share`: The percent that is lost. This is `loss / supply`.

## Examples

``` r
# Note: These are smaller samples to show outputs, not the real data.
# For all data, call the function with default version (i.e. no arguments).
get_feed_intake(version = "example")
#> ℹ Fetching files for feed_intake...
#> # A tibble: 10,000 × 10
#>     year area_code live_anim_code item_cbs_code feed_type   supply   intake
#>    <dbl>     <dbl>          <dbl>         <dbl> <chr>        <dbl>    <dbl>
#>  1  1983        15           1096          2102 crops         21.1     18.9
#>  2  1985       251           1016          3000 grass     269616.  269616. 
#>  3  2021       222           1052          2781 animals       35.9     32.3
#>  4  2017       105           1079          2598 crops       1727.    1554. 
#>  5  2000        39           1053          2106 residues   12662.   11396. 
#>  6  1968        84           1016          2002 residues    3015.    2714. 
#>  7  2008       170           1053          2595 crops       4294.    3864. 
#>  8  2015         3            976          2101 crops         40.1     36.1
#>  9  2002        79           1052          2558 crops       3788.    3409. 
#> 10  2014        41           1068          2518 crops     148505.  133654. 
#> # ℹ 9,990 more rows
#> # ℹ 3 more variables: intake_dry_matter <dbl>, loss <dbl>, loss_share <dbl>
```
