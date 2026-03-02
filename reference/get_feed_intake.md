# Livestock feed intake

Get amount of items used for feeding livestock.

## Usage

``` r
get_feed_intake(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

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
get_feed_intake(example = TRUE)
#> # A tibble: 10 × 10
#>     year area_code live_anim_code item_cbs_code feed_type    supply     intake
#>    <dbl>     <dbl>          <dbl>         <dbl> <chr>         <dbl>      <dbl>
#>  1  1990        51           1096          2515 crops        429       386    
#>  2  2007         3            976          2570 crops          5.51      4.96 
#>  3  1996        54            960          2531 crops       5451      4906    
#>  4  2011        NA           1052          2532 crops          0.84      0.756
#>  5  1996       110           1052          2549 crops         17.1      15.4  
#>  6  1986         4           1053          2514 crops     297464    267717    
#>  7  2010       150           1068          2595 crops       2645      2380    
#>  8  1978        NA           1096          2536 crops        159       143    
#>  9  2021        23           1053          2511 crops        627       347    
#> 10  1977       114            976          2517 crops         32.5      29.2  
#> # ℹ 3 more variables: intake_dry_matter <dbl>, loss <dbl>, loss_share <dbl>
```
