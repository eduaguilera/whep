# Livestock feed intake

Get amount of items used for feeding livestock.

## Usage

``` r
get_feed_intake(
  example = FALSE,
  grain = c("national", "local"),
  demand_tier = c("ipcc", "fcr"),
  years = NULL
)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

- grain:

  Spatial grain of the feed allocation. `"national"` (default, one
  allocation per country) or `"local"` (the per-cell 0.5-degree engine,
  which is heavy and run via
  [`build_feed_intake_local()`](https://eduaguilera.github.io/whep/reference/build_feed_intake_local.md);
  calling it here redirects there).

- demand_tier:

  Demand-estimation tier. `"ipcc"` (default, the rigorous IPCC Tier-2
  energy demand for the ruminant species it covers, Bouwman FCR for pigs
  and poultry, Krausmann per-head for draft / other species) or `"fcr"`
  (the Bouwman / Krausmann feed-conversion magnitude for every species).
  Both grains allocate with
  [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md).

- years:

  Integer vector of years to build, or `NULL` (default) for every year
  in the production data (1850-2023 via the LUH2 extension). Restricting
  the range cuts run time proportionally; allocation is independent per
  year, so a subset returns exactly the same rows for those years.

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
#> # A tibble: 11 × 14
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name 
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                 
#>  1  1990        51               51 F51-1947-1993         Czechoslovakia (1947-…
#>  2  2007         3                3 ALB-1913-2025         Albania (1913-2025)   
#>  3  1996        54               54 DNK-1920-2025         Denmark               
#>  4  2011        NA               NA NA                    NA                    
#>  5  1996       110              110 JPN-1952-2025         Japan                 
#>  6  1986         4                4 DZA-1962-2025         Algeria (1962-2025)   
#>  7  2010       150              150 NLD-1830-2025         Netherlands           
#>  8  1978        NA               NA NA                    NA                    
#>  9  2021        23               23 BLZ-1800-2025         Belize                
#> 10  1977       114              114 KEN-1963-2025         Kenya                 
#> 11  2020        32               32 CMR-1961-2025         Cameroon              
#> # ℹ 9 more variables: reporting_polity_has_geometry <lgl>,
#> #   live_anim_code <dbl>, item_cbs_code <dbl>, feed_type <chr>, supply <dbl>,
#> #   intake <dbl>, intake_dry_matter <dbl>, loss <dbl>, loss_share <dbl>
```
