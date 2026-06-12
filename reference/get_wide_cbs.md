# Commodity balance sheet data.

Retrieve supply and use parts for each commodity balance sheet (CBS)
item. Stock variations are split into two non-negative columns following
the FABIO methodology.

## Usage

``` r
get_wide_cbs(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with the commodity balance sheet data in wide format. It
contains the following columns:

- `year`: The year in which the recorded event occurred.

- `area_code`: The code of the country where the data is from. For code
  details see e.g.
  [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md).

- `item_cbs_code`: FAOSTAT internal code for each item. For code details
  see e.g.
  [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md).

The other columns are quantities where total supply and total use should
be balanced. Units are tonnes for most items, and heads for live animals
(see
[items_cbs](https://eduaguilera.github.io/whep/reference/items_cbs.md)
`item_type`).

For supply:

- `production`: Produced locally.

- `import`: Obtained from importing from other countries.

- `stock_withdrawal`: Biomass taken out of storage (non-negative).
  Positive when stocks decrease.

For use:

- `food`: Food for humans.

- `feed`: Food for animals.

- `export`: Released as export for other countries.

- `seed`: Intended for new production.

- `processing`: Used to obtain other subproducts.

- `other_uses`: Any other use not included above.

- `stock_addition`: Biomass placed into storage (non-negative). Positive
  when stocks increase.

There is an additional column `domestic_supply` which is computed as
total use excluding `export`.

## Examples

``` r
get_wide_cbs(example = TRUE)
#> # A tibble: 10 × 18
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name 
#>    <int>     <int>            <int> <chr>                 <chr>                 
#>  1  1987       250              250 COD-1960-2025         Democratic Republic o…
#>  2  2012        41               41 CHN-1950-2025         China (PRC)           
#>  3  1984       123              123 LBR-1847-2025         Liberia               
#>  4  1982       165              165 PAK-1971-2025         Pakistan              
#>  5  1977       159              159 NGA-1961-2025         Nigeria               
#>  6  1995       234              234 URY-1828-2025         Uruguay               
#>  7  1975        10               10 AUS-1901-2025         Australia             
#>  8  1961       156              156 NZL-1840-2025         New Zealand           
#>  9  1961       236              236 VEN-1821-2025         Venezuela             
#> 10  1995        49               49 CUB-1800-2025         Cuba                  
#> # ℹ 13 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_cbs_code <dbl>, domestic_supply <dbl>, food <dbl>, production <dbl>,
#> #   feed <dbl>, seed <dbl>, import <dbl>, export <dbl>, other_uses <dbl>,
#> #   processing <dbl>, stock_withdrawal <dbl>, stock_addition <dbl>
```
