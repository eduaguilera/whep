# Commodity balance sheet data

States supply and use parts for each commodity balance sheet (CBS) item.

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

The other columns are quantities (measured in tonnes), where total
supply and total use should be balanced.

For supply:

- `production`: Produced locally.

- `import`: Obtained from importing from other countries.

- `stock_retrieval`: Available as net stock from previous years. For
  ease, only one stock column is included here as supply. If the value
  is positive, there is a stock quantity available as supply. Otherwise,
  it means a larger quantity was stored for later years and cannot be
  used as supply, having to deduce it from total supply. Since in this
  case it is negative, the total supply is still computed as the sum of
  all of these.

For use:

- `food`: Food for humans.

- `feed`: Food for animals.

- `export`: Released as export for other countries.

- `seed`: Intended for new production.

- `processing`: The product will be used to obtain other subproducts.

- `other_uses`: Any other use not included in the above ones.

There is an additional column `domestic_supply` which is computed as the
total use excluding `export`.

## Examples

``` r
get_wide_cbs(example = TRUE)
#> # A tibble: 10 × 13
#>     year area_code item_cbs_code domestic_supply    food production   feed  seed
#>    <int>     <int>         <dbl>           <dbl>   <dbl>      <dbl>  <dbl> <dbl>
#>  1  1987       250          2106      13741247       0     13741247 1.37e7     0
#>  2  2012        41          2633         82000   82000            0 0          0
#>  3  1984       123          2595          1207       0         3854 1.21e3     0
#>  4  1982       165          2633            86.5    86.5          0 0          0
#>  5  1977       159          2658          2218    2218            0 0          0
#>  6  1995       234          2671          4312       0         2500 0          0
#>  7  1975        10           677          2270    2270         2270 0          0
#>  8  1961       156          2658          6877    6877         2000 0          0
#>  9  1961       236          2620         11177   11177            0 0          0
#> 10  1995        49          2734         71117   71117        56724 0          0
#> # ℹ 5 more variables: import <dbl>, export <dbl>, other_uses <dbl>,
#> #   processing <dbl>, stock_retrieval <dbl>
```
