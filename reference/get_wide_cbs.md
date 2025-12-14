# Commodity balance sheet data

States supply and use parts for each commodity balance sheet (CBS) item.

## Usage

``` r
get_wide_cbs(version = NULL)
```

## Arguments

- version:

  File version to use as input. See
  [whep_inputs](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  for details.

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
# Note: These are smaller samples to show outputs, not the real data.
# For all data, call the function with default version (i.e. no arguments).
get_wide_cbs(version = "example")
#> ℹ Fetching files for commodity_balance_sheet...
#> # A tibble: 9,959 × 13
#>     year area_code item_cbs_code processing production other_uses  feed  seed
#>    <int>     <int>         <dbl>      <dbl>      <dbl>      <dbl> <dbl> <dbl>
#>  1  1968       216          2775          0         0           0     0     0
#>  2  2011        57          2731          0    297800           0     0     0
#>  3  1962        51          2590          0         0           0     0     0
#>  4  1973        48          2737          0      6656.          0     0     0
#>  5  1987       171          2106          0         0           0     0     0
#>  6  2012       129          2105          0         0           0     0     0
#>  7  1980       162          2737          0         0           0     0     0
#>  8  2001        97          2781          0         0           0     0     0
#>  9  2021       250          2613          0         0           0     0     0
#> 10  1968       202          2807          0         0           0     0     0
#> # ℹ 9,949 more rows
#> # ℹ 5 more variables: import <dbl>, domestic_supply <dbl>, food <dbl>,
#> #   export <dbl>, stock_retrieval <dbl>
```
