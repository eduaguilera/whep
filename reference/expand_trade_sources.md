# Trade data sources

Create a new dataframe where each row has a year range into one where
each row is a single year, effectively 'expanding' the whole year range.

## Usage

``` r
expand_trade_sources(trade_sources)
```

## Arguments

- trade_sources:

  A tibble dataframe where each row contains the year range.

## Value

A tibble dataframe where each row corresponds to a single year for a
given source.

## Examples

``` r
trade_sources <- tibble::tibble(
  Name = c("a", "b", "c"),
  Trade = c("t1", "t2", "t3"),
  Info_Format = c("year", "partial_series", "year"),
  Timeline_Start = c(1, 1, 2),
  Timeline_End = c(3, 4, 5),
  Timeline_Freq = c(1, 1, 2),
  `Imp/Exp` = "Imp",
  SACO_link = NA,
)
expand_trade_sources(trade_sources)
#> # A tibble: 9 × 12
#> # Groups:   No [3]
#>      No  Year Name  Trade Info_Format  Timeline_Start Timeline_End Timeline_Freq
#>   <int> <dbl> <chr> <chr> <chr>                 <dbl>        <dbl>         <dbl>
#> 1     1     1 a_1   t1    year                      1            3             1
#> 2     1     2 a_2   t1    year                      1            3             1
#> 3     1     3 a_3   t1    year                      1            3             1
#> 4     2     1 b     t2    partial_ser…              1            4             1
#> 5     2     2 b     t2    partial_ser…              1            4             1
#> 6     2     3 b     t2    partial_ser…              1            4             1
#> 7     2     4 b     t2    partial_ser…              1            4             1
#> 8     3     2 c_2   t3    year                      2            5             2
#> 9     3     4 c_4   t3    year                      2            5             2
#> # ℹ 4 more variables: `Imp/Exp` <chr>, SACO_link <lgl>, ImpExp <chr>,
#> #   In_Saco <int>
```
