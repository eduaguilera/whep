# Fill gaps using growth rates from proxy variables

Fills missing values using growth rates from a proxy variable (reference
series). Supports regional aggregations, weighting, and linear
interpolation for small gaps.

## Usage

``` r
fill_proxy_growth(
  data,
  value_col,
  proxy_col,
  time_col = year,
  .by = NULL,
  max_gap = Inf,
  max_gap_linear = 3,
  fill_scope = NULL,
  value_smooth_window = NULL,
  proxy_smooth_window = 1,
  output_format = "clean",
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame containing time series data.

- value_col:

  The column containing values to fill.

- proxy_col:

  Character or vector. Proxy variable(s) for calculating growth rates.
  Supports multiple syntax formats:

  - **Simple numeric proxy** (e.g., `"population"`): Auto-detects
    numeric columns and uses them as proxy variable. Inherits the `.by`
    parameter to compute proxy values per group.

  - **Simple categorical proxy** (e.g., `"region"`): Auto-detects
    categorical columns and interprets as `value_col:region`. Aggregates
    `value_col` by the specified groups.

  - **Advanced syntax** (e.g., `"gdp:region"`): Format is
    `"variable:group1+group2"`. Aggregates variable by specified groups.

  - **Hierarchical fallback** (e.g., `c("population", "gdp:region")`):
    Tries first proxy, falls back to second if first fails.

  - **Weighted aggregation** (e.g., `"gdp[population]"`): Weight
    variable by specified column during aggregation.

- time_col:

  The column containing time values. Default: `year`.

- .by:

  A character vector with the grouping variables (optional).

- max_gap:

  Numeric. Maximum gap size to fill using growth method. Default: Inf.

- max_gap_linear:

  Numeric. Maximum gap size for linear interpolation fallback. Default:
  3.

- fill_scope:

  Quosure. Filter expression to limit filling scope. Default: NULL.

- value_smooth_window:

  Integer. Window size for a centered moving average applied to the
  value column before gap-filling. Useful for variables with high
  inter-annual variability. If `NULL` (default), no smoothing is
  applied.

- proxy_smooth_window:

  Integer. Window size for moving average smoothing of proxy reference
  values before computing growth rates. Default: 1.

- output_format:

  Character. Output format: "clean" or "detailed". Default: "clean".

- verbose:

  Logical. Print progress messages. Default: TRUE.

## Value

A data frame with filled values. If output_format = "clean", returns
original columns with updated value_col and added source column. If
"detailed", includes all intermediate columns.

## Details

**Combined Growth Sequence (Hierarchical Interpolation):**

When using multiple proxies with hierarchical fallback, the function
implements an intelligent combined growth sequence strategy:

1.  Better proxies (earlier in hierarchy) are tried first for each gap.

2.  If a better proxy has partial coverage within a gap, those growth
    rates are used for the covered positions.

3.  Fallback proxies fill only the remaining positions where better
    proxies are not available.

4.  Values filled by better proxies are protected from being
    overwritten.

## See also

[`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md),
[`fill_sum()`](https://eduaguilera.github.io/whep/reference/fill_sum.md)

## Examples

``` r
# Fill GDP using population as proxy
data <- tibble::tibble(
  country = rep("ESP", 4),
  year = 2010:2013,
  gdp = c(1000, NA, NA, 1200),
  population = c(46, 46.5, 47, 47.5)
)

fill_proxy_growth(
  data,
  value_col = gdp,
  proxy_col = "population",
  .by = "country"
)
#> Auto-detected 'population' as numeric -> using as source variable
#> Calculating growth rates for: population
#> Step 2: Applying hierarchical filling...
#>   Applying proxy level 1: population
#> Total filled: 2 out of 2 missing values
#> # A tibble: 4 × 5
#>   country  year   gdp population source_gdp   
#>   <chr>   <int> <dbl>      <dbl> <chr>        
#> 1 ESP      2010 1000        46   original     
#> 2 ESP      2011 1067.       46.5 linear_interp
#> 3 ESP      2012 1133.       47   linear_interp
#> 4 ESP      2013 1200        47.5 original     
```
