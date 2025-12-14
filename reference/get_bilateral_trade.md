# Bilateral trade data

Reports trade between pairs of countries in given years.

## Usage

``` r
get_bilateral_trade(trade_version = NULL, cbs_version = NULL)
```

## Arguments

- trade_version:

  File version used for bilateral trade input. See
  [whep_inputs](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  for version details.

- cbs_version:

  File version passed to
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  call.

## Value

A tibble with the reported trade between countries. For efficient memory
usage, the tibble is not exactly in tidy format. It contains the
following columns:

- `year`: The year in which the recorded event occurred.

- `item_cbs_code`: FAOSTAT internal code for the item that is being
  traded. For code details see e.g.
  [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md).

- `bilateral_trade`: Square matrix of `NxN` dimensions where `N` is the
  total number of countries being considered. The matrix row and column
  names are exactly equal and they represent country codes.

  - Row name: The code of the country where the data is from. For code
    details see e.g.
    [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md).

  - Column name: FAOSTAT internal code for the country that is importing
    the item. See row name explanation above.

  If `m` is the matrix, the value at `m["A", "B"]` is the trade in
  tonnes from country `"A"` to country `"B"`, for the corresponding year
  and item. The matrix can be considered *balanced*. This means:

  - The sum of all values from row `"A"`, where `"A"` is any country,
    should match the total exports from country `"A"` reported in the
    commodity balance sheet (which is considered more accurate for
    totals).

  - The sum of all values from column `"A"`, where `"A"` is any country,
    should match the total imports into country `"A"` reported in the
    commodity balance sheet (which is considered more accurate for
    totals).

  The sums may not be exactly the expected values because of precision
  issues and/or the iterative proportional fitting algorithm not
  converging fast enough, but should be relatively very close to the
  desired totals.

The step by step approach to obtain this data tries to follow the FABIO
model and is explained below. All the steps are performed separately for
each group of year and item.

- From the FAOSTAT reported bilateral trade, there are sometimes two
  values for one trade flow: the exported amount claimed by the reporter
  country and the import amount claimed by the partner country. Here,
  the export data was preferred, i.e., if country `"A"` says it exported
  `X` tonnes to country `"B"` but country `"B"` claims they got `Y`
  tonnes from country `"A"`, we trust the export data `X`. This choice
  is only needed if there exists a reported amount from both sides.
  Otherwise, the single existing report is chosen.

- Complete the country data, that is, add any missing combinations of
  country trade with NAs, which will be estimated later. In the matrix
  form, this doesn't increase the memory usage since we had to build a
  matrix anyway (for the balancing algorithm), and the *empty* parts
  also take up memory. This is also done for total imports/exports from
  the commodity balance sheet, but these are directly filled with 0s
  instead.

- The total imports and exports from the commodity balance sheet are
  balanced by downscaling the largest of the two to match the lowest.
  This is done in the following way:

  - If `total_imports > total_exports`: Set `import` as
    `total_exports * import / total_import`.

  - If `total_exports > total_exports`: Set `export` as
    `total_exports * export / total_export`.

- The missing data in the matrix must be estimated. It's done like this:

  - For each pair of exporter `i` and importer `j`, we estimate a
    bilateral trade `m[i, j]` using the export shares of `i` and import
    shares of `j` from the commodity balance sheet:

    - `est_1 <- exports[i] * imports[j] / sum(imports)`, i.e., total
      exports of country `i` spread among other countries' import
      shares.

    - `est_2 <- imports[j] * exports[i] / sum(exports)`, i.e. total
      imports of country `j` spread among other countries' export
      shares.

    - `est <- (est_1 + est_2) / 2`, i.e., the mean of both estimates.

    In the above computations, exports and imports are the original
    values before they were balanced.

  - The estimates for data that already existed (i.e. non-NA) are
    discarded. For the ones left, for each row (i.e. exporter country),
    we get the difference between its balanced total export and the sum
    of original non-estimated data. The result is the *`gap`* we can
    actually fill with estimates, so as to not get past the reported
    total export. If the sum of non-discarded estimates is larger, it
    must be downscaled and spread by computing
    `gap * non_discarded_estimate / sum(non_discarded_estimates)`.

  - The estimates are divided by a *trust factor*, in the sense that we
    don't rely on the whole value, thinking that a non-present value
    might actually be because that specific trade was 0, so we don't
    overestimate too much. The chosen factor is 10%, so only 10% of the
    estimate's value is actually used to fill the NA from the original
    bilateral trade matrix.

- The matrix is balanced, as mentioned before, using the [iterative
  proportional fitting
  algorithm](https://en.wikipedia.org/wiki/Iterative_proportional_fitting).
  The target sums for rows and columns are respectively the balanced
  exports and imports computed from the commodity balance sheet. The
  algorithm is performed directly using the [mipfp R
  package](https://CRAN.R-project.org/package=mipfp).

## Examples

``` r
# Note: These are smaller samples to show outputs, not the real data.
# For all data, call the function with default versions (i.e. no arguments).
get_bilateral_trade(
  trade_version = "example",
  cbs_version = "example"
)
#> ℹ Fetching files for commodity_balance_sheet...
#> ℹ Fetching files for bilateral_trade...
#> # A tibble: 72 × 3
#>     year item_cbs_code bilateral_trade  
#>    <int>         <dbl> <list>           
#>  1  1995          2630 <dbl [187 × 187]>
#>  2  2001          2733 <dbl [187 × 187]>
#>  3  2005          2671 <dbl [187 × 187]>
#>  4  2017          2513 <dbl [187 × 187]>
#>  5  2011          2601 <dbl [187 × 187]>
#>  6  2011          2655 <dbl [187 × 187]>
#>  7  2015          2558 <dbl [187 × 187]>
#>  8  2014          2577 <dbl [187 × 187]>
#>  9  2000          2625 <dbl [187 × 187]>
#> 10  2018          2661 <dbl [187 × 187]>
#> # ℹ 62 more rows
```
