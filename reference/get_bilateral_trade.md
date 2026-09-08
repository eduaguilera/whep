# Bilateral trade data

Reports trade between pairs of countries in given years.

## Usage

``` r
get_bilateral_trade(
  example = FALSE,
  cbs = NULL,
  method_items_not_in_cbs = c("drop", "keep", "abort")
)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

- cbs:

  Optional pre-computed wide CBS tibble from
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).
  If `NULL` (default), it is built internally.

- method_items_not_in_cbs:

  How to treat a traded item whose `item_cbs_code` has no commodity
  balance sheet row to balance it against. See the *Items with no CBS
  row* section. One of:

  - `"drop"` (default): discard those flows, the historical behaviour.
    Published values are unaffected by this argument's existence as long
    as the default is kept.

  - `"keep"`: keep the flows and take the row and column margins from
    the reported bilateral data itself instead of from the CBS.

  - `"abort"`: fail, so that a refreshed pin cannot introduce unanchored
    items unnoticed.

  `example = TRUE` always returns the `"drop"` fixture.

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

- `has_cbs_totals`: `TRUE` when the matrix margins came from the
  commodity balance sheet, `FALSE` when the item had no CBS row for that
  year and its margins were taken from the reported bilateral flows
  themselves. Always `TRUE` unless `method_items_not_in_cbs = "keep"`.

- `method_items_not_in_cbs`: the treatment chosen for items with no CBS
  row, recorded so a downstream consumer can tell which variant it is
  holding.

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
  exports and imports computed from the commodity balance sheet.

## Items with no CBS row

The bilateral trade matrices are balanced against the total exports and
imports reported in the commodity balance sheet, so an item with no CBS
supply/use row has nothing to balance against. Historically those flows
were discarded silently. Measured on the `bilateral_trade` pin
`20250714T123347Z-2c392` (after the export-preference deduplication and
the tonnes filter, i.e. exactly what reaches this step): 12.51 Gt of
47.48 Gt, **26.4% of the traded tonnage over 1986-2021**, in 7 items, of
which 99.1% is the FABIO-style aggregate placeholder `"Other"`
(`item_cbs_code` 5001). The drop is now reported with
[`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html)
whichever method is chosen, because a quarter of world trade should not
disappear without a message.

The share is far from constant: 9.1% over 1986-2003, **49.6% over
2004-2013** and 4.3% over 2014-2021. The middle block is not a real
trade signal. It contains physically impossible flows booked in tonnes:
Colombia to the United States, 2004, 2.58 Gt of `"Other"` in a single
cell, more than world cereal production; Kenya to the Netherlands,
515-594 Mt/year over 2005-2009. Excluding item 5001 altogether, the
whole drop is 112 Mt, 0.24% of traded tonnage.

Which treatment is right is therefore a methodological question, not a
lookup: `"Other"` is an unallocated residual whose 2004-2013 values are
demonstrably corrupt, so `"keep"` carries that corruption into the
output, and mapping the residual onto real CBS items would need a
sourced disaggregation key that does not exist in the package. Nothing
downstream consumes the kept rows yet either:
[`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
takes its item dimension from supply-use and the CBS, so an item absent
from both is ignored by `.build_trade_shares()` regardless of this
argument.

## Examples

``` r
get_bilateral_trade(example = TRUE)
#> # A tibble: 10 × 5
#>     year item_cbs_code bilateral_trade   has_cbs_totals method_items_not_in_cbs
#>    <int>         <dbl> <list>            <lgl>          <chr>                  
#>  1  2003          2552 <dbl [187 × 187]> TRUE           drop                   
#>  2  2015          2672 <dbl [187 × 187]> TRUE           drop                   
#>  3  2015          2664 <dbl [187 × 187]> TRUE           drop                   
#>  4  2011          2543 <dbl [187 × 187]> TRUE           drop                   
#>  5  1991          2613 <dbl [187 × 187]> TRUE           drop                   
#>  6  1999          2578 <dbl [187 × 187]> TRUE           drop                   
#>  7  2001          2590 <dbl [187 × 187]> TRUE           drop                   
#>  8  2003          2613 <dbl [187 × 187]> TRUE           drop                   
#>  9  2018          2671 <dbl [187 × 187]> TRUE           drop                   
#> 10  2021          2582 <dbl [187 × 187]> TRUE           drop                   
```
