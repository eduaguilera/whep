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

  Under **every** method, an item that survives this step whose tonnes
  are not masses is refused; see the *Items with no CBS row* section.

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

Those figures are not masses, and that has now been traced to the
FAOSTAT source (whep#1023). CBS item 5001 is fed by FAOSTAT trade item
1293 (*Crude organic material n.e.c.*), for which FAOSTAT's aggregate
*Trade: Crops and livestock products* domain publishes a value but no
country-level mass, while its Detailed Trade Matrix reports tonnages
worth USD 0.01-0.5 per tonne whose mirrored report of the same flow
disagrees by factors of 356 to 838,000. What the large side counts is
unverified and the implied units per tonne are not constant, so nothing
can be rescaled. See
[`build_detailed_trade()`](https://eduaguilera.github.io/whep/reference/build_detailed_trade.md)'s
*Quantities FAOSTAT does not back with a mass* section for the full
measurement.

This function therefore **aborts** with class
`"whep_unbacked_mass_trade"` whenever such an item would survive this
step, rather than distributing 2.58 Gt through a matrix. The test is on
what is *kept*, under every method, and not on whether the item has a
CBS row: those two coincide on today's data, and that coincidence was
the only thing keeping the figures out of a published number. Give item
5001 a CBS row and the earlier, `"keep"`-only refusal let the whole
12.40 Gt through on the default method, silently.

No published number moves. Measured on the live `bilateral_trade` pin
`20250714T123347Z-2c392`, item 5001 carries 12.40 Gt of `tonnes` over
277,201 rows - 8.9% of 1986-2003, **49.4% of 2004-2013** and 3.9% of
2014-2021 - and no commodity balance sheet carries item 5001, so
`"drop"` removes all of it exactly as before.

A caller who wants a trade matrix that carries item 5001 has to obtain a
mass for it first.
[`build_detailed_trade()`](https://eduaguilera.github.io/whep/reference/build_detailed_trade.md)
screens the same rows at the producer, where the fix belongs; that
screen is latent until the `bilateral_trade` pin is regenerated from it,
which whep#1122 tracks.

## The two batch pins on the build path

Two further aliases were published in that same 2025-07-14 batch —
`crop_residues` and `bilateral_trade`, alongside the four above, between
12:33:43Z and 12:33:50Z. Unlike the four, these two are read on the
default build path, which is why no warning is attached to them: every
commodity balance build reads them, so a warning at the read is noise
rather than information.

They do **not** share a provenance despite sharing a timestamp. Each was
established separately (#1054), because the timestamp alone establishes
nothing.

### `crop_residues` is predecessor output

Read by
[`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md),
and from there by
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md).
All 475,688 of its `Product` rows equal the `primary_prod` pin's
`tonnes` values exactly — no key unmatched on either side, no value
differing at a relative tolerance of 1e-6 — so it is a downstream
artifact of the same predecessor run, carrying that run's production
series into the commodity balance.

Its residue quantities are those production numbers times a
residue-to-product ratio that varies by year and does not exist in this
repository: 100 of its 116 `Name_biomass` items carry between 77 and 252
distinct ratios across 1961–2021 (the 16 that carry one flat ratio are
all fodder items), 33,411 area-item-year keys carry a residue of exactly
0, and `biomass_coefs$kg_residue_kg_product_FM` reproduces only 3,189 of
the 472,790 keys where the comparison can be made. The ratios are
therefore not recoverable here, and the artifact is not reproducible
from this package.

It is not a small input.
[`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md)
supplies 7.63 Gt to the 2010 commodity balance (Straw 3.60 Gt, Other
crop residues 2.49 Gt, Firewood 1.54 Gt) and 327.7 Gt over 1961–2021,
and 3,998 of its 249,095 output rows carry `NA` polity columns because
the pin is name-keyed.

Driving the same residue model off a fresh
[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
would move those numbers. Measured for 2010 at (`area_code`,
`item_prod`): 1.958 Gt of current production sits on 6,425 keys the pin
never sees (809 Mt of it primary crops, including the modelled
temporary-grassland item), 64.8 Mt on 296 keys is pin-only, and of the
8,010 shared keys 538 disagree, putting 1.102 Gt — 11.01% of the pin's
shared-key mass — more than 1% apart. The largest class is rice, 673.9
Mt in the pin against 463.3 Mt fresh, a ratio of 0.6876: current code
puts rice on a milled-equivalent basis while the pin's is paddy, which
is a basis difference rather than an error, since straw scales with the
field crop. The second is fodder, where the current build is close to
twice the pin on every forage and silage item, the pin predating that
work. Replacing the pin therefore means choosing a residue model, which
is a science decision and not a refresh.

### `bilateral_trade` is a curated FAOSTAT input

Read by `get_bilateral_trade()` and by the live-animal branch of
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md).
Its values are the FAOSTAT Detailed Trade Matrix, not model output:
against the `faostat-trade-bilateral` pin over 2010, all 296,642 shared
`tonnes` keys and all 5,102 shared `Head` keys agree exactly, with not
one key differing. Over the full 1986–2021 span it carries
83,147,095,412 tonnes against FAOSTAT's 83,159,972,741 on CBS-mapped
quantity rows, and 11,707,083,640 head against 11,708,244,416.

The only transformations are an aggregation of FAOSTAT trade items onto
CBS item names, a fold of 18 FAOSTAT areas into area code 999 (Bhutan,
Comoros, Cook Islands, Equatorial Guinea, Faroe Islands, Marshall
Islands, Micronesia, Nauru, New Caledonia, North Macedonia, Niue,
Seychelles, Eswatini, Syrian Arab Republic, China Taiwan Province of,
Tonga, Tuvalu and Palestine — 3.18% of its rows, 1.892% of its tonnage,
0.492% of its head counts), and a `Country_share` column the reader
discards. So the shared timestamp implies nothing about it: what it
holds is what FAOSTAT published, through the filters its producer
applied.

Two things it does not hold. FAOSTAT's `1000 Head` rows are absent
entirely — 89,073 rows and 76,141,882 thousand head over 1986–2021,
against the 11,707,083,640 head the pin does carry, all of it live
broiler chicken, turkey, duck, rabbit and goose trade — and so are its
5,011 `No` rows.
[`build_detailed_trade()`](https://eduaguilera.github.io/whep/reference/build_detailed_trade.md)
recovers the first of those since PR \#1113: run on the raw
`faostat-trade-bilateral` pin today it emits 87,840,869,640 head, 7.50x
what this pin carries, against 64,450,133,319 tonnes, 22.5% below it.
That gain is latent until this pin is regenerated from the producer
(#1122), which is a schema migration rather than a refresh: the two
shapes share exactly one column name, `area_code`.
`.clean_bilateral_trade()` reads either.

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
