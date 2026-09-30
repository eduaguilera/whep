#' Bilateral trade data
#'
#' @description
#' Reports trade between pairs of countries in given years.
#'
#' @param example If `TRUE`, return a small example output without
#'   downloading remote data. Default is `FALSE`.
#' @param cbs Optional pre-computed wide CBS tibble from
#'   [get_wide_cbs()]. If `NULL` (default), it is built internally.
#' @param method_items_not_in_cbs How to treat a traded item whose
#'   `item_cbs_code` has no commodity balance sheet row to balance it
#'   against. See the *Items with no CBS row* section. One of:
#'   - `"drop"` (default): discard those flows, the historical
#'     behaviour. Published values are unaffected by this argument's
#'     existence as long as the default is kept.
#'   - `"keep"`: keep the flows and take the row and column margins from
#'     the reported bilateral data itself instead of from the CBS.
#'   - `"abort"`: fail, so that a refreshed pin cannot introduce
#'     unanchored items unnoticed.
#'
#'   Under **every** method, an item that survives this step whose tonnes
#'   are not masses is refused; see the *Items with no CBS row* section.
#'
#'   `example = TRUE` always returns the `"drop"` fixture.
#' @param method_seed_unit Which reported bilateral rows seed each trade
#'   matrix before it is balanced. See the *Which unit seeds a matrix*
#'   section. One of:
#'   - `"target"` (default): seed each (year, item) matrix from the rows
#'     reported in the unit of the CBS margins it is balanced onto, i.e.
#'     head counts for the live-animal rows of the livestock balance and
#'     tonnes for everything else.
#'   - `"tonnes"`: seed every matrix from its tonnes rows only, the
#'     behaviour before whep#1031. A live-animal (year, item) with no
#'     tonnes rows gets no matrix at all.
#'
#'   `example = TRUE` always returns the `"target"` fixture.
#'
#' @section Which unit seeds a matrix:
#' Each matrix is balanced onto the CBS `export`/`import` margins of its
#' (year, item), so it is denominated in the `unit` of those margins: tonnes
#' for the FAO-style balance and **head counts** for the live-animal rows
#' that [get_livestock_cbs()] adds, whose margins are summed from exactly the
#' `Head` rows of the same bilateral data. The seed only supplies the partner
#' structure; iterative proportional fitting imposes the level. The choice is
#' therefore which observed rows supply that structure (whep#1031).
#'
#' `"tonnes"` keeps the mass rows alone. On the `bilateral_trade` pin
#' `20250714T123347Z-2c392`, FAOSTAT reports the live-animal items in `Head`
#' only up to 2013, so 283 of the pin's 4,589 (year, item) groups, over 11
#' items and 1986-2013, have no tonnes row. Such a group has no seed cell at
#' all, so it produces **no matrix**, and [build_io_model()] then treats the
#' item as wholly domestically sourced (`.item_trade_shares()` returns the
#' identity for an item with no matrix). In 2010 that is all 9 live-animal
#' items with CBS trade: 62.9 million imported head with no country of
#' origin. Over 2014-2021 a further 86 groups carry both units; their
#' structure then comes from the tonnes rows while their margins are head
#' counts.
#'
#' `"target"` seeds each matrix in its margins' own unit, so seed and target
#' agree and the observed head-count partner structure is kept. Measured on
#' that pin against the 2010 and 2016 wide CBS:
#' - every tonnes-denominated matrix (110 in 2010, 111 in 2016) is
#'   identical under both methods;
#' - 2010: `"target"` returns 9 head-count matrices where `"tonnes"` returns
#'   none. Their export-weighted partner shares differ from the reported
#'   head flows by a total-variation distance of 0 to 0.096 (0.0001 for
#'   pigs, 0.0006 for sheep).
#' - 2016: both return the 10 head-count matrices with non-zero margins. The
#'   export-weighted total-variation distance between the two methods'
#'   partner shares is 0.11 (camels) to 0.82 (mules), 0.49 for pigs and 0.70
#'   for sheep, and the `"tonnes"` shares sit that far from the reported
#'   head flows while the `"target"` ones sit within 0.056.
#'
#' `"tonnes"` also returns 5 more 2016 matrices, all zero: chickens,
#' turkeys, rabbits, geese and rodents, whose CBS head margins are zero
#' because the pin carries their `1000 Head` rows nowhere (whep#1092). An
#' all-zero matrix carries no partner structure either, so `"target"` drops
#' them as seedless (with a warning) and nothing observed is lost.
#'
#' The seed is not only a structure. `.fill_missing_trade()` fills each
#' unreported cell from the margins, capped by the gap between an exporter's
#' margin and its reported row sum, and the balancing step then starts from
#' reported and estimated cells together. Both comparisons are made in the
#' margins' unit, so a tonnes seed under head-count margins leaves most of
#' each gap unfilled by what was reported and lets the estimate outweigh it.
#' In 2016 the pin's summed live-animal head counts exceed its summed tonnes
#' by 4.6 times for non-dairy cattle and 31 times for sheep, consistent with
#' how far the 2016 `"tonnes"` shares sit from the reported flows.
#'
#' Converting head counts to mass is deliberately not offered. It would put
#' the seed in tonnes under head-count margins, the mismatch just
#' described, unless the margins were converted as well, and converting them
#' needs a sourced live weight per species. The package holds none:
#' `gleam_animal_weights` is an unsourced placeholder (whep#881, whep#182).
#'
#' @section Items with no CBS row:
#' The bilateral trade matrices are balanced against the total exports and
#' imports reported in the commodity balance sheet, so an item with no CBS
#' supply/use row has nothing to balance against. Historically those flows
#' were discarded silently. Measured on the `bilateral_trade` pin
#' `20250714T123347Z-2c392` (after the export-preference deduplication and
#' the tonnes filter, i.e. exactly what reaches this step): 12.51 Gt of
#' 47.48 Gt, **26.4% of the traded tonnage over 1986-2021**, in 7 items, of
#' which 99.1% is the FABIO-style aggregate placeholder `"Other"`
#' (`item_cbs_code` 5001). The drop is now reported with
#' `cli::cli_warn()` whichever method is chosen, because a quarter of world
#' trade should not disappear without a message.
#'
#' The share is far from constant: 9.1% over 1986-2003, **49.6% over
#' 2004-2013** and 4.3% over 2014-2021. The middle block is not a real
#' trade signal. It contains physically impossible flows booked in tonnes:
#' Colombia to the United States, 2004, 2.58 Gt of `"Other"` in a single
#' cell, more than world cereal production; Kenya to the Netherlands,
#' 515-594 Mt/year over 2005-2009. Excluding item 5001 altogether, the
#' whole drop is 112 Mt, 0.24% of traded tonnage.
#'
#' Which treatment is right is therefore a methodological question, not a
#' lookup: `"Other"` is an unallocated residual whose 2004-2013 values are
#' demonstrably corrupt, so `"keep"` carries that corruption into the
#' output, and mapping the residual onto real CBS items would need a
#' sourced disaggregation key that does not exist in the package. Nothing
#' downstream consumes the kept rows yet either: [build_io_model()] takes
#' its item dimension from supply-use and the CBS, so an item absent from
#' both is ignored by `.build_trade_shares()` regardless of this argument.
#'
#' Those figures are not masses, and that has now been traced to the
#' FAOSTAT source (whep#1023). CBS item 5001 is fed by FAOSTAT trade item
#' 1293 (*Crude organic material n.e.c.*), for which FAOSTAT's aggregate
#' *Trade: Crops and livestock products* domain publishes a value but no
#' country-level mass, while its Detailed Trade Matrix reports tonnages
#' worth USD 0.01-0.5 per tonne whose mirrored report of the same flow
#' disagrees by factors of 356 to 838,000. What the large side
#' counts is unverified and the implied units per tonne are not constant,
#' so nothing can be rescaled. See
#' [build_detailed_trade()]'s *Quantities FAOSTAT does not back with a
#' mass* section for the full measurement.
#'
#' This function therefore **aborts** with class
#' `"whep_unbacked_mass_trade"` whenever such an item would survive this
#' step, rather than distributing 2.58 Gt through a matrix. The test is on
#' what is *kept*, under every method, and not on whether the item has a
#' CBS row: those two coincide on today's data, and that coincidence was
#' the only thing keeping the figures out of a published number. Give item
#' 5001 a CBS row and the earlier, `"keep"`-only refusal let the whole
#' 12.40 Gt through on the default method, silently.
#'
#' No published number moves. Measured on the live `bilateral_trade` pin
#' `20250714T123347Z-2c392`, item 5001 carries 12.40 Gt of `tonnes` over
#' 277,201 rows - 8.9% of 1986-2003, **49.4% of 2004-2013** and 3.9% of
#' 2014-2021 - and no commodity balance sheet carries item 5001, so
#' `"drop"` removes all of it exactly as before.
#'
#' A caller who wants a trade matrix that carries item 5001 has to obtain
#' a mass for it first. [build_detailed_trade()] screens the same rows at
#' the producer, where the fix belongs; that screen is latent until the
#' `bilateral_trade` pin is regenerated from it, which whep#1122 tracks.
#'
#' @returns
#' A tibble with the reported trade between countries. For efficient
#' memory usage, the tibble is not exactly in tidy format.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `item_cbs_code`: FAOSTAT internal code for the item that is being traded.
#'   For code details see e.g. `add_item_cbs_name()`.
#' - `bilateral_trade`: Square matrix of `NxN` dimensions where `N` is the
#'   total number of countries being considered. The matrix row and column
#'   names are exactly equal and they represent country codes.
#'   - Row name: The code of the country where the data is from. For code
#'    details see e.g. `add_area_name()`.
#'   - Column name: FAOSTAT internal code for the country that is importing the
#'     item. See row name explanation above.
#'
#'   If `m` is the matrix, the value at `m["A", "B"]` is the trade from
#'   country `"A"` to country `"B"`, for the corresponding year and item, in
#'   the row's `unit` (tonnes, or head counts for live animals).
#'   The matrix can be considered _balanced_. This means:
#'   - The sum of all values from row `"A"`, where `"A"` is any country,
#'     should match the total exports from country `"A"` reported in the
#'     commodity balance sheet (which is considered more accurate for totals).
#'   - The sum of all values from column `"A"`, where `"A"` is any country,
#'     should match the total imports into country `"A"` reported in the
#'     commodity balance sheet (which is considered more accurate for totals).
#'
#'   The sums may not be exactly the expected values because of precision
#'   issues and/or the iterative proportional fitting algorithm not converging
#'   fast enough, but should be relatively very close to the desired totals.
#' - `has_cbs_totals`: `TRUE` when the matrix margins came from the
#'   commodity balance sheet, `FALSE` when the item had no CBS row for
#'   that year and its margins were taken from the reported bilateral
#'   flows themselves. Always `TRUE` unless
#'   `method_items_not_in_cbs = "keep"`.
#' - `method_items_not_in_cbs`: the treatment chosen for items with no
#'   CBS row, recorded so a downstream consumer can tell which variant it
#'   is holding.
#' - `unit`: the denomination of the matrix values, taken from the CBS
#'   margins it was balanced onto: `"tonnes"`, or `"heads"` for the live
#'   animals. A matrix balanced onto its own reported flows
#'   (`has_cbs_totals == FALSE`) is seeded from, and so denominated in,
#'   tonnes.
#' - `method_seed_unit`: the seed-unit method chosen, see
#'   `method_seed_unit`.
#'
#'  The step by step approach to obtain this data tries to follow the FABIO
#'  model and is explained below. All the steps are performed separately for
#'  each group of year and item.
#'  - From the FAOSTAT reported bilateral trade, there are sometimes two values
#'    for one trade flow: the exported amount claimed by the reporter country
#'    and the import amount claimed by the partner country. Here, the export
#'    data was preferred, i.e., if country `"A"` says it exported `X` tonnes to
#'    country `"B"` but country `"B"` claims they got `Y` tonnes from country
#'    `"A"`, we trust the export data `X`. This choice is only needed if there
#'    exists a reported amount from both sides. Otherwise, the single existing
#'    report is chosen.
#'  - Complete the country data, that is, add any missing combinations of
#'    country trade with NAs, which will be estimated later. In the matrix
#'    form, this doesn't increase the memory usage since we had to build a
#'    matrix anyway (for the balancing algorithm), and the _empty_ parts also
#'    take up memory. This is also done for total imports/exports from the
#'    commodity balance sheet, but these are directly filled with 0s instead.
#'  - The total imports and exports from the commodity balance sheet are
#'    balanced by downscaling the largest of the two to match the lowest.
#'    This is done in the following way:
#'    - If `total_imports > total_exports`: Set `import` as
#'      `total_exports * import / total_import`.
#'    - If `total_exports > total_exports`: Set `export` as
#'      `total_exports * export / total_export`.
#'  - The missing data in the matrix must be estimated. It's done like this:
#'    - For each pair of exporter `i` and importer `j`, we estimate a bilateral
#'      trade `m[i, j]` using the export shares of `i` and import shares of `j`
#'      from the commodity balance sheet:
#'        - `est_1 <- exports[i] * imports[j] / sum(imports)`, i.e., total
#'          exports of country `i` spread among other countries' import shares.
#'        - `est_2 <- imports[j] * exports[i] / sum(exports)`, i.e. total
#'          imports of country `j` spread among other countries' export shares.
#'        - `est <- (est_1 + est_2) / 2`, i.e., the mean of both estimates.
#'
#'      In the above computations, exports and imports are the original values
#'      before they were balanced.
#'    - The estimates for data that already existed (i.e. non-NA) are discarded.
#'      For the ones left, for each row (i.e. exporter country), we get the
#'      difference between its balanced total export and the sum of original
#'      non-estimated data. The result is the _`gap`_ we can actually fill with
#'      estimates, so as to not get past the reported total export. If the sum
#'      of non-discarded estimates is larger, it must be downscaled and spread
#'      by computing
#'      `gap * non_discarded_estimate / sum(non_discarded_estimates)`.
#'    - The estimates are divided by a _trust factor_, in the sense that we
#'      don't rely on the whole value, thinking that a non-present value might
#'      actually be because that specific trade was 0, so we don't overestimate
#'      too much. The chosen factor is 10%, so only 10% of the estimate's value
#'      is actually used to fill the NA from the original bilateral trade
#'      matrix.
#'  - The matrix is balanced, as mentioned before, using the
#'    [iterative proportional fitting algorithm](
#'      https://en.wikipedia.org/wiki/Iterative_proportional_fitting
#'    ). The target sums for rows and columns are respectively the balanced
#'    exports and imports computed from the commodity balance sheet.
#'
#' @inheritSection whep_read_file The two batch pins on the build path
#'
#' @export
#'
#' @examples
#' get_bilateral_trade(example = TRUE)
get_bilateral_trade <- function(
  example = FALSE,
  cbs = NULL,
  method_items_not_in_cbs = c("drop", "keep", "abort"),
  method_seed_unit = c("target", "tonnes")
) {
  method <- rlang::arg_match(method_items_not_in_cbs)
  seed_method <- rlang::arg_match(method_seed_unit)

  if (example) {
    return(.example_get_bilateral_trade())
  }

  if (is.null(cbs)) {
    cbs <- get_wide_cbs()
  }
  # `unit` is kept when present and checked in `.cbs_margin_units()`, which
  # aborts naming the missing column rather than letting `select()` do it.
  cbs <- cbs |>
    dplyr::select(
      year,
      item_cbs_code,
      area_code,
      dplyr::any_of("unit"),
      export,
      import
    )

  cli::cli_progress_step("Reading raw bilateral trade data")
  # The `bilateral_trade` pin shares the predecessor pipeline's 2025-07-14
  # timestamp but not its provenance: every value it holds is the FAOSTAT
  # Detailed Trade Matrix, matching the raw `faostat-trade-bilateral` pin
  # exactly. What it adds is the CBS item aggregation and a fold of 18 areas
  # into code 999; what it drops is FAOSTAT's `1000 Head` and `No` rows. See
  # the pin-batch section above for the figures (#1054).
  btd <- "bilateral_trade" |>
    whep_read_file() |>
    .clean_bilateral_trade()

  codes <- .get_all_country_codes(btd, cbs)

  cli::cli_progress_step(
    "Balancing trade matrices ({nrow(btd)} year-item groups)"
  )
  btd |>
    .nest_by_year_item_code(cbs, codes, method, seed_method) |>
    .process_bilateral_trade(codes) |>
    dplyr::select(-total_trade) |>
    dplyr::mutate(method_items_not_in_cbs = method) |>
    dplyr::relocate(unit, method_seed_unit, .after = dplyr::last_col())
}

.process_bilateral_trade <- function(btd, codes) {
  n <- length(codes)
  code_int <- as.integer(levels(codes))
  ngroups <- nrow(btd)
  # mclapply() forks, which is unavailable on Windows, and R CMD check caps
  # forked workers at two. `.parallel_workers()` applies both constraints,
  # here and in spatialize.R. Each group is balanced independently and
  # mclapply preserves input order, so the output is identical at any
  # worker count -- asserted in test_bilateral_trade.R.
  n_cores <- .parallel_workers()

  btd$bilateral_trade <- parallel::mclapply(
    seq_len(ngroups),
    function(i) {
      btd$bilateral_trade[[i]] |>
        .build_trade_matrix(n, code_int) |>
        .fill_missing_trade(btd$total_trade[[i]]) |>
        .balance_matrix(btd$total_trade[[i]])
    },
    mc.cores = n_cores
  )
  btd
}

.balance_matrix <- function(trade_matrix, total_trade) {
  targets <- .trade_targets(total_trade, trade_matrix)
  exports <- targets$balanced_export
  imports <- targets$balanced_import
  n <- length(exports)

  if (sum(exports) == 0 && sum(imports) == 0) {
    # Keep the country dimnames here too: the documented contract is name
    # indexing (`m["A", "B"]`), so the zero path must stay indexable.
    return(matrix(
      0,
      nrow = n,
      ncol = n,
      dimnames = dimnames(trade_matrix)
    ))
  }

  # Only run IPF on active countries to reduce matrix size.
  # Inactive countries (0 export and 0 import) would be
  # forced to 0 by IPF anyway, so excluding them gives the
  # same result with much less computation.
  active <- which(exports > 0 | imports > 0)
  sub <- trade_matrix[active, active, drop = FALSE]
  sub[sub == 0] <- 1
  # RAS/IPF scales multiplicatively, so a cell that is genuinely 0 going in
  # stays 0 through every iteration; but the seeding line above treats the
  # diagonal (self-trade, always 0) like any other unobserved zero, which
  # would let IPF allocate a spurious i -> i flow to hit the row/column
  # totals. Re-zero it so self-trade can never re-enter the balanced matrix.
  sub <- .zero_diagonal(sub)
  sub <- .ipf_2d(sub, exports[active], imports[active])

  result <- matrix(
    0,
    nrow = n,
    ncol = n,
    dimnames = dimnames(trade_matrix)
  )
  result[active, active] <- sub
  result
}

.balance_total_trade <- function(total_trade) {
  total_trade |>
    dplyr::mutate(
      total_export = sum(export),
      total_import = sum(import),
      balanced_export = ifelse(
        total_export > total_import,
        total_import * export / total_export,
        export
      ),
      balanced_import = ifelse(
        total_import > total_export,
        total_export * import / total_import,
        import
      )
    )
}

# Put either published shape of the `bilateral_trade` data onto the six
# columns the rest of this file works in.
#
# The alias has been published in two schemas that share exactly one column
# name, `area_code` (whep#1122):
#
# - the live pin `20250714T123347Z-2c392`, the predecessor pipeline's:
#   `Year, area_code, area_code_p, Element, item, Unit, Value, area, area_p,
#   Country_share` -- the partner keyed as `area_code_p`, the item carried as
#   a CBS item *name*, `Element` capitalised;
# - what `build_detailed_trade()`, this package's own producer, emits: the
#   partner as `area_code_partner`, the item already resolved to
#   `item_cbs_code`, `element` lower case, plus the polity columns.
#
# So the three columns that differ are resolved by what is present rather
# than by a pin version, and a frame carrying neither spelling aborts naming
# what it holds. Reading both is what lets the reader and the pin be updated
# in either order; without it, uploading a regenerated pin breaks
# `get_bilateral_trade()` and the live-animal branch of the commodity balance
# at the same moment.
.clean_bilateral_trade <- function(btd) {
  btd <- dplyr::rename_with(btd, tolower)
  partner <- .btd_partner_column(btd)
  btd$unit[btd$unit == "Head"] <- "heads"
  btd$element <- stringr::str_to_lower(btd$element)
  is_export <- btd$element == "export"
  from <- btd[[partner]]
  from[is_export] <- btd$area_code[is_export]
  to <- btd$area_code
  to[is_export] <- btd[[partner]][is_export]
  btd$from_code <- as.integer(from)
  btd$to_code <- as.integer(to)
  btd$year <- as.integer(btd$year)

  btd$item_cbs_code <- .btd_item_codes(btd)

  btd <- .prefer_flow_direction(btd, "export")
  btd[c("year", "from_code", "to_code", "item_cbs_code", "unit", "value")]
}

# The partner-country key, under whichever of the two spellings the data
# carries. `area_code_partner` wins when both are present, because that is
# the producer's own name for it.
.btd_partner_column <- function(btd) {
  partner <- intersect(c("area_code_partner", "area_code_p"), names(btd))
  if (length(partner) == 0) {
    cli::cli_abort(
      c(
        "Bilateral trade data has no partner-country column.",
        "i" = "Expected {.field area_code_partner} \\
               ({.fun build_detailed_trade}) or {.field area_code_p} \\
               (the 2025-07-14 pin).",
        "i" = "Columns present: {.field {names(btd)}}."
      ),
      class = "whep_btd_schema"
    )
  }
  partner[[1]]
}

# CBS item codes, from whichever item key the data carries. The producer
# resolves them itself, so they are taken as given; the name-keyed pin needs
# `.match_btd_item_codes()`, which warns on anything it cannot resolve.
#
# A code the producer left `NA` is dropped by the CBS inner join later on,
# exactly as an unresolved name is, so it is reported the same way rather
# than passed through silently.
.btd_item_codes <- function(btd) {
  if (rlang::has_name(btd, "item_cbs_code")) {
    codes <- as.integer(btd$item_cbs_code)
    .report_missing_btd_codes(codes)
    return(codes)
  }
  if (rlang::has_name(btd, "item")) {
    return(.match_btd_item_codes(btd$item))
  }
  cli::cli_abort(
    c(
      "Bilateral trade data has no item column.",
      "i" = "Expected {.field item_cbs_code} ({.fun build_detailed_trade}) \\
             or {.field item} (the 2025-07-14 pin).",
      "i" = "Columns present: {.field {names(btd)}}."
    ),
    class = "whep_btd_schema"
  )
}

.report_missing_btd_codes <- function(codes) {
  n_missing <- sum(is.na(codes))
  if (n_missing == 0) {
    return(invisible(codes))
  }
  cli::cli_warn(
    c(
      "{n_missing} bilateral trade row{?s} carr{?ies/y} no \\
       {.field item_cbs_code} and will be dropped.",
      "i" = "{.fun build_detailed_trade} drops its own unmapped items, so a \\
             missing code here means the data was not built by it."
    ),
    class = "whep_btd_item_code_missing"
  )
}

# Resolve the bilateral trade pin's `item` strings to CBS item codes.
# The pin ships pre-harmonized CBS item names: verified against the 20250714
# pin, all 146 distinct item-unit combinations (135 item names, 10,344,152
# rows, 94.85 Gt) resolve to a code, and `whep::items_cbs` is 1:1 between its
# 170 names and 170 codes, so the name match is both exact and unambiguous
# here. Unlike `build_trade.R`, this path must NOT go through
# `whep::cbs_trade_codes`: that crosswalk is keyed on raw FAOSTAT trade names
# (`item_trade`), a different vocabulary. Anything the match cannot resolve
# keeps `NA` and is silently dropped later by the CBS inner join, so warn
# rather than letting a refreshed pin lose rows without a trace.
.match_btd_item_codes <- function(item) {
  items <- .get_cbs_items("item", "item_cbs_code")
  codes <- items$item_cbs_code[match(item, items$item)]
  unmatched <- unique(item[is.na(codes)])

  if (length(unmatched) > 0) {
    n_rows <- sum(is.na(codes))
    cli::cli_warn(c(
      paste(
        "{length(unmatched)} bilateral trade item name{?s} did not match",
        "any {.field item_cbs_name}: {n_rows} row{?s} will be dropped."
      ),
      i = "Unmatched: {.val {unmatched}}.",
      i = paste(
        "The {.val bilateral_trade} pin is expected to carry CBS item",
        "names. If it now carries raw FAOSTAT trade names, route them",
        "through {.code whep::cbs_trade_codes} as {.file R/build_trade.R}",
        "does."
      )
    ))
  }

  codes
}

# Keep all rows with preferred direction (Import, Export)
# when both of them exist. Otherwise use the one present.
.prefer_flow_direction <- function(bilateral_trade, direction) {
  is_preferred <- bilateral_trade$element == direction
  key <- bilateral_trade$from_code +
    bilateral_trade$to_code * 1e3 +
    bilateral_trade$year * 1e6 +
    bilateral_trade$item_cbs_code * 1e10
  has_preferred <- key %in% key[is_preferred]
  bilateral_trade[is_preferred | !has_preferred, ]
}

.fill_missing_trade <- function(trade_matrix, total_trade) {
  targets <- .trade_targets(total_trade, trade_matrix)
  exports <- targets$export
  imports <- targets$import
  balanced_exports <- targets$balanced_export

  na_mask <- is.na(trade_matrix)
  estimate <- .estimate_bilateral_trade(exports, imports)
  estimate[!na_mask] <- 0

  balances <- balanced_exports -
    .rowSums(trade_matrix, nrow(trade_matrix), ncol(trade_matrix), na.rm = TRUE)
  balances <- pmax(balances, 0)

  estimate <- .downscale_estimate_matrix(estimate, balances)

  # According to FABIO, missing data may be because it's truly zero,
  # so only use a small ratio of the estimate just in case.
  # TODO: Adapt this to our needs
  k_trust_factor <- 0.1
  trade_matrix[na_mask] <- estimate[na_mask] * k_trust_factor
  # The diagonal (a country trading with itself) starts NA from
  # .build_trade_matrix() and would otherwise be filled by the estimate above,
  # like any other missing cell -- but self-trade is never a real flow.
  .zero_diagonal(trade_matrix)
}

# Force the diagonal of a country x country trade matrix to exactly 0. A
# country never trades with itself; used both after estimating missing trade
# (.fill_missing_trade()) and after IPF's zero-seeding step (.balance_matrix())
# so self-trade can never re-enter the matrix.
.zero_diagonal <- function(trade_matrix) {
  diag(trade_matrix) <- 0
  trade_matrix
}

.trade_targets <- function(total_trade, trade_matrix) {
  use_names <- rlang::has_name(total_trade, "area_code") &&
    !is.null(rownames(trade_matrix)) &&
    !is.null(colnames(trade_matrix))

  if (!use_names) {
    return(list(
      export = total_trade$export,
      import = total_trade$import,
      balanced_export = total_trade$balanced_export,
      balanced_import = total_trade$balanced_import
    ))
  }

  area_codes <- as.character(total_trade$area_code)
  row_idx <- match(rownames(trade_matrix), area_codes)
  col_idx <- match(colnames(trade_matrix), area_codes)

  list(
    export = .target_by_index(total_trade$export, row_idx),
    import = .target_by_index(total_trade$import, col_idx),
    balanced_export = .target_by_index(
      total_trade$balanced_export,
      row_idx
    ),
    balanced_import = .target_by_index(
      total_trade$balanced_import,
      col_idx
    )
  )
}

.target_by_index <- function(values, idx) {
  result <- rep(0, length(idx))
  valid <- !is.na(idx)
  result[valid] <- values[idx[valid]]
  result[is.na(result)] <- 0
  result
}

.downscale_estimate_matrix <- function(needed_estimates, balances) {
  nr <- nrow(needed_estimates)
  nc <- ncol(needed_estimates)
  row_sums <- .rowSums(needed_estimates, nr, nc, na.rm = TRUE)
  scale <- rep.int(1, nr)
  needs_scale <- row_sums > 0 & row_sums > balances
  scale[needs_scale] <- balances[needs_scale] / row_sums[needs_scale]
  needed_estimates * scale
}

.nest_by_year_item_code <- function(
  btd,
  cbs,
  codes,
  method = "drop",
  seed_method = "target"
) {
  margin_units <- .cbs_margin_units(cbs)
  cbs <- cbs |>
    dplyr::select(-unit) |>
    dplyr::mutate(area_code = factor(area_code, levels = codes))

  # The CBS-item filter is a row filter on the item code alone, so it can run
  # before the unit filter without changing what survives both. Doing it first
  # scopes the two warnings below to the items that would actually have
  # reached a matrix.
  #
  # The unit step is `.normalise_trade_units()` rather than a bare
  # `unit %in% c("tonnes", "heads")` filter so that a label outside that pair
  # cannot leave without a word, as FAOSTAT's `1000 Head` did everywhere else
  # in the trade chain (whep#1092). On the current `bilateral_trade` pin,
  # which carries only `tonnes` and `Head`, it is a no-op. Which of those
  # units *seeds* each matrix is `seed_method` (whep#1031).
  in_cbs <- btd |>
    .normalise_trade_units() |>
    .filter_only_items_in_cbs(cbs, method)

  in_cbs |>
    .seed_bilateral_trade(margin_units, seed_method) |>
    .warn_seedless_trade_groups(in_cbs) |>
    tidyr::nest(
      bilateral_trade = c(from_code, to_code, value),
      .by = c(year, item_cbs_code)
    ) |>
    .attach_total_trade(cbs, codes, method) |>
    .attach_matrix_unit(margin_units, seed_method)
}

# One unit per (year, item) of the CBS margins, the unit each matrix is
# balanced onto and therefore denominated in. `get_wide_cbs()` labels every
# row since whep#1055; a frame without the label, or with two units for one
# (year, item), cannot say what its matrix would be counting.
.cbs_margin_units <- function(cbs) {
  cbs |>
    .abort_if_units_mixed(
      "CBS trade margins",
      key_cols = c("year", "item_cbs_code")
    ) |>
    dplyr::distinct(year, item_cbs_code, margin_unit = unit)
}

# Reduce the bilateral rows to the ones that seed a matrix, and drop `unit`.
#
# - `"tonnes"`: the mass rows only, whatever the margins count.
# - `"target"`: the rows in the unit of the margins the matrix is balanced
#   onto. A (year, item) with no CBS row has no margins to take a unit from;
#   it is either dropped by the inner join in `.attach_total_trade()` or,
#   under `method_items_not_in_cbs = "keep"`, balanced onto its own seed, and
#   there the documented denomination is tonnes, so tonnes seed it.
.seed_bilateral_trade <- function(btd, margin_units, seed_method) {
  if (seed_method == "tonnes") {
    return(.mass_only_bilateral_trade(btd))
  }

  seeded <- btd |>
    dplyr::left_join(margin_units, dplyr::join_by(year, item_cbs_code)) |>
    dplyr::mutate(margin_unit = dplyr::coalesce(margin_unit, "tonnes"))
  .warn_off_margin_unit_rows(dplyr::filter(seeded, unit != margin_unit))

  seeded |>
    dplyr::filter(unit == margin_unit) |>
    dplyr::select(-unit, -margin_unit)
}

# Say what the `"target"` seed leaves out: rows reported in a unit other than
# the one their matrix is balanced onto. On the 20250714 pin these are the
# tonnes rows FAOSTAT adds for live animals from 2014, whose matrices are
# balanced onto head counts.
.warn_off_margin_unit_rows <- function(off) {
  if (nrow(off) == 0) {
    return(invisible(NULL))
  }
  by_unit <- off |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(unit, margin_unit)
    ) |>
    dplyr::mutate(
      label = paste0(signif(value, 4), " ", unit, " vs ", margin_unit)
    )
  items <- length(unique(off$item_cbs_code))
  cli::cli_warn(c(
    "Dropped {nrow(off)} bilateral trade row{?s} not in the unit of the \\
     CBS margins {?its/their} matrix is balanced onto.",
    "i" = "Reported vs margin unit: {by_unit$label}.",
    "i" = "{items} CBS item{cli::qty(items)}{?s} affected; each matrix is \\
           seeded in its margins' own unit ({.arg method_seed_unit} = \\
           {.val target})."
  ))
}

# Record what each matrix counts. It is balanced onto its CBS margins, so it
# takes their unit under either seed method; a matrix balanced onto its own
# flows (`has_cbs_totals == FALSE`) was seeded from tonnes.
.attach_matrix_unit <- function(nested, margin_units, seed_method) {
  nested |>
    dplyr::left_join(margin_units, dplyr::join_by(year, item_cbs_code)) |>
    dplyr::mutate(
      unit = dplyr::if_else(has_cbs_totals, margin_unit, "tonnes"),
      method_seed_unit = seed_method
    ) |>
    dplyr::select(-margin_unit)
}

# Attach the CBS export/import margins each trade matrix is balanced against.
# The inner join is what actually removes an item with no CBS row, and it is
# stricter than `.filter_only_items_in_cbs()`: it keys on (year, item), so it
# also removes a CBS item in a year the CBS does not cover. Under
# `method = "keep"` those (year, item) groups are kept instead, with margins
# derived from their own reported flows -- see `.own_margin_totals()`.
.attach_total_trade <- function(nested, cbs, codes, method) {
  nested_cbs <- .get_nested_cbs(cbs, codes)
  anchored <- nested |>
    dplyr::inner_join(nested_cbs, dplyr::join_by(year, item_cbs_code)) |>
    dplyr::mutate(has_cbs_totals = TRUE)

  if (method != "keep") {
    return(anchored)
  }

  unanchored <- nested |>
    dplyr::anti_join(nested_cbs, dplyr::join_by(year, item_cbs_code)) |>
    dplyr::mutate(
      total_trade = purrr::map(bilateral_trade, .own_margin_totals, codes),
      has_cbs_totals = FALSE
    )

  dplyr::bind_rows(anchored, unanchored) |>
    dplyr::arrange(year, item_cbs_code)
}

# Margins for an item the CBS cannot anchor: use the item's own reported
# bilateral flows as its total exports (row sums) and imports (column sums).
# This is the honest choice available without inventing a CBS total -- it
# leaves the reported flows essentially untouched by the balancing step
# (row and column sums already agree by construction) while keeping the
# downstream code path identical to the anchored one. It is *not* a
# statement that the totals are right; `has_cbs_totals` records that they
# were self-derived.
.own_margin_totals <- function(flows, codes) {
  exports <- flows |>
    dplyr::summarise(export = sum(value, na.rm = TRUE), .by = from_code)
  imports <- flows |>
    dplyr::summarise(import = sum(value, na.rm = TRUE), .by = to_code)

  tibble::tibble(area_code = codes) |>
    dplyr::mutate(area_code_int = as.integer(as.character(area_code))) |>
    dplyr::left_join(exports, dplyr::join_by(area_code_int == from_code)) |>
    dplyr::left_join(imports, dplyr::join_by(area_code_int == to_code)) |>
    dplyr::select(-area_code_int) |>
    tidyr::replace_na(list(export = 0, import = 0)) |>
    .balance_total_trade()
}

# Reduce bilateral trade to its mass (tonnes) rows and drop `unit`, matching
# the documented `get_bilateral_trade()` contract: `m["A", "B"]` is trade in
# tonnes. Without this, `.build_trade_matrix()` sums `value` over
# `(from_code, to_code)` with no unit dimension at all, so a head-count row
# (live animals) is added straight into the tonnes column -- the same
# collapse PR #925 fixed for `.aggregate_fao_trade_to_cbs()` in build_cbs.R,
# on the same day, in a different file (whep#962). Measured on the real pin:
# 11 dual-unit items, 1.08 billion head summed into 71.0 Mt across 25.2% of
# the matrix's cells. IPF renormalises row/column totals afterwards, so the
# level comes out right and only the partner allocation is silently wrong --
# which is why this stayed invisible. Dropped rows are warned about, not
# silently discarded.
.mass_only_bilateral_trade <- function(btd) {
  non_mass <- btd |> dplyr::filter(unit != "tonnes")
  if (nrow(non_mass) > 0) {
    units <- sort(unique(non_mass$unit))
    items <- length(unique(non_mass$item_cbs_code))
    cli::cli_warn(c(
      "Dropped {nrow(non_mass)} bilateral trade row{?s} not \\
       denominated in mass.",
      "i" = "Unit{cli::qty(length(units))}{?s}: {.val {units}}.",
      "i" = "{items} CBS item{cli::qty(items)}{?s} affected, totalling \\
             {round(sum(non_mass$value, na.rm = TRUE))}.",
      "i" = "Live-animal trade is in head counts; the bilateral trade \\
             matrix is denominated in tonnes."
    ))
  }
  btd |>
    dplyr::filter(unit == "tonnes") |>
    dplyr::select(-unit)
}

# A seed-unit filter does not merely trim a mixed cell: where a whole
# year-item group has no row in the kept unit it empties that group outright,
# and the inner join in `.nest_by_year_item_code()` then drops the group from
# the result. Under `seed_method = "tonnes"`, on the 20250714 pin, that is 283
# groups over 11 live-animal items and 1986-2013, carrying 80,104 observed
# seed cells and 7.95 bn head; FAOSTAT reports those items in Head only up to
# 2013. No matrix means `build_io_model()` sources the item wholly
# domestically (`.item_trade_shares()` returns the identity).
#
# Those cells are not a unit mixup: the items are balanced onto head-count
# margins derived by `get_livestock_cbs()` from exactly those rows, which is
# why `"target"` (whep#1031) keeps them. Under `"target"` a group empties only
# when no row is in its margins' unit, which in 2010 and 2016 happened only for
# items whose head margins are zero. Either way the loss is warned about, so
# nobody has to rediscover a gap from a row count.
.warn_seedless_trade_groups <- function(mass, all_units) {
  lost <- all_units |>
    dplyr::distinct(year, item_cbs_code) |>
    dplyr::anti_join(
      dplyr::distinct(mass, year, item_cbs_code),
      by = c("year", "item_cbs_code")
    )

  if (nrow(lost) > 0) {
    items <- sort(unique(lost$item_cbs_code))
    years <- range(lost$year)
    cli::cli_warn(c(
      "{nrow(lost)} year-item trade matri{?x/ces} lost every seed cell to \\
       the seed-unit filter and {?is/are} absent from the result.",
      "i" = "{length(items)} CBS item{?s}: {.val {items}}, \\
             {years[[1]]}-{years[[2]]}.",
      "i" = "No reported flow is in the unit this seed keeps, so their \\
             observed partner structure is lost with them. \\
             {.arg method_seed_unit} = {.val target} seeds a \\
             head-count matrix from its head rows (whep#1031)."
    ))
  }

  mass
}

.get_nested_cbs <- function(cbs, codes) {
  cbs |>
    .complete_total_trade(codes) |>
    dplyr::group_by(year, item_cbs_code) |>
    .balance_total_trade() |>
    dplyr::ungroup() |>
    tidyr::nest(
      total_trade = c(
        area_code,
        export,
        import,
        balanced_export,
        balanced_import
      ),
      .by = c(year, item_cbs_code)
    )
}

.complete_total_trade <- function(total_trade, codes) {
  df_codes <- tibble::tibble(area_code = codes)
  combs <- total_trade |>
    dplyr::distinct(year, item_cbs_code) |>
    dplyr::cross_join(df_codes)

  total_trade |>
    dplyr::right_join(combs, by = c("year", "item_cbs_code", "area_code")) |>
    tidyr::replace_na(list(export = 0, import = 0))
}

.filter_only_items_in_cbs <- function(btd, cbs, method = "drop") {
  btd_items <- btd |>
    dplyr::pull(item_cbs_code) |>
    unique() |>
    sort()

  cbs_items <- cbs |>
    dplyr::pull(item_cbs_code) |>
    unique() |>
    sort()

  items_not_in_cbs <- btd_items[!btd_items %in% cbs_items]

  # Refuse on what survives this step, not on what has no CBS row. Those two
  # sets coincide on today's data and that coincidence is the whole defect:
  # see the comment on `.refuse_unbacked_mass_items()`.
  kept_items <- if (method == "keep") btd_items else cbs_items
  .refuse_unbacked_mass_items(btd, intersect(btd_items, kept_items))

  if (length(items_not_in_cbs) == 0) {
    return(btd)
  }

  .report_items_not_in_cbs(btd, items_not_in_cbs, method)

  if (method == "keep") {
    return(btd)
  }

  btd |>
    dplyr::filter(!item_cbs_code %in% items_not_in_cbs)
}

# Refuse to build a matrix for an item whose `tonnes` are not masses. For the
# items `.unbacked_mass_cbs_items()` names they are not: FAOSTAT publishes no
# country-level mass for the trade items feeding them, and the matrix figures
# reach 2.58 Gt in one cell at USD 0.227/tonne (whep#1023). No conversion is
# derivable, so refusing is the only honest option left here; the fix itself
# belongs in the producer, `build_detailed_trade()`.
#
# `kept_items` is what survives `.filter_only_items_in_cbs()`, which is not
# the same question as which items have no CBS row. It used to be called with
# `items_not_in_cbs` on the `"keep"` branch alone, so the only thing keeping
# the live pin's 12.40 Gt of item 5001 out of a published number was that
# 5001 happens to have no CBS row and so was dropped by the default -- "luck,
# not design", as whep#1023 puts it. Give the item a CBS row and the old call
# site balanced it against CBS margins and pushed it through IPF in silence.
# Today no CBS carries 5001, so this changes nothing; it is the guard that
# makes the current safety a property of the code rather than of the data.
.refuse_unbacked_mass_items <- function(btd, kept_items) {
  unbacked <- intersect(kept_items, .unbacked_mass_cbs_items())
  if (length(unbacked) == 0) {
    return(invisible(btd))
  }

  # Scope to mass rows: this runs before `.mass_only_bilateral_trade()`, so
  # summing alongside `heads` would report head counts as tonnage.
  affected <- btd |>
    dplyr::filter(item_cbs_code %in% unbacked)
  if (rlang::has_name(affected, "unit")) {
    affected <- dplyr::filter(affected, unit == "tonnes")
  }
  tonnage <- sum(affected$value, na.rm = TRUE)

  cli::cli_abort(
    c(
      "Refusing to balance {nrow(affected)} kept row{?s} whose \\
       {.field tonnes} are not masses.",
      "i" = "CBS item {cli::qty(length(unbacked))}code{?s}: \\
             {.val {unbacked}}, {signif(tonnage, 4)} reported tonnes.",
      "i" = "FAOSTAT publishes no country-level mass for the trade \\
             item{?s} feeding {cli::qty(length(unbacked))}{?it/them}, and \\
             the true unit is unverified, so the values can be dropped but \\
             not corrected (whep#1023).",
      "i" = "Screen the rows at the producer with \\
             {.fun build_detailed_trade}, whose \\
             {.arg method_unbacked_quantity} {.val drop} removes them, and \\
             regenerate the {.val bilateral_trade} pin (whep#1122)."
    ),
    class = "whep_unbacked_mass_trade"
  )
}

# Say out loud how much trade has no CBS row to be balanced against. This used
# to be a bare TODO comment and no message at all, which hid a quarter of the
# pin's traded tonnage (whep#943): 12.51 Gt of 47.48 Gt over 1986-2021, 99.1%
# of it the FABIO-style aggregate placeholder "Other" (item_cbs_code 5001).
# The "abort" method turns the same finding into a hard failure, for a caller
# that would rather not build a matrix set at all than build one missing an
# item. See the "Items with no CBS row" section of get_bilateral_trade().
.report_items_not_in_cbs <- function(btd, items_not_in_cbs, method) {
  # Scope the tonnage to mass rows. This runs BEFORE
  # `.mass_only_bilateral_trade()` (see `.nest_by_year_item_code()`), so the
  # frame still carries `heads` rows, and summing them alongside `tonnes`
  # would report head counts as tonnage -- the same collapse whep#865 and
  # whep#962 fixed elsewhere. `unit` is absent when a caller has already
  # reduced to mass, in which case every row is mass.
  mass <- if (rlang::has_name(btd, "unit")) {
    dplyr::filter(btd, unit == "tonnes")
  } else {
    btd
  }
  affected <- mass |>
    dplyr::filter(item_cbs_code %in% items_not_in_cbs)
  total <- sum(mass$value, na.rm = TRUE)
  dropped <- sum(affected$value, na.rm = TRUE)
  share <- if (total > 0) 100 * dropped / total else 0
  n_items <- length(items_not_in_cbs)

  report <- c(
    "{n_items} bilateral trade item{?s} ha{?s/ve} no commodity balance \\
     sheet row to balance against.",
    "i" = "Item {cli::qty(n_items)}code{?s}: {.val {items_not_in_cbs}}.",
    "i" = "{nrow(affected)} row{?s}, {signif(dropped, 4)} of \\
           {signif(total, 4)} tonnes, {round(share, 1)} percent of the \\
           traded tonnage reaching this step.",
    "i" = "{.arg method_items_not_in_cbs} is {.val {method}}."
  )

  if (method == "abort") {
    cli::cli_abort(report)
  }
  cli::cli_warn(report)
}

.get_all_country_codes <- function(btd, cbs) {
  c(
    dplyr::pull(btd, from_code),
    dplyr::pull(btd, to_code),
    dplyr::pull(cbs, area_code)
  ) |>
    unique() |>
    sort() |>
    as.factor()
}

.build_trade_matrix <- function(btd, n, code_int) {
  code_levels <- as.character(code_int)
  m <- matrix(
    NA_real_,
    nrow = n,
    ncol = n,
    dimnames = list(code_levels, code_levels)
  )
  btd <- btd |>
    dplyr::summarise(
      value = sum(.data$value, na.rm = TRUE),
      .by = c("from_code", "to_code")
    )
  rows <- match(btd$from_code, code_int)
  cols <- match(btd$to_code, code_int)
  m[cbind(rows, cols)] <- btd$value
  m
}

.estimate_bilateral_trade <- function(exports, imports) {
  sum_exp <- sum(exports)
  sum_imp <- sum(imports)
  if (sum_exp == 0 || sum_imp == 0) {
    return(matrix(0, nrow = length(exports), ncol = length(imports)))
  }
  scale <- (1 / sum_imp + 1 / sum_exp) / 2
  tcrossprod(exports, imports * scale)
}

# Iterative proportional fitting of the bilateral trade matrix. This
# is RAS / biproportional fitting and delegates to the shared core in
# balance.R; trade matrices are small and dense, so they take the
# dense scaling path. Returns the best estimate even if not converged.
.ipf_2d <- function(
  seed,
  target_rows,
  target_cols,
  max_iter = 1000L,
  tol = 0.1
) {
  .ras_iterate(seed, target_rows, target_cols, max_iter, tol)$m
}
