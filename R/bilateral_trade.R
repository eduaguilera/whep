#' Bilateral trade data
#'
#' @description
#' Reports trade between pairs of countries in given years.
#'
#' @param cbs_version File version passed to `get_wide_cbs()` call.
#' @param trade_version File version used for bilateral trade input.
#'   See [whep_inputs] for version details.
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
#'   If `m` is the matrix, the value at `m["A", "B"]` is the trade in tonnes
#'   from country `"A"` to country `"B"`, for the corresponding year and item.
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
#'    exports and imports computed from the commodity balance sheet. The
#'    algorithm is performed directly using the [mipfp R package](
#'    https://CRAN.R-project.org/package=mipfp).
#'
#' @export
#'
#' @examples
#' # Note: These are smaller samples to show outputs, not the real data.
#' # For all data, call the function with default versions (i.e. no arguments).
#' get_bilateral_trade(
#'   trade_version = "20250721T141553Z-5707e",
#'   cbs_version = "20250721T132006Z-8ea47"
#' )
get_bilateral_trade <- function(trade_version = NULL, cbs_version = NULL) {
  cbs <- get_wide_cbs(version = cbs_version) |>
    dplyr::select(year, item_cbs_code, area_code, export, import)

  btd <- "bilateral_trade" |>
    whep_read_file(version = trade_version) |>
    .clean_bilateral_trade()

  codes <- .get_all_country_codes(btd, cbs)

  btd |>
    .nest_by_year_item_code(cbs, codes) |>
    .process_bilateral_trade(codes) |>
    dplyr::select(-total_trade)
}

.process_bilateral_trade <- function(btd, codes) {
  btd |>
    dplyr::mutate(
      bilateral_trade = purrr::map2(
        bilateral_trade,
        total_trade,
        ~ .x |>
          .build_trade_matrix(codes) |>
          .fill_missing_trade(.y) |>
          .balance_matrix(.y)
      )
    )
}

.balance_matrix <- function(trade_matrix, total_trade) {
  exports <- total_trade |>
    dplyr::pull(balanced_export)
  imports <- total_trade |>
    dplyr::pull(balanced_import)

  stopifnot(abs(sum(exports) - sum(imports)) < 1e-4)
  stopifnot(length(exports) == length(imports))

  fitting <- mipfp::Ipfp(
    ifelse(trade_matrix == 0, 1, trade_matrix),
    target.list = list(1, 2),
    target.data = list(exports, imports),
    tol = 1e-1,
    tol.margins = 1e-1
  )
  balanced_matrix <- fitting$x.hat
  balanced_matrix
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

.clean_bilateral_trade <- function(btd) {
  btd |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(
      unit = ifelse(unit == "Head", "heads", unit),
      from_code = ifelse(element == "Export", area_code, area_code_p),
      to_code = ifelse(element == "Export", area_code_p, area_code),
      dplyr::across(c(year, from_code, to_code), as.integer)
    ) |>
    add_item_cbs_code(name_column = "item") |>
    .prefer_flow_direction("Export") |>
    dplyr::select(year, from_code, to_code, item_cbs_code, unit, value)
}

# Keep all rows with preferred direction (Import, Export)
# when both of them exist. Otherwise use the one present.
.prefer_flow_direction <- function(bilateral_trade, direction) {
  preferred_direction <- bilateral_trade |>
    dplyr::filter(element == direction)

  bilateral_trade |>
    dplyr::anti_join(
      preferred_direction,
      by = c("from_code", "to_code", "year", "item_cbs_code")
    ) |>
    dplyr::bind_rows(preferred_direction)
}

.fill_missing_trade <- function(trade_matrix, total_trade) {
  exports <- total_trade |>
    dplyr::pull(export)
  imports <- total_trade |>
    dplyr::pull(import)
  balanced_exports <- total_trade |>
    dplyr::pull(balanced_export)

  estimate <- .estimate_bilateral_trade(exports, imports)
  needed_estimates <- ifelse(is.na(trade_matrix), estimate, 0)
  balances <- balanced_exports - rowSums(trade_matrix, na.rm = TRUE)
  balances <- ifelse(balances < 0, 0, balances)

  estimate <- purrr::map2(
    purrr::array_branch(needed_estimates, 1),
    balances,
    .downscale_large_estimates
  ) |>
    do.call(rbind, args = _)

  stopifnot(dim(trade_matrix) == dim(estimate))
  stopifnot(all(!is.na(estimate)))

  # According to FABIO, missing data may be because it's truly zero,
  # so only use a small ratio of the estimate just in case.
  # TODO: Adapt this to our needs
  k_trust_factor <- 0.1
  ifelse(is.na(trade_matrix), estimate * k_trust_factor, trade_matrix)
}

.downscale_large_estimates <- function(needed_estimates_row, balance) {
  estimates_sum <- sum(needed_estimates_row, na.rm = TRUE)
  if (0 < estimates_sum && estimates_sum > balance) {
    balance * needed_estimates_row / estimates_sum
  } else {
    needed_estimates_row
  }
}

.nest_by_year_item_code <- function(btd, cbs, codes) {
  cbs <- cbs |>
    dplyr::mutate(area_code = factor(area_code, levels = codes))

  btd |>
    dplyr::filter(unit == "tonnes") |>
    dplyr::select(-unit) |>
    dplyr::mutate(
      from_code = factor(from_code, levels = codes),
      to_code = factor(to_code, levels = codes),
    ) |>
    .filter_only_items_in_cbs(cbs) |>
    tidyr::nest(
      bilateral_trade = c(from_code, to_code, value),
      .by = c(year, item_cbs_code)
    ) |>
    dplyr::inner_join(.get_nested_cbs(cbs, codes), c("year", "item_cbs_code"))
}

.get_nested_cbs <- function(cbs, codes) {
  cbs |>
    .complete_total_trade(codes) |>
    dplyr::group_by(year, item_cbs_code) |>
    .balance_total_trade() |>
    dplyr::ungroup() |>
    tidyr::nest(
      total_trade = c(
        area_code, export, import, balanced_export, balanced_import
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

.filter_only_items_in_cbs <- function(btd, cbs) {
  btd_items <- btd |>
    dplyr::pull(item_cbs_code) |>
    unique() |>
    sort()

  cbs_items <- cbs |>
    dplyr::pull(item_cbs_code) |>
    unique() |>
    sort()

  # TODO: Also include these (need total export/import reports)
  items_not_in_cbs <- btd_items[!btd_items %in% cbs_items]

  btd |>
    dplyr::filter(!item_cbs_code %in% items_not_in_cbs)
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

.build_trade_matrix <- function(btd, codes) {
  btd |>
    tidyr::pivot_wider(
      names_from = to_code,
      values_from = value,
      names_expand = TRUE
    ) |>
    tidyr::complete(from_code = codes) |>
    tibble::column_to_rownames(var = "from_code") |>
    as.matrix()
}

.estimate_bilateral_trade <- function(exports, imports) {
  if (sum(exports) == 0 || sum(imports) == 0) {
    return(matrix(0, nrow = length(exports), ncol = length(imports)))
  }
  est1 <- outer(exports, imports) / sum(imports)
  est2 <- outer(exports, imports) / sum(exports)
  (est1 + est2) / 2
}
