#' Build detailed bilateral trade matrix
#'
#' @description
#' Construct the detailed bilateral trade matrix (DTM) from the FAOSTAT
#' Detailed Trade Matrix pin. Reports trade flows between pairs of
#' countries with their trade shares, aggregated to polity level and
#' mapped to CBS item codes.
#'
#' Optionally extends the time series by joining with commodity balance
#' sheet years and gap-filling country shares via linear interpolation.
#'
#' @section Time extension is uniform across groups:
#' With `extend_time = TRUE` the extension is driven by the **year axis** of
#' CBS only. Every `(area, item, partner, element, unit)` group observed in
#' any trade year is carried across the union of trade and CBS years, and
#' [fill_linear()] interpolates inside a group's observed span and holds the
#' first and last observed share constant outside it. Whether CBS actually
#' reports that area/item/element in that year is **not** consulted, so shares
#' are also emitted for country-item-year cells CBS never reports. The
#' FAOSTAT detailed trade matrix starts in 1986 while CBS starts in 1961, so
#' the pre-1986 quarter of the CBS span is entirely constant back-cast from
#' the 1986 partner mix. Scoping the extension to the CBS coverage a group
#' actually has is a methodological choice, not a bug fix, and is tracked in
#' issue #232.
#'
#' @param raw_trade A data.table or tibble of raw FAOSTAT bilateral
#'   trade data. If `NULL` (default), the data is read from the
#'   `"faostat-trade-bilateral"` pin.
#' @param cbs A tibble of commodity balance sheets in wide format, as
#'   returned by [build_commodity_balances()] or [get_wide_cbs()].
#'   Required when `extend_time = TRUE`.
#' @param min_share Numeric. Partners with a country share below this
#'   threshold are dropped when extending time. Default `0.0001`.
#' @param extend_time Logical. If `TRUE`, extend the time series using
#'   CBS years and linear interpolation of country shares.
#'   Default `FALSE`.
#' @param example Logical. If `TRUE`, return a small example tibble
#'   without downloading remote data. Default `FALSE`.
#'
#' @returns A tibble with columns:
#'   - `year`: Integer year.
#'   - `area_code`: Numeric polity code of the reporter country.
#'   - `area_code_partner`: Numeric polity code of the partner country.
#'   - `element`: Either `"import"` or `"export"`.
#'   - `item_cbs_code`: Numeric CBS item code.
#'   - `unit`: Measurement unit (`"tonnes"` or `"heads"`).
#'   - `value`: Trade quantity.
#'   - `country_share`: Share of total trade for this partner.
#'
#' @export
#'
#' @examples
#' build_detailed_trade(example = TRUE)
build_detailed_trade <- function(
  raw_trade = NULL,
  cbs = NULL,
  min_share = 1e-4,
  extend_time = FALSE,
  example = FALSE
) {
  if (example) {
    return(.example_build_detailed_trade())
  }
  cli::cli_h1("Building detailed trade matrix")

  dtm <- .read_and_clean_dtm(raw_trade)
  dtm <- .map_dtm_to_cbs_items(dtm)
  dtm <- .aggregate_dtm_to_polities(dtm)
  dtm <- .compute_country_shares(dtm)

  if (extend_time) {
    dtm <- .extend_dtm_time(dtm, cbs, min_share)
  }

  dtm <- .add_trade_polity_columns(dtm)

  tibble::as_tibble(dtm)
}

# -- Helpers -------------------------------------------------------------------

.read_and_clean_dtm <- function(raw_trade = NULL) {
  cli::cli_progress_step("Reading bilateral trade data")
  dt <- raw_trade %||% whep_read_file("faostat-trade-bilateral")
  if (!data.table::is.data.table(dt)) {
    data.table::setDT(dt)
  }
  data.table::setnames(dt, tolower)

  # Rename FAOSTAT columns to internal names
  fao_cols <- c(
    "reporter country code",
    "partner country code",
    "item code",
    "element",
    "year",
    "unit",
    "value"
  )
  internal_cols <- c(
    "area_code",
    "area_code_p",
    "item_code_trade",
    "element",
    "year",
    "unit",
    "value"
  )
  present <- fao_cols %in% names(dt)
  data.table::setnames(dt, fao_cols[present], internal_cols[present])

  # Keep only needed columns
  keep <- intersect(
    c(
      "area_code",
      "area_code_p",
      "item_code_trade",
      "item",
      "element",
      "year",
      "unit",
      "value"
    ),
    names(dt)
  )
  dt <- dt[, ..keep]

  # Standardise element names
  dt[,
    element := data.table::fifelse(
      element == "Import Quantity" | element == "Import",
      "import",
      data.table::fifelse(
        element == "Export Quantity" | element == "Export",
        "export",
        tolower(element)
      )
    )
  ]

  # Remove self-trade
  dt <- dt[area_code != area_code_p]

  # Standardise units
  dt[unit == "Head", unit := "heads"]

  # Keep only quantity rows
  dt <- dt[unit %in% c("tonnes", "heads")]

  dt
}

.map_dtm_to_cbs_items <- function(dt) {
  cli::cli_progress_step("Mapping trade items to CBS items")
  cbs_trade <- data.table::as.data.table(whep::cbs_trade_codes)
  bridge <- unique(cbs_trade[, .(item_code_trade, item_cbs)])

  items_full <- data.table::as.data.table(whep::items_full)
  items_bridge <- unique(items_full[, .(item_cbs, item_cbs_code)])

  # Prefer the stable trade item *code* join. The code bridge is also more
  # complete than the name bridge, so this maps more items. Fall back to
  # joining by item *name* only when no code column is present -- that path is
  # brittle to label drift, so warn when it is used (relates to #170).
  if ("item_code_trade" %in% names(dt)) {
    dt <- merge(dt, bridge, by = "item_code_trade", all.x = TRUE)
  } else if ("item" %in% names(dt)) {
    cli::cli_warn(
      "No trade item code column; joining trade items to CBS items by name,
       which is brittle to label drift."
    )
    name_bridge <- unique(cbs_trade[, .(item_trade, item_cbs)])
    dt <- merge(
      dt,
      name_bridge,
      by.x = "item",
      by.y = "item_trade",
      all.x = TRUE
    )
  }

  .warn_unmapped_items(dt)
  dt <- dt[!is.na(item_cbs)]
  dt <- merge(dt, items_bridge, by = "item_cbs", all.x = TRUE)
  dt <- dt[!is.na(item_cbs_code)]

  # Aggregate across trade items that map to the same CBS item
  by_cols <- c(
    "year",
    "unit",
    "area_code",
    "area_code_p",
    "item_cbs",
    "item_cbs_code",
    "element"
  )
  dt <- dt[, .(value = sum(value, na.rm = TRUE)), by = by_cols]
  dt
}

.aggregate_dtm_to_polities <- function(dt) {
  cli::cli_progress_step("Aggregating to polity level")

  # Map reporter
  dt <- .add_polity_columns_dt(
    dt,
    code_col = "area_code",
    year_col = "year",
    include_unmapped = FALSE
  )
  .warn_unmapped_codes(dt, "polity_area_code", "area_code", "reporter")
  dt[, area_code := polity_area_code]

  # Map partner
  dt <- .add_polity_columns_dt(
    dt,
    code_col = "area_code_p",
    year_col = "year",
    prefix = "partner_",
    include_unmapped = FALSE
  )
  .warn_unmapped_codes(
    dt,
    "partner_polity_area_code",
    "area_code_p",
    "partner"
  )
  dt[, area_code_partner := partner_polity_area_code]

  # Drop unmatched
  dt <- dt[!is.na(area_code) & !is.na(area_code_partner)]
  drop_cols <- intersect(
    c(
      "area_name",
      "area_iso3c",
      "polity_area_code",
      "polity_code",
      "polity_name",
      "polity_start_year",
      "polity_end_year",
      "mapping_status",
      "has_geometry",
      "partner_area_name",
      "partner_area_iso3c",
      "partner_polity_area_code",
      "partner_polity_code",
      "partner_polity_name",
      "partner_polity_start_year",
      "partner_polity_end_year",
      "partner_mapping_status",
      "partner_has_geometry",
      "area_code_p"
    ),
    names(dt)
  )
  dt[, (drop_cols) := NULL]

  # Re-aggregate at polity level
  by_cols <- c(
    "year",
    "area_code",
    "area_code_partner",
    "element",
    "item_cbs",
    "item_cbs_code",
    "unit"
  )
  dt <- dt[, .(value = sum(value, na.rm = TRUE)), by = by_cols]

  # Remove self-trade at polity level, but only for genuine single-country
  # polities. Distinct FAOSTAT areas that collapse to an *aggregate* polity
  # (e.g. the 62 territories mapped to Rest of World, 999) are different
  # contemporaneous countries, so a flow between two of them (say American
  # Samoa -> Andorra) is legitimate bilateral trade, not self-trade -- yet both
  # collapse to 999, so a naive `a == a` filter would delete it (deepens #152).
  # Genuine self-trade (same original area) was already dropped upstream in
  # .read_and_clean_dtm(); here we keep a collapsed `a -> a` row only when `a`
  # is an aggregate bucket, so its distinct-origin flows survive (aggregated).
  aggregate_codes <- .aggregate_polity_codes()
  dt <- dt[area_code != area_code_partner | area_code %in% aggregate_codes]
  dt[value == 0, value := NA_real_]
  dt <- dt[!is.na(value)]
  dt
}

# Polity codes that are artificial aggregates (e.g. Rest of World, 999) rather
# than real single countries. Distinct areas collapsing to such a bucket are
# different countries, so self-loops on them must not be treated as self-trade.
.aggregate_polity_codes <- function() {
  crosswalk <- data.table::as.data.table(polity_area_crosswalk)
  unique(crosswalk[polity_type == "aggregate", polity_area_code])
}

.compute_country_shares <- function(dt) {
  cli::cli_progress_step("Computing country shares")
  dt[,
    country_share := value / sum(value, na.rm = TRUE),
    by = c("year", "area_code", "element", "item_cbs_code", "unit")
  ]
  dt
}

.add_trade_polity_columns <- function(dt) {
  dt |>
    .add_reporting_polity_columns(code_column = "area_code") |>
    .add_partner_polity_columns(code_column = "area_code_partner") |>
    dplyr::select(
      dplyr::any_of(c(
        "year",
        "area_code",
        "polity_area_code",
        "reporting_polity_code",
        "reporting_polity_name",
        "reporting_polity_has_geometry",
        "area_code_partner",
        "partner_polity_code",
        "partner_polity_name",
        "partner_polity_has_geometry"
      )),
      dplyr::everything()
    )
}

.extend_dtm_time <- function(dt, cbs, min_share) {
  cli::cli_progress_step("Extending time series")

  cbs_years <- .extract_cbs_years_for_dtm(cbs)

  # Drop small partners to reduce dataset size
  dt[country_share < min_share, value := NA_real_]
  by_cols <- c(
    "year",
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "area_code_partner",
    "element",
    "unit"
  )
  dt <- dt[, .(value = sum(value, na.rm = TRUE)), by = by_cols]

  # Complete all year combinations within each group
  nesting_cols <- c(
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "area_code_partner",
    "element",
    "unit"
  )
  dt <- tidyr::complete(
    tibble::as_tibble(dt),
    year,
    tidyr::nesting(!!!rlang::syms(nesting_cols)),
    fill = list(value = 0)
  )
  data.table::setDT(dt)

  # Recompute shares after completing
  dt[,
    country_share := value / sum(value, na.rm = TRUE),
    by = c("year", "area_code", "element", "item_cbs_code", "unit")
  ]

  # Extend year range to cover CBS years, then re-complete
  all_years <- sort(unique(c(dt$year, cbs_years)))
  dt <- tidyr::complete(
    tibble::as_tibble(dt),
    year = all_years,
    tidyr::nesting(!!!rlang::syms(nesting_cols))
  )
  data.table::setDT(dt)

  # Gap-fill country shares
  dt <- fill_linear(
    dt,
    country_share,
    time_col = year,
    .by = c(
      "area_code",
      "item_cbs",
      "item_cbs_code",
      "area_code_partner",
      "element",
      "unit"
    )
  )
  data.table::setDT(dt)

  dt <- dt[!is.na(country_share) & country_share != 0]
  dt
}

# Years in which CBS reports an import or export flow for anything.
#
# The extension consumes the year axis and nothing else: it returns a plain
# year vector rather than the (year, area, item, element) coverage tuples,
# because the per-tuple coverage was never read. That made the code look as
# though the extension were scoped to what CBS reports when it is not (#232);
# the granularity is deliberately dropped here so the omission is visible, and
# the documented consequence lives in build_detailed_trade()'s "Time extension
# is uniform across groups" section.
#
# Accepts wide format (import/export as columns) or long format (element col).
.extract_cbs_years_for_dtm <- function(cbs) {
  cbs <- data.table::as.data.table(cbs)
  data.table::setnames(cbs, tolower)
  nms <- names(cbs)

  # Wide format: import / export are value columns
  flows <- intersect(c("import", "export"), nms)
  if (length(flows) > 0) {
    reported <- rowSums(!is.na(cbs[, ..flows])) > 0
    return(sort(unique(cbs$year[reported])))
  }

  # Long format: element column present
  if ("element" %in% nms) {
    return(sort(unique(cbs$year[cbs$element %in% c("import", "export")])))
  }

  cli::cli_abort(
    "CBS must have either {.field import}/{.field export} columns (wide format)
     or an {.field element} column (long format)."
  )
}

.warn_unmapped_items <- function(dt) {
  if ("item_code_trade" %in% names(dt)) {
    codes <- unique(dt[is.na(item_cbs), item_code_trade])
    if (length(codes) > 0) {
      cli::cli_warn(
        "Trade item codes not found in CBS mapping, dropping: {codes}"
      )
    }
  } else if ("item" %in% names(dt)) {
    items <- unique(dt[is.na(item_cbs), item])
    if (length(items) > 0) {
      cli::cli_warn(
        "Trade items not found in CBS mapping, dropping: {items}"
      )
    }
  }
}

.warn_unmapped_codes <- function(dt, mapped_col, original_col, role) {
  codes <- unique(dt[is.na(get(mapped_col)), get(original_col)])
  if (length(codes) > 0) {
    cli::cli_warn(
      "{stringr::str_to_sentence(role)} area codes not mapped to
       a polity, dropping: {codes}"
    )
  }
}
