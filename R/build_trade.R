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
#' are also emitted for country-item-year cells CBS never reports.
#'
#' The year axis this rests on is wide. The `"faostat-trade-bilateral"` pin
#' covers 1986-2021, while [build_commodity_balances()] defaults to 1850-2023,
#' so 138 of the 174 extended years (79%) lie outside the trade record
#' entirely and carry the 1986 (or 2021) partner mix held constant. On the
#' full pin that is 1.17 million groups spread over up to 174 years each,
#' against 9.97 million observed rows, and half of the emitted
#' `(year, area, item, element)` cells are cells CBS never reports (measured:
#' 3.42 of 6.85 million). Scoping the extension to the CBS
#' coverage a group actually has is a methodological choice, not a bug fix,
#' and is tracked in issue #232.
#'
#' @section Quantities FAOSTAT does not back with a mass:
#' The Detailed Trade Matrix carries a `tonnes` column for every item, but
#' FAOSTAT does not stand behind all of it. For trade item 1293
#' (*Crude organic material n.e.c.*, mapped here to CBS item 5001
#' `"Other"`) the aggregate *Trade: Crops and livestock products* domain
#' publishes a **value** but no country-level **mass**: on the
#' `faostat-trade-totals` pin `20260325T120525Z-7b85f` that item has
#' 28,213 value rows against 6,000 quantity rows, only 1,830 of them
#' non-zero, and every quantity above 10 Mt belongs to a FAOSTAT *regional
#' aggregate* (area code >= 51000) flagged `E`. Spot-checked for Colombia,
#' Kenya, South Africa and Japan over 2002-2010, the country-level
#' quantity is `0` or absent in every year.
#'
#' The detailed matrix nonetheless reports masses for it that cannot be
#' masses (whep#1023). On the `faostat-trade-bilateral` pin
#' `20260407T095142Z-b3f81`, Colombia's 2004 export of item 1293 to the
#' United States is 2,579,549,000 tonnes against an export value of
#' USD 584.4 million, i.e. **USD 0.227 per tonne**; South Africa's
#' 2008-2013 flows with Uganda run at USD 0.011-0.015 per tonne. The
#' mirrored report of the same flow disagrees by factors of 356 to
#' 838,000 - Kenya says it exported 534,611,300 tonnes to the
#' Netherlands in 2009, the Netherlands says it imported 72,076 tonnes
#' (7,417x) - and it is always the large side that is not a mass. What the
#' large side actually counts (stems, pieces, bunches) is **not recorded
#' anywhere in the data and is therefore unverified**, and the implied
#' units per tonne are not constant across cells, so no conversion factor
#' can be derived: the figures can be dropped or carried, not corrected.
#'
#' `method_unbacked_quantity` selects the treatment. Screened tonnage on
#' the full pin, restricted to items that map to a CBS item: 15.31 Gt of
#' 83.16 Gt (**18.4%**), 99.78% of it item 1293. By year the screen takes
#' 12.7-70.0% of 2003-2013, 5.6-16.5% of 1986-1988, 10.1-13.0% of
#' 2000-2002, at most 3.3% of 1989-1999 and at most 0.04% of 2014-2021 -
#' item 1293 carries no bilateral quantity at all from 2014, which is why
#' the recent record looks clean.
#'
#' Those tonnes do not reach this function's output today whichever method
#' is chosen, for an unrelated reason: item 1293's CBS name `"Other"` has
#' no row in `whep::items_full`, so the item-code bridge leaves it without
#' an `item_cbs_code` and it is dropped a step later. That was silent
#' until now and is warned about separately; four CBS names and 14 of the
#' 710 trade item codes in `whep::cbs_trade_codes` are affected. The
#' screen is therefore explicit where the mapping gap was accidental, and
#' it keeps working if the gap is ever filled.
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
#' @param method_unbacked_quantity How to treat a reported `tonnes`
#'   quantity for a FAOSTAT trade item whose country-level mass FAOSTAT
#'   itself does not publish. See the *Quantities FAOSTAT does not back
#'   with a mass* section. One of:
#'   - `"drop"` (default): discard those rows, warning with the tonnage
#'     removed. They are not masses, and no conversion to mass is
#'     derivable.
#'   - `"keep"`: carry them verbatim, with the same warning. The
#'     historical behaviour, and unsafe for anything that treats the
#'     column as mass.
#'   - `"abort"`: fail, so a refreshed pin cannot reintroduce them
#'     unnoticed.
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
#'   - `method_unbacked_quantity`: the treatment chosen for quantities
#'     FAOSTAT does not back with a mass, recorded so a downstream
#'     consumer can tell which variant it is holding.
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
  method_unbacked_quantity = c("drop", "keep", "abort"),
  example = FALSE
) {
  method <- rlang::arg_match(method_unbacked_quantity)

  if (example) {
    return(.example_build_detailed_trade())
  }
  cli::cli_h1("Building detailed trade matrix")

  dtm <- .read_and_clean_dtm(raw_trade)
  dtm <- .screen_unbacked_quantities(dtm, method)
  dtm <- .map_dtm_to_cbs_items(dtm)
  dtm <- .aggregate_dtm_to_polities(dtm)
  dtm <- .compute_country_shares(dtm)

  if (extend_time) {
    dtm <- .extend_dtm_time(dtm, cbs, min_share)
  }

  dtm <- .add_trade_polity_columns(dtm)

  dtm |>
    tibble::as_tibble() |>
    dplyr::mutate(method_unbacked_quantity = method)
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

# FAOSTAT trade items whose Detailed Trade Matrix `tonnes` figure FAOSTAT's own
# aggregate domain does not back with a country-level mass, so the column is
# not a mass however it is labelled. See the "Quantities FAOSTAT does not back
# with a mass" section of build_detailed_trade() for the evidence and whep#1023
# for the investigation.
#
# The membership test is a measurement, not an opinion, and it is cheap to
# redo when the pins are refreshed: for each (item, area, year, direction),
# compare the Detailed Trade Matrix quantity against the same key in the
# aggregate `faostat-trade-totals` pin. An item belongs here when the
# aggregate reports the trade but reports its quantity as `0`, or omits the
# quantity element while reporting a value. Run over the whole
# `faostat-trade-bilateral` pin `20260407T095142Z-b3f81` that test flags
# 15.31 Gt, of which 99.78% is item 1293; the next largest is item 828
# (Tobacco) at 4.0 Mt, 0.03%. The list is short because the measurement is,
# not because it was cut short.
#
# Item 631 ("Waters, ice etc") is deliberately NOT here. It too is missing
# from the aggregate domain, but its 1.59 Gt reads as a genuine mass: the
# 97.7-101.3 Mt/year China-to-Macao flow of 2017-2018 at USD 0.47/tonne is
# what bulk raw water costs. Whether water belongs in a biomass account is a
# separate question from whether the number is a mass.
.unbacked_mass_trade_items <- function() {
  1293L
}

# CBS items that `.unbacked_mass_trade_items()` feeds. Derived from the
# shipped crosswalks rather than hardcoded, so a change to either table moves
# this with it. Currently CBS item 5001 ("Other"), which is why whep#1023
# surfaced there.
#
# The CBS side resolves against `whep::items_cbs`, the CBS item registry that
# `get_bilateral_trade()` also uses, and NOT against `whep::items_full`:
# `items_full` has no "Other" row at all, which is why
# `.map_dtm_to_cbs_items()` already loses every item 1293 row on the
# `items_bridge` merge -- silently, and for a reason unrelated to whether the
# tonnage is a mass. That gap is a separate defect; this screen must not
# depend on it.
.unbacked_mass_cbs_items <- function() {
  cbs_names <- whep::cbs_trade_codes |>
    dplyr::filter(item_code_trade %in% .unbacked_mass_trade_items()) |>
    dplyr::pull(item_cbs)

  whep::items_cbs |>
    dplyr::filter(item_cbs_name %in% cbs_names) |>
    dplyr::pull(item_cbs_code) |>
    unique() |>
    sort()
}

# Screen the mass rows of items whose mass FAOSTAT does not publish. The
# quantity cannot be repaired -- the mirrored reports of the same flow differ
# by factors of 356 to 838,000 and the implied units per tonne are not
# constant, so there is no factor to apply -- hence the methods are drop,
# carry, or refuse, and never a conversion.
.screen_unbacked_quantities <- function(dt, method = "drop") {
  codes <- .unbacked_mass_trade_items()
  hit <- if ("item_code_trade" %in% names(dt)) {
    dt$item_code_trade %in% codes
  } else {
    dt$item %in% .unbacked_mass_trade_names(codes)
  }
  hit <- hit & dt$unit == "tonnes"

  if (!any(hit)) {
    return(dt)
  }

  .report_unbacked_quantities(dt, hit, codes, method)

  if (method == "keep") {
    return(dt)
  }
  dt[!hit]
}

# The name-keyed fallback for a caller that injects raw trade without the item
# code column, matching the same fallback in `.map_dtm_to_cbs_items()`.
.unbacked_mass_trade_names <- function(codes) {
  whep::cbs_trade_codes |>
    dplyr::filter(item_code_trade %in% codes) |>
    dplyr::pull(item_trade) |>
    unique()
}

.report_unbacked_quantities <- function(dt, hit, codes, method) {
  screened <- sum(dt$value[hit], na.rm = TRUE)
  mass <- sum(dt$value[dt$unit == "tonnes"], na.rm = TRUE)
  share <- if (mass > 0) 100 * screened / mass else 0
  years <- range(dt$year[hit], na.rm = TRUE)

  report <- c(
    "{sum(hit)} bilateral trade row{?s} report{?s/} a {.field tonnes} \\
     quantity that FAOSTAT does not publish as a country-level mass.",
    "i" = "Trade item {cli::qty(length(codes))}code{?s}: {.val {codes}}.",
    "i" = "{signif(screened, 4)} of {signif(mass, 4)} tonnes, \\
           {round(share, 1)} percent of the reported mass, over \\
           {years[1]}-{years[2]}.",
    "i" = "The true unit is unverified and not constant across cells, so no \\
           conversion to mass is derivable (whep#1023).",
    "i" = "{.arg method_unbacked_quantity} is {.val {method}}."
  )

  if (method == "abort") {
    cli::cli_abort(report, class = "whep_unbacked_mass_quantity")
  }
  cli::cli_warn(report, class = "whep_unbacked_mass_quantity")
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
  .warn_items_without_cbs_code(dt)
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
# Measured on the full pin against a real 1850-2023 CBS, 30.8% of the 201.9
# million partner-level grid rows also sit in cells CBS never reports.
#
# Accepts wide format (import/export as columns) or long format (element col).
.extract_cbs_years_for_dtm <- function(cbs) {
  cbs <- data.table::as.data.table(cbs)
  data.table::setnames(cbs, tolower)
  nms <- names(cbs)

  # Wide format: import / export are value columns
  flows <- intersect(c("import", "export"), nms)
  if (length(flows) > 0) {
    reported <- rowSums(!is.na(cbs[, flows, with = FALSE])) > 0
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

# A trade item can carry a CBS item *name* that `whep::items_full` has no row
# for, in which case the `items_bridge` merge leaves `item_cbs_code` NA and
# the row is dropped. That drop used to be silent, which is how the whole of
# CBS item "Other" left this producer without a message (found while tracing
# whep#1023). Measured on the shipped tables: 4 of the 149 CBS names in
# `cbs_trade_codes` are absent from `items_full` -- "Other", "Infant food",
# "Other fodder" and "Oil palm fruit" -- covering 14 of its 710 trade item
# codes. Three of the four do have a code in `whep::items_cbs`, so this is an
# `items_full` coverage gap rather than a genuinely unknown item.
.warn_items_without_cbs_code <- function(dt) {
  unmapped <- dt[is.na(item_cbs_code)]
  if (nrow(unmapped) == 0) {
    return(invisible(dt))
  }
  names_missing <- sort(unique(unmapped$item_cbs))
  mass <- sum(unmapped$value[unmapped$unit == "tonnes"], na.rm = TRUE)

  cli::cli_warn(
    c(
      "{length(names_missing)} CBS item name{?s} ha{?s/ve} no
       {.field item_cbs_code} in {.code whep::items_full}: dropping
       {nrow(unmapped)} trade row{?s}, {signif(mass, 4)} tonnes.",
      "i" = "Name{?s}: {.val {names_missing}}."
    ),
    class = "whep_item_cbs_code_missing"
  )
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
