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
#' @section Time extension and CBS coverage:
#' With `extend_time = TRUE` every `(area, item, partner, element, unit)`
#' group observed in any trade year is carried across the union of trade and
#' CBS years, and [fill_linear()] interpolates inside a group's observed span
#' and holds the first and last observed share constant outside it. That
#' fills two kinds of row: years outside the trade record, and trade years
#' in which the reporter reported nothing at all for that item and element
#' (the group's share there is 0/0). `method_time_coverage` then decides
#' which of those filled shares are kept:
#'
#' - `"cbs_cells"` (default) keeps an extended share only in a
#'   `(year, area_code, item_cbs_code, element)` cell where `cbs` reports a
#'   non-missing, non-zero flow for that element. A share partitions a CBS
#'   total among partners; where there is no total there is nothing to
#'   partition, so no share is invented there. Observed trade rows are
#'   always kept, whether or not CBS reports the cell.
#' - `"cbs_years"` keeps every extended share in every CBS year. The
#'   historical behaviour: the CBS *year axis* alone drives the extension,
#'   so shares are also emitted for cells CBS never reports.
#'
#' The two methods differ only in which rows survive: a share kept by both
#' is identical, because it comes from the same interpolation of the group's
#' own anchors, and every kept cell keeps all its partners, so its shares
#' still sum to one. Because filled rows inside the trade record are scoped
#' too, a `cbs` that omits some trade years drops the filled rows of those
#' years under `"cbs_cells"`; pass a CBS spanning the trade record.
#'
#' Measured on the `"faostat-trade-bilateral"` pin against a 1850-2023 CBS,
#' half of the `(year, area, item, element)` cells the uniform extension
#' emits are cells CBS never reports (3.42 of 6.85 million; whep#232).
#' Against single-year CBS builds, `"cbs_cells"` keeps 144,812 of the
#' 286,762 rows `"cbs_years"` emits for 1975 (50.5%) and 342,617 of 384,954
#' for 2023 (89.0%), extending from trade years 1986-1987 and 2020-2021;
#' and 231,530 of 249,833 for 2000 (92.7%) and 279,210 of 293,815 for 2010
#' (95.0%), with trade years 1999-2001 and 2009-2011.
#'
#' Neither method bounds the **year axis**. The pin covers 1986-2021, while
#' [build_commodity_balances()] defaults to 1850-2023, so 138 of the 174
#' extended years (79%) lie outside the trade record entirely and carry the
#' 1986 (or 2021) partner mix held constant. CBS is itself extended back to
#' 1850, so scoping to its cells does not shorten that back-cast; pass a
#' year-scoped `cbs` to limit it.
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
#' @section Live animals are reported in two head units:
#' FAOSTAT denominates live-animal trade in `Head` for the large species
#' and in `1000 Head` for the small ones, and the unit filter here used to
#' cover only the first label. On the `faostat-trade-bilateral` pin
#' `20260407T095142Z-b3f81` that silently removed **89,073 rows carrying
#' 76,141,882 thousand head** over 1986-2021 - live chicken, turkey, duck,
#' goose, other-bird, rabbit and rodent trade - against the 11,708,244,416
#' head the `Head` rows carry. It also removed 5,011 `No` rows (whep#1092).
#'
#' `1000 Head` is a decimal prefix the source states, not a coefficient:
#' `.aggregate_fao_trade_to_cbs()` already rescales its sibling label
#' `1000 An` the same way (whep#865). The reported trade *value* of the
#' same flows corroborates it - USD 1,408 per `1000 Head` of chickens,
#' 2,392 for ducks, 2,483 for geese, 3,469 for turkeys and 4,945 for
#' rabbits, i.e. USD 1.41-4.95 a bird, against USD 654 per head of cattle,
#' 105 per pig and 74 per sheep on the `Head` rows. Read as single head,
#' a live broiler chick would cost USD 1,408. **No head-to-mass factor is
#' involved**: head counts stay head counts, on their own `unit` key.
#'
#' The `No` rows are a different case and are **not** converted. They are
#' FAOSTAT trade item 1181, which `whep::cbs_trade_codes` names
#' *Beehives* while the pin names it *Bees*, so what the number counts -
#' insects, packages or colonies - is not established by the source. They
#' are dropped, with the warning every unrecognised label now raises.
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
#' @param method_time_coverage Which extended shares `extend_time` keeps.
#'   See the *Time extension and CBS coverage* section. One of:
#'   - `"cbs_cells"` (default): only in the year, area, item and element
#'     cells where `cbs` reports a non-zero flow.
#'   - `"cbs_years"`: in every CBS year, whether or not CBS reports the
#'     cell. The historical behaviour.
#'
#'   Ignored when `extend_time = FALSE`.
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
#' @param method_head_units How to treat the FAOSTAT rows denominated in
#'   `1000 Head`. See the *Live animals are reported in two head units*
#'   section. One of:
#'   - `"convert"` (default): rescale them by 1,000 onto `heads`, the
#'     denomination the rest of the live-animal record already uses.
#'   - `"drop"`: discard them, warning with the head count removed. The
#'     historical behaviour, which leaves live poultry, rabbit and rodent
#'     trade out of the record entirely.
#'   - `"abort"`: fail, so a refreshed pin cannot reintroduce an
#'     unhandled unit unnoticed.
#'
#'   Any other unit label is dropped under every method, with a
#'   `"whep_unhandled_trade_unit"` warning naming it, and aborts under
#'   `"abort"`. Monetary units are removed silently, on purpose.
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
#'   - `method_head_units`: the treatment chosen for the `1000 Head`
#'     rows, recorded for the same reason.
#'   - `method_time_coverage`: the coverage rule of the time extension, or
#'     `NA` when `extend_time = FALSE`.
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
  method_head_units = c("convert", "drop", "abort"),
  method_time_coverage = c("cbs_cells", "cbs_years"),
  example = FALSE
) {
  method <- rlang::arg_match(method_unbacked_quantity)
  head_method <- rlang::arg_match(method_head_units)
  coverage_method <- rlang::arg_match(method_time_coverage)

  if (example) {
    return(.example_build_detailed_trade())
  }
  cli::cli_h1("Building detailed trade matrix")

  dtm <- .read_and_clean_dtm(raw_trade, head_method)
  dtm <- .screen_unbacked_quantities(dtm, method)
  dtm <- .map_dtm_to_cbs_items(dtm)
  dtm <- .aggregate_dtm_to_polities(dtm)
  dtm <- .compute_country_shares(dtm)

  if (extend_time) {
    dtm <- .extend_dtm_time(dtm, cbs, min_share, coverage_method)
  } else {
    coverage_method <- NA_character_
  }

  dtm <- .add_trade_polity_columns(dtm)

  dtm |>
    tibble::as_tibble() |>
    dplyr::mutate(
      method_unbacked_quantity = method,
      method_head_units = head_method,
      method_time_coverage = coverage_method
    )
}

# -- Helpers -------------------------------------------------------------------

.read_and_clean_dtm <- function(
  raw_trade = NULL,
  method_head_units = "convert"
) {
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

  # Standardise units and keep only the quantity rows
  .normalise_trade_units(dt, method_head_units)
}

# The unit labels the FAOSTAT trade record uses for a *quantity*, and the
# factor each carries onto the denominations this package works in. `tonnes`
# is already one of them; `Head` is a label variant of `heads`; `1000 Head`
# is the same quantity at a decimal prefix.
#
# That thousand is a prefix the source states, not a coefficient anyone chose,
# and the package already applies exactly this rescale to the sibling label
# `1000 An` in `.aggregate_fao_trade_to_cbs()` (whep#865). It is corroborated
# by the implied unit value on the `faostat-trade-bilateral` pin
# `20260407T095142Z-b3f81`: dividing the reported quantity into the reported
# trade value of the same flow gives USD 1,408 per `1000 Head` of chickens,
# 2,392 for ducks, 2,483 for geese, 3,469 for turkeys, 4,177 for other birds
# and 4,945 for rabbits -- USD 1.41 to 4.95 a bird once the thousand is taken
# out, against USD 654 per head of cattle, 105 per pig and 74 per sheep on
# the rows FAOSTAT labels `Head`. Read as single head instead, a live broiler
# chick would have to cost USD 1,408.
#
# No head-to-mass factor is implied or applied anywhere here: head counts stay
# head counts, and the two denominations stay separate keys (whep#1092).
.trade_unit_rescale <- function(method = "convert") {
  scales <- tibble::tribble(
    ~unit,       ~unit_out, ~rescale,
    "tonnes",    "tonnes",  1,
    "Head",      "heads",   1,
    "heads",     "heads",   1,
    "1000 Head", "heads",   1000
  )
  if (method == "convert") scales else scales[scales$rescale == 1, ]
}

# Unit labels that denominate a monetary *value* rather than a quantity. They
# are removed on purpose, so they must not be reported as unrecognised.
.trade_value_units <- function() {
  c("1000 US$", "1000 USD", "US$", "USD")
}

# Put every quantity row onto one of the package's own unit labels, and refuse
# to lose a label without saying so. Takes a data.table or a tibble and
# returns the class it was given.
#
# Before whep#1092 this step was a bare `unit %in% c("tonnes", "heads")`
# filter and every label outside it left without a word. On the
# `faostat-trade-bilateral` pin `20260407T095142Z-b3f81` that was 89,073
# `1000 Head` rows carrying 76,141,882 thousand head of live poultry, rabbit
# and rodent trade -- against the 11,708,244,416 head the `Head` rows carry --
# plus 5,011 `No` rows (bees or beehives; FAOSTAT's own labels disagree, so
# there is nothing to convert them onto and they still go, now loudly).
.normalise_trade_units <- function(x, method = "convert") {
  scales <- .trade_unit_rescale(method)
  idx <- match(x[["unit"]], scales$unit)
  .report_unhandled_trade_units(x, is.na(idx), method)

  keep <- !is.na(idx)
  x <- x[keep, , drop = FALSE]
  rescale <- scales$rescale[idx[keep]]
  .report_rescaled_head_units(x, rescale)

  x[["value"]] <- x[["value"]] * rescale
  x[["unit"]] <- scales$unit_out[idx[keep]]
  x
}

.report_unhandled_trade_units <- function(x, hit, method) {
  hit <- hit & !x[["unit"]] %in% .trade_value_units()
  if (!any(hit)) {
    return(invisible(NULL))
  }
  units <- sort(unique(x[["unit"]][hit]))
  total <- sum(x[["value"]][hit], na.rm = TRUE)

  report <- c(
    "Dropping {sum(hit)} trade row{?s} whose unit this package cannot \\
     denominate.",
    "i" = "Unit{cli::qty(length(units))}{?s}: {.val {units}}, \\
           {signif(total, 4)} in total.",
    "i" = "Add the label to {.fn .trade_unit_rescale} once the quantity it \\
           counts is known. Nothing may be converted onto {.val tonnes} or \\
           {.val heads} without one (whep#1092)."
  )

  if (method == "abort") {
    cli::cli_abort(report, class = "whep_unhandled_trade_unit")
  }
  cli::cli_warn(report, class = "whep_unhandled_trade_unit")
}

.report_rescaled_head_units <- function(x, rescale) {
  hit <- rescale != 1
  if (!any(hit)) {
    return(invisible(NULL))
  }
  thousands <- sum(x[["value"]][hit], na.rm = TRUE)
  cli::cli_inform(c(
    "i" = "Rescaled {sum(hit)} {.val {'1000 Head'}} trade row{?s} onto \\
           {.val heads}: {signif(thousands, 5)} thousand head."
  ))
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

.extend_dtm_time <- function(dt, cbs, min_share, coverage = "cbs_cells") {
  cli::cli_progress_step("Extending time series")

  cbs_flows <- .cbs_flow_cells(cbs)
  cbs_years <- sort(unique(cbs_flows$year))

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
  if (coverage == "cbs_cells") {
    dt <- .scope_to_cbs_cells(dt, .covered_cbs_cells(cbs_flows))
  }
  dt
}

# Keep an extended share only where CBS reports the flow it partitions
# (whep#232). Observed rows (`source_country_share == "Original"`) are data
# and stay; only the rows fill_linear() created are scoped. The key carries
# the year, so this is not a year-free territorial join.
.scope_to_cbs_cells <- function(dt, cbs_cells) {
  covered <- data.table::copy(cbs_cells)[, covered := TRUE]
  dt[, `:=`(
    area_code = as.integer(area_code),
    item_cbs_code = as.integer(item_cbs_code)
  )]
  dt <- covered[dt, on = c("year", "area_code", "item_cbs_code", "element")]
  dt <- dt[source_country_share == "Original" | !is.na(covered)]
  dt[, covered := NULL]
  dt
}

# The (year, area_code, item_cbs_code, element) cells in which CBS reports a
# non-missing, non-zero import or export flow: the cells an extended share is
# kept in under `method_time_coverage = "cbs_cells"` (whep#232). A zero is not
# coverage: a share of a zero total partitions nothing, and a CBS that
# zero-fills absent flows would otherwise cover every cell.
.extract_cbs_cells_for_dtm <- function(cbs) {
  .covered_cbs_cells(.cbs_flow_cells(cbs))
}

# Years in which CBS reports an import or export flow for anything: the year
# axis of the extension under either coverage method. Unlike the cells above
# it counts a zero flow, as it always has, so `"cbs_years"` keeps its
# historical axis.
.extract_cbs_years_for_dtm <- function(cbs) {
  sort(unique(.cbs_flow_cells(cbs)$year))
}

.covered_cbs_cells <- function(flows) {
  flows <- flows[!is.na(value) & value != 0]
  unique(flows[, c("year", "area_code", "item_cbs_code", "element")])
}

# One row per reported CBS import/export flow, keyed by year, area, item and
# element. Wide format (import/export as columns) keeps the non-missing
# values; long format (an element column) keeps every import/export row.
.cbs_flow_cells <- function(cbs) {
  cbs <- data.table::as.data.table(cbs)
  data.table::setnames(cbs, tolower)
  # The trade side is aggregated onto the polity bucket, so coverage is keyed
  # on it too when the CBS carries it; `area_code` there is provenance, and
  # e.g. Sudan's 276 folds into bucket 206.
  if ("polity_area_code" %in% names(cbs)) {
    cbs[, area_code := polity_area_code]
  }
  keys <- c("year", "area_code", "item_cbs_code")
  flows <- intersect(c("import", "export"), names(cbs))

  if (length(flows) > 0) {
    cells <- data.table::melt(
      cbs[, c(keys, flows), with = FALSE],
      id.vars = keys,
      measure.vars = flows,
      variable.name = "element",
      value.name = "value",
      variable.factor = FALSE,
      na.rm = TRUE
    )
  } else if ("element" %in% names(cbs)) {
    if (!"value" %in% names(cbs)) {
      cbs[, value := NA_real_]
    }
    cells <- cbs[
      element %in% c("import", "export"),
      c(keys, "element", "value"),
      with = FALSE
    ]
  } else {
    cli::cli_abort(
      "CBS must have either {.field import}/{.field export} columns (wide
       format) or an {.field element} column (long format)."
    )
  }
  cells[, `:=`(
    year = as.integer(year),
    area_code = as.integer(area_code),
    item_cbs_code = as.integer(item_cbs_code)
  )]
  cells
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
