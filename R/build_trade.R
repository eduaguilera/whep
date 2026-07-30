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

  cbs_ie <- .extract_cbs_ie_for_dtm(cbs)

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
  all_years <- sort(unique(c(dt$year, cbs_ie$year)))
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

# Extract unique (year, area_code, item_cbs_code, element) rows from CBS.
# Accepts wide format (import/export as columns) or long format (element col).
.extract_cbs_ie_for_dtm <- function(cbs) {
  cbs <- data.table::as.data.table(cbs)
  data.table::setnames(cbs, tolower)
  nms <- names(cbs)

  # Wide format: import / export are value columns
  if ("import" %in% nms || "export" %in% nms) {
    parts <- list()
    if ("import" %in% nms) {
      parts[["import"]] <- cbs[
        !is.na(import),
        .(year, area_code, item_cbs_code, element = "import")
      ]
    }
    if ("export" %in% nms) {
      parts[["export"]] <- cbs[
        !is.na(export),
        .(year, area_code, item_cbs_code, element = "export")
      ]
    }
    return(unique(data.table::rbindlist(parts)))
  }

  # Long format: element column present
  if ("element" %in% nms) {
    return(unique(cbs[
      element %in% c("import", "export"),
      .(year, area_code, item_cbs_code, element)
    ]))
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

.warn_unmapped_codes <- function(dt, mapped_col, original_col, role, ...) {
  # Three outcomes, not one message. Dropping rows is correct in all three, but the reason a user
  # needs to hear differs, and an earlier version of this said "not mapped to a polity, dropping" for
  # everything.
  #
  #   a deliberate non-mapping   the area is in the crosswalk and resolves to no polity, because
  #                              mapping it would double-count — FAOSTAT 351 "China" alongside its
  #                              own components, 15 Belgium-Luxembourg
  #   a FAOSTAT regional group   code >= 5000: World, Africa, Eastern Africa. Never territories, so
  #                              their absence is the source's design, not a gap in ours
  #   an unknown code            neither of the above: the input carries an area code this project
  #                              does not know, which is the only case that means something is wrong
  #
  # Classified from the CROSSWALK rather than from mapping_status, because every call site passes
  # `include_unmapped = FALSE`, which strips the unmapped rows out of the lookup and makes
  # mapping_status NA for the deliberate case too. A version of this that read mapping_status
  # reported China as an unknown code.
  # Accepts a data.table or a plain data frame: the trade path is data.table, the grassland path is
  # a dplyr pipeline, and the classification has nothing to do with either representation.
  # Pull the two columns as vectors rather than converting the table. `[[` works the same on a
  # data.table, a tibble and a data frame, so this stays representation-agnostic without copying —
  # and it is called once per input, on tables of millions of rows. The as.data.frame() version cost
  # 0.9s and ~300 MB per call on 4M rows to compute a handful of distinct codes.
  if (!all(c(mapped_col, original_col) %in% names(dt))) {
    return(invisible(NULL))
  }
  mapped <- dt[[mapped_col]]
  original <- dt[[original_col]]
  codes <- sort(unique(original[is.na(mapped)]))
  if (length(codes) == 0L) {
    return(invisible(NULL))
  }
  known <- unique(stats::na.omit(
    as.data.frame(whep::polity_area_crosswalk)$area_code
  ))

  # A code upstream names as deliberately unmapped counts as such even if it is absent from the
  # crosswalk, which is the part inference got wrong.
  deliberate <- codes[
    codes %in% known | codes %in% faostat_deliberate_area_codes
  ]
  rest <- codes[!codes %in% deliberate]
  # Both values come from upstream's published `faostat_unmapped_areas`, embedded in sysdata by
  # data-raw/constants.R. They were hardcoded here until upstream published them — the threshold
  # measured against real production, "deliberate" inferred from crosswalk membership — and neither
  # inference could distinguish a decision from an absence.
  group_min <- faostat_group_code_min
  # A threshold is a rule of thumb, and FAOSTAT breaks it. `>= 5000` covers the main
  # groups (World, the continents, the income bands) but the emissions domains carry
  # aggregates in the country range: 420 "Sub-Saharan Africa" is 14,427 rows of
  # faostat-emissions-livestock, and it was reported as "an area code this project does
  # not know" on every real build until upstream published the exception list.
  #
  # Found by running a build rather than by reading, and only visible because the
  # unknown bucket is a WARNING while the other two are informational -- the whole point
  # of separating them.
  sub_groups <- faostat_subthreshold_groups
  groups <- rest[rest >= group_min | rest %in% sub_groups]
  unknown <- rest[rest < group_min & !rest %in% sub_groups]

  if (length(deliberate) > 0) {
    cli::cli_inform(
      "{stringr::str_to_sentence(role)} area codes deliberately unmapped, dropping:
       {deliberate}. Statistical aggregates reported alongside their own components;
       routing them would double-count."
    )
  }
  if (length(groups) > 0) {
    # Name the below-threshold ones explicitly. The count alone used to be followed by
    # "(>= 5000)", which stopped being true once 420 joined the bucket, and a reader
    # checking why a low code was treated as a group would have found a message
    # asserting the opposite. The main groups stay a count, because a real build drops
    # 38 of them and listing those is noise.
    low <- sort(groups[groups < group_min])
    low_note <- if (length(low) > 0) {
      paste0(
        " Includes ",
        paste(low, collapse = ", "),
        ", which upstream lists as a group despite being below ",
        group_min,
        "."
      )
    } else {
      ""
    }
    cli::cli_inform(
      "{stringr::str_to_sentence(role)}: dropping {length(groups)} FAOSTAT regional
       group code{?s}, which are not territories.{low_note}"
    )
  }
  if (length(unknown) > 0) {
    cli::cli_warn(
      "{stringr::str_to_sentence(role)} area codes NOT FOUND in the polity crosswalk,
       dropping: {unknown}. Unlike a deliberate non-mapping or a FAOSTAT group, this
       means the input carries an area code this project does not know."
    )
  }
  invisible(NULL)
}
