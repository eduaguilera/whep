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
  cbs,
  min_share = 1e-4,
  extend_time = FALSE,
  example = FALSE
) {
  if (example) {
    return(.example_build_detailed_trade())
  }
  cli::cli_h1("Building detailed trade matrix")

  dtm <- .read_and_clean_dtm()
  dtm <- .map_dtm_to_cbs_items(dtm)
  dtm <- .aggregate_dtm_to_polities(dtm)
  dtm <- .compute_country_shares(dtm)

  if (extend_time) {
    dtm <- .extend_dtm_time(dtm, cbs, min_share)
  }

  tibble::as_tibble(dtm)
}

# -- Helpers -------------------------------------------------------------------

.read_and_clean_dtm <- function() {
  cli::cli_progress_step("Reading bilateral trade data")
  dt <- whep_read_file("faostat-trade-bilateral")
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
    c("area_code", "area_code_p", "item_code_trade", "item",
      "element", "year", "unit", "value"),
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

  # If the DTM has item names, map through cbs_trade_codes
  if ("item" %in% names(dt)) {
    name_bridge <- unique(cbs_trade[, .(item_trade, item_cbs)])
    dt <- merge(
      dt,
      name_bridge,
      by.x = "item",
      by.y = "item_trade",
      all.x = TRUE
    )
  } else if ("item_code_trade" %in% names(dt)) {
    dt <- merge(dt, bridge, by = "item_code_trade", all.x = TRUE)
  }

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
  regions <- data.table::as.data.table(whep::regions_full)
  regions <- regions[!is.na(polity_code)]
  polities <- data.table::as.data.table(whep::polities)

  # Build bridge: FAOSTAT area_code -> polity area_code
  bridge <- merge(
    regions[, .(fao_code = code, polity_code)],
    polities[, .(polity_code = iso3c, polity_area_code = area_code)],
    by = "polity_code"
  )

  # Map reporter
  dt <- merge(
    dt,
    bridge,
    by.x = "area_code",
    by.y = "fao_code",
    all.x = TRUE
  )
  dt[, area_code := NULL]
  data.table::setnames(dt, "polity_area_code", "area_code")

  # Map partner
  dt <- merge(
    dt,
    bridge[, .(fao_code, partner_polity = polity_area_code)],
    by.x = "area_code_p",
    by.y = "fao_code",
    all.x = TRUE
  )
  dt[, area_code_p := NULL]
  data.table::setnames(dt, "partner_polity", "area_code_partner")

  # Drop unmatched and remove temp columns
  dt <- dt[!is.na(area_code) & !is.na(area_code_partner)]
  dt[, polity_code := NULL]

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

  # Remove self-trade at polity level

  dt <- dt[area_code != area_code_partner]
  dt[value == 0, value := NA_real_]
  dt <- dt[!is.na(value)]
  dt
}

.compute_country_shares <- function(dt) {
  cli::cli_progress_step("Computing country shares")
  dt[,
    country_share := value / sum(value, na.rm = TRUE),
    by = c("year", "area_code", "element", "item_cbs_code", "unit")
  ]
  dt
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

  # Join CBS import/export years to extend range
  dt <- merge(
    dt,
    cbs_ie,
    by = c("year", "area_code", "item_cbs_code", "element"),
    all = TRUE
  )

  # Re-complete after CBS join for full time range
  dt <- tidyr::complete(
    tibble::as_tibble(dt),
    year,
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
      "element"
    )
  )
  data.table::setDT(dt)

  dt <- dt[!is.na(country_share) & country_share != 0]
  dt
}

# Extract unique (year, area_code, item_cbs_code, element) rows from CBS.
# Accepts wide format (import/export as columns) or long format (element col).
.extract_cbs_ie_for_dtm <- function(cbs) {
  if (!data.table::is.data.table(cbs)) {
    data.table::setDT(cbs)
  }
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
