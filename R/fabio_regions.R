# FABIO region collapse ---------------------------------------------------------
#
# WHEP base datasets are kept at per-country grain (every identifiable FAOSTAT
# reporting area is its own row). FABIO matrix workflows instead need a closed
# region list with a Rest of World catch-all. The collapse from per-country to
# FABIO regions is one-way, so it is applied here, explicitly, at the
# IO/matrix boundary — never inside the base data builders.

#' Collapse per-country tables to FABIO regions
#'
#' @description
#' Aggregate a per-country table to the closed FABIO region list required at
#' the IO/matrix boundary. Tables keyed by FAOSTAT `area_code` have each
#' area mapped to its FABIO reporting region (the `polity_area_code` column
#' of [polity_area_crosswalk]); tables keyed by `polity_code` (e.g. the
#' output of [build_primary_production()]) have each polity mapped to the
#' FABIO region its reporting areas belong to. In both cases countries that
#' FABIO does not enumerate are summed into the Rest of World region (code
#' `999`), and Sudan/South Sudan are combined into the former Sudan region
#' (code `206`). The area-keyed mapping is year-aware when a `year` column
#' is present.
#'
#' All columns other than the value columns (and an optional `fao_flag`
#' column, which keeps its first value per group) are treated as grouping
#' keys. If an `area` name column is present it is replaced by the FABIO
#' region name. Rows with no FABIO region mapping (e.g. statistical
#' aggregates such as FAOSTAT area 351 "China") are dropped with a warning.
#'
#' The collapse preserves totals: each value column sums to the same total
#' before and after, up to the dropped unmapped rows. Applying it to an
#' already collapsed table is a no-op, since every FABIO region code maps to
#' itself.
#'
#' @param table A data frame keyed by either a FAOSTAT numeric `area_code`
#'   column or a WHEP `polity_code` column, with the value columns and any
#'   other grouping columns.
#' @param value_columns Character vector of columns to sum within each
#'   collapsed group. Default `"value"`.
#'
#' @returns A tibble at FABIO region grain, with `area_code` holding the
#'   FABIO region code and the value columns summed per group.
#' @export
#'
#' @examples
#' # Syria (212) is not enumerated by FABIO and folds into Rest of World
#' # (999); South Sudan (277) folds into the former Sudan region (206).
#' tibble::tribble(
#'   ~year, ~area_code, ~item_cbs_code, ~value,
#'   2015L, 212L, 2511L, 100,
#'   2015L, 277L, 2511L, 50,
#'   2015L, 203L, 2511L, 25
#' ) |>
#'   collapse_to_fabio_regions()
collapse_to_fabio_regions <- function(table, value_columns = "value") {
  if (
    !rlang::has_name(table, "area_code") &&
      !rlang::has_name(table, "polity_code")
  ) {
    cli::cli_abort(
      "{.arg table} must include {.field area_code} or {.field polity_code}."
    )
  }
  missing_values <- setdiff(value_columns, names(table))
  if (length(missing_values) > 0L) {
    cli::cli_abort(
      "{.arg table} is missing value column{?s}: {.field {missing_values}}."
    )
  }

  table |>
    data.table::as.data.table() |>
    .collapse_to_fabio_regions_dt(value_columns = value_columns) |>
    tibble::as_tibble()
}

# data.table worker behind collapse_to_fabio_regions(); internal callers can
# use it directly to avoid tibble round-trips inside pipelines.
.collapse_to_fabio_regions_dt <- function(df, value_columns = "value") {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  if (!"area_code" %in% names(df) && "polity_code" %in% names(df)) {
    df <- .polity_to_fabio_area(df)
  }
  year_col <- if ("year" %in% names(df)) "year" else NULL
  dt <- .add_polity_columns_dt(
    df,
    code_col = "area_code",
    year_col = year_col,
    include_unmapped = FALSE
  )

  unmapped <- unique(dt[is.na(polity_area_code), area_code])
  if (length(unmapped) > 0L) {
    cli::cli_warn(c(
      "Dropping {length(unmapped)} area code{?s} without a FABIO
       region mapping.",
      "i" = "Unmapped area code{?s}: {unmapped}."
    ))
    dt <- dt[!is.na(polity_area_code)]
  }

  dt[, area_code := polity_area_code]
  metadata_cols <- intersect(.polity_metadata_cols(), names(dt))
  dt[, (metadata_cols) := NULL]
  if ("area" %in% names(dt)) {
    # Label with the region's own canonical name: member polities keep
    # distinct polity names (e.g. Sudan and South Sudan inside region 206),
    # which must not split the collapsed group.
    region_names <- .current_area_lookup(include_unmapped = FALSE)[
      !is.na(area_code),
      .(area_code, region_area_name = area_name)
    ]
    region_names <- unique(region_names, by = "area_code")
    dt[region_names, area := i.region_area_name, on = "area_code"]
  }

  by_cols <- setdiff(names(dt), c(value_columns, "fao_flag"))
  if ("fao_flag" %in% names(dt)) {
    dt[,
      c(
        lapply(.SD, sum, na.rm = TRUE),
        .(fao_flag = fao_flag[1L])
      ),
      by = by_cols,
      .SDcols = value_columns
    ]
  } else {
    dt[, lapply(.SD, sum, na.rm = TRUE), by = by_cols, .SDcols = value_columns]
  }
}

# Convert a polity-keyed table to FABIO-area keying: each polity maps to the
# FABIO region its reporting areas belong to (derived from the crosswalk, so
# it needs no year: a polity is period-specific already). Polities with no
# region mapping are dropped with a warning.
.polity_to_fabio_area <- function(dt) {
  lookup <- data.table::as.data.table(polity_area_crosswalk)[
    !is.na(polity_code) & !is.na(fabio_code),
    .(polity_code, fabio_code)
  ]
  lookup <- unique(lookup)
  ambiguous <- lookup[, .N, by = polity_code][N > 1L]
  if (nrow(ambiguous) > 0L) {
    cli::cli_warn(c(
      "{nrow(ambiguous)} polit{?y/ies} map to more than one FABIO region;
       keeping the first.",
      "i" = "Polit{?y/ies}: {ambiguous$polity_code}."
    ))
    lookup <- unique(lookup, by = "polity_code")
  }

  dt <- merge(dt, lookup, by = "polity_code", all.x = TRUE, sort = FALSE)
  unmapped <- unique(dt[is.na(fabio_code), polity_code])
  if (length(unmapped) > 0L) {
    cli::cli_warn(c(
      "Dropping {length(unmapped)} polit{?y/ies} without a FABIO
       region mapping.",
      "i" = "Unmapped polit{?y/ies}: {unmapped}."
    ))
    dt <- dt[!is.na(fabio_code)]
  }
  dt[, area_code := fabio_code]
  dt[, c("polity_code", "fabio_code") := NULL]
  dt
}

# Collapse primary production (polity-keyed) to FABIO regions, dropping the
# per-row annotation columns first so the collapsed keys stay unique. No-op
# on already collapsed (area-keyed) input.
.collapse_production_to_fabio <- function(primary_prod) {
  primary_prod |>
    dplyr::select(
      -dplyr::any_of(c("source", "fao_flag", .reporting_polity_cols()))
    ) |>
    collapse_to_fabio_regions()
}

.polity_metadata_cols <- function() {
  c(
    "area_name",
    "area_iso3c",
    "polity_area_code",
    "polity_code",
    "polity_name",
    "polity_start_year",
    "polity_end_year",
    "mapping_status",
    "has_geometry"
  )
}
