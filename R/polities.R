# Polity helpers ---------------------------------------------------------------

.polity_crosswalk <- function(include_unmapped = TRUE) {
  out <- data.table::as.data.table(polity_area_crosswalk)
  if (!include_unmapped) {
    out <- out[!is.na(polity_code)]
  }
  data.table::copy(out)
}

.current_area_lookup <- function(include_unmapped = TRUE) {
  out <- .polity_crosswalk(include_unmapped = include_unmapped)
  out <- out[!is.na(area_code)]
  out[,
    `:=`(
      has_polity = !is.na(polity_code),
      is_current = !is.na(polity_end_year) & polity_end_year >= 2025
    )
  ]
  data.table::setorderv(
    out,
    c(
      "area_code",
      "has_polity",
      "is_current",
      "polity_end_year",
      "polity_start_year"
    ),
    order = c(1L, -1L, -1L, -1L, -1L),
    na.last = TRUE
  )
  out <- unique(out, by = "area_code")
  out[, c("has_polity", "is_current") := NULL]
  out
}

.add_polity_columns_dt <- function(
  data,
  code_col = "area_code",
  year_col = "year",
  prefix = "",
  include_unmapped = FALSE,
  backcast_anchor = 1961L
) {
  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }
  dt <- data.table::copy(data)

  if (!code_col %in% names(dt)) {
    cli::cli_abort("Column {.field {code_col}} is required for polity mapping.")
  }

  base_cols <- c(
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
  mapped_cols <- paste0(prefix, base_cols)
  old_cols <- intersect(mapped_cols, names(dt))
  if (length(old_cols) > 0) {
    dt[, (old_cols) := NULL]
  }

  rowid_col <- "..whep_polity_rowid"
  dt[, (rowid_col) := .I]

  if (!is.null(year_col) && year_col %in% names(dt)) {
    lookup <- .polity_crosswalk(include_unmapped = include_unmapped)
    lookup <- lookup[!is.na(area_code)]
    lookup <- lookup[,
      c(
        "area_code",
        "join_start_year",
        "join_end_year",
        base_cols,
        "lookup_polity_type"
      ) := .(
        area_code,
        data.table::fifelse(
          is.na(polity_start_year),
          -Inf,
          as.numeric(polity_start_year)
        ),
        data.table::fifelse(
          is.na(polity_end_year),
          Inf,
          as.numeric(polity_end_year)
        ),
        area_name,
        area_iso3c,
        polity_area_code,
        polity_code,
        polity_name,
        polity_start_year,
        polity_end_year,
        mapping_status,
        has_geometry,
        get("polity_type")
      )
    ][,
      c(
        "area_code",
        "join_start_year",
        "join_end_year",
        base_cols,
        "lookup_polity_type"
      ),
      with = FALSE
    ]

    # WHEP's pre-1962 series are NOT reported under their data-year borders:
    # they are back-cast from the first reported FAOSTAT year (~1961) onto that
    # year's territory. So a 1900 "Austria" figure represents 1961 Austria, not
    # the 1900 Habsburg crownland. Floor the polity-lookup year at the anchor so
    # pre-anchor data maps to the entity active in 1961 (e.g. AUT-1919-2025, the
    # modern republic; USSR/Yugoslavia/Czechoslovakia for entities that only
    # dissolved AFTER 1961) instead of a larger historical-extent period.
    # Genuine historical-source data (reported under real historical borders) is
    # handled separately, keyed directly to its polity, not via this lookup.
    join_data <- dt[,
      .(
        ..whep_polity_rowid = get(rowid_col),
        area_code = get(code_col),
        year = pmax(as.numeric(get(year_col)), as.numeric(backcast_anchor))
      )
    ]
    matches <- lookup[
      join_data,
      on = .(
        area_code,
        join_start_year <= year,
        join_end_year >= year
      ),
      allow.cartesian = TRUE
    ]
    matches[,
      exact_start := !is.na(polity_start_year) &
        polity_start_year == join_start_year
    ]
    data.table::setorderv(
      matches,
      c("..whep_polity_rowid", "exact_start", "polity_start_year"),
      order = c(1L, -1L, -1L),
      na.last = TRUE
    )
    matches <- unique(matches, by = "..whep_polity_rowid")
    map <- matches[, c("..whep_polity_rowid", base_cols), with = FALSE]
    fallback_rowids <- map[is.na(polity_code), get(rowid_col)]
    if (length(fallback_rowids) > 0L) {
      fallback_data <- join_data[
        get(rowid_col) %in% fallback_rowids & !is.na(area_code)
      ]
      fallback_matches <- lookup[
        fallback_data,
        on = "area_code",
        allow.cartesian = TRUE
      ]
      # Do not silently extend dataset-specific aggregate reporting areas.
      fallback_matches <- fallback_matches[
        !is.na(polity_code) & get("lookup_polity_type") != "aggregate"
      ]
      if (nrow(fallback_matches) > 0L) {
        fallback_matches[,
          "year_distance" := data.table::fcase(
            year < join_start_year ,
            join_start_year - year ,
            year > join_end_year   ,
            year - join_end_year   ,
            default = 0
          )
        ]
        data.table::setorderv(
          fallback_matches,
          c("..whep_polity_rowid", "year_distance", "join_start_year"),
          order = c(1L, 1L, 1L),
          na.last = TRUE
        )
        fallback_matches <- unique(
          fallback_matches,
          by = "..whep_polity_rowid"
        )
        fallback_map <- fallback_matches[,
          c("..whep_polity_rowid", base_cols),
          with = FALSE
        ]
        # Every row reaching here failed the span join, so the period the
        # fallback lands on does NOT contain the (anchored) year: the polity did
        # not exist then. FAOSTAT area 206 "Sudan (former)" in 1970 lands on
        # SDN-2011-2025, post-secession Sudan, which by definition excludes the
        # territory the 1970 figure covers. Copying the crosswalk's "matched" or
        # "manual" status made that indistinguishable from a real period hit, so
        # the misattribution was invisible rather than merely uncertain. Over the
        # FAOSTAT era, 993 of 16638 resolved area-years across 36 areas are such
        # nearest-period stand-ins, in both directions: pre-independence years
        # (Sudan 1961-2010) and post-dissolution years (Czechoslovakia 1994-2023
        # on F51-1947-1993). Report the substitution instead of hiding it.
        fallback_map[, mapping_status := "out_of_span"]
        data.table::setkeyv(map, rowid_col)
        data.table::setkeyv(fallback_map, rowid_col)
        for (col in base_cols) {
          map[fallback_map, (col) := get(paste0("i.", col))]
        }
        data.table::setkey(map, NULL)
      }
    }
  } else {
    lookup <- .current_area_lookup(include_unmapped = include_unmapped)
    lookup <- lookup[, c("area_code", base_cols), with = FALSE]
    join_data <- dt[,
      .(
        ..whep_polity_rowid = get(rowid_col),
        area_code = get(code_col)
      )
    ]
    map <- lookup[join_data, on = "area_code"]
    map <- map[, c("..whep_polity_rowid", base_cols), with = FALSE]
  }

  data.table::setnames(map, base_cols, mapped_cols)
  out <- merge(dt, map, by = rowid_col, all.x = TRUE, sort = FALSE)
  data.table::setorderv(out, rowid_col)
  out[, (rowid_col) := NULL]
  out
}

#' Add WHEP polity codes to a table
#'
#' @description
#' Adds periodized `polity_code` information from [polity_area_crosswalk] to
#' a table with FAOSTAT/FABIO `area_code` values. If a `year` column is
#' present, the mapping is year-aware; otherwise the current/default mapping
#' is used.
#'
#' When no mapped period covers a row's year, the nearest period of the same
#' area is used as a stand-in and `mapping_status` reports `"out_of_span"`
#' rather than the crosswalk's `"matched"`/`"manual"`. Such a row is attributed
#' to a polity that did not exist in that year, so treat it as a coverage gap:
#' either the area needs the missing period added to the crosswalk, or the
#' reporting area outlived (or predates) every polity mapped to it.
#'
#' @param table A data frame.
#' @param code_column Name of the column containing numeric area codes.
#' @param year_column Name of the column containing years. Set to `NULL` to
#'   force current/default mapping.
#' @param polity_code_column Name of the output polity-code column.
#' @param backcast_anchor First year of reported (non-back-cast) FAOSTAT data,
#'   default `1961`. Years before it are matched to the polity active in the
#'   anchor year, because WHEP's pre-anchor series are back-cast onto the
#'   anchor-year territory rather than reported under their data-year borders.
#'   Set to `-Inf` to disable and match strictly by data year.
#'
#' @returns A tibble with added polity metadata columns.
#' @export
add_polity_code <- function(
  table,
  code_column = "area_code",
  year_column = "year",
  polity_code_column = "polity_code",
  backcast_anchor = 1961L
) {
  dt <- data.table::as.data.table(table)
  year_col <- if (!is.null(year_column) && year_column %in% names(dt)) {
    year_column
  } else {
    NULL
  }
  out <- .add_polity_columns_dt(
    dt,
    code_col = code_column,
    year_col = year_col,
    include_unmapped = TRUE,
    backcast_anchor = backcast_anchor
  )

  if (polity_code_column != "polity_code" && "polity_code" %in% names(out)) {
    data.table::setnames(out, "polity_code", polity_code_column)
  }
  tibble::as_tibble(out)
}

# ---- ISO3 -> numeric area_code -----------------------------------------
#
# The canonical iso3c -> area_code lookup. It maps to `polity_area_code`, NOT
# to `code`: two ISO3 codes carry a historical predecessor as a second `code`
# (ETH is both 238 Ethiopia and 62 Ethiopia PDR; SDN is both 276 Sudan and 206
# Sudan (former)), so a `code` lookup returns two rows for them. Both members
# of each pair already share one `polity_area_code` (238 and 206), which is
# also the code the commodity balances actually carry, so mapping there is
# unique by construction rather than by picking a winner: 257 iso3c, 257 rows.
.iso3c_area_code_lookup <- function() {
  whep::regions_full |>
    dplyr::filter(!is.na(.data$iso3c), !is.na(.data$polity_area_code)) |>
    dplyr::distinct(
      iso3c = as.character(.data$iso3c),
      area_code = as.integer(.data$polity_area_code)
    )
}

# Resolve a character vector of ISO3 codes to numeric area codes, preserving
# length and order. Unknown codes come back NA; the caller decides whether that
# is fatal, since some callers legitimately carry non-country aggregates.
.iso3c_to_area_code <- function(iso3c) {
  lookup <- .iso3c_area_code_lookup()
  lookup$area_code[match(as.character(iso3c), lookup$iso3c)]
}

.add_reporting_polity_columns <- function(
  table,
  code_column = "area_code"
) {
  dt <- data.table::as.data.table(table)
  drop_existing <- intersect(
    c(
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry"
    ),
    names(dt)
  )
  if (length(drop_existing) > 0L) {
    dt[, (drop_existing) := NULL]
  }

  year_col <- if ("year" %in% names(dt)) "year" else NULL
  out <- .add_polity_columns_dt(
    dt,
    code_col = code_column,
    year_col = year_col,
    prefix = "reporting_",
    include_unmapped = TRUE
  )
  if ("reporting_has_geometry" %in% names(out)) {
    data.table::setnames(
      out,
      "reporting_has_geometry",
      "reporting_polity_has_geometry"
    )
  }
  out[, polity_area_code := reporting_polity_area_code]
  out[,
    c(
      "reporting_area_name",
      "reporting_area_iso3c",
      "reporting_polity_area_code",
      "reporting_polity_start_year",
      "reporting_polity_end_year",
      "reporting_mapping_status"
    ) := NULL
  ]

  leading_cols <- c(
    "year",
    code_column,
    "polity_area_code",
    "reporting_polity_code",
    "reporting_polity_name",
    "reporting_polity_has_geometry"
  )
  data.table::setcolorder(
    out,
    c(intersect(leading_cols, names(out)), setdiff(names(out), leading_cols))
  )
  out <- tibble::as_tibble(out)
  # data.table's over-allocation pointer survives the tibble conversion, which
  # makes an otherwise unchanged output compare unequal to a plain tibble and
  # can trigger data.table's shallow-copy warning downstream.
  attr(out, ".internal.selfref") <- NULL
  out
}

# Attach the reporting-polity columns only to a frame that still carries the
# area key. A few outputs have no `area_code` to resolve a polity from -- the
# IMAGE-region aggregate is keyed by region, and `calculate_n_surplus()` accepts
# any balance the caller hands it -- and aborting there would be a regression
# rather than a caught error.
.add_polity_columns_if_keyed <- function(table, code_column = "area_code") {
  if (!rlang::has_name(table, code_column)) {
    return(table)
  }
  .add_reporting_polity_columns(table, code_column = code_column)
}

.add_partner_polity_columns <- function(
  table,
  code_column = "area_code_partner"
) {
  dt <- data.table::as.data.table(table)
  drop_existing <- intersect(
    c(
      "partner_polity_code",
      "partner_polity_name",
      "partner_polity_has_geometry"
    ),
    names(dt)
  )
  if (length(drop_existing) > 0L) {
    dt[, (drop_existing) := NULL]
  }

  year_col <- if ("year" %in% names(dt)) "year" else NULL
  out <- .add_polity_columns_dt(
    dt,
    code_col = code_column,
    year_col = year_col,
    prefix = "partner_",
    include_unmapped = TRUE
  )
  if ("partner_has_geometry" %in% names(out)) {
    data.table::setnames(
      out,
      "partner_has_geometry",
      "partner_polity_has_geometry"
    )
  }
  out[,
    c(
      "partner_area_name",
      "partner_area_iso3c",
      "partner_polity_area_code",
      "partner_polity_start_year",
      "partner_polity_end_year",
      "partner_mapping_status"
    ) := NULL
  ]

  leading_cols <- c(
    "year",
    code_column,
    "partner_polity_code",
    "partner_polity_name",
    "partner_polity_has_geometry"
  )
  data.table::setcolorder(
    out,
    c(intersect(leading_cols, names(out)), setdiff(names(out), leading_cols))
  )
  tibble::as_tibble(out)
}

.reporting_polity_cols <- function() {
  c(
    "polity_area_code",
    "reporting_polity_code",
    "reporting_polity_name",
    "reporting_polity_has_geometry"
  )
}

.role_polity_cols <- function(role) {
  paste0(
    role,
    c(
      "_polity_code",
      "_polity_name",
      "_polity_has_geometry"
    )
  )
}

.add_label_polity_cols <- function(labels, year = NULL) {
  out <- tibble::as_tibble(labels)
  if (!"area_code" %in% names(out)) {
    cli::cli_abort("{.arg labels} must include {.field area_code}.")
  }

  if (all(.reporting_polity_cols() %in% names(out))) {
    return(out)
  }

  added_year <- FALSE
  if (!is.null(year) && !"year" %in% names(out)) {
    out <- dplyr::mutate(out, year = as.integer(year))
    added_year <- TRUE
  }

  out <- .add_reporting_polity_columns(out)
  if (added_year) {
    out <- dplyr::select(out, -year)
  }
  out
}

.label_reporting_polity_lookup <- function(labels) {
  .add_label_polity_cols(labels) |>
    dplyr::select(dplyr::any_of(c("area_code", .reporting_polity_cols()))) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)
}

.bind_area_label_sources <- function(...) {
  sources <- list(...)
  sources <- purrr::keep(
    sources,
    ~ is.data.frame(.x) && "area_code" %in% names(.x)
  )
  if (length(sources) == 0L) {
    return(tibble::tibble(area_code = integer(0)))
  }

  sources |>
    purrr::map(.add_label_polity_cols) |>
    dplyr::bind_rows() |>
    dplyr::select(dplyr::any_of(c("area_code", .reporting_polity_cols()))) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)
}

.add_role_polity_from_labels <- function(
  table,
  labels,
  role,
  code_column = paste0(role, "_area")
) {
  out <- tibble::as_tibble(table)
  if (!code_column %in% names(out)) {
    cli::cli_abort(
      "Column {.field {code_column}} is required for polity mapping."
    )
  }

  role_cols <- .role_polity_cols(role)
  out <- dplyr::select(out, -dplyr::any_of(role_cols))
  lookup <- .label_reporting_polity_lookup(labels) |>
    dplyr::transmute(
      "{code_column}" := .data$area_code,
      "{role_cols[[1]]}" := .data$reporting_polity_code,
      "{role_cols[[2]]}" := .data$reporting_polity_name,
      "{role_cols[[3]]}" := .data$reporting_polity_has_geometry
    )

  out |>
    dplyr::left_join(lookup, by = code_column) |>
    dplyr::relocate(
      dplyr::all_of(role_cols),
      .after = dplyr::all_of(code_column)
    )
}

#' Get WHEP polity geometries
#'
#' @description
#' Returns the periodized polity database, including geometry. Pass
#' `polity_codes` to retrieve a subset that can be joined to outputs from
#' [add_polity_code()].
#'
#' @param polity_codes Optional character vector of WHEP polity codes.
#'
#' @returns An sf data frame.
#' @export
get_polity_geometries <- function(polity_codes = NULL) {
  out <- polities
  if (!is.null(polity_codes)) {
    out <- out[out$polity_code %in% polity_codes, ]
  }
  out
}

#' Find FAOSTAT areas whose polity resolution is ambiguous
#'
#' A FAOSTAT area maps to a sequence of polities that is meant to partition time,
#' so `(area_code, year)` has exactly one answer. Where two live polities cover the
#' same year the answer depends on row order rather than on the data, and
#' [add_polity_code()] silently returns whichever the ordering surfaces.
#'
#' This detects that. It is separate from the upstream check that no two periods of
#' one polity *family* overlap: two different families can both map to one FAOSTAT
#' area, which is the case this finds and that one does not.
#'
#' @param crosswalk A crosswalk frame; defaults to [polity_area_crosswalk].
#' @return A data frame with one row per ambiguous `(area_code, year)`, carrying
#'   `area_code`, `year`, `n` and `polity_codes` (comma-separated). Zero rows when
#'   resolution is unique, which is the intended state.
#' @keywords internal
#' @noRd
.area_year_polity_conflicts <- function(crosswalk = NULL) {
  cw <- if (is.null(crosswalk)) whep::polity_area_crosswalk else crosswalk
  cw <- as.data.frame(cw)
  keep <- !is.na(cw$area_code) &
    !is.na(cw$polity_code) &
    !is.na(cw$polity_start_year) &
    !is.na(cw$polity_end_year)
  cw <- unique(cw[
    keep,
    c("area_code", "polity_code", "polity_start_year", "polity_end_year")
  ])
  if (nrow(cw) == 0L) {
    return(.empty_conflict_frame())
  }

  # One row per (area, year) a polity covers. `polity_end_year` is EXCLUSIVE, so a
  # period [1920, 1947) covers 1920:1946 -- getting that wrong would report a
  # spurious conflict at every boundary.
  spans <- Map(
    function(a, p, s, e) {
      if (e <= s) {
        return(NULL)
      }
      data.frame(
        area_code = a,
        year = seq.int(s, e - 1L),
        polity_code = p,
        stringsAsFactors = FALSE
      )
    },
    cw$area_code,
    cw$polity_code,
    as.integer(cw$polity_start_year),
    as.integer(cw$polity_end_year)
  )
  spans <- spans[!vapply(spans, is.null, logical(1))]
  if (length(spans) == 0L) {
    return(.empty_conflict_frame())
  }
  long <- do.call(rbind, spans)

  key <- paste(long$area_code, long$year, sep = ":")
  counts <- table(key)
  dup <- names(counts)[counts > 1L]
  if (length(dup) == 0L) {
    return(.empty_conflict_frame())
  }

  hit <- long[key %in% dup, ]
  hit <- hit[order(hit$area_code, hit$year, hit$polity_code), ]
  agg <- stats::aggregate(
    polity_code ~ area_code + year,
    data = hit,
    FUN = function(x) paste(sort(unique(x)), collapse = ", ")
  )
  names(agg)[names(agg) == "polity_code"] <- "polity_codes"
  agg$n <- lengths(strsplit(agg$polity_codes, ", ", fixed = TRUE))
  agg <- agg[order(-agg$n, agg$area_code, agg$year), ]
  rownames(agg) <- NULL
  agg[, c("area_code", "year", "n", "polity_codes")]
}

.empty_conflict_frame <- function() {
  data.frame(
    area_code = integer(0),
    year = integer(0),
    n = integer(0),
    polity_codes = character(0),
    stringsAsFactors = FALSE
  )
}
