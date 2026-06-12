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
  include_unmapped = FALSE
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

    join_data <- dt[,
      .(
        ..whep_polity_rowid = get(rowid_col),
        area_code = get(code_col),
        year = as.numeric(get(year_col))
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
#' @param table A data frame.
#' @param code_column Name of the column containing numeric area codes.
#' @param year_column Name of the column containing years. Set to `NULL` to
#'   force current/default mapping.
#' @param polity_code_column Name of the output polity-code column.
#'
#' @returns A tibble with added polity metadata columns.
#' @export
add_polity_code <- function(
  table,
  code_column = "area_code",
  year_column = "year",
  polity_code_column = "polity_code"
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
    include_unmapped = TRUE
  )

  if (polity_code_column != "polity_code" && "polity_code" %in% names(out)) {
    data.table::setnames(out, "polity_code", polity_code_column)
  }
  tibble::as_tibble(out)
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
  tibble::as_tibble(out)
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
