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
    # NOTE for anyone cross-checking this against whep-polities' matcher: the two
    # deliberately DISAGREE here, and it is not a bug in either.
    # `matchlib.Matcher.pick_by_year` prefers `national` over an overlapping
    # aggregate; this prefers the aggregate. Both are right for what they resolve:
    # matchlib maps a source's LABEL onto a polity family, where a country is
    # normally meant, while this maps a FAOSTAT reporting AREA, and some of those
    # areas are themselves aggregates. A cross-check over 16,960 area-years found
    # the divergence confined to FAOSTAT area 15 for 1961-1999 (39 cases), which is
    # the one family where two live polities genuinely overlap (whep-polities
    # issue 40). Do not align this with matchlib without resolving that overlap
    # upstream first.
    #
    # Prefer an `aggregate` polity when one covers the year. Those rows exist
    # PRECISELY to serve a dataset's aggregate reporting area — BLX-1850-1999
    # "Belgium-Luxembourg" for FAOSTAT area 15, ANT-1961-2010 "Netherlands
    # Antilles" for 151, the RAFR/ROW region rows — so when such an area also
    # has a narrower same-prefix sibling, the aggregate is the intended target.
    #
    # Without this, refreshing the polity data displaces them: the newly
    # imported BLX-1921-1999 "Belgium-Luxembourg Economic Union" (the 1921 BLEU
    # treaty entity, `national`) and ANT-1816-1960 "Dutch Caribbean" (`colonial`)
    # both start later than their aggregate siblings, so the
    # `polity_start_year` tiebreak below picked them for 1961 data. That also
    # defeats the guard further down which refuses to extend an aggregate
    # reporting area beyond its range — the guard only fires when the chosen row
    # IS the aggregate.
    matches[,
      is_aggregate := !is.na(get("lookup_polity_type")) &
        get("lookup_polity_type") == "aggregate"
    ]
    data.table::setorderv(
      matches,
      c(
        "..whep_polity_rowid",
        "exact_start",
        "is_aggregate",
        "polity_start_year"
      ),
      order = c(1L, -1L, -1L, -1L),
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
      #
      # The test is on the AREA, not on the candidate row. Excluding merely the
      # aggregate ROWS worked only while such an area had no other polity: once
      # BLX-1921-1999 (`national`) and ANT-1816-1960 (`colonial`) arrived
      # alongside their aggregate siblings, a year outside the aggregate's range
      # fell through to those narrower rows and got extended anyway — exactly
      # what this guard exists to prevent. So an area that HAS an aggregate
      # polity is never extended, whichever row would have been chosen.
      aggregate_area_codes <- unique(
        lookup[
          !is.na(get("lookup_polity_type")) &
            get("lookup_polity_type") == "aggregate",
          area_code
        ]
      )
      fallback_matches <- fallback_matches[
        !is.na(polity_code) &
          !(area_code %in% aggregate_area_codes)
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
  # Record the anchor here rather than in each caller: this is the single point where it is
  # applied, and there are seven call sites (add_polity_code, the reporting- and role-column
  # helpers, build_trade twice, build_cbs, read_raw_inputs). Setting it once means every path that
  # assigns a polity carries its provenance, including the production, CBS and trade outputs — and
  # a constant-territory series of production is the realistic case the check exists for.
  data.table::setattr(out, "whep_backcast_anchor", backcast_anchor)
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
#' @param backcast_anchor First year of reported (non-back-cast) FAOSTAT data,
#'   default `1961`. Years before it are matched to the polity active in the
#'   anchor year, because WHEP's pre-anchor series are back-cast onto the
#'   anchor-year territory rather than reported under their data-year borders.
#'   Set to `-Inf` to disable and match strictly by data year.
#'
#'   Strict matching is correct for values genuinely reported under their own
#'   year's borders, but it must not be fed to
#'   [build_constant_territory_series()] for a back-cast series: that function
#'   spreads each value over its polity's extent, so a 1900 row carrying a
#'   1900-era polity while the value describes 1961 borders would be reallocated
#'   from the wrong extent, silently and plausibly.
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
  # The anchor marker is set by .add_polity_columns_dt(), so it is already present here.
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
    # Say which codes are not in the database, because the alternative is a row
    # count the caller cannot interpret. Asking for two codes and getting one
    # row means either "that polity has no geometry" or "that code does not
    # exist", and those need different fixes. A code can legitimately exist with
    # no polygon — `has_geometry` reports that, and such a row IS returned — so
    # silence here only ever hides the typo case.
    unknown <- setdiff(polity_codes, out$polity_code)
    if (length(unknown) > 0L) {
      cli::cli_warn(c(
        "!" = "{length(unknown)} of {length(polity_codes)} requested polity codes
           are not in the database, so no rows are returned for them.",
        "i" = "Unknown: {.val {utils::head(sort(unknown), 5)}}.",
        "i" = "A polity that exists but has no polygon is returned with
           {.field has_geometry} FALSE, so this is a code that does not exist rather
           than a missing geometry."
      ))
    }
    out <- out[out$polity_code %in% polity_codes, ]
  }
  out
}


#' Resolve a source's country label to a polity
#'
#' @description Maps a country or area **label**, as a source writes it, to a
#' WHEP polity code. This complements [add_polity_code()], which resolves
#' numeric FAOSTAT area codes: before this existed there was no supported path
#' from a label to a polity, so datasets carrying labels went unresolved.
#' `mueller_synthetic_n`'s `iso3c` column holds FAO-style legacy codes (`"BZE"`
#' for Belize, `"ROM"` for Romania, `"ZAR"` for Zaire) and
#' `lassaletta_grassland_share`'s `Country` holds name variants (`"Cape Verde"`,
#' `"Swaziland"`), none of which resolve against [polities] directly.
#'
#' @details
#' The mapping is [polity_label_aliases], a copy of the map published by
#' whep-polities. It is deliberately NOT computed here: a label's meaning is a
#' fact about the source, upstream already decides it, and a second lookup in
#' this package would be a second authority for the same question.
#'
#' Resolution is **source- and year-aware**, and both matter:
#'
#' - An alias may be scoped to one `source`, because the same label can mean
#'   different things in different sources. A scoped alias never applies to
#'   another source; an unscoped one applies to any.
#' - An alias may be scoped to a year range, because a label's referent changes.
#'   `"Cape Verde"` in 1970 is the Portuguese colony `CPV-1886-1975`; in 1990 it
#'   is `CPV-1975-2025`.
#'
#' Where several aliases match, the most specific wins: year-scoped over
#' unscoped, then source-scoped, then the narrower year range. That ordering
#' mirrors `matchlib.Matcher.match_alias` upstream, so both sides agree.
#'
#' Where no alias applies, a second route tries the polity's own
#' `polity_name`, mirroring upstream's "alias, then ISO/name family + year
#' containment". Without it a caller passing the database's own name for a
#' polity got `NA`: `resolve_polity_label("Netherlands")` found nothing while
#' [polities] carried a polity named exactly that. Two guards bound it.
#'
#' - A name resolves only when **exactly one** polity carries it in the year
#'   asked about. 52 pairs of polities share a normalised name and overlap in
#'   years, so row order would otherwise decide, and `NA` is the honest answer.
#' - An alias covering that year outranks the name whatever its source. Every
#'   alias for `"djibouti"` routes to the French Somaliland family, so the name
#'   route may not answer `DJI-1886-2025` over it. Outside its own span a
#'   year-ranged alias is silent rather than contradicted, so `"natal"` in 1900
#'   still reaches `NAT-1895-1910`.
#'
#' Returns `NA` when neither route resolves, which is a real answer rather than
#' a failure. Some labels are aggregates a source keeps reporting after the
#' territory stopped existing — `"FSU"` runs to 2009 though nothing has held
#' that territory since 1991 — and those years are deliberately unmapped rather
#' than routed to a polity that had ended.
#'
#' @param label Character vector of source labels.
#' @param source Optional source slug (e.g. `"lassaletta-grassland-share"`).
#'   Length 1, or the same length as `label`. On the alias route `NULL` matches
#'   unscoped aliases only — 171 of 869 — so a `NULL` source narrows that route
#'   sharply; the name route then gets its turn, subject to the guards above.
#' @param year Optional integer vector of years. Length 1, or the same length as
#'   `label`. On the alias route `NULL` matches aliases with no year scope only,
#'   which is the 17 of 869 published aliases carrying NEITHER bound — an
#'   eighteenth is bounded on one side only, and is scoped, not unscoped. The
#'   name route can still answer
#'   without a year, but only for a name exactly one polity has ever carried, so
#'   supplying a year remains much the stronger question: it is what lets a
#'   label resolve to the right *period* rather than to nothing.
#'
#' @returns A character vector of polity codes, `NA` where nothing matched.
#'
#' @examples
#' resolve_polity_label("ZAR", source = "mueller-synthetic-n", year = 2000)
#' resolve_polity_label(
#'   c("Cape Verde", "Cape Verde"),
#'   source = "lassaletta-grassland-share",
#'   year = c(1970L, 1990L)
#' )
#'
#' @seealso [add_polity_code()] for numeric area codes.
#' @export
resolve_polity_label <- function(label, source = NULL, year = NULL) {
  aliases <- whep::polity_label_aliases
  n <- length(label)

  recycle <- function(x, nm) {
    if (is.null(x)) {
      return(rep(NA, n))
    }
    if (length(x) == 1L) {
      return(rep(x, n))
    }
    if (length(x) != n) {
      cli::cli_abort(
        "{.arg {nm}} must be length 1 or the same length as {.arg label}."
      )
    }
    x
  }
  source <- recycle(source, "source")
  year <- recycle(year, "year")

  # Normalisation must match `matchlib.norm` upstream EXACTLY, or the two sides
  # resolve differently for the same input. My first version only lowercased and
  # squished whitespace, and a cross-check against the Python implementation
  # over 6,627 probes found 25 disagreements because of it: upstream also folds
  # accents, DROPS parenthesised qualifiers, and strips a leading "the".
  #
  # The parenthetical rule is the consequential one. Upstream reduces "Sudan
  # (former)" to "sudan", which merges it into the `sudan` rule set and changes
  # which alias wins; without it, R saw a separate label and picked a different
  # polity for 2011. Each step below mirrors one line of matchlib.norm.
  norm <- function(x) {
    x <- tolower(trimws(x))
    # NFKD + drop non-ASCII: "Réunion" -> "reunion", "Türkiye" -> "turkiye".
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- gsub("\\s*\\(.*?\\)\\s*", " ", x) # drop "(former)", "(to 1919)"
    x <- sub("^the\\s+", "", x)
    x <- gsub("[^a-z0-9 ]", " ", x)
    trimws(gsub("\\s+", " ", x))
  }
  alias_key <- norm(aliases$source_label)
  # Normalise the inputs once. Both routes below need the same key for a label,
  # and `norm()` was being recomputed per element per route.
  label_key <- norm(label)

  # Canonical-name fallback, tried only after the alias route misses.
  #
  # A caller passing the database's OWN name for a polity used to get NA:
  # `resolve_polity_label("Netherlands")` was NA while `whep::polities` carries
  # a polity named exactly "Netherlands". Upstream's matcher resolves "by alias,
  # then by ISO/name family + year containment"
  # (pipelines/polity-autoimprove/matchlib.py builds a `name_fam` index over
  # `norm(polity_name)`), so the two implementations of one question — which
  # polity does this label mean in this year — disagreed on some of the most
  # common country names there are.
  #
  # Found from the consumer side. `get_primary_residues()` resolves areas by
  # name and left 44,985 of 475,688 residue rows (9.5%) with no area code,
  # across 14 labels. Nine are verbatim polity names: Iran, Bolivia, Tanzania,
  # South Korea, North Korea, Netherlands, Venezuela, United Kingdom, Moldova.
  # The crosswalk holds every one of them under a FAOSTAT long form instead
  # ("Netherlands (Kingdom of the)", "United Republic of Tanzania"), so the code
  # was reachable and the name was not.
  #
  # The cross-check gate could not have found this: it draws its probes FROM the
  # alias map, so every label it tests is one the alias route resolves by
  # construction.
  #
  # THE AMBIGUITY GUARD IS THE DESIGN. 52 pairs of polities share a normalised
  # name AND overlap in years — nested periodisations (ITA-1861-1919 alongside
  # ITA-1861-1866, PER-1825-1909 alongside PER-1825-1884) and known duplicates
  # (SER-2006-2008 / SRB-2006-2008, whep-polities#43). Resolving those by row
  # order would invent an answer, which is precisely what the alias map exists
  # to state explicitly. So a name resolves only when EXACTLY ONE polity of that
  # name is live in the year asked about; otherwise NA, and the label needs an
  # alias. `end_year` is exclusive, so live in Y means start_year <= Y <
  # end_year.
  pol <- sf::st_drop_geometry(whep::polities)
  name_key <- norm(pol$polity_name)

  # A label that NAMES a deliberately-unmapped reporting area is refused by the
  # name route, and this guard exists because the route got one wrong.
  #
  # FAOSTAT area 351 "China" is the AGGREGATE of mainland (41), Hong Kong (96),
  # Macao (128) and Taiwan (214), each of which reports separately. Upstream
  # publishes 351 as deliberately unmapped and this package embeds that list.
  # The name route resolved the label anyway: normalisation drops parenthesised
  # qualifiers, which is what lets "Zimbabwe (1900-1953)" answer to "zimbabwe" —
  # and it also folds CHN-1950-2025 "China (PRC)" onto "china". That attributes
  # aggregate rows to the mainland polity and double-counts them against 41 + 96
  # + 128 + 214. An earlier, broader rule was rejected for exactly this case; it
  # came back through a different door, which is why the refusal is now derived
  # from the published contract rather than from a reviewer noticing.
  #
  # The ALIAS route is untouched. A curator who decides what a given source
  # means by "China" still wins, and that is where such a decision belongs.
  regions <- as.data.frame(regions_full)
  regions <- regions[
    regions$code %in% faostat_deliberate_area_codes,
    ,
    drop = FALSE
  ]
  refuse_names <- unique(norm(c(regions$name, regions$FAOSTAT_name)))

  family <- function(code) sub("-.*", "", code)

  by_name <- function(i) {
    if (label_key[i] %in% refuse_names) {
      return(NA_character_)
    }
    hit <- which(name_key == label_key[i])
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    cand <- pol[hit, , drop = FALSE]
    if (!is.na(year[i])) {
      live <- year[i] >= cand$start_year & year[i] < cand$end_year
      cand <- cand[!is.na(live) & live, , drop = FALSE]
    }
    # With no year given, several periods of one territory are as ambiguous as
    # two different territories — guessing a period would invent an answer,
    # exactly as guessing a year does on the alias route above.
    if (nrow(cand) != 1L) {
      return(NA_character_)
    }

    # A CURATED RULE THAT SPEAKS ABOUT THIS YEAR OUTRANKS THE NAME, whatever its
    # source.
    #
    # This replaced two cruder rules, both of which I measured and discarded.
    # Falling through on any alias miss produced 69 answers contradicting every
    # rule written for the label: `djibouti` resolved to DJI-1886-2025 where the
    # faostat AND iia aliases both route to the FRS (French Somaliland) family,
    # and `burundi` 1930 to BDI-1922-1962 where the iia rule covering 1922-1961
    # routes to RWB, Ruanda-Urundi. Refusing to fall through on a source
    # mismatch killed the contradictions but also killed the fix: "Iran",
    # "Netherlands" and the other seven residue labels all carry source-scoped
    # faostat aliases, so they took the same path and went back to NA.
    #
    # What separates the two groups is not source but AGREEMENT. The rules
    # covering 2010 for "Iran" name the IRN family, which is what the name route
    # says; the rule covering 1930 for "burundi" names RWB, which is not. So the
    # name answers only where no rule speaks about that year, or where the rules
    # that do speak agree on the family. Year-scoped rules are silent outside
    # their span — that is why `natal` 1900 still reaches NAT-1895-1910 while
    # its 1910-1957 alias stays authoritative inside its own range.
    speaks <- which(alias_key == label_key[i])
    if (length(speaks) > 0L) {
      rules <- aliases[speaks, ]
      # Same half-open rule as the year filter below: a rule bounded on one side
      # speaks about the years inside that bound, not about every year.
      rlo <- ifelse(is.na(rules$year_start), -Inf, rules$year_start)
      rhi <- ifelse(is.na(rules$year_end), Inf, rules$year_end)
      scoped <- !is.na(rules$year_start) | !is.na(rules$year_end)
      covering <- !scoped |
        is.na(year[i]) |
        (year[i] >= rlo & year[i] <= rhi)
      fams <- unique(family(rules$polity_code[covering]))
      if (length(fams) > 0L && !family(cand$polity_code[1]) %in% fams) {
        return(NA_character_)
      }
    }
    cand$polity_code[1]
  }

  vapply(
    seq_len(n),
    function(i) {
      hit <- which(alias_key == label_key[i])
      if (length(hit) == 0L) {
        return(by_name(i))
      }
      cand <- aliases[hit, ]

      # A source-scoped alias applies only to that source.
      keep <- is.na(cand$source) |
        cand$source == "" |
        (!is.na(source[i]) & cand$source == source[i])
      cand <- cand[keep, ]
      # No applicable rule for this source — the name route gets its turn, and
      # its own agreement check decides whether answering would contradict the
      # rules that do exist.
      if (nrow(cand) == 0L) {
        return(by_name(i))
      }

      # A year-scoped alias applies only inside its range. With no year given,
      # only unscoped aliases can match — guessing a year would invent an
      # answer.
      #
      # A MISSING BOUND IS UNBOUNDED ON THAT SIDE, not unscoped on both.
      # Requiring both bounds to be present before honouring either meant a
      # half-open range was ignored entirely: one published alias is `italy |
      # iia | (blank) | 1860 -> SAR-1800-1860`, and because `year_start` was
      # empty the 1860 bound did nothing, so IIA data labelled "italy" resolved
      # to Sardinia in the year 2000. This is the rule
      # validate_alias_chain_overlaps.py upstream already applies for the same
      # reason — it found the "turkey" case by treating a missing bound as
      # unbounded rather than skipping the row.
      lo <- ifelse(is.na(cand$year_start), -Inf, cand$year_start)
      hi <- ifelse(is.na(cand$year_end), Inf, cand$year_end)
      scoped <- !is.na(cand$year_start) | !is.na(cand$year_end)
      in_range <- !scoped |
        (!is.na(year[i]) & year[i] >= lo & year[i] <= hi)
      cand <- cand[in_range, ]
      # A YEAR mismatch does fall through, unlike the source mismatch above. The
      # same probe justified keeping this one: for years no alias speaks about,
      # the name route answers correct history rather than overriding curation.
      #
      #   natal      1900  ->  NAT-1895-1910, the colony; its only alias covers 1910-1957
      #   Morocco    1900  ->  MOR-1800-1904, pre-protectorate; its aliases start at 1961
      #   Palestine  1930  ->  PAL-1920-1948, the Mandate; its faostat alias starts at 1961
      #
      # A source-scoped alias claims a label's meaning for a reporter. A
      # year-scoped one claims it for a span, and outside that span it is
      # silent, not contradicted.
      if (nrow(cand) == 0L) {
        return(by_name(i))
      }

      # Most specific first: year-scoped, then source-scoped, then narrower
      # span. A half-open range counts as scoped and gets an infinite span, so
      # it outranks a rule with no bounds at all and loses to any rule bounded
      # on both sides.
      lo <- ifelse(is.na(cand$year_start), -Inf, cand$year_start)
      hi <- ifelse(is.na(cand$year_end), Inf, cand$year_end)
      scoped <- !is.na(cand$year_start) | !is.na(cand$year_end)
      span <- hi - lo
      ord <- order(
        -(2L *
          as.integer(scoped) +
          as.integer(!is.na(cand$source) & cand$source != "")),
        span
      )
      cand$polity_code[ord[1]]
    },
    character(1)
  )
}
