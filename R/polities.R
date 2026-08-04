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

# Normalisation of a country label, mirroring `matchlib.norm` in whep-polities.
# It must match EXACTLY or the two sides resolve the same input differently.
# Lowercasing and whitespace squishing alone are not enough: upstream also folds
# accents, DROPS parenthesised qualifiers and strips a leading "the".
#
# The parenthetical rule is the consequential one. Upstream reduces
# "Sudan (former)" to "sudan", which merges it into the `sudan` rule set and
# decides which alias wins; treating it as a separate label picks a different
# polity for 2011. Each step below mirrors one line of matchlib.norm.
.norm_polity_label <- function(x) {
  x <- tolower(trimws(x))
  # NFKD + drop non-ASCII: "Reunion" and "Turkiye" lose their diacritics.
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\s*\\(.*?\\)\\s*", " ", x)
  x <- sub("^the\\s+", "", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Labels the canonical-name route must refuse, derived from the crosswalk rather
# than listed here.
#
# An area the crosswalk leaves `unmapped` has no polity to attribute to, and
# letting a name lookup supply one behind its back is the second-authority
# problem again. Today that is exactly one area: FAOSTAT 351 "China", the
# aggregate of mainland (41), Hong Kong (96), Macao (128) and Taiwan (214), each
# of which reports separately. The name route resolved it anyway, because
# normalisation drops parenthesised qualifiers -- the rule that lets
# "Zimbabwe (1900-1953)" answer to "zimbabwe" also folds CHN-1950-2025
# "China (PRC)" onto "china" -- which attributes aggregate rows to the mainland
# polity and double-counts them against 41 + 96 + 128 + 214.
#
# The ALIAS route is untouched. A curator who decides what a given source means
# by "China" still wins, and that is where such a decision belongs.
.refused_polity_label_names <- function() {
  cw <- polity_area_crosswalk
  unmapped <- cw[!is.na(cw$mapping_status) & cw$mapping_status == "unmapped", ]
  unique(stats::na.omit(.norm_polity_label(c(
    unmapped$area_name,
    unmapped$reporting_polity_name
  ))))
}

#' Resolve a source's country label to a polity
#'
#' @description
#' Maps a country or area **label**, as a source writes it, to a WHEP polity
#' code. This complements [add_polity_code()], which resolves numeric
#' FAOSTAT/FABIO area codes: before this existed there was no supported path
#' from a label to a polity, so datasets carrying labels went unresolved.
#' [mueller_synthetic_n]'s `iso3c` column holds FAO-style legacy codes (`"BZE"`
#' for Belize, `"ROM"` for Romania, `"ZAR"` for Zaire) and
#' [lassaletta_grassland_share]'s `Country` holds name variants (`"Cape Verde"`,
#' `"Swaziland"`), none of which resolve against [polities] directly.
#'
#' @details
#' The mapping is [polity_label_aliases], a copy of the map published by
#' `whep-polities`. It is deliberately NOT computed here: a label's meaning is a
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
#' Where no alias applies, a second route tries the polity's own `polity_name`
#' and then, for a three-letter label, its `iso3_code`. That mirrors upstream's
#' "alias, then ISO/name family + year containment", and both halves are needed.
#' Without the name half a caller passing the database's own name for a polity
#' got `NA`: `resolve_polity_label("Netherlands")` found nothing while [polities]
#' carried a polity named exactly that. Without the ISO3 half the map answers
#' only for labels a curator had to decide about, which is 380 of
#' [mueller_synthetic_n]'s 5,043 rows -- the 10 legacy codes -- against 4,999 with
#' it. Two guards bound both halves.
#'
#' - An identifier resolves only when **exactly one** polity carries it in the
#'   year asked about. 9 pairs of polities in the shipped [polities] snapshot
#'   share a normalised name and overlap in years, so row order would otherwise
#'   decide, and `NA` is the honest answer. `"SDN"` in 2000 is the case that
#'   matters: the only polity carrying that ISO3 runs from 2011, so the answer is
#'   `NA` rather than the post-secession state.
#' - An alias covering that year outranks both, whatever its source, and a label
#'   naming an area the crosswalk leaves unmapped is refused outright.
#'
#' Returns `NA` when neither route resolves, which is a real answer rather than a
#' failure. Some labels are aggregates a source keeps reporting after the
#' territory stopped existing -- `"FSU"` runs to 2009 though nothing has held
#' that territory since 1991 -- and those years are deliberately unmapped rather
#' than routed to a polity that had ended.
#'
#' A resolved code names a polity in the published upstream database, which is
#' ahead of the [polities] snapshot this package ships (740 rows upstream against
#' 603 here). 225 of the 869 published aliases therefore point at one of 115
#' codes [get_polity_geometries()] cannot yet return a row for; refreshing
#' [polities] closes that gap without changing any resolution.
#'
#' @param label Character vector of source labels.
#' @param source Optional source slug (e.g. `"lassaletta-grassland-share"`).
#'   Length 1, or the same length as `label`. On the alias route `NULL` matches
#'   unscoped aliases only -- 171 of 869 -- so a `NULL` source narrows that route
#'   sharply; the identity routes then get their turn, subject to the guards
#'   above.
#' @param year Optional integer vector of years. Length 1, or the same length as
#'   `label`. On the alias route `NULL` matches aliases with no year scope only,
#'   which is the 17 of 869 published aliases carrying NEITHER bound. The name
#'   and ISO3 routes can still answer without a year, but only for an identifier
#'   exactly one polity has ever carried, so supplying a year remains much the
#'   stronger question: it is what lets a label resolve to the right *period*
#'   rather than to nothing.
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
  aliases <- polity_label_aliases
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

  # Normalise both sides once: each route below needs the same key for a label.
  alias_key <- .norm_polity_label(aliases$source_label)
  label_key <- .norm_polity_label(label)

  # Identity fallbacks, tried only after the alias route misses.
  #
  # `polities` is an sf data frame and sf is only suggested, so the attribute
  # columns are taken by name rather than through `sf::st_drop_geometry()`.
  pol <- data.frame(
    polity_code = polities$polity_code,
    start_year = polities$start_year,
    end_year = polities$end_year,
    stringsAsFactors = FALSE
  )
  name_key <- .norm_polity_label(polities$polity_name)
  # The ISO3 index is what makes this usable for the datasets that motivated it.
  # The alias map is keyed on the labels curators had to decide about, so a label
  # that is simply a current ISO3 code is not in it: without this route,
  # `mueller_synthetic_n`'s `iso3c` column resolved 380 of 5,043 rows -- only the
  # 10 FAO-style legacy codes the map does carry -- and `crops_manure_n`'s `ISO`
  # 860 of 31,648. Upstream's matcher resolves "by alias, then ISO/name family +
  # year containment", so this is the second half of that rule, not a new one.
  iso_key <- toupper(trimws(polities$iso3_code))
  refuse_names <- .refused_polity_label_names()

  family <- function(code) sub("-.*", "", code)

  by_name <- function(i) {
    if (label_key[i] %in% refuse_names) {
      return(NA_character_)
    }
    hit <- which(name_key == label_key[i])
    # An ISO3 code is only ever three letters, so trying the ISO3 index for
    # anything longer cannot match and would only widen the failure surface.
    if (length(hit) == 0L && grepl("^[a-z]{3}$", label_key[i])) {
      hit <- which(!is.na(iso_key) & iso_key == toupper(label_key[i]))
    }
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    cand <- pol[hit, , drop = FALSE]
    if (!is.na(year[i])) {
      # `end_year` is exclusive, so live in Y means start_year <= Y < end_year.
      live <- year[i] >= cand$start_year & year[i] < cand$end_year
      cand <- cand[!is.na(live) & live, , drop = FALSE]
    }
    # THE AMBIGUITY GUARD IS THE DESIGN. Nested periodisations and known
    # duplicates make several polities share a normalised name, so resolving by
    # row order would invent an answer -- which is precisely what the alias map
    # exists to state explicitly. With no year given, several periods of one
    # territory are as ambiguous as two different territories.
    if (nrow(cand) != 1L) {
      return(NA_character_)
    }

    # A CURATED RULE THAT SPEAKS ABOUT THIS YEAR OUTRANKS THE NAME, whatever its
    # source. Falling through on any alias miss lets the name contradict every
    # rule written for the label; refusing to fall through at all sends labels
    # that merely carry a source-scoped alias back to NA. What separates the two
    # is not source but AGREEMENT, so the name answers only where no rule speaks
    # about that year, or where the rules that do speak agree on the family.
    # Year-scoped rules are silent outside their span.
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
      # No applicable rule for this source -- the name route gets its turn, and
      # its own agreement check decides whether answering would contradict the
      # rules that do exist.
      if (nrow(cand) == 0L) {
        return(by_name(i))
      }

      # A year-scoped alias applies only inside its range. With no year given,
      # only unscoped aliases can match -- guessing a year would invent an
      # answer.
      #
      # A MISSING BOUND IS UNBOUNDED ON THAT SIDE, not unscoped on both.
      # Requiring both bounds before honouring either makes a half-open range
      # do nothing at all: one published alias is `italy | iia | (blank) | 1860
      # -> SAR-1800-1860`, and with `year_start` empty the 1860 bound would be
      # ignored, so IIA data labelled "italy" would resolve to Sardinia in the
      # year 2000. Upstream's validate_alias_chain_overlaps.py applies the same
      # rule for the same reason.
      lo <- ifelse(is.na(cand$year_start), -Inf, cand$year_start)
      hi <- ifelse(is.na(cand$year_end), Inf, cand$year_end)
      scoped <- !is.na(cand$year_start) | !is.na(cand$year_end)
      in_range <- !scoped |
        (!is.na(year[i]) & year[i] >= lo & year[i] <= hi)
      cand <- cand[in_range, ]
      # A YEAR mismatch does fall through, unlike the source mismatch above: for
      # years no alias speaks about, the name route answers correct history
      # rather than overriding curation. A source-scoped alias claims a label's
      # meaning for a reporter; a year-scoped one claims it for a span, and
      # outside that span it is silent, not contradicted.
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
