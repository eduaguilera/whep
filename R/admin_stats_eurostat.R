#' Read Eurostat regional crop and livestock statistics
#'
#' @description
#' Read one of Eurostat's four regional agricultural dataflows over the
#' SDMX-CSV dissemination API and return its rows in the source's own
#' identifiers: the NUTS code and label exactly as Eurostat served them,
#' the crop or animal code and label exactly as Eurostat served them, and
#' the value converted to WHEP units. Nothing is mapped to a WHEP item
#' code and nothing is resolved to a polity here; those are separate
#' steps, so a change in either leaves this reader untouched.
#'
#' Tables, verified live on 2026-09-02:
#'
#' - `"apro_cpshr"` -- crop production by NUTS 2 region, 2000-2024.
#' - `"apro_cpnhr_h"` -- the historical companion, 1975-1999.
#' - `"apro_mt_ls_r"` -- animal populations by NUTS 2 region, 1977-2025.
#'   It carries dairy cows (`A2300F`) and non-dairy cows (`A2300G`)
#'   separately but **no poultry**.
#' - `"ef_lsk_poultry"` -- farm-structure-survey poultry by NUTS 2 region,
#'   census years only (2005, 2007, 2010, 2013, 2016, 2020, 2023).
#'
#' No registration or key is needed. The dataflow identifier is checked
#' against Eurostat's own dataflow registry before any data request, so a
#' renamed table aborts naming the tables this reader knows instead of
#' returning an empty tibble.
#'
#' @section Units and the values this reader converts:
#' Eurostat serves crop areas in thousand hectares (`AR_THS_HA`,
#' `MAR_THS_HA`), crop production in thousand tonnes (`HPRD_THS_T`,
#' `HPRD_HUMD_EU_THS_T`) and animal populations in thousand head
#' (`THS_HD`) or head (`HD`). Every response carries those units in the
#' measure dimension's own label, so the conversion factor is not a
#' constant this package asserts from outside: the reader checks each
#' response's label against the unit it is about to assume and aborts if
#' Eurostat changes it.
#'
#' Humidity (`HUMD_PC`, `HUMD_EU_PC`) and yield (`YLD_HUMD_EU_T_HA`) rows
#' are dropped and counted. Yield is exactly production divided by area,
#' both of which this reader returns, and its tonnes-per-hectare unit is
#' outside this contract's `value_unit` vocabulary.
#'
#' @section What the area figures mean, and the 2025 break:
#' Eurostat's own metadata (ESMS `apro_cp_esms`, section 3.4, metadata
#' last updated 7 October 2025) states that "under the pre-SAIO data
#' collection, up to the 2024 reference year, the area concept was area
#' under cultivation, i.e. the area actually harvested, with non-harvested
#' areas (for example due to natural disasters) excluded", which is why
#' `AR_THS_HA` maps to `indicator_used = "area_harvested"`. From the 2025
#' reference year the SAIO regulation changes that: "for cereals for the
#' production of grain, dry pulses and protein crops, root crops,
#' industrial crops, and plants harvested green, the areas refer to the
#' sown area", while vegetables stay on harvested area and permanent crops
#' on production area.
#'
#' Because the new concept differs by crop group, and this reader has no
#' crop vocabulary, it does **not** relabel 2025+ rows. It flags them:
#' every `"apro_cpshr"` row with `year >= 2025` gets
#' `concept_break = TRUE`, and choosing the right `indicator_used` for
#' them is the resolver's job once a crop-group vocabulary exists. As of
#' 2026-09-02 the dataflow held no year beyond 2024, so the flag is
#' prospective.
#'
#' @section NUTS versions, and why rows are deduplicated:
#' Eurostat labels every geography whose code has been retired with the
#' last nomenclature in which it was valid -- `FR21` arrives as
#' `"Champagne-Ardenne (NUTS 2013)"`. `nuts_version` is read from that
#' suffix, and a code without one is on the current nomenclature, NUTS
#' 2024 (regulation 2023/674, in force 2024-2026; see
#' <https://ec.europa.eu/eurostat/web/nuts/history>). The suffix is
#' Eurostat's own retirement marker, so `nuts_version` means "the last
#' nomenclature this code was valid in", which is exactly what a later
#' code-system-scoped resolution step needs.
#'
#' One response therefore mixes vintages: `apro_cpnhr_h` carries `FR21`
#' for 1989-1999 and `FRF2` -- a pure recoding of the same polygon -- for
#' 1990-1999. Rows for the same territory in the same year are
#' deduplicated keeping the newest vintage, and the dropped count, the
#' code pairs and any value disagreement are reported. Two rows count as
#' the same territory only when they share a country, a NUTS level and a
#' label once the version suffix is stripped; that comparison finds
#' recodings only, never resolves a code to a polity, and never merges
#' across levels, so `FI2`/`FI20` (both labelled "Aland") and `DE3`/`DE30`
#' (both "Berlin") stay separate. A genuine boundary change between
#' vintages keeps both rows, visible, for the resolution step to settle.
#'
#' @section Grain, and the levels this returns:
#' NUTS 2 is `grain = "admin1"` and NUTS 3 is `"admin2"`, the mapping the
#' admin-shares grain rule uses for EU countries. NUTS 1 rows are returned
#' only when asked for (`nuts_level = 1L`) and carry `grain = NA`: in some
#' countries NUTS 1 is a genuine first-order division (Germany's
#' Bundeslaender) and in others it is an aggregate of the units below it,
#' so which of the two it is cannot be decided per row here. Germany is
#' the case that forces the question -- it reports crop area at NUTS 2
#' only up to 2004 and at NUTS 1 from 2005 onward (verified against
#' `apro_cpshr` on 2026-09-02).
#'
#' NUTS 0 rows are not units at all; they are the national totals the
#' units should sum to, and they are returned separately as the
#' `"national_check"` attribute of the result. Supranational aggregates
#' (`EU`, `EU27_2020`) and composite codes that would double-count their
#' own members (`EL41_42`, served for 2007-2014 alongside `EL41` and
#' `EL42`) are dropped and counted.
#'
#' @param table Dataflow to read, one of `"apro_cpshr"`, `"apro_cpnhr_h"`,
#'   `"apro_mt_ls_r"` or `"ef_lsk_poultry"`.
#' @param filters Named list narrowing the request, any of: `geo` (NUTS
#'   codes, e.g. `c("FR", "FRF2")`), `items` (crop or animal dimension
#'   codes, e.g. `"C1110"` or `"A2000"`) and `years` (integer years; the
#'   request spans their range and the result is filtered to them
#'   exactly). An absent or empty element means "everything Eurostat
#'   serves", which for `"ef_lsk_poultry"` is large.
#' @param nuts_level Integer NUTS levels to return as units, a subset of
#'   `1:3`. Defaults to `2L`, the level every one of these tables is
#'   published at.
#' @param file Path to an already-downloaded SDMX-CSV response to parse
#'   instead of calling the API. It must have been requested with
#'   `label=both`, since the version suffix is read from the labels.
#' @param example If `TRUE`, return a small fixture instead of reading
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble of source-native rows, one per unit x item x measure x
#'   year, with columns `source` (`"Eurostat_<table>"`),
#'   `source_native_unit_id`, `source_native_unit_name`,
#'   `source_native_item_code`, `source_native_item_name`, `quantity`
#'   (`"area"`, `"production"` or `"heads"`), `indicator_used` (the
#'   admin-shares indicator vocabulary for area and production rows, `NA`
#'   for head counts, which that vocabulary does not cover), `year`,
#'   `value` (converted to hectares, tonnes or head), `value_unit`,
#'   `value_flag` (Eurostat's observation flag and confidentiality status
#'   verbatim, joined by `"|"` when both are present, `NA` when clean),
#'   `concept_break`, `grain`, `nuts_level`, `nuts_version`,
#'   `source_version` (the response's `LAST UPDATE` stamp verbatim) and
#'   `recorded_at`. The NUTS 0 rows are attached as the `"national_check"`
#'   attribute, in the same columns with `grain = NA`.
#'
#' @export
#'
#' @examples
#' read_admin_stats_eurostat(example = TRUE)
read_admin_stats_eurostat <- function(
  table = c("apro_cpshr", "apro_cpnhr_h", "apro_mt_ls_r", "ef_lsk_poultry"),
  filters = list(),
  nuts_level = 2L,
  file = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_admin_stats_eurostat())
  }
  table <- rlang::arg_match(table)
  .eurostat_check_filters(filters)
  nuts_level <- .eurostat_check_levels(nuts_level)
  raw <- if (is.null(file)) {
    .eurostat_assert_code(table)
    .fetch_eurostat(
      table,
      .eurostat_build_key(table, filters),
      .eurostat_build_query(filters)
    )
  } else {
    .eurostat_parse_sdmx(readr::read_file(file))
  }
  .eurostat_normalize(raw, table, filters, nuts_level)
}

# ---- The four dataflows -----------------------------------------------

# Everything that differs between the four tables, in one place: which
# dimension carries the item, which carries the measure, and the SDMX key
# template. Key order is positional and was read off each dataflow's own
# response header on 2026-09-02 (`apro_cpshr` and `apro_cpnhr_h`:
# freq.crops.strucpro.geo; `apro_mt_ls_r`: freq.animals.unit.geo;
# `ef_lsk_poultry`: freq.statinfo.lsu.uaarea.animals.unit.geo). The
# poultry table's three breakdown dimensions are pinned to TOTAL and its
# unit to HD because the unpinned 2020 slice alone is 539,000 rows of
# farm-size detail this reader has no use for.
.eurostat_tables <- function() {
  list(
    apro_cpshr = list(
      item_dim = "crops",
      measure_dim = "strucpro",
      key_template = "A.{items}..{geo}"
    ),
    apro_cpnhr_h = list(
      item_dim = "crops",
      measure_dim = "strucpro",
      key_template = "A.{items}..{geo}"
    ),
    apro_mt_ls_r = list(
      item_dim = "animals",
      measure_dim = "unit",
      key_template = "A.{items}..{geo}"
    ),
    ef_lsk_poultry = list(
      item_dim = "animals",
      measure_dim = "unit",
      key_template = "A.TOTAL.TOTAL.TOTAL.{items}.HD.{geo}"
    )
  )
}

.eurostat_base_url <- function() {
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1"
}

# Eurostat retires and renames dataflow identifiers, and a request for a
# renamed one comes back as an SDMX fault rather than as data, so the
# identifier is checked against the registry before the data request.
# Identifiers are uppercase in the dataflow endpoint and lowercase in the
# data endpoint. Mirrors .hydro_resolve_var(): name what is available
# rather than failing on an empty result.
.eurostat_code_exists <- function(code) {
  url <- paste0(.eurostat_base_url(), "/dataflow/ESTAT/", toupper(code))
  status <- httr::status_code(httr::GET(url))
  if (status == 200L) {
    return(TRUE)
  }
  if (status == 404L) {
    return(FALSE)
  }
  cli::cli_abort(
    "Eurostat's dataflow registry answered {.val {status}} for
     {.val {toupper(code)}}."
  )
}

.eurostat_assert_code <- function(code) {
  if (.eurostat_code_exists(code)) {
    return(invisible(code))
  }
  known <- toupper(names(.eurostat_tables()))
  cli::cli_abort(c(
    "Eurostat has no dataflow {.val {toupper(code)}}.",
    i = "This reader knows {.val {known}}.",
    i = "If Eurostat renamed it, update {.fun .eurostat_tables}."
  ))
}

.eurostat_build_key <- function(table, filters) {
  spec <- .eurostat_tables()[[table]]
  spec$key_template |>
    stringr::str_replace(
      stringr::fixed("{items}"),
      .eurostat_key_part(filters$items)
    ) |>
    stringr::str_replace(
      stringr::fixed("{geo}"),
      .eurostat_key_part(filters$geo)
    )
}

.eurostat_key_part <- function(x) {
  if (length(x) == 0L) {
    return("")
  }
  paste(x, collapse = "+")
}

# The API takes a period range, not a set, so the request spans the
# requested years and .eurostat_filter_years() trims to the exact set.
.eurostat_build_query <- function(filters) {
  years <- filters$years
  if (length(years) == 0L) {
    return(list())
  }
  list(startPeriod = min(years), endPeriod = max(years))
}

# `label=both` puts "CODE:Label" in every dimension column. The labels are
# what carries the NUTS retirement suffix and the measure's unit, so this
# reader always asks for them.
.fetch_eurostat <- function(code, key, query = list()) {
  url <- paste0(.eurostat_base_url(), "/data/", code, "/", key)
  response <- httr::GET(
    url,
    query = c(list(format = "SDMX-CSV", label = "both"), query)
  )
  if (httr::http_error(response)) {
    cli::cli_abort(
      "Eurostat returned {.val {httr::status_code(response)}} for
       {.url {url}}."
    )
  }
  .eurostat_parse_sdmx(
    httr::content(response, as = "text", encoding = "UTF-8")
  )
}

# Everything is read as character: labels carry commas, quotes and
# non-breaking spaces, values carry Eurostat's own missing conventions,
# and the columns are typed once, deliberately, downstream.
.eurostat_parse_sdmx <- function(text) {
  if (!stringr::str_detect(text, "\n")) {
    cli::cli_abort("The SDMX-CSV response is empty or not a CSV document.")
  }
  readr::read_csv(
    I(text),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  ) |>
    dplyr::rename_with(
      \(x) stringr::str_replace_all(stringr::str_to_lower(x), " ", "_")
    )
}

# ---- Codes, labels and versions ---------------------------------------

.eurostat_label_code <- function(x) {
  stringr::str_extract(x, "^[^:]*")
}

.eurostat_label_name <- function(x) {
  dplyr::if_else(
    stringr::str_detect(x, ":"),
    stringr::str_remove(x, "^[^:]*:"),
    NA_character_
  )
}

# The nomenclature unsuffixed codes belong to. NUTS 2024 was established
# by EU regulation 2023/674 and is in force for 2024-2026; NUTS 2027
# (regulation 2026/195) applies from 2027. Source, checked 2026-09-02:
# https://ec.europa.eu/eurostat/web/nuts/history. Corroborated in the data
# itself: codes retired at NUTS 2021 (NL31, NL33, PT16, PT17, PT18) carry
# a "(NUTS 2021)" suffix, so unsuffixed codes are on its successor.
.eurostat_current_nuts <- function() {
  "2024"
}

.eurostat_nuts_version <- function(label) {
  suffix <- stringr::str_match(label, "\\(NUTS\\s+(\\d{4})\\)")[, 2L]
  dplyr::coalesce(suffix, .eurostat_current_nuts())
}

# A NUTS code is a two-letter country code plus up to three characters,
# one per level. Two shapes served alongside them are not units: the
# supranational aggregates, and codes joining several units for
# confidentiality (EL41_42 is served for 2007-2014 beside EL41 and EL42,
# so keeping it would double-count Greece's islands).
.eurostat_geo_class <- function(code) {
  dplyr::case_when(
    stringr::str_detect(code, "^(EU|EA)([0-9_].*)?$") ~ "supranational",
    stringr::str_detect(code, "_") ~ "composite",
    stringr::str_detect(code, "^[A-Z]{2}[A-Z0-9]{0,3}$") ~ "nuts",
    .default = "unknown"
  )
}

.eurostat_nuts_level <- function(code, geo_class) {
  dplyr::if_else(
    geo_class == "nuts",
    nchar(code) - 2L,
    NA_integer_
  )
}

# NUTS 2 is admin1 and NUTS 3 is admin2 for EU countries. NUTS 1 gets no
# grain on purpose: it is a first-order division in some countries and an
# aggregate of admin1 units in others, and deciding which is the grain
# rule's call, not this reader's.
.eurostat_grain <- function(nuts_level) {
  dplyr::case_when(
    nuts_level == 2L ~ "admin1",
    nuts_level == 3L ~ "admin2",
    .default = NA_character_
  )
}

# ---- Measures ---------------------------------------------------------

# The measure codes these four tables serve, with the unit each is
# published in and the factor to WHEP units. Enumerated live on
# 2026-09-02 from full-crop, full-strucpro slices of both crop tables and
# from the two livestock tables. `label_check` is the substring the
# response's own measure label must still contain: it is what makes the
# factor evidence rather than an assumption.
.eurostat_measure_map <- function() {
  tibble::tribble(
    ~measure_code,        ~quantity,    ~indicator_used,  ~value_unit, ~to_whep, ~label_check,
    "AR_THS_HA",          "area",       "area_harvested", "ha",        1000,     "thousand hectares",
    "MAR_THS_HA",         "area",       "area_main",      "ha",        1000,     "thousand hectares",
    "HPRD_THS_T",         "production", "production",     "tonnes",    1000,     "thousand tonnes",
    "HPRD_HUMD_EU_THS_T", "production", "production",     "tonnes",    1000,     "thousand tonnes",
    "THS_HD",             "heads",      NA_character_,    "heads",     1000,     "Thousand heads",
    "HD",                 "heads",      NA_character_,    "heads",     1,        "Head"
  )
}

.eurostat_check_measure_labels <- function(rows) {
  wrong <- rows |>
    dplyr::distinct(measure_code, measure_label, label_check) |>
    dplyr::filter(
      !stringr::str_detect(measure_label, stringr::fixed(label_check))
    )
  if (nrow(wrong) == 0L) {
    return(invisible(rows))
  }
  cli::cli_abort(c(
    "Eurostat changed the unit of {.val {wrong$measure_code}}.",
    i = "Expected a label containing {.val {wrong$label_check}}.",
    i = "The response says {.val {wrong$measure_label}}.",
    i = "Update {.fun .eurostat_measure_map} before trusting these values."
  ))
}

# The 2025 reference year moves crop statistics onto the SAIO regulation,
# which redefines the area concept per crop group (ESMS apro_cp_esms
# 3.4). Only apro_cpshr reaches that far; the historical table stops in
# 1999.
.eurostat_concept_break_year <- function() {
  2025L
}

# ---- Normalisation ----------------------------------------------------

# A misspelt filter name would otherwise be dropped in silence, and the
# request would quietly widen to everything Eurostat serves.
.eurostat_check_filters <- function(filters) {
  known <- c("geo", "items", "years")
  if (!is.list(filters)) {
    cli::cli_abort(
      "{.arg filters} must be a list, not {.cls {class(filters)}}."
    )
  }
  unknown <- setdiff(names(filters), known)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      "{.arg filters} accepts only {.val {known}}, not {.val {unknown}}."
    )
  }
  invisible(filters)
}

.eurostat_check_levels <- function(nuts_level) {
  levels <- suppressWarnings(as.integer(nuts_level))
  if (length(levels) == 0L || anyNA(levels) || !all(levels %in% 1:3)) {
    cli::cli_abort(
      "{.arg nuts_level} must be a non-empty subset of {.val {1:3}}, not
       {.val {nuts_level}}."
    )
  }
  unique(levels)
}

.eurostat_normalize <- function(raw, table, filters, levels_wanted) {
  .eurostat_assert_not_empty(raw, table, filters)
  rows <- raw |>
    .eurostat_split_dimensions(table) |>
    .eurostat_apply_measures(table) |>
    .eurostat_filter_years(filters$years)
  .eurostat_report_dropped_geo(rows)
  units <- rows |>
    dplyr::filter(geo_class == "nuts", nuts_level %in% levels_wanted) |>
    .eurostat_dedupe_vintage() |>
    dplyr::arrange(
      source_native_unit_id,
      source_native_item_code,
      quantity,
      year
    )
  structure(units, national_check = .eurostat_national_check(rows))
}

# Eurostat answers a key it serves no data for with the header row alone,
# which is a mis-specified request far more often than it is news about the
# world. Stopping is what keeps such a request out of a share denominator
# as a silent zero.
.eurostat_assert_not_empty <- function(raw, table, filters) {
  if (nrow(raw) > 0L) {
    return(invisible(raw))
  }
  asked <- purrr::imap_chr(
    filters,
    \(value, name) paste0(name, " = ", paste(value, collapse = ", "))
  )
  cli::cli_abort(c(
    "Eurostat served no observations for {.val {table}}.",
    i = if (length(asked) > 0L) {
      "Requested {.val {asked}}."
    } else {
      "The whole table came back empty."
    }
  ))
}

.eurostat_split_dimensions <- function(raw, table) {
  spec <- .eurostat_tables()[[table]]
  missing <- setdiff(
    c(
      "geo",
      "time_period",
      "obs_value",
      "last_update",
      spec$item_dim,
      spec$measure_dim
    ),
    names(raw)
  )
  if (length(missing) > 0L) {
    cli::cli_abort(
      "The SDMX-CSV response is missing column{?s} {.field {missing}}."
    )
  }
  out <- raw |>
    dplyr::mutate(
      item_raw = raw[[spec$item_dim]],
      measure_raw = raw[[spec$measure_dim]],
      source = paste0("Eurostat_", table),
      source_native_unit_id = .eurostat_label_code(geo),
      source_native_unit_name = .eurostat_label_name(geo),
      source_native_item_code = .eurostat_label_code(item_raw),
      source_native_item_name = .eurostat_label_name(item_raw),
      measure_code = .eurostat_label_code(measure_raw),
      measure_label = .eurostat_label_name(measure_raw),
      year = as.integer(time_period),
      source_version = last_update,
      recorded_at = .eurostat_recorded_at()
    )
  if (all(is.na(out$source_native_unit_name))) {
    cli::cli_abort(c(
      "The response carries no geography labels.",
      i = "Request it with {.code label=both}: the NUTS version is read
           from the label suffix, and without it every retired code would
           be silently dated to the current nomenclature."
    ))
  }
  out |>
    dplyr::mutate(
      geo_class = .eurostat_geo_class(source_native_unit_id),
      nuts_level = .eurostat_nuts_level(source_native_unit_id, geo_class),
      nuts_version = .eurostat_nuts_version(source_native_unit_name),
      grain = .eurostat_grain(nuts_level)
    )
}

.eurostat_apply_measures <- function(rows, table) {
  known <- .eurostat_measure_map()
  unknown <- setdiff(unique(rows$measure_code), known$measure_code)
  if (length(unknown) > 0L) {
    dropped <- sum(rows$measure_code %in% unknown)
    cli::cli_inform(
      "Dropped {dropped} row{?s} on measure{?s} {.val {unknown}}, which
       carry no admin-shares quantity."
    )
  }
  rows |>
    dplyr::inner_join(known, by = "measure_code") |>
    .eurostat_check_measure_labels() |>
    dplyr::mutate(
      value = as.numeric(obs_value) * to_whep,
      value_flag = .eurostat_value_flag(obs_flag, conf_status),
      concept_break = table == "apro_cpshr" &
        year >= .eurostat_concept_break_year()
    )
}

.eurostat_filter_years <- function(rows, years) {
  if (length(years) == 0L) {
    return(rows)
  }
  dplyr::filter(rows, year %in% as.integer(years))
}

.eurostat_value_flag <- function(obs_flag, conf_status) {
  flag <- dplyr::coalesce(obs_flag, "")
  conf <- dplyr::coalesce(conf_status, "")
  dplyr::case_when(
    nzchar(flag) & nzchar(conf) ~ paste(flag, conf, sep = "|"),
    nzchar(flag) ~ flag,
    nzchar(conf) ~ conf,
    .default = NA_character_
  )
}

.eurostat_recorded_at <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

.eurostat_report_dropped_geo <- function(rows) {
  dropped <- dplyr::filter(rows, geo_class != "nuts")
  if (nrow(dropped) == 0L) {
    return(invisible(rows))
  }
  codes <- sort(unique(dropped$source_native_unit_id))
  cli::cli_inform(
    "Dropped {nrow(dropped)} row{?s} on {length(codes)} non-unit
     geograph{?y/ies} {.val {codes}}: aggregates of units this reader also
     returns."
  )
  invisible(rows)
}

# The national totals, kept apart from the units so a caller can check the
# units against them without either one contaminating a share denominator.
.eurostat_national_check <- function(rows) {
  rows |>
    dplyr::filter(geo_class == "nuts", nuts_level == 0L) |>
    .eurostat_select_output() |>
    dplyr::arrange(
      source_native_unit_id,
      source_native_item_code,
      quantity,
      year
    )
}

# ---- Mixed NUTS vintages ----------------------------------------------

# One response can hold the same territory under two codes: apro_cpnhr_h
# carries FR21 for 1989-1999 and FRF2, a pure recoding of the same
# polygon, for 1990-1999. Keeping both would double the territory's mass
# in every overlapping year. Two rows are treated as the same territory
# only when they agree on country, NUTS level and label once the version
# suffix is stripped -- never across levels, where Eurostat reuses labels
# (FI2/FI20 "Aland", DE3/DE30 "Berlin"). The newest vintage wins; a tie at
# the newest vintage keeps everything and warns, because that is a label
# collision this rule cannot settle. A genuine boundary change between
# vintages produces different labels, so both rows survive for the
# polity-resolution step to settle on codes.
.eurostat_dedupe_vintage <- function(rows) {
  if (nrow(rows) == 0L) {
    return(.eurostat_select_output(rows))
  }
  # Grouped on the quantity rather than on Eurostat's measure code, so a
  # territory whose two vintages were served under different but
  # equivalent measure codes still deduplicates. The finer key would keep
  # both, which is the direction that double-counts.
  groups <- c(
    "country_code",
    "label_key",
    "nuts_level",
    "source_native_item_code",
    "quantity",
    "indicator_used",
    "year"
  )
  ranked <- rows |>
    dplyr::mutate(
      country_code = stringr::str_sub(source_native_unit_id, 1L, 2L),
      label_key = stringr::str_squish(
        stringr::str_remove(
          source_native_unit_name,
          "\\s*\\(NUTS\\s+\\d{4}\\)\\s*$"
        )
      ),
      vintage_rank = as.integer(nuts_version)
    ) |>
    dplyr::mutate(
      n_codes = dplyr::n_distinct(source_native_unit_id),
      max_rank = max(vintage_rank),
      n_at_max = dplyr::n_distinct(
        source_native_unit_id[vintage_rank == max_rank]
      ),
      kept_value = value[which(vintage_rank == max_rank)[1L]],
      .by = dplyr::all_of(groups)
    ) |>
    dplyr::mutate(
      keep_row = !(n_codes > 1L & n_at_max == 1L & vintage_rank < max_rank)
    )
  .eurostat_report_vintage_ties(ranked)
  .eurostat_report_vintage_drops(ranked)
  ranked |>
    dplyr::filter(keep_row) |>
    .eurostat_select_output()
}

.eurostat_report_vintage_ties <- function(ranked) {
  ties <- dplyr::filter(ranked, n_codes > 1L, n_at_max > 1L)
  if (nrow(ties) == 0L) {
    return(invisible(ranked))
  }
  codes <- sort(unique(ties$source_native_unit_id))
  cli::cli_warn(c(
    "{nrow(ties)} row{?s} share a label at the newest NUTS version.",
    i = "Code{?s} {.val {codes}} were all kept: the vintage preference
         cannot separate them."
  ))
  invisible(ranked)
}

.eurostat_report_vintage_drops <- function(ranked) {
  dropped <- dplyr::filter(ranked, !keep_row)
  if (nrow(dropped) == 0L) {
    return(invisible(ranked))
  }
  pairs <- dropped |>
    dplyr::distinct(source_native_unit_id, nuts_version) |>
    dplyr::mutate(
      pair = paste0(source_native_unit_id, " (NUTS ", nuts_version, ")")
    )
  disagree <- sum(
    !is.na(dropped$value) &
      !is.na(dropped$kept_value) &
      dropped$value != dropped$kept_value
  )
  cli::cli_inform(c(
    "Dropped {nrow(dropped)} older-vintage row{?s} for {nrow(pairs)}
     recoded territor{?y/ies}: {.val {pairs$pair}}.",
    i = "{disagree} dropped row{?s} disagreed in value with the newer
         vintage kept in its place."
  ))
  invisible(ranked)
}

.eurostat_select_output <- function(rows) {
  rows |>
    dplyr::select(
      "source",
      "source_native_unit_id",
      "source_native_unit_name",
      "source_native_item_code",
      "source_native_item_name",
      "quantity",
      "indicator_used",
      "year",
      "value",
      "value_unit",
      "value_flag",
      "concept_break",
      "grain",
      "nuts_level",
      "nuts_version",
      "source_version",
      "recorded_at"
    )
}
