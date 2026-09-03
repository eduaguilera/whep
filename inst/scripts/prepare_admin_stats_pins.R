# -----------------------------------------------------------------------
# prepare_admin_stats_pins.R
#
# Builds the five tier-2 / tier-3 administrative-statistics families of the
# subnational spatialization (#1000, T24) out of the harmonized subnational
# production panel, stages one pin folder per family and writes the
# manifest recording what each family ships and under which permission. It
# uploads nothing: registering a pin (its `_pins.yaml` section and its
# `inst/extdata/whep_inputs.csv` row) is a separate, deliberate step.
#
# Input
# -----
# The panel at `Sys.getenv("WHEP_SUBNATIONAL")`, a single parquet file.
# Its own provenance lives beside it in `SOURCE.md`: a 2026-08-31 snapshot
# of the compilation, 8,929,673 rows, 26 countries, 455 admin units. Read
# `lane`, `source_primary` and `source_detail` before treating a row as an
# observation -- only `lane == "observational"` rows are directly
# transcribed statistics.
#
# What each family ships (T23, decided 2026-09-02)
# ------------------------------------------------
# Japan (prefectures), Spain (provinces), Australia (states and
# territories) and France (departments, livestock only) ship as RAW
# OBSERVED ROWS with attribution.
#
# The Latin American panel of Infante-Amate, Urrego-Mesa, Badia-Miro and
# Aguilera is in progress and ships as DERIVED SHARES ONLY: each row's
# share of its container's total for the same item, indicator and year,
# with no source value. Its co-authors agreed to that on 2026-09-02 and
# the agreement is recorded in the manifest this script writes. A `value`
# column must therefore never appear in that family; `read_admin_family()`
# aborts if one ever does.
#
# Row selection, common to every family
# -------------------------------------
# - `year >= 1961`;
# - `indicator` in area / production / livestock_stock (the panel's
#   `yield` and `landuse` rows are not allocation constraints);
# - `unit_canonical` in ha / tonnes / heads, matching those indicators;
# - a non-missing, non-negative `value_canonical`;
# - rows whose `quality_flag` names `duplicate_cell` or
#   `admin_level_overlap` are dropped and counted: both say the row is not
#   an independent observation of its own unit;
# - residual exact-key duplicates -- same country, unit, item, indicator
#   and year -- are collapsed by their mean and counted. The 2026-09-03
#   build has none in any family; the step exists so that a later snapshot
#   cannot double-count silently.
#
# Output shape
# ------------
# The source-native long shape the other admin-statistics readers emit
# (`R/admin_stats_nass.R`, `R/admin_stats_eurostat.R`,
# `R/admin_stats_sidra.R`), so that T25 resolves one contract: source,
# source_native_unit_id, source_native_unit_name, source_native_item_code,
# source_native_item_name, indicator_used, quantity, year, value (or
# share), value_unit, value_flag, grain, nuts_version, source_version,
# recorded_at.
#
# `nuts_version` is NA in every family. Spain's provinces and France's
# departments carry NUTS-shaped unit codes, but the panel states no NUTS
# vintage for them and this script does not guess one.
#
# Usage
# -----
#   Sys.setenv(WHEP_SUBNATIONAL = "<dir>/whep_production_subnational.parquet")
#   source("inst/scripts/prepare_admin_stats_pins.R")
#   result <- prepare_admin_stats_pins()
# -----------------------------------------------------------------------

ADMIN_PINS_MANIFEST_PATH <- file.path(
  "inst",
  "extdata",
  "admin_stats_pins_manifest.csv"
)

ADMIN_PINS_STAGING_DIR <- file.path("inst", "scripts", "pin_upload")

# The first year the subnational constraint covers.
ADMIN_PINS_FIRST_YEAR <- 1961L

# ---- The families -----------------------------------------------------

# One row per family: the pin alias, its tier, the reporting grain of its
# units, whether it ships source values or derived shares, and a label.
# Spain is `admin2` because its provinces sit below the NUTS-2 autonomous
# communities; every other family reports at its country's first
# administrative level.
.admin_pins_families <- function() {
  tibble::tribble(
    ~alias,                         ~tier, ~grain,   ~measure,
    "admin-stats-japan",            2L,    "admin1", "value",
    "admin-stats-spain-provinces",  2L,    "admin2", "value",
    "admin-stats-australia",        2L,    "admin1", "value",
    "admin-stats-france-livestock", 2L,    "admin1", "value",
    "admin-stats-latam",            3L,    "admin1", "share"
  )
}

# The panel's indicator vocabulary mapped onto the reader contract's.
# `area` is taken as harvested area, the indicator decision 8 binds; head
# counts have no FAOSTAT-style indicator, so `indicator_used` is NA there,
# exactly as the NASS reader leaves it.
.admin_pins_indicators <- function() {
  tibble::tribble(
    ~indicator,        ~indicator_used,  ~quantity,     ~expected_unit,
    "area",            "area_harvested", "area",        "ha",
    "production",      "production",     "production",  "tonnes",
    "livestock_stock", NA_character_,    "heads",       "heads"
  )
}

# Flags that say a row is not an independent observation of its own unit.
.admin_pins_drop_flags <- function() {
  c("duplicate_cell", "admin_level_overlap")
}

.admin_pins_panel_columns <- function() {
  c(
    "year",
    "indicator",
    "unit_canonical",
    "value_canonical",
    "quality_flag",
    "item_code",
    "item_clean",
    "admin_unit_id",
    "admin_name_clean",
    "country_clean",
    "source_primary",
    "source_detail",
    "lane"
  )
}

# ---- Reading the panel ------------------------------------------------

#' Open the harmonized subnational panel.
#'
#' Aborts with the instruction rather than guessing a path: the panel is a
#' local artifact, not a pin.
open_admin_panel <- function(panel_path = NULL) {
  panel_path <- panel_path %||% Sys.getenv("WHEP_SUBNATIONAL", "")
  if (!nzchar(panel_path)) {
    cli::cli_abort(c(
      "{.envvar WHEP_SUBNATIONAL} is not set.",
      i = "Point it at the harmonized subnational panel
           {.file whep_production_subnational.parquet}, then re-run."
    ))
  }
  if (!file.exists(panel_path)) {
    cli::cli_abort("No panel at {.file {panel_path}}.")
  }
  arrow::open_dataset(panel_path)
}

# Pull one family out of the panel. The pushdown is an equality on
# `country_clean` or `source_primary` -- the two columns that partition the
# panel -- and everything finer is decided in R, where the rule reads.
.admin_pins_family_rows <- function(panel, alias) {
  med <- "PI_COMPILATION:MEDITERRANEAN_SUBNATIONAL"
  rows <- switch(
    alias,
    "admin-stats-japan" = .admin_pins_pull(panel, "country_clean", "Japan") |>
      dplyr::filter(
        startsWith(.data$source_primary, "NATIONAL_OFFICIAL:JPN")
      ),
    "admin-stats-spain-provinces" = .admin_pins_pull(
      panel,
      "country_clean",
      "Spain"
    ) |>
      dplyr::filter(
        .data$source_primary == med,
        .data$lane == "observational"
      ),
    "admin-stats-australia" = .admin_pins_pull(
      panel,
      "country_clean",
      "Australia"
    ) |>
      dplyr::filter(.data$source_primary == "NATIONAL_OFFICIAL:AUS:ABS"),
    "admin-stats-france-livestock" = .admin_pins_pull(
      panel,
      "country_clean",
      "France"
    ) |>
      dplyr::filter(
        .data$source_primary == med,
        .data$indicator == "livestock_stock"
      ),
    "admin-stats-latam" = .admin_pins_pull(
      panel,
      "source_primary",
      "PI_COMPILATION:LATAM_SUBNATIONAL"
    ),
    cli::cli_abort("No family definition for {.val {alias}}.")
  )
  if (nrow(rows) == 0) {
    cli::cli_abort("The panel holds no rows for {.val {alias}}.")
  }
  rows
}

.admin_pins_pull <- function(panel, column, value) {
  panel |>
    dplyr::filter(!!rlang::sym(column) == value) |>
    dplyr::select(dplyr::all_of(.admin_pins_panel_columns())) |>
    dplyr::collect()
}

# ---- Filters ----------------------------------------------------------

# The common row selection. Returns the kept rows and the size of every
# drop, so a run reports what it removed and not only what it kept.
.admin_pins_filter <- function(rows) {
  spec <- .admin_pins_indicators()
  in_scope <- rows |>
    dplyr::filter(
      .data$year >= ADMIN_PINS_FIRST_YEAR,
      .data$indicator %in% spec$indicator,
      .data$unit_canonical %in% spec$expected_unit,
      !is.na(.data$value_canonical),
      .data$value_canonical >= 0
    )
  flagged <- .admin_pins_is_flagged(in_scope$quality_flag)
  list(
    rows = in_scope[!flagged, ],
    rows_in = nrow(rows),
    rows_in_scope = nrow(in_scope),
    rows_flag_dropped = sum(flagged),
    units_in_scope = dplyr::n_distinct(in_scope$admin_unit_id)
  )
}

.admin_pins_is_flagged <- function(flag) {
  pattern <- stringr::str_c(.admin_pins_drop_flags(), collapse = "|")
  stringr::str_detect(dplyr::coalesce(flag, ""), pattern)
}

# One name per unit id. The panel maps several native labels onto one id
# where a unit was renamed or transliterated (README_map_admin_units_v2.md
# lists 13 such ids); the pin carries the most frequent label, ties broken
# alphabetically, so the diagnostic name is stable across runs.
.admin_pins_unit_names <- function(rows) {
  rows |>
    dplyr::count(.data$admin_unit_id, .data$admin_name_clean) |>
    dplyr::arrange(
      .data$admin_unit_id,
      dplyr::desc(.data$n),
      .data$admin_name_clean
    ) |>
    dplyr::distinct(.data$admin_unit_id, .keep_all = TRUE) |>
    dplyr::select(
      "admin_unit_id",
      source_native_unit_name = "admin_name_clean"
    )
}

.admin_pins_keys <- function() {
  c(
    "country_clean",
    "admin_unit_id",
    "item_clean",
    "item_code",
    "indicator",
    "unit_canonical",
    "year"
  )
}

# Collapse residual exact-key duplicates by their mean. Flags and source
# strings of a collapsed group are joined rather than picked, so a collapse
# cannot hide that its rows disagreed. Only the duplicated groups go
# through the row-wise join -- a per-group summarise over a million
# single-row groups costs minutes and buys nothing.
.admin_pins_collapse <- function(rows) {
  keys <- .admin_pins_keys()
  prepared <- rows |>
    dplyr::mutate(
      value = .data$value_canonical,
      value_flag = dplyr::na_if(dplyr::coalesce(.data$quality_flag, ""), "")
    ) |>
    dplyr::select(dplyr::all_of(c(
      keys,
      "value",
      "value_flag",
      "source_primary"
    )))
  duplicated_keys <- prepared |>
    dplyr::mutate(n_rows = dplyr::n(), .by = dplyr::all_of(keys)) |>
    dplyr::filter(.data$n_rows > 1L)
  if (nrow(duplicated_keys) == 0) {
    return(prepared)
  }
  collapsed <- duplicated_keys |>
    dplyr::summarise(
      value = mean(.data$value),
      value_flag = .admin_pins_join(.data$value_flag),
      source_primary = .admin_pins_join(.data$source_primary),
      .by = dplyr::all_of(keys)
    )
  prepared |>
    dplyr::anti_join(collapsed, by = keys) |>
    dplyr::bind_rows(collapsed)
}

.admin_pins_join <- function(values) {
  kept <- unique(values[!is.na(values) & nzchar(values)])
  if (length(kept) == 0) {
    return(NA_character_)
  }
  stringr::str_c(sort(kept), collapse = "; ")
}

# ---- The tier-3 share derivation --------------------------------------

# The Latin American family ships shares, so its source values never leave
# this script. A country whose only unit is its own national total carries
# no within-country shape -- its share is 1 by construction -- and is
# dropped rather than shipped as a unit; a group whose units all report
# zero has no defined share and is dropped too. Both are counted.
.admin_pins_shares <- function(rows) {
  units <- rows |>
    dplyr::summarise(
      n_units = dplyr::n_distinct(.data$admin_unit_id),
      unit = .admin_pins_join(unique(.data$admin_unit_id)),
      .by = "country_clean"
    )
  single <- units |> dplyr::filter(.data$n_units == 1L)
  kept <- rows |>
    dplyr::filter(!.data$country_clean %in% single$country_clean) |>
    dplyr::mutate(
      total = sum(.data$value),
      .by = c("country_clean", "item_clean", "indicator", "year")
    )
  zero <- kept |> dplyr::filter(.data$total <= 0)
  list(
    rows = kept |>
      dplyr::filter(.data$total > 0) |>
      dplyr::mutate(share = .data$value / .data$total) |>
      dplyr::select(-"value", -"total"),
    dropped_countries = single,
    rows_single_unit = sum(rows$country_clean %in% single$country_clean),
    rows_zero_group = nrow(zero),
    groups_zero = nrow(dplyr::distinct(
      zero,
      .data$country_clean,
      .data$item_clean,
      .data$indicator,
      .data$year
    ))
  )
}

# ---- The output shape -------------------------------------------------

.admin_pins_shape <- function(rows, spec, recorded_at) {
  measure <- spec$measure
  tibble::tibble(
    source = spec$alias,
    source_native_unit_id = rows$admin_unit_id,
    source_native_unit_name = rows$source_native_unit_name,
    source_native_item_code = as.character(rows$item_code),
    source_native_item_name = rows$item_clean,
    indicator_used = rows$indicator_used,
    quantity = rows$quantity,
    year = as.integer(rows$year),
    "{measure}" := rows[[measure]],
    value_unit = rows$unit_canonical,
    value_flag = rows$value_flag,
    grain = spec$grain,
    nuts_version = NA_character_,
    source_version = rows$source_version,
    recorded_at = recorded_at
  ) |>
    dplyr::arrange(
      .data$year,
      .data$source_native_item_name,
      .data$source_native_unit_id
    )
}

# The vintage each family is read at. The Latin American panel states its
# own snapshot date in `source_detail`, which is parsed rather than typed
# in; the others carry no date, so their vintage is the compilation
# identifier the panel gives every row.
.admin_pins_source_version <- function(rows, alias, source_detail) {
  if (alias != "admin-stats-latam") {
    return(rows$source_primary)
  }
  matched <- stringr::str_match(
    source_detail,
    "[Ss]napshot imported (\\d{4}-\\d{2}-\\d{2})"
  )
  dates <- unique(matched[, 2])
  dates <- dates[!is.na(dates)]
  if (length(dates) != 1) {
    cli::cli_abort(c(
      "The {.val {alias}} rows carry {length(dates)} snapshot date{?s}.",
      i = "Expected exactly one, parsed from {.field source_detail}."
    ))
  }
  rep(dates, nrow(rows))
}

# ---- One family -------------------------------------------------------

#' Build one family's pin table from the panel.
#'
#' Returns the table, its attribution text and the counts of every row the
#' build dropped.
build_admin_family <- function(panel, alias, recorded_at) {
  spec <- .admin_pins_families() |>
    dplyr::filter(.data$alias == !!alias)
  raw <- .admin_pins_family_rows(panel, alias)
  detail <- unique(raw$source_detail)
  filtered <- .admin_pins_filter(raw)
  unit_names <- .admin_pins_unit_names(filtered$rows)
  collapsed <- .admin_pins_collapse(filtered$rows)
  shares <- if (spec$measure == "share") .admin_pins_shares(collapsed)
  measured <- shares$rows %||% collapsed
  built <- measured |>
    dplyr::left_join(unit_names, by = "admin_unit_id") |>
    dplyr::left_join(.admin_pins_indicators(), by = "indicator") |>
    dplyr::mutate(
      source_version = .admin_pins_source_version(
        dplyr::pick(dplyr::everything()),
        alias,
        detail
      )
    ) |>
    .admin_pins_shape(spec, recorded_at)
  list(
    alias = alias,
    spec = spec,
    data = built,
    attribution = .admin_pins_attribution(spec, unique(built$source_version)),
    report = .admin_pins_report(
      alias,
      spec,
      filtered,
      collapsed,
      built,
      shares
    )
  )
}

.admin_pins_report <- function(
  alias,
  spec,
  filtered,
  collapsed,
  built,
  shares
) {
  tibble::tibble(
    alias = alias,
    tier = spec$tier,
    measure = spec$measure,
    rows_in = filtered$rows_in,
    rows_in_scope = filtered$rows_in_scope,
    rows_flag_dropped = filtered$rows_flag_dropped,
    rows_collapsed = nrow(filtered$rows) - nrow(collapsed),
    rows_single_unit = shares$rows_single_unit %||% 0L,
    rows_zero_group = shares$rows_zero_group %||% 0L,
    groups_zero = shares$groups_zero %||% 0L,
    rows_out = nrow(built),
    units_in_scope = filtered$units_in_scope,
    units_out = dplyr::n_distinct(built$source_native_unit_id),
    items_out = dplyr::n_distinct(built$source_native_item_name),
    year_min = min(built$year),
    year_max = max(built$year),
    dropped_countries = .admin_pins_join(
      shares$dropped_countries$country_clean %||% character(0)
    )
  )
}

# ---- Attribution and permission ---------------------------------------

# Tier 2 ships observed rows, so the manifest carries the attribution the
# compilation is used under. Tier 3 ships derived shares under an explicit
# co-author agreement, and the manifest is where that agreement lives.
.admin_pins_attribution <- function(spec, source_primary) {
  if (spec$measure == "share") {
    return(paste0(
      "derived shares only from the Infante-Amate, Urrego-Mesa, ",
      "Badia-Miro, Aguilera subnational panel (in progress); source ",
      "values withheld until publication; co-author agreement recorded ",
      "2026-09-02 (Aguilera)"
    ))
  }
  paste0(
    "in-house compilation of official statistics; attribution: ",
    stringr::str_c(sort(source_primary), collapse = "; ")
  )
}

# ---- Staging and manifest ---------------------------------------------

# The two files `create_version()` in prepare_upload.R writes, in the same
# `<alias>/<version>/` layout, staged locally. Nothing is uploaded.
stage_admin_pin <- function(
  data,
  alias,
  staging_dir = ADMIN_PINS_STAGING_DIR,
  version = NULL
) {
  version <- version %||% format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  target <- file.path(staging_dir, alias, version)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(target, paste0(alias, c(".csv", ".parquet")))
  readr::write_csv(data, paths[[1L]])
  nanoparquet::write_parquet(data, paths[[2L]])
  cli::cli_alert_success("{alias}: staged at {.file {target}}")
  tibble::tibble(
    alias = alias,
    pin_version = version,
    parquet_file = basename(paths[[2L]]),
    bytes = as.numeric(unname(file.info(paths[[2L]])$size)),
    md5 = unname(tools::md5sum(paths[[2L]])),
    csv_bytes = as.numeric(unname(file.info(paths[[1L]])$size))
  )
}

#' Write the per-family manifest.
#'
#' One row per family: what it ships, how much of it, and the attribution
#' or permission it ships under.
write_admin_pins_manifest <- function(
  families,
  staged,
  path = ADMIN_PINS_MANIFEST_PATH,
  retrieved_at
) {
  out <- families |>
    purrr::map(\(fam) {
      fam$report |>
        dplyr::select(
          "alias",
          "tier",
          "measure",
          rows = "rows_out",
          units = "units_out",
          "year_min",
          "year_max"
        ) |>
        dplyr::mutate(attribution = fam$attribution)
    }) |>
    purrr::list_rbind() |>
    dplyr::left_join(staged, by = "alias") |>
    dplyr::mutate(retrieved_at = retrieved_at) |>
    dplyr::select(
      "alias",
      "tier",
      "measure",
      "rows",
      "units",
      "year_min",
      "year_max",
      "attribution",
      "pin_version",
      "parquet_file",
      "bytes",
      "md5",
      "retrieved_at"
    )
  readr::write_csv(out, path)
  cli::cli_alert_success("admin-stats: manifest at {.file {path}}")
  out
}

# ---- Entry point ------------------------------------------------------

#' Build, stage and manifest the five admin-statistics family pins.
#'
#' Uploads nothing. Returns the built tables, the per-family build report,
#' the staged-file record and the manifest.
prepare_admin_stats_pins <- function(
  panel_path = NULL,
  aliases = NULL,
  staging_dir = ADMIN_PINS_STAGING_DIR,
  manifest_path = ADMIN_PINS_MANIFEST_PATH,
  stage = TRUE
) {
  panel <- open_admin_panel(panel_path)
  known <- .admin_pins_families()$alias
  aliases <- aliases %||% known
  unknown <- setdiff(aliases, known)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown famil{?y/ies}: {.val {unknown}}.",
      i = "Known famil{?y/ies}: {.val {known}}."
    ))
  }
  recorded_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  version <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  families <- aliases |>
    rlang::set_names() |>
    purrr::map(\(alias) build_admin_family(panel, alias, recorded_at))
  report <- families |> purrr::map(\(fam) fam$report) |> purrr::list_rbind()
  .admin_pins_print_report(report)
  if (!stage) {
    return(list(families = families, report = report, manifest = NULL))
  }
  staged <- families |>
    purrr::map(\(fam) {
      stage_admin_pin(fam$data, fam$alias, staging_dir, version)
    }) |>
    purrr::list_rbind()
  manifest <- write_admin_pins_manifest(
    families,
    staged,
    manifest_path,
    recorded_at
  )
  .admin_pins_print_next_steps(staged, staging_dir)
  list(
    families = families,
    report = report,
    staged = staged,
    manifest = manifest
  )
}

.admin_pins_print_report <- function(report) {
  cli::cli_h2("Admin-statistics families")
  print(as.data.frame(report), row.names = FALSE)
}

.admin_pins_print_next_steps <- function(staged, staging_dir) {
  cli::cli_alert_info(c(
    "Nothing uploaded. To publish a family: upload its folder under
     {.file {staging_dir}} to the board, add
     {.val {paste0('<alias>/', staged$pin_version[[1L]], '/')}} under that
     alias's section of {.file _pins.yaml}, add one row per alias to
     {.file inst/extdata/whep_inputs.csv} and rebuild with
     {.file data-raw/whep_inputs.R}."
  ))
}
