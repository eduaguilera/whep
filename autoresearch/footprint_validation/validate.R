# Point-level validation harness for WHEP outputs.
#
# Idea: instead of scoring WHEP against one aggregate reference (e.g. FABIO),
# sample concrete factual points - "USA wheat production 2000", "China rice
# production 1970" - and check each against a value fetched from a source the
# field already trusts (USDA NASS, EUROSTAT, FAOSTAT, national statistics).
#
# A *probe* is one such point: {layer, area_iso3, item_name, year, element,
# unit}. WHEP values are EXTRACTED from a WHEP output tibble the caller passes
# in; this harness never triggers the remote pipeline itself, so the
# deterministic core runs offline and is fully testable.
#
# Ground truth lives in a pinned corpus (ground_truth.csv): each row is a value
# fetched once from a trusted source, with citation, definition and tolerance.
# Research (web subagents) populates it; checks are then deterministic and free
# - "research once, pin forever". judge_probes() canonicalises units and
# assigns a verdict per probe.
#
# Sourced by run_validation.R. Depends on whep datasets regions_full,
# items_prod_full, items_cbs for code lookups.

# --- Helpers -----------------------------------------------------------------

# Coerce to integer without the noisy "NAs introduced by coercion" warning;
# non-numeric codes (some lookup rows) become NA, which is the intended
# "did not resolve" signal.
.as_int <- function(x) {
  suppressWarnings(as.integer(x))
}

# Return the cached object if present, else run build_fn() and persist it.
# The cache key is the file path; callers encode the relevant parameters (e.g.
# years) in the name. The cache is NOT keyed on a code or input-pin hash, so
# delete the file or pass refresh = TRUE when the WHEP code or its input pins
# change, otherwise a stale build is reused.
harness_build_or_cache <- function(cache_path, build_fn, refresh = FALSE) {
  if (!refresh && file.exists(cache_path)) {
    cli::cli_alert_info("Using cached {.path {cache_path}}.")
    return(readRDS(cache_path))
  }
  result <- build_fn()
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_path)
  cli::cli_alert_success("Built and cached {.path {cache_path}}.")
  result
}

# --- Lookups -----------------------------------------------------------------

whep_validation_lookups <- function() {
  list(
    regions = whep::regions_full,
    items_prod = whep::items_prod_full,
    items_cbs = whep::items_cbs
  )
}

# Resolve human probe keys (ISO3 country, item name) to WHEP integer codes.
resolve_probes <- function(probes, lookups = whep_validation_lookups()) {
  area_lookup <- lookups$regions |>
    dplyr::transmute(
      area_iso3 = .data$iso3c,
      area_code = .as_int(.data$code),
      polity_area_code = .as_int(.data$polity_area_code)
    ) |>
    dplyr::filter(!is.na(.data$area_iso3)) |>
    dplyr::distinct(.data$area_iso3, .keep_all = TRUE)

  item_lookup <- lookups$items_prod |>
    dplyr::transmute(
      item_name = .data$item_prod,
      item_prod_code = .as_int(.data$item_prod_code),
      item_cbs_code = .as_int(.data$item_cbs_code)
    ) |>
    dplyr::distinct(.data$item_name, .keep_all = TRUE)

  probes |>
    dplyr::left_join(area_lookup, by = "area_iso3") |>
    dplyr::left_join(item_lookup, by = "item_name") |>
    dplyr::mutate(
      resolve_status = dplyr::case_when(
        is.na(.data$area_code) ~ "unknown_area",
        is.na(.data$item_prod_code) ~ "unknown_item",
        TRUE ~ "ok"
      )
    )
}

# --- WHEP value extraction ---------------------------------------------------

# whep_data is a named list of WHEP output tibbles keyed by layer: a
# "production" tibble from get_primary_production() and, when CBS probes are
# researched, a "cbs" tibble from build_commodity_balances().
extract_whep_values <- function(probes, whep_data) {
  layers <- unique(probes$layer)
  purrr::map_dfr(layers, function(layer) {
    rows <- dplyr::filter(probes, .data$layer == !!layer)
    extractor <- switch(
      layer,
      production = .extract_production,
      cbs = .extract_cbs,
      .extract_unsupported
    )
    extractor(rows, whep_data)
  })
}

# Production layer: harvested area is just unit == "ha" here.
.extract_production <- function(rows, whep_data) {
  rows <- dplyr::mutate(
    rows,
    item_prod_code = .as_int(.data$item_prod_code),
    year = .as_int(.data$year)
  )
  prod <- whep_data[["production"]]
  if (is.null(prod)) {
    return(dplyr::mutate(rows, whep_value = NA_real_, whep_unit = rows$unit))
  }
  prod |>
    dplyr::transmute(
      area_code = .as_int(.data$area_code),
      item_prod_code = .as_int(.data$item_prod_code),
      year = .as_int(.data$year),
      unit = .data$unit,
      whep_value = .data$value
    ) |>
    dplyr::right_join(
      rows,
      by = c("area_code", "item_prod_code", "year", "unit")
    ) |>
    dplyr::mutate(whep_unit = .data$unit)
}

# Commodity-balance layer: keyed by element (production, food, feed, ...).
.extract_cbs <- function(rows, whep_data) {
  rows <- dplyr::mutate(
    rows,
    item_cbs_code = .as_int(.data$item_cbs_code),
    year = .as_int(.data$year)
  )
  cbs <- whep_data[["cbs"]]
  if (is.null(cbs)) {
    return(dplyr::mutate(rows, whep_value = NA_real_, whep_unit = "tonnes"))
  }
  cbs |>
    dplyr::transmute(
      area_code = .as_int(.data$area_code),
      item_cbs_code = .as_int(.data$item_cbs_code),
      year = .as_int(.data$year),
      element = .data$element,
      whep_value = .data$value
    ) |>
    dplyr::right_join(
      rows,
      by = c("area_code", "item_cbs_code", "year", "element")
    ) |>
    dplyr::mutate(whep_unit = "tonnes")
}

# Footprint and any future layer: not wired as a probe target yet.
.extract_unsupported <- function(rows, whep_data) {
  dplyr::mutate(rows, whep_value = NA_real_, whep_unit = rows$unit)
}

# --- Judging -----------------------------------------------------------------

# checks must have: whep_value, whep_unit, gt_value, gt_unit, tolerance_pct.
judge_probes <- function(checks, default_tolerance_pct = 10) {
  base <- .unit_base_table()
  whep_base <- dplyr::rename(
    base,
    whep_unit = "unit",
    whep_dim = "dim",
    whep_factor = "factor"
  )
  gt_base <- dplyr::rename(
    base,
    gt_unit = "unit",
    gt_dim = "dim",
    gt_factor = "factor"
  )
  checks |>
    dplyr::left_join(whep_base, by = "whep_unit") |>
    dplyr::left_join(gt_base, by = "gt_unit") |>
    dplyr::mutate(
      tolerance_pct = dplyr::coalesce(
        .data$tolerance_pct,
        default_tolerance_pct
      ),
      whep_canonical = .data$whep_value * .data$whep_factor,
      gt_canonical = .data$gt_value * .data$gt_factor,
      ratio = .data$whep_canonical / .data$gt_canonical,
      deviation_pct = abs(.data$ratio - 1) * 100,
      verdict = dplyr::case_when(
        !is.na(.data$resolve_status) & .data$resolve_status != "ok" ~
          .data$resolve_status,
        is.na(.data$gt_value) ~ "uncovered",
        is.na(.data$whep_value) ~ "missing_whep",
        is.na(.data$whep_dim) | is.na(.data$gt_dim) ~ "unknown_unit",
        .data$whep_dim != .data$gt_dim ~ "unit_mismatch",
        .data$deviation_pct <= .data$tolerance_pct ~ "pass",
        .data$ratio > 1 ~ "flag_high",
        TRUE ~ "flag_low"
      )
    )
}

# Canonicalise reported units to one base per physical dimension so a WHEP
# value in "tonnes" can be compared to ground truth reported in "Mt", etc.
.unit_base_table <- function() {
  tibble::tribble(
    ~unit, ~dim, ~factor,
    "t", "mass_t", 1,
    "tonne", "mass_t", 1,
    "tonnes", "mass_t", 1,
    "kg", "mass_t", 1e-3,
    "kt", "mass_t", 1e3,
    "1000 tonnes", "mass_t", 1e3,
    "thousand tonnes", "mass_t", 1e3,
    "Mt", "mass_t", 1e6,
    "ha", "area_ha", 1,
    "kha", "area_ha", 1e3,
    "1000 ha", "area_ha", 1e3,
    "Mha", "area_ha", 1e6,
    "km2", "area_ha", 100,
    "heads", "count", 1,
    "head", "count", 1,
    "An", "count", 1,
    "1000 head", "count", 1e3
  )
}

# --- Orchestration -----------------------------------------------------------

run_validation <- function(
  probes,
  whep_data,
  corpus,
  lookups = whep_validation_lookups()
) {
  corpus_join <- corpus |>
    dplyr::select(
      "probe_id",
      "gt_value",
      "gt_unit",
      "tolerance_pct",
      "source",
      "url",
      "definition",
      "confidence"
    )

  probes |>
    resolve_probes(lookups) |>
    extract_whep_values(whep_data) |>
    dplyr::left_join(corpus_join, by = "probe_id") |>
    judge_probes() |>
    dplyr::select(
      "probe_id",
      "pool",
      "layer",
      "area_iso3",
      "item_name",
      "year",
      "element",
      "verdict",
      "whep_value",
      "whep_unit",
      "gt_value",
      "gt_unit",
      "ratio",
      "deviation_pct",
      "tolerance_pct",
      "source",
      "url",
      "definition",
      "confidence"
    )
}

# --- Ground-truth corpus I/O -------------------------------------------------

ground_truth_columns <- function() {
  c(
    "probe_id",
    "gt_value",
    "gt_unit",
    "source",
    "url",
    "definition",
    "tolerance_pct",
    "confidence",
    "retrieved_on",
    "researcher",
    "notes"
  )
}

read_ground_truth <- function(path) {
  if (!file.exists(path)) {
    return(.empty_ground_truth())
  }
  path |>
    readr::read_csv(show_col_types = FALSE, col_types = readr::cols()) |>
    dplyr::mutate(
      gt_value = as.numeric(.data$gt_value),
      tolerance_pct = as.numeric(.data$tolerance_pct)
    )
}

# Append researched rows, keeping the most recent entry per probe_id.
append_ground_truth <- function(path, new_rows) {
  existing <- read_ground_truth(path)
  dplyr::bind_rows(existing, new_rows) |>
    dplyr::distinct(.data$probe_id, .keep_all = TRUE) |>
    readr::write_csv(path)
}

.empty_ground_truth <- function() {
  cols <- ground_truth_columns()
  empty <- rlang::rep_named(cols, list(character(0)))
  out <- tibble::as_tibble(empty)
  out |>
    dplyr::mutate(
      gt_value = numeric(0),
      tolerance_pct = numeric(0)
    )
}
