# Source-exact Schulte-Uebbing grid-boundary accounting. Crops are aggregated
# before a cell consumes its one critical allowance; the resulting cell values
# are then attributed back to crops by Eduardo's approved pressure shares.

#' Build source-exact gridded critical-nitrogen exceedance.
#'
#' @description
#' Compares WHEP crop nitrogen pressure with the deposited Schulte-Uebbing et
#' al. (2022) 2010 yield-gap critical surface. The calculation first aggregates
#' every crop and polity contribution in a source cell, compares that one cell
#' pressure with one critical allowance, and only then attributes the cell
#' allowance, signed margin and positive overshoot back to crops. Total-input
#' attribution uses crop input shares; surplus attribution uses signed crop
#' surplus shares. Signed surplus shares can be negative or greater than one.
#'
#' The critical layer must carry its deposited `source_area_ha` and
#' `image_region`. `resolution = "cell"` returns one row per source cell and
#' year. Other resolutions return the approved crop attribution. Exactly zero
#' or numerically near-zero pressure denominators keep the complete cell result
#' but allocate no crop share; an explicit `cell_residual` record carries the
#' unallocated critical allowance, signed margin and positive overshoot. APIs
#' requiring complete crop attribution hard-error rather than fabricate a
#' fallback.
#'
#' @param surplus A [calculate_n_surplus()] output with the grid/crop/year key.
#'   Surplus mode uses signed `surplus_n_t` when present, otherwise derives it
#'   from `surplus_kgn_ha * area_ha / 1000`. Input mode uses
#'   `n_input_std_t`.
#' @param critical A [read_critical_n()] critical layer. In addition to layer
#'   identity, it must carry deposited `source_area_ha` and `image_region`.
#' @param land_use Source land class: `"ara"`, `"igl"`, or `"all"`.
#' @param resolution Output grain: source `"cell"`, crop-cell `"grid"`, crop
#'   `"polity"`/`"country"`, or crop `"image_region"`.
#' @param metric Selected actual pressure: signed `"surplus"` or total
#'   agricultural `"input"`.
#' @param cell_polity Retained for API compatibility. IMAGE membership now
#'   comes from the deposited cell-key crosswalk in `critical`; country
#'   attribution comes from the crop rows themselves.
#' @param allocation_scenario Only source-exact `"yield_gap"` is supported.
#'   `"no_increase"` and `"new_fixation"` hard-error.
#' @param actual_year The actual-pressure year to compare. Must select exactly
#'   one year from `surplus` and is always retained in results.
#' @param critical_reference_year Must be `2010`, matching the deposited fixed
#'   reference surface, and is always retained in results.
#' @param actual Alias of `surplus` for the selector-oriented interface.
#' @param boundary Alias of `critical` for the selector-oriented interface.
#' @param indicator Selector-oriented pressure name: `"surplus"`,
#'   `"total_input"`, or the unsupported `"new_fixation"` mode (which
#'   hard-errors). When supplied, it overrides `metric`.
#' @param land_class Alias of `land_use`.
#' @param impact_scope Deposited impact surface: `"mi"`, `"sw"`, `"gw"`, or
#'   `"de"`. When supplied, it is validated against the critical layer.
#' @param example If `TRUE`, return the package fixture.
#' @return A tibble at the requested grain. Cell results retain actual and
#'   critical masses, signed margin, positive overshoot, coverage state,
#'   integer source-grid key, IMAGE context, explicit years, selectors, and
#'   provenance. Crop results additionally retain the signed pressure share and
#'   crop-attributed quantities, which reconcile algebraically to the cell.
#' @export
#' @examples
#' build_n_boundary_exceedance(example = TRUE)
build_n_boundary_exceedance <- function(
  surplus = NULL,
  critical = NULL,
  land_use = c("ara", "all", "igl"),
  resolution = c("grid", "cell", "polity", "country", "image_region"),
  metric = c("surplus", "input", "new_fixation"),
  cell_polity = NULL,
  allocation_scenario = c("yield_gap", "no_increase", "new_fixation"),
  actual_year = NULL,
  critical_reference_year = NULL,
  actual = NULL,
  boundary = NULL,
  indicator = NULL,
  land_class = NULL,
  impact_scope = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_n_boundary_exceedance())
  }
  surplus <- actual %||% surplus
  critical <- boundary %||% critical
  if (!is.null(indicator)) {
    metric <- switch(
      indicator,
      total_input = "input",
      surplus = "surplus",
      new_fixation = "new_fixation",
      indicator
    )
  }
  if (!is.null(land_class)) {
    land_use <- land_class
  }
  land_use <- rlang::arg_match(land_use)
  resolution <- rlang::arg_match(resolution)
  metric <- .nbx_match_metric(metric)
  allocation_scenario <- .nbx_match_scenario(allocation_scenario)
  .nbx_validate_supported(metric, allocation_scenario)
  if (is.null(surplus) || is.null(critical)) {
    cli::cli_abort(
      "Both actual pressure and its boundary surface are required."
    )
  }
  critical <- .nbx_normalize_critical(critical, metric, land_use, impact_scope)
  .nbx_validate_years(surplus, critical, actual_year, critical_reference_year)
  actual_year <- as.integer(actual_year %||% unique(surplus$year))
  .check_columns(surplus, .nbx_surplus_required(metric), "surplus")
  .check_columns(
    critical,
    c(
      "lon",
      "lat",
      "value",
      "source_area_ha",
      "image_region",
      "critical_var",
      "critical_land_use",
      "critical_year"
    ),
    "critical"
  )
  .nbx_validate_critical(critical, metric, land_use)

  actual <- surplus |>
    dplyr::filter(.data$year == .env$actual_year) |>
    .nbx_filter_land_use(land_use) |>
    .nbx_prepare_actual(metric)
  support <- .nbx_prepare_critical(critical)
  cells <- .nbx_build_cells(actual, support, actual_year, metric, land_use)
  cells <- .nbx_stamp(
    cells,
    metric,
    land_use,
    allocation_scenario,
    actual_year,
    critical_reference_year
  )
  if (resolution == "cell") {
    return(.nbx_cell_cols(cells))
  }

  crop <- .nbx_attribute_crops(actual, cells, metric)
  .nbx_resolve(crop, resolution) |>
    .add_polity_columns_if_keyed()
}

.nbx_match_metric <- function(metric) {
  if (length(metric) > 1L) {
    metric <- metric[[1L]]
  }
  if (
    !is.character(metric) ||
      length(metric) != 1L ||
      !metric %in% c("surplus", "input", "new_fixation")
  ) {
    cli::cli_abort(
      "{.arg metric} must be one of {.val surplus}, {.val input}, or
       {.val new_fixation}."
    )
  }
  metric
}

.nbx_match_scenario <- function(x) {
  if (length(x) > 1L) {
    x <- x[[1L]]
  }
  if (
    !is.character(x) ||
      length(x) != 1L ||
      !x %in% c("yield_gap", "no_increase", "new_fixation")
  ) {
    cli::cli_abort(
      "{.arg allocation_scenario} must be {.val yield_gap},
       {.val no_increase}, or {.val new_fixation}."
    )
  }
  x
}

.nbx_validate_supported <- function(metric, scenario) {
  if (metric == "new_fixation" || scenario == "new_fixation") {
    cli::cli_abort(c(
      "The Schulte-Uebbing new-fixation grid mode is unsupported.",
      i = "An exact upstream critical surface or reproducing code is required."
    ))
  }
  if (scenario == "no_increase") {
    cli::cli_abort(c(
      "The Schulte-Uebbing no-increase grid mode is unsupported.",
      i = "An exact upstream critical surface or reproducing code is required."
    ))
  }
  invisible(TRUE)
}

.nbx_normalize_critical <- function(x, metric, land_use, impact_scope) {
  if (
    rlang::has_name(x, "critical_kgn_ha") &&
      !rlang::has_name(x, "value")
  ) {
    x <- dplyr::rename(x, value = "critical_kgn_ha")
  }
  if (
    rlang::has_name(x, "source_land_area_ha") &&
      !rlang::has_name(x, "source_area_ha")
  ) {
    x <- dplyr::rename(x, source_area_ha = "source_land_area_ha")
  }
  if (!rlang::has_name(x, "critical_var")) {
    x$critical_var <- if (metric == "input") {
      "critical_n_input"
    } else {
      "critical_n_surplus"
    }
  }
  if (!rlang::has_name(x, "critical_land_use")) {
    x$critical_land_use <- land_use
  }
  if (!rlang::has_name(x, "critical_year")) {
    x$critical_year <- if (rlang::has_name(x, "critical_reference_year")) {
      x$critical_reference_year
    } else {
      2010L
    }
  }
  if (!rlang::has_name(x, "critical_threshold")) {
    x$critical_threshold <- if (rlang::has_name(x, "impact_scope")) {
      x$impact_scope
    } else {
      "mi"
    }
  }
  if (!rlang::has_name(x, "image_region")) {
    x$image_region <- NA_integer_
  }
  if (!rlang::has_name(x, "critical_state")) {
    x$critical_state <- ifelse(
      is.na(x$value),
      "missing_critical",
      "valid"
    )
  }
  if (!is.null(impact_scope)) {
    scopes <- unique(x$critical_threshold[!is.na(x$critical_threshold)])
    if (length(scopes) > 0L && !identical(scopes, impact_scope)) {
      cli::cli_abort(c(
        "The critical impact scope does not match the request.",
        i = "Expected {.val {impact_scope}}; found {.val {scopes}}."
      ))
    }
  }
  x
}

.nbx_validate_years <- function(
  surplus,
  critical,
  actual_year,
  reference_year
) {
  .check_columns(surplus, "year", "surplus")
  years <- sort(unique(surplus$year[!is.na(surplus$year)]))
  if (is.null(actual_year)) {
    cli::cli_abort(c(
      "{.arg actual_year} must be supplied explicitly.",
      i = "The caller must record which annual pressure is compared with the
           fixed 2010 critical surface."
    ))
  }
  if (
    length(actual_year) != 1L ||
      !is.finite(actual_year) ||
      !actual_year %in% years
  ) {
    cli::cli_abort("{.arg actual_year} must select exactly one available year.")
  }
  if (is.null(reference_year)) {
    cli::cli_abort(
      "{.arg critical_reference_year = 2010} must be supplied explicitly."
    )
  }
  if (
    length(reference_year) != 1L ||
      !identical(as.integer(reference_year), 2010L)
  ) {
    cli::cli_abort(
      "The deposited yield-gap surfaces require
       {.arg critical_reference_year = 2010}."
    )
  }
  if (rlang::has_name(critical, "critical_year")) {
    crit_years <- unique(critical$critical_year[!is.na(critical$critical_year)])
    if (!identical(as.integer(crit_years), 2010L)) {
      cli::cli_abort("The critical layer is not the deposited 2010 reference.")
    }
  }
  invisible(TRUE)
}

.nbx_validate_critical <- function(critical, metric, land_use) {
  expected <- if (metric == "input") {
    "critical_n_input"
  } else {
    "critical_n_surplus"
  }
  vars <- unique(critical$critical_var[!is.na(critical$critical_var)])
  scopes <- unique(critical$critical_land_use[
    !is.na(critical$critical_land_use)
  ])
  if (!identical(vars, expected)) {
    cli::cli_abort(c(
      "The critical layer does not match {.arg metric = {metric}}.",
      i = "Expected {.val {expected}}; found {.val {vars}}."
    ))
  }
  if (!identical(scopes, land_use)) {
    cli::cli_abort(c(
      "The critical layer does not match {.arg land_use = {land_use}}.",
      i = "Found critical land-use scope {.val {scopes}}."
    ))
  }
  if (any(!is.finite(critical$source_area_ha) | critical$source_area_ha < 0)) {
    cli::cli_abort(
      "Critical source areas must be finite non-negative hectares."
    )
  }
  invisible(TRUE)
}

.nbx_surplus_required <- function(metric) {
  base <- c("lon", "lat", "area_code", "item_cbs_code", "year", "area_ha")
  if (metric == "input") {
    return(c(base, "n_input_std_t"))
  }
  base
}

.nbx_filter_land_use <- function(surplus, land_use) {
  grass <- c(3000L, 3002L, 3003L)
  x <- dplyr::filter(surplus, !is.na(.data$item_cbs_code))
  if (land_use == "ara") {
    return(dplyr::filter(x, !.data$item_cbs_code %in% grass))
  }
  if (land_use == "igl") {
    return(dplyr::filter(x, .data$item_cbs_code %in% grass))
  }
  x
}

.nbx_prepare_actual <- function(x, metric) {
  if (metric == "input") {
    x <- dplyr::mutate(x, actual_n_t = .data$n_input_std_t)
  } else if (rlang::has_name(x, "surplus_n_t")) {
    x <- dplyr::mutate(x, actual_n_t = .data$surplus_n_t)
  } else {
    .check_columns(x, "surplus_kgn_ha", "surplus")
    x <- dplyr::mutate(
      x,
      actual_n_t = .data$surplus_kgn_ha * .data$area_ha / 1000
    )
  }
  keyed <- .nbx_add_cell_key(x, "actual pressure")
  dplyr::select(
    keyed,
    "cell_id",
    "source_row",
    "source_col",
    "lon",
    "lat",
    "area_code",
    "item_cbs_code",
    "year",
    "area_ha",
    "actual_n_t",
    dplyr::any_of("production_n_t")
  )
}

.nbx_prepare_critical <- function(x) {
  x |>
    .nbx_add_cell_key("critical layer") |>
    dplyr::transmute(
      cell_id = .data$cell_id,
      source_row = .data$source_row,
      source_col = .data$source_col,
      lon = .data$lon,
      lat = .data$lat,
      critical_kgn_ha = .data$value,
      source_area_ha = .data$source_area_ha,
      image_region = as.integer(.data$image_region),
      critical_threshold = dplyr::coalesce(.data$critical_threshold, "mi"),
      critical_state = .data$critical_state,
      critical_present = TRUE
    ) |>
    .nbx_validate_support()
}

.nbx_add_cell_key <- function(x, source) {
  col <- round((x$lon + 179.75) / 0.5) + 1L
  row <- round((89.75 - x$lat) / 0.5) + 1L
  lon_expected <- -179.75 + (col - 1L) * 0.5
  lat_expected <- 89.75 - (row - 1L) * 0.5
  bad <- !is.finite(x$lon) |
    !is.finite(x$lat) |
    col < 1L |
    col > 720L |
    row < 1L |
    row > 360L |
    abs(x$lon - lon_expected) > 1e-9 |
    abs(x$lat - lat_expected) > 1e-9
  if (any(bad)) {
    cli::cli_abort(c(
      "{source} does not align to the canonical 0.5-degree source grid.",
      i = "Cell centres must follow -179.75 + 0.5*k longitude and
           89.75 - 0.5*k latitude."
    ))
  }
  dplyr::mutate(
    x,
    source_row = as.integer(row),
    source_col = as.integer(col),
    cell_id = as.integer((row - 1L) * 720L + col)
  )
}

.nbx_validate_support <- function(x) {
  duplicate <- duplicated(x$cell_id)
  if (any(duplicate)) {
    cli::cli_abort("The critical layer has duplicate canonical cell keys.")
  }
  bad_region <- !is.na(x$image_region) & !x$image_region %in% 1:26
  if (any(bad_region)) {
    cli::cli_abort(
      "The critical-domain IMAGE crosswalk must use regions 1--26."
    )
  }
  x
}

.nbx_build_cells <- function(actual, support, actual_year, metric, land_use) {
  actual_cell <- dplyr::summarise(
    actual,
    cell_actual_n_t = if (any(is.na(.data$actual_n_t))) {
      NA_real_
    } else {
      sum(.data$actual_n_t)
    },
    absolute_pressure_n_t = if (any(is.na(.data$actual_n_t))) {
      NA_real_
    } else {
      sum(abs(.data$actual_n_t))
    },
    .by = c("cell_id", "source_row", "source_col", "lon", "lat", "year")
  )
  full <- dplyr::full_join(
    support,
    actual_cell,
    by = c("cell_id", "source_row", "source_col", "lon", "lat"),
    relationship = "one-to-one"
  ) |>
    dplyr::mutate(
      year = dplyr::coalesce(.data$year, as.integer(actual_year)),
      coverage_state = dplyr::case_when(
        is.na(.data$critical_present) ~ "out_of_domain",
        is.na(.data$critical_kgn_ha) &
          .data$critical_state == "out_of_domain" ~ "out_of_domain",
        is.na(.data$critical_kgn_ha) ~ "missing_critical",
        .data$source_area_ha == 0 ~ "zero_land",
        is.na(.data$cell_actual_n_t) ~ "missing_actual",
        TRUE ~ "valid"
      ),
      cell_critical_n_t = dplyr::if_else(
        .data$coverage_state == "valid",
        .data$critical_kgn_ha * .data$source_area_ha / 1000,
        NA_real_
      ),
      cell_actual_kgn_ha = dplyr::if_else(
        .data$coverage_state == "valid",
        .data$cell_actual_n_t * 1000 / .data$source_area_ha,
        NA_real_
      ),
      cell_signed_margin_n_t = dplyr::if_else(
        .data$coverage_state == "valid",
        .data$cell_actual_n_t - .data$cell_critical_n_t,
        NA_real_
      ),
      cell_positive_overshoot_n_t = dplyr::if_else(
        .data$coverage_state == "valid",
        pmax(.data$cell_signed_margin_n_t, 0),
        NA_real_
      ),
      pressure_condition_ratio = dplyr::if_else(
        .data$coverage_state == "valid" & .data$absolute_pressure_n_t > 0,
        abs(.data$cell_actual_n_t) / .data$absolute_pressure_n_t,
        dplyr::if_else(
          .data$coverage_state == "valid" & .data$cell_actual_n_t == 0,
          0,
          NA_real_
        )
      )
    )
  full
}

.nbx_stamp <- function(
  x,
  metric,
  land_use,
  scenario,
  actual_year,
  reference_year
) {
  dplyr::mutate(
    x,
    actual_year = as.integer(actual_year),
    critical_reference_year = as.integer(reference_year),
    metric = .env$metric,
    indicator = if (.env$metric == "input") "total_input" else "surplus",
    land_use = .env$land_use,
    allocation_scenario = .env$scenario,
    method_boundary = "schulte_uebbing_grid",
    critical_source_doi = "10.5281/zenodo.6395016",
    critical_source_version = "1.0",
    archive_md5 = .critn_archive_md5(),
    urban_treatment = "included_provisionally_in_whep_actual",
    provisional_reason = paste(
      "urban allocation is provisional; manure-management boundary may differ",
      "from the source; intensive-grass scope depends on WHEP item mapping"
    )
  )
}

.nbx_attribute_crops <- function(actual, cells, metric) {
  valid <- dplyr::filter(cells, .data$coverage_state == "valid")
  cell_cols <- c(
    "cell_id",
    "source_row",
    "source_col",
    "lon",
    "lat",
    "year",
    "source_area_ha",
    "image_region",
    "critical_threshold",
    "cell_actual_n_t",
    "absolute_pressure_n_t",
    "critical_kgn_ha",
    "cell_critical_n_t",
    "cell_actual_kgn_ha",
    "cell_signed_margin_n_t",
    "cell_positive_overshoot_n_t",
    "pressure_condition_ratio",
    "coverage_state",
    "actual_year",
    "critical_reference_year",
    "metric",
    "indicator",
    "land_use",
    "allocation_scenario",
    "method_boundary",
    "critical_source_doi",
    "critical_source_version",
    "archive_md5",
    "urban_treatment",
    "provisional_reason"
  )
  joined <- dplyr::inner_join(
    actual,
    dplyr::select(valid, dplyr::all_of(cell_cols)),
    by = c("cell_id", "source_row", "source_col", "lon", "lat", "year"),
    relationship = "many-to-one"
  ) |>
    dplyr::mutate(
      attribution_defined = .data$cell_actual_n_t != 0 &
        .data$pressure_condition_ratio >= sqrt(.Machine$double.eps),
      pressure_share = dplyr::if_else(
        .data$attribution_defined,
        .data$actual_n_t / .data$cell_actual_n_t,
        NA_real_
      ),
      critical_n_t = dplyr::if_else(
        .data$attribution_defined,
        .data$pressure_share * .data$cell_critical_n_t,
        0
      ),
      crop_critical_n_t = .data$critical_n_t,
      signed_margin_n_t = dplyr::if_else(
        .data$attribution_defined,
        .data$pressure_share * .data$cell_signed_margin_n_t,
        0
      ),
      positive_overshoot_n_t = dplyr::if_else(
        .data$attribution_defined,
        .data$pressure_share * .data$cell_positive_overshoot_n_t,
        0
      ),
      exceedance_n_t = .data$positive_overshoot_n_t,
      within_boundary_n_t = dplyr::if_else(
        .data$attribution_defined,
        .data$actual_n_t - .data$exceedance_n_t,
        NA_real_
      ),
      unallocated_critical_n_t = 0,
      unallocated_signed_margin_n_t = 0,
      unallocated_positive_overshoot_n_t = 0,
      attribution_record_type = "crop_allocation",
      attribution_method = if (.env$metric == "input") {
        "crop_input_share"
      } else {
        "signed_crop_surplus_share"
      },
      attribution_status = dplyr::if_else(
        .data$attribution_defined,
        "defined",
        dplyr::if_else(
          .data$cell_actual_n_t == 0,
          "undefined_zero_denominator",
          "undefined_near_zero_denominator"
        )
      ),
      attribution_state = .data$attribution_status,
      land_scope_status = "provisional",
      urban_treatment = "included_provisional"
    )
  residual <- joined |>
    dplyr::filter(!.data$attribution_defined) |>
    dplyr::arrange(
      .data$cell_id,
      .data$year,
      .data$area_code,
      .data$item_cbs_code
    ) |>
    dplyr::slice_head(n = 1L, by = c("cell_id", "year")) |>
    dplyr::mutate(
      area_code = NA_integer_,
      item_cbs_code = NA_integer_,
      area_ha = NA_real_,
      actual_n_t = 0,
      production_n_t = NA_real_,
      pressure_share = NA_real_,
      critical_n_t = 0,
      crop_critical_n_t = 0,
      signed_margin_n_t = 0,
      positive_overshoot_n_t = 0,
      exceedance_n_t = 0,
      within_boundary_n_t = NA_real_,
      unallocated_critical_n_t = .data$cell_critical_n_t,
      unallocated_signed_margin_n_t = .data$cell_signed_margin_n_t,
      unallocated_positive_overshoot_n_t = .data$cell_positive_overshoot_n_t,
      attribution_record_type = "cell_residual"
    )
  .nbx_assert_reconciliation(dplyr::bind_rows(joined, residual))
}

.nbx_assert_reconciliation <- function(x, tolerance = 1e-10) {
  check <- dplyr::summarise(
    x,
    allocated_actual = sum(.data$actual_n_t),
    allocated_critical = sum(.data$critical_n_t),
    allocated_margin = sum(.data$signed_margin_n_t),
    allocated_overshoot = sum(.data$positive_overshoot_n_t),
    residual_critical = sum(.data$unallocated_critical_n_t),
    residual_margin = sum(.data$unallocated_signed_margin_n_t),
    residual_overshoot = sum(.data$unallocated_positive_overshoot_n_t),
    allocated_abs_actual = sum(abs(.data$actual_n_t)),
    allocated_abs_critical = sum(abs(.data$critical_n_t)),
    allocated_abs_margin = sum(abs(.data$signed_margin_n_t)),
    allocated_abs_overshoot = sum(abs(.data$positive_overshoot_n_t)),
    cell_actual = dplyr::first(.data$cell_actual_n_t),
    cell_critical = dplyr::first(.data$cell_critical_n_t),
    cell_margin = dplyr::first(.data$cell_signed_margin_n_t),
    cell_overshoot = dplyr::first(.data$cell_positive_overshoot_n_t),
    .by = c("cell_id", "year")
  )
  scale <- pmax(
    1,
    abs(check$cell_actual),
    abs(check$cell_critical),
    abs(check$cell_margin),
    abs(check$cell_overshoot)
  )
  residual <- pmax(
    abs(check$allocated_actual - check$cell_actual),
    abs(
      check$allocated_critical + check$residual_critical - check$cell_critical
    ),
    abs(check$allocated_margin + check$residual_margin - check$cell_margin),
    abs(
      check$allocated_overshoot +
        check$residual_overshoot -
        check$cell_overshoot
    )
  )
  allocated_scale <- pmax(
    1,
    check$allocated_abs_actual,
    check$allocated_abs_critical,
    check$allocated_abs_margin,
    check$allocated_abs_overshoot
  )
  numerical_bound <- pmax(
    tolerance * scale,
    64 * .Machine$double.eps * allocated_scale
  )
  if (any(!is.finite(residual) | residual > numerical_bound)) {
    cli::cli_abort("Crop attribution does not reconcile to its cell result.")
  }
  x
}

.nbx_cell_cols <- function(x) {
  dplyr::select(
    x,
    "cell_id",
    "source_row",
    "source_col",
    "lon",
    "lat",
    "year",
    "actual_year",
    "critical_reference_year",
    "source_area_ha",
    "image_region",
    "critical_threshold",
    "critical_kgn_ha",
    "cell_actual_kgn_ha",
    "cell_actual_n_t",
    "cell_critical_n_t",
    "cell_signed_margin_n_t",
    "cell_positive_overshoot_n_t",
    "pressure_condition_ratio",
    "coverage_state",
    "metric",
    "indicator",
    "land_use",
    "allocation_scenario",
    "method_boundary",
    "critical_source_doi",
    "critical_source_version",
    "archive_md5",
    "urban_treatment",
    "provisional_reason"
  ) |>
    tibble::as_tibble()
}

.nbx_resolve <- function(crop, resolution) {
  if (resolution == "grid") {
    return(.nbx_grid_cols(crop))
  }
  key <- if (resolution == "image_region") {
    c("image_region", "item_cbs_code", "year")
  } else {
    c("area_code", "item_cbs_code", "year")
  }
  .nbx_aggregate(crop, key)
}

.nbx_grid_cols <- function(x) {
  dplyr::select(
    x,
    "cell_id",
    "source_row",
    "source_col",
    "lon",
    "lat",
    "area_code",
    "item_cbs_code",
    "year",
    "actual_year",
    "critical_reference_year",
    "area_ha",
    "source_area_ha",
    "image_region",
    "critical_threshold",
    "actual_n_t",
    "pressure_share",
    "pressure_condition_ratio",
    "critical_n_t",
    "crop_critical_n_t",
    "signed_margin_n_t",
    "positive_overshoot_n_t",
    "unallocated_critical_n_t",
    "unallocated_signed_margin_n_t",
    "unallocated_positive_overshoot_n_t",
    "attribution_record_type",
    "exceedance_n_t",
    "within_boundary_n_t",
    dplyr::any_of("production_n_t"),
    "cell_actual_kgn_ha",
    "cell_actual_n_t",
    "critical_kgn_ha",
    "cell_critical_n_t",
    "cell_signed_margin_n_t",
    "cell_positive_overshoot_n_t",
    "coverage_state",
    "attribution_method",
    "attribution_status",
    "attribution_state",
    "land_scope_status",
    "metric",
    "indicator",
    "land_use",
    "allocation_scenario",
    "method_boundary",
    "critical_source_doi",
    "critical_source_version",
    "archive_md5",
    "urban_treatment",
    "provisional_reason"
  ) |>
    tibble::as_tibble()
}

.nbx_aggregate <- function(x, key) {
  mass <- intersect(
    c(
      "actual_n_t",
      "critical_n_t",
      "signed_margin_n_t",
      "crop_critical_n_t",
      "positive_overshoot_n_t",
      "exceedance_n_t",
      "within_boundary_n_t",
      "unallocated_critical_n_t",
      "unallocated_signed_margin_n_t",
      "unallocated_positive_overshoot_n_t",
      "production_n_t"
    ),
    names(x)
  )
  stamps <- intersect(
    c(
      "actual_year",
      "critical_reference_year",
      "metric",
      "indicator",
      "land_use",
      "allocation_scenario",
      "method_boundary",
      "critical_source_doi",
      "critical_source_version",
      "archive_md5",
      "urban_treatment",
      "provisional_reason",
      "attribution_method",
      "attribution_status",
      "attribution_state",
      "attribution_record_type"
    ),
    names(x)
  )
  dplyr::summarise(
    x,
    dplyr::across(dplyr::all_of(mass), .sum_if_any),
    dplyr::across(dplyr::all_of(stamps), dplyr::first),
    .by = dplyr::all_of(key)
  ) |>
    tibble::as_tibble()
}

# Shared with the scientifically distinct pathway-boundary implementation.
# That pathway still broadcasts a medium-specific critical loss to crop rows,
# but it must retain the established complete-coverage guard.
.n_join_critical_complete <- function(x, critical, value_col, source) {
  joined <- dplyr::left_join(
    x,
    critical,
    by = c("lon", "lat"),
    relationship = "many-to-one"
  )
  uncovered <- is.finite(joined$area_ha) &
    joined$area_ha > 0 &
    is.na(joined[[value_col]])
  if (!any(uncovered)) {
    return(joined)
  }
  cells <- joined[uncovered, c("lon", "lat"), drop = FALSE] |>
    dplyr::distinct()
  first_cells <- utils::head(sprintf("(%s, %s)", cells$lon, cells$lat), 5L)
  cli::cli_abort(c(
    "Critical-layer coverage is incomplete.",
    x = sprintf(
      "%s positive-area row(s) in %s cell(s) lack a non-missing value from %s.",
      sum(uncovered),
      nrow(cells),
      source
    ),
    i = sprintf(
      "First uncovered cell(s): %s.",
      paste(first_cells, collapse = ", ")
    )
  ))
}

# Shared legacy pathway helper. Pathway-boundary calculations still require a
# [0,1] pressure decomposition and are scientifically distinct from the signed
# cell-margin attribution above.
.n_exceed_split <- function(actual, critical) {
  raw <- dplyr::if_else(
    actual <= 0 | actual < critical,
    0,
    (actual - critical) / actual
  )
  pmin(1, pmax(0, raw))
}
