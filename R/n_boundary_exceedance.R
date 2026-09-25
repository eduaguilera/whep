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
#' Actual-pressure rows naming no crop cannot meet a critical allowance and are
#' excluded before the cell comparison. The exclusion is reported, never
#' silent: a message names the rows and the pressure they carried when that
#' pressure is zero (the only case a gridded [build_nitrogen_balance()]
#' produces), and a warning when it is not.
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
#' @param negative_critical Treatment of a cell whose critical value is below
#'   zero. `"keep"` (default) compares the actual pressure with the deposited
#'   value as it is, as the source does. `"clamp"` sets it to zero (a zero
#'   allowance) before the cell comparison. The choice is stamped in every
#'   output row as `negative_critical`; see the Negative critical surplus
#'   section.
#' @param binding Optional [build_critical_n_binding()] output for the same
#'   land-use scope. When supplied, its per-cell `binding_threshold` and
#'   `binding_matches_mi` are carried into the cell and grid results; when
#'   `NULL` (default) both columns are `NA`.
#' @param example If `TRUE`, return the package fixture.
#' @return A tibble at the requested grain. Cell results retain actual and
#'   critical masses, signed margin, positive overshoot, coverage state,
#'   integer source-grid key, IMAGE context, explicit years, selectors, and
#'   provenance. `critical_kgn_ha` is the value compared (after the
#'   `negative_critical` treatment) and `source_critical_kgn_ha` the deposited
#'   one; the two differ only in clamped cells. Crop results additionally
#'   retain the signed pressure share and crop-attributed quantities, which
#'   reconcile algebraically to the cell. `exceedance_n_t` is the crop's share
#'   of the cell overshoot `pmax(actual - critical, 0)` and
#'   `within_boundary_n_t` is `actual_n_t - exceedance_n_t`, so the two always
#'   sum to the actual pressure. Summed over a cell, `within_boundary_n_t` is
#'   `min(actual, critical)`: under `negative_critical = "keep"` it is negative
#'   wherever the critical value is negative, and under `"clamp"` it is
#'   negative only where the actual pressure itself is.
#'
#' @section Negative critical surplus:
#' Schulte-Uebbing et al. (2022, Methods) set critical fertilizer and manure
#' inputs to zero where non-agricultural losses alone exceed a threshold, but
#' keep biological fixation and deposition in the critical input, so their
#' deposited critical surplus stays negative in those cells (on the `"mi"`
#' surface: 1,796 of 28,881 cells for `"all"`, minimum -396 kg N/ha; 2,075 of
#' 28,573 cells for `"ara"`, minimum -317 kg N/ha). `negative_critical =
#' "keep"` follows the source, and it is the setting under which the published
#' 2010 decomposition (43 Mt N allowable plus 76 Mt N exceedance, 119 Mt N
#' current surplus) is reproduced. `"clamp"` is a declared departure from the
#' source: it gives such cells a zero allowance instead of a negative one,
#' which lowers their overshoot to the actual pressure and keeps the cell
#' within-boundary mass at or above zero wherever the actual pressure is.
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
  negative_critical = c("keep", "clamp"),
  binding = NULL,
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
  negative_critical <- rlang::arg_match(negative_critical)
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
    .nbx_filter_land_use(land_use, metric) |>
    .nbx_prepare_actual(metric)
  support <- .nbx_prepare_critical(critical) |>
    .nbx_treat_negative(negative_critical) |>
    .nbx_join_binding(binding, land_use)
  cells <- .nbx_build_cells(actual, support, actual_year, metric, land_use)
  cells <- .nbx_stamp(
    cells,
    metric,
    land_use,
    allocation_scenario,
    actual_year,
    critical_reference_year
  ) |>
    dplyr::mutate(negative_critical = .env$negative_critical)
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

.nbx_filter_land_use <- function(surplus, land_use, metric) {
  grass <- c(3000L, 3002L, 3003L)
  .nbx_report_no_crop(surplus, metric)
  x <- dplyr::filter(surplus, !is.na(.data$item_cbs_code))
  if (land_use == "ara") {
    return(dplyr::filter(x, !.data$item_cbs_code %in% grass))
  }
  if (land_use == "igl") {
    return(dplyr::filter(x, .data$item_cbs_code %in% grass))
  }
  x
}

# A row naming no crop cannot meet a critical allowance -- the allowance is
# defined per crop-carrying cell -- so it has to leave before the comparison.
# Deciding that with a bare is.na() and saying nothing is what #532 objected
# to and what #1173 asks for here: name the rows and the pressure they carried,
# so a consumer can see what the denominators below exclude.
#
# On the package's own chain that pressure is exactly zero, and not by luck.
# build_nitrogen_balance() refuses a grid n_inputs whose key is incomplete
# (.nb_validate_input_grain()), so the only crop-less rows a gridded balance
# carries are the ones .nb_merge_output_term()'s full join MANUFACTURES for a
# cell whose SOM sequestration has no input row to attach to: every numeric
# column on such a row is the join's zero fill except som_sequestration_n_t,
# and .nb_cap_som() then caps that to pmax(0, inputs - other outputs) = 0.
# A join that creates rows which a later filter removes is a round trip that
# looks clean at both ends, which is exactly why it needs saying out loud.
# A NON-zero mass means the surplus was assembled some other way, and then the
# exclusion does move every cell denominator -- hence the warning.
.nbx_report_no_crop <- function(surplus, metric) {
  dropped <- dplyr::filter(surplus, is.na(.data$item_cbs_code))
  n_dropped <- nrow(dropped)
  if (n_dropped == 0L) {
    return(invisible(NULL))
  }
  mass <- sum(.nbx_actual_mass(dropped, metric), na.rm = TRUE)
  if (isTRUE(all.equal(mass, 0, tolerance = 1e-8))) {
    cli::cli_inform(
      c(
        "i" = "{n_dropped} actual-pressure row{?s} name{?s/} no crop and
               leave{?s/} before the cell comparison.",
        "i" = "Excluded {metric} pressure: {mass} t N, so no cell denominator
               changes."
      ),
      class = "whep_nbx_no_crop_dropped"
    )
    return(invisible(NULL))
  }
  cli::cli_warn(
    c(
      "{n_dropped} actual-pressure row{?s} name{?s/} no crop and leave{?s/}
       before the cell comparison.",
      x = "Excluded {metric} pressure: {mass} t N, absent from every cell
           denominator below and from the crop attribution.",
      i = "A gridded {.fn build_nitrogen_balance} emits a crop-less row only
           as the zero-mass artefact of its output-term full join; a non-zero
           one means the surplus was assembled another way."
    ),
    class = "whep_nbx_no_crop_mass"
  )
  invisible(NULL)
}

# The pressure column the cell comparison actually reads, in the metric's own
# currency. Shared with .nbx_report_no_crop() so an excluded row is reported
# in the same units the comparison would have used it in.
.nbx_actual_mass <- function(x, metric) {
  if (metric == "input") {
    return(x$n_input_std_t)
  }
  if (rlang::has_name(x, "surplus_n_t")) {
    return(x$surplus_n_t)
  }
  .check_columns(x, "surplus_kgn_ha", "surplus")
  x$surplus_kgn_ha * x$area_ha / .kg_per_tonne()
}

.nbx_prepare_actual <- function(x, metric) {
  mass <- .nbx_actual_mass(x, metric)
  x <- dplyr::mutate(x, actual_n_t = .env$mass)
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

# The deposited critical value is kept as `source_critical_kgn_ha`;
# `critical_kgn_ha` becomes the value the cell comparison actually uses. Under
# "keep" the two are identical. Under "clamp" a negative allowance becomes zero:
# a declared departure from Schulte-Uebbing et al. (2022), who keep negative
# critical surpluses (see the "Negative critical surplus" roxygen section).
# pmax() leaves a missing critical value missing, so coverage is unaffected.
.nbx_treat_negative <- function(support, negative_critical) {
  dplyr::mutate(
    support,
    source_critical_kgn_ha = .data$critical_kgn_ha,
    critical_kgn_ha = if (.env$negative_critical == "clamp") {
      pmax(.data$critical_kgn_ha, 0)
    } else {
      .data$critical_kgn_ha
    }
  )
}

# Carry the per-cell binding threshold onto the critical support. Absent a
# binding table both columns are NA -- never a guessed label.
.nbx_join_binding <- function(support, binding, land_use) {
  if (is.null(binding)) {
    return(dplyr::mutate(
      support,
      binding_threshold = NA_character_,
      binding_matches_mi = NA
    ))
  }
  binding <- .nbx_validate_binding(binding, support, land_use)
  dplyr::left_join(
    support,
    dplyr::select(
      binding,
      "cell_id",
      "binding_threshold",
      "binding_matches_mi"
    ),
    by = "cell_id",
    relationship = "one-to-one"
  )
}

.nbx_validate_binding <- function(binding, support, land_use) {
  .check_columns(
    binding,
    c(
      "cell_id",
      "binding_threshold",
      "binding_matches_mi",
      "critical_mi_kgn_ha",
      "critical_land_use"
    ),
    "binding"
  )
  scopes <- unique(binding$critical_land_use[
    !is.na(binding$critical_land_use)
  ])
  if (!identical(scopes, land_use)) {
    cli::cli_abort(c(
      "The binding-threshold table does not match {.arg land_use}.",
      i = "Expected {.val {land_use}}; found {.val {scopes}}."
    ))
  }
  if (anyDuplicated(binding$cell_id) > 0L) {
    cli::cli_abort("The binding-threshold table has duplicate cell keys.")
  }
  .nbx_check_binding_mi(binding, support)
  binding
}

# Where the compared surface is the deposited "mi" layer and the binding table
# carries that same layer, the two must agree cell by cell. Both come from one
# file, so a difference means the tables are from different land-use scopes or
# archives.
.nbx_check_binding_mi <- function(binding, support) {
  joined <- dplyr::inner_join(
    dplyr::filter(support, .data$critical_threshold == "mi"),
    dplyr::select(binding, "cell_id", "critical_mi_kgn_ha"),
    by = "cell_id"
  )
  both <- !is.na(joined$source_critical_kgn_ha) &
    !is.na(joined$critical_mi_kgn_ha)
  deposited <- joined$critical_mi_kgn_ha[both]
  gap <- abs(joined$source_critical_kgn_ha[both] - deposited)
  bad <- gap > 1e-9 * pmax(1, abs(deposited))
  if (any(bad)) {
    cli::cli_abort(c(
      "The binding-threshold table is not built from this critical layer.",
      i = "Its {.field critical_mi_kgn_ha} differs from the compared {.val mi}
           surface in {sum(bad)} cell{?s}."
    ))
  }
  invisible(TRUE)
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
        .data$critical_kgn_ha * .data$source_area_ha / .kg_per_tonne(),
        NA_real_
      ),
      cell_actual_kgn_ha = dplyr::if_else(
        .data$coverage_state == "valid",
        .data$cell_actual_n_t * .kg_per_tonne() / .data$source_area_ha,
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
    "binding_threshold",
    "binding_matches_mi",
    "cell_actual_n_t",
    "absolute_pressure_n_t",
    "source_critical_kgn_ha",
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
    "negative_critical",
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
    "binding_threshold",
    "binding_matches_mi",
    "source_critical_kgn_ha",
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
    "negative_critical",
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
    "binding_threshold",
    "binding_matches_mi",
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
    "source_critical_kgn_ha",
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
    "negative_critical",
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
      "negative_critical",
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
