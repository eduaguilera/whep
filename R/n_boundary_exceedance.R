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
#' @section Grassland intensity split (`land_use = "all"`):
#' The deposited `all`-scope critical rate is per hectare of cropland plus
#' IMAGE-intensive grassland, while WHEP's pressure covers all grassland. With
#' `grassland_split = "image_density"` (the default) each cell is compared as
#' two independent components:
#' * **managed** -- crop rows plus grassland rows (CBS 3000, 3002, 3003) of a
#'   cell classed intensive, against the cell's own `"ara"` critical rate
#'   times its IMAGE 2010 cropland plus, when the cell is classed intensive,
#'   an `"igl"` critical rate times its IMAGE 2010 grassland. The deposited
#'   per-hectare `"ara"` and `"igl"` layers combine exactly into the `"all"`
#'   layer (area-weighted, measured on the archive for every threshold and
#'   both metrics), so with the IMAGE 2010 classes this reproduces the
#'   `"all"`-scope allowance;
#' * **extensive** -- grassland rows of a cell classed extensive, against
#'   IMAGE's 2010 extensive-grassland input or surplus per hectare
#'   (Schulte-Uebbing et al. 2022, SI Supplementary Table 4) times the cell's
#'   IMAGE 2010 grassland.
#'
#' Cell overshoot is the **sum** of the two component overshoots: headroom on
#' one never nets against excess on the other. Cell actual, critical and
#' margin are the sums over the compared components. A component with no
#' allowance area but non-zero pressure (in practice crop pressure where IMAGE
#' has no cropland and the cell is extensive) is excluded from its comparison
#' (`managed_coverage_state`/`extensive_coverage_state` `"zero_land"`); a
#' component with area but no rate even after transfer is
#' `"missing_critical"`. Excluded pressure is reported per cell in
#' `excluded_actual_n_t` and in a message (classes
#' `whep_nbx_zero_land_component`, `whep_nbx_missing_critical_component`),
#' and never counts as overshoot. Crop attribution shares each component's
#' allowance, margin and overshoot among that component's rows only, and
#' reconciles to the component and to the cell. A compared component with
#' allowance area but no pressure row keeps its allowance in a
#' `cell_residual` record naming the component.
#'
#' Declared assumptions (constructed methods without published precedent):
#' the classes are IMAGE's 2010 production-system map moved through time by a
#' national grazing-density proxy (see `grassland$classes`); the extensive
#' allowance is IMAGE's 2010 budget held constant -- "no more than in 2010",
#' not an environmental limit, so 2010 extensive exceedance is zero wherever
#' WHEP's 2010 extensive pressure equals IMAGE's; a rate a cell lacks for its
#' class is borrowed from the nearest cell (great-circle distance) with one in
#' the same 2010 country, else the same IMAGE region -- an `"igl"` rate for
#' grassland promoted from extensive to intensive, an extensive budget rate
#' for grassland classed extensive; cropland keeps its own `"ara"` rate and
#' is never lent one -- stamped in
#' `method_allowance_managed`/`method_allowance_extensive`
#' (`"archive"`, `"nearest_country"`, `"nearest_region"`, `"none"`, or
#' `"no_area"` for a component without area; `NA` outside the critical
#' domain). IMAGE 2010 intensive grassland with no published `"igl"` value
#' is not lent one (maintainer decision 2026-09-24): its pressure is left out
#' of the comparison and reported in `excluded_igl_actual_n_t`, the cell's
#' cropland is still compared at its own `"ara"` rate, and the managed method
#' is `"none"`. Allowance areas are IMAGE 2010
#' areas, fixed, except WHEP grassland in a cell with no IMAGE grassland,
#' whose extensive allowance uses WHEP's own grassland area of the year
#' (method suffix `"_whep_area"`, maintainer decision 2026-09-24).
#' `grassland_split = "none"` reproduces the unsplit comparison exactly.
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
#' @param grassland_split Grassland treatment under `land_use = "all"`:
#'   `"image_density"` (default) splits each cell into a managed and an
#'   extensive component (see the section below); `"none"` compares one cell
#'   pressure with the deposited allowance, as before the split existed.
#'   Ignored for `"ara"` and `"igl"`. Under `"image_density"`,
#'   `metric = "new_fixation"` aborts: the archive has no extensive budget for
#'   it.
#' @param grassland Named list of the inputs the split needs, all required
#'   under `grassland_split = "image_density"` (their absence aborts; there is
#'   no fallback): `classes`, the `build_grassland_intensity_classes()` table
#'   (one row per cell and year: `cell_id`, `lon`, `lat`, `year`,
#'   `country_2010`, `image_region`, `a_crop_ha`, `grass_ha_image`,
#'   `whep_grass_ha`, `image_class_2010`, `grassland_class`,
#'   `method_grassland_split`), and `extensive_budget`, IMAGE's 2010
#'   extensive-grassland budget per cell (`cell_id`, `ext_input_kgn_ha`,
#'   `ext_surplus_kgn_ha`); and `critical_ara` and `critical_igl`, the
#'   [read_critical_n()] layers with `land_use = "ara"` and `"igl"` for the
#'   same threshold and metric as `critical` (validated). `critical` itself
#'   still defines the cell domain and is checked against the class table's
#'   areas. A cell absent from `classes` must carry no grassland pressure; it
#'   is compared as cropland only.
#' @param example If `TRUE`, return the package fixture.
#' @return A tibble at the requested grain. Cell results retain actual and
#'   critical masses, signed margin, positive overshoot, coverage state,
#'   integer source-grid key, IMAGE context, explicit years, selectors, and
#'   provenance. Crop results additionally retain the signed pressure share and
#'   crop-attributed quantities, which reconcile algebraically to the cell.
#'   Cell and grid results also carry the split components:
#'   `managed_actual_n_t`, `managed_critical_n_t`,
#'   `managed_positive_overshoot_n_t`, `extensive_actual_n_t`,
#'   `extensive_critical_n_t`, `extensive_positive_overshoot_n_t`, their areas
#'   (`managed_area_ha`, `extensive_area_ha`), rates
#'   (`managed_critical_kgn_ha`, `extensive_critical_kgn_ha`) and coverage
#'   states, `excluded_actual_n_t` (of which `excluded_igl_actual_n_t` is
#'   intensive grassland without an `"igl"` rate), `grassland_class`,
#'   `method_allowance_managed`, `method_allowance_extensive` and
#'   `method_grassland_split` (the per-cell class method, `"no_grassland"` for
#'   a cell outside the class table, `"none"` without the split). These are
#'   `NA` when the split is not applied, so the schema does not depend on it.
#'   Grid rows add `boundary_component` (`"managed"`/`"extensive"`, `NA`
#'   without the split); within the split, `pressure_share` is the row's share
#'   of its component. Every row carries the call-level `grassland_split`;
#'   aggregated rows list the per-cell methods they span in
#'   `method_grassland_split`, separated by `;`.
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
  grassland_split = c("image_density", "none"),
  grassland = list(
    classes = NULL,
    extensive_budget = NULL,
    critical_ara = NULL,
    critical_igl = NULL
  ),
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
  grassland_split <- rlang::arg_match(grassland_split)
  metric <- .nbx_match_metric(metric)
  allocation_scenario <- .nbx_match_scenario(allocation_scenario)
  # The split only has meaning where grassland shares the allowance with
  # cropland; "ara" and "igl" ignore it and are computed exactly as before.
  split <- land_use == "all" && grassland_split == "image_density"
  if (split) {
    .nbx_check_grassland_request(metric, grassland)
  }
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
  support <- .nbx_prepare_critical(critical)
  if (split) {
    parts <- .nbx_split_cells(actual, support, grassland, actual_year, metric)
    actual <- parts$actual
    cells <- parts$cells
  } else {
    cells <- .nbx_build_cells(actual, support, actual_year, metric, land_use) |>
      .nbx_no_split_cols()
  }
  cells <- .nbx_stamp(
    cells,
    metric,
    land_use,
    allocation_scenario,
    actual_year,
    critical_reference_year,
    split
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

# ---- Intensive/extensive grassland split (land_use = "all") ----------------
#
# Issue #1285 and its design spec of 2026-09-24 (every decision there was
# confirmed by the maintainer that day). With `land_use = "all"`
# the deposited critical surface is a rate per ha of cropland plus IMAGE
# intensive grassland only, while WHEP's actual pressure covers all grassland.
# The split compares like with like, per cell and year, as two independent
# components:
#   managed   = cropland + grassland classed intensive, against the critical
#               rate (critical_kgn_ha);
#   extensive = grassland classed extensive, against IMAGE's 2010 extensive
#               budget per ha (.critical_n_extensive_budget()), held constant.
# Allowance areas are IMAGE 2010 areas, fixed as cropland already is; the one
# exception is WHEP grassland in a cell with no IMAGE grassland, whose
# extensive allowance is multiplied by WHEP's own grassland area of the year
# (stamped "*_whep_area"). A rate a cell lacks for its class is borrowed from
# the nearest cell that has one (.nearest_class_rate(), stamped). The two
# overshoots are added, never netted: headroom on extensive grassland does not
# absorb excess on cropland in the same cell.

.nbx_check_grassland_request <- function(metric, grassland) {
  if (metric == "new_fixation") {
    cli::cli_abort(
      c(
        "{.arg grassland_split = \"image_density\"} has no new-fixation
         budget.",
        i = "The deposited archive carries an extensive-grassland input and
             surplus only; use {.arg metric = \"surplus\"} or
             {.val input}, or {.arg grassland_split = \"none\"}."
      ),
      class = "whep_nbx_grassland_metric"
    )
  }
  parts <- c("classes", "extensive_budget", "critical_ara", "critical_igl")
  supplied <- is.list(grassland) &&
    !is.data.frame(grassland) &&
    all(purrr::map_lgl(parts, \(p) is.data.frame(grassland[[p]])))
  if (!supplied) {
    cli::cli_abort(
      c(
        "{.arg grassland_split = \"image_density\"} needs
         {.arg grassland$classes}, {.arg grassland$extensive_budget},
         {.arg grassland$critical_ara} and {.arg grassland$critical_igl}.",
        i = "Pass the {.fn build_grassland_intensity_classes} table, the
             IMAGE 2010 extensive budget and the {.val ara} and {.val igl}
             critical layers of the same threshold and metric, or choose
             {.arg grassland_split = \"none\"} explicitly. The split never
             falls back to the unsplit comparison on its own."
      ),
      class = "whep_nbx_grassland_missing"
    )
  }
  invisible(TRUE)
}

.nbx_split_output_cols <- function() {
  c(
    "managed_actual_n_t",
    "managed_critical_n_t",
    "managed_positive_overshoot_n_t",
    "extensive_actual_n_t",
    "extensive_critical_n_t",
    "extensive_positive_overshoot_n_t",
    "managed_area_ha",
    "extensive_area_ha",
    "managed_critical_kgn_ha",
    "extensive_critical_kgn_ha",
    "managed_coverage_state",
    "extensive_coverage_state",
    "excluded_actual_n_t",
    "excluded_igl_actual_n_t",
    "grassland_class",
    "method_allowance_managed",
    "method_allowance_extensive",
    "method_grassland_split"
  )
}

# Without the split the component columns exist but are empty, so the schema
# does not depend on the method.
.nbx_no_split_cols <- function(x) {
  dplyr::mutate(
    x,
    managed_actual_n_t = NA_real_,
    managed_critical_n_t = NA_real_,
    managed_positive_overshoot_n_t = NA_real_,
    extensive_actual_n_t = NA_real_,
    extensive_critical_n_t = NA_real_,
    extensive_positive_overshoot_n_t = NA_real_,
    managed_area_ha = NA_real_,
    extensive_area_ha = NA_real_,
    managed_critical_kgn_ha = NA_real_,
    extensive_critical_kgn_ha = NA_real_,
    managed_coverage_state = NA_character_,
    extensive_coverage_state = NA_character_,
    excluded_actual_n_t = NA_real_,
    excluded_igl_actual_n_t = NA_real_,
    grassland_class = NA_character_,
    method_allowance_managed = NA_character_,
    method_allowance_extensive = NA_character_,
    method_grassland_split = "none",
    managed_absolute_n_t = NA_real_,
    extensive_absolute_n_t = NA_real_
  )
}

# Component states that enter the comparison. "empty" is a component with no
# allowance area and no pressure: it contributes an exact zero.
.nbx_compared_states <- function() {
  c("valid", "empty")
}

.nbx_grass_codes <- function() {
  c(3000L, 3002L, 3003L)
}

.nbx_split_cells <- function(actual, support, grassland, actual_year, metric) {
  classes <- .nbx_split_classes(grassland$classes, actual_year)
  budget <- .nbx_split_budget(grassland$extensive_budget, metric)
  layers <- .nbx_split_layers(grassland, support, metric)
  .nbx_check_class_areas(classes, support)
  .nbx_check_layer_consistency(layers, support, classes)
  classes <- .nbx_flag_unrated_igl(classes, layers)
  actual <- .nbx_assign_components(actual, classes)
  rates <- .nbx_split_rates(classes, layers, budget)
  cells <- .nbx_split_domain(actual, support, classes, rates, actual_year) |>
    dplyr::left_join(
      dplyr::select(layers, "cell_id", "ara_rate"),
      by = "cell_id",
      relationship = "one-to-one"
    ) |>
    .nbx_split_components() |>
    .nbx_split_cell_totals()
  .nbx_report_excluded(cells, actual_year)
  list(actual = actual, cells = cells)
}

# The managed allowance is built from its two land classes rather than from
# the `all`-scope rate. The deposited per-ha `ara` and `igl` layers combine
# exactly into `all`: all = (ara * crop_ha + igl * igl_ha) / (crop_ha +
# igl_ha) in the 11,431 mixed cells (median ratio 1.0000, all within 1 %),
# all == ara in crop-only and all == igl in intensive-only cells, for every
# threshold and for critical surpluses and inputs (measured on the archive,
# maintainer decision 2026-09-24). So a cell whose grassland changes class
# keeps its own cropland rate, and only the grassland part needs a rate.
.nbx_split_layers <- function(grassland, support, metric) {
  thresholds <- unique(support$critical_threshold[
    !is.na(support$critical_threshold)
  ])
  ara <- .nbx_split_layer(grassland$critical_ara, "ara", thresholds, metric)
  igl <- .nbx_split_layer(grassland$critical_igl, "igl", thresholds, metric)
  dplyr::full_join(
    dplyr::rename(ara, ara_rate = "rate"),
    dplyr::rename(igl, igl_rate = "rate"),
    by = "cell_id",
    relationship = "one-to-one"
  )
}

# The `ara`, `igl` and `all` layers must be the same published surface: in
# the archive all = (ara * crop + igl * igl_ha) / (crop + igl_ha) holds within
# 1 % in every one of the 11,431 mixed cells and exactly in single-use cells.
# Areas are IMAGE 2010's (intensive grassland by its 2010 class). A layer from
# another threshold, metric or archive version breaks this where no label
# check can see it.
.nbx_check_layer_consistency <- function(layers, support, classes) {
  x <- classes |>
    dplyr::transmute(
      .data$cell_id,
      crop_ha = .data$a_crop_ha,
      igl_ha = .data$grass_ha_image *
        dplyr::coalesce(.data$image_class_2010 == "intensive", FALSE)
    ) |>
    dplyr::inner_join(layers, by = "cell_id", relationship = "one-to-one") |>
    dplyr::inner_join(
      dplyr::select(support, "cell_id", all_rate = "critical_kgn_ha"),
      by = "cell_id",
      relationship = "one-to-one"
    ) |>
    dplyr::filter(
      is.finite(.data$ara_rate),
      is.finite(.data$igl_rate),
      is.finite(.data$all_rate),
      .data$crop_ha + .data$igl_ha > 0
    ) |>
    dplyr::mutate(
      combined = (.data$ara_rate *
        .data$crop_ha +
        .data$igl_rate * .data$igl_ha) /
        (.data$crop_ha + .data$igl_ha),
      gap = abs(.data$all_rate - .data$combined) / pmax(1, abs(.data$all_rate))
    )
  bad <- x$gap > 0.01
  if (!any(bad)) {
    return(invisible(TRUE))
  }
  worst <- x[which.max(x$gap), ]
  cli::cli_abort(
    c(
      "The {.val ara} and {.val igl} layers do not combine into the
       {.val all} surface in {sum(bad)} cell{?s}.",
      x = "Worst: cell {worst$cell_id}, all {signif(worst$all_rate, 6)}
           kg N/ha against {signif(worst$combined, 6)} from ara and igl.",
      i = "All three must be the same threshold, metric and archive; they
           agree within 1 % in every mixed cell of the published archive."
    ),
    class = "whep_nbx_grassland_bad_layer"
  )
}

.nbx_split_layer <- function(layer, scope, thresholds, metric) {
  arg <- paste0("grassland$critical_", scope)
  if (
    rlang::has_name(layer, "critical_kgn_ha") &&
      !rlang::has_name(layer, "value")
  ) {
    layer <- dplyr::rename(layer, value = "critical_kgn_ha")
  }
  .check_columns(
    layer,
    c(
      "lon",
      "lat",
      "value",
      "critical_var",
      "critical_land_use",
      "critical_threshold",
      "critical_year"
    ),
    arg
  )
  check_inputs_supplied(
    layer,
    rlang::set_names("value", paste(scope, "critical rate")),
    details = c(i = "Read it with {.fn read_critical_n}.")
  )
  expected_var <- if (metric == "input") {
    "critical_n_input"
  } else {
    "critical_n_surplus"
  }
  found <- list(
    critical_land_use = unique(layer$critical_land_use),
    critical_var = unique(layer$critical_var),
    critical_threshold = unique(layer$critical_threshold),
    critical_year = unique(layer$critical_year)
  )
  bad <- c(
    critical_land_use = !identical(found$critical_land_use, scope),
    critical_var = !identical(found$critical_var, expected_var),
    critical_threshold = !identical(found$critical_threshold, thresholds),
    critical_year = !identical(as.numeric(found$critical_year), 2010)
  )
  if (any(bad)) {
    wrong <- names(bad)[bad]
    detail <- purrr::map_chr(wrong, \(col) {
      paste0(col, ": ", paste(found[[col]], collapse = ", "))
    })
    cli::cli_abort(
      c(
        "{.arg {arg}} is not the {.val {scope}} layer of the compared
         surface.",
        i = "Expected {.val {scope}}, {.val {expected_var}}, threshold
             {.val {thresholds}} and reference year 2010.",
        rlang::set_names(detail, rep("x", length(detail)))
      ),
      class = "whep_nbx_grassland_bad_layer"
    )
  }
  keyed <- .nbx_add_cell_key(layer, arg)
  if (anyDuplicated(keyed$cell_id) > 0L) {
    cli::cli_abort(
      "{.arg {arg}} has duplicated cell keys.",
      class = "whep_nbx_grassland_bad_layer"
    )
  }
  dplyr::transmute(keyed, .data$cell_id, rate = .data$value)
}

.nbx_class_columns <- function() {
  c(
    "cell_id",
    "lon",
    "lat",
    "year",
    "country_2010",
    "image_region",
    "a_crop_ha",
    "grass_ha_image",
    "whep_grass_ha",
    "image_class_2010",
    "grassland_class",
    "method_grassland_split"
  )
}

.nbx_split_classes <- function(classes, actual_year) {
  .check_columns(classes, .nbx_class_columns(), "grassland$classes")
  x <- dplyr::filter(classes, .data$year == .env$actual_year)
  if (nrow(x) == 0L) {
    cli::cli_abort(
      "{.arg grassland$classes} has no rows for {.arg actual_year}
       {actual_year}.",
      class = "whep_nbx_grassland_uncovered"
    )
  }
  keyed <- x |>
    dplyr::rename(class_cell_id = "cell_id") |>
    .nbx_add_cell_key("grassland classes")
  .nbx_check_classes(keyed)
  keyed |>
    dplyr::transmute(
      .data$cell_id,
      .data$source_row,
      .data$source_col,
      .data$lon,
      .data$lat,
      country_2010 = as.integer(.data$country_2010),
      image_region = as.integer(.data$image_region),
      .data$a_crop_ha,
      .data$grass_ha_image,
      .data$whep_grass_ha,
      .data$image_class_2010,
      .data$grassland_class,
      .data$method_grassland_split,
      in_classes = TRUE
    )
}

# A class row the comparison cannot interpret has no defensible reading, so
# it aborts rather than being dropped or defaulted.
.nbx_check_classes <- function(x) {
  no_image <- x$method_grassland_split == "no_image_grassland"
  problems <- c(
    "{.field cell_id} differs from the canonical key of lon/lat" = any(
      x$class_cell_id != x$cell_id
    ),
    "duplicated {.field cell_id}" = anyDuplicated(x$cell_id) > 0L,
    "{.field grassland_class} outside intensive/extensive" = any(
      !x$grassland_class %in% c("intensive", "extensive")
    ),
    "{.field method_grassland_split} outside the known methods" = any(
      !x$method_grassland_split %in% .nbx_class_methods()
    ),
    "{.field image_class_2010} outside intensive/extensive/NA" = any(
      !is.na(x$image_class_2010) &
        !x$image_class_2010 %in% c("intensive", "extensive")
    ),
    "{.field a_crop_ha} or {.field grass_ha_image} missing or negative" = any(
      !.nbx_is_area(x$a_crop_ha) | !.nbx_is_area(x$grass_ha_image)
    ),
    "{.field whep_grass_ha} missing or negative on a no-IMAGE row" = any(
      no_image & !.nbx_is_area(x$whep_grass_ha)
    ),
    "a no-IMAGE row with IMAGE grassland or an intensive class" = any(
      no_image &
        (x$grass_ha_image != 0 | x$grassland_class != "extensive")
    ),
    "{.field image_class_2010} missing on a row with IMAGE grassland" = any(
      !no_image & is.na(x$image_class_2010)
    )
  )
  bad <- names(problems)[problems]
  if (length(bad) > 0L) {
    cli::cli_abort(
      c(
        "{.arg grassland$classes} is not a valid class table.",
        rlang::set_names(bad, rep("x", length(bad)))
      ),
      class = "whep_nbx_grassland_bad_classes"
    )
  }
  invisible(x)
}

.nbx_class_methods <- function() {
  c(
    "image2010_density_rank",
    "image2010_fixed_no_density",
    "no_image_grassland"
  )
}

.nbx_is_area <- function(x) {
  is.finite(x) & x >= 0
}

.nbx_split_budget <- function(budget, metric) {
  rate_col <- if (metric == "input") {
    "ext_input_kgn_ha"
  } else {
    "ext_surplus_kgn_ha"
  }
  .check_columns(budget, c("cell_id", rate_col), "grassland$extensive_budget")
  if (anyDuplicated(budget$cell_id) > 0L) {
    cli::cli_abort(
      "{.arg grassland$extensive_budget} has duplicated {.field cell_id}.",
      class = "whep_nbx_grassland_bad_budget"
    )
  }
  # A budget whose rate column is entirely missing or zero was not supplied:
  # every extensive allowance would silently become zero or be transferred.
  check_inputs_supplied(
    budget,
    rlang::set_names(rate_col, "extensive grassland budget"),
    details = c(i = "Build it with the IMAGE 2010 archive layers.")
  )
  dplyr::transmute(
    budget,
    cell_id = as.integer(.data$cell_id),
    extensive_rate = .data[[rate_col]]
  )
}

# The class table and the critical surface must describe the same IMAGE 2010
# map: the deposited `all`-scope source area is cropland plus IMAGE-intensive
# grassland, so it has to equal the class table's own areas wherever both
# carry the cell. A mismatch means the two were built from different maps.
.nbx_check_class_areas <- function(classes, support) {
  both <- dplyr::inner_join(
    classes,
    dplyr::select(support, "cell_id", "source_area_ha"),
    by = "cell_id",
    relationship = "one-to-one"
  )
  intensive_2010 <- dplyr::coalesce(both$image_class_2010 == "intensive", FALSE)
  expected <- both$a_crop_ha + both$grass_ha_image * intensive_2010
  bad <- abs(expected - both$source_area_ha) >
    1e-6 * pmax(1, abs(both$source_area_ha))
  if (any(bad)) {
    cli::cli_abort(
      c(
        "{sum(bad)} cell{?s} disagree{?s/} between the class table and the
         critical surface's source area.",
        x = "{.field a_crop_ha} + IMAGE-intensive {.field grass_ha_image}
             must equal {.field source_area_ha}.",
        i = "First {cli::qty(min(sum(bad), 5L))}cell{?s}:
             {.val {utils::head(both$cell_id[bad], 5)}}."
      ),
      class = "whep_nbx_grassland_area_mismatch"
    )
  }
  invisible(TRUE)
}

# Crop rows are managed land. A grassland row goes wholly to its cell's class,
# so the two components partition the cell's pressure exactly.
.nbx_assign_components <- function(actual, classes) {
  x <- dplyr::left_join(
    actual,
    dplyr::select(classes, "cell_id", "grassland_class", "igl_unrated"),
    by = "cell_id",
    relationship = "many-to-one"
  )
  grass <- x$item_cbs_code %in% .nbx_grass_codes()
  carries <- is.na(x$actual_n_t) |
    x$actual_n_t != 0 |
    (!is.na(x$area_ha) & x$area_ha > 0)
  uncovered <- grass & is.na(x$grassland_class) & carries
  if (any(uncovered)) {
    n_cells <- dplyr::n_distinct(x$cell_id[uncovered])
    cli::cli_abort(
      c(
        "{n_cells} cell{?s} carr{?ies/y} grassland pressure but no row in
         {.arg grassland$classes}.",
        i = "First {cli::qty(min(n_cells, 5L))}cell{?s}:
             {.val {utils::head(unique(x$cell_id[uncovered]), 5)}}."
      ),
      class = "whep_nbx_grassland_uncovered"
    )
  }
  x |>
    dplyr::mutate(
      boundary_component = dplyr::case_when(
        !.data$item_cbs_code %in% .nbx_grass_codes() ~ "managed",
        .data$grassland_class == "extensive" ~ "extensive",
        # IMAGE-intensive grassland the published igl surface has no value
        # for: excluded from the managed comparison, never lent a rate.
        .data$igl_unrated ~ "igl_unrated",
        # Intensive grassland; and a grassland row with no area and no
        # pressure in a cell the class table does not carry, which is a
        # structural zero wherever it is booked.
        .default = "managed"
      )
    ) |>
    dplyr::select(-"grassland_class", -"igl_unrated")
}

# IMAGE 2010 intensive grassland still classed intensive, with no `igl` value
# in the published surface (435 of the 12,175 IMAGE-intensive cells; 45 of
# them without cropland, 6.43 Mha). The maintainer decided on 2026-09-24 that
# these cells do not borrow an `igl` rate: only grassland promoted from
# extensive may. Their intensive-grassland part stays uncompared, as it is in
# the published surface, and its pressure is reported as excluded; their
# cropland is still compared at the cell's own `ara` rate.
.nbx_flag_unrated_igl <- function(classes, layers) {
  own_igl <- dplyr::select(layers, "cell_id", own_igl = "igl_rate")
  classes |>
    dplyr::left_join(own_igl, by = "cell_id", relationship = "one-to-one") |>
    dplyr::mutate(
      igl_unrated = .data$grassland_class == "intensive" &
        dplyr::coalesce(.data$image_class_2010 == "intensive", FALSE) &
        .data$grass_ha_image > 0 &
        !is.finite(.data$own_igl)
    ) |>
    dplyr::select(-"own_igl")
}

# Rates per class, borrowed only from a cell of the same class (maintainer
# rule, 2026-09-24: nearest same-class cell in the 2010 country, then the
# IMAGE region). Grassland promoted to intensive borrows an `igl` rate; the
# donors are the cells with a finite `igl` rate, which by construction are
# IMAGE-intensive cells. Extensive donors are the cells with a finite IMAGE
# 2010 extensive budget, which only IMAGE-extensive cells have. Cropland is
# never lent a rate: its `ara` rate is the cell's own or it is missing.
.nbx_split_rates <- function(classes, layers, budget) {
  classes |>
    dplyr::select(
      "cell_id",
      "lon",
      "lat",
      country = "country_2010",
      "image_region"
    ) |>
    dplyr::left_join(
      dplyr::select(layers, "cell_id", managed_rate = "igl_rate"),
      by = "cell_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      dplyr::select(budget, "cell_id", extensive_rate = "extensive_rate"),
      by = "cell_id",
      relationship = "one-to-one"
    ) |>
    .nearest_class_rate() |>
    dplyr::rename(igl_rate = "managed_rate", igl_method = "managed_method")
}

# Every cell that the critical surface, the class table or the actual pressure
# carries, with the per-component actual pressure. A component with no row in
# a cell that has pressure rows is an exact zero (the rows partition the
# cell); a cell with no rows or any missing row keeps NA.
.nbx_split_domain <- function(actual, support, classes, rates, actual_year) {
  key_cols <- c("cell_id", "source_row", "source_col", "lon", "lat")
  pressure <- dplyr::summarise(
    actual,
    any_missing = anyNA(.data$actual_n_t),
    managed_actual_n_t = sum(
      .data$actual_n_t[.data$boundary_component == "managed"]
    ),
    extensive_actual_n_t = sum(
      .data$actual_n_t[.data$boundary_component == "extensive"]
    ),
    managed_absolute_n_t = sum(
      abs(.data$actual_n_t[.data$boundary_component == "managed"])
    ),
    extensive_absolute_n_t = sum(
      abs(.data$actual_n_t[.data$boundary_component == "extensive"])
    ),
    excluded_igl_actual_n_t = sum(
      .data$actual_n_t[.data$boundary_component == "igl_unrated"]
    ),
    .by = dplyr::all_of(key_cols)
  ) |>
    dplyr::mutate(
      dplyr::across(
        c(
          "managed_actual_n_t",
          "extensive_actual_n_t",
          "managed_absolute_n_t",
          "extensive_absolute_n_t",
          "excluded_igl_actual_n_t"
        ),
        \(v) dplyr::if_else(.data$any_missing, NA_real_, v)
      )
    )
  thresholds <- unique(support$critical_threshold[
    !is.na(support$critical_threshold)
  ])
  threshold <- if (length(thresholds) == 1L) thresholds else NA_character_
  dplyr::bind_rows(
    dplyr::select(support, dplyr::all_of(key_cols)),
    dplyr::select(classes, dplyr::all_of(key_cols)),
    dplyr::select(pressure, dplyr::all_of(key_cols))
  ) |>
    dplyr::distinct(.data$cell_id, .keep_all = TRUE) |>
    dplyr::left_join(
      dplyr::select(support, -dplyr::any_of(key_cols[-1L])),
      by = "cell_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      classes |>
        dplyr::select(-dplyr::any_of(key_cols[-1L])) |>
        dplyr::rename(class_image_region = "image_region"),
      by = "cell_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(rates, by = "cell_id", relationship = "one-to-one") |>
    dplyr::left_join(
      dplyr::select(pressure, -dplyr::any_of(key_cols[-1L])),
      by = "cell_id",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      year = as.integer(actual_year),
      in_classes = dplyr::coalesce(.data$in_classes, FALSE),
      igl_unrated = dplyr::coalesce(.data$igl_unrated, FALSE),
      image_region = dplyr::coalesce(
        .data$image_region,
        .data$class_image_region
      ),
      critical_threshold = dplyr::coalesce(
        .data$critical_threshold,
        .env$threshold
      )
    )
}

# The managed allowance, per land class: the cell's own `ara` rate on its
# IMAGE 2010 cropland plus an `igl` rate (own, else borrowed) on its IMAGE
# 2010 grassland when that grassland is classed intensive. A part with area
# and no rate makes the component "missing_critical". The rate reported is
# the area-weighted rate of the two parts; the method is the `igl` transfer's
# wherever intensive grassland is part of the allowance.
.nbx_split_managed <- function(x) {
  x |>
    dplyr::mutate(
      crop_area_ha = dplyr::if_else(
        .data$in_classes,
        .data$a_crop_ha,
        .data$source_area_ha
      ),
      igl_area_ha = dplyr::if_else(
        .data$in_classes & !.data$igl_unrated,
        .data$grass_ha_image * (.data$grassland_class == "intensive"),
        0
      ),
      # Own rate, or a borrowed one for grassland promoted from extensive.
      promoted = .data$in_classes &
        .data$grassland_class == "intensive" &
        !dplyr::coalesce(.data$image_class_2010 == "intensive", FALSE),
      igl_rate = dplyr::if_else(
        .data$in_classes &
          (.data$igl_method %in% "archive" | .data$promoted),
        .data$igl_rate,
        NA_real_
      ),
      managed_area_ha = .data$crop_area_ha + .data$igl_area_ha,
      crop_missing = .data$crop_area_ha > 0 & !is.finite(.data$ara_rate),
      igl_missing = .data$igl_area_ha > 0 & !is.finite(.data$igl_rate),
      managed_allowance_n_t = dplyr::if_else(
        .data$crop_missing | .data$igl_missing,
        NA_real_,
        (dplyr::if_else(.data$crop_area_ha > 0, .data$ara_rate, 0) *
          .data$crop_area_ha +
          dplyr::if_else(.data$igl_area_ha > 0, .data$igl_rate, 0) *
            .data$igl_area_ha) /
          .kg_per_tonne()
      ),
      # A rate is reported only where it multiplies an area.
      managed_critical_kgn_ha = dplyr::if_else(
        .data$managed_area_ha > 0,
        .data$managed_allowance_n_t * .kg_per_tonne() / .data$managed_area_ha,
        NA_real_
      ),
      method_allowance_managed = dplyr::case_when(
        .data$igl_unrated ~ "none",
        .data$managed_area_ha == 0 ~ "no_area",
        .data$crop_missing ~ "none",
        .data$igl_area_ha > 0 ~ dplyr::coalesce(.data$igl_method, "none"),
        .default = "archive"
      )
    )
}

# Areas, rates and methods per component. A cell outside the class table has
# no grassland in either map (the table carries every IMAGE grassland cell and
# every cell with WHEP grassland), so its managed area is the deposited source
# area -- cropland only -- and it has no extensive component.
.nbx_split_components <- function(x) {
  x |>
    .nbx_split_managed() |>
    dplyr::mutate(
      whep_area = .data$in_classes &
        .data$method_grassland_split == "no_image_grassland",
      extensive_area_ha = dplyr::case_when(
        !.data$in_classes ~ 0,
        .data$whep_area ~ .data$whep_grass_ha,
        .default = .data$grass_ha_image *
          (.data$grassland_class == "extensive")
      ),
      extensive_critical_kgn_ha = dplyr::if_else(
        .data$in_classes & .data$extensive_area_ha > 0,
        .data$extensive_rate,
        NA_real_
      ),
      extensive_method = dplyr::coalesce(.data$extensive_method, "none"),
      method_allowance_extensive = dplyr::case_when(
        .data$extensive_area_ha == 0 ~ "no_area",
        .data$whep_area & .data$extensive_method != "none" ~
          paste0(.data$extensive_method, "_whep_area"),
        .default = .data$extensive_method
      ),
      method_grassland_split = dplyr::if_else(
        .data$in_classes,
        .data$method_grassland_split,
        "no_grassland"
      ),
      out_of_domain = !.data$in_classes &
        (is.na(.data$critical_present) |
          (is.na(.data$critical_kgn_ha) &
            .data$critical_state == "out_of_domain")),
      # Outside the critical domain there is no allowance to have a method,
      # matching the NA component states there.
      method_allowance_managed = dplyr::if_else(
        .data$out_of_domain,
        NA_character_,
        .data$method_allowance_managed
      ),
      method_allowance_extensive = dplyr::if_else(
        .data$out_of_domain,
        NA_character_,
        .data$method_allowance_extensive
      ),
      managed_coverage_state = .nbx_component_state(
        .data$managed_area_ha,
        .data$managed_critical_kgn_ha,
        .data$managed_actual_n_t,
        .data$out_of_domain
      ),
      extensive_coverage_state = .nbx_component_state(
        .data$extensive_area_ha,
        .data$extensive_critical_kgn_ha,
        .data$extensive_actual_n_t,
        .data$out_of_domain
      ),
      managed_critical_n_t = dplyr::case_when(
        .data$managed_coverage_state == "empty" ~ 0,
        .data$managed_coverage_state == "valid" ~ .data$managed_allowance_n_t,
        .default = NA_real_
      ),
      extensive_critical_n_t = .nbx_component_critical(
        .data$extensive_coverage_state,
        .data$extensive_critical_kgn_ha,
        .data$extensive_area_ha
      ),
      managed_positive_overshoot_n_t = pmax(
        .data$managed_actual_n_t - .data$managed_critical_n_t,
        0
      ),
      extensive_positive_overshoot_n_t = pmax(
        .data$extensive_actual_n_t - .data$extensive_critical_n_t,
        0
      )
    )
}

# A component with no allowance area cannot be compared; if it carries
# pressure that pressure is excluded ("zero_land", the cell-level meaning
# applied to one component) and reported, never charged as overshoot. A
# component with area but no rate even after the nearest-cell transfer is
# "missing_critical".
.nbx_component_state <- function(area, rate, actual, out_of_domain) {
  dplyr::case_when(
    out_of_domain | is.na(area) ~ NA_character_,
    area == 0 & !is.na(actual) & actual != 0 ~ "zero_land",
    area == 0 ~ "empty",
    is.na(rate) ~ "missing_critical",
    is.na(actual) ~ "missing_actual",
    .default = "valid"
  )
}

.nbx_component_critical <- function(state, rate, area) {
  dplyr::case_when(
    state == "empty" ~ 0,
    state == "valid" ~ rate * area / .kg_per_tonne(),
    .default = NA_real_
  )
}

# Cell results are the sums over the compared components; overshoot is the sum
# of the component overshoots, so extensive headroom never nets against
# managed excess. A cell with at least one compared component is "valid"; an
# excluded component's pressure is `excluded_actual_n_t`.
.nbx_split_cell_totals <- function(x) {
  x |>
    dplyr::mutate(
      coverage_state = dplyr::case_when(
        .data$out_of_domain ~ "out_of_domain",
        .data$managed_coverage_state == "valid" |
          .data$extensive_coverage_state == "valid" ~ "valid",
        .data$managed_coverage_state == "missing_critical" |
          .data$extensive_coverage_state == "missing_critical" |
          .data$igl_unrated ~
          "missing_critical",
        .data$managed_coverage_state == "zero_land" |
          .data$extensive_coverage_state == "zero_land" ~ "zero_land",
        is.na(.data$managed_actual_n_t) ~ "missing_actual",
        .default = "zero_land"
      ),
      valid = .data$coverage_state == "valid",
      use_managed = .data$valid &
        .data$managed_coverage_state %in% .nbx_compared_states(),
      use_extensive = .data$valid &
        .data$extensive_coverage_state %in% .nbx_compared_states(),
      compared_area_ha = .nbx_pick(.data$use_managed, .data$managed_area_ha) +
        .nbx_pick(.data$use_extensive, .data$extensive_area_ha),
      cell_actual_n_t = dplyr::if_else(
        .data$valid,
        .nbx_pick(.data$use_managed, .data$managed_actual_n_t) +
          .nbx_pick(.data$use_extensive, .data$extensive_actual_n_t),
        .data$managed_actual_n_t + .data$extensive_actual_n_t
      ),
      absolute_pressure_n_t = dplyr::if_else(
        .data$valid,
        .nbx_pick(.data$use_managed, .data$managed_absolute_n_t) +
          .nbx_pick(.data$use_extensive, .data$extensive_absolute_n_t),
        .data$managed_absolute_n_t + .data$extensive_absolute_n_t
      ),
      cell_critical_n_t = dplyr::if_else(
        .data$valid,
        .nbx_pick(.data$use_managed, .data$managed_critical_n_t) +
          .nbx_pick(.data$use_extensive, .data$extensive_critical_n_t),
        NA_real_
      ),
      cell_signed_margin_n_t = dplyr::if_else(
        .data$valid,
        .data$cell_actual_n_t - .data$cell_critical_n_t,
        NA_real_
      ),
      cell_positive_overshoot_n_t = dplyr::if_else(
        .data$valid,
        .nbx_pick(.data$use_managed, .data$managed_positive_overshoot_n_t) +
          .nbx_pick(
            .data$use_extensive,
            .data$extensive_positive_overshoot_n_t
          ),
        NA_real_
      ),
      cell_actual_kgn_ha = dplyr::if_else(
        .data$valid & .data$compared_area_ha > 0,
        .data$cell_actual_n_t * .kg_per_tonne() / .data$compared_area_ha,
        NA_real_
      ),
      pressure_condition_ratio = dplyr::if_else(
        .data$valid,
        .nbx_condition_ratio(
          .data$cell_actual_n_t,
          .data$absolute_pressure_n_t
        ),
        NA_real_
      ),
      excluded_actual_n_t = dplyr::if_else(
        .data$valid,
        .nbx_pick(!.data$use_managed, .data$managed_actual_n_t) +
          .nbx_pick(!.data$use_extensive, .data$extensive_actual_n_t) +
          .data$excluded_igl_actual_n_t,
        NA_real_
      )
    )
}

# `value` where `use` holds, else an exact zero (the component is not part of
# the sum); NA values are only ever picked where `use` holds.
.nbx_pick <- function(use, value) {
  dplyr::if_else(use, value, 0)
}

# Pressure left out of a component comparison is named, with its mass, so no
# reader mistakes an uncompared component for one within its boundary.
.nbx_report_excluded <- function(cells, actual_year) {
  states <- c(
    zero_land = "has no allowance area",
    missing_critical = paste(
      "has no critical rate, even after the nearest-cell",
      "transfer"
    )
  )
  purrr::iwalk(states, \(why, state) {
    component <- dplyr::coalesce(cells$managed_coverage_state == state, FALSE)
    unrated <- state == "missing_critical" & cells$igl_unrated
    managed <- component | unrated
    extensive <- dplyr::coalesce(cells$extensive_coverage_state == state, FALSE)
    if (!any(managed | extensive)) {
      return(invisible(NULL))
    }
    managed_t <- sum(cells$managed_actual_n_t[component], na.rm = TRUE) +
      sum(cells$excluded_igl_actual_n_t[unrated], na.rm = TRUE)
    extensive_t <- sum(cells$extensive_actual_n_t[extensive], na.rm = TRUE)
    ids <- utils::head(cells$cell_id[managed | extensive], 5L)
    n_cells <- sum(managed | extensive)
    cli::cli_inform(
      c(
        "i" = "{n_cells} cell{?s} in {actual_year}: a grassland-split
               component {why}, so its pressure is left out of that
               component's comparison.",
        "i" = "Excluded pressure: managed {managed_t} t N in
               {sum(managed)} cell{?s}, extensive {extensive_t} t N in
               {sum(extensive)} cell{?s}.",
        "i" = "First {cli::qty(length(ids))}cell{?s}: {.val {ids}}."
      ),
      class = paste0("whep_nbx_", state, "_component")
    )
  })
  invisible(NULL)
}

.nbx_stamp <- function(
  x,
  metric,
  land_use,
  scenario,
  actual_year,
  reference_year,
  split = FALSE
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
    provisional_reason = .nbx_provisional_reason(split),
    grassland_split = if (split) "image_density" else "none"
  )
}

.nbx_provisional_reason <- function(split) {
  base <- paste(
    "urban allocation is provisional; manure-management boundary may differ",
    "from the source; intensive-grass scope depends on WHEP item mapping"
  )
  if (!split) {
    return(base)
  }
  paste0(
    base,
    "; ",
    paste(
      "intensive/extensive grassland classes are the IMAGE 2010 map moved by",
      "a national grazing-density proxy; the extensive allowance is IMAGE's",
      "2010 extensive-grassland budget held constant, not an environmental",
      "limit; allowance areas are IMAGE 2010 areas except WHEP grassland in",
      "cells without IMAGE grassland; transferred rates are stamped per cell"
    )
  )
}

.nbx_attribute_crops <- function(actual, cells, metric) {
  valid <- dplyr::filter(cells, .data$coverage_state == "valid")
  if (!rlang::has_name(actual, "boundary_component")) {
    actual$boundary_component <- NA_character_
  }
  joined <- dplyr::inner_join(
    actual,
    dplyr::select(valid, dplyr::all_of(.nbx_attribution_cell_cols())),
    by = c("cell_id", "source_row", "source_col", "lon", "lat", "year"),
    relationship = "many-to-one"
  ) |>
    .nbx_attribution_units() |>
    dplyr::mutate(
      attribution_defined = .data$unit_actual_n_t != 0 &
        .data$unit_condition_ratio >= sqrt(.Machine$double.eps),
      pressure_share = dplyr::if_else(
        .data$attribution_defined,
        .data$actual_n_t / .data$unit_actual_n_t,
        NA_real_
      ),
      critical_n_t = dplyr::if_else(
        .data$attribution_defined,
        .data$pressure_share * .data$unit_critical_n_t,
        0
      ),
      crop_critical_n_t = .data$critical_n_t,
      signed_margin_n_t = dplyr::if_else(
        .data$attribution_defined,
        .data$pressure_share * .data$unit_signed_margin_n_t,
        0
      ),
      positive_overshoot_n_t = dplyr::if_else(
        .data$attribution_defined,
        .data$pressure_share * .data$unit_positive_overshoot_n_t,
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
          .data$unit_actual_n_t == 0,
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
    dplyr::slice_head(
      n = 1L,
      by = c("cell_id", "year", "boundary_component")
    ) |>
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
      unallocated_critical_n_t = .data$unit_critical_n_t,
      unallocated_signed_margin_n_t = .data$unit_signed_margin_n_t,
      unallocated_positive_overshoot_n_t = .data$unit_positive_overshoot_n_t,
      attribution_record_type = "cell_residual"
    )
  rowless <- .nbx_rowless_components(valid, joined, metric)
  dplyr::bind_rows(joined, residual, rowless) |>
    .nbx_assert_reconciliation() |>
    dplyr::select(-dplyr::starts_with("unit_"))
}

# A compared component can hold allowance area and no pressure row at all: an
# IMAGE-extensive cell where WHEP books no grassland that year, or IMAGE
# cropland where WHEP books only grassland. Its pressure is an exact zero, but
# its allowance is real, and with no row to carry it that allowance would
# vanish from every crop-level total. One `cell_residual` record per such
# component carries it: unallocated critical, a signed margin of
# `0 - critical` and the component's overshoot (`max(-critical, 0)`).
.nbx_rowless_components <- function(valid, joined, metric) {
  if (nrow(valid) == 0L || all(is.na(valid$managed_coverage_state))) {
    return(NULL)
  }
  present <- dplyr::distinct(
    joined,
    .data$cell_id,
    .data$year,
    .data$boundary_component
  )
  cells <- dplyr::select(valid, dplyr::all_of(.nbx_attribution_cell_cols()))
  purrr::map(c("managed", "extensive"), \(component) {
    cells |>
      dplyr::mutate(
        boundary_component = component,
        unit_state = .data[[paste0(component, "_coverage_state")]],
        unit_critical_n_t = .data[[paste0(component, "_critical_n_t")]],
        unit_positive_overshoot_n_t = .data[[
          paste0(component, "_positive_overshoot_n_t")
        ]]
      ) |>
      dplyr::filter(.data$unit_state == "valid")
  }) |>
    dplyr::bind_rows() |>
    dplyr::anti_join(
      present,
      by = c("cell_id", "year", "boundary_component")
    ) |>
    dplyr::mutate(
      area_code = NA_integer_,
      item_cbs_code = NA_integer_,
      area_ha = NA_real_,
      actual_n_t = 0,
      production_n_t = NA_real_,
      unit_actual_n_t = 0,
      unit_signed_margin_n_t = -.data$unit_critical_n_t,
      unit_condition_ratio = 0,
      attribution_defined = FALSE,
      pressure_share = NA_real_,
      critical_n_t = 0,
      crop_critical_n_t = 0,
      signed_margin_n_t = 0,
      positive_overshoot_n_t = 0,
      exceedance_n_t = 0,
      within_boundary_n_t = NA_real_,
      unallocated_critical_n_t = .data$unit_critical_n_t,
      unallocated_signed_margin_n_t = .data$unit_signed_margin_n_t,
      unallocated_positive_overshoot_n_t = .data$unit_positive_overshoot_n_t,
      attribution_record_type = "cell_residual",
      attribution_method = if (.env$metric == "input") {
        "crop_input_share"
      } else {
        "signed_crop_surplus_share"
      },
      attribution_status = "undefined_zero_denominator",
      attribution_state = .data$attribution_status,
      land_scope_status = "provisional",
      urban_treatment = "included_provisional"
    ) |>
    dplyr::select(-"unit_state")
}

.nbx_attribution_cell_cols <- function() {
  c(
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
    "provisional_reason",
    .nbx_split_output_cols(),
    "managed_absolute_n_t",
    "extensive_absolute_n_t",
    "grassland_split"
  )
}

# The unit a crop row's attribution shares one allowance with. Without the
# split that unit is the cell, exactly as before. With it, the cell holds two
# independent comparisons (no netting), so a row shares only its own
# component's allowance, margin and overshoot. Rows of a component excluded
# from the comparison (zero allowance area, or no rate) leave here, as rows of
# an uncompared cell always have; their mass is reported by
# .nbx_report_excluded().
.nbx_attribution_units <- function(x) {
  if (all(is.na(x$boundary_component))) {
    return(dplyr::mutate(
      x,
      unit_actual_n_t = .data$cell_actual_n_t,
      unit_critical_n_t = .data$cell_critical_n_t,
      unit_signed_margin_n_t = .data$cell_signed_margin_n_t,
      unit_positive_overshoot_n_t = .data$cell_positive_overshoot_n_t,
      unit_condition_ratio = .data$pressure_condition_ratio
    ))
  }
  x |>
    dplyr::filter(.data$boundary_component %in% c("managed", "extensive")) |>
    dplyr::mutate(
      unit_state = .nbx_by_component(
        .data$boundary_component,
        .data$managed_coverage_state,
        .data$extensive_coverage_state
      )
    ) |>
    dplyr::filter(.data$unit_state %in% .nbx_compared_states()) |>
    dplyr::mutate(
      unit_actual_n_t = .nbx_by_component(
        .data$boundary_component,
        .data$managed_actual_n_t,
        .data$extensive_actual_n_t
      ),
      unit_critical_n_t = .nbx_by_component(
        .data$boundary_component,
        .data$managed_critical_n_t,
        .data$extensive_critical_n_t
      ),
      unit_signed_margin_n_t = .data$unit_actual_n_t - .data$unit_critical_n_t,
      unit_positive_overshoot_n_t = .nbx_by_component(
        .data$boundary_component,
        .data$managed_positive_overshoot_n_t,
        .data$extensive_positive_overshoot_n_t
      ),
      unit_absolute_n_t = .nbx_by_component(
        .data$boundary_component,
        .data$managed_absolute_n_t,
        .data$extensive_absolute_n_t
      ),
      unit_condition_ratio = .nbx_condition_ratio(
        .data$unit_actual_n_t,
        .data$unit_absolute_n_t
      )
    ) |>
    dplyr::select(-"unit_state", -"unit_absolute_n_t")
}

.nbx_by_component <- function(component, managed, extensive) {
  dplyr::if_else(component == "managed", managed, extensive)
}

.nbx_condition_ratio <- function(actual, absolute) {
  dplyr::if_else(
    absolute > 0,
    abs(actual) / absolute,
    dplyr::if_else(actual == 0, 0, NA_real_)
  )
}

# Every attributed quantity must sum back to the result it was attributed
# from: the cell always, and under the grassland split each component too.
# The cell identity alone cannot see a mass moved between the two components,
# because both sum to the same cell.
.nbx_assert_reconciliation <- function(x, tolerance = 1e-10) {
  .nbx_assert_reconciled_by(x, c("cell_id", "year"), "cell", tolerance)
  if (any(!is.na(x$boundary_component))) {
    .nbx_assert_reconciled_by(
      x,
      c("cell_id", "year", "boundary_component"),
      "unit",
      tolerance
    )
  }
  x
}

.nbx_assert_reconciled_by <- function(x, by, prefix, tolerance) {
  target <- paste0(
    prefix,
    c(
      "_actual_n_t",
      "_critical_n_t",
      "_signed_margin_n_t",
      "_positive_overshoot_n_t"
    )
  )
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
    cell_actual = dplyr::first(.data[[target[[1L]]]]),
    cell_critical = dplyr::first(.data[[target[[2L]]]]),
    cell_margin = dplyr::first(.data[[target[[3L]]]]),
    cell_overshoot = dplyr::first(.data[[target[[4L]]]]),
    .by = dplyr::all_of(by)
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
    what <- if (prefix == "cell") "cell" else "grassland-split component"
    cli::cli_abort(
      "Crop attribution does not reconcile to its {what} result.",
      class = "whep_nbx_reconciliation"
    )
  }
  invisible(x)
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
    "provisional_reason",
    dplyr::all_of(.nbx_split_output_cols()),
    "grassland_split"
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
    "provisional_reason",
    "boundary_component",
    dplyr::all_of(.nbx_split_output_cols()),
    "grassland_split"
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
      "attribution_record_type",
      "grassland_split"
    ),
    names(x)
  )
  dplyr::summarise(
    x,
    dplyr::across(dplyr::all_of(mass), .sum_if_any),
    dplyr::across(dplyr::all_of(stamps), dplyr::first),
    method_grassland_split = .nbx_collapse_methods(
      .data$method_grassland_split
    ),
    .by = dplyr::all_of(key)
  ) |>
    tibble::as_tibble()
}

# An aggregate spans cells that may carry different split methods; the row
# names all of them rather than whichever cell happened to come first.
.nbx_collapse_methods <- function(x) {
  paste(sort(unique(x)), collapse = ";")
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
