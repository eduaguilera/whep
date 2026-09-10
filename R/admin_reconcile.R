# NSE globals for admin_reconcile.R (#1000, T14) -- reconciliation
# diagnostics over the tables `allocate_level_crops()` returns. These 36
# symbols are read bare here and are NOT yet in `utils::globalVariables()`,
# so they are the block to append in `R/utils.R` (verified with
# `codetools::checkUsage()` against the namespace's declared list; the
# other symbols this file names are either already declared or passed as
# strings):
#   admin_sum, area_share_renorm, area_sum, basis, beyond_tolerance,
#   breach_national_ha, breach_unit_ha, common, coverage_complete,
#   cropping_intensity, discrepancy_frac, first_observed, floor_binding,
#   floor_binding_ha, interior, irrigation_clipped_ha, last_observed,
#   longest_interior_run, mc_basis, mc_factor_national, n_common,
#   n_indicators, n_years_observed, national_total, national_total_ha,
#   observed, over_ha, prod_sum, production_basis, production_raw,
#   run_end, run_start, share_divergence, sown_ha, target_ha, usable
#
# `n_valued`, `value_sum` and `n_declared` are read bare too, but only
# inside `dplyr::case_when()` formulas, which `codetools` does not walk --
# so `R CMD check` never sees them and they need no declaration.

#' Reconcile a level allocation against the national totals that bind
#'
#' @description
#' The diagnostic half of [allocate_level_crops()]. The allocation itself
#' decides nothing here: this reads the tables that function returns and
#' says, per container and per unit, how far the administrative statistics
#' sat from the national total they were never allowed to move, what the
#' discarded production series would have implied, and where a unit's
#' target outran the land under it.
#'
#' Two things it deliberately does not do. It never re-derives a share, so
#' a number in this report is the number the allocator used; and it reads
#' the breach as the **table** `allocate_level_crops()` returns, never by
#' parsing the warning the engine also emits.
#'
#' @section What binds and what is only measured:
#' The national total binds and the reported area shares set the shape
#' within the country (decisions 2 and 8). So a difference between the
#' national total and the sum of the reported units is a property of the
#' evidence, not of the allocation: it is reported, and past both
#' tolerances it refuses the run, but it never moves a national total.
#'
#' `discrepancy` is `national_total - admin_sum` and `discrepancy_frac`
#' divides it by the national total, which is `NA` where that total is
#' zero -- the absolute hectares stay beside it, because a zero total is
#' exactly the case where the fraction says nothing and the level does.
#' A group whose fraction cannot be evaluated is never refused: refusal
#' asks for BOTH tolerances, and one of them has no value.
#'
#' `admin_sum` is `NA`, not `0`, where no unit reported an absolute area
#' (`basis` is `"pattern"` or `"share_normalised"`). The sum of nothing is
#' zero and would read as a discrepancy of the whole national total, which
#' would refuse every consented shares-only family in the pin while
#' nothing at all had been measured against it. Hectares can only be
#' compared with hectares.
#'
#' `admin_sum` is also the point where a failure to harmonise units
#' becomes visible: the readers convert to hectares, tonnes and heads
#' before the pin (Eurostat serves thousands of each), so a source left in
#' its native unit shows up here as a discrepancy three orders of
#' magnitude wide rather than as a quietly wrong shape.
#'
#' @section Refusal:
#' A group is refused when `|discrepancy_frac| > tolerance_relative` AND
#' `|discrepancy| > tolerance_absolute`, and only where coverage is
#' complete -- every unit of the layer reported an absolute area, so the
#' two sides are comparable. Under partial coverage the same difference is
#' the residual pseudo-unit's own target and is not a discrepancy at all
#' (decision T31(a)), so it is reported as `beyond_tolerance` and left to
#' the reader rather than being refused or hidden.
#'
#' Both tolerances are arguments. Their defaults, 10% and 1,000 ha or
#' head, are decision T31(d), and [build_level_crop_targets()] applies the
#' same rule while the targets are built. The rule is restated here so
#' that a report assembled from stored coverage rows is gated too, and so
#' that a run can be re-scored at a different tolerance without being
#' allocated again.
#'
#' @section The production-implied divergence:
#' Production never anchors (decision T31(i)): `t0` is the first observed
#' AREA year and a production row is dropped by the allocator. The
#' information is not therefore worthless, so it is reported here.
#'
#' Per unit, `production_share` is the unit's share of its group's
#' reported production and `area_share_renorm` is the binding area share
#' on the same denominator: both are renormalised over the **common set**,
#' the units carrying both quantities, because a share over one unit set
#' and a share over another are not comparable. `share_divergence` is
#' their difference, `implied_yield_ratio` their ratio -- the unit's yield
#' over the common set's mean yield -- and `production_divergence_tvd` on
#' the group is half the summed absolute divergence, the fraction of the
#' crop the two shapes place in different units.
#'
#' A unit with production and no area share has no yield ratio: the
#' quotient is undefined and is left `NA` rather than reported as `Inf`,
#' while its whole production share stands in `share_divergence`, which is
#' where that case is meant to be read. A common set of fewer than two
#' units supports no comparison at all, and every column is `NA`.
#'
#' `production_share` comes from reported production values where the
#' group has any (`production_basis` is `"value"`), and otherwise from the
#' producers' own declared shares (`"declared"`), which is what a
#' consented shares-only family ships.
#'
#' @section Interior bridges:
#' Decision T31(e) admits a LUH2 proxy bridge across an interior gap of
#' any length, with no refusal threshold, so the length is the only thing
#' that makes a 60-year bridge legible rather than merely legal.
#'
#' A year of a `(container, item)` series is observed when any unit's row
#' that year is `treatment == "observed"`, and carried otherwise. A run of
#' consecutive carried years is a **bridge** when the series has an
#' observed year both before and after it: the leading run that a
#' pre-seam back-cast produces is not a bridge, and reporting the longer
#' of the two would claim the series was interpolated across years it was
#' never observed in at all. `longest_run` keeps the unrestricted maximum
#' beside `longest_interior_run` so the difference is visible.
#'
#' @section The implied cropping intensity:
#' Per unit-year, the whole allocated harvested area over the unit's
#' cropland: `sown_ha / cropland_ha`, flagged where it exceeds the
#' national multi-cropping factor, which is the ceiling a level-0 run
#' would have imposed. Both inputs are supplied by the caller through
#' `intensity` and neither is guessed: with no extent the intensity is
#' `NA`, and with no factor the flag is `NA`. [unit_cropland_extent()]
#' builds the extent from the same layer and cropland the allocation ran
#' on, weighting each cell by `cell_area_frac` exactly as the capacity
#' ceiling does.
#'
#' @param allocation The list [allocate_level_crops()] returns. Its
#'   `targets`, `coverage` and `breach` elements are read; the others are
#'   ignored, so a stored subset of the three is a valid input.
#' @param admin_shares The resolved admin shares the allocation was given,
#'   BEFORE the allocator drops non-area indicators: the production rows
#'   it discards are what the divergence diagnostic reads. `NULL` leaves
#'   the indicator, production and bridge columns empty or `NA`, and a
#'   table matching no `(container, item)` of the allocation warns
#'   rather than degrading to that same empty report in silence.
#' @param intensity Named list for the cropping-intensity diagnostic, with
#'   `unit_cropland` (`year`, `area_code`, `level_polity_code`,
#'   `cropland_ha`, as [unit_cropland_extent()] returns) and `mc_national`
#'   (either a single number or a tibble of `year`, `area_code`,
#'   `mc_factor`). Any other name is refused rather than ignored.
#' @param tolerance_relative Relative discrepancy above which a
#'   complete-coverage group is refused; `0.10` by decision T31(d).
#' @param tolerance_absolute Absolute discrepancy, in hectares or head,
#'   above which the same group is refused; `1000` by decision T31(d).
#'   BOTH must be breached.
#'
#' @return A list of three tibbles:
#'
#' - `groups`: one row per `(year, area_code, item_prod_code)` with the
#'   binding `indicator`, `national_total`, `admin_sum`,
#'   `n_units_reporting`, `n_units_valid`, `coverage`,
#'   `coverage_complete`, the allocator's `basis`, `residual_target`,
#'   `discrepancy`, `discrepancy_frac`, `beyond_tolerance`,
#'   `n_units_production` and `production_divergence_tvd`.
#'   `n_units_valid` is `allocate_level_crops()`'s own `n_units`: the
#'   units of the allocation layer valid for that year, which is the
#'   denominator its coverage decision -- and so its choice between
#'   rescaling and a residual -- actually used. Decision T31(b)'s pattern
#'   extension gives every granted unit a row even where the crop has no
#'   gridded pattern inside it, so a unit is not quietly dropped from the
#'   denominator for lacking one.
#' - `units`: one row per `(year, area_code, level_polity_code,
#'   item_prod_code)` with `area_share`, `production_share`,
#'   `area_share_renorm`, `share_divergence`, `implied_yield_ratio`,
#'   `production_basis`, the irrigation-floor binding and its per-unit-year
#'   count, `sown_ha`, `cropland_ha`, `cropping_intensity`,
#'   `mc_factor_national`, `intensity_exceeds_mc`, and the capacity breach
#'   at both multi-cropping factors.
#' - `bridges`: one row per `(area_code, item_prod_code)` that carried any
#'   year, with `n_years_observed`, `n_years_carried`, `longest_run`,
#'   `longest_interior_run` and the longest interior run's span and
#'   treatments.
#'
#' @seealso [allocate_level_crops()], [build_level_crop_targets()],
#'   [unit_cropland_extent()].
#' @export
#'
#' @examples
#' allocation <- list(
#'   coverage = tibble::tibble(
#'     year = 2000L, area_code = 1L, item_prod_code = 15L,
#'     n_units = 2L, n_units_reporting = 2L, coverage = 1,
#'     basis = "admin_sum", national_total_ha = 250, admin_sum = 225
#'   ),
#'   targets = tibble::tibble(
#'     year = 2000L, area_code = 1L,
#'     level_polity_code = c("A1", "A2"), item_prod_code = 15L,
#'     share = c(0.6, 0.4), target_ha = c(150, 100),
#'     irrigation_clipped_ha = 0, method_crop_alloc = "admin_area_shares"
#'   ),
#'   breach = tibble::tibble(
#'     year = integer(), area_code = integer(),
#'     level_polity_code = character(), item_prod_code = integer(),
#'     mc_basis = character(), in_force = logical(), over_ha = numeric()
#'   )
#' )
#' shares <- tibble::tibble(
#'   year = 2000L, area_code = 1L,
#'   level_polity_code = c("A1", "A2"), item_prod_code = 15L,
#'   indicator_used = "area_harvested", value = c(135, 90),
#'   treatment = "observed"
#' )
#' reconcile_admin_allocation(allocation, shares)$groups
reconcile_admin_allocation <- function(
  allocation,
  admin_shares = NULL,
  intensity = list(),
  tolerance_relative = 0.1,
  tolerance_absolute = 1000
) {
  .recon_check_allocation(allocation)
  extras <- .recon_split_intensity(intensity)
  shares <- .recon_prepare_shares(admin_shares)
  .recon_check_shares_match(shares, allocation$coverage)
  unit_tables <- .recon_unit_tables(
    allocation$targets,
    allocation$breach,
    .recon_production_shares(shares),
    extras
  )
  groups <- .recon_groups(
    allocation$coverage,
    shares,
    unit_tables$production,
    tolerance_relative,
    tolerance_absolute
  )
  .recon_refuse(groups, tolerance_relative, tolerance_absolute)
  list(
    groups = groups,
    units = unit_tables$units,
    bridges = .recon_bridges(shares)
  )
}

#' The cropland each unit holds, on the basis the capacity ceiling uses
#'
#' @description
#' Sum a gridded cropland extent onto the units of an allocation layer,
#' weighting every cell by `cell_area_frac`. This is the denominator
#' `reconcile_admin_allocation()`'s implied cropping intensity divides by,
#' and it is deliberately the same quantity `.capacity_bases()` multiplies
#' by the multi-cropping factor: an intensity taken against any other
#' land basis would not be comparable with the ceiling it is flagged
#' against.
#'
#' A physical cell shared by two units contributes its cropland to each in
#' proportion, so the units' extents inside one cell add up to that cell's
#' own cropland and nothing is double counted. Rows are selected per year
#' by the layer's own validity convention, so an interval-keyed layer
#' returns the unit that existed in each cropland year.
#'
#' @param allocation_layer The country grid the allocation ran on, from
#'   [build_allocation_layer()]: `lon`, `lat`, `area_code`,
#'   `cell_area_frac` and, where a depth is granted,
#'   `level_polity_code`.
#' @param gridded_cropland Per-cell cropland extent, as
#'   [build_gridded_landuse()] takes it: `lon`, `lat`, `year`,
#'   `cropland_ha`.
#'
#' @return A tibble of `year`, `area_code`, `level_polity_code`,
#'   `cropland_ha` and `n_cells`.
#'
#' @seealso [reconcile_admin_allocation()].
#' @export
#'
#' @examples
#' layer <- tibble::tibble(
#'   lon = c(0.25, 0.75, 0.75), lat = 50.25, area_code = 1L,
#'   level_polity_code = c("A1", "A1", "A2"), level = 1L,
#'   cell_area_frac = c(1, 0.25, 0.75)
#' )
#' cropland <- tibble::tibble(
#'   lon = c(0.25, 0.75), lat = 50.25, year = 2000L,
#'   cropland_ha = c(100, 400)
#' )
#' unit_cropland_extent(layer, cropland)
unit_cropland_extent <- function(allocation_layer, gridded_cropland) {
  # A level-0 grid carries no `level_polity_code` at all, and the extent of
  # a container is still a well-formed answer: the column is completed to
  # `NA` rather than the call failing on a grouping key that is not there.
  layer <- .normalize_country_grid(allocation_layer, "allocation_layer") |>
    ensure_columns(tibble::tibble(level_polity_code = character()))
  .check_columns(
    gridded_cropland,
    c("lon", "lat", "year", "cropland_ha"),
    "gridded_cropland"
  )
  cropland <- tibble::as_tibble(gridded_cropland) |>
    dplyr::mutate(year = as.integer(year))
  years <- sort(unique(cropland$year))
  purrr::map(years, \(yr) .recon_extent_one_year(yr, layer, cropland)) |>
    purrr::list_rbind()
}

.recon_extent_one_year <- function(yr, layer, cropland) {
  cells <- cropland |>
    dplyr::filter(year == yr) |>
    dplyr::select("lon", "lat", "cropland_ha")
  .filter_country_grid_year(layer, yr) |>
    dplyr::inner_join(cells, by = c("lon", "lat")) |>
    dplyr::summarise(
      cropland_ha = sum(cropland_ha * cell_area_frac, na.rm = TRUE),
      n_cells = dplyr::n(),
      .by = c("area_code", "level_polity_code")
    ) |>
    dplyr::mutate(year = yr, .before = 1L)
}

# --- Input contract ----------------------------------------------------------

.recon_check_allocation <- function(allocation) {
  needed <- c("targets", "coverage", "breach")
  missing <- needed[!rlang::has_name(allocation, needed)]
  if (!is.list(allocation) || length(missing) > 0L) {
    cli::cli_abort(
      c(
        "{.arg allocation} must hold the tables
         {.fn allocate_level_crops} returns.",
        x = "Missing element{?s}: {.field {missing}}.",
        i = "Pass its return value, or the stored {.field targets},
             {.field coverage} and {.field breach} tables."
      ),
      class = "whep_recon_missing_table"
    )
  }
  .check_columns(
    allocation$coverage,
    c(
      .recon_group_cols(),
      "n_units",
      "n_units_reporting",
      "coverage",
      "basis",
      "national_total_ha",
      "admin_sum"
    ),
    "allocation$coverage"
  )
  .check_columns(
    allocation$targets,
    c(
      .recon_unit_cols(),
      "share",
      "target_ha",
      "irrigation_clipped_ha",
      "method_crop_alloc"
    ),
    "allocation$targets"
  )
  if (nrow(allocation$breach) > 0L) {
    .check_columns(
      allocation$breach,
      c(.recon_unit_cols(), "mc_basis", "in_force", "over_ha"),
      "allocation$breach"
    )
  }
}

.recon_group_cols <- function() {
  c("year", "area_code", "item_prod_code")
}

.recon_unit_cols <- function() {
  c("year", "area_code", "level_polity_code", "item_prod_code")
}

.recon_unit_year_cols <- function() {
  c("year", "area_code", "level_polity_code")
}

.recon_split_intensity <- function(intensity) {
  defaults <- list(unit_cropland = NULL, mc_national = NULL)
  unnamed <- length(intensity) > 0L && is.null(names(intensity))
  if (!is.list(intensity) || unnamed) {
    cli::cli_abort(
      "{.arg intensity} must be a named list.",
      class = "whep_recon_bad_intensity"
    )
  }
  unknown <- setdiff(names(intensity), names(defaults))
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "Unknown {.arg intensity} key{?s}: {.field {unknown}}.",
        i = "Known key{?s}: {.field {names(defaults)}}."
      ),
      class = "whep_recon_unknown_key"
    )
  }
  out <- utils::modifyList(defaults, intensity)
  if (!is.null(out$unit_cropland)) {
    .check_columns(
      out$unit_cropland,
      c(.recon_unit_year_cols(), "cropland_ha"),
      "intensity$unit_cropland"
    )
  }
  .recon_check_mc(out$mc_national)
  out
}

# A single number is the whole-run factor; a table gives one per container
# and year. Anything else would have to be guessed at, and the factor is a
# ceiling a unit is judged against, so it is never invented.
.recon_check_mc <- function(mc) {
  if (is.null(mc) || is.data.frame(mc)) {
    if (is.data.frame(mc)) {
      .check_columns(
        mc,
        c("year", "area_code", "mc_factor"),
        "intensity$mc_national"
      )
    }
    return(invisible(NULL))
  }
  if (!is.numeric(mc) || length(mc) != 1L || is.na(mc)) {
    cli::cli_abort(
      "{.arg intensity$mc_national} must be one number or a table of
       {.field year}, {.field area_code} and {.field mc_factor}.",
      class = "whep_recon_bad_mc"
    )
  }
  invisible(NULL)
}

# --- Admin shares ------------------------------------------------------------

# `value`, `share` and `treatment` are optional: a contract table has no
# `treatment` until the seam back-cast adds one, and a consented family has
# no `value`. `indicator_used` is NOT optional -- it is `allow_missing =
# FALSE` in `admin_shares_schema()`, and without it an area row and a
# production row cannot be told apart, which is the one distinction this
# whole report turns on.
.recon_prepare_shares <- function(admin_shares) {
  proto <- tibble::tibble(
    year = integer(),
    area_code = integer(),
    level_polity_code = character(),
    item_prod_code = integer(),
    indicator_used = character(),
    value = numeric(),
    share = numeric(),
    treatment = character()
  )
  if (is.null(admin_shares) || nrow(admin_shares) == 0L) {
    return(proto)
  }
  .check_columns(
    admin_shares,
    c(.recon_unit_cols(), "indicator_used"),
    "admin_shares"
  )
  rows <- tibble::as_tibble(admin_shares)
  rows |>
    dplyr::mutate(
      year = as.integer(year),
      area_code = as.integer(area_code),
      item_prod_code = as.integer(item_prod_code),
      level_polity_code = as.character(level_polity_code),
      indicator_used = as.character(indicator_used),
      value = .recon_optional_num(rows, "value"),
      share = .recon_optional_num(rows, "share"),
      treatment = .recon_treatment(rows)
    ) |>
    dplyr::select(dplyr::all_of(names(proto)))
}

.recon_optional_num <- function(rows, column) {
  if (rlang::has_name(rows, column)) {
    return(as.numeric(rows[[column]]))
  }
  rep(NA_real_, nrow(rows))
}

.recon_treatment <- function(rows) {
  if (rlang::has_name(rows, "treatment")) {
    return(as.character(rows$treatment))
  }
  cli::cli_inform(c(
    "{.arg admin_shares} carries no {.field treatment} column, so no year
     can be told from a carried one.",
    i = "The bridge report is empty; {.fn backcast_admin_shares} adds the
         column."
  ))
  rep(NA_character_, nrow(rows))
}

# The indicators that bind an allocation. This must agree, member for
# member, with `.alloc_drop_non_area()` in `R/spatialize_levels.R`, which
# keeps the same four; `test_admin_reconcile.R` walks the closed
# vocabulary of `admin_shares_schema()` and asserts the two classify every
# member alike, so a vocabulary change fails there rather than silently
# splitting the two files apart.
.recon_is_area_indicator <- function(indicator) {
  indicator %in%
    c(
      "area_harvested",
      "area_planted_or_sown",
      "area_main",
      "area_cultivated"
    )
}

.recon_area_rows <- function(shares) {
  dplyr::filter(shares, .recon_is_area_indicator(indicator_used))
}

# A shares table keyed off a space the allocation does not use -- another
# `area_code`, another item vocabulary -- joins to nothing, and the report
# it then produces is an all-NA one. That is exactly what a run carrying
# no admin evidence looks like, and a real run legitimately has NA rows
# wherever the basis is `"pattern"`, so the mismatch is invisible unless
# it is said out loud. The key is (container, item) and NOT the year: a
# shares table legitimately carries the pre-t0 back-cast years the
# allocation never ran, and the bridge report is about exactly those.
.recon_check_shares_match <- function(shares, coverage) {
  cols <- c("area_code", "item_prod_code")
  if (nrow(shares) == 0L || nrow(coverage) == 0L) {
    return(invisible(NULL))
  }
  keys <- coverage |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer)) |>
    dplyr::distinct()
  matched <- nrow(dplyr::semi_join(shares, keys, by = cols))
  if (matched == nrow(shares)) {
    return(invisible(NULL))
  }
  .recon_warn_unmatched(matched, nrow(shares), keys)
}

.recon_warn_unmatched <- function(matched, supplied, keys) {
  if (matched > 0L) {
    cli::cli_warn(
      c(
        "{matched} of {supplied} {.arg admin_shares} row{?s} match a
         (container, item) the allocation covers.",
        i = "The other {supplied - matched} contribute nothing to the
             report."
      ),
      class = "whep_recon_shares_partial"
    )
    return(invisible(NULL))
  }
  codes <- utils::head(sort(unique(keys$area_code)), 5L)
  cli::cli_warn(
    c(
      "None of the {supplied} {.arg admin_shares} row{?s} match a
       (container, item) the allocation covers.",
      x = "The indicator, production and bridge columns are all
           {.val {NA}}, which is what a run carrying no admin evidence
           looks like.",
      i = "The allocation's containers include area_code
           {.val {codes}}."
    ),
    class = "whep_recon_shares_unmatched"
  )
}

# --- The group report --------------------------------------------------------

.recon_groups <- function(coverage, shares, production, relative, absolute) {
  coverage |>
    tibble::as_tibble() |>
    .recon_group_measures() |>
    .recon_group_tolerance(relative, absolute) |>
    dplyr::left_join(
      .recon_group_indicator(shares),
      by = .recon_group_cols()
    ) |>
    dplyr::left_join(production, by = .recon_group_cols()) |>
    dplyr::relocate("indicator", "n_indicators", .after = "item_prod_code") |>
    dplyr::arrange(year, area_code, item_prod_code)
}

# `basis` decides what `admin_sum` means. Under `"pattern"` no unit
# reported at all and under `"share_normalised"` none reported an absolute
# area, so `.alloc_coverage()`'s zero is the sum of nothing, not a measured
# zero: comparing it with a national total in hectares would manufacture a
# 100% discrepancy out of an absence of evidence.
.recon_group_measures <- function(coverage) {
  coverage |>
    dplyr::mutate(
      admin_sum = dplyr::if_else(
        basis %in% c("pattern", "share_normalised"),
        NA_real_,
        as.numeric(admin_sum)
      ),
      national_total = as.numeric(national_total_ha),
      coverage_complete = basis == "admin_sum",
      residual_target = dplyr::if_else(
        basis == "residual",
        pmax(national_total - admin_sum, 0),
        NA_real_
      ),
      discrepancy = national_total - admin_sum,
      discrepancy_frac = dplyr::if_else(
        !is.na(national_total) & national_total > 0,
        discrepancy / national_total,
        NA_real_
      )
    ) |>
    dplyr::select(
      dplyr::all_of(.recon_group_cols()),
      "national_total",
      "admin_sum",
      "n_units_reporting",
      n_units_valid = "n_units",
      "coverage",
      "coverage_complete",
      "basis",
      "residual_target",
      "discrepancy",
      "discrepancy_frac"
    )
}

# Both tolerances, or neither: an undefined fraction (a zero national
# total) cannot be breached, so the group is flagged only on evidence that
# exists.
.recon_group_tolerance <- function(groups, relative, absolute) {
  dplyr::mutate(
    groups,
    beyond_tolerance = !is.na(discrepancy_frac) &
      abs(discrepancy_frac) > relative &
      abs(discrepancy) > absolute
  )
}

.recon_group_indicator <- function(shares) {
  area <- .recon_area_rows(shares)
  out <- area |>
    dplyr::summarise(
      n_indicators = dplyr::n_distinct(indicator_used),
      # Not `unique(...)[[1L]]`: `dplyr::summarise()` types the column by
      # evaluating the expression once on a zero-row slice, where that
      # subscript is out of bounds.
      indicator = dplyr::if_else(
        dplyr::n_distinct(indicator_used) == 1L,
        .recon_join_labels(indicator_used),
        "mixed"
      ),
      .by = dplyr::all_of(.recon_group_cols())
    )
  .recon_warn_mixed(out)
  out
}

# A resolved group should carry one indicator: the source precedence picks
# per container x indicator x item x year. Two of them in one group means
# hectares of harvested area are being added to hectares of sown area, and
# the sum is then compared with a national total that is neither.
.recon_warn_mixed <- function(groups) {
  mixed <- dplyr::filter(groups, n_indicators > 1L)
  if (nrow(mixed) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "{nrow(mixed)} (container, item, year) group{?s} report more than one
     area indicator, and their {.field admin_sum} adds the two together.",
    i = "Resolve the sources first with {.fn resolve_admin_shares}; the
         groups are marked {.val mixed}."
  ))
}

.recon_refuse <- function(groups, relative, absolute) {
  bad <- dplyr::filter(groups, coverage_complete, beyond_tolerance)
  if (nrow(bad) == 0L) {
    return(invisible(NULL))
  }
  worst <- bad[which.max(abs(bad$discrepancy)), , drop = FALSE]
  cli::cli_abort(
    c(
      "{nrow(bad)} (container, item, year) group{?s} report a complete set
       of units whose values do not add up to the national total.",
      x = "Worst: area_code {.val {worst$area_code}}, item
           {.val {worst$item_prod_code}}, {.val {worst$year}} --
           {round(worst$admin_sum)} reported against
           {round(worst$national_total)} national
           ({round(100 * worst$discrepancy_frac, 1)}%).",
      i = "Refused because BOTH tolerances are breached
           ({.val {relative}} relative and {.val {absolute}} absolute,
           decision T31(d)). Under partial coverage the same difference
           would be the residual pseudo-unit's target."
    ),
    class = "whep_recon_admin_discrepancy"
  )
}

# --- The unit companion ------------------------------------------------------

.recon_unit_tables <- function(targets, breach, production, extras) {
  units <- targets |>
    tibble::as_tibble() |>
    dplyr::select(
      dplyr::all_of(.recon_unit_cols()),
      "method_crop_alloc",
      "target_ha",
      area_share = "share",
      "irrigation_clipped_ha"
    ) |>
    dplyr::left_join(
      production,
      by = .recon_unit_cols(),
      relationship = "many-to-one"
    ) |>
    .recon_divergence() |>
    .recon_floor_binding() |>
    .recon_intensity(extras) |>
    dplyr::left_join(
      .recon_breach_wide(breach),
      by = .recon_unit_cols()
    ) |>
    .recon_breach_defaults(targets, breach) |>
    dplyr::arrange(year, area_code, item_prod_code, level_polity_code)
  list(
    units = .recon_units_out(units),
    production = .recon_divergence_sum(units)
  )
}

# The comparison runs on the COMMON set -- units carrying both a production
# observation and an area share -- because a share taken over one unit set
# is not comparable with a share over another. Fewer than two common units
# is not a set: one unit's renormalised production share would be 1 by
# construction and its divergence would measure the missing sibling.
.recon_divergence <- function(units) {
  units |>
    dplyr::mutate(
      common = !is.na(production_raw) & !is.na(area_share),
      n_common = sum(common),
      prod_sum = sum(production_raw[common]),
      area_sum = sum(area_share[common]),
      .by = dplyr::all_of(.recon_group_cols())
    ) |>
    dplyr::mutate(
      usable = common & n_common > 1L & prod_sum > 0 & area_sum > 0,
      production_share = dplyr::if_else(
        usable,
        production_raw / prod_sum,
        NA_real_
      ),
      area_share_renorm = dplyr::if_else(
        usable,
        area_share / area_sum,
        NA_real_
      ),
      share_divergence = production_share - area_share_renorm,
      # A unit with production and no allocated area: the ratio is
      # undefined, and `Inf` would be a value the evidence never carried.
      # The divergence keeps the whole signal.
      implied_yield_ratio = dplyr::if_else(
        !is.na(area_share_renorm) & area_share_renorm > 0,
        production_share / area_share_renorm,
        NA_real_
      ),
      production_basis = dplyr::if_else(
        usable,
        production_basis,
        NA_character_
      )
    )
}

.recon_divergence_sum <- function(units) {
  units |>
    dplyr::summarise(
      n_units_production = dplyr::first(n_common),
      production_divergence_tvd = dplyr::if_else(
        any(!is.na(share_divergence)),
        0.5 * sum(abs(share_divergence), na.rm = TRUE),
        NA_real_
      ),
      .by = dplyr::all_of(.recon_group_cols())
    )
}

# The rainfed target is the unit's area target minus its irrigated one,
# floored at zero, so the floor binds exactly where the irrigated target
# was clipped. `1e-9` is `.alloc_warn_clipped()`'s own threshold.
.recon_floor_binding <- function(units) {
  units |>
    dplyr::mutate(
      floor_binding_ha = dplyr::coalesce(irrigation_clipped_ha, 0),
      floor_binding = floor_binding_ha > 1e-9
    ) |>
    dplyr::mutate(
      n_floor_binding = sum(floor_binding),
      .by = dplyr::all_of(.recon_unit_year_cols())
    )
}

.recon_intensity <- function(units, extras) {
  units |>
    dplyr::mutate(
      sown_ha = sum(target_ha, na.rm = TRUE),
      .by = dplyr::all_of(.recon_unit_year_cols())
    ) |>
    .recon_attach_cropland(extras$unit_cropland) |>
    .recon_attach_mc(extras$mc_national) |>
    dplyr::mutate(
      cropping_intensity = dplyr::if_else(
        !is.na(cropland_ha) & cropland_ha > 0,
        sown_ha / cropland_ha,
        NA_real_
      ),
      intensity_exceeds_mc = cropping_intensity > mc_factor_national
    )
}

.recon_attach_cropland <- function(units, unit_cropland) {
  if (is.null(unit_cropland)) {
    return(dplyr::mutate(units, cropland_ha = NA_real_))
  }
  dplyr::left_join(
    units,
    unit_cropland |>
      tibble::as_tibble() |>
      dplyr::select(dplyr::all_of(.recon_unit_year_cols()), "cropland_ha"),
    by = .recon_unit_year_cols(),
    relationship = "many-to-one"
  )
}

.recon_attach_mc <- function(units, mc) {
  if (is.null(mc)) {
    return(dplyr::mutate(units, mc_factor_national = NA_real_))
  }
  if (!is.data.frame(mc)) {
    return(dplyr::mutate(units, mc_factor_national = as.numeric(mc)))
  }
  dplyr::left_join(
    units,
    mc |>
      tibble::as_tibble() |>
      dplyr::select("year", "area_code", mc_factor_national = "mc_factor"),
    by = c("year", "area_code"),
    relationship = "many-to-one"
  )
}

# --- The capacity breach, at both factors ------------------------------------

# The breach table holds only the rows that breached, so a unit-item absent
# from it exceeded nothing at a basis that WAS scored. Whether the unit
# basis was scored at all is not in the table: `.capacity_bases()` builds
# it only where the layer carries a granted depth, which is exactly what
# the targets say. Zero and NA are different claims and the run decides
# which one is true.
.recon_breach_wide <- function(breach) {
  bases <- .recon_breach_bases()
  proto <- tibble::tibble(
    year = integer(),
    area_code = integer(),
    level_polity_code = character(),
    item_prod_code = integer(),
    breach_national_ha = numeric(),
    breach_unit_ha = numeric()
  )
  if (nrow(breach) == 0L) {
    return(proto)
  }
  breach |>
    tibble::as_tibble() |>
    dplyr::filter(mc_basis %in% bases) |>
    dplyr::summarise(
      over_ha = sum(over_ha, na.rm = TRUE),
      .by = c(.recon_unit_cols(), "mc_basis")
    ) |>
    tidyr::pivot_wider(
      names_from = "mc_basis",
      values_from = "over_ha",
      names_glue = "breach_{mc_basis}_ha"
    ) |>
    ensure_columns(proto)
}

.recon_breach_bases <- function() {
  c("national", "unit")
}

# Which basis the redistribution actually ran against. It is a property of
# the run, not of the row, so one value or none: where nothing breached at
# the basis in force, `.apply_capacity_constraint()` omits it from the
# table entirely and the answer is unknown rather than absent.
#
# Two of them in force is a third state, and today's engine cannot reach it
# -- but the report also reads STORED breach tables, and naming one of the
# two would attribute every unit's judgement to a ceiling half of them were
# never judged against. It is unknown too, and said out loud rather than
# folded into the same silent `NA` as "nothing breached".
.recon_in_force <- function(breach) {
  if (nrow(breach) == 0L) {
    return(NA_character_)
  }
  in_force <- unique(breach$mc_basis[dplyr::coalesce(breach$in_force, FALSE)])
  if (length(in_force) > 1L) {
    cli::cli_warn(
      c(
        "The breach table marks {length(in_force)} multi-cropping bases
         in force at once: {.val {in_force}}.",
        x = "The basis is a property of the run, so at most one can be,
             and {.field breach_in_force_basis} is left {.val {NA}}.",
        i = "Rebuild the allocation: {.fn allocate_level_crops} marks the
             one basis the redistribution actually ran against."
      ),
      class = "whep_recon_in_force_ambiguous"
    )
  }
  if (length(in_force) != 1L) {
    return(NA_character_)
  }
  in_force
}

.recon_breach_defaults <- function(units, targets, breach) {
  unit_basis <- any(!is.na(targets$level_polity_code))
  # Resolved BEFORE the mutate: a condition raised inside one is rewrapped
  # by dplyr in its own class, and a caller cannot then catch the one this
  # function actually signalled.
  in_force <- .recon_in_force(breach)
  dplyr::mutate(
    units,
    breach_national_ha = dplyr::coalesce(breach_national_ha, 0),
    breach_unit_ha = dplyr::if_else(
      rep(unit_basis, dplyr::n()),
      dplyr::coalesce(breach_unit_ha, 0),
      NA_real_
    ),
    # A property of the run, not of the row: every unit is judged against
    # the same basis, so the column is constant rather than absent
    # wherever a unit happened not to breach.
    breach_in_force_basis = in_force
  )
}

.recon_units_out <- function(units) {
  dplyr::select(
    units,
    dplyr::all_of(.recon_unit_cols()),
    "method_crop_alloc",
    "target_ha",
    "area_share",
    "area_share_renorm",
    "production_share",
    "production_basis",
    "share_divergence",
    "implied_yield_ratio",
    "floor_binding",
    "floor_binding_ha",
    "n_floor_binding",
    "sown_ha",
    "cropland_ha",
    "cropping_intensity",
    "mc_factor_national",
    "intensity_exceeds_mc",
    "breach_national_ha",
    "breach_unit_ha",
    "breach_in_force_basis"
  )
}

# --- Production-implied shares -----------------------------------------------

# Values where the group reports any, and otherwise the producers' own
# declared shares -- which is all a consented shares-only family ships.
# Never both: a hectare of production and a fraction are not addable.
.recon_production_shares <- function(shares) {
  proto <- tibble::tibble(
    year = integer(),
    area_code = integer(),
    level_polity_code = character(),
    item_prod_code = integer(),
    production_raw = numeric(),
    production_basis = character()
  )
  rows <- dplyr::filter(shares, indicator_used == "production")
  if (nrow(rows) == 0L) {
    return(proto)
  }
  rows |>
    dplyr::mutate(
      n_valued = sum(!is.na(value)),
      value_sum = sum(value, na.rm = TRUE),
      n_declared = sum(!is.na(share)),
      .by = dplyr::all_of(.recon_group_cols())
    ) |>
    dplyr::mutate(
      production_basis = dplyr::case_when(
        n_valued > 0L & value_sum > 0 ~ "value",
        n_declared > 0L ~ "declared",
        .default = NA_character_
      ),
      production_raw = dplyr::case_when(
        production_basis == "value" ~ value / value_sum,
        production_basis == "declared" ~ share,
        .default = NA_real_
      )
    ) |>
    dplyr::select(dplyr::all_of(names(proto)))
}

# --- Interior bridges --------------------------------------------------------

.recon_bridges <- function(shares) {
  proto <- tibble::tibble(
    area_code = integer(),
    item_prod_code = integer(),
    n_years_observed = integer(),
    n_years_carried = integer(),
    longest_run = integer(),
    longest_interior_run = integer(),
    interior_run_start = integer(),
    interior_run_end = integer(),
    interior_run_treatment = character()
  )
  area <- .recon_area_rows(shares)
  if (nrow(area) == 0L || all(is.na(area$treatment))) {
    return(proto)
  }
  years <- .recon_series_years(area)
  runs <- .recon_carried_runs(years, area)
  if (nrow(runs) == 0L) {
    return(proto)
  }
  .recon_bridge_summary(runs, years)
}

# A series year is observed when ANY unit observed it. A unit back-cast
# across a year its siblings reported is a unit-level gap, not a gap in
# the (container, item) series the plan asks about.
.recon_series_years <- function(area) {
  area |>
    dplyr::summarise(
      observed = any(!is.na(treatment) & treatment == "observed"),
      .by = c("area_code", "item_prod_code", "year")
    ) |>
    dplyr::arrange(area_code, item_prod_code, year)
}

.recon_carried_runs <- function(years, area) {
  carried <- years |>
    dplyr::filter(!observed) |>
    dplyr::mutate(
      run_id = cumsum(c(TRUE, diff(year) != 1L)),
      .by = c("area_code", "item_prod_code")
    )
  if (nrow(carried) == 0L) {
    return(tibble::tibble())
  }
  carried |>
    dplyr::inner_join(
      dplyr::select(
        area,
        "area_code",
        "item_prod_code",
        "year",
        "treatment"
      ),
      by = c("area_code", "item_prod_code", "year"),
      relationship = "one-to-many"
    ) |>
    dplyr::summarise(
      run_length = dplyr::n_distinct(year),
      # `first`/`last`, not `min`/`max`: the rows arrive sorted by year
      # within a run, and a bare `min()` on the zero-row slice
      # `dplyr::summarise()` types the column with warns and returns `Inf`.
      run_start = dplyr::first(year),
      run_end = dplyr::last(year),
      treatments = .recon_join_labels(treatment),
      .by = c("area_code", "item_prod_code", "run_id")
    )
}

.recon_join_labels <- function(labels) {
  unique(dplyr::coalesce(as.character(labels), "<NA>")) |>
    sort(method = "radix") |>
    paste(collapse = "|")
}

# A run is a BRIDGE only with observed years on both sides of it. The
# leading run a pre-seam back-cast produces has none before it, and calling
# that a bridge would claim the series was interpolated across years it was
# never observed in.
.recon_bridge_summary <- function(runs, years) {
  observed <- years |>
    dplyr::filter(observed) |>
    dplyr::summarise(
      first_observed = dplyr::first(year),
      last_observed = dplyr::last(year),
      n_years_observed = dplyr::n(),
      .by = c("area_code", "item_prod_code")
    )
  marked <- runs |>
    dplyr::left_join(observed, by = c("area_code", "item_prod_code")) |>
    dplyr::mutate(
      n_years_observed = dplyr::coalesce(n_years_observed, 0L),
      interior = dplyr::coalesce(
        run_start > first_observed & run_end < last_observed,
        FALSE
      )
    )
  dplyr::left_join(
    .recon_run_totals(marked),
    .recon_longest_interior(marked),
    by = c("area_code", "item_prod_code")
  ) |>
    dplyr::mutate(
      longest_interior_run = dplyr::coalesce(longest_interior_run, 0L)
    ) |>
    dplyr::arrange(area_code, item_prod_code)
}

.recon_run_totals <- function(marked) {
  dplyr::summarise(
    marked,
    n_years_observed = dplyr::first(n_years_observed),
    n_years_carried = sum(run_length),
    longest_run = max(c(0L, run_length)),
    .by = c("area_code", "item_prod_code")
  )
}

.recon_longest_interior <- function(marked) {
  marked |>
    dplyr::filter(interior) |>
    dplyr::arrange(area_code, item_prod_code, dplyr::desc(run_length)) |>
    dplyr::slice_head(n = 1L, by = c("area_code", "item_prod_code")) |>
    dplyr::select(
      "area_code",
      "item_prod_code",
      longest_interior_run = "run_length",
      interior_run_start = "run_start",
      interior_run_end = "run_end",
      interior_run_treatment = "treatments"
    )
}
