# Country-year summary of the gridded critical-nitrogen exceedance. The grid
# comparison (build_n_boundary_exceedance()) is cell-first: each cell spends
# its one allowance before crops and polities are attributed. Country-level
# quantities therefore have to be formed from the attributed crop rows AFTER
# that comparison, never by re-comparing country totals with a critical
# surplus, and never by summing a whole-cell quantity per country (a cell
# shared by two polities would be counted once for each). The unit of that
# comparison is the cell, or under the grassland split its managed and its
# extensive-grassland component, which are compared independently and never
# net against each other.

#' Summarise gridded nitrogen exceedance to a country-year table.
#'
#' @description
#' Turns the crop rows of a [build_n_boundary_exceedance()] grid result into
#' one row per country and year, holding the exceedance, the share of nitrogen
#' inputs it represents, the share of the country's positive surplus it
#' represents, and the share of that surplus that lies in comparison units
#' above their critical surplus. Every quantity is
#' formed from the attributed crop rows of cells with
#' `coverage_state == "valid"`, so the numerator and every denominator cover
#' the same rows. Inputs in cells that are out of the critical-surplus domain,
#' lack a critical value or have no allowance area fall outside all of them,
#' as do rows of a grassland-split component the comparison leaves out; rows
#' naming no crop were removed by the exceedance itself.
#'
#' The comparison unit is the cell, or, under the grassland split
#' (`land_use = "all"`, `grassland_split = "image_density"`), the managed and
#' the extensive-grassland component of a cell, which are compared with their
#' own allowances and never net against each other. Under
#' `regime_comparison = "separate"` the unit is the rainfed or the irrigated
#' part of the cell or component, and `surplus` must keep its `water_regime`
#' rows, which are then joined regime by regime. Deficit units never offset
#' excess elsewhere. Two country quantities follow and both use the country's
#' **own** crop surplus (`actual_n_t`), never a whole-cell surplus, which sums
#' every polity in a shared border cell and would count that cell once per
#' polity:
#' * `positive_surplus_n_t`: the country's own crop surplus in the comparison
#'   units whose surplus is positive. Without the split this is the cells with
#'   positive whole-cell surplus; with it, the positive managed surplus plus
#'   the positive extensive-grassland surplus of each cell, so a positive
#'   managed surplus is counted even beside a negative extensive one;
#' * `exceeding_surplus_n_t`: the same, restricted to the units whose overshoot
#'   over their allowance is positive.
#'
#' The fraction of the country's positive surplus that is excess is
#' `exceedance_share_of_positive_surplus = exceedance_n_t /
#' positive_surplus_n_t`. It is the country exceedance itself over the same
#' positive surplus, so it is not `beyond_share`, which counts the whole
#' surplus of an exceeding unit and not only the part above its allowance.
#'
#' Unit membership is decided from the unit's own state (its surplus and its
#' overshoot), never from the crop-attributed exceedance. A unit whose crop
#' shares are undefined (zero or ill-conditioned total surplus) attributes no
#' exceedance to any crop and leaves it on a residual record, yet its crop rows
#' stay in `positive_surplus_n_t` and `exceeding_surplus_n_t` when the unit is
#' positive and exceeding, so `beyond_share` does not depend on the
#' attribution. The residual exceedance stays in `unallocated_exceedance_n_t`
#' of the diagnostics, which also count the units and rows involved.
#'
#' The country's own crop surplus in a shared unit can be negative while the
#' unit total is positive (the other polity carries the excess), and crop
#' exceedance is a signed share of the unit overshoot, so a country ratio can
#' fall outside `[0, 1]`: such negative own rows lower the country's positive
#' surplus, and its exceedance can be negative or larger than that surplus.
#' This affects `exceedance_share_of_positive_surplus`, `beyond_share` and
#' `excess_share_of_inputs`. Such rows are kept, flagged in
#' `ratio_outside_unit`, and counted in the diagnostics; they are never
#' clipped.
#'
#' The world sum of `positive_surplus_n_t` equals the sum of the positive unit
#' surpluses of the valid cells. The world sum of `exceedance_n_t` equals the
#' summed cell exceedance minus the exceedance left on unallocated residual
#' rows, which the diagnostics report and reconcile per year; the function
#' aborts if they do not.
#'
#' Every exceeding unit is a positive-surplus unit only with
#' `negative_critical = "clamp"`: a zero allowance means an overshoot needs a
#' positive surplus. With `"keep"` a unit with a negative critical surplus can
#' overshoot with a zero or negative surplus. Its exceedance stays in
#' `exceedance_n_t`, but the unit is outside both `positive_surplus_n_t` and
#' `exceeding_surplus_n_t`, so `beyond_share` does not see it; the diagnostics
#' report that overshoot in `overshoot_without_surplus_n_t` (zero under the
#' clamp).
#'
#' @section Boundary side:
#' `boundary_side` is decided after aggregation, from the country
#' `beyond_share = exceeding_surplus_n_t / positive_surplus_n_t`: the country
#' is `"Exceedance"` when more than `beyond_share_cut` of its positive surplus
#' lies in units above their critical surplus, and `"Within_boundary"`
#' otherwise. The one-half default of `beyond_share_cut` is a WHEP criterion,
#' not a published threshold. The share is `NA`, and the side `NA`, when
#' `positive_surplus_n_t` is zero or negative: the country-year is left
#' unclassified, flagged in `signed_denominator_nonpositive` and counted in the
#' diagnostics. The labels are those [classify_sjos_n()] uses for its per-crop
#' boundary side, which stays the producer-side classification.
#'
#' @section Excess share of inputs:
#' `excess_share_of_inputs = exceedance_n_t / input_std_n_t`, where
#' `input_std_n_t` is the sum of `n_input_std_t` (synthetic fertiliser, manure
#' and excreta, biological fixation, deposition and urban nitrogen) over the
#' same crop rows as the exceedance. It is `NA` when `input_std_n_t` is zero or
#' negative. The country ratio has a signed numerator, so it is bounded by
#' `[0, 1]` only when the country's contributions to every cell with
#' exceedance are non-negative; at world level, summed over all attributed
#' rows, it is bounded once negative critical surpluses are clamped
#' (`negative_critical = "clamp"`), because a unit overshoot is then at most
#' its positive surplus, which is at most its inputs. The last step needs
#' `surplus_n_t <= n_input_std_t` on every row. That holds for
#' `surplus_method = "harvest_removal"`, where the surplus is the inputs minus
#' non-negative harvest removals, but not for `"full_balance"`: its
#' `n_balance_t` includes soil organic matter mineralisation, which
#' `n_input_std_t` excludes, so the world share can exceed one even after the
#' clamp. Under `"keep"` a unit with a negative critical surplus can overshoot
#' by more than its inputs. The world ratio outside `[0, 1]` is reported in
#' `world_ratio_outside_unit` of the diagnostics.
#'
#' @section Exceedance share of positive surplus:
#' `exceedance_share_of_positive_surplus = exceedance_n_t /
#' positive_surplus_n_t`, with the exceedance signed as above. It is `NA` when
#' `positive_surplus_n_t` is zero or negative, exactly where `beyond_share` is
#' `NA`. The country ratio is guaranteed to lie in `[0, 1]` only when negative
#' critical surpluses are clamped and the country's contributions to every
#' positive unit are non-negative. At world level it is bounded once negative
#' critical surpluses are clamped, because a unit overshoot is then at most its
#' positive surplus; unlike the share of inputs, that bound needs no assumption
#' on the surplus method.
#'
#' @param exceedance A [build_n_boundary_exceedance()] result at
#'   `resolution = "grid"` with `metric = "surplus"`, possibly bound over
#'   several years. Its `negative_critical`, `land_use`, `grassland_split`
#'   and `regime_comparison` stamps must each be constant. It may carry the grassland split.
#' @param surplus The [calculate_n_surplus()] output the grid was computed
#'   from, carrying `lon`, `lat`, `area_code`, `item_cbs_code`, `year` and
#'   `n_input_std_t`. Every crop row of `exceedance` must find exactly one
#'   row here; a row that does not aborts.
#' @param ag_land A [build_ag_land_support()] table (`area_code`, `year`,
#'   `area_ha`). Its hectares are summed per country and year into
#'   `ag_area_ha`, WHEP's agricultural area, the basis of per-hectare
#'   exceedance. A country-year with no row keeps `NA`.
#' @param nourishment Optional [normalize_nourishment()] output (`year`,
#'   `area_code`, `nourish`), one row per country-year. When supplied, the
#'   result gains `nourish` and `sjos_class`, the country boundary side crossed
#'   with the nourishment class over the levels of [sjos_levels]; a country-year
#'   with no boundary side or no class is `NA`.
#' @param beyond_share_cut Share of the positive surplus above which the
#'   country is on the `"Exceedance"` side, a number in `[0, 1)`. Defaults to
#'   `0.5`, a WHEP criterion.
#' @param example If `TRUE`, return a small fixture.
#'
#' @return A named list of two tibbles.
#'
#' `country`, one row per `area_code` and `year`:
#' * `exceedance_n_t`: country exceedance, t N (sum of the crop-attributed
#'   `exceedance_n_t`, signed).
#' * `input_std_n_t`: sum of `n_input_std_t` over the same rows, t N.
#' * `excess_share_of_inputs`: `exceedance_n_t / input_std_n_t`.
#' * `exceedance_share_of_positive_surplus`: `exceedance_n_t /
#'   positive_surplus_n_t`, `NA` when the denominator is not positive.
#' * `positive_surplus_n_t`, `exceeding_surplus_n_t`, `beyond_share`,
#'   `exceedance_share_of_positive_surplus`, `boundary_side`: see above.
#' * `ag_area_ha`: WHEP agricultural area, ha.
#' * `signed_denominator_nonpositive`: `positive_surplus_n_t` or
#'   `input_std_n_t` is zero or negative.
#' * `ratio_outside_unit`: `beyond_share`,
#'   `exceedance_share_of_positive_surplus` or `excess_share_of_inputs` lies
#'   outside `[0, 1]` (beyond a rounding tolerance of `1e-9`).
#' * `nourish`, `sjos_class` when `nourishment` is given.
#' * `negative_critical`, `land_use`, `grassland_split`,
#'   `regime_comparison`, `beyond_share_cut`: the run stamps.
#' * the polity columns below.
#'
#' `diagnostics`, one row per `year`, world level: `n_countries`,
#' `input_std_n_t` and `all_input_n_t` (inputs in the compared rows and in
#' every crop row of the land-use scope), `valid_input_fraction` (their
#' ratio), `exceedance_n_t` (sum over countries), `unallocated_exceedance_n_t`
#' (exceedance left on residual rows), `cell_exceedance_n_t` (summed cell
#' exceedance), `exceedance_gap_n_t` (`cell - country - unallocated`, zero up
#' to rounding), `positive_surplus_n_t`,
#' `exceedance_share_of_positive_surplus` and `excess_share_of_inputs` (world
#' ratios), `world_ratio_outside_unit` (either world ratio lies outside
#' `[0, 1]`, beyond the `1e-9` tolerance),
#' `overshoot_without_surplus_n_t` (overshoot of units with no positive
#' surplus, zero under the clamp), and the counts `n_undefined_beyond_share`,
#' `n_undefined_exceedance_share`, `n_undefined_excess_share`,
#' `n_signed_denominator_nonpositive`,
#' `n_ratio_outside_unit`, `n_missing_ag_area`, `n_undefined_attribution_rows`
#' (crop rows whose attribution is undefined), `n_unallocated_units` (units
#' whose overshoot sits on a residual record) and `n_unclassified` (`NA`
#' without `nourishment`).
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_n_boundary_country(example = TRUE)
build_n_boundary_country <- function(
  exceedance,
  surplus,
  ag_land,
  nourishment = NULL,
  beyond_share_cut = 0.5,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_n_boundary_country())
  }
  .nbc_check_cut(beyond_share_cut)
  surplus <- .nbc_collapse_regimes(surplus, exceedance)
  crop <- .nbc_crop_rows(exceedance, surplus)
  country <- .nbc_country(crop, ag_land, beyond_share_cut) |>
    .nbc_add_class(nourishment)
  diagnostics <- .nbc_diagnostics(exceedance, crop, country, surplus)
  list(
    country = .add_polity_columns_if_keyed(country),
    diagnostics = diagnostics
  )
}

# ---- Private helpers -------------------------------------------------------

.nbc_check_cut <- function(cut) {
  if (
    !is.numeric(cut) ||
      length(cut) != 1L ||
      !is.finite(cut) ||
      cut < 0 ||
      cut >= 1
  ) {
    cli::cli_abort(
      "{.arg beyond_share_cut} must be one number in [0, 1), not {.val {cut}}."
    )
  }
  invisible(cut)
}

.nbc_grid_cols <- function() {
  c(
    "cell_id",
    "lon",
    "lat",
    "area_code",
    "item_cbs_code",
    "year",
    "actual_n_t",
    "exceedance_n_t",
    "unallocated_positive_overshoot_n_t",
    "cell_actual_n_t",
    "cell_positive_overshoot_n_t",
    "managed_actual_n_t",
    "extensive_actual_n_t",
    "managed_positive_overshoot_n_t",
    "extensive_positive_overshoot_n_t",
    "boundary_component",
    "water_regime",
    "regime_actual_n_t",
    "regime_positive_overshoot_n_t",
    "coverage_state",
    "attribution_record_type",
    "attribution_status",
    "metric",
    "land_use",
    "grassland_split",
    "negative_critical",
    "regime_comparison"
  )
}

# The crop rows the country quantities are formed from, each with its input
# mass. Only valid cells carry attributed rows, and rows of a compared
# component excluded by the grassland split have already left the grid, so the
# rows joined here are exactly the rows the exceedance sums over.
.nbc_crop_rows <- function(exceedance, surplus) {
  .check_columns(exceedance, .nbc_grid_cols(), "exceedance")
  .check_columns(
    surplus,
    c("lon", "lat", "area_code", "item_cbs_code", "year", "n_input_std_t"),
    "surplus"
  )
  .nbc_check_stamps(exceedance)
  crop <- exceedance |>
    dplyr::filter(
      .data$coverage_state == "valid",
      .data$attribution_record_type == "crop_allocation"
    )
  if (nrow(crop) == 0L) {
    cli::cli_abort(
      c(
        "{.arg exceedance} has no crop rows in valid cells.",
        i = "Pass a {.fn build_n_boundary_exceedance} result at
             {.code resolution = \"grid\"}."
      ),
      class = "whep_nbc_no_crop_rows"
    )
  }
  if (anyNA(crop$area_code)) {
    cli::cli_abort(
      "{.arg exceedance} has crop rows with a missing {.field area_code}.",
      class = "whep_nbc_missing_area"
    )
  }
  .nbc_join_inputs(crop, surplus) |>
    .nbc_add_units()
}

# The comparison unit of a row: its cell, or under the grassland split its
# managed or extensive component, or under `regime_comparison = "separate"`
# the rainfed or irrigated part of either, with that unit's surplus and
# overshoot. The unit's state, not the crop's attributed exceedance, decides
# membership of the country's positive and exceeding surplus.
.nbc_add_units <- function(x) {
  part <- !is.na(x$water_regime)
  dplyr::mutate(
    x,
    unit_actual_n_t = dplyr::case_when(
      part ~ .data$regime_actual_n_t,
      is.na(.data$boundary_component) ~ .data$cell_actual_n_t,
      .default = .nbx_by_component(
        .data$boundary_component,
        .data$managed_actual_n_t,
        .data$extensive_actual_n_t
      )
    ),
    unit_positive_overshoot_n_t = dplyr::case_when(
      part ~ .data$regime_positive_overshoot_n_t,
      is.na(.data$boundary_component) ~ .data$cell_positive_overshoot_n_t,
      .default = .nbx_by_component(
        .data$boundary_component,
        .data$managed_positive_overshoot_n_t,
        .data$extensive_positive_overshoot_n_t
      )
    )
  )
}

# The country quantities are surplus-mode quantities, and each stamp has to be
# one value so that the table can carry it: a bound-together clamp and keep
# run would read as one series.
.nbc_check_stamps <- function(exceedance) {
  if (!all(exceedance$metric == "surplus")) {
    cli::cli_abort(
      c(
        "{.arg exceedance} must be a surplus-mode result.",
        i = "Found {.field metric} {.val {unique(exceedance$metric)}}."
      ),
      class = "whep_nbc_metric"
    )
  }
  purrr::walk(
    c("negative_critical", "land_use", "grassland_split", "regime_comparison"),
    \(col) {
      found <- unique(exceedance[[col]])
      if (length(found) != 1L) {
        cli::cli_abort(
          c(
            "{.arg exceedance} mixes runs: {.field {col}} is
             {.val {found}}.",
            i = "Summarise one run at a time."
          ),
          class = "whep_nbc_mixed_runs"
        )
      }
    }
  )
  invisible(exceedance)
}

# A balance split into rainfed and irrigated rows (build_nitrogen_balance()'s
# `methods$regime`, whep#1233) carries two surplus rows per grid, crop and year
# key. Under `regime_comparison = "netted"` build_n_boundary_exceedance() sums
# them back to one row before it compares a cell with its critical surplus, so
# the input read here is summed the same way; the split conserves
# `n_input_std_t`, so the sum is the unsplit input. Under "separate" the grid
# keeps one row per regime (whep#1345), so the inputs stay split and join
# regime by regime (.nbc_input_key()). Only the key and `n_input_std_t` are
# read from `surplus`. A key repeated within one regime is left for
# `.nbc_join_inputs()` to refuse rather than being summed away.
.nbc_collapse_regimes <- function(surplus, exceedance) {
  if (
    !rlang::has_name(surplus, "water_regime") ||
      .nbc_separate_regimes(exceedance)
  ) {
    return(surplus)
  }
  key <- .nbc_input_key()
  .check_columns(surplus, c(key, "n_input_std_t"), "surplus")
  per_regime <- dplyr::count(
    surplus,
    dplyr::across(dplyr::all_of(c(key, "water_regime")))
  )
  if (any(per_regime$n > 1L)) {
    return(surplus)
  }
  surplus |>
    dplyr::summarise(
      n_input_std_t = sum(.data$n_input_std_t),
      .by = dplyr::all_of(key)
    )
}

.nbc_input_key <- function(separate = FALSE) {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  if (separate) c(key, "water_regime") else key
}

.nbc_separate_regimes <- function(exceedance) {
  identical(unique(exceedance$regime_comparison), "separate")
}

# A crop row that finds no input row means `surplus` is not the table the grid
# was computed from; an input summed as zero there would understate the
# denominator without a trace, so it aborts.
.nbc_join_inputs <- function(crop, surplus) {
  key <- .nbc_input_key(.nbc_separate_regimes(crop))
  .check_columns(surplus, key, "surplus")
  inputs <- surplus |>
    dplyr::filter(
      .data$year %in% unique(crop$year),
      !is.na(.data$item_cbs_code)
    )
  duplicated_key <- nrow(dplyr::distinct(
    inputs,
    dplyr::across(dplyr::all_of(key))
  )) <
    nrow(inputs)
  if (duplicated_key) {
    cli::cli_abort(
      "{.arg surplus} has more than one row for a grid, crop and year key.",
      class = "whep_nbc_duplicate_surplus"
    )
  }
  joined <- dplyr::left_join(
    crop,
    dplyr::select(inputs, dplyr::all_of(key), "n_input_std_t"),
    by = key,
    relationship = "many-to-one"
  )
  if (anyNA(joined$n_input_std_t)) {
    cli::cli_abort(
      c(
        "{sum(is.na(joined$n_input_std_t))} crop row{?s} of
         {.arg exceedance} {?has/have} no {.field n_input_std_t} in
         {.arg surplus}.",
        i = "{.arg surplus} must be the {.fn calculate_n_surplus} output the
             grid was computed from."
      ),
      class = "whep_nbc_missing_input"
    )
  }
  joined
}

.nbc_country <- function(crop, ag_land, cut) {
  by <- c("area_code", "year")
  # The exceedance is summed by the package's own crop aggregation, so a
  # country's exceedance here is the sum of its resolution = "country" rows.
  exceedance <- .nbx_aggregate(crop, by) |>
    dplyr::select(
      dplyr::all_of(by),
      "exceedance_n_t",
      "negative_critical",
      "land_use",
      "grassland_split",
      "regime_comparison"
    )
  crop |>
    dplyr::summarise(
      input_std_n_t = sum(.data$n_input_std_t),
      positive_surplus_n_t = sum(.data$actual_n_t[.data$unit_actual_n_t > 0]),
      exceeding_surplus_n_t = sum(
        .data$actual_n_t[
          .data$unit_actual_n_t > 0 & .data$unit_positive_overshoot_n_t > 0
        ]
      ),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::left_join(exceedance, by = by, relationship = "one-to-one") |>
    .nbc_add_shares(cut) |>
    .nbc_add_ag_area(ag_land) |>
    dplyr::select(
      "year",
      "area_code",
      "exceedance_n_t",
      "input_std_n_t",
      "excess_share_of_inputs",
      "positive_surplus_n_t",
      "exceeding_surplus_n_t",
      "beyond_share",
      "exceedance_share_of_positive_surplus",
      "boundary_side",
      "ag_area_ha",
      "signed_denominator_nonpositive",
      "ratio_outside_unit",
      "negative_critical",
      "land_use",
      "grassland_split",
      "regime_comparison",
      "beyond_share_cut"
    ) |>
    dplyr::arrange(.data$year, .data$area_code)
}

# Shares are undefined, not zero, where the denominator is not positive. The
# unit-range test allows a rounding tolerance so that a country whose whole
# positive surplus lies in exceeding cells is not flagged for summing to
# 1 + 1e-16.
.nbc_add_shares <- function(x, cut) {
  dplyr::mutate(
    x,
    beyond_share = dplyr::if_else(
      .data$positive_surplus_n_t > 0,
      .data$exceeding_surplus_n_t / .data$positive_surplus_n_t,
      NA_real_
    ),
    exceedance_share_of_positive_surplus = dplyr::if_else(
      .data$positive_surplus_n_t > 0,
      .data$exceedance_n_t / .data$positive_surplus_n_t,
      NA_real_
    ),
    excess_share_of_inputs = dplyr::if_else(
      .data$input_std_n_t > 0,
      .data$exceedance_n_t / .data$input_std_n_t,
      NA_real_
    ),
    boundary_side = dplyr::case_when(
      is.na(.data$beyond_share) ~ NA_character_,
      .data$beyond_share > .env$cut ~ "Exceedance",
      .default = "Within_boundary"
    ),
    signed_denominator_nonpositive = .data$positive_surplus_n_t <= 0 |
      .data$input_std_n_t <= 0,
    ratio_outside_unit = .nbc_outside_unit(.data$beyond_share) |
      .nbc_outside_unit(.data$exceedance_share_of_positive_surplus) |
      .nbc_outside_unit(.data$excess_share_of_inputs),
    beyond_share_cut = .env$cut
  )
}

.nbc_outside_unit <- function(x) {
  tol <- 1e-9
  dplyr::coalesce(x < -tol | x > 1 + tol, FALSE)
}

# WHEP agricultural area per country-year. A country-year with no land row
# stays NA rather than zero, and a table that matches no country-year at all
# is not a land table for this run.
.nbc_add_ag_area <- function(x, ag_land) {
  .check_columns(ag_land, c("area_code", "year", "area_ha"), "ag_land")
  check_inputs_supplied(
    ag_land,
    c("agricultural land area" = "area_ha"),
    details = c(i = "Build it with {.fn build_ag_land_support}.")
  )
  area <- ag_land |>
    dplyr::filter(.data$year %in% unique(x$year)) |>
    dplyr::summarise(
      ag_area_ha = sum(.data$area_ha),
      .by = c("area_code", "year")
    )
  out <- dplyr::left_join(
    x,
    area,
    by = c("area_code", "year"),
    relationship = "one-to-one"
  )
  if (all(is.na(out$ag_area_ha))) {
    cli::cli_abort(
      "{.arg ag_land} has no row for any country-year of {.arg exceedance}.",
      class = "whep_nbc_no_ag_land"
    )
  }
  out
}

.nbc_add_class <- function(country, nourishment) {
  if (is.null(nourishment)) {
    return(country)
  }
  .check_columns(nourishment, c("year", "area_code", "nourish"), "nourishment")
  if (
    dplyr::n_distinct(nourishment$year, nourishment$area_code) <
      nrow(nourishment)
  ) {
    cli::cli_abort(
      "{.arg nourishment} must have one row per {.field year} and
       {.field area_code}.",
      class = "whep_nbc_duplicate_nourishment"
    )
  }
  country |>
    dplyr::left_join(
      dplyr::select(nourishment, "year", "area_code", "nourish"),
      by = c("year", "area_code"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      # The same levels as classify_sjos_n(); a missing side or class gives
      # a label outside them, hence NA.
      sjos_class = factor(
        paste(.data$boundary_side, .data$nourish),
        levels = whep::sjos_levels$level
      )
    )
}

# World-level, per-year record of what the country table does and does not
# cover, and the reconciliation of its exceedance with the cell result.
.nbc_diagnostics <- function(exceedance, crop, country, surplus) {
  valid <- dplyr::filter(exceedance, .data$coverage_state == "valid")
  cells <- valid |>
    dplyr::distinct(.data$cell_id, .data$year, .keep_all = TRUE) |>
    dplyr::summarise(
      cell_exceedance_n_t = sum(.data$cell_positive_overshoot_n_t),
      .by = "year"
    )
  .nbc_world(country) |>
    dplyr::left_join(cells, by = "year", relationship = "one-to-one") |>
    dplyr::left_join(
      .nbc_residual(valid, crop),
      by = "year",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      .nbc_all_input(surplus, unique(crop$year), unique(crop$land_use)),
      by = "year",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      valid_input_fraction = dplyr::if_else(
        .data$all_input_n_t > 0,
        .data$input_std_n_t / .data$all_input_n_t,
        NA_real_
      ),
      exceedance_gap_n_t = .data$cell_exceedance_n_t -
        .data$exceedance_n_t -
        .data$unallocated_exceedance_n_t
    ) |>
    .nbc_assert_reconciled() |>
    dplyr::select(
      "year",
      "n_countries",
      "input_std_n_t",
      "all_input_n_t",
      "valid_input_fraction",
      "exceedance_n_t",
      "unallocated_exceedance_n_t",
      "cell_exceedance_n_t",
      "exceedance_gap_n_t",
      "positive_surplus_n_t",
      "exceedance_share_of_positive_surplus",
      "excess_share_of_inputs",
      "world_ratio_outside_unit",
      "overshoot_without_surplus_n_t",
      dplyr::starts_with("n_")
    ) |>
    dplyr::arrange(.data$year)
}

# What the attribution leaves out of the country rows, per year: the
# exceedance on residual records and the units carrying it, the crop rows whose
# shares are undefined, and the overshoot of units with no positive surplus
# (the units that are outside both the positive and the exceeding surplus;
# zero once negative critical surpluses are clamped).
.nbc_residual <- function(valid, crop) {
  units <- valid |>
    .nbc_add_units() |>
    dplyr::distinct(
      .data$cell_id,
      .data$year,
      .data$boundary_component,
      .data$water_regime,
      .keep_all = TRUE
    ) |>
    dplyr::summarise(
      overshoot_without_surplus_n_t = sum(
        .data$unit_positive_overshoot_n_t[.data$unit_actual_n_t <= 0]
      ),
      .by = "year"
    )
  valid |>
    dplyr::summarise(
      unallocated_exceedance_n_t = sum(
        .data$unallocated_positive_overshoot_n_t
      ),
      n_unallocated_units = sum(
        .data$attribution_record_type == "cell_residual" &
          .data$unallocated_positive_overshoot_n_t > 0
      ),
      .by = "year"
    ) |>
    dplyr::left_join(units, by = "year", relationship = "one-to-one") |>
    dplyr::left_join(
      dplyr::summarise(
        crop,
        n_undefined_attribution_rows = sum(
          .data$attribution_status != "defined"
        ),
        .by = "year"
      ),
      by = "year",
      relationship = "one-to-one"
    )
}

.nbc_world <- function(country) {
  country |>
    dplyr::summarise(
      n_countries = dplyr::n(),
      input_std_n_t = sum(.data$input_std_n_t),
      exceedance_n_t = sum(.data$exceedance_n_t),
      positive_surplus_n_t = sum(.data$positive_surplus_n_t),
      n_undefined_beyond_share = sum(is.na(.data$beyond_share)),
      n_undefined_exceedance_share = sum(
        is.na(.data$exceedance_share_of_positive_surplus)
      ),
      n_undefined_excess_share = sum(is.na(.data$excess_share_of_inputs)),
      n_signed_denominator_nonpositive = sum(
        .data$signed_denominator_nonpositive
      ),
      n_ratio_outside_unit = sum(.data$ratio_outside_unit),
      n_missing_ag_area = sum(is.na(.data$ag_area_ha)),
      .by = "year"
    ) |>
    dplyr::mutate(
      exceedance_share_of_positive_surplus = dplyr::if_else(
        .data$positive_surplus_n_t > 0,
        .data$exceedance_n_t / .data$positive_surplus_n_t,
        NA_real_
      ),
      excess_share_of_inputs = dplyr::if_else(
        .data$input_std_n_t > 0,
        .data$exceedance_n_t / .data$input_std_n_t,
        NA_real_
      ),
      world_ratio_outside_unit = .nbc_outside_unit(
        .data$exceedance_share_of_positive_surplus
      ) |
        .nbc_outside_unit(.data$excess_share_of_inputs)
    ) |>
    dplyr::left_join(
      .nbc_unclassified(country),
      by = "year",
      relationship = "one-to-one"
    )
}

# Country-years the joint classification leaves unclassified (no boundary side
# or no nourishment class). Without a nourishment table nothing was classified,
# so the count is NA rather than every row.
.nbc_unclassified <- function(country) {
  if (!rlang::has_name(country, "sjos_class")) {
    return(
      dplyr::distinct(country, .data$year) |>
        dplyr::mutate(n_unclassified = NA_integer_)
    )
  }
  dplyr::summarise(
    country,
    n_unclassified = sum(is.na(.data$sjos_class)),
    .by = "year"
  )
}

# Every crop row of the land-use scope in the run's years, whether or not its
# cell was compared: the denominator of the valid-cell input fraction. Rows
# naming no crop were removed before the comparison and are not inputs here.
.nbc_all_input <- function(surplus, years, land_use) {
  grass <- .nbx_grass_codes()
  surplus |>
    dplyr::filter(.data$year %in% .env$years, !is.na(.data$item_cbs_code)) |>
    dplyr::filter(
      switch(
        .env$land_use,
        ara = !.data$item_cbs_code %in% .env$grass,
        igl = .data$item_cbs_code %in% .env$grass,
        all = TRUE
      )
    ) |>
    dplyr::summarise(all_input_n_t = sum(.data$n_input_std_t), .by = "year")
}

# The crop attribution reconciles to each cell inside
# build_n_boundary_exceedance(); this repeats the identity on the table the
# country result is read from, so a grid that was filtered or edited after that
# check cannot pass unnoticed.
.nbc_assert_reconciled <- function(x, tolerance = 1e-8) {
  bad <- abs(x$exceedance_gap_n_t) >
    tolerance * pmax(1, abs(x$cell_exceedance_n_t))
  if (any(bad)) {
    cli::cli_abort(
      c(
        "Country exceedance does not reconcile to the cell exceedance.",
        x = "{cli::qty(sum(bad))}Year{?s} {.val {x$year[bad]}}: gap
             {.val {x$exceedance_gap_n_t[bad]}} t N.",
        i = "Country sums plus the unallocated residual must equal the summed
             cell exceedance; pass the complete grid result."
      ),
      class = "whep_nbc_unreconciled"
    )
  }
  x
}
