# Joint origin of GRAFS processing feedstock (#1014, PR #1591).
#
# A processing target (the CBS `value_to_process` of a primary item) can be
# met from post-seed domestic production or from gross imports. No source
# observes which tonnes were crushed, so the split is an explicit assumption.
#
# Default, "proportional" (maintainer decision on PR #1591, 2026-11-05):
# domestic and imported feedstock are equally likely to be processed, so the
# target is shared over post-seed domestic production plus gross imports in
# proportion to each pool. This is the denominator of afsetools
# (eduaguilera/afsetools@0da7ff712d551a7b299cbf63b751d1ff7a7a9b83,
# R/calculate_footprints.R, lines 320-333) and the homogeneous-mixing
# assumption of FABIO (Bruckner et al. 2019, Environ. Sci. Technol. 53,
# 11302-11312, doi:10.1021/acs.est.9b03554). Neither observes origins.
#
# "domestic_first" (the booking before this PR) and "imports_first" are the
# two bounding orders, kept selectable for sensitivity analysis only.
#
# Net imports are not a cap: exports do not say which tonnes were re-exported.
# Stock withdrawals and balancing residuals are not origin pools; a target
# the two pools cannot cover is reported, never booked to a third origin
# (#1625).

#' @title Allocate processing targets over physical origin pools -------------
#' @description Splits each `Year`/`Item` target over its explicitly supplied
#' `domestic` and `import` pools. Both pools must be supplied, zeros
#' included: a missing pool is not a zero pool.
#'
#' @param availability Tibble with Year, Item, origin (`"domestic"` or
#'   `"import"`), available (tonnes FM) and source (provenance label).
#' @param targets Tibble with Year, Item and requested (tonnes FM).
#' @param method One of `"proportional"`, `"domestic_first"`,
#'   `"imports_first"`.
#'
#' @return `availability` joined with requested, total_available, the
#'   feasible bounds origin_lower and origin_upper, allocated and
#'   method_processing_origin.
#' @keywords internal
#' @noRd
.allocate_processing_origins <- function(
  availability,
  targets,
  method = c("proportional", "domestic_first", "imports_first")
) {
  method <- rlang::arg_match(method)
  .check_proc_origin_inputs(availability, targets)
  .check_processing_origin_pools(availability, targets)
  keys <- c("Year", "Item")
  requested <- targets |>
    dplyr::left_join(
      availability |>
        dplyr::summarise(
          total_available = sum(.data$available),
          .by = dplyr::all_of(keys)
        ),
      by = keys
    )
  .check_processing_origin_cover(requested)
  availability |>
    dplyr::inner_join(requested, by = keys) |>
    dplyr::mutate(
      origin_lower = pmax(
        0,
        .data$requested - (.data$total_available - .data$available)
      ),
      origin_upper = pmin(.data$requested, .data$available)
    ) |>
    .split_processing_origins(method) |>
    dplyr::mutate(method_processing_origin = method)
}

#' @title Processing origin ledger for the GRAFS builder ----------------------
#' @description Splits each processing target of a Cropland item Spain
#' grows over post-seed domestic production and gross imports. The part the
#' two pools cannot cover is reported as excess, not booked. Inputs Spain
#' does not grow (second-stage inputs such as Molasses or Wine) keep the
#' domestic-only booking: no domestic share and their whole target as
#' excess, as with `import_processing = "product"`.
#'
#' @param targets Tibble with Year, Item and value_to_process.
#' @param national_production Output of `.national_item_production()`.
#' @param imports Output of `.spain_gross_imports()`.
#' @param cropland_items Items with Cropland production.
#' @param method See `.allocate_processing_origins()`.
#'
#' @return A list of `shares` (Year, Item, share_processing of domestic
#'   production), `import_fed` (Year, Item, import_fed_fm, positive rows
#'   only) and `excess` (as `.calculate_processing_excess()`, excess_fm
#'   being the target neither pool covers).
#' @keywords internal
#' @noRd
.processing_origin_ledger <- function(
  targets,
  national_production,
  imports,
  cropland_items,
  method = "proportional"
) {
  base <- targets |>
    dplyr::left_join(national_production, by = c("Year", "Item")) |>
    dplyr::mutate(
      # Structural zero: an item absent from production is not grown.
      national_production_fm = dplyr::coalesce(.data$national_production_fm, 0),
      eligible = .data$Item %in%
        cropland_items &
        .data$national_production_fm > 0
    )
  allocated <- .allocate_ledger_origins(
    dplyr::filter(base, .data$eligible),
    imports,
    method
  )
  ledger <- base |>
    dplyr::left_join(allocated, by = c("Year", "Item")) |>
    dplyr::mutate(
      domestic_fm = dplyr::if_else(
        .data$eligible,
        .data$domestic_fm,
        pmin(.data$value_to_process, .data$national_production_fm)
      ),
      import_fed_fm = dplyr::if_else(.data$eligible, .data$import_fed_fm, 0),
      share_processing = dplyr::if_else(
        .data$national_production_fm > 0,
        .data$domestic_fm / .data$national_production_fm,
        0
      ),
      # The target neither pool covers; exact zero when the pools cover it.
      excess_fm = pmax(
        .data$value_to_process -
          dplyr::if_else(
            .data$eligible,
            .data$supply_fm,
            .data$national_production_fm
          ),
        0
      )
    )
  list(
    shares = dplyr::select(ledger, "Year", "Item", "share_processing"),
    import_fed = ledger |>
      dplyr::filter(.data$import_fed_fm > 0) |>
      dplyr::select("Year", "Item", "import_fed_fm"),
    excess = dplyr::select(
      ledger,
      "Year",
      "Item",
      "value_to_process",
      "national_production_fm",
      "excess_fm"
    )
  )
}

#' @title Spain's gross imports per CBS item (#1014) --------------------------
#' @description Gross, not net: re-exports do not identify which tonnes
#' were processed, so every imported tonne is in the pool.
#'
#' @param wide_cbs Output of `get_wide_cbs()`.
#'
#' @return A dataframe with Year, Item and import_fm (tonnes FM).
#' @keywords internal
#' @noRd
.spain_gross_imports <- function(wide_cbs) {
  wide_cbs |>
    dplyr::filter(.data$area_code == .spain_area_code) |>
    add_item_cbs_name(code_column = "item_cbs_code", name_column = "Item") |>
    dplyr::transmute(
      Year = .data$year,
      Item = .data$Item,
      import_fm = .data$import
    )
}

.allocate_ledger_origins <- function(eligible, imports, method) {
  pools <- dplyr::bind_rows(
    eligible |>
      dplyr::transmute(
        Year = .data$Year,
        Item = .data$Item,
        origin = "domestic",
        available = .data$national_production_fm,
        source = "post_seed_production"
      ),
    eligible |>
      dplyr::select("Year", "Item") |>
      dplyr::inner_join(imports, by = c("Year", "Item")) |>
      dplyr::transmute(
        Year = .data$Year,
        Item = .data$Item,
        origin = "import",
        available = .data$import_fm,
        source = "cbs_gross_import"
      )
  )
  supply <- pools |>
    dplyr::summarise(supply_fm = sum(.data$available), .by = c("Year", "Item"))
  targets <- eligible |>
    dplyr::left_join(supply, by = c("Year", "Item")) |>
    dplyr::transmute(
      Year = .data$Year,
      Item = .data$Item,
      # The feasible part; the remainder is reported as excess (#1625).
      requested = pmin(.data$value_to_process, .data$supply_fm)
    )
  .allocate_processing_origins(pools, targets, method) |>
    dplyr::select("Year", "Item", "origin", "allocated") |>
    tidyr::pivot_wider(names_from = "origin", values_from = "allocated") |>
    dplyr::left_join(supply, by = c("Year", "Item")) |>
    dplyr::select(
      "Year",
      "Item",
      domestic_fm = "domestic",
      import_fed_fm = "import",
      "supply_fm"
    )
}

.check_proc_origin_inputs <- function(availability, targets) {
  keys <- c("Year", "Item")
  if (
    !all(rlang::has_name(
      availability,
      c(keys, "origin", "available", "source")
    )) ||
      !all(rlang::has_name(targets, c(keys, "requested")))
  ) {
    cli::cli_abort(
      "Supply physical origin quantities, provenance and keyed targets.",
      class = "whep_processing_origin_invalid"
    )
  }
  if (
    anyNA(availability[c(keys, "origin", "source")]) ||
      anyNA(targets[keys]) ||
      any(!availability$origin %in% c("domestic", "import")) ||
      any(!nzchar(availability$source)) ||
      any(!is.finite(availability$available) | availability$available < 0) ||
      any(!is.finite(targets$requested) | targets$requested < 0)
  ) {
    cli::cli_abort(
      "Processing origins require known nonnegative mass and provenance.",
      class = "whep_processing_origin_invalid"
    )
  }
  if (
    anyDuplicated(availability[c(keys, "origin")]) ||
      anyDuplicated(targets[keys])
  ) {
    cli::cli_abort(
      "Processing origin and target keys must be unique.",
      class = "whep_processing_origin_duplicate"
    )
  }
}

# A zero must be supplied explicitly; a missing input is not a zero pool.
.check_processing_origin_pools <- function(availability, targets) {
  keys <- c("Year", "Item")
  expected <- dplyr::cross_join(
    dplyr::select(targets, dplyr::all_of(keys)),
    tibble::tibble(origin = c("domestic", "import"))
  )
  missing <- dplyr::anti_join(expected, availability, by = c(keys, "origin"))
  extra <- dplyr::anti_join(availability, expected, by = c(keys, "origin"))
  if (nrow(missing) > 0 || nrow(extra) > 0) {
    cli::cli_abort(
      c(
        "Supply both origin pools for every processing target.",
        i = "Missing: {nrow(missing)} pool{?s}; without a target: \\
             {nrow(extra)}."
      ),
      class = "whep_processing_origin_missing",
      unresolved = missing,
      outside_scope = extra
    )
  }
}

.check_processing_origin_cover <- function(requested) {
  # Scale-aware floating-point allowance, not an estimation tolerance or cap.
  unresolved <- requested |>
    dplyr::mutate(shortfall = .data$requested - .data$total_available) |>
    dplyr::filter(
      .data$shortfall >
        sqrt(.Machine$double.eps) *
          pmax(1, .data$requested, .data$total_available)
    )
  if (nrow(unresolved) > 0) {
    cli::cli_abort(
      "Requested processing exceeds supplied physical origin quantities.",
      class = "whep_processing_origin_infeasible",
      unresolved = unresolved
    )
  }
}

.split_processing_origins <- function(pools, method) {
  if (method == "proportional") {
    return(
      pools |>
        dplyr::mutate(
          allocated = dplyr::if_else(
            .data$total_available > 0,
            .data$requested * .data$available / .data$total_available,
            0
          )
        )
    )
  }
  first <- if (method == "domestic_first") "domestic" else "import"
  pools |>
    dplyr::mutate(
      allocated = dplyr::if_else(
        .data$origin == first,
        .data$origin_upper,
        .data$origin_lower
      )
    )
}
