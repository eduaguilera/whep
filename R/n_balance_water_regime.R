# Rainfed/irrigated split of the nitrogen balance rows (whep#1233).
#
# build_nitrogen_balance() keys its rows on the crop and the cell only, so a
# crop's rainfed and irrigated land in one cell share one row and one set of
# loss drivers. build_gridded_landuse() already allocates every crop's
# harvested area into `rainfed_ha` and `irrigated_ha` per cell and year, and
# the carbon chain splits on exactly that (.ci_spatialized_regime_share(),
# R/carbon_inputs.R). This file reuses that one regime layer for the nitrogen
# rows.
#
# WHAT HAS A REGIME BASIS, AND WHAT DOES NOT. The only regime-resolved
# quantity any input carries is the crop's harvested AREA. No nitrogen term
# is regime-resolved upstream: synthetic N is spread over crops by harvested
# area and Coello rates, manure by the crop layer, deposition and urban N by
# land support, and the NPP chain (production, residues, BNF, recycling) by
# harvested-area weights with no `water_regime` (inst/scripts/
# run_nitrogen_balance.R, .nbd_grid_npp()). So:
#
# - `method = "area_share"` splits every extensive term of a crop row in
#   proportion to the crop's irrigated share of its harvested area in the
#   cell. That is an allocation ASSUMPTION for the nitrogen terms (equal
#   per-hectare rates under both regimes), not a measurement; its
#   alternatives (e.g. higher synthetic rates on irrigated land) are
#   whep#1233's open question and need a sourced rate ratio this package does
#   not hold. What the split does make possible is attaching regime-specific
#   loss DRIVERS (`n_balance_drivers` / `n_balance_leaching_drivers` keyed on
#   `water_regime`), which is where the regime changes the losses.
# - Rows with no crop regime basis stay whole, with `water_regime = NA` and
#   `method_water_regime = "unsplit_no_basis"`: grassland (item_cbs_code
#   3000/3002/3003, which build_gridded_landuse() does not allocate), the
#   `item_cbs_code = NA` rows (at grid resolution only the carbon-balance SOM
#   sequestration output: grid inputs must carry an item, so non-item
#   deposition and urban N already sit on crop rows and are split with them),
#   and any crop cell-year the supplied layer has no positive area for. None of them is booked as
#   rainfed: that would be a zero irrigated share invented for rows nobody
#   measured.
#
# Totals conserve by construction (share + (1 - share) = 1), so the checks
# that CAN fail are the input ones: the layer must be supplied and non-empty,
# and the fraction of crop nitrogen it actually reached is reported.

# The regime share of each balance-key crop row, from the build_gridded_
# landuse() area layer. Several item_prod_codes map to one item_cbs_code, so
# the share is formed from summed hectares, never averaged across crops.
.nb_regime_shares <- function(method, data, key) {
  if (identical(method, "none")) {
    return(NULL)
  }
  layer <- data$crop_regime_area
  if (is.null(layer) || nrow(layer) == 0L) {
    .nb_abort_no_regime_area()
  }
  .check_columns(
    layer,
    c("area_code", "item_prod_code", "year", "rainfed_ha", "irrigated_ha"),
    "crop_regime_area"
  )
  shares <- layer |>
    dplyr::mutate(
      item_cbs_code = .ni_item_cbs_from_prod(.data$item_prod_code),
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year)
    ) |>
    .nb_round_coords() |>
    dplyr::summarise(
      # total first: a summarise column shadows its own input for every
      # later expression, so summing irrigated_ha first would add the pooled
      # irrigated area to each row's rainfed area.
      total_ha = sum(.data$rainfed_ha + .data$irrigated_ha, na.rm = TRUE),
      irrigated_ha = sum(.data$irrigated_ha, na.rm = TRUE),
      .by = dplyr::all_of(key)
    ) |>
    dplyr::filter(!is.na(.data$item_cbs_code), .data$total_ha > 0) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(key)),
      irrigated_share = pmin(pmax(.data$irrigated_ha / .data$total_ha, 0), 1)
    )
  # A layer whose every row is zero or NA hectares (or an unmapped crop) is an
  # absent input, not an all-rainfed world: refuse it rather than leave every
  # row unsplit behind totals that still conserve.
  if (nrow(shares) == 0L) {
    .nb_abort_no_regime_area()
  }
  shares
}

.nb_abort_no_regime_area <- function() {
  cli::cli_abort(
    c(
      "{.code methods$water_regime = \"area_share\"} needs
         {.field data$crop_regime_area}.",
      i = "Supply the rainfed/irrigated crop areas of
             [build_gridded_landuse()] ({.field lon}, {.field lat},
             {.field area_code}, {.field item_prod_code}, {.field year},
             {.field rainfed_ha}, {.field irrigated_ha}), keyed on the same
             {.field area_code} vocabulary as the nitrogen inputs."
    ),
    class = c("whep_missing_regime_area", "whep_absent_input")
  )
}

# The balance key once the regime split is in force.
.nb_regime_key <- function(key, shares) {
  if (is.null(shares)) key else c(key, "water_regime")
}

# The key a caller's driver table joins on. A driver table keyed on
# `water_regime` gives each regime its own drivers; one without it is
# regime-invariant and reaches both parts of a split row. Only
# `water_regime` may be absent: every other key column is still required, so a
# table missing `lon` fails the join rather than attaching at a coarser grain.
.nb_driver_key <- function(key, drivers) {
  if (rlang::has_name(drivers, "water_regime")) {
    return(key)
  }
  setdiff(key, "water_regime")
}

# Split every row of `x` that has a regime share into a rainfed and an
# irrigated row, scaling each numeric non-key column by the regime's share.
# Parts with zero share are dropped (a fully rainfed crop yields one row).
# Rows without a share are returned whole with `water_regime = NA`.
.nb_split_regime <- function(x, shares, key) {
  if (is.null(shares)) {
    return(x)
  }
  extensive <- setdiff(names(x)[vapply(x, is.numeric, TRUE)], key)
  joined <- x |>
    .nb_round_coords() |>
    dplyr::left_join(shares, by = key, relationship = "many-to-one")
  unsplit <- joined |>
    dplyr::filter(is.na(.data$irrigated_share)) |>
    dplyr::mutate(
      water_regime = NA_character_,
      method_water_regime = "unsplit_no_basis"
    )
  split <- joined |>
    dplyr::filter(!is.na(.data$irrigated_share)) |>
    # expand_grid(), not crossing(): crossing() de-duplicates its inputs, and
    # build_n_inputs() can emit identical rows for one key and fert_type, so
    # it silently dropped nitrogen before the loss cascade.
    tidyr::expand_grid(water_regime = c("rainfed", "irrigated")) |>
    dplyr::mutate(
      regime_share = dplyr::if_else(
        .data$water_regime == "irrigated",
        .data$irrigated_share,
        1 - .data$irrigated_share
      )
    ) |>
    dplyr::filter(.data$regime_share > 0) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(extensive), \(v) v * .data$regime_share),
      method_water_regime = "area_share"
    ) |>
    dplyr::select(-"regime_share")
  dplyr::bind_rows(split, unsplit) |>
    dplyr::select(-"irrigated_share")
}

# Coordinates are matched after rounding to 0.01 degree, as the carbon chain
# does for the same layer (.ci_split_into_groups()), so a float-formatting
# difference between two grids cannot leave a cell unsplit.
.nb_round_coords <- function(x) {
  if (!rlang::has_name(x, "lon")) {
    return(x)
  }
  dplyr::mutate(x, lon = round(.data$lon, 2), lat = round(.data$lat, 2))
}

# How much crop nitrogen the regime layer reached. A layer keyed on another
# area vocabulary, or on other years, matches nothing and would leave every
# row unsplit behind totals that still conserve; say so instead.
.nb_report_regime_cover <- function(x) {
  if (!rlang::has_name(x, "water_regime")) {
    return(invisible(x))
  }
  crop <- !is.na(x$item_cbs_code) &
    !x$item_cbs_code %in% c(3000L, 3002L, 3003L)
  total <- sum(x$n_input_full_t[crop], na.rm = TRUE)
  reached <- sum(
    x$n_input_full_t[crop & !is.na(x$water_regime)],
    na.rm = TRUE
  )
  share <- if (total > 0) reached / total else NA_real_
  if (!is.na(share) && share == 0) {
    cli::cli_warn(
      c(
        "The crop regime layer matched no crop nitrogen row.",
        i = "Check that {.field data$crop_regime_area} covers the balance
             years and uses the same {.field area_code} vocabulary."
      ),
      class = "whep_regime_area_unmatched"
    )
  } else if (!is.na(share)) {
    cli::cli_inform(c(
      i = "Rainfed/irrigated split reached {signif(100 * share, 4)}% of crop
           nitrogen input; the rest stays unsplit."
    ))
  }
  invisible(x)
}
