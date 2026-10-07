# Rainfed/irrigated comparison units of the critical-N boundary (issue #1345).
#
# build_nitrogen_balance() splits every grid row into a rainfed and an
# irrigated part (#1233). Under `regime_comparison = "netted"` the boundary
# sums the two back before a cell meets its allowance, so an irrigated excess
# is offset by rainfed headroom in the same cell. Under "separate" each regime
# is its own comparison part inside the cell, or inside the managed or
# extensive component under the grassland split (#1285):
#   part allowance = unit allowance * part area / unit area,
# where the unit is the cell or the component, and the part area is the
# summed `area_ha` of its rows. The deposited critical surface is one rate per
# hectare of a cell, with no regime axis, so sharing it by area keeps that
# rate equal on rainfed and irrigated land and keeps every unit allowance
# exactly what the netted comparison uses. Only the netting moves:
#   unit overshoot = sum over parts of max(part actual - part allowance, 0),
# never less than max(unit actual - unit allowance, 0). The unit's actual,
# allowance and signed margin are unchanged.

.nbx_regimes <- function() {
  c("rainfed", "irrigated")
}

.nbx_regime_output_cols <- function() {
  c(
    "rainfed_actual_n_t",
    "rainfed_critical_n_t",
    "rainfed_positive_overshoot_n_t",
    "irrigated_actual_n_t",
    "irrigated_critical_n_t",
    "irrigated_positive_overshoot_n_t"
  )
}

# "separate" needs rows that carry the regime. A balance built with
# `methods$regime = "none"`, or at polity resolution, has nothing to separate:
# that is an input the call did not get, so it aborts rather than quietly
# returning the netted result under the "separate" stamp.
.nbx_check_regime_rows <- function(rows, regime_comparison) {
  if (regime_comparison == "netted") {
    return(invisible(TRUE))
  }
  if (!rlang::has_name(rows, "water_regime")) {
    cli::cli_abort(
      c(
        "{.arg regime_comparison = \"separate\"} needs a {.field water_regime}
         column in {.arg surplus}.",
        i = "Build the balance at grid resolution with the rainfed/irrigated
             split ({.fn build_nitrogen_balance}, {.code methods$regime}), or
             choose {.arg regime_comparison = \"netted\"}."
      ),
      class = "whep_nbx_regime_missing"
    )
  }
  bad <- !rows$water_regime %in% .nbx_regimes()
  if (any(bad)) {
    found <- unique(rows$water_regime[bad])
    cli::cli_abort(
      c(
        "{sum(bad)} actual-pressure row{?s} carr{?ies/y} a
         {.field water_regime} outside {.val {(.nbx_regimes())}}.",
        x = "Found {.val {as.character(found)}}."
      ),
      class = "whep_nbx_regime_bad"
    )
  }
  invisible(TRUE)
}

# The uncollapsed regime rows, with the grassland-split component of their
# cell-crop row: the component depends on the cell and the crop, never on the
# regime, so both regime rows of a crop fall in the same component.
.nbx_regime_rows <- function(rows, actual) {
  if (!rlang::has_name(actual, "boundary_component")) {
    return(dplyr::mutate(rows, boundary_component = NA_character_))
  }
  key <- c("cell_id", "area_code", "item_cbs_code", "year")
  dplyr::left_join(
    rows,
    dplyr::distinct(
      actual,
      dplyr::across(dplyr::all_of(c(key, "boundary_component")))
    ),
    by = key,
    relationship = "many-to-one"
  )
}

# One row per compared unit and regime: the part's actual pressure, area,
# allowance, signed margin and positive overshoot. Rows of a component the
# comparison leaves out (no allowance area, no rate, unrated intensive
# grassland) are excluded here exactly as they are from the netted unit.
.nbx_regime_parts <- function(rows, cells, split) {
  unit_key <- c("cell_id", "year", "boundary_component")
  rows |>
    dplyr::summarise(
      part_actual_n_t = sum(.data$actual_n_t),
      part_absolute_n_t = sum(abs(.data$actual_n_t)),
      part_area_ha = sum(.data$area_ha),
      .by = dplyr::all_of(c(unit_key, "water_regime"))
    ) |>
    dplyr::inner_join(
      .nbx_regime_units(cells, split),
      by = unit_key,
      relationship = "many-to-one"
    ) |>
    .nbx_check_part_areas() |>
    dplyr::mutate(
      unit_area_ha = sum(.data$part_area_ha),
      n_pressured = sum(.data$part_actual_n_t != 0),
      part_share = .nbx_part_share(
        .data$part_area_ha,
        .data$unit_area_ha,
        .data$part_actual_n_t,
        .data$water_regime
      ),
      .by = dplyr::all_of(unit_key)
    ) |>
    .nbx_check_zero_area_units() |>
    dplyr::mutate(
      part_critical_n_t = .data$unit_critical_n_t * .data$part_share,
      part_signed_margin_n_t = .data$part_actual_n_t - .data$part_critical_n_t,
      part_positive_overshoot_n_t = pmax(.data$part_signed_margin_n_t, 0),
      part_condition_ratio = .nbx_condition_ratio(
        .data$part_actual_n_t,
        .data$part_absolute_n_t
      )
    ) |>
    dplyr::select(-"n_pressured", -"unit_area_ha", -"part_absolute_n_t")
}

# The allowance of every compared unit: the cell's without the grassland
# split, else each compared component's.
.nbx_regime_units <- function(cells, split) {
  valid <- dplyr::filter(cells, .data$coverage_state == "valid")
  if (!split) {
    return(dplyr::transmute(
      valid,
      .data$cell_id,
      .data$year,
      boundary_component = NA_character_,
      unit_critical_n_t = .data$cell_critical_n_t
    ))
  }
  purrr::map(c("managed", "extensive"), \(component) {
    state <- valid[[paste0(component, "_coverage_state")]]
    valid[state %in% .nbx_compared_states(), ] |>
      dplyr::transmute(
        .data$cell_id,
        .data$year,
        boundary_component = component,
        unit_critical_n_t = .data[[paste0(component, "_critical_n_t")]]
      )
  }) |>
    dplyr::bind_rows()
}

# A part's area is its share of the allowance, so a missing or negative area
# in a compared unit has no defensible reading.
.nbx_check_part_areas <- function(parts) {
  bad <- !is.finite(parts$part_area_ha) | parts$part_area_ha < 0
  if (any(bad)) {
    cells <- unique(parts$cell_id[bad])
    cli::cli_abort(
      c(
        "{length(cells)} compared cell{?s} carr{?ies/y} a missing or negative
         {.field area_ha}, so the allowance cannot be shared between the
         rainfed and the irrigated part.",
        i = "First {cli::qty(min(length(cells), 5L))}cell{?s}:
             {.val {utils::head(cells, 5L)}}."
      ),
      class = "whep_nbx_regime_area"
    )
  }
  parts
}

# The area share of each part. A unit whose rows hold no area at all (a cell
# where WHEP books pressure but no harvested hectares) has no area to share
# the allowance by. If one part at most carries pressure the share is not a
# choice: the whole allowance goes to that part (to the rainfed part when none
# does), which is exactly the netted comparison. Two pressured parts without
# area are refused by .nbx_check_zero_area_units().
.nbx_part_share <- function(area, unit_area, actual, regime) {
  if (unit_area[[1L]] > 0) {
    return(area / unit_area)
  }
  pressured <- actual != 0
  if (!any(pressured)) {
    pressured <- regime == "rainfed"
    if (!any(pressured)) {
      pressured <- seq_along(regime) == 1L
    }
  }
  as.numeric(pressured)
}

.nbx_check_zero_area_units <- function(parts) {
  bad <- parts$unit_area_ha == 0 & parts$n_pressured > 1L
  if (any(bad)) {
    cells <- unique(parts$cell_id[bad])
    cli::cli_abort(
      c(
        "{length(cells)} compared cell{?s} carr{?ies/y} rainfed and irrigated
         pressure on no harvested area.",
        x = "Without area there is no basis to share the allowance between
             the two parts.",
        i = "First {cli::qty(min(length(cells), 5L))}cell{?s}:
             {.val {utils::head(cells, 5L)}}."
      ),
      class = "whep_nbx_regime_no_area"
    )
  }
  parts
}

# The cell result under "separate": each unit's overshoot is the sum of its
# parts' overshoots, and the cell's is the sum over its compared units. A
# compared component without pressure rows (an allowance with nothing to
# meet it) keeps its own overshoot, as no part exists to separate.
.nbx_regime_cells <- function(cells, parts, split) {
  unit_os <- dplyr::summarise(
    parts,
    unit_overshoot = sum(.data$part_positive_overshoot_n_t),
    .by = c("cell_id", "year", "boundary_component")
  )
  cells <- if (split) {
    .nbx_regime_unit_overshoot(cells, unit_os)
  } else {
    cells |>
      dplyr::left_join(
        dplyr::select(unit_os, -"boundary_component"),
        by = c("cell_id", "year"),
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        cell_positive_overshoot_n_t = dplyr::if_else(
          .data$coverage_state == "valid",
          .data$unit_overshoot,
          .data$cell_positive_overshoot_n_t
        )
      ) |>
      dplyr::select(-"unit_overshoot")
  }
  # A valid cell with no part (its compared components carry no pressure
  # row) sums an empty set of parts: a structural zero, not a missing value.
  # Cells outside the comparison have no regime totals.
  cells |>
    dplyr::left_join(
      .nbx_regime_cell_totals(parts),
      by = c("cell_id", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(.nbx_regime_output_cols()),
        \(v) {
          dplyr::if_else(
            .data$coverage_state == "valid",
            dplyr::coalesce(v, 0),
            NA_real_
          )
        }
      )
    )
}

.nbx_regime_unit_overshoot <- function(cells, unit_os) {
  wide <- unit_os |>
    tidyr::pivot_wider(
      names_from = "boundary_component",
      values_from = "unit_overshoot",
      names_glue = "{boundary_component}_parts_overshoot"
    ) |>
    ensure_columns(tibble::tibble(
      managed_parts_overshoot = numeric(),
      extensive_parts_overshoot = numeric()
    ))
  cells |>
    dplyr::left_join(
      wide,
      by = c("cell_id", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      managed_positive_overshoot_n_t = dplyr::coalesce(
        .data$managed_parts_overshoot,
        .data$managed_positive_overshoot_n_t
      ),
      extensive_positive_overshoot_n_t = dplyr::coalesce(
        .data$extensive_parts_overshoot,
        .data$extensive_positive_overshoot_n_t
      ),
      cell_positive_overshoot_n_t = dplyr::if_else(
        .data$coverage_state == "valid",
        .nbx_pick(.data$use_managed, .data$managed_positive_overshoot_n_t) +
          .nbx_pick(
            .data$use_extensive,
            .data$extensive_positive_overshoot_n_t
          ),
        .data$cell_positive_overshoot_n_t
      )
    ) |>
    dplyr::select(-"managed_parts_overshoot", -"extensive_parts_overshoot")
}

# Per-cell rainfed and irrigated totals over the compared parts. A compared
# component with no pressure rows has no part, so its allowance is in neither.
.nbx_regime_cell_totals <- function(parts) {
  parts |>
    dplyr::summarise(
      actual = sum(.data$part_actual_n_t),
      critical = sum(.data$part_critical_n_t),
      positive_overshoot = sum(.data$part_positive_overshoot_n_t),
      .by = c("cell_id", "year", "water_regime")
    ) |>
    tidyr::pivot_wider(
      names_from = "water_regime",
      values_from = c("actual", "critical", "positive_overshoot"),
      names_glue = "{water_regime}_{.value}_n_t",
      values_fill = 0
    ) |>
    # A regime no compared row carries holds an exact zero of every quantity.
    ensure_columns(
      .nbx_regime_prototype(),
      defaults = purrr::map(.nbx_regime_prototype(), \(col) 0)
    ) |>
    dplyr::select("cell_id", "year", dplyr::all_of(.nbx_regime_output_cols()))
}

.nbx_regime_prototype <- function() {
  .nbx_regime_output_cols() |>
    rlang::set_names() |>
    purrr::map(\(col) numeric()) |>
    tibble::as_tibble()
}

# Under "netted" the regime columns exist but are empty, so the schema does
# not depend on the method.
.nbx_no_regime_cols <- function(cells) {
  empty <- rlang::set_names(
    rep(list(NA_real_), length(.nbx_regime_output_cols())),
    .nbx_regime_output_cols()
  )
  dplyr::mutate(cells, !!!empty)
}

# The part a crop row shares its allowance with. Without the regime
# separation the part is the unit itself.
.nbx_attribution_parts <- function(x, parts) {
  if (is.null(parts)) {
    return(dplyr::mutate(
      x,
      part_actual_n_t = .data$unit_actual_n_t,
      part_critical_n_t = .data$unit_critical_n_t,
      part_signed_margin_n_t = .data$unit_signed_margin_n_t,
      part_positive_overshoot_n_t = .data$unit_positive_overshoot_n_t,
      part_condition_ratio = .data$unit_condition_ratio
    ))
  }
  dplyr::left_join(
    x,
    dplyr::select(
      parts,
      "cell_id",
      "year",
      "boundary_component",
      "water_regime",
      dplyr::starts_with("part_"),
      -"part_area_ha",
      -"part_share"
    ),
    by = c("cell_id", "year", "boundary_component", "water_regime"),
    relationship = "many-to-one"
  )
}

# Grid rows name the part they were compared in; empty under "netted".
.nbx_regime_grid_cols <- function(x) {
  separate <- !is.na(x$water_regime)
  dplyr::mutate(
    x,
    regime_actual_n_t = dplyr::if_else(separate, .data$part_actual_n_t, NA),
    regime_critical_n_t = dplyr::if_else(
      separate,
      .data$part_critical_n_t,
      NA
    ),
    regime_positive_overshoot_n_t = dplyr::if_else(
      separate,
      .data$part_positive_overshoot_n_t,
      NA
    )
  )
}
