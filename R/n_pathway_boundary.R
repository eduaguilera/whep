# Gridded critical-nitrogen boundary exceedance, pathway mode (SJOS-N Module 2,
# Task 2.3, Mode B). Instead of comparing a single lumped surplus to a single
# critical surplus (Mode A, R/n_boundary_exceedance.R), the pathway mode routes
# each of WHEP's process-based nitrogen losses to its own medium-specific
# critical load from Schulte-Uebbing et al. (2022) and decomposes each medium
# separately with the shared exceed-share formula (.n_exceed_split()):
# - air: field ammonia emission (nh3_n_t; optionally plus manure housing and
#   storage ammonia) vs the critical ammonia emission (crit_nh3_emission);
# - water: nitrate loss (no3_n_t) vs the tighter of the critical groundwater
#   leaching (crit_leaching_gw) and critical surface-water load (crit_load_sw);
#   the lower of the two binds, recorded in binding_water_medium.
# The comparison is per crop (item_cbs_code): the cell's single critical value
# broadcasts to every crop sharing the cell (locked plan decision 14), a
# documented per-cell environmental-limit simplification. Conservation holds
# per medium (exceedance + within == actual) with the exceedance share in
# [0, 1]. binding_boundary names the medium exceeded most.
#
# Climate / nitrous-oxide is deliberately NOT a pathway here: the
# Schulte-Uebbing archive ships no critical nitrous-oxide (climate) load, so a
# climate-nitrogen pathway is a documented future hook. Nitrous oxide continues
# to be accounted in the balance's total_gwp_co2e_kg, not double-counted here.

#' Build the pathway-mode critical-nitrogen boundary exceedance.
#'
#' @description
#' Compares a [build_nitrogen_balance()] output's process-based nitrogen losses
#' against the medium-specific critical loads of Schulte-Uebbing et al. (2022),
#' per grid cell and per crop (Mode B). The air medium compares field ammonia
#' emission (`nh3_n_t`, converted to a per-hectare rate) to the critical
#' ammonia emission; the water medium compares nitrate loss (`no3_n_t`) to the
#' tighter (lower) of the critical groundwater leaching and critical
#' surface-water load, recording which sub-medium binds. Each medium is split
#' into an `exceedance` part (above the load) and a `within` part with the
#' shared exceed-share formula, as a per-hectare intensity and a mass, so
#' `exceedance + within == actual` per medium. `binding_boundary` names the
#' medium (`"air"`, `"water"`, `"both"` on an exact positive tie, or `"none"`)
#' with the highest exceedance share. `resolution = "grid"` keeps the full
#' per-crop grid key and per-medium columns; `"polity"` and `"country"` sum the
#' mass terms over cells to `area_code`, `item_cbs_code`, `year`. Climate /
#' nitrous oxide is not a pathway (the archive ships no critical climate-N
#' load); it stays in the balance's `total_gwp_co2e_kg`, and a climate-N
#' pathway is a documented future hook.
#'
#' @param balance A [build_nitrogen_balance()] output keyed by `lon`, `lat`,
#'   `area_code`, `item_cbs_code`, `year`, carrying `area_ha`, `nh3_n_t` and
#'   `no3_n_t`.
#' @param critical_loads A named list of [read_critical_n()] outputs (each
#'   `lon`, `lat`, `value` in kg N per hectare) with elements
#'   `crit_nh3_emission`, `crit_leaching_gw` and `crit_load_sw`.
#' @param nh3_source Air-pressure scope: `"soil"` (default, field `nh3_n_t`
#'   only, consistent with the surplus boundary) or `"total_agricultural"`
#'   (also adds manure housing and storage ammonia from
#'   `data$manure_mgmt_nh3_n_t`, keyed to the balance grid; if absent, warns and
#'   falls back to soil ammonia).
#' @param resolution Output grain: `"grid"` (default, per crop per cell) or
#'   `"polity"` / `"country"` (per crop per country, summing the mass terms).
#' @param data Optional named list of injected inputs. `manure_mgmt_nh3_n_t` (a
#'   tibble keyed to the balance grid with a `manure_mgmt_nh3_n_t` column)
#'   supplies the housing and storage ammonia for
#'   `nh3_source = "total_agricultural"`. Defaults to `list()`.
#' @param example If `TRUE`, return a small fixture instead of computing.
#'   Defaults to `FALSE`.
#' @return For `resolution = "grid"`, a tibble keyed `lon`, `lat`, `area_code`,
#'   `item_cbs_code`, `year` with `area_ha`, the air-medium columns
#'   (`critical_air_kgn_ha`, `actual_air_kgn_ha`, `exceed_share_air`,
#'   `exceedance_air_kgn_ha`, `within_air_kgn_ha`, `exceedance_air_n_t`,
#'   `within_air_n_t`, `actual_air_n_t`), the water-medium columns (the same
#'   set with a `water` suffix, plus `critical_gw_kgn_ha`, `critical_sw_kgn_ha`
#'   and `binding_water_medium`), `binding_boundary`, and the `nh3_source` /
#'   `method_boundary` stamps. For the aggregate resolutions, the grouping key
#'   with the summed per-medium mass terms and the same stamps.
#' @export
#' @examples
#' build_n_pathway_exceedance(example = TRUE)
build_n_pathway_exceedance <- function(
  balance,
  critical_loads,
  nh3_source = c("soil", "total_agricultural"),
  resolution = c("grid", "polity", "country"),
  data = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_n_pathway_exceedance())
  }
  nh3_source <- rlang::arg_match(nh3_source)
  resolution <- rlang::arg_match(resolution)
  .check_columns(balance, .npb_balance_required(), "balance")
  .npb_check_loads(critical_loads)
  balance |>
    .npb_join_critical(critical_loads) |>
    .npb_air(nh3_source, data) |>
    .npb_water() |>
    .npb_binding() |>
    .npb_stamp(nh3_source) |>
    .npb_resolve(resolution)
}

# ---- Private helpers -------------------------------------------------------

# Columns the balance input must carry: the full grid key, harvested area and
# the two loss terms compared to the medium-specific critical loads.
.npb_balance_required <- function() {
  c(
    "lon",
    "lat",
    "area_code",
    "item_cbs_code",
    "year",
    "area_ha",
    "nh3_n_t",
    "no3_n_t"
  )
}

# Validate the injected critical-load list has the three medium layers, each a
# lon/lat/value tibble.
.npb_check_loads <- function(critical_loads) {
  needed <- c("crit_nh3_emission", "crit_leaching_gw", "crit_load_sw")
  missing <- needed[!rlang::has_name(critical_loads, needed)]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg critical_loads} must be a named list with the medium layers.",
      "x" = "Missing element{?s}: {.field {missing}}."
    ))
  }
  purrr::walk2(
    critical_loads[needed],
    needed,
    \(layer, nm) .check_columns(layer, c("lon", "lat", "value"), nm)
  )
}

# Broadcast each cell's three critical loads to every crop in the cell,
# renaming value to the medium-specific critical column.
.npb_join_critical <- function(balance, critical_loads) {
  balance |>
    .npb_join_one(critical_loads$crit_nh3_emission, "critical_air_kgn_ha") |>
    .npb_join_one(critical_loads$crit_leaching_gw, "critical_gw_kgn_ha") |>
    .npb_join_one(critical_loads$crit_load_sw, "critical_sw_kgn_ha")
}

# Inner-join one critical layer by cell, so only cells with all three loads
# survive (the water minimum needs both sub-media).
.npb_join_one <- function(x, layer, name) {
  crit <- dplyr::transmute(
    layer,
    lon = .data$lon,
    lat = .data$lat,
    "{name}" := .data$value
  )
  dplyr::inner_join(x, crit, by = c("lon", "lat"))
}

# Air medium: build the per-hectare ammonia pressure (soil, or soil plus manure
# management), then decompose it against the critical ammonia emission.
.npb_air <- function(x, nh3_source, data) {
  x |>
    .npb_air_pressure(nh3_source, data) |>
    .npb_decompose_medium("air")
}

# The per-hectare ammonia pressure. area_ha == 0 yields NA rather than a
# non-finite rate.
.npb_air_pressure <- function(x, nh3_source, data) {
  x |>
    .npb_air_nh3(nh3_source, data) |>
    dplyr::mutate(
      actual_air_kgn_ha = dplyr::if_else(
        .data$area_ha > 0,
        .data$nh3_air_n_t * 1000 / .data$area_ha,
        NA_real_
      )
    )
}

# The ammonia mass feeding the air pressure: field nh3_n_t for "soil"; field
# plus manure housing/storage ammonia for "total_agricultural". A missing
# manure input warns and falls back to soil, so the two pools stay disjoint and
# are never double-counted.
.npb_air_nh3 <- function(x, nh3_source, data) {
  manure <- data$manure_mgmt_nh3_n_t
  if (nh3_source != "total_agricultural") {
    return(dplyr::mutate(x, nh3_air_n_t = .data$nh3_n_t))
  }
  if (is.null(manure)) {
    cli::cli_warn(c(
      "!" = "No manure-management ammonia supplied for
             {.val total_agricultural}; using soil ammonia only.",
      "i" = "Provide {.field data$manure_mgmt_nh3_n_t} keyed to the balance
             grid to add housing and storage ammonia."
    ))
    return(dplyr::mutate(x, nh3_air_n_t = .data$nh3_n_t))
  }
  x |>
    dplyr::left_join(
      manure,
      by = intersect(.npb_balance_required(), names(manure))
    ) |>
    dplyr::mutate(
      nh3_air_n_t = .data$nh3_n_t +
        tidyr::replace_na(.data$manure_mgmt_nh3_n_t, 0)
    )
}

# Water medium: the per-hectare nitrate pressure, the binding (tighter) water
# critical load and which sub-medium binds, then the decomposition.
.npb_water <- function(x) {
  x |>
    dplyr::mutate(
      actual_water_kgn_ha = dplyr::if_else(
        .data$area_ha > 0,
        .data$no3_n_t * 1000 / .data$area_ha,
        NA_real_
      ),
      critical_water_kgn_ha = pmin(
        .data$critical_gw_kgn_ha,
        .data$critical_sw_kgn_ha
      ),
      binding_water_medium = dplyr::if_else(
        .data$critical_gw_kgn_ha <= .data$critical_sw_kgn_ha,
        "groundwater",
        "surface_water"
      )
    ) |>
    .npb_decompose_medium("water")
}

# Decompose one medium's per-hectare pressure into the within/exceedance parts
# (intensity and mass) with the shared exceed-share formula, writing
# medium-suffixed columns (e.g. "air" -> exceed_share_air, exceedance_air_n_t).
.npb_decompose_medium <- function(x, medium) {
  actual <- x[[paste0("actual_", medium, "_kgn_ha")]]
  critical <- x[[paste0("critical_", medium, "_kgn_ha")]]
  area <- x$area_ha
  share <- .n_exceed_split(actual, critical)
  dplyr::mutate(
    x,
    "exceed_share_{medium}" := share,
    "exceedance_{medium}_kgn_ha" := actual * share,
    "within_{medium}_kgn_ha" := actual * (1 - share),
    "exceedance_{medium}_n_t" := actual * share * area / 1000,
    "within_{medium}_n_t" := actual * (1 - share) * area / 1000,
    "actual_{medium}_n_t" := actual * area / 1000
  )
}

# The medium exceeded most: NA when a share is NA (a per-cell row with no
# agricultural area, e.g. the deposition/urban/SOM non-crop rows, cannot have a
# binding medium), "none" when neither medium exceeds, otherwise the medium with
# the strictly greater exceedance share, or "both" on an exact positive tie. The
# NA clause comes first so those rows are not swept into the "both" default by
# case_when's non-TRUE-on-NA comparisons.
.npb_binding <- function(x) {
  dplyr::mutate(
    x,
    binding_boundary = dplyr::case_when(
      is.na(.data$exceed_share_air) | is.na(.data$exceed_share_water) ~
        NA_character_,
      .data$exceed_share_air <= 0 & .data$exceed_share_water <= 0 ~ "none",
      .data$exceed_share_air > .data$exceed_share_water ~ "air",
      .data$exceed_share_water > .data$exceed_share_air ~ "water",
      .default = "both"
    )
  )
}

# Stamp the air-pressure scope and the boundary mode.
.npb_stamp <- function(x, nh3_source) {
  dplyr::mutate(x, nh3_source = nh3_source, method_boundary = "pathway")
}

# Return the requested resolution: the per-crop grid table or the
# polity/country mass aggregate (same grouping key here).
.npb_resolve <- function(x, resolution) {
  if (resolution == "grid") {
    return(.npb_grid_cols(x))
  }
  .npb_aggregate(x, c("area_code", "item_cbs_code", "year"))
}

# The per-crop grid table Module 4 consumes.
.npb_grid_cols <- function(x) {
  dplyr::select(
    x,
    "lon",
    "lat",
    "area_code",
    "item_cbs_code",
    "year",
    "area_ha",
    "critical_air_kgn_ha",
    "actual_air_kgn_ha",
    "exceed_share_air",
    "exceedance_air_kgn_ha",
    "within_air_kgn_ha",
    "exceedance_air_n_t",
    "within_air_n_t",
    "actual_air_n_t",
    "critical_gw_kgn_ha",
    "critical_sw_kgn_ha",
    "critical_water_kgn_ha",
    "actual_water_kgn_ha",
    "exceed_share_water",
    "exceedance_water_kgn_ha",
    "within_water_kgn_ha",
    "exceedance_water_n_t",
    "within_water_n_t",
    "actual_water_n_t",
    "binding_water_medium",
    "binding_boundary",
    "nh3_source",
    "method_boundary"
  )
}

# Sum the per-medium mass terms over cells to the requested key, carrying the
# constant provenance stamps.
.npb_aggregate <- function(x, key) {
  dplyr::summarise(
    x,
    exceedance_air_n_t = sum(.data$exceedance_air_n_t, na.rm = TRUE),
    within_air_n_t = sum(.data$within_air_n_t, na.rm = TRUE),
    actual_air_n_t = sum(.data$actual_air_n_t, na.rm = TRUE),
    exceedance_water_n_t = sum(.data$exceedance_water_n_t, na.rm = TRUE),
    within_water_n_t = sum(.data$within_water_n_t, na.rm = TRUE),
    actual_water_n_t = sum(.data$actual_water_n_t, na.rm = TRUE),
    nh3_source = dplyr::first(.data$nh3_source),
    method_boundary = dplyr::first(.data$method_boundary),
    .by = dplyr::all_of(key)
  )
}
