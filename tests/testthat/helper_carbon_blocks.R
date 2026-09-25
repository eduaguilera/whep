# A small multi-cell, multi-year carbon-balance input set for the block tests
# (`test_carbon_balance_blocks.R`). Offline and deterministic. It is shaped to
# reach every term a year boundary could break:
#
# * cells whose string key sorts differently from their numbers (-10.25 before
#   -0.25 as numbers, after it as strings), and two polities sharing one
#   (lon, lat), so the output order and the per-(lon, lat) soil-nitrogen ratio
#   are both exercised;
# * areas that change every year, so every year moves carbon through the
#   land-use-change transfer;
# * a crop group that vanishes and reappears (irrigated herbaceous in the
#   first cell: present 2007-2008 and 2011-2012 only), and a natural class
#   whose ROW vanishes in 2009-2010 and comes back in 2011;
# * a class with no carbon-input row (the fifth cell's grassland), grouped
#   inputs where LUH2 has no cropland (the third cell in 2012), a compartment
#   with no climate at all, and a polity (68) with no modelled land, so every
#   report the class table raises fires;
# * area 277, South Sudan, whose polity starts in 2011, so 2007-2010 resolve
#   to a polity that did not exist and the polity-validity report fires;
# * an input C:N with gaps.
.cbb_years <- function() 2007:2013

.cbb_cells <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell,
    -10.25, 40.25, 203L, 1L,
    -0.25, 40.25, 203L, 2L,
    5.25, 50.25, 79L, 3L,
    5.25, 50.25, 203L, 4L,
    100.25, -5.25, 21L, 5L,
    30.25, 7.25, 277L, 6L,
    31.25, 7.25, 277L, 7L,
    60.25, 60.25, 68L, 8L
  )
}

# A smooth, deterministic wobble in [1 - amp, 1 + amp].
.cbb_wobble <- function(cell, year, k, amp = 0.15) {
  1 + amp * sin(1.7 * cell + 0.9 * (year - 2000) + 2.3 * k)
}

.cbb_land_use <- function() {
  base <- c(cropland = 40, grassland = 25, natural = 30, urban = 5)
  tidyr::expand_grid(
    .cbb_cells(),
    year = .cbb_years(),
    land_use = names(base)
  ) |>
    dplyr::mutate(
      k = match(.data$land_use, names(base)),
      area_ha = base[.data$land_use] *
        .cbb_wobble(.data$cell, .data$year, .data$k)
    ) |>
    # The third cell has no LUH2 cropland, and its natural row vanishes for
    # two years and comes back.
    dplyr::filter(
      !(.data$cell == 3L & .data$land_use == "cropland"),
      !(.data$cell == 3L &
        .data$land_use == "natural" &
        .data$year %in% c(2009L, 2010L))
    ) |>
    dplyr::select("lon", "lat", "area_code", "year", "land_use", "area_ha")
}

.cbb_group_rows <- function() {
  groups <- c(
    "cropland_rainfed_herbaceous",
    "cropland_irrigated_herbaceous",
    "cropland_rainfed_olive"
  )
  tidyr::expand_grid(
    .cbb_cells(),
    year = .cbb_years(),
    land_use = groups
  ) |>
    dplyr::mutate(k = match(.data$land_use, groups)) |>
    dplyr::filter(
      # The second cell keeps plain, ungrouped cropland.
      .data$cell != 2L,
      # The third cell has grouped inputs only in 2012, where LUH2 has none.
      .data$cell != 3L | .data$year == 2012L,
      # Irrigated herbaceous comes and goes in the first cell.
      !(.data$cell == 1L &
        .data$land_use == "cropland_irrigated_herbaceous" &
        !.data$year %in% c(2007L, 2008L, 2011L, 2012L))
    ) |>
    dplyr::mutate(
      group_area_ha = (10 + 5 * .data$k) *
        .cbb_wobble(.data$cell, .data$year, .data$k + 4)
    )
}

.cbb_c_inputs <- function() {
  other <- tidyr::expand_grid(
    .cbb_cells(),
    year = .cbb_years(),
    land_use = c("cropland", "grassland", "natural")
  ) |>
    dplyr::filter(
      .data$land_use != "cropland" | .data$cell == 2L,
      # No carbon-input row for the fifth cell's grassland.
      !(.data$cell == 5L & .data$land_use == "grassland")
    ) |>
    dplyr::mutate(
      k = match(.data$land_use, c("cropland", "grassland", "natural"))
    )
  dplyr::bind_rows(other, .cbb_group_rows()) |>
    dplyr::mutate(
      c_input_mgc_ha_yr = (1 + 0.4 * .data$k) *
        .cbb_wobble(.data$cell, .data$year, .data$k + 8),
      humified_fraction = 0.2 + 0.03 * .data$k,
      input_cn = dplyr::if_else(
        (.data$cell + .data$year) %% 3L == 0L,
        NA_real_,
        20 + 15 * .data$k + .data$cell
      )
    ) |>
    dplyr::select(-"k", -"cell")
}

# Raw monthly drivers for every compartment but the seventh and eighth.
.cbb_climate <- function() {
  tidyr::expand_grid(
    dplyr::filter(.cbb_cells(), .data$cell <= 6L),
    year = .cbb_years(),
    month = 1:12
  ) |>
    dplyr::mutate(
      temp_c = 8 +
        0.2 * .data$cell +
        12 * sin((.data$month - 4 + .data$lat / 90) / 12 * 2 * pi) +
        0.3 * (.data$year - 2007),
      precip_mm = 40 + 30 * cos((.data$month + .data$cell) / 12 * 2 * pi),
      pet_mm = 20 + 60 * pmax(0, sin((.data$month - 3) / 12 * 2 * pi)),
      irrig_mm = dplyr::if_else(.data$cell %in% c(1L, 5L), 15, 0),
      water_minus_pet_mm = .data$precip_mm + .data$irrig_mm - .data$pet_mm,
      # Per (lon, lat): the third and fourth compartments share a cell.
      clay_pct = 15 + abs(.data$lon) %% 7
    ) |>
    dplyr::select(-"cell", -"irrig_mm")
}

# Both cover layers are per (lon, lat), as LPJmL's are.
.cbb_natural_cover <- function() {
  tidyr::expand_grid(
    dplyr::distinct(.cbb_cells(), .data$lon, .data$lat),
    year = .cbb_years()
  ) |>
    dplyr::mutate(
      natural_cover = 0.5 + 0.4 * sin(.data$lon + .data$year)^2
    )
}

.cbb_cropland_cover <- function() {
  tidyr::expand_grid(
    dplyr::distinct(.cbb_cells(), .data$lon, .data$lat),
    year = .cbb_years(),
    month = 1:12
  ) |>
    dplyr::mutate(
      cropland_cover = 0.1 +
        0.7 * pmax(0, sin((.data$month - 3 + .data$lat / 10) / 12 * 2 * pi))
    )
}

# One colder representative cycle per compartment, as the equilibrium normal
# is shaped: year-less in meaning, carried at `year = 0`.
.cbb_equilibrium_climate <- function() {
  .cbb_climate() |>
    dplyr::filter(.data$year == 2007L) |>
    dplyr::mutate(temp_c = .data$temp_c - 4, year = 0L)
}

.cbb_data <- function(extras = FALSE) {
  d <- list(
    land_use = .cbb_land_use(),
    c_inputs = .cbb_c_inputs(),
    climate = .cbb_climate()
  )
  if (extras) {
    d$natural_cover <- .cbb_natural_cover()
    d$cropland_cover <- .cbb_cropland_cover()
    d$equilibrium_climate <- .cbb_equilibrium_climate()
  }
  d
}

# Every condition a call raises, in order, as (class, message) pairs, and the
# value. Messages and warnings are recorded and silenced; errors propagate.
.cbb_record <- function(expr) {
  seen <- list()
  value <- withCallingHandlers(
    expr,
    warning = function(cnd) {
      seen[[length(seen) + 1L]] <<- c("warning", conditionMessage(cnd))
      invokeRestart("muffleWarning")
    },
    message = function(cnd) {
      seen[[length(seen) + 1L]] <<- c("message", conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, conditions = seen)
}
