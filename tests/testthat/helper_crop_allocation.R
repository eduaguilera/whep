# Offline inputs for the crop allocation (R/crop_allocation.R): the
# spatialization engine's three inputs and a fodder layer, small enough to
# check by hand, on the two-cell grid of test_n_balance_spatialize.R (0.25 and
# 0.75 E on 50.25 N). Nothing here reads a pin.

# National harvested area per polity and production item (wheat 15, rice 27),
# for polity 10 and for the border polity 20.
.ca_country_areas <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2010L, 10L,        15L,             600,
    2010L, 10L,        27L,             300,
    2010L, 20L,        15L,             100,
    2010L, 20L,        27L,             50
  )
}

.ca_crop_patterns <- function() {
  tibble::tribble(
    ~lon, ~lat,  ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L,             0.6,
    0.75, 50.25, 15L,             0.2,
    0.25, 50.25, 27L,             0.1,
    0.75, 50.25, 27L,             0.3
  )
}

.ca_gridded_cropland <- function() {
  tibble::tribble(
    ~lon, ~lat,  ~year, ~cropland_ha,
    0.25, 50.25, 2010L, 1200,
    0.75, 50.25, 2010L, 1000
  )
}

.ca_engine_inputs <- function() {
  list(
    country_areas = .ca_country_areas(),
    crop_patterns = .ca_crop_patterns(),
    gridded_cropland = .ca_gridded_cropland()
  )
}

# A fodder layer for alfalfa (641), which the engine does not place.
.ca_fodder_patterns <- function() {
  tibble::tribble(
    ~lon, ~lat,  ~item_prod_code, ~crop_area_ha,
    0.25, 50.25, "641",           30,
    0.75, 50.25, "641",           10
  )
}

.ca_inputs <- function() {
  list(
    engine_inputs = .ca_engine_inputs(),
    fodder_patterns = .ca_fodder_patterns()
  )
}

# A hand-built allocation for exact expectations: polity 10, wheat (15) on
# both cells 3:1, rice (27) on the second cell only; the fallback cropland
# support 0.6 / 0.4.
.ca_allocation <- function() {
  list(
    weights = tibble::tribble(
      ~lon, ~lat,  ~area_code, ~item_prod_code, ~year, ~crop_area_ha,
      ~area_weight,
      0.25, 50.25, 10L,        15L,             2010L, 450,
      0.75,
      0.75, 50.25, 10L,        15L,             2010L, 150,
      0.25,
      0.75, 50.25, 10L,        27L,             2010L, 300,
      1
    ),
    fallback = tibble::tribble(
      ~lon, ~lat,  ~area_code, ~year, ~cropland_weight,
      0.25, 50.25, 10L,        2010L, 0.6,
      0.75, 50.25, 10L,        2010L, 0.4
    )
  )
}

# An allocation whose weights are the static crop-pattern formula the grid
# step used before whep#1002 (pattern fraction x LUH2 cropland x the polity's
# share of the cell; the fallback the polity's LUH2 cropland), for tests that
# pin how a term applies an allocation rather than how one is built.
.ca_static_allocation <- function(
  cell_polity,
  crop_patterns,
  type_cropland,
  frac = "polity_frac"
) {
  cropland <- dplyr::summarise(
    type_cropland,
    type_ha = sum(.data$type_ha),
    .by = c("lon", "lat", "year")
  )
  cells <- dplyr::transmute(
    cell_polity,
    .data$lon,
    .data$lat,
    area_code = as.integer(.data$area_code),
    .frac = .data[[frac]]
  )
  weights <- crop_patterns |>
    dplyr::inner_join(cropland, by = c("lon", "lat")) |>
    dplyr::inner_join(
      cells,
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$area_code,
      item_prod_code = as.integer(.data$item_prod_code),
      year = as.integer(.data$year),
      crop_area_ha = .data$type_ha * .data$harvest_fraction * .data$.frac
    ) |>
    dplyr::filter(.data$crop_area_ha > 0) |>
    dplyr::mutate(
      area_weight = .data$crop_area_ha / sum(.data$crop_area_ha),
      .by = c("area_code", "item_prod_code", "year")
    )
  fallback <- cropland |>
    dplyr::inner_join(
      cells,
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$area_code,
      year = as.integer(.data$year),
      .w = .data$type_ha * .data$.frac
    ) |>
    dplyr::filter(.data$.w > 0) |>
    dplyr::mutate(
      cropland_weight = .data$.w / sum(.data$.w),
      .by = c("area_code", "year")
    ) |>
    dplyr::select(-".w")
  list(weights = weights, fallback = fallback)
}
