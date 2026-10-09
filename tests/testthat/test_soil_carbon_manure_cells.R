# Cropland manure carbon placed on the cells where the livestock are
# (whep#1307). Offline: every input is injected, and the one reader the
# turnkey path would call (the cell-grain feed intake) is mocked.

.smc_grid <- function() {
  # Two cells of polity 1. Crop 15: 30 ha in A, 10 ha in B; crop 27: 5 ha in
  # A, 15 ha in B.
  list(
    country_grid = tibble::tribble(
      ~lon, ~lat, ~area_code, ~cell_area_frac,
      0.25, 0.25, 1L, 1,
      0.75, 0.25, 1L, 1
    ),
    crop_patterns = tibble::tribble(
      ~lon, ~lat, ~item_prod_code, ~harvest_fraction, ~crop_area_ha,
      0.25, 0.25, "15", 0.75, 30,
      0.75, 0.25, "15", 0.25, 10,
      0.25, 0.25, "27", 0.25, 5,
      0.75, 0.25, "27", 0.75, 15
    )
  )
}

.smc_npp <- function() {
  tibble::tribble(
    ~area_code, ~item_prod_code, ~year,
    ~residue_soil_c_t, ~root_c_t, ~weed_npp_c_t,
    1L, "15", 2020L, 60, 40, 0,
    1L, "27", 2020L, 30, 10, 0
  )
}

# A cell-grain `applied` stream, the shape build_livestock_nutrient_flows()
# returns at "subnational": the herds are in cell A, whose crops take what
# their cap allows; the rest is trucked to cell B and lands there with no crop
# (within B's room), and what no cell had room for is over-applied on A, also
# with no crop.
.smc_cell_manure <- function() {
  tibble::tribble(
    ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~source_stream,
    ~applied_n, ~applied_c, ~over_cap,
    2020L, "1", "0.25_0.25", "Cropland", "15", "collected", 2, 20, FALSE,
    2020L, "1", "0.25_0.25", "Cropland", "27", "collected", 0.4, 4, FALSE,
    2020L, "1", "0.75_0.25", "Cropland", NA, "transported", 0.8, 8, FALSE,
    2020L, "1", "0.25_0.25", "Cropland", NA, "collected", 0.6, 6, TRUE,
    2020L, "1", "0.25_0.25", "Grassland", NA, "grazing", 5, 50, FALSE
  )
}

.smc_data <- function(manure = .smc_cell_manure()) {
  grid <- .smc_grid()
  list(
    npp = .smc_npp(),
    manure = manure,
    country_grid = grid$country_grid,
    crop_patterns = grid$crop_patterns,
    residue_humification = whep::residue_humification
  )
}

.smc_build <- function(data = .smc_data(), ...) {
  whep::build_soil_carbon_inputs(
    data = data,
    method_crop_weights = "static",
    method_unspatialized = "drop",
    ...
  )
}

.smc_manure_mass <- function(out) {
  out |>
    dplyr::mutate(manure_c = .data$manure_c_mgc_ha_yr * .data$crop_area_ha) |>
    dplyr::select(dplyr::any_of(c("lon", "lat")), "item_prod_code", "manure_c")
}

test_that("manure carbon lands on the cells its applied stream names", {
  out <- suppressWarnings(.smc_build(method_manure_placement = "livestock"))
  got <- .smc_manure_mass(out) |>
    dplyr::arrange(.data$lon, .data$item_prod_code)
  # Cell A keeps its own crops' manure plus the 6 t C over-applied there,
  # spread by area (30 and 5 of its 35 ha); cell B gets only the trucked
  # 8 t C, spread over its crops by area (10 and 15 of its 25 ha).
  testthat::expect_equal(got$lon, c(0.25, 0.25, 0.75, 0.75))
  testthat::expect_equal(got$item_prod_code, c("15", "27", "15", "27"))
  testthat::expect_equal(
    got$manure_c,
    c(20 + 6 * 30 / 35, 4 + 6 * 5 / 35, 8 * 10 / 25, 8 * 15 / 25)
  )
  testthat::expect_true(all(out$method_manure_placement == "livestock"))
})

test_that("crop-area placement spreads the same stream by crop area", {
  out <- suppressWarnings(.smc_build(method_manure_placement = "crop_area"))
  got <- .smc_manure_mass(out) |>
    dplyr::arrange(.data$lon, .data$item_prod_code)
  # Polity-crop totals (15: 20, 27: 4) gridded by each crop's area share.
  testthat::expect_equal(got$manure_c, c(15, 1, 5, 3))
  testthat::expect_true(all(out$method_manure_placement == "crop_area"))
})

test_that("each cell books the cropland manure the engine placed there", {
  stream <- .smc_cell_manure()
  out <- suppressWarnings(.smc_build(method_manure_placement = "livestock"))
  booked <- .smc_manure_mass(out) |>
    dplyr::summarise(c = sum(.data$manure_c), .by = c("lon", "lat"))
  # What the nitrogen balance books per cell from the same stream: every
  # Cropland row, over-cap ones included (R/n_balance_inputs.R,
  # .manure_to_n_inputs()).
  placed <- dplyr::filter(stream, .data$land_use == "Cropland")
  cell <- whep:::.parse_cell_id(placed$sub_territory)
  placed <- placed |>
    dplyr::mutate(lon = cell$lon, lat = cell$lat) |>
    dplyr::summarise(c = sum(.data$applied_c), .by = c("lon", "lat"))
  joined <- dplyr::inner_join(booked, placed, by = c("lon", "lat"))
  testthat::expect_equal(nrow(joined), 2L)
  testthat::expect_equal(joined$c.x, joined$c.y)
})

test_that("polity output sums the cell placements", {
  out <- suppressWarnings(.smc_build(
    resolution = "polity",
    method_manure_placement = "livestock"
  ))
  got <- .smc_manure_mass(out) |> dplyr::arrange(.data$item_prod_code)
  testthat::expect_equal(
    got$manure_c,
    c(20 + 8 * 10 / 25 + 6 * 30 / 35, 4 + 8 * 15 / 25 + 6 * 5 / 35)
  )
})

test_that("over-cap cropland manure is kept on its cell and reported", {
  testthat::expect_message(
    suppressWarnings(.smc_build(method_manure_placement = "livestock")),
    class = "whep_sci_manure_over_cap_kept"
  )
})

test_that("manure on a crop-less cell goes to its polity's cropland", {
  manure <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~source_stream,
    ~applied_n, ~applied_c, ~over_cap,
    2020L, "1", "0.25_0.25", "Cropland", "15", "collected", 2, 20, FALSE,
    2020L, "1", "5.25_0.25", "Cropland", NA, "collected", 1, 10, TRUE
  )
  testthat::expect_message(
    out <- suppressWarnings(.smc_build(
      data = .smc_data(manure),
      method_manure_placement = "livestock"
    )),
    class = "whep_sci_manure_reallocated"
  )
  got <- .smc_manure_mass(out) |>
    dplyr::summarise(c = sum(.data$manure_c), .by = "item_prod_code") |>
    dplyr::arrange(.data$item_prod_code)
  # The 10 t C follows the polity's crop area: 40 ha of crop 15, 20 of 27.
  testthat::expect_equal(got$c, c(20 + 10 * 40 / 60, 10 * 20 / 60))
})

test_that("manure in a polity with no cropland cell is dropped, loudly", {
  manure <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~source_stream,
    ~applied_n, ~applied_c, ~over_cap,
    2020L, "1", "0.25_0.25", "Cropland", "15", "collected", 2, 20, FALSE,
    2020L, "2", "5.25_0.25", "Cropland", NA, "collected", 1, 10, TRUE
  )
  testthat::expect_warning(
    out <- .smc_build(
      data = .smc_data(manure),
      method_manure_placement = "livestock"
    ),
    class = "whep_sci_manure_no_cropland"
  )
  testthat::expect_equal(sum(.smc_manure_mass(out)$manure_c), 20)
})

test_that("a national stream cannot be placed by livestock", {
  national <- dplyr::mutate(
    .smc_cell_manure(),
    sub_territory = NA_character_
  )
  testthat::expect_error(
    .smc_build(
      data = .smc_data(national),
      method_manure_placement = "livestock"
    ),
    class = "whep_sci_manure_grain_mismatch"
  )
})

test_that("the turnkey path builds manure from where the herds are", {
  # All the animals stand in cell B. The engine is the real one; only the
  # cell-grain intake reader is stubbed.
  intake <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~livestock_category,
    ~item_cbs_code, ~feed_quality, ~intake_dm_t,
    2020L, "1", "0.75_0.25", "Cattle_milk", 2513L, "high_quality", 2,
    2020L, "1", "0.75_0.25", "Cattle_milk", NA, "grass", 6
  )
  testthat::local_mocked_bindings(
    .sci_cell_intake_context = function(years) list(),
    .sci_cell_intake = function(yr, ctx, country_grid) intake,
    .package = "whep"
  )
  data <- .smc_data()
  data$manure <- NULL
  out <- suppressWarnings(.smc_build(
    data = data,
    method_manure_placement = "livestock"
  ))
  by_cell <- .smc_manure_mass(out) |>
    dplyr::summarise(c = sum(.data$manure_c), .by = "lon")
  testthat::expect_gt(by_cell$c[by_cell$lon == 0.75], 0)
  testthat::expect_equal(by_cell$c[by_cell$lon == 0.25], 0)
})

test_that("the crop-area placement reports no cell manure", {
  # No year contributes a cell-manure row, so the bound tables have no columns.
  testthat::expect_no_warning(
    whep:::.sci_report_cell_manure(list(NULL, NULL))
  )
})
