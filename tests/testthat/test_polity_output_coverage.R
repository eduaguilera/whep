expect_polity_match <- function(data, code_col, polity_col) {
  testthat::expect_true(code_col %in% names(data))
  testthat::expect_true(polity_col %in% names(data))
  has_code <- !is.na(data[[code_col]])
  testthat::expect_false(any(is.na(data[[polity_col]][has_code])))
}

testthat::test_that("public area-code example outputs carry reporting polities", {
  outputs <- list(
    get_primary_production = get_primary_production(example = TRUE),
    build_primary_production = build_primary_production(example = TRUE),
    get_wide_cbs = get_wide_cbs(example = TRUE),
    build_commodity_balances = build_commodity_balances(example = TRUE),
    build_processing_coefs = build_processing_coefs(example = TRUE),
    get_processing_coefs = get_processing_coefs(example = TRUE),
    get_primary_residues = get_primary_residues(example = TRUE),
    get_feed_intake = get_feed_intake(example = TRUE),
    get_land_fp_production = get_land_fp_production(example = TRUE),
    build_supply_use = build_supply_use(example = TRUE)
  )

  purrr::walk(
    outputs,
    \(output) {
      expect_polity_match(
        output,
        "area_code",
        "reporting_polity_code"
      )
    }
  )
})

testthat::test_that("public trade example output carries reporter and partner polities", {
  output <- build_detailed_trade(example = TRUE)

  expect_polity_match(output, "area_code", "reporting_polity_code")
  expect_polity_match(output, "area_code_partner", "partner_polity_code")
})

testthat::test_that("IO and footprint role outputs carry role polities", {
  supply_use <- tibble::tribble(
    ~year, ~area_code, ~proc_group, ~proc_cbs_code, ~item_cbs_code, ~type, ~value,
    2000L, 1L, "crop_production", 10L, 10L, "supply", 100,
    2000L, 1L, "crop_production", 10L, 10L, "use", 5,
    2000L, 2L, "crop_production", 10L, 10L, "supply", 80,
    2000L, 2L, "crop_production", 10L, 10L, "use", 3
  )
  bilateral_trade <- tibble::tibble(
    year = 2000L,
    item_cbs_code = 10L,
    bilateral_trade = list(matrix(c(0, 5, 3, 0), nrow = 2))
  )
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~production, ~import, ~export, ~food, ~other_uses, ~stock_withdrawal, ~stock_addition,
    2000L, 1L, 10L, 100, 3, 5, 50, 20, 0, 0,
    2000L, 2L, 10L, 80, 5, 3, 40, 15, 0, 0
  )

  io <- build_io_model(supply_use, bilateral_trade, cbs)
  labels <- io$labels[[1]]
  fd_labels <- io$fd_labels[[1]]
  extensions <- rep(1, length(io$X[[1]]))

  expect_polity_match(labels, "area_code", "reporting_polity_code")
  expect_polity_match(fd_labels, "area_code", "reporting_polity_code")

  footprint <- compute_footprint(
    x_vec = io$X[[1]],
    y_mat = io$Y[[1]],
    extensions = extensions,
    labels = labels,
    z_mat = io$Z[[1]],
    fd_labels = fd_labels
  )
  paths <- compute_footprint_paths(
    io$Z[[1]],
    io$X[[1]],
    io$Y[[1]],
    extensions,
    labels,
    fd_labels,
    conserve_extensions = FALSE
  )
  product_paths <- compute_fp_product_paths(
    io$Z[[1]],
    io$X[[1]],
    io$Y[[1]],
    extensions,
    labels,
    fd_labels,
    conserve_extensions = FALSE
  )

  expect_polity_match(footprint, "origin_area", "origin_polity_code")
  expect_polity_match(footprint, "target_area", "target_polity_code")
  expect_polity_match(paths, "origin_area", "origin_polity_code")
  expect_polity_match(paths, "use_area", "use_polity_code")
  expect_polity_match(paths, "target_area", "target_polity_code")
  expect_polity_match(product_paths, "origin_area", "origin_polity_code")
  expect_polity_match(product_paths, "product_area", "product_polity_code")
  expect_polity_match(product_paths, "target_area", "target_polity_code")
})

testthat::test_that("spatialized public outputs carry reporting polities", {
  country_areas <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_prod_code = 15L,
    harvested_area_ha = 1000,
    irrigated_area_ha = 0
  )
  crop_patterns <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = c(50.25, 50.25),
    item_prod_code = 15L,
    harvest_fraction = c(0.6, 0.4)
  )
  gridded_cropland <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = c(50.25, 50.25),
    year = 2000L,
    cropland_ha = c(800, 500),
    irrigated_ha = 0
  )
  country_grid <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = c(50.25, 50.25),
    area_code = 1L
  )

  landuse <- build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid,
    config = list(years = 2000L)
  )
  livestock <- build_gridded_livestock(
    tibble::tibble(
      year = 2000L,
      area_code = 1L,
      species_group = "cattle",
      heads = 5000
    ),
    tibble::tibble(
      lon = c(0.25, 0.75),
      lat = c(50.25, 50.25),
      year = 2000L,
      pasture_ha = c(600, 400),
      rangeland_ha = c(200, 100)
    ),
    gridded_cropland,
    country_grid
  )

  expect_polity_match(landuse, "area_code", "reporting_polity_code")
  expect_polity_match(livestock, "area_code", "reporting_polity_code")
})

testthat::test_that("legacy area reference tables are backed by polities", {
  testthat::expect_false(any(is.na(whep::polity_area_crosswalk$polity_code)))
  testthat::expect_true(all(whep::polity_area_crosswalk$has_geometry))

  for (data in list(whep::regions_full, whep::polities_cats)) {
    expect_polity_match(data, "code", "reporting_polity_code")
    testthat::expect_false(any(is.na(data$polity_code)))
    coded_rows <- !is.na(data$code)
    testthat::expect_true(all(data$reporting_polity_has_geometry[coded_rows]))
  }
})
