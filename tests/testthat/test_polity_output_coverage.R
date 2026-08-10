# `require_code` is what whep#417 needed: the polity check below only looks at
# rows that HAVE a code, so a row carrying no code at all passed it while
# identifying no territory whatsoever. Reference tables are exempt --
# `regions_full` legitimately holds 12 rows with no FAOSTAT code -- but an
# example output is not, so it defaults to on.
expect_polity_match <- function(
  data,
  code_col,
  polity_col,
  require_code = TRUE
) {
  testthat::expect_true(code_col %in% names(data))
  testthat::expect_true(polity_col %in% names(data))
  has_code <- !is.na(data[[code_col]])
  if (require_code) {
    testthat::expect_false(any(!has_code))
  }
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
  # FAOSTAT area 351 "China" is a statistical aggregate of its components
  # (mainland 41, Hong Kong 96, Macao 128, Taiwan 214), reported alongside them
  # for every year. It is intentionally left unmapped so it cannot double-count
  # China; every OTHER reporting area must still map to a polity. The dedup
  # lives in the table_mappings data-raw script.
  aggregate_codes <- 351L

  cw <- whep::polity_area_crosswalk
  testthat::expect_true(all(is.na(cw$polity_code[
    cw$area_code %in% aggregate_codes
  ])))
  cw <- cw[!cw$area_code %in% aggregate_codes, ]
  testthat::expect_false(any(is.na(cw$polity_code)))
  # Every crosswalk polity must have a polygon UNLESS it is explicitly
  # polygon_status == "unassigned": some historical periods (e.g. pre-1883
  # Chile, before the War of the Pacific) have no faithful-vintage polygon,
  # and we record an honest gap rather than back-project a later/modern border.
  #
  # One upstream defect is PINNED rather than tolerated: `CAN-1800-1866` declares
  # polygon_status "proxy", which asserts a substitute polygon was attached, and
  # ships none. That contradiction is filed upstream as whep-polities issue 59,
  # which upstream is working through -- 4 were reachable from the crosswalk before
  # #517 and this is the last. Naming it keeps a NEW offender failing here while
  # the known one is visible; the pin must shrink to zero, never grow.
  known_status_defects <- "CAN-1800-1866"
  no_geometry <- cw[
    !cw$has_geometry & !cw$polity_code %in% known_status_defects,
  ]
  testthat::expect_true(all(no_geometry$polygon_status == "unassigned"))
  testthat::expect_setequal(
    intersect(cw$polity_code[!cw$has_geometry], known_status_defects),
    known_status_defects
  )

  for (data in list(whep::regions_full, whep::polities_cats)) {
    data <- data[!data$code %in% aggregate_codes, ]
    expect_polity_match(data, "code", "reporting_polity_code", FALSE)
    # `[[` rather than `$`: a partial-matching `$` on a renamed column returns
    # NULL, and `any(is.na(NULL))` is FALSE, so this assertion would pass
    # vacuously instead of failing (which is what it did through whep#687's
    # rename until it was noticed).
    testthat::expect_false(any(is.na(data[["legacy_polity_prefix"]])))
    coded_rows <- !is.na(data$code)
    testthat::expect_true(all(data$reporting_polity_has_geometry[coded_rows]))
  }
})
