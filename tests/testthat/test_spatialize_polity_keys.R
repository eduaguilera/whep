.polity_crop_fixture <- function() {
  list(
    country_areas = tibble::tribble(
      ~year, ~polity_code, ~item_prod_code, ~harvested_area_ha,
      1900L, "AAA-1800-1950", 15L, 30,
      1900L, "BBB-1800-1950", 15L, 70
    ),
    crop_patterns = tibble::tribble(
      ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 0.25, 15L, 1,
      0.75, 0.25, 15L, 1
    ),
    gridded_cropland = tibble::tribble(
      ~lon, ~lat, ~year, ~cropland_ha,
      0.25, 0.25, 1900L, 100,
      0.75, 0.25, 1900L, 100
    ),
    country_grid = tibble::tribble(
      ~lon, ~lat, ~polity_code, ~cell_area_frac, ~start_year, ~end_year,
      0.25, 0.25, "AAA-1800-1950", 1, 1800L, 1950L,
      0.75, 0.25, "BBB-1800-1950", 1, 1800L, 1950L
    )
  )
}

.run_polity_crop <- function(fixture, config = list()) {
  do.call(whep::build_gridded_landuse, c(fixture, list(config = config)))
}

testthat::test_that("crop allocation uses polity identity without FAOSTAT", {
  out <- .run_polity_crop(.polity_crop_fixture())
  testthat::expect_setequal(
    out$polity_code,
    c("AAA-1800-1950", "BBB-1800-1950")
  )
  testthat::expect_equal(out$rainfed_ha, c(30, 70))
  testthat::expect_equal(out$lon, c(0.25, 0.75))
  testthat::expect_false("area_code" %in% names(out))
})

testthat::test_that("a shared numeric bucket cannot merge distinct polities", {
  fix <- .polity_crop_fixture()
  fix$country_areas$area_code <- 999L
  fix$country_grid$area_code <- 999L
  testthat::expect_equal(
    .run_polity_crop(fix),
    .run_polity_crop(.polity_crop_fixture())
  )
})

testthat::test_that("native identity refuses unresolved and ambiguous inputs", {
  fix <- .polity_crop_fixture()
  fix$country_areas$polity_code[1] <- NA_character_
  testthat::expect_error(.run_polity_crop(fix), class = "whep_polity_key")
  fix <- .polity_crop_fixture()
  fix$country_areas <- dplyr::bind_rows(
    fix$country_areas,
    fix$country_areas[1, ]
  )
  testthat::expect_error(.run_polity_crop(fix), class = "whep_polity_key")
  fix <- .polity_crop_fixture()
  fix$country_grid$area_code <- 1L
  fix$country_grid$polity_code <- NULL
  testthat::expect_error(.run_polity_crop(fix), class = "whep_polity_key")
})

testthat::test_that("a missing polity or wrong epoch cannot reassign targets", {
  fix <- .polity_crop_fixture()
  fix$country_grid$polity_code[1] <- "SUCCESSOR-1950-2025"
  testthat::expect_error(.run_polity_crop(fix), class = "whep_polity_support")
  fix <- .polity_crop_fixture()
  fix$country_grid$end_year[1] <- 1900L
  testthat::expect_error(.run_polity_crop(fix), class = "whep_polity_support")
})

testthat::test_that("capacity redistribution stays within the polity", {
  fix <- .polity_crop_fixture()
  fix$country_areas$harvested_area_ha <- c(150, 10)
  fix$country_grid <- dplyr::bind_rows(
    fix$country_grid,
    dplyr::mutate(fix$country_grid[1, ], lon = 1.25)
  )
  fix$gridded_cropland <- dplyr::bind_rows(
    fix$gridded_cropland,
    tibble::tibble(lon = 1.25, lat = 0.25, year = 1900L, cropland_ha = 100)
  )
  fix$crop_patterns <- dplyr::bind_rows(
    fix$crop_patterns,
    tibble::tibble(
      lon = 1.25,
      lat = 0.25,
      item_prod_code = 15L,
      harvest_fraction = 0.1
    )
  )
  testthat::expect_warning(out <- .run_polity_crop(fix), "capacity")
  sums <- dplyr::summarise(out, total = sum(rainfed_ha), .by = polity_code)
  testthat::expect_equal(
    sums$total[match(fix$country_areas$polity_code, sums$polity_code)],
    c(150, 10)
  )
  testthat::expect_equal(out$rainfed_ha[out$polity_code == "BBB-1800-1950"], 10)
  # The existing capacity ceiling is soft; this migration must retain its
  # redistribution and national conservation without crossing polity borders.
  testthat::expect_gt(out$rainfed_ha[out$lon == 1.25], 150 * 0.1 / 1.1)
})


testthat::test_that("native CFT aggregation retains independent polity totals", {
  fix <- .polity_crop_fixture()
  out <- .run_polity_crop(
    fix,
    list(
      cft_mapping = tibble::tribble(
    ~item_prod_code, ~cft_name,
    15L, "cereal"
  )
    )
  )
  testthat::expect_setequal(out$polity_code, fix$country_areas$polity_code)
  testthat::expect_equal(out$rainfed_ha, c(30, 70))
  testthat::expect_equal(out$cft_name, rep("cereal", 2))
})

testthat::test_that("native inputs reject duplicate support and bucket folding", {
  fix <- .polity_crop_fixture()
  fix$country_grid <- dplyr::bind_rows(fix$country_grid, fix$country_grid[1, ])
  testthat::expect_error(.run_polity_crop(fix), class = "whep_polity_key")
  testthat::expect_error(
    .run_polity_crop(.polity_crop_fixture(), list(area_key = "polity_area")),
    class = "whep_polity_key"
  )
})

testthat::test_that("type constraints and pattern underflow use polity groups", {
  fix <- .polity_crop_fixture()
  fix$crop_patterns$harvest_fraction <- c(1e-10, 1)
  out <- suppressMessages(.run_polity_crop(
    fix,
    list(
      pattern_signal_floor = 1e-8,
      type_mapping = tibble::tribble(~item_prod_code, ~luh2_type, 15L, "c3ann"),
      type_cropland = tibble::tribble(
      ~lon, ~lat, ~year, ~luh2_type, ~type_ha, ~type_irrig_ha,
      0.25, 0.25, 1900L, "c3ann", 100, 0,
      0.75, 0.25, 1900L, "c3ann", 100, 0
    )
    )
  ))
  testthat::expect_equal(out$rainfed_ha, c(30, 70))
  testthat::expect_equal(out$polity_code, fix$country_areas$polity_code)
})

testthat::test_that("native crop support excludes its terminal end year", {
  fix <- .polity_crop_fixture()
  fix$country_areas$year <- 1950L
  fix$gridded_cropland$year <- 1950L
  testthat::expect_error(
    .run_polity_crop(fix),
    class = "whep_polity_support"
  )
})

testthat::test_that("native static and explicit annual grids preserve exact keys", {
  fix <- .polity_crop_fixture()
  fix$country_grid <- dplyr::select(fix$country_grid, -start_year, -end_year)
  static <- .run_polity_crop(fix)
  fix$country_grid$year <- 1900L
  testthat::expect_equal(.run_polity_crop(fix), static)
  fix$country_grid$year <- 1901L
  testthat::expect_error(.run_polity_crop(fix), class = "whep_polity_support")
})
