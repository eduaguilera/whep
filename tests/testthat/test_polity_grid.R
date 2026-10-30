.polity_grid_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~polity_code, ~start_year, ~end_year,
    0.25, 0.25, "AAA-1900-1950", 1900L, 1950L,
    0.25, 0.25, "BBB-1900-1950", 1900L, 1950L,
    0.25, 0.25, "CCC-1950-2000", 1950L, 2000L
  ) |>
    dplyr::mutate(cell_area_ha = 100, land_area_ha = c(30, 70, 100))
}

testthat::test_that("a polity grid needs no source reporting code", {
  support <- .polity_grid_fixture()
  grid <- whep::read_level_country_grid(
    support = support,
    geographic_key = "polity"
  )
  testthat::expect_setequal(grid$polity_code, support$polity_code)
  testthat::expect_false("area_code" %in% names(grid))
  testthat::expect_equal(grid$land_area_ha, support$land_area_ha)
  totals <- grid |>
    dplyr::summarise(total = sum(cell_area_frac), .by = start_year)
  testthat::expect_equal(totals$total, c(1, 1))
  testthat::expect_equal(grid$cell_area_frac, c(0.3, 0.7, 1))
})

testthat::test_that("shared source buckets do not merge distinct polities", {
  support <- dplyr::mutate(.polity_grid_fixture(), area_code = 999L)
  grid <- whep::read_level_country_grid(
    support = support,
    geographic_key = "polity",
    reference_year = 1949L
  )
  testthat::expect_setequal(
    grid$polity_code,
    c("AAA-1900-1950", "BBB-1900-1950")
  )
  next_grid <- whep::read_level_country_grid(
    support = support,
    geographic_key = "polity",
    reference_year = 1950L
  )
  testthat::expect_identical(next_grid$polity_code, "CCC-1950-2000")
})

testthat::test_that("native grid refuses missing identity and duplicate claims", {
  support <- .polity_grid_fixture()
  support$polity_code[1] <- NA_character_
  testthat::expect_error(
    whep::read_level_country_grid(support = support, geographic_key = "polity"),
    class = "whep_polity_grid_identity"
  )
  duplicate <- dplyr::bind_rows(.polity_grid_fixture(), .polity_grid_fixture())
  testthat::expect_error(
    whep::read_level_country_grid(
      support = duplicate,
      geographic_key = "polity"
    ),
    class = "whep_polity_grid_duplicate"
  )
})

testthat::test_that("native grid refuses measured overclaim and invalid areas", {
  support <- .polity_grid_fixture()
  support$land_area_ha[1] <- 40
  testthat::expect_error(
    whep::read_level_country_grid(support = support, geographic_key = "polity"),
    class = "whep_polity_grid_overclaim"
  )
  support$land_area_ha[1] <- NA_real_
  testthat::expect_error(
    whep::read_level_country_grid(support = support, geographic_key = "polity"),
    class = "whep_polity_grid_area"
  )
})

testthat::test_that("native grid refuses unsupported vintage and depth", {
  testthat::expect_error(
    whep::read_level_country_grid(
      support = .polity_grid_fixture(),
      geographic_key = "polity",
      level = 1L
    ),
    class = "whep_polity_grid_mode"
  )
  testthat::expect_error(
    whep::read_level_country_grid(
      support = .polity_grid_fixture(),
      geographic_key = "polity",
      grid_vintage = "snapshot_2015"
    ),
    class = "whep_polity_grid_mode"
  )
})

testthat::test_that("native reader rejects overlap roles and partial epochs", {
  support <- dplyr::mutate(.polity_grid_fixture(), support_role = "overlap")
  testthat::expect_error(
    whep::read_level_country_grid(support = support, geographic_key = "polity"),
    class = "whep_polity_grid_role"
  )
  support <- .polity_grid_fixture()
  support$start_year[2] <- 1920L
  testthat::expect_error(
    whep::read_level_country_grid(support = support, geographic_key = "polity"),
    class = "whep_level0_overlapping_epochs"
  )
})

testthat::test_that("native default reader is injectable and empty data refuse", {
  testthat::local_mocked_bindings(
    read_polycell_support = function(...) .polity_grid_fixture(),
    .package = "whep"
  )
  grid <- whep::read_level_country_grid(geographic_key = "polity")
  testthat::expect_equal(nrow(grid), 3L)
  testthat::expect_error(
    whep::read_level_country_grid(
      support = .polity_grid_fixture()[0, ],
      geographic_key = "polity"
    ),
    class = "whep_polity_grid_area"
  )
})

testthat::test_that("native grid feeds crop allocation without numeric codes", {
  grid <- whep::read_level_country_grid(
    support = .polity_grid_fixture(),
    geographic_key = "polity"
  )
  targets <- tibble::tribble(
    ~year, ~polity_code, ~item_prod_code, ~harvested_area_ha,
    1949L, "AAA-1900-1950", 15L, 20,
    1949L, "BBB-1900-1950", 15L, 40,
    1950L, "CCC-1950-2000", 15L, 60
  )
  pattern <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 0.25, 15L, 1
  )
  land <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 0.25, 1949L, 100,
    0.25, 0.25, 1950L, 100
  )
  result <- whep::build_gridded_landuse(targets, pattern, land, grid)
  testthat::expect_false("area_code" %in% names(result))
  totals <- result |>
    dplyr::summarise(
      allocated = sum(rainfed_ha + irrigated_ha),
      .by = c("polity_code", "year")
    ) |>
    dplyr::left_join(targets, by = c("polity_code", "year"))
  testthat::expect_equal(nrow(totals), nrow(targets))
  testthat::expect_setequal(totals$polity_code, targets$polity_code)
  testthat::expect_equal(totals$allocated, totals$harvested_area_ha)
})

testthat::test_that("native periods exclude even the last end year", {
  grid <- whep::read_level_country_grid(
    support = .polity_grid_fixture(),
    geographic_key = "polity",
    reference_year = 2000L
  )
  testthat::expect_equal(nrow(grid), 0L)
})

testthat::test_that("native mode validates arguments before dispatch", {
  testthat::expect_error(
    whep::read_level_country_grid(
      support = .polity_grid_fixture(),
      geographic_key = "polity",
      level = 0.5
    )
  )
  testthat::expect_error(
    whep::read_level_country_grid(
      support = .polity_grid_fixture(),
      geographic_key = "polity",
      grid_vintage = c("year_aware", "invalid")
    )
  )
})
