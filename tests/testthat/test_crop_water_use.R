# Per-crop monthly applied irrigation. Offline throughout: inputs injected.

.cwu_airrig_fixture <- function() {
  # Two cells, two bands, one month. Values are per-STAND mm.
  tibble::tribble(
    ~lon, ~lat, ~year, ~month, ~band, ~band_name, ~value,
    0.25, 0.25, 2010L, 7L, 19L, "irrigated maize", 100,
    0.25, 0.25, 2010L, 7L, 2L, "rainfed rice", 50,
    0.75, 0.25, 2010L, 7L, 19L, "irrigated maize", 200
  )
}

.cwu_stand_frac_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~band, ~band_name, ~value,
    0.25, 0.25, 2010L, 19L, "irrigated maize", 0.4,
    0.25, 0.25, 2010L, 2L, "rainfed rice", 0.1,
    0.75, 0.25, 2010L, 19L, "irrigated maize", 0.5
  )
}

.cwu_grid_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 0.25, 1L, 1,
    0.75, 0.25, 1L, 1
  )
}

.cwu_data <- function() {
  list(
    airrig_month = .cwu_airrig_fixture(),
    stand_frac = .cwu_stand_frac_fixture(),
    country_grid = .cwu_grid_fixture()
  )
}

testthat::test_that("both unit conventions are returned and consistent", {
  out <- whep::build_crop_water_use(data = .cwu_data())
  testthat::expect_true(all(
    c(
      "lon",
      "lat",
      "area_code",
      "cell_area_frac",
      "year",
      "month",
      "band",
      "band_name",
      "stand_frac",
      "airrig_stand_mm",
      "airrig_cell_mm"
    ) %in%
      names(out)
  ))
  testthat::expect_equal(
    out$airrig_cell_mm,
    out$airrig_stand_mm * out$stand_frac
  )
  maize_a <- out[out$lon == 0.25 & out$band_name == "irrigated maize", ]
  testthat::expect_equal(maize_a$airrig_stand_mm, 100)
  testthat::expect_equal(maize_a$airrig_cell_mm, 40)
})

testthat::test_that("rainfed bands are kept: paddy water is real water", {
  out <- whep::build_crop_water_use(data = .cwu_data())
  rice <- out[out$band_name == "rainfed rice", ]
  testthat::expect_identical(nrow(rice), 1L)
  testthat::expect_equal(rice$airrig_cell_mm, 5)
})

testthat::test_that("a stand absent from a cell yields no row", {
  d <- .cwu_data()
  # Rice loses its stand in cell A.
  d$stand_frac <- d$stand_frac[d$stand_frac$band_name != "rainfed rice", ]
  out <- whep::build_crop_water_use(data = d)
  testthat::expect_false("rainfed rice" %in% out$band_name)
})

testthat::test_that("summing airrig_cell_mm over crops gives the cell total", {
  out <- whep::build_crop_water_use(data = .cwu_data())
  cell_a <- out[out$lon == 0.25, ]
  # 100 * 0.4 + 50 * 0.1: the quantity the crop-less irrig cube carries.
  testthat::expect_equal(sum(cell_a$airrig_cell_mm), 45)
})

testthat::test_that("polity aggregation weights each convention properly", {
  out <- whep::build_crop_water_use(resolution = "polity", data = .cwu_data())
  maize <- out[out$band_name == "irrigated maize", ]
  # Intensity weighted by stand area: (100*0.4 + 200*0.5) / 0.9.
  testthat::expect_equal(maize$airrig_stand_mm, 140 / 0.9, tolerance = 1e-9)
  # Cell depth weighted by cell area: (40 + 100) / 2.
  testthat::expect_equal(maize$airrig_cell_mm, 70)
  testthat::expect_false(rlang::has_name(out, "lon"))
})

testthat::test_that("a malformed input is named", {
  d <- .cwu_data()
  d$airrig_month <- dplyr::select(d$airrig_month, -"month")
  testthat::expect_error(
    whep::build_crop_water_use(data = d),
    "airrig_month"
  )
})

testthat::test_that("the example fixture is consistent", {
  x <- whep::build_crop_water_use(example = TRUE)
  testthat::expect_identical(nrow(x), 12L)
  testthat::expect_equal(x$airrig_cell_mm, x$airrig_stand_mm * x$stand_frac)
})

testthat::test_that("a border cell fans out per polity, area split, mm kept", {
  d <- .cwu_data()
  d$country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 0.25, 1L, 0.7,
    0.25, 0.25, 2L, 0.3,
    0.75, 0.25, 1L, 1
  )
  out <- whep::build_crop_water_use(data = d)
  maize_a <- out[out$lon == 0.25 & out$band_name == "irrigated maize", ]
  # One row per polity sharing the cell; the density is NOT scaled.
  testthat::expect_identical(nrow(maize_a), 2L)
  testthat::expect_equal(maize_a$airrig_cell_mm, c(40, 40))
  testthat::expect_setequal(maize_a$cell_area_frac, c(0.7, 0.3))
})

testthat::test_that("each band carries its soil-carbon crop group, or NA", {
  testthat::expect_identical(
    whep:::.cwu_band_group(c(
      "irrigated maize",
      "rainfed temperate cereals",
      "irrigated others",
      "rainfed grassland",
      "irrigated biomass tree",
      "Rainfed Rice "
    )),
    c(
      "cropland_irrigated_herbaceous",
      "cropland_rainfed_herbaceous",
      NA_character_,
      NA_character_,
      NA_character_,
      "cropland_rainfed_herbaceous"
    )
  )
  # The labels are soc_crop_group()'s own, so the ledgers can be joined.
  testthat::expect_true(all(whep:::.soc_is_cropland(
    stats::na.omit(whep:::.cwu_band_group(c("irrigated maize", "rainfed rice")))
  )))
})

testthat::test_that("the group rides through grid and polity output", {
  ex <- whep::build_crop_water_use(example = TRUE)
  testthat::expect_true("crop_group" %in% names(ex))
  testthat::expect_identical(ex$crop_group[1], "cropland_irrigated_herbaceous")
})
