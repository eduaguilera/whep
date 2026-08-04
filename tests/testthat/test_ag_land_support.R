# Tests for build_ag_land_support(), the native producer of the agricultural
# land support the gridded nitrogen balance allocates its non-crop inputs over.

.alsf_cell_polity <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~polity_frac,
    0.25,
    50.25,
    10L,
    1,
    0.75,
    50.25,
    10L,
    0.6,
    0.75,
    50.25,
    20L,
    0.4
  )
}

# Two LUH2 cropland classes in one cell: 600 + 400 = 1000 physical hectares.
.alsf_type_cropland <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~luh2_type,
    ~type_ha,
    0.25,
    50.25,
    2010L,
    "c3ann",
    600,
    0.25,
    50.25,
    2010L,
    "c3nfx",
    400,
    0.75,
    50.25,
    2010L,
    "c3ann",
    500
  )
}

# Harvest fractions summing to 1.2 in the first cell: a multicropping pattern
# that must apportion, never inflate, the physical hectares.
.alsf_crop_patterns <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~item_prod_code,
    ~harvest_fraction,
    0.25,
    50.25,
    15L,
    0.9,
    0.25,
    50.25,
    44L,
    0.3,
    0.75,
    50.25,
    15L,
    0.5
  )
}

.alsf_gridded_pasture <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~pasture_ha,
    ~rangeland_ha,
    0.25,
    50.25,
    2010L,
    900,
    100,
    0.75,
    50.25,
    2010L,
    400,
    600
  )
}

.alsf_states <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~land_use,
    ~fraction,
    0.25,
    50.25,
    2010L,
    "pastr",
    0.01,
    0.75,
    50.25,
    2010L,
    "range",
    0.02
  )
}

.alsf_empty_grassland <- function() {
  tibble::tibble(
    lon = double(),
    lat = double(),
    area_code = integer(),
    year = integer(),
    area_ha = double()
  )
}

.alsf_data <- function() {
  list(
    cell_polity = .alsf_cell_polity(),
    type_cropland = .alsf_type_cropland(),
    crop_patterns = .alsf_crop_patterns(),
    gridded_pasture = .alsf_gridded_pasture(),
    states = .alsf_states()
  )
}

testthat::test_that("the example fixture matches the documented contract", {
  out <- whep::build_ag_land_support(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "item_cbs_code",
      "year",
      "land_use",
      "area_ha"
    )
  )
  testthat::expect_true(all(out$area_ha > 0))
  testthat::expect_setequal(out$land_use, c("cropland", "grassland"))
})

testthat::test_that("physical cropland area is apportioned, never inflated", {
  out <- whep::build_ag_land_support(data = .alsf_data())
  cropland <- dplyr::filter(out, .data$land_use == "cropland")
  # 1000 ha (whole) + 500 ha (split 0.6/0.4 across two polities) = 1500 ha,
  # NOT the 1200 + 250 the raw harvest fractions would give.
  testthat::expect_equal(sum(cropland$area_ha), 1500)
  cell_one <- dplyr::filter(cropland, .data$lon == 0.25)
  testthat::expect_equal(sum(cell_one$area_ha), 1000)
})

testthat::test_that("the crop pattern sets the composition of the cell", {
  out <- whep::build_ag_land_support(data = .alsf_data())
  cell_one <- out |>
    dplyr::filter(.data$lon == 0.25, .data$land_use == "cropland") |>
    dplyr::arrange(.data$item_cbs_code)
  # 0.9 wheat / 0.3 barley normalise to 0.75 / 0.25 of the 1000 physical ha.
  testthat::expect_equal(cell_one$item_cbs_code, c(2511L, 2513L))
  testthat::expect_equal(cell_one$area_ha, c(750, 250))
})

testthat::test_that("a border cell is split by polity_frac", {
  out <- whep::build_ag_land_support(data = .alsf_data())
  border <- out |>
    dplyr::filter(.data$lon == 0.75, .data$land_use == "cropland") |>
    dplyr::arrange(.data$area_code)
  testthat::expect_equal(border$area_code, c(10L, 20L))
  testthat::expect_equal(border$area_ha, c(300, 200))
})

testthat::test_that("grassland support is all carried on CBS 3000", {
  out <- whep::build_ag_land_support(data = .alsf_data())
  grass <- dplyr::filter(out, .data$land_use == "grassland") |>
    dplyr::arrange(.data$lon, .data$area_code)
  testthat::expect_setequal(grass$item_cbs_code, 3000L)
  # pasture_ha + rangeland_ha, split by polity_frac: 1000 whole, then 1000
  # shared 60/40. Pasture and rangeland are pooled, no class inferred.
  testthat::expect_equal(grass$area_ha, c(1000, 600, 400))
})

testthat::test_that("the luh2 grassland source stays selectable", {
  out <- whep::build_ag_land_support(
    grassland = "luh2",
    data = .alsf_data()
  )
  grass <- dplyr::filter(out, .data$land_use == "grassland")
  testthat::expect_setequal(grass$item_cbs_code, 3000L)
  # 0.01 of a cell at 50.25 degrees, plus 0.02 of the border cell split 60/40.
  # The absolute areas follow the spherical cell-area formula, not the
  # gridded_pasture hectares the default source reads.
  testthat::expect_equal(
    sum(grass$area_ha),
    0.03 * whep:::.luh2_cell_area_ha(50.25),
    tolerance = 1e-8
  )
})

testthat::test_that("grassland = 'none' returns cropland-only support", {
  out <- whep::build_ag_land_support(grassland = "none", data = .alsf_data())
  testthat::expect_setequal(out$land_use, "cropland")
})

testthat::test_that("years filter the support without touching composition", {
  data <- .alsf_data()
  data$type_cropland <- dplyr::bind_rows(
    data$type_cropland,
    tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      year = 2011L,
      luh2_type = "c3ann",
      type_ha = 800
    )
  )
  data$gridded_pasture <- dplyr::bind_rows(
    data$gridded_pasture,
    tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      year = 2011L,
      pasture_ha = 700,
      rangeland_ha = 0
    )
  )
  out <- whep::build_ag_land_support(years = 2011L, data = data)
  testthat::expect_setequal(out$year, 2011L)
  testthat::expect_equal(
    sum(out$area_ha[out$land_use == "cropland"]),
    800
  )
})

testthat::test_that("years without grassland coverage warn and keep cropland", {
  data <- .alsf_data()
  data$grassland_ha <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    year = 1999L,
    area_ha = 100
  )
  data$states <- NULL
  testthat::expect_warning(
    out <- whep::build_ag_land_support(data = data),
    "grassland support"
  )
  testthat::expect_setequal(out$land_use, "cropland")
})

testthat::test_that("cropland cells with no pattern warn instead of vanishing", {
  data <- .alsf_data()
  data$type_cropland <- tibble::tibble(
    lon = 10.25,
    lat = 50.25,
    year = 2010L,
    luh2_type = "c3ann",
    type_ha = 900
  )
  data$states <- NULL
  data$grassland_ha <- .alsf_empty_grassland()
  testthat::expect_warning(
    out <- whep::build_ag_land_support(data = data),
    "crop-pattern composition"
  )
  testthat::expect_equal(nrow(out), 0L)
})
