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

# The crop allocation the composition comes from (R/crop_allocation.R): in
# polity 10, wheat (15) on both cells 3:1 and barley (44) on the first only;
# in polity 20, wheat on the border cell. With the national harvested areas
# below the first cell holds 900 ha of wheat and 300 of barley -- 1200 ha of
# harvested area on 1000 physical ha, a multicropping cell that must
# apportion, never inflate, the physical hectares. 277 is the out-of-span
# fixture's polity.
.alsf_allocation <- function(years = c(2010L, 2011L)) {
  weights <- tibble::tribble(
    ~lon, ~lat,  ~area_code, ~item_prod_code, ~crop_area_ha, ~area_weight,
    0.25, 50.25, 10L,        15L,             900,           0.75,
    0.75, 50.25, 10L,        15L,             300,           0.25,
    0.25, 50.25, 10L,        44L,             300,           1,
    0.75, 50.25, 20L,        15L,             100,           1,
    0.25, 50.25, 277L,       15L,             900,           1
  )
  fallback <- tibble::tribble(
    ~lon, ~lat,  ~area_code, ~cropland_weight,
    0.25, 50.25, 10L,        0.8,
    0.75, 50.25, 10L,        0.2,
    0.75, 50.25, 20L,        1,
    0.25, 50.25, 277L,       1
  )
  list(
    weights = tidyr::expand_grid(weights, year = years),
    fallback = tidyr::expand_grid(fallback, year = years)
  )
}

.alsf_harvested_area <- function(years = c(2010L, 2011L)) {
  tibble::tribble(
    ~area_code, ~item_prod_code, ~faostat_area_ha,
    10L,        15L,             1200,
    10L,        44L,             300,
    20L,        15L,             100,
    277L,       15L,             900
  ) |>
    tidyr::expand_grid(year = years)
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
    crop_allocation = .alsf_allocation(),
    harvested_area = .alsf_harvested_area(),
    gridded_pasture = .alsf_gridded_pasture(),
    states = .alsf_states()
  )
}

testthat::test_that("the example fixture matches the documented contract", {
  out <- whep::build_ag_land_support(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  # The reporting-polity columns are part of the contract (whep#424), and they
  # lead the frame because that is where the attach helper puts the key.
  testthat::expect_named(
    out,
    c(
      "year",
      "area_code",
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry",
      "lon",
      "lat",
      "item_cbs_code",
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
  # NOT the 1200 + 400 harvested hectares the allocation places there.
  testthat::expect_equal(sum(cropland$area_ha), 1500)
  cell_one <- dplyr::filter(cropland, .data$lon == 0.25)
  testthat::expect_equal(sum(cell_one$area_ha), 1000)
})

testthat::test_that("the crop allocation sets the composition of the cell", {
  out <- whep::build_ag_land_support(data = .alsf_data())
  cell_one <- out |>
    dplyr::filter(.data$lon == 0.25, .data$land_use == "cropland") |>
    dplyr::arrange(.data$item_cbs_code)
  # 900 ha wheat / 300 ha barley normalise to 0.75 / 0.25 of the 1000
  # physical ha.
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

testthat::test_that("cropland cells with no allocated crop warn, not vanish", {
  data <- .alsf_data()
  # Cropland in the first cell, where the allocation places no crop of
  # polity 10 and its cropland support holds no cell either.
  data$type_cropland <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = 2010L,
    luh2_type = "c3ann",
    type_ha = 900
  )
  data$cell_polity <- dplyr::filter(.alsf_cell_polity(), .data$lon == 0.25)
  data$crop_allocation <- list(
    weights = dplyr::filter(.alsf_allocation()$weights, .data$area_code == 20L),
    fallback = dplyr::filter(
      .alsf_allocation()$fallback,
      .data$area_code == 20L
    )
  )
  data$states <- NULL
  data$grassland_ha <- .alsf_empty_grassland()
  testthat::expect_warning(
    out <- whep::build_ag_land_support(data = data),
    "no crop in the crop allocation"
  )
  testthat::expect_equal(nrow(out), 0L)
})

# ---- polity_validity (#675) -------------------------------------------

# The same fixture re-keyed onto area 277 (South Sudan, SSD-2011-2025), whose
# polity postdates the fixture's 2010 year.
.alsf_out_of_span_data <- function() {
  data <- .alsf_data()
  data$cell_polity <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac,
    0.25, 50.25, 277L, 1
  )
  data
}

testthat::test_that("build_ag_land_support names an anachronistic polity", {
  testthat::expect_warning(
    out <- whep::build_ag_land_support(data = .alsf_out_of_span_data()),
    "did not exist in that row's year"
  )

  testthat::expect_gt(nrow(out), 0L)
  testthat::expect_true(all(out$reporting_polity_code == "SSD-2011-2025"))
})

testthat::test_that("build_ag_land_support honours drop and flag", {
  testthat::expect_warning(
    dropped <- whep::build_ag_land_support(
      data = .alsf_out_of_span_data(),
      polity_validity = "drop"
    )
  )
  testthat::expect_warning(
    flagged <- whep::build_ag_land_support(
      data = .alsf_out_of_span_data(),
      polity_validity = "flag"
    )
  )

  testthat::expect_equal(nrow(dropped), 0L)
  testthat::expect_true(all(flagged$reporting_polity_out_of_span))
})

testthat::test_that("build_ag_land_support is silent on an in-span polity", {
  testthat::expect_no_warning(
    whep::build_ag_land_support(data = .alsf_data())
  )
})
