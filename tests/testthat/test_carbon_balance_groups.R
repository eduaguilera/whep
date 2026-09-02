# The carbon balance marching crop GROUPS as classes (Spain_Hist convention).
# Offline throughout.

testthat::test_that("cover profiles map groups without enumerating them", {
  x <- c(
    "cropland",
    "cropland_rainfed_herbaceous",
    "cropland_irrigated_herbaceous",
    "cropland_rainfed_olive",
    "cropland_irrigated_peaches_nectarines",
    "grassland",
    "natural",
    "urban"
  )
  testthat::expect_identical(
    whep:::.cb_cover_profile(x),
    c(
      "cropland",
      "cropland",
      "cropland",
      "woody_cropland",
      "woody_cropland",
      "grassland",
      "natural",
      "urban"
    )
  )
})

testthat::test_that("the curve carries a perennial woody-cropland profile", {
  # An ASSUMED value: no sourced constant exists in the repo, so woody groups
  # take the perennial 0.85 grassland/natural already use, flagged in NEWS
  # as an assumption to replace. It must at least exist in every month, or a
  # woody group would silently run bare.
  curve <- whep::soc_soil_cover_curve
  woody <- curve[curve$land_use == "woody_cropland", ]
  testthat::expect_identical(nrow(woody), 12L)
  testthat::expect_true(all(woody$soil_cover == 0.85))
  testthat::expect_setequal(woody$months_from_peak, -5:6)
})

.cbg_drivers <- function() {
  tidyr::expand_grid(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    year = 2010L,
    month = 1:12
  ) |>
    dplyr::mutate(
      temp_c = 22,
      precip_mm = 90,
      pet_mm = 80,
      clay_pct = 25,
      water_minus_pet_mm = precip_mm - pet_mm
    )
}

testthat::test_that("crossing by profile reproduces the per-class cover", {
  classes <- c("cropland", "grassland", "natural")
  out <- whep:::.cb_attach_soil_cover(.cbg_drivers(), classes)
  # One row per class-month, every class present, and the class rows equal
  # the curve rows for that class.
  testthat::expect_identical(nrow(out), 36L)
  testthat::expect_setequal(unique(out$land_use), classes)
  curve <- whep::soc_soil_cover_curve
  for (cl in classes) {
    got <- out[out$land_use == cl, ]
    testthat::expect_setequal(
      round(got$soil_cover, 6),
      round(curve$soil_cover[curve$land_use == cl], 6)
    )
  }
})

testthat::test_that("groups sharing a profile share its cover rows", {
  classes <- c(
    "cropland_rainfed_herbaceous",
    "cropland_irrigated_herbaceous",
    "cropland_rainfed_olive",
    "natural"
  )
  out <- whep:::.cb_attach_soil_cover(.cbg_drivers(), classes)
  testthat::expect_identical(nrow(out), 48L)
  rf <- out[out$land_use == "cropland_rainfed_herbaceous", ]
  ir <- out[out$land_use == "cropland_irrigated_herbaceous", ]
  plain <- whep:::.cb_attach_soil_cover(.cbg_drivers(), "cropland")
  testthat::expect_equal(
    rf$soil_cover[order(rf$month)],
    plain$soil_cover[order(plain$month)]
  )
  testthat::expect_equal(
    ir$soil_cover[order(ir$month)],
    plain$soil_cover[order(plain$month)]
  )
  ol <- out[out$land_use == "cropland_rainfed_olive", ]
  testthat::expect_true(all(ol$soil_cover == 0.85))
})

testthat::test_that("the crop-calendar override reaches herbaceous groups only", {
  cover <- tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    year = 2010L,
    month = 1:12,
    cropland_cover = seq(0, 1, length.out = 12)
  )
  classes <- c("cropland_rainfed_herbaceous", "cropland_rainfed_olive")
  out <- whep:::.cb_attach_soil_cover(
    .cbg_drivers(),
    classes,
    cropland_cover = cover
  )
  rf <- out[out$land_use == "cropland_rainfed_herbaceous", ]
  testthat::expect_equal(rf$soil_cover[order(rf$month)], cover$cropland_cover)
  ol <- out[out$land_use == "cropland_rainfed_olive", ]
  testthat::expect_true(all(ol$soil_cover == 0.85))
})

testthat::test_that("the C:N lookup treats every crop group as cropland", {
  testthat::expect_identical(
    whep:::.cb_cropland_class(c(
      "cropland",
      "cropland_irrigated_herbaceous",
      "cropland_rainfed_olive",
      "grassland"
    )),
    c("Cropland", "Cropland", "Cropland", "NonCropland")
  )
})

testthat::test_that("LUH2 cropland is split over groups in proportion to area", {
  land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "cropland", 100,
    0.25, 0.25, 1L, 2000L, "natural", 300,
    0.75, 0.25, 1L, 2000L, "cropland", 50
  )
  c_inputs <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~c_input_mgc_ha_yr,
    ~humified_fraction, ~group_area_ha,
    0.25, 0.25, 1L, 2000L, "cropland_rainfed_herbaceous", 2, 0.2, 30,
    0.25, 0.25, 1L, 2000L, "cropland_rainfed_olive", 4, 0.3, 10,
    0.25, 0.25, 1L, 2000L, "natural", 1, 0.3, NA
  )
  out <- whep:::.cb_split_cropland_groups(land_use, c_inputs)
  cell_a <- out[out$lon == 0.25, ]
  # 100 ha of LUH2 cropland split 30:10 -> 75 and 25; LUH2's total is kept,
  # not the crop-pattern basis' 40.
  testthat::expect_setequal(
    cell_a$land_use,
    c("cropland_rainfed_herbaceous", "cropland_rainfed_olive", "natural")
  )
  testthat::expect_equal(
    cell_a$area_ha[cell_a$land_use == "cropland_rainfed_herbaceous"],
    75
  )
  testthat::expect_equal(
    cell_a$area_ha[cell_a$land_use == "cropland_rainfed_olive"],
    25
  )
  testthat::expect_equal(sum(cell_a$area_ha), 400)
  # A cell with cropland but no grouped inputs keeps its plain row.
  cell_b <- out[out$lon == 0.75, ]
  testthat::expect_identical(cell_b$land_use, "cropland")
  testthat::expect_equal(cell_b$area_ha, 50)
})

testthat::test_that("an ungrouped run leaves the land-use table untouched", {
  land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "cropland", 100,
    0.25, 0.25, 1L, 2000L, "natural", 300
  )
  c_inputs <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~c_input_mgc_ha_yr,
    ~humified_fraction,
    0.25, 0.25, 1L, 2000L, "cropland", 2, 0.2,
    0.25, 0.25, 1L, 2000L, "natural", 1, 0.3
  )
  testthat::expect_identical(
    whep:::.cb_split_cropland_groups(land_use, c_inputs),
    land_use
  )
})

testthat::test_that("build_carbon_balance rejects a malformed crop_groups", {
  testthat::expect_error(
    whep::build_carbon_balance(crop_groups = list(method = "spainhist")),
    "crop_groups"
  )
})
