# Tests for build_carbon_inputs() (Module B, Task B2c-4): assemble the
# per-land-use-class carbon-input layer build_carbon_balance() consumes, by
# aggregating the per-crop cropland inputs (build_soil_carbon_inputs) to the
# cropland class and row-binding the grassland and natural inputs
# (build_grass_natural_carbon_inputs). All densities in MgC/ha/yr.

# -- Fixtures -----------------------------------------------------------------

# Per-crop cropland soil carbon inputs (build_soil_carbon_inputs grid output):
# two crops in one cell, differing per-ha C and humified fraction.
.ci_cropland_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year,
    ~residue_c_mgc_ha_yr, ~root_c_mgc_ha_yr, ~manure_c_mgc_ha_yr,
    ~total_c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    0.25, 0.25, 1L, "15", 2000L, 1.5, 1.0, 0.5, 3.0, 0.20, "humified_weighted",
    0.25, 0.25, 1L, "27", 2000L, 1.0, 0.5, 0.5, 2.0, 0.10, "humified_weighted"
  )
}

# Per cell-crop harvested area (ha), used to area-weight the crop densities up
# to the cropland class. Static (time-invariant), matching the crop_patterns
# spatialization, so it carries no year key.
.ci_crop_area_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~crop_area_ha,
    0.25, 0.25, 1L, "15", 30,
    0.25, 0.25, 1L, "27", 10
  )
}

# Grassland + natural class inputs (build_grass_natural_carbon_inputs grid).
.ci_grass_natural_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use,
    ~c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    0.25, 0.25, 1L, 2000L, "grassland", 4.0, 0.1153467, "lpjml_npp_minus_harvest",
    0.25, 0.25, 1L, 2000L, "natural", 6.0, 0.325, "lpjml_npp_minus_harvest"
  )
}

.ci_test_data <- function() {
  list(
    cropland = .ci_cropland_fixture(),
    crop_area = .ci_crop_area_fixture(),
    grass_natural = .ci_grass_natural_fixture()
  )
}

# -- Cropland aggregation -----------------------------------------------------

test_that("cropland class = area-weighted per-ha C, C-weighted humification", {
  out <- whep::build_carbon_inputs(resolution = "grid", data = .ci_test_data())
  crop <- dplyr::filter(out, land_use == "cropland")
  # Area-weighted per-ha C: (3.0*30 + 2.0*10) / (30+10) = 110/40 = 2.75.
  testthat::expect_equal(crop$c_input_mgc_ha_yr, 2.75, tolerance = 1e-9)
  # C-weighted humified fraction: weight by carbon MASS (C/ha * area):
  # mass15 = 3.0*30 = 90, mass27 = 2.0*10 = 20. hf = (90*0.2 + 20*0.1)/110.
  expected_hf <- (90 * 0.20 + 20 * 0.10) / 110
  testthat::expect_equal(crop$humified_fraction, expected_hf, tolerance = 1e-9)
})

test_that("output carries every class with the balance c_inputs schema", {
  out <- whep::build_carbon_inputs(resolution = "grid", data = .ci_test_data())
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "land_use",
      "c_input_mgc_ha_yr",
      "humified_fraction"
    )
  )
  testthat::expect_setequal(
    unique(out$land_use),
    c("cropland", "grassland", "natural")
  )
  # Grassland + natural pass through build_grass_natural values unchanged.
  gr <- dplyr::filter(out, land_use == "grassland")
  testthat::expect_equal(gr$c_input_mgc_ha_yr, 4.0, tolerance = 1e-9)
})

test_that("build_carbon_inputs output joins into build_carbon_balance", {
  # The assembled c_inputs must satisfy build_carbon_balance's join contract:
  # (lon, lat, area_code, year, land_use) with c_input_mgc_ha_yr +
  # humified_fraction. Build a matching land-use + climate + clay and run.
  c_inputs <- whep::build_carbon_inputs(
    resolution = "grid",
    data = .ci_test_data()
  )
  land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "cropland", 40,
    0.25, 0.25, 1L, 2000L, "grassland", 30,
    0.25, 0.25, 1L, 2000L, "natural", 30
  )
  climate <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000L,
    climate_modifier = 1
  )
  clay <- tibble::tibble(lon = 0.25, lat = 0.25, clay_pct = 20)
  cb <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = list(
      c_inputs = c_inputs,
      land_use = land_use,
      climate = climate,
      clay = clay
    )
  )
  testthat::expect_true(all(cb$stock_mgc_ha >= 0))
  testthat::expect_setequal(
    unique(cb$land_use),
    c("cropland", "grassland", "natural")
  )
})

test_that("polity resolution aggregates cropland conserving carbon mass", {
  grid <- whep::build_carbon_inputs(resolution = "grid", data = .ci_test_data())
  pol <- whep::build_carbon_inputs(
    resolution = "polity",
    data = .ci_test_data()
  )
  # Single cell so the cropland per-ha density is identical at both grains.
  grid_crop <- dplyr::filter(grid, land_use == "cropland")$c_input_mgc_ha_yr
  pol_crop <- dplyr::filter(pol, land_use == "cropland")$c_input_mgc_ha_yr
  testthat::expect_equal(pol_crop, grid_crop, tolerance = 1e-9)
  testthat::expect_false(rlang::has_name(pol, "lon"))
})

test_that("example = TRUE returns the documented schema", {
  out <- whep::build_carbon_inputs(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "land_use",
      "c_input_mgc_ha_yr",
      "humified_fraction",
      "method_c_input"
    )
  )
  testthat::expect_true(all(
    out$land_use %in% c("cropland", "grassland", "natural")
  ))
})
