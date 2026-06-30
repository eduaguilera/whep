# Hand-built fixtures keep the arithmetic checkable by inspection.

.sci_npp_fixture <- function() {
  # Two crops in one polity, one year. Carbon masses in tonnes C.
  tibble::tribble(
    ~area_code, ~item_prod_code, ~year, ~residue_c_t, ~root_c_t,
    1L, "15", 2020L, 60, 40,
    1L, "27", 2020L, 30, 10
  )
}

.sci_manure_fixture <- function() {
  # build_livestock_nutrient_flows()$applied shape. applied_c is tonnes C.
  # Cropland rows carry a crop name that maps to item_prod_code via the lookup.
  tibble::tribble(
    ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~applied_c,
    2020L, "1", NA, "Cropland", "15", 20,
    2020L, "1", NA, "Cropland", "27", 10
  )
}

.sci_grid_fixture <- function() {
  # Country grid: two cells, both fully in polity 1.
  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 0.25, 1L, 1,
    0.75, 0.25, 1L, 1
  )
  # Crop patterns carry the per-cell harvested area (ha) of each crop.
  # Crop 15: 30 ha in cell A, 10 ha in cell B (total 40).
  # Crop 27: 5 ha in cell A, 15 ha in cell B (total 20).
  crop_patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction, ~crop_area_ha,
    0.25, 0.25, "15", 0.75, 30,
    0.75, 0.25, "15", 0.25, 10,
    0.25, 0.25, "27", 0.25, 5,
    0.75, 0.25, "27", 0.75, 15
  )
  list(country_grid = country_grid, crop_patterns = crop_patterns)
}

.sci_fixture_data <- function() {
  grid <- .sci_grid_fixture()
  list(
    npp = .sci_npp_fixture(),
    manure = .sci_manure_fixture(),
    country_grid = grid$country_grid,
    crop_patterns = grid$crop_patterns,
    residue_humification = whep::residue_humification
  )
}

test_that("polity output has the documented schema and keys", {
  out <- whep::build_soil_carbon_inputs(
    resolution = "polity",
    data = .sci_fixture_data()
  )
  expected <- c(
    "area_code",
    "item_prod_code",
    "year",
    "residue_c_mgc_ha_yr",
    "root_c_mgc_ha_yr",
    "manure_c_mgc_ha_yr",
    "total_c_input_mgc_ha_yr",
    "humified_fraction",
    "method_c_input"
  )
  testthat::expect_true(all(expected %in% names(out)))
  testthat::expect_false(rlang::has_name(out, "lon"))
  # One row per (area_code, item_prod_code, year).
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_setequal(out$item_prod_code, c("15", "27"))
  testthat::expect_true(all(out$method_c_input == "humified_weighted"))
})

test_that("grid output is keyed by cell x crop x year", {
  out <- whep::build_soil_carbon_inputs(
    resolution = "grid",
    data = .sci_fixture_data()
  )
  testthat::expect_true(all(c("lon", "lat", "area_code") %in% names(out)))
  # 2 cells x 2 crops = 4 rows.
  testthat::expect_equal(nrow(out), 4L)
})

test_that("total C input equals residue + root + manure (mass closure)", {
  out <- whep::build_soil_carbon_inputs(
    resolution = "grid",
    data = .sci_fixture_data()
  )
  testthat::expect_equal(
    out$total_c_input_mgc_ha_yr,
    out$residue_c_mgc_ha_yr +
      out$root_c_mgc_ha_yr +
      out$manure_c_mgc_ha_yr
  )
})

test_that("polity per-ha equals total C mass over total crop area", {
  out <- whep::build_soil_carbon_inputs(
    resolution = "polity",
    data = .sci_fixture_data()
  )
  # Crop 15: residue 60 + root 40 + manure 20 = 120 Mg C over 40 ha = 3.0.
  crop15 <- out[out$item_prod_code == "15", ]
  testthat::expect_equal(crop15$total_c_input_mgc_ha_yr, 120 / 40)
  testthat::expect_equal(crop15$residue_c_mgc_ha_yr, 60 / 40)
  testthat::expect_equal(crop15$root_c_mgc_ha_yr, 40 / 40)
  testthat::expect_equal(crop15$manure_c_mgc_ha_yr, 20 / 40)
})

test_that("humified_fraction is the C-weighted mean of components", {
  out <- whep::build_soil_carbon_inputs(
    resolution = "polity",
    data = .sci_fixture_data()
  )
  hum <- whep::residue_humification
  h_res <- hum$humified_fraction[hum$input_type == "crop_residue"]
  h_root <- hum$humified_fraction[hum$input_type == "root"]
  h_man <- hum$humified_fraction[hum$input_type == "manure"]
  # Crop 15: residue 60, root 40, manure 20 (Mg C). C-weighted humified.
  expected15 <- (60 * h_res + 40 * h_root + 20 * h_man) / (60 + 40 + 20)
  crop15 <- out[out$item_prod_code == "15", ]
  testthat::expect_equal(crop15$humified_fraction, expected15)
})

test_that("grid to polity aggregation conserves total C mass", {
  grid <- whep::build_soil_carbon_inputs(
    resolution = "grid",
    data = .sci_fixture_data()
  )
  polity <- whep::build_soil_carbon_inputs(
    resolution = "polity",
    data = .sci_fixture_data()
  )
  # Recover per-cell C mass = per-ha x crop area, sum over cells per crop.
  cp <- .sci_grid_fixture()$crop_patterns
  grid_mass <- grid |>
    dplyr::left_join(
      dplyr::select(cp, lon, lat, item_prod_code, crop_area_ha),
      by = c("lon", "lat", "item_prod_code")
    ) |>
    dplyr::summarise(
      mass = sum(total_c_input_mgc_ha_yr * crop_area_ha),
      .by = c("area_code", "item_prod_code", "year")
    )
  polity_mass <- polity |>
    dplyr::transmute(
      area_code,
      item_prod_code,
      year,
      mass = total_c_input_mgc_ha_yr * 40 # 40 ha for crop 15, 20 for crop 27
    )
  # Compare by recomputing polity mass = per-ha x polity area.
  area_tot <- cp |>
    dplyr::summarise(area = sum(crop_area_ha), .by = "item_prod_code")
  polity_mass <- polity |>
    dplyr::left_join(area_tot, by = "item_prod_code") |>
    dplyr::transmute(
      area_code,
      item_prod_code,
      year,
      mass = total_c_input_mgc_ha_yr * area
    )
  joined <- dplyr::inner_join(
    grid_mass,
    polity_mass,
    by = c("area_code", "item_prod_code", "year"),
    suffix = c("_grid", "_polity")
  )
  testthat::expect_equal(joined$mass_grid, joined$mass_polity)
})

test_that("example = TRUE returns the documented schema", {
  out <- whep::build_soil_carbon_inputs(example = TRUE)
  expected <- c(
    "lon",
    "lat",
    "area_code",
    "item_prod_code",
    "year",
    "residue_c_mgc_ha_yr",
    "root_c_mgc_ha_yr",
    "manure_c_mgc_ha_yr",
    "total_c_input_mgc_ha_yr",
    "humified_fraction",
    "method_c_input"
  )
  testthat::expect_true(all(expected %in% names(out)))
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_gt(nrow(out), 0L)
})
