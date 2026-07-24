# Hand-built fixtures keep the arithmetic checkable by inspection.

.sci_npp_fixture <- function() {
  # Two crops in one polity, one year. Carbon masses in tonnes C. residue_c_t
  # is the GROSS residue carbon; residue_soil_c_t is the soil-returned fraction
  # (here 40% removed for feed/fuel/burning) and is the value the soil-carbon
  # input must use. weed_npp_c_t is the combined AG + BG weed carbon.
  tibble::tribble(
    ~area_code, ~item_prod_code, ~year,
    ~residue_c_t, ~residue_soil_c_t, ~root_c_t, ~weed_npp_c_t,
    1L, "15", 2020L, 100, 60, 40, 10,
    1L, "27", 2020L, 50, 30, 10, 5
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
    "weed_c_mgc_ha_yr",
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

test_that("total C input equals residue + root + weed + manure (mass closure)", {
  out <- whep::build_soil_carbon_inputs(
    resolution = "grid",
    data = .sci_fixture_data()
  )
  testthat::expect_equal(
    out$total_c_input_mgc_ha_yr,
    out$residue_c_mgc_ha_yr +
      out$root_c_mgc_ha_yr +
      out$weed_c_mgc_ha_yr +
      out$manure_c_mgc_ha_yr
  )
})

test_that("polity per-ha equals total C mass over total crop area", {
  out <- whep::build_soil_carbon_inputs(
    resolution = "polity",
    data = .sci_fixture_data()
  )
  # Crop 15: residue-soil 60 + root 40 + weed 10 + manure 20 = 130 over 40 ha.
  # The residue term uses residue_soil_c_t (60), NOT gross residue_c_t (100).
  crop15 <- out[out$item_prod_code == "15", ]
  testthat::expect_equal(crop15$total_c_input_mgc_ha_yr, 130 / 40)
  testthat::expect_equal(crop15$residue_c_mgc_ha_yr, 60 / 40)
  testthat::expect_equal(crop15$root_c_mgc_ha_yr, 40 / 40)
  testthat::expect_equal(crop15$weed_c_mgc_ha_yr, 10 / 40)
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
  h_weed <- hum$humified_fraction[hum$input_type == "weed"]
  h_man <- hum$humified_fraction[hum$input_type == "manure"]
  # Crop 15: residue-soil 60, root 40, weed 10, manure 20 (Mg C). C-weighted.
  expected15 <- (60 * h_res + 40 * h_root + 10 * h_weed + 20 * h_man) /
    (60 + 40 + 10 + 20)
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

test_that("residue C uses residue_soil_c_t, not gross residue_c_t", {
  # 50% straw removal: gross residue_c_t = 100, residue_soil_c_t = 50. The soil
  # input must count 50, so residue_c_mgc_ha_yr = 50 / 40 ha = 1.25, NOT 2.5.
  npp <- tibble::tribble(
    ~area_code, ~item_prod_code, ~year,
    ~residue_c_t, ~residue_soil_c_t, ~root_c_t, ~weed_npp_c_t,
    1L, "15", 2020L, 100, 50, 0, 0
  )
  manure <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~applied_c,
    2020L, "1", NA, "Cropland", "15", 0
  )
  grid <- .sci_grid_fixture()
  data <- list(
    npp = npp,
    manure = manure,
    country_grid = grid$country_grid,
    crop_patterns = grid$crop_patterns,
    residue_humification = whep::residue_humification
  )
  out <- whep::build_soil_carbon_inputs(resolution = "polity", data = data)
  crop15 <- out[out$item_prod_code == "15", ]
  testthat::expect_equal(crop15$residue_c_mgc_ha_yr, 50 / 40)
})

test_that("manure territory as an iso3c resolves instead of dropping to NA", {
  esp_code <- whep::regions_full |>
    dplyr::filter(.data$iso3c == "ESP") |>
    dplyr::distinct(.data$code) |>
    dplyr::pull(.data$code)
  npp <- tibble::tribble(
    ~area_code, ~item_prod_code, ~year,
    ~residue_c_t, ~residue_soil_c_t, ~root_c_t, ~weed_npp_c_t,
    esp_code, "15", 2020L, 100, 60, 40, 10
  )
  manure <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~applied_c,
    2020L, "ESP", NA, "Cropland", "15", 20
  )
  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 0.25, esp_code, 1
  )
  crop_patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction, ~crop_area_ha,
    0.25, 0.25, "15", 1.0, 40
  )
  data <- list(
    npp = npp,
    manure = manure,
    country_grid = country_grid,
    crop_patterns = crop_patterns,
    residue_humification = whep::residue_humification
  )
  out <- whep::build_soil_carbon_inputs(resolution = "polity", data = data)
  # 20 t manure C over 40 ha = 0.5, not 0 (which a silent as.integer NA drop
  # would give).
  testthat::expect_equal(out$manure_c_mgc_ha_yr, 20 / 40)
})

test_that("manure territory that is neither area_code nor iso3c aborts", {
  data <- .sci_fixture_data()
  data$manure$territory <- "not_a_place"
  testthat::expect_error(
    whep::build_soil_carbon_inputs(resolution = "polity", data = data),
    "Could not resolve"
  )
})

test_that("manure crop names resolve case-insensitively to item_prod_code", {
  manure <- .sci_manure_fixture()
  manure$crop[1] <- "wHeAt"

  out <- whep:::.sci_manure_components(manure)

  wheat <- out[out$item_prod_code == "15", ]
  testthat::expect_equal(nrow(wheat), 1L)
  testthat::expect_equal(wheat$c_mass_mg, 20)
  # The other row proves existing item_prod_code strings remain valid.
  testthat::expect_setequal(out$item_prod_code, c("15", "27"))
})

test_that("unknown manure crop names abort instead of losing carbon", {
  manure <- .sci_manure_fixture()[1, ]
  manure$crop <- "not_a_known_crop"

  testthat::expect_error(
    whep:::.sci_manure_components(manure),
    "Could not resolve manure"
  )
})

test_that("npp missing residue_soil_c_t or weed_npp_c_t aborts", {
  data <- .sci_fixture_data()
  data$npp <- dplyr::select(data$npp, -"residue_soil_c_t")
  testthat::expect_error(
    whep::build_soil_carbon_inputs(resolution = "polity", data = data),
    "residue_soil_c_t"
  )
})

test_that("a crop with no crop-pattern cells warns and is not silent", {
  # Crop 27 has NPP carbon but is absent from crop_patterns (no cells): its
  # carbon must not vanish silently.
  data <- .sci_fixture_data()
  data$crop_patterns <- dplyr::filter(
    data$crop_patterns,
    .data$item_prod_code == "15"
  )
  testthat::expect_warning(
    out <- whep::build_soil_carbon_inputs(resolution = "polity", data = data),
    "no crop-pattern cells"
  )
  testthat::expect_setequal(out$item_prod_code, "15")
})

test_that("a crop with zero-only pattern area warns instead of producing NaN", {
  data <- .sci_fixture_data()
  data$crop_patterns <- data$crop_patterns |>
    dplyr::mutate(
      crop_area_ha = dplyr::if_else(
        .data$item_prod_code == "27",
        0,
        .data$crop_area_ha
      )
    )

  testthat::expect_warning(
    out <- whep::build_soil_carbon_inputs(resolution = "polity", data = data),
    "no crop-pattern cells"
  )
  testthat::expect_setequal(out$item_prod_code, "15")
  testthat::expect_true(all(is.finite(out$total_c_input_mgc_ha_yr)))
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
    "weed_c_mgc_ha_yr",
    "manure_c_mgc_ha_yr",
    "total_c_input_mgc_ha_yr",
    "humified_fraction",
    "method_c_input"
  )
  testthat::expect_true(all(expected %in% names(out)))
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_gt(nrow(out), 0L)
})

# -- Default input readers (wiring) -------------------------------------------

# A minimal get_primary_production()-shaped table: two crops (production +
# harvested area), one livestock row and one grassland row that must be dropped.
.sci_primary_prod_fixture <- function() {
  tibble::tribble(
    ~area_code, ~item_prod_code, ~item_cbs_code, ~live_anim_code,
    ~year, ~unit, ~value,
    203L, 15, 2511L, NA, 2000L, "tonnes", 100,
    203L, 15, 2511L, NA, 2000L, "ha", 40,
    203L, 27, 2807L, NA, 2000L, "tonnes", 50,
    203L, 27, 2807L, NA, 2000L, "ha", 20,
    203L, 866, 2731L, 866L, 2000L, "tonnes", 999,
    203L, 3000, 3000L, NA, 2000L, "ha", 999
  )
}

test_that(".sci_npp_from_primary_prod runs the crop chain to soil carbon", {
  out <- suppressWarnings(
    whep:::.sci_npp_from_primary_prod(.sci_primary_prod_fixture())
  )
  testthat::expect_setequal(
    names(out),
    c(
      "area_code",
      "item_prod_code",
      "year",
      "residue_soil_c_t",
      "root_c_t",
      "weed_npp_c_t"
    )
  )
  # Only the two crops survive (livestock and grassland dropped).
  testthat::expect_setequal(out$item_prod_code, c("15", "27"))
  # The crop chain returns no weed carbon (weeds need the components step).
  testthat::expect_true(all(out$weed_npp_c_t == 0))
  # Soil-returned residue and root carbon are positive for a real crop.
  wheat <- out[out$item_prod_code == "15", ]
  testthat::expect_gt(wheat$residue_soil_c_t, 0)
  testthat::expect_gt(wheat$root_c_t, 0)
})

test_that(".sci_combine_crop_patterns scales harvest_fraction by cropland", {
  patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 0.25, "15", 0.6,
    0.25, 0.25, "27", 0.4,
    0.75, 0.25, "15", 1.0
  )
  cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 0.25, 2000L, 100,
    0.25, 0.25, 2001L, 200,
    0.75, 0.25, 2000L, 50
  )
  out <- whep:::.sci_combine_crop_patterns(patterns, cropland)
  testthat::expect_setequal(
    names(out),
    c("lon", "lat", "item_prod_code", "crop_area_ha")
  )
  # Cell (0.25, 0.25) mean cropland = 150: crop 15 = 90, crop 27 = 60.
  wheat_a <- out[out$lon == 0.25 & out$item_prod_code == "15", ]
  testthat::expect_equal(wheat_a$crop_area_ha, 90)
  rice_a <- out[out$lon == 0.25 & out$item_prod_code == "27", ]
  testthat::expect_equal(rice_a$crop_area_ha, 60)
  # Cell (0.75, 0.25) cropland 50: crop 15 = 50.
  wheat_b <- out[out$lon == 0.75, ]
  testthat::expect_equal(wheat_b$crop_area_ha, 50)
})

test_that(".sci_read_manure aborts with an actionable message", {
  testthat::expect_error(
    whep:::.sci_read_manure(),
    "turnkey"
  )
})
