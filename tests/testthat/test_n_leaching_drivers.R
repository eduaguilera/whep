# build_n_loss_drivers(): the Meisinger and IMAGE-GNM leaching drivers.
# Offline: every layer is injected through `data` (helper_n_loss_drivers.R
# for the classes, the layers below for the rest).
#
# Cells A (9.25), B (9.75) and C (10.25) on 47.75 N, polity 79. LPJmL and the
# HWSD topsoil mass cover A and B only: C takes B's, 0.5 degrees of longitude
# away.

.nlx_years <- 2010L

.nlx_lpjml <- function(values) {
  tibble::tibble(
    lon = c(9.25, 9.75),
    lat = 47.75,
    year = .nlx_years,
    value = values
  )
}

.nlx_layers <- function() {
  list(
    lpjml_drainage = .nlx_lpjml(c(120, 300)),
    lpjml_runoff = .nlx_lpjml(c(200, 450)),
    soil_hydraulic = tibble::tribble(
      ~lon,  ~lat,  ~t_field, ~t_wilt, ~porosity,
      9.25,  47.75, 0.29,     0.14,    0.43,
      9.75,  47.75, 0.32,     0.12,    0.46,
      10.25, 47.75, 0.21,     0.09,    0.40
    ),
    soil_mass = tibble::tribble(
      ~lon, ~lat,  ~soil_mass_mg_ha, ~soc_obs_mgc_ha, ~method_soil_mass,
      9.25, 47.75, 4000,             60,              "measured",
      9.75, 47.75, 3600,             72,              "measured"
    ),
    carbon_balance = tibble::tribble(
      ~lon,  ~lat,  ~area_code, ~land_use,   ~year, ~stock_mgc_ha,
      ~c_input_mgc_ha, ~area_ha,
      9.25,  47.75, 79L,        "cropland",  2010L, 42,
      2,               80,
      9.75,  47.75, 79L,        "grassland", 2010L, 92,
      3,               500,
      9.75,  47.75, 79L,        "cropland",  2010L, 54,
      1.5,             20,
      10.25, 47.75, 79L,        "cropland",  2010L, 37,
      2.5,             40
    ),
    carbon_inputs = tibble::tribble(
      ~lon,  ~lat,  ~area_code, ~item_prod_code, ~year,
      ~total_c_input_mgc_ha_yr, ~crop_area_ha,
      9.25,  47.75, 79L,        56L,             2010L, 2.5, 80,
      9.25,  47.75, 79L,        27L,             2010L, 1.0, 10,
      9.75,  47.75, 79L,        27L,             2010L, 1.2, 15,
      10.25, 47.75, 79L,        56L,             2010L, 2.0, 40
    ),
    soil_texture_fao = tibble::tribble(
      ~lon, ~lat,  ~soil_texture_fao, ~method_soil_texture_fao,
      9.25, 47.75, "medium",          "hwsd_v1.2_fao3_clay_sand",
      9.75, 47.75, "fine",            "hwsd_v1.2_fao3_clay_sand"
    ),
    soil_drainage = tibble::tribble(
      ~lon,  ~lat,  ~soil_drainage_class,      ~method_soil_drainage,
      9.25,  47.75, "well_drained",            "hwsd_v1.2_drainage_code",
      9.75,  47.75, "moderately_well_drained", "hwsd_v1.2_drainage_code",
      10.25, 47.75, "poorly_drained",          "hwsd_v1.2_drainage_code"
    ),
    image_gnm_runoff = tibble::tribble(
      ~lon,  ~lat,  ~surface_runoff_fraction, ~slope_m_per_km,
      9.25,  47.75, 0.1,                      12,
      9.75,  47.75, 0.2,                      30,
      10.25, 47.75, 0.15,                     20
    )
  )
}

# The balance rows of helper_n_loss_drivers.R, with the item-less row's N
# moved to the crop: a row without a crop item carries no N.
.nlx_rows <- function() {
  rows <- .nld_rows()
  rows$n_input_t[is.na(rows$item_cbs_code)] <- 0
  rows
}

.nlx_build <- function(
  leaching,
  rows = .nlx_rows(),
  layers = .nlx_layers(),
  ...
) {
  suppressMessages(whep::build_n_loss_drivers(
    rows,
    window = "annual",
    data = c(.nld_inputs(.nlx_years), layers),
    methods = list(leaching = leaching, ...)
  ))$n_balance_leaching_drivers
}

.nlx_row <- function(tbl, lon, item, regime) {
  tbl[
    tbl$lon == lon &
      tbl$item_cbs_code %in% item &
      tbl$water_regime == regime,
  ]
}

testthat::test_that("the Meisinger table carries every driver it reads", {
  testthat::skip_if_not_installed("pointblank")
  out <- .nlx_build("meisinger_drainage")
  testthat::expect_identical(nrow(out), nrow(.nlx_rows()))
  needed <- c(
    "climate",
    "irrig_cat",
    "land_use",
    "c_input_t",
    "tillage",
    "som_share",
    "drainage_mm",
    "root_depth_cm",
    "soil_porosity",
    "soil_field_capacity"
  )
  out |>
    pointblank::expect_col_exists(dplyr::all_of(needed)) |>
    pointblank::expect_col_vals_not_null(dplyr::all_of(setdiff(
      needed,
      "c_input_t"
    ))) |>
    pointblank::expect_col_vals_in_set("tillage", "Tillage") |>
    pointblank::expect_col_vals_in_set("method_tillage", "constant_tillage") |>
    pointblank::expect_col_vals_in_set(
      "irrig_cat",
      c("Rainfed", "Irrigated")
    ) |>
    pointblank::expect_col_vals_in_set("land_use", c("Cropland", "Grassland"))
  testthat::expect_identical(
    out$irrig_cat == "Irrigated",
    out$water_regime == "irrigated"
  )
  testthat::expect_identical(
    out$climate,
    dplyr::if_else(out$moisture_regime == "wet", "ATL", "MED")
  )
  testthat::expect_identical(
    out$land_use == "Grassland",
    out$item_cbs_code %in% 3000L
  )
})

testthat::test_that("drainage is LPJmL's whole-cell seepage, nearest cell off grid", {
  out <- .nlx_build("meisinger_drainage")
  a <- out[out$lon == 9.25, ]
  testthat::expect_true(all(a$drainage_mm == 120))
  testthat::expect_true(all(a$method_drainage == "lpjml_seepage"))
  c_row <- out[out$lon == 10.25, ]
  km <- round(.nld_km(10.25, 47.75, 9.75, 47.75), 1)
  testthat::expect_true(all(c_row$drainage_mm == 300))
  testthat::expect_true(all(
    c_row$method_drainage ==
      paste0("lpjml_seepage;nearest_lpjml_cell=", km, "km")
  ))
})

testthat::test_that("the SOM share is carbon-balance SOC over the topsoil mass", {
  out <- .nlx_build("meisinger_drainage")
  # Cell A cropland: 42 Mg C/ha over 4,000 Mg/ha of topsoil, times 2.
  maize <- .nlx_row(out, 9.25, 2514L, "rainfed")
  testthat::expect_equal(maize$som_share, 42 / 4000 * 2)
  testthat::expect_match(maize$method_som, "^carbon_balance_soc;som_factor=2;")
  # Cell B grassland: its grassland stock, 92.
  grass <- .nlx_row(out, 9.75, 3000L, "rainfed")
  testthat::expect_equal(grass$som_share, 92 / 3600 * 2)
  # Cell C takes B's topsoil mass, stamped.
  c_row <- .nlx_row(out, 10.25, 2514L, "rainfed")
  testthat::expect_equal(c_row$som_share, 37 / 3600 * 2)
  testthat::expect_match(c_row$method_som, "nearest_hwsd_cell=")
  conventional <- .nlx_build("meisinger_drainage", som_factor = "conventional")
  testthat::expect_equal(
    .nlx_row(conventional, 9.25, 2514L, "rainfed")$som_share,
    42 / 4000 * 1.724
  )
  hwsd <- .nlx_build("meisinger_drainage", som_source = "hwsd")
  testthat::expect_equal(
    .nlx_row(hwsd, 9.25, 2514L, "rainfed")$som_share,
    60 / 4000 * 2
  )
})

testthat::test_that("the C input is the crop's own, the grassland's on grass", {
  out <- .nlx_build("meisinger_drainage")
  maize <- out[out$lon == 9.25 & out$item_cbs_code %in% 2514L, ]
  # Both regime rows carry the item's whole C, 2.5 x 80 t.
  testthat::expect_equal(maize$c_input_t, c(200, 200))
  testthat::expect_true(all(
    maize$method_c_input == "soil_carbon_inputs_per_crop"
  ))
  grass <- .nlx_row(out, 9.75, 3000L, "rainfed")
  testthat::expect_equal(grass$c_input_t, 3 * 500)
  testthat::expect_identical(grass$method_c_input, "carbon_balance_grassland")
  no_item <- out[is.na(out$item_cbs_code), ]
  testthat::expect_true(all(is.na(no_item$c_input_t)))
  testthat::expect_true(all(no_item$method_c_input == "no_crop_item"))
})

testthat::test_that("the root depth is FAO-56's by water regime", {
  out <- .nlx_build("meisinger_drainage")
  # Maize (56): Table 22 gives 1.0-1.7 m; rainfed rows take the upper end,
  # irrigated rows the lower one.
  testthat::expect_equal(
    .nlx_row(out, 9.25, 2514L, "rainfed")$root_depth_cm,
    170
  )
  testthat::expect_equal(
    .nlx_row(out, 9.25, 2514L, "irrigated")$root_depth_cm,
    100
  )
  testthat::expect_identical(
    .nlx_row(out, 9.25, 2514L, "rainfed")$method_root_depth,
    "fao56_by_regime"
  )
  # Grass (3000, pasture 0.5-1.5 m) holds no regime hectares: the plain mean
  # over its production items, stamped.
  grass <- .nlx_row(out, 9.75, 3000L, "rainfed")
  testthat::expect_equal(grass$root_depth_cm, 150)
  testthat::expect_match(grass$method_root_depth, "unweighted_no_regime_area")
  no_item <- out[is.na(out$item_cbs_code), ]
  testthat::expect_true(all(no_item$root_depth_cm == 30))
  testthat::expect_true(all(
    no_item$method_root_depth == "no_crop_item_topsoil_30cm"
  ))
  lower <- .nlx_build("meisinger_drainage", root_depth = "lower")
  testthat::expect_equal(
    .nlx_row(lower, 9.25, 2514L, "rainfed")$root_depth_cm,
    100
  )
  fixed <- .nlx_build("meisinger_drainage", root_depth = "top_1m")
  testthat::expect_true(all(fixed$root_depth_cm == 100))
})

testthat::test_that("the soil hydraulic properties are HWSD's per cell", {
  out <- .nlx_build("meisinger_drainage")
  b <- .nlx_row(out, 9.75, 3000L, "rainfed")
  testthat::expect_equal(b$soil_porosity, 0.46)
  testthat::expect_equal(b$soil_field_capacity, 0.32)
  testthat::expect_identical(b$method_soil_hydraulic, "hwsd_usda_texture_class")
})

testthat::test_that("the IMAGE-GNM table carries every driver it reads", {
  testthat::skip_if_not_installed("pointblank")
  out <- .nlx_build("image_gnm")
  needed <- c(
    "runoff_mm",
    "surface_runoff_fraction",
    "slope_m_per_km",
    "land_use",
    "temp_c_annual_mean",
    "soil_field_capacity",
    "soil_wilting_point",
    "soil_texture_class",
    "soil_drainage_class",
    "soil_organic_carbon_pct"
  )
  out |>
    pointblank::expect_col_exists(dplyr::all_of(needed)) |>
    pointblank::expect_col_vals_not_null(dplyr::all_of(needed))
  testthat::expect_false(rlang::has_name(out, "drainage_mm"))
  a <- .nlx_row(out, 9.25, 2514L, "rainfed")
  testthat::expect_equal(a$runoff_mm, 200)
  testthat::expect_equal(a$temp_c_annual_mean, 12)
  testthat::expect_equal(a$soil_organic_carbon_pct, 100 * 42 / 4000)
  testthat::expect_identical(a$soil_texture_class, "medium")
  # Cell C's texture is its nearest cell's (B, fine), stamped.
  c_row <- .nlx_row(out, 10.25, 2514L, "rainfed")
  testthat::expect_identical(c_row$soil_texture_class, "fine")
  testthat::expect_match(c_row$method_soil_texture_fao, "nearest_hwsd_cell=")
  testthat::expect_identical(c_row$soil_drainage_class, "poorly_drained")
})

testthat::test_that("the built drivers feed calculate_n_leaching", {
  rows <- .nlx_rows()
  drivers <- .nlx_build("meisinger_drainage", rows = rows)
  x <- drivers |>
    dplyr::mutate(
      fert_type = "Synthetic",
      n_surplus_t = 10,
      cn_input = NA_real_
    )
  out <- whep::calculate_n_leaching(x, drainage_mm = "drainage_mm")
  testthat::expect_equal(
    out$no3_n_t + out$denitrification_n_t + out$retained_profile_n_t,
    out$n_surplus_t + out$retained_carried_in_n_t,
    tolerance = 1e-12
  )
  gnm <- .nlx_build("image_gnm", rows = rows) |>
    dplyr::mutate(
      n_surplus_t = 10,
      n_input_std_t = 20,
      nh3_n_t = 1,
      area_ha = 5
    )
  testthat::expect_no_error(whep::calculate_n_leaching(
    gnm,
    method = "image_gnm"
  ))
})

# ---- Guards -------------------------------------------------------------------

testthat::test_that("an item-less row carrying N is refused", {
  testthat::expect_error(
    .nlx_build("meisinger_drainage", rows = .nld_rows()),
    class = "whep_n_loss_drivers_no_item_n"
  )
})

testthat::test_that("a missing carbon balance is refused, not zero-filled", {
  layers <- .nlx_layers()
  layers$carbon_balance <- NULL
  testthat::expect_error(
    .nlx_build("meisinger_drainage", layers = layers),
    class = "whep_n_loss_drivers_carbon_balance"
  )
})

testthat::test_that("a carbon balance with no stock is refused", {
  layers <- .nlx_layers()
  layers$carbon_balance$stock_mgc_ha <- 0
  expect_supplied_guard(
    # som_share = stock / mass x factor closes on a zero stock.
    identity = all(layers$carbon_balance$stock_mgc_ha / 4000 * 2 == 0),
    guard = .nlx_build("meisinger_drainage", layers = layers)
  )
})

testthat::test_that("an LPJmL seepage layer of zeros is refused", {
  layers <- .nlx_layers()
  layers$lpjml_drainage$value <- 0
  expect_supplied_guard(
    # Zero drainage still closes the partition: nothing moves, all retained.
    identity = all(layers$lpjml_drainage$value >= 0),
    guard = .nlx_build("meisinger_drainage", layers = layers)
  )
})

testthat::test_that("a soil carbon input layer of zeros is refused", {
  layers <- .nlx_layers()
  layers$carbon_inputs$total_c_input_mgc_ha_yr <- 0
  expect_supplied_guard(
    # A zero C input is a valid C:N of 0 (no attenuation), so nothing else
    # would notice.
    identity = all(layers$carbon_inputs$total_c_input_mgc_ha_yr * 80 == 0),
    guard = .nlx_build("meisinger_drainage", layers = layers)
  )
})

testthat::test_that("a topsoil mass layer of zeros is refused", {
  layers <- .nlx_layers()
  layers$soil_mass$soil_mass_mg_ha <- 0
  expect_supplied_guard(
    identity = all(layers$soil_mass$soil_mass_mg_ha == 0),
    guard = .nlx_build("meisinger_drainage", layers = layers)
  )
})

testthat::test_that("an LPJmL runoff layer of zeros is refused", {
  layers <- .nlx_layers()
  layers$lpjml_runoff$value <- 0
  expect_supplied_guard(
    identity = all(layers$lpjml_runoff$value == 0),
    guard = .nlx_build("image_gnm", layers = layers)
  )
})

testthat::test_that("a crop row with N and no FAO-56 depth is refused", {
  rows <- dplyr::bind_rows(
    .nlx_rows(),
    tibble::tibble(
      lon = 9.25,
      lat = 47.75,
      area_code = 79L,
      item_cbs_code = 2731L,
      year = 2010L,
      water_regime = "rainfed",
      n_input_t = 1
    )
  )
  testthat::expect_error(
    .nlx_build("meisinger_drainage", rows = rows),
    class = "whep_n_loss_drivers_root_depth"
  )
})

testthat::test_that("a row with N and no soil carbon is refused", {
  layers <- .nlx_layers()
  layers$carbon_balance <- layers$carbon_balance[
    layers$carbon_balance$land_use != "grassland",
  ]
  testthat::expect_error(
    .nlx_build("meisinger_drainage", layers = layers),
    class = "whep_n_loss_drivers_soc"
  )
})

testthat::test_that("unknown driver methods are refused", {
  testthat::expect_error(
    whep:::.nld_methods(list(leaching = "fracleach")),
    class = "rlang_error"
  )
  testthat::expect_error(
    whep:::.nld_methods(list(som = "x")),
    class = "whep_n_loss_drivers_methods"
  )
  defaults <- whep:::.nld_methods(list())
  testthat::expect_identical(defaults$leaching, "ipcc_fracleach")
  testthat::expect_identical(defaults$som_factor, "pribyl_2010")
  testthat::expect_identical(defaults$root_depth, "by_regime")
})

testthat::test_that("a crop row with N and no carbon input is refused", {
  layers <- .nlx_layers()
  layers$carbon_inputs <- layers$carbon_inputs[
    layers$carbon_inputs$lon != 10.25,
  ]
  testthat::expect_error(
    .nlx_build("meisinger_drainage", layers = layers),
    class = "whep_n_loss_drivers_c_input"
  )
})
