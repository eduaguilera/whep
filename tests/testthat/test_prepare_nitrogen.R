# Smoke tests for the country-level N-totals helpers added in
# inst/scripts/prepare_spatialize_all.R. Helpers live at script scope
# (not package R/) so we source the script once and exercise the
# offline-safe ones with fixture data. The pin-backed helpers
# (`.faostat_synth_country_total`, `.faostat_manure_country_long`) need
# whep_read_file() over the network and are not covered here.

local({
  pkg_root <- testthat::test_path("..", "..")
  script_path <- file.path(
    pkg_root,
    "inst",
    "scripts",
    "prepare_spatialize_all.R"
  )
  if (file.exists(script_path)) {
    sys.source(script_path, envir = topenv())
  }
})


test_that(".smil_global_yearly interpolates linearly between anchors", {
  skip_if_not(exists(".smil_global_yearly", mode = "function"))
  out <- .smil_global_yearly(first_year = 1950L, last_year = 1965L)
  expect_true(all(out$year >= 1950L & out$year <= 1965L))
  expect_equal(nrow(out), 16L)
  # 1955 + 1960 + 1965 are Smil anchors; reproduce them exactly
  smil <- whep::smil_2001_synthetic_n_global
  for (anchor in c(1955L, 1960L, 1965L)) {
    expect_equal(
      out$global_mg_n[out$year == anchor],
      smil$global_kt_n[smil$year == anchor] * 1000
    )
  }
  # Midpoint between 1955 (6300 kt) and 1960 (10500 kt) is at 1957.5;
  # linear interpolation gives 8400 kt at 1957 (3/5 of the way).
  v_1957 <- out$global_mg_n[out$year == 1957L]
  expect_equal(v_1957, (6300 + (10500 - 6300) * 2 / 5) * 1000, tolerance = 1)
})


test_that(".smil_synth_pre_1961 backcasts country synth using shares", {
  skip_if_not(exists(".smil_synth_pre_1961", mode = "function"))
  synth_faostat <- tibble::tribble(
    ~area_code, ~area_name, ~year, ~mg_n,
    1L,         "A",        1961L, 60000,
    1L,         "A",        1962L, 60000,
    1L,         "A",        1963L, 60000,
    1L,         "A",        1964L, 60000,
    1L,         "A",        1965L, 60000,
    2L,         "B",        1961L, 40000,
    2L,         "B",        1962L, 40000,
    2L,         "B",        1963L, 40000,
    2L,         "B",        1964L, 40000,
    2L,         "B",        1965L, 40000
  )
  out <- .smil_synth_pre_1961(synth_faostat)
  expect_true(all(out$year >= 1913L & out$year <= 1960L))
  expect_true(all(out$mg_n > 0))
  # Global pre-1961 should grow over time (Smil temporal shape).
  global_1920 <- sum(out$mg_n[out$year == 1920L])
  global_1950 <- sum(out$mg_n[out$year == 1950L])
  expect_true(global_1950 > global_1920)
  # Country A holds 60 % of the calibration-window global total, so its
  # share of any pre-1961 year should also be ~60 %.
  share_a_1950 <- out$mg_n[out$area_code == 1L & out$year == 1950L] /
    sum(out$mg_n[out$year == 1950L])
  expect_equal(share_a_1950, 0.6, tolerance = 0.01)
})


test_that(".faostat_manure_shares_const averages shares over 1961-65", {
  skip_if_not(exists(".faostat_manure_shares_const", mode = "function"))
  manure_long <- tibble::tribble(
    ~area_code, ~area_name, ~year, ~element,   ~mg_n,
    1L,         "A",        1961L, "excreted", 1000,
    1L,         "A",        1961L, "applied",  300,
    1L,         "A",        1961L, "pasture",  500,
    1L,         "A",        1962L, "excreted", 1100,
    1L,         "A",        1962L, "applied",  330,
    1L,         "A",        1962L, "pasture",  550,
    1L,         "A",        1980L, "excreted", 5000, # outside calibration window
    1L,         "A",        1980L, "applied",  100,  # should be ignored
    1L,         "A",        1980L, "pasture",  100
  )
  out <- .faostat_manure_shares_const(manure_long, share_window = 1961L:1962L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$applied_share, 0.3, tolerance = 1e-6)
  expect_equal(out$pasture_share, 0.5, tolerance = 1e-6)
})


test_that(".livestock_manure_split applies per-country shares", {
  skip_if_not(exists(".livestock_manure_split", mode = "function"))
  excreted <- tibble::tribble(
    ~year, ~area_code, ~area_name, ~mg_n_excreted,
    1851L, 1L,         "A",        1000,
    1900L, 1L,         "A",        2000,
    1961L, 1L,         "A",        4000
  )
  shares <- tibble::tribble(
    ~area_code, ~area_name, ~applied_share, ~pasture_share,
    1L,         "A",        0.3,            0.5
  )
  out <- .livestock_manure_split(excreted, shares)
  expect_true(all(c("Manure", "Grassland_excretion") %in% out$fert_type))
  applied_1851 <- out$mg_n[
    out$year == 1851L & out$fert_type == "Manure"
  ]
  pasture_1851 <- out$mg_n[
    out$year == 1851L & out$fert_type == "Grassland_excretion"
  ]
  expect_equal(applied_1851, 300, tolerance = 1e-6)
  expect_equal(pasture_1851, 500, tolerance = 1e-6)
})


test_that(".fill_n_inputs_to_target_year carry-forwards beyond last obs", {
  skip_if_not(exists(".fill_n_inputs_to_target_year", mode = "function"))
  n_in <- tibble::tribble(
    ~year, ~area_code, ~area_name, ~crop_name, ~land_use, ~fert_type, ~area_ha, ~mg_n, ~kg_n_ha,
    2019L, 1L,         "A",        "wheat",    "Cropland", "Synthetic", 1000,    150,   150,
    2020L, 1L,         "A",        "wheat",    "Cropland", "Synthetic", 1000,    155,   155,
    2021L, 1L,         "A",        "wheat",    "Cropland", "Synthetic", 1000,    160,   160
  )
  out <- .fill_n_inputs_to_target_year(n_in, target_year = 2023L)
  expect_equal(max(out$year), 2023L)
  expect_true(all(2019:2023 %in% out$year))
  # whep::fill_linear carries the last observation forward as a constant;
  # 2022 and 2023 take the 2021 mg_n value (160).
  expect_equal(out$mg_n[out$year == 2022L], 160, tolerance = 1e-6)
  expect_equal(out$mg_n[out$year == 2023L], 160, tolerance = 1e-6)
})


test_that(".fill_n_inputs_to_target_year leaves pre-first-obs as NA", {
  skip_if_not(exists(".fill_n_inputs_to_target_year", mode = "function"))
  n_in <- tibble::tribble(
    ~year, ~area_code, ~area_name, ~crop_name, ~land_use, ~fert_type, ~area_ha, ~mg_n, ~kg_n_ha,
    1920L, 1L,         "A",        "wheat",    "Cropland", "Synthetic", 1000,    50,    50,
    1921L, 1L,         "A",        "wheat",    "Cropland", "Synthetic", 1000,    55,    55
  )
  out <- .fill_n_inputs_to_target_year(n_in, target_year = 1925L)
  # 1920 is the first observed year; backward fill is disabled.
  expect_false(any(out$year < 1920L))
})
