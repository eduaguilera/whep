# Fixtures mirror the runnable examples of the two exported panel
# functions: two provinces at two dates, enough to exercise all four
# indicator panels without touching a pin.

.indicators_panel_args <- function() {
  list(
    finn_data = tibble::tribble(
      ~year, ~province_name, ~finn_index,
      1960, "A", 0.12,
      1960, "B", 0.18,
      2000, "A", 0.07,
      2000, "B", 0.09
    ),
    n_prov_destiny = tibble::tribble(
      ~year, ~province_name, ~box, ~origin, ~destiny, ~mg_n,
      1960, "A", "Cropland", "Synthetic", "Cropland", 900,
      1960, "A", "Cropland", "Outside", "livestock_mono", 300,
      1960, "A", "Cropland", "Cropland", "population_food", 500,
      1960, "B", "Cropland", "Synthetic", "Cropland", 400,
      1960, "B", "Cropland", "Outside", "livestock_mono", 100,
      2000, "A", "Cropland", "Synthetic", "Cropland", 2600,
      2000, "A", "Cropland", "Outside", "livestock_mono", 1800,
      2000, "A", "Cropland", "Cropland", "population_food", 700,
      2000, "B", "Cropland", "Synthetic", "Cropland", 900,
      2000, "B", "Cropland", "Outside", "livestock_mono", 500
    ),
    area_df = tibble::tribble(
      ~year, ~province_name, ~area_ha,
      1960, "A", 10000,
      1960, "B", 8000,
      2000, "A", 9000,
      2000, "B", 7000
    ),
    typo_df = tibble::tribble(
      ~year, ~province_name, ~Typology_base,
      1960, "A", "Specialized cropping systems",
      1960, "B", "Semi-natural agroecosystems",
      2000, "A", "Specialized cropping systems",
      2000, "B", "Semi-natural agroecosystems"
    )
  )
}

.periods_panel_args <- function() {
  flows <- tibble::tribble(
    ~year, ~province_name, ~box, ~origin, ~destiny, ~mg_n,
    1865, "A", "Cropland", "Synthetic", "Cropland", 900,
    1865, "A", "Cropland", "Outside", "livestock_mono", 300,
    1865, "B", "Cropland", "Synthetic", "Cropland", 400,
    1965, "A", "Cropland", "Synthetic", "Cropland", 2600,
    1965, "A", "Cropland", "Outside", "livestock_mono", 1800,
    1965, "B", "Cropland", "Synthetic", "Cropland", 900
  )
  list(
    finn_data = tibble::tribble(
      ~year, ~province_name, ~finn_index,
      1865, "A", 0.12,
      1865, "B", 0.18,
      1965, "A", 0.07,
      1965, "B", 0.09
    ),
    n_prov_destiny = flows,
    n_nat_destiny = flows,
    panel_data = list(
      area_df = tibble::tribble(
        ~year, ~province_name, ~area_ha,
        1865, "A", 10000,
        1865, "B", 8000,
        1965, "A", 9000,
        1965, "B", 7000
      ),
      typo_df = tibble::tribble(
        ~year, ~province_name, ~Typology_base,
        1865, "A", "Specialized cropping systems",
        1865, "B", "Semi-natural agroecosystems",
        1965, "A", "Specialized cropping systems",
        1965, "B", "Semi-natural agroecosystems"
      )
    )
  )
}

.stub_check_installed <- function(record) {
  testthat::local_mocked_bindings(
    check_installed = function(pkg, ...) {
      record$pkg <- pkg
      cli::cli_abort("Stub guard.", class = "whep_test_stub_guard")
    },
    .package = "rlang",
    .env = parent.frame()
  )
}

test_that("indicators panel composes without patchwork attached", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if("package:patchwork" %in% search(), "patchwork is attached")

  panel <- rlang::exec(
    whep::plot_typology_indicators_panel,
    !!!.indicators_panel_args()
  )

  expect_s3_class(panel, "patchwork")
})

test_that("periods panel composes without patchwork attached", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if("package:patchwork" %in% search(), "patchwork is attached")

  panel <- rlang::exec(
    whep::plot_typology_periods_panel,
    !!!.periods_panel_args()
  )

  expect_s3_class(panel, "patchwork")
})

test_that("indicators panel checks its plotting packages up front", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  record <- new.env(parent = emptyenv())
  .stub_check_installed(record)

  expect_error(
    rlang::exec(
      whep::plot_typology_indicators_panel,
      !!!.indicators_panel_args()
    ),
    class = "whep_test_stub_guard"
  )
  expect_equal(record$pkg, c("ggplot2", "patchwork"))
})

test_that("periods panel checks its plotting packages up front", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  record <- new.env(parent = emptyenv())
  .stub_check_installed(record)

  expect_error(
    rlang::exec(whep::plot_typology_periods_panel, !!!.periods_panel_args()),
    class = "whep_test_stub_guard"
  )
  expect_equal(record$pkg, c("ggplot2", "patchwork"))
})


# .panel_area_df -----------------------------------------------------------

test_that(".panel_area_df sums total agricultural area, not cropland only", {
  # A province where semi-natural land dominates: if the denominator were
  # cropland-only (100 ha), a per-hectare indicator would be inflated
  # relative to using the full agricultural area (100 + 50 + 20 + 10 + 15 +
  # 5 = 200 ha, matching .calc_n_productivity_prov()'s agri_area_ha).
  npp_ygpit <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
    2000, "A", "Cropland", 100,
    2000, "A", "Dehesa", 50,
    2000, "A", "Forest_high", 20,
    2000, "A", "Forest_low", 10,
    2000, "A", "Pasture_Shrubland", 15,
    2000, "A", "Other", 5
  )
  testthat::local_mocked_bindings(
    whep_read_file = function(...) npp_ygpit
  )

  out <- whep:::.panel_area_df()

  expect_equal(out$area_ha, 200)
})

test_that(".panel_area_df excludes a non-agricultural LandUse category", {
  # A future npp_ygpit vintage might add a LandUse the N-productivity
  # indicator does not recognise as agricultural (e.g. "Urban"); the
  # denominator must not silently absorb it.
  npp_ygpit <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
    2000, "A", "Cropland", 100,
    2000, "A", "Urban", 900
  )
  testthat::local_mocked_bindings(
    whep_read_file = function(...) npp_ygpit
  )

  out <- whep:::.panel_area_df()

  expect_equal(out$area_ha, 100)
})


# .ext_dep_values / .pollution_values -------------------------------------

test_that(".ext_dep_values counts population_food_inedible on both sides", {
  # population_food_inedible is the remainder .split_food_inedible_loss()
  # (n_prov_destiny.R) split out of population_food; it must count on
  # whichever side (external/internal) its origin belongs to.
  flows <- tibble::tribble(
    ~year, ~province_name, ~origin, ~destiny, ~mg_n,
    2000, "A", "Outside", "population_food", 10,
    2000, "A", "Outside", "population_food_inedible", 5,
    2000, "A", "Cropland", "population_food", 20,
    2000, "A", "Cropland", "population_food_inedible", 10
  )

  out <- whep:::.ext_dep_values(flows)

  # ext_mg = 15 (10 + 5), int_mg = 30 (20 + 10); value = 15 / 45.
  expect_equal(out$value, 15 / 45)
})

test_that(".pollution_values counts population_food_inedible in soil_out and lv_out", {
  flows <- tibble::tribble(
    ~year, ~province_name, ~origin, ~destiny, ~mg_n,
    2000, "A", "Cropland", "population_food", 20,
    2000, "A", "Cropland", "population_food_inedible", 5,
    2000, "A", "Livestock", "population_food", 8,
    2000, "A", "Livestock", "population_food_inedible", 2
  )
  area_df <- tibble::tribble(
    ~year, ~province_name, ~area_ha,
    2000, "A", 100
  )

  out <- whep:::.pollution_values(flows, area_df)

  expect_equal(out$soil_out, 25)
  expect_equal(out$lv_out, 10)
})
