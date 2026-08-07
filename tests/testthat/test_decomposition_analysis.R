# test_decomposition_analysis.R — tests for R/decomposition_analysis.R
#
# Both builders read national N flows plus a pinned population / crop-area
# table. All three readers are stubbed, so the tests are offline.

# National N flows: soil inputs into the two agricultural boxes, harvested
# output leaving them, and one flow that is neither.
lmdi_flows_fixture <- function() {
  tibble::tribble(
    ~year, ~origin,      ~destiny,                     ~mg_n,
    1990,  "Synthetic",  "Cropland",                   10,
    2000,  "Synthetic",  "Cropland",                   100,
    2000,  "Deposition", "semi_natural_agroecosystems", 20,
    2000,  "Cropland",   "population_food",            30,
    2000,  "Cropland",   "export",                     10,
    2000,  "semi_nat",   "livestock_rum",              5,
    2000,  "Cropland",   "Losses",                     40,
    2001,  "Synthetic",  "Cropland",                   200,
    2001,  "Cropland",   "population_food",            50
  ) |>
    dplyr::mutate(
      origin = dplyr::if_else(
        origin == "semi_nat",
        "semi_natural_agroecosystems",
        origin
      )
    )
}

lmdi_population_fixture <- function() {
  tibble::tribble(
    ~Year, ~POP_MPEOP_YG,
    2000,  15,
    2000,  25,
    2001,  50
  )
}

lmdi_area_fixture <- function() {
  tibble::tribble(
    ~Year, ~Area_ygpit_ha,
    2000,  40,
    2000,  50,
    2001,  100
  )
}

local_mocked_lmdi_readers <- function() {
  local_mocked_bindings(
    create_n_nat_destiny = function(...) lmdi_flows_fixture(),
    whep_read_file = function(name, ...) {
      if (name == "population_yg") {
        lmdi_population_fixture()
      } else {
        lmdi_area_fixture()
      }
    },
    .env = parent.frame()
  )
}

# prepare_lmdi_dataset --------------------------------------------------------

test_that("prepare_lmdi_dataset nets harvested output out of soil inputs", {
  local_mocked_lmdi_readers()

  out <- prepare_lmdi_dataset()

  expect_equal(
    names(out),
    c("year", "surplus", "population", "food", "A", "t_ratio")
  )
  expect_equal(out$year, c(1990, 2000, 2001))
  # 2000: inputs 100 + 20 = 120, output 30 + 10 + 5 = 45. The 40 MgN going to
  # "Losses" is neither an input to the boxes nor a harvested output.
  expect_equal(out$surplus, c(10, 75, 150))
  expect_equal(out$food, c(0, 30, 50))
})

test_that("prepare_lmdi_dataset derives per-capita food and the N ratio", {
  local_mocked_lmdi_readers()

  out <- prepare_lmdi_dataset()

  # Population is summed over the sub-national rows of the pin.
  expect_equal(out$population, c(0, 40, 50))
  expect_equal(out$A, c(NA, 0.75, 1))
  expect_equal(out$t_ratio, c(NA, 2.5, 3))
})

test_that("prepare_lmdi_dataset guards the divisions instead of emitting Inf", {
  local_mocked_lmdi_readers()

  out <- prepare_lmdi_dataset() |> dplyr::filter(year == 1990)

  # 1990 has a soil input but no harvest and no population row. Both ratios
  # come back NA rather than Inf or NaN.
  expect_equal(out$surplus, 10)
  expect_true(is.na(out$A))
  expect_true(is.na(out$t_ratio))
  expect_false(is.nan(out$A))
})

# prepare_lmdi_production_area ------------------------------------------------

test_that("prepare_lmdi_production_area yields output per hectare", {
  local_mocked_lmdi_readers()

  out <- prepare_lmdi_production_area()

  expect_equal(names(out), c("year", "surplus", "area", "yield", "intensity"))
  expect_equal(out$year, c(1990, 2000, 2001))
  # Cropland area is summed over the rows of the pin: 40 + 50 in 2000.
  expect_equal(out$area, c(NA, 90, 100))
  # Harvested output is 45 MgN in 2000 and 50 in 2001.
  expect_equal(out$yield, c(NA, 45 / 90, 50 / 100))
})

test_that("prepare_lmdi_production_area totals every flow as `surplus`", {
  local_mocked_lmdi_readers()

  out <- prepare_lmdi_production_area()

  # Unlike prepare_lmdi_dataset(), this builder's `surplus` column is the sum
  # of *all* national flows, harvest and losses included, not inputs minus
  # outputs: 100 + 20 + 30 + 10 + 5 + 40 in 2000.
  expect_equal(out$surplus, c(10, 205, 250))
  expect_equal(out$intensity, c(NA, 205 / 45, 5))
})
