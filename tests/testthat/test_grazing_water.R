grazing_water_fixture <- function() {
  tibble::tribble(
    ~component, ~grass_water_m3, ~grass_intake_dm_t, ~reference_dm_t,
    "green",    1000,            20,                 100,
    "blue",      100,            20,                 100
  ) |>
    dplyr::mutate(
      source_water = "Illustrative measured ET",
      source_reference = "Illustrative reference experiment",
      reference_basis = "maximum_grazing_yield",
      reference_status = "sourced",
      water_scenario = "illustrative_reference_run",
      reference_scenario = "illustrative_reference_run",
      source_intake = "Illustrative intake observations"
    )
}

test_that("biomass attribution preserves the water partition and input mass", {
  x <- grazing_water_fixture()
  out <- whep::calculate_grazing_water(x)
  expect_equal(out$water_attributed_m3, c(200, 20))
  expect_equal(
    out$water_attributed_m3 + out$water_unallocated_m3,
    x$grass_water_m3
  )
  expect_equal(out$grass_intake_dm_t, x$grass_intake_dm_t)
  expect_equal(out$excess_intake_dm_t, c(0, 0))
  expect_true(all(out$method_grazing_water == "biomass_allocation"))
  expect_equal(out$source_reference, x$source_reference)
})

test_that("occupation uses observed water without inventing biomass inputs", {
  x <- grazing_water_fixture() |>
    dplyr::select(component, grass_water_m3, source_water)
  out <- whep::calculate_grazing_water(x, method = "occupation")
  expect_equal(out$water_attributed_m3, x$grass_water_m3)
  expect_equal(out$water_unallocated_m3, c(0, 0))
  expect_true(all(is.na(out$excess_intake_dm_t)))
  expect_true(all(out$quantity_basis == "occupied_grassland_et"))
})

test_that("economic allocation requires a supplied fraction and its source", {
  x <- grazing_water_fixture() |>
    dplyr::mutate(value_fraction = 0.1, source_fraction = "Toy valuation")
  out <- whep::calculate_grazing_water(x, method = "value_allocation")
  expect_equal(out$water_attributed_m3, c(100, 10))
  expect_equal(
    out$water_attributed_m3 + out$water_unallocated_m3,
    x$grass_water_m3
  )
  expect_error(
    whep::calculate_grazing_water(
      dplyr::select(x, -source_fraction),
      method = "value_allocation"
    ),
    class = "whep_grazing_water_input"
  )
  expect_error(
    whep::calculate_grazing_water(
      dplyr::mutate(x, value_fraction = 1.1),
      method = "value_allocation"
    ),
    class = "whep_grazing_water_input"
  )
})

test_that("a missing reference cannot pass by water conservation alone", {
  x <- grazing_water_fixture()
  # Occupation reconciles exactly without a biomass reference. That does not
  # supply the denominator needed to attribute water to consumed grass.
  missing <- dplyr::select(x, -reference_dm_t)
  occupied <- whep::calculate_grazing_water(missing, method = "occupation")
  expect_equal(
    occupied$water_attributed_m3 + occupied$water_unallocated_m3,
    x$grass_water_m3
  )
  expect_error(
    whep::calculate_grazing_water(missing),
    class = "whep_grazing_water_input"
  )
  expect_error(
    whep::calculate_grazing_water(dplyr::mutate(x, reference_dm_t = NA_real_)),
    class = "whep_grazing_water_input"
  )
  expect_error(
    whep::calculate_grazing_water(dplyr::mutate(x, source_reference = "")),
    class = "whep_grazing_water_input"
  )
})

test_that("excess intake is preserved and never silently capped", {
  x <- grazing_water_fixture() |>
    dplyr::mutate(grass_intake_dm_t = 125)
  expect_error(
    whep::calculate_grazing_water(x),
    class = "whep_grazing_reference_exceeded"
  )
  expect_warning(
    out <- whep::calculate_grazing_water(x, excess = "extrapolate"),
    class = "whep_grazing_extrapolated"
  )
  expect_equal(out$grass_intake_dm_t, c(125, 125))
  expect_equal(out$excess_intake_dm_t, c(25, 25))
  expect_equal(out$allocation_fraction, c(1.25, 1.25))
  expect_equal(out$water_attributed_m3, c(1250, 125))
  expect_true(all(is.na(out$water_unallocated_m3)))
  expect_true(all(out$quantity_basis == "extrapolated_grazing_requirement"))
})

test_that("known zero intake is distinct from an absent reference", {
  x <- grazing_water_fixture() |>
    dplyr::mutate(grass_intake_dm_t = 0, reference_dm_t = 0)
  out <- whep::calculate_grazing_water(x)
  expect_equal(out$water_attributed_m3, c(0, 0))
  expect_equal(out$water_unallocated_m3, x$grass_water_m3)
  expect_error(
    whep::calculate_grazing_water(dplyr::mutate(x, grass_intake_dm_t = 1)),
    class = "whep_grazing_zero_reference"
  )
})

test_that("invalid quantities and unlabeled assumptions are rejected", {
  x <- grazing_water_fixture()
  expect_error(
    whep::calculate_grazing_water(dplyr::mutate(x, grass_water_m3 = -1)),
    class = "whep_grazing_water_input"
  )
  expect_error(
    whep::calculate_grazing_water(dplyr::mutate(x, grass_intake_dm_t = Inf)),
    class = "whep_grazing_water_input"
  )
  expect_error(
    whep::calculate_grazing_water(dplyr::mutate(x, component = "withdrawal")),
    class = "whep_grazing_water_input"
  )
  assumed <- dplyr::mutate(x, reference_status = "assumed_unverified")
  expect_error(
    whep::calculate_grazing_water(assumed),
    class = "whep_grazing_water_input"
  )
  assumed <- dplyr::mutate(
    assumed,
    reference_assumption = "Illustrative unverified forage reference"
  )
  out <- whep::calculate_grazing_water(assumed)
  expect_equal(out$reference_assumption, assumed$reference_assumption)
  expect_true(all(out$reference_status == "assumed_unverified"))
  expect_error(whep::calculate_grazing_water(x, method = "fallback"))
})

test_that("empty valid schemas remain empty", {
  out <- whep::calculate_grazing_water(grazing_water_fixture()[0, ])
  expect_equal(nrow(out), 0L)
  expect_type(out$water_attributed_m3, "double")
})

test_that("reference biomass and its ET must identify the same scenario", {
  x <- grazing_water_fixture()
  expect_error(
    whep::calculate_grazing_water(
      dplyr::mutate(x, reference_scenario = "different_management_run")
    ),
    class = "whep_grazing_water_input"
  )
  expect_error(
    whep::calculate_grazing_water(dplyr::select(x, -water_scenario)),
    class = "whep_grazing_water_input"
  )
})

test_that("calculations cannot overwrite an existing quantity interpretation", {
  x <- grazing_water_fixture()
  expect_error(
    whep::calculate_grazing_water(
      dplyr::mutate(x, quantity_basis = "existing_meaning")
    ),
    class = "whep_grazing_water_input"
  )
  out <- whep::calculate_grazing_water(x)
  expect_error(
    whep::calculate_grazing_water(out),
    class = "whep_grazing_water_input"
  )
})
