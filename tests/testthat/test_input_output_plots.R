# Tests for input_output_plots.R functions
testthat::local_edition(3)


.fixture_nat_destiny <- function() {
  tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Origin, ~Destiny, ~MgN,
    2000, "Spain", "Wheat and products", "Synthetic", "Cropland", 800,
    2000, "Spain", "Wheat and products", "Fixation", "Cropland", 200,
    2000, "Spain", "Wheat and products", "Cropland", "population_food", 300,
    2000, "Spain", "Straw", "Cropland", "livestock_rum", 100,
    2000, "Spain", "Bovine Meat", "Livestock", "population_food", 50,
    2000, "Spain", "Wheat and products", "Cropland", "livestock_rum", 120,
    2000, "Spain", "Bovine Meat", "Outside", "population_food", 30,
    2000, "Spain", "Wheat and products", "Outside", "livestock_mono", 40,
    2000, "Spain", "Wheat and products", "Cropland", "export", 60
  )
}


# .surplus_from_totals ---------------------------------------------------------

test_that(".surplus_from_totals clamps to zero when positive_only is TRUE", {
  inputs <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "A", 100
  )
  outputs <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "B", 250
  )

  clamped <- .surplus_from_totals(inputs, outputs, positive_only = TRUE)
  signed <- .surplus_from_totals(inputs, outputs, positive_only = FALSE)

  expect_equal(clamped$MgN, 0)
  expect_equal(signed$MgN, -150)
  expect_equal(unique(clamped$Type), "Surplus")
})


# .system_production -----------------------------------------------------------

test_that(".system_production separates residues from production", {
  out <- .system_production(.fixture_nat_destiny(), "Cropland")

  residues <- out |>
    dplyr::filter(Type == "Residues") |>
    dplyr::pull(MgN)
  production <- out |>
    dplyr::filter(Type == "Production") |>
    dplyr::pull(MgN)

  expect_equal(residues, 100)
  expect_equal(production, 300 + 120 + 60)
})


# .stack_plot_df ---------------------------------------------------------------

test_that(".stack_plot_df negates input types and rescales to Gg", {
  inputs <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "Synthetic_fertilizer", 1000
  )
  outputs <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "Production", 2000
  )
  surplus <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "Surplus", 500
  )

  out <- .stack_plot_df(
    inputs,
    outputs,
    surplus,
    negative_types = "Synthetic_fertilizer",
    type_levels = c("Synthetic_fertilizer", "Surplus", "Production")
  )

  synth <- out |>
    dplyr::filter(Type == "Synthetic_fertilizer") |>
    dplyr::pull(MgN)
  prod <- out |>
    dplyr::filter(Type == "Production") |>
    dplyr::pull(MgN)

  expect_equal(synth, -1)
  expect_equal(prod, 2)
  expect_s3_class(out$Type, "factor")
})


# exported plot builders -------------------------------------------------------

test_that("plot_input_output builders return ggplot objects on example data", {
  expect_s3_class(whep::plot_input_output(example = TRUE), "ggplot")
  expect_s3_class(whep::plot_input_output_livestock(example = TRUE), "ggplot")
  expect_s3_class(whep::plot_input_output_system(example = TRUE), "ggplot")
})
