# Tests for prepare_livestock_emissions() bridge function

test_that("validates required columns", {
  bad_data <- tibble::tibble(x = 1)
  expect_error(
    prepare_livestock_emissions(bad_data),
    "Missing required column"
  )
})

test_that("filters to heads only", {
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value,
    960L,           "heads",  1000,
    960L,           "LU",     1000,
    960L,           "tonnes", 5000
  )
  result <- prepare_livestock_emissions(data)
  expect_equal(nrow(result), 1)
  expect_equal(result$heads, 1000)
})

test_that("excludes non-IPCC species", {
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,   ~value,
    960L,           "heads", 1000,
    1181L,          "heads", 500,
    1190L,          "heads", 200
  )
  expect_message(
    result <- prepare_livestock_emissions(data),
    "Excluded"
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$item_cbs_code, 960L)
})

test_that("maps species via animals_codes", {
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,   ~value,
    960L,           "heads", 1000,
    961L,           "heads", 2000,
    946L,           "heads", 500,
    976L,           "heads", 300
  )
  result <- prepare_livestock_emissions(data)
  expect_true(rlang::has_name(result, "species"))
  expect_equal(
    result$species,
    c("Cattle, dairy", "Cattle, non-dairy", "Buffalo", "Sheep")
  )
})

test_that("maps area_code to iso3 via polities", {
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,   ~value, ~area_code,
    960L,           "heads", 1000,   4
  )
  result <- prepare_livestock_emissions(data)
  expect_true(rlang::has_name(result, "iso3"))
  expect_equal(result$iso3, "DZA")
})

test_that("unknown area_code produces iso3 = NA", {
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,   ~value, ~area_code,
    960L,           "heads", 1000,   99999
  )
  result <- prepare_livestock_emissions(data)
  expect_true(is.na(result$iso3))
})

test_that("extracts milk yield and converts to kg/day", {
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value, ~year, ~area_code,
    ~live_anim_code, ~item_prod_code,
    960L,  "heads",  1000, 2020L, 4L, NA_character_, "960",
    960L,  "t_head", 5.0,  2020L, 4L, "960",        "882"
  )
  result <- prepare_livestock_emissions(data)
  expect_true(rlang::has_name(result, "milk_yield_kg_day"))
  expected_milk <- 5.0 * 1000 / 365
  expect_equal(result$milk_yield_kg_day, expected_milk, tolerance = 0.01)
})

test_that("extracts meat yield as raw t_head", {
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value, ~year, ~area_code,
    ~live_anim_code, ~item_prod_code,
    961L,  "heads",  2000, 2020L, 4L, NA_character_, "961",
    961L,  "t_head", 0.2,  2020L, 4L, "961",        "867"
  )
  result <- prepare_livestock_emissions(data)
  expect_true(rlang::has_name(result, "meat_yield_t_head"))
  expect_equal(result$meat_yield_t_head, 0.2)
})

test_that("preserves extra columns from input", {
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,   ~value, ~weight, ~diet_quality,
    960L,           "heads", 1000,   600,     "High"
  )
  result <- prepare_livestock_emissions(data)
  expect_true(rlang::has_name(result, "weight"))
  expect_true(rlang::has_name(result, "diet_quality"))
  expect_equal(result$weight, 600)
  expect_equal(result$diet_quality, "High")
})

test_that("cohort expansion works", {
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,   ~value,
    946L,           "heads", 1000
  )
  result <- prepare_livestock_emissions(
    data,
    expand_cohorts = TRUE
  )
  expect_true(rlang::has_name(result, "cohort"))
  expect_true(rlang::has_name(result, "system"))
  expect_true(nrow(result) > 1)
})

test_that("result pipes to .calc_enteric_ch4_tier1", {
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,   ~value,
    960L,           "heads", 1000,
    976L,           "heads", 500
  )
  result <- data |>
    prepare_livestock_emissions() |>
    whep:::.calc_enteric_ch4_tier1()
  expect_true(rlang::has_name(result, "enteric_ch4_tier1"))
  expect_true(all(result$enteric_ch4_tier1 > 0))
})
