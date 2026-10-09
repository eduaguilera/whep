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

test_that("meat yield is converted to weight_gain_kg_day for energy model", {
  # Non-dairy cattle (961): carcass 0.2 t/head => live 0.2/0.55*1000 = 364 kg
  # weight_gain = (364 - 40) / 547.5 = 0.592 kg/day (IPCC cattle defaults).
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value, ~year, ~area_code,
    ~live_anim_code, ~item_prod_code,
    961L,  "heads",  2000, 2020L, 4L, NA_character_, "961",
    961L,  "t_head", 0.2,  2020L, 4L, "961",        "867"
  )
  result <- prepare_livestock_emissions(data)
  expect_true(rlang::has_name(result, "weight_gain_kg_day"))
  expect_equal(
    result$weight_gain_kg_day,
    (0.2 * 1000 / 0.55 - 40) / 547.5,
    tolerance = 0.01
  )
})

test_that("secondary products under one live_anim_code do not duplicate heads", {
  # Production reports several t_head products for one animal (e.g. raw milk
  # plus a minor dairy product), all sharing the animal's live_anim_code. Only
  # the animal's designated product (Item_Code_product 882 for dairy cattle)
  # must drive the yield; the head row must not fan out across the others.
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value, ~year, ~area_code,
    ~live_anim_code, ~item_prod_code,
    960L,  "heads",  1000,  2020L, 4L, NA_character_, "960",
    960L,  "t_head", 5.0,   2020L, 4L, "960",        "882",
    960L,  "t_head", 0.01,  2020L, 4L, "960",        "886"
  )
  result <- prepare_livestock_emissions(data)
  expect_equal(nrow(result), 1)
  expect_equal(result$milk_yield_kg_day, 5.0 * 1000 / 365, tolerance = 0.01)
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

# whep#1136: a year-scoped primary-production read hands back a data.table,
# because .filter_years() converts, and every dplyr verb in this bridge keeps
# that class, so the frame reached the emission engines as a data.table and
# aborted there. This is an exported function, and the package contract is that
# one returns a tibble; a tibble fixture cannot catch the regression.
test_that("a data.table production table comes back as a tibble", {
  data <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit,   ~value,
    2000L, 10L,        960L,           "heads", 1000,
    2000L, 10L,        961L,           "heads", 2000
  )

  from_tibble <- prepare_livestock_emissions(data)
  from_dt <- prepare_livestock_emissions(data.table::as.data.table(data))

  expect_true(tibble::is_tibble(from_dt))
  expect_false(data.table::is.data.table(from_dt))
  expect_equal(as.data.frame(from_dt), as.data.frame(from_tibble))
})

# ---- whep#1034: a moved head unit --------------------------------------------

test_that("a moved head unit cannot ship as no livestock", {
  # Same herd, the unit spelled in another vocabulary ("Head").
  # Unguarded, no row passes the head filter and every emission engine
  # downstream receives no animals: the herd sums to exactly zero heads.
  relabelled <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value,
    960L,           "Head",   1000,
    976L,           "Head",   500,
    960L,           "tonnes", 5000
  )
  unguarded <- testthat::with_mocked_bindings(
    prepare_livestock_emissions(relabelled),
    check_labels_supplied = function(data, ...) invisible(data)
  )
  expect_supplied_guard(
    identity = nrow(unguarded) == 0L && sum(unguarded$heads) == 0,
    guard = prepare_livestock_emissions(relabelled),
    class = "whep_absent_label"
  )
})

test_that("yield rows that tag to no product cannot fall back to defaults", {
  # The milk row's product code arrives in another vocabulary (the CBS milk
  # item rather than FAOSTAT's 882). Unguarded, no yield joins, the head row
  # goes on unchanged, and the energy model would take the species default.
  recoded <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value, ~year, ~area_code,
    ~live_anim_code, ~item_prod_code,
    960L,  "heads",  1000, 2020L, 4L, NA_character_, "960",
    960L,  "t_head", 5.0,  2020L, 4L, "960",        "2848"
  )
  unguarded <- testthat::with_mocked_bindings(
    prepare_livestock_emissions(recoded),
    check_inputs_supplied = function(data, ...) invisible(data)
  )
  keyed <- prepare_livestock_emissions(
    dplyr::mutate(recoded, item_prod_code = c("960", "882"))
  )
  expect_true(rlang::has_name(keyed, "milk_yield_kg_day"))
  expect_supplied_guard(
    identity = nrow(unguarded) == 1L &&
      unguarded$heads == 1000 &&
      !rlang::has_name(unguarded, "milk_yield_kg_day"),
    guard = prepare_livestock_emissions(recoded)
  )
})

test_that("a production table with no yield rows is not judged", {
  heads_only <- tibble::tribble(
    ~item_cbs_code, ~unit,   ~value, ~live_anim_code, ~item_prod_code,
    960L,           "heads", 1000,   NA_character_,   "960"
  )
  expect_no_error(prepare_livestock_emissions(heads_only))
})

testthat::test_that("weight gain is NA for a species without fattening params", {
  # Poultry has no row in the dressing-fraction table. It must come out NA
  # rather than borrow cattle's 0.55 / 547.5 d (whep#1034). Cattle: live
  # weight 0.3 t / 0.55 = 545.45 kg, gain (545.45 - 40) / 547.5 kg/day.
  animals <- tibble::tribble(
    ~item_cbs_code, ~item_cbs,
    866L, "Cattle",
    1057L, "Poultry Birds"
  )
  meat_yields <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~meat_yield_t_head,
    2010L, 10L, 866L, 0.3,
    2010L, 10L, 1057L, 0.002
  )
  result <- whep:::.meat_to_weight_gain(meat_yields, animals)

  testthat::expect_equal(
    result$weight_gain_kg_day[result$item_cbs_code == 866L],
    (300 / 0.55 - 40) / 547.5
  )
  testthat::expect_true(
    is.na(result$weight_gain_kg_day[result$item_cbs_code == 1057L])
  )
})

# whep#1472: sheep, goats and buffalo are designated to their meat (or to no
# product), so their milk `t_head` rows used to be dropped and every milking
# ewe, doe and buffalo cow reached the energy balance with no lactation energy.
.milked_species_fixture <- function() {
  tibble::tribble(
    ~item_cbs_code, ~unit,    ~value, ~year, ~area_code,
    ~live_anim_code, ~item_prod_code,
    976L,  "heads",  1000,   2020L, 4L, NA_character_, "976",
    976L,  "t_head", 0.12,   2020L, 4L, "976",         "982",
    976L,  "t_head", 0.015,  2020L, 4L, "976",         "977",
    1016L, "heads",  2000,   2020L, 4L, NA_character_, "1016",
    1016L, "t_head", 0.06,   2020L, 4L, "1016",        "1020",
    946L,  "heads",  500,    2020L, 4L, NA_character_, "946",
    946L,  "t_head", 0.8,    2020L, 4L, "946",         "951",
    946L,  "t_head", 0.05,   2020L, 4L, "946",         "947"
  )
}

test_that("sheep, goat and buffalo milk yields are read (whep#1472)", {
  result <- prepare_livestock_emissions(.milked_species_fixture())

  expect_equal(nrow(result), 3L)
  milk <- rlang::set_names(result$milk_yield_kg_day, result$item_cbs_code)
  expect_equal(
    unname(milk[c("976", "1016", "946")]),
    c(0.12, 0.06, 0.8) * 1000 / 365
  )
  # Sheep keep the realised gain of their designated meat.
  expect_equal(
    result$weight_gain_kg_day[result$item_cbs_code == 976L],
    (0.015 * 1000 / 0.45 - 4) / 365
  )
  # Buffalo have no designated product; their meat now gives a realised gain.
  expect_equal(
    result$weight_gain_kg_day[result$item_cbs_code == 946L],
    (0.05 * 1000 / 0.55 - 40) / 547.5
  )
  expect_equal(unique(result$method_milk_yield), "whole_herd")
})

test_that("reported milk goes to the milked cohort and is conserved", {
  result <- prepare_livestock_emissions(
    .milked_species_fixture(),
    expand_cohorts = TRUE
  )
  milked <- result$system %in% "Dairy" & result$cohort %in% "Adult Female"

  # Only milking ewes, does and buffalo cows carry milk.
  expect_true(all(result$milk_yield_kg_day[milked] > 0))
  expect_true(all(result$milk_yield_kg_day[!milked] == 0))
  expect_equal(
    unique(result$method_milk_yield[milked]),
    "milked_cohort"
  )
  # The milk the cohorts carry is the milk FAOSTAT reports, per species.
  carried <- result |>
    dplyr::summarise(
      milk_t = sum(cohort_heads * milk_yield_kg_day) * 365 / 1000,
      .by = item_cbs_code
    ) |>
    dplyr::arrange(item_cbs_code)
  expect_equal(carried$item_cbs_code, c(946L, 976L, 1016L))
  expect_equal(carried$milk_t, c(500 * 0.8, 1000 * 0.12, 2000 * 0.06))
})

test_that("the milking cohorts reach the energy balance with lactation", {
  energy <- .milked_species_fixture() |>
    dplyr::mutate(diet_quality = "Medium") |>
    prepare_livestock_emissions(expand_cohorts = TRUE) |>
    estimate_energy_demand()
  milked <- energy$system %in% "Dairy" & energy$cohort %in% "Adult Female"

  expect_true(all(energy$ne_lactation[milked] > 0))
  expect_true(all(energy$ne_lactation[!milked] == 0))
})

test_that("reported milk with no milked cohort to carry it aborts", {
  shares <- tibble::tribble(
    ~species_gen, ~system, ~system_share,
    "Sheep",      "Meat",  1
  )
  sheep <- .milked_species_fixture() |>
    dplyr::filter(item_cbs_code == 976L)
  expect_error(
    prepare_livestock_emissions(
      sheep,
      expand_cohorts = TRUE,
      system_shares = shares
    ),
    class = "whep_milk_without_milked_cohort"
  )
})

test_that("a species with no cohorts keeps its whole-herd milk yield", {
  camels <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value, ~year, ~area_code,
    ~live_anim_code, ~item_prod_code,
    1126L, "heads",  100,  2020L, 4L, NA_character_, "1126",
    1126L, "t_head", 0.4,  2020L, 4L, "1126",        "1130"
  )
  result <- prepare_livestock_emissions(camels, expand_cohorts = TRUE)

  expect_equal(nrow(result), 1L)
  expect_equal(result$milk_yield_kg_day, 0.4 * 1000 / 365)
  expect_equal(result$method_milk_yield, "whole_herd")
})

test_that("without product codes only the designated product is tagged", {
  # A t_head row with no item_prod_code cannot say whether it is milk or meat,
  # so the co-products added for whep#1472 must not fan it out.
  data <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value, ~year, ~area_code, ~live_anim_code,
    976L,           "heads",  1000,   2020L, 4L,         NA_character_,
    976L,           "t_head", 0.015,  2020L, 4L,         "976"
  )
  result <- prepare_livestock_emissions(data)
  expect_equal(nrow(result), 1L)
  expect_false(rlang::has_name(result, "milk_yield_kg_day"))
})
