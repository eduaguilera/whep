# test_build_production.R — unit tests for R/build_production.R helpers

# -- Fixtures ------------------------------------------------------------------

.make_afse_stub <- function() {
  list(
    items_full = tibble::tribble(
      ~item_cbs, ~item_code_cbs, ~comm_group, ~group,
      ~default_destiny,
      "Wheat", 2511L, "Cereals", "Crop products", "Food",
      "Maize", 2514L, "Cereals", "Crop products", "Feed"
    ),
    items_prod_full = tibble::tribble(
      ~item_prod, ~item_code_prod, ~item_cbs, ~item_code_cbs,
      ~live_anim, ~live_anim_code,
      "Wheat", 15L, "Wheat", 2511L, NA_character_, NA_integer_,
      "Maize", 56L, "Maize", 2514L, NA_character_, NA_integer_
    ),
    regions_full = tibble::tribble(
      ~polity_name, ~polity_code, ~iso3c,
      "Spain", 203L, "ESP",
      "France", 68L, "FRA"
    ),
    polities_cats = tibble::tribble(
      ~polity_name, ~polity_code, ~dissolved,
      "Spain", 203L, FALSE,
      "France", 68L, FALSE
    ),
    Primary_double = tibble::tibble(
      Item_area = character(),
      multi_type = character()
    ),
    NoDataProducts = character()
  )
}

.make_primary_raw <- function() {
  tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_code_prod,
    ~item_cbs, ~item_code_cbs, ~unit, ~value,
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, "tonnes", 5000,
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, "ha", 200,
    2001L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, "tonnes", 5500,
    2001L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, "ha", 210
  )
}


# -- filter_dissolved_countries ------------------------------------------------

test_that(".filter_dissolved_countries removes dissolved polities", {
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~value,
    2000L, "Spain", 203L, 10,
    2000L, "Czechoslovakia", 999L, 20,
    1990L, "Czechoslovakia", 999L, 30
  )

  result <- whep:::.filter_dissolved_countries(df)
  # Czechoslovakia after 1992 should be removed
  expect_false(
    any(result$area == "Czechoslovakia" & result$year > 1992)
  )
  # Czechoslovakia before 1993 should be kept
  expect_true(
    any(result$area == "Czechoslovakia" & result$year == 1990)
  )
})


# -- combine_primary -----------------------------------------------------------

test_that(".combine_primary aggregates and keeps item_prod columns", {
  afse <- .make_afse_stub()

  fao_combined <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_code_prod,
    ~unit, ~value,
    2000L, "Spain", 203L, "Wheat", 15L, "t", 5000,
    2000L, "Spain", 203L, "Wheat", 15L, "ha", 200
  ) |>
    dplyr::mutate(source = NA_character_)

  fao_liv_all <- tibble::tibble(
    year = integer(), area = character(),
    area_code = integer(),
    item_prod = character(), item_code_prod = integer(),
    unit = character(), value = double(),
    source = character()
  )

  result <- whep:::.combine_primary(
    fao_combined, fao_liv_all, afse
  )
  expect_true("item_prod" %in% names(result))
  expect_true("item_code_prod" %in% names(result))
  expect_true("source" %in% names(result))
  expect_equal(nrow(result), 2L)
  # NA source gets tagged as FAOSTAT
  expect_true(all(result$source == "FAOSTAT"))
})


# -- correct_tea ---------------------------------------------------------------

test_that(".correct_tea divides Tea leaves value by 4.37 after 1990", {
  df <- tibble::tribble(
    ~item_prod, ~item_code_prod, ~unit, ~value, ~year,
    "Tea leaves", 667L, "t", 437, 2000L,
    "Tea leaves", 667L, "t", 437, 1980L,
    "Wheat",      15L,  "t", 200, 2000L
  )

  result <- whep:::.correct_tea(df)
  # post-1990 Tea leaves value should be divided by 4.37
  expect_equal(result$value[1], 437 / 4.37)
  # pre-1990 Tea leaves value should remain unchanged
  expect_equal(result$value[2], 437)
  # Wheat unchanged

  expect_equal(result$value[3], 200)
})
