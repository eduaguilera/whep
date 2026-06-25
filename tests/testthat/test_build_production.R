# test_build_production.R — unit tests for R/build_production.R helpers

# -- Fixtures ------------------------------------------------------------------

.make_afse_stub <- function() {
  list(
    items_full = tibble::tribble(
      ~item_cbs, ~item_cbs_code, ~comm_group, ~group, ~default_destiny,
      "Wheat", 2511L, "Cereals", "Crop products", "Food",
      "Maize", 2514L, "Cereals", "Crop products", "Feed"
    ),
    items_prod_full = tibble::tribble(
      ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code,
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
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code, ~unit, ~value,
    2000L, "Spain", 203L, "Wheat", 15L, "Wheat", 2511L, "tonnes", 5000,
    2000L, "Spain", 203L, "Wheat", 15L, "Wheat", 2511L, "ha", 200,
    2001L, "Spain", 203L, "Wheat", 15L, "Wheat", 2511L, "tonnes", 5500,
    2001L, "Spain", 203L, "Wheat", 15L, "Wheat", 2511L, "ha", 210
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

test_that(".combine_primary_raw aggregates and keeps item_prod columns", {
  fao_combined <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~unit, ~value,
    2000L, "Spain", 203L, "Wheat", 15L, "t", 5000,
    2000L, "Spain", 203L, "Wheat", 15L, "ha", 200
  ) |>
    dplyr::mutate(source = NA_character_)

  fao_liv_all <- tibble::tibble(
    year = integer(),
    area = character(),
    area_code = integer(),
    item_prod = character(),
    item_prod_code = integer(),
    unit = character(),
    value = double(),
    source = character()
  )

  result <- whep:::.combine_primary_raw(
    fao_combined,
    fao_liv_all
  )
  expect_true("item_prod" %in% names(result))
  expect_true("item_prod_code" %in% names(result))
  expect_true("source" %in% names(result))
  expect_equal(nrow(result), 2L)
  # NA source gets tagged as FAOSTAT
  expect_true(all(result$source == "FAOSTAT_prod"))
})


# -- correct_tea ---------------------------------------------------------------

test_that(".correct_tea divides Tea leaves value by 4.37 after 1990", {
  df <- tibble::tribble(
    ~item_prod, ~item_prod_code, ~unit, ~value, ~year,
    "Tea leaves", 667L, "t", 437, 2000L,
    "Tea leaves", 667L, "t", 437, 1980L,
    "Wheat", 15L, "t", 200, 2000L
  )

  result <- whep:::.correct_tea(df)
  # post-1990 Tea leaves value should be divided by 4.37
  expect_equal(result$value[1], 437 / 4.37)
  # pre-1990 Tea leaves value should remain unchanged
  expect_equal(result$value[2], 437)
  # Wheat unchanged

  expect_equal(result$value[3], 200)
})


# -- deduplication regression tests -------------------------------------------

test_that(".collapse_yield_rows aggregates duplicate key rows", {
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~live_anim_code, ~unit, ~t, ~fu, ~yield_c, ~source, ~Multi_type, ~live_anim,
    2010L, "Spain", 203L, "Milk", 951L, "946", "t_LU", 8, 4, 2, NA_character_, NA_character_, NA_character_,
    2010L, "Spain", 203L, "Milk", 951L, "946", "t_LU", 12, 6, 2, "FAOSTAT_prod", "Primary", "Buffalo",
    2010L, "Spain", 203L, "Wheat", 15L, NA_character_, "t_ha", 20, 10, 2, "FAOSTAT_prod", NA_character_, NA_character_
  )

  result <- whep:::.collapse_yield_rows(df)

  expect_equal(nrow(result), 2L)

  milk <- result |>
    dplyr::filter(item_prod == "Milk")

  expect_equal(milk$t, 20)
  expect_equal(milk$fu, 10)
  expect_equal(milk$yield_c, 2)
  expect_equal(milk$source, "FAOSTAT_prod")
  expect_equal(milk$Multi_type, "Primary")
  expect_equal(milk$live_anim, "Buffalo")
})

test_that(".collapse_cbs_ratio_rows aggregates duplicate ratio rows", {
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code, ~unit, ~group, ~t, ~fu, ~yield_c, ~yield_glo, ~t_cbs, ~prod_cbs_ratio, ~prod_cbs_count, ~sumprod_cbs_ratio, ~source, ~Multi_type,
    2010L, "Spain", 203L, "Wheat", 15L, "Wheat and products", 2511L, NA_character_, NA_character_, "t_ha", "Crop products", 8, 4, 2, 2, 20, 0.4, 2, 0.4, NA_character_, NA_character_,
    2010L, "Spain", 203L, "Wheat", 15L, "Wheat and products", 2511L, NA_character_, NA_character_, "t_ha", "Crop products", 12, 6, 2, 2, 20, 0.6, 2, 0.6, "FAOSTAT_prod", "Primary"
  )

  result <- whep:::.collapse_cbs_ratio_rows(df)

  expect_equal(nrow(result), 1L)
  expect_equal(result$t, 20)
  expect_equal(result$fu, 10)
  expect_equal(result$yield_c, 2)
  expect_equal(result$yield_glo, 2)
  expect_equal(result$t_cbs, 40)
  expect_equal(result$prod_cbs_ratio, 0.5)
  expect_equal(result$prod_cbs_count, 2)
  expect_equal(result$sumprod_cbs_ratio, 0.5)
  expect_equal(result$source, "FAOSTAT_prod")
  expect_equal(result$Multi_type, "Primary")
})

test_that(".compute_cbs_ratios handles duplicate year rows without warning", {
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code, ~unit, ~group, ~t, ~fu, ~yield_c, ~yield_glo, ~t_cbs, ~source, ~Multi_type,
    2010L, "Spain", 203L, "Wheat", 15L, "Wheat and products", 2511L, NA_character_, NA_character_, "t_ha", "Crop products", 8, 4, 2, 2, 20, "FAOSTAT_prod", NA_character_,
    2010L, "Spain", 203L, "Wheat", 15L, "Wheat and products", 2511L, NA_character_, NA_character_, "t_ha", "Crop products", 12, 6, 2, 2, 20, "FAOSTAT_prod", "Primary"
  )

  result <- expect_no_warning(
    whep:::.compute_cbs_ratios(df)
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$prod_cbs_count, 2)
})


# -- .fill_yields item join ----------------------------------------------------

test_that(".fill_yields joins items_prod by item_prod_code only", {
  items_prod <- tibble::tribble(
    ~item_prod_code, ~item_cbs_code, ~group,
    "15", 2511L, "Primary crops",
    "305", 2570L, "Primary crops"
  )

  yield_data <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~live_anim_code, ~unit, ~t, ~fu, ~yield_c,
    ~source, ~Multi_type, ~live_anim, ~yield_glo,
    2000L, "Spain", 203L, "Wheat WRONG NAME", "15",
    NA_character_, "t_ha", 100, 50, 2,
    "FAOSTAT_prod", NA_character_, NA_character_, 2
  )

  result <- dplyr::left_join(
    yield_data,
    items_prod |>
      dplyr::select(item_prod_code, item_cbs_code, group) |>
      dplyr::distinct(item_prod_code, .keep_all = TRUE),
    by = "item_prod_code"
  )
  expect_equal(result$item_cbs_code, 2511L)
  expect_equal(result$group, "Primary crops")
})


# -- year range defaults -------------------------------------------------------

test_that("build_primary_production defaults to end_year 2023", {
  formals_prod <- formals(whep::build_primary_production)
  expect_equal(formals_prod$end_year, 2023)
})

test_that(".extend_historical keeps modern rows when LUH2 land columns are absent", {
  primary <- tibble::tibble(
    year = 2023L,
    area = "Spain",
    area_code = 203L,
    item_prod = "Wheat",
    item_prod_code = 15L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = "tonnes",
    value = 100,
    source = "FAOSTAT_prod"
  )
  years <- tibble::tibble(year = 2023L)
  land <- tibble::tibble(
    year = 2023L,
    area = "Spain",
    Land_Use = "urban",
    Area_Mha = 1
  )

  result <- whep:::.extend_historical(primary, years, land)

  expect_equal(nrow(result), 1L)
  expect_equal(result$year, 2023L)
  expect_equal(result$value, 100)
})

test_that(".prepare_historical_production normalizes generic historical rows", {
  historical <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value, ~source,
    1950L, 203L, "15.0", "tonnes", 100, "future_source",
    1950L, 203L, "15.0", "tonnes", 120, "historical_future_source",
    1800L, 203L, "15.0", "tonnes", 999, "future_source"
  )

  result <- whep:::.prepare_historical_production(
    historical,
    years = 1950:1951
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$year, 1950L)
  expect_equal(result$area, "Spain")
  expect_equal(result$item_prod, "Wheat")
  expect_equal(result$item_prod_code, "15")
  expect_equal(result$item_cbs_code, 2511L)
  expect_equal(result$unit, "tonnes")
  expect_equal(result$value, 110)
  expect_true(stringr::str_starts(result$source, "historical_"))
  # Prod-side item codes must be character to bind with the FAOSTAT pipeline
  # (primary_raw2). live_anim_code being integer broke build on real data.
  expect_type(result$item_prod_code, "character")
  expect_type(result$live_anim_code, "character")
})

test_that(".extend_historical uses historical rows as LUH2 anchors", {
  primary <- tibble::tibble(
    year = c(1950L, 1961L),
    area = "Spain",
    area_code = 203L,
    item_prod = "Wheat",
    item_prod_code = "15",
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = "tonnes",
    value = c(50, 100),
    source = c("historical_test", "FAOSTAT_prod")
  )
  years <- tibble::tibble(year = 1949:1961)
  land <- tibble::tibble(
    year = 1949:1961,
    area = "Spain",
    Land_Use = "c3ann",
    Area_Mha = 1
  )

  result <- whep:::.extend_historical(primary, years, land)

  observed <- result |>
    dplyr::filter(.data$year == 1950L, .data$unit == "tonnes")
  filled <- result |>
    dplyr::filter(.data$year == 1951L, .data$unit == "tonnes")

  expect_equal(observed$value, 50)
  expect_equal(observed$source, "historical_test")
  expect_equal(filled$source, "historical_LUH2_cropland")
  expect_false(is.na(filled$value))
})

test_that(".add_historical_yields preserves direct historical tonnes", {
  df <- tibble::tibble(
    year = 1950L,
    area = "Spain",
    area_code = 203L,
    item_prod = "Wheat",
    item_prod_code = "15",
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    land_use = "Cropland",
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = c("tonnes", "ha"),
    value = c(50, 10),
    source = "historical_test"
  )
  int_yields <- tibble::tibble(
    year = 1950L,
    area = "Spain",
    item_prod_code = "15",
    yield = 999
  )

  result <- whep:::.add_historical_yields(df, int_yields)

  expect_equal(result$tonnes, 50)
  expect_equal(result$t_ha, 5)
})


# -- rice unit convention ------------------------------------------------------

test_that(".fix_rice_milled_equiv converts paddy production only", {
  rate <- whep:::.rice_milled_extraction_rate()
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code,
    ~unit, ~value, ~source,
    2000L, "China", 41L, "Rice", "27",
    "Rice and products", 2807L, NA, NA,
    "tonnes", 100, "FAOSTAT_prod",
    2000L, "China", 41L, "Rice", "27",
    "Rice and products", 2807L, NA, NA,
    "t_ha", 10, "imputed_yield:Global",
    2000L, "China", 41L, "Rice", "27",
    "Rice and products", 2807L, NA, NA,
    "ha", 20, "FAOSTAT_prod",
    2000L, "China", 41L, "Rice", "27",
    "Rice and products", 2807L, NA, NA,
    "tonnes", 80, "imputed_cbs_ratio",
    2000L, "China", 41L, "Rice", "27",
    "Rice and products", 2807L, NA, NA,
    "tonnes", 200, "historical_mitchell",
    2000L, "China", 41L, "Wheat", "15",
    "Wheat and products", 2511L, NA, NA,
    "tonnes", 50, "FAOSTAT_prod"
  )

  result <- whep:::.fix_rice_milled_equiv(df) |>
    dplyr::arrange(.data$item_prod_code, .data$unit, .data$source)

  rice <- result |>
    dplyr::filter(.data$item_prod_code == "27")

  testthat::expect_equal(
    rice$value[rice$unit == "tonnes" & rice$source == "FAOSTAT_prod"],
    100 * rate
  )
  testthat::expect_equal(
    rice$value[rice$unit == "t_ha"],
    10 * rate
  )
  testthat::expect_equal(
    rice$value[rice$unit == "ha"],
    20
  )
  testthat::expect_equal(
    rice$value[rice$source == "imputed_cbs_ratio"],
    80
  )
  # observed historical rice is paddy too and must be milled-converted
  testthat::expect_equal(
    rice$value[rice$source == "historical_mitchell"],
    200 * rate
  )
  testthat::expect_equal(
    result$value[result$item_prod_code == "15"],
    50
  )
})


# -- deduplication --------------------------------------------------------------

test_that(".dedup_production keeps highest-priority source", {
  duped <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code,
    ~unit, ~value, ~source,
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "t", 100, "imputed_yield",
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "t", 200, "FAOSTAT_prod",
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "ha", 10, "LUH2_cropland",
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "ha", 20, "EuropeAgriDB"
  )

  result <- whep:::.dedup_production(duped)
  expect_equal(nrow(result), 2L)

  tonnes_row <- result |>
    dplyr::filter(unit == "t")
  expect_equal(tonnes_row$source, "FAOSTAT_prod")
  expect_equal(tonnes_row$value, 200)

  ha_row <- result |>
    dplyr::filter(unit == "ha")
  expect_equal(ha_row$source, "EuropeAgriDB")
  expect_equal(ha_row$value, 20)
})

test_that(".show_prod_duplicates returns wide format of competing sources", {
  duped <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code,
    ~unit, ~value, ~source,
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "t", 100, "imputed_yield",
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "t", 200, "FAOSTAT_prod",
    2000L, "Spain", 203L, "Maize", 56L,
    "Maize", 2514L, NA, NA, "t", 50, "FAOSTAT_prod"
  )

  result <- whep:::.show_prod_duplicates(duped)
  # Only the duplicated key (Wheat/t) should appear
  expect_equal(nrow(result), 1L)
  # Columns should include the two competing sources
  expect_true("FAOSTAT_prod" %in% names(result))
  expect_true("imputed_yield" %in% names(result))
  # FAOSTAT_prod column first because it has higher priority
  src_cols <- setdiff(
    names(result),
    c("year", "area_code", "item_prod_code", "unit")
  )
  expect_equal(src_cols[1], "FAOSTAT_prod")
})

test_that("build_primary_production output has no duplicate keys", {
  result <- whep::build_primary_production(example = TRUE)
  keys <- dplyr::select(
    result,
    year,
    area_code,
    item_prod_code,
    unit
  )
  expect_equal(nrow(keys), nrow(dplyr::distinct(keys)))
})
