# `cft_mapping` decides which harvested areas reach the grid at all:
# `prepare_country_areas()` (`inst/scripts/prepare_spatialize_all.R`) keeps
# only the production rows whose `item_prod_code` it lists, and
# `build_gridded_landuse()` takes that table as its only harvested-area input.
# A crop whose area sits on a code the mapping does not list is dropped from
# every country and every year, with no error.
#
# That is how coconut, linum, kapok and hemp went missing (whep#1292): the
# mapping listed 249, 333, 773, 336, 777 and 778, while
# `build_primary_production()` books their area on 248, 772, 310 and 776
# (`primary_double.csv`) and leaves the listed codes holding tonnes only.
# 13.86 Mha of 2010 harvested area never reached the grid. These tests are
# the guard; they need neither the pins nor the network.

# Every item `build_primary_production(start_year = 2010, end_year = 2010)`
# emits with `unit == "ha"` and a positive value, with its world total and
# the number of reporting areas. Snapshot taken on `main` at 8af8fd0e.
.production_ha_items <- function() {
  readr::read_csv(
    testthat::test_path("fixtures", "production_ha_items_2010.csv"),
    col_types = readr::cols(
      item_prod_code = readr::col_integer(),
      item_prod_name = readr::col_character(),
      harvested_area_ha = readr::col_double(),
      n_areas = readr::col_integer()
    )
  )
}

# The items with harvested area that `cft_mapping` leaves out, each with the
# reason. Asserting the gap is EXACTLY this set makes the test fail both ways:
# an item that loses its row fails here, and one that gains a row has to be
# removed from the list.
.cft_excluded_ha_items <- function() {
  fodder <- "Fodder crop: not gridded as cropland, pending whep#1372"
  missing <- "Crop not yet mapped to a CFT (whep#1364)"
  tibble::tribble(
    ~item_prod_code, ~why,
    3001L, "Permanent pasture: gridded separately, not cropland",
    3002L, "Rangeland: gridded separately, not cropland",
    996L, fodder,
    636L, fodder,
    637L, fodder,
    638L, fodder,
    639L, fodder,
    640L, fodder,
    641L, fodder,
    642L, fodder,
    643L, fodder,
    644L, fodder,
    645L, fodder,
    646L, fodder,
    647L, fodder,
    648L, fodder,
    649L, fodder,
    651L, fodder,
    655L, fodder,
    149L, missing,
    161L, missing,
    420L, missing,
    459L, missing,
    542L, missing,
    591L, missing,
    782L, missing,
    809L, missing
  )
}

# The mapped items with no harvested area in the 2010 build. Each is a crop
# FAOSTAT reports in tonnes only, not a sibling of a code that holds the area.
.cft_items_without_area <- function() {
  tibble::tribble(
    ~item_prod_code, ~why,
    216L, "Brazil nuts, in shell -- wild-harvested, no area reported",
    378L, "Cassava leaves -- the area is the root's (125)",
    449L, "Mushrooms and truffles -- no area reported",
    839L, "Balata, gutta-percha and similar gums -- no area reported"
  )
}

testthat::test_that("every item with harvested area is mapped or excluded", {
  ha_items <- .production_ha_items()$item_prod_code
  mapped <- whep::cft_mapping$item_prod_code
  excluded <- .cft_excluded_ha_items()

  unaccounted <- setdiff(ha_items, c(mapped, excluded$item_prod_code))
  testthat::expect_equal(unaccounted, integer(0))
  # A stale exclusion is a gap that the list would hide.
  testthat::expect_false(anyDuplicated(excluded$item_prod_code) > 0)
  testthat::expect_true(all(excluded$item_prod_code %in% ha_items))
  testthat::expect_false(any(excluded$item_prod_code %in% mapped))
})

testthat::test_that("every mapped item carries harvested area", {
  ha_items <- .production_ha_items()$item_prod_code
  mapped <- whep::cft_mapping$item_prod_code

  no_area <- sort(setdiff(mapped, ha_items))
  testthat::expect_equal(
    no_area,
    sort(.cft_items_without_area()$item_prod_code)
  )
})

testthat::test_that("co-products are mapped on the code that holds their area", {
  double <- whep::primary_double |>
    dplyr::mutate(item_prod_code = as.integer(.data$item_prod_code))
  area_codes <- double |>
    dplyr::filter(.data$Multi_type %in% c("Multi_area", "Primary_area")) |>
    dplyr::pull("item_prod_code")
  products <- double |>
    dplyr::filter(.data$Multi_type %in% c("Multi", "Primary")) |>
    dplyr::pull("item_prod_code")
  mapped <- whep::cft_mapping$item_prod_code

  testthat::expect_setequal(intersect(area_codes, mapped), area_codes)
  testthat::expect_equal(intersect(products, mapped), integer(0))
})

testthat::test_that("coconut, linum, kapok and hemp keep their component CFT", {
  restored <- whep::cft_mapping |>
    dplyr::filter(.data$item_prod_code %in% c(248L, 310L, 772L, 776L)) |>
    dplyr::arrange(.data$item_prod_code)

  # Each has the LPJmL stand and LUH2 type its components had; LandInG's
  # default table puts coconuts and kapok fruit on "Others, perennial" and
  # linseed, flax and hemp on "Others, annual".
  testthat::expect_equal(restored$item_prod_code, c(248L, 310L, 772L, 776L))
  testthat::expect_equal(restored$cft_lpjml, rep("others", 4L))
  testthat::expect_equal(
    restored$luh2_type,
    c("c3per", "c3per", "c3ann", "c3ann")
  )
  testthat::expect_equal(
    restored$cft_name,
    c("oil_crops_coconut", "fibres_other", "oil_crops_other", "fibres_other")
  )
})

testthat::test_that("the fixture holds the area the four crops lost", {
  restored <- .production_ha_items() |>
    dplyr::filter(.data$item_prod_code %in% c(248L, 310L, 772L, 776L))

  testthat::expect_equal(nrow(restored), 4L)
  # 13.86 Mha in 2010, the figure whep#1292 reports.
  testthat::expect_equal(
    sum(restored$harvested_area_ha) / 1e6,
    13.86,
    tolerance = 0.005
  )
})
