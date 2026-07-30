# A country-resolution exceedance fixture with two crops in an exceeding
# country (crop 2511 over the boundary, crop 2513 fully within) plus a second
# country whose crop sits entirely within the boundary.
.sjos_class_exceedance_fixture <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~exceedance_n_t,
    ~within_boundary_n_t,
    ~actual_n_t,
    2010L, 10L, 2511L, 5, 3, 8,
    2010L, 10L, 2513L, 0, 4, 4,
    2010L, 20L, 2511L, 0, 6, 6
  )
}

# Nourishment classes: country 10 over-nourished, country 20 under-nourished.
.sjos_class_nourish_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~nourish,
    2010L, 10L, "Over",
    2010L, 20L, "Under"
  )
}

testthat::test_that("an exceeding crop in an Over country is Exceedance Over", {
  out <- whep::classify_sjos_n(
    .sjos_class_exceedance_fixture(),
    .sjos_class_nourish_fixture()
  )
  row <- dplyr::filter(out, area_code == 10L, item_cbs_code == 2511L)
  testthat::expect_equal(row$boundary_side, "Exceedance")
  testthat::expect_equal(as.character(row$sjos_class), "Exceedance Over")
})

testthat::test_that("a within-boundary crop in an Under country is classed", {
  out <- whep::classify_sjos_n(
    .sjos_class_exceedance_fixture(),
    .sjos_class_nourish_fixture()
  )
  row <- dplyr::filter(out, area_code == 20L, item_cbs_code == 2511L)
  testthat::expect_equal(row$boundary_side, "Within_boundary")
  testthat::expect_equal(as.character(row$sjos_class), "Within_boundary Under")
})

testthat::test_that("sjos_class is a factor over all six sjos_levels", {
  out <- whep::classify_sjos_n(
    .sjos_class_exceedance_fixture(),
    .sjos_class_nourish_fixture()
  )
  testthat::expect_s3_class(out$sjos_class, "factor")
  testthat::expect_equal(levels(out$sjos_class), whep::sjos_levels$level)
  testthat::expect_length(levels(out$sjos_class), 6L)
})

testthat::test_that("the country nourishment broadcasts to multiple crops", {
  out <- whep::classify_sjos_n(
    .sjos_class_exceedance_fixture(),
    .sjos_class_nourish_fixture()
  )
  country10 <- dplyr::filter(out, area_code == 10L)
  # Both crops of country 10 inherit its single "Over" class.
  testthat::expect_equal(nrow(country10), 2L)
  testthat::expect_true(all(country10$nourish == "Over"))
})

testthat::test_that("known zero pressure is within and missing evidence is NA", {
  exceedance <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~exceedance_n_t,
    ~within_boundary_n_t,
    ~actual_n_t,
    2010L, 30L, 2511L, 0, 0, 0,
    2010L, 30L, 2513L, NA_real_, 2, 2
  )
  nourishment <- tibble::tribble(
    ~year, ~area_code, ~nourish,
    2010L, 30L, "Adequate"
  )
  out <- whep::classify_sjos_n(exceedance, nourishment)
  zero <- dplyr::filter(out, .data$item_cbs_code == 2511L)
  missing <- dplyr::filter(out, .data$item_cbs_code == 2513L)
  testthat::expect_equal(zero$boundary_side, "Within_boundary")
  testthat::expect_equal(
    as.character(zero$sjos_class),
    "Within_boundary Adequate"
  )
  testthat::expect_true(is.na(missing$boundary_side))
  testthat::expect_true(is.na(missing$sjos_class))
})

testthat::test_that("level_col renames the classification column", {
  out <- whep::classify_sjos_n(
    .sjos_class_exceedance_fixture(),
    .sjos_class_nourish_fixture(),
    level_col = impact_class
  )
  pointblank::expect_col_exists(out, "impact_class")
  testthat::expect_false(rlang::has_name(out, "sjos_class"))
})
