testthat::test_that("footprint_scope builds a one-row record with defaults", {
  scope <- whep::footprint_scope("cropland", "ha", "FABIO-MRIO")

  scope |>
    pointblank::expect_col_exists(
      c(
        "stressor",
        "units",
        "method",
        "boundary",
        "allocation",
        "vintage",
        "limitations"
      )
    )
  testthat::expect_equal(nrow(scope), 1)
  testthat::expect_equal(scope$allocation, "mass")
  testthat::expect_true(is.na(scope$vintage))
})

testthat::test_that("footprint_scope applies detail overrides", {
  scope <- whep::footprint_scope(
    "cropland",
    "ha",
    "land-balance",
    details = list(vintage = "1850-2023", allocation = "economic")
  )

  testthat::expect_equal(scope$vintage, "1850-2023")
  testthat::expect_equal(scope$allocation, "economic")
  testthat::expect_equal(scope$method, "land-balance")
})

testthat::test_that("footprint_scope rejects bad inputs", {
  testthat::expect_error(
    whep::footprint_scope("", "ha", "m"),
    "non-empty string"
  )
  testthat::expect_error(
    whep::footprint_scope("a", "ha", "m", details = list(bogus = 1)),
    "Unknown"
  )
})

testthat::test_that("attach/get scope round-trips", {
  scope <- whep::footprint_scope("cropland", "ha", "FABIO-MRIO")
  result <- whep::attach_scope(tibble::tibble(value = 1), scope)

  testthat::expect_equal(whep::get_scope(result), scope)
  testthat::expect_null(whep::get_scope(tibble::tibble(value = 1)))
})
