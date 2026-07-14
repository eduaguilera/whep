# A country-resolution exceedance fixture: crop 2511 partly exceeds, crop 2513
# is fully within the boundary (zero exceedance).
.nex_exceedance_fixture <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~exceedance_n_t,
    ~within_boundary_n_t,
    ~actual_n_t,
    2010L, 10L, 2511L, 5, 3, 8,
    2010L, 10L, 2513L, 0, 4, 4
  )
}

testthat::test_that("the exceedance category carries exceedance_n_t", {
  out <- whep::build_n_exceedance_extension(
    .nex_exceedance_fixture(),
    category = "exceedance"
  )
  testthat::expect_equal(out$impact_u, c(5, 0))
  testthat::expect_true(all(out$method_n_exceedance == "exceedance"))
})

testthat::test_that("the within_boundary category carries within_boundary_n_t", {
  out <- whep::build_n_exceedance_extension(
    .nex_exceedance_fixture(),
    category = "within_boundary"
  )
  testthat::expect_equal(out$impact_u, c(3, 4))
  testthat::expect_true(all(out$method_n_exceedance == "within_boundary"))
})

testthat::test_that("the production category carries actual_n_t", {
  out <- whep::build_n_exceedance_extension(
    .nex_exceedance_fixture(),
    category = "production"
  )
  testthat::expect_equal(out$impact_u, c(8, 4))
  testthat::expect_true(all(out$method_n_exceedance == "production"))
})

testthat::test_that("the default category is exceedance", {
  out <- whep::build_n_exceedance_extension(.nex_exceedance_fixture())
  testthat::expect_true(all(out$method_n_exceedance == "exceedance"))
})

testthat::test_that("the extension contract columns exist", {
  out <- whep::build_n_exceedance_extension(.nex_exceedance_fixture())
  pointblank::expect_col_exists(
    out,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_n_exceedance")
  )
})

testthat::test_that("an unknown category is rejected", {
  testthat::expect_error(
    whep::build_n_exceedance_extension(
      .nex_exceedance_fixture(),
      category = "surplus"
    )
  )
})

testthat::test_that("the per-crop key is preserved and zero-impact kept", {
  out <- whep::build_n_exceedance_extension(
    .nex_exceedance_fixture(),
    category = "exceedance"
  )
  # Both crops survive: the zero-exceedance crop 2513 is kept (it still trades).
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_setequal(out$item_cbs_code, c(2511L, 2513L))
})

testthat::test_that("rows with a missing key are dropped", {
  exceedance <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~exceedance_n_t,
    ~within_boundary_n_t,
    ~actual_n_t,
    2010L, 10L, 2511L, 5, 3, 8,
    2010L, NA_integer_, 2513L, 1, 1, 2
  )
  out <- whep::build_n_exceedance_extension(exceedance)
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$item_cbs_code, 2511L)
})
