# A two-country, one-item economy: country 1 produces 100 t using
# 50 ha and exports 40 t to country 2.
.balance_inputs <- function() {
  list(
    production = tibble::tibble(
      area_code = c(1L, 2L),
      item_cbs_code = c(10L, 10L),
      value = c(100, 0)
    ),
    trade = tibble::tibble(
      from_code = 1L,
      to_code = 2L,
      item_cbs_code = 10L,
      value = 40
    ),
    extension = tibble::tibble(
      area_code = c(1L, 2L),
      item_cbs_code = c(10L, 10L),
      value = c(50, 0)
    )
  )
}

testthat::test_that("land balance conserves direct land across consumers", {
  inp <- .balance_inputs()
  fp <- whep::compute_footprint_balance(
    inp$production,
    inp$trade,
    inp$extension
  )

  fp |>
    pointblank::expect_col_exists(
      c("area_code", "item_cbs_code", "value", "method")
    )
  # Importer 2 gets 40 t * 0.5 ha/t = 20 ha; producer 1 keeps 60 t
  # * 0.5 = 30 ha. Total embodied equals the 50 ha of direct land.
  by_area <- fp |> dplyr::arrange(area_code)
  testthat::expect_equal(by_area$value, c(30, 20))
  testthat::expect_equal(sum(fp$value), 50)
  testthat::expect_true(all(fp$method == "land_balance"))
})

testthat::test_that("land balance returns empty tibble for no data", {
  empty <- whep::compute_footprint_balance(
    tibble::tibble(
      area_code = integer(),
      item_cbs_code = integer(),
      value = numeric()
    ),
    tibble::tibble(
      from_code = integer(),
      to_code = integer(),
      item_cbs_code = integer(),
      value = numeric()
    ),
    tibble::tibble(
      area_code = integer(),
      item_cbs_code = integer(),
      value = numeric()
    )
  )
  testthat::expect_equal(nrow(empty), 0)
})

testthat::test_that("land balance validates inputs", {
  testthat::expect_error(
    whep::compute_footprint_balance(
      tibble::tibble(area_code = 1L),
      tibble::tibble(
        from_code = 1L,
        to_code = 2L,
        item_cbs_code = 1L,
        value = 1
      ),
      tibble::tibble(area_code = 1L, item_cbs_code = 1L, value = 1)
    ),
    "missing column"
  )
})

testthat::test_that("compare_footprint_methods reports differences", {
  a <- tibble::tibble(
    area_code = c(1L, 2L),
    item_cbs_code = c(10L, 10L),
    value = c(30, 20)
  )
  b <- tibble::tibble(
    area_code = c(1L, 2L),
    item_cbs_code = c(10L, 10L),
    value = c(25, 20)
  )
  cmp <- whep::compare_footprint_methods(a, b)

  cmp |>
    pointblank::expect_col_exists(
      c("value_a", "value_b", "abs_diff", "rel_diff")
    )
  top <- cmp |> dplyr::slice(1)
  testthat::expect_equal(top$area_code, 1L)
  testthat::expect_equal(top$abs_diff, 5)
})

testthat::test_that("compare handles items present in only one method", {
  a <- tibble::tibble(area_code = 1L, item_cbs_code = 10L, value = 30)
  b <- tibble::tibble(area_code = 1L, item_cbs_code = 99L, value = 12)
  cmp <- whep::compare_footprint_methods(a, b)

  testthat::expect_equal(nrow(cmp), 2)
  testthat::expect_equal(sum(cmp$value_a), 30)
  testthat::expect_equal(sum(cmp$value_b), 12)
})

# melt_bilateral_trade ------------------------------------------------

testthat::test_that("melt_bilateral_trade melts items and drops self-trade", {
  m1 <- matrix(
    c(0, 40, 5, 0),
    nrow = 2,
    dimnames = list(c("1", "2"), c("1", "2"))
  )
  m2 <- matrix(
    c(0, 0, 7, 0),
    nrow = 2,
    dimnames = list(c("1", "2"), c("1", "2"))
  )
  bt <- tibble::tibble(
    year = c(2010L, 2010L),
    item_cbs_code = c(10L, 20L),
    bilateral_trade = list(m1, m2)
  )
  out <- whep::melt_bilateral_trade(bt)

  out |>
    pointblank::expect_col_exists(
      c("year", "from_code", "to_code", "item_cbs_code", "value")
    )
  testthat::expect_equal(nrow(out), 3)
  testthat::expect_true(all(out$from_code != out$to_code))
  testthat::expect_setequal(out$value, c(40, 5, 7))
})

testthat::test_that("melt_bilateral_trade requires the matrix columns", {
  testthat::expect_error(
    whep::melt_bilateral_trade(tibble::tibble(year = 2010L)),
    "missing column"
  )
})

# build_land_balance_footprint ----------------------------------------

testthat::test_that("build_land_balance_footprint uses injected inputs", {
  inp <- .balance_inputs()
  out <- whep::build_land_balance_footprint(
    2010,
    production = inp$production,
    trade = inp$trade,
    extension = inp$extension
  )

  by_area <- dplyr::arrange(out, area_code)
  testthat::expect_equal(by_area$value, c(30, 20))
  testthat::expect_true(all(out$method == "land_balance"))
})

testthat::test_that("build_land_balance_footprint example has output schema", {
  ex <- whep::build_land_balance_footprint(example = TRUE)

  ex |>
    pointblank::expect_col_exists(
      c("area_code", "item_cbs_code", "value", "method")
    )
  testthat::expect_true(all(ex$method == "land_balance"))
})

testthat::test_that("build_land_balance_footprint validates year", {
  inp <- .balance_inputs()
  testthat::expect_error(
    whep::build_land_balance_footprint(
      "nope",
      production = inp$production,
      trade = inp$trade,
      extension = inp$extension
    ),
    "single number"
  )
})
