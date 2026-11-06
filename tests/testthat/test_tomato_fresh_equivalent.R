tomato_trade_fixture <- function() {
  tibble::tribble(
    ~reporter, ~flow,    ~cn8,       ~value,
    "DE",      "import", "07020000", 700,
    "DE",      "import", "20021010", 200,
    "DE",      "import", "20029019", 50,
    "DE",      "import", "20029031", 120,
    "DE",      "import", "20029091", 50,
    "DE",      "import", "20095090", 10,
    "DE",      "import", "21032000", 90,
    "DE",      "import", "07129030", 8,
    "DE",      "export", "07020000", 20,
    "DE",      "export", "20029091", 15
  )
}

testthat::test_that("each factor set converts every FBS 2601 product class", {
  classes <- whep:::.tomato_cn8_classes() |>
    dplyr::filter(in_fbs_2601) |>
    dplyr::pull(product_class) |>
    unique()
  factors <- whep:::.tomato_fresh_eq_factors()

  for (set in unique(factors$factor_set)) {
    in_set <- factors |>
      dplyr::filter(factor_set == set)
    testthat::expect_setequal(in_set$product_class, classes)
  }
  testthat::expect_equal(
    nrow(dplyr::distinct(factors, product_class, factor_set)),
    nrow(factors)
  )
})

testthat::test_that("the dry-matter bounds bracket the other sets", {
  wide <- whep:::.tomato_fresh_eq_factors() |>
    tidyr::pivot_wider(
      names_from = factor_set,
      values_from = kg_fresh_per_kg
    )

  testthat::expect_true(all(wide$dm_bound_low >= 1))
  testthat::expect_true(all(wide$dm_bound_low <= wide$grade_typical))
  testthat::expect_true(all(wide$grade_typical <= wide$dm_bound_high))
  fresh <- wide |>
    dplyr::filter(product_class == "fresh")
  testthat::expect_equal(fresh$dm_bound_low, 1)
  testthat::expect_equal(fresh$dm_bound_high, 1)
})

testthat::test_that("fresh equivalent sums product tonnes times factors", {
  out <- whep:::.tomato_fresh_equivalent(
    tomato_trade_fixture(),
    factor_sets = c("fao_tcf_modal", "grade_typical")
  )

  modal_import <- out |>
    dplyr::filter(flow == "import", factor_set == "fao_tcf_modal")
  testthat::expect_equal(
    modal_import$fresh_equivalent,
    700 + 200 / 0.8 + (50 + 120 + 50) / 0.3 + 10 / 0.8
  )
  # Ketchup and dried tomatoes are outside FBS 2601 and are not converted.
  testthat::expect_equal(modal_import$value, 700 + 200 + 50 + 120 + 50 + 10)

  typical_import <- out |>
    dplyr::filter(flow == "import", factor_set == "grade_typical")
  testthat::expect_equal(
    typical_import$fresh_equivalent,
    700 + 200 / 0.78 + 50 * 2.4 + 120 * 5.6 + 50 * 7.2 + 10 / 0.74
  )

  typical_export <- out |>
    dplyr::filter(flow == "export", factor_set == "grade_typical")
  testthat::expect_equal(typical_export$fresh_equivalent, 20 + 15 * 7.2)
  testthat::expect_named(
    out,
    c("reporter", "flow", "factor_set", "fresh_equivalent", "value"),
    ignore.order = TRUE
  )
})

testthat::test_that("an unknown CN8 code aborts instead of being dropped", {
  trade <- tomato_trade_fixture() |>
    tibble::add_row(
      reporter = "DE",
      flow = "import",
      cn8 = "20019065",
      value = 5
    )

  testthat::expect_error(
    whep:::.tomato_fresh_equivalent(trade),
    class = "whep_tomato_trade_unknown_cn8"
  )
})

testthat::test_that("a trade table without cn8 or value aborts", {
  testthat::expect_error(
    whep:::.tomato_fresh_equivalent(
      dplyr::select(tomato_trade_fixture(), -value)
    ),
    class = "whep_tomato_trade_columns"
  )
})
