# Fixtures ------------------------------------------------------------

.conservation_labels <- function() {
  tibble::tibble(
    area_code = c(1L, 1L, 2L),
    item_cbs_code = c(10L, 20L, 10L)
  )
}

# Footprint where origin (1,10) is fully traced, (1,20) is
# under-traced, and (2,10) never appears (silently dropped).
.partial_footprint <- function() {
  tibble::tibble(
    origin_area = c(1L, 1L),
    origin_item = c(10L, 20L),
    value = c(50, 15)
  )
}

.cbs_balance_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code,
    ~production, ~import, ~stock_withdrawal,
    ~export, ~food, ~feed, ~seed, ~processing, ~other_uses,
    ~stock_addition,
    # balanced: supply 6000 == use 6000
    2000L, 203L, 2511L, 5000, 1000, 0, 500, 3000, 1500, 200, 500, 300, 0,
    # unbalanced: supply 3500 != use 3500 - 100
    2000L, 68L, 2514L, 3000, 500, 0, 200, 2000, 800, 100, 200, 100, 0
  )
}

# check_footprint_conservation ----------------------------------------

testthat::test_that("conservation report flags under-traced and dropped", {
  report <- whep::check_footprint_conservation(
    .partial_footprint(),
    extensions = c(50, 30, 8),
    labels = .conservation_labels(),
    x_vec = c(100, 200, 50)
  )

  report |>
    pointblank::expect_col_exists(
      c("origin_area", "origin_item", "direct", "embodied", "status")
    )

  status_by_origin <- report |>
    dplyr::select(origin_area, origin_item, status) |>
    dplyr::arrange(origin_area, origin_item)

  testthat::expect_equal(
    status_by_origin$status,
    c("ok", "under_traced", "dropped")
  )
})

testthat::test_that("conservation flags over-tracing", {
  report <- whep::check_footprint_conservation(
    tibble::tibble(origin_area = 1L, origin_item = 10L, value = 60),
    extensions = 50,
    labels = tibble::tibble(area_code = 1L, item_cbs_code = 10L),
    x_vec = 100
  )

  testthat::expect_equal(report$status, "over_traced")
  testthat::expect_equal(report$discrepancy, 10)
})

testthat::test_that("near-zero-output sectors contribute no direct pressure", {
  report <- whep::check_footprint_conservation(
    tibble::tibble(
      origin_area = integer(),
      origin_item = integer(),
      value = numeric()
    ),
    extensions = 99,
    labels = tibble::tibble(area_code = 1L, item_cbs_code = 10L),
    x_vec = 1e-12
  )

  testthat::expect_equal(report$direct, 0)
  testthat::expect_equal(report$status, "ok")
})

testthat::test_that("conservation holds for a real compute_footprint run", {
  z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
  x_vec <- c(100, 200)
  y_mat <- matrix(c(85, 195), ncol = 1)
  extensions <- c(50, 30)
  labels <- tibble::tibble(area_code = c(1L, 1L), item_cbs_code = c(1L, 2L))

  fp <- whep::compute_footprint(
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    z_mat = z_mat
  )
  summary <- whep::check_footprint_conservation(
    fp,
    extensions,
    labels,
    x_vec
  ) |>
    whep::summarise_conservation()

  # Engine bounds embodied <= direct, so no over-tracing.
  testthat::expect_equal(summary$n_over_traced, 0)
  testthat::expect_true(summary$total_embodied <= summary$total_direct + 1e-6)
})

testthat::test_that("conservation input validation aborts", {
  testthat::expect_error(
    whep::check_footprint_conservation(
      tibble::tibble(origin_area = 1L),
      extensions = 1,
      labels = tibble::tibble(area_code = 1L, item_cbs_code = 1L),
      x_vec = 1
    ),
    "missing column"
  )
})

# summarise_conservation ----------------------------------------------

testthat::test_that("summary rolls up status counts", {
  summary <- whep::check_footprint_conservation(
    .partial_footprint(),
    extensions = c(50, 30, 8),
    labels = .conservation_labels(),
    x_vec = c(100, 200, 50)
  ) |>
    whep::summarise_conservation()

  testthat::expect_equal(summary$n_origin, 3)
  testthat::expect_equal(summary$n_ok, 1)
  testthat::expect_equal(summary$n_under_traced, 1)
  testthat::expect_equal(summary$n_dropped, 1)
  testthat::expect_equal(summary$total_direct, 88)
  testthat::expect_equal(summary$total_embodied, 65)
})

# assert_footprint_invariants -----------------------------------------

testthat::test_that("assert passes on a conserving footprint", {
  z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
  x_vec <- c(100, 200)
  y_mat <- matrix(c(85, 195), ncol = 1)
  extensions <- c(50, 30)
  labels <- tibble::tibble(area_code = c(1L, 1L), item_cbs_code = c(1L, 2L))

  fp <- whep::compute_footprint(
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    z_mat = z_mat
  )
  summary <- whep::assert_footprint_invariants(fp, extensions, labels, x_vec)
  testthat::expect_equal(summary$n_over_traced, 0)
})

testthat::test_that("assert aborts on over-tracing", {
  testthat::expect_error(
    whep::assert_footprint_invariants(
      tibble::tibble(origin_area = 1L, origin_item = 10L, value = 60),
      extensions = 50,
      labels = tibble::tibble(area_code = 1L, item_cbs_code = 10L),
      x_vec = 100
    ),
    "over-traces"
  )
})

testthat::test_that("assert aborts when too much pressure is lost", {
  testthat::expect_error(
    whep::assert_footprint_invariants(
      .partial_footprint(),
      extensions = c(50, 30, 8),
      labels = .conservation_labels(),
      x_vec = c(100, 200, 50),
      max_rel_loss = 0.01
    ),
    "loses"
  )
})

# check_supply_use_balance --------------------------------------------

testthat::test_that("supply-use balance flags unbalanced rows", {
  report <- whep::check_supply_use_balance(.cbs_balance_fixture())

  balanced_by_item <- report |>
    dplyr::arrange(item_cbs_code)

  testthat::expect_equal(balanced_by_item$balanced, c(TRUE, FALSE))
  testthat::expect_equal(max(report$abs_diff), 100)
})

testthat::test_that("supply-use balance requires the accounting columns", {
  testthat::expect_error(
    whep::check_supply_use_balance(tibble::tibble(production = 1)),
    "missing column"
  )
})
