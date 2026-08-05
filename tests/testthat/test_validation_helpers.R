# Unit tests for the pure conservation-scoring helpers behind the
# inst/scripts validation figures (issues #260, #261).

test_that(".conservation_rel_error scores agreement and leaks correctly", {
  # Exact agreement scores 0.
  testthat::expect_equal(whep:::.conservation_rel_error(100, 100), 0)
  # A true zero-against-zero pair is legitimate agreement, not a failure.
  testthat::expect_equal(whep:::.conservation_rel_error(0, 0), 0)
  # Ordinary relative error.
  testthat::expect_equal(whep:::.conservation_rel_error(110, 100), 10)
})

test_that(".conservation_rel_error flags spurious gridded mass (#261)", {
  # Zero reference but nonzero gridded mass is a real disagreement and
  # must NOT score as a perfect match.
  err <- whep:::.conservation_rel_error(50, 0)
  testthat::expect_true(is.infinite(err))
  testthat::expect_false(err < 0.01)
})

test_that(".conservation_rel_error is vectorised", {
  testthat::expect_equal(
    whep:::.conservation_rel_error(c(50, 100, 0, 110), c(0, 100, 0, 100)),
    c(Inf, 0, 0, 10)
  )
})

test_that(".join_conservation keeps fully-leaked countries (#260)", {
  gridded <- tibble::tribble(
    ~area_code,
    ~grid_heads,
    1L,
    500,
    2L,
    300 # spurious gridded country absent from the reference
  )
  reference <- tibble::tribble(
    ~area_code,
    ~fao_heads,
    1L,
    500,
    3L,
    900 # reference country with zero gridded output (total leak)
  )

  out <- whep:::.join_conservation(
    gridded,
    reference,
    by = "area_code",
    fill = c("grid_heads", "fao_heads")
  )

  # An inner join would have dropped countries 2 and 3.
  testthat::expect_setequal(out$area_code, c(1L, 2L, 3L))

  leaked <- dplyr::filter(out, area_code == 3L)
  testthat::expect_equal(leaked$grid_heads, 0)
  testthat::expect_equal(leaked$fao_heads, 900)

  spurious <- dplyr::filter(out, area_code == 2L)
  testthat::expect_equal(spurious$fao_heads, 0)
})

test_that("join + scoring flags a total leak as a failure, not perfect", {
  gridded <- tibble::tribble(~area_code, ~grid_heads, 1L, 100)
  reference <- tibble::tribble(
    ~area_code,
    ~fao_heads,
    1L,
    100,
    3L,
    900
  )

  scored <- whep:::.join_conservation(
    gridded,
    reference,
    by = "area_code",
    fill = c("grid_heads", "fao_heads")
  ) |>
    dplyr::mutate(
      err = whep:::.conservation_rel_error(grid_heads, fao_heads)
    )

  leaked_err <- dplyr::filter(scored, area_code == 3L)$err
  testthat::expect_equal(leaked_err, 100)
  testthat::expect_false(leaked_err < 0.01)
})
