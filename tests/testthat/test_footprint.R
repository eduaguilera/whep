# Helper fixtures -------------------------------------------------------

footprint_2sector_fixture <- function() {
  z <- matrix(
    c(10, 5, 3, 2),
    nrow = 2, byrow = TRUE
  )
  x <- c(100, 50)
  l_inv <- compute_leontief_inverse(z, x)
  y <- matrix(c(82, 43), ncol = 1)
  extensions <- c(50, 20)
  labels <- tibble::tibble(
    area_code = c(1L, 1L),
    item_cbs_code = c(10L, 20L)
  )
  list(
    z = z, x = x, l_inv = l_inv, y = y,
    extensions = extensions, labels = labels
  )
}

# compute_footprint -----------------------------------------------------

testthat::test_that(
  "compute_footprint returns tidy tibble",
  {
    f <- footprint_2sector_fixture()
    result <- compute_footprint(
      f$l_inv, f$x, f$y, f$extensions, f$labels
    )

    testthat::expect_s3_class(result, "tbl_df")
    pointblank::expect_col_exists(
      result,
      c(
        "origin_area", "origin_item",
        "target_area", "target_item", "value"
      )
    )
    testthat::expect_true(nrow(result) > 0)
  }
)

testthat::test_that(
  "footprint sums match total extensions",
  {
    z <- matrix(
      c(5, 10, 3, 2),
      nrow = 2, byrow = TRUE
    )
    y <- matrix(c(85, 45), ncol = 1)
    x <- rowSums(z) + as.vector(y)
    l_inv <- compute_leontief_inverse(z, x)
    extensions <- c(30, 10)
    labels <- tibble::tibble(
      area_code = c(1L, 1L),
      item_cbs_code = c(10L, 20L)
    )

    result <- compute_footprint(
      l_inv, x, y, extensions, labels
    )

    testthat::expect_equal(
      sum(result$value), sum(extensions),
      tolerance = 1e-6
    )
  }
)

testthat::test_that(
  "footprint zeros yield no rows",
  {
    z <- matrix(c(0, 0, 0, 0), nrow = 2)
    x <- c(100, 200)
    l_inv <- compute_leontief_inverse(z, x)
    y <- matrix(c(100, 200), ncol = 1)
    extensions <- c(0, 0)
    labels <- tibble::tibble(
      area_code = c(1L, 1L),
      item_cbs_code = c(10L, 20L)
    )

    result <- compute_footprint(
      l_inv, x, y, extensions, labels
    )
    testthat::expect_equal(nrow(result), 0)
  }
)

testthat::test_that("compute_footprint validates inputs", {
  l_inv <- matrix(1, 2, 2)
  x <- c(1, 2)
  y <- matrix(1, 2, 1)
  ext <- c(1, 2)
  labels <- tibble::tibble(
    area_code = 1L,
    item_cbs_code = 10L
  )

  testthat::expect_error(
    compute_footprint(l_inv, x, y, ext, labels),
    "must have 2 rows"
  )
  testthat::expect_error(
    compute_footprint(
      x_vec = x, y_mat = y, extensions = ext,
      labels = labels
    ),
    "Provide either"
  )
})

testthat::test_that(
  "2-country footprint traces origin correctly",
  {
    z <- matrix(
      c(5, 2, 1, 0, 3, 1, 0, 2, 0, 1, 4, 0, 1, 0, 1, 3),
      nrow = 4, byrow = TRUE
    )
    y <- diag(4) * 50
    x <- rowSums(z) + rowSums(y)
    l_inv <- compute_leontief_inverse(z, x)
    extensions <- c(10, 5, 8, 3)
    labels <- tibble::tibble(
      area_code = c(1L, 1L, 2L, 2L),
      item_cbs_code = c(10L, 20L, 10L, 20L)
    )

    result <- compute_footprint(
      l_inv, x, y, extensions, labels
    )

    testthat::expect_true(nrow(result) > 0)
    testthat::expect_equal(
      sum(result$value), sum(extensions),
      tolerance = 1e-6
    )
  }
)

testthat::test_that(
  "sparse path (z_mat) matches dense path (l_inv)",
  {
    f <- footprint_2sector_fixture()
    dense <- compute_footprint(
      f$l_inv, f$x, f$y, f$extensions, f$labels
    )
    sparse <- compute_footprint(
      x_vec = f$x, y_mat = f$y,
      extensions = f$extensions, labels = f$labels,
      z_mat = f$z
    )

    testthat::expect_equal(
      sum(sparse$value), sum(dense$value),
      tolerance = 1e-6
    )
    testthat::expect_equal(nrow(sparse), nrow(dense))
  }
)
