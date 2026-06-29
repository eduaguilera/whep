.fp_labels <- function() {
  tibble::tibble(
    index = 1:2,
    area_code = c(1L, 1L),
    item_cbs_code = c(10L, 20L)
  )
}

.fp_io <- function() {
  tibble::tibble(
    year = 2000L,
    Z = list(matrix(c(0, 5, 10, 0), nrow = 2)),
    X = list(c(100, 200)),
    Y = list(matrix(c(85, 195), ncol = 1)),
    labels = list(.fp_labels()),
    fd_labels = list(tibble::tibble(area_code = 1L, fd_col = "food"))
  )
}

testthat::test_that("align_extension orders by index and zero-fills missing sectors", {
  extension <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_cbs_code = 10L,
    impact_u = 5
  )
  result <- whep::align_extension(extension, .fp_labels(), 2000L)

  testthat::expect_equal(result, c(5, 0))
})

testthat::test_that("align_extension sums duplicate sectors and drops out-of-model rows", {
  extension <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2000L, 1L, 10L, 3,
    2000L, 1L, 10L, 4, # duplicate sector -> summed
    2000L, 9L, 99L, 100, # outside the model -> dropped
    1999L, 1L, 20L, 50 # wrong year -> ignored
  )
  result <- whep::align_extension(extension, .fp_labels(), 2000L)

  testthat::expect_equal(result, c(7, 0))
})

testthat::test_that("align_extension honours an alternative value column", {
  extension <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_cbs_code = 20L,
    co2e = 42
  )
  result <- whep::align_extension(
    extension,
    .fp_labels(),
    2000L,
    value_col = "co2e"
  )

  testthat::expect_equal(result, c(0, 42))
})

testthat::test_that("align_extension requires an index column on labels", {
  extension <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_cbs_code = 10L,
    impact_u = 5
  )
  testthat::expect_error(
    whep::align_extension(
      extension,
      tibble::tibble(area_code = 1L, item_cbs_code = 10L),
      2000L
    ),
    "index"
  )
})

testthat::test_that("build_footprint matches a manual compute_footprint call", {
  io <- .fp_io()
  extension <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_cbs_code = c(10L, 20L),
    impact_u = c(50, 30)
  )

  result <- whep::build_footprint(extension, io = io)

  manual <- whep::compute_footprint(
    z_mat = io$Z[[1]],
    x_vec = io$X[[1]],
    y_mat = io$Y[[1]],
    extensions = whep::align_extension(extension, io$labels[[1]], 2000L),
    labels = io$labels[[1]],
    fd_labels = io$fd_labels[[1]]
  )

  testthat::expect_true(all(result$year == 2000L))
  testthat::expect_equal(
    dplyr::arrange(result, origin_item, target_item)$value,
    dplyr::arrange(manual, origin_item, target_item)$value
  )
})

testthat::test_that("build_footprint validates its inputs", {
  io <- .fp_io()
  good <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_cbs_code = 10L,
    impact_u = 1
  )

  testthat::expect_error(
    whep::build_footprint(dplyr::select(good, -item_cbs_code), io = io),
    "item_cbs_code"
  )
  testthat::expect_error(
    whep::build_footprint(good, io = dplyr::select(io, -fd_labels)),
    "fd_labels"
  )
})
