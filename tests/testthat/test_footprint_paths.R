testthat::test_that("compute_footprint_paths decomposes first intermediate use", {
  z_mat <- matrix(c(0, 50, 0, 0), nrow = 2, byrow = TRUE)
  x_vec <- c(100, 200)
  y_mat <- matrix(c(10, 100), nrow = 2)
  extensions <- c(200, 0)
  labels <- tibble::tibble(
    area_code = c(1L, 1L),
    item_cbs_code = c(10L, 20L)
  )
  fd_labels <- tibble::tibble(
    area_code = 2L,
    fd_col = "food"
  )

  paths <- compute_footprint_paths(
    z_mat = z_mat,
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    fd_labels = fd_labels,
    origin_area = 1L,
    origin_item = 10L,
    conserve_extensions = FALSE
  )

  testthat::expect_equal(nrow(paths), 2)
  testthat::expect_equal(sum(paths$value), 70)

  direct <- dplyr::filter(paths, path_type == "final_demand")
  testthat::expect_equal(direct$use_area, 2L)
  testthat::expect_equal(direct$use_item, 10L)
  testthat::expect_equal(direct$value, 20)

  intermediate <- dplyr::filter(paths, path_type == "intermediate")
  testthat::expect_equal(intermediate$use_area, 1L)
  testthat::expect_equal(intermediate$use_item, 20L)
  testthat::expect_equal(intermediate$target_area, 2L)
  testthat::expect_equal(intermediate$target_item, 20L)
  testthat::expect_equal(intermediate$value, 50)
})

testthat::test_that("compute_footprint_paths returns empty output for absent origin", {
  paths <- compute_footprint_paths(
    z_mat = matrix(0, nrow = 1, ncol = 1),
    x_vec = 1,
    y_mat = matrix(1, nrow = 1),
    extensions = 1,
    labels = tibble::tibble(area_code = 1L, item_cbs_code = 1L),
    fd_labels = tibble::tibble(area_code = 1L, fd_col = "food"),
    origin_area = 2L
  )

  testthat::expect_equal(nrow(paths), 0)
  testthat::expect_named(
    paths,
    c(
      "origin_area",
      "origin_polity_code",
      "origin_polity_name",
      "origin_polity_has_geometry",
      "origin_item",
      "use_area",
      "use_polity_code",
      "use_polity_name",
      "use_polity_has_geometry",
      "use_item",
      "target_area",
      "target_polity_code",
      "target_polity_name",
      "target_polity_has_geometry",
      "target_item",
      "target_fd",
      "path_type",
      "value"
    )
  )
})

testthat::test_that("compute_fp_product_paths keeps supplied product area and item", {
  z_mat <- matrix(c(0, 50, 0, 0), nrow = 2, byrow = TRUE)
  x_vec <- c(100, 200)
  y_mat <- matrix(c(10, 100), nrow = 2)
  extensions <- c(200, 0)
  labels <- tibble::tibble(
    area_code = c(1L, 3L),
    item_cbs_code = c(10L, 20L)
  )
  fd_labels <- tibble::tibble(
    area_code = 2L,
    fd_col = "food"
  )

  paths <- compute_fp_product_paths(
    z_mat = z_mat,
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    fd_labels = fd_labels,
    origin_area = 1L,
    origin_item = 10L,
    conserve_extensions = FALSE
  )

  testthat::expect_equal(nrow(paths), 2)
  testthat::expect_equal(sum(paths$value), 70)

  direct <- dplyr::filter(paths, product_item == 10L)
  testthat::expect_equal(direct$product_area, 1L)
  testthat::expect_equal(direct$target_area, 2L)
  testthat::expect_equal(direct$value, 20)

  downstream <- dplyr::filter(paths, product_item == 20L)
  testthat::expect_equal(downstream$product_area, 3L)
  testthat::expect_equal(downstream$target_area, 2L)
  testthat::expect_equal(downstream$value, 50)
})

testthat::test_that("add_footprint_product_stage splits by supplier shares", {
  footprints <- tibble::tibble(
    origin_area = 1L,
    origin_item = 10L,
    target_area = 1L,
    target_area_name = "Target",
    target_item = 20L,
    target_fd = "food",
    value = 100
  )
  y_mat <- Matrix::Matrix(c(80, 20), nrow = 2, sparse = TRUE)
  labels <- tibble::tibble(
    area_code = c(1L, 2L),
    item_cbs_code = c(20L, 20L)
  )
  fd_labels <- tibble::tibble(
    area_code = 1L,
    fd_col = "food"
  )

  result <- add_footprint_product_stage(
    footprints,
    y_mat,
    labels,
    fd_labels,
    max_product_areas = 1
  )

  testthat::expect_equal(sum(result$value), 100)
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_true("Other" %in% result$product_area_name)
  testthat::expect_equal(
    result$value[result$product_area_name == "Other"],
    20
  )
  testthat::expect_equal(unique(result$product_item), 20L)
})

testthat::test_that("add_footprint_product_stage fills fallback product area code", {
  footprints <- tibble::tibble(
    origin_area = 1L,
    origin_item = 10L,
    target_area = 2L,
    target_area_name = "Target",
    target_item = 20L,
    target_fd = "food",
    value = 100
  )
  y_mat <- Matrix::Matrix(100, nrow = 1, sparse = TRUE)
  labels <- tibble::tibble(
    area_code = 1L,
    item_cbs_code = 99L
  )
  fd_labels <- tibble::tibble(
    area_code = 2L,
    fd_col = "food"
  )

  result <- add_footprint_product_stage(
    footprints,
    y_mat,
    labels,
    fd_labels
  )

  testthat::expect_equal(result$product_area, 2L)
  testthat::expect_equal(result$product_area_name, "Target")
  testthat::expect_equal(result$product_item, 20L)
  testthat::expect_equal(result$value, 100)
})

testthat::test_that("add_footprint_product_stage applies min_share per split row", {
  footprints <- tibble::tibble(
    origin_area = c(1L, 1L),
    origin_item = c(10L, 10L),
    target_area = c(1L, 1L),
    target_area_name = c("Target", "Target"),
    target_item = c(20L, 20L),
    target_fd = c("food", "food"),
    value = c(40, 40)
  )
  y_mat <- Matrix::Matrix(c(50, 50), nrow = 2, sparse = TRUE)
  labels <- tibble::tibble(
    area_code = c(1L, 2L),
    item_cbs_code = c(20L, 20L)
  )
  fd_labels <- tibble::tibble(
    area_code = 1L,
    fd_col = "food"
  )

  result <- add_footprint_product_stage(
    footprints,
    y_mat,
    labels,
    fd_labels,
    max_product_areas = 2,
    min_share = 30
  )

  testthat::expect_equal(nrow(result), 0)
})

testthat::test_that("add_footprint_product_stage rejects invalid area labels", {
  footprints <- tibble::tibble(
    origin_area = 1L,
    origin_item = 10L,
    target_area = 1L,
    target_area_name = "Target",
    target_item = 20L,
    target_fd = "food",
    value = 100
  )
  y_mat <- Matrix::Matrix(100, nrow = 1, sparse = TRUE)
  labels <- tibble::tibble(
    area_code = 1L,
    item_cbs_code = 20L
  )
  fd_labels <- tibble::tibble(
    area_code = 1L,
    fd_col = "food"
  )

  testthat::expect_error(
    add_footprint_product_stage(
      footprints,
      y_mat,
      labels,
      fd_labels,
      other_area_name = NA_character_
    ),
    "other_area_name"
  )
})
