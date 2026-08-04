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

testthat::test_that("path totals reconcile with footprint under equal cap", {
  # Column 2 of A sums to 100 / 50 = 2, i.e. within (0.999, 100]. The old
  # path default cap of 0.999 would clip it while compute_footprint's cap of
  # 100 would not, breaking reconciliation. With a shared cap the path totals
  # must equal the footprint totals exactly.
  z_mat <- matrix(c(0, 100, 0, 0), nrow = 2, byrow = TRUE)
  x_vec <- c(100, 50)
  y_mat <- matrix(c(0, 50), nrow = 2)
  extensions <- c(100, 0)
  labels <- tibble::tibble(
    area_code = c(1L, 1L),
    item_cbs_code = c(10L, 20L)
  )
  fd_labels <- tibble::tibble(area_code = 2L, fd_col = "food")

  fp <- compute_footprint(
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    z_mat = z_mat,
    fd_labels = fd_labels,
    conserve_extensions = FALSE
  )
  paths <- compute_footprint_paths(
    z_mat = z_mat,
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    fd_labels = fd_labels,
    conserve_extensions = FALSE
  )
  product_paths <- compute_fp_product_paths(
    z_mat = z_mat,
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    fd_labels = fd_labels,
    conserve_extensions = FALSE
  )

  testthat::expect_equal(sum(fp$value), 100)
  testthat::expect_equal(sum(paths$value), sum(fp$value))
  testthat::expect_equal(sum(product_paths$value), sum(fp$value))
})

testthat::test_that("compute_footprint_paths validates max_column_sum", {
  testthat::expect_error(
    compute_footprint_paths(
      z_mat = matrix(0, nrow = 1, ncol = 1),
      x_vec = 1,
      y_mat = matrix(1, nrow = 1),
      extensions = 1,
      labels = tibble::tibble(area_code = 1L, item_cbs_code = 1L),
      fd_labels = tibble::tibble(area_code = 1L, fd_col = "food"),
      max_column_sum = -1
    ),
    "max_column_sum"
  )
})

testthat::test_that("compute_footprint_paths returns empty output for extension-free origin", {
  # This used to select an area absent from `labels` (`origin_area = 2L`), which
  # is now an error rather than a silent empty table. The empty-output schema
  # still has to hold, so reach it the legitimate way: a selected origin area
  # that carries no extension at all.
  paths <- compute_footprint_paths(
    z_mat = matrix(0, nrow = 2, ncol = 2),
    x_vec = c(1, 1),
    y_mat = matrix(c(1, 1), nrow = 2),
    extensions = c(0, 1),
    labels = tibble::tibble(area_code = c(1L, 2L), item_cbs_code = c(1L, 2L)),
    fd_labels = tibble::tibble(area_code = 1L, fd_col = "food"),
    origin_area = 1L
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

testthat::test_that("origin_area accepts a polity_area_code for a folded area", {
  # `origin_area` filters on `labels$area_code`, the legacy numeric area code.
  # For the 64 areas whose FABIO aggregation key differs from their own code,
  # `polity_area_code` is a different number: Sudan 276 and South Sudan 277 both
  # sit in bucket 206. Passing 206 used to return 0 rows and no message at all,
  # so a caller working in the polity vocabulary got a silent, valid-looking
  # empty footprint. It must now resolve to the areas the bucket covers.
  z_mat <- matrix(c(0, 50, 0, 0), nrow = 2, byrow = TRUE)
  x_vec <- c(100, 200)
  y_mat <- matrix(c(10, 100), nrow = 2)
  extensions <- c(200, 0)
  labels <- tibble::tibble(
    area_code = c(276L, 277L),
    item_cbs_code = c(10L, 20L)
  )
  fd_labels <- tibble::tibble(area_code = 277L, fd_col = "food")

  legacy <- compute_footprint_paths(
    z_mat = z_mat,
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    fd_labels = fd_labels,
    origin_area = 276L,
    conserve_extensions = FALSE
  )
  bucket <- compute_footprint_paths(
    z_mat = z_mat,
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    fd_labels = fd_labels,
    origin_area = 206L,
    conserve_extensions = FALSE
  )
  polity <- compute_footprint_paths(
    z_mat = z_mat,
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    fd_labels = fd_labels,
    origin_area = "SDN-2011-2025",
    conserve_extensions = FALSE
  )

  testthat::expect_equal(nrow(legacy), 2)
  testthat::expect_equal(bucket, legacy)
  testthat::expect_equal(polity, legacy)
  testthat::expect_equal(unique(bucket$origin_polity_code), "SDN-2011-2025")
})

testthat::test_that("origin_area keeps the legacy meaning of an area code", {
  # American Samoa (5) is folded into rest-of-world, so its `polity_area_code`
  # is 999 -- which is also a legacy `area_code` in its own right. Resolving the
  # polity vocabulary must not widen an existing legacy selection: 999 keeps
  # meaning the single rest-of-world sector, while the polity code shared by
  # both label rows selects both.
  z_mat <- matrix(0, nrow = 2, ncol = 2)
  x_vec <- c(100, 200)
  y_mat <- matrix(c(10, 20), nrow = 2)
  extensions <- c(100, 200)
  labels <- tibble::tibble(
    area_code = c(5L, 999L),
    item_cbs_code = c(10L, 20L)
  )
  fd_labels <- tibble::tibble(area_code = 999L, fd_col = "food")

  row_only <- compute_footprint_paths(
    z_mat = z_mat,
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    fd_labels = fd_labels,
    origin_area = 999L,
    conserve_extensions = FALSE
  )
  both <- compute_footprint_paths(
    z_mat = z_mat,
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    fd_labels = fd_labels,
    origin_area = "ROW-1850-2023",
    conserve_extensions = FALSE
  )

  testthat::expect_equal(row_only$origin_area, 999L)
  testthat::expect_setequal(both$origin_area, c(5L, 999L))
})

testthat::test_that("origin_area rejects values that resolve to no sector", {
  # An unresolvable value is a caller mistake, and used to produce an empty
  # table indistinguishable from a genuine zero footprint. A partly resolvable
  # vector stays backward compatible -- the resolvable areas are still traced --
  # but the dropped values are reported instead of vanishing.
  args <- list(
    z_mat = matrix(0, nrow = 2, ncol = 2),
    x_vec = c(100, 200),
    y_mat = matrix(c(10, 20), nrow = 2),
    extensions = c(100, 200),
    labels = tibble::tibble(
      area_code = c(276L, 277L),
      item_cbs_code = c(10L, 20L)
    ),
    fd_labels = tibble::tibble(area_code = 277L, fd_col = "food"),
    conserve_extensions = FALSE
  )

  testthat::expect_error(
    do.call(compute_footprint_paths, c(args, list(origin_area = 424242L))),
    "matches no sector"
  )
  testthat::expect_error(
    do.call(compute_fp_product_paths, c(args, list(origin_area = 424242L))),
    "matches no sector"
  )

  testthat::expect_warning(
    partial <- do.call(
      compute_footprint_paths,
      c(args, list(origin_area = c(276L, 424242L)))
    ),
    "no sector"
  )
  testthat::expect_equal(unique(partial$origin_area), 276L)
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

testthat::test_that("product_area_name matches crosswalk polity name", {
  footprints <- tibble::tibble(
    origin_area = 231L,
    origin_item = 10L,
    target_area = 231L,
    target_area_name = "United States of America",
    target_item = 20L,
    target_fd = "food",
    value = 100
  )
  y_mat <- Matrix::Matrix(c(60, 40), nrow = 2, sparse = TRUE)
  labels <- tibble::tibble(
    area_code = c(231L, 68L),
    item_cbs_code = c(20L, 20L)
  )
  fd_labels <- tibble::tibble(area_code = 231L, fd_col = "food")

  result <- add_footprint_product_stage(
    footprints,
    y_mat,
    labels,
    fd_labels,
    max_product_areas = 2
  )

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_false("Other" %in% result$product_area_name)
  testthat::expect_equal(
    result$product_area_name,
    result$product_polity_name
  )
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
