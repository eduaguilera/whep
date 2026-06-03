#' Compute first-use footprint paths.
#'
#' @description
#' Decompose an origin footprint into the first sector that directly uses the
#' origin product before the footprint reaches final demand. This is useful for
#' Sankey views that show paths such as origin product -> first-use area ->
#' first-use product -> final-demand area.
#'
#' The decomposition uses the IO identity \eqn{x = d + A x}. For each selected
#' origin sector \eqn{i} and final-demand target, the origin requirement
#' \eqn{x_i} is split into direct final demand \eqn{d_i} and direct
#' intermediate use \eqn{A_{ij} x_j}. Values are multiplied by the origin
#' extension intensity \eqn{e_i / X_i}.
#'
#' @param z_mat Inter-industry flow matrix from [build_io_model()].
#' @param x_vec Numeric vector of total output per sector.
#' @param y_mat Final demand matrix from [build_io_model()].
#' @param extensions Numeric vector of environmental extensions per sector.
#' @param labels Tibble with `area_code` and `item_cbs_code` mapping sectors.
#' @param fd_labels Tibble labelling Y columns, from [build_io_model()].
#' @param origin_area Optional area code vector limiting origin sectors.
#' @param origin_item Optional item code vector limiting origin sectors.
#' @param output_tol Minimum output considered valid when computing extension
#'   intensities.
#' @param value_added_floor Minimum non-intermediate leakage share used when
#'   constructing technical coefficients from `z_mat`.
#' @param conserve_extensions If `TRUE`, rescale positive paths within each
#'   origin area/item so their sum does not exceed the corresponding positive
#'   extension total.
#' @param min_value Drop paths with values less than or equal to this value
#'   before returning.
#'
#' @return A tibble with `origin_area`, `origin_item`, `use_area`, `use_item`,
#'   `target_area`, `target_item`, `target_fd`, `path_type`, and `value`.
#'
#' @export
compute_footprint_paths <- function(
  z_mat,
  x_vec,
  y_mat,
  extensions,
  labels,
  fd_labels,
  origin_area = NULL,
  origin_item = NULL,
  output_tol = 1e-8,
  value_added_floor = 1e-3,
  conserve_extensions = TRUE,
  min_value = 0
) {
  .validate_footprint_path_inputs(
    z_mat,
    x_vec,
    y_mat,
    extensions,
    labels,
    fd_labels,
    output_tol,
    value_added_floor,
    conserve_extensions,
    min_value
  )

  origin_idx <- .footprint_path_origin_indices(
    labels,
    extensions,
    x_vec,
    origin_area,
    origin_item,
    output_tol
  )
  if (length(origin_idx) == 0) {
    return(.empty_footprint_paths())
  }

  n <- length(x_vec)
  n_fd <- ncol(y_mat)
  cli::cli_inform(c(
    "i" = "Computing first-use footprint paths for {length(origin_idx)} origin sector{?s}.",
    " " = "Final demand: {n_fd} column{?s}."
  ))

  intensity <- .extension_intensity(extensions, x_vec, output_tol)
  a_mat <- .technical_coefficients(
    z_mat,
    x_vec,
    value_added_floor = value_added_floor
  )
  ia <- Matrix::Diagonal(n) - a_mat
  lu_fact <- .factor_ia(ia)

  items <- sort(unique(labels$item_cbs_code))
  g_mat <- .build_item_grouping(labels, items)

  cli::cli_inform("  Decomposing first-use paths...")
  paths <- purrr::map(seq_len(n_fd), function(j) {
    .footprint_paths_one_fd_col(
      origin_idx,
      intensity,
      a_mat,
      lu_fact,
      y_mat[, j],
      g_mat,
      labels,
      items,
      fd_labels$area_code[j],
      fd_labels$fd_col[j],
      min_value
    )
  }) |>
    dplyr::bind_rows()

  if (nrow(paths) == 0) {
    return(paths)
  }

  if (isTRUE(conserve_extensions)) {
    paths <- .conserve_footprint_extensions(
      paths,
      labels,
      extensions,
      x_vec,
      output_tol
    )
  }

  paths <- paths |>
    dplyr::filter(.data$value > min_value)

  cli::cli_alert_success(
    "First-use paths complete: {nrow(paths)} non-zero flows."
  )
  paths
}

.footprint_path_origin_indices <- function(
  labels,
  extensions,
  x_vec,
  origin_area,
  origin_item,
  output_tol
) {
  keep <- extensions > 0 & x_vec > output_tol
  if (!is.null(origin_area)) {
    keep <- keep & labels$area_code %in% origin_area
  }
  if (!is.null(origin_item)) {
    keep <- keep & labels$item_cbs_code %in% origin_item
  }
  which(keep)
}

.footprint_paths_one_fd_col <- function(
  origin_idx,
  intensity,
  a_mat,
  lu_fact,
  y_vec,
  g_mat,
  labels,
  items,
  consumer_area,
  fd_col,
  min_value
) {
  y_vec <- as.numeric(y_vec)
  if (all(y_vec == 0)) {
    return(NULL)
  }

  v_mat <- Matrix::Diagonal(x = y_vec) %*% g_mat
  req_mat <- Matrix::solve(lu_fact, v_mat)

  purrr::map(origin_idx, function(i) {
    if (intensity[i] <= 0) {
      return(NULL)
    }
    dplyr::bind_rows(
      .footprint_direct_paths(
        i,
        intensity[i],
        v_mat,
        labels,
        items,
        consumer_area,
        fd_col,
        min_value
      ),
      .footprint_intermediate_paths(
        i,
        intensity[i],
        a_mat,
        req_mat,
        labels,
        items,
        consumer_area,
        fd_col,
        min_value
      )
    )
  }) |>
    dplyr::bind_rows()
}

.footprint_direct_paths <- function(
  origin_idx,
  intensity,
  v_mat,
  labels,
  items,
  consumer_area,
  fd_col,
  min_value
) {
  direct <- Matrix::summary(
    methods::as(v_mat[origin_idx, , drop = FALSE], "CsparseMatrix")
  )
  if (nrow(direct) == 0) {
    return(NULL)
  }
  direct$value <- direct$x * intensity
  direct <- direct[direct$value > min_value, , drop = FALSE]
  if (nrow(direct) == 0) {
    return(NULL)
  }

  target_items <- as.integer(items[direct$j])
  tibble::tibble(
    origin_area = as.integer(labels$area_code[origin_idx]),
    origin_item = as.integer(labels$item_cbs_code[origin_idx]),
    use_area = as.integer(consumer_area),
    use_item = target_items,
    target_area = as.integer(consumer_area),
    target_item = target_items,
    target_fd = fd_col,
    path_type = "final_demand",
    value = direct$value
  )
}

.footprint_intermediate_paths <- function(
  origin_idx,
  intensity,
  a_mat,
  req_mat,
  labels,
  items,
  consumer_area,
  fd_col,
  min_value
) {
  a_row <- Matrix::summary(
    methods::as(a_mat[origin_idx, , drop = FALSE], "CsparseMatrix")
  )
  if (nrow(a_row) == 0) {
    return(NULL)
  }
  a_row <- a_row[a_row$x > 0, , drop = FALSE]
  if (nrow(a_row) == 0) {
    return(NULL)
  }

  use_idx <- a_row$j
  req_use <- req_mat[use_idx, , drop = FALSE]
  contrib <- Matrix::Diagonal(x = as.numeric(a_row$x) * intensity) %*%
    req_use
  contrib <- Matrix::drop0(methods::as(contrib, "CsparseMatrix"))
  sp <- Matrix::summary(contrib)
  if (nrow(sp) == 0) {
    return(NULL)
  }
  sp <- sp[sp$x > min_value, , drop = FALSE]
  if (nrow(sp) == 0) {
    return(NULL)
  }

  actual_use_idx <- use_idx[sp$i]
  tibble::tibble(
    origin_area = as.integer(labels$area_code[origin_idx]),
    origin_item = as.integer(labels$item_cbs_code[origin_idx]),
    use_area = as.integer(labels$area_code[actual_use_idx]),
    use_item = as.integer(labels$item_cbs_code[actual_use_idx]),
    target_area = as.integer(consumer_area),
    target_item = as.integer(items[sp$j]),
    target_fd = fd_col,
    path_type = "intermediate",
    value = sp$x
  )
}

.validate_footprint_path_inputs <- function(
  z_mat,
  x_vec,
  y_mat,
  extensions,
  labels,
  fd_labels,
  output_tol,
  value_added_floor,
  conserve_extensions,
  min_value
) {
  .validate_footprint_inputs(
    l_inv = NULL,
    x_vec = x_vec,
    y_mat = y_mat,
    extensions = extensions,
    labels = labels,
    z_mat = z_mat,
    output_tol = output_tol,
    value_added_floor = value_added_floor,
    conserve_extensions = conserve_extensions
  )
  if (!is.data.frame(fd_labels) || nrow(fd_labels) != ncol(y_mat)) {
    cli::cli_abort(
      "{.arg fd_labels} must be a data frame with one row per Y column."
    )
  }
  missing_fd <- setdiff(c("area_code", "fd_col"), names(fd_labels))
  if (length(missing_fd) > 0) {
    cli::cli_abort(
      "{.arg fd_labels} is missing column{?s}: {.field {missing_fd}}."
    )
  }
  if (!is.numeric(min_value) || length(min_value) != 1 ||
    is.na(min_value) || min_value < 0) {
    cli::cli_abort("{.arg min_value} must be one non-negative number.")
  }
  .validate_value_added_floor(value_added_floor)
}

.empty_footprint_paths <- function() {
  tibble::tibble(
    origin_area = integer(0),
    origin_item = integer(0),
    use_area = integer(0),
    use_item = integer(0),
    target_area = integer(0),
    target_item = integer(0),
    target_fd = character(0),
    path_type = character(0),
    value = numeric(0)
  )
}

#' Compute final-product footprint paths.
#'
#' @description
#' Decompose an origin footprint by the area and item of the product supplied
#' to final demand. This adds the missing FABIO-viewer style phase between
#' origin product and final-demand area: origin product -> supplied product
#' area -> supplied product -> final-demand area.
#'
#' Unlike [compute_footprint_paths()], this does not show the first direct
#' intermediate input. It shows the downstream product row in `Y` whose final
#' demand carries the origin footprint.
#'
#' @inheritParams compute_footprint_paths
#'
#' @return A tibble with `origin_area`, `origin_item`, `product_area`,
#'   `product_item`, `target_area`, `target_fd`, and `value`.
#'
#' @export
compute_footprint_product_paths <- function(
  z_mat,
  x_vec,
  y_mat,
  extensions,
  labels,
  fd_labels,
  origin_area = NULL,
  origin_item = NULL,
  output_tol = 1e-8,
  value_added_floor = 1e-3,
  conserve_extensions = TRUE,
  min_value = 0
) {
  .validate_footprint_path_inputs(
    z_mat,
    x_vec,
    y_mat,
    extensions,
    labels,
    fd_labels,
    output_tol,
    value_added_floor,
    conserve_extensions,
    min_value
  )

  origin_idx <- .footprint_path_origin_indices(
    labels,
    extensions,
    x_vec,
    origin_area,
    origin_item,
    output_tol
  )
  if (length(origin_idx) == 0) {
    return(.empty_footprint_product_paths())
  }

  n <- length(x_vec)
  n_fd <- ncol(y_mat)
  cli::cli_inform(c(
    "i" = "Computing final-product footprint paths for {length(origin_idx)} origin sector{?s}.",
    " " = "Final demand: {n_fd} column{?s}."
  ))

  intensity <- .extension_intensity(extensions, x_vec, output_tol)
  a_mat <- .technical_coefficients(
    z_mat,
    x_vec,
    value_added_floor = value_added_floor
  )
  ia_t <- Matrix::t(Matrix::Diagonal(n) - a_mat)
  lu_fact <- .factor_ia(ia_t)

  cli::cli_inform("  Decomposing final-product paths...")
  paths <- purrr::map(origin_idx, function(i) {
    .footprint_product_paths_one_origin(
      i,
      intensity[i],
      lu_fact,
      y_mat,
      labels,
      fd_labels,
      min_value
    )
  }) |>
    dplyr::bind_rows()

  if (nrow(paths) == 0) {
    return(paths)
  }

  if (isTRUE(conserve_extensions)) {
    paths <- .conserve_footprint_extensions(
      paths,
      labels,
      extensions,
      x_vec,
      output_tol
    )
  }

  paths <- paths |>
    dplyr::filter(.data$value > min_value)

  cli::cli_alert_success(
    "Final-product paths complete: {nrow(paths)} non-zero flows."
  )
  paths
}

.footprint_product_paths_one_origin <- function(
  origin_idx,
  intensity,
  lu_fact,
  y_mat,
  labels,
  fd_labels,
  min_value
) {
  if (intensity <= 0) {
    return(NULL)
  }

  n <- nrow(y_mat)
  unit <- Matrix::sparseVector(
    i = origin_idx,
    x = 1,
    length = n
  )
  l_row <- as.numeric(Matrix::solve(lu_fact, unit))
  weights <- l_row * intensity
  if (all(weights == 0)) {
    return(NULL)
  }

  purrr::map(seq_len(ncol(y_mat)), function(j) {
    y_col <- Matrix::summary(
      methods::as(y_mat[, j, drop = FALSE], "CsparseMatrix")
    )
    if (nrow(y_col) == 0) {
      return(NULL)
    }
    y_col$value <- weights[y_col$i] * y_col$x
    y_col <- y_col[y_col$value > min_value, , drop = FALSE]
    if (nrow(y_col) == 0) {
      return(NULL)
    }

    tibble::tibble(
      origin_area = as.integer(labels$area_code[origin_idx]),
      origin_item = as.integer(labels$item_cbs_code[origin_idx]),
      product_area = as.integer(labels$area_code[y_col$i]),
      product_item = as.integer(labels$item_cbs_code[y_col$i]),
      target_area = as.integer(fd_labels$area_code[j]),
      target_fd = fd_labels$fd_col[j],
      value = y_col$value
    )
  }) |>
    dplyr::bind_rows()
}

.empty_footprint_product_paths <- function() {
  tibble::tibble(
    origin_area = integer(0),
    origin_item = integer(0),
    product_area = integer(0),
    product_item = integer(0),
    target_area = integer(0),
    target_fd = character(0),
    value = numeric(0)
  )
}

#' Add a final-demand product-area stage to footprints.
#'
#' @description
#' Split footprint rows by the area that supplied the final-demand product,
#' using shares from `y_mat`. This preserves the standard footprint totals
#' while adding a FABIO-viewer style phase:
#' origin product -> product/supplier area -> product -> final-demand area.
#'
#' This is a compact global-view helper. It does not recompute the full
#' origin-sector by product-sector Leontief cube. Instead, each existing
#' footprint row is allocated over the product-area shares observed in final
#' demand for the same `target_area`, `target_fd`, and `target_item`.
#'
#' @param footprints Footprint table from [compute_footprint()] with
#'   `target_area`, `target_item`, `target_fd`, and `value`.
#' @param y_mat Final demand matrix from [build_io_model()].
#' @param labels Tibble mapping Y rows to `area_code` and `item_cbs_code`.
#' @param fd_labels Tibble mapping Y columns to `area_code` and `fd_col`.
#' @param max_product_areas Maximum number of supplier/product areas to keep
#'   separately for each final-demand area, item, and demand category. Smaller
#'   supplier areas are grouped into `other_area_name`.
#' @param other_area_name Label for grouped supplier/product areas.
#' @param min_share Drop split paths smaller than this percentage of the total
#'   input footprint value. Use 0 to keep all split paths.
#'
#' @return `footprints` with `product_area`, `product_area_name`,
#'   `product_item`, and `product_share` columns. `value` is replaced by the
#'   split path value.
#'
#' @export
add_footprint_product_stage <- function(
  footprints,
  y_mat,
  labels,
  fd_labels,
  max_product_areas = 5,
  other_area_name = "Other",
  min_share = 0
) {
  .validate_add_product_stage_inputs(
    footprints,
    y_mat,
    labels,
    fd_labels,
    max_product_areas,
    other_area_name,
    min_share
  )

  total <- sum(footprints$value, na.rm = TRUE)
  min_value <- total * min_share / 100
  shares <- .final_demand_product_area_shares(
    y_mat,
    labels,
    fd_labels,
    max_product_areas,
    other_area_name
  )

  result <- footprints |>
    dplyr::left_join(
      shares,
      by = c(
        "target_area",
        "target_fd",
        "target_item" = "product_item"
      )
    ) |>
    dplyr::mutate(
      product_area = dplyr::if_else(
        is.na(.data$product_area),
        .data$target_area,
        .data$product_area
      ),
      product_area_name = dplyr::if_else(
        is.na(.data$product_area_name),
        .data$target_area_name,
        .data$product_area_name
      ),
      product_item = .data$target_item,
      product_share = tidyr::replace_na(.data$product_share, 1),
      value = .data$value * .data$product_share
    ) |>
    dplyr::filter(.data$value > min_value)

  group_cols <- setdiff(names(result), c("value", "product_share"))
  result |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      value = sum(.data$value, na.rm = TRUE),
      product_share = dplyr::first(.data$product_share),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$value))
}

.final_demand_product_area_shares <- function(
  y_mat,
  labels,
  fd_labels,
  max_product_areas,
  other_area_name
) {
  y_sp <- Matrix::summary(methods::as(y_mat, "CsparseMatrix"))
  if (nrow(y_sp) == 0) {
    return(tibble::tibble(
      target_area = integer(0),
      target_fd = character(0),
      product_item = integer(0),
      product_area = integer(0),
      product_area_name = character(0),
      product_share = numeric(0)
    ))
  }

  area_names <- whep::regions_full |>
    dplyr::select(product_area = code, product_area_name = name) |>
    dplyr::distinct(.data$product_area, .keep_all = TRUE)

  y_sp |>
    tibble::as_tibble() |>
    dplyr::filter(.data$x > 0) |>
    dplyr::mutate(
      target_area = as.integer(fd_labels$area_code[.data$j]),
      target_fd = fd_labels$fd_col[.data$j],
      product_area = as.integer(labels$area_code[.data$i]),
      product_item = as.integer(labels$item_cbs_code[.data$i])
    ) |>
    dplyr::summarise(
      product_value = sum(.data$x, na.rm = TRUE),
      .by = c(
        "target_area",
        "target_fd",
        "product_item",
        "product_area"
      )
    ) |>
    dplyr::group_by(
      .data$target_area,
      .data$target_fd,
      .data$product_item
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$product_value),
      .data$product_area,
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      product_total = sum(.data$product_value, na.rm = TRUE),
      product_area_rank = dplyr::row_number(),
      product_area = dplyr::if_else(
        .data$product_area_rank <= max_product_areas,
        .data$product_area,
        NA_integer_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(area_names, by = "product_area") |>
    dplyr::mutate(
      product_area_name = tidyr::replace_na(
        .data$product_area_name,
        other_area_name
      )
    ) |>
    dplyr::summarise(
      product_value = sum(.data$product_value, na.rm = TRUE),
      product_total = dplyr::first(.data$product_total),
      .by = c(
        "target_area",
        "target_fd",
        "product_item",
        "product_area",
        "product_area_name"
      )
    ) |>
    dplyr::mutate(
      product_share = dplyr::if_else(
        .data$product_total > 0,
        .data$product_value / .data$product_total,
        0
      )
    ) |>
    dplyr::select(
      "target_area",
      "target_fd",
      "product_item",
      "product_area",
      "product_area_name",
      "product_share"
    )
}

.validate_add_product_stage_inputs <- function(
  footprints,
  y_mat,
  labels,
  fd_labels,
  max_product_areas,
  other_area_name,
  min_share
) {
  if (!is.data.frame(footprints)) {
    cli::cli_abort("{.arg footprints} must be a data frame.")
  }
  required <- c("target_area", "target_item", "target_fd", "value")
  missing <- setdiff(required, names(footprints))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg footprints} is missing column{?s}: {.field {missing}}."
    )
  }
  if (!"target_area_name" %in% names(footprints)) {
    cli::cli_abort(
      "{.arg footprints} must include {.field target_area_name}."
    )
  }
  if (!is.numeric(footprints$value)) {
    cli::cli_abort("{.field value} must be numeric.")
  }
  if (!methods::is(y_mat, "Matrix") && !is.matrix(y_mat)) {
    cli::cli_abort("{.arg y_mat} must be a matrix.")
  }
  if (nrow(labels) != nrow(y_mat)) {
    cli::cli_abort(
      "{.arg labels} must have one row per row of {.arg y_mat}."
    )
  }
  if (!is.data.frame(fd_labels) || nrow(fd_labels) != ncol(y_mat)) {
    cli::cli_abort(
      "{.arg fd_labels} must have one row per column of {.arg y_mat}."
    )
  }
  missing_labels <- setdiff(c("area_code", "item_cbs_code"), names(labels))
  if (length(missing_labels) > 0) {
    cli::cli_abort(
      "{.arg labels} is missing column{?s}: {.field {missing_labels}}."
    )
  }
  missing_fd <- setdiff(c("area_code", "fd_col"), names(fd_labels))
  if (length(missing_fd) > 0) {
    cli::cli_abort(
      "{.arg fd_labels} is missing column{?s}: {.field {missing_fd}}."
    )
  }
  if (!is.numeric(max_product_areas) || length(max_product_areas) != 1 ||
    is.na(max_product_areas) || max_product_areas < 1) {
    cli::cli_abort(
      "{.arg max_product_areas} must be one positive number."
    )
  }
  if (!is.character(other_area_name) || length(other_area_name) != 1 ||
    is.na(other_area_name) || !nzchar(other_area_name)) {
    cli::cli_abort(
      "{.arg other_area_name} must be one non-empty string."
    )
  }
  if (!is.numeric(min_share) || length(min_share) != 1 ||
    is.na(min_share) || min_share < 0) {
    cli::cli_abort("{.arg min_share} must be one non-negative number.")
  }
}
