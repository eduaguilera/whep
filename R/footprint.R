#' Compute environmental footprints.
#'
#' @description
#' Trace environmental extensions through the supply chain using
#' the Leontief inverse, following the FABIO methodology
#' (Bruckner et al., 2019). The footprint shows how much of an
#' environmental pressure (e.g. land use, water, emissions) is
#' embodied in the final consumption of each product in each
#' country.
#'
#' The multiplier matrix is computed as
#' \eqn{MP_{ij} = (e_i / X_i) \cdot L_{ij}}, where \eqn{e_i}
#' is the extension for sector \eqn{i}. For each demand
#' category, the footprint is decomposed per target item using
#' the FABIO diagonal approach:
#' \eqn{FP = MP \cdot \text{diag}(y)}, aggregated by item.
#'
#' For large systems, pass `z_mat` and `x_vec` instead of
#' `l_inv`. This solves \eqn{(I - A) x = Y} directly using a
#' sparse LU factorisation, avoiding the dense Leontief inverse
#' entirely and reducing memory from \eqn{O(n^2)} to
#' \eqn{O(nnz)}.
#'
#' @param l_inv Leontief inverse matrix from
#'   [compute_leontief_inverse()]. Ignored when `z_mat` is
#'   provided.
#' @param x_vec Numeric vector of total output per sector.
#' @param y_mat Final demand matrix from [build_io_model()].
#' @param extensions Numeric vector of environmental extensions
#'   (e.g. hectares of land use) per sector. Must have the same
#'   length as `x_vec`.
#' @param labels Tibble with `area_code` and `item_cbs_code`
#'   mapping row/column indices to their meaning. From
#'   [build_io_model()].
#' @param z_mat Optional inter-industry flow matrix from
#'   [build_io_model()]. When provided, the system is solved
#'   directly (sparse LU), and `l_inv` is not needed.
#' @param fd_labels Optional tibble labelling Y columns. Pass
#'   `fd_labels[[i]]` from [build_io_model()] output. When
#'   provided, footprints are decomposed per target item using
#'   the FABIO diagonal approach, and the result includes a
#'   `target_fd` column. When omitted, columns of Y are treated
#'   as sectors (appropriate only when Y is square).
#'
#' @return A tibble with footprint results containing:
#'   - `origin_area`: Country where the pressure occurs.
#'   - `origin_item`: Item causing the pressure.
#'   - `target_area`: Country consuming the product.
#'   - `target_item`: Item consumed.
#'   - `target_fd`: Demand category (e.g. `"food"`). Only
#'     present when `fd_labels` is provided.
#'   - `value`: Footprint value in extension units.
#'
#' @export
#'
#' @examples
#' z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
#' x_vec <- c(100, 200)
#' l_inv <- compute_leontief_inverse(z_mat, x_vec)
#' y_mat <- matrix(c(85, 195), ncol = 1)
#' extensions <- c(50, 30)
#' labels <- tibble::tibble(
#'   area_code = c(1L, 1L),
#'   item_cbs_code = c(1L, 2L)
#' )
#'
#' # Small system: pass pre-computed L
#' compute_footprint(l_inv, x_vec, y_mat, extensions, labels)
#'
#' # Using Z directly (computes L internally)
#' compute_footprint(
#'   x_vec = x_vec, y_mat = y_mat,
#'   extensions = extensions, labels = labels,
#'   z_mat = z_mat
#' )
compute_footprint <- function(
  l_inv = NULL,
  x_vec,
  y_mat,
  extensions,
  labels,
  z_mat = NULL,
  fd_labels = NULL
) {
  n <- length(x_vec)
  .validate_footprint_inputs(
    l_inv,
    x_vec,
    y_mat,
    extensions,
    labels,
    z_mat
  )
  n_fd <- ncol(y_mat)
  n_ext <- sum(extensions != 0)

  cli::cli_inform(c(
    "i" = "Computing footprint for {n} sectors.",
    " " = "{n_ext} sectors have non-zero extensions.",
    " " = "Final demand: {n_fd} column{?s}."
  ))

  intensity <- .extension_intensity(extensions, x_vec)
  f_diag <- Matrix::Diagonal(x = intensity)

  if (!is.null(z_mat)) {
    cli::cli_inform(
      "  Sparse solve path (no dense Leontief inverse)."
    )
    a_mat <- .technical_coefficients(z_mat, x_vec)
    ia <- Matrix::Diagonal(n) - a_mat
    lu_fact <- .factor_ia(ia)
    multiply_fn <- function(rhs) f_diag %*% Matrix::solve(lu_fact, rhs)
  } else {
    cli::cli_inform("  Computing multiplier matrix...")
    mp_mat <- f_diag %*% l_inv
    multiply_fn <- function(rhs) mp_mat %*% rhs
  }

  cli::cli_inform("  Computing footprints...")
  result <- if (!is.null(fd_labels)) {
    .footprint_by_item(multiply_fn, y_mat, labels, fd_labels)
  } else {
    .footprint_direct(multiply_fn, y_mat, labels)
  }

  cli::cli_alert_success(
    "Footprint complete: {nrow(result)} non-zero flows."
  )
  result
}

# --- Extension intensity ---

.extension_intensity <- function(extensions, x_vec) {
  ifelse(x_vec == 0, 0, extensions / x_vec)
}

# --- FABIO-style per-item footprint ---

.footprint_by_item <- function(
  multiply_fn,
  y_mat,
  labels,
  fd_labels
) {
  items <- sort(unique(labels$item_cbs_code))
  g_mat <- .build_item_grouping(labels, items)
  n_y_cols <- ncol(y_mat)

  purrr::map(seq_len(n_y_cols), function(j) {
    .footprint_one_fd_col(
      multiply_fn,
      y_mat[, j],
      g_mat,
      labels,
      items,
      fd_labels$area_code[j],
      fd_labels$fd_col[j]
    )
  }) |>
    dplyr::bind_rows()
}

.footprint_one_fd_col <- function(
  multiply_fn,
  y_vec,
  g_mat,
  labels,
  items,
  consumer_area,
  fd_col
) {
  y_vec <- as.numeric(y_vec)
  if (all(y_vec == 0)) {
    return(NULL)
  }

  v_mat <- Matrix::Diagonal(x = y_vec) %*% g_mat
  fp_item <- multiply_fn(v_mat)
  .fp_grouped_to_tidy(
    fp_item,
    labels,
    items,
    consumer_area,
    fd_col
  )
}

.fp_grouped_to_tidy <- function(
  fp_mat,
  labels,
  items,
  consumer_area,
  fd_col
) {
  fp_mat <- Matrix::drop0(
    methods::as(fp_mat, "CsparseMatrix")
  )
  sp <- Matrix::summary(fp_mat)
  if (nrow(sp) == 0) {
    return(NULL)
  }
  keep <- sp$x > 0
  if (!any(keep)) {
    return(NULL)
  }

  tibble::tibble(
    origin_area = as.integer(labels$area_code[sp$i[keep]]),
    origin_item = as.integer(labels$item_cbs_code[sp$i[keep]]),
    target_area = as.integer(consumer_area),
    target_item = as.integer(items[sp$j[keep]]),
    target_fd = fd_col,
    value = sp$x[keep]
  )
}

.build_item_grouping <- function(labels, items) {
  item_idx <- match(labels$item_cbs_code, items)
  Matrix::sparseMatrix(
    i = seq_len(nrow(labels)),
    j = item_idx,
    x = rep(1, nrow(labels)),
    dims = c(nrow(labels), length(items))
  )
}

# --- Direct footprint (no fd_labels) ---

.footprint_direct <- function(multiply_fn, y_mat, labels) {
  fp_mat <- multiply_fn(y_mat)
  target_labs <- .infer_target_labels(fp_mat, labels)
  .fp_dense_to_tidy(fp_mat, labels, target_labs)
}

.fp_dense_to_tidy <- function(
  fp_mat,
  labels,
  target_labs
) {
  fp_mat <- Matrix::drop0(
    methods::as(fp_mat, "CsparseMatrix")
  )
  sp <- Matrix::summary(fp_mat)
  if (nrow(sp) == 0) {
    return(.empty_footprint())
  }
  keep <- sp$x > 0
  if (!any(keep)) {
    return(.empty_footprint())
  }

  tibble::tibble(
    origin_area = as.integer(labels$area_code[sp$i[keep]]),
    origin_item = as.integer(labels$item_cbs_code[sp$i[keep]]),
    target_area = as.integer(target_labs$area_code[sp$j[keep]]),
    target_item = as.integer(target_labs$item_cbs_code[sp$j[keep]]),
    value = sp$x[keep]
  )
}

.infer_target_labels <- function(fp_mat, labels) {
  n_cols <- ncol(fp_mat)
  n_rows <- nrow(labels)
  if (n_cols == n_rows) {
    return(labels)
  }

  n_areas <- dplyr::n_distinct(labels$area_code)
  if (n_cols %% n_areas == 0) {
    n_fd <- n_cols %/% n_areas
    areas <- sort(unique(labels$area_code))
    return(tibble::tibble(
      area_code = rep(areas, each = n_fd),
      item_cbs_code = rep(NA_integer_, n_cols)
    ))
  }

  tibble::tibble(
    area_code = rep(NA_integer_, n_cols),
    item_cbs_code = rep(NA_integer_, n_cols)
  )
}

.empty_footprint <- function() {
  tibble::tibble(
    origin_area = integer(0),
    origin_item = integer(0),
    target_area = integer(0),
    target_item = integer(0),
    value = numeric(0)
  )
}

# --- Sparse LU factorisation with regularisation ---

# Factor (I - A) with diagonal regularisation. Sectors where
# column sums of A were capped at 1 make (I - A) near-singular.
# Adding a small epsilon to the diagonal is equivalent to
# assuming a tiny value-added leakage per sector.
.factor_ia <- function(ia, eps = 1e-10) {
  Matrix::diag(ia) <- Matrix::diag(ia) + eps
  Matrix::lu(ia)
}

# --- Input validation ---

.validate_footprint_inputs <- function(
  l_inv,
  x_vec,
  y_mat,
  extensions,
  labels,
  z_mat
) {
  n <- length(x_vec)

  if (is.null(l_inv) && is.null(z_mat)) {
    cli::cli_abort(
      "Provide either {.arg l_inv} or {.arg z_mat}."
    )
  }
  if (!is.null(l_inv)) {
    .validate_square_mat(l_inv, n, "l_inv")
  }
  if (!is.null(z_mat)) {
    .validate_square_mat(z_mat, n, "z_mat")
  }
  if (nrow(y_mat) != n) {
    cli::cli_abort(
      "{.arg y_mat} must have {n} rows to match
      {.arg x_vec}."
    )
  }
  if (length(extensions) != n) {
    cli::cli_abort(
      "{.arg extensions} length ({length(extensions)})
      must match {.arg x_vec} length ({n})."
    )
  }
  if (nrow(labels) != n) {
    cli::cli_abort(
      "{.arg labels} must have {n} rows to match
      {.arg x_vec}."
    )
  }
}

.validate_square_mat <- function(mat, n, name) {
  is_mat <- is.matrix(mat) || methods::is(mat, "Matrix")
  wrong_size <- nrow(mat) != n || ncol(mat) != n
  if (!is_mat || wrong_size) {
    cli::cli_abort(
      "{.arg {name}} must be a square matrix matching
      {.arg x_vec} length ({n})."
    )
  }
}
