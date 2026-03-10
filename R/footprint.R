#' Compute environmental footprints.
#'
#' @description
#' Trace environmental extensions through the supply chain using
#' the Leontief inverse. The footprint shows how much of an
#' environmental pressure (e.g. land use, water, emissions) is
#' embodied in the final consumption of each product in each
#' country.
#'
#' The multiplier matrix is computed as
#' \eqn{MP_{ij} = (e_i / X_i) \cdot L_{ij}}, where \eqn{e_i}
#' is the extension for sector \eqn{i}. The footprint is then
#' \eqn{FP = MP \times Y}.
#'
#' For large systems, pass `z_mat` and `x_vec` instead of
#' `l_inv`. This solves the system \eqn{(I - A)x = Y} directly
#' using a sparse LU factorisation, avoiding the dense Leontief
#' inverse entirely. This reduces memory from
#' \eqn{O(n^2)} to \eqn{O(nnz)} and is dramatically faster for
#' systems with >10 000 sectors.
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
#'   [build_io_model()]. When provided, the Leontief system is
#'   solved directly (sparse), and `l_inv` is not needed.
#'
#' @return A tibble with footprint results containing:
#'   - `origin_area`: Country where the pressure occurs.
#'   - `origin_item`: Item causing the pressure.
#'   - `target_area`: Country consuming the product.
#'   - `target_item`: Product consumed.
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
#' # Large system: pass Z directly (avoids dense L)
#' compute_footprint(
#'   x_vec = x_vec, y_mat = y_mat,
#'   extensions = extensions, labels = labels,
#'   z_mat = z_mat
#' )
compute_footprint <- function(
  l_inv = NULL, x_vec, y_mat, extensions, labels,
  z_mat = NULL
) {
  n <- length(x_vec)
  .validate_footprint_inputs(
    l_inv, x_vec, y_mat, extensions, labels, z_mat
  )
  n_fd <- ncol(y_mat)
  n_ext <- sum(extensions != 0)

  cli::cli_inform(c(
    "i" = "Computing footprint for {n} sectors.",
    " " = "{n_ext} sectors have non-zero extensions.",
    " " = "Final demand: {n_fd} column{?s}."
  ))

  cli::cli_inform("  Computing extension intensities...")
  intensity <- .extension_intensity(extensions, x_vec)
  f_diag <- Matrix::Diagonal(x = intensity)

  if (!is.null(z_mat)) {
    fp_mat <- .footprint_sparse(
      z_mat, x_vec, y_mat, f_diag, n
    )
  } else {
    fp_mat <- .footprint_dense(l_inv, y_mat, f_diag, n)
  }

  cli::cli_inform("  Converting to tidy tibble...")
  result <- .footprint_to_tibble(fp_mat, labels)

  cli::cli_alert_success(
    "Footprint complete: {nrow(result)} non-zero flows."
  )
  result
}

.footprint_sparse <- function(
  z_mat, x_vec, y_mat, f_diag, n
) {
  cli::cli_inform(
    "  Sparse path: solving (I-A)x = Y directly..."
  )
  a_mat <- .technical_coefficients(z_mat, x_vec)
  i_minus_a <- Matrix::Diagonal(n) - a_mat

  cli::cli_inform(
    "  Solving system ({n}x{n} * {n}x{ncol(y_mat)})..."
  )
  l_times_y <- Matrix::solve(i_minus_a, y_mat)

  cli::cli_inform("  Applying extension intensities...")
  fp_mat <- f_diag %*% l_times_y
  fp_mat[fp_mat < 0] <- 0
  fp_mat
}

.footprint_dense <- function(l_inv, y_mat, f_diag, n) {
  cli::cli_inform(
    "  Dense path: multiplying f*L*Y ({n}x{n})..."
  )
  mp_mat <- f_diag %*% l_inv
  mp_mat %*% y_mat
}

.extension_intensity <- function(extensions, x_vec) {
  ifelse(x_vec == 0, 0, extensions / x_vec)
}

.footprint_to_tibble <- function(fp_mat, labels) {
  target_labels <- .build_target_labels(fp_mat, labels)

  if (methods::is(fp_mat, "sparseMatrix")) {
    sp <- Matrix::summary(fp_mat)
    keep <- sp$x != 0
    tibble::tibble(
      origin_area = labels$area_code[sp$i[keep]],
      origin_item = labels$item_cbs_code[sp$i[keep]],
      target_area = target_labels$area_code[sp$j[keep]],
      target_item = target_labels$item_cbs_code[
        sp$j[keep]
      ],
      value = sp$x[keep]
    )
  } else {
    origins <- rep(
      seq_len(nrow(fp_mat)),
      times = ncol(fp_mat)
    )
    targets <- rep(
      seq_len(ncol(fp_mat)),
      each = nrow(fp_mat)
    )
    values <- as.vector(fp_mat)
    keep <- values != 0
    tibble::tibble(
      origin_area = labels$area_code[origins[keep]],
      origin_item = labels$item_cbs_code[origins[keep]],
      target_area = target_labels$area_code[
        targets[keep]
      ],
      target_item = target_labels$item_cbs_code[
        targets[keep]
      ],
      value = values[keep]
    )
  }
}

.build_target_labels <- function(fp_mat, labels) {
  n_cols <- ncol(fp_mat)
  n_rows <- nrow(labels)
  if (n_cols == n_rows) {
    return(labels)
  }

  n_fd <- n_cols %/% n_rows
  if (n_cols == n_fd * n_rows) {
    return(
      labels |>
        dplyr::slice(rep(
          dplyr::row_number(),
          times = n_fd
        ))
    )
  }

  tibble::tibble(
    area_code = rep(NA_integer_, n_cols),
    item_cbs_code = rep(NA_integer_, n_cols)
  )
}

.validate_footprint_inputs <- function(
  l_inv, x_vec, y_mat, extensions, labels, z_mat
) {
  n <- length(x_vec)

  if (is.null(l_inv) && is.null(z_mat)) {
    cli::cli_abort(
      "Provide either {.arg l_inv} or {.arg z_mat}."
    )
  }
  if (!is.null(l_inv)) {
    is_mat <- is.matrix(l_inv) ||
      methods::is(l_inv, "Matrix")
    if (!is_mat || nrow(l_inv) != n ||
      ncol(l_inv) != n) {
      cli::cli_abort(
        "{.arg l_inv} must be a square matrix matching
        {.arg x_vec} length ({n})."
      )
    }
  }
  if (!is.null(z_mat)) {
    is_mat <- is.matrix(z_mat) ||
      methods::is(z_mat, "Matrix")
    if (!is_mat || nrow(z_mat) != n ||
      ncol(z_mat) != n) {
      cli::cli_abort(
        "{.arg z_mat} must be a square matrix matching
        {.arg x_vec} length ({n})."
      )
    }
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
