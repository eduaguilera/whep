#' Compute Leontief inverse.
#'
#' @description
#' Compute the Leontief inverse matrix from intermediate flows
#' and total output. The Leontief inverse captures both direct
#' and indirect requirements across the entire supply chain,
#' enabling footprint tracing.
#'
#' The technical coefficients matrix is computed as
#' \eqn{A_{ij} = Z_{ij} / X_j}, representing the input of
#' sector \eqn{i} needed per unit of output from sector \eqn{j}.
#' Column sums of A are capped using `max_column_sum` to avoid singular
#' systems from inconsistent supply-use data. By default this uses
#' `1 - value_added_floor`, preserving the previous conservative behavior
#' for explicit Leontief inverses. The Leontief inverse is then
#' \eqn{L = (I - A)^{-1}}.
#'
#' For large systems (thousands of sectors) this function is not
#' usable: the dense L matrix requires \eqn{n^2 \times 8} bytes
#' of memory (e.g. ~4.8 GiB for n = 25 000). Use
#' [compute_footprint()] directly with `z_mat` and `x_vec`
#' instead, which solves \eqn{(I - A) x = Y} without ever
#' materialising L.
#'
#' Accepts both dense and sparse (Matrix package) inputs.
#'
#' @param z_mat Square numeric matrix of inter-industry flows.
#'   Entry \eqn{Z_{ij}} is the flow from sector \eqn{i} to
#'   sector \eqn{j}. Can be dense or sparse.
#' @param x_vec Numeric vector of total output per sector. Must
#'   have the same length as `nrow(z_mat)`.
#' @param max_n Maximum system size before aborting. Defaults to
#'   5000. Set higher at your own risk of memory exhaustion.
#' @param value_added_floor Minimum share of each sector's output that
#'   is treated as non-intermediate leakage when constructing A if
#'   `max_column_sum` is left at its default.
#' @param max_column_sum Maximum allowed column sum in A. Columns above this
#'   value are rescaled. Defaults to `1 - value_added_floor`.
#'
#' @return The Leontief inverse matrix \eqn{L}. Negative
#'   values are set to zero. Returns a dense matrix.
#'
#' @export
#'
#' @examples
#' z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
#' x_vec <- c(100, 200)
#' compute_leontief_inverse(z_mat, x_vec)
compute_leontief_inverse <- function(
  z_mat,
  x_vec,
  max_n = 5000,
  value_added_floor = 1e-3,
  max_column_sum = 1 - value_added_floor
) {
  .validate_leontief_inputs(
    z_mat,
    x_vec,
    value_added_floor,
    max_column_sum
  )
  n <- nrow(z_mat)

  if (n > max_n) {
    gb <- round(n^2 * 8 / 1e9, 1)
    cli::cli_abort(c(
      "System too large to invert explicitly (n = {n}).",
      "i" = "The dense L matrix would require ~{gb} GiB.",
      "i" = "Use {.fn compute_footprint} with {.arg z_mat}
        and {.arg x_vec} instead: it solves
        {.code (I - A) x = Y} without forming L.",
      "i" = "Override with {.code max_n = {n}} only if you
        have sufficient memory."
    ))
  }

  cli::cli_inform(
    "Computing Leontief inverse ({n}x{n} matrix)..."
  )
  a_mat <- .technical_coefficients(
    z_mat,
    x_vec,
    value_added_floor = value_added_floor,
    max_column_sum = max_column_sum
  )

  cli::cli_inform("  Inverting (I - A)...")
  i_minus_a <- Matrix::Diagonal(n) - a_mat
  l_inv <- as.matrix(solve(i_minus_a))

  n_neg <- sum(l_inv < 0)
  if (n_neg > 0) {
    cli::cli_inform(
      "  Zeroing {n_neg} negative entr{?y/ies} in L."
    )
  }
  l_inv[l_inv < 0] <- 0

  cli::cli_alert_success("Leontief inverse computed.")
  l_inv
}

.technical_coefficients <- function(
  z_mat,
  x_vec,
  value_added_floor = 1e-3,
  max_column_sum = 1 - value_added_floor
) {
  .validate_value_added_floor(value_added_floor)
  .validate_max_column_sum(max_column_sum)
  x_inv <- ifelse(x_vec == 0, 0, 1 / x_vec)
  a_mat <- z_mat %*% Matrix::Diagonal(x = x_inv)
  # Zero negative entries in A (FABIO convention): these arise
  # from data inconsistencies and would distort the inverse.
  if (methods::is(a_mat, "sparseMatrix")) {
    neg_count <- sum(a_mat@x < 0)
    if (neg_count > 0) {
      cli::cli_inform(
        "  Zeroing {neg_count} negative entr{?y/ies} in A."
      )
      a_mat@x <- pmax(a_mat@x, 0)
    }
  } else {
    neg_count <- sum(a_mat < 0)
    if (neg_count > 0) {
      cli::cli_inform(
        "  Zeroing {neg_count} negative entr{?y/ies} in A."
      )
      a_mat[a_mat < 0] <- 0
    }
  }
  col_sums <- Matrix::colSums(a_mat)
  over <- col_sums > max_column_sum
  if (any(over)) {
    n_over <- sum(over)
    cli::cli_warn(
      "  Capping {n_over} column{?s} of A with sum > {signif(max_column_sum, 6)}."
    )
    scale <- ifelse(over & col_sums > 0, max_column_sum / col_sums, 1)
    a_mat <- a_mat %*% Matrix::Diagonal(x = scale)
  }
  a_mat
}

.validate_leontief_inputs <- function(
  z_mat,
  x_vec,
  value_added_floor,
  max_column_sum
) {
  if (!methods::is(z_mat, "Matrix") && !is.matrix(z_mat)) {
    cli::cli_abort("{.arg z_mat} must be a matrix.")
  }
  if (nrow(z_mat) != ncol(z_mat)) {
    cli::cli_abort(
      "{.arg z_mat} must be a square matrix."
    )
  }
  if (length(x_vec) != nrow(z_mat)) {
    cli::cli_abort(
      "{.arg x_vec} length ({length(x_vec)}) must match
      {.arg z_mat} dimensions ({nrow(z_mat)})."
    )
  }
  .validate_value_added_floor(value_added_floor)
  .validate_max_column_sum(max_column_sum)
}

.validate_value_added_floor <- function(value_added_floor) {
  if (
    !is.numeric(value_added_floor) ||
      length(value_added_floor) != 1 ||
      is.na(value_added_floor) ||
      !is.finite(value_added_floor) ||
      value_added_floor < 0 ||
      value_added_floor >= 1
  ) {
    cli::cli_abort(
      "{.arg value_added_floor} must be one finite number in [0, 1)."
    )
  }
}

.validate_max_column_sum <- function(max_column_sum) {
  if (
    !is.numeric(max_column_sum) ||
      length(max_column_sum) != 1 ||
      is.na(max_column_sum) ||
      !is.finite(max_column_sum) ||
      max_column_sum <= 0
  ) {
    cli::cli_abort(
      "{.arg max_column_sum} must be one finite positive number."
    )
  }
}
