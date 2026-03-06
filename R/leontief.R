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
#' The Leontief inverse is then \eqn{L = (I - A)^{-1}}.
#'
#' @param z_mat Square numeric matrix of inter-industry flows.
#'   Entry \eqn{Z_{ij}} is the flow from sector \eqn{i} to
#'   sector \eqn{j}.
#' @param x_vec Numeric vector of total output per sector. Must
#'   have the same length as `nrow(z_mat)`.
#'
#' @return The Leontief inverse matrix \eqn{L}. Negative
#'   values are set to zero.
#'
#' @export
#'
#' @examples
#' z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
#' x_vec <- c(100, 200)
#' compute_leontief_inverse(z_mat, x_vec)
compute_leontief_inverse <- function(z_mat, x_vec) {
  .validate_leontief_inputs(z_mat, x_vec)
  n <- nrow(z_mat)

  cli::cli_inform(
    "Computing Leontief inverse ({n}x{n} matrix)..."
  )
  a_mat <- .technical_coefficients(z_mat, x_vec)

  cli::cli_inform("  Inverting (I - A)...")
  l_inv <- solve(diag(n) - a_mat)

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

.technical_coefficients <- function(z_mat, x_vec) {
  x_inv <- ifelse(x_vec == 0, 0, 1 / x_vec)
  t(t(z_mat) * x_inv)
}

.validate_leontief_inputs <- function(z_mat, x_vec) {
  if (!is.matrix(z_mat)) {
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
}
