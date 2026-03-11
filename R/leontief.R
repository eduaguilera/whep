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
#' Column sums of A are capped at 1 (FABIO convention) to ensure
#' \eqn{(I - A)} is invertible even when supply-use data are
#' inconsistent. The Leontief inverse is then \eqn{L = (I - A)^{-1}}.
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
compute_leontief_inverse <- function(z_mat, x_vec, max_n = 5000) {
  .validate_leontief_inputs(z_mat, x_vec)
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
  a_mat <- .technical_coefficients(z_mat, x_vec)

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

.technical_coefficients <- function(z_mat, x_vec) {
  x_inv <- ifelse(x_vec == 0, 0, 1 / x_vec)
  a_mat <- z_mat %*% Matrix::Diagonal(x = x_inv)
  # Cap column sums at 1 (FABIO convention): prevents (I-A)
  # from becoming singular when input shares exceed output.
  col_sums <- Matrix::colSums(a_mat)
  over <- col_sums > 1
  if (any(over)) {
    n_over <- sum(over)
    cli::cli_warn(
      "  Capping {n_over} column{?s} of A with sum > 1."
    )
    scale <- ifelse(over, 1 / col_sums, 1)
    a_mat <- a_mat %*% Matrix::Diagonal(x = scale)
  }
  a_mat
}

.validate_leontief_inputs <- function(z_mat, x_vec) {
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
}
