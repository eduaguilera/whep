#' Balance a matrix to target margins by RAS.
#'
#' @description
#' Reconcile a non-negative matrix to prescribed row and column sums
#' using the RAS (biproportional / iterative proportional fitting)
#' algorithm: alternately rescale rows then columns until both
#' margins are matched, preserving the matrix's structure (zero
#' pattern and relative magnitudes). The row and column targets must
#' have equal totals.
#'
#' Works on dense and sparse (`Matrix`) inputs, staying in the input
#' representation so it is efficient for both small dense tables and
#' large sparse input-output systems. (The same core also balances
#' the bilateral trade matrices.) For matrices with negative entries
#' the signed GRAS variant is needed and is not implemented here.
#'
#' @param x Non-negative matrix (dense or `Matrix`).
#' @param target_rows Desired row sums, length `nrow(x)`.
#' @param target_cols Desired column sums, length `ncol(x)`.
#' @param max_iter Maximum RAS iterations.
#' @param tol Convergence tolerance on the largest margin deviation,
#'   relative to the margin total (so it is scale-free).
#'
#' @return The balanced matrix, in the same representation as `x`
#'   (dense in, dense out; sparse in, sparse out).
#'
#' @export
#'
#' @examples
#' m <- matrix(c(1, 2, 3, 4), nrow = 2)
#' balance_ras(m, target_rows = c(10, 20), target_cols = c(12, 18))
balance_ras <- function(
  x,
  target_rows,
  target_cols,
  max_iter = 1000L,
  tol = 1e-9
) {
  .validate_ras_inputs(x, target_rows, target_cols, tol)
  res <- .ras_iterate(
    x,
    target_rows,
    target_cols,
    max_iter,
    tol,
    relative = TRUE
  )
  if (!res$converged) {
    cli::cli_warn(
      "RAS did not converge in {max_iter} iterations; returning best estimate."
    )
  }
  res$m
}

#' Balance input-output flows so the footprint conserves.
#'
#' @description
#' RAS-balance an inter-industry flow matrix toward the product-balance
#' margin. The row target is intermediate use,
#' \eqn{u = X - \text{rowSums}(Y)}; the column target keeps the original
#' column composition, rescaled so its total matches \eqn{\sum u}.
#'
#' When the balanced system is productive (the spectral radius of
#' \eqn{A} is below 1), \eqn{(I - A) X = Y} holds and the footprint
#' conserves -- the case for the productive systems in the examples and
#' tests.
#'
#' Caveat for physical agriculture: livestock feed conversion makes
#' some columns of \eqn{A} sum well above 1 (many tonnes of feed per
#' tonne of product), so the balanced system is not productive.
#' Balancing to faithful margins does not remove that, and the traced
#' footprint then over-traces rather than conserving (on the real 2010
#' model, by about 15%). RAS therefore does not, on its own, fix the
#' grassland under-tracing; always verify with
#' [check_footprint_conservation()].
#'
#' Pass the result as `z_mat` to [compute_footprint()] with a large
#' `max_column_sum` (e.g. `1e12`) and `conserve_extensions = FALSE`.
#'
#' @param z_mat Inter-industry flow matrix (dense or `Matrix`).
#' @param x_vec Total output per sector.
#' @param y_mat Final demand vector, or matrix whose row sums give
#'   total final demand per product.
#' @param max_iter,tol Passed to [balance_ras()].
#'
#' @return The balanced flow matrix.
#'
#' @export
#'
#' @examples
#' z_mat <- matrix(c(1, 4, 2, 3), nrow = 2)
#' x_vec <- c(10, 10)
#' y_mat <- c(3, 3)
#' balance_io_flows(z_mat, x_vec, y_mat)
balance_io_flows <- function(
  z_mat,
  x_vec,
  y_mat,
  max_iter = 1000L,
  tol = 1e-6
) {
  .validate_io_flows(z_mat, x_vec, y_mat)
  z0 <- z_mat
  if (any(z0 < 0)) {
    z0[z0 < 0] <- 0
  }
  y_total <- if (is.null(dim(y_mat))) {
    as.numeric(y_mat)
  } else {
    Matrix::rowSums(y_mat)
  }
  u <- pmax(x_vec - y_total, 0)
  col0 <- Matrix::colSums(z0)
  total_col <- sum(col0)
  v <- if (total_col > 0) col0 * (sum(u) / total_col) else rep(0, length(col0))
  balance_ras(z0, u, v, max_iter = max_iter, tol = tol)
}

# --- Shared RAS core (also used by .ipf_2d for bilateral trade) ---

# Alternately rescale rows then columns toward the targets. Operates
# in place on dense or sparse matrices; returns the matrix and whether
# it converged. Convergence is checked every few iterations to keep
# the per-iteration cost low on the large input-output matrix.
.ras_iterate <- function(
  m,
  target_rows,
  target_cols,
  max_iter = 1000L,
  tol = 1e-9,
  relative = FALSE
) {
  sparse <- methods::is(m, "sparseMatrix")
  nr <- nrow(m)
  # Relative convergence (scaled by the margin total) is needed for the
  # large input-output matrix, whose margins span many orders of
  # magnitude; the trade path keeps the original absolute tolerance.
  err_scale <- if (relative) max(sum(target_rows), sum(target_cols), 1) else 1
  converged <- FALSE
  for (i in seq_len(max_iter)) {
    rs <- Matrix::rowSums(m)
    m <- .ras_scale_rows(m, ifelse(rs > 0, target_rows / rs, 1), sparse)
    cs <- Matrix::colSums(m)
    m <- .ras_scale_cols(m, ifelse(cs > 0, target_cols / cs, 1), sparse, nr)
    if (
      i %% 5L == 0L &&
        max(abs(Matrix::rowSums(m) - target_rows)) < tol * err_scale &&
        max(abs(Matrix::colSums(m) - target_cols)) < tol * err_scale
    ) {
      converged <- TRUE
      break
    }
  }
  list(m = m, converged = converged)
}

.ras_scale_rows <- function(m, r, sparse) {
  if (sparse) Matrix::Diagonal(x = r) %*% m else m * r
}

.ras_scale_cols <- function(m, s, sparse, nr) {
  if (sparse) m %*% Matrix::Diagonal(x = s) else m * rep(s, each = nr)
}

# --- Validation ---

.validate_ras_inputs <- function(x, target_rows, target_cols, tol) {
  if (!is.matrix(x) && !methods::is(x, "Matrix")) {
    cli::cli_abort("{.arg x} must be a matrix.")
  }
  if (any(x < 0, na.rm = TRUE)) {
    cli::cli_abort(
      "{.arg x} must be non-negative (use GRAS for signed matrices)."
    )
  }
  if (length(target_rows) != nrow(x) || length(target_cols) != ncol(x)) {
    cli::cli_abort("Target lengths must match {.arg x} dimensions.")
  }
  if (any(target_rows < 0) || any(target_cols < 0)) {
    cli::cli_abort("Targets must be non-negative.")
  }
  rt <- sum(target_rows)
  ct <- sum(target_cols)
  if (abs(rt - ct) > tol * max(rt, ct, 1)) {
    cli::cli_abort(
      "Target row total ({signif(rt, 6)}) must equal column total
      ({signif(ct, 6)})."
    )
  }
  invisible(NULL)
}

.validate_io_flows <- function(z_mat, x_vec, y_mat) {
  if (!is.matrix(z_mat) && !methods::is(z_mat, "Matrix")) {
    cli::cli_abort("{.arg z_mat} must be a matrix.")
  }
  n <- length(x_vec)
  if (nrow(z_mat) != n || ncol(z_mat) != n) {
    cli::cli_abort("{.arg z_mat} must be square matching {.arg x_vec}.")
  }
  y_rows <- if (is.null(dim(y_mat))) length(y_mat) else nrow(y_mat)
  if (y_rows != n) {
    cli::cli_abort("{.arg y_mat} must have {n} rows to match {.arg x_vec}.")
  }
}
