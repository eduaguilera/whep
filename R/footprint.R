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
#' @param l_inv Leontief inverse matrix from
#'   [compute_leontief_inverse()].
#' @param x_vec Numeric vector of total output per sector.
#' @param y_mat Final demand matrix from [build_io_model()].
#' @param extensions Numeric vector of environmental extensions
#'   (e.g. hectares of land use) per sector. Must have the same
#'   length as `x_vec`.
#' @param labels Tibble with `area_code` and `item_cbs_code`
#'   mapping row/column indices to their meaning. From
#'   [build_io_model()].
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
#' compute_footprint(l_inv, x_vec, y_mat, extensions, labels)
compute_footprint <- function(
  l_inv, x_vec, y_mat, extensions, labels
) {
  .validate_footprint_inputs(
    l_inv, x_vec, y_mat, extensions, labels
  )
  n <- length(x_vec)
  n_fd <- ncol(y_mat)
  n_ext <- sum(extensions != 0)

  cli::cli_inform(c(
    "i" = "Computing footprint for {n} sectors.",
    " " = "{n_ext} sectors have non-zero extensions.",
    " " = "Final demand: {n_fd} column{?s}."
  ))

  cli::cli_inform("  Computing extension intensities...")
  intensity <- .extension_intensity(extensions, x_vec)

  cli::cli_inform(
    "  Multiplying L x Y ({n}x{n} * {n}x{n_fd})..."
  )
  mp_mat <- intensity * l_inv
  fp_mat <- mp_mat %*% y_mat

  cli::cli_inform("  Converting to tidy tibble...")
  result <- .footprint_to_tibble(fp_mat, labels)

  cli::cli_alert_success(
    "Footprint complete: {nrow(result)} non-zero flows."
  )
  result
}

.extension_intensity <- function(extensions, x_vec) {
  ifelse(x_vec == 0, 0, extensions / x_vec)
}

.footprint_to_tibble <- function(fp_mat, labels) {
  target_labels <- .build_target_labels(fp_mat, labels)

  origins <- rep(
    seq_len(nrow(fp_mat)), times = ncol(fp_mat)
  )
  targets <- rep(
    seq_len(ncol(fp_mat)), each = nrow(fp_mat)
  )
  values <- as.vector(fp_mat)
  keep <- values != 0

  tibble::tibble(
    origin_area = labels$area_code[origins[keep]],
    origin_item = labels$item_cbs_code[origins[keep]],
    target_area = target_labels$area_code[targets[keep]],
    target_item = target_labels$item_cbs_code[
      targets[keep]
    ],
    value = values[keep]
  )
}

.build_target_labels <- function(fp_mat, labels) {
  n_cols <- ncol(fp_mat)
  n_rows <- nrow(labels)
  if (n_cols == n_rows) return(labels)

  n_fd <- n_cols %/% n_rows
  if (n_cols == n_fd * n_rows) {
    return(
      labels |>
        dplyr::slice(rep(
          dplyr::row_number(), times = n_fd
        ))
    )
  }

  tibble::tibble(
    area_code = rep(NA_integer_, n_cols),
    item_cbs_code = rep(NA_integer_, n_cols)
  )
}

.validate_footprint_inputs <- function(
  l_inv, x_vec, y_mat, extensions, labels
) {
  n <- length(x_vec)
  if (!is.matrix(l_inv) ||
        nrow(l_inv) != n ||
        ncol(l_inv) != n) {
    cli::cli_abort(
      "{.arg l_inv} must be a square matrix matching
      {.arg x_vec} length ({n})."
    )
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
