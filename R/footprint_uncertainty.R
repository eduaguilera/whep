#' Propagate input uncertainty through a footprint.
#'
#' @description
#' Monte Carlo propagation of extension uncertainty: perturb the
#' extension vector with multiplicative lognormal noise (so values
#' stay non-negative and the expected factor is one), re-run the
#' footprint for each draw, and summarise the spread of each output
#' cell. A point estimate with no interval is not a trustworthy
#' result; this turns one into a distribution.
#'
#' @param run_fn Function taking a perturbed extension vector and
#'   returning a footprint tibble with the grouping columns named
#'   in `options$by` plus a `value` column. Wrap
#'   [compute_footprint()] with the other arguments fixed.
#' @param extensions Numeric vector of base extensions per sector.
#' @param cov Coefficient of variation of the extensions: one
#'   number, or one per sector. Zero means no uncertainty.
#' @param options Named list overriding `n` (draws, default 200),
#'   `probs` (lower/median/upper quantiles), `by` (grouping
#'   columns) and `seed` (for reproducible draws).
#'
#' @return A tibble with the `by` columns plus `mean`, `sd`, `cv`,
#'   `q_low`, `q_med` and `q_high` per output cell.
#'
#' @export
#'
#' @examples
#' run_fn <- function(ext) {
#'   tibble::tibble(
#'     target_area = 1L, target_item = 10L, value = sum(ext)
#'   )
#' }
#' propagate_fp_uncertainty(
#'   run_fn,
#'   extensions = c(60, 40),
#'   cov = 0.1,
#'   options = list(n = 100, seed = 1, by = c("target_area", "target_item"))
#' )
propagate_fp_uncertainty <- function(
  run_fn,
  extensions,
  cov = 0.1,
  options = list()
) {
  .validate_run_fn(run_fn)
  opt <- .uncertainty_options(options)
  if (!is.null(opt$seed)) {
    set.seed(opt$seed)
  }
  factors <- .lognormal_factors(cov, length(extensions), opt$n)
  purrr::map(seq_len(opt$n), function(k) {
    .aggregate_run(run_fn, extensions * factors[, k], opt$by) |>
      dplyr::mutate(.draw = k)
  }) |>
    purrr::list_rbind() |>
    .summarise_draws(opt$by, opt$probs)
}

#' Combine independent coefficient-of-variation components.
#'
#' @description
#' Aggregate several independent sources of relative uncertainty
#' into a single coefficient of variation by adding them in
#' quadrature. This is how per-indicator data-quality scores (for
#' example the reliability, completeness and representativeness
#' columns of a pedigree matrix) are turned into one CoV for
#' [propagate_fp_uncertainty()]. Supply the per-indicator
#' CoV values yourself from a sourced, citable pedigree table; no
#' uncertainty factors are hard-coded here.
#'
#' @param ... Numeric vectors of equal length, one per uncertainty
#'   source, or a single matrix/data frame with one column per
#'   source.
#'
#' @return A numeric vector of combined coefficients of variation.
#'
#' @export
#'
#' @examples
#' combine_cov(c(0.3, 0.1), c(0.4, 0.2))
combine_cov <- function(...) {
  args <- list(...)
  if (length(args) == 1 && (is.matrix(args[[1]]) || is.data.frame(args[[1]]))) {
    m <- as.matrix(args[[1]])
  } else {
    m <- do.call(cbind, args)
  }
  if (!is.numeric(m) || any(m < 0, na.rm = TRUE)) {
    cli::cli_abort("CoV components must be non-negative numbers.")
  }
  sqrt(rowSums(m^2))
}

#' Local sensitivity of a footprint to each extension.
#'
#' @description
#' One-at-a-time sensitivity analysis: nudge each sector's
#' extension by a small relative step and measure the elasticity of
#' the total footprint. High-elasticity sectors are where data
#' quality matters most and where a result is most fragile.
#'
#' @param run_fn Function taking an extension vector and returning a
#'   footprint tibble with a `value` column.
#' @param extensions Numeric vector of base extensions per sector.
#' @param options Named list overriding `delta` (relative step,
#'   default 0.05) and `which` (sector indices to test; default all
#'   non-zero extensions).
#'
#' @return A tibble with `sector` (index into `extensions`) and
#'   `elasticity`, ordered by descending absolute elasticity.
#'
#' @export
#'
#' @examples
#' run_fn <- function(ext) tibble::tibble(value = sum(ext))
#' footprint_sensitivity(run_fn, extensions = c(60, 40))
footprint_sensitivity <- function(run_fn, extensions, options = list()) {
  .validate_run_fn(run_fn)
  opt <- .sensitivity_options(options)
  base <- .total_output(run_fn, extensions)
  test <- if (is.null(opt$which)) which(extensions != 0) else opt$which
  elasticity <- purrr::map_dbl(test, function(i) {
    bumped <- extensions
    bumped[i] <- bumped[i] * (1 + opt$delta)
    .elasticity(base, .total_output(run_fn, bumped), opt$delta)
  })
  tibble::tibble(sector = test, elasticity = elasticity) |>
    dplyr::arrange(dplyr::desc(abs(elasticity)))
}

# --- Helpers ---

.lognormal_factors <- function(cov, n_sector, n_draw) {
  cov <- if (length(cov) == 1) rep(cov, n_sector) else cov
  if (length(cov) != n_sector || any(cov < 0)) {
    cli::cli_abort(
      "{.arg cov} must be one non-negative number or one per sector."
    )
  }
  sdlog <- sqrt(log(1 + cov^2))
  meanlog <- -sdlog^2 / 2
  matrix(
    stats::rlnorm(n_sector * n_draw, meanlog, sdlog),
    nrow = n_sector
  )
}

.aggregate_run <- function(run_fn, extensions, by) {
  result <- run_fn(extensions)
  .require_cols(result, c(by, "value"), "run_fn output")
  dplyr::summarise(result, value = sum(value), .by = dplyr::all_of(by))
}

.summarise_draws <- function(draws, by, probs) {
  draws |>
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(by)),
      .draw,
      fill = list(value = 0)
    ) |>
    dplyr::summarise(
      mean = mean(value),
      sd = stats::sd(value),
      cv = .safe_rel(stats::sd(value), mean(value)),
      q_low = stats::quantile(value, probs[1], names = FALSE),
      q_med = stats::quantile(value, probs[2], names = FALSE),
      q_high = stats::quantile(value, probs[3], names = FALSE),
      .by = dplyr::all_of(by)
    )
}

.total_output <- function(run_fn, extensions) {
  result <- run_fn(extensions)
  .require_cols(result, "value", "run_fn output")
  sum(result$value)
}

.elasticity <- function(base, bumped, delta) {
  if (base == 0) {
    return(NA_real_)
  }
  ((bumped - base) / base) / delta
}

.uncertainty_options <- function(options) {
  .merge_options(
    options,
    list(
      n = 200L,
      probs = c(0.025, 0.5, 0.975),
      by = c("target_area", "target_item"),
      seed = NULL
    )
  )
}

.sensitivity_options <- function(options) {
  .merge_options(options, list(delta = 0.05, which = NULL))
}

.merge_options <- function(options, defaults) {
  if (!is.list(options)) {
    cli::cli_abort("{.arg options} must be a named list.")
  }
  unknown <- setdiff(names(options), names(defaults))
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown option{?s}: {.field {unknown}}.")
  }
  utils::modifyList(defaults, options)
}

.validate_run_fn <- function(run_fn) {
  if (!is.function(run_fn)) {
    cli::cli_abort("{.arg run_fn} must be a function.")
  }
}
