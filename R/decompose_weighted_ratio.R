#' Decompose a weighted aggregate ratio.
#'
#' @description
#'   Separates a change in an aggregate numerator-to-denominator ratio into a
#'   between-group composition effect and a within-group ratio effect. The
#'   default symmetric Kitagawa allocation is equivalent to a two-factor
#'   Shapley value for the weight and within-group-ratio factor blocks.
#'
#' @details
#'   For group `g` and endpoint `t`, the aggregate ratio is
#'   `R_t = sum_g(w_gt * r_gt)`, where `w_gt` is the group's share of the total
#'   denominator and `r_gt` is its numerator-to-denominator ratio.
#'
#'   Write `dw_g = w_g1 - w_g0` and `dr_g = r_g1 - r_g0`. The symmetric
#'   Kitagawa contributions are
#'   \deqn{B_g = dw_g (r_g0 + r_g1) / 2,}
#'   \deqn{W_g = (w_g0 + w_g1) dr_g / 2.}
#'   The weights-first polar contributions are `B_g = dw_g r_g0` and
#'   `W_g = w_g1 dr_g`; the ratios-first polar contributions are
#'   `B_g = dw_g r_g1` and `W_g = w_g0 dr_g`. Kitagawa is their arithmetic
#'   mean. Its Shapley interpretation concerns the complete weight and ratio
#'   factor blocks; groups are not treated as Shapley players.
#'
#'   `"lmdi"` implements additive LMDI-I. With `y_gt = w_gt r_gt` and the
#'   logarithmic mean `L(a, b) = (a - b) / (log(a) - log(b))`, using
#'   `L(a, a) = a`, its contributions are
#'   \deqn{B_g = L(y_g1, y_g0) log(w_g1 / w_g0),}
#'   \deqn{W_g = L(y_g1, y_g0) log(r_g1 / r_g0).}
#'   Every method satisfies `B_g + W_g = y_g1 - y_g0` and therefore
#'   `sum_g(B_g + W_g) = R_1 - R_0`, up to floating-point error.
#'   `"all"` returns every method on their common strictly positive domain.
#'
#'   The function requires exactly two periods and identical, unique group
#'   support. Denominators must be positive. Numerators may be zero for the
#'   arithmetic methods but must be positive for LMDI. Groups are never
#'   silently dropped and zeros are never replaced by an epsilon.
#'
#'   Reversing endpoints negates Kitagawa and LMDI-I contributions. Reversing a
#'   polar path negates the opposite forward polar path. Percentage
#'   contributions are signed and are not clipped to zero or 100. They are
#'   missing when `abs(R_1 - R_0)` is no larger than
#'   `sqrt(.Machine$double.eps) * max(abs(R_0), abs(R_1))`. High
#'   `cancellation_index` values indicate cancellation between the aggregate
#'   between and within effects. It is defined as
#'   `1 - abs(total_change) / (abs(between) + abs(within))`, with zero used when
#'   every effect is zero. Results are descriptive accounting identities, not
#'   causal attribution, and depend on the chosen group resolution.
#'
#' @param data A tibble with exactly two periods and one row per group-period.
#' @param time An unquoted numeric, date-time, or ordered-factor column
#'   identifying the two ordered periods.
#' @param .by An unquoted column identifying persistent groups.
#' @param ratio An unquoted expression of the exact form
#'   `numerator / denominator`, using two bare numeric column names.
#' @param method Decomposition method. One of `"kitagawa"`, `"lmdi"`,
#'   `"weights_first"`, `"ratios_first"`, or `"all"`.
#'
#' @return
#'   A tibble with one `component_type = "group"` row per group and one
#'   `component_type = "summary"` row for each requested method. Group rows
#'   contain endpoint stocks, weights, ratios, signed contributions, and group
#'   closure. Summary rows contain aggregate endpoint ratios, signed effects,
#'   percentage contributions, closure diagnostics, and cancellation metrics.
#'   Additive signed contributions have the same units as `ratio`.
#'
#' @references
#' Kitagawa, E. M. (1955). Components of a Difference Between Two Rates.
#' *Journal of the American Statistical Association*, 50(272), 1168-1194.
#' \doi{10.1080/01621459.1955.10501299}.
#'
#' Ang, B. W. (2005). The LMDI approach to decomposition analysis: a practical
#' guide. *Energy Policy*, 33(7), 867-871.
#' \doi{10.1016/j.enpol.2003.10.010}.
#'
#' @export
#'
#' @examples
#' ratio_data <- tibble::tribble(
#'   ~year, ~group, ~numerator, ~denominator,
#'   2000, "a", 400, 40,
#'   2000, "b", 1200, 60,
#'   2020, "a", 600, 50,
#'   2020, "b", 900, 50
#' )
#'
#' decompose_weighted_ratio(
#'   ratio_data,
#'   year,
#'   group,
#'   numerator / denominator
#' )
decompose_weighted_ratio <- function(
  data,
  time,
  .by,
  ratio,
  method = c(
    "kitagawa",
    "lmdi",
    "weights_first",
    "ratios_first",
    "all"
  )
) {
  method <- rlang::arg_match(method)
  columns <- .ratio_decomp_columns(
    data,
    rlang::enquo(time),
    rlang::enquo(.by),
    rlang::enquo(ratio)
  )
  prepared <- .ratio_decomp_prepare(data, columns)
  .validate_ratio_decomp(prepared, method)
  state <- .ratio_decomp_state(
    prepared,
    columns$ratio_expression,
    columns$group
  )
  .validate_ratio_state(state$values, method)

  .ratio_decomp_methods(method) |>
    purrr::map_dfr(.ratio_decomp_one, state = state)
}

.ratio_decomp_columns <- function(data, time, .by, ratio) {
  if (!tibble::is_tibble(data)) {
    cli::cli_abort(
      "{.arg data} must be a tibble.",
      class = "whep_ratio_input"
    )
  }
  time_name <- .ratio_bare_column(time, "time")
  group_name <- .ratio_bare_column(.by, ".by")
  ratio_names <- .ratio_expression_columns(ratio)
  column_names <- c(time_name, group_name, ratio_names)
  missing_names <- column_names[
    !vapply(
      column_names,
      rlang::has_name,
      logical(1),
      x = data
    )
  ]

  if (length(missing_names) > 0L) {
    cli::cli_abort(
      "Missing required column{?s}: {.field {missing_names}}.",
      class = "whep_ratio_input"
    )
  }
  if (anyDuplicated(column_names)) {
    cli::cli_abort(
      "Time, group, numerator, and denominator columns must be distinct.",
      class = "whep_ratio_input"
    )
  }

  list(
    time = time_name,
    group = group_name,
    numerator = ratio_names[[1]],
    denominator = ratio_names[[2]],
    ratio_expression = stringr::str_c(
      ratio_names[[1]],
      " / ",
      ratio_names[[2]]
    )
  )
}

.ratio_bare_column <- function(column, argument) {
  if (!rlang::quo_is_symbol(column)) {
    cli::cli_abort(
      "{.arg {argument}} must be one unquoted column name.",
      class = "whep_ratio_input"
    )
  }
  rlang::as_name(rlang::quo_get_expr(column))
}

.ratio_expression_columns <- function(ratio) {
  expression <- rlang::quo_get_expr(ratio)
  valid <- rlang::is_call(expression, "/") &&
    length(expression) == 3L &&
    rlang::is_symbol(expression[[2]]) &&
    rlang::is_symbol(expression[[3]])

  if (!valid) {
    cli::cli_abort(
      "{.arg ratio} must have the form {.code numerator / denominator} using bare column names.",
      class = "whep_ratio_input"
    )
  }
  c(rlang::as_name(expression[[2]]), rlang::as_name(expression[[3]]))
}

.ratio_decomp_prepare <- function(data, columns) {
  data |>
    dplyr::transmute(
      .period = .data[[columns$time]],
      .group = .data[[columns$group]],
      .numerator = .data[[columns$numerator]],
      .denominator = .data[[columns$denominator]]
    )
}

.validate_ratio_decomp <- function(data, method) {
  .validate_ratio_keys(data)
  .validate_ratio_values(data)
  .validate_ratio_support(data)

  if (method %in% c("lmdi", "all") && any(data$.numerator <= 0)) {
    cli::cli_abort(
      "LMDI requires strictly positive numerator values; {.arg method} = {.val {method}} uses the common positive-data domain.",
      class = "whep_ratio_zero"
    )
  }
  invisible(data)
}

.validate_ratio_keys <- function(data) {
  if (nrow(data) == 0L || anyNA(data$.period) || anyNA(data$.group)) {
    cli::cli_abort(
      "Time and group values must be non-missing and data must not be empty.",
      class = "whep_ratio_input"
    )
  }
  periods <- dplyr::n_distinct(data$.period)
  if (periods != 2L) {
    cli::cli_abort(
      "{.arg time} must contain exactly two distinct periods, not {periods}.",
      class = "whep_ratio_periods"
    )
  }
  if (!is.atomic(data$.group)) {
    cli::cli_abort(
      "{.arg .by} must identify groups with an atomic vector.",
      class = "whep_ratio_input"
    )
  }
  .validate_ratio_time(data$.period)
}

.validate_ratio_time <- function(period) {
  valid <- is.numeric(period) ||
    inherits(period, "Date") ||
    inherits(period, "POSIXt") ||
    is.ordered(period)
  if (!valid) {
    cli::cli_abort(
      "{.arg time} must be numeric, date-time, or an ordered factor.",
      class = "whep_ratio_periods"
    )
  }
  if (is.numeric(period) && any(!is.finite(period))) {
    cli::cli_abort(
      "Numeric {.arg time} values must be finite.",
      class = "whep_ratio_periods"
    )
  }
}

.validate_ratio_values <- function(data) {
  if (!is.numeric(data$.numerator) || !is.numeric(data$.denominator)) {
    cli::cli_abort(
      "Numerator and denominator columns must be numeric.",
      class = "whep_ratio_input"
    )
  }
  if (
    any(!is.finite(data$.numerator)) ||
      any(!is.finite(data$.denominator))
  ) {
    cli::cli_abort(
      "Numerator and denominator values must be finite and non-missing.",
      class = "whep_ratio_input"
    )
  }
  if (any(data$.numerator < 0) || any(data$.denominator <= 0)) {
    cli::cli_abort(
      "Numerators must be non-negative and denominators must be positive.",
      class = "whep_ratio_domain"
    )
  }
}

.validate_ratio_support <- function(data) {
  counts <- data |>
    dplyr::count(.data$.period, .data$.group, name = ".row_count")
  if (any(counts$.row_count != 1L)) {
    cli::cli_abort(
      "Every group-period key must occur exactly once.",
      class = "whep_ratio_keys"
    )
  }
  support <- counts |>
    dplyr::count(.data$.group, name = ".period_count")
  if (any(support$.period_count != 2L)) {
    cli::cli_abort(
      "The two periods must have identical group support.",
      class = "whep_ratio_support"
    )
  }
}

.ratio_decomp_state <- function(data, ratio_expression, group_name) {
  periods <- data |>
    dplyr::distinct(.data$.period) |>
    dplyr::arrange(.data$.period) |>
    dplyr::pull(.data$.period)
  start <- .ratio_endpoint(data, periods[1], "start")
  end <- .ratio_endpoint(data, periods[2], "end")
  values <- dplyr::inner_join(start, end, by = ".group") |>
    dplyr::arrange(.data$.group) |>
    dplyr::mutate(
      weight_start = .data$denominator_start /
        sum(.data$denominator_start),
      weight_end = .data$denominator_end / sum(.data$denominator_end),
      within_ratio_start = .data$numerator_start /
        .data$denominator_start,
      within_ratio_end = .data$numerator_end / .data$denominator_end,
      ratio_contribution_start = .data$weight_start *
        .data$within_ratio_start,
      ratio_contribution_end = .data$weight_end * .data$within_ratio_end
    )
  .validate_ratio_weights(values)

  list(
    values = values,
    periods = periods,
    ratio_expression = ratio_expression,
    group_name = group_name
  )
}

.validate_ratio_weights <- function(data) {
  tolerance <- sqrt(.Machine$double.eps)
  weights <- c(data$weight_start, data$weight_end)
  if (any(!is.finite(weights))) {
    cli::cli_abort(
      "Derived denominator weights must be finite.",
      class = "whep_ratio_weights"
    )
  }
  if (
    abs(sum(data$weight_start) - 1) > tolerance ||
      abs(sum(data$weight_end) - 1) > tolerance
  ) {
    cli::cli_abort(
      "Derived denominator weights do not sum to one.",
      class = "whep_ratio_weights"
    )
  }
  invisible(data)
}

.validate_ratio_state <- function(data, method) {
  derived <- c(
    data$weight_start,
    data$weight_end,
    data$within_ratio_start,
    data$within_ratio_end,
    data$ratio_contribution_start,
    data$ratio_contribution_end
  )
  if (any(!is.finite(derived))) {
    cli::cli_abort(
      "Derived weights and ratios must be finite.",
      class = "whep_ratio_domain"
    )
  }
  if (
    method %in%
      c("lmdi", "all") &&
      any(
        data$ratio_contribution_start <= 0 |
          data$ratio_contribution_end <= 0
      )
  ) {
    cli::cli_abort(
      "LMDI requires positive representable ratio contributions.",
      class = "whep_ratio_zero"
    )
  }
  invisible(data)
}

.ratio_endpoint <- function(data, period, suffix) {
  data |>
    dplyr::filter(.data$.period == period) |>
    dplyr::select(
      ".group",
      numerator = ".numerator",
      denominator = ".denominator"
    ) |>
    dplyr::rename_with(
      ~ stringr::str_c(.x, "_", suffix),
      c("numerator", "denominator")
    )
}

.ratio_decomp_methods <- function(method) {
  if (identical(method, "all")) {
    c("kitagawa", "lmdi", "weights_first", "ratios_first")
  } else {
    method
  }
}

.ratio_decomp_one <- function(method, state) {
  contributions <- .ratio_method_contributions(state$values, method) |>
    dplyr::mutate(
      group_change = .data$ratio_contribution_end -
        .data$ratio_contribution_start,
      group_closure_residual = .data$between_contribution +
        .data$within_contribution -
        .data$group_change
    )
  summary <- .ratio_decomp_summary(contributions, method)
  .validate_ratio_closure(contributions, summary)

  output <- dplyr::bind_rows(
    .ratio_group_output(contributions, method, state),
    .ratio_summary_output(contributions, summary, method, state)
  )
  dplyr::rename(output, !!state$group_name := "group")
}

.ratio_method_contributions <- function(data, method) {
  switch(
    method,
    kitagawa = .ratio_kitagawa(data),
    lmdi = .ratio_lmdi(data),
    weights_first = .ratio_weights_first(data),
    ratios_first = .ratio_ratios_first(data)
  )
}

.ratio_kitagawa <- function(data) {
  data |>
    dplyr::mutate(
      between_contribution = (.data$weight_end - .data$weight_start) *
        (.data$within_ratio_start / 2 + .data$within_ratio_end / 2),
      within_contribution = (.data$within_ratio_end -
        .data$within_ratio_start) *
        (.data$weight_start / 2 + .data$weight_end / 2)
    )
}

.ratio_weights_first <- function(data) {
  data |>
    dplyr::mutate(
      between_contribution = (.data$weight_end - .data$weight_start) *
        .data$within_ratio_start,
      within_contribution = .data$weight_end *
        (.data$within_ratio_end - .data$within_ratio_start)
    )
}

.ratio_ratios_first <- function(data) {
  data |>
    dplyr::mutate(
      between_contribution = (.data$weight_end - .data$weight_start) *
        .data$within_ratio_end,
      within_contribution = .data$weight_start *
        (.data$within_ratio_end - .data$within_ratio_start)
    )
}

.ratio_lmdi <- function(data) {
  log_mean <- .ratio_log_mean(
    data$ratio_contribution_end,
    data$ratio_contribution_start
  )
  data |>
    dplyr::mutate(
      between_contribution = log_mean *
        (log(.data$weight_end) - log(.data$weight_start)),
      within_contribution = log_mean *
        (log(.data$within_ratio_end) - log(.data$within_ratio_start))
    )
}

.ratio_log_mean <- function(value_end, value_start) {
  difference <- value_end - value_start
  changed <- difference != 0
  near <- changed &
    abs(difference) <= sqrt(.Machine$double.eps) * pmax(value_end, value_start)
  far <- changed & !near
  result <- value_start
  result[near] <- difference[near] /
    log1p(difference[near] / value_start[near])
  result[far] <- difference[far] /
    (log(value_end[far]) - log(value_start[far]))
  result
}

.ratio_decomp_summary <- function(data, method) {
  ratio_start <- sum(data$ratio_contribution_start)
  ratio_end <- sum(data$ratio_contribution_end)
  between <- sum(data$between_contribution)
  within <- sum(data$within_contribution)
  total_change <- ratio_end - ratio_start
  gross_group <- sum(abs(data$between_contribution)) +
    sum(abs(data$within_contribution))
  gross_effect <- abs(between) + abs(within)
  tolerance <- .ratio_scaled_tolerance(
    ratio_start,
    ratio_end,
    gross_group
  )
  near_zero <- abs(total_change) <=
    .ratio_scaled_tolerance(
      ratio_start,
      ratio_end
    )

  tibble::tibble(
    method = method,
    global_ratio_start = ratio_start,
    global_ratio_end = ratio_end,
    total_change = total_change,
    between_contribution = between,
    within_contribution = within,
    between_share_pct = .ratio_share(between, total_change, near_zero),
    within_share_pct = .ratio_share(within, total_change, near_zero),
    closure_residual = between + within - total_change,
    closure_tolerance = tolerance,
    max_group_closure_residual = max(abs(data$group_closure_residual)),
    gross_effect_contribution = gross_effect,
    cancellation_index = .ratio_cancellation(gross_effect, total_change),
    effect_opposition = .ratio_effect_opposition(
      between,
      within,
      .ratio_scaled_tolerance(between, within)
    ),
    group_count = nrow(data),
    net_change_near_zero = near_zero
  )
}

.ratio_share <- function(contribution, total_change, near_zero) {
  if (near_zero) NA_real_ else contribution / total_change * 100
}

.ratio_cancellation <- function(gross, total_change) {
  if (gross == 0) 0 else max(0, 1 - abs(total_change) / gross)
}

.ratio_effect_opposition <- function(between, within, tolerance) {
  if (abs(between) <= tolerance || abs(within) <= tolerance) {
    FALSE
  } else {
    sign(between) != sign(within)
  }
}

.ratio_scaled_tolerance <- function(...) {
  sqrt(.Machine$double.eps) * max(abs(c(...)))
}

.validate_ratio_closure <- function(data, summary) {
  finite_summary <- summary[c(
    "global_ratio_start",
    "global_ratio_end",
    "total_change",
    "between_contribution",
    "within_contribution",
    "closure_residual",
    "closure_tolerance",
    "max_group_closure_residual",
    "gross_effect_contribution",
    "cancellation_index"
  )]
  computed <- c(
    data$between_contribution,
    data$within_contribution,
    data$group_change,
    data$group_closure_residual,
    unlist(finite_summary, use.names = FALSE),
    stats::na.omit(c(
      summary$between_share_pct,
      summary$within_share_pct
    ))
  )
  if (any(!is.finite(computed))) {
    cli::cli_abort(
      "Weighted-ratio decomposition produced non-finite results.",
      class = "whep_ratio_numerical"
    )
  }
  global_failed <- abs(summary$closure_residual) > summary$closure_tolerance
  group_tolerance <- sqrt(.Machine$double.eps) *
    pmax(
      abs(data$group_change),
      abs(data$between_contribution),
      abs(data$within_contribution)
    )
  group_failed <- any(abs(data$group_closure_residual) > group_tolerance)
  if (global_failed || group_failed) {
    cli::cli_abort(
      "Weighted-ratio decomposition did not close within numerical tolerance.",
      class = "whep_ratio_closure"
    )
  }
  invisible(summary)
}

.ratio_group_output <- function(data, method, state) {
  tibble::tibble(
    method = method,
    component_type = "group",
    group = data$.group,
    period_start = state$periods[1],
    period_end = state$periods[2],
    ratio_expression = state$ratio_expression,
    numerator_start = data$numerator_start,
    numerator_end = data$numerator_end,
    denominator_start = data$denominator_start,
    denominator_end = data$denominator_end,
    weight_start = data$weight_start,
    weight_end = data$weight_end,
    within_ratio_start = data$within_ratio_start,
    within_ratio_end = data$within_ratio_end,
    ratio_contribution_start = data$ratio_contribution_start,
    ratio_contribution_end = data$ratio_contribution_end,
    group_change = data$group_change,
    between_contribution = data$between_contribution,
    within_contribution = data$within_contribution,
    group_closure_residual = data$group_closure_residual,
    global_ratio_start = NA_real_,
    global_ratio_end = NA_real_,
    total_change = NA_real_,
    between_share_pct = NA_real_,
    within_share_pct = NA_real_,
    closure_residual = NA_real_,
    closure_tolerance = NA_real_,
    max_group_closure_residual = NA_real_,
    gross_effect_contribution = NA_real_,
    cancellation_index = NA_real_,
    effect_opposition = NA,
    group_count = NA_integer_,
    net_change_near_zero = NA,
    support_policy = "identical_positive_denominator",
    zero_policy = .ratio_zero_policy(method),
    status = "ok"
  )
}

.ratio_summary_output <- function(data, summary, method, state) {
  tibble::tibble(
    method = method,
    component_type = "summary",
    group = data$.group[NA_integer_],
    period_start = state$periods[1],
    period_end = state$periods[2],
    ratio_expression = state$ratio_expression,
    numerator_start = NA_real_,
    numerator_end = NA_real_,
    denominator_start = NA_real_,
    denominator_end = NA_real_,
    weight_start = NA_real_,
    weight_end = NA_real_,
    within_ratio_start = NA_real_,
    within_ratio_end = NA_real_,
    ratio_contribution_start = NA_real_,
    ratio_contribution_end = NA_real_,
    group_change = NA_real_,
    between_contribution = summary$between_contribution,
    within_contribution = summary$within_contribution,
    group_closure_residual = NA_real_,
    global_ratio_start = summary$global_ratio_start,
    global_ratio_end = summary$global_ratio_end,
    total_change = summary$total_change,
    between_share_pct = summary$between_share_pct,
    within_share_pct = summary$within_share_pct,
    closure_residual = summary$closure_residual,
    closure_tolerance = summary$closure_tolerance,
    max_group_closure_residual = summary$max_group_closure_residual,
    gross_effect_contribution = summary$gross_effect_contribution,
    cancellation_index = summary$cancellation_index,
    effect_opposition = summary$effect_opposition,
    group_count = summary$group_count,
    net_change_near_zero = summary$net_change_near_zero,
    support_policy = "identical_positive_denominator",
    zero_policy = .ratio_zero_policy(method),
    status = "ok"
  )
}

.ratio_zero_policy <- function(method) {
  if (identical(method, "lmdi")) {
    "strictly_positive"
  } else {
    "zero_numerator_allowed"
  }
}
