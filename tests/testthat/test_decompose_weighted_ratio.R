ratio_decomp_fixture <- function() {
  tibble::tribble(
    ~year, ~taxon, ~numerator, ~denominator,
    2000, "a", 400, 40,
    2000, "b", 1200, 60,
    2020, "a", 600, 50,
    2020, "b", 900, 50
  )
}

ratio_summary_rows <- function(result, method = NULL) {
  summary <- result |>
    dplyr::filter(.data$component_type == "summary")
  if (!is.null(method)) {
    summary <- summary |>
      dplyr::filter(.data$method == .env$method)
  }
  summary
}

ratio_group_rows <- function(result, method = NULL) {
  groups <- result |>
    dplyr::filter(.data$component_type == "group")
  if (!is.null(method)) {
    groups <- groups |>
      dplyr::filter(.data$method == .env$method)
  }
  groups
}

test_that("weighted-ratio methods reproduce the analytic fixture", {
  result <- whep::decompose_weighted_ratio(
    ratio_decomp_fixture(),
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  summary <- ratio_summary_rows(result)

  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 12L)
  expect_identical(
    summary$method,
    c("kitagawa", "lmdi", "weights_first", "ratios_first")
  )
  expect_equal(summary$global_ratio_start, rep(16, 4))
  expect_equal(summary$global_ratio_end, rep(15, 4))
  expect_equal(summary$total_change, rep(-1, 4))
  expect_equal(
    summary$between_contribution,
    c(-0.8, -0.80060231045881, -1, -0.6),
    tolerance = 1e-12
  )
  expect_equal(
    summary$within_contribution,
    c(-0.2, -0.19939768954119, 0, -0.4),
    tolerance = 1e-12
  )
  expect_lt(max(abs(summary$closure_residual)), 1e-12)
  expect_equal(summary$group_count, rep(2L, 4))
  expect_true(all(summary$status == "ok"))
})

test_that("output schema separates group and summary grain", {
  result <- whep::decompose_weighted_ratio(
    ratio_decomp_fixture(),
    year,
    taxon,
    numerator / denominator
  )
  expected_names <- c(
    "method",
    "component_type",
    "taxon",
    "period_start",
    "period_end",
    "ratio_expression",
    "numerator_start",
    "numerator_end",
    "denominator_start",
    "denominator_end",
    "weight_start",
    "weight_end",
    "within_ratio_start",
    "within_ratio_end",
    "ratio_contribution_start",
    "ratio_contribution_end",
    "group_change",
    "between_contribution",
    "within_contribution",
    "group_closure_residual",
    "global_ratio_start",
    "global_ratio_end",
    "total_change",
    "between_share_pct",
    "within_share_pct",
    "closure_residual",
    "closure_tolerance",
    "max_group_closure_residual",
    "gross_effect_contribution",
    "cancellation_index",
    "effect_opposition",
    "group_count",
    "net_change_near_zero",
    "support_policy",
    "zero_policy",
    "status"
  )

  expect_identical(names(result), expected_names)
  expect_identical(result$component_type, c("group", "group", "summary"))
  expect_identical(result$taxon, c("a", "b", NA_character_))
  expect_true(all(is.na(ratio_group_rows(result)$global_ratio_start)))
  expect_true(all(is.na(ratio_summary_rows(result)$weight_start)))
  expect_identical(unique(result$ratio_expression), "numerator / denominator")
  expect_identical(
    unique(result$support_policy),
    "identical_positive_denominator"
  )
})

test_that("Kitagawa gives exact group effects and polar averages", {
  result <- whep::decompose_weighted_ratio(
    ratio_decomp_fixture(),
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  kitagawa <- ratio_group_rows(result, "kitagawa")
  lmdi <- ratio_group_rows(result, "lmdi")
  weights_first <- ratio_group_rows(result, "weights_first")
  ratios_first <- ratio_group_rows(result, "ratios_first")

  expect_equal(kitagawa$between_contribution, c(1.1, -1.9))
  expect_equal(kitagawa$within_contribution, c(0.9, -1.1))
  expect_equal(kitagawa$group_change, c(2, -3))
  expect_lt(max(abs(kitagawa$group_closure_residual)), 1e-12)
  expect_equal(
    lmdi$between_contribution,
    c(1.1006794264264168, -1.9012817368852277),
    tolerance = 1e-12
  )
  expect_equal(
    lmdi$within_contribution,
    c(0.8993205735735822, -1.0987182631147736),
    tolerance = 1e-12
  )
  expect_equal(
    kitagawa$between_contribution,
    (weights_first$between_contribution +
      ratios_first$between_contribution) /
      2
  )
  expect_equal(
    kitagawa$within_contribution,
    (weights_first$within_contribution +
      ratios_first$within_contribution) /
      2
  )
})

test_that("symmetric methods reverse and polar paths cross-reverse", {
  forward <- whep::decompose_weighted_ratio(
    ratio_decomp_fixture(),
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  reversed_data <- ratio_decomp_fixture() |>
    dplyr::mutate(year = dplyr::if_else(.data$year == 2000, 2020, 2000))
  reversed <- whep::decompose_weighted_ratio(
    reversed_data,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )

  purrr::walk(c("kitagawa", "lmdi"), function(method) {
    x <- ratio_group_rows(forward, method)
    y <- ratio_group_rows(reversed, method)
    expect_equal(y$between_contribution, -x$between_contribution)
    expect_equal(y$within_contribution, -x$within_contribution)
  })
  expect_equal(
    ratio_group_rows(reversed, "weights_first")$between_contribution,
    -ratio_group_rows(forward, "ratios_first")$between_contribution
  )
  expect_equal(
    ratio_group_rows(reversed, "weights_first")$within_contribution,
    -ratio_group_rows(forward, "ratios_first")$within_contribution
  )
})

test_that("unchanged factors receive zero contributions", {
  same_weights <- tibble::tribble(
    ~year, ~taxon, ~numerator, ~denominator,
    2000, "a", 400, 40,
    2000, "b", 1200, 60,
    2020, "a", 480, 40,
    2020, "b", 1080, 60
  )
  weight_result <- whep::decompose_weighted_ratio(
    same_weights,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  expect_equal(
    ratio_summary_rows(weight_result)$between_contribution,
    rep(0, 4),
    tolerance = 1e-12
  )

  same_ratios <- tibble::tribble(
    ~year, ~taxon, ~numerator, ~denominator,
    2000, "a", 400, 40,
    2000, "b", 1200, 60,
    2020, "a", 500, 50,
    2020, "b", 1000, 50
  )
  ratio_result <- whep::decompose_weighted_ratio(
    same_ratios,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  expect_equal(
    ratio_summary_rows(ratio_result)$within_contribution,
    rep(0, 4),
    tolerance = 1e-12
  )
})

test_that("a single group has only a within-group effect", {
  single_group <- tibble::tribble(
    ~year, ~taxon, ~numerator, ~denominator,
    2000, "a", 10, 2,
    2020, "a", 18, 3
  )
  result <- whep::decompose_weighted_ratio(
    single_group,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  summary <- ratio_summary_rows(result)

  expect_equal(summary$global_ratio_start, rep(5, 4))
  expect_equal(summary$global_ratio_end, rep(6, 4))
  expect_equal(summary$between_contribution, rep(0, 4), tolerance = 1e-12)
  expect_equal(summary$within_contribution, rep(1, 4), tolerance = 1e-12)
  expect_equal(summary$between_share_pct, rep(0, 4), tolerance = 1e-12)
  expect_equal(summary$within_share_pct, rep(100, 4), tolerance = 1e-12)
})

test_that("identical endpoints have zero effects and missing shares", {
  start <- ratio_decomp_fixture() |>
    dplyr::filter(.data$year == 2000)
  identical_data <- dplyr::bind_rows(
    start,
    dplyr::mutate(start, year = 2020)
  )
  result <- whep::decompose_weighted_ratio(
    identical_data,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  summary <- ratio_summary_rows(result)

  expect_equal(summary$total_change, rep(0, 4))
  expect_equal(summary$between_contribution, rep(0, 4))
  expect_equal(summary$within_contribution, rep(0, 4))
  expect_true(all(is.na(summary$between_share_pct)))
  expect_true(all(is.na(summary$within_share_pct)))
  expect_true(all(summary$net_change_near_zero))
  expect_equal(summary$cancellation_index, rep(0, 4))
  expect_false(any(summary$effect_opposition))
})

test_that("results are invariant to rows and equivariant to ratio scale", {
  original <- ratio_decomp_fixture()
  shuffled <- original[c(4, 1, 3, 2), ]
  reference <- whep::decompose_weighted_ratio(
    original,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  reordered <- whep::decompose_weighted_ratio(
    shuffled,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  expect_equal(reordered, reference)
  expect_identical(original, ratio_decomp_fixture())

  scaled <- original |>
    dplyr::mutate(numerator = .data$numerator * 7)
  scaled_result <- whep::decompose_weighted_ratio(
    scaled,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  absolute_columns <- c(
    "between_contribution",
    "within_contribution",
    "total_change",
    "global_ratio_start",
    "global_ratio_end"
  )
  expect_equal(
    unname(as.matrix(ratio_summary_rows(scaled_result)[absolute_columns])),
    unname(as.matrix(ratio_summary_rows(reference)[absolute_columns])) * 7,
    tolerance = 1e-10
  )
  expect_equal(
    ratio_summary_rows(scaled_result)$between_share_pct,
    ratio_summary_rows(reference)$between_share_pct
  )

  small_scaled <- original |>
    dplyr::mutate(numerator = .data$numerator * 1e-12)
  small_summary <- whep::decompose_weighted_ratio(
    small_scaled,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  ) |>
    ratio_summary_rows()
  reference_summary <- ratio_summary_rows(reference)
  expect_equal(
    small_summary$between_share_pct,
    reference_summary$between_share_pct,
    tolerance = 1e-10
  )
  expect_equal(
    small_summary$within_share_pct,
    reference_summary$within_share_pct,
    tolerance = 1e-10
  )
  expect_false(any(small_summary$net_change_near_zero))
})

test_that("opposing effects remain signed and cancellation is diagnosed", {
  opposing <- tibble::tribble(
    ~year, ~taxon, ~numerator, ~denominator,
    2000, "a", 500, 50,
    2000, "b", 1000, 50,
    2020, "a", 630, 60,
    2020, "b", 820, 40
  )
  result <- whep::decompose_weighted_ratio(
    opposing,
    year,
    taxon,
    numerator / denominator
  )
  summary <- ratio_summary_rows(result)

  expect_equal(summary$total_change, -0.5)
  expect_equal(summary$between_contribution, -1)
  expect_equal(summary$within_contribution, 0.5)
  expect_equal(summary$between_share_pct, 200)
  expect_equal(summary$within_share_pct, -100)
  expect_true(summary$effect_opposition)
  expect_equal(summary$cancellation_index, 2 / 3)

  zero_net <- opposing |>
    dplyr::mutate(
      numerator = dplyr::case_when(
        .data$year == 2020 & .data$taxon == "a" ~ 660,
        .data$year == 2020 & .data$taxon == "b" ~ 840,
        TRUE ~ .data$numerator
      )
    )
  zero_summary <- whep::decompose_weighted_ratio(
    zero_net,
    year,
    taxon,
    numerator / denominator
  ) |>
    ratio_summary_rows()
  expect_true(zero_summary$net_change_near_zero)
  expect_true(is.na(zero_summary$between_share_pct))
  expect_true(is.na(zero_summary$within_share_pct))
  expect_equal(zero_summary$cancellation_index, 1)
})

test_that("proportional group splitting preserves aggregate effects", {
  base <- ratio_decomp_fixture()
  split_a <- base |>
    dplyr::filter(.data$taxon == "a") |>
    tidyr::crossing(part = c("1", "2")) |>
    dplyr::mutate(
      share = dplyr::if_else(.data$part == "1", 0.3, 0.7),
      taxon = stringr::str_c(.data$taxon, .data$part),
      numerator = .data$numerator * .data$share,
      denominator = .data$denominator * .data$share
    ) |>
    dplyr::select(-"part", -"share")
  split_data <- base |>
    dplyr::filter(.data$taxon == "b") |>
    dplyr::bind_rows(split_a)
  base_summary <- whep::decompose_weighted_ratio(
    base,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  ) |>
    ratio_summary_rows()
  split_summary <- whep::decompose_weighted_ratio(
    split_data,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  ) |>
    ratio_summary_rows()
  fields <- c(
    "global_ratio_start",
    "global_ratio_end",
    "total_change",
    "between_contribution",
    "within_contribution"
  )

  expect_equal(split_summary[fields], base_summary[fields], tolerance = 1e-12)
})

test_that("zero numerator policy is method-specific and explicit", {
  zero_numerator <- ratio_decomp_fixture()
  zero_numerator$numerator[1] <- 0

  purrr::walk(
    c("kitagawa", "weights_first", "ratios_first"),
    function(method) {
      result <- whep::decompose_weighted_ratio(
        zero_numerator,
        year,
        taxon,
        numerator / denominator,
        method = method
      )
      expect_identical(unique(result$zero_policy), "zero_numerator_allowed")
      expect_lt(
        abs(ratio_summary_rows(result)$closure_residual),
        1e-12
      )
    }
  )
  expect_error(
    whep::decompose_weighted_ratio(
      zero_numerator,
      year,
      taxon,
      numerator / denominator,
      method = "lmdi"
    ),
    class = "whep_ratio_zero"
  )
  expect_error(
    whep::decompose_weighted_ratio(
      zero_numerator,
      year,
      taxon,
      numerator / denominator,
      method = "all"
    ),
    class = "whep_ratio_zero"
  )

  zero_at_end <- ratio_decomp_fixture()
  zero_at_end$numerator[3] <- 0
  end_result <- whep::decompose_weighted_ratio(
    zero_at_end,
    year,
    taxon,
    numerator / denominator
  )
  expect_lt(abs(ratio_summary_rows(end_result)$closure_residual), 1e-12)
  expect_error(
    whep::decompose_weighted_ratio(
      zero_at_end,
      year,
      taxon,
      numerator / denominator,
      method = "lmdi"
    ),
    class = "whep_ratio_zero"
  )
})

test_that("invalid values fail closed", {
  denominator_zero <- ratio_decomp_fixture()
  denominator_zero$denominator[1] <- 0
  expect_error(
    whep::decompose_weighted_ratio(
      denominator_zero,
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_domain"
  )

  negative <- ratio_decomp_fixture()
  negative$numerator[1] <- -1
  expect_error(
    whep::decompose_weighted_ratio(
      negative,
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_domain"
  )

  purrr::walk(c(NA_real_, Inf), function(value) {
    invalid <- ratio_decomp_fixture()
    invalid$numerator[1] <- value
    expect_error(
      whep::decompose_weighted_ratio(
        invalid,
        year,
        taxon,
        numerator / denominator
      ),
      class = "whep_ratio_input"
    )
  })

  nonnumeric <- ratio_decomp_fixture() |>
    dplyr::mutate(numerator = as.character(.data$numerator))
  expect_error(
    whep::decompose_weighted_ratio(
      nonnumeric,
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_input"
  )
})

test_that("duplicate and changing support fail closed", {
  fixture <- ratio_decomp_fixture()
  duplicated <- dplyr::bind_rows(fixture, fixture[1, ])
  expect_error(
    whep::decompose_weighted_ratio(
      duplicated,
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_keys"
  )

  changed_support <- fixture[-4, ]
  expect_error(
    whep::decompose_weighted_ratio(
      changed_support,
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_support"
  )

  missing_group <- fixture
  missing_group$taxon[1] <- NA_character_
  expect_error(
    whep::decompose_weighted_ratio(
      missing_group,
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_input"
  )
})

test_that("period validation is explicit and ordering is deterministic", {
  one_period <- ratio_decomp_fixture() |>
    dplyr::filter(.data$year == 2000)
  expect_error(
    whep::decompose_weighted_ratio(
      one_period,
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_periods"
  )

  three_periods <- dplyr::bind_rows(
    ratio_decomp_fixture(),
    ratio_decomp_fixture() |>
      dplyr::filter(.data$year == 2000) |>
      dplyr::mutate(year = 2040)
  )
  expect_error(
    whep::decompose_weighted_ratio(
      three_periods,
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_periods"
  )

  character_time <- ratio_decomp_fixture() |>
    dplyr::mutate(year = as.character(.data$year))
  expect_error(
    whep::decompose_weighted_ratio(
      character_time,
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_periods"
  )

  ordered_time <- ratio_decomp_fixture() |>
    dplyr::mutate(
      year = factor(
        dplyr::if_else(.data$year == 2000, "before", "after"),
        levels = c("before", "after"),
        ordered = TRUE
      )
    )
  ordered_result <- whep::decompose_weighted_ratio(
    ordered_time,
    year,
    taxon,
    numerator / denominator
  )
  expect_identical(as.character(ordered_result$period_start), rep("before", 3))
  expect_identical(as.character(ordered_result$period_end), rep("after", 3))

  date_time <- ratio_decomp_fixture() |>
    dplyr::mutate(
      year = as.Date(dplyr::if_else(
        .data$year == 2000,
        "2000-01-01",
        "2020-01-01"
      ))
    )
  date_result <- whep::decompose_weighted_ratio(
    date_time,
    year,
    taxon,
    numerator / denominator
  )
  expect_s3_class(date_result$period_start, "Date")

  posix_time <- date_time |>
    dplyr::mutate(year = as.POSIXct(.data$year, tz = "UTC"))
  posix_result <- whep::decompose_weighted_ratio(
    posix_time,
    year,
    taxon,
    numerator / denominator
  )
  expect_s3_class(posix_result$period_start, "POSIXct")
})

test_that("column and identity validation reject ambiguous calls", {
  fixture <- ratio_decomp_fixture()
  expect_error(
    whep::decompose_weighted_ratio(
      as.data.frame(fixture),
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_input"
  )
  expect_error(
    whep::decompose_weighted_ratio(
      fixture,
      year + 0,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_input"
  )
  expect_error(
    whep::decompose_weighted_ratio(
      fixture,
      year,
      taxon,
      numerator
    ),
    class = "whep_ratio_input"
  )
  expect_error(
    whep::decompose_weighted_ratio(
      fixture,
      year,
      taxon,
      numerator / (denominator * 1)
    ),
    class = "whep_ratio_input"
  )
  expect_error(
    whep::decompose_weighted_ratio(
      fixture,
      year,
      taxon,
      missing_numerator / denominator
    ),
    class = "whep_ratio_input"
  )
  expect_error(
    whep::decompose_weighted_ratio(
      fixture,
      year,
      taxon,
      numerator / numerator
    ),
    class = "whep_ratio_input"
  )
  expect_error(
    whep::decompose_weighted_ratio(
      fixture,
      year,
      taxon,
      numerator / denominator,
      method = "unknown"
    )
  )
})

test_that("large opposing terms still satisfy scaled closure", {
  extreme <- tibble::tribble(
    ~year, ~taxon, ~numerator, ~denominator,
    2000, "a", 1e12, 1,
    2000, "b", 1, 1e12,
    2020, "a", 1, 1e12,
    2020, "b", 1e12, 1
  )
  result <- whep::decompose_weighted_ratio(
    extreme,
    year,
    taxon,
    numerator / denominator,
    method = "all"
  )
  summary <- ratio_summary_rows(result)

  expect_true(all(abs(summary$closure_residual) <= summary$closure_tolerance))
  expect_true(all(
    summary$max_group_closure_residual <= summary$closure_tolerance
  ))
  expect_true(all(is.na(summary$between_share_pct)))
})

test_that("near-overflow Kitagawa means remain finite", {
  near_limit <- tibble::tribble(
    ~year, ~taxon, ~numerator, ~denominator,
    2000, "a", 1e308, 1,
    2000, "b", 1e308, 1,
    2020, "a", 1.2e308, 1.2,
    2020, "b", 0.8e308, 0.8
  )
  result <- whep::decompose_weighted_ratio(
    near_limit,
    year,
    taxon,
    numerator / denominator
  )
  numeric_result <- result[vapply(result, is.numeric, logical(1))]

  expect_true(all(vapply(
    numeric_result,
    function(x) {
      all(is.finite(x) | is.na(x))
    },
    logical(1)
  )))
  expect_identical(unique(result$status), "ok")
})

test_that("large finite effects yield finite percentage shares", {
  large_effects <- tibble::tribble(
    ~year, ~taxon, ~numerator, ~denominator,
    2000, "a", 3.6e307, 0.4,
    2000, "b", 4.8e307, 0.6,
    2020, "a", 5.0e307, 0.5,
    2020, "b", 3.5e307, 0.5
  )
  summary <- whep::decompose_weighted_ratio(
    large_effects,
    year,
    taxon,
    numerator / denominator
  ) |>
    ratio_summary_rows()

  expect_equal(summary$between_share_pct, 200, tolerance = 1e-12)
  expect_equal(summary$within_share_pct, -100, tolerance = 1e-12)
})

test_that("LMDI handles subnormal weight ratios without quotient overflow", {
  subnormal <- tibble::tribble(
    ~year, ~taxon, ~numerator, ~denominator,
    2000, "a", 1e-310, 1e-310,
    2000, "b", 1, 1,
    2020, "a", 1, 1,
    2020, "b", 1, 1
  )
  result <- whep::decompose_weighted_ratio(
    subnormal,
    year,
    taxon,
    numerator / denominator,
    method = "lmdi"
  )

  expect_true(all(is.finite(result$between_contribution)))
  expect_true(all(is.finite(result$within_contribution)))
  expect_identical(unique(result$status), "ok")
})

test_that("non-finite computed diagnostics fail closed", {
  overflowing_gross <- tibble::tribble(
    ~year, ~taxon, ~numerator, ~denominator,
    2000, "a", 0.017, 1e-310,
    2000, "b", 1.7e308, 1,
    2020, "a", 1.7e308, 1,
    2020, "b", 0.017, 1e-310
  )

  expect_error(
    whep::decompose_weighted_ratio(
      overflowing_gross,
      year,
      taxon,
      numerator / denominator
    ),
    class = "whep_ratio_numerical"
  )
})
