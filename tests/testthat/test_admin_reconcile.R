# Tests for R/admin_reconcile.R (#1000, T14).
#
# Every fixture here is hand-built: the reconciliation reads the tables
# `allocate_level_crops()` RETURNS, so a report can be built from a stored
# coverage row without re-running an engine, and the tests say so by never
# needing one. The three end-to-end cases that do call the allocator are
# marked; they exist because the machine criterion is about what the
# allocation does, not only about what the report prints.

.t14_coverage <- function(
  national = 250,
  admin_sum = 225,
  basis = "admin_sum",
  n_units = 2L,
  n_units_reporting = 2L
) {
  tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_prod_code = 15L,
    n_units = n_units,
    n_units_reporting = n_units_reporting,
    coverage = n_units_reporting / n_units,
    basis = basis,
    national_total_ha = national,
    admin_sum = admin_sum
  )
}

.t14_targets <- function(
  share = c(0.6, 0.4),
  national = 250,
  clipped = 0,
  units = c("A1", "A2")
) {
  tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = units,
    item_prod_code = 15L,
    share = share,
    target_ha = share * national,
    irrigated_target_ha = 0,
    rainfed_target_ha = share * national,
    irrigation_clipped_ha = clipped,
    method_crop_alloc = "admin_area_shares",
    national_total_ha = national
  )
}

.t14_breach <- function() {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    level_polity_code = character(),
    item_prod_code = integer(),
    mc_basis = character(),
    in_force = logical(),
    over_ha = numeric()
  )
}

.t14_allocation <- function(
  coverage = .t14_coverage(),
  targets = .t14_targets(),
  breach = .t14_breach()
) {
  list(coverage = coverage, targets = targets, breach = breach)
}

.t14_shares <- function(
  indicator = "area_harvested",
  value = c(135, 90),
  units = c("A1", "A2"),
  year = 2000L
) {
  tibble::tibble(
    area_code = 1L,
    level_polity_code = units,
    level = 1L,
    item_prod_code = 15L,
    indicator_used = indicator,
    year = year,
    value = value,
    share = NA_real_,
    treatment = "observed"
  )
}

# --- The reported group table ------------------------------------------------

testthat::test_that("the group report carries every locked column", {
  out <- reconcile_admin_allocation(.t14_allocation(), .t14_shares())
  testthat::expect_true(all(
    c(
      "national_total",
      "admin_sum",
      "n_units_reporting",
      "n_units_valid",
      "coverage",
      "residual_target",
      "discrepancy",
      "discrepancy_frac",
      "indicator"
    ) %in%
      names(out$groups)
  ))
  testthat::expect_equal(nrow(out$groups), 1L)
  testthat::expect_equal(out$groups$indicator, "area_harvested")
  testthat::expect_equal(out$groups$n_units_valid, 2L)
  testthat::expect_equal(out$groups$national_total, 250)
  testthat::expect_equal(out$groups$admin_sum, 225)
  testthat::expect_equal(out$groups$discrepancy, 25)
  testthat::expect_equal(out$groups$discrepancy_frac, 0.1)
  # Complete coverage rescales; there is no residual pseudo-unit, so the
  # residual target is NOT zero -- it does not exist (decision T31(a)).
  testthat::expect_true(is.na(out$groups$residual_target))
})

testthat::test_that("the residual target appears only under partial cover", {
  out <- reconcile_admin_allocation(
    .t14_allocation(
      coverage = .t14_coverage(
        national = 250,
        admin_sum = 150,
        basis = "residual",
        n_units = 3L,
        n_units_reporting = 2L
      )
    ),
    .t14_shares()
  )
  testthat::expect_equal(out$groups$residual_target, 100)
  testthat::expect_equal(out$groups$discrepancy, 100)
  # `n_units_valid` is the layer's valid units -- the denominator the
  # coverage decision used -- and NOT the reporting count beside it.
  testthat::expect_equal(out$groups$n_units_valid, 3L)
  testthat::expect_equal(out$groups$n_units_reporting, 2L)
  testthat::expect_false(out$groups$coverage_complete)
  testthat::expect_equal(out$groups$coverage, 2 / 3)
})

testthat::test_that("an over-reporting residual group floors at zero", {
  # `max(national - admin_sum, 0)`: the reported units already exceed the
  # national total, so the residual pseudo-unit carries nothing while the
  # discrepancy keeps its sign and stays visible.
  out <- reconcile_admin_allocation(
    .t14_allocation(
      coverage = .t14_coverage(
        national = 250,
        admin_sum = 400,
        basis = "residual",
        n_units = 3L,
        n_units_reporting = 2L
      )
    ),
    .t14_shares()
  )
  testthat::expect_equal(out$groups$residual_target, 0)
  testthat::expect_equal(out$groups$discrepancy, -150)
})

testthat::test_that("a zero national total gives NA fraction, real absolute", {
  out <- reconcile_admin_allocation(
    .t14_allocation(coverage = .t14_coverage(national = 0, admin_sum = 40)),
    .t14_shares()
  )
  testthat::expect_true(is.na(out$groups$discrepancy_frac))
  testthat::expect_equal(out$groups$discrepancy, -40)
  # An unevaluable fraction cannot be breached, so the group is not refused
  # however large the absolute discrepancy is.
  testthat::expect_false(out$groups$beyond_tolerance)
})

testthat::test_that("a shares-only group has no admin sum to compare", {
  # `.alloc_coverage()` reports `admin_sum = 0` for a group where no unit
  # gave an absolute area, because the sum of nothing is zero. Read as a
  # discrepancy that is 100% of the national total, and both tolerances
  # would be breached on every consented shares-only family in the pin.
  # Nothing was reported in hectares, so there is nothing to compare.
  out <- reconcile_admin_allocation(
    .t14_allocation(
      coverage = .t14_coverage(
        national = 1e6,
        admin_sum = 0,
        basis = "share_normalised"
      )
    ),
    .t14_shares(value = c(NA_real_, NA_real_))
  )
  testthat::expect_true(is.na(out$groups$admin_sum))
  testthat::expect_true(is.na(out$groups$discrepancy))
  testthat::expect_true(is.na(out$groups$discrepancy_frac))
  testthat::expect_false(out$groups$beyond_tolerance)
})

testthat::test_that("a group with no admin row at all reports no indicator", {
  out <- reconcile_admin_allocation(
    .t14_allocation(
      coverage = .t14_coverage(national = 250, admin_sum = 0, basis = "pattern")
    ),
    admin_shares = NULL
  )
  testthat::expect_true(is.na(out$groups$indicator))
  testthat::expect_true(is.na(out$groups$admin_sum))
  testthat::expect_equal(nrow(out$bridges), 0L)
})

testthat::test_that("a basis with no hectares is not complete coverage", {
  # `coverage_complete` is the refusal gate: `.recon_refuse()` reads only
  # the groups it marks. A pattern group had no unit report at all and a
  # share-normalised group had none report in hectares, so neither has two
  # sides to compare; reading "not residual" as complete would gate them
  # exactly as a group every unit reported in hectares is gated.
  pattern <- reconcile_admin_allocation(
    .t14_allocation(
      coverage = .t14_coverage(national = 250, admin_sum = 0, basis = "pattern")
    ),
    admin_shares = NULL
  )
  testthat::expect_false(pattern$groups$coverage_complete)
  normalised <- reconcile_admin_allocation(
    .t14_allocation(
      coverage = .t14_coverage(
        national = 1e6,
        admin_sum = 0,
        basis = "share_normalised"
      )
    ),
    .t14_shares(value = c(NA_real_, NA_real_))
  )
  testthat::expect_false(normalised$groups$coverage_complete)
  # The hectare basis is what the column marks, and nothing else.
  hectares <- reconcile_admin_allocation(.t14_allocation(), .t14_shares())
  testthat::expect_true(hectares$groups$coverage_complete)
})

testthat::test_that("a group mixing indicators says so and is counted", {
  shares <- dplyr::bind_rows(
    .t14_shares(indicator = "area_harvested", value = 135, units = "A1"),
    .t14_shares(indicator = "area_planted_or_sown", value = 90, units = "A2")
  )
  out <- NULL
  testthat::expect_warning(
    out <- reconcile_admin_allocation(.t14_allocation(), shares),
    "more than one area indicator"
  )
  testthat::expect_equal(out$groups$indicator, "mixed")
  testthat::expect_equal(out$groups$n_indicators, 2L)
})

# --- Refusal -----------------------------------------------------------------

testthat::test_that("both tolerances breached under full cover refuses", {
  testthat::expect_error(
    reconcile_admin_allocation(
      .t14_allocation(
        coverage = .t14_coverage(national = 1e6, admin_sum = 5e5)
      ),
      .t14_shares()
    ),
    class = "whep_recon_admin_discrepancy"
  )
})

testthat::test_that("the refusal quotes the worst group, not the mildest", {
  # Two groups breach both tolerances and the message quotes one of them.
  # Quoting the milder would send the reader to the lesser of two
  # problems, and the number they would then go and check is the wrong
  # one. Item 27 is off by 9,000 ha against item 15's 2,000, so the
  # national total the message must carry is 12,000.
  coverage <- dplyr::bind_rows(
    .t14_coverage(national = 3000, admin_sum = 1000),
    .t14_coverage(national = 12000, admin_sum = 3000) |>
      dplyr::mutate(item_prod_code = 27L)
  )
  targets <- dplyr::bind_rows(
    .t14_targets(),
    .t14_targets() |> dplyr::mutate(item_prod_code = 27L)
  )
  testthat::expect_error(
    reconcile_admin_allocation(
      .t14_allocation(coverage = coverage, targets = targets),
      admin_shares = NULL
    ),
    regexp = "12000",
    class = "whep_recon_admin_discrepancy"
  )
})

testthat::test_that("past the fraction but under the floor does not refuse", {
  # 20% of 1000 ha is 200 ha: the relative tolerance is breached and the
  # absolute floor is not, so decision T31(d) reports rather than refuses.
  out <- reconcile_admin_allocation(
    .t14_allocation(coverage = .t14_coverage(national = 1000, admin_sum = 800)),
    .t14_shares()
  )
  testthat::expect_equal(out$groups$discrepancy_frac, 0.2)
  testthat::expect_equal(out$groups$discrepancy, 200)
  testthat::expect_false(out$groups$beyond_tolerance)
})

testthat::test_that("past the floor but under the fraction does not refuse", {
  out <- reconcile_admin_allocation(
    .t14_allocation(
      coverage = .t14_coverage(national = 1e6, admin_sum = 9.5e5)
    ),
    .t14_shares()
  )
  testthat::expect_equal(out$groups$discrepancy, 5e4)
  testthat::expect_equal(out$groups$discrepancy_frac, 0.05)
  testthat::expect_false(out$groups$beyond_tolerance)
})

testthat::test_that("a discrepancy at the relative tolerance is not past it", {
  # The rule is strict, `|discrepancy_frac| > tolerance_relative`, as the
  # docstring states it. Off by exactly 10% of a million hectares the
  # group is 100,000 ha past the absolute floor, so this boundary alone
  # decides whether the run is refused.
  out <- NULL
  testthat::expect_no_error(
    out <- reconcile_admin_allocation(
      .t14_allocation(
        coverage = .t14_coverage(national = 1e6, admin_sum = 9e5)
      ),
      .t14_shares()
    )
  )
  testthat::expect_true(out$groups$coverage_complete)
  testthat::expect_equal(out$groups$discrepancy_frac, 0.1)
  testthat::expect_equal(out$groups$discrepancy, 1e5)
  testthat::expect_false(out$groups$beyond_tolerance)
})

testthat::test_that("a discrepancy at the absolute floor is not past it", {
  # The sibling boundary: 20% of 5,000 ha is exactly the 1,000 ha floor.
  # The fraction is past its tolerance, so the floor alone decides, and
  # `|discrepancy| > tolerance_absolute` is strict too.
  out <- NULL
  testthat::expect_no_error(
    out <- reconcile_admin_allocation(
      .t14_allocation(
        coverage = .t14_coverage(national = 5000, admin_sum = 4000)
      ),
      .t14_shares()
    )
  )
  testthat::expect_equal(out$groups$discrepancy, 1000)
  testthat::expect_equal(out$groups$discrepancy_frac, 0.2)
  testthat::expect_false(out$groups$beyond_tolerance)
})

testthat::test_that("incomplete coverage never refuses, however large", {
  # The same numbers that refuse above, under partial coverage: the
  # difference is the residual pseudo-unit's own target and is not a
  # discrepancy at all (decision T31(a)). It stays visible as
  # `beyond_tolerance` so the exemption is measured, not hidden.
  out <- reconcile_admin_allocation(
    .t14_allocation(
      coverage = .t14_coverage(
        national = 1e6,
        admin_sum = 1,
        basis = "residual",
        n_units = 3L,
        n_units_reporting = 1L
      )
    ),
    .t14_shares()
  )
  testthat::expect_true(out$groups$beyond_tolerance)
  testthat::expect_false(out$groups$coverage_complete)
  testthat::expect_equal(out$groups$residual_target, 1e6 - 1)
})

testthat::test_that("the tolerances are arguments, not constants", {
  alloc <- .t14_allocation(
    coverage = .t14_coverage(national = 1e6, admin_sum = 9.5e5)
  )
  # Under the locked 10% this group passes (tested above). Tighten the
  # relative tolerance alone and the same group refuses.
  testthat::expect_error(
    reconcile_admin_allocation(alloc, .t14_shares(), tolerance_relative = 0.01),
    class = "whep_recon_admin_discrepancy"
  )
  # Raise the floor past the discrepancy and it passes again.
  testthat::expect_no_error(
    reconcile_admin_allocation(
      alloc,
      .t14_shares(),
      tolerance_relative = 0.01,
      tolerance_absolute = 1e5
    )
  )
})

# --- The production-implied divergence ---------------------------------------

testthat::test_that("production shares diverge from the shares that bind", {
  # Production never anchors (decision T31(i)), so the information it
  # carries has to stay visible somewhere: area 50/50 against production
  # 75/25 is a unit yielding half again the container mean and one at half.
  shares <- dplyr::bind_rows(
    .t14_shares(value = c(125, 125)),
    .t14_shares(indicator = "production", value = c(300, 100))
  )
  out <- reconcile_admin_allocation(
    .t14_allocation(targets = .t14_targets(share = c(0.5, 0.5))),
    shares
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(units$production_share, c(0.75, 0.25))
  testthat::expect_equal(units$area_share_renorm, c(0.5, 0.5))
  testthat::expect_equal(units$share_divergence, c(0.25, -0.25))
  testthat::expect_equal(units$implied_yield_ratio, c(1.5, 0.5))
  testthat::expect_equal(units$production_basis, c("value", "value"))
  testthat::expect_equal(out$groups$production_divergence_tvd, 0.25)
  testthat::expect_equal(out$groups$n_units_production, 2L)
})

testthat::test_that("a declared production share is used where no value is", {
  shares <- dplyr::bind_rows(
    .t14_shares(value = c(125, 125)),
    .t14_shares(indicator = "production", value = c(NA_real_, NA_real_)) |>
      dplyr::mutate(share = c(0.9, 0.1))
  )
  out <- reconcile_admin_allocation(
    .t14_allocation(targets = .t14_targets(share = c(0.5, 0.5))),
    shares
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(units$production_basis, c("declared", "declared"))
  testthat::expect_equal(units$production_share, c(0.9, 0.1))
  testthat::expect_equal(units$implied_yield_ratio, c(1.8, 0.2))
})

testthat::test_that("a measured production value outranks a declared share", {
  # A family shipping BOTH: 300 and 100 tonnes measured, and declared
  # shares of 0.9 and 0.1 that do not agree with them. `production_basis`
  # exists to say which of the two the report read, and reading the
  # declaration over the measurement would make the column a label for a
  # precedence that is not the one in force. The measured 300/400 stands.
  shares <- dplyr::bind_rows(
    .t14_shares(value = c(125, 125)),
    .t14_shares(indicator = "production", value = c(300, 100)) |>
      dplyr::mutate(share = c(0.9, 0.1))
  )
  out <- reconcile_admin_allocation(
    .t14_allocation(targets = .t14_targets(share = c(0.5, 0.5))),
    shares
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(units$production_basis, c("value", "value"))
  testthat::expect_equal(units$production_share, c(0.75, 0.25))
  testthat::expect_equal(units$share_divergence, c(0.25, -0.25))
  testthat::expect_equal(units$implied_yield_ratio, c(1.5, 0.5))
})

testthat::test_that("zero production values fall back to the declared shares", {
  # A production series that is all zeros, shipped beside declared shares
  # of 0.9 and 0.1. A sum of zero is no measurement: dividing by it would
  # make every share `NaN`, and "value" would then name a basis the report
  # could not read. The declared shares are read instead, and the basis
  # says so.
  shares <- dplyr::bind_rows(
    .t14_shares(value = c(125, 125)),
    .t14_shares(indicator = "production", value = c(0, 0)) |>
      dplyr::mutate(share = c(0.9, 0.1))
  )
  out <- reconcile_admin_allocation(
    .t14_allocation(targets = .t14_targets(share = c(0.5, 0.5))),
    shares
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(units$production_basis, c("declared", "declared"))
  testthat::expect_equal(units$production_share, c(0.9, 0.1))
  testthat::expect_equal(units$share_divergence, c(0.4, -0.4))
  testthat::expect_equal(out$groups$production_divergence_tvd, 0.4)
})

testthat::test_that("a production row is not read as an area indicator", {
  # The group reports one area indicator and a production series beside
  # it. Counting the production row as an indicator would mark the group
  # `"mixed"` -- claiming hectares of two different area definitions had
  # been added together -- and warn about a resolution failure that never
  # happened.
  shares <- dplyr::bind_rows(
    .t14_shares(value = c(125, 125)),
    .t14_shares(indicator = "production", value = c(300, 100))
  )
  out <- NULL
  testthat::expect_no_warning(
    out <- reconcile_admin_allocation(
      .t14_allocation(targets = .t14_targets(share = c(0.5, 0.5))),
      shares
    )
  )
  testthat::expect_equal(out$groups$indicator, "area_harvested")
  testthat::expect_equal(out$groups$n_indicators, 1L)
})

testthat::test_that("production without an allocated area has no yield ratio", {
  # A unit the area shares put at zero, which the production series says
  # produced two thirds of the crop: the ratio is undefined and must not be
  # invented as `Inf`, while the divergence carries the whole signal.
  shares <- dplyr::bind_rows(
    .t14_shares(value = c(250, 0)),
    .t14_shares(indicator = "production", value = c(100, 200))
  )
  out <- reconcile_admin_allocation(
    .t14_allocation(targets = .t14_targets(share = c(1, 0))),
    shares
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(units$implied_yield_ratio, c(1 / 3, NA_real_))
  testthat::expect_equal(units$share_divergence, c(-2 / 3, 2 / 3))
})

testthat::test_that("units outside the common set carry no divergence", {
  # Only A1 reports production. Renormalising over a set of one would make
  # its production share 1 and its divergence an artefact of the missing
  # sibling, so the comparison needs at least two common units.
  shares <- dplyr::bind_rows(
    .t14_shares(value = c(125, 125)),
    .t14_shares(indicator = "production", value = c(300, NA_real_))
  )
  out <- reconcile_admin_allocation(
    .t14_allocation(targets = .t14_targets(share = c(0.5, 0.5))),
    shares
  )
  testthat::expect_true(all(is.na(out$units$share_divergence)))
  testthat::expect_equal(out$groups$n_units_production, 1L)
  testthat::expect_true(is.na(out$groups$production_divergence_tvd))
  # `production_basis` names the evidence a reported `production_share`
  # came from. Where no share is reported it must name nothing: a row
  # reading "value" beside an empty share advertises a comparison the
  # report refused to make.
  testthat::expect_true(all(is.na(out$units$production_basis)))
})

testthat::test_that("the area indicators read here are the ones that bind", {
  # A vocabulary copied into a second file drifts from the first. This
  # walks the whole closed vocabulary of `admin_shares_schema()` and
  # asserts the reconciliation's classification agrees, indicator by
  # indicator, with the allocator's own `.alloc_drop_non_area()` -- so a
  # new member of the vocabulary fails here rather than being silently
  # binned by one of the two.
  vocab <- purrr::keep(
    admin_shares_schema()$columns,
    \(col) identical(col$name, "indicator_used")
  )[[1L]]$allowed
  testthat::expect_gt(length(vocab), 2L)
  kept_by_allocator <- purrr::map_lgl(vocab, function(ind) {
    row <- .t14_shares(indicator = ind, value = 1, units = "A1")
    nrow(suppressWarnings(whep:::.alloc_drop_non_area(row))) == 1L
  })
  testthat::expect_equal(
    whep:::.recon_is_area_indicator(vocab),
    kept_by_allocator
  )
})

# --- Interior bridges --------------------------------------------------------

.t14_bridge_shares <- function(observed, carried) {
  dplyr::bind_rows(
    tibble::tibble(year = observed, treatment = "observed"),
    tibble::tibble(year = carried, treatment = "backcast_t0_geometry")
  ) |>
    dplyr::mutate(
      area_code = 1L,
      level_polity_code = "A1",
      level = 1L,
      item_prod_code = 15L,
      indicator_used = "area_harvested",
      value = 1,
      share = NA_real_,
      year = as.integer(year)
    )
}

testthat::test_that("the longest interior bridge is not the longest run", {
  # A ten-year back-cast BEFORE the first observation and a five-year gap
  # between two observations. Both are runs of carried years; only the
  # second is a bridge, and reporting the longer number would say the
  # series was interpolated across a decade it was never observed in.
  out <- reconcile_admin_allocation(
    .t14_allocation(),
    .t14_bridge_shares(
      observed = c(1950:1959, 1965:1970),
      carried = 1960:1964
    )
  )
  testthat::expect_equal(nrow(out$bridges), 1L)
  testthat::expect_equal(out$bridges$longest_run, 5L)
  testthat::expect_equal(out$bridges$longest_interior_run, 5L)

  out2 <- reconcile_admin_allocation(
    .t14_allocation(),
    .t14_bridge_shares(
      observed = c(1960:1961, 1967:1970),
      carried = c(1950:1959, 1962:1966)
    )
  )
  testthat::expect_equal(out2$bridges$longest_run, 10L)
  testthat::expect_equal(out2$bridges$longest_interior_run, 5L)
  testthat::expect_equal(out2$bridges$interior_run_start, 1962L)
  testthat::expect_equal(out2$bridges$interior_run_end, 1966L)
  testthat::expect_equal(out2$bridges$n_years_carried, 15L)
  testthat::expect_equal(out2$bridges$n_years_observed, 6L)
})

testthat::test_that("a sixty-year bridge is legible, not merely legal", {
  # Decision T31(e) admits a proxy bridge of ANY length. The number is what
  # makes it reviewable.
  out <- reconcile_admin_allocation(
    .t14_allocation(),
    .t14_bridge_shares(observed = c(1900, 1961), carried = 1901:1960)
  )
  testthat::expect_equal(out$bridges$longest_interior_run, 60L)
  testthat::expect_equal(out$bridges$interior_run_start, 1901L)
  testthat::expect_equal(out$bridges$interior_run_end, 1960L)
  testthat::expect_equal(
    out$bridges$interior_run_treatment,
    "backcast_t0_geometry"
  )
})

testthat::test_that("a year some unit observed is not a bridged year", {
  # Two units, one back-cast across 1962-1964 and one observed throughout.
  # The series is observed in those years; only the unit is not, and the
  # per-(country, item) bridge the plan asks for must not report a gap the
  # container never had.
  shares <- dplyr::bind_rows(
    .t14_bridge_shares(observed = c(1960:1961, 1965), carried = 1962:1964),
    .t14_bridge_shares(observed = 1960:1965, carried = integer()) |>
      dplyr::mutate(level_polity_code = "A2")
  )
  out <- reconcile_admin_allocation(.t14_allocation(), shares)
  testthat::expect_equal(nrow(out$bridges), 0L)
})

testthat::test_that("a run carried by two units counts years, not rows", {
  # Both units back-cast across 1961-1963 between the same observations.
  # The series was carried for three years, not six. Every other bridge
  # fixture here has one unit, where a count of rows and a count of years
  # agree; a real shares table has many units per container, and a per-row
  # count would inflate every bridge by the unit count without a fixture
  # noticing.
  shares <- dplyr::bind_rows(
    .t14_bridge_shares(observed = c(1960L, 1964L), carried = 1961:1963),
    .t14_bridge_shares(observed = c(1960L, 1964L), carried = 1961:1963) |>
      dplyr::mutate(level_polity_code = "A2")
  )
  out <- reconcile_admin_allocation(.t14_allocation(), shares)
  testthat::expect_equal(nrow(out$bridges), 1L)
  testthat::expect_equal(out$bridges$n_years_carried, 3L)
  testthat::expect_equal(out$bridges$longest_run, 3L)
  testthat::expect_equal(out$bridges$longest_interior_run, 3L)
  testthat::expect_equal(out$bridges$interior_run_start, 1961L)
  testthat::expect_equal(out$bridges$interior_run_end, 1963L)
  testthat::expect_equal(out$bridges$n_years_observed, 2L)
})

testthat::test_that("an unfilled year is carried and labelled as such", {
  shares <- .t14_bridge_shares(observed = c(1960, 1964), carried = 1961:1963) |>
    dplyr::mutate(
      treatment = dplyr::if_else(year == 1962L, NA_character_, treatment)
    )
  out <- reconcile_admin_allocation(.t14_allocation(), shares)
  testthat::expect_equal(out$bridges$longest_interior_run, 3L)
  testthat::expect_equal(
    out$bridges$interior_run_treatment,
    "<NA>|backcast_t0_geometry"
  )
})

testthat::test_that("a leading carried run is no bridge, and counts zero", {
  # A pre-seam back-cast and nothing else: five carried years before the
  # first observation. The docstring's convention is that no interior
  # bridge is 0, not NA -- NA would read as "not evaluated" and hide the
  # one series shape the length is meant to distinguish, while the run
  # itself stays visible in `longest_run`.
  out <- reconcile_admin_allocation(
    .t14_allocation(),
    .t14_bridge_shares(observed = 1960:1961, carried = 1955:1959)
  )
  testthat::expect_equal(nrow(out$bridges), 1L)
  testthat::expect_equal(out$bridges$longest_run, 5L)
  testthat::expect_equal(out$bridges$longest_interior_run, 0L)
  testthat::expect_false(is.na(out$bridges$longest_interior_run))
  testthat::expect_true(is.na(out$bridges$interior_run_start))
  testthat::expect_equal(out$bridges$n_years_carried, 5L)
  testthat::expect_equal(out$bridges$n_years_observed, 2L)
})

testthat::test_that("a production row does not observe a carried year", {
  # The series is an AREA series. A production row reported in the very
  # years the area series was carried across would, if it were read as an
  # area observation, erase the bridge entirely.
  shares <- dplyr::bind_rows(
    .t14_bridge_shares(observed = c(1960L, 1964L), carried = 1961:1963),
    .t14_bridge_shares(observed = 1961:1963, carried = integer()) |>
      dplyr::mutate(indicator_used = "production")
  )
  out <- NULL
  testthat::expect_no_warning(
    out <- reconcile_admin_allocation(.t14_allocation(), shares)
  )
  testthat::expect_equal(nrow(out$bridges), 1L)
  testthat::expect_equal(out$bridges$longest_interior_run, 3L)
  testthat::expect_equal(out$bridges$interior_run_start, 1961L)
  testthat::expect_equal(out$bridges$interior_run_end, 1963L)
})

testthat::test_that("shares with no treatment column report no bridges", {
  shares <- dplyr::select(.t14_shares(), -"treatment")
  out <- NULL
  testthat::expect_message(
    out <- reconcile_admin_allocation(.t14_allocation(), shares),
    "treatment"
  )
  testthat::expect_equal(nrow(out$bridges), 0L)
})

# --- The unit companion ------------------------------------------------------

testthat::test_that("the irrigation floor is counted per unit and per year", {
  targets <- dplyr::bind_rows(
    .t14_targets(clipped = c(12, 0)),
    .t14_targets(clipped = c(3, 0)) |>
      dplyr::mutate(item_prod_code = 27L)
  )
  out <- reconcile_admin_allocation(
    .t14_allocation(targets = targets),
    .t14_shares()
  )
  units <- dplyr::arrange(out$units, level_polity_code, item_prod_code)
  testthat::expect_equal(units$floor_binding, c(TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(units$floor_binding_ha, c(12, 3, 0, 0))
  # Per unit-year: both of A1's items bound the floor, neither of A2's.
  testthat::expect_equal(units$n_floor_binding, c(2L, 2L, 0L, 0L))
})

testthat::test_that("the implied cropping intensity needs a stated extent", {
  # No extent supplied: the intensity is not evaluable and is NA, rather
  # than being computed against an invented denominator.
  out <- reconcile_admin_allocation(.t14_allocation(), .t14_shares())
  testthat::expect_true(all(is.na(out$units$cropping_intensity)))
  testthat::expect_true(all(is.na(out$units$intensity_exceeds_mc)))
})

testthat::test_that("the implied cropping intensity is flagged past the mc", {
  targets <- dplyr::bind_rows(
    .t14_targets(share = c(0.6, 0.4)),
    .t14_targets(share = c(0.6, 0.4)) |>
      dplyr::mutate(item_prod_code = 27L)
  )
  extent <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("A1", "A2"),
    cropland_ha = c(100, 500)
  )
  out <- reconcile_admin_allocation(
    .t14_allocation(targets = targets),
    .t14_shares(),
    intensity = list(
      unit_cropland = extent,
      mc_national = tibble::tibble(
        year = 2000L,
        area_code = 1L,
        mc_factor = 1.5
      )
    )
  )
  units <- dplyr::arrange(out$units, level_polity_code, item_prod_code)
  # A1 sows 2 x 150 ha on 100 ha of cropland; A2 sows 2 x 100 on 500.
  testthat::expect_equal(units$sown_ha, c(300, 300, 200, 200))
  testthat::expect_equal(units$cropping_intensity, c(3, 3, 0.4, 0.4))
  testthat::expect_equal(
    units$intensity_exceeds_mc,
    c(TRUE, TRUE, FALSE, FALSE)
  )
  testthat::expect_equal(units$mc_factor_national, rep(1.5, 4))
})

testthat::test_that("a single multicropping factor may be given as a number", {
  out <- reconcile_admin_allocation(
    .t14_allocation(),
    .t14_shares(),
    intensity = list(
      unit_cropland = tibble::tibble(
        year = 2000L,
        area_code = 1L,
        level_polity_code = c("A1", "A2"),
        cropland_ha = c(100, 500)
      ),
      mc_national = 1
    )
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(units$cropping_intensity, c(1.5, 0.2))
  testthat::expect_equal(units$intensity_exceeds_mc, c(TRUE, FALSE))
})

testthat::test_that("an unknown intensity key is refused, not ignored", {
  testthat::expect_error(
    reconcile_admin_allocation(
      .t14_allocation(),
      .t14_shares(),
      intensity = list(unit_cropland_ha = tibble::tibble())
    ),
    class = "whep_recon_unknown_key"
  )
})

testthat::test_that("an unnamed intensity list is refused, not positional", {
  # `list(1)` has no names, so the keys cannot be told apart and the one
  # number would have to be guessed into a slot. Silently taking the
  # defaults instead would report every intensity as NA while the caller
  # believed a factor had been supplied.
  testthat::expect_error(
    reconcile_admin_allocation(
      .t14_allocation(),
      .t14_shares(),
      intensity = list(1)
    ),
    class = "whep_recon_bad_intensity"
  )
  testthat::expect_error(
    reconcile_admin_allocation(
      .t14_allocation(),
      .t14_shares(),
      intensity = "mc_national"
    ),
    class = "whep_recon_bad_intensity"
  )
})

testthat::test_that("a multicropping factor is one number or a table", {
  # The factor is a ceiling a unit is judged against, so an ambiguous one
  # is refused rather than recycled: `c(1, 2)` would silently flag the
  # units on alternating rows.
  testthat::expect_error(
    reconcile_admin_allocation(
      .t14_allocation(),
      .t14_shares(),
      intensity = list(mc_national = c(1, 2))
    ),
    class = "whep_recon_bad_mc"
  )
  testthat::expect_error(
    reconcile_admin_allocation(
      .t14_allocation(),
      .t14_shares(),
      intensity = list(mc_national = NA_real_)
    ),
    class = "whep_recon_bad_mc"
  )
  testthat::expect_error(
    reconcile_admin_allocation(
      .t14_allocation(),
      .t14_shares(),
      intensity = list(mc_national = "1.5")
    ),
    class = "whep_recon_bad_mc"
  )
})

testthat::test_that("an intensity at the multicropping factor is not over", {
  # The flag is `>`, not `>=`: a unit sowing exactly the ceiling has not
  # exceeded it, and `.apply_capacity_constraint()` would not have
  # redistributed a hectare of it.
  out <- reconcile_admin_allocation(
    .t14_allocation(),
    .t14_shares(),
    intensity = list(
      unit_cropland = tibble::tibble(
        year = 2000L,
        area_code = 1L,
        level_polity_code = c("A1", "A2"),
        cropland_ha = c(100, 50)
      ),
      mc_national = 1.5
    )
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  # A1 sows 150 ha on 100 ha of cropland, exactly 1.5; A2 sows 100 on 50.
  testthat::expect_equal(units$cropping_intensity, c(1.5, 2))
  testthat::expect_equal(units$intensity_exceeds_mc, c(FALSE, TRUE))
})

testthat::test_that("the unit breach is reported at both mc factors", {
  breach <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("A1", "A1"),
    item_prod_code = 15L,
    mc_basis = c("national", "unit"),
    in_force = c(FALSE, TRUE),
    over_ha = c(50, 12)
  )
  out <- reconcile_admin_allocation(
    .t14_allocation(breach = breach),
    .t14_shares()
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(units$breach_national_ha, c(50, 0))
  testthat::expect_equal(units$breach_unit_ha, c(12, 0))
  testthat::expect_equal(units$breach_in_force_basis, c("unit", "unit"))
})

testthat::test_that("a level-0 row has no unit factor, so its breach is NA", {
  # Zero would claim the unit basis was scored and found clean. It does not
  # exist: `.capacity_bases()` builds it only where a granted depth does.
  out <- reconcile_admin_allocation(
    .t14_allocation(
      targets = .t14_targets(
        share = 1,
        units = NA_character_
      )[1L, ]
    ),
    .t14_shares()
  )
  testthat::expect_equal(out$units$breach_national_ha, 0)
  testthat::expect_true(is.na(out$units$breach_unit_ha))
})

testthat::test_that("an empty breach at a granted depth is zero, not NA", {
  out <- reconcile_admin_allocation(.t14_allocation(), .t14_shares())
  testthat::expect_equal(out$units$breach_unit_ha, c(0, 0))
  testthat::expect_equal(out$units$breach_national_ha, c(0, 0))
  testthat::expect_true(all(is.na(out$units$breach_in_force_basis)))
})

# --- Input contract ----------------------------------------------------------

testthat::test_that("a report needs the tables the allocator returns", {
  testthat::expect_error(
    reconcile_admin_allocation(list(targets = .t14_targets())),
    class = "whep_recon_missing_table"
  )
  testthat::expect_error(
    reconcile_admin_allocation(
      .t14_allocation(coverage = dplyr::select(.t14_coverage(), -"admin_sum"))
    ),
    "admin_sum"
  )
})

# --- The land the ceiling uses -----------------------------------------------

testthat::test_that("the unit extent weights each cell by its own fraction", {
  layer <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    0.25, 50.25,         1L,               "A1",     1L,             1.0,
    0.75, 50.25,         1L,               "A1",     1L,             0.25,
    0.75, 50.25,         1L,               "A2",     1L,             0.75
  )
  cropland <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 50.25,
    year = 2000L,
    cropland_ha = c(100, 400)
  )
  out <- dplyr::arrange(
    unit_cropland_extent(layer, cropland),
    level_polity_code
  )
  testthat::expect_equal(out$cropland_ha, c(200, 300))
  testthat::expect_equal(out$n_cells, c(2L, 1L))
  testthat::expect_equal(out$year, c(2000L, 2000L))
  # The physical cell is not double-counted: the two units' extents in the
  # shared cell add up to the cell's own cropland.
  testthat::expect_equal(sum(out$cropland_ha), 100 + 400)
})

testthat::test_that("a level-0 layer still has a container extent", {
  layer <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 50.25,
    area_code = 1L,
    cell_area_frac = c(1, 0.5)
  )
  cropland <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 50.25,
    year = 2000L,
    cropland_ha = c(100, 400)
  )
  out <- unit_cropland_extent(layer, cropland)
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_true(is.na(out$level_polity_code))
  testthat::expect_equal(out$cropland_ha, 300)
})

testthat::test_that("a layer cell the cropland does not cover is not land", {
  # The cropland raster covers one of the layer's three cells. A unit with
  # no covered cell has an UNKNOWN extent, not an extent of zero, and a
  # covered unit's `n_cells` counts the cells that carried cropland -- so
  # the intensity denominator and the count beside it describe the same
  # cells. Carrying the uncovered cells through would report A2 at 0 ha,
  # which divides into an infinite cropping intensity.
  layer <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    0.25, 50.25,         1L,               "A1",     1L,               1,
    0.75, 50.25,         1L,               "A1",     1L,               1,
    1.25, 50.25,         1L,               "A2",     1L,               1
  )
  cropland <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = 2000L,
    cropland_ha = 100
  )
  out <- unit_cropland_extent(layer, cropland)
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$level_polity_code, "A1")
  testthat::expect_equal(out$n_cells, 1L)
  testthat::expect_equal(out$cropland_ha, 100)
  testthat::expect_false("A2" %in% out$level_polity_code)
})

testthat::test_that("the unit extent follows the layer's validity years", {
  layer <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    ~valid_from, ~valid_to,
    0.25, 50.25, 1L, "A1", 1L, 1, 1900L, 1950L,
    0.25, 50.25, 1L, "A2", 1L, 1, 1950L, 2100L
  )
  cropland <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = c(1940L, 2000L),
    cropland_ha = c(100, 400)
  )
  out <- dplyr::arrange(unit_cropland_extent(layer, cropland), year)
  testthat::expect_equal(out$level_polity_code, c("A1", "A2"))
  testthat::expect_equal(out$cropland_ha, c(100, 400))
})

# --- The common set is the denominator, on both sides ------------------------

testthat::test_that("the area share is renormalised over the common set", {
  # A1 and A2 report production and A3 does not, so the common set is a
  # strict subset of the group and the binding area shares over it sum to
  # 0.8, not 1. Comparing a production share taken over two units with an
  # area share taken over three would read the difference in denominator as
  # a divergence in shape.
  out <- reconcile_admin_allocation(
    .t14_allocation(
      coverage = .t14_coverage(
        national = 250,
        admin_sum = 250,
        n_units = 3L,
        n_units_reporting = 3L
      ),
      targets = .t14_targets(
        share = c(0.5, 0.3, 0.2),
        units = c("A1", "A2", "A3")
      )
    ),
    dplyr::bind_rows(
      .t14_shares(value = c(125, 75, 50), units = c("A1", "A2", "A3")),
      .t14_shares(indicator = "production", value = c(300, 100))
    )
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  # 0.5 / 0.8 and 0.3 / 0.8 -- not the raw 0.5 and 0.3 the allocator used.
  testthat::expect_equal(units$area_share_renorm, c(0.625, 0.375, NA_real_))
  testthat::expect_equal(units$production_share, c(0.75, 0.25, NA_real_))
  # Both sides sum to one over the common set: that is what makes them
  # comparable, and the raw shares would sum to 0.8.
  testthat::expect_equal(sum(units$area_share_renorm, na.rm = TRUE), 1)
  testthat::expect_equal(units$share_divergence, c(0.125, -0.125, NA_real_))
  testthat::expect_equal(units$implied_yield_ratio, c(1.2, 2 / 3, NA_real_))
  testthat::expect_equal(out$groups$production_divergence_tvd, 0.125)
  testthat::expect_equal(out$groups$n_units_production, 2L)
})

testthat::test_that("a declared production share is renormalised too", {
  # The same strict subset on the other side: a consented shares-only
  # family declares production shares for two of three units, so they sum
  # to 0.9. Read raw, that 0.9 would be compared with an area share
  # renormalised to 1 and every unit would look short.
  out <- reconcile_admin_allocation(
    .t14_allocation(
      coverage = .t14_coverage(
        national = 250,
        admin_sum = 250,
        n_units = 3L,
        n_units_reporting = 3L
      ),
      targets = .t14_targets(
        share = c(0.5, 0.3, 0.2),
        units = c("A1", "A2", "A3")
      )
    ),
    dplyr::bind_rows(
      .t14_shares(value = c(125, 75, 50), units = c("A1", "A2", "A3")),
      .t14_shares(indicator = "production", value = c(NA_real_, NA_real_)) |>
        dplyr::mutate(share = c(0.6, 0.3))
    )
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(
    units$production_basis,
    c("declared", "declared", NA_character_)
  )
  testthat::expect_equal(units$production_share, c(2 / 3, 1 / 3, NA_real_))
  testthat::expect_equal(sum(units$production_share, na.rm = TRUE), 1)
  testthat::expect_equal(units$area_share_renorm, c(0.625, 0.375, NA_real_))
  testthat::expect_equal(
    units$implied_yield_ratio,
    c((2 / 3) / 0.625, (1 / 3) / 0.375, NA_real_)
  )
})

testthat::test_that("an all-zero area shape supports no comparison", {
  # Every unit's binding share is zero, so the common set's area
  # denominator is zero and the renormalised share would be `NaN` -- a
  # value the evidence never carried. Neither side is reported.
  out <- reconcile_admin_allocation(
    .t14_allocation(targets = .t14_targets(share = c(0, 0))),
    dplyr::bind_rows(
      .t14_shares(value = c(0, 0)),
      .t14_shares(indicator = "production", value = c(300, 100))
    )
  )
  testthat::expect_true(all(is.na(out$units$production_share)))
  testthat::expect_true(all(is.na(out$units$area_share_renorm)))
  testthat::expect_false(any(is.nan(out$units$area_share_renorm)))
  testthat::expect_true(is.na(out$groups$production_divergence_tvd))
  # The production values are real and would carry a basis of their own.
  # Nothing is reported from them here, so nothing names their source.
  testthat::expect_true(all(is.na(out$units$production_basis)))
})

testthat::test_that("a unit with no cropland has no intensity, not Inf", {
  # Dividing a sown area by an extent of zero is `Inf`, which would read as
  # the most over-cropped unit in the run rather than as an unmeasured one.
  out <- reconcile_admin_allocation(
    .t14_allocation(),
    .t14_shares(),
    intensity = list(
      unit_cropland = tibble::tibble(
        year = 2000L,
        area_code = 1L,
        level_polity_code = c("A1", "A2"),
        cropland_ha = c(0, 500)
      ),
      mc_national = 1
    )
  )
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(units$cropping_intensity, c(NA_real_, 0.2))
  testthat::expect_equal(units$intensity_exceeds_mc, c(NA, FALSE))
})

testthat::test_that("the interior bridge reported is the longest one", {
  # Two interior runs, the shorter one first. Reporting the first would
  # understate the longest stretch the series was carried across.
  out <- reconcile_admin_allocation(
    .t14_allocation(),
    .t14_bridge_shares(
      observed = c(1950L, 1953L, 1959L),
      carried = c(1951:1952, 1954:1958)
    )
  )
  testthat::expect_equal(out$bridges$longest_interior_run, 5L)
  testthat::expect_equal(out$bridges$interior_run_start, 1954L)
  testthat::expect_equal(out$bridges$interior_run_end, 1958L)
  testthat::expect_equal(out$bridges$n_years_carried, 7L)
})

# --- Two bases in force at once ----------------------------------------------

testthat::test_that("two bases in force at once is refused as an answer", {
  # Unreachable from today's engine -- `.apply_capacity_constraint()` marks
  # one basis -- but the column encodes a claim about the run, and a stored
  # breach table is a valid input. Naming one of the two would attribute
  # every unit's judgement to a ceiling half of them were not judged
  # against.
  breach <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = "A1",
    item_prod_code = 15L,
    mc_basis = c("national", "unit"),
    in_force = c(TRUE, TRUE),
    over_ha = c(50, 12)
  )
  out <- NULL
  testthat::expect_warning(
    out <- reconcile_admin_allocation(
      .t14_allocation(breach = breach),
      .t14_shares()
    ),
    class = "whep_recon_in_force_ambiguous"
  )
  testthat::expect_true(all(is.na(out$units$breach_in_force_basis)))
  # The breach hectares themselves still read back at both bases.
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(units$breach_national_ha, c(50, 0))
  testthat::expect_equal(units$breach_unit_ha, c(12, 0))
})

testthat::test_that("no basis in force is unknown, and says nothing", {
  breach <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = "A1",
    item_prod_code = 15L,
    mc_basis = "national",
    in_force = FALSE,
    over_ha = 50
  )
  out <- NULL
  testthat::expect_no_warning(
    out <- reconcile_admin_allocation(
      .t14_allocation(breach = breach),
      .t14_shares()
    )
  )
  testthat::expect_true(all(is.na(out$units$breach_in_force_basis)))
})

testthat::test_that("an unmarked in-force flag names no basis either", {
  # A stored breach table whose `in_force` was never filled in. An
  # unmarked row is not a marked one: reading it as in force would name a
  # ceiling the run was never said to have used, and the column claims
  # exactly that the redistribution ran against it.
  breach <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = "A1",
    item_prod_code = 15L,
    mc_basis = "national",
    in_force = NA,
    over_ha = 50
  )
  out <- NULL
  testthat::expect_no_warning(
    out <- reconcile_admin_allocation(
      .t14_allocation(breach = breach),
      .t14_shares()
    )
  )
  testthat::expect_true(all(is.na(out$units$breach_in_force_basis)))
  # The hectares themselves are still read back: only the claim about
  # which ceiling was in force is withheld.
  units <- dplyr::arrange(out$units, level_polity_code)
  testthat::expect_equal(units$breach_national_ha, c(50, 0))
})

# --- Shares the allocation never saw -----------------------------------------

testthat::test_that("shares matching no allocation group say so", {
  # An all-NA report is what a run carrying no admin evidence looks like,
  # so a key mismatch that produces one is invisible: a real run has
  # NA-indicator groups wherever the basis is pattern.
  out <- NULL
  testthat::expect_warning(
    out <- reconcile_admin_allocation(
      .t14_allocation(),
      dplyr::mutate(.t14_shares(), area_code = 999L)
    ),
    class = "whep_recon_shares_unmatched"
  )
  testthat::expect_true(is.na(out$groups$indicator))
  testthat::expect_equal(nrow(out$bridges), 0L)
})

testthat::test_that("a partial key match reports how much matched", {
  shares <- dplyr::bind_rows(
    .t14_shares(),
    dplyr::mutate(.t14_shares(), area_code = 999L)
  )
  testthat::expect_warning(
    reconcile_admin_allocation(.t14_allocation(), shares),
    "2 of 4",
    class = "whep_recon_shares_partial"
  )
})

testthat::test_that("shares carrying pre-allocation years are not a mismatch", {
  # The key is (container, item), never the year: a shares table
  # legitimately carries the back-cast years the allocation never ran, and
  # the bridge report is about exactly those.
  testthat::expect_no_warning(
    reconcile_admin_allocation(
      .t14_allocation(),
      .t14_bridge_shares(observed = c(1900L, 1961L), carried = 1901:1960)
    )
  )
})

# --- The reported shape ------------------------------------------------------

testthat::test_that("the three tables report a locked column order", {
  # A report is read by a person and diffed against a stored one. Order is
  # part of what it says: the measure sits beside the denominator it was
  # taken over, the production block sits together, and the breach columns
  # end the unit row. A column that moves rewrites every stored report
  # without changing a number in it, so the order is pinned here and not
  # left to whichever `select()` ran last.
  out <- reconcile_admin_allocation(
    .t14_allocation(),
    .t14_shares(),
    intensity = list(
      unit_cropland = tibble::tibble(
        year = 2000L,
        area_code = 1L,
        level_polity_code = c("A1", "A2"),
        cropland_ha = c(100, 500)
      ),
      mc_national = 1
    )
  )
  testthat::expect_equal(
    names(out$groups),
    c(
      "year",
      "area_code",
      "item_prod_code",
      "indicator",
      "n_indicators",
      "national_total",
      "admin_sum",
      "n_units_reporting",
      "n_units_valid",
      "coverage",
      "coverage_complete",
      "basis",
      "residual_target",
      "discrepancy",
      "discrepancy_frac",
      "beyond_tolerance",
      "n_units_production",
      "production_divergence_tvd"
    )
  )
  testthat::expect_equal(
    names(out$units),
    c(
      "year",
      "area_code",
      "level_polity_code",
      "item_prod_code",
      "method_crop_alloc",
      "target_ha",
      "area_share",
      "area_share_renorm",
      "production_share",
      "production_basis",
      "share_divergence",
      "implied_yield_ratio",
      "floor_binding",
      "floor_binding_ha",
      "n_floor_binding",
      "sown_ha",
      "cropland_ha",
      "cropping_intensity",
      "mc_factor_national",
      "intensity_exceeds_mc",
      "breach_national_ha",
      "breach_unit_ha",
      "breach_in_force_basis"
    )
  )
  bridges <- reconcile_admin_allocation(
    .t14_allocation(),
    .t14_bridge_shares(observed = c(1960L, 1964L), carried = 1961:1963)
  )$bridges
  testthat::expect_equal(
    names(bridges),
    c(
      "area_code",
      "item_prod_code",
      "n_years_observed",
      "n_years_carried",
      "longest_run",
      "longest_interior_run",
      "interior_run_start",
      "interior_run_end",
      "interior_run_treatment"
    )
  )
})

# --- End to end, through the allocator ---------------------------------------

testthat::test_that("a fabricated discrepancy surfaces and moves no total", {
  # THE MACHINE CRITERION (plan T14). The units report 225 ha where the
  # national total is 250 -- a fabricated 10% discrepancy. It must appear
  # in the report, and the allocation must still place exactly 250 ha,
  # because the national total binds and the shares only set the shape.
  layer <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    0.25, 50.25,         1L,               "A1",     1L,               1,
    0.75, 50.25,         1L,               "A2",     1L,               1
  )
  patterns <- tibble::tribble(
    ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25,             15L,               0.5,
    0.75, 50.25,             15L,               0.5
  )
  cropland <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 50.25,
    year = 2000L,
    cropland_ha = 10000
  )
  national <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_prod_code = 15L,
    harvested_area_ha = 250
  )
  shares <- .t14_shares(value = c(135, 90))
  out <- allocate_level_crops(national, patterns, cropland, layer, shares)
  report <- reconcile_admin_allocation(out, shares)

  testthat::expect_equal(report$groups$admin_sum, 225)
  testthat::expect_equal(report$groups$discrepancy, 25)
  testthat::expect_equal(report$groups$discrepancy_frac, 0.1)
  testthat::expect_true(report$groups$coverage_complete)

  allocated <- sum(out$allocation$rainfed_ha + out$allocation$irrigated_ha)
  testthat::expect_equal(allocated, 250, tolerance = 1e-9)
  testthat::expect_equal(report$groups$national_total, 250)
  # The shape is the reported one, rescaled onto the binding total.
  units <- dplyr::arrange(report$units, level_polity_code)
  testthat::expect_equal(units$target_ha, c(150, 100), tolerance = 1e-9)
  testthat::expect_equal(units$area_share, c(0.6, 0.4), tolerance = 1e-9)
})

testthat::test_that("the allocator's own breach table reads back per unit", {
  # Consumes the table `allocate_level_crops()` returns; the warning text
  # it also emits is never parsed.
  layer <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    0.25, 50.25,         1L,               "A1",     1L,               1,
    0.75, 50.25,         1L,               "A2",     1L,               1
  )
  patterns <- tibble::tribble(
    ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25,             15L,               0.5,
    0.75, 50.25,             15L,               0.5
  )
  cropland <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 50.25,
    year = 2000L,
    cropland_ha = c(100, 1000)
  )
  national <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_prod_code = 15L,
    harvested_area_ha = 250
  )
  shares <- .t14_shares(value = c(150, 100))
  out <- NULL
  testthat::expect_warning(
    out <- allocate_level_crops(
      national,
      patterns,
      cropland,
      layer,
      shares,
      config = list(mc_factor = "national")
    ),
    "50 ha over"
  )
  report <- reconcile_admin_allocation(out, shares)
  units <- dplyr::arrange(report$units, level_polity_code)
  testthat::expect_equal(units$breach_national_ha, c(50, 0), tolerance = 1e-6)
  testthat::expect_equal(units$breach_in_force_basis, c("national", "national"))
  # The reported share bound: the breach was measured, not rescaled away.
  testthat::expect_equal(units$target_ha, c(150, 100), tolerance = 1e-9)
})
