# Every fixture is injected, so the suite is offline.

.nb_inputs <- function(
  sigma = 0.26,
  omega = 0.02,
  average = 32,
  safe = 40,
  population = 1e6
) {
  list(
    requirement = tibble::tribble(
      ~year, ~area_code, ~requirement_g_cap_day, ~population,
      2010L, 10L,        average,                population
    ),
    requirement_safe = tibble::tribble(
      ~year, ~area_code, ~requirement_g_cap_day,
      2010L, 10L,        safe
    ),
    dispersion = tibble::tribble(
      ~year, ~area_code, ~sigma,
      2010L, 10L,        sigma
    ),
    loss_wedge = tibble::tribble(
      ~year, ~area_code, ~omega,
      2010L, 10L,        omega
    )
  )
}

.nb_supply <- function(value) {
  tibble::tribble(
    ~year, ~area_code, ~protein_g_cap_day,
    2010L, 10L,        value
  )
}

testthat::test_that("TRS 935 Table 2 reproduces at the printed S_I values", {
  # The report's own worked case: a population whose MEDIAN intake sits at the
  # 0.83 safe level, i.e. M_D = 1.96 * S_R = 0.2352 in logs, printed as 0.24.
  # TRS 935 Table 2 (p.45) gives 7.9% below requirement at S_I = 0.12 and 18.2%
  # at the printed S_I of 0.24 (really 0.2352).
  s_r <- 0.12
  low <- whep:::.nf_prevalence(0.24, whep:::.nf_deficit_sd(0.12, s_r))
  high <- whep:::.nf_prevalence(0.24, whep:::.nf_deficit_sd(0.2352, s_r))
  testthat::expect_equal(round(100 * low, 1), 7.9)
  testthat::expect_equal(round(100 * high, 1), 18.2)
})

testthat::test_that("the deficit SD is a quadrature sum, not a geometric mean", {
  s_d <- whep:::.nf_deficit_sd(0.30, 0.12)
  testthat::expect_gte(s_d, 0.30)
  testthat::expect_gte(s_d, 0.12)
  testthat::expect_equal(s_d, sqrt(0.30^2 + 0.12^2))
})

testthat::test_that("supply at the floor gives exactly the tolerated shortfall", {
  out <- whep::build_nourishment_band(data = .nb_inputs())
  round_trip <- whep::build_nourishment_band(
    data = c(.nb_inputs(), list(supply = .nb_supply(out$floor_g_cap_day)))
  )
  testthat::expect_equal(
    round_trip$prevalence_protein_deficit,
    0.025,
    tolerance = 1e-8
  )
})

testthat::test_that("supply at the ceiling gives exactly the tolerated share", {
  # The upper round trip, and the mirror of the one above. It is what catches a
  # sign slip in the ceiling's own z, which is qnorm(share) and NOT
  # qnorm(1 - share) -- the two differ for every share except 0.5, so a default
  # of 0.5 would hide the error.
  out <- whep::build_nourishment_band(
    data = .nb_inputs(),
    ceiling = list(multiple = 2, share = 0.25)
  )
  round_trip <- whep::build_nourishment_band(
    data = c(.nb_inputs(), list(supply = .nb_supply(out$ceiling_g_cap_day))),
    ceiling = list(multiple = 2, share = 0.25)
  )
  testthat::expect_equal(
    round_trip$prevalence_protein_excess,
    0.25,
    tolerance = 1e-8
  )
})

testthat::test_that("the band does not invert on realistic inputs", {
  # At the default share of 0.5 the lowest ceiling still sits above the highest
  # floor, measured across the 2010 build (74.81 against 73.75). Here: the two
  # extremes of the observed dispersion range must both keep floor < ceiling.
  for (s in c(0.18, 0.38)) {
    out <- whep::build_nourishment_band(data = .nb_inputs(sigma = s))
    testthat::expect_lt(out$floor_g_cap_day, out$ceiling_g_cap_day)
  }
})

testthat::test_that("a symmetric tolerance inverts the band, which is why 0.5", {
  # Demanding that only 2.5% exceed the upper limit -- mirroring the floor's
  # shortfall -- puts the ceiling BELOW the floor. That is not a tuning problem:
  # TRS 935 calls intakes below requirement harmful and twice the safe level
  # "unlikely to be associated with any risk", so the two tails are not the same
  # kind of risk and cannot carry the same tolerance.
  out <- whep::build_nourishment_band(
    data = .nb_inputs(),
    ceiling = list(multiple = 2, share = 0.025)
  )
  testthat::expect_gt(out$floor_g_cap_day, out$ceiling_g_cap_day)
})

testthat::test_that("the ceiling scales with the multiple of the safe level", {
  # TRS 935 names 2x as "previously identified as a safe upper limit" and 3-4x
  # as approaching the tolerable upper limit. Both stay selectable.
  two <- whep::build_nourishment_band(data = .nb_inputs())
  three <- whep::build_nourishment_band(
    data = .nb_inputs(),
    ceiling = list(multiple = 3, share = 0.5)
  )
  testthat::expect_equal(
    three$ceiling_g_cap_day,
    two$ceiling_g_cap_day * 1.5
  )
})

testthat::test_that("a larger tolerated share raises the ceiling", {
  strict <- whep::build_nourishment_band(
    data = .nb_inputs(),
    ceiling = list(multiple = 2, share = 0.25)
  )
  loose <- whep::build_nourishment_band(
    data = .nb_inputs(),
    ceiling = list(multiple = 2, share = 0.75)
  )
  testthat::expect_gt(loose$ceiling_g_cap_day, strict$ceiling_g_cap_day)
})

testthat::test_that("headcounts are the prevalences times population", {
  # The whole point of reporting them: a country is not uniformly under or over,
  # and the number of people on each side is the quantity a reader can act on.
  out <- whep::build_nourishment_band(
    data = c(.nb_inputs(population = 5e7), list(supply = .nb_supply(60)))
  )
  testthat::expect_equal(
    out$people_under,
    out$prevalence_protein_deficit * 5e7
  )
  testthat::expect_equal(
    out$people_over,
    out$prevalence_protein_excess * 5e7
  )
  # Both tails are populated at a supply inside the band: this is a
  # distribution, not a switch.
  testthat::expect_gt(out$people_under, 0)
  testthat::expect_gt(out$people_over, 0)
})

testthat::test_that("a lower tolerated shortfall raises the floor", {
  strict <- whep::build_nourishment_band(data = .nb_inputs())
  loose <- whep::build_nourishment_band(
    data = .nb_inputs(),
    shortfall = 0.05
  )
  testthat::expect_gt(strict$floor_g_cap_day, loose$floor_g_cap_day)
})

testthat::test_that("the floor rises with intake dispersion", {
  equal <- whep::build_nourishment_band(data = .nb_inputs(sigma = 0.10))
  unequal <- whep::build_nourishment_band(data = .nb_inputs(sigma = 0.40))
  testthat::expect_gt(unequal$floor_g_cap_day, equal$floor_g_cap_day)
})

testthat::test_that("the mean/median term is present and raises the floor", {
  inputs <- .nb_inputs(sigma = 0.3, average = 50, omega = 0.02)
  out <- whep::build_nourishment_band(data = inputs)
  median_only <- 50 *
    exp(stats::qnorm(0.975) * whep:::.nf_deficit_sd(0.3, 0.12)) /
    (1 - 0.02)
  testthat::expect_gt(out$floor_g_cap_day, median_only)
  testthat::expect_equal(out$floor_g_cap_day, median_only * exp(0.3^2 / 2))
})

testthat::test_that("the loss wedge divides and the requirement scales", {
  base <- whep::build_nourishment_band(data = .nb_inputs(omega = 0))
  wedged <- whep::build_nourishment_band(data = .nb_inputs(omega = 0.02))
  testthat::expect_equal(wedged$floor_g_cap_day, base$floor_g_cap_day / 0.98)
  testthat::expect_equal(
    wedged$ceiling_g_cap_day,
    base$ceiling_g_cap_day / 0.98
  )
  doubled <- whep::build_nourishment_band(
    data = .nb_inputs(omega = 0, average = 64)
  )
  testthat::expect_equal(doubled$floor_g_cap_day, base$floor_g_cap_day * 2)
})

testthat::test_that("both method choices are stamped", {
  out <- whep::build_nourishment_band(data = .nb_inputs())
  testthat::expect_equal(out$method_quality, "none")
  testthat::expect_equal(out$method_ceiling, "2x_safe_level_at_50pct")
})

testthat::test_that("a safe-level average requirement is refused", {
  inputs <- .nb_inputs()
  inputs$requirement <- dplyr::mutate(
    inputs$requirement,
    method_requirement = "safe"
  )
  testthat::expect_warning(
    whep::build_nourishment_band(data = inputs),
    "safe"
  )
})

testthat::test_that("a missing input aborts naming the table", {
  testthat::expect_error(
    whep::build_nourishment_band(
      data = .nb_inputs()[c("requirement", "requirement_safe")]
    ),
    "dispersion"
  )
  testthat::expect_error(
    whep::build_nourishment_band(
      data = .nb_inputs()[c("requirement", "dispersion", "loss_wedge")]
    ),
    "requirement_safe"
  )
  inputs <- .nb_inputs()
  inputs$loss_wedge <- dplyr::select(inputs$loss_wedge, -"omega")
  testthat::expect_error(whep::build_nourishment_band(data = inputs), "omega")
})

testthat::test_that("out-of-range tolerances are rejected", {
  testthat::expect_error(
    whep::build_nourishment_band(data = .nb_inputs(), shortfall = 0),
    "shortfall"
  )
  testthat::expect_error(
    whep::build_nourishment_band(
      data = .nb_inputs(),
      ceiling = list(multiple = 2, share = 1.5)
    ),
    "share"
  )
  testthat::expect_error(
    whep::build_nourishment_band(
      data = .nb_inputs(),
      ceiling = list(multiple = -1, share = 0.5)
    ),
    "multiple"
  )
})

testthat::test_that("a country-year missing any term gets no band", {
  inputs <- .nb_inputs()
  inputs$dispersion <- tibble::tribble(
    ~year, ~area_code, ~sigma,
    2010L, 99L,        0.2
  )
  out <- whep::build_nourishment_band(data = inputs)
  testthat::expect_setequal(out$area_code, c(10L, 99L))
  testthat::expect_true(all(is.na(out$floor_g_cap_day)))
  testthat::expect_true(all(is.na(out$ceiling_g_cap_day)))
})

testthat::test_that("a duplicated country-year key aborts, never fans out", {
  # FAOSTAT publishes per-capita protein supply (element 674) PER ITEM, so a
  # supply table that forgot to filter to the Grand Total carries ~90 rows per
  # country. Joined, that multiplies every headcount by ~90 silently -- it read
  # as 713 billion people under requirement before this guard existed.
  inputs <- .nb_inputs()
  inputs$supply <- dplyr::bind_rows(.nb_supply(60), .nb_supply(70))
  testthat::expect_error(
    whep::build_nourishment_band(data = inputs),
    "one row per"
  )
  inputs2 <- .nb_inputs()
  inputs2$dispersion <- dplyr::bind_rows(
    inputs2$dispersion,
    inputs2$dispersion
  )
  testthat::expect_error(
    whep::build_nourishment_band(data = inputs2),
    "one row per"
  )
})
