# Every fixture is injected, so the suite is offline.

.nf_inputs <- function(sigma = 0.2, omega = 0.02, requirement = 50) {
  list(
    requirement = tibble::tribble(
      ~year, ~area_code, ~requirement_g_cap_day,
      2010L, 10L,        requirement
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

testthat::test_that("TRS 935 Table 2 reproduces at the printed S_I values", {
  # The report's own worked case: a population whose MEDIAN intake sits at the
  # 0.83 safe level, i.e. M_D = 1.96 * S_R = 0.2352 in logs, printed as 0.24.
  # TRS 935 Table 2 (p.45) gives 7.9% below requirement at S_I = 0.12 and 18.2%
  # at the printed S_I of 0.24 (really 0.2352). Both are the whole argument for
  # why a population needs a margin the individual safe level does not supply,
  # so the prevalence arm has to land on them.
  s_r <- 0.12
  low <- whep:::.nf_prevalence(0.24, whep:::.nf_deficit_sd(0.12, s_r))
  high <- whep:::.nf_prevalence(0.24, whep:::.nf_deficit_sd(0.2352, s_r))
  testthat::expect_equal(round(100 * low, 1), 7.9)
  testthat::expect_equal(round(100 * high, 1), 18.2)
})

testthat::test_that("the deficit SD is a quadrature sum, not a geometric mean", {
  # FAO's own prose trap: S_D = sqrt(S_I^2 + S_R^2) must dominate BOTH parts.
  # A geometric mean would sit between them and understate every floor.
  s_d <- whep:::.nf_deficit_sd(0.30, 0.12)
  testthat::expect_gte(s_d, 0.30)
  testthat::expect_gte(s_d, 0.12)
  testthat::expect_equal(s_d, sqrt(0.30^2 + 0.12^2))
})

testthat::test_that("supply at the floor gives exactly the tolerated shortfall", {
  # The inversion round-trip. It is the single test that catches a z-sign slip,
  # a qnorm(p) for qnorm(1 - p), or a dropped exp(S_I^2/2).
  out <- whep::build_nourishment_floor(
    data = c(
      .nf_inputs(),
      list(
        supply = tibble::tribble(
      ~year, ~area_code, ~protein_g_cap_day,
      2010L, 10L,        NA_real_
    )
      )
    )
  )
  supply <- dplyr::mutate(
    tibble::tribble(~year, ~area_code, 2010L, 10L),
    protein_g_cap_day = out$floor_g_cap_day
  )
  round_trip <- whep::build_nourishment_floor(
    data = c(.nf_inputs(), list(supply = supply))
  )
  testthat::expect_equal(
    round_trip$prevalence_protein_deficit,
    0.025,
    tolerance = 1e-8
  )
})

testthat::test_that("a lower tolerated shortfall raises the floor", {
  strict <- whep::build_nourishment_floor(data = .nf_inputs())
  loose <- whep::build_nourishment_floor(
    data = .nf_inputs(),
    shortfall = 0.05
  )
  testthat::expect_gt(strict$floor_g_cap_day, loose$floor_g_cap_day)
})

testthat::test_that("the floor rises with intake dispersion", {
  equal <- whep::build_nourishment_floor(data = .nf_inputs(sigma = 0.10))
  unequal <- whep::build_nourishment_floor(data = .nf_inputs(sigma = 0.40))
  testthat::expect_gt(unequal$floor_g_cap_day, equal$floor_g_cap_day)
})

testthat::test_that("the mean/median term is present and raises the floor", {
  # Omitting exp(S_I^2/2) sets the required per-capita MEAN equal to the
  # required MEDIAN, which is anti-conservative by about 5% at a typical
  # dispersion -- the opposite of the intuition that dropping a term is safe.
  inputs <- .nf_inputs(sigma = 0.3)
  out <- whep::build_nourishment_floor(data = inputs)
  median_only <- 50 *
    exp(stats::qnorm(0.975) * whep:::.nf_deficit_sd(0.3, 0.12)) /
    (1 - 0.02)
  testthat::expect_gt(out$floor_g_cap_day, median_only)
  testthat::expect_equal(out$floor_g_cap_day, median_only * exp(0.3^2 / 2))
})

testthat::test_that("the loss wedge divides and the requirement scales", {
  base <- whep::build_nourishment_floor(data = .nf_inputs(omega = 0))
  wedged <- whep::build_nourishment_floor(data = .nf_inputs(omega = 0.02))
  testthat::expect_equal(wedged$floor_g_cap_day, base$floor_g_cap_day / 0.98)
  doubled <- whep::build_nourishment_floor(
    data = .nf_inputs(omega = 0, requirement = 100)
  )
  testthat::expect_equal(doubled$floor_g_cap_day, base$floor_g_cap_day * 2)
})

testthat::test_that("the missing protein-quality term is stamped, not hidden", {
  # TRS 935 defines its 0.83 g/kg safe level "for proteins with a PDCAAS value
  # of 1.0". Real diets score below that, so a quality-adjusted floor is
  # HIGHER. Until the term exists the floor is a known understatement, and the
  # method column has to say so rather than the number looking complete.
  out <- whep::build_nourishment_floor(data = .nf_inputs())
  testthat::expect_equal(out$method_quality, "none")
})

testthat::test_that("a safe-level requirement is refused, not silently doubled", {
  # Feeding the class SAFE level into a formula that adds its own population
  # margin counts the requirement margin twice. TRS 935 forbids exactly this.
  inputs <- .nf_inputs()
  inputs$requirement <- dplyr::mutate(
    inputs$requirement,
    method_requirement = "safe"
  )
  testthat::expect_warning(
    whep::build_nourishment_floor(data = inputs),
    "safe"
  )
})

testthat::test_that("a missing input aborts naming the table", {
  testthat::expect_error(
    whep::build_nourishment_floor(data = .nf_inputs()[c("requirement")]),
    "dispersion"
  )
  inputs <- .nf_inputs()
  inputs$loss_wedge <- dplyr::select(inputs$loss_wedge, -"omega")
  testthat::expect_error(
    whep::build_nourishment_floor(data = inputs),
    "omega"
  )
})

testthat::test_that("an out-of-range shortfall is rejected", {
  testthat::expect_error(
    whep::build_nourishment_floor(data = .nf_inputs(), shortfall = 0),
    "shortfall"
  )
  testthat::expect_error(
    whep::build_nourishment_floor(data = .nf_inputs(), shortfall = 1.5),
    "shortfall"
  )
})

testthat::test_that("a country-year missing any term gets no floor", {
  # An inner join would make the row vanish; the floor must be absent IN the
  # output rather than absent FROM it, so a gap stays countable. Both sides of
  # the mismatch survive: the area with no dispersion AND the dispersion with
  # no requirement, each with an NA floor.
  inputs <- .nf_inputs()
  inputs$dispersion <- tibble::tribble(
    ~year, ~area_code, ~sigma,
    2010L, 99L,        0.2
  )
  out <- whep::build_nourishment_floor(data = inputs)
  testthat::expect_setequal(out$area_code, c(10L, 99L))
  testthat::expect_true(all(is.na(out$floor_g_cap_day)))
})
