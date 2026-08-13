# The packaged coefficient table is TRS 935's own, so these tests assert against
# values read out of the report rather than against fixtures of our own making.
# Each fixture is a complete literal tribble() so air's tribble skip preserves
# the alignment; a `...`-forwarding helper would defeat it.

.pr_adults <- function() {
  tibble::tribble(
    ~year, ~area_code, ~age_start, ~age_span, ~sex, ~population,
    2010L, 10L,        30L,        5L,        "m",  500,
    2010L, 10L,        30L,        5L,        "f",  500
  )
}

# Ages 4-6 only, the class where TRS 935 publishes 17.1 / 16.2 g/day.
.pr_children <- function() {
  tibble::tribble(
    ~year, ~area_code, ~age_start, ~age_span, ~sex, ~population,
    2010L, 10L,        4L,         3L,        "m",  500,
    2010L, 10L,        4L,         3L,        "f",  500
  )
}

.pr_young <- function() {
  tibble::tribble(
    ~year, ~area_code, ~age_start, ~age_span, ~sex, ~population,
    2010L, 10L,        0L,         5L,        "m",  600,
    2010L, 10L,        0L,         5L,        "f",  600,
    2010L, 10L,        30L,        5L,        "m",  400,
    2010L, 10L,        30L,        5L,        "f",  400
  )
}

.pr_old <- function() {
  tibble::tribble(
    ~year, ~area_code, ~age_start, ~age_span, ~sex, ~population,
    2010L, 20L,        0L,         5L,        "m",  100,
    2010L, 20L,        0L,         5L,        "f",  100,
    2010L, 20L,        30L,        5L,        "m",  900,
    2010L, 20L,        30L,        5L,        "f",  900
  )
}

.pr_mixed <- function() {
  tibble::tribble(
    ~year, ~area_code, ~age_start, ~age_span, ~sex, ~population,
    2010L, 10L,        0L,         5L,        "m",  300,
    2010L, 10L,        10L,        10L,       "f",  300,
    2010L, 10L,        40L,        20L,       "m",  400
  )
}

testthat::test_that("an adult-only population gets the adult requirement", {
  # TRS 935 Table 46: 0.66 g/kg per day average at the 55 kg reference weight
  # WHEP uses = 36.30 g/day. The safe level at the same weight is 46 g/day,
  # which is the value the axis shipped as a flat floor for every population.
  out <- whep::build_protein_requirement(
    data = list(population_age = .pr_adults())
  )
  testthat::expect_equal(out$requirement_g_cap_day, 36.30)
  testthat::expect_equal(out$population, 1000)
  testthat::expect_equal(out$method_requirement, "average")

  safe <- whep::build_protein_requirement(
    data = list(population_age = .pr_adults()),
    requirement = "safe"
  )
  testthat::expect_equal(safe$requirement_g_cap_day, 46.0)
  testthat::expect_equal(safe$method_requirement, "safe")
})

testthat::test_that("children require far less than the flat adult floor", {
  # This is the defect the axis had: 46 g/day applied to everyone. TRS 935
  # Table 47 gives 17.1 (boys) and 16.2 (girls) g/day at ages 4-6 on the safe
  # basis -- roughly a third of the adult value.
  out <- whep::build_protein_requirement(
    data = list(population_age = .pr_children()),
    requirement = "safe"
  )
  testthat::expect_equal(out$requirement_g_cap_day, (17.1 + 16.2) / 2)
  testthat::expect_lt(out$requirement_g_cap_day, 46 / 2)
})

testthat::test_that("a young population sits below an old one", {
  young <- whep::build_protein_requirement(
    data = list(population_age = .pr_young())
  )
  old <- whep::build_protein_requirement(
    data = list(population_age = .pr_old())
  )
  testthat::expect_lt(young$requirement_g_cap_day, old$requirement_g_cap_day)
  # Both must lie inside the per-class envelope: no weighted mean of the class
  # values can escape the range of those values.
  coefs <- whep::whep_coef_table("protein_requirement")
  testthat::expect_gte(young$requirement_g_cap_day, min(coefs$avg_req_g_day))
  testthat::expect_lte(old$requirement_g_cap_day, max(coefs$avg_req_g_day))
})

testthat::test_that("the average basis is below the safe basis everywhere", {
  avg <- whep::build_protein_requirement(
    data = list(population_age = .pr_mixed())
  )
  safe <- whep::build_protein_requirement(
    data = list(population_age = .pr_mixed()),
    requirement = "safe"
  )
  # The safe level is the average plus 1.96 SD, so it must exceed the average
  # for every population, and by the per-class ratio (0.80-0.86), never by the
  # uniform adult ratio of 0.7952.
  testthat::expect_lt(avg$requirement_g_cap_day, safe$requirement_g_cap_day)
  ratio <- avg$requirement_g_cap_day / safe$requirement_g_cap_day
  testthat::expect_gt(ratio, 0.79)
  testthat::expect_lt(ratio, 0.86)
})

testthat::test_that("an unexpected sex code aborts rather than dropping rows", {
  bad <- dplyr::mutate(.pr_adults(), sex = "male")
  testthat::expect_error(
    whep::build_protein_requirement(data = list(population_age = bad)),
    "male"
  )
})

testthat::test_that("a missing input column aborts", {
  bad <- dplyr::select(.pr_adults(), -"population")
  testthat::expect_error(
    whep::build_protein_requirement(data = list(population_age = bad)),
    "population"
  )
})

testthat::test_that("an unknown requirement basis is rejected", {
  testthat::expect_error(
    whep::build_protein_requirement(
      data = list(population_age = .pr_adults()),
      requirement = "nope"
    ),
    "arg_match|must be one of|nope"
  )
})

testthat::test_that("the packaged requirement table is internally consistent", {
  coefs <- whep::whep_coef_table("protein_requirement")
  # Every class carries both sexes, and the average never exceeds the safe
  # level -- the safe level IS the average plus 1.96 SD.
  testthat::expect_setequal(unique(coefs$sex), c("m", "f"))
  testthat::expect_true(all(coefs$avg_req_g_day < coefs$safe_req_g_day))
  testthat::expect_true(all(coefs$avg_req_g_kg_day > 0))
  testthat::expect_true(all(coefs$reference_weight_kg > 0))
  # The g/day column must reproduce the published safe level times the
  # average-to-safe ratio, which is how it was derived.
  testthat::expect_equal(
    coefs$avg_req_g_day,
    round(coefs$safe_req_g_day * coefs$avg_to_safe_ratio, 2),
    tolerance = 0.01
  )
})
