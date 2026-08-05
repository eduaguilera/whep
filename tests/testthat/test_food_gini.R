# A perfectly equal year (three identical countries) and a concentrated year
# (equal populations, unequal per-capita supply) sharing one fixture, so a
# single call returns one Gini per year. The equal year has a hand-computed
# Gini of 0; the concentrated year 4/9 (worked below).
.gini_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~protein_g_cap_day, ~population,
    2000L, 10L, 40, 100,
    2000L, 20L, 40, 100,
    2000L, 30L, 40, 100,
    2001L, 10L, 10, 1,
    2001L, 20L, 20, 1,
    2001L, 30L, 90, 1
  )
}

testthat::test_that("a perfectly equal distribution has a Gini of zero", {
  out <- whep::calculate_food_gini(.gini_fixture())
  g2000 <- dplyr::filter(out, year == 2000L)
  testthat::expect_equal(g2000$gini, 0)
})

testthat::test_that("a concentrated distribution has a higher, exact Gini", {
  # value_frac = c(90, 20, 10) / 120 = c(3/4, 1/6, 1/12) (sorted descending),
  # pop_frac = 1/3 each, richer_frac = c(0, 1/3, 2/3), so
  # score = c(3/4 * 1/3, 1/6 * 1, 1/12 * 5/3) = c(1/4, 1/6, 5/36), sum = 5/9
  # and gini = 1 - 5/9 = 4/9.
  out <- whep::calculate_food_gini(.gini_fixture())
  g2001 <- dplyr::filter(out, year == 2001L)
  testthat::expect_equal(g2001$gini, 4 / 9)
  g2000 <- dplyr::filter(out, year == 2000L)
  testthat::expect_gt(g2001$gini, g2000$gini)
})

testthat::test_that("calculate_food_gini returns one row per year", {
  out <- whep::calculate_food_gini(.gini_fixture())
  testthat::expect_setequal(out$year, c(2000L, 2001L))
  testthat::expect_named(out, c("year", "gini"))
})

testthat::test_that("value_col and pop_col select the inequality axis", {
  # Weighting an equal-per-capita year by unequal populations still yields a
  # Gini of zero, because the supply is equal per capita regardless of weights.
  x <- tibble::tribble(
    ~year, ~area_code, ~energy_kcal_cap_day, ~pop_alt,
    2000L, 10L, 2500, 300,
    2000L, 20L, 2500, 100,
    2000L, 30L, 2500, 50
  )
  out <- whep::calculate_food_gini(
    x,
    value_col = energy_kcal_cap_day,
    pop_col = pop_alt
  )
  testthat::expect_equal(out$gini, 0)
})

# ---- disaggregate_ussr -----------------------------------------------------

.ussr_shares_fixture <- function() {
  tibble::tribble(
    ~ussr_area_code, ~successor_area_code, ~pop_share,
    228L, 1L, 0.5,
    228L, 2L, 0.3,
    228L, 3L, 0.2
  )
}

.ussr_supply_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~protein_g_cap_day, ~population,
    1990L, 228L, 50, 100, # aggregate USSR, pre-cutoff -> split
    1995L, 228L, 55, 120, # same area, post-cutoff -> kept
    1990L, 999L, 40, 30 # non-USSR, pre-cutoff -> kept
  )
}

testthat::test_that("the pre-1992 USSR split conserves total population", {
  out <- whep::disaggregate_ussr(
    .ussr_supply_fixture(),
    .ussr_shares_fixture()
  )
  split <- dplyr::filter(out, year == 1990L, area_code %in% c(1L, 2L, 3L))
  testthat::expect_equal(sum(split$population), 100)
  testthat::expect_setequal(split$area_code, c(1L, 2L, 3L))
})

testthat::test_that("successors inherit the aggregate per-capita supply", {
  out <- whep::disaggregate_ussr(
    .ussr_supply_fixture(),
    .ussr_shares_fixture()
  )
  split <- dplyr::filter(out, year == 1990L, area_code %in% c(1L, 2L, 3L))
  testthat::expect_equal(split$protein_g_cap_day, c(50, 50, 50))
  s1 <- dplyr::filter(split, area_code == 1L)
  testthat::expect_equal(s1$population, 100 * 0.5)
})

testthat::test_that("the aggregate USSR row is gone only before the cutoff", {
  out <- whep::disaggregate_ussr(
    .ussr_supply_fixture(),
    .ussr_shares_fixture()
  )
  testthat::expect_equal(
    nrow(dplyr::filter(out, year == 1990L, area_code == 228L)),
    0L
  )
  post <- dplyr::filter(out, year == 1995L, area_code == 228L)
  testthat::expect_equal(post$population, 120)
  testthat::expect_equal(post$protein_g_cap_day, 55)
})

testthat::test_that("non-USSR pre-cutoff rows pass through untouched", {
  out <- whep::disaggregate_ussr(
    .ussr_supply_fixture(),
    .ussr_shares_fixture()
  )
  other <- dplyr::filter(out, year == 1990L, area_code == 999L)
  testthat::expect_equal(other$population, 30)
  testthat::expect_equal(other$protein_g_cap_day, 40)
})
