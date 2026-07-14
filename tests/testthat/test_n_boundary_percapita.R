# One year with a world population of 6e9 (three countries summing to 6e9), so
# the world per-capita bounds are low_pc = 60e9 / 6e9 = 10 and
# high_pc = 125e9 / 6e9 kg N/cap/yr. Three n_percapita values exercise the
# below-bound, in-band and above-cap branches of the piecewise normalization.
.low_pc <- 60e9 / 6e9
.high_pc <- 125e9 / 6e9

.npc_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~n_percapita_kg,
    2000L, 10L, 5, # below low_pc (10)
    2000L, 20L, 15, # in band (10 .. 20.83)
    2000L, 30L, 500 # far above high_pc -> hits the cap of 6
  )
}

.nourish_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~value_norm, ~population,
    2000L, 10L, 0.8, 3e9,
    2000L, 20L, 1.5, 2e9,
    2000L, 30L, 2.4, 1e9
  )
}

testthat::test_that("below-bound N normalizes to n/low_pc times afs_share", {
  out <- whep::build_n_boundary_percapita(.npc_fixture(), .nourish_fixture())
  a10 <- dplyr::filter(out, area_code == 10L)
  testthat::expect_equal(a10$boundary_norm, (5 / .low_pc) * 0.8)
})

testthat::test_that("in-band N maps linearly onto the [1, 2] band", {
  out <- whep::build_n_boundary_percapita(.npc_fixture(), .nourish_fixture())
  a20 <- dplyr::filter(out, area_code == 20L)
  expected <- (1 + (15 - .low_pc) / (.high_pc - .low_pc)) * 0.8
  testthat::expect_equal(a20$boundary_norm, expected)
  testthat::expect_gt(a20$boundary_norm / 0.8, 1)
  testthat::expect_lt(a20$boundary_norm / 0.8, 2)
})

testthat::test_that("far-above-bound N is capped at 6 before afs_share", {
  out <- whep::build_n_boundary_percapita(.npc_fixture(), .nourish_fixture())
  a30 <- dplyr::filter(out, area_code == 30L)
  # 1 + 500 / high_pc = 25 > 6, so the pre-share norm is capped at 6.
  testthat::expect_equal(a30$boundary_norm, 6 * 0.8)
})

testthat::test_that("afs_share scales the boundary norm linearly", {
  full <- whep::build_n_boundary_percapita(
    .npc_fixture(),
    .nourish_fixture(),
    afs_share = 1.0
  )
  scaled <- whep::build_n_boundary_percapita(
    .npc_fixture(),
    .nourish_fixture(),
    afs_share = 0.8
  )
  full <- dplyr::arrange(full, area_code)
  scaled <- dplyr::arrange(scaled, area_code)
  testthat::expect_equal(scaled$boundary_norm, full$boundary_norm * 0.8)
})

testthat::test_that("the join yields both scatter axes and population", {
  out <- whep::build_n_boundary_percapita(.npc_fixture(), .nourish_fixture())
  testthat::expect_named(
    out,
    c("year", "area_code", "nourish_norm", "boundary_norm", "population")
  )
  a20 <- dplyr::filter(out, area_code == 20L)
  # nourish_norm is passed through from the nourishment value_norm.
  testthat::expect_equal(a20$nourish_norm, 1.5)
  testthat::expect_equal(a20$population, 2e9)
})

testthat::test_that("a country absent from nourishment drops out of the scatter", {
  npc <- dplyr::bind_rows(
    .npc_fixture(),
    tibble::tibble(year = 2000L, area_code = 99L, n_percapita_kg = 12)
  )
  out <- whep::build_n_boundary_percapita(npc, .nourish_fixture())
  testthat::expect_false(99L %in% out$area_code)
})

testthat::test_that("build_n_boundary_percapita aborts on a missing column", {
  testthat::expect_error(
    whep::build_n_boundary_percapita(
      dplyr::rename(.npc_fixture(), n_kg = n_percapita_kg),
      .nourish_fixture()
    ),
    "n_percapita_kg"
  )
})
