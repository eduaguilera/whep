# Tests for R/n_percapita.R (SJOS-N Module 3): build_n_percapita().

# Helper fixtures --------------------------------------------------------------

# A build_n_inputs()-style long fixture: synthetic + bnf are the anthropogenic
# reactive-N terms; manure_solid / deposition / urban must be excluded. Two
# countries, with country 10's synthetic split across two cells so the grid
# key must collapse to the country total.
.npc_n_inputs <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~lon,
    ~lat,
    ~item_cbs_code,
    ~fert_type,
    ~n_input_t,
    2000L, 10L, 0.25, 0.25, 2511L, "synthetic", 2,
    2000L, 10L, 0.75, 0.25, 2511L, "synthetic", 3,
    2000L, 10L, 0.25, 0.25, 2511L, "bnf", 3,
    2000L, 10L, 0.25, 0.25, 2511L, "manure_solid", 2,
    2000L, 10L, 0.25, 0.25, NA_integer_, "deposition", 1,
    2000L, 20L, 0.25, 0.25, 2511L, "synthetic", 10,
    2000L, 20L, 0.25, 0.25, 2511L, "bnf", 4,
    2000L, 20L, 0.25, 0.25, NA_integer_, "urban", 5
  )
}

.npc_population <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~population,
    2000L, 10L, 1000,
    2000L, 20L, 2000
  )
}

# Tests ------------------------------------------------------------------------

testthat::test_that("only synthetic and bnf are summed into the anthropogenic total", {
  out <- whep::build_n_percapita(.npc_n_inputs(), .npc_population())
  a10 <- dplyr::filter(out, area_code == 10L)
  a20 <- dplyr::filter(out, area_code == 20L)
  ratio <- (109 + 33) / (0.85 * 109)
  # area 10: synthetic (2 + 3) * ratio + bnf (3); manure/deposition excluded.
  testthat::expect_equal(a10$n_percapita_kg, 5 * ratio + 3)
  # area 20: synthetic (10) * ratio + bnf (4); urban excluded.
  testthat::expect_equal(a20$n_percapita_kg, (10 * ratio + 4) / 2)
})

testthat::test_that("excluded fert_types do not leak into the per-capita value", {
  out <- whep::build_n_percapita(.npc_n_inputs(), .npc_population())
  a10 <- dplyr::filter(out, area_code == 10L)
  ratio <- (109 + 33) / (0.85 * 109)
  expected <- 5 * ratio + 3
  testthat::expect_equal(a10$n_percapita_kg, expected)
})

testthat::test_that("the x1000 tonnes->kg over population conversion holds", {
  # Doubling population halves the per-capita value (linear in 1 / population).
  base <- whep::build_n_percapita(.npc_n_inputs(), .npc_population())
  pop2 <- dplyr::mutate(.npc_population(), population = population * 2)
  halved <- whep::build_n_percapita(.npc_n_inputs(), pop2)
  base <- dplyr::arrange(base, area_code)
  halved <- dplyr::arrange(halved, area_code)
  testthat::expect_equal(halved$n_percapita_kg, base$n_percapita_kg / 2)
})

testthat::test_that("finer grid keys collapse to one country total row", {
  out <- whep::build_n_percapita(.npc_n_inputs(), .npc_population())
  # Country 10's synthetic is split across two cells (2 + 3); the output has a
  # single country row, not one per cell.
  testthat::expect_equal(sum(out$area_code == 10L), 1L)
})

testthat::test_that("the output contract carries the key, value and framing", {
  out <- whep::build_n_percapita(.npc_n_inputs(), .npc_population())
  testthat::expect_named(
    out,
    c("year", "area_code", "n_percapita_kg", "framing")
  )
  testthat::expect_equal(nrow(out), 2L)
  # The chosen framing is provenance, not a silent default (multi-method rule).
  testthat::expect_true(all(out$framing == "synthetic_bnf"))
})

testthat::test_that("country-years without a population row drop out", {
  pop_partial <- dplyr::filter(.npc_population(), area_code == 10L)
  out <- whep::build_n_percapita(.npc_n_inputs(), pop_partial)
  testthat::expect_equal(out$area_code, 10L)
  testthat::expect_false(20L %in% out$area_code)
})

testthat::test_that("zero population aborts instead of producing infinity", {
  pop <- dplyr::mutate(
    .npc_population(),
    population = dplyr::if_else(.data$area_code == 10L, 0, .data$population)
  )
  testthat::expect_error(
    whep::build_n_percapita(.npc_n_inputs(), pop),
    "strictly positive"
  )
})

testthat::test_that("build_n_percapita output composes into build_n_boundary_percapita", {
  # The integration seam: build_n_percapita() supplies the n_percapita input
  # that build_n_boundary_percapita() (Module 3) consumes.
  n_percapita <- whep::build_n_percapita(.npc_n_inputs(), .npc_population())
  nourishment <- tibble::tribble(
    ~year,
    ~area_code,
    ~value_norm,
    ~population,
    2000L, 10L, 0.8, 3e9,
    2000L, 20L, 1.5, 3e9
  )
  scatter <- whep::build_n_boundary_percapita(n_percapita, nourishment)
  testthat::expect_named(
    scatter,
    c("year", "area_code", "nourish_norm", "boundary_norm", "population")
  )
  testthat::expect_setequal(scatter$area_code, c(10L, 20L))
})

testthat::test_that("an unknown framing is rejected", {
  testthat::expect_error(
    whep::build_n_percapita(
      .npc_n_inputs(),
      .npc_population(),
      framing = "all_inputs"
    ),
    "framing"
  )
})

testthat::test_that("build_n_percapita aborts on a missing input column", {
  testthat::expect_error(
    whep::build_n_percapita(
      dplyr::rename(.npc_n_inputs(), ft = fert_type),
      .npc_population()
    ),
    "fert_type"
  )
})

testthat::test_that("the example fixture matches the output contract", {
  out <- whep::build_n_percapita(example = TRUE)
  testthat::expect_named(
    out,
    c("year", "area_code", "n_percapita_kg", "framing")
  )
  testthat::expect_true(all(out$n_percapita_kg > 0))
  testthat::expect_true(all(out$framing == "synthetic_bnf"))
})
