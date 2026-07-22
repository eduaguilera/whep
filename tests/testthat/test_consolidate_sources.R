# Tests for consolidate_sources(): the general multi-source panel
# winner-consolidation. Cases mirror the energy-hist tests/test_dedup.R
# synthetic frames (priority, coverage tie-break, quality, hard-drop,
# measure demotion, exemption, continuity override, deterministic ordering).

tier_levels <- c(
  "Observed",
  "Estimated",
  "Interpolated",
  "Modeled",
  "Reconstructed"
)

# Priority wins ----------------------------------------------------------------

testthat::test_that("consolidate_sources keeps the highest-priority source", {
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "WLD", "Coal", "OWID", 10,
    1900, "WLD", "Coal", "Malanima", 20
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(OWID = 1L, Malanima = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )

  testthat::expect_equal(nrow(won), 1L)
  testthat::expect_equal(won$source, "OWID")
  testthat::expect_equal(won$value, 10)
  testthat::expect_equal(won$n_sources, 2L)
  testthat::expect_equal(won$source_rank, 1L)
})

# Coverage tie-break beats alphabetical ----------------------------------------

testthat::test_that("broader coverage breaks an equal-rank tie and beats name order", {
  # Mitchell and Malanima are both rank 4. Mitchell covers three years,
  # Malanima only the contested one. Ascending name order would pick Malanima
  # (M-a < M-i), so a Mitchell winner proves coverage decides.
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "WLD", "Coal", "Mitchell", 30,
    1901, "WLD", "Coal", "Mitchell", 31,
    1902, "WLD", "Coal", "Mitchell", 32,
    1901, "WLD", "Coal", "Malanima", 99
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(Mitchell = 4L, Malanima = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )

  win_1901 <- won[won$year == 1901, ]
  testthat::expect_equal(win_1901$source, "Mitchell")
  testthat::expect_equal(win_1901$value, 31)

  # With coverage off, the equal-rank tie falls straight to ascending source
  # name: Malanima wins the contested 1901 cell. Continuity override is disabled
  # here so the isolated flip is not reverted, isolating the name-order path.
  won_no_cov <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(Mitchell = 4L, Malanima = 4L),
    .by = c("region", "category"),
    tie_break_coverage = FALSE,
    continuity_override = FALSE,
    verbose = FALSE
  )
  testthat::expect_equal(
    won_no_cov[won_no_cov$year == 1901, ]$source,
    "Malanima"
  )
})

# Quality preference -----------------------------------------------------------

testthat::test_that("quality_col decides an equal-rank, equal-coverage tie", {
  # Etemad (Estimated) and Malanima (Observed) are both rank 4 and each covers
  # exactly the one test year. Etemad sorts first by name, so a Malanima winner
  # proves the Observed quality level wins.
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value, ~tier,
    1900, "WLD", "Coal", "Etemad", 40, "Estimated",
    1900, "WLD", "Coal", "Malanima", 41, "Observed"
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(Etemad = 4L, Malanima = 4L),
    .by = c("region", "category"),
    quality_col = tier,
    quality_levels = tier_levels,
    verbose = FALSE
  )

  testthat::expect_equal(nrow(won), 1L)
  testthat::expect_equal(won$source, "Malanima")
  testthat::expect_equal(won$tier, "Observed")
})

# Hard drop, including an uncontested pinned source ----------------------------

testthat::test_that("sources ranked >= drop_at are dropped, even uncontested", {
  # Contested cell: OWID (rank 1) wins, Ember (rank 100) is dropped.
  contested <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "WLD", "Coal", "OWID", 1,
    1900, "WLD", "Coal", "Ember", 2
  )
  won <- whep::consolidate_sources(
    contested,
    value_col = value,
    source_col = source,
    priority = c(OWID = 1L, Ember = 100L),
    .by = c("region", "category"),
    verbose = FALSE
  )
  testthat::expect_equal(won$source, "OWID")
  testthat::expect_equal(won$n_sources, 1L)

  # Uncontested cell of ONLY a pinned source must vanish entirely: this is the
  # failure the pre-consolidation hard drop exists to prevent (a lone pinned
  # source otherwise wins its cell).
  lone <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1850, "WLD", "Coal", "Ember", 5
  )
  won_lone <- whep::consolidate_sources(
    lone,
    value_col = value,
    source_col = source,
    priority = c(OWID = 1L, Ember = 100L),
    .by = c("region", "category"),
    verbose = FALSE
  )
  testthat::expect_equal(nrow(won_lone), 0L)
})

# Measure penalty flips a contested cell; a lone demoted source still wins -----

testthat::test_that("measure demotion flips a contested cell but spares a lone reporter", {
  # Etemad reports oil PRODUCTION (measure-mismatched) at rank 1; OWID reports
  # consumption at rank 4. Without demotion Etemad's better rank would win.
  measure_basis <- tibble::tibble(source = "Etemad", category = "Oil")
  contested <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1960, "SAU", "Oil", "Etemad", 2.70,
    1960, "SAU", "Oil", "OWID", 0.036
  )

  # Control: no measure_basis -> Etemad (rank 1) wins.
  won_plain <- whep::consolidate_sources(
    contested,
    value_col = value,
    source_col = source,
    priority = c(Etemad = 1L, OWID = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )
  testthat::expect_equal(won_plain$source, "Etemad")

  # With measure_basis -> the penalty flips the contested cell to OWID.
  won <- whep::consolidate_sources(
    contested,
    value_col = value,
    source_col = source,
    priority = c(Etemad = 1L, OWID = 4L),
    .by = c("region", "category"),
    measure_basis = measure_basis,
    verbose = FALSE
  )
  testthat::expect_equal(won$source, "OWID")
  testthat::expect_equal(won$value, 0.036)
  testthat::expect_false(won$measure_demoted)
  testthat::expect_equal(won$effective_rank, 4L)

  # A lone demoted source still wins its own cell (no consumption alternative).
  lone <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "GBR", "Oil", "Etemad", 0.5
  )
  won_lone <- whep::consolidate_sources(
    lone,
    value_col = value,
    source_col = source,
    priority = c(Etemad = 1L, OWID = 4L),
    .by = c("region", "category"),
    measure_basis = measure_basis,
    verbose = FALSE
  )
  testthat::expect_equal(won_lone$source, "Etemad")
  testthat::expect_true(won_lone$measure_demoted)
  testthat::expect_equal(won_lone$effective_rank, 1001L)
})

# Exempt keys unaffected by the penalty ----------------------------------------

testthat::test_that("measure_exempt keys keep their base rank", {
  # At world level production equals consumption, so Etemad must not be demoted
  # and its rank-1 series wins the contested WLD cell.
  measure_basis <- tibble::tibble(source = "Etemad", category = "Oil")
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1960, "WLD", "Oil", "Etemad", 1.0,
    1960, "WLD", "Oil", "OWID", 1.1
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(Etemad = 1L, OWID = 4L),
    .by = c("region", "category"),
    measure_basis = measure_basis,
    measure_exempt = region == "WLD",
    verbose = FALSE
  )

  testthat::expect_equal(won$source, "Etemad")
  testthat::expect_false(won$measure_demoted)
})

# Continuity override ----------------------------------------------------------

testthat::test_that("continuity override reverts an isolated single-year flip", {
  # B (rank 1) wins only 1901, flanked by A (rank 4) winning 1900 and 1902.
  # A also reports 1901, so the isolated flip is reverted to A.
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "WLD", "Coal", "A", 10,
    1901, "WLD", "Coal", "A", 11,
    1901, "WLD", "Coal", "B", 99,
    1902, "WLD", "Coal", "A", 12
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(B = 1L, A = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )
  win_1901 <- won[won$year == 1901, ]
  testthat::expect_equal(win_1901$source, "A")
  testthat::expect_equal(win_1901$value, 11)

  # With the override off, B keeps the isolated 1901 cell.
  won_off <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(B = 1L, A = 4L),
    .by = c("region", "category"),
    continuity_override = FALSE,
    verbose = FALSE
  )
  testthat::expect_equal(won_off[won_off$year == 1901, ]$source, "B")
})

testthat::test_that("continuity override leaves a genuine two-year run in place", {
  # B (rank 1) wins 1901 AND 1902: a real switch, not a single-year tooth.
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "WLD", "Coal", "A", 10,
    1901, "WLD", "Coal", "A", 11,
    1901, "WLD", "Coal", "B", 98,
    1902, "WLD", "Coal", "A", 12,
    1902, "WLD", "Coal", "B", 99,
    1903, "WLD", "Coal", "A", 13
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(B = 1L, A = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )

  testthat::expect_equal(won[won$year == 1901, ]$source, "B")
  testthat::expect_equal(won[won$year == 1902, ]$source, "B")
})

# Deterministic output ordering ------------------------------------------------

testthat::test_that("output is ordered by .by then time regardless of input order", {
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1902, "WLD", "Coal", "A", 3,
    1900, "WLD", "Coal", "A", 1,
    1901, "ARG", "Coal", "A", 5,
    1901, "WLD", "Coal", "A", 2
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(A = 1L),
    .by = c("region", "category"),
    verbose = FALSE
  )

  testthat::expect_equal(won$region, c("ARG", "WLD", "WLD", "WLD"))
  testthat::expect_equal(won$year, c(1901, 1900, 1901, 1902))
  testthat::expect_equal(
    names(won),
    c(
      "year",
      "region",
      "category",
      "source",
      "value",
      "n_sources",
      "source_rank",
      "effective_rank",
      "measure_demoted"
    )
  )
})

# Global (no .by) consolidation ------------------------------------------------

testthat::test_that("consolidate_sources keys cells by time alone when .by is NULL", {
  panel <- tibble::tribble(
    ~year, ~source, ~value,
    1900, "OWID", 10,
    1900, "Malanima", 20,
    1901, "Malanima", 21
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(OWID = 1L, Malanima = 4L),
    .by = NULL,
    verbose = FALSE
  )

  testthat::expect_equal(nrow(won), 2L)
  testthat::expect_equal(won[won$year == 1900, ]$source, "OWID")
  testthat::expect_equal(won[won$year == 1901, ]$source, "Malanima")
})

# Duplicate rows abort ---------------------------------------------------------

testthat::test_that("consolidate_sources aborts on duplicate source-cell rows", {
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "WLD", "Coal", "OWID", 10,
    1900, "WLD", "Coal", "OWID", 12
  )

  testthat::expect_error(
    whep::consolidate_sources(
      panel,
      value_col = value,
      source_col = source,
      priority = c(OWID = 1L),
      .by = c("region", "category"),
      verbose = FALSE
    ),
    "one row per source per cell"
  )
})
