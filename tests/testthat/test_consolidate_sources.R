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

# NA-valued high-rank row never beats a real lower-rank value ------------------

testthat::test_that("a rank-1 NA does not beat a rank-4 real observation", {
  # OWID (rank 1) reports year 2000 as NA; Malanima (rank 4) reports 20. The
  # consolidated cell must hold Malanima's real 20, not OWID's NA.
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    2000, "WLD", "Coal", "OWID", NA_real_,
    2000, "WLD", "Coal", "Malanima", 20
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
  testthat::expect_equal(won$source, "Malanima")
  testthat::expect_equal(won$value, 20)
  testthat::expect_equal(won$source_rank, 4L)

  # A cell wins NA only when every source is missing there.
  all_na <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    2001, "WLD", "Coal", "OWID", NA_real_,
    2001, "WLD", "Coal", "Malanima", NA_real_
  )
  won_na <- whep::consolidate_sources(
    all_na,
    value_col = value,
    source_col = source,
    priority = c(OWID = 1L, Malanima = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )
  testthat::expect_equal(nrow(won_na), 1L)
  testthat::expect_true(is.na(won_na$value))
})

testthat::test_that("continuity override does not reinstate an NA neighbour", {
  # B (rank 1) wins 1901; A (rank 4) flanks 1900 and 1902 but reports 1901 as
  # NA. Continuity must not smooth 1901 back to A's NA, so B keeps the cell.
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "WLD", "Coal", "A", 10,
    1901, "WLD", "Coal", "A", NA_real_,
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
  testthat::expect_equal(win_1901$source, "B")
  testthat::expect_equal(win_1901$value, 99)
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
    tie_break = list(coverage = FALSE),
    continuity_override = FALSE,
    verbose = FALSE
  )
  testthat::expect_equal(
    won_no_cov[won_no_cov$year == 1901, ]$source,
    "Malanima"
  )
})

# Positive-only coverage -------------------------------------------------------

# A mostly-zero series and a shorter all-positive one, both rank 4, contesting
# 1902. Non-missing coverage counts the zeros (5 > 3) and picks the zero-padded
# source; strictly-positive coverage counts only real quantities (3 > 1) and
# picks the shorter one. The source names are chosen so that in each direction
# the expected winner is NOT the one ascending name order would pick, which
# rules out the name-order fallback deciding the cell.
zero_padded_panel <- function(wide_source, narrow_source) {
  tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "GBR", "Biomass", wide_source, 0,
    1901, "GBR", "Biomass", wide_source, 0,
    1902, "GBR", "Biomass", wide_source, 5,
    1903, "GBR", "Biomass", wide_source, 0,
    1904, "GBR", "Biomass", wide_source, 0,
    1901, "GBR", "Biomass", narrow_source, 7,
    1902, "GBR", "Biomass", narrow_source, 8,
    1903, "GBR", "Biomass", narrow_source, 9
  )
}

consolidate_zero_padded <- function(panel, coverage) {
  whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(Zeta = 4L, Beta = 4L, Alpha = 4L, Zulu = 4L),
    .by = c("region", "category"),
    tie_break = list(coverage = coverage),
    verbose = FALSE
  )
}

testthat::test_that("coverage counts non-missing cells by default", {
  # Zeta pads four zero-years around a single real 1902 value; Beta reports
  # three real years. Non-missing coverage is 5 vs 3, so Zeta takes 1902 even
  # though ascending name order would have picked Beta.
  panel <- zero_padded_panel("Zeta", "Beta")

  default <- consolidate_zero_padded(panel, coverage = TRUE)
  win_1902 <- default[default$year == 1902, ]
  testthat::expect_equal(win_1902$source, "Zeta")
  testthat::expect_equal(win_1902$value, 5)

  # `TRUE` and "nonmissing" are the same option spelled two ways.
  spelled <- consolidate_zero_padded(panel, coverage = "nonmissing")
  testthat::expect_equal(spelled, default)

  # Omitting `tie_break` entirely must match the explicit default.
  implicit <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(Zeta = 4L, Beta = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )
  testthat::expect_equal(implicit, default)
})

testthat::test_that("coverage = 'positive' flips a tie the zeros would decide", {
  # Same shape, names swapped so the strictly-positive winner is the one
  # ascending name order would NOT pick: Alpha pads zeros, Zulu reports three
  # real years. Positive coverage is 1 vs 3, so Zulu takes 1902.
  panel <- zero_padded_panel("Alpha", "Zulu")

  nonmissing <- consolidate_zero_padded(panel, coverage = TRUE)
  testthat::expect_equal(nonmissing[nonmissing$year == 1902, ]$source, "Alpha")

  positive <- consolidate_zero_padded(panel, coverage = "positive")
  win_1902 <- positive[positive$year == 1902, ]
  testthat::expect_equal(win_1902$source, "Zulu")
  testthat::expect_equal(win_1902$value, 8)

  # The zero-padded source still wins the years only it reports: positive
  # coverage changes the tie-break, not which cells exist.
  testthat::expect_equal(positive[positive$year == 1900, ]$source, "Alpha")
  testthat::expect_equal(positive[positive$year == 1900, ]$value, 0)
})

testthat::test_that("coverage option is validated", {
  panel <- zero_padded_panel("Alpha", "Zulu")

  testthat::expect_error(
    consolidate_zero_padded(panel, coverage = "nonzero"),
    "must be"
  )
  testthat::expect_error(
    consolidate_zero_padded(panel, coverage = c("positive", "positive")),
    "must be"
  )

  # Strict positivity is undefined for a non-numeric value column.
  chr_panel <- panel
  chr_panel$value <- as.character(chr_panel$value)
  testthat::expect_error(
    consolidate_zero_padded(chr_panel, coverage = "positive"),
    "numeric value column"
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
    tie_break = list(quality_col = "tier", quality_levels = tier_levels),
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
    measure = list(basis = measure_basis),
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
    measure = list(basis = measure_basis),
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
    measure = list(basis = measure_basis, exempt = ~ region == "WLD"),
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

testthat::test_that("continuity never reverts to a measure-demoted source", {
  # Etemad (rank 1, measure-flagged) wins 1900 and 1902 as a lone demoted
  # reporter. OWID (rank 4, measure-consistent) wins the contested 1901 via
  # the penalty. The isolated 1901 flip must NOT be smoothed back to Etemad:
  # continuity never undoes the measure penalty.
  measure_basis <- tibble::tibble(source = "Etemad", category = "Oil")
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "SAU", "Oil", "Etemad", 2.60,
    1901, "SAU", "Oil", "Etemad", 2.70,
    1901, "SAU", "Oil", "OWID", 0.036,
    1902, "SAU", "Oil", "Etemad", 2.80
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(Etemad = 1L, OWID = 4L),
    .by = c("region", "category"),
    measure = list(basis = measure_basis),
    verbose = FALSE
  )

  win_1901 <- won[won$year == 1901, ]
  testthat::expect_equal(win_1901$source, "OWID")
  testthat::expect_equal(win_1901$value, 0.036)
  testthat::expect_false(win_1901$measure_demoted)
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

# Verbose tie reporting ---------------------------------------------------------

testthat::test_that("verbose mode reports name-order tie resolution", {
  # A and B tie on rank, coverage, and (absent) quality in the single 1900
  # cell, so ascending source name decides and verbose mode must say so.
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "WLD", "Coal", "A", 10,
    1900, "WLD", "Coal", "B", 20
  )

  msgs <- testthat::capture_messages(
    won <- whep::consolidate_sources(
      panel,
      value_col = value,
      source_col = source,
      priority = c(A = 4L, B = 4L),
      .by = c("region", "category"),
      verbose = TRUE
    )
  )

  testthat::expect_true(any(grepl("name resolved 1 cell tie", msgs)))
  testthat::expect_equal(won$source, "A")
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
      "measure_demoted",
      "method_source"
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

# Per-source quality variants --------------------------------------------------

# BOKU contributes two data-tier variants of the same 1919 cell (an Estimated
# and an Interpolated figure), the pattern the energy-hist panel carries into
# its own tier tie-break. IEA reports the cell once at a worse priority.
variant_panel <- tibble::tribble(
  ~year, ~region, ~category, ~source, ~value, ~tier,
  1919, "AUT", "Food_feed", "BOKU", 1.5, "Estimated",
  1919, "AUT", "Food_feed", "BOKU", 9.9, "Interpolated",
  1919, "AUT", "Food_feed", "IEA", 3.0, "Observed"
)

consolidate_variants <- function(panel, levels = tier_levels, ...) {
  whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(BOKU = 2L, IEA = 5L),
    .by = c("region", "category"),
    tie_break = list(
      quality_col = "tier",
      quality_levels = levels,
      quality_variants = TRUE
    ),
    ...
  )
}

testthat::test_that("quality_variants keeps a source's best tier variant", {
  won <- consolidate_variants(variant_panel, verbose = FALSE)

  # BOKU outranks IEA, and its Estimated variant outranks its Interpolated one.
  testthat::expect_equal(nrow(won), 1L)
  testthat::expect_equal(won$source, "BOKU")
  testthat::expect_equal(won$tier, "Estimated")
  testthat::expect_equal(won$value, 1.5)
  # The collapsed variant must not inflate the contesting-source count.
  testthat::expect_equal(won$n_sources, 2L)

  # The variant is chosen by `quality_levels` order, not by row order: reversing
  # the ordering hands the cell to the Interpolated row.
  reversed <- consolidate_variants(
    variant_panel,
    levels = rev(tier_levels),
    verbose = FALSE
  )
  testthat::expect_equal(reversed$tier, "Interpolated")
  testthat::expect_equal(reversed$value, 9.9)
})

testthat::test_that("quality_variants is opt-in and off by default", {
  # The same panel aborts under the default: a source with two rows in one cell
  # is a double-count until the caller says otherwise.
  testthat::expect_error(
    whep::consolidate_sources(
      variant_panel,
      value_col = value,
      source_col = source,
      priority = c(BOKU = 2L, IEA = 5L),
      .by = c("region", "category"),
      tie_break = list(quality_col = "tier", quality_levels = tier_levels),
      verbose = FALSE
    ),
    "one row per source per cell"
  )

  # It also needs a quality column to resolve the variants with.
  testthat::expect_error(
    whep::consolidate_sources(
      variant_panel,
      value_col = value,
      source_col = source,
      priority = c(BOKU = 2L, IEA = 5L),
      .by = c("region", "category"),
      tie_break = list(quality_variants = TRUE),
      verbose = FALSE
    ),
    "requires `tie_break\\$quality_col`"
  )
})

testthat::test_that("quality_variants still aborts on unresolvable duplicates", {
  # Two rows sharing source, cell AND tier are a true duplicate, not a variant.
  same_tier <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value, ~tier,
    1919, "AUT", "Food_feed", "BOKU", 1.5, "Estimated",
    1919, "AUT", "Food_feed", "BOKU", 9.9, "Estimated"
  )
  testthat::expect_error(
    consolidate_variants(same_tier, verbose = FALSE),
    "one row per source per cell and quality level"
  )

  # Two variants whose tiers are both outside `quality_levels` tie for best
  # rank, and resolving that would need a tie-break the caller never stated.
  unlisted <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value, ~tier,
    1919, "AUT", "Food_feed", "BOKU", 1.5, "Guessed",
    1919, "AUT", "Food_feed", "BOKU", 9.9, "Assumed"
  )
  testthat::expect_error(
    consolidate_variants(unlisted, verbose = FALSE),
    "tie on best quality rank"
  )
})

testthat::test_that("verbose mode reports resolved quality variants", {
  msgs <- testthat::capture_messages(
    consolidate_variants(variant_panel, verbose = TRUE)
  )
  testthat::expect_true(any(grepl("Resolved 1 per-source cell variant", msgs)))
})

testthat::test_that("positive coverage counts the surviving best variant", {
  # Pins the ordering of the two options: variants are resolved BEFORE coverage
  # is counted. BOKU's 1919 coverage therefore comes from the Estimated 0 that
  # wins its variant contest, not from the Interpolated 4 that loses it, so
  # BOKU covers one positive year against Zed's two and Zed takes 1919 --
  # against the ascending name order, which would have picked BOKU.
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value, ~tier,
    1919, "AUT", "Food_feed", "BOKU", 0, "Estimated",
    1919, "AUT", "Food_feed", "BOKU", 4, "Interpolated",
    1920, "AUT", "Food_feed", "BOKU", 3, "Estimated",
    1919, "AUT", "Food_feed", "Zed", 9, "Observed",
    1920, "AUT", "Food_feed", "Zed", 8, "Observed"
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(BOKU = 4L, Zed = 4L),
    .by = c("region", "category"),
    tie_break = list(
      coverage = "positive",
      quality_col = "tier",
      quality_levels = tier_levels,
      quality_variants = TRUE
    ),
    verbose = FALSE
  )

  win_1919 <- won[won$year == 1919, ]
  testthat::expect_equal(win_1919$source, "Zed")
  testthat::expect_equal(win_1919$value, 9)
  testthat::expect_equal(win_1919$n_sources, 2L)
})

testthat::test_that("quality_variants is inert on a panel without variants", {
  # Turning the option on must not change a panel that already holds one row
  # per source per cell: the resolution only ever drops losing variants.
  panel <- variant_panel[variant_panel$tier != "Interpolated", ]

  opted_in <- consolidate_variants(panel, verbose = FALSE)
  plain <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(BOKU = 2L, IEA = 5L),
    .by = c("region", "category"),
    tie_break = list(quality_col = "tier", quality_levels = tier_levels),
    verbose = FALSE
  )

  testthat::expect_equal(opted_in, plain)
})

# Scoped priority (issue 393) --------------------------------------------------

# One source pinned above the other inside a single category. The same two
# sources contest both categories, so whatever the scope does in Coal it must
# not do in Oil -- which is the whole point of scoping rather than restating
# the source's global rank.
scoped_panel <- tibble::tribble(
  ~year, ~region, ~category, ~source, ~value,
  1900, "WLD", "Coal", "OWID", 10,
  1900, "WLD", "Coal", "Smil", 20,
  1900, "WLD", "Oil", "OWID", 30,
  1900, "WLD", "Oil", "Smil", 40
)

scoped_priority <- tibble::tribble(
  ~source, ~category, ~rank,
  "OWID", NA_character_, 1L,
  "Smil", NA_character_, 4L,
  "Smil", "Coal", 0L
)

testthat::test_that("a scoped priority entry outranks its source's own tier", {
  won <- whep::consolidate_sources(
    scoped_panel,
    value_col = value,
    source_col = source,
    priority = scoped_priority,
    .by = c("region", "category"),
    verbose = FALSE
  )

  coal <- won[won$category == "Coal", ]
  oil <- won[won$category == "Oil", ]

  # Scoped rank 0 wins Coal; the unscoped rank 4 still loses Oil.
  testthat::expect_equal(coal$source, "Smil")
  testthat::expect_equal(coal$value, 20)
  testthat::expect_equal(coal$source_rank, 0L)
  testthat::expect_equal(coal$method_source, "priority_scoped")
  testthat::expect_equal(oil$source, "OWID")
  testthat::expect_equal(oil$value, 30)
  testthat::expect_equal(oil$source_rank, 1L)
  testthat::expect_equal(oil$method_source, "priority")
})

testthat::test_that("priority_scope = 'source' reads the table unscoped", {
  # The sensitivity run: the same table with its scoped entries ignored is
  # exactly what it expressed before scoping existed, so Coal reverts to OWID.
  unscoped <- whep::consolidate_sources(
    scoped_panel,
    value_col = value,
    source_col = source,
    priority = scoped_priority,
    .by = c("region", "category"),
    priority_scope = "source",
    verbose = FALSE
  )

  testthat::expect_equal(unscoped$source, c("OWID", "OWID"))
  testthat::expect_equal(unscoped$value, c(10, 30))
  testthat::expect_equal(unique(unscoped$method_source), "priority")

  # And it matches what the plain named vector of the unscoped ranks gives.
  plain <- whep::consolidate_sources(
    scoped_panel,
    value_col = value,
    source_col = source,
    priority = c(OWID = 1L, Smil = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )
  testthat::expect_equal(unscoped, plain)
})

testthat::test_that("priority_scope rejects an unknown value", {
  testthat::expect_error(
    whep::consolidate_sources(
      scoped_panel,
      value_col = value,
      source_col = source,
      priority = scoped_priority,
      .by = c("region", "category"),
      priority_scope = "category",
      verbose = FALSE
    ),
    class = "rlang_error"
  )
})

testthat::test_that("a wider priority table must name its rank column", {
  # Silently taking column 2 as the rank and discarding the rest is how a
  # scope key becomes a published number from the wrong source.
  unnamed <- tibble::tribble(
    ~src, ~rnk, ~category,
    "OWID", 1L, NA_character_,
    "Smil", 4L, NA_character_
  )

  testthat::expect_error(
    whep::consolidate_sources(
      scoped_panel,
      value_col = value,
      source_col = source,
      priority = unnamed,
      .by = c("region", "category"),
      verbose = FALSE
    ),
    "scoped `priority` table needs"
  )
})

testthat::test_that("a priority key column absent from data aborts", {
  bad <- tibble::tribble(
    ~source, ~sector, ~rank,
    "OWID", NA_character_, 1L,
    "Smil", "Industry", 0L
  )

  testthat::expect_error(
    whep::consolidate_sources(
      scoped_panel,
      value_col = value,
      source_col = source,
      priority = bad,
      .by = c("region", "category"),
      verbose = FALSE
    ),
    "key column"
  )
})

testthat::test_that("duplicate priority entries abort rather than pick one", {
  dup <- tibble::tribble(
    ~source, ~category, ~rank,
    "OWID", NA_character_, 1L,
    "OWID", NA_character_, 9L,
    "Smil", NA_character_, 4L
  )

  testthat::expect_error(
    whep::consolidate_sources(
      scoped_panel,
      value_col = value,
      source_col = source,
      priority = dup,
      .by = c("region", "category"),
      verbose = FALSE
    ),
    "more than one rank"
  )
})

testthat::test_that("equally specific priority entries that disagree abort", {
  # `Smil` x Coal and `Smil` x WLD are both one key deep and give different
  # ranks, so the winner would depend on the order of the priority table.
  ambiguous <- tibble::tribble(
    ~source, ~category, ~region, ~rank,
    "OWID", NA_character_, NA_character_, 1L,
    "Smil", "Coal", NA_character_, 0L,
    "Smil", NA_character_, "WLD", 3L
  )

  testthat::expect_error(
    whep::consolidate_sources(
      scoped_panel,
      value_col = value,
      source_col = source,
      priority = ambiguous,
      .by = c("region", "category"),
      verbose = FALSE
    ),
    "equally specific"
  )
})

testthat::test_that("the most specific matching priority entry wins", {
  # Two keys deep beats one key deep, so no abort and the deepest rank applies.
  nested <- tibble::tribble(
    ~source, ~category, ~region, ~rank,
    "OWID", NA_character_, NA_character_, 1L,
    "Smil", NA_character_, NA_character_, 4L,
    "Smil", "Coal", NA_character_, 3L,
    "Smil", "Coal", "WLD", 0L
  )

  won <- whep::consolidate_sources(
    scoped_panel,
    value_col = value,
    source_col = source,
    priority = nested,
    .by = c("region", "category"),
    verbose = FALSE
  )

  coal <- won[won$category == "Coal", ]
  testthat::expect_equal(coal$source, "Smil")
  testthat::expect_equal(coal$source_rank, 0L)
})

testthat::test_that("an empty scoped priority table falls back for every row", {
  # Nothing matches, so every source takes the documented `drop_at - 1L`
  # fallback rather than erroring on a table with no rows to index by.
  empty <- tibble::tibble(
    source = character(0),
    category = character(0),
    rank = integer(0)
  )

  won <- whep::consolidate_sources(
    scoped_panel,
    value_col = value,
    source_col = source,
    priority = empty,
    .by = c("region", "category"),
    drop_at = 10L,
    verbose = FALSE
  )

  testthat::expect_equal(nrow(won), 2L)
  testthat::expect_equal(unique(won$source_rank), 9L)
  # All ranks equal, so the name order settles both cells.
  testthat::expect_equal(won$source, c("OWID", "OWID"))
})

testthat::test_that("verbose reports how many rows took a scoped rank", {
  testthat::expect_message(
    whep::consolidate_sources(
      scoped_panel,
      value_col = value,
      source_col = source,
      priority = scoped_priority,
      .by = c("region", "category"),
      verbose = TRUE
    ),
    "scope-specific priority rank"
  )
})

# Continuity exemption (issue 393) ---------------------------------------------

# Anchor is a 20-year milestone grid meant to be interpolated between; Annual
# reports every year at a lower priority. Every anchor sits inside Annual's run
# and so reads as an isolated single-year flip.
anchor_panel <- tibble::tribble(
  ~year, ~region, ~category, ~source, ~value,
  1900, "WLD", "Coal", "Annual", 1,
  1900, "WLD", "Coal", "Anchor", 100,
  1901, "WLD", "Coal", "Annual", 2,
  1902, "WLD", "Coal", "Annual", 3,
  1903, "WLD", "Coal", "Annual", 4,
  1904, "WLD", "Coal", "Annual", 5,
  1905, "WLD", "Coal", "Annual", 6,
  1905, "WLD", "Coal", "Anchor", 105,
  1906, "WLD", "Coal", "Annual", 7,
  1907, "WLD", "Coal", "Annual", 8
)

testthat::test_that("continuity strips a sparse source's anchors by default", {
  won <- whep::consolidate_sources(
    anchor_panel,
    value_col = value,
    source_col = source,
    priority = c(Anchor = 1L, Annual = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )

  # 1900 is the series start, so it has no preceding period and survives; 1905
  # sits inside Annual's run, is read as an isolated flip and is reverted.
  testthat::expect_equal(won$source[won$year == 1905], "Annual")
  testthat::expect_equal(won$value[won$year == 1905], 6)
})

testthat::test_that("continuity exempt keeps a sparse source's anchors", {
  won <- whep::consolidate_sources(
    anchor_panel,
    value_col = value,
    source_col = source,
    priority = c(Anchor = 1L, Annual = 4L),
    .by = c("region", "category"),
    continuity_override = list(exempt = ~ source == "Anchor"),
    verbose = FALSE
  )

  testthat::expect_equal(won$source[won$year == 1905], "Anchor")
  testthat::expect_equal(won$value[won$year == 1905], 105)
  testthat::expect_equal(won$method_source[won$year == 1905], "priority")
  # The exemption changes nothing where the override was not going to fire.
  testthat::expect_equal(won$source[won$year == 1903], "Annual")
})

testthat::test_that("continuity adjacency chooses what counts as flanking", {
  # A half-step time axis: the flanking periods sit 0.5 apart, so "step" sees
  # no adjacency and leaves the flip, while "within" reverts it.
  fine <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900.0, "WLD", "Coal", "A", 1,
    1900.5, "WLD", "Coal", "A", 2,
    1900.5, "WLD", "Coal", "B", 99,
    1901.0, "WLD", "Coal", "A", 3
  )

  step <- whep::consolidate_sources(
    fine,
    value_col = value,
    source_col = source,
    priority = c(B = 1L, A = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )
  within <- whep::consolidate_sources(
    fine,
    value_col = value,
    source_col = source,
    priority = c(B = 1L, A = 4L),
    .by = c("region", "category"),
    continuity_override = list(adjacency = "within"),
    verbose = FALSE
  )

  testthat::expect_equal(step$source[step$year == 1900.5], "B")
  testthat::expect_equal(within$source[within$year == 1900.5], "A")
  testthat::expect_equal(
    within$method_source[within$year == 1900.5],
    "continuity"
  )
})

testthat::test_that("continuity_override validates its options", {
  testthat::expect_error(
    whep::consolidate_sources(
      anchor_panel,
      value_col = value,
      source_col = source,
      priority = c(Anchor = 1L, Annual = 4L),
      .by = c("region", "category"),
      continuity_override = list(adjacency = "loose"),
      verbose = FALSE
    ),
    class = "rlang_error"
  )

  testthat::expect_error(
    whep::consolidate_sources(
      anchor_panel,
      value_col = value,
      source_col = source,
      priority = c(Anchor = 1L, Annual = 4L),
      .by = c("region", "category"),
      continuity_override = "yes",
      verbose = FALSE
    ),
    "must be"
  )

  testthat::expect_error(
    whep::consolidate_sources(
      anchor_panel,
      value_col = value,
      source_col = source,
      priority = c(Anchor = 1L, Annual = 4L),
      .by = c("region", "category"),
      continuity_override = list(exempt = "source == 'Anchor'"),
      verbose = FALSE
    ),
    "one-sided formula"
  )
})

# Coverage grain (issue 393) ---------------------------------------------------

# Broad and Thin are the same rank. Inside the contested subcategory Thin
# covers more years, but across the whole category Broad covers far more, so
# the two grains decide the tie differently.
grain_panel <- tibble::tribble(
  ~year, ~region, ~category, ~subcategory, ~source, ~value,
  1900, "WLD", "Coal", "Hard", "Broad", 1,
  1901, "WLD", "Coal", "Hard", "Broad", 2,
  1900, "WLD", "Coal", "Soft", "Broad", 3,
  1901, "WLD", "Coal", "Soft", "Broad", 4,
  1902, "WLD", "Coal", "Soft", "Broad", 5,
  1903, "WLD", "Coal", "Soft", "Broad", 6,
  1900, "WLD", "Coal", "Hard", "Thin", 99,
  1901, "WLD", "Coal", "Hard", "Thin", 98,
  1902, "WLD", "Coal", "Hard", "Thin", 97
)

testthat::test_that("coverage_by counts coverage at a coarser grain", {
  cell_grain <- whep::consolidate_sources(
    grain_panel,
    value_col = value,
    source_col = source,
    priority = c(Broad = 4L, Thin = 4L),
    .by = c("region", "category", "subcategory"),
    continuity_override = FALSE,
    verbose = FALSE
  )
  coarse <- whep::consolidate_sources(
    grain_panel,
    value_col = value,
    source_col = source,
    priority = c(Broad = 4L, Thin = 4L),
    .by = c("region", "category", "subcategory"),
    tie_break = list(coverage_by = c("region", "category")),
    continuity_override = FALSE,
    verbose = FALSE
  )

  hard <- function(won) won[won$subcategory == "Hard" & won$year == 1900, ]

  # At the cell grain Thin covers 3 Hard years against Broad's 2 and wins.
  testthat::expect_equal(hard(cell_grain)$source, "Thin")
  testthat::expect_equal(hard(cell_grain)$method_source, "coverage")
  # At the category grain Broad covers 6 cells against Thin's 3 and wins.
  testthat::expect_equal(hard(coarse)$source, "Broad")
  testthat::expect_equal(hard(coarse)$method_source, "coverage")
})

testthat::test_that("coverage_by = character(0) counts across the panel", {
  won <- whep::consolidate_sources(
    grain_panel,
    value_col = value,
    source_col = source,
    priority = c(Broad = 4L, Thin = 4L),
    .by = c("region", "category", "subcategory"),
    tie_break = list(coverage_by = character(0)),
    continuity_override = FALSE,
    verbose = FALSE
  )

  hard <- won[won$subcategory == "Hard" & won$year == 1900, ]
  testthat::expect_equal(hard$source, "Broad")
})

testthat::test_that("coverage_by must be a subset of .by", {
  testthat::expect_error(
    whep::consolidate_sources(
      grain_panel,
      value_col = value,
      source_col = source,
      priority = c(Broad = 4L, Thin = 4L),
      .by = c("region", "category", "subcategory"),
      tie_break = list(coverage_by = c("region", "year")),
      verbose = FALSE
    ),
    "not in `.by`"
  )

  testthat::expect_error(
    whep::consolidate_sources(
      grain_panel,
      value_col = value,
      source_col = source,
      priority = c(Broad = 4L, Thin = 4L),
      .by = c("region", "category", "subcategory"),
      tie_break = list(coverage_by = 1L),
      verbose = FALSE
    ),
    "character vector"
  )
})

# method_source provenance (issue 393) -----------------------------------------

testthat::test_that("method_source names the stage that decided each cell", {
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~tier, ~value,
    # Only one source reports 1900.
    1900, "WLD", "Coal", "A", "Observed", 1,
    # The higher-ranked source reports 1901 as NA.
    1901, "WLD", "Coal", "A", "Observed", 2,
    1901, "WLD", "Coal", "B", "Observed", NA_real_,
    # Plain rank decides 1902.
    1902, "WLD", "Coal", "A", "Observed", 3,
    1902, "WLD", "Coal", "C", "Observed", 30,
    # Equal rank and equal coverage, decided by quality tier, at 1903.
    1903, "WLD", "Coal", "D", "Observed", 4,
    1903, "WLD", "Coal", "E", "Modeled", 40
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(B = 1L, C = 1L, A = 4L, D = 4L, E = 4L),
    .by = c("region", "category"),
    tie_break = list(
      coverage = FALSE,
      quality_col = "tier",
      quality_levels = tier_levels
    ),
    continuity_override = FALSE,
    verbose = FALSE
  )

  testthat::expect_equal(
    won$method_source,
    c("sole_source", "nonmissing", "priority", "quality")
  )
  testthat::expect_equal(won$source, c("A", "A", "C", "D"))
})

testthat::test_that("method_source reports a name-order tie and a reversion", {
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "WLD", "Coal", "Aaa", 1,
    1900, "WLD", "Coal", "Bbb", 2
  )

  won <- whep::consolidate_sources(
    panel,
    value_col = value,
    source_col = source,
    priority = c(Aaa = 4L, Bbb = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )
  testthat::expect_equal(won$method_source, "name_order")

  flip <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value,
    1900, "WLD", "Coal", "A", 10,
    1901, "WLD", "Coal", "A", 11,
    1901, "WLD", "Coal", "B", 99,
    1902, "WLD", "Coal", "A", 12
  )
  reverted <- whep::consolidate_sources(
    flip,
    value_col = value,
    source_col = source,
    priority = c(B = 1L, A = 4L),
    .by = c("region", "category"),
    verbose = FALSE
  )
  testthat::expect_equal(
    reverted$method_source[reverted$year == 1901],
    "continuity"
  )
})

testthat::test_that("method_source is a reserved output column", {
  panel <- tibble::tribble(
    ~year, ~region, ~category, ~source, ~value, ~method_source,
    1900, "WLD", "Coal", "A", 1, "mine"
  )

  testthat::expect_error(
    whep::consolidate_sources(
      panel,
      value_col = value,
      source_col = source,
      priority = c(A = 1L),
      .by = c("region", "category"),
      verbose = FALSE
    ),
    "reserved output column"
  )
})
