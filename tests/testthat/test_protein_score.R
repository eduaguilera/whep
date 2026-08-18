# The whole file is TRS 935 Table 6 (printed p.100), FAO's own worked example
# of the correct aggregation, transcribed from the report and reproduced here.
# It is the only independent check the tier 2 machinery can have until an amino
# acid composition table exists, and it exercises every load-bearing choice:
# digestible-protein weighting, the min() over amino acids, and truncation
# before digestibility rather than after.

# Wheat, chickpea and milk powder. Columns A, B, C-F, G of the printed table:
# weight (g), protein (g/100 g), the four amino acids (mg/g protein) and true
# digestibility. Protein supply is A * B / 100, the crude protein the table's
# column P is derived from.
.ps_table6_supply <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        1L,             400 * 13 / 100,
    2010L, 10L,        2L,             100 * 22 / 100,
    2010L, 10L,        3L,             35 * 34 / 100
  )
}

.ps_table6_digestibility <- function() {
  tibble::tribble(
    ~item_cbs_code, ~digestibility,
    1L,             0.85,
    2L,             0.80,
    3L,             0.95
  )
}

.ps_table6_amino_acids <- function() {
  tibble::tribble(
    ~item_cbs_code, ~amino_acid, ~mg_per_g_protein,
    1L,             "lysine",    25,
    1L,             "saa",       35,
    1L,             "threonine", 30,
    1L,             "tryptophan", 11,
    2L,             "lysine",    70,
    2L,             "saa",       25,
    2L,             "threonine", 42,
    2L,             "tryptophan", 13,
    3L,             "lysine",    80,
    3L,             "saa",       30,
    3L,             "threonine", 37,
    3L,             "tryptophan", 12
  )
}

# TRS 935 Table 6's own four reference patterns, printed with the example.
.ps_pattern <- function(lys, saa, thr, trp) {
  tibble::tribble(
    ~amino_acid,  ~mg_per_g_protein,
    "lysine",     lys,
    "saa",        saa,
    "threonine",  thr,
    "tryptophan", trp
  )
}

.ps_table6 <- function(pattern) {
  whep::build_protein_score(
    data = list(
      protein_supply = .ps_table6_supply(),
      digestibility = .ps_table6_digestibility(),
      amino_acids = .ps_table6_amino_acids(),
      pattern = pattern
    )
  )
}

testthat::test_that("TRS 935 Table 6 reproduces the printed intermediates", {
  # Digestible protein 73.1 g of 85.9 g crude, so a weighted digestibility of
  # 0.85; and an aggregate digestible profile of 44 / 32 / 34 / 12 mg per g.
  out <- .ps_table6(.ps_pattern(45, 22, 23, 6.0))
  testthat::expect_equal(round(out$digestibility, 2), 0.85)
  testthat::expect_equal(out$protein_scored_share, 1)
  # The lysine ratio is the score, so the printed 44 mg/g is recoverable from
  # it: score x reference = profile.
  testthat::expect_equal(round(out$amino_acid_score * 45, 0), 44)
  testthat::expect_equal(out$limiting_amino_acid, "lysine")
})

testthat::test_that("TRS 935 Table 6's printed totals reproduce exactly", {
  # These are the table's own intermediate columns, and they are what
  # reproduces to the printed precision. The digestible amino acid totals
  # (mg) and the aggregate profile (mg per g of digestible protein):
  #
  #   lysine 3241 -> 44   sulfur 2326 -> 32
  #   threonine 2483 -> 34   tryptophan 851 -> 12
  #
  # with digestible protein 73 g of 85.9 g crude.
  profile <- whep:::.ps_profile(
    whep:::.ps_digestible(.ps_table6_supply(), .ps_table6_digestibility()),
    .ps_table6_amino_acids()
  )
  totals <- stats::setNames(profile$amino_acid_mg, profile$amino_acid)
  mg_per_g <- stats::setNames(profile$profile_mg_per_g, profile$amino_acid)
  testthat::expect_equal(round(totals[["lysine"]]), 3241)
  testthat::expect_equal(round(totals[["saa"]]), 2326)
  testthat::expect_equal(round(totals[["threonine"]]), 2483)
  testthat::expect_equal(round(totals[["tryptophan"]]), 851)
  testthat::expect_equal(round(mg_per_g[["lysine"]]), 44)
  testthat::expect_equal(round(mg_per_g[["saa"]]), 32)
  testthat::expect_equal(round(mg_per_g[["threonine"]]), 34)
  testthat::expect_equal(round(mg_per_g[["tryptophan"]]), 12)
  testthat::expect_equal(round(unique(profile$digestible_sum)), 73)
})

testthat::test_that("lysine limits at every one of the four age patterns", {
  # The report's own point about cereal-based mixtures, and the one qualitative
  # result that holds at every pattern regardless of rounding.
  patterns <- list(
    .ps_pattern(45, 22, 23, 6.0),
    .ps_pattern(48, 23, 25, 6.5),
    .ps_pattern(52, 26, 27, 7.4),
    .ps_pattern(57, 28, 31, 8.5)
  )
  scores <- vapply(
    patterns,
    function(p) {
      out <- .ps_table6(p)
      testthat::expect_equal(out$limiting_amino_acid, "lysine")
      out$quality
    },
    numeric(1)
  )
  # A more demanding pattern can only lower the score.
  testthat::expect_true(all(diff(scores) < 0))
})

testthat::test_that("Table 6's printed PDCAAS reproduces where it is self-consistent", {
  # The printed score and PDCAAS columns are rounded to two decimals and are NOT
  # all self-consistent at that precision. Measured, from a profile of 44.339
  # mg/g and a digestibility of 0.851048:
  #
  #   adult      score 0.9853 -> 0.99   PDCAAS 0.83855 -> 0.84   printed 0.99 / 0.84
  #   older      score 0.9237 -> 0.92   PDCAAS 0.78614 -> 0.79   printed 0.93 / 0.79
  #   preschool  score 0.8527 -> 0.85   PDCAAS 0.72566 -> 0.73   printed 0.85 / 0.72
  #   infant     score 0.7779 -> 0.78   PDCAAS 0.66201 -> 0.66   printed 0.78 / 0.67
  #
  # Three of the eight printed cells disagree with exact arithmetic on the
  # table's own inputs, in both directions, so they are rounding artefacts of
  # the printing rather than a different method. The infant row is independently
  # suspect: the evidence record flags a printed typo in it, its label reading
  # "Infants (0-5 years)" while its pattern is Table 43's 0.5-year one. Only the
  # cells that follow from the printed inputs are asserted as golden; bending
  # the code to hit the others would be fitting to a misprint.
  adults <- .ps_table6(.ps_pattern(45, 22, 23, 6.0))
  older <- .ps_table6(.ps_pattern(48, 23, 25, 6.5))
  testthat::expect_equal(round(adults$amino_acid_score, 2), 0.99)
  testthat::expect_equal(round(adults$quality, 2), 0.84)
  testthat::expect_equal(round(older$quality, 2), 0.79)
  # The unrounded adult value, as a regression lock. The evidence record carries
  # 0.83848, which differs in the fourth decimal because it rounded the
  # intermediates; both round to the printed 0.84.
  testthat::expect_equal(adults$quality, 0.8385455, tolerance = 1e-6)
})

testthat::test_that("the profile is weighted by DIGESTIBLE protein", {
  # The correction TRS 935 makes to its own 1991 report: weighting the amino
  # acid content by crude protein gives 44.14 mg/g lysine against the digestible
  # weighting's 44.34. Small here, and the report says it "could be significant"
  # where digestibility varies markedly.
  out <- .ps_table6(.ps_pattern(45, 22, 23, 6.0))
  digestible_profile <- out$amino_acid_score * 45
  supply <- .ps_table6_supply()
  aa <- dplyr::filter(.ps_table6_amino_acids(), .data$amino_acid == "lysine")
  crude_profile <- sum(supply$protein_t * aa$mg_per_g_protein) /
    sum(supply$protein_t)
  testthat::expect_equal(round(digestible_profile, 2), 44.34)
  testthat::expect_equal(round(crude_profile, 2), 44.14)
  testthat::expect_false(isTRUE(all.equal(digestible_profile, crude_profile)))
})

testthat::test_that("truncation happens before digestibility, not after", {
  # TRS 935 truncates the SCORE at 1 and then multiplies by digestibility, so
  # the ceiling is the diet's digestibility. FNP 92 truncates the DIAAS itself,
  # so its ceiling is 1.0. For a diet that scores above 1 the two differ by the
  # whole digestibility penalty, and mixing them within a series is the error
  # this pins down.
  rich <- tibble::tribble(
    ~item_cbs_code, ~amino_acid, ~mg_per_g_protein,
    1L,             "lysine",    90
  )
  out <- whep::build_protein_score(
    data = list(
      protein_supply = tibble::tribble(
        ~year, ~area_code, ~item_cbs_code, ~protein_t,
        2010L, 10L,        1L,             100
      ),
      digestibility = tibble::tribble(
        ~item_cbs_code, ~digestibility,
        1L,             0.85
      ),
      amino_acids = rich,
      pattern = tibble::tribble(
        ~amino_acid, ~mg_per_g_protein,
        "lysine",    45
      )
    )
  )
  testthat::expect_equal(out$amino_acid_score, 2)
  # TRS 935: min(1, 2) * 0.85 = 0.85, the digestibility. NOT 1.0.
  testthat::expect_equal(out$quality, 0.85)
})

testthat::test_that("the limiting amino acid is named, not just the score", {
  # Swap the pattern so tryptophan binds instead of lysine: which acid limits is
  # the diagnostic, and a score alone cannot say.
  out <- .ps_table6(.ps_pattern(20, 22, 23, 40))
  testthat::expect_equal(out$limiting_amino_acid, "tryptophan")
})

testthat::test_that("items with no digestibility leave and are reported", {
  supply <- dplyr::bind_rows(
    .ps_table6_supply(),
    tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~protein_t,
      2010L, 10L,        99L,            85.9
    )
  )
  out <- whep::build_protein_score(
    data = list(
      protein_supply = supply,
      digestibility = .ps_table6_digestibility(),
      amino_acids = .ps_table6_amino_acids(),
      pattern = .ps_pattern(45, 22, 23, 6.0)
    )
  )
  testthat::expect_equal(out$protein_scored_share, 0.5)
  # The unscored item changes coverage but not the score of what was scored.
  testthat::expect_equal(out$quality, 0.8385455, tolerance = 1e-6)
})

testthat::test_that("a missing input aborts naming the table", {
  testthat::expect_error(
    whep::build_protein_score(data = list()),
    "protein_supply"
  )
  testthat::expect_error(
    whep::build_protein_score(
      data = list(protein_supply = .ps_table6_supply())
    ),
    "digestibility"
  )
  testthat::expect_error(
    whep::build_protein_score(
      data = list(
        protein_supply = .ps_table6_supply(),
        digestibility = .ps_table6_digestibility()
      )
    ),
    "amino_acids"
  )
})

testthat::test_that("the PACKAGED TRS 935 Table 5 digestibilities are locked", {
  d <- whep::whep_coef_table("protein_digestibility_trs935")
  testthat::expect_equal(nrow(d), 35L)
  testthat::expect_equal(sum(d$entry_type == "mixture"), 9L)
  testthat::expect_equal(sum(d$entry_type == "single_food"), 26L)
  testthat::expect_true(all(d$true_digestibility > 0.5))
  testthat::expect_true(all(d$true_digestibility <= 1))
  val <- function(nm) d$true_digestibility[d$source_name == nm]
  testthat::expect_equal(val("Egg"), 0.97)
  testthat::expect_equal(val("Meat, fish"), 0.94)
  testthat::expect_equal(val("Milk, cheese"), 0.95)
  testthat::expect_equal(val("Beans"), 0.78)
  testthat::expect_equal(val("Maize"), 0.85)
  testthat::expect_equal(val("Wheat, whole"), 0.86)
  # The milling spread CBS cannot observe: whole 86 against refined 96, and
  # three distinct maize rows. Do not collapse them.
  testthat::expect_equal(val("Wheat, refined"), 0.96)
  testthat::expect_equal(val("Corn, whole"), 0.87)
  testthat::expect_equal(val("Corn, cereal"), 0.70)
})

testthat::test_that("the wide packaged pattern is accepted as-is", {
  # build_protein_requirement() emits the age-weighted pattern in wide columns,
  # and TRS 935 Table 50 prints it that way. Reshaping should not be the
  # caller's job.
  wide <- tibble::tribble(
    ~lysine_mg_g, ~saa_mg_g, ~threonine_mg_g, ~tryptophan_mg_g,
    45,           22,        23,              6.0
  )
  out <- .ps_table6(wide)
  testthat::expect_equal(out$quality, 0.8385455, tolerance = 1e-6)
})

testthat::test_that("the default adult pattern is the packaged 19+ row", {
  out <- whep::build_protein_score(
    data = list(
      protein_supply = .ps_table6_supply(),
      digestibility = .ps_table6_digestibility(),
      amino_acids = .ps_table6_amino_acids()
    )
  )
  testthat::expect_equal(out$quality, 0.8385455, tolerance = 1e-6)
})

testthat::test_that("the age pattern matters: infants score far lower", {
  # 57 mg lysine per g protein against the adult 45, on the same diet. Using an
  # adult pattern for a young population understates the correction by a fifth.
  adult <- .ps_table6(.ps_pattern(45, 22, 23, 6.0))
  infant <- .ps_table6(.ps_pattern(57, 28, 31, 8.5))
  testthat::expect_lt(infant$quality, adult$quality)
  testthat::expect_equal(round(infant$quality / adult$quality, 2), 0.79)
})

testthat::test_that("a malformed pattern aborts", {
  testthat::expect_error(
    .ps_table6(tibble::tibble(lysine_mg_g = c(45, 48))),
    "one row|reference pattern"
  )
  testthat::expect_error(
    .ps_table6(tibble::tibble(nonsense = 1)),
    "reference pattern"
  )
})
