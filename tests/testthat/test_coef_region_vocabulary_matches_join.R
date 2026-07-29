# Every coefficient table keyed by a region label is a silent-drop point. The file
# carries one vocabulary, whep::regions_full carries another, the pipeline joins
# them, and a mismatch produces NA coefficients that a fallback then papers over.
# Nothing structural prevents it, and it has already happened once.
#
# There are exactly three such columns across the 22 coefficient tables, found by
# sweeping every table rather than by guessing which ones to look at:
#
#   modern_variety_adoption  region_hanpp      8 values, all in region_HANPP
#   residue_krausmann        region_krausmann  8 values, all in region_HANPP
#   residue_feed_fraction    region_hanpp     17 values, 0 in region_HANPP,
#                                             14 in region_UN_sub
#
# The invariant that separates the healthy two from the broken one: THE
# regions_full COLUMN A FILE BEST MATCHES MUST BE THE COLUMN THE PIPELINE JOINS IT
# AGAINST. residue_feed_fraction violates exactly that -- its best match is
# region_UN_sub while the join supplies region_HANPP, so every row falls through to
# the "Global" default of 0.20 and a ninefold spread of published coefficients
# (0.05 to 0.45) is discarded. See whep#405 and
# test_residue_feed_region_vocabulary.R, which pins that specific case.
#
# residue_krausmann is the interesting healthy case, and the reason this test states
# the join target explicitly instead of inferring it from the column NAME. Its
# column is called region_krausmann but holds HANPP labels, which looks like the
# same defect. It is not: .residue_recovery_region() deliberately translates
# Krausmann labels to HANPP labels before the join, so the file matches what the
# code actually supplies. A name-based check would flag it, and a reader trusting
# that flag would "fix" a correct file.
#
# This test exists for the fourth table, not the three below. A coefficient file
# added with a region column whose vocabulary belongs to some other taxonomy will
# fail here on the day it is added, rather than after someone eventually wonders why
# a coefficient never varies.
coef_region_joins <- list(
  list(
    table = "modern_variety_adoption",
    column = "region_hanpp",
    joined_against = "region_HANPP",
    known_mismatch = FALSE
  ),
  list(
    table = "residue_krausmann",
    column = "region_krausmann",
    joined_against = "region_HANPP",
    known_mismatch = FALSE
  ),
  list(
    table = "residue_feed_fraction",
    column = "region_hanpp",
    joined_against = "region_HANPP",
    # Baselined, not tolerated: the assertion below still runs and still names the
    # taxonomy the file really uses, so the shape of the defect cannot drift
    # unnoticed while whep#405 is open.
    known_mismatch = TRUE,
    actual_taxonomy = "region_UN_sub"
  )
)

# Which regions_full column a set of labels most nearly belongs to. Returns the
# column name and the overlap, so a caller can distinguish "matches nothing" from
# "matches the wrong one".
best_regions_full_match <- function(values) {
  r <- as.data.frame(whep::regions_full)
  candidates <- grep("^region|^ADB", names(r), value = TRUE)
  overlaps <- vapply(
    candidates,
    function(k) {
      length(intersect(
        values,
        unique(stats::na.omit(as.character(r[[k]])))
      ))
    },
    integer(1)
  )
  list(
    column = if (max(overlaps) == 0L) {
      NA_character_
    } else {
      names(
        which.max(overlaps)
      )
    },
    overlap = max(overlaps),
    n = length(values)
  )
}

testthat::test_that("every region-keyed coefficient table is joined on the taxonomy it uses", {
  # Sentinels that are not territories in any taxonomy.
  sentinels <- c("Global", "global", "World", "RoW")

  for (spec in coef_region_joins) {
    d <- as.data.frame(whep::whep_coef_table(spec$table))
    testthat::expect_true(
      spec$column %in% names(d),
      info = paste0(spec$table, " has no column ", spec$column)
    )

    values <- setdiff(
      sort(unique(stats::na.omit(as.character(d[[spec$column]])))),
      sentinels
    )
    # Non-vacuous: an emptied column would make every overlap zero and the whole
    # loop would pass while comparing nothing.
    testthat::expect_gt(length(values), 2L)

    best <- best_regions_full_match(values)
    expected <- if (isTRUE(spec$known_mismatch)) {
      spec$actual_taxonomy
    } else {
      spec$joined_against
    }
    testthat::expect_equal(
      best$column,
      expected,
      info = paste0(
        spec$table,
        "$",
        spec$column,
        " best matches regions_full$",
        best$column,
        " (",
        best$overlap,
        " of ",
        best$n,
        ") but the pipeline joins it against ",
        spec$joined_against,
        ". A file whose labels belong to one taxonomy joined against another ",
        "yields NA coefficients that a fallback hides."
      )
    )
  }
})

testthat::test_that("the sweep that found these three still finds only three", {
  # The list above is a snapshot. If a new coefficient table arrives with a region
  # column, it must be added and its join target stated -- otherwise the guard above
  # silently covers less than it appears to. This is the failure mode that made the
  # upstream contract tests useless for twelve fixes: a check that stops covering
  # its subject while still reporting green.
  tables <- whep:::.coef_table_names()
  testthat::expect_gt(length(tables), 15L)

  region_keyed <- character(0)
  for (t in tables) {
    d <- tryCatch(
      as.data.frame(whep::whep_coef_table(t)),
      error = function(e) NULL
    )
    if (is.null(d)) {
      next
    }
    cols <- grep(
      "region|iso|country|area",
      names(d),
      value = TRUE,
      ignore.case = TRUE
    )
    for (col in cols) {
      region_keyed <- c(region_keyed, paste0(t, "$", col))
    }
  }

  declared <- vapply(
    coef_region_joins,
    function(s) paste0(s$table, "$", s$column),
    character(1)
  )
  testthat::expect_setequal(region_keyed, declared)
})
