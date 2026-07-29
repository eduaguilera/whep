# whep::polities_cats and whep::regions_full are two hand-maintained copies of one
# table. Identical 40 columns; polities_cats' 198 area codes are a strict subset of
# regions_full's 272, with none outside. It is a row-filtered view that is not
# derived from the thing it is a view of, and the two are read from separate vendored
# CSVs.
#
# Predictably, they disagree. Comparing all 40 columns over the 136 shared codes
# found 18 columns differing, in three classes that need separating because only one
# of them was a defect:
#
#   ADB_Region        codes 11 and 229 (Austria, United Kingdom): AT/GB in one table,
#                     NA in the other. NOT a modelling difference -- an identity key
#                     that is either right or missing. It happened because the
#                     override filling those two codes was written inline on
#                     regions_full alone. FIXED: the override is now a function
#                     applied to both, and this class is empty.
#
#   cbs, fabio_code   Bhutan (18) and Comoros (45): regions_full gives them their own
#                     FABIO codes and cbs = TRUE, polities_cats folds them to 999
#                     with cbs = FALSE. This one is DELIBERATE and documented in
#                     data-raw/harmonization_tables.R, which files Bhutan under RASI
#                     and Comoros under RAFR.
#
#                     Worth recording anyway, because the ground has moved: the
#                     faostat-cbs-new pin carries 91 rows for Bhutan and 135 for
#                     Comoros, while the older faostat-cbs-old-crops pin carries none
#                     for either. The fold was reasonable when neither had a balance
#                     sheet. Whether it still is, is a maintainer's call -- whep#406.
#
#   13 region_* plus   cells where polities_cats holds the literal "0" and
#     columns         regions_full holds NA. An encoding artefact of the vendored
#                     file, not a choice: "0" is not the name of any region in any
#                     taxonomy. Left as-is and pinned, because polities_cats is
#                     vendored and unused (below), so rewriting its values buys
#                     nothing today.
#
# THE FACT THAT MAKES ALL OF THIS LOW-SEVERITY, and which took a grep to establish
# rather than an assumption: polities_cats has NO consumers in package code. Only
# R/datasets.R mentions it, to document it. regions_full is read by eight files. So
# the operative table is the one that is right about Bhutan, Comoros, Austria and the
# UK, and the disagreements were never reaching a computation. That is also why the
# fix here is a guard plus an issue rather than a rewrite of an exported dataset.
testthat::test_that("polities_cats is a strict same-schema subset of regions_full", {
  a <- as.data.frame(whep::regions_full)
  b <- as.data.frame(whep::polities_cats)

  testthat::expect_setequal(names(a), names(b))
  testthat::expect_equal(ncol(a), 40L)
  testthat::expect_gt(nrow(a), nrow(b))

  outside <- setdiff(
    stats::na.omit(b$code),
    stats::na.omit(a$code)
  )
  testthat::expect_equal(
    length(outside),
    0L,
    info = paste0(
      "polities_cats areas absent from regions_full, so the subset relation no ",
      "longer holds: ",
      paste(utils::head(outside, 10), collapse = ", ")
    )
  )
})

testthat::test_that("the two tables disagree only where they are known to", {
  a <- as.data.frame(whep::regions_full)
  b <- as.data.frame(whep::polities_cats)
  a <- a[!duplicated(a$code) & !is.na(a$code), ]
  b <- b[!duplicated(b$code) & !is.na(b$code), ]
  shared <- intersect(a$code, b$code)
  # Non-vacuous: an empty overlap would make every column agree trivially.
  testthat::expect_gt(length(shared), 100L)

  a <- a[match(shared, a$code), ]
  b <- b[match(shared, b$code), ]

  differing <- character(0)
  for (col in setdiff(names(a), "geom")) {
    x <- as.character(a[[col]])
    y <- as.character(b[[col]])
    same <- (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
    if (any(!same)) {
      differing <- c(differing, col)
    }
  }

  # `cbs` and `fabio_code` carry the deliberate Bhutan/Comoros folds; the region_*
  # columns carry the "0" encoding artefact. ADB_Region is deliberately NOT in this
  # list -- it was the one real defect and it is fixed, so its reappearance fails.
  # Measured, not recalled: my first version of this list was written from a
  # truncated console dump and omitted polity_prefix, polity_name, eia and iea while
  # inventing four columns that agree. The test caught it, which is the argument for
  # pinning a measured set rather than a remembered one.
  #
  # polity_prefix and polity_name carry the same two rows as cbs and fabio_code:
  # polities_cats files Comoros as "Africa Other" (RAFR) and Bhutan as "Asia Other"
  # (RASI), which is the fold itself rather than a separate difference.
  #
  # eia and iea are the US Energy Information Administration and International Energy
  # Agency country-name vocabularies. Two similar names, two different agencies --
  # regions_full documents both correctly, and they are NOT a misspelt duplicate,
  # which is what I assumed before comparing their values ("Taiwan" against "Chinese
  # Taipei", "Congo (Brazzaville)" against "Republic of the Congo").
  expected <- c(
    "polity_prefix",
    "polity_name",
    "cbs",
    "fabio_code",
    "eia",
    "iea",
    "region_UN_sub",
    "region_UN",
    "region_ILO1",
    "region_ILO2",
    "region_ILO3",
    "region_IEA",
    "region_IPCC",
    "region_labour",
    "region_labour_agg",
    "region_labour_mech",
    "region_test"
  )
  testthat::expect_setequal(differing, expected)

  # And the fold disagreement is exactly two areas, named. A third would mean a new
  # modelling choice landed in one table only.
  fold_rows <- which(
    as.character(a$fabio_code) != as.character(b$fabio_code)
  )
  testthat::expect_setequal(a$code[fold_rows], c(18L, 45L))
})

testthat::test_that("ADB_Region agrees between the two tables", {
  a <- as.data.frame(whep::regions_full)
  b <- as.data.frame(whep::polities_cats)
  a <- a[!duplicated(a$code) & !is.na(a$code), ]
  b <- b[!duplicated(b$code) & !is.na(b$code), ]
  shared <- intersect(a$code, b$code)
  x <- a$ADB_Region[match(shared, a$code)]
  y <- b$ADB_Region[match(shared, b$code)]

  disagree <- shared[
    !((is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y))
  ]
  testthat::expect_equal(
    length(disagree),
    0L,
    info = paste0(
      "ADB_Region differs for areas ",
      paste(disagree, collapse = ", "),
      " -- the override must be applied to BOTH tables, not one"
    )
  )
  # Both tables must carry all 28, not merely agree on a smaller number.
  testthat::expect_equal(sum(!is.na(a$ADB_Region)), 28L)
  testthat::expect_equal(sum(!is.na(b$ADB_Region)), 28L)
})
