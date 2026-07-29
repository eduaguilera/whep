# inst/extdata/coefs/residue_feed_fraction.csv has a column named `region_hanpp`
# that does NOT contain HANPP regions. Its 17 named values are UN M49 sub-regions
# -- Sub-Saharan Africa, Western Asia, South-Eastern Asia, Melanesia, Australia and
# New Zealand -- and 14 of them match whep::regions_full$region_UN_sub exactly,
# while 0 match region_HANPP.
#
# .residue_destiny_krausmann() joins that file `by = "region_hanpp"`, and the
# pipeline supplies that column from regions_full$region_HANPP (see
# soil_carbon_inputs.R, `region_hanpp = .data$region_HANPP`). Eight HANPP labels
# against seventeen UN sub-region labels match nothing, and the next line replaces
# the resulting NA feed fraction with `global_feed` -- so every row silently takes
# the "Global" default of 0.20. Confirmed by running
# the function rather than by reading it: six different regions, six identical
# implied fractions of 0.2, while recovery_rates from the OTHER join varied
# correctly across the same rows. The Krausmann join works; only this one is dead.
#
# What is lost: the file spans 0.05 (Northern America, Oceania) to 0.45 (Southern
# Asia), a ninefold range, and all of it collapses to one number. That shifts the
# feed-versus-burn split of crop residues almost everywhere, which propagates into
# the nitrogen and carbon balances.
#
# NOT FIXED HERE, deliberately. The repair is clear -- join on region_UN_sub, which
# the pipeline can carry because regions_full already has it, after normalising
# "South-Eastern Asia" to regions_full's "South-eastern Asia" -- but it changes
# model outputs across most of the world, and the values come from cited literature
# whose regional assignment is a scientific judgement rather than a lookup. Filed as
# whep#405 with this evidence.
#
# So this test BASELINES the defect: it fails if the situation changes in either
# direction, whether the vocabularies start agreeing (the fix landed -- delete this
# test and assert the real behaviour) or the overlap changes shape (something else
# moved). A silent bug that nothing measures is how a ninefold coefficient range
# stays dead; a measured one at least cannot get quieter.
testthat::test_that("the residue feed-fraction file uses UN sub-regions, not HANPP regions", {
  path <- system.file(
    "extdata",
    "coefs",
    "residue_feed_fraction.csv",
    package = "whep"
  )
  testthat::skip_if(path == "", "residue_feed_fraction.csv not installed")
  f <- utils::read.csv(path, stringsAsFactors = FALSE)
  testthat::expect_true("region_hanpp" %in% names(f))

  named <- setdiff(sort(unique(stats::na.omit(f$region_hanpp))), "Global")
  # Non-vacuous: an empty column would make every intersection below zero and the
  # baseline would "hold" while measuring nothing.
  testthat::expect_gt(length(named), 15L)

  r <- as.data.frame(whep::regions_full)
  hanpp <- unique(stats::na.omit(as.character(r$region_HANPP)))
  un_sub <- unique(stats::na.omit(as.character(r$region_UN_sub)))

  # The column is named for one taxonomy and populated from another.
  testthat::expect_equal(
    length(intersect(named, hanpp)),
    0L,
    info = paste0(
      "values now shared with region_HANPP, so the join may no longer be dead: ",
      paste(intersect(named, hanpp), collapse = ", ")
    )
  )
  testthat::expect_equal(
    length(intersect(named, un_sub)),
    14L,
    info = paste0(
      "overlap with region_UN_sub changed; it was 14 of 17. Now: ",
      paste(intersect(named, un_sub), collapse = ", ")
    )
  )

  # The three that do not match are two genuine extra granularities the file
  # carries and one pure case difference, which is worth naming because a reader
  # fixing this will otherwise wonder why 14 and not 15.
  testthat::expect_setequal(
    setdiff(named, un_sub),
    c("Caribbean", "Oceania", "South-Eastern Asia")
  )
})

testthat::test_that("residue feed fractions are currently uniform, which they should not be", {
  r <- whep::regions_full
  keep <- which(!is.na(r$region_HANPP) & !is.na(r$region_krausmann))
  pairs <- unique(data.frame(
    region_krausmann = r$region_krausmann[keep],
    region_hanpp = r$region_HANPP[keep],
    stringsAsFactors = FALSE
  ))
  testthat::expect_gt(nrow(pairs), 5L)

  x <- data.frame(
    item_prod_code = "15",
    residue_dm_t = 1000,
    region_krausmann = pairs$region_krausmann,
    region_hanpp = pairs$region_hanpp,
    stringsAsFactors = FALSE
  )
  out <- as.data.frame(whep:::.residue_destiny_krausmann(x))

  recovered <- out$residue_feed_dm_t + out$residue_burn_dm_t
  fraction <- round(
    out$residue_feed_dm_t[recovered > 0] / recovered[recovered > 0],
    6
  )

  # Every region gets the Global fallback. When whep#405 is fixed this becomes
  # several distinct values and this expectation flips -- which is the point of
  # writing it down rather than leaving the uniformity to be rediscovered.
  testthat::expect_equal(
    unique(fraction),
    0.2,
    info = paste0(
      "implied feed fractions: ",
      paste(unique(fraction), collapse = ", "),
      " -- more than one value means the region join now matches, so whep#405 is ",
      "fixed and this test should be replaced by one asserting the real spread"
    )
  )

  # The neighbouring join is NOT broken, and saying so keeps a reader from
  # concluding the whole method is inert. Recovery rates vary across these regions.
  testthat::expect_gt(length(unique(round(recovered, 6))), 1L)
})
