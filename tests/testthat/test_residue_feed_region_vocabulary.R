# Guards the vocabulary of residue_feed_fraction.csv against the failure mode of
# #405: the file's region column was named region_hanpp but held UN M49
# sub-regions, and .residue_destiny_krausmann() joined it against a column the
# pipeline filled from regions_full$region_HANPP. None of the 8 HANPP labels is
# an M49 sub-region label, so the left_join matched NOTHING and the very next
# line, tidyr::replace_na(feed_use_fraction, global_feed), handed every polity on
# earth the "Global" default of 0.2. Measured on this branch before the fix, with
# the region pairs regions_full actually supplies: 11 of 11 rows fell back, and
# the single implied feed fraction was 0.2 for every one of the 8 HANPP regions.
# A coefficient table spanning 0.05 to 0.45 was dead in full, and silently --
# replace_na() leaves an unmatched region indistinguishable from a matched one,
# mass still conserves, and no column is ever NA, so nothing could see it.
#
# These tests are the measurement that was missing. Run against the pre-fix code
# they report 8 failures over 4 of the 5 blocks below; they also fail if
# regions_full and the file ever drift apart again, in either direction.

test_that("the feed-fraction table is keyed by UN M49 sub-region", {
  feed <- whep::whep_coef_table("residue_feed_fraction")
  testthat::expect_true("region_un_sub" %in% names(feed))
  testthat::expect_false("region_hanpp" %in% names(feed))

  named <- setdiff(feed$region_un_sub, "Global")
  un_sub <- unique(stats::na.omit(whep::regions_full$region_UN_sub))
  hanpp <- unique(stats::na.omit(whep::regions_full$region_HANPP))

  # 15 of the 17 named rows are exactly a regions_full region_UN_sub value.
  # ZERO of them are a region_HANPP value -- that second expectation is the one
  # that pins the mislabel, and it held (0 of 17) before the rename too.
  testthat::expect_length(intersect(named, un_sub), 15L)
  testthat::expect_length(intersect(named, hanpp), 0L)

  # The two that are not: coarser levels of the same M49 hierarchy, kept in the
  # file because deleting published coefficients is an authoring decision, not a
  # bug fix. Caribbean costs nothing -- its 0.2 equals the Latin America and the
  # Caribbean row that Caribbean areas do match. Oceania at 0.05 would only ever
  # apply to Micronesia and Polynesia, which have no row of their own and so keep
  # the 0.2 Global fallback they already had; whether the coarse Oceania figure
  # should cover them is the open question left on #405.
  testthat::expect_equal(
    sort(setdiff(named, un_sub)),
    c("Caribbean", "Oceania")
  )
  fraction_of <- function(region) {
    feed$feed_use_fraction[feed$region_un_sub == region]
  }
  testthat::expect_equal(
    fraction_of("Caribbean"),
    fraction_of("Latin America and the Caribbean")
  )
})

test_that("the feed-fraction join is live across regions_full sub-regions", {
  # Drive the split with the region pairs a real caller supplies -- straight out
  # of .sci_crop_regions(), which is what .sci_crop_prod_wide() joins onto the
  # production table -- rather than a hand-built fixture, so this exercises the
  # actual vocabulary contract between regions_full and the coefficient file.
  regions <- whep:::.sci_crop_regions() |>
    dplyr::filter(
      !is.na(.data$region_un_sub),
      !is.na(.data$region_krausmann)
    ) |>
    dplyr::distinct(.data$region_krausmann, .data$region_un_sub)
  testthat::expect_gt(nrow(regions), 20L)

  out <- regions |>
    dplyr::mutate(item_prod_code = "15", residue_dm_t = 100) |>
    whep::calculate_residue_destinies()

  # The implied feed fraction recovered from the split: feed and burn together
  # are residue_dm_t * recovery_rates, so their ratio is feed_use_fraction
  # exactly, whatever the recovery rate is.
  implied <- round(
    out$residue_feed_dm_t / (out$residue_feed_dm_t + out$residue_burn_dm_t),
    4
  )
  # Pre-fix this was the single value 0.2. Post-fix it is the file's full range.
  testthat::expect_equal(
    sort(unique(implied)),
    c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.45)
  )
})

test_that("each sub-region receives its published feed-use fraction", {
  feed <- whep::whep_coef_table("residue_feed_fraction")
  published <- feed$feed_use_fraction[feed$region_un_sub != "Global"]
  names(published) <- feed$region_un_sub[feed$region_un_sub != "Global"]
  # Southern Asia (0.45) against Northern America (0.05) is the ninefold spread
  # the dead join collapsed onto 0.2; Melanesia (0.1) is a third distinct value
  # so this cannot pass on a two-valued accident.
  probes <- c("Southern Asia", "Northern America", "Melanesia")

  out <- tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = 100,
    # West Europe is a recovery region that exists for every crop category, so
    # recovery_rates is non-zero and the feed:burn ratio is well defined. It is
    # deliberately held constant while region_un_sub varies: the feed fraction
    # must come from region_un_sub alone.
    region_krausmann = "West Europe",
    region_un_sub = probes
  ) |>
    whep::calculate_residue_destinies()

  testthat::expect_equal(
    out$residue_feed_dm_t / (out$residue_feed_dm_t + out$residue_burn_dm_t),
    unname(published[probes])
  )
  testthat::expect_equal(
    out$residue_feed_dm_t + out$residue_burn_dm_t + out$residue_soil_dm_t,
    rep(100, length(probes))
  )
})

test_that("the fix moves the feed:burn split only, not the soil return", {
  # Scope of the value move, as algebra rather than as a claim: residue_soil_dm_t
  # is residue_dm_t * (1 - recovery_rates) and residue_feed + residue_burn is
  # residue_dm_t * recovery_rates, neither of which mentions feed_use_fraction.
  # So the removed total and the soil-returned total are identical for two
  # different feed fractions, and everything downstream of residue_soil_dm_t
  # (build_soil_carbon_inputs()'s residue carbon) is untouched. What moves is
  # feed versus burn, hence build_residue_feed_avail() and the two residue-N
  # destiny columns of the nitrogen balance.
  split_for <- function(region) {
    whep::calculate_residue_destinies(tibble::tibble(
      item_prod_code = "15",
      residue_dm_t = 100,
      region_krausmann = "West Europe",
      region_un_sub = region
    ))
  }
  low <- split_for("Northern America")
  high <- split_for("Southern Asia")

  testthat::expect_equal(low$residue_soil_dm_t, high$residue_soil_dm_t)
  testthat::expect_equal(
    low$residue_feed_dm_t + low$residue_burn_dm_t,
    high$residue_feed_dm_t + high$residue_burn_dm_t
  )
  testthat::expect_lt(low$residue_feed_dm_t, high$residue_feed_dm_t)
})

test_that("krausmann_regional demands the M49 sub-region column", {
  # region_hanpp is a real pipeline column keying a real HANPP table (the
  # modern-variety adoption share in calculate_crop_npp()), so supplying it and
  # not region_un_sub is exactly the mistake #405 was. It must abort rather than
  # quietly fall back to 0.2 for every row, which is what it used to do.
  testthat::expect_error(
    whep::calculate_residue_destinies(tibble::tibble(
      item_prod_code = "15",
      residue_dm_t = 100,
      region_krausmann = "West Europe",
      region_hanpp = "West Europe"
    )),
    "region_un_sub"
  )
})
