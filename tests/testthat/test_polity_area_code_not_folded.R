# `polity_area_code` is the numeric area key the builds actually key on —
# get_primary_production() emits it AS its `area_code`, and build_trade.R assigns
# `area_code := polity_area_code` outright. So whatever this column says about an area, the
# output obeys, regardless of what `polity_code` says.
#
# This file used to assert the OPPOSITE of what it asserts now, and the reversal is the point.
#
# Eleven areas that FABIO folds into rest-of-world (fabio_code 999) report data of their own,
# so their data was being summed into area 999 and attributed to ROW-1850-2023 — the Faroe
# Islands' 2,458 raw production rows, Palestine's 9,606. Promoting them to their own numeric
# code looked like the fix, and this file pinned it.
#
# It is not safe with the pipeline as it stands. Measured on a full-range `get_wide_cbs()`,
# promoting those areas changes global totals:
#
#   feed        8.232e10 -> 1.132e12   (13.7x)
#   export      3.011e10 -> 3.988e11   (13.2x)
#   production  1.736e12 -> 3.172e12   ( 1.8x)
#
# And it is an artifact rather than recovered data: the entire feed increase lands on ONE
# area, 212 Syrian Arab Republic, at 1.049e12 — twelve times the world total inside a single
# country — concentrated in item codes 2590 and 2598 and growing monotonically from 2.9e10
# (2001) to 3.0e11 (2009). That compounding shape says something scales on bucket membership,
# so an area promoted to its own bucket carries bucket-level magnitudes with it. See whep#419;
# the underlying attribution problem is still open, deliberately, rather than traded for a
# 13x error elsewhere.
#
# Nothing in 5151 tests caught either the promotion or its withdrawal, because both changed
# MAGNITUDES and not row counts, and no test compared a total against anything. That is the
# gap this file now closes for this particular key.

testthat::test_that("the numeric area key honours FABIO's fold", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  keep <- which(!is.na(cw$fabio_code) & !is.na(cw$polity_area_code))
  # Non-vacuous: an empty selection would make the comparison below assert nothing.
  testthat::expect_gt(length(keep), 200L)

  mismatch <- cw[keep, ][
    which(
      cw$fabio_code[keep] != cw$polity_area_code[keep]
    ),
  ]
  testthat::expect_equal(
    nrow(mismatch),
    0L,
    info = paste0(
      "areas whose numeric key departs from `fabio_code`: ",
      paste(unique(mismatch$area_code), collapse = ", "),
      " — promoting a folded area out of its bucket inflated global feed 13.7x ",
      "the last time this diverged (whep#419)"
    )
  )
})

testthat::test_that("both published tables state the fold identically", {
  # The promotion survived one round of withdrawal precisely because it is written down
  # TWICE — data-raw/table_mappings.R builds `polity_area_crosswalk`, and
  # data-raw/harmonization_tables.R builds `regions_full` — and only the first was rebuilt.
  # `regions_full` then carried 16 areas the crosswalk had already refolded. Checking one
  # representation cannot see a disagreement between two, so this checks the pair.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  r <- as.data.frame(whep::regions_full)

  a <- unique(cw[
    which(!is.na(cw$area_code)),
    c("area_code", "polity_area_code")
  ])
  b <- unique(r[which(!is.na(r$code)), c("code", "polity_area_code")])
  names(b) <- c("area_code", "regions_full_key")
  j <- merge(a, b, by = "area_code")
  # Non-vacuous: the two tables must actually share area codes.
  testthat::expect_gt(nrow(j), 200L)

  same <- (is.na(j$polity_area_code) & is.na(j$regions_full_key)) |
    (!is.na(j$polity_area_code) &
      !is.na(j$regions_full_key) &
      j$polity_area_code == j$regions_full_key)
  testthat::expect_equal(
    sum(!same),
    0L,
    info = paste0(
      "areas where regions_full and polity_area_crosswalk disagree on the ",
      "numeric aggregation key: ",
      paste(j$area_code[!same], collapse = ", "),
      " — rebuild BOTH data-raw scripts, not one"
    )
  )
})

# whep#403 asked whether unfolding territories from FABIO's rest-of-world bucket breaks a
# comparison against FABIO. It does not, and now cannot: nothing is unfolded at the numeric
# level. Kept as a guard on the route rather than deleted, because the promotion is expected
# to return once whep#419 is settled, and this is where a promotion that DID fall inside the
# published comparison's targets would surface.
#
# inst/scripts/compare_fabio_footprints.R reproduces FABIO land footprints for benchmark years
# and targets CHN, USA and EU28, selecting areas with `filter(iso3c %in% target_iso)`. The
# thirteen candidate territories carry BMU, COK, GNQ, FRO, GUF, GLP, GUM, MTQ, NIU, PLW, REU,
# SWZ and PSE — the French overseas departments are GUF/GLP/MTQ/REU rather than FRA, so none
# is part of the EU28 selection even though France is.
testthat::test_that("no unfolded territory falls inside the FABIO comparison's targets", {
  r <- as.data.frame(whep::regions_full)
  unfolded <- unique(r$code[which(
    r$fabio_code == 999L &
      r$polity_area_code == r$code &
      !is.na(r$code) &
      r$code != 999L
  )])

  eu28 <- c(
    "AUT",
    "BEL",
    "BGR",
    "HRV",
    "CYP",
    "CZE",
    "DNK",
    "EST",
    "FIN",
    "FRA",
    "DEU",
    "GRC",
    "HUN",
    "IRL",
    "ITA",
    "LVA",
    "LTU",
    "LUX",
    "MLT",
    "NLD",
    "POL",
    "PRT",
    "ROU",
    "SVK",
    "SVN",
    "ESP",
    "SWE",
    "GBR"
  )
  targets <- c("CHN", "USA", eu28)

  iso <- r$iso3c[match(unfolded, r$code)]
  clash <- unfolded[which(iso %in% targets)]
  testthat::expect_equal(
    length(clash),
    0L,
    info = paste0(
      "unfolded areas whose iso3c is a target of ",
      "inst/scripts/compare_fabio_footprints.R, so unfolding them changes a ",
      "published FABIO comparison: ",
      paste(clash, collapse = ", ")
    )
  )
})
