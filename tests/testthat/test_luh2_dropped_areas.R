# `.read_land_areas()` drops LUH2 rows whose ISO3 reaches no area. That warning fired on
# every production build as ONE message listing "JEY, GGY, IMN, -99, BLM, ALA, and SXM",
# which conflated two facts in wildly unequal proportions. Measured over the whole pin:
#
#   -99, LUH2's own unassigned marker   17,876 Mha   0.391% of all LUH2 area
#   six real territories                    35 Mha   0.0008%
#
# The sentinel is 510x the territories, so almost everything the message appeared to be
# losing is land the SOURCE does not attribute to any country. Reporting it alongside six
# territories made a property of LUH2 read as a failure of this project's coverage.
#
# The six are also not "not found in polity_area_crosswalk", as the old text said. They
# are all there, each carrying its sovereign's polity: JEY, GGY and IMN under
# GBR-1921-2025, ALA under Finland, BLM under France, SXM under the Netherlands. What
# they lack is a FAOSTAT area_code, and the bridge uses `include_unmapped = FALSE`, which
# drops precisely those rows. Whether to attribute their land to the sovereign is a
# modelling decision -- it redefines what the GBR series means -- so it is whep#407.
#
# This test pins the split. The failure it exists for is the sentinel quietly rejoining
# the territory list, or a new territory appearing in the sentinel bucket, because both
# would restore exactly the ambiguity that hid a 510x proportion difference.
testthat::test_that("the LUH2 sentinel and the unmapped territories are reported apart", {
  testthat::skip_on_ci()
  d <- tryCatch(whep:::whep_read_file("luh2-areas"), error = function(e) NULL)
  testthat::skip_if(is.null(d), "luh2-areas pin unavailable")
  d <- as.data.frame(d)
  testthat::expect_true(all(c("ISO3", "Area_Mha") %in% names(d)))

  cw <- as.data.frame(whep::polity_area_crosswalk)
  bridged <- unique(cw$area_iso3c[which(
    !is.na(cw$area_iso3c) & !is.na(cw$polity_area_code)
  )])
  unmatched <- setdiff(unique(stats::na.omit(d$ISO3)), bridged)

  sentinels <- unmatched[!grepl("^[A-Z]{3}$", unmatched)]
  territories <- setdiff(unmatched, sentinels)

  # Pinned by identity: a count would let a real country slip into either bucket.
  testthat::expect_setequal(sentinels, "-99")
  testthat::expect_setequal(
    territories,
    c("JEY", "GGY", "IMN", "BLM", "ALA", "SXM")
  )

  # And the proportion, because it is the reason the split matters. If the sentinel ever
  # stops dominating, the reasoning in the comment above needs revisiting rather than
  # being inherited.
  total <- sum(d$Area_Mha, na.rm = TRUE)
  sentinel_area <- sum(d$Area_Mha[d$ISO3 %in% sentinels], na.rm = TRUE)
  territory_area <- sum(d$Area_Mha[d$ISO3 %in% territories], na.rm = TRUE)
  testthat::expect_gt(sentinel_area / territory_area, 100)
  testthat::expect_lt(territory_area / total, 0.0001)
})

testthat::test_that("each dropped LUH2 territory has a sovereign polity upstream", {
  # The claim that makes whep#407 a decision rather than a gap: the mapping exists. If a
  # future crosswalk change removed it, option 2 in that issue would silently stop being
  # available, and the warning text pointing at it would become wrong.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  for (iso in c("JEY", "GGY", "IMN", "BLM", "ALA", "SXM")) {
    rows <- cw[which(cw$area_iso3c == iso), ]
    testthat::expect_true(
      nrow(rows) > 0L,
      info = paste0(iso, " is no longer in the crosswalk at all")
    )
    testthat::expect_true(
      any(!is.na(rows$polity_code)),
      info = paste0(iso, " has crosswalk rows but no polity on any of them")
    )
    # And no area code, which is precisely why they drop.
    testthat::expect_true(
      all(is.na(rows$polity_area_code)),
      info = paste0(
        iso,
        " now has a polity_area_code, so it should no longer be dropped and this ",
        "test and whep#407 both need revisiting"
      )
    )
  }
})
