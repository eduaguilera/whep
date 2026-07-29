# whep::crops_manure_n (West et al. 2014) is keyed by ISO 3166-1 alpha-3 and carries
# no year column, because it is a single circa-2000 snapshot. 180 of its 184 codes
# already reached a polity through the crosswalk; four did not, and the reason is
# the interesting part.
#
# THE SOURCE USES MODERN CODES FOR A CIRCA-2000 SNAPSHOT. Three of the four name
# states that did not exist in the data year:
#
#   SRB, MNE   Serbia and Montenegro were one polity until 2006
#   SSD        South Sudan did not exist until 2011
#   RoW        the source's own residual bucket, not a territory
#
# Each is now aliased upstream under source "crops-manure-n": SRB and MNE to
# SCG-1992-2006, SSD to SUD-1956-2011, RoW to the Rest of World aggregate. Coverage
# is 184 of 184 codes and 100% of the 112,887,685 Mg.
#
# WHY MANY-TO-ONE IS CORRECT HERE AND WRONG NEXT DOOR. Two codes resolving to one
# polity is normally a defect: test_mueller_country_codes.R asserts that Mueller's
# 156 codes reach 156 DISTINCT polities, and it exists because SRM had been aliased
# to Suriname, which already appeared under its own code, so one polity held two
# rate sets while SRM's real entity held none.
#
# The difference is what the values mean. Mueller's are RATES (kg/ha), which do not
# add: two rate sets on one polity is a collision with no correct resolution.
# Manure_N_Mg is a QUANTITY, which does add. Serbia's manure plus Montenegro's
# manure IS Serbia and Montenegro's manure. Likewise the file reports SDN and SSD
# separately as modern halves of pre-partition Sudan, so both resolving to
# SUD-1956-2011 reconstructs the whole rather than double-counting it.
#
# So this test asserts total coverage and additive conservation, and deliberately
# does NOT assert distinctness. The pairing is pinned by name, so a future change
# that folds some other code into an existing polity fails rather than silently
# joining the exemption.
testthat::test_that("every crops_manure_n country code reaches a polity", {
  d <- whep::crops_manure_n
  codes <- sort(unique(stats::na.omit(d$ISO)))
  # Non-vacuous: a renamed column would make the rest pass for free.
  testthat::expect_gt(length(codes), 180L)

  aliased <- resolve_polity_label(
    codes,
    source = "crops-manure-n",
    year = 2000L
  )
  cw <- as.data.frame(whep::polity_area_crosswalk)
  iso <- cw[
    which(
      !is.na(cw$iso3_code) &
        nzchar(cw$iso3_code) &
        !is.na(cw$polity_code) &
        cw$polity_start_year <= 2000L &
        cw$polity_end_year > 2000L
    ),
    c("iso3_code", "polity_code")
  ]
  direct <- iso$polity_code[match(codes, iso$iso3_code)]
  resolved <- ifelse(is.na(aliased), direct, aliased)

  unresolved <- codes[is.na(resolved)]
  testthat::expect_equal(
    length(unresolved),
    0L,
    info = paste0(
      "codes reaching no polity: ",
      paste(utils::head(unresolved, 10), collapse = ", ")
    )
  )

  # Coverage by code says nothing about coverage by mass; a code carrying most of
  # the nitrogen counts for more than one carrying none.
  key <- data.frame(ISO = codes, polity = resolved, stringsAsFactors = FALSE)
  joined <- merge(d, key, by = "ISO", all.x = TRUE)
  total <- sum(joined$Manure_N_Mg, na.rm = TRUE)
  covered <- sum(
    joined$Manure_N_Mg[!is.na(joined$polity)],
    na.rm = TRUE
  )
  testthat::expect_equal(covered, total)
})

testthat::test_that("the codes that share a polity are exactly the additive pairs", {
  d <- whep::crops_manure_n
  codes <- sort(unique(stats::na.omit(d$ISO)))
  aliased <- resolve_polity_label(
    codes,
    source = "crops-manure-n",
    year = 2000L
  )
  cw <- as.data.frame(whep::polity_area_crosswalk)
  iso <- cw[
    which(
      !is.na(cw$iso3_code) &
        nzchar(cw$iso3_code) &
        !is.na(cw$polity_code) &
        cw$polity_start_year <= 2000L &
        cw$polity_end_year > 2000L
    ),
    c("iso3_code", "polity_code")
  ]
  resolved <- ifelse(
    is.na(aliased),
    iso$polity_code[match(codes, iso$iso3_code)],
    aliased
  )

  by_polity <- split(codes, resolved)
  shared <- by_polity[vapply(by_polity, length, integer(1)) > 1L]
  # Pinned by content, not by count: the point is WHICH codes share a polity, since
  # additivity is what makes it correct, and that is a property of these specific
  # pairs rather than of sharing in general.
  observed <- lapply(shared, sort)
  expected <- list(
    "SCG-1992-2006" = c("MNE", "SRB"),
    "SUD-1956-2011" = c("SDN", "SSD")
  )
  testthat::expect_equal(observed[order(names(observed))], expected)
})
