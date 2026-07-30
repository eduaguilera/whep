# Where does this package still define areas, countries or regions in its own words?
#
# Swept `R/` and `inst/scripts/` for string literals that are real ISO3 codes in the polities
# database, keeping files with four or more. Four files carry hand-written country lists, and
# they are NOT the same kind of thing -- which is the point of pinning the result rather than
# filing "remove hardcoded lists":
#
#   R/scrape_faostat.R                       7   FIXED. A duplicate of published data: all
#                                                seven ISO3 patches agreed with `area_iso3c`,
#                                                so they now come from the crosswalk.
#   R/energy_co2_extension.R                55   KEEP. GLEAM's own regional scheme plus the UN
#                                                least-developed-country list. Both are
#                                                external classifications, not this project's
#                                                to define; the LDC codes are verified against
#                                                the database in test_gleam_hierarchy_coverage.R.
#   inst/scripts/compare_fabio_footprints.R 30   DECISION. Duplicates the published `EU27` flag
#                                                -- but they answer different questions. Below.
#   inst/scripts/prepare_spatialize_all.R   14   DECISION. A source's own non-standard codes,
#                                                which is what the published alias map is for.
#
# The two decisions are whep#421. Neither is a mechanical substitution, and this file exists so
# that nobody performs one, having reached for it and stopped.

testthat::test_that("the EU27 flag and an EU28 membership list are not interchangeable", {
  # `regions_full$EU27` looks like the obvious replacement for the 28-element literal in
  # inst/scripts/compare_fabio_footprints.R. It is not.
  #
  # The flag marks 29 ISO3s, and two of them -- BLX Belgium-Luxembourg and CSK Czechoslovakia
  # -- no longer exist. That is coherent: the flag means "territory that is now inside the
  # EU27", which is what a long time series needs, and a predecessor's territory is inside it.
  # The script's list means something different: the 28 member STATES before the UK left.
  #
  # So the two differ by exactly {BLX, CSK, GBR}, and substituting one for the other would
  # silently add two dissolved states to a published FABIO comparison. Asserted by identity so
  # the relationship is documented rather than rediscovered.
  r <- as.data.frame(whep::regions_full)
  testthat::expect_true(is.logical(r$EU27))

  flagged <- sort(unique(stats::na.omit(r$iso3c[which(r$EU27 %in% TRUE)])))
  # Non-vacuous: an empty flag would make every set operation below trivially pass.
  testthat::expect_gt(length(flagged), 20L)

  eu28_membership <- c(
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

  # In the flag but not a pre-Brexit member state: the two dissolved predecessors.
  testthat::expect_setequal(
    setdiff(flagged, eu28_membership),
    c("BLX", "CSK")
  )
  # A member state but not in the flag: the UK, which left.
  testthat::expect_setequal(
    setdiff(eu28_membership, flagged),
    "GBR"
  )

  # And both dissolved entities really are dissolved, so the reasoning above is not resting on
  # my reading of two ISO3 codes.
  pol <- sf::st_drop_geometry(whep::polities)
  for (iso in c("BLX", "CSK")) {
    rows <- pol[which(pol$iso3_code == iso | pol$iso3c == iso), ]
    testthat::skip_if(nrow(rows) == 0L, paste(iso, "absent from polities"))
    testthat::expect_true(all(rows$end_year <= 2025L))
  }
})

testthat::test_that("the spatialize recode maps a source's own codes onto real ISO3", {
  # inst/scripts/prepare_spatialize_all.R recodes fourteen codes -- SRM, GUA, BZE, COS, ELS,
  # HAI, HON, ROM, TRI, ZAR, BHA, BAR, DMI, STL -- onto ISO3. That is a source-scoped alias
  # table, which is exactly what `label_alias_map.csv` publishes, so it is a candidate for
  # onboarding upstream rather than a defect here (whep#421).
  #
  # What is asserted is the property that makes it one: the TARGETS are codes the database
  # knows and the SOURCES are not. If a source code ever became a real ISO3 -- codes get
  # reassigned -- the recode would start rewriting a valid code into a different country, and
  # this is where that would surface.
  pol <- sf::st_drop_geometry(whep::polities)
  cw <- as.data.frame(whep::polity_area_crosswalk)
  known <- unique(stats::na.omit(c(pol$iso3_code, pol$iso3c, cw$area_iso3c)))
  testthat::expect_gt(length(known), 200L)

  recode_map <- c(
    SRM = "SCG",
    GUA = "GTM",
    BZE = "BLZ",
    COS = "CRI",
    ELS = "SLV",
    HAI = "HTI",
    HON = "HND",
    ROM = "ROU",
    TRI = "TTO",
    ZAR = "COD",
    BHA = "BHS",
    BAR = "BRB",
    DMI = "DMA",
    STL = "LCA"
  )

  # Every target is a code the database carries: the recode lands somewhere real.
  testthat::expect_equal(setdiff(unname(recode_map), known), character(0))
  # And no source is, so the recode cannot be silently rewriting a valid code.
  testthat::expect_equal(intersect(names(recode_map), known), character(0))
})
