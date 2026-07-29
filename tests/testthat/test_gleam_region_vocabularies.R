# The namespace census -- both the lazy-loaded datasets and the internal ones, since an
# exported-only sweep missed gleam_geographic_hierarchy entirely -- turned up 21
# area-keyed tables with no polity column. Most are GLEAM and IPCC coefficient tables
# keyed by their own REGION taxonomies, which is legitimate and not a gap: a GLEAM
# region is no more derivable from a polity than a Bouwman region is (see
# test_dataset_area_identifiers.R for that argument).
#
# What is not legitimate is a table keyed by one vocabulary being joined against
# another. That has already cost a ninefold coefficient range (whep#405), so the two
# GLEAM region vocabularies are checked here.
#
# THE ABBREVIATION LAYER. gleam_dressing_percentages is keyed by ABBREVIATIONS -- EE,
# ESEA, LAC, NENA, SSA -- while gleam_geographic_hierarchy uses full names.
# .energy_region_abbrev() translates them, applied as
# `coalesce(abbrev[gleam_region], gleam_region)`, so a value already in full form passes
# through. That fallback is load-bearing rather than defensive: the table carries BOTH
# "WE" and "Western Europe", and they must end up as one region. They do.
#
# The failure this guards is a NEW abbreviation with no map entry. It would coalesce to
# itself, become its own region in the summarise, and silently split one region's rows in
# two -- with no error, because an unmapped key is indistinguishable from a full name.
#
# Measured today: all 11 values translate into the hierarchy's 11 regions, the map has no
# dead keys, and Antarctica is the only hierarchy region with no dressing row, which is
# correct.
testthat::test_that("every GLEAM dressing region translates to a hierarchy region", {
  d <- as.data.frame(get(
    "gleam_dressing_percentages",
    envir = asNamespace("whep")
  ))
  g <- as.data.frame(get(
    "gleam_geographic_hierarchy",
    envir = asNamespace("whep")
  ))
  abbrev <- whep:::.energy_region_abbrev()

  values <- sort(unique(stats::na.omit(as.character(d$gleam_region))))
  regions <- unique(stats::na.omit(as.character(g$gleam_region)))
  # Non-vacuous: an empty column would make every setdiff below empty.
  testthat::expect_gt(length(values), 5L)
  testthat::expect_gt(length(regions), 5L)

  translated <- ifelse(values %in% names(abbrev), abbrev[values], values)
  untranslatable <- setdiff(translated, regions)
  testthat::expect_equal(
    length(untranslatable),
    0L,
    info = paste0(
      "region keys that neither map through .energy_region_abbrev() nor already ",
      "name a hierarchy region, so each becomes its own region and splits that ",
      "region's rows: ",
      paste(untranslatable, collapse = ", ")
    )
  )

  # A dead map entry is not harmless: it suggests a key the table no longer uses, so
  # the next person adding a region cannot tell which spelling is current.
  testthat::expect_equal(
    length(setdiff(names(abbrev), values)),
    0L,
    info = paste0(
      "abbreviations no table row uses: ",
      paste(setdiff(names(abbrev), values), collapse = ", ")
    )
  )

  # Antarctica has no livestock, so it is the one region legitimately without a
  # dressing row. Pinned by name rather than by count, so a second gap is looked at.
  testthat::expect_setequal(setdiff(regions, translated), "Antarctica")

  # The mixed-form case is deliberate and must stay merged: both spellings of Western
  # Europe are present and must collapse to one region.
  testthat::expect_true(all(c("WE", "Western Europe") %in% values))
  merged <- whep:::.energy_dressing_by_group()
  testthat::expect_equal(
    sum(merged$reg == "Western Europe" & merged$grp == "bovine"),
    1L
  )
})

# The IPCC tables, and several GLEAM ones, use a COARSER taxonomy -- Africa, Asia, Latin
# America, plus a "Global" fallback row -- which overlaps the GLEAM regions only
# partially. That is correct: IPCC emission factors are published for IPCC regions.
#
# Asserted so that a reader comparing them to gleam_region sees a deliberate difference
# rather than an unfinished one. This is the same reasoning that keeps someone from
# "fixing" conv_bouwman, and it matters more here because the partial overlap looks like
# a bug: four of nine values matching is exactly what a broken join looks like.
testthat::test_that("IPCC coefficient regions are their own taxonomy, not GLEAM's", {
  g <- as.data.frame(get(
    "gleam_geographic_hierarchy",
    envir = asNamespace("whep")
  ))
  regions <- unique(stats::na.omit(as.character(g$gleam_region)))

  ipcc <- c(
    "ipcc_2006_enteric_ef",
    "ipcc_2006_manure_ef",
    "ipcc_2019_enteric_ef_cattle",
    "ipcc_2019_manure_ch4_ef_cattle",
    "ipcc_2019_n_excretion"
  )
  for (nm in ipcc) {
    x <- as.data.frame(get(nm, envir = asNamespace("whep")))
    testthat::expect_true("region" %in% names(x))
    v <- unique(stats::na.omit(as.character(x$region)))
    testthat::expect_gt(length(v), 3L)

    # Partial overlap, and a Global fallback row. Both are properties of the IPCC
    # taxonomy, so both are required: losing "Global" would leave unmatched regions
    # with no coefficient at all.
    testthat::expect_gt(length(intersect(v, regions)), 0L)
    testthat::expect_lt(length(intersect(v, regions)), length(v))
    testthat::expect_true("Global" %in% v)
  }
})
