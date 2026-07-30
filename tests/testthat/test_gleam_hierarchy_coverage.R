# Which reporting areas does the GLEAM country table not know about?
#
# `.energy_country_grouping()` derives all three GLEAM regional schemes by transmuting
# `gleam_geographic_hierarchy`, so that table defines the country universe for the energy
# extension. An area absent from it gets no grouping at all — not a wrong group, no row —
# and a join on the grouping then drops it in silence.
#
# The sibling test compares the two tables' `continent` where both have a row. This one asks
# the prior question: which areas have no row to compare. 18 of the 217 self-reporting areas
# (those whose `area_code` is its own `polity_area_code`, so not folded into a bucket), and
# they fall into three classes, only one of which is a defect:
#
#   7 regional aggregates   RAFR, RASI, REUR, RLAM, RNAM, ROCE, ROW. GLEAM is a country
#                           table; an aggregate has no business in it.
#   6 dissolved entities    Belgium-Luxembourg, Czechoslovakia, Netherlands Antilles,
#                           Serbia and Montenegro, USSR, Yugoslav SFR. GLEAM is a
#                           present-day table, so these are absent by construction.
#   5 LIVE TERRITORIES      Bermuda, Guam, Nauru, Palau, Tuvalu. Each exists today, reports
#                           under its own area code, and is simply missing from GLEAM's 204
#                           countries — so each is unclassifiable by the energy extension.
#
# The third class is the finding. Tuvalu is the sharpest case because
# `.energy_ldc_iso3()` lists TUV as a least-developed country, so the code asserts a
# classification for a country the table it joins against cannot represent. All 46 LDC codes
# do exist in the polities database, so the list itself is sound; the gap is GLEAM's.
#
# Pinned by identity rather than by count: a NEW absence is a country the extension has
# started dropping, and it would hide inside a number.

test_that("the GLEAM country table's gaps are the known 18, by identity", {
  gleam <- tryCatch(
    get("gleam_geographic_hierarchy", envir = asNamespace("whep")),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(gleam), "gleam_geographic_hierarchy unavailable")

  cw <- as.data.frame(whep::polity_area_crosswalk)
  keep <- which(
    !is.na(cw$area_code) &
      !is.na(cw$area_iso3c) &
      cw$area_code == cw$polity_area_code
  )
  own <- unique(cw[keep, c("area_code", "area_name", "area_iso3c")])
  # Non-vacuous: an empty universe would make the setdiff empty and pass on nothing.
  expect_gt(nrow(own), 150L)

  absent <- sort(setdiff(own$area_iso3c, as.data.frame(gleam)$iso3))
  expect_setequal(
    absent,
    c(
      # regional aggregates
      "RAFR",
      "RASI",
      "REUR",
      "RLAM",
      "RNAM",
      "ROCE",
      "ROW",
      # dissolved entities
      "ANT",
      "BLX",
      "CSK",
      "SCG",
      "SUN",
      "YUG",
      # live territories with no GLEAM row — the defect class
      "BMU",
      "GUM",
      "NRU",
      "PLW",
      "TUV"
    )
  )
})

test_that("every LDC code names a polity in the database", {
  # The list is hand-typed, so a mistyped ISO3 would silently drop a country from the
  # least-developed group rather than erroring. Checked against the database rather than
  # against a second hand-typed list: all 46 resolve.
  ldc <- get(".energy_ldc_iso3", envir = asNamespace("whep"))()
  expect_equal(length(unique(ldc)), length(ldc))

  pol <- sf::st_drop_geometry(whep::polities)
  known <- unique(stats::na.omit(c(pol$iso3_code, pol$iso3c)))
  expect_equal(setdiff(ldc, known), character(0))
})

test_that("the energy grouping says which live areas it cannot classify", {
  # Option 2 from whep#415, and the one that is safe whichever way the modelling question
  # goes: name the drop without inventing a region for it. Previously an area absent from
  # `gleam_geographic_hierarchy` got no row at all, and a join then lost it in silence.
  expect_warning(
    whep:::.energy_country_grouping(),
    "no row in"
  )
  msg <- tryCatch(
    {
      withCallingHandlers(
        whep:::.energy_country_grouping(),
        warning = function(w) stop(conditionMessage(w), call. = FALSE)
      )
      ""
    },
    error = function(e) conditionMessage(e)
  )

  # The five live territories, by name.
  for (nm in c("Bermuda", "Guam", "Nauru", "Palau", "Tuvalu")) {
    expect_match(msg, nm, fixed = TRUE)
  }
  # And NOT the thirteen that are legitimately absent: a country table should not carry
  # regional aggregates, and GLEAM is a present-day table so dissolved states have no
  # business in it. Naming those too would make the warning noise and get it muffled.
  for (nm in c(
    "RoW",
    "Czechoslovakia",
    "USSR",
    "Yugoslav SFR",
    "Belgium-Luxembourg"
  )) {
    expect_false(grepl(nm, msg, fixed = TRUE))
  }
})
