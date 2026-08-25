# Tests for population_source_reach().
#
# The suite is fully offline, so the "present-day ISO3 vocabulary" a real
# population source publishes is stood in for by the ISO3 codes of the polities
# that are still live in the shipped snapshot. That is the right stand-in and
# not a convenience: the property under test is whether a source keyed on a
# CURRENT territory can reach a DISSOLVED one, and a live polity's ISO3 is
# exactly what such a source publishes. It is also a superset of UN WPP's
# `Country/Area` vocabulary in the direction that matters -- if an area is
# unreachable from every live polity's ISO3, no real source can reach it either.

# A small crosswalk in the shape `population_source_reach()` reads, with real
# polity codes so the successor walk has something to walk.
.reach_crosswalk <- function() {
  spans <- tibble::tribble(
    ~polity_area_code, ~polity_code,    ~map_year_start, ~map_year_end,
    51L,               "F51-1947-1993", 1961L,           1992L,
    167L,              "CZE-1993-2025", 1993L,           2024L,
    151L,              "ANT-1961-2010", 1961L,           2010L,
    150L,              "NLD-1830-2025", NA_integer_,     NA_integer_
  )
  names <- tibble::tribble(
    ~polity_code,    ~polity_name,           ~iso3_code,
    "F51-1947-1993", "Czechoslovakia",       "CSK",
    "CZE-1993-2025", "Czechia",              "CZE",
    "ANT-1961-2010", "Netherlands Antilles", "ANT",
    "NLD-1830-2025", "Netherlands",          "NLD"
  )
  dplyr::left_join(spans, names, by = "polity_code")
}

# The ISO3 codes of every polity still live in the shipped snapshot: the
# stand-in vocabulary described at the top of this file.
.live_polity_iso3 <- function() {
  polities <- whep::polities
  live <- polities$end_year >= max(polities$end_year, na.rm = TRUE)
  sort(unique(polities$iso3_code[live & !is.na(polities$iso3_code)]))
}

testthat::test_that("a period whose own ISO3 is published reads as direct", {
  out <- whep::population_source_reach(
    c("CZE", "SVK"),
    crosswalk = .reach_crosswalk()
  )
  czechia <- dplyr::filter(out, .data$polity_code == "CZE-1993-2025")
  testthat::expect_identical(czechia$reach, "direct")
  testthat::expect_identical(czechia$n_iso3, 1L)
  testthat::expect_identical(czechia$iso3_reached, "CZE")
})

testthat::test_that("a dissolved federation reads through its successors", {
  out <- whep::population_source_reach(
    c("CZE", "SVK"),
    crosswalk = .reach_crosswalk()
  )
  csk <- dplyr::filter(out, .data$polity_code == "F51-1947-1993")
  testthat::expect_identical(csk$reach, "successor")
  testthat::expect_identical(csk$n_iso3, 2L)
  testthat::expect_identical(csk$iso3_reached, "CZE, SVK")
})

testthat::test_that("a period with no declared reporting years is dropped", {
  out <- whep::population_source_reach(
    c("CZE", "SVK", "NLD"),
    crosswalk = .reach_crosswalk()
  )
  testthat::expect_false("NLD-1830-2025" %in% out$polity_code)
})

testthat::test_that("iso3_codes must be a non-empty character vector", {
  testthat::expect_error(
    whep::population_source_reach(character(0)),
    class = "whep_bad_iso3_vocabulary"
  )
  testthat::expect_error(
    whep::population_source_reach(151L),
    class = "whep_bad_iso3_vocabulary"
  )
})

# THE RATCHET (whep#787).
#
# UN WPP 2024 has no `ANT` record in any year; it publishes the successor
# territories instead (CUW 156.879k, SXM 33.794k, BES 20.558k in 2010). So the
# only way a population source could supply area 151 is through the `successor`
# relation -- and the polities database publishes none for `ANT-1961-2010`,
# while Sint Maarten and the BES islands have no polity row at all. This test is
# that disproof, kept checked so it cannot rot.
#
# IT FAILS WHEN UPSTREAM FIXES THE GAP, which is the point: the day
# whep-polities publishes ANT's successors, area 151 becomes reconstructable and
# whep#787 becomes actionable. Change this test then, and read the note at the
# top of `R/population_reach.R` first -- reachable is not the same as safe to
# sum.
# UPSTREAM FIXED THE GAP, and this is the changed test the note above asked for.
# The #890 snapshot resync published ANT's three successors, so area 151 is
# reconstructable and whep#787 is actionable. Read the note at the top of
# `R/population_reach.R` before acting on it: reachable is not safe to sum, and
# this territory has its own version of the Kosovo shortfall recorded there --
# see the Aruba assertion below.
testthat::test_that("area 151 reads through its successors once upstream names them", {
  vocabulary <- union(
    .live_polity_iso3(),
    c("CUW", "SXM", "BES", "ABW", "XKX")
  )
  out <- whep::population_source_reach(vocabulary)
  ant <- dplyr::filter(out, .data$polity_code == "ANT-1961-2010")
  testthat::expect_identical(nrow(ant), 1L)
  testthat::expect_identical(ant$area_code, 151L)
  testthat::expect_identical(ant$reach, "successor")
  testthat::expect_identical(ant$n_iso3, 3L)
  testthat::expect_identical(ant$iso3_reached, "BES, CUW, SXM")

  # THE CAVEAT, pinned so a denominator built on this cannot claim more than the
  # relation gives. Those three are the 2010 partition. Aruba left the
  # Netherlands Antilles in 1986, and upstream models it as continuous --
  # `ABW-1800-2025` carries no predecessor -- so it is not in the successor set
  # and never will be by this walk. A successor sum therefore reaches the
  # territory as upstream defines it, not FAOSTAT's area 151 as it stood before
  # 1986. Same shape as the Yugoslav/Kosovo shortfall in the file note.
  testthat::expect_false(grepl("ABW", ant$iso3_reached, fixed = TRUE))
  # Base subsetting, not `dplyr::filter()`: `whep::polities` carries an `sfc`
  # geometry column, which dplyr cannot row-slice.
  abw_pred <- whep::polities$predecessor[
    match("ABW-1800-2025", whep::polities$polity_code)
  ]
  testthat::expect_true(is.na(abw_pred) || !nzchar(abw_pred))
})

testthat::test_that("no reporting area outside bucket 999 is unreachable", {
  # Area 151 was the last one. It was `unreachable` until the #890 resync, and
  # the count is asserted at zero rather than as a set, so a NEW stranded area
  # from any later vocabulary change is a failure here.
  out <- whep::population_source_reach(
    union(.live_polity_iso3(), c("CUW", "SXM", "BES", "ABW", "XKX"))
  )
  stranded <- out |>
    dplyr::filter(.data$reach == "unreachable", .data$area_code != 999L)
  testthat::expect_identical(nrow(stranded), 0L)
})

testthat::test_that("the other dissolved federations do read through successors", {
  out <- whep::population_source_reach(.live_polity_iso3())
  federations <- out |>
    dplyr::filter(
      .data$polity_code %in%
        c(
          "BLX-1850-1999",
          "F51-1947-1993",
          "SCG-1992-2006",
          "F228-1945-1991",
          "F248-1947-1991"
        )
    )
  testthat::expect_identical(nrow(federations), 5L)
  testthat::expect_true(all(federations$reach == "successor"))
  testthat::expect_true(all(federations$n_iso3 >= 2L))
})

# The ratchet above must fail for the right reason -- because upstream publishes
# no edge, not because the walk cannot see one. Inject the edge whep-polities is
# missing and the same call must report `"successor"`. Without this, the ratchet
# would also pass if `.successor_iso3_map()` silently stopped working.
testthat::test_that("the ratchet flips once the successor edge exists", {
  edges <- whep:::.polity_successor_edges()
  edges[["ANT-1961-2010"]] <- c("CUW-2010-2025", "SXM-2010-2025")
  iso3 <- whep:::.polity_iso3_lookup()
  iso3[["SXM-2010-2025"]] <- "SXM"
  testthat::local_mocked_bindings(
    .polity_successor_edges = function() edges,
    .polity_iso3_lookup = function() iso3
  )
  out <- whep::population_source_reach(
    union(.live_polity_iso3(), c("CUW", "SXM", "BES"))
  )
  ant <- dplyr::filter(out, .data$polity_code == "ANT-1961-2010")
  testthat::expect_identical(ant$reach, "successor")
  testthat::expect_identical(ant$iso3_reached, "CUW, SXM")
})

# The NEAR MISS. A successor edge that exists but leads nowhere the source
# publishes must still read `"unreachable"`, not `"successor"` with an empty
# code list. Sint Maarten is exactly that case today: the crosswalk gives it the
# Netherlands' polity, so an edge drawn to `NLD-1830-2025` would resolve area
# 151 to the Netherlands' 17 million people.
testthat::test_that("a successor edge outside the vocabulary stays unreachable", {
  edges <- whep:::.polity_successor_edges()
  edges[["ANT-1961-2010"]] <- "SXM-2010-2025"
  iso3 <- whep:::.polity_iso3_lookup()
  iso3[["SXM-2010-2025"]] <- "SXM"
  testthat::local_mocked_bindings(
    .polity_successor_edges = function() edges,
    .polity_iso3_lookup = function() iso3
  )
  out <- whep::population_source_reach(setdiff(.live_polity_iso3(), "SXM"))
  ant <- dplyr::filter(out, .data$polity_code == "ANT-1961-2010")
  testthat::expect_identical(ant$reach, "unreachable")
  testthat::expect_identical(ant$n_iso3, 0L)
})
