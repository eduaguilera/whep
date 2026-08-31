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

# THE RATCHET (whep#787), AND WHAT TRIPPED IT.
#
# UN WPP 2024 has no `ANT` record in any year; it publishes the successor
# territories instead (CUW 156.879k, SXM 33.794k, BES 20.558k in 2010). So the
# only way a population source could supply area 151 is through the `successor`
# relation -- and when this file was written the polities database published
# none for `ANT-1961-2010`, while Sint Maarten and the BES islands had no polity
# row at all. These tests were that disproof, kept checked so it could not rot,
# and written to FAIL the day upstream closed the gap.
#
# IT FAILED, on the 2026-08-25 whep-polities re-sync, which publishes
# `ANT-1961-2010 -> CUW-2010-2025; SXM-2010-2025; BES-2010-2025` and mints
# `SXM-2010-2025` and `BES-2010-2025` as polities in their own right. That
# answers the question whep#787/#870 asked -- whether `polities$successor`
# already publishes the relation, because if it does, reconstructing area 151 is
# a LOOKUP rather than a hardcoded list in this package. It does.
#
# So the claims below are the same two as before, read the other way round: the
# reach classification is right, and the reconstruction comes from the published
# column rather than from anything written here.
#
# REACHABLE IS STILL NOT THE SAME AS SAFE TO SUM: read the note at the top of
# `R/population_reach.R` before turning any of this into a denominator.
testthat::test_that("area 151 reads through the successors upstream publishes", {
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

  # BY LOOKUP, NOT BY LIST. `.polity_successor_edges()` splits
  # `polities$successor` and nothing else, so asserting the three codes against
  # it pins that the answer above is read from the published relation. Writing
  # the same three codes into the package would not satisfy this.
  testthat::expect_setequal(
    whep:::.polity_successor_edges()[["ANT-1961-2010"]],
    c("CUW-2010-2025", "SXM-2010-2025", "BES-2010-2025")
  )
})

testthat::test_that("no reporting area outside bucket 999 is stranded", {
  out <- whep::population_source_reach(
    union(.live_polity_iso3(), c("CUW", "SXM", "BES", "ABW", "XKX"))
  )
  stranded <- out |>
    dplyr::filter(.data$reach == "unreachable", .data$area_code != 999L)
  # Area 151 was the last one outside the Rest-of-World bucket, and closing it
  # emptied the set. A code appearing here is a reporting territory no
  # present-day-ISO3 population source can reach even through succession, which
  # is the whole subject of whep#787; it is a thing to go and fix upstream, not
  # a reason to widen the filter.
  testthat::expect_identical(stranded$polity_code, character(0))
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

# The walk must be reading the edges it is given, not the shipped snapshot by
# some other route. Substituting a NARROWER edge set than upstream now
# publishes -- Curacao and Sint Maarten, no BES -- must produce exactly those
# two, which the un-mocked test above cannot show because it agrees with the
# snapshot. Written when upstream published no edge at all and this had to
# inject one; kept because a controlled edge set is what makes the assertion
# above mean "the relation drove this" rather than "some list happened to
# match".
testthat::test_that("the walk reports exactly the successor edges it is given", {
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
# code list -- the difference between "no denominator" and a denominator built
# from nothing. Sint Maarten stands in for it by being withheld from the
# vocabulary: a source that simply does not carry `SXM` leaves the ANT edge
# resolving to no population at all. Until the 2026-08-25 re-sync SXM had no
# polity of its own and the crosswalk filed it under the Netherlands', so the
# same edge drawn one step differently would have answered area 151 with the
# Netherlands' 17 million people.
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

# REACHED IS NOT THE SAME EXTENT. `reach` says a code exists; it does not say
# the code covers the same ground as the period. For ten polities upstream
# publishes a partition in which the parent keeps one part's ISO3
# (`.successor_code_reuse()`), so the walk stops on the parent and the answer is
# a territory short -- and on the `"direct"` branch it looks certain.
# `extent_exceeds_iso3` is the flag, and the two worst cases are area 206
# Sudan (1956-2011), whose `SDN` excludes South Sudan, and area 272's 2006-2007
# period, whose `SRB` excludes Kosovo (whep#863). Both read `"direct"`.
testthat::test_that("the extent flag fires where the ISO3 is narrower", {
  out <- whep::population_source_reach(
    union(.live_polity_iso3(), c("CUW", "SXM", "BES", "XKX"))
  )
  flagged <- dplyr::filter(out, .data$extent_exceeds_iso3)
  testthat::expect_true(
    all(c("SUD-1956-2011", "SRB-2006-2008") %in% flagged$polity_code)
  )
  sudan <- dplyr::filter(out, .data$polity_code == "SUD-1956-2011")
  testthat::expect_identical(sudan$area_code, 206L)
  testthat::expect_identical(sudan$reach, "direct")
  testthat::expect_true(sudan$extent_exceeds_iso3)

  # Reached THROUGH such a stop counts too: area 186 Serbia and Montenegro
  # resolves to MNE + SRB, and the SRB it lands on is the Kosovo-inclusive one.
  scg <- dplyr::filter(out, .data$polity_code == "SCG-1992-2006")
  testthat::expect_identical(scg$iso3_reached, "MNE, SRB")
  testthat::expect_true(scg$extent_exceeds_iso3)

  # Czechoslovakia partitions into two codes neither of which it reuses, so its
  # successor sum really is the whole territory and the flag must stay FALSE.
  czs <- dplyr::filter(out, .data$polity_code == "F51-1947-1993")
  testthat::expect_identical(czs$iso3_reached, "CZE, SVK")
  testthat::expect_false(czs$extent_exceeds_iso3)

  # An unreachable period reaches nothing, so nothing can be short of it.
  unreachable <- dplyr::filter(out, .data$reach == "unreachable")
  testthat::expect_false(any(unreachable$extent_exceeds_iso3))
})
