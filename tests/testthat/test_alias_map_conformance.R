# Does this package's resolver actually honour the map upstream publishes?
#
# `resolve_polity_label()` is a THIRD implementation of "which polity does this label mean
# in this year". whep-polities has two — `match.R`, which writes the aliases, and
# `matchlib.Matcher`, which reads them — and `crosscheck_matchers.py` compares those to each
# other. Nothing compared this one to either, so every test of it until now asserted
# hand-picked cases rather than the contract as a whole.
#
# This asserts the whole thing: for EVERY ranged alias in the published map, resolving that
# label under that source in a year inside its range must return that alias's target. All
# 851 do.
#
# THE PROBE YEAR IS year_start + 1, NOT THE MIDPOINT, and the reason is a real convention
# rather than fussiness. An alias `year_end` is inclusive while a polity `end_year` is
# exclusive, so an alias's last year is also its successor's first, and which polity a
# boundary year resolves to is settled by the shared-transition rule rather than by the map
# alone. Probing midpoints found three apparent divergences upstream — `romania` 1919,
# `serbia` 1919, `china 22 provinces` 1950 — every one of them a two-year range whose
# midpoint IS the boundary. They were an artefact of the probe, not a defect, which is why
# this samples strictly inside the range where the range allows it.
#
# What this cannot see, stated so nobody assumes otherwise: the NAME route. That route
# deliberately diverges from upstream's — it refuses ambiguous names, refuses names of
# deliberately-unmapped areas, and lets a year-covering rule outrank a name — so comparing
# it against `matchlib` would compare two things that are meant to differ. Those guards are
# tested by identity in test_resolve_polity_label_name_route.R instead.

test_that("every ranged alias in the published map resolves to its own target", {
  aliases <- as.data.frame(whep::polity_label_aliases)
  ranged <- aliases[!is.na(aliases$year_start) & !is.na(aliases$year_end), ]
  # Non-vacuous: the map must actually contain ranged aliases, or this passes on nothing.
  expect_gt(nrow(ranged), 500L)

  probe <- ifelse(
    ranged$year_end > ranged$year_start,
    ranged$year_start + 1L,
    ranged$year_start
  )
  got <- resolve_polity_label(
    ranged$source_label,
    source = ranged$source,
    year = probe
  )

  # No NAs: an alias that cannot resolve its own label under its own source in its own
  # year range is unreachable, and unreachable is indistinguishable from unmapped.
  expect_equal(sum(is.na(got)), 0L)
  # And no substitutions: a different code means this package and the published map
  # disagree about what the source meant, which is the whole thing being ruled out.
  expect_equal(got, ranged$polity_code)
})

test_that("the conformance check would notice a substituted target", {
  # The assertion above passes on all 851 rows, so it has to be shown capable of failing.
  # Mutating the resolver is not possible from here; mutating the EXPECTATION is, and it
  # exercises the same comparison: one target swapped for another real code must not match.
  aliases <- as.data.frame(whep::polity_label_aliases)
  ranged <- aliases[!is.na(aliases$year_start) & !is.na(aliases$year_end), ]
  probe <- ifelse(
    ranged$year_end > ranged$year_start,
    ranged$year_start + 1L,
    ranged$year_start
  )
  got <- resolve_polity_label(
    ranged$source_label,
    source = ranged$source,
    year = probe
  )

  tampered <- ranged$polity_code
  other <- setdiff(unique(tampered), tampered[1])[1]
  tampered[1] <- other
  expect_false(isTRUE(all.equal(got, tampered)))
})
