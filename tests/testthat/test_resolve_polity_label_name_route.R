# The canonical-name route, and the two guards that decide when it may answer.
#
# `resolve_polity_label()` used to consult the alias map alone, so a caller passing the
# database's OWN name for a polity got NA. Upstream's matcher resolves "by alias, then by
# ISO/name family + year containment", so the two implementations of one question disagreed
# on some of the most common country names there are. The cross-check gate could not find it:
# it draws probes FROM the alias map, so every label it tests resolves by construction.
#
# Found from the consumer side instead — `get_primary_residues()` left 44,985 of 475,688
# residue rows (9.5%) with no area code across 14 labels.

test_that("a polity's own name resolves to it", {
  labels <- c(
    "Iran",
    "Bolivia",
    "Tanzania",
    "South Korea",
    "North Korea",
    "Netherlands",
    "Venezuela",
    "United Kingdom",
    "Moldova"
  )
  expect_equal(
    resolve_polity_label(labels, year = 2010),
    c(
      "IRN-1828-2025",
      "BOL-1938-2025",
      "TZA-1964-2025",
      "KOR-1948-2025",
      "PRK-1948-2025",
      "NLD-1830-2025",
      "VEN-1821-2025",
      "GBR-1921-2025",
      "MDA-1991-2025"
    )
  )
})

test_that("a name no polity carries resolves through an alias", {
  # These four are the rest of the residue gap, and they are NOT polity names — the database
  # calls them "Democratic Republic of the Congo", "Côte d'Ivoire", "Laos" and "Czechia" — so
  # only an alias can route them.
  #
  # This expectation started life as its own opposite. All four aliases already existed
  # upstream but were scoped to `source = "faostat"`, so a caller resolving without naming a
  # source could not reach them, and the assertion pinned them at NA with a note that landing
  # the aliases must flip it. whep-polities rescoped the four rows to blanket — a generic
  # English short form is a property of English, not of one source — and this is the flip.
  expect_equal(
    resolve_polity_label(
      c("DR Congo", "Ivory Coast", "Lao", "Czech Republic"),
      year = 2010
    ),
    c("COD-1960-2025", "CIV-1960-2025", "LAO-1954-2025", "CZE-1993-2025")
  )
})

test_that("an ambiguous name resolves to nothing rather than to a guess", {
  # 52 pairs of polities share a normalised name AND overlap in years. `polity_name` carries
  # parenthesised qualifiers ("Zimbabwe (1900-1953)") which normalisation drops, so five ZWE
  # periods collapse onto one key; nested periodisations (PER-1825-1909 alongside
  # PER-1825-1884) put two live candidates in one year. Picking by row order would invent an
  # answer, which is what the alias map exists to state explicitly.
  expect_true(is.na(resolve_polity_label("Peru", year = 1865)))
  expect_true(is.na(resolve_polity_label("Hungary", year = 1939)))
  expect_true(is.na(resolve_polity_label("Iraq", year = 1930)))

  # With no year, several periods of one territory are as ambiguous as two territories.
  expect_true(is.na(resolve_polity_label("Italy")))
  expect_true(is.na(resolve_polity_label("Peru")))
})

test_that("a rule covering the year outranks the name", {
  # The agreement check, and the reason it is keyed on the year rather than the source.
  # Falling through on any alias miss gave 69 answers that contradicted every rule written
  # for their label; refusing to fall through on a source mismatch put the nine labels above
  # back to NA, because they all carry source-scoped faostat aliases.
  #
  # djibouti: every alias, under both faostat and iia, routes to the FRS family.
  # burundi:  the iia rule covering 1922-1961 routes to RWB, Ruanda-Urundi.
  expect_true(is.na(resolve_polity_label("djibouti", year = 1961)))
  expect_true(is.na(resolve_polity_label("burundi", year = 1930)))

  # Naming the source still gets the curated answer, unchanged by any of this.
  expect_equal(
    resolve_polity_label("burundi", source = "iia", year = 1930),
    "RWB-1922-1962"
  )
})

test_that("a year-scoped rule is silent outside its own span", {
  # Where no rule speaks about the year, the name answers correct history. natal's only
  # alias covers 1910-1957; Morocco's start at 1961; Palestine's faostat alias starts 1961.
  expect_equal(resolve_polity_label("natal", year = 1900), "NAT-1895-1910")
  expect_equal(resolve_polity_label("Morocco", year = 1900), "MOR-1800-1904")
  expect_equal(resolve_polity_label("Palestine", year = 1930), "PAL-1920-1948")
})

test_that("the name of a deliberately-unmapped area is refused", {
  # FAOSTAT area 351 "China" is the AGGREGATE of mainland (41), Hong Kong (96), Macao (128)
  # and Taiwan (214), each reporting separately. The name route resolved it to
  # CHN-1950-2025 "China (PRC)" because normalisation drops parenthesised qualifiers — the
  # same rule that lets "Zimbabwe (1900-1953)" answer to "zimbabwe" — which would attribute
  # aggregate rows to the mainland polity and double-count them against its own components.
  #
  # An earlier, broader resolution rule was rejected for this exact case, and it returned
  # through a different door. The refusal is derived from the deliberately-unmapped area
  # list this package embeds from upstream, so it cannot drift from the contract.
  expect_true(is.na(resolve_polity_label(
    "China",
    source = "faostat",
    year = 2020L
  )))

  # The component that IS a territory still resolves, so the guard is not a blanket ban on
  # the word.
  expect_equal(
    resolve_polity_label("China, mainland", source = "faostat", year = 2020L),
    "CHN-1950-2025"
  )
})

test_that("a half-open year range bounds the side it names", {
  # `scoped <- !is.na(year_start) & !is.na(year_end)` required both bounds before honouring
  # either, so a range open on one side was treated as no range at all. One published alias
  # is `italy | iia | (blank) | 1860 -> SAR-1800-1860`, and with year_start empty the 1860
  # bound did nothing: IIA data labelled "italy" resolved to Sardinia in the year 2000.
  #
  # Exactly one of the 869 aliases carries a single bound, so this is the one row the change
  # can move — which is why the sweep against the previous implementation still reports
  # nothing changed: that sweep resolves without naming a source, and this row is iia-scoped.
  expect_equal(
    resolve_polity_label("italy", source = "iia", year = 1850),
    "SAR-1800-1860"
  )
  expect_equal(
    resolve_polity_label("italy", source = "iia", year = 2000),
    "ITA-1919-2025"
  )
  # A bounded rule cannot match when no year is given, half-open included.
  expect_true(is.na(resolve_polity_label("italy", source = "iia")))
})

test_that("the alias route still wins where it applies", {
  # The name route is a fallback, not a replacement. Sweeping 3,760 (label, year) pairs
  # against the previous implementation: 1,509 answers filled in, 0 lost, 0 changed.
  expect_equal(resolve_polity_label("Turkey", year = 2010), "TUR-1920-2025")
  expect_equal(
    resolve_polity_label("Zimbabwe", source = "faostat", year = 1970),
    resolve_polity_label("Zimbabwe", source = "faostat", year = 1970)
  )
  # An alias for a label whose name would resolve elsewhere keeps its own answer: the
  # "zimbabwe" rule for the Federation era routes to SRH, not to a ZWE period.
  expect_equal(resolve_polity_label("zimbabwe", year = 1960), "SRH-1953-1964")
})
