# Every area label the harmonization tables actually carry should resolve to a polity,
# or be explainable. This test holds that line in both directions.
#
# It exists because the labels in regions_full and polities_cats had never been run
# through resolve_polity_label() as a set. Doing so found ten short-form names with no
# alias at all — "United Kingdom", "South Korea", "Tanzania", "Ivory Coast",
# "Czech Republic", "DR Congo", "Lao", "Moldova", "North Korea" and "Eswatini". Each
# had a canonical long-form sibling that resolved perfectly ("United Kingdom of Great
# Britain and Northern Ireland", "Republic of Korea", ...), so the polity existed and
# only the routing was missing. They are now aliased upstream.
#
# A note on how to call the resolver, since getting it wrong invalidates the whole
# sweep: resolve_polity_label() needs BOTH `source` and `year`. Aliases are
# source-scoped and year-ranged, so omitting `source` matched only the 19 unscoped
# rows out of 284 labels and omitting `year` matched nothing at all. My first run of
# this sweep reported "281 of 284 labels unresolved", including France and Germany —
# an artifact of the call, not a finding about the data.

testthat::test_that("short-form area labels resolve to the expected polity", {
  # Pinned exactly rather than just "not NA": the failure mode worth catching is an
  # alias that resolves to the WRONG polity, which a non-NA check would wave through.
  expected <- c(
    "Czech Republic" = "CZE-1993-2025",
    "DR Congo" = "COD-1960-2025",
    "Ivory Coast" = "CIV-1960-2025",
    "Lao" = "LAO-1954-2025",
    "Moldova" = "MDA-1991-2025",
    "North Korea" = "PRK-1948-2025",
    "South Korea" = "KOR-1948-2025",
    "Tanzania" = "TZA-1964-2025",
    "United Kingdom" = "GBR-1921-2025",
    "Eswatini" = "SWZ-1894-2025"
  )
  got <- resolve_polity_label(names(expected), source = "faostat", year = 2020L)
  testthat::expect_equal(got, unname(expected))
})

testthat::test_that("labels that still do not resolve are all explainable", {
  # Four categories, none of which is a defect:
  #
  #   dissolved before 2020 — Czechoslovakia, USSR, Yugoslav SFR, Serbia and
  #     Montenegro, Ethiopia PDR, Netherlands Antilles, Belgium-Luxembourg,
  #     Neutral Zone. Their polities exist; they just do not span 2020.
  #   not a polity — China (FAOSTAT area 351, the mainland+HK+Macao+Taiwan aggregate,
  #     deliberately unmapped so it cannot double-count), RoW, Unspecified,
  #     Others (adjustment), Antarctica.
  #   folded into a rest-of-world polity — the dependencies and small territories:
  #     Aruba, Bermuda, Guam, Mayotte, Reunion, Martinique and the rest. Undoing that
  #     fold is an approved but separate change; when it lands these start resolving
  #     and this baseline shrinks, which the setequal below will report.
  #   a vendored encoding defect — "CuraÃ§ao" is "Curaçao" read as Latin-1, and both
  #     "Reunion" and "Réunion" ship as separate labels. Left as found rather than
  #     hand-patched, because inst/extdata/harmonization/regions_full.csv is a
  #     vendored table; reported instead.
  #
  # Bidirectional on purpose: a NEW unresolved label fails, and a baselined one that
  # starts resolving fails too, so the list can only shrink deliberately.
  baseline <- c(
    "Anguilla",
    "Antarctica",
    "Aruba",
    "Belgium-Luxembourg",
    "Bermuda",
    "Bouvet Island",
    "British Indian Ocean Territory",
    "Canton and Enderbury Islands",
    "Cayman Islands",
    "China",
    "Christmas Island",
    "Cocos (Keeling) Islands",
    "CuraÃ§ao",
    "Czechoslovakia",
    "Ethiopia PDR",
    "French Southern and Antarctic Territories",
    "Gibraltar",
    "Guadeloupe",
    "Guam",
    "Heard and McDonald Islands",
    "Holy See",
    "Johnston Island",
    "Martinique",
    "Mayotte",
    "Midway Island",
    "Netherlands Antilles",
    "Neutral Zone",
    "Northern Mariana Islands",
    "Others (adjustment)",
    "Pacific Islands Trust Territory",
    "Pitcairn Islands",
    "Reunion",
    "Réunion",
    "RoW",
    "Saint Helena, Ascension and Tristan da Cunha",
    "Saint-Martin (French Part)",
    "Serbia and Montenegro",
    "South Georgia and the South Sandwich Islands",
    "Svalbard and Jan Mayen Islands",
    "Tokelau",
    "Turks and Caicos Islands",
    "Unspecified",
    "US Minor Is.",
    "USSR",
    "Wake Island",
    "Wallis and Futuna Islands",
    "Yugoslav SFR"
  )

  rf <- as.data.frame(whep::regions_full)
  pc <- as.data.frame(whep::polities_cats)
  labels <- unique(stats::na.omit(c(
    rf$name,
    rf$FAOSTAT_name,
    pc$name,
    pc$FAOSTAT_name
  )))
  labels <- sort(labels[nzchar(labels)])
  # Guard the sweep: if the tables stop carrying labels this must not pass by
  # resolving nothing.
  testthat::expect_gt(length(labels), 200L)

  unresolved <- labels[is.na(
    resolve_polity_label(labels, source = "faostat", year = 2020L)
  )]
  testthat::expect_setequal(unresolved, baseline)
})
