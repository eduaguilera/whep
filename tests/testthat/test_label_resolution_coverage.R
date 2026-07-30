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
  #   FIXED, and the entry below now reads correctly: "CuraÃ§ao" was "Curaçao" decoded as
  #     Latin-1. It was left as found and reported (whep#399) on the reasoning that
  #     inst/extdata/harmonization/regions_full.csv is vendored — but repairing on READ in
  #     data-raw leaves the vendored file untouched, which is the same override pattern
  #     `fill_adb_region()` already uses two files over. The label still does not resolve,
  #     because Curaçao has no alias in any spelling; it is simply spelled correctly now.
  #
  #     The companion claim in that issue was WRONG and is withdrawn: "Reunion" and
  #     "Réunion" are not duplicate spellings of one label. They are area 182's `name` and
  #     `FAOSTAT_name`, which is the documented short-versus-canonical pattern — exactly
  #     like Turkey/Türkiye and Czech Republic/Czechia.
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
    "Curaçao",
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

# Case is the last way an exact label match fails invisibly, after mojibake and
# whitespace. The resolver is CASE-INSENSITIVE, which the sweep established rather than
# assumed: "Algeria", "algeria" and "ALGERIA" all resolve. So the 62 label keys that exist
# in two capitalisations are not duplicates — they are an any-source historical chain
# entered in lower case alongside source-scoped rows entered in title case.
#
# What matters is whether two spellings of one key ever disagree about the polity in a
# shared year, because then the answer depends on which row the resolver reaches first.
# Swept every key and year: 26 do, in three classes.
#
#   15  same-family boundary years. An alias `year_end` is inclusive while a polity
#       `end_year` is exclusive, so adjacent periods both cover the boundary. Benign, and
#       the same convention the hand-curated Cape Verde rows use.
#    7  predecessor/successor pairs at a transition year across a family rename:
#       Bechuanaland/Botswana 1966, Danish West Indies/Virgin Islands 1917, Northern
#       Rhodesia/Zambia 1964, Southern Rhodesia/Zimbabwe 1964 and similar. The same
#       boundary phenomenon with a different prefix either side.
#    4  genuine territorial coexistence: ethiopia (AOI/ETH), israel (PAL/ISR at 1948),
#       somalia (two protectorates at once), china, manchuria province of (CHN/MAN).
#
# ETHIOPIA IS THE ONE WORTH WATCHING, and it is pinned below. Two rows assert different
# FAMILIES for the identical 1936-1941 range, so "Ethiopia" in 1938 is Africa Orientale
# Italiana for faostat and Ethiopia for fao1952. Both readings are historically defensible
# — the country was occupied and administered as part of AOI, while the yearbooks report
# its own figures — and the mechanism is working. What is missing is any record that the
# divergence is deliberate, which is whep-polities#53.
#
# Pinned rather than asserted-away because the resolution is not wrong, only undocumented:
# if it changes, that should be a decision someone took and not a silent shift in what
# "Ethiopia, 1938" means.
testthat::test_that("label resolution is case-insensitive", {
  for (variant in c("Algeria", "algeria", "ALGERIA")) {
    testthat::expect_equal(
      resolve_polity_label(variant, source = "faostat", year = 2000L),
      "DZA-1962-2025",
      info = paste0("case variant did not resolve: ", variant)
    )
  }
})

testthat::test_that("the Ethiopia 1938 divergence stays as documented", {
  # Source-scoped rows override the any-source default, which is the mechanism.
  testthat::expect_equal(
    resolve_polity_label("Ethiopia", source = "fao1952", year = 1938L),
    "ETH-1936-1941"
  )
  testthat::expect_equal(
    resolve_polity_label("Ethiopia", source = "iia", year = 1938L),
    "ETH-1936-1941"
  )
  # And a source without its own row gets the territorial reading.
  testthat::expect_equal(
    resolve_polity_label("Ethiopia", source = "faostat", year = 1938L),
    "AOI-1936-1941"
  )
  testthat::expect_equal(
    resolve_polity_label("Ethiopia", source = NULL, year = 1938L),
    "AOI-1936-1941"
  )
})
