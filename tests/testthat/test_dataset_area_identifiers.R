# Several published datasets carry a country or area identifier that is NOT
# resolvable against the polities database. None of them is joined by anything in
# R/ today, so nothing is currently wrong in the outputs — but each is a trap
# primed for the first person who writes the obvious join, which is exactly how
# `regions_full$polity_code` (271 bare prefixes, joined to nothing) went unnoticed.
#
# Each set is baselined rather than asserted empty, and asserted in BOTH
# directions: an identifier that stops resolving fails, and one that starts
# resolving fails until it is removed from the baseline. So the lists shrink as
# the data is fixed and cannot quietly license a regression.
#
# RESOLVED for 17 of the 19 labels. whep-polities published a label -> polity map
# (data/final/label_alias_map.csv), this package embeds it as
# `polity_label_aliases`, and `resolve_polity_label()` reads it. The baselines
# below are now the labels that remain unresolved THROUGH that path, which is a
# much sharper claim than "unresolvable against polity names".
#
# Two are deliberately unresolved, and only for part of their range: `FSU` and
# `Belgium-Luxemburg` are aggregates the source keeps reporting after the
# territory stopped existing (1991 and 1999). Routing those years anywhere would
# attribute data to a polity that had ended, so they are left NA pending a
# constant-territory aggregate polity. See whep#389.

known_area_iso3 <- function() {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  unique(stats::na.omit(cw$area_iso3c))
}

known_area_names <- function() {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  unique(c(stats::na.omit(cw$area_name), stats::na.omit(cw$polity_name)))
}

# CLOSED. Held the ten FAO-style legacy codes in `mueller_synthetic_n$iso3c`
# (Belize is BLZ not BZE, Romania ROU not ROM, ZAR is Zaire, renamed COD in 1997).
# All ten now resolve via `resolve_polity_label()`. Kept as an empty vector so a
# regression names the code rather than silently re-baselining it.
mueller_unresolved_iso3 <- character(0)

# Reduced from nine to two. Seven name variants (Cape Verde, Swaziland, Turkey,
# Reunion, Cote d'Ivoire, DPRepublic of Korea, Occupied Palestinian Territory) now
# resolve, Cape Verde via a year-split at independence. The two that remain are
# aggregates reported past the end of the territory they name, and are unresolved
# only for the tail: FSU resolves 1961-1991 and Belgium-Luxemburg 1961-1999.
lassaletta_unresolved_names <- c("Belgium-Luxemburg", "FSU")

test_that("every mueller_synthetic_n label resolves to a polity", {
  m <- as.data.frame(whep::mueller_synthetic_n)
  labels <- unique(m$iso3c)

  # Resolved the way a consumer should: try the area-code iso3 first, then the
  # published alias map for anything it does not cover.
  direct <- labels %in% known_area_iso3()
  via_alias <- !is.na(resolve_polity_label(
    labels,
    "mueller-synthetic-n",
    # No year column — the table is application RATES — so aliases are scoped to
    # the extant polity's span and any year inside it resolves.
    2000L
  ))
  unresolved <- sort(labels[!direct & !via_alias])

  expect_setequal(unresolved, mueller_unresolved_iso3)
})

test_that("every lassaletta_grassland_share label resolves, bar two aggregates", {
  l <- as.data.frame(whep::lassaletta_grassland_share)
  labels <- unique(l$Country)

  direct <- labels %in% known_area_names()
  # Resolved at the FIRST year the source reports, 1961. Using a single year is
  # deliberate: a label that resolves for part of its range is still a gap for the
  # rest, and the two aggregates below are exactly that case.
  via_alias <- !is.na(resolve_polity_label(
    labels,
    "lassaletta-grassland-share",
    1961L
  ))
  unresolved <- sort(labels[!direct & !via_alias])

  expect_setequal(unresolved, character(0))

  # The real remaining gap: the tail years, after the territory ended.
  tail_unresolved <- sort(labels[
    is.na(resolve_polity_label(
      labels,
      "lassaletta-grassland-share",
      2009L
    )) &
      !direct
  ])
  expect_setequal(tail_unresolved, lassaletta_unresolved_names)
})

test_that("urban_n_reference's area_code is an ISO3 string, not a FAOSTAT area code", {
  # Documented as a fact rather than asserted away. The column is named
  # `area_code`, which everywhere else in this package means the numeric FAOSTAT
  # reporting area, but it holds "ESP". A consumer joining it to `area_code` gets
  # nothing, and gets it silently. The dataset is Spain-only reference data used
  # in a derivation, so renaming the column is a breaking change and the
  # maintainer's call.
  u <- as.data.frame(whep::urban_n_reference)
  expect_true(is.character(u$area_code))
  expect_setequal(unique(u$area_code), "ESP")
  # And the thing that makes it a trap: it does not resolve as an area code.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  expect_false(any(as.character(cw$area_code) %in% unique(u$area_code)))
  # It WOULD resolve as an iso3, which is what it actually is.
  expect_true("ESP" %in% known_area_iso3())
})

test_that("region-taxonomy columns are not expected to resolve to polities", {
  # conv_bouwman$region_bouwman is a 17-region source taxonomy, like
  # region_krausmann — owned by the model that uses it, not derivable from
  # polities (see test_region_classifications.R). Asserted so that a future
  # reader does not mistake it for an unfixed gap alongside the three above.
  b <- as.data.frame(whep::conv_bouwman)
  expect_true("region_bouwman" %in% names(b))

  regions <- unique(b$region_bouwman)
  expect_equal(length(regions), 17L)

  # The taxonomy mixes multi-country regions ("Eastern Africa", "South Asia")
  # with singletons that are one country ("Canada", "Japan", "USA"). So it is not
  # true that no value looks like a country identifier — "USA" is also an ISO3.
  # Pinned exactly, because a blanket "no region resolves as an area" assertion is
  # false here and pretending otherwise would just be a failing test someone
  # eventually deletes.
  expect_setequal(intersect(regions, known_area_iso3()), "USA")
})

# `resolve_polity_label()` must normalise labels the same way
# `matchlib.norm` does upstream, or the two sides resolve the same input
# differently. A cross-check over 6,627 probes (every alias x its boundary years x
# three sources, R against the Python implementation) found 25 disagreements from
# exactly this: the first R version only lowercased and squished whitespace.
#
# These pin each rule that diverged. The parenthetical one is the consequential
# one — upstream reduces "Sudan (former)" to "sudan", which merges it into the
# `sudan` rule set and changes which alias wins.
test_that("label normalisation matches the upstream matcher's rules", {
  # Accent folding: the map stores "Reunion", data may carry "Réunion".
  expect_equal(
    resolve_polity_label("Réunion", "lassaletta-grassland-share", 1980L),
    resolve_polity_label("Reunion", "lassaletta-grassland-share", 1980L)
  )

  # Parenthesised qualifiers are dropped, so these are the same label.
  expect_equal(
    resolve_polity_label("Sudan (former)", "faostat", 1990L),
    resolve_polity_label("Sudan", "faostat", 1990L)
  )

  # Case and surrounding whitespace are irrelevant.
  expect_equal(
    resolve_polity_label("  SWAZILAND  ", "lassaletta-grassland-share", 1980L),
    resolve_polity_label("Swaziland", "lassaletta-grassland-share", 1980L)
  )

  # Punctuation becomes a space, so an apostrophe variant still resolves.
  expect_equal(
    resolve_polity_label("Cote d'Ivoire", "lassaletta-grassland-share", 1980L),
    resolve_polity_label("Cote d Ivoire", "lassaletta-grassland-share", 1980L)
  )

  # And a label that genuinely has no alias still returns NA rather than
  # normalising its way onto something unrelated.
  expect_true(is.na(resolve_polity_label(
    "Not A Real Country",
    "lassaletta-grassland-share",
    1980L
  )))
})

# `resolve_polity_label()` is new public API, and its argument handling encodes
# deliberate choices that are easy to "simplify" into silent behaviour changes. These
# pin the documented contract rather than the implementation.
test_that("resolve_polity_label handles its arguments as documented", {
  # Vectorised over `label`, with `source`/`year` recycled from length 1.
  expect_equal(
    resolve_polity_label(c("BZE", "ZAR"), "mueller-synthetic-n", 2000L),
    c("BLZ-1981-2025", "COD-1960-2025")
  )

  # Element-wise years, which is what makes a year-split alias usable at all.
  expect_equal(
    resolve_polity_label(
      c("Cape Verde", "Cape Verde"),
      "lassaletta-grassland-share",
      c(1970L, 1990L)
    ),
    c("CPV-1886-1975", "CPV-1975-2025")
  )

  # A length that is neither 1 nor length(label) is a caller error, not something to
  # recycle silently — recycling would pair labels with the wrong sources.
  expect_error(
    resolve_polity_label(c("BZE", "ZAR"), c("a", "b", "c"), 2000L),
    "length 1 or the same length"
  )

  # NULL year matches only aliases with NO year scope. This is deliberate: inventing a
  # year to make a year-scoped alias apply would fabricate an answer. `Swaziland`'s
  # alias is year-scoped, so it must NOT resolve without one.
  expect_true(is.na(
    resolve_polity_label("Swaziland", "lassaletta-grassland-share", NULL)
  ))

  # NULL source matches only unscoped aliases, for the same reason in the other
  # dimension: `Swaziland`'s alias is scoped to a source, so a caller who does not say
  # which source their data came from gets NA rather than a guess.
  expect_true(is.na(resolve_polity_label("Swaziland", NULL, 1980L)))

  # Degenerate inputs return the same shape rather than erroring.
  expect_length(
    resolve_polity_label(character(0), "mueller-synthetic-n", 2000L),
    0L
  )
  expect_true(is.na(
    resolve_polity_label(NA_character_, "mueller-synthetic-n", 2000L)
  ))
})
