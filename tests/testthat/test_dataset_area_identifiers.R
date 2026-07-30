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

test_that("urban_n_reference's area_code is a FAOSTAT code, as its name says", {
  # This test asserted the OPPOSITE until the defect was fixed: that the column named
  # `area_code` held the ISO3 string "ESP". It was written to document the trap rather
  # than assert it away, on the reasoning that renaming the column would be a breaking
  # change and the maintainer's call.
  #
  # Renaming was never the only option. The column name is right — `area_code` means a
  # numeric FAOSTAT area everywhere else in this package, and the same workflow's toy
  # example uses 203L for Spain — so the VALUE was what disagreed. Resolving "ESP" to 203
  # through the crosswalk at build time keeps the name, keeps the provenance in the
  # vendored CSV, and makes the series joinable. No output changes, because nothing in the
  # package joins it: it is a benchmark a reader compares against by hand, which is how
  # the column could hold a string for as long as it did (whep#401).
  #
  # Resolved through the crosswalk rather than written as a literal 203, so a renamed or
  # re-coded territory becomes a build error instead of a wrong join.
  u <- as.data.frame(whep::urban_n_reference)
  expect_true(is.numeric(u$area_code))
  expect_setequal(unique(u$area_code), 203L)

  # And it now resolves as an area code, which is the property the old version asserted
  # the absence of.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  expect_true(203L %in% as.integer(cw$area_code))
  expect_setequal(
    unique(cw$area_name[which(as.integer(cw$area_code) == 203L)]),
    "Spain"
  )
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

# `polity_label_aliases` and `polities` are two embedded datasets built by the same
# data-raw script but stored as separate .rda files, so they can be regenerated
# independently — and nothing asserted they agree. data-raw does abort when an alias
# targets a polity the package does not carry, but that is BUILD time. Rebuilding
# polities.rda after an upstream retirement without rebuilding the alias copy would
# leave `resolve_polity_label()` returning codes a consumer cannot use, and no test
# would notice.
#
# This needs no upstream checkout — it compares two datasets that ship in the package —
# so unlike the contract tests it runs unconditionally, including on CI.
test_that("every label alias targets a live polity this package carries", {
  aliases <- as.data.frame(whep::polity_label_aliases)
  pol <- as.data.frame(whep::polities)

  # Non-vacuous: a silently empty dataset would make everything below pass for free.
  expect_gte(nrow(aliases), 500L)

  expect_setequal(
    names(aliases),
    c(
      "source_label",
      "source",
      "year_start",
      "year_end",
      "polity_code",
      "common_name",
      "confidence",
      # New in the upstream contract: source rows actually observed for this label,
      # 0 when the label is merely mappable. Consumed by the crosswalk to decide which
      # FABIO rest-of-world areas may be folded — an area that reports data must not be
      # collapsed into an aggregate.
      "observed_rows"
    )
  )

  blank <- sum(is.na(aliases$polity_code) | aliases$polity_code == "")
  expect_equal(
    blank,
    0L,
    info = "an alias with no target can never route anything"
  )

  unknown <- sort(setdiff(unique(aliases$polity_code), pol$polity_code))
  expect_equal(
    length(unknown),
    0L,
    info = paste0(
      "alias targets absent from whep::polities: ",
      paste(utils::head(unknown, 5), collapse = ", "),
      " — the two datasets were built from different upstream revisions; re-run ",
      "data-raw/table_mappings.R and commit both."
    )
  )

  # Dead rows must never receive data, so an alias pointing at one is worse than an
  # alias pointing at nothing: it resolves, and to a row the contract forbids.
  dead <- pol$polity_code[pol$wiki_status %in% c("retired", "superseded")]
  routed_dead <- sort(intersect(unique(aliases$polity_code), dead))
  expect_equal(
    length(routed_dead),
    0L,
    info = paste0(
      "aliases route to retired or superseded polities: ",
      paste(utils::head(routed_dead, 5), collapse = ", ")
    )
  )
})

testthat::test_that("a column called area_code holds a numeric area code, or is the known case", {
  # The fourth instance of this branch's recurring defect: a column whose name promises one kind of
  # identifier and holds another. The earlier three were regions_full$polity_code and
  # polity_area_crosswalk$reporting_polity_code, both holding prefixes and both renamed, and
  # gdp-population$area_code, which holds ISO3-shaped prefixes and cannot be renamed because it is a
  # pinned input.
  #
  # urban_n_reference was the fourth and is now FIXED, so the baseline is empty. That
  # reverses an earlier decision on this branch and the reasoning matters, because the
  # earlier decision was not wrong.
  #
  # What was declined then: RENAME the column to `area_iso3c` and add a derived numeric
  # `area_code`. That is a breaking change to a published dataset's schema — a consumer
  # has to learn a new column — so it was left to the maintainer.
  #
  # What was done now: keep the name and correct the VALUE, resolving "ESP" to 203 through
  # the crosswalk at build time. No column appears or disappears. The only consumer that
  # breaks is one relying on `area_code` holding something the name says it does not hold.
  #
  # And the new information that justifies acting rather than deferring again: nothing in
  # the package JOINS this dataset. It is a benchmark series a reader compares against by
  # hand, referenced only in comments and roxygen, so the change is provably output-neutral
  # — which is what the earlier pass could not say.
  #
  # Swept across every exported dataset so the NEXT one fails here rather than being found by
  # accident five iterations later.
  baseline <- character(0)

  exported <- utils::data(package = "whep")$results[, "Item"]
  offenders <- character()
  checked <- 0L
  for (nm in exported) {
    d <- tryCatch(get(nm, envir = asNamespace("whep")), error = function(e) {
      NULL
    })
    if (is.null(d) || !is.data.frame(d) || !"area_code" %in% names(d)) {
      next
    }
    checked <- checked + 1L
    v <- d[["area_code"]]
    numeric_like <- is.numeric(v) ||
      all(is.na(v) | grepl("^[0-9]+$", as.character(v)))
    if (!numeric_like && !nm %in% baseline) {
      offenders <- c(
        offenders,
        paste0(
          nm,
          " (e.g. ",
          paste(utils::head(unique(stats::na.omit(v)), 3), collapse = ", "),
          ")"
        )
      )
    }
  }
  testthat::expect_gte(checked, 2L)
  testthat::expect_equal(
    length(offenders),
    0L,
    info = paste0(
      "these datasets have an `area_code` column holding something other than a numeric area ",
      "code — rename it to say what it holds: ",
      paste(offenders, collapse = "; ")
    )
  )

  # The baseline is now EMPTY, and that is the interesting part. It held
  # urban_n_reference, whose area_code was the ISO3 string "ESP" — and the shrink side of
  # this check is what failed when that was fixed, exactly as intended. An entry that has
  # been resolved must come out, or the baseline licenses a regression later.
  testthat::expect_equal(baseline, character(0))
})
