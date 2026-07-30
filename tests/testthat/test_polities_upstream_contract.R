# The polity data embedded in this package is a COPY of the whep-polities
# database, built by data-raw/table_mappings.R. That copy silently drifted to
# 603 rows against 740 upstream — 144 polities missing, 7 that no longer
# existed, and 24 RETIRED or SUPERSEDED, so 24 FAOSTAT area codes were resolving
# to withdrawn polities (area 21 Brazil to the collapsed BRA-1800-2025 rather
# than the three rows that replaced it at the 1903 Acre acquisition). Nobody
# noticed because checking meant diffing whole tables across two repositories.
#
# whep-polities now publishes data/final/polities_manifest.json — row counts, an
# identity hash over the fields a consumer resolves against, the live codes, the
# dead codes, and its known polygon gaps. These tests assert this package's copy
# against that contract, so the drift fails loudly instead of being rediscovered
# by inspection.
#
# They SKIP when the manifest is absent (a CI runner without the sibling
# checkout, or before the paired PR lands) rather than failing, because an
# unavailable contract is not evidence of drift. Point WHEP_POLITIES_MANIFEST at
# it to override the default location.

manifest_path <- function() {
  Sys.getenv(
    "WHEP_POLITIES_MANIFEST",
    unset = path.expand("~/whep-polities/data/final/polities_manifest.json")
  )
}

read_manifest <- function() {
  path <- manifest_path()
  testthat::skip_if_not(
    file.exists(path),
    paste0(
      "whep-polities manifest not found at ",
      path,
      " — set WHEP_POLITIES_MANIFEST or check out the sibling repository"
    )
  )
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

test_that("the embedded polity table matches the upstream manifest", {
  mf <- read_manifest()

  expect_equal(
    nrow(whep::polities),
    mf$counts$total,
    info = paste0(
      "polities has ",
      nrow(whep::polities),
      " rows against ",
      mf$counts$total,
      " upstream. Re-run data-raw/table_mappings.R and commit data/."
    )
  )

  # Set comparison, not just a count: equal counts with different membership is
  # exactly what a split-plus-merge upstream produces.
  expect_setequal(
    whep::polities$polity_code,
    mf$live_polity_codes |> c(mf$dead_polity_codes)
  )
})

test_that("the embedded copy agrees with upstream about WHICH rows are dead", {
  mf <- read_manifest()

  # This test exists because its absence let real drift through. When
  # GCO-1884-2025 was retired upstream, every other assertion here still passed:
  # the row count was unchanged (a retirement moves a code between lists, it does
  # not remove it), the code set was unchanged for the same reason, and nothing
  # routed to it, so the dead-routing check was satisfied too. The embedded copy
  # sat with `wiki_status: draft` against `retired` upstream and no test could
  # see it.
  #
  # `wiki_status` is one of the manifest's identity fields precisely because a
  # changed status must invalidate a downstream copy. Comparing the SET of codes
  # each side considers dead is the field-level form of that check, and unlike
  # recomputing `identity_sha256` in R it says which row disagrees.
  pol <- as.data.frame(whep::polities)
  dead_here <- sort(pol$polity_code[pol$wiki_status %in% mf$dead_status])

  expect_setequal(dead_here, mf$dead_polity_codes)
})

test_that("no polity resolution targets a dead upstream polity", {
  mf <- read_manifest()

  # `retired` means withdrawn; `superseded` means split or merged into finer
  # rows. Either way the row must never receive data — whep-polities enforces
  # the same rule in matchlib.Matcher.DEAD_STATUS, and data-raw/table_mappings.R
  # excludes them from polity_attrs on this side.
  cw <- whep::polity_area_crosswalk
  offending <- unique(cw$polity_code[cw$polity_code %in% mf$dead_polity_codes])

  expect_equal(
    length(offending),
    0L,
    info = paste0(
      "crosswalk routes area codes to withdrawn polities: ",
      paste(utils::head(offending, 10), collapse = ", ")
    )
  )
})

test_that("no polity lacking geometry claims one, beyond the upstream backlog", {
  mf <- read_manifest()

  # Upstream tracks a backlog of rows whose polygon_status asserts a polygon the
  # build cannot attach (feature ids recorded as prose rather than resolvable
  # values) in scripts/validate_polygons_baseline.txt, published here as
  # polygon_gap_polity_codes. Asserting "no gaps at all" would fail on that
  # known set and get ignored; asserting "no gap OUTSIDE it" starts failing the
  # moment a NEW one appears, and goes green as the backlog is worked down.
  cw <- whep::polity_area_crosswalk
  gaps <- cw[
    !is.na(cw$polity_code) &
      !cw$has_geometry &
      cw$polygon_status %in% mf$claims_polygon_status,
  ]
  unexpected <- setdiff(unique(gaps$polity_code), mf$polygon_gap_polity_codes)

  expect_equal(
    length(unexpected),
    0L,
    info = paste0(
      "polities claim a polygon they do not have and are NOT in the upstream ",
      "backlog: ",
      paste(utils::head(unexpected, 10), collapse = ", ")
    )
  )
})

upstream_csv_path <- function() {
  Sys.getenv(
    "WHEP_POLITIES_CSV",
    unset = path.expand("~/whep-polities/data/final/polities_database.csv")
  )
}

test_that("every identity field matches upstream, row by row", {
  mf <- read_manifest()
  path <- upstream_csv_path()
  testthat::skip_if_not(
    file.exists(path),
    paste0("upstream database CSV not found at ", path)
  )

  # The manifest publishes `identity_sha256` over exactly the fields a consumer
  # resolves against, precisely so drift in any of them invalidates a downstream
  # copy. None of the other tests here used it, which is how a changed
  # `wiki_status` slipped through: the row count, the code set and the
  # dead-routing check were all still satisfied.
  #
  # The hash itself is NOT replicated. It is a digest of Python's json.dumps
  # output, and reproducing that byte-for-byte from R depends on serialiser
  # details neither side promises — a naive attempt gives a different digest even
  # when the DATA is identical, which would be a test that fails for the wrong
  # reason. Comparing the fields directly is both robust and better: it names the
  # row and the field that disagree, which a hash cannot.
  fields <- mf$identity_fields
  upstream <- utils::read.csv(
    path,
    colClasses = "character",
    check.names = FALSE
  )
  here <- as.data.frame(whep::polities)

  # `polities` renames two fields on import; map them back before comparing.
  if (!"start_year" %in% names(here) && "polity_start_year" %in% names(here)) {
    names(here)[names(here) == "polity_start_year"] <- "start_year"
  }
  if (!"end_year" %in% names(here) && "polity_end_year" %in% names(here)) {
    names(here)[names(here) == "polity_end_year"] <- "end_year"
  }

  comparable <- intersect(fields, intersect(names(upstream), names(here)))
  expect_true(
    "polity_code" %in% comparable,
    info = "cannot compare without polity_code on both sides"
  )

  upstream <- upstream[order(upstream$polity_code), comparable, drop = FALSE]
  here <- here[order(here$polity_code), comparable, drop = FALSE]
  expect_setequal(upstream$polity_code, here$polity_code)

  # Compared as character throughout: the embedded copy carries integers where the
  # CSV carries text, and a type difference is not drift.
  norm <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    trimws(x)
  }
  mismatches <- character(0)
  for (f in setdiff(comparable, "polity_code")) {
    differs <- norm(upstream[[f]]) != norm(here[[f]])
    if (any(differs)) {
      codes <- utils::head(upstream$polity_code[differs], 3)
      mismatches <- c(
        mismatches,
        sprintf(
          "%s (%d row(s), e.g. %s: upstream=%s here=%s)",
          f,
          sum(differs),
          codes[1],
          norm(upstream[[f]])[differs][1],
          norm(here[[f]])[differs][1]
        )
      )
    }
  }
  expect_equal(
    length(mismatches),
    0L,
    info = paste0(
      "identity fields disagree with upstream: ",
      paste(mismatches, collapse = "; "),
      " — re-run data-raw/table_mappings.R and commit data/."
    )
  )
})

# `observed_rows` carries THREE states, and two of them were published as one until
# upstream stopped coercing empty to zero:
#
#   positive   measured, that many rows
#   0          measured, and genuinely none
#   NA         NOT measured -- that source's corpus is not in the upstream repository
#
# The third state is most of the non-FAOSTAT sources. Every
# lassaletta-grassland-share, mueller-synthetic-n and crops-manure-n alias is NA,
# because those datasets live in THIS package and upstream never sees them. Measured
# here they are the opposite of inert: 6,781 Lassaletta country-years resolve, 184
# crops_manure_n codes, 156 Mueller codes. Published as 0, they read as 393 dead
# aliases, and any check using "observed_rows == 0" as an inertness test would have
# flagged all 152 of those three sources.
#
# Asserted here rather than left to the roxygen, because the collapse is invisible:
# coercing NA to 0 changes no row count, breaks no join, and fails no existing test.
# The only symptom is a column that quietly means something else.
testthat::test_that("observed_rows distinguishes measured-zero from not-measured", {
  al <- as.data.frame(whep::polity_label_aliases)
  testthat::expect_true("observed_rows" %in% names(al))

  n_na <- sum(is.na(al$observed_rows))
  n_zero <- sum(al$observed_rows == 0, na.rm = TRUE)
  n_pos <- sum(al$observed_rows > 0, na.rm = TRUE)
  testthat::expect_equal(n_na + n_zero + n_pos, nrow(al))

  # All three states must be occupied. If NA vanishes the coercion is back; if 0
  # vanishes the distinction has become vacuous from the other side.
  testthat::expect_gt(n_na, 100L)
  testthat::expect_gt(n_zero, 10L)
  testthat::expect_gt(n_pos, 100L)

  # The sources whose data lives in this package must be entirely NA -- upstream
  # cannot have measured them. A 0 appearing here means the coercion returned.
  ours <- c(
    "lassaletta-grassland-share",
    "mueller-synthetic-n",
    "crops-manure-n"
  )
  mine <- al[which(al$source %in% ours), ]
  testthat::expect_gt(nrow(mine), 140L)
  testthat::expect_equal(
    sum(!is.na(mine$observed_rows)),
    0L,
    info = paste0(
      "aliases for sources whose corpus is not upstream, yet carrying a measured ",
      "count: ",
      paste(
        utils::head(mine$source_label[!is.na(mine$observed_rows)], 10),
        collapse = ", "
      )
    )
  )

  # And the aliases those sources' data actually exercises are NOT inert, which is
  # the claim the NA state protects. Measured here, where the data lives.
  d <- whep::crops_manure_n
  codes <- sort(unique(stats::na.omit(d$ISO)))
  resolved <- resolve_polity_label(
    codes,
    source = "crops-manure-n",
    year = 2000L
  )
  testthat::expect_gt(sum(!is.na(resolved)), 0L)
})

# Aliases must not resolve a year to a polity that did not exist in it. Swept all 869:
#
#   268 have a `year_end` at or past their target's `end_year`, overshooting by exactly
#       one. That is the convention, not a defect — an alias `year_end` is INCLUSIVE while
#       a polity `end_year` is EXCLUSIVE, so an alias covering a polity's last year reads
#       one higher than the span does.
#     2 overshoot by FOUR: "tanganyika" and "tanzania", both 1922-1964 against
#       TAN-1922-1964, whose columns say 1922-1961.
#
# The cause is upstream and is now gated there: that polity's CODE says 1964 while its
# columns say 1961, and the aliases were written against the code. A polity code is
# documented as PREFIX-start-end, so reading years off the identifier is a reasonable thing
# to do — it is cheaper than a join — and here it gives a different answer from the columns.
# whep-polities added validate_code_year_agreement.py, which baselines this and NNG-1949-1963
# pending a curation decision (independence versus union; transfer versus Act of Free
# Choice).
#
# Asserted here as a ceiling on the overshoot rather than as zero, because the one-year case
# is correct and demanding zero would fail on 268 rows that are fine. What must not grow is
# the number that overshoot by MORE, since each of those resolves real years to a polity
# that had ended.
testthat::test_that("no alias reaches years its target polity did not exist in", {
  al <- as.data.frame(whep::polity_label_aliases)
  p <- as.data.frame(sf::st_drop_geometry(as.data.frame(whep::polities)))
  span <- p[, c("polity_code", "start_year", "end_year")]

  idx <- match(al$polity_code, span$polity_code)
  # Non-vacuous: unresolvable targets would make every comparison NA and pass.
  testthat::expect_equal(sum(is.na(idx)), 0L)

  ends <- span$end_year[idx]
  starts <- span$start_year[idx]
  overshoot <- ifelse(
    !is.na(al$year_end) & !is.na(ends),
    al$year_end - (ends - 1L),
    0L
  )
  overshoot[overshoot < 0L] <- 0L

  # The boundary convention: many aliases sit exactly one year past the exclusive end.
  testthat::expect_gt(sum(overshoot == 1L), 100L)

  beyond <- al$source_label[which(overshoot > 1L)]
  testthat::expect_setequal(beyond, c("tanganyika", "tanzania"))

  # THE OTHER DIRECTION, which the first version of this test got wrong by asserting a
  # one-year bound and finding a 98-year one. Four aliases begin before their target
  # polity existed:
  #
  #   Portuguese Timor  1702-1975  ->  TLS-1800-2025   98 years early
  #   Gold Coast        1821-1956  ->  GHA-1898-1956   77
  #   Trieste           1937-1946  ->  TRS-1947-1954   10
  #   French Morocco    1904-1956  ->  MAR-1911-1958    7
  #
  # Portuguese Timor is harmless: 1702 is before the database's own 1800 start, so no
  # polity could be named and no data exists there. The other three route real pre-existence
  # years to the earliest polity available — 1821 Gold Coast data reaches a polity created in
  # 1898 — which is either a deliberate "nearest available" choice or an oversight, and the
  # registry does not say which.
  #
  # Pinned by identity in both directions rather than bounded, because a bound cannot tell
  # a deliberate historical stretch from a new mistake, and each of these is a specific
  # curation question.
  undershoot <- ifelse(
    !is.na(al$year_start) & !is.na(starts),
    starts - al$year_start,
    0L
  )
  early <- al$source_label[which(undershoot > 1L)]
  testthat::expect_setequal(
    early,
    c("Portuguese Timor", "Gold Coast", "Trieste", "French Morocco")
  )
})

# Every period boundary in an alias chain is a one-year ambiguity, and the cause is a
# convention rather than a mistake: an alias `year_end` is INCLUSIVE while a polity
# `end_year` is EXCLUSIVE, so an alias ending at a polity's last year and the next alias
# starting at the successor's first year BOTH cover that year. Which polity a value lands
# in is then decided by match order.
#
# Swept every (label, source) group with more than one row. 25 have a year where two rows
# name different polities, and the rate varies by how the aliases were WRITTEN:
#
#   (any)                        18 of 31 chains ambiguous   hand-entered
#   fao1952                       3 of 18
#   faostat                       2 of 43
#   lassaletta-grassland-share    1 of 30   generated
#   trade-sources                 0 of  7   generated
#
# The generated sets are clean because they were built with `year_end = polity_end_year -
# 1`, so consecutive ranges do not touch. That is the fix, and this measurement is the
# argument for applying it to the hand-entered chains: 18 boundary years currently resolve
# by match order. Reported as whep-polities#54.
#
# The one Lassaletta case is Cape Verde, whose two rows overlap at 1975 by deliberate
# curation — a choice about which polity gets a mid-year independence — and is exempted by
# name in test_lassaletta_polity_coverage.R rather than by weakening the rule.
#
# Most of the 25 are benign in effect: the two candidates are ADJACENT PERIODS OF ONE
# FAMILY, so a boundary-year value lands in one of two neighbouring periods of the same
# territory. Five are cross-family and are the ones worth a curation decision.
testthat::test_that("generated alias chains have no ambiguous boundary years", {
  al <- as.data.frame(whep::polity_label_aliases)
  generated <- c("trade-sources", "crops-manure-n")

  ambiguous <- character(0)
  for (src in generated) {
    rows <- al[which(al$source == src), ]
    testthat::expect_gt(nrow(rows), 3L)
    for (lbl in unique(rows$source_label)) {
      chain <- rows[rows$source_label == lbl, ]
      if (nrow(chain) < 2L) {
        next
      }
      chain <- chain[order(chain$year_start), ]
      # Consecutive ranges must not touch: the next start is strictly after the previous
      # end. This is the property the inclusive/exclusive mismatch breaks.
      if (any(chain$year_start[-1] <= chain$year_end[-nrow(chain)])) {
        ambiguous <- c(ambiguous, paste0(src, ": ", lbl))
      }
    }
  }
  testthat::expect_equal(
    length(ambiguous),
    0L,
    info = paste0(
      "generated alias chains whose consecutive ranges touch, so a boundary year ",
      "resolves by match order: ",
      paste(ambiguous, collapse = "; ")
    )
  )
})
