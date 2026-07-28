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
      "whep-polities manifest not found at ", path,
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
      "polities has ", nrow(whep::polities), " rows against ", mf$counts$total,
      " upstream. Re-run data-raw/table_mappings.R and commit data/."
    )
  )

  # Set comparison, not just a count: equal counts with different membership is
  # exactly what a split-plus-merge upstream produces.
  expect_setequal(whep::polities$polity_code, mf$live_polity_codes |> c(mf$dead_polity_codes))
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
      "backlog: ", paste(utils::head(unexpected, 10), collapse = ", ")
    )
  )
})
