# Tests for the USDA NASS admin-statistics reader (#1000, T08).
#
# Nothing here reaches the network or a `WHEP_*` path: every test that reads
# a dump stubs the private `.fetch_nass_dump()` with
# `fixtures/nass_qs_sample.txt`. That fixture is 181 rows taken verbatim
# (bytes, tab separators, CRLF line endings and the CV_% NUL bytes included)
# from the real 2026-09-02 dumps `qs.crops_20260902.txt.gz` and
# `qs.animals_products_20260902.txt.gz`; only the selection of rows is ours.
# It deliberately carries, besides the rows the reader keeps:
#
# - a CORN, GRAIN / CORN, SILAGE pair, so the series filter is load-bearing;
# - IRRIGATED production-practice rows;
# - FIRST OF JUL inventory rows, and HOGS - INVENTORY at its real FIRST OF
#   DEC reference period;
# - CENSUS rows and DOMAIN_DESC breakdown rows of the wanted series;
# - in-season forecast rows ("YEAR - AUG FORECAST");
# - COUNTY, AGRICULTURAL DISTRICT, ZIP CODE and NATIONAL (US TOTAL) rows;
# - a suppressed `(D)` value and two OTHER STATES (FIPS 98) residual rows;
# - PRODUCTION in bushels, YIELD in bushels per acre and a PRICE RECEIVED
#   row, which the reader must refuse rather than convert.
#
# The package's NAMESPACE is not regenerated in this wave, so the exported
# reader is reached as `whep:::read_admin_stats_nass()` rather than `whep::`.

nass_fixture <- function() {
  testthat::test_path("fixtures", "nass_qs_sample.txt")
}

# Stub the one file-system entry point. `source_version` is the stamp
# `.fetch_nass_dump()` would have read off the real dump's name.
nass_local_dump <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    .fetch_nass_dump = function(domain, nass_dir) {
      list(path = nass_fixture(), source_version = "20260902")
    },
    .package = "whep",
    .env = env
  )
}

nass_crops <- function(...) {
  suppressWarnings(whep:::read_admin_stats_nass("crops", ...))
}

nass_animals <- function(...) {
  suppressWarnings(whep:::read_admin_stats_nass("animals", ...))
}

# ---- Fixture parsing ---------------------------------------------------

testthat::test_that("the fixture parses whole, NUL bytes and all", {
  raw <- whep:::.nass_scan_dump(
    nass_fixture(),
    series = NULL,
    agg_levels = c(
      "STATE",
      "COUNTY",
      "NATIONAL",
      "AGRICULTURAL DISTRICT",
      "ZIP CODE"
    )
  )
  # Pinned deliberately: readr::read_tsv_chunked(quote = "") silently
  # returns 88 of these 181 rows on a file with NUL bytes, which is why the
  # reader streams with readLines(skipNul = TRUE) instead.
  testthat::expect_equal(nrow(raw), 181L)
  testthat::expect_equal(ncol(raw), 15L)
  testthat::expect_true(all(
    c("SHORT_DESC", "AGG_LEVEL_DESC", "STATE_FIPS_CODE", "VALUE") %in%
      names(raw)
  ))
  # Leading zeros in a FIPS code are significant, so it stays character.
  testthat::expect_type(raw$STATE_FIPS_CODE, "character")
  testthat::expect_true("06" %in% raw$STATE_FIPS_CODE)
  testthat::expect_type(raw$YEAR, "integer")
})

# ---- Output contract ---------------------------------------------------

testthat::test_that("crop rows carry the reader's column contract", {
  nass_local_dump()
  out <- nass_crops()

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(
    out,
    c(
      "source",
      "source_native_unit_id",
      "source_native_unit_name",
      "source_native_item_code",
      "source_native_item_name",
      "indicator_used",
      "quantity",
      "year",
      "value",
      "value_unit",
      "value_flag",
      "grain",
      "nuts_version",
      "source_version",
      "recorded_at"
    )
  )
  testthat::expect_equal(nrow(out), 60L)
  testthat::expect_equal(unique(out$source), "USDA_NASS")
  testthat::expect_equal(unique(out$indicator_used), "area_harvested")
  testthat::expect_equal(unique(out$quantity), "area")
  testthat::expect_equal(unique(out$value_unit), "ha")
  testthat::expect_equal(unique(out$grain), "admin1")
  testthat::expect_equal(unique(out$source_version), "20260902")
  testthat::expect_true(all(is.na(out$nuts_version)))
  testthat::expect_true(all(is.na(out$source_native_item_code)))
  testthat::expect_type(out$year, "integer")
  testthat::expect_type(out$value, "double")
  testthat::expect_match(
    unique(out$recorded_at),
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
  )
})

testthat::test_that("keys are the state FIPS code and the verbatim name", {
  nass_local_dump()
  out <- nass_crops()

  testthat::expect_type(out$source_native_unit_id, "character")
  testthat::expect_true(all(nchar(out$source_native_unit_id) == 2))
  # US TOTAL is FIPS 99 at NATIONAL level and must never reach state grain.
  testthat::expect_false("99" %in% out$source_native_unit_id)
  testthat::expect_equal(
    unique(out$source_native_unit_name[out$source_native_unit_id == "19"]),
    "IOWA"
  )
  # The series string is the item name; NASS publishes no item code.
  testthat::expect_setequal(
    unique(out$source_native_item_name),
    whep:::.nass_default_series("crops")
  )
})

testthat::test_that("animal inventories are head counts with no indicator", {
  nass_local_dump()
  out <- nass_animals()

  testthat::expect_equal(nrow(out), 46L)
  testthat::expect_true(all(is.na(out$indicator_used)))
  testthat::expect_equal(unique(out$quantity), "heads")
  testthat::expect_equal(unique(out$value_unit), "heads")
  testthat::expect_equal(unique(out$grain), "admin1")
})

# ---- Unit conversion ---------------------------------------------------

testthat::test_that("acres convert to hectares and head pass through", {
  nass_local_dump()

  # WISCONSIN, OATS - ACRES HARVESTED, 1876: 835,000 acres in the dump.
  crops <- nass_crops()
  wisconsin <- crops[
    crops$source_native_unit_id == "55" &
      crops$year == 1876L,
  ]
  testthat::expect_equal(nrow(wisconsin), 1L)
  testthat::expect_equal(wisconsin$value, 835000 * 0.40468564224)

  # OTHER STATES, SHEEP, LAMBS, MARKET - INVENTORY, 2020: 25,000 head.
  sheep <- nass_animals(short_desc = "SHEEP, LAMBS, MARKET - INVENTORY")
  testthat::expect_equal(sheep$value, 25000)
  testthat::expect_equal(sheep$value_unit, "heads")
})

testthat::test_that("a planted-area series maps to its own indicator", {
  nass_local_dump()
  out <- nass_crops(short_desc = "CORN - ACRES PLANTED")

  testthat::expect_equal(unique(out$indicator_used), "area_planted_or_sown")
  testthat::expect_equal(unique(out$quantity), "area")
  testthat::expect_equal(unique(out$value_unit), "ha")
})

testthat::test_that("an unconvertible unit aborts instead of guessing", {
  nass_local_dump()

  testthat::expect_error(
    nass_crops(short_desc = "CORN, GRAIN - PRODUCTION, MEASURED IN BU"),
    "BU"
  )
  testthat::expect_error(
    nass_crops(short_desc = "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE"),
    "cannot be converted"
  )
})

testthat::test_that("an unmapped statistic category aborts", {
  nass_local_dump()

  testthat::expect_error(
    nass_crops(
      short_desc = paste(
        "CABBAGE, FRESH MARKET - PRICE RECEIVED,",
        "MEASURED IN $ / CWT"
      )
    ),
    "PRICE RECEIVED"
  )
})

# ---- Exclusions --------------------------------------------------------

testthat::test_that("only the requested series survive", {
  nass_local_dump()
  out <- nass_crops()

  # The fixture holds CORN, SILAGE rows beside the CORN, GRAIN ones.
  testthat::expect_false(
    "CORN, SILAGE - ACRES HARVESTED" %in% out$source_native_item_name
  )
  testthat::expect_true(
    "CORN, GRAIN - ACRES HARVESTED" %in% out$source_native_item_name
  )
})

testthat::test_that("census, breakdown, practice and forecast rows drop", {
  nass_local_dump()
  raw <- whep:::.nass_scan_dump(
    nass_fixture(),
    whep:::.nass_default_series("crops"),
    "STATE"
  )
  kept <- whep:::.nass_common_filters(
    raw,
    whep:::.nass_default_series("crops"),
    "YEAR",
    "ANNUAL"
  )

  testthat::expect_equal(nrow(raw), 74L)
  testthat::expect_equal(nrow(kept), 60L)
  testthat::expect_equal(unique(kept$SOURCE_DESC), "SURVEY")
  testthat::expect_equal(unique(kept$DOMAIN_DESC), "TOTAL")
  testthat::expect_equal(
    unique(kept$PRODN_PRACTICE_DESC),
    "ALL PRODUCTION PRACTICES"
  )
  testthat::expect_equal(unique(kept$REFERENCE_PERIOD_DESC), "YEAR")
  # The dropped rows are exactly census, breakdown and forecast rows.
  testthat::expect_true(any(raw$SOURCE_DESC == "CENSUS"))
  testthat::expect_true(any(raw$DOMAIN_DESC != "TOTAL"))
  testthat::expect_true(any(grepl("FORECAST", raw$REFERENCE_PERIOD_DESC)))
})

testthat::test_that("an irrigated-practice row never reaches the output", {
  nass_local_dump()
  out <- nass_crops(
    short_desc = c(
      "CORN, GRAIN - ACRES HARVESTED",
      "CORN, GRAIN, IRRIGATED - ACRES HARVESTED"
    )
  )

  testthat::expect_false(
    "CORN, GRAIN, IRRIGATED - ACRES HARVESTED" %in%
      out$source_native_item_name
  )
})

testthat::test_that("an off-season inventory date never reaches the output", {
  nass_local_dump()
  out <- nass_animals()
  july <- nass_animals(reference_period = "FIRST OF JUL")

  testthat::expect_equal(nrow(july), 4L)
  testthat::expect_equal(
    nrow(dplyr::semi_join(
      out,
      july,
      by = c("source_native_unit_id", "source_native_item_name", "year")
    )),
    0L
  )
})

testthat::test_that("other aggregation levels never reach state grain", {
  nass_local_dump()
  out <- nass_crops()
  raw <- whep:::.nass_scan_dump(
    nass_fixture(),
    series = NULL,
    agg_levels = c("NATIONAL", "COUNTY", "AGRICULTURAL DISTRICT", "ZIP CODE")
  )

  testthat::expect_gt(nrow(raw), 0L)
  testthat::expect_false("US TOTAL" %in% out$source_native_unit_name)
  testthat::expect_true(all(nchar(out$source_native_unit_id) == 2))
})

testthat::test_that("a FIPS 99 row at state level aborts the read", {
  # A hand-built row: the dump has never published one, and the assertion
  # exists so a change in NASS's aggregation semantics cannot pass silently.
  intruder <- tibble::tibble(
    AGG_LEVEL_DESC = c("STATE", "STATE"),
    STATE_FIPS_CODE = c("19", "99")
  )

  testthat::expect_error(
    whep:::.nass_assert_agg_level(intruder, "STATE"),
    "US TOTAL"
  )
})

# ---- Residual and suppression flags ------------------------------------

testthat::test_that("OTHER STATES is kept and flagged as the residual", {
  nass_local_dump()
  out <- nass_crops(
    short_desc = "BEANS, DRY EDIBLE, GREAT NORTHERN - ACRES HARVESTED"
  )

  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$source_native_unit_id, "98")
  testthat::expect_equal(out$source_native_unit_name, "OTHER STATES")
  testthat::expect_equal(out$value_flag, "residual")
  testthat::expect_equal(out$value, 1300 * 0.40468564224)
})

testthat::test_that("a suppressed value becomes NA with its code kept", {
  nass_local_dump()
  out <- nass_animals(
    short_desc = "FOOD FISH, CATFISH, BROODSTOCK - INVENTORY"
  )

  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_true(is.na(out$value))
  testthat::expect_equal(out$value_flag, "(D)")
  # The row is kept, not dropped: a withheld unit still reports.
  testthat::expect_equal(out$source_native_unit_id, "37")
})

testthat::test_that("residual and suppression flags compose", {
  testthat::expect_equal(
    whep:::.nass_join_flags("residual", "(D)"),
    "residual; (D)"
  )
  testthat::expect_equal(whep:::.nass_join_flags(NA, "(S)"), "(S)")
  testthat::expect_equal(whep:::.nass_join_flags("residual", NA), "residual")
  testthat::expect_true(is.na(whep:::.nass_join_flags(NA, NA)))
})

testthat::test_that("every documented NASS value code is preserved", {
  codes <- c("(D)", "(S)", "(Z)", "(NA)", "(X)", "(H)", "(L)", "1,234", "")

  testthat::expect_equal(
    whep:::.nass_parse_number(codes),
    c(rep(NA_real_, 7), 1234, NA)
  )
  testthat::expect_equal(
    whep:::.nass_value_code(codes),
    c("(D)", "(S)", "(Z)", "(NA)", "(X)", "(H)", "(L)", NA, NA)
  )
})

# ---- The species the default reference period misses --------------------

testthat::test_that("a series matching no row warns by name", {
  nass_local_dump()

  # NASS publishes the annual hog inventory on 1 December, so the January
  # default matches nothing. The warning is what keeps that visible.
  testthat::expect_warning(
    whep:::read_admin_stats_nass("animals"),
    "HOGS - INVENTORY"
  )
  testthat::expect_no_warning(
    whep:::read_admin_stats_nass(
      "animals",
      short_desc = "CATTLE, INCL CALVES - INVENTORY"
    )
  )
})

testthat::test_that("reference_period reaches the hog inventory", {
  nass_local_dump()
  out <- nass_animals(
    short_desc = "HOGS - INVENTORY",
    reference_period = "FIRST OF DEC"
  )

  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_equal(unique(out$quantity), "heads")
})

# ---- County grain ------------------------------------------------------

testthat::test_that("county rows return at admin2 grain with 5-digit ids", {
  nass_local_dump()
  out <- nass_crops(agg_level = "COUNTY")

  testthat::expect_equal(nrow(out), 12L)
  testthat::expect_equal(unique(out$grain), "admin2")
  testthat::expect_true(all(nchar(out$source_native_unit_id) == 5))
  testthat::expect_match(out$source_native_unit_name[[1]], ", ")
  # The state read of the same fixture returns different units entirely.
  testthat::expect_length(
    intersect(out$source_native_unit_id, nass_crops()$source_native_unit_id),
    0L
  )
})

# ---- The in-file national check ----------------------------------------

testthat::test_that("the national check compares states with US TOTAL", {
  nass_local_dump()
  check <- suppressWarnings(whep:::.nass_national_check("animals"))

  testthat::expect_true(all(
    c(
      "source_native_item_name",
      "year",
      "national_value",
      "n_national",
      "state_sum",
      "n_states",
      "n_suppressed",
      "n_residual",
      "ratio"
    ) %in%
      names(check)
  ))

  # SHEEP, INCL LAMBS - INVENTORY 1 January 2010: the fixture holds the
  # US TOTAL row (5,620,000 head) and one state row (Wyoming, 375,000).
  paired <- check[
    check$source_native_item_name == "SHEEP, INCL LAMBS - INVENTORY" &
      check$year == 2010L,
  ]
  testthat::expect_equal(nrow(paired), 1L)
  testthat::expect_equal(paired$national_value, 5620000)
  testthat::expect_equal(paired$n_national, 1L)
  testthat::expect_equal(paired$state_sum, 375000)
  testthat::expect_equal(paired$n_states, 1L)
  testthat::expect_equal(paired$ratio, 375000 / 5620000)
})

# ---- Locating a dump ---------------------------------------------------

testthat::test_that("the newest date-stamped dump wins", {
  dir <- withr::local_tempdir()
  file.create(file.path(
    dir,
    c(
      "qs.crops_20260101.txt.gz",
      "qs.crops_20260902.txt.gz",
      "qs.animals_products_20260902.txt.gz",
      "qs.census2022.txt.gz",
      "Readme.txt"
    )
  ))

  crops <- whep:::.fetch_nass_dump("crops", dir)
  testthat::expect_equal(basename(crops$path), "qs.crops_20260902.txt.gz")
  testthat::expect_equal(crops$source_version, "20260902")

  animals <- whep:::.fetch_nass_dump("animals", dir)
  testthat::expect_equal(
    basename(animals$path),
    "qs.animals_products_20260902.txt.gz"
  )
})

testthat::test_that("an already-extracted dump is accepted", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "qs.crops_20250801.txt"))

  testthat::expect_equal(
    whep:::.fetch_nass_dump("crops", dir)$source_version,
    "20250801"
  )
})

testthat::test_that("a directory with no dump aborts naming the script", {
  dir <- withr::local_tempdir()

  testthat::expect_error(
    whep:::.fetch_nass_dump("crops", dir),
    "download_nass\\.R"
  )
})

testthat::test_that("an unset WHEP_NASS_DIR aborts naming the script", {
  withr::local_envvar(WHEP_NASS_DIR = "")

  testthat::expect_error(
    whep:::read_admin_stats_nass("crops"),
    "WHEP_NASS_DIR"
  )
  testthat::expect_error(
    whep:::read_admin_stats_nass("crops"),
    "download_nass\\.R"
  )
})

# ---- Argument validation and the example fixture ------------------------

testthat::test_that("domain and agg_level are closed vocabularies", {
  testthat::expect_error(whep:::read_admin_stats_nass("livestock"))
  testthat::expect_error(
    whep:::read_admin_stats_nass("crops", agg_level = "STATE OR COUNTY")
  )
})

testthat::test_that("example = TRUE returns the documented shape", {
  example <- whep:::read_admin_stats_nass(example = TRUE)
  nass_local_dump()
  real <- nass_crops()

  testthat::expect_s3_class(example, "tbl_df")
  testthat::expect_equal(nrow(example), 10L)
  testthat::expect_named(example, names(real))
  testthat::expect_equal(
    vapply(example, class, character(1)),
    vapply(real, class, character(1))
  )
  testthat::expect_equal(unique(example$source), "USDA_NASS")
  testthat::expect_true("residual" %in% example$value_flag)
})
