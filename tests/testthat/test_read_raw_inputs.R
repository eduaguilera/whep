# test_read_raw_inputs.R — tests for R/read_raw_inputs.R helpers

# -- .aggregate_to_polities fao_flag handling ----------------------------------

# `.aggregate_to_polities()` resolves areas through `.polity_crosswalk()` (via
# `.add_polity_columns_dt()`), so that is the binding a hermetic test has to
# mock. These tests used to mock a `.polity_bridge()` helper that nothing
# called, and passed only because the shipped crosswalk happens to resolve area
# 203 to Spain (whep#590). The fixture below names the polity `Testland` so an
# inert mock cannot be mistaken for a live one.
.local_aggregator_crosswalk <- function(env = parent.frame()) {
  fixture <- data.table::data.table(
    area_code = 203L,
    area_name = "Testland",
    area_iso3c = "TST",
    polity_area_code = 203L,
    polity_code = "TST-1900-2025",
    polity_name = "Testland",
    polity_start_year = 1900L,
    polity_end_year = 2025L,
    polity_type = "national",
    mapping_status = "matched",
    has_geometry = TRUE
  )
  testthat::local_mocked_bindings(
    .polity_crosswalk = function(include_unmapped = TRUE) {
      data.table::copy(fixture)
    },
    .env = env
  )
}

test_that(".aggregate_to_polities resolves through the mocked crosswalk", {
  dt <- data.table::data.table(
    area_code = 203L,
    year = 2000L,
    element = "production",
    unit = "tonnes",
    item_prod_code = "15",
    item_prod = "Wheat",
    value = 5000
  )

  .local_aggregator_crosswalk()

  result <- whep:::.aggregate_to_polities(dt, item_prod_code, item_prod)
  expect_equal(result$area, "Testland")
  expect_equal(result$reporting_polity_code, "TST-1900-2025")
})

test_that(".aggregate_to_polities preserves fao_flag when present", {
  dt <- data.table::data.table(
    area_code = c(203L, 203L),
    year = c(2000L, 2000L),
    element = c("production", "production"),
    unit = c("tonnes", "tonnes"),
    item_prod_code = c("15", "56"),
    item_prod = c("Wheat", "Maize"),
    value = c(5000, 3000),
    fao_flag = c("A", "E")
  )

  .local_aggregator_crosswalk()

  result <- whep:::.aggregate_to_polities(dt, item_prod_code, item_prod)
  expect_true("fao_flag" %in% names(result))
})

test_that(".aggregate_to_polities works without fao_flag", {
  dt <- data.table::data.table(
    area_code = 203L,
    year = 2000L,
    element = "production",
    unit = "tonnes",
    item_prod_code = "15",
    item_prod = "Wheat",
    value = 5000
  )

  .local_aggregator_crosswalk()

  result <- whep:::.aggregate_to_polities(dt, item_prod_code, item_prod)
  expect_false("fao_flag" %in% names(result))
  expect_true("value" %in% names(result))
})

test_that("a bucket comes out of the aggregator under exactly one area label", {
  # THE INVARIANT THAT VALUE-NEUTRALITY CHECKS CANNOT SEE (whep#563).
  #
  # `.aggregate_to_polities()` renames `polity_area_code`/`polity_name` onto
  # `area_code`/`area`. A change that gives two members of one bucket different
  # polities splits the bucket's rows WITHOUT moving a value: mass conserves,
  # membership is unchanged, `polity_area_code` is untouched, and every check
  # anyone thought to run passes. What breaks is downstream -- `area` is a join
  # key, four inner joins use `c("area", "area_code")`, and duplicate
  # `(area_code, year, item)` keys are what fed the `dcast()` fallback in #425.
  #
  # KEYED ON BUCKET 206, not 999. The first version of this guard used areas 212
  # and 999, and both reviewers of #563 pointed out that it could not fail:
  # bucket 999's members all resolved to `Rest of World`, so the one bucket that
  # WAS splitting -- 206, folding Sudan and South Sudan -- went unnoticed while
  # it corrupted the published production series. 999 is now un-folded outright
  # (#459), so 206 is both the honest case and the only remaining one.
  raw <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,     ~unit,    ~value,
    2015L, 276L,       2511,           "production", "tonnes", 100,
    2015L, 277L,       2511,           "production", "tonnes", 25,
    2015L, 40L,        2511,           "production", "tonnes", 7
  )

  out <- suppressWarnings(
    whep:::.aggregate_to_polities(
      data.table::as.data.table(raw),
      item_cbs_code
    )
  )

  # One label per bucket. This is the assertion the reverted #480 failed.
  labels_per_code <- tapply(out$area, out$area_code, function(x) {
    length(unique(x))
  })
  expect_true(all(labels_per_code == 1L))

  # And the fold therefore SUMS rather than merely conserving.
  expect_equal(nrow(out[out$area_code == 206L, ]), 1L)
  expect_equal(sum(out$value[out$area_code == 206L]), 125)

  # Mass conservation is asserted too, but note it holds either way -- which is
  # precisely why it is not sufficient on its own.
  expect_equal(sum(out$value), sum(raw$value))
})

test_that("the live Sudan bucket sums instead of splitting in two", {
  # The same defect, already live on the shipped crosswalk rather than
  # hypothetical: bucket 206 folds FAOSTAT areas 206 "Sudan (former)", 276
  # Sudan and 277 South Sudan, and the three resolve to three different
  # polities. Measured on `main` before this change, over the real pins, four
  # sources came out split on bucket 206 -- faostat-fbs-new (4 area-years,
  # 2,056 duplicate keys), faostat-production (13, 2,000),
  # faostat-trade-totals (13, 3,739) and faostat-emissions-livestock (12, 144).
  # Every one of those duplicate keys is a `(area_code, year, item, element)`
  # the cast in `.select_best_source()` has to disambiguate, which is the
  # class of defect whep#425 came from.
  raw <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,     ~unit,    ~value,
    2015L, 276L,       2511,           "production", "tonnes", 100,
    2015L, 277L,       2511,           "production", "tonnes", 25,
    2005L, 206L,       2511,           "production", "tonnes", 90
  )

  out <- suppressWarnings(
    whep:::.aggregate_to_polities(
      data.table::as.data.table(raw),
      item_cbs_code
    )
  )

  post <- out[out$year == 2015L, ]
  expect_equal(nrow(post), 1L)
  expect_equal(post$value, 125)
  # And the label no longer flips mid-series: the same bucket is the same
  # territory in 2005 and 2015, which is what a join key has to be.
  expect_equal(length(unique(out$area)), 1L)
  expect_equal(sum(out$value), sum(raw$value))
})

test_that("the aggregator labels a bucket from the bucket's own code", {
  # Which label a fold carries is decided in exactly one place, and this is it:
  # the polity the BUCKET code resolves to for the year. `polity_bucket_coverage()`
  # already documents that as the label a fold carries and
  # `add_reporting_polity_columns()` already resolves the reporting columns from
  # the same code, so a member-derived label made the aggregator disagree with
  # both.
  raw <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,     ~unit,    ~value,
    2015L, 277L,       2511,           "production", "tonnes", 25
  )

  out <- suppressWarnings(
    whep:::.aggregate_to_polities(
      data.table::as.data.table(raw),
      item_cbs_code
    )
  )

  # 277's own polity is South Sudan; the bucket it is summed into is not.
  expect_equal(out$area_code, 206L)
  expect_false(out$area == "South Sudan")
  expect_equal(
    out$area,
    whep::polity_area_crosswalk$polity_name[
      whep::polity_area_crosswalk$polity_code == "SUD-1956-2011"
    ][[1]]
  )
})

# -- .iso3_to_fao_area_code tie-break (#586) -----------------------------------

test_that("an ISO3 naming two areas resolves to the canonical one", {
  # FAOSTAT keeps a pre-split entity beside its successor, so two ISO3 codes
  # name two reporting areas each: ETH is 62 ("Ethiopia PDR", dissolved 1993)
  # and 238 ("Ethiopia"); SDN is 206 ("Sudan (former)") and 276 ("Sudan").
  # The tie used to be broken by `unique(bridge, by = "iso3c")` -- row order --
  # and `.current_area_lookup()` orders by `area_code`, so it kept the LOWEST,
  # which for ETH is the dissolved 62, for every year.
  bridge <- whep:::.iso3_area_code_bridge()

  expect_equal(bridge$area_code_fao[bridge$iso3c == "ETH"], 238L)
  # 276 folds into bucket 206, so 206 is the canonical area for SDN and this
  # one was already right; it is pinned so the fix cannot move it.
  expect_equal(bridge$area_code_fao[bridge$iso3c == "SDN"], 206L)
  expect_equal(bridge$area_code_fao[bridge$iso3c == "SSD"], 277L)
})

test_that("the ISO3 bridge is one row per ISO3 and never the dissolved area", {
  # An invariant rather than two hand-picked codes: whenever an ISO3 names any
  # area that IS its polity's `polity_area_code`, that is the one chosen. This
  # is what row order got wrong, and it holds for every ISO3, not just ETH.
  bridge <- whep:::.iso3_area_code_bridge()
  expect_false(any(duplicated(bridge$iso3c)))

  lookup <- whep:::.current_area_lookup(include_unmapped = FALSE)
  lookup <- lookup[!is.na(lookup$area_iso3c), ]
  canonical <- lookup[
    !is.na(lookup$polity_area_code) &
      lookup$area_code == lookup$polity_area_code,
    c("area_iso3c", "area_code")
  ]
  data.table::setnames(
    canonical,
    c("area_iso3c", "area_code"),
    c("iso3c", "canonical_code")
  )
  checked <- merge(bridge, canonical, by = "iso3c")
  expect_gt(nrow(checked), 0L)
  expect_equal(checked$area_code_fao, checked$canonical_code)
})

test_that(".iso3_to_fao_area_code stamps the canonical code on real rows", {
  dt <- data.table::data.table(
    area_code = c("ETH", "SDN", "ESP"),
    year = c(2000L, 2000L, 2000L),
    value = c(1, 2, 3)
  )

  out <- whep:::.iso3_to_fao_area_code(dt)
  out <- out[order(out$value), ]

  expect_equal(out$area_code, c(238L, 206L, 203L))
  # The values ride along untouched: this helper only restamps the key.
  expect_equal(out$value, c(1, 2, 3))
  expect_equal(nrow(out), 3L)
})

test_that("an ISO3 still ambiguous after the rule aborts instead of guessing", {
  # The ISO3 codes that fold into an aggregate have no canonical area at all,
  # so the rule cannot decide for them. Today each names exactly one area, but
  # that is a property of the data, not a guarantee -- so the function must
  # refuse rather than silently take whichever row came first.
  fake <- data.table::data.table(
    area_code = c(900L, 901L),
    area_iso3c = c("XXX", "XXX"),
    polity_area_code = c(999L, 999L),
    polity_code = c("ROW-1850-2025", "ROW-1850-2025")
  )
  testthat::local_mocked_bindings(
    .current_area_lookup = function(...) fake,
    .package = "whep"
  )

  expect_error(
    whep:::.iso3_area_code_bridge(),
    class = "whep_ambiguous_iso3_area"
  )
})

# -- .extract_cb row order -----------------------------------------------------

# The four CB extracts travel as the `.cb_extracts` attribute of
# `build_primary_production()`, so their row order is part of a published
# object. `.read_input()` reads the parquet through arrow's multi-threaded
# scanner, which hands back the same rows in a session-dependent order, and
# nothing downstream pinned one -- so the build was not `identical()` to itself
# across sessions (whep#747). Feeding the same rows in two orders reproduces
# that here without touching a pin or the network.
.cb_order_fixture <- function() {
  tibble::tribble(
    ~`Area Code`, ~Area,      ~`Item Code`, ~Item,                ~Element,     ~Unit,    ~Year, ~Value,
    203L,         "Testland", 2511,         "Wheat and products", "Production", "tonnes", 2000L, 100,
    203L,         "Testland", 2511,         "Wheat and products", "Food",       "tonnes", 2000L, 40,
    203L,         "Testland", 2514,         "Maize and products", "Production", "tonnes", 2000L, 70,
    203L,         "Testland", 2514,         "Maize and products", "Production", "tonnes", 2001L, 80,
    203L,         "Testland", 2511,         "Wheat and products", "Production", "tonnes", 2001L, 110
  ) |>
    data.table::as.data.table()
}

test_that(".extract_cb row order does not depend on the read order", {
  fixture <- .cb_order_fixture()
  .local_aggregator_crosswalk()

  extract_in_order <- function(rows) {
    testthat::local_mocked_bindings(
      .read_input = function(pin_alias, years = NULL, year_col = NULL) {
        data.table::copy(rows)
      }
    )
    whep:::.extract_cb("faostat-fbs-old") |>
      as.data.frame()
  }

  forward <- extract_in_order(fixture)
  reversed <- extract_in_order(fixture[rev(seq_len(nrow(fixture)))])

  expect_gt(nrow(forward), 1L)
  expect_identical(forward, reversed)
})

# -- .extract_fao row order ----------------------------------------------------

# The same defect one stage earlier, and the stage the CBS build consumes
# directly: `.read_fao_trade()` and the `faostat-cbs-new` extract stop at
# `.extract_fao()`, so `.extract_cb()`'s sort never reaches them. Measured on
# the real pins at 1950-1965, `.read_fao_trade()` came back in a different row
# order in every one of three sessions (339,220 rows, same rows and same
# values), because the `by=` aggregation in `.aggregate_to_polities()` emits
# groups in order of first appearance and arrow's multi-threaded scanner
# decides what appears first (whep#420).
test_that(".extract_fao row order does not depend on the read order", {
  fixture <- .cb_order_fixture()
  .local_aggregator_crosswalk()

  extract_in_order <- function(rows) {
    testthat::local_mocked_bindings(
      .read_input = function(pin_alias, years = NULL, year_col = NULL) {
        data.table::copy(rows)
      }
    )
    whep:::.extract_fao("faostat-trade-totals") |>
      as.data.frame()
  }

  forward <- extract_in_order(fixture)
  reversed <- extract_in_order(fixture[rev(seq_len(nrow(fixture)))])

  expect_gt(nrow(forward), 1L)
  expect_identical(forward, reversed)
})

# Issue whep#833. `.correct_processed()` calibrates a processing output by
# dividing the observed production of that output by the production its parent's
# `processing` implies, and then carries the one ratio it finds across the
# whole year axis. Where it finds none, `scaling` collapses to 0 for every item
# that is neither `Required` in `cb_processing` nor a `no_data_product`, and
# the output is deleted by the `value != 0` filters downstream. So the SPAN of
# the frame decides whether the row exists, which is how a year-scoped build
# loses 14 keys the full-range build has (Italy's Ricebran Oil at 2010 is
# calibrated off a single 1961 observation, 49 years away).
.processed_axis_fixture <- function(years) {
  key <- tibble::tibble(
    area = "Testland",
    area_code = 999L,
    item_cbs = "Ricebran Oil",
    element = "production"
  )
  list(
    processed = tidyr::expand_grid(year = years, key) |>
      dplyr::mutate(value_proc = 200),
    cbs = tidyr::expand_grid(year = years, key) |>
      dplyr::mutate(item_cbs_code = 2581L, value = 100) |>
      dplyr::filter(year == 2000L)
  )
}

.processed_axis_value <- function(years) {
  fixture <- .processed_axis_fixture(years)
  whep:::.correct_processed(fixture$processed, fixture$cbs) |>
    tibble::as_tibble() |>
    dplyr::filter(year == 2010L) |>
    dplyr::pull(value_final)
}

test_that(".correct_processed carries one anchor across the year axis", {
  expect_equal(.processed_axis_value(2000:2010), 100)
})

test_that(".correct_processed deletes the output off-anchor (whep#833)", {
  # The defect, pinned so it is reproducible without a pipeline build: the same
  # 2010 output is worth 100 t on an axis holding the 2000 anchor and 0 t on
  # one that does not. Fixing #833 makes these two agree, and this expectation
  # must then be replaced by an equality against the full-axis answer.
  expect_equal(.processed_axis_value(2005:2010), 0)
})
