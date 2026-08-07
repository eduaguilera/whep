# test_read_raw_inputs.R — tests for R/read_raw_inputs.R helpers

# -- .aggregate_to_polities fao_flag handling ----------------------------------

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

  local_mocked_bindings(
    .polity_bridge = function() {
      data.table::data.table(
        area_code = 203L,
        polity_code = "ESP",
        polity_name = "Spain",
        polity_area_code = 203L
      )
    }
  )

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

  local_mocked_bindings(
    .polity_bridge = function() {
      data.table::data.table(
        area_code = 203L,
        polity_code = "ESP",
        polity_name = "Spain",
        polity_area_code = 203L
      )
    }
  )

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
