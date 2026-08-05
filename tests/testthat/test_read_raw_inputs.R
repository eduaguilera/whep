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
  # `.aggregate_to_polities()` groups by `polity_name` as well as
  # `polity_area_code`, and renames the pair to `area`/`area_code`. So a change
  # that gives two members of one FABIO bucket different polities splits the
  # bucket's rows WITHOUT moving any value: mass still conserves, the bucket keeps
  # its members, `polity_area_code` is untouched, and every check anyone thought
  # to run passes. What breaks is downstream -- `area` is a join key, four inner
  # joins use `c("area", "area_code")`, and duplicate `(area_code, year, item)`
  # keys are what fed the `dcast()` `length()` fallback in whep#425.
  #
  # PR whep#480 did exactly this and had to be reverted (whep#561): FAOSTAT area
  # 212 Syria was un-folded onto `SYR-*` while keeping bucket 999, so
  # `999 Rest of World 340` became `999 Syrian Arab Republic 300` plus
  # `999 Rest of World 40`. 347 tonnes in, 347 tonnes out, and a split bucket.
  #
  # Areas 212 and 999 are the live pair: both carry `polity_area_code` 999 today.
  raw <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,     ~unit,    ~value,
    2010L, 212L,       2511,           "production", "tonnes", 300,
    2010L, 999L,       2511,           "production", "tonnes", 40,
    2010L, 40L,        2511,           "production", "tonnes", 7
  )

  out <- suppressWarnings(
    whep:::.aggregate_to_polities(
      data.table::as.data.table(raw),
      item_cbs_code
    )
  )

  # One label per bucket. This is the assertion the reverted change failed.
  labels_per_code <- tapply(out$area, out$area_code, function(x) {
    length(unique(x))
  })
  expect_true(all(labels_per_code == 1L))

  # And the fold therefore SUMS rather than merely conserving.
  expect_equal(nrow(out[out$area_code == 999L, ]), 1L)
  expect_equal(sum(out$value[out$area_code == 999L]), 340)

  # Mass conservation is asserted too, but note it holds either way -- which is
  # precisely why it is not sufficient on its own.
  expect_equal(sum(out$value), sum(raw$value))
})
