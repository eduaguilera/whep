valid_admin_shares_rows <- function() {
  tibble::tibble(
    area_code = c(840L, 840L),
    level_polity_code = c("USA-IOWA", "USA-ILLINOIS"),
    level = c(1L, 1L),
    item_prod_code = c(44L, 44L),
    indicator_used = c("area_harvested", "area_harvested"),
    year = c(2020L, 2020L),
    value = c(1000000, 800000),
    share = c(0.42, 0.34),
    source = c("USDA_NASS", "USDA_NASS"),
    tier = c(1L, 1L),
    grain = c("admin1", "admin1"),
    concept_break = c(FALSE, FALSE),
    nuts_version = NA_character_,
    source_native_id = c("19", "17"),
    source_native_name = c("Iowa", "Illinois"),
    source_id = c("USDA_NASS", "USDA_NASS"),
    source_version = c("2021-05", "2021-05"),
    recorded_at = "2026-01-01T00:00:00Z",
    treatment_year = c("observed", "observed"),
    value_flag = NA_character_
  )
}

test_that("the prototype has exactly the contract columns and types", {
  proto <- whep::admin_shares_prototype()

  expect_s3_class(proto, "tbl_df")
  expect_equal(nrow(proto), 0)
  expect_named(
    proto,
    c(
      "area_code",
      "level_polity_code",
      "level",
      "item_prod_code",
      "indicator_used",
      "year",
      "value",
      "share",
      "source",
      "tier",
      "grain",
      "concept_break",
      "nuts_version",
      "source_native_id",
      "source_native_name",
      "source_id",
      "source_version",
      "recorded_at",
      "treatment_year",
      "value_flag"
    )
  )
  expect_true(is.integer(proto$area_code))
  expect_true(is.character(proto$level_polity_code))
  expect_true(is.integer(proto$level))
  expect_true(is.integer(proto$item_prod_code))
  expect_true(is.character(proto$indicator_used))
  expect_true(is.integer(proto$year))
  expect_true(is.double(proto$value))
  expect_true(is.double(proto$share))
  expect_true(is.character(proto$source))
  expect_true(is.integer(proto$tier))
  expect_true(is.character(proto$grain))
  expect_true(is.logical(proto$concept_break))
  expect_true(is.character(proto$nuts_version))
  expect_true(is.character(proto$source_native_id))
  expect_true(is.character(proto$source_native_name))
  expect_true(is.character(proto$source_id))
  expect_true(is.character(proto$source_version))
  expect_true(is.character(proto$recorded_at))
  expect_true(is.character(proto$treatment_year))
  expect_true(is.character(proto$value_flag))

  # Built from the schema, so it conforms by construction.
  expect_equal(
    nrow(whep::check_table_schema(proto, whep::admin_shares_schema())),
    0
  )
})

test_that("the schema declares the documented key and closes extra columns", {
  schema <- whep::admin_shares_schema()

  expect_equal(
    schema$key,
    c(
      "area_code",
      "level_polity_code",
      "level",
      "item_prod_code",
      "indicator_used",
      "year"
    )
  )
  expect_equal(schema$extra_columns, "forbid")
})

test_that("the schema accepts a valid two-row tibble", {
  expect_equal(
    nrow(whep::check_table_schema(
      valid_admin_shares_rows(),
      whep::admin_shares_schema()
    )),
    0
  )
})

test_that("the schema rejects an extra column", {
  rows <- dplyr::mutate(valid_admin_shares_rows(), bogus = 1)
  diagnostics <- whep::check_table_schema(rows, whep::admin_shares_schema())

  expect_true("unexpected_column" %in% diagnostics$rule)
  expect_true("bogus" %in% diagnostics$column)
})

test_that("the schema rejects a bad indicator_used", {
  rows <- valid_admin_shares_rows()
  rows$indicator_used[1] <- "nonsense"
  diagnostics <- whep::check_table_schema(rows, whep::admin_shares_schema())

  expect_true(any(
    diagnostics$rule == "not_allowed" &
      diagnostics$column == "indicator_used"
  ))
})

test_that("the schema rejects a share outside [0, 1]", {
  rows <- valid_admin_shares_rows()
  rows$share[1] <- 1.5
  diagnostics <- whep::check_table_schema(rows, whep::admin_shares_schema())

  expect_true(any(
    diagnostics$rule == "above_max" & diagnostics$column == "share"
  ))
})

test_that("the schema rejects a non-ordered (out-of-vocabulary) grain", {
  rows <- valid_admin_shares_rows()
  rows$grain[1] <- "county"
  diagnostics <- whep::check_table_schema(rows, whep::admin_shares_schema())

  expect_true(any(
    diagnostics$rule == "not_allowed" & diagnostics$column == "grain"
  ))
})

test_that("ensure_admin_shares completes a partial tibble to the contract", {
  # Omits only the columns the contract allows missing. `level_polity_code`
  # stays present and distinct per row even though the contract allows it
  # to be `NA`, because it is part of the key: two rows that both fall back
  # to `NA` there would collide on every other key column.
  partial <- tibble::tibble(
    area_code = c(840L, 840L),
    level_polity_code = c("USA-IOWA", "USA-ILLINOIS"),
    level = c(1L, 1L),
    item_prod_code = c(44L, 44L),
    indicator_used = c("area_harvested", "area_harvested"),
    year = c(2020L, 2020L),
    value = c(1000000, 800000),
    source = c("USDA_NASS", "USDA_NASS"),
    tier = c(1L, 1L),
    grain = c("admin1", "admin1"),
    concept_break = c(FALSE, FALSE),
    source_id = c("USDA_NASS", "USDA_NASS"),
    recorded_at = "2026-01-01T00:00:00Z",
    treatment_year = c("observed", "observed")
  )

  completed <- whep::ensure_admin_shares(partial)

  expect_named(completed, names(whep::admin_shares_prototype()))
  expect_equal(nrow(completed), 2)
  expect_true(all(is.na(completed$share)))
  expect_true(all(is.na(completed$nuts_version)))
  expect_equal(
    nrow(whep::check_table_schema(completed, whep::admin_shares_schema())),
    0
  )
})

test_that("ensure_admin_shares aborts, naming the offending column", {
  partial <- tibble::tibble(
    area_code = c(840L, 840L),
    level_polity_code = c("USA-IOWA", "USA-ILLINOIS"),
    level = c(1L, 1L),
    item_prod_code = c(44L, 44L),
    year = c(2020L, 2020L),
    value = c(1000000, 800000),
    source = c("USDA_NASS", "USDA_NASS"),
    tier = c(1L, 1L),
    grain = c("admin1", "admin1"),
    concept_break = c(FALSE, FALSE),
    source_id = c("USDA_NASS", "USDA_NASS"),
    recorded_at = "2026-01-01T00:00:00Z",
    treatment_year = c("observed", "observed")
  )

  err <- expect_error(
    whep::ensure_admin_shares(partial),
    class = "whep_error_schema_violation"
  )
  expect_match(conditionMessage(err), "indicator_used")
})
