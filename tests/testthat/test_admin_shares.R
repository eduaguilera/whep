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

# --- T38: value is allow-missing where share is present ----------------------
#
# The Latin American panel is consented as DERIVED SHARES ONLY (T23,
# 2026-09-02), so a row carrying `share` and no `value` is a first-class
# case the contract has to admit. What stays refused is a row carrying
# NEITHER measurement, and any synthetic value invented to satisfy the old
# rule.

# One shares-only container-item-year, in the shape the Latin American
# family ships: `share` present, `value` absent, everything else as the
# contract requires.
shares_only_rows <- function() {
  tibble::tibble(
    area_code = c(19L, 19L),
    level_polity_code = c("BOL-LAPAZ", "BOL-SANTACRUZ"),
    level = c(1L, 1L),
    item_prod_code = c(661L, 661L),
    indicator_used = c("area_harvested", "area_harvested"),
    year = c(2020L, 2020L),
    value = NA_real_,
    share = c(0.79, 0.21),
    source = "admin-stats-latam",
    tier = 3L,
    grain = "admin1",
    concept_break = FALSE,
    nuts_version = NA_character_,
    source_native_id = c("BOL-LAPAZ", "BOL-SANTACRUZ"),
    source_native_name = c("La Paz", "Santa Cruz"),
    source_id = "admin-stats-latam",
    source_version = "2026-05-21",
    recorded_at = "2026-09-03T06:21:54Z",
    treatment_year = "observed",
    value_flag = NA_character_
  )
}

test_that("the schema accepts a shares-only row with no value", {
  expect_equal(
    nrow(whep::check_table_schema(
      shares_only_rows(),
      whep::admin_shares_schema()
    )),
    0
  )
})

test_that("a share out of bounds is still refused where value is absent", {
  # Relaxing `value` must not relax `share`'s own bounds: it is still
  # checked wherever it is present. A *missing* share is allowed, and the
  # valued fixtures above rely on that.
  rows <- shares_only_rows()
  rows$share[1] <- -0.1
  diagnostics <- whep::check_table_schema(rows, whep::admin_shares_schema())

  expect_true(any(
    diagnostics$rule == "below_min" & diagnostics$column == "share"
  ))
})

# --- T38 repair: a non-finite measurement is not a missing one ---------------
#
# `is.na(NaN)` is TRUE, so with `value` allow-missing every downstream
# `is.na(value)` branch reads a 0/0 artefact as "this row ships no value"
# -- laundering computed garbage into the consented shares-only case. The
# schema's own bounds cannot catch it either: `.schema_bound_rows()` guards
# with `!is.na(values)`, and `value` has no maximum, so `Inf` clears
# `min = 0` as well.

test_that("a NaN value is refused rather than read as a missing one", {
  rows <- shares_only_rows()
  rows$value <- c(NaN, 12)

  err <- expect_error(
    whep::ensure_admin_shares(rows),
    class = "whep_error_admin_nonfinite"
  )
  expect_match(conditionMessage(err), "BOL-LAPAZ")
  expect_match(conditionMessage(err), "value")
})

test_that("an infinite value is refused, which no schema bound catches", {
  rows <- valid_admin_shares_rows()
  rows$value[2] <- Inf

  expect_equal(
    nrow(whep::check_table_schema(rows, whep::admin_shares_schema())),
    0
  )
  expect_error(
    whep::ensure_admin_shares(rows),
    class = "whep_error_admin_nonfinite"
  )
})

test_that("a NaN share is refused on the same rule", {
  rows <- valid_admin_shares_rows()
  rows$share[1] <- NaN

  err <- expect_error(
    whep::ensure_admin_shares(rows),
    class = "whep_error_admin_nonfinite"
  )
  expect_match(conditionMessage(err), "share")
})

test_that("a genuinely missing value is still accepted", {
  # The repair must not re-close what T23 opened: `NA_real_` is the
  # consented shares-only case and stays legal.
  completed <- whep::ensure_admin_shares(shares_only_rows())

  expect_true(all(is.na(completed$value)))
  expect_false(any(is.nan(completed$value)))
})

test_that("ensure_admin_shares carries a shares-only tibble unchanged", {
  completed <- whep::ensure_admin_shares(shares_only_rows())

  expect_equal(nrow(completed), 2)
  expect_true(all(is.na(completed$value)))
  expect_equal(completed$share, c(0.79, 0.21))
})

test_that("ensure_admin_shares aborts on a row with neither measurement", {
  rows <- shares_only_rows()
  rows$share[2] <- NA_real_

  err <- expect_error(
    whep::ensure_admin_shares(rows),
    class = "whep_error_admin_no_measure"
  )
  expect_match(conditionMessage(err), "BOL-SANTACRUZ")
})

test_that("a row with neither measurement is refused even in bulk", {
  # The abort names how many rows and one offender, not a wall of rows.
  rows <- dplyr::bind_rows(shares_only_rows(), valid_admin_shares_rows())
  rows$share[1:2] <- NA_real_

  err <- expect_error(
    whep::ensure_admin_shares(rows),
    class = "whep_error_admin_no_measure"
  )
  expect_match(conditionMessage(err), "2")
})
