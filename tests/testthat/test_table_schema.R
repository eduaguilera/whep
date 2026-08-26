schema_fixture <- function() {
  list(
    columns = list(
      list(name = "year", type = "integer", min = 1961L, max = 2023L),
      list(name = "area_code", type = "integer", allow_missing = FALSE),
      list(
        name = "source",
        type = "character",
        allowed = c("FAOSTAT_prod", "LUH2")
      ),
      list(name = "value", type = "double", min = 0)
    ),
    key = c("year", "area_code")
  )
}

data_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~source,        ~value,
    2000L, 4L,         "FAOSTAT_prod", 1,
    2001L, 4L,         "LUH2",         2
  )
}

test_that("a conforming table yields zero diagnostics", {
  diagnostics <- whep::check_table_schema(data_fixture(), schema_fixture())

  expect_s3_class(diagnostics, "tbl_df")
  expect_equal(nrow(diagnostics), 0)
  expect_named(
    diagnostics,
    c("row", "column", "rule", "value", "severity", "detail")
  )
  expect_type(diagnostics$row, "integer")
})

test_that("validation never changes the input table", {
  data <- data_fixture() |>
    dplyr::group_by(area_code)
  before <- data

  whep::check_table_schema(data, schema_fixture())
  returned <- whep::assert_table_schema(data, schema_fixture())

  expect_identical(data, before)
  expect_identical(returned, before)
})

test_that("out-of-bounds and out-of-vocabulary values are reported", {
  data <- tibble::tribble(
    ~year, ~area_code, ~source,        ~value,
    1900L, 4L,         "FAOSTAT_prod", 1,
    2000L, 8L,         "guess",        -2
  )

  diagnostics <- whep::check_table_schema(data, schema_fixture())

  expect_equal(
    diagnostics$rule,
    c("below_min", "not_allowed", "below_min")
  )
  expect_equal(diagnostics$column, c("year", "source", "value"))
  expect_equal(diagnostics$row, c(1L, 2L, 2L))
  expect_equal(diagnostics$value, c("1900", "guess", "-2"))
  expect_true(all(diagnostics$severity == "error"))
})

test_that("an above-max value is reported", {
  data <- dplyr::mutate(data_fixture(), year = c(2024L, 2025L))

  diagnostics <- whep::check_table_schema(data, schema_fixture())

  expect_equal(diagnostics$rule, c("above_max", "above_max"))
  expect_equal(diagnostics$row, c(1L, 2L))
})

test_that("a type mismatch is reported once, not once per row", {
  data <- dplyr::mutate(data_fixture(), year = as.double(year))

  diagnostics <- whep::check_table_schema(data, schema_fixture())

  expect_equal(nrow(diagnostics), 1)
  expect_equal(diagnostics$rule, "type_mismatch")
  expect_equal(diagnostics$column, "year")
  expect_equal(diagnostics$value, "double")
  expect_true(is.na(diagnostics$row))
})

test_that("missing values are reported only where forbidden", {
  data <- dplyr::mutate(
    data_fixture(),
    year = c(NA_integer_, 2001L),
    area_code = c(NA_integer_, 4L)
  )

  diagnostics <- whep::check_table_schema(data, schema_fixture())

  expect_equal(diagnostics$rule, "missing_value")
  expect_equal(diagnostics$column, "area_code")
  expect_equal(diagnostics$row, 1L)
})

test_that("a missing required column is reported, an optional one is not", {
  schema <- schema_fixture()
  schema$columns[[4]]$required <- FALSE
  data <- dplyr::select(data_fixture(), -source, -value)

  diagnostics <- whep::check_table_schema(data, schema)

  expect_equal(diagnostics$rule, "missing_column")
  expect_equal(diagnostics$column, "source")
  expect_match(diagnostics$detail, "character column")
})

test_that("duplicate keys and duplicate column values are reported", {
  schema <- schema_fixture()
  schema$columns[[3]]$unique <- TRUE
  data <- tibble::tribble(
    ~year, ~area_code, ~source, ~value,
    2000L, 4L,         "LUH2",  1,
    2000L, 4L,         "LUH2",  2
  )

  diagnostics <- whep::check_table_schema(data, schema)

  expect_equal(
    diagnostics$rule,
    c("duplicate_value", "duplicate_value", "duplicate_key", "duplicate_key")
  )
  expect_equal(diagnostics$row, c(1L, 2L, 1L, 2L))
  expect_equal(diagnostics$value[3:4], c("2000 | 4", "2000 | 4"))
})

test_that("an empty table triggers only table- and column-scope rules", {
  empty <- dplyr::filter(data_fixture(), FALSE)

  expect_equal(nrow(whep::check_table_schema(empty, schema_fixture())), 0)

  schema <- schema_fixture()
  schema$allow_empty <- FALSE
  diagnostics <- whep::check_table_schema(empty, schema)

  expect_equal(diagnostics$rule, "empty_table")
  expect_true(is.na(diagnostics$column))
  expect_true(is.na(diagnostics$row))
})

test_that("a zero-column schema accepts any table", {
  schema <- list(columns = list())

  expect_equal(nrow(whep::check_table_schema(data_fixture(), schema)), 0)
})

test_that("undeclared columns are reported only when forbidden", {
  data <- dplyr::mutate(data_fixture(), note = "extra")

  expect_equal(nrow(whep::check_table_schema(data, schema_fixture())), 0)

  schema <- schema_fixture()
  schema$extra_columns <- "forbid"
  diagnostics <- whep::check_table_schema(data, schema)

  expect_equal(diagnostics$rule, "unexpected_column")
  expect_equal(diagnostics$column, "note")
})

test_that("strict column order is enforced only when requested", {
  data <- dplyr::relocate(data_fixture(), value)

  expect_equal(nrow(whep::check_table_schema(data, schema_fixture())), 0)

  schema <- schema_fixture()
  schema$column_order <- "strict"
  diagnostics <- whep::check_table_schema(data, schema)

  expect_equal(diagnostics$rule, "column_order")
  expect_match(diagnostics$detail, "found value, year")
})

test_that("severity comes from the column specification", {
  schema <- schema_fixture()
  schema$columns[[3]]$severity <- "warning"
  data <- dplyr::mutate(data_fixture(), source = c("guess", "LUH2"))

  diagnostics <- whep::check_table_schema(data, schema)

  expect_equal(diagnostics$severity, "warning")
  expect_warning(
    whep::assert_table_schema(data, schema),
    "not_allowed"
  )
})

test_that("diagnostics follow schema column order, not input order", {
  schema <- schema_fixture()
  data <- tibble::tribble(
    ~value, ~source,  ~area_code, ~year,
    -1,     "guess",  4L,         1900L
  )

  diagnostics <- whep::check_table_schema(data, schema)

  expect_equal(diagnostics$column, c("year", "source", "value"))
  expect_identical(
    diagnostics,
    whep::check_table_schema(data, schema)
  )
})

test_that("every documented rule is reachable", {
  documented <- c(
    "empty_table",
    "column_order",
    "missing_column",
    "unexpected_column",
    "type_mismatch",
    "missing_value",
    "below_min",
    "above_max",
    "not_allowed",
    "duplicate_value",
    "duplicate_key"
  )
  schema <- list(
    columns = list(
      list(name = "code", type = "integer", min = 0, max = 10),
      list(name = "label", type = "character", allowed = "ok", unique = TRUE),
      list(name = "flag", type = "logical", allow_missing = FALSE),
      list(name = "gone", type = "double")
    ),
    key = "code",
    extra_columns = "forbid",
    column_order = "strict",
    allow_empty = FALSE
  )
  populated <- tibble::tribble(
    ~label, ~code, ~flag,        ~extra,
    "bad",  -1L,   NA,           1,
    "bad",  -1L,   TRUE,         2,
    "bad",  99L,   FALSE,        3
  )
  wrong_type <- dplyr::mutate(populated, code = as.character(code))

  reachable <- c(
    whep::check_table_schema(populated, schema)$rule,
    whep::check_table_schema(wrong_type, schema)$rule,
    whep::check_table_schema(populated[0, ], schema)$rule
  )

  expect_setequal(unique(reachable), documented)
})

test_that("the schema survives a YAML round trip", {
  schema <- schema_fixture()
  data <- dplyr::mutate(data_fixture(), value = c(-1, 2))

  expect_identical(
    whep::check_table_schema(
      data,
      yaml::yaml.load(yaml::as.yaml(schema))
    ),
    whep::check_table_schema(data, schema)
  )
})

test_that("malformed input tables abort", {
  expect_error(
    whep::check_table_schema(1:3, schema_fixture()),
    class = "whep_error_schema_input"
  )
  duplicated_names <- data.frame(x = 1, x = 2, check.names = FALSE)
  expect_error(
    whep::check_table_schema(duplicated_names, list(columns = list())),
    class = "whep_error_schema_input"
  )
})

test_that("malformed schemas abort with an actionable error", {
  malformed <- list(
    not_a_list = "columns",
    missing_columns = list(key = "year"),
    named_columns = list(columns = list(year = list(type = "integer"))),
    unknown_field = list(columns = list(list(name = "a", kind = "integer"))),
    unknown_top_field = list(columns = list(), extra = TRUE),
    no_type = list(columns = list(list(name = "a"))),
    bad_type = list(columns = list(list(name = "a", type = "int"))),
    empty_name = list(columns = list(list(name = "", type = "integer"))),
    duplicate_name = list(
      columns = list(
        list(name = "a", type = "integer"),
        list(name = "a", type = "double")
      )
    ),
    bound_on_character = list(
      columns = list(list(name = "a", type = "character", min = 1))
    ),
    vocabulary_on_list = list(
      columns = list(list(name = "a", type = "list", allowed = 1))
    ),
    uncastable_bound = list(
      columns = list(list(name = "a", type = "integer", min = "low"))
    ),
    vector_bound = list(
      columns = list(list(name = "a", type = "integer", min = c(1L, 2L)))
    ),
    empty_vocabulary = list(
      columns = list(
        list(name = "a", type = "character", allowed = character())
      )
    ),
    bad_flag = list(
      columns = list(list(name = "a", type = "integer", required = "yes"))
    ),
    bad_severity = list(
      columns = list(list(name = "a", type = "integer", severity = "fatal"))
    ),
    undeclared_key = list(columns = list(), key = "year"),
    bad_key = list(columns = list(), key = 1),
    bad_choice = list(columns = list(), extra_columns = "maybe")
  )

  purrr::iwalk(malformed, function(schema, label) {
    expect_error(
      whep::check_table_schema(data_fixture(), schema),
      class = "whep_error_schema_spec",
      label = label
    )
  })
})

test_that("assert_table_schema aborts on error diagnostics", {
  data <- dplyr::mutate(data_fixture(), value = c(-1, 2))

  expect_error(
    whep::assert_table_schema(data, schema_fixture()),
    class = "whep_error_schema_violation"
  )
  expect_error(
    whep::assert_table_schema(data, schema_fixture(), arg = "production"),
    "production"
  )
  expect_silent(
    whep::assert_table_schema(data_fixture(), schema_fixture())
  )
})
