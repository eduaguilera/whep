stamp <- function() {
  as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
}

evidence_rows <- function() {
  tibble::tribble(
    ~area_code, ~year, ~fao_flag,
    724L,       2020L, "A",
    724L,       2021L, "E",
    76L,        2020L, NA_character_
  )
}

sample_evidence <- function(source_id = "FAOSTAT_prod", ...) {
  whep::row_evidence(
    evidence_rows(),
    source_id = source_id,
    key = c("area_code", "year"),
    fields = "fao_flag",
    recorded_at = stamp(),
    ...
  )
}

test_that("the producer emits the documented format", {
  evidence <- sample_evidence(source_version = "2024-03-14")

  expect_s3_class(evidence, "tbl_df")
  expect_named(
    evidence,
    c(
      "row_key",
      "key_columns",
      "source_id",
      "source_version",
      "recorded_at",
      "field",
      "value"
    )
  )
  expect_true(all(purrr::map_lgl(evidence, is.character)))
  expect_equal(nrow(evidence), 3)
  expect_equal(unique(evidence$key_columns), "area_code,year")
  expect_equal(unique(evidence$source_id), "FAOSTAT_prod")
  expect_equal(unique(evidence$source_version), "2024-03-14")
  expect_equal(unique(evidence$recorded_at), "2026-01-01T12:00:00Z")
  expect_equal(unique(evidence$field), "fao_flag")
  expect_setequal(evidence$value, c("A", "E", NA))
})

test_that("the producer's own schema accepts its output", {
  expect_silent(
    whep::assert_table_schema(sample_evidence(), whep::row_evidence_schema())
  )
  expect_equal(
    nrow(whep::check_table_schema(
      sample_evidence(),
      whep::row_evidence_schema()
    )),
    0
  )
})

test_that("the schema is serializable data", {
  schema <- whep::row_evidence_schema()

  expect_identical(yaml::yaml.load(yaml::as.yaml(schema)), schema)
})

test_that("row keys are injective across key columns", {
  # Plain concatenation would map both rows to "abc"; the separator is
  # what keeps a two-column key injective.
  ambiguous <- tibble::tibble(
    left = c("a", "ab"),
    right = c("bc", "c"),
    flag = "x"
  )
  evidence <- whep::row_evidence(
    ambiguous,
    source_id = "S",
    key = c("left", "right"),
    fields = "flag",
    recorded_at = stamp()
  )

  expect_equal(dplyr::n_distinct(evidence$row_key), 2)
})

test_that("a key value holding the separator aborts", {
  colliding <- tibble::tibble(
    left = c("a\u001fbc", "x"),
    right = c("d", "y"),
    flag = "x"
  )
  expect_error(
    whep::row_evidence(
      colliding,
      source_id = "S",
      key = c("left", "right"),
      fields = "flag",
      recorded_at = stamp()
    ),
    class = "whep_error_evidence_key"
  )
})

test_that("output is deterministic and independent of row order", {
  shuffled <- evidence_rows()[c(3, 1, 2), ]
  straight <- sample_evidence()
  reordered <- whep::row_evidence(
    shuffled,
    source_id = "FAOSTAT_prod",
    key = c("area_code", "year"),
    fields = "fao_flag",
    recorded_at = stamp()
  )

  expect_identical(reordered, straight)
  expect_identical(sample_evidence(), straight)
})

test_that("fields may be given as columns or as values", {
  from_columns <- sample_evidence()
  from_values <- whep::row_evidence(
    evidence_rows(),
    source_id = "FAOSTAT_prod",
    key = c("area_code", "year"),
    fields = list(fao_flag = evidence_rows()$fao_flag),
    recorded_at = stamp()
  )

  expect_identical(from_values, from_columns)
})

test_that("a scalar field is recycled and typed values are formatted", {
  evidence <- whep::row_evidence(
    evidence_rows(),
    source_id = "S",
    key = c("area_code", "year"),
    fields = list(
      method_land = "reported",
      revised = c(TRUE, FALSE, TRUE),
      share = c(0.5, 1, 2)
    ),
    recorded_at = stamp()
  )

  expect_equal(nrow(evidence), 9)
  expect_equal(
    unique(evidence$value[evidence$field == "method_land"]),
    "reported"
  )
  expect_setequal(
    evidence$value[evidence$field == "revised"],
    c("TRUE", "FALSE")
  )
  expect_setequal(
    evidence$value[evidence$field == "share"],
    c("0.5", "1", "2")
  )
})

test_that("a zero-row table yields a zero-row evidence table", {
  evidence <- whep::row_evidence(
    evidence_rows()[0, ],
    source_id = "S",
    key = c("area_code", "year"),
    fields = "fao_flag",
    recorded_at = stamp()
  )

  expect_equal(nrow(evidence), 0)
  expect_silent(
    whep::assert_table_schema(evidence, whep::row_evidence_schema())
  )
})

test_that("the evidence table round-trips through Parquet and CSV", {
  evidence <- sample_evidence(source_version = "v1")

  parquet_path <- withr::local_tempfile(fileext = ".parquet")
  whep::write_table_checked(evidence, parquet_path)
  expect_identical(
    tibble::as_tibble(nanoparquet::read_parquet(parquet_path)),
    evidence
  )

  csv_path <- withr::local_tempfile(fileext = ".csv")
  whep::write_table_checked(evidence, csv_path)
  back <- readr::read_csv(
    csv_path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
  expect_identical(back, evidence)
})

test_that("malformed input aborts rather than producing evidence", {
  rows <- evidence_rows()
  expect_error(
    whep::row_evidence(
      "not a table",
      "S",
      "area_code",
      "fao_flag",
      recorded_at = stamp()
    ),
    class = "whep_error_evidence_input"
  )
  expect_error(
    whep::row_evidence(rows, "", c("area_code", "year"), "fao_flag"),
    class = "whep_error_evidence_input"
  )
  expect_error(
    whep::row_evidence(
      rows,
      "S",
      c("area_code", "year"),
      "fao_flag",
      recorded_at = "2026-01-01"
    ),
    class = "whep_error_evidence_input"
  )
  expect_error(
    whep::row_evidence(rows, "S", "no_such_column", "fao_flag"),
    class = "whep_error_evidence_key"
  )
  expect_error(
    whep::row_evidence(rows, "S", "area_code", "fao_flag"),
    class = "whep_error_evidence_key"
  )
  expect_error(
    whep::row_evidence(rows, "S", "fao_flag", "area_code"),
    class = "whep_error_evidence_key"
  )
  expect_error(
    whep::row_evidence(rows, "S", c("area_code", "year"), character()),
    class = "whep_error_evidence_field"
  )
  expect_error(
    whep::row_evidence(rows, "S", c("area_code", "year"), "nope"),
    class = "whep_error_evidence_field"
  )
  expect_error(
    whep::row_evidence(rows, "S", c("area_code", "year"), list("unnamed")),
    class = "whep_error_evidence_field"
  )
  expect_error(
    whep::row_evidence(
      rows,
      "S",
      c("area_code", "year"),
      list(flag = c("a", "b"))
    ),
    class = "whep_error_evidence_field"
  )
  expect_error(
    whep::row_evidence(
      rows,
      "S",
      c("area_code", "year"),
      list(flag = list(1, 2, 3))
    ),
    class = "whep_error_evidence_field"
  )
  expect_error(
    whep::row_evidence(rows, "S", c("area_code", "year"), 42),
    class = "whep_error_evidence_field"
  )
})

test_that("combining is order-independent and collapses duplicates", {
  first <- sample_evidence("A")
  second <- sample_evidence("B")

  combined <- whep::combine_row_evidence(first, second)
  expect_identical(whep::combine_row_evidence(second, first), combined)
  expect_equal(nrow(combined), 6)

  # An identical claim recorded twice is one claim.
  expect_identical(whep::combine_row_evidence(first, first), first)
  expect_identical(whep::combine_row_evidence(list(first, second)), combined)
})

test_that("combining keeps both claims instead of overwriting", {
  rows <- evidence_rows()[1, ]
  key <- c("area_code", "year")
  faostat <- whep::row_evidence(
    rows,
    "FAOSTAT_prod",
    key,
    list(method_land = "reported"),
    recorded_at = stamp()
  )
  luh2 <- whep::row_evidence(
    rows,
    "LUH2",
    key,
    list(method_land = "back-cast"),
    recorded_at = stamp()
  )
  combined <- whep::combine_row_evidence(faostat, luh2)

  expect_equal(nrow(combined), 2)
  expect_setequal(combined$value, c("reported", "back-cast"))
  # Source identity is preserved, not merged away.
  expect_setequal(combined$source_id, c("FAOSTAT_prod", "LUH2"))

  conflicts <- whep::evidence_conflicts(combined)
  expect_equal(nrow(conflicts), 1)
  expect_equal(conflicts$field, "method_land")
  expect_equal(conflicts$n_values, 2)
  expect_equal(conflicts$values, "back-cast | reported")
  expect_equal(conflicts$source_ids, "FAOSTAT_prod, LUH2")
})

test_that("agreement and repetition are not conflicts", {
  agreeing <- whep::combine_row_evidence(
    sample_evidence("A"),
    sample_evidence("B")
  )

  expect_equal(nrow(whep::evidence_conflicts(agreeing)), 0)
  expect_named(
    whep::evidence_conflicts(agreeing),
    c("row_key", "field", "n_values", "values", "source_ids")
  )
})

test_that("a missing value is a claim of its own", {
  rows <- evidence_rows()[1, ]
  key <- c("area_code", "year")
  known <- whep::row_evidence(
    rows,
    "A",
    key,
    list(flag = "E"),
    recorded_at = stamp()
  )
  unknown <- whep::row_evidence(
    rows,
    "B",
    key,
    list(flag = NA_character_),
    recorded_at = stamp()
  )
  conflicts <- whep::evidence_conflicts(
    whep::combine_row_evidence(known, unknown)
  )

  expect_equal(nrow(conflicts), 1)
  expect_equal(conflicts$n_values, 2)
  expect_equal(conflicts$values, "E | NA")
})

test_that("evidence keyed differently cannot be combined", {
  rows <- evidence_rows()
  by_pair <- sample_evidence()
  by_year <- whep::row_evidence(
    dplyr::distinct(rows, year),
    "S",
    "year",
    list(flag = "x"),
    recorded_at = stamp()
  )

  expect_error(
    whep::combine_row_evidence(by_pair, by_year),
    class = "whep_error_evidence_key"
  )
  expect_error(
    whep::combine_row_evidence(),
    class = "whep_error_evidence_input"
  )
  expect_error(
    whep::combine_row_evidence(42),
    class = "whep_error_evidence_input"
  )
})

test_that("combining rejects a table that is not evidence", {
  expect_error(
    whep::combine_row_evidence(tibble::tibble(row_key = 1)),
    class = "whep_error_schema_violation"
  )
  expect_error(
    whep::evidence_conflicts(dplyr::mutate(sample_evidence(), extra = 1)),
    class = "whep_error_schema_violation"
  )
})

test_that("evidence survives a join and a filter unchanged", {
  evidence <- sample_evidence()
  values <- tibble::tibble(
    area_code = c(724L, 724L, 76L),
    year = c(2020L, 2021L, 2020L),
    value = c(1, 2, 3)
  )
  names <- tibble::tibble(area_code = c(724L, 76L), area = c("Spain", "Chile"))
  joined <- dplyr::left_join(values, names, by = "area_code")

  expect_identical(whep::evidence_for(joined, evidence), evidence)
  expect_identical(
    whep::evidence_for(dplyr::filter(joined, area_code == 76L), evidence),
    dplyr::filter(evidence, source_id == "FAOSTAT_prod" & is.na(value))
  )
})

test_that("a fan-out join does not duplicate or re-attribute evidence", {
  evidence <- sample_evidence()
  fanned <- tibble::tibble(
    area_code = c(724L, 724L, 724L),
    year = c(2020L, 2020L, 2021L),
    item = c("a", "b", "a")
  )
  carried <- whep::evidence_for(fanned, evidence)

  expect_equal(nrow(carried), 2)
  expect_setequal(carried$value, c("A", "E"))
})

test_that("rows without evidence warn instead of passing silently", {
  evidence <- sample_evidence()
  extended <- dplyr::bind_rows(
    evidence_rows(),
    tibble::tibble(area_code = 231L, year = 2020L)
  )

  expect_warning(
    carried <- whep::evidence_for(extended, evidence),
    class = "whep_warn_evidence_gap"
  )
  expect_identical(carried, evidence)
})

test_that("an aggregation that drops the key aborts", {
  evidence <- sample_evidence()
  aggregated <- dplyr::distinct(evidence_rows(), area_code)

  expect_error(
    whep::evidence_for(aggregated, evidence),
    class = "whep_error_evidence_key"
  )
  expect_error(
    whep::evidence_for("not a table", evidence),
    class = "whep_error_evidence_input"
  )
})

test_that("empty evidence is carried without complaint", {
  empty <- whep::row_evidence(
    evidence_rows()[0, ],
    "S",
    c("area_code", "year"),
    "fao_flag",
    recorded_at = stamp()
  )

  expect_identical(whep::evidence_for(evidence_rows(), empty), empty)
})

test_that("row evidence composes with the dataset-level provenance", {
  provenance <- whep::record_provenance(
    aliases = "bilateral_trade",
    recorded_at = stamp()
  )
  evidence <- whep::row_evidence(
    evidence_rows(),
    source_id = provenance$input_alias[[1]],
    key = c("area_code", "year"),
    fields = "fao_flag",
    source_version = provenance$input_version[[1]],
    recorded_at = stamp()
  )
  carried <- whep::attach_provenance(evidence, provenance)

  expect_equal(unique(evidence$source_id), "bilateral_trade")
  expect_equal(
    unique(evidence$source_version),
    provenance$input_version[[1]]
  )
  expect_identical(whep::get_provenance(carried), provenance)
  # The scope record composes the same way.
  scope <- whep::footprint_scope("cropland", "ha", "FABIO-MRIO")
  expect_identical(
    whep::get_scope(whep::attach_scope(evidence, scope)),
    scope
  )
})
