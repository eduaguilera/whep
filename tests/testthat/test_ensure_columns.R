.column_prototype <- function() {
  tibble::tibble(
    id = integer(),
    value = double(),
    active = logical(),
    label = factor(levels = c("low", "high")),
    day = as.Date(character()),
    recorded_at = as.POSIXct(character(), tz = "UTC"),
    details = list()
  )
}

testthat::test_that("ensure_columns completes and safely casts columns", {
  data <- tibble::tibble(
    value = 2L,
    id = c(1, 2),
    extra_b = c("b1", "b2"),
    extra_a = c("a1", "a2")
  )

  result <- whep::ensure_columns(data, .column_prototype())

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), nrow(data))
  testthat::expect_equal(
    names(result),
    c(names(.column_prototype()), "extra_b", "extra_a")
  )
  testthat::expect_type(result$id, "integer")
  testthat::expect_type(result$value, "double")
  testthat::expect_type(result$active, "logical")
  testthat::expect_s3_class(result$label, "factor")
  testthat::expect_s3_class(result$day, "Date")
  testthat::expect_s3_class(result$recorded_at, "POSIXct")
  testthat::expect_type(result$details, "list")
  testthat::expect_true(all(vapply(result$details, is.null, logical(1))))
  testthat::expect_equal(result$extra_b, data$extra_b)
  testthat::expect_equal(result$extra_a, data$extra_a)
})

testthat::test_that("ensure_columns uses typed scalar defaults", {
  data <- tibble::tibble(id = c(1L, 2L), value = c(NA, 4))
  defaults <- list(
    active = TRUE,
    label = "high",
    day = as.Date("2026-07-26"),
    recorded_at = as.POSIXct("2026-07-26 10:00:00", tz = "UTC"),
    details = list(list(source = "fixture"))
  )

  result <- whep::ensure_columns(data, .column_prototype(), defaults)

  testthat::expect_identical(result$active, c(TRUE, TRUE))
  testthat::expect_identical(
    result$label,
    factor(c("high", "high"), levels = c("low", "high"))
  )
  testthat::expect_identical(
    result$day,
    as.Date(c("2026-07-26", "2026-07-26"))
  )
  testthat::expect_identical(
    attr(result$recorded_at, "tzone"),
    "UTC"
  )
  testthat::expect_identical(
    result$details,
    list(list(source = "fixture"), list(source = "fixture"))
  )
  testthat::expect_true(is.na(result$value[[1]]))
})

testthat::test_that("ensure_columns does not overwrite present columns", {
  data <- tibble::tibble(id = 1L, active = NA)

  result <- whep::ensure_columns(
    data,
    .column_prototype(),
    defaults = list(active = TRUE)
  )

  testthat::expect_true(is.na(result$active))
})

testthat::test_that("ensure_columns drops extras on request", {
  data <- tibble::tibble(other = "drop", id = 1L)

  result <- whep::ensure_columns(
    data,
    .column_prototype(),
    extra = "drop"
  )

  testthat::expect_identical(names(result), names(.column_prototype()))
})

testthat::test_that("ensure_columns preserves zero-row prototype types", {
  data <- tibble::tibble(other = character())
  defaults <- list(active = TRUE, label = "low")

  result <- whep::ensure_columns(data, .column_prototype(), defaults)

  testthat::expect_equal(nrow(result), 0L)
  testthat::expect_identical(
    names(result),
    c(names(.column_prototype()), "other")
  )
  testthat::expect_type(result$id, "integer")
  testthat::expect_type(result$value, "double")
  testthat::expect_s3_class(result$label, "factor")
  testthat::expect_identical(levels(result$label), c("low", "high"))
  testthat::expect_s3_class(result$day, "Date")
  testthat::expect_s3_class(result$recorded_at, "POSIXct")
  testthat::expect_identical(attr(result$recorded_at, "tzone"), "UTC")
  testthat::expect_type(result$details, "list")
})

testthat::test_that("ensure_columns casts all-missing logical columns", {
  data <- tibble::tibble(
    id = c(NA, NA),
    label = c(NA, NA)
  )

  result <- whep::ensure_columns(data, .column_prototype())

  testthat::expect_type(result$id, "integer")
  testthat::expect_s3_class(result$label, "factor")
  testthat::expect_identical(levels(result$label), c("low", "high"))
})

testthat::test_that("ensure_columns preserves rich prototype classes", {
  prototype <- tibble::tibble(
    level = ordered(levels = c("low", "high")),
    recorded_at = as.POSIXct(character(), tz = "Europe/Madrid"),
    details = list()
  )
  input_time <- as.POSIXct("2026-07-26 08:00:00", tz = "UTC")
  data <- tibble::tibble(
    level = "high",
    recorded_at = input_time,
    details = list(list(source = "observed"))
  )

  result <- whep::ensure_columns(data, prototype)

  testthat::expect_s3_class(result$level, "ordered")
  testthat::expect_identical(levels(result$level), c("low", "high"))
  testthat::expect_identical(attr(result$recorded_at, "tzone"), "Europe/Madrid")
  testthat::expect_identical(
    as.numeric(result$recorded_at),
    as.numeric(input_time)
  )
  testthat::expect_identical(result$details, data$details)
})

testthat::test_that("ensure_columns rejects lossy existing columns", {
  data <- tibble::tibble(id = 1.5)

  testthat::expect_error(
    whep::ensure_columns(data, .column_prototype()),
    "id",
    class = "whep_error_columns_cast"
  )
})

testthat::test_that("ensure_columns does not guess character NA types", {
  data <- tibble::tibble(id = c(NA_character_, NA_character_))

  testthat::expect_error(
    whep::ensure_columns(data, .column_prototype()),
    "id",
    class = "whep_error_columns_cast"
  )
})

testthat::test_that("ensure_columns rejects incompatible factor levels", {
  data <- tibble::tibble(id = 1L, label = "middle")

  testthat::expect_error(
    whep::ensure_columns(data, .column_prototype()),
    "label",
    class = "whep_error_columns_cast"
  )
})

testthat::test_that("ensure_columns validates data and prototype inputs", {
  prototype <- .column_prototype()
  duplicate <- tibble::as_tibble(
    rlang::set_names(list(1L, 2L), c("id", "id")),
    .name_repair = "minimal"
  )
  unnamed <- tibble::as_tibble(
    rlang::set_names(list(1L), ""),
    .name_repair = "minimal"
  )
  duplicate_prototype <- tibble::as_tibble(
    rlang::set_names(
      list(integer(), integer()),
      c("id", "id")
    ),
    .name_repair = "minimal"
  )

  testthat::expect_error(
    whep::ensure_columns(data.frame(id = 1L), prototype),
    "data.*tibble",
    class = "whep_error_columns_input"
  )
  testthat::expect_error(
    whep::ensure_columns(tibble::tibble(id = 1L), data.frame(id = integer())),
    "prototype.*tibble",
    class = "whep_error_columns_input"
  )
  testthat::expect_error(
    whep::ensure_columns(tibble::tibble(id = 1L), tibble::tibble(id = 1L)),
    "zero rows",
    class = "whep_error_columns_input"
  )
  testthat::expect_error(
    whep::ensure_columns(duplicate, prototype),
    "duplicate",
    class = "whep_error_columns_input"
  )
  testthat::expect_error(
    whep::ensure_columns(unnamed, prototype),
    "non-empty",
    class = "whep_error_columns_input"
  )
  testthat::expect_error(
    whep::ensure_columns(tibble::tibble(id = 1L), duplicate_prototype),
    "duplicate",
    class = "whep_error_columns_input"
  )
  testthat::expect_error(
    whep::ensure_columns(tibble::tibble(id = 1L), prototype, extra = "error"),
    "keep.*drop"
  )
})

testthat::test_that("ensure_columns validates defaults eagerly", {
  data <- tibble::tibble(id = 1L)
  prototype <- .column_prototype()

  testthat::expect_identical(
    whep::ensure_columns(data, prototype, defaults = list()),
    whep::ensure_columns(data, prototype)
  )
  testthat::expect_error(
    whep::ensure_columns(data, prototype, defaults = 1),
    "named list",
    class = "whep_error_columns_default"
  )
  testthat::expect_error(
    whep::ensure_columns(data, prototype, defaults = list(1)),
    "non-empty name",
    class = "whep_error_columns_default"
  )
  testthat::expect_error(
    whep::ensure_columns(
      data,
      prototype,
      defaults = structure(list(1L, 2), names = c("id", ""))
    ),
    "non-empty name",
    class = "whep_error_columns_default"
  )
  testthat::expect_error(
    whep::ensure_columns(
      data,
      prototype,
      defaults = structure(list(1L, 2L), names = c("id", "id"))
    ),
    "duplicate",
    class = "whep_error_columns_default"
  )
  testthat::expect_error(
    whep::ensure_columns(data, prototype, defaults = list(unknown = 1)),
    "unknown",
    class = "whep_error_columns_default"
  )
  testthat::expect_error(
    whep::ensure_columns(data, prototype, defaults = list(value = numeric())),
    "size one",
    class = "whep_error_columns_default"
  )
  testthat::expect_error(
    whep::ensure_columns(data, prototype, defaults = list(value = c(1, 2))),
    "size one",
    class = "whep_error_columns_default"
  )
  testthat::expect_error(
    whep::ensure_columns(data, prototype, defaults = list(id = 1.5)),
    "id",
    class = "whep_error_columns_default"
  )
  testthat::expect_error(
    whep::ensure_columns(data, prototype, defaults = list(value = identity)),
    "vector",
    class = "whep_error_columns_default"
  )
})

testthat::test_that("ensure_columns handles empty schemas", {
  data <- tibble::tibble(extra = c("a", "b"))
  prototype <- tibble::tibble()

  kept <- whep::ensure_columns(data, prototype)
  dropped <- whep::ensure_columns(data, prototype, extra = "drop")

  testthat::expect_identical(kept, data)
  testthat::expect_equal(nrow(dropped), 2L)
  testthat::expect_length(dropped, 0L)
})

testthat::test_that("ensure_columns returns a plain ungrouped tibble", {
  data <- tibble::tibble(id = c(1L, 1L)) |>
    dplyr::group_by(id)

  result <- whep::ensure_columns(data, .column_prototype())

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_false(dplyr::is_grouped_df(result))
})

testthat::test_that("ensure_columns matches the AFS exact-schema contract", {
  prototype <- tibble::tibble(
    source_id = character(),
    year_data = integer(),
    crop_original = character()
  )
  data <- tibble::tibble(
    year_data = 2020L,
    source_id = "test",
    extra = "remove"
  )

  result <- whep::ensure_columns(data, prototype, extra = "drop")

  testthat::expect_identical(names(result), names(prototype))
  testthat::expect_identical(result$source_id, "test")
  testthat::expect_identical(result$year_data, 2020L)
  testthat::expect_identical(result$crop_original, NA_character_)
})

testthat::test_that("ensure_columns matches the China typed-empty contract", {
  prototype <- tibble::tibble(
    Year = integer(),
    Flow = character(),
    Partner = character(),
    Value = double(),
    Source_tier = integer(),
    VLM_page = integer()
  )

  result <- whep::ensure_columns(
    tibble::tibble(),
    prototype,
    extra = "drop"
  )

  testthat::expect_identical(names(result), names(prototype))
  testthat::expect_equal(nrow(result), 0L)
  testthat::expect_type(result$Year, "integer")
  testthat::expect_type(result$Value, "double")
  testthat::expect_type(result$Source_tier, "integer")
  testthat::expect_type(result$Flow, "character")
})

testthat::test_that("ensure_columns matches WHEP zero-default helpers", {
  prototype <- tibble::tibble(
    id = integer(),
    livestock_rum = double(),
    livestock_mono = double(),
    export = double()
  )

  result <- whep::ensure_columns(
    tibble::tibble(id = 1L, note = "keep"),
    prototype,
    defaults = list(
      livestock_rum = 0,
      livestock_mono = 0,
      export = 0
    )
  )

  testthat::expect_identical(
    names(result),
    c(names(prototype), "note")
  )
  testthat::expect_identical(
    result[c("livestock_rum", "livestock_mono", "export")],
    tibble::tibble(
      livestock_rum = 0,
      livestock_mono = 0,
      export = 0
    )
  )
})
