# .nbd_capture_conditions() and .nbd_stage_row() are the pieces of
# inst/scripts/run_nitrogen_balance.R's nbd_stage() that were split into
# R/nbd_stage.R so they are testable at all: the driver script runs a live
# pipeline top to bottom the moment it is sourced (pins, WHEP_* rasters), so
# nothing in it can be exercised from an offline test suite (whep#1288).
#
# Before this, nbd_stage() wrapped every stage in
# suppressMessages(suppressWarnings(...)) and recorded a bare "ok" with no
# detail, discarding whatever the stage warned or reported -- including the
# nitrogen-balance's own accounts of what it moved or dropped.

test_that("a stage that warns and succeeds returns the value and the warning", {
  result <- .nbd_capture_conditions({
    warning("52 rows moved")
    42
  })
  expect_equal(result$value, 42)
  expect_equal(result$conditions$class, "warning")
  expect_equal(result$conditions$message, "52 rows moved")
})

test_that("capturing raises neither the warning nor the message outward", {
  expect_silent(.nbd_capture_conditions({
    message("informational")
    warning("noisy")
    1
  }))
})

test_that("a stage that messages returns the value and the message", {
  result <- .nbd_capture_conditions({
    message("Reallocated 3 rows (120 t N).")
    "ok"
  })
  expect_equal(result$value, "ok")
  expect_equal(result$conditions$class, "message")
  # message() appends a trailing newline to conditionMessage(); trimmed so a
  # stored/printed report does not carry it.
  expect_equal(result$conditions$message, "Reallocated 3 rows (120 t N).")
})

test_that("a stage with several conditions keeps them in order", {
  result <- .nbd_capture_conditions({
    message("first")
    warning("second")
    message("third")
    99
  })
  expect_equal(result$value, 99)
  expect_equal(result$conditions$class, c("message", "warning", "message"))
  expect_equal(result$conditions$message, c("first", "second", "third"))
})

test_that("a stage with no conditions returns an empty, typed table", {
  result <- .nbd_capture_conditions(7)
  expect_equal(result$value, 7)
  expect_equal(nrow(result$conditions), 0L)
  expect_equal(names(result$conditions), c("class", "message"))
})

test_that("a failing stage returns the error condition, not a raise", {
  # Mirrors what nbd_stage() relies on: tryCatch(expr, error = function(e) e)
  # hands back the condition object instead of propagating it, so the driver
  # can inherits(value, "error") and record a FAIL row rather than aborting.
  # Calling it directly (rather than through expect_no_error()) is itself
  # part of the assertion: an escaping error would fail this test.
  result <- .nbd_capture_conditions(stop("boom"))
  expect_true(inherits(result$value, "error"))
  expect_equal(conditionMessage(result$value), "boom")
})

test_that("a warning raised before the error is still captured", {
  result <- .nbd_capture_conditions({
    warning("stranded rows dropped")
    stop("boom")
  })
  expect_true(inherits(result$value, "error"))
  expect_equal(result$conditions$class, "warning")
  expect_equal(result$conditions$message, "stranded rows dropped")
})

test_that("a stage row carries its captured conditions", {
  captured <- .nbd_capture_conditions({
    warning("Dropping 834 t N")
    1
  })
  row <- .nbd_stage_row(
    "n_inputs",
    "ok",
    1.2,
    10L,
    NA_character_,
    captured$conditions
  )
  expect_equal(row$input, "n_inputs")
  expect_equal(row$conditions[[1]]$class, "warning")
  expect_equal(row$conditions[[1]]$message, "Dropping 834 t N")
})

test_that("a stage row with no conditions still carries an empty table", {
  row <- .nbd_stage_row("cell_polity", "ok", 0.1, 5L, NA_character_)
  expect_equal(nrow(row$conditions[[1]]), 0L)
})

test_that("binding many stage rows keeps one nested table per row", {
  # This is the shape saved under WHEP_NBD_OUT: result$report is
  # dplyr::bind_rows() of these rows, so a caller can read
  # result$report$conditions[[i]] for stage i.
  warned <- .nbd_capture_conditions({
    warning("Dropping 834 t N")
    1
  })
  clean <- .nbd_capture_conditions(2)
  report <- dplyr::bind_rows(
    .nbd_stage_row("n_inputs", "ok", 1, 1L, NA_character_, warned$conditions),
    .nbd_stage_row("cropland_ha", "ok", 1, 1L, NA_character_, clean$conditions)
  )
  expect_equal(nrow(report), 2L)
  expect_equal(report$conditions[[1]]$message, "Dropping 834 t N")
  expect_equal(nrow(report$conditions[[2]]), 0L)
})

test_that("a long detail is still truncated to 1200 characters", {
  # .nbd_stage_row() took over .nbd_record()'s truncation; pin the behaviour
  # so moving it did not silently drop it.
  row <- .nbd_stage_row(
    "n_inputs",
    "FAIL",
    0,
    NA_integer_,
    strrep("x", 2000)
  )
  expect_equal(nchar(row$detail), 1200L)
})

test_that("warnings and messages are counted apart from the capture", {
  result <- .nbd_capture_conditions({
    message("one")
    warning("two")
    warning("three")
    4
  })
  expect_equal(result$warnings, 2L)
  expect_equal(result$messages, 1L)
})

test_that("a warning lost from the capture is still counted (#1411)", {
  # A warning with no message is recorded as zero rows, so it never reaches
  # `conditions`; the count must disagree with the capture, or the balance
  # manifest's reader could not tell a lossy capture from a complete one.
  result <- .nbd_capture_conditions({
    .nbd_unreadable_warning()
    warning("seen")
    1
  })
  expect_equal(sum(result$conditions$class == "warning"), 1L)
  expect_equal(result$warnings, 2L)
})

test_that("a stage row carries its counts, and none is NA for a ran stage", {
  captured <- .nbd_capture_conditions({
    warning("w")
    1
  })
  counted <- .nbd_stage_row(
    "n_inputs",
    "ok",
    1,
    1L,
    NA_character_,
    captured$conditions,
    captured[c("warnings", "messages")]
  )
  expect_equal(counted$warnings, 1L)
  expect_equal(counted$messages, 0L)
  uncounted <- .nbd_stage_row(
    "n_inputs",
    "ok",
    1,
    1L,
    NA_character_,
    captured$conditions
  )
  expect_true(is.na(uncounted$warnings))
  skipped <- .nbd_stage_row("carbon_balance", "skip", 0, NA_integer_, "skip")
  expect_equal(skipped$warnings, 0L)
  expect_equal(skipped$messages, 0L)
})
