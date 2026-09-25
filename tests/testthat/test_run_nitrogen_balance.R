testthat::test_that("carbon_balance tolerated, livestock_intake not (#1289)", {
  tolerated <- whep:::.nbd_tolerated_stages()
  testthat::expect_true("carbon_balance" %in% tolerated)
  testthat::expect_false("livestock_intake" %in% tolerated)
})

testthat::test_that("a failed livestock_intake blocks the run", {
  report <- tibble::tribble(
    ~input, ~status,
    "cell_polity", "ok",
    "carbon_balance", "FAIL",
    "livestock_intake", "FAIL"
  )
  blockers <- whep:::.nbd_blocking_failures(report)
  testthat::expect_equal(blockers$input, "livestock_intake")
})

testthat::test_that("a failed carbon_balance alone blocks nothing", {
  report <- tibble::tribble(
    ~input, ~status,
    "cell_polity", "ok",
    "carbon_balance", "FAIL",
    "livestock_intake", "ok"
  )
  blockers <- whep:::.nbd_blocking_failures(report)
  testthat::expect_equal(nrow(blockers), 0L)
})

testthat::test_that("a genuine blocker outside the tolerated set still stops", {
  report <- tibble::tribble(
    ~input, ~status,
    "cell_polity", "FAIL",
    "carbon_balance", "ok",
    "livestock_intake", "ok"
  )
  blockers <- whep:::.nbd_blocking_failures(report)
  testthat::expect_equal(blockers$input, "cell_polity")
})

testthat::test_that("an explicit tolerated set overrides the default", {
  report <- tibble::tribble(
    ~input, ~status,
    "livestock_intake", "FAIL"
  )
  blockers <- whep:::.nbd_blocking_failures(
    report,
    tolerated = c("livestock_intake")
  )
  testthat::expect_equal(nrow(blockers), 0L)
})

testthat::test_that("a skipped livestock_intake is reported, not hidden", {
  report <- tibble::tribble(
    ~input, ~status,
    "cell_polity", "ok",
    "carbon_balance", "skip",
    "livestock_intake", "skip"
  )
  gaps <- whep:::.nbd_carried_gaps(report)
  testthat::expect_equal(gaps$input, c("carbon_balance", "livestock_intake"))
  testthat::expect_equal(nrow(whep:::.nbd_blocking_failures(report)), 0L)
  testthat::expect_equal(
    whep:::.nbd_gap_terms(gaps$input),
    c("som_mineralization", "manure and livestock intake")
  )
})

testthat::test_that("a failed livestock_intake is a blocker, not a gap", {
  report <- tibble::tribble(
    ~input, ~status,
    "carbon_balance", "FAIL",
    "livestock_intake", "FAIL"
  )
  testthat::expect_equal(
    whep:::.nbd_carried_gaps(report)$input,
    "carbon_balance"
  )
  testthat::expect_equal(
    whep:::.nbd_gap_terms("carbon_balance"),
    "som_mineralization"
  )
})
