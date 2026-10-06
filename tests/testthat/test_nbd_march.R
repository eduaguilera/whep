# The gridded balance root written by inst/scripts/run_nitrogen_balance.R under
# WHEP_NBD_MARCH_ROOT (R/nbd_march.R): one parquet partition per year and a
# run manifest, the layout the SJOS-N driver reads (whep#1411). The round trip
# through the driver itself is in test_run_sjos_nitrogen.R.

.nbd_march_balance <- function(year, rows = 3L) {
  tibble::tibble(
    lon = seq_len(rows) * 0.5,
    lat = 0.25,
    area_code = 1L,
    item_cbs_code = 2511L,
    year = as.integer(year),
    n_input_std_t = 10
  )
}

.nbd_march_report <- function() {
  .nbd_march_test_report(list(
    cell_polity = rlang::quo(1),
    n_inputs = rlang::quo({
      message("Reallocated 3 rows (120 t N).")
      warning("a warning")
      2
    })
  ))
}

.nbd_march_write <- function(root, year, rows = 3L, identity = NULL) {
  whep:::.nbd_write_march_year(
    root,
    year,
    .nbd_march_balance(year, rows),
    .nbd_march_report(),
    identity %||% .nbd_march_test_identity()
  )
}

.nbd_march_manifest <- function(root) {
  jsonlite::read_json(
    file.path(root, "whep_n_balance_run_manifest.json"),
    simplifyVector = FALSE
  )
}

testthat::test_that("a year writes its partition and its manifest record", {
  root <- withr::local_tempdir()
  .nbd_march_write(root, 2010L)
  part <- file.path(root, "whep_n_balance_grid", "year=2010", "part.parquet")
  testthat::expect_true(file.exists(part))
  testthat::expect_equal(nrow(arrow::read_parquet(part)), 3L)
  manifest <- .nbd_march_manifest(root)
  testthat::expect_equal(manifest$schema_version, 1L)
  testthat::expect_equal(manifest$whep_commit, strrep("c", 40))
  testthat::expect_equal(manifest$partitions[[1]]$resolution, "grid")
  testthat::expect_equal(manifest$partitions[[1]]$year, 2010L)
  testthat::expect_equal(manifest$partitions[[1]]$rows, 3L)
  testthat::expect_equal(
    manifest$partitions[[1]]$sha256,
    unname(tools::sha256sum(part))
  )
  report <- manifest$driver_report[["2010"]]
  testthat::expect_equal(report$resolution, "grid")
  testthat::expect_equal(
    purrr::map_chr(report$stages, "input"),
    c("cell_polity", "n_inputs")
  )
  n_inputs <- report$stages[[2]]
  testthat::expect_equal(n_inputs$warnings, 1L)
  testthat::expect_equal(n_inputs$messages, 1L)
  testthat::expect_equal(
    purrr::map_chr(n_inputs$conditions, "class"),
    c("message", "warning")
  )
  # An empty record (`{}`), not a missing one: the reader requires the key.
  testthat::expect_equal(
    report$second_resolution_conditions,
    stats::setNames(list(), character())
  )
  testthat::expect_false(file.exists(paste0(part, ".tmp")))
})

testthat::test_that("years accumulate, and a rewritten year replaces itself", {
  root <- withr::local_tempdir()
  .nbd_march_write(root, 2011L)
  .nbd_march_write(root, 2010L)
  .nbd_march_write(root, 2011L, rows = 5L)
  manifest <- .nbd_march_manifest(root)
  testthat::expect_equal(purrr::map_int(manifest$partitions, "year"), 2010:2011)
  testthat::expect_equal(purrr::map_int(manifest$partitions, "rows"), c(3L, 5L))
  testthat::expect_equal(names(manifest$driver_report), c("2010", "2011"))
  testthat::expect_equal(unlist(manifest$years), 2010:2011)
})

testthat::test_that("an interrupted year leaves the finished years intact", {
  root <- withr::local_tempdir()
  .nbd_march_write(root, 2010L)
  before <- readLines(file.path(root, "whep_n_balance_run_manifest.json"))
  testthat::local_mocked_bindings(
    .nbd_march_write_partition = \(...) cli::cli_abort("disk full")
  )
  testthat::expect_error(.nbd_march_write(root, 2011L), "disk full")
  testthat::expect_identical(
    readLines(file.path(root, "whep_n_balance_run_manifest.json")),
    before
  )
})

testthat::test_that("another commit or option set is refused", {
  root <- withr::local_tempdir()
  .nbd_march_write(root, 2010L)
  other_commit <- .nbd_march_test_identity(sha = strrep("d", 40))
  testthat::expect_error(
    .nbd_march_write(root, 2011L, identity = other_commit),
    class = "whep_nbd_march_incompatible"
  )
  other_regime <- .nbd_march_test_identity(regime = "none")
  testthat::expect_error(
    whep:::.nbd_march_has_year(root, 2011L, other_regime),
    class = "whep_nbd_march_incompatible"
  )
  testthat::expect_equal(
    purrr::map_int(.nbd_march_manifest(root)$partitions, "year"),
    2010L
  )
})

testthat::test_that("a finished year is known, a missing partition is not", {
  root <- withr::local_tempdir()
  identity <- .nbd_march_test_identity()
  testthat::expect_false(whep:::.nbd_march_has_year(root, 2010L, identity))
  .nbd_march_write(root, 2010L)
  testthat::expect_true(whep:::.nbd_march_has_year(root, 2010L, identity))
  testthat::expect_false(whep:::.nbd_march_has_year(root, 2011L, identity))
  unlink(file.path(root, "whep_n_balance_grid", "year=2010", "part.parquet"))
  testthat::expect_false(whep:::.nbd_march_has_year(root, 2010L, identity))
})

testthat::test_that("no balance, another year or another resolution aborts", {
  root <- withr::local_tempdir()
  identity <- .nbd_march_test_identity()
  write <- \(balance, identity) {
    whep:::.nbd_write_march_year(
      root,
      2010L,
      balance,
      .nbd_march_report(),
      identity
    )
  }
  testthat::expect_error(
    write(NULL, identity),
    class = "whep_nbd_march_no_balance"
  )
  testthat::expect_error(
    write(.nbd_march_balance(2010L, 0L), identity),
    class = "whep_nbd_march_no_balance"
  )
  testthat::expect_error(
    write(.nbd_march_balance(2011L), identity),
    class = "whep_nbd_march_no_balance"
  )
  polity <- identity
  polity$options$resolution <- "polity"
  testthat::expect_error(
    write(.nbd_march_balance(2010L), polity),
    class = "whep_nbd_march_resolution"
  )
  testthat::expect_length(list.files(root, recursive = TRUE), 0L)
})

testthat::test_that("a run at another resolution is refused before building", {
  testthat::expect_error(
    whep:::.nbd_march_identity(
      list(sha = strrep("c", 40), clean = TRUE),
      list(resolution = "polity")
    ),
    class = "whep_nbd_march_resolution"
  )
})
