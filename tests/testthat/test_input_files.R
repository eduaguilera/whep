testthat::test_that(".find_cache_dir returns NULL for uncached version", {
  file_info <- .fetch_file_info(
    "commodity_balance_sheet",
    whep::whep_inputs
  )
  result <- .find_cache_dir(
    file_info,
    "commodity_balance_sheet",
    "99999999T999999Z-fake0"
  )

  testthat::expect_null(result)
})

testthat::test_that("whep_read_file produces valid tibble", {
  testthat::expect_message(
    result <- whep_read_file("read_example"),
    "Fetching files"
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(ncol(result) > 0)
})

testthat::test_that("whep_read_file reads both csv and parquet formats", {
  result_csv <- whep_read_file("read_example", type = "csv")
  result_parquet <- whep_read_file("read_example", type = "parquet")

  testthat::expect_s3_class(result_csv, "tbl_df")
  testthat::expect_s3_class(result_parquet, "tbl_df")
  testthat::expect_equal(nrow(result_csv), nrow(result_parquet))
  testthat::expect_equal(ncol(result_csv), ncol(result_parquet))
})

testthat::test_that("whep_read_file errors with invalid file alias", {
  testthat::expect_error(
    whep_read_file("nonexistent_alias_xyz"),
    "There is no file entry"
  )
})

testthat::test_that("whep_read_file errors with invalid file type", {
  testthat::expect_error(
    whep_read_file("read_example", type = "invalid_type"),
    "Unknown file type"
  )
})

testthat::test_that("whep_read_file errors when remote down and no cache", {
  local_mocked_bindings(
    .check_remote_reachable = function(...) {
      cli::cli_abort("Remote host is not reachable.")
    },
    .find_cache_dir = function(...) NULL
  )

  testthat::expect_error(
    whep_read_file("commodity_balance_sheet"),
    "No local cached copy"
  )
})

# .choose_version -----------------------------------------------------------

testthat::test_that(".choose_version returns frozen when user is NULL", {
  result <- .choose_version("20240101T000000Z-abc", NULL)
  testthat::expect_equal(
    result,
    "20240101T000000Z-abc"
  )
})

testthat::test_that(".choose_version returns NULL for 'latest'", {
  result <- .choose_version(
    "20240101T000000Z-abc",
    "latest"
  )
  testthat::expect_null(result)
})

testthat::test_that(".choose_version returns user version when specified", {
  result <- .choose_version(
    "20240101T000000Z-abc",
    "custom-version"
  )
  testthat::expect_equal(result, "custom-version")
})

# .fetch_file_info ----------------------------------------------------------

testthat::test_that(".fetch_file_info returns correct entry", {
  result <- .fetch_file_info(
    "read_example",
    whep::whep_inputs
  )
  testthat::expect_type(result, "list")
  testthat::expect_equal(result$alias, "read_example")
})

testthat::test_that(".fetch_file_info errors on unknown alias", {
  testthat::expect_error(
    .fetch_file_info(
      "nonexistent_xyz",
      whep::whep_inputs
    ),
    "There is no file entry"
  )
})

testthat::test_that(".fetch_file_info errors on duplicate alias", {
  duped_inputs <- dplyr::bind_rows(
    whep::whep_inputs,
    whep::whep_inputs |> dplyr::slice(1)
  )
  alias <- whep::whep_inputs$alias[[1]]

  testthat::expect_error(
    .fetch_file_info(alias, duped_inputs),
    "there should be only one"
  )
})

# whep_list_file_versions ---------------------------------------------------

testthat::test_that("whep_list_file_versions works for local example", {
  result <- whep_list_file_versions("read_example")
  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true(nrow(result) >= 1)
})
