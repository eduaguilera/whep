testthat::test_that(
  ".find_cache_dir correctly locates cached files",
  {
    file_info <- .fetch_file_info(
      "commodity_balance_sheet", whep::whep_inputs
    )
    version <- .choose_version(file_info$version, NULL)
    cache_dir <- .find_cache_dir(
      file_info, "commodity_balance_sheet", version
    )

    testthat::expect_true(fs::dir_exists(cache_dir))

    files <- list.files(cache_dir)
    testthat::expect_true(
      any(stringr::str_detect(files, "\\.parquet$|\\.csv$"))
    )
  }
)

testthat::test_that(
  ".find_cache_dir returns NULL for uncached version",
  {
    file_info <- .fetch_file_info(
      "commodity_balance_sheet", whep::whep_inputs
    )
    result <- .find_cache_dir(
      file_info, "commodity_balance_sheet", "99999999T999999Z-fake0"
    )

    testthat::expect_null(result)
  }
)

testthat::test_that(
  ".pins_cache_base returns existing directory",
  {
    cache_base <- .pins_cache_base()
    testthat::expect_true(fs::dir_exists(cache_base))
  }
)

testthat::test_that(
  "whep_read_file produces valid tibble",
  {
    testthat::expect_message(
      result <- suppressWarnings(whep_read_file("commodity_balance_sheet")),
      "Fetching files"
    )

    testthat::expect_s3_class(result, "tbl_df")
    testthat::expect_true(nrow(result) > 0)
    testthat::expect_true(ncol(result) > 0)
  }
)

testthat::test_that(
  "whep_read_file reads both csv and parquet formats",
  {
    result_csv <- suppressWarnings(
      whep_read_file("commodity_balance_sheet", type = "csv")
    )
    result_parquet <- suppressWarnings(
      whep_read_file("commodity_balance_sheet", type = "parquet")
    )

    testthat::expect_s3_class(result_csv, "tbl_df")
    testthat::expect_s3_class(result_parquet, "tbl_df")
    testthat::expect_equal(nrow(result_csv), nrow(result_parquet))
    testthat::expect_equal(ncol(result_csv), ncol(result_parquet))
  }
)

testthat::test_that(
  "whep_read_file errors with invalid file alias",
  {
    testthat::expect_error(
      whep_read_file("nonexistent_alias_xyz"),
      "There is no file entry"
    )
  }
)

testthat::test_that(
  "whep_read_file errors with invalid file type",
  {
    testthat::expect_error(
      suppressWarnings(
        whep_read_file("commodity_balance_sheet", type = "invalid_type")
      ),
      "Unknown file type"
    )
  }
)

testthat::test_that(
  "whep_read_file falls back to cache when remote unreachable",
  {
    local_mocked_bindings(
      .check_remote_reachable = function(...) {
        cli::cli_abort("Remote host is not reachable.")
      }
    )

    testthat::expect_warning(
      result <- whep_read_file("commodity_balance_sheet"),
      "Could not reach remote"
    )

    testthat::expect_s3_class(result, "tbl_df")
    testthat::expect_true(nrow(result) > 0)
  }
)

testthat::test_that(
  "whep_read_file errors when remote down and no cache",
  {
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
  }
)
