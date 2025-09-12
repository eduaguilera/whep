testthat::test_that(".fetch_file_info fails for non-existent file_alias", {
  file_inputs <- dplyr::tribble(
    ~alias, ~board_url, ~version,
    "file_alias_1", "some_url", "some_version"
  )

  testthat::expect_error(
    .fetch_file_info("file_alias_2", file_inputs),
    "There is no file entry with alias file_alias_2"
  )
})

testthat::test_that(".fetch_file_info fails for duplicated entries only in filtered rows", {
  file_inputs <- dplyr::tribble(
    ~alias, ~board_url, ~version,
    "file_alias_1", "some_url", "some_version",
    "file_alias_1", "some_other_url", "some_other_version",
    "file_alias_2", "some_url", "some_version_2"
  )

  testthat::expect_error(
    .fetch_file_info("file_alias_1", file_inputs),
    paste0(
      "There are 2 file entries with alias file_alias_1 and there should ",
      "be only one. Double check the content of ",
      "'whep_inputs' dataset."
    )
  )

  # Don't bother making the call fully work. It fails when trying to download,
  # so it already passed the filter we wanted to test.
  testthat::expect_error(whep_read_file("file_alias_2"))
})

testthat::test_that(".read_file reads file with correct extension", {
  testthat::local_mocked_bindings(
    read_csv = function(...) {
      tibble::tibble(a = 1)
    },
    .package = "readr"
  )
  testthat::local_mocked_bindings(
    read_parquet = function(...) {
      tibble::tibble(a = 2)
    },
    .package = "nanoparquet"
  )

  paths <- c("some_file.csv", "some_file.parquet", "some_file.tsv")

  .read_file(paths, "csv") |>
    testthat::expect_equal(tibble::tibble(a = 1))

  .read_file(paths, "parquet") |>
    testthat::expect_equal(tibble::tibble(a = 2))

  .read_file(paths, "txt") |>
    testthat::expect_error(
      "Unknown file type txt. Available for this file: csv, parquet, and tsv"
    )
})

testthat::test_that(".choose_version sets correct version for pins call", {
  paths <- c("some_file.csv", "some_file.parquet", "some_file.tsv")

  .choose_version(frozen_version = "some_version", user_version = NULL) |>
    testthat::expect_equal("some_version")

  .choose_version(frozen_version = "some_version", user_version = "latest") |>
    testthat::expect_equal(NULL)

  .choose_version(
    frozen_version = "some_version",
    user_version = "some_other_version"
  ) |>
    testthat::expect_equal("some_other_version")
})
