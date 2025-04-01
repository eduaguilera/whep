testthat::test_that("get_file_path fails for non-existent file_alias", {
  testthat::local_mocked_bindings(
    .get_input_files_data = function(...) {
      dplyr::tribble(
        ~extension, ~alias, ~drive_file_id,
        "csv", "file_alias_1", "some_id"
      )
    }
  )

  testthat::expect_error(
    get_file_path("file_alias_2"),
    "There is no file entry with alias file_alias_2"
  )
})

testthat::test_that(
  "get_file_path fails for duplicated entries only in filtered rows",
  {
    testthat::local_mocked_bindings(
      .get_input_files_data = function(...) {
        dplyr::tribble(
          ~extension, ~alias, ~drive_file_id,
          "csv", "file_alias_1", "some_id",
          "csv", "file_alias_1", "some_other_id",
          "csv", "file_alias_2", "some_id_2"
        )
      }
    )

    testthat::expect_error(
      get_file_path("file_alias_1"),
      paste0(
        "There are 2 file entries with alias file_alias_1 and there should ",
        "be only one. Double check the content of file ",
        '"inst/extdata/input/raw/input_files.csv"'
      )
    )

    # Don't bother making the call fully work. It fails when trying to download,
    # so it already passed the filter we wanted to test.
    testthat::expect_error(
      get_file_path("file_alias_2"),
      "File was not downloaded correctly. Try again"
    )
  }
)

testthat::test_that(
  "get_file_path correctly first downloads file if necessary",
  {
    file_alias <- "my_file_alias"
    destdir <- .get_destdir()
    destfile <- .get_destfile(destdir, file_alias, "csv")
    testthat::expect_false(file.exists(destfile))

    testthat::local_mocked_bindings(
      .get_input_files_data = function(...) {
        dplyr::tribble(
          ~extension, ~alias, ~drive_file_id,
          "csv", file_alias, "some_id",
        )
      },
      .download_from_drive = function(...) file.create(destfile)
    )

    force_download_msg <-
      "Local file existed, but forcing redownload as requested."

    testthat::expect_no_message(
      get_file_path("my_file_alias"),
      message = force_download_msg
    )
    testthat::expect_message(
      get_file_path("my_file_alias", force_download = TRUE),
      force_download_msg
    )

    testthat::local_mocked_bindings(
      # Make sure .download_from_drive function was not called
      .download_from_drive = function(...) testthat::expect_true(FALSE)
    )
    testthat::expect_message(
      get_file_path("my_file_alias"),
      "Using already existing local file"
    )

    file.remove(destfile)
  }
)
