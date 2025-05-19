testthat::test_that("add_area_name correctly sets new column in table", {
  table <- tibble::tibble(area_code = c(1, 2, 4444, 3))

  table |>
    add_area_name() |>
    testthat::expect_equal(
      tibble::tribble(
        ~area_code, ~area_name,
        1, "Armenia",
        2, "Afghanistan",
        4444, NA,
        3, "Albania"
      )
    )

  table |>
    dplyr::rename(dummy_name = area_code) |>
    add_area_name(code_column = "dummy_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~area_name,
        1, "Armenia",
        2, "Afghanistan",
        4444, NA,
        3, "Albania"
      )
    )

  table |>
    dplyr::rename(dummy_name = area_code) |>
    add_area_name(code_column = "dummy_name", name_column = "my_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~my_name,
        1, "Armenia",
        2, "Afghanistan",
        4444, NA,
        3, "Albania"
      )
    )
})

testthat::test_that("add_area_code correctly sets new column in table", {
  table <- tibble::tibble(
    area_name = c("Armenia", "Afghanistan", "Dummy Country", "Albania")
  )

  table |>
    add_area_code() |>
    testthat::expect_equal(
      tibble::tribble(
        ~area_name, ~area_code,
        "Armenia", 1,
        "Afghanistan", 2,
        "Dummy Country", NA,
        "Albania", 3
      )
    )

  table |>
    dplyr::rename(dummy_name = area_name) |>
    add_area_code(name_column = "dummy_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~area_code,
        "Armenia", 1,
        "Afghanistan", 2,
        "Dummy Country", NA,
        "Albania", 3
      )
    )

  table |>
    dplyr::rename(dummy_name = area_name) |>
    add_area_code(name_column = "dummy_name", code_column = "dummy_code") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~dummy_code,
        "Armenia", 1,
        "Afghanistan", 2,
        "Dummy Country", NA,
        "Albania", 3
      )
    )
})
