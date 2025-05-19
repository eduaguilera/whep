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

testthat::test_that("add_item_name correctly sets new column in table", {
  table <- tibble::tibble(item_code = c(2559, 2744, 9876))

  table |>
    add_item_name() |>
    testthat::expect_equal(
      tibble::tribble(
        ~item_code, ~item_name,
        2559, "Cottonseed",
        2744, "Eggs",
        9876, NA
      )
    )

  table |>
    dplyr::rename(dummy_code = item_code) |>
    add_item_name(code_column = "dummy_code") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_code, ~item_name,
        2559, "Cottonseed",
        2744, "Eggs",
        9876, NA
      )
    )

  table |>
    dplyr::rename(dummy_code = item_code) |>
    add_item_name(code_column = "dummy_code", name_column = "my_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_code, ~my_name,
        2559, "Cottonseed",
        2744, "Eggs",
        9876, NA
      )
    )
})

testthat::test_that("add_item_code correctly sets new column in table", {
  table <- tibble::tibble(item_name = c("Cottonseed", "Eggs", "Dummy item"))

  table |>
    add_item_code() |>
    testthat::expect_equal(
      tibble::tribble(
        ~item_name, ~item_code,
        "Cottonseed", 2559,
        "Eggs", 2744,
        "Dummy item", NA
      )
    )

  table |>
    dplyr::rename(dummy_name = item_name) |>
    add_item_code(name_column = "dummy_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~item_code,
        "Cottonseed", 2559,
        "Eggs", 2744,
        "Dummy item", NA
      )
    )

  table |>
    dplyr::rename(dummy_name = item_name) |>
    add_item_code(name_column = "dummy_name", code_column = "dummy_code") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~dummy_code,
        "Cottonseed", 2559,
        "Eggs", 2744,
        "Dummy item", NA
      )
    )
})
