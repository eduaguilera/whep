library("testthat")

test_that("get_faostat_data fails for wrong activity_data_param", {
  skip_on_cran()

  get_faostat_data() |>
    expect_error('argument "activity_data" is missing, with no default')

  error <- .bad_activity_data_param_error()

  "bad_param" |>
    get_faostat_data() |>
    expect_error(error)

  c("bad_param") |>
    get_faostat_data() |>
    expect_error(error)

  c("livestock", "bad_param") |>
    get_faostat_data() |>
    expect_error(error)

  # Also fails because function only accepts one activity_data
  c("livestock", "crop_area") |>
    get_faostat_data() |>
    expect_error(error)
})

test_that("get_faostat_data returns correct filtered results", {
  skip_on_cran()

  expect_warning(
    result <- get_faostat_data(
      "livestock",
      year = 2010,
      area = "Portugal",
      whatever = 30
    ),
    "Column whatever not found in FAOSTAT data."
  )

  result |>
    dplyr::distinct(element) |>
    dplyr::pull() |>
    expect_equal("stocks")

  result |>
    dplyr::distinct(year) |>
    dplyr::pull() |>
    expect_equal(2010)

  result |>
    dplyr::distinct(area) |>
    dplyr::pull() |>
    expect_equal("Portugal")
})
