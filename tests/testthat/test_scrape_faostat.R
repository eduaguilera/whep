library("testthat")

test_that("get_faostat_data fails for wrong activity_data_param", {
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
  result <- get_faostat_data("livestock", year = 2010, area = "Portugal")

  result[, "element"] |>
    unique() |>
    expect_equal("stocks")

  result[, "year"] |>
    unique() |>
    expect_equal(2010)

  result[, "area"] |>
    unique() |>
    expect_equal("Portugal")
})
