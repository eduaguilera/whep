library("testthat")

test_that("get_faostat_data fails for wrong activity_data_param", {
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
})
