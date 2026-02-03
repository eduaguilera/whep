test_that("Harmonization is completed successfully", {
  test_tibble_input <- tibble::tibble(
    items = c("prodone, prodtwo, prodonetwo, prodonetwo, prodone, prodtwo"),
    measurement = "lbs",
    year = c(1900, 1900, 1901, 1901, 1902, 1902),
    value = c(10, 0, 10, 10, 0, 10),
    item_code_harm = c(1, 2, 1, 2, 1, 2),
    item_name_harm = c("one", "two", "one", "two", "one", "two"),
    type = c("Simple", "Simple", "1:N", "1:N", "Simple", "Simple")
  )
  test_tibble_output <- tibble::tibble(
    item_code = c(1, 2, 1, 2, 1, 2),
    year = c(1900, 1900, 1901, 1901, 1902, 1902),
    measurement = "lbs",
    value = c(10, 0, 5, 5, 0, 10)
  )

  actual <- test_tibble_input |>
    harmonize_interpolate(measurement) |>
    dplyr::arrange(year, item_code)

  expect_equal(actual, test_tibble_output)
})
