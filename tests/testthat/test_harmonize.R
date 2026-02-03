test_that("Complex harmonization (1 group) is completed successfully", {
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


test_that("Complex harmonization (2 groups) is completed successfully", {
  test_tibble_input <- tibble::tribble(
    ~items,            ~measurement, ~country,  ~`1898`, ~`1899`, ~`1900`, ~`1901`, ~`1902`, ~`1903`, ~`1904`, ~`1905`, ~`1906`,
   "prodonetwo",      "lbs",        "usa",     NA,      NA,      NA,      NA,      NA,      20,      20,      NA,      NA,
   "prodone",         "lbs",        "usa",     NA,      5,       5,       5,       NA,      NA,      6,       6,       6,
   "prodtwo",         "lbs",        "usa",     NA,      5,       NA,      5,       NA,      NA,      4,       4,       4,
   "prodonetwo",      "lbs",        "germany", 1,       NA,      NA,      NA,      NA,      10,      10,      NA,      NA,
   "prodone",         "lbs",        "germany", NA,      NA,      5,       5,       NA,      NA,      6,       6,       6,
   "prodtwo",         "lbs",        "germany", NA,      NA,      5,       5,       NA,      NA,      4,       4,       4,
    "prodthreefour",   "lbs",        "germany", NA,      1,       10,      10,      10,      10,      10,      10,      10,
   "prodthree",       "lbs",        "germany", 3,       NA,      NA,      NA,      NA,      NA,      NA,      NA,      NA,
   "prodfour",        "lbs",        "germany", 7,       NA,      NA,      NA,      NA,      NA,      NA,      NA,      NA,
   "prodfivesix",     "lbs",        "germany", 5,       5,       5,       5,       5,       NA,      NA,      NA,      5,
   "prodfive",        "lbs",        "germany", NA,      NA,      NA,      NA,      NA,      4,       4,       4,       NA,
    "prodsix",         "lbs",        "germany", NA,      NA,      NA,      NA,      NA,      1,       1,       1,       NA,
    "prodseveneight",  "lbs",        "china",   1,       5,       5,       NA,      NA,      5,       5,       5,       5,
   "prodseven",       "lbs",        "china",   3,       NA,      NA,      3,       3,       NA,      NA,      NA,      NA,
    "prodeight",       "lbs",        "china",   2,       NA,      NA,      2,       2,       NA,      NA,      NA,      NA
  ) |>
    tidyr::pivot_longer(
      cols = starts_with("1"),
      names_to = "year",
      values_to = "value"
    ) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::left_join(
      tibble::tribble(
      ~items,            ~item_name_harm, ~item_code_harm, ~type,
      "prodone",         "one",       1,     "Simple",
      "prodtwo",         "two",       2,     "Simple",
      "prodonetwo",      "one",       1,     "1:N",
     "prodonetwo",      "two",       2,     "1:N",
     "prodthreefour",   "three",     3,     "1:N",
     "prodthreefour",   "four",      4,     "1:N",
     "prodthree",       "three",     3,     "Simple",
      "prodfour",        "four",      4,     "Simple",
     "prodfivesix",     "five",      5,     "1:N",
     "prodfivesix",     "six",       6,     "1:N",
     "prodfive",        "five",      5,     "Simple",
     "prodsix",         "six",       6,     "Simple",
     "prodseveneight",  "seven",     7,     "1:N",
     "prodseveneight",  "eight",     8,     "1:N",
     "prodseven",       "seven",     7,     "Simple",
     "prodeight",       "eight",     8,     "Simple",
     "prodnineten",     "nine",      9,     "1:N",
     "prodnineten",     "ten",       10,    "1:N"
      ),
      by = c("items"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(is.na(value) == 0)

  actual <- test_tibble_input |>
    harmonize_interpolate(measurement, country) |>
    dplyr::arrange(year, item_code)

  # Snapshot the harmonized result. On first run the snapshot will be
  # recorded; review and commit the generated snapshot file under
  # `tests/testthat/_snaps/`.
  testthat::expect_snapshot_value(actual, style = "json2")
})

# Test simple harmonization
test_that("Simple harmonization (0 groups) is completed successfully", {
  test_tibble_input <- tibble::tribble(
      ~items,        ~`1899`, ~`1900`, ~`1902`,
      "prodone",     5,       5,       5,
      "prodtwo",     2,       2,       2,
      "prodtwotwo",  3,       3,       3
    ) |>
    tidyr::pivot_longer(
      cols = starts_with("1"),
      names_to = "year",
      values_to = "value"
    ) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::left_join(
      tibble::tribble(
        ~items,        ~item_name_harm, ~item_code_harm, ~type,
        "prodone",     "one",           1,               "Simple",
        "prodtwo",     "two",           2,               "Simple",
        "prodtwotwo",  "two",           2,               "Simple"
      ),
      by = c("items"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(is.na(value) == 0)

  test_tibble_output <- tibble::tribble(
      ~item_code, ~year, ~value,
      1,          1899,  5,
      1,          1900,  5,
      1,          1902,  5,
      2,          1899,  5,
      2,          1900,  5,
      2,          1902,  5
    )

  actual <- test_tibble_input |>
    harmonize_interpolate()

  expect_equal(actual, test_tibble_output)
})


# Test missing group members

test_that("Complex harmonization (1 group, missing member) is failed successfully", {
  test_tibble_input <- tibble::tribble(
        ~items,        ~`1901`, ~`1902`,
        "prodonetwo",  NA,      20,
        "prodone",     5,       NA
    ) |>
    tidyr::pivot_longer(
      cols = starts_with("1"),
      names_to = "year",
      values_to = "value"
    ) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::left_join(
      tibble::tribble(
        ~items,       ~item_name_harm, ~item_code_harm, ~type,
        "prodone",    "one",            1,              "Simple",
        "prodonetwo", "two",            2,              "1:N",
        "prodonetwo", "one",            1,              "1:N"
      ),
      by = c("items"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(is.na(value) == 0)

  testthat::expect_error(
    harmonize_interpolate(harmonize_interpolate(test_tibble_input)),
    "prodonetwo"
  )
})
