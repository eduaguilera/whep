test_that("Complex harmonization (1 group) is completed successfully", {
  test_tibble_input <- tibble::tribble(
    ~item,          ~measurement, ~year, ~value, ~item_code_harm, ~item_name_harm, ~type,
    "prodone",      "lbs",        1900,  10,     1,               "one",           "simple",
    "prodtwo",      "lbs",        1900,   0,     2,               "two",           "simple",
    "prodonetwo",   "lbs",        1901,  10,     1,               "one",           "1:n",
    "prodonetwo",   "lbs",        1901,  10,     2,               "two",           "1:n",
    "prodone",      "lbs",        1902,   0,     1,               "one",           "simple",
    "prodtwo",      "lbs",        1902,  10,     2,               "two",           "simple"
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
    ~item,             ~measurement, ~country,  ~`1898`, ~`1899`, ~`1900`, ~`1901`, ~`1902`, ~`1903`, ~`1904`, ~`1905`, ~`1906`,
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
        ~item,            ~item_name_harm, ~item_code_harm, ~type,
        "prodone",         "one",       1,     "simple",
        "prodtwo",         "two",       2,     "simple",
        "prodonetwo",      "one",       1,     "1:n",
        "prodonetwo",      "two",       2,     "1:n",
        "prodthreefour",   "three",     3,     "1:n",
        "prodthreefour",   "four",      4,     "1:n",
        "prodthree",       "three",     3,     "simple",
        "prodfour",        "four",      4,     "simple",
        "prodfivesix",     "five",      5,     "1:n",
        "prodfivesix",     "six",       6,     "1:n",
        "prodfive",        "five",      5,     "simple",
        "prodsix",         "six",       6,     "simple",
        "prodseveneight",  "seven",     7,     "1:n",
        "prodseveneight",  "eight",     8,     "1:n",
        "prodseven",       "seven",     7,     "simple",
        "prodeight",       "eight",     8,     "simple",
        "prodnineten",     "nine",      9,     "1:n",
        "prodnineten",     "ten",       10,    "1:n"
      ),
      by = c("item"),
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
    ~item,        ~`1899`, ~`1900`, ~`1902`,
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
        ~item,        ~item_name_harm, ~item_code_harm, ~type,
        "prodone",     "one",           1,               "simple",
        "prodtwo",     "two",           2,               "simple",
        "prodtwotwo",  "two",           2,               "simple"
      ),
      by = c("item"),
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
    ~item,        ~`1901`, ~`1902`,
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
        ~item,       ~item_name_harm, ~item_code_harm, ~type,
        "prodone",    "one",            1,              "simple",
        "prodonetwo", "two",            2,              "1:n",
        "prodonetwo", "one",            1,              "1:n"
      ),
      by = c("item"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(is.na(value) == 0)

  testthat::expect_error(
    harmonize_interpolate(harmonize_interpolate(test_tibble_input)),
    "prodonetwo"
  )
})

# harmonize_simple -------------------------------------------------------------

test_that("harmonize_simple sums N:1 mappings per year", {
  input <- tibble::tribble(
    ~item_code_harm, ~year, ~value, ~type,
    1,               2000,   4,     "simple",
    1,               2000,   6,     "simple",
    2,               2000,   3,     "simple",
    2,               2001,   7,     "simple"
  )

  result <- input |> harmonize_simple()

  result |>
    pointblank::expect_col_vals_not_null(value) |>
    pointblank::expect_col_vals_equal(
      value,
      c(10, 3, 7)
    )
})

test_that("harmonize_simple ignores non-simple rows", {
  input <- tibble::tribble(
    ~item_code_harm, ~year, ~value, ~type,
    1,               2000,  10,     "simple",
    1,               2000,  99,     "1:n",
    2,               2000,   5,     "simple"
  )

  result <- input |> harmonize_simple()

  result |>
    pointblank::expect_col_vals_equal(
      value,
      c(10, 5)
    )
})

test_that("harmonize_simple respects grouping columns", {
  input <- tibble::tribble(
    ~item_code_harm, ~year, ~value, ~type,    ~country,
    1,               2000,   4,     "simple", "usa",
    1,               2000,   6,     "simple", "usa",
    1,               2000,   9,     "simple", "germany"
  )

  result <- input |>
    harmonize_simple(country) |>
    dplyr::arrange(country)

  result |>
    pointblank::expect_col_exists(
      columns = c("item_code_harm", "year", "value", "country")
    ) |>
    pointblank::expect_col_vals_equal(
      value,
      c(9, 10)
    )
})

test_that("harmonize_simple handles NA values via na.rm", {
  input <- tibble::tribble(
    ~item_code_harm, ~year, ~value, ~type,
    1,               2000,   5,     "simple",
    1,               2000,  NA,     "simple",
    2,               2000,   3,     "simple"
  )

  result <- input |> harmonize_simple()

  result |>
    pointblank::expect_col_vals_equal(
      value,
      c(5, 3)
    )
})

# harmonize_interpolate share interpolation ------------------------------------

test_that("harmonize_interpolate interpolates shares linearly", {
  # Year 2000: shares are 60/40. Year 2002: shares are 80/20.
  # Year 2001 should interpolate to 70/30.
  input <- tibble::tribble(
    ~item,       ~item_code_harm, ~year, ~value, ~type,
    "wheat",     1,               2000,   6,     "simple",
    "rice",      2,               2000,   4,     "simple",
    "wheatrice", 1,               2001,  10,     "1:n",
    "wheatrice", 2,               2001,  10,     "1:n",
    "wheat",     1,               2002,   8,     "simple",
    "rice",      2,               2002,   2,     "simple"
  )

  result <- input |>
    harmonize_interpolate() |>
    dplyr::filter(year == 2001) |>
    dplyr::arrange(item_code)

  result |>
    dplyr::pull(value) |>
    expect_equal(c(7, 3))
})

test_that("harmonize_interpolate preserves simple values unchanged", {
  input <- tibble::tribble(
    ~item,       ~item_code_harm, ~year, ~value, ~type,
    "wheat",     1,               2000,   6,     "simple",
    "rice",      2,               2000,   4,     "simple",
    "wheatrice", 1,               2001,  10,     "1:n",
    "wheatrice", 2,               2001,  10,     "1:n",
    "wheat",     1,               2002,   8,     "simple",
    "rice",      2,               2002,   2,     "simple"
  )

  result <- input |>
    harmonize_interpolate() |>
    dplyr::filter(year != 2001) |>
    dplyr::arrange(year, item_code)

  result |>
    dplyr::pull(value) |>
    expect_equal(c(6, 4, 8, 2))
})
