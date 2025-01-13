library("testthat")

test_that("trade source data is expanded from year range to single year rows", {
  trade_sources <- tibble::tibble(
    Name = c("a", "b", "c", "d", "e"),
    Trade = c("t1", "t2", "t3", NA, "t5"),
    Info_Format = c("year", "partial_series", "year", "year", "year"),
    Timeline_Start = c(1, 1, 2, 1, 3),
    Timeline_End = c(3, 4, 5, 1, 2),
    Timeline_Freq = c(1, 1, 2, 1, NA),
    `Imp/Exp` = "Imp",
    SACO_link = NA,
  )
  expected <- tibble::tibble(
    Name = c("a_1", "a_2", "a_3", "b", "b", "b", "b", "c_2", "c_4"),
    Trade = c("t1", "t1", "t1", "t2", "t2", "t2", "t2", "t3", "t3"),
    Info_Format = c(
      "year", "year", "year", "partial_series", "partial_series",
      "partial_series", "partial_series", "year", "year"
    ),
    Year = c(1, 2, 3, 1, 2, 3, 4, 2, 4),
  )

  actual <-
    trade_sources |>
    expand_trade_sources() |>
    dplyr::ungroup()

  expect_equal(
    dplyr::select(actual, Name, Trade, Info_Format, Year),
    expected
  )
})
