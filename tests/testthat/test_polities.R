test_that("add_polity_code maps area codes by year", {
  mapped <- tibble::tibble(
    area_code = c(2L, 4L, 51L, 228L, 248L, 15L, 901L, 999L),
    year = c(1961L, 1850L, 1961L, 1970L, 1980L, 1961L, 2020L, 2020L)
  ) |>
    add_polity_code()

  expect_equal(mapped$polity_code[1], "AFG-1919-2025")
  expect_equal(mapped$polity_code[2], "DZA-1831-1886")
  expect_equal(mapped$polity_code[3], "F51-1947-1993")
  expect_equal(mapped$polity_code[4], "F228-1945-1991")
  expect_equal(mapped$polity_code[5], "F248-1920-1991")
  expect_equal(mapped$polity_code[6], "BLX-1850-1999")
  expect_equal(mapped$polity_code[7], "RAFR-1850-2021")
  expect_equal(mapped$polity_code[8], "ROW-1850-2023")
})

test_that("add_polity_code does not extend aggregate rows outside their range", {
  mapped <- tibble::tibble(
    area_code = c(2L, 15L, 151L, 904L),
    year = c(1790L, 2000L, 2023L, 2021L)
  ) |>
    add_polity_code()

  expect_equal(mapped$polity_code[1], "AFG-1800-1893")
  expect_true(is.na(mapped$polity_code[2]))
  expect_true(is.na(mapped$polity_code[3]))
  expect_true(is.na(mapped$polity_code[4]))
})

test_that("get_polity_geometries returns requested polygon rows", {
  geoms <- get_polity_geometries(c(
    "AFG-1919-2025",
    "NCL-1800-2025",
    "ROW-1850-2023"
  ))

  expect_equal(
    sort(geoms$polity_code),
    c("AFG-1919-2025", "NCL-1800-2025", "ROW-1850-2023")
  )
  expect_true(all(geoms$has_geometry))
})
