test_that("add_polity_code maps area codes by year", {
  mapped <- tibble::tibble(
    area_code = c(2L, 4L, 51L, 228L, 248L, 15L),
    year = c(1961L, 1850L, 1961L, 1970L, 1980L, 1961L)
  ) |>
    add_polity_code()

  expect_equal(mapped$polity_code[1], "AFG-1919-2025")
  expect_equal(mapped$polity_code[2], "DZA-1831-1886")
  expect_equal(mapped$polity_code[3], "F51-1947-1993")
  expect_equal(mapped$polity_code[4], "F228-1945-1991")
  expect_equal(mapped$polity_code[5], "F248-1920-1991")
  expect_true(is.na(mapped$polity_code[6]))
  expect_equal(mapped$mapping_status[6], "unmapped")
})

test_that("get_polity_geometries returns requested polygon rows", {
  geoms <- get_polity_geometries(c("AFG-1919-2025", "NCL-1800-2025"))

  expect_equal(
    sort(geoms$polity_code),
    c("AFG-1919-2025", "NCL-1800-2025")
  )
  expect_true(all(geoms$has_geometry))
})
