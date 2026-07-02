# test_read_raw_inputs.R — tests for R/read_raw_inputs.R helpers

# -- .harmonize_areas ------------------------------------------------------------

.harmonize_areas_fixture <- function(area_codes, values, fao_flag = NULL) {
  dt <- data.table::data.table(
    area_code = area_codes,
    year = 2015L,
    element = "production",
    unit = "tonnes",
    item_prod_code = "15",
    item_prod = "Wheat",
    value = values
  )
  if (!is.null(fao_flag)) {
    dt[, fao_flag := fao_flag]
  }
  dt
}

test_that(".harmonize_areas keeps per-country grain for RoW members", {
  # Syria (212) is not enumerated by FABIO; the base path must keep it as
  # its own row instead of collapsing it into Rest of World (999).
  dt <- .harmonize_areas_fixture(c(203L, 212L), c(5000, 3000))

  result <- whep:::.harmonize_areas(dt, item_prod_code, item_prod) |>
    tibble::as_tibble()

  expect_setequal(result$area_code, c(203L, 212L))
  expect_false(999L %in% result$area_code)
  syria <- dplyr::filter(result, area_code == 212L)
  expect_equal(syria$value, 3000)
  expect_equal(syria$area, "Syrian Arab Republic")
})

test_that(".harmonize_areas drops the redundant China aggregate", {
  # FAOSTAT area 351 "China" duplicates its components (41/96/128/214),
  # which are reported alongside it; keeping both double-counts China.
  dt <- .harmonize_areas_fixture(c(41L, 351L), c(100, 150))

  result <- whep:::.harmonize_areas(dt, item_prod_code, item_prod) |>
    tibble::as_tibble()

  expect_equal(result$area_code, 41L)
  expect_equal(result$value, 100)
})

test_that(".harmonize_areas sums rows recoded to the same keys", {
  dt <- .harmonize_areas_fixture(c(203L, 203L), c(5000, 3000))

  result <- whep:::.harmonize_areas(dt, item_prod_code, item_prod) |>
    tibble::as_tibble()

  expect_equal(nrow(result), 1L)
  expect_equal(result$value, 8000)
})

test_that(".harmonize_areas preserves fao_flag when present", {
  dt <- .harmonize_areas_fixture(
    c(203L, 203L),
    c(5000, 3000),
    fao_flag = c("A", "E")
  )

  result <- whep:::.harmonize_areas(dt, item_prod_code, item_prod)
  expect_true("fao_flag" %in% names(result))
  expect_equal(result$fao_flag, "A")
})

test_that(".harmonize_areas works without fao_flag", {
  dt <- .harmonize_areas_fixture(203L, 5000)

  result <- whep:::.harmonize_areas(dt, item_prod_code, item_prod)
  expect_false("fao_flag" %in% names(result))
  expect_true("value" %in% names(result))
})
