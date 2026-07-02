# test_fabio_regions.R — tests for whep::collapse_to_fabio_regions()

.fabio_collapse_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~area, ~item_cbs_code, ~value,
    # Spain is enumerated by FABIO and maps to itself.
    2015L, 203L, "Spain", 2511L, 100,
    # Syria and Equatorial Guinea are not enumerated and fold into RoW.
    2015L, 212L, "Syrian Arab Republic", 2511L, 40,
    2015L, 61L, "Equatorial Guinea", 2511L, 5,
    # Sudan and South Sudan combine into the former Sudan region.
    2015L, 276L, "Sudan", 2511L, 30,
    2015L, 277L, "South Sudan", 2511L, 20
  )
}

test_that("collapse_to_fabio_regions maps countries to FABIO regions", {
  result <- whep::collapse_to_fabio_regions(.fabio_collapse_fixture())

  expect_setequal(result$area_code, c(203L, 999L, 206L))
  expect_equal(
    dplyr::pull(dplyr::filter(result, area_code == 999L), value),
    45
  )
  expect_equal(
    dplyr::pull(dplyr::filter(result, area_code == 206L), value),
    50
  )
  expect_equal(
    dplyr::pull(dplyr::filter(result, area_code == 999L), area),
    "RoW"
  )
  expect_equal(
    dplyr::pull(dplyr::filter(result, area_code == 206L), area),
    "Sudan (former)"
  )
})

test_that("collapse_to_fabio_regions preserves the total value", {
  input <- .fabio_collapse_fixture()
  result <- whep::collapse_to_fabio_regions(input)

  expect_equal(sum(result$value), sum(input$value))
})

test_that("collapse_to_fabio_regions is idempotent", {
  once <- whep::collapse_to_fabio_regions(.fabio_collapse_fixture())
  twice <- whep::collapse_to_fabio_regions(once)

  expect_equal(
    dplyr::arrange(twice, area_code),
    dplyr::arrange(once, area_code)
  )
})

test_that("collapse_to_fabio_regions drops unmapped aggregates with warning", {
  input <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~value,
    2015L, 41L, 2511L, 100,
    # FAOSTAT area 351 "China" is a redundant aggregate with no polity.
    2015L, 351L, 2511L, 150
  )

  expect_warning(
    result <- whep::collapse_to_fabio_regions(input),
    "without a FABIO region mapping"
  )
  expect_equal(result$area_code, 41L)
  expect_equal(result$value, 100)
})

test_that("collapse_to_fabio_regions supports custom value columns", {
  input <- tibble::tribble(
    ~year, ~area_code, ~land_use, ~area_mha,
    2015L, 212L, "cropland", 1.5,
    2015L, 61L, "cropland", 0.5
  )

  result <- whep::collapse_to_fabio_regions(input, value_columns = "area_mha")
  expect_equal(result$area_code, 999L)
  expect_equal(result$area_mha, 2)
})

test_that("collapse_to_fabio_regions keeps first fao_flag per group", {
  input <- tibble::tribble(
    ~year, ~area_code, ~value, ~fao_flag,
    2015L, 212L, 40, "A",
    2015L, 61L, 5, "E"
  )

  result <- whep::collapse_to_fabio_regions(input)
  expect_equal(result$fao_flag, "A")
})

test_that("collapse_to_fabio_regions validates its input", {
  expect_error(
    whep::collapse_to_fabio_regions(tibble::tibble(year = 2015L, value = 1)),
    "area_code"
  )
  expect_error(
    whep::collapse_to_fabio_regions(tibble::tibble(area_code = 203L, x = 1)),
    "value"
  )
})
