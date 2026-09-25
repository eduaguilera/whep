test_that("a pre-anchor row whose label and present-day polity differ drifts", {
  out <- whep::polity_anchor_drift(
    tibble::tibble(area_code = 238L, year = 1850L, value = 1)
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$anchor_polity_code, "ETH-1952-1993")
  expect_equal(out$reference_polity_code, "ETH-1993-2025")
  expect_equal(out$drift_kind, "interval")
  expect_equal(out$n_rows, 1L)
})

test_that("the data-year polity is reported unfloored beside the anchor", {
  out <- whep::polity_anchor_drift(
    tibble::tibble(area_code = 238L, year = 1850L)
  )

  # `land_method = "historical_polity"` measures the hectares inside this
  # polity, which is a third territorial reference and neither of the two the
  # drift is between.
  expect_equal(out$data_year_polity_code, "ETH-1800-1889")
  expect_false(out$data_year_polity_code == out$anchor_polity_code)
})

test_that("an area whose territory never changed does not drift", {
  out <- whep::polity_anchor_drift(
    tibble::tibble(area_code = 11L, year = c(1850L, 1900L))
  )

  expect_equal(nrow(out), 0L)
  expect_true(tibble::is_tibble(out))
  expect_named(
    out,
    c(
      "area_code",
      "year",
      "anchor_polity_code",
      "anchor_polity_name",
      "data_year_polity_code",
      "reference_polity_code",
      "reference_polity_name",
      "drift_kind",
      "n_rows"
    )
  )
})

test_that("rows at or after the anchor are not reported", {
  out <- whep::polity_anchor_drift(
    tibble::tibble(area_code = 238L, year = c(1961L, 1970L, 2000L))
  )

  expect_equal(nrow(out), 0L)
})

test_that("a different entity is told apart from a different vintage", {
  out <- whep::polity_anchor_drift(
    tibble::tibble(area_code = c(181L, 238L), year = 1850L)
  )

  kinds <- out |>
    dplyr::select("area_code", "drift_kind") |>
    dplyr::arrange(.data$area_code)
  expect_equal(kinds$area_code, c(181L, 238L))
  expect_equal(kinds$drift_kind, c("entity", "interval"))
})

test_that("an area with no present-day polity is named, not dropped", {
  out <- whep::polity_anchor_drift(
    tibble::tibble(area_code = 15L, year = 1850L)
  )

  expect_equal(out$drift_kind, "unmapped_reference")
  expect_true(is.na(out$reference_polity_code))
  expect_equal(out$anchor_polity_code, "BLX-1850-1999")
})

test_that("rows are counted per (area_code, year) pair", {
  out <- whep::polity_anchor_drift(
    tibble::tibble(
      area_code = c(238L, 238L, 238L),
      year = c(1850L, 1850L, 1851L)
    )
  )

  expect_equal(nrow(out), 2L)
  expect_equal(out$n_rows, c(2L, 1L))
})

test_that("a missing year column aborts rather than reporting no drift", {
  expect_error(
    whep::polity_anchor_drift(tibble::tibble(area_code = 238L)),
    class = "whep_anchor_drift_no_year"
  )
  expect_error(
    whep::polity_anchor_drift(tibble::tibble(year = 1850L)),
    class = "whep_anchor_drift_no_code"
  )
})

test_that("the column names are honoured", {
  out <- whep::polity_anchor_drift(
    tibble::tibble(polity_area_code = 238L, reference_period = 1850L),
    code_column = "polity_area_code",
    year_column = "reference_period"
  )

  expect_equal(out$area_code, 238L)
  expect_equal(out$year, 1850L)
})

test_that("the anchor and the reference year are both selectable", {
  # With the anchor moved past the 1993 succession, area 238's label is the
  # modern republic and agrees with the reference.
  expect_equal(
    nrow(whep::polity_anchor_drift(
      tibble::tibble(area_code = 238L, year = 1850L),
      backcast_anchor = 2000L
    )),
    0L
  )
  # And with the reference moved back before it, the label agrees again.
  expect_equal(
    nrow(whep::polity_anchor_drift(
      tibble::tibble(area_code = 238L, year = 1850L),
      reference_year = 1970L
    )),
    0L
  )
})

test_that("the crosswalk's own back-cast grid drifts for 49 areas", {
  # The population whep#748 is about, measured on package data alone: an area
  # drifts when the polity it is labelled with at the anchor is not the polity
  # its present-day ISO3 resolves to, which is what the `luh2-areas` growth
  # proxy is keyed on.
  grid <- tidyr::expand_grid(
    area_code = sort(unique(as.integer(whep::polity_area_crosswalk$area_code))),
    year = 1850:1960
  )
  out <- whep::polity_anchor_drift(grid)

  expect_equal(dplyr::n_distinct(out$area_code), 49L)
  expect_setequal(
    unique(out$drift_kind),
    c("entity", "interval", "unmapped_reference")
  )
  # Same entity, different vintage is the larger and quieter class.
  by_kind <- out |>
    dplyr::summarise(
      areas = dplyr::n_distinct(.data$area_code),
      .by = "drift_kind"
    )
  expect_equal(by_kind$areas[by_kind$drift_kind == "entity"], 7L)
  expect_equal(by_kind$areas[by_kind$drift_kind == "interval"], 40L)
  expect_equal(by_kind$areas[by_kind$drift_kind == "unmapped_reference"], 2L)
})

test_that("the answer does not depend on which present-day year is asked", {
  grid <- tidyr::expand_grid(
    area_code = sort(unique(as.integer(whep::polity_area_crosswalk$area_code))),
    year = 1850:1960
  )
  base <- whep::polity_anchor_drift(grid, reference_year = 2023L)

  purrr::walk(
    c(2014L, 2018L, 2024L),
    \(yr) {
      expect_equal(whep::polity_anchor_drift(grid, reference_year = yr), base)
    }
  )
})
