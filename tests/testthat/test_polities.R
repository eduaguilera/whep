test_that("add_polity_code maps area codes by year", {
  mapped <- tibble::tibble(
    area_code = c(2L, 4L, 51L, 228L, 248L, 15L, 901L, 999L),
    year = c(1961L, 1850L, 1961L, 1970L, 1980L, 1961L, 2020L, 2020L)
  ) |>
    add_polity_code()

  expect_equal(mapped$polity_code[1], "AFG-1919-2025")
  # area 4 (Algeria) year 1850 is pre-anchor back-cast data -> floored to the
  # 1961 anchor territory (French Algeria), not the 1831-1886 historical period.
  expect_equal(mapped$polity_code[2], "DZA-1919-1962")
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
    # disable the back-cast anchor floor here to exercise the raw out-of-range
    # behaviour: a non-aggregate area falls back to its nearest period, while
    # aggregate reporting areas are NOT extended beyond their range.
    add_polity_code(backcast_anchor = -Inf)

  expect_equal(mapped$polity_code[1], "AFG-1800-1893")
  expect_true(is.na(mapped$polity_code[2]))
  expect_true(is.na(mapped$polity_code[3]))
  expect_true(is.na(mapped$polity_code[4]))
})

test_that("add_polity_code floors pre-1961 back-cast years to the anchor territory", {
  # WHEP's pre-1962 series are back-cast onto ~1961 borders, so a 1900 figure
  # represents 1961 territory and must map to the entity active in 1961, not a
  # larger historical-extent period.
  aut <- tibble::tibble(area_code = 11L, year = c(1900L, 2000L)) |>
    add_polity_code()
  expect_equal(aut$polity_code, c("AUT-1919-2025", "AUT-1919-2025"))

  # disabling the floor exposes the raw year-aware historical period.
  aut_raw <- tibble::tibble(area_code = 11L, year = 1900L) |>
    add_polity_code(backcast_anchor = -Inf)
  expect_equal(aut_raw$polity_code, "AUT-1800-1918")

  # entities that dissolved AFTER 1961 resolve pre-anchor data to the 1961
  # entity (USSR), not a present-day successor.
  ussr <- tibble::tibble(area_code = 228L, year = 1930L) |>
    add_polity_code()
  expect_equal(ussr$polity_code, "F228-1945-1991")
})

test_that("China aggregate area 351 is unmapped so it cannot double-count", {
  # FAOSTAT reports area 351 "China" (mainland + Hong Kong + Macao + Taiwan) for
  # every year ALONGSIDE its components (41, 96, 128, 214), which carry their own
  # polities. Mapping 351 to a polity too summed China twice across every FAOSTAT
  # domain, so 351 must stay unmapped (dropped as a statistical aggregate) while
  # each component keeps its own polity.
  mapped <- tibble::tibble(
    area_code = c(351L, 41L, 96L, 128L, 214L),
    year = 2020L
  ) |>
    add_polity_code()

  expect_true(is.na(mapped$polity_code[mapped$area_code == 351L]))
  expect_equal(mapped$polity_code[mapped$area_code == 41L], "CHN-1950-2025")
  expect_equal(mapped$polity_code[mapped$area_code == 96L], "HKG-1842-2025")
  expect_equal(mapped$polity_code[mapped$area_code == 128L], "MAC-1800-2025")
  expect_equal(mapped$polity_code[mapped$area_code == 214L], "TWN-1945-2025")

  # 351 carries no reporting polity in the crosswalk (its iso3c is also NA).
  cw <- whep::polity_area_crosswalk
  agg <- cw[cw$area_code == 351L, ]
  expect_true(all(is.na(agg$polity_code)))
  expect_true(all(is.na(agg$reporting_polity_code)))
})

test_that("add_reporting_polity_columns re-joins derived polity metadata", {
  out <- tibble::tibble(
    year = c(2015L, 2015L),
    area_code = c(212L, 203L),
    value = c(1, 2)
  ) |>
    whep::add_reporting_polity_columns()

  expect_true(all(
    c(
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry"
    ) %in%
      names(out)
  ))
  syria <- dplyr::filter(out, area_code == 212L)
  expect_equal(syria$polity_area_code, 999L)
  expect_equal(syria$reporting_polity_code, "ROW-1850-2023")
  spain <- dplyr::filter(out, area_code == 203L)
  expect_equal(spain$polity_area_code, 203L)
  expect_equal(spain$reporting_polity_code, "ESP-1800-2025")
})

test_that("add_reporting_polity_columns requires the code column", {
  expect_error(
    whep::add_reporting_polity_columns(tibble::tibble(x = 1)),
    "area_code"
  )
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
