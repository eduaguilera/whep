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

test_that("add_polity_code extends out-of-period rows to their nearest period", {
  mapped <- tibble::tibble(
    area_code = c(2L, 15L, 151L, 904L),
    year = c(1790L, 2000L, 2023L, 2021L)
  ) |>
    # disable the back-cast anchor floor here to exercise the raw out-of-range
    # behaviour: rows whose year no period covers fall back to their nearest
    # period. Aggregate reporting areas (15, 151, 904) are extended too, so
    # their most-recent years are not silently dropped.
    add_polity_code(backcast_anchor = -Inf)

  expect_equal(mapped$polity_code[1], "AFG-1800-1893")
  expect_equal(mapped$polity_code[2], "BLX-1850-1999")
  expect_equal(mapped$polity_code[3], "ANT-1961-2010")
  expect_equal(mapped$polity_code[4], "RLAM-1850-2013")
})

test_that("aggregate area 904 keeps its most-recent years (2014-2023)", {
  # area 904 "Latin America Other" carries only RLAM-1850-2013, so its post-2013
  # FABIO data previously mapped to NA and was dropped by callers. It must now
  # extend to the aggregate polity instead.
  mapped <- tibble::tibble(area_code = 904L, year = 2013:2023) |>
    add_polity_code()

  expect_true(all(mapped$polity_code == "RLAM-1850-2013"))
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

test_that("partner polity mapping emits partner_polity_area_code", {
  # the partner path must canonicalize partners symmetrically with the
  # reporting side (which promotes reporting_polity_area_code -> polity_area_code).
  mapped <- tibble::tibble(
    area_code_partner = c(2L, 41L),
    year = c(2000L, 2020L)
  ) |>
    whep:::.add_partner_polity_columns()

  expect_true("partner_polity_area_code" %in% names(mapped))
  expect_equal(mapped$partner_polity_area_code, c(2L, 41L))
  expect_equal(mapped$partner_polity_code, c("AFG-1919-2025", "CHN-1950-2025"))
})

test_that("current mapping picks the open (latest-ending) period", {
  # the open-period sentinel is derived from the crosswalk's latest end year,
  # not a hardcoded literal, so the current mapping selects the still-open
  # period over Afghanistan's earlier closed periods.
  cw <- whep::polity_area_crosswalk
  afg <- cw[cw$area_code == 2L & !is.na(cw$polity_code), ]
  latest <- afg$polity_code[which.max(afg$polity_end_year)]

  mapped <- tibble::tibble(area_code = 2L) |>
    add_polity_code(year_column = NULL)

  expect_equal(mapped$polity_code, latest)
  expect_equal(mapped$polity_code, "AFG-1919-2025")
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
