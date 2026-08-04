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

test_that("add_polity_code reports nearest-period stand-ins as out_of_span", {
  # When no mapped period covers a row's year the row is not dropped: it falls
  # back to the nearest period of the same area. That stand-in attributes the
  # figure to a polity which did not exist in that year, and it used to inherit
  # the crosswalk's own "matched"/"manual" status, so the misattribution was
  # invisible rather than merely uncertain -- the failure mode reported in #387.
  #
  # Measured over the FAOSTAT era on the shipped crosswalk, 266 areas x
  # 1961:2023: 993 of 16638 resolved area-years are stand-ins, spread over 36
  # areas, and they run in BOTH directions. Backward, FAOSTAT area 206 "Sudan
  # (former)" for 1961-2010 lands on SDN-2011-2025, post-secession Sudan, which
  # by definition excludes the territory those figures cover. Forward, area 51
  # Czechoslovakia for 1994-2023 lands on F51-1947-1993, a state that had already
  # dissolved. Of the 993, 900 previously read "matched" and 93 read "manual";
  # relabelling moved those statuses and 0 of 16758 polity_code values.
  mapped <- tibble::tibble(
    area_code = c(206L, 181L, 51L, 2L),
    year = c(1970L, 1962L, 2000L, 1961L)
  ) |>
    add_polity_code()

  # Backward: the only period mapped to area 206 starts in 2011.
  expect_equal(mapped$polity_code[1], "SDN-2011-2025")
  expect_equal(mapped$mapping_status[1], "out_of_span")
  # Backward inside an otherwise covered chain: area 181 reaches no period for
  # 1953-1964, because that period carries the SRH prefix rather than ZWE.
  expect_equal(mapped$polity_code[2], "ZWE-1964-1980")
  expect_equal(mapped$mapping_status[2], "out_of_span")
  # Forward: a manually mapped dissolved-state area outliving its own polity.
  expect_equal(mapped$polity_code[3], "F51-1947-1993")
  expect_equal(mapped$mapping_status[3], "out_of_span")
  # A genuine period hit keeps the status the crosswalk assigned it.
  expect_equal(mapped$polity_code[4], "AFG-1919-2025")
  expect_equal(mapped$mapping_status[4], "matched")

  # The flag must be exactly the out-of-span set: no stand-in unflagged, and no
  # real period hit falsely flagged. Asserted as an invariant rather than as the
  # 993 count, because the count moves whenever the polities snapshot is
  # refreshed while the invariant must not. Restricted to 1961 onwards so the
  # back-cast anchor floor is inert and the row's own year is the year that was
  # resolved; pre-anchor rows are deliberately matched to the anchor territory,
  # where the row year lying outside the period is correct, not a stand-in.
  crosswalk <- whep::polity_area_crosswalk
  grid <- expand.grid(
    area_code = sort(unique(stats::na.omit(crosswalk$area_code))),
    year = 1961:2023
  )
  resolved <- add_polity_code(grid, "area_code", "year")
  stand_in <- !is.na(resolved$polity_code) &
    ((!is.na(resolved$polity_start_year) &
      resolved$year < resolved$polity_start_year) |
      (!is.na(resolved$polity_end_year) &
        resolved$year > resolved$polity_end_year))
  flagged <- !is.na(resolved$mapping_status) &
    resolved$mapping_status == "out_of_span"

  expect_gt(sum(stand_in), 0L)
  expect_equal(which(flagged), which(stand_in))
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

# ---- ISO3 -> numeric area_code lookup --------------------------------------

testthat::test_that("the iso3c lookup is unique per code", {
  lut <- whep:::.iso3c_area_code_lookup()
  testthat::expect_equal(sum(duplicated(lut$iso3c)), 0L)
  testthat::expect_equal(
    whep:::.iso3c_to_area_code(c("ESP", "DEU", "ETH", "SDN")),
    c(203L, 79L, 238L, 206L)
  )
  testthat::expect_true(is.na(whep:::.iso3c_to_area_code("ZZZ")))
})
