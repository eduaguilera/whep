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
  # `F248-1920-1991` is RETIRED upstream, superseded by `F248-1920-1947` and
  # `F248-1947-1991`, so it must never be what an area resolves to. The published
  # upstream map names `F248-1947-1991` for area 248 over 1961-1990.
  expect_equal(mapped$polity_code[5], "F248-1947-1991")
  # And NOT `BLX-1921-1999`, which prefix `BLX` also reached: the map names
  # `BLX-1850-1999` for area 15 over 1961-1999.
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
    # period.
    add_polity_code(backcast_anchor = -Inf)

  # Area 2 Afghanistan is a national polity, so it extends to its nearest period.
  expect_equal(mapped$polity_code[1], "AFG-1800-1893")

  # The other three are AGGREGATE reporting buckets -- 15 Belgium-Luxembourg
  # (BLX-1850-1999), 151 Netherlands Antilles (ANT-1961-2010) and 904 Latin
  # America Other (RLAM-1850-2013) are all typed `aggregate` -- and aggregates are
  # deliberately NOT extended past their period, so these rows have no polity. See
  # the test below for why that is the right answer rather than a gap to paper
  # over.
  expect_true(all(is.na(mapped$polity_code[2:4])))
})

test_that("aggregate area 904 loses post-2013 years, and upstream owns that", {
  # This test asserted the opposite until the branch was brought up to date with
  # main: that 904 should be EXTENDED to `RLAM-1850-2013` for 2014-2023 so its
  # FABIO data stopped being dropped. The symptom is real -- area 904 "Latin
  # America Other" carries only `RLAM-1850-2013` in the polities vintage this
  # package embeds, so every post-2013 row resolves to NA -- but the fix does not
  # belong here, on two counts that are both measured rather than argued.
  #
  # 1. It is an upstream data defect and upstream has already fixed it. All seven
  #    reporting buckets now run to 2025: `RLAM-1850-2013` -> `RLAM-1850-2025`,
  #    and `RAFR`/`RASI`/`REUR`/`RNAM`/`ROCE-1850-2021` and `ROW-1850-2023`
  #    likewise. The embedded vintage still has the short spans, which is what
  #    makes this visible; the re-sync tracked in #530 removes the cause. Under
  #    the epic's rule (#458) a territorial validity span is upstream's fact.
  #
  # 2. The downstream workaround was not safely asymmetric. Extending on nearest
  #    distance back-fills years BEFORE an aggregate's start exactly as readily as
  #    after its end, booking an 1830 Guadeloupe figure to `ROW-1850-2023` -- a
  #    bucket that did not exist. That is 64 rows / 1,722,000 t of the historical
  #    trade feed, and `test_build_cbs.R` pins dropping them as deliberate.
  #
  # So this pins the CURRENT honest answer, and it is expected to flip when #530
  # lands: with `RLAM-1850-2025` in place these years resolve through the ordinary
  # period join, with no fallback and no aggregate extension involved.
  mapped <- tibble::tibble(area_code = 904L, year = 2013:2023) |>
    add_polity_code()

  expect_equal(mapped$polity_code[1], "RLAM-1850-2013")
  expect_true(all(is.na(mapped$polity_code[-1])))
})

test_that("add_polity_code reports nearest-period stand-ins as out_of_span", {
  # When no mapped period covers a row's year the row is not dropped: it falls
  # back to the nearest period of the same area. That stand-in attributes the
  # figure to a polity which did not exist in that year, and it used to inherit
  # the crosswalk's own "matched"/"manual" status, so the misattribution was
  # invisible rather than merely uncertain -- the failure mode reported in #387.
  #
  # The stand-ins run in BOTH directions and both are exercised here. Two cases
  # that used to be listed here are gone, because they were prefix-inference
  # artefacts rather than coverage gaps and the upstream FAOSTAT map resolves
  # them: area 206 "Sudan (former)" for 1961-2011 now reaches `SUD-1956-2011`
  # instead of standing in on post-secession `SDN-2011-2025`, and area 181 for
  # 1961-1963 now reaches `SRH-1953-1964` instead of standing in on
  # `ZWE-1964-1980`. Both are asserted as genuine hits below.
  mapped <- tibble::tibble(
    area_code = c(52L, 51L, 2L, 206L, 181L),
    year = c(1970L, 2000L, 1961L, 1970L, 1962L)
  ) |>
    add_polity_code()

  # Backward: FAOSTAT reports area 52 only from 1992, so the map's earliest span
  # for it starts there and a 1970 row can only be a stand-in.
  expect_equal(mapped$polity_code[1], "AZE-1991-2025")
  expect_equal(mapped$mapping_status[1], "out_of_span")
  # Forward: a manually mapped dissolved-state area outliving its own polity.
  expect_equal(mapped$polity_code[2], "F51-1947-1993")
  expect_equal(mapped$mapping_status[2], "out_of_span")
  # A genuine period hit keeps the status the crosswalk assigned it.
  expect_equal(mapped$polity_code[3], "AFG-1919-2025")
  expect_equal(mapped$mapping_status[3], "matched")
  # The two former stand-ins, now real period hits.
  expect_equal(mapped$polity_code[4], "SUD-1956-2011")
  expect_equal(mapped$mapping_status[4], "manual")
  expect_equal(mapped$polity_code[5], "SRH-1953-1964")
  expect_equal(mapped$mapping_status[5], "matched")

  # The flag must be exactly the out-of-span set: no stand-in unflagged, and no
  # real period hit falsely flagged. Asserted as an invariant rather than as a
  # stand-in count, because the count moves whenever the polities snapshot or the
  # upstream map is refreshed while the invariant must not. Restricted to 1961
  # onwards so the back-cast anchor floor is inert and the row's own year is the
  # year that was resolved; pre-anchor rows are deliberately matched to the anchor
  # territory, where the row year lying outside the period is correct, not a
  # stand-in.
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
