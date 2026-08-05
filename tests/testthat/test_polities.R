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
  # `BLX-1850-1999` for area 15 over 1961-1999. Upstream has since retired
  # `BLX-1921-1999` outright (whep-polities#117), so the dead-polity filter now
  # removes it as well -- two independent reasons for the same answer.
  expect_equal(mapped$polity_code[6], "BLX-1850-1999")
  # The regional "Other" buckets and Rest of World were extended to 2025
  # upstream (whep-polities#127) because they used to stop before FAOSTAT did.
  expect_equal(mapped$polity_code[7], "RAFR-1850-2025")
  expect_equal(mapped$polity_code[8], "ROW-1850-2025")
})

test_that("add_polity_code extends out-of-period rows to their nearest period", {
  # The aggregate years are DERIVED from the crosswalk rather than written in, so
  # this test says "one year past whatever period this vintage declares" instead
  # of naming a literal. Areas 15, 151 and 904 all had their bucket end year moved
  # by an upstream re-sync while this branch was open (#530 extends RLAM-1850-2013
  # to RLAM-1850-2025), and a hardcoded year silently starts asking the opposite
  # question when that happens -- 2021 is outside the old bucket and inside the
  # new one.
  aggregate_areas <- c(15L, 151L, 904L)
  cw <- as.data.frame(whep::polity_area_crosswalk)
  past_period_end <- vapply(
    aggregate_areas,
    function(area) max(cw$polity_end_year[which(cw$area_code == area)]) + 1L,
    integer(1L)
  )

  mapped <- tibble::tibble(
    area_code = c(2L, aggregate_areas),
    # main's version of this hunk hardcoded `2026`, with a comment saying "this
    # year has to track the bucket's end". Deriving it does exactly that, so the
    # derived form is kept and the literal dropped -- same intent, one fewer thing
    # to remember on the next re-sync.
    year = c(1790L, past_period_end)
  ) |>
    # disable the back-cast anchor floor here to exercise the raw out-of-range
    # behaviour: rows whose year no period covers fall back to their nearest
    # period.
    add_polity_code(backcast_anchor = -Inf)

  # Area 2 Afghanistan is a national polity, so it extends to its nearest period.
  expect_equal(mapped$polity_code[1], "AFG-1800-1893")

  # The other three are AGGREGATE reporting buckets -- 15 Belgium-Luxembourg,
  # 151 Netherlands Antilles and 904 Latin America Other are all typed
  # `aggregate` -- and aggregates are deliberately NOT extended past their period,
  # so these rows have no polity. See the test below for why that is the right
  # answer rather than a gap to paper over.
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
  # So this asserts the RULE rather than the vintage: inside the bucket's declared
  # period area 904 resolves to it, and one year past the end it resolves to
  # nothing. Both years are read off the crosswalk, so #530 extending
  # `RLAM-1850-2013` to `RLAM-1850-2025` moves which calendar years are tested
  # without changing what is being tested -- and after it lands the years that
  # used to be NA resolve through the ordinary period join, which is the point.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  rlam <- cw[which(cw$area_code == 904L), ]
  bucket_end <- max(rlam$polity_end_year)
  bucket_code <- rlam$polity_code[which.max(rlam$polity_end_year)]

  mapped <- tibble::tibble(
    area_code = 904L,
    year = c(bucket_end - 1L, bucket_end + 1L)
  ) |>
    add_polity_code()

  expect_equal(mapped$polity_code[1], bucket_code)
  expect_true(is.na(mapped$polity_code[2]))
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
  #
  # `polity_end_year` is EXCLUSIVE, so a period covers
  # `polity_start_year:(polity_end_year - 1)` and a row AT `polity_end_year` is
  # already a stand-in -- unless the upstream map declares that reported year
  # for the pair, which is the one thing allowed to reach past the territorial
  # span (#550).
  crosswalk <- whep::polity_area_crosswalk
  grid <- expand.grid(
    area_code = sort(unique(stats::na.omit(crosswalk$area_code))),
    year = 1961:2023
  )
  resolved <- add_polity_code(grid, "area_code", "year") |>
    dplyr::left_join(
      crosswalk |>
        dplyr::distinct(
          area_code = .data$area_code,
          polity_code = .data$polity_code,
          map_year_end = .data$map_year_end
        ),
      by = c("area_code", "polity_code")
    )
  covered_to <- pmax(
    resolved$polity_end_year - 1L,
    dplyr::coalesce(resolved$map_year_end, -Inf)
  )
  stand_in <- !is.na(resolved$polity_code) &
    ((!is.na(resolved$polity_start_year) &
      resolved$year < resolved$polity_start_year) |
      (!is.na(resolved$polity_end_year) & resolved$year > covered_to))
  flagged <- !is.na(resolved$mapping_status) &
    resolved$mapping_status == "out_of_span"

  expect_gt(sum(stand_in), 0L)
  expect_equal(which(flagged), which(stand_in))
})

test_that("a period does not answer for its exclusive end year", {
  # `polity_end_year` is exclusive everywhere else in the package -- the
  # crosswalk build, `.area_year_polity_conflicts()`, `resolve_polity_label()`
  # -- but the resolver used to join on `>= year`, so a period answered for one
  # year past its end and the row still read "matched"/"manual" (#550). Three
  # FAOSTAT areas landed that way in a state that had already dissolved.
  dissolved <- tibble::tibble(
    area_code = c(51L, 186L, 248L),
    year = c(1993L, 2006L, 1992L)
  ) |>
    add_polity_code()

  expect_equal(dissolved$year, dissolved$polity_end_year)
  expect_equal(
    dissolved$mapping_status,
    rep("out_of_span", 3L)
  )

  # The successor owns the hand-over year: 1993 is Czechia's and Slovakia's,
  # not Czechoslovakia's.
  successors <- tibble::tibble(
    area_code = c(167L, 199L),
    year = 1993L
  ) |>
    add_polity_code()
  expect_equal(
    successors$polity_code,
    c("CZE-1993-2025", "SVK-1993-2025")
  )
  expect_equal(successors$mapping_status, c("matched", "matched"))
})

test_that("a reported year past a polity's end is kept, not dropped", {
  # The upstream map is the authority on which years an area REPORTS under a
  # period, and its `map_year_end` is inclusive. Four areas report a final year
  # equal to their polity's exclusive `polity_end_year`, so the exclusive join
  # alone would have cost them that year -- two of them (15, 151) to `NA`,
  # because the nearest-period fallback deliberately skips aggregates.
  reported <- tibble::tibble(
    area_code = c(15L, 151L, 206L, 228L),
    year = c(1999L, 2010L, 2011L, 1991L)
  ) |>
    add_polity_code()

  expect_equal(reported$year, reported$polity_end_year)
  expect_equal(
    reported$polity_code,
    c("BLX-1850-1999", "ANT-1961-2010", "SUD-1956-2011", "F228-1945-1991")
  )
  expect_false(any(reported$mapping_status == "out_of_span"))
})

test_that(".polity_join_end_year widens only to a later reported year", {
  # NA `polity_end_year` is an open period; NA `map_year_end` is a row the
  # upstream map does not cover, which must not drag the bound down.
  expect_equal(
    whep:::.polity_join_end_year(
      c(1993L, 1999L, 2025L, NA, 1993L),
      c(1992L, 1999L, 2023L, 2000L, NA)
    ),
    c(1993, 2000, 2025, Inf, 1993)
  )
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
    "ROW-1850-2025"
  ))

  expect_equal(
    sort(geoms$polity_code),
    c("AFG-1919-2025", "NCL-1800-2025", "ROW-1850-2025")
  )
  expect_true(all(geoms$has_geometry))
})

# ---- Dissolved-federation successor closure ---------------------------------

test_that(".successor_iso3_map resolves the dissolved federations", {
  # LUH2 land use is keyed on present-day ISO3, so a federation that no longer
  # exists is unreachable by ISO3 alone (whep#408). The polities `successor`
  # relation recovers it, but only transitively: the Yugoslav SFR reaches Serbia
  # and Montenegro through the 1992-2006 Serbia-and-Montenegro union.
  available <- c(
    "ARM",
    "AZE",
    "BLR",
    "EST",
    "GEO",
    "KAZ",
    "KGZ",
    "LTU",
    "LVA",
    "MDA",
    "RUS",
    "TJK",
    "TKM",
    "UKR",
    "UZB",
    "CZE",
    "SVK",
    "BIH",
    "HRV",
    "MKD",
    "MNE",
    "SRB",
    "SVN",
    "BEL",
    "LUX",
    "ESP"
  )
  res <- whep:::.successor_iso3_map(
    c("F228-1945-1991", "F51-1947-1993", "F248-1991-1992"),
    available
  )

  expect_equal(res[["F51-1947-1993"]], c("CZE", "SVK"))
  expect_equal(
    res[["F248-1991-1992"]],
    c("BIH", "HRV", "MKD", "MNE", "SRB", "SVN")
  )
  expect_equal(length(res[["F228-1945-1991"]]), 15L)
  expect_true(all(c("RUS", "UKR", "UZB") %in% res[["F228-1945-1991"]]))
})

test_that(".successor_iso3_map stops at the first reachable ISO3", {
  # RUS-1991-2014 is itself succeeded by RUS-2014-2025; a branch must not be
  # expanded once it has landed inside the caller's vocabulary, or a later
  # boundary change would silently widen the union.
  res <- whep:::.successor_iso3_map("F51-1947-1993", c("CZE", "SVK", "ESP"))
  expect_equal(res[["F51-1947-1993"]], c("CZE", "SVK"))

  # A polity already in the vocabulary resolves to itself, not to its successor.
  self <- whep:::.successor_iso3_map("BEL-1831-2025", c("BEL", "LUX"))
  expect_equal(self[["BEL-1831-2025"]], "BEL")
})

test_that(".successor_iso3_map returns nothing when no successor is published", {
  # Belgium-Luxembourg carries no `successor` upstream, so it stays unreachable
  # and the caller must keep warning rather than invent an attribution.
  res <- whep:::.successor_iso3_map("BLX-1850-1999", c("BEL", "LUX"))
  expect_equal(res[["BLX-1850-1999"]], character(0))
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

testthat::test_that("the iso3c lookup is many-to-one, deliberately", {
  # Unique per iso3c (above) says nothing about the other direction, and the
  # other direction is where the aggregation lives: `polity_area_code` is a
  # bucket, so 257 ISO3 codes share 195 codes. Anything reading a population or
  # per-capita row as one country depends on knowing that (#482), so the fold is
  # pinned here: if upstream changes which ISO3 codes land on 999, this fails and
  # the numbers on the 999 denominator have to be re-checked.
  lut <- whep:::.iso3c_area_code_lookup()
  testthat::expect_equal(nrow(lut), 257L)
  testthat::expect_equal(dplyr::n_distinct(lut$area_code), 195L)
  testthat::expect_equal(sum(duplicated(lut$area_code)), 62L)

  row <- sort(lut$iso3c[lut$area_code == 999L])
  testthat::expect_equal(length(row), 62L)
  # The members that are present-day sovereign states, not small territories --
  # the ones whose population a reader would look for as its own row.
  testthat::expect_true(
    all(c("SYR", "MKD", "PSE", "SWZ", "GNQ", "AND", "LIE", "MCO") %in% row)
  )
  testthat::expect_equal(
    whep:::.iso3c_to_area_code(c("SYR", "MKD", "PSE", "SWZ", "GNQ", "GUF")),
    rep(999L, 6)
  )
  # 206 "Sudan (former)" is the same shape at a smaller scale: post-secession
  # Sudan and South Sudan both resolve to the pre-2011 bucket.
  testthat::expect_equal(whep:::.iso3c_to_area_code("SSD"), 206L)
  testthat::expect_equal(sum(lut$area_code == 206L), 2L)
})
