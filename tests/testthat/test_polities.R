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
  # An OPEN period covers its own `polity_end_year`, which is the third arm of
  # `.polity_join_end_year()` and the one this model used to omit. It had no
  # effect while every area it applies to was folded into Rest of World;
  # promoting them (whep#717) exposed it on FAOSTAT area 187 Saint Helena, whose
  # `SHN-1834-1967` upstream records no successor for, so the resolver reads
  # 1967 as a period hit while a strictly exclusive end would call it a stand-in.
  # Read from `.open_polity_codes()` rather than re-derived, because openness is
  # upstream's `successor`/`predecessor` record and a second reading of it here
  # would be a second authority, not a check.
  covered_to <- pmax(
    resolved$polity_end_year -
      1L +
      (resolved$polity_code %in%
        whep:::.open_polity_codes()),
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
      c(1992L, 1999L, 2023L, 2000L, NA),
      is_open = FALSE
    ),
    c(1993, 2000, 2025, Inf, 1993)
  )
})

test_that(".polity_join_start_year narrows only to a later declared year", {
  # The mirror of the end bound, and deliberately NOT symmetric with it: the end
  # bound WIDENS to a reported year past the territorial span, while this one
  # NARROWS to a year the row itself declares. `NA` is the normal case -- an
  # area row is bounded by the polity's own start -- and an `applies_from_year`
  # earlier than the polity's start can never move the bound.
  expect_equal(
    whep:::.polity_join_start_year(
      c(1956L, 2011L, 2011L, NA, 2011L),
      c(NA, 2012L, NA, 1900L, 1990L)
    ),
    c(1956, 2012, 2011, 1900, 2011)
  )
})

test_that("a still-open period covers its terminal year, a succeeded one does not", {
  # EXCLUSIVE AT A SUCCESSION, INCLUSIVE AT THE OPEN END.
  #
  # A strictly exclusive reading deletes the current year: all 227 live polities
  # that end at 2025 stop covering 2025, so every present-day row degrades from
  # `matched` to `out_of_span` and is resolved by the nearest-period fallback
  # instead of by a real period. A succeeded period must still yield its terminal
  # year to its successor, which is the whole point of the exclusive bound.
  expect_equal(
    whep:::.polity_join_end_year(2025L, NA_integer_, is_open = TRUE),
    2026
  )
  expect_equal(
    whep:::.polity_join_end_year(1993L, NA_integer_, is_open = FALSE),
    1993
  )

  # Openness is ABSENCE OF A SUCCESSOR, not `end_year == max(end_year)`. The year
  # test breaks for any polity whose last interval ends at the maximum and has a
  # successor, and the maximum moves with every upstream re-sync. Measured on the
  # shipped snapshot: no live polity ending at 2025 carries a successor, so the
  # two agree today -- this pins the reason they will keep agreeing.
  p <- as.data.frame(whep::polities)
  live <- is.na(p$wiki_status) | !p$wiki_status %in% c("retired", "superseded")
  ends_at_max <- live &
    !is.na(p$end_year) &
    p$end_year == max(p$end_year, na.rm = TRUE)
  has_successor <- !is.na(p$successor) & nzchar(trimws(p$successor))
  expect_equal(sum(ends_at_max & has_successor), 0L)
  expect_true(all(p$polity_code[ends_at_max] %in% whep:::.open_polity_codes()))

  # End to end: the current year resolves as a real period hit, while a
  # succession boundary belongs to the successor.
  now <- tibble::tibble(area_code = c(203L, 229L), year = 2025L) |>
    add_polity_code()
  expect_true(all(now$mapping_status == "matched"))

  handover <- tibble::tibble(
    area_code = c(51L, 248L, 186L),
    year = c(1993L, 1992L, 2006L)
  ) |>
    add_polity_code()
  expect_true(all(handover$mapping_status == "out_of_span"))
})

test_that(".iso3_year_to_polity_code gives a boundary year to the successor", {
  # The fourth re-implementation of the `polity_end_year` convention lived in
  # `data-raw/balance_coefficients.R` and read the bound INCLUSIVELY (#565).
  # It is the only one of the four that was silent: where a successor sits under
  # a DIFFERENT ISO3 there is no second candidate to trip the ambiguity abort,
  # so the boundary year resolved to the interval that ended on it and a
  # coefficient was booked to a polity that no longer existed.
  #
  # Synthetic spans, because the real defect cannot be witnessed on the one
  # dataset the builder stamps: `urban_n_reference` is Spain over 1860-2022 and
  # `ESP-1800-2025` covers every benchmark year on either reading.
  crosswalk <- tibble::tribble(
    ~area_iso3c, ~polity_code,    ~polity_start_year, ~polity_end_year,
    "XAA",       "XAA-1900-1950", 1900L,              1950L,
    "XAA",       "XAA-1950-2025", 1950L,              2025L,
    "XBB",       "XBB-1900-1950", 1900L,              1950L
  )
  open <- "XAA-1950-2025"
  resolve <- function(iso3, year) {
    whep:::.iso3_year_to_polity_code(iso3, year, crosswalk, open)
  }

  # The hand-over year belongs to the successor, and is not ambiguous.
  expect_equal(resolve("XAA", 1950L), "XAA-1950-2025")
  expect_equal(resolve("XAA", 1949L), "XAA-1900-1950")
  # Vectorised, because the builder resolves a whole column at once.
  expect_equal(
    resolve(c("XAA", "XAA"), c(1949L, 1950L)),
    c("XAA-1900-1950", "XAA-1950-2025")
  )

  # INCLUSIVE AT THE OPEN END: nothing succeeds `XAA-1950-2025`, so there is no
  # double-count to prevent and excluding 2025 would simply delete a year.
  expect_equal(resolve("XAA", 2025L), "XAA-1950-2025")
  expect_error(resolve("XAA", 2026L), "No polity active")

  # THE SILENT CASE. `XBB-1900-1950` ends in 1950 and no `XBB` interval follows,
  # so the inclusive read answered "XBB-1900-1950" for 1950 with no complaint.
  # The builder must stop instead.
  expect_error(resolve("XBB", 1950L), "No polity active")
  expect_equal(resolve("XBB", 1949L), "XBB-1900-1950")
})

test_that(".iso3_year_to_polity_code aborts rather than pick a candidate", {
  crosswalk <- tibble::tribble(
    ~area_iso3c, ~polity_code,    ~polity_start_year, ~polity_end_year,
    "XCC",       "XCC-1900-2000", 1900L,              2000L,
    "XCC",       "XCC-1950-2000", 1950L,              2000L
  )
  expect_error(
    whep:::.iso3_year_to_polity_code("XCC", 1960L, crosswalk, character()),
    "more than one polity"
  )
  expect_error(
    whep:::.iso3_year_to_polity_code("XZZ", 1960L, crosswalk, character()),
    "No polity active"
  )
})

test_that("urban_n_reference is stamped with the polity live in that year", {
  # The shipped end of the same fix: every benchmark row of the one dataset the
  # builder stamps must name a polity whose span really covers its year, under
  # the exclusive-at-a-succession / inclusive-at-an-open-end reading.
  spans <- whep::polity_area_crosswalk |>
    dplyr::filter(!is.na(polity_code)) |>
    dplyr::distinct(polity_code, polity_start_year, polity_end_year)
  covered_to <- spans$polity_end_year +
    (spans$polity_code %in% whep:::.open_polity_codes())

  urban <- whep::urban_n_reference
  idx <- match(urban$polity_code, spans$polity_code)
  expect_false(anyNA(idx))
  expect_true(all(spans$polity_start_year[idx] <= urban$year))
  expect_true(all(covered_to[idx] > urban$year))
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
  skip_if_not_installed("sf")
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
  # class(geoms) and attr(, "sf_column") both survive a plain `[.data.frame`,
  # so the column they point at is what has to be checked (whep#620).
  expect_s3_class(geoms[[attr(geoms, "sf_column")]], "sfc")
  expect_s3_class(sf::st_geometry(geoms), "sfc")
})

test_that("get_polity_geometries subsets in a session that never loaded sf", {
  # The geometry column only survives a row subset through `[.sf` (whep#620),
  # and that method is registered when the sf namespace loads. sf is suggested
  # rather than imported, so the broken state is a session that never loaded
  # it -- which cannot be recreated in this one. Earlier test files call
  # `sf::`, and `unloadNamespace("sf")` does NOT undo the S3 registration, so
  # `[.sf` keeps dispatching after an unload and an in-process test passes on
  # the unfixed code. A fresh process is the only faithful reproduction.
  skip_if_not_installed("sf")
  skip_if_not_installed("callr")
  installed <- tryCatch(
    file.exists(file.path(find.package("whep"), "Meta", "package.rds")),
    error = function(cnd) FALSE
  )
  skip_if_not(installed, "whep is not installed in a library")

  probe <- callr::r(
    function() {
      library(whep)
      loaded_before <- "sf" %in% loadedNamespaces()
      geoms <- get_polity_geometries(c("AFG-1919-2025", "NCL-1800-2025"))
      list(
        loaded_before = loaded_before,
        rows = nrow(geoms),
        geometry_class = class(geoms[[attr(geoms, "sf_column")]])
      )
    },
    libpath = .libPaths()
  )

  expect_false(probe$loaded_before)
  expect_equal(probe$rows, 2L)
  expect_true("sfc" %in% probe$geometry_class)
})

test_that("get_polity_geometries says why it cannot subset without sf", {
  local_mocked_bindings(
    .sf_namespace_available = function() FALSE,
    .package = "whep"
  )

  expect_error(
    whep::get_polity_geometries("AFG-1919-2025"),
    class = "whep_sf_required"
  )
  # the whole-table path never subsets, so it must keep working without sf
  expect_equal(nrow(whep::get_polity_geometries()), nrow(whep::polities))
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
  # A polity with no published `successor` stays unreachable and the caller must
  # keep warning rather than invent an attribution. `BLX-1850-1999` used to be
  # the example here; the #835 upstream re-sync gave it
  # `BEL-1831-2025; LUX-1839-2025`, so it now resolves (asserted below) and the
  # no-successor property needs a row that still has none. `CEM-1800-2025`
  # (Ceuta and Melilla) is one, and it is a terminal enclave rather than a
  # federation, so it is not a candidate for gaining one.
  res <- whep:::.successor_iso3_map("CEM-1800-2025", c("ESP", "MAR"))
  expect_equal(res[["CEM-1800-2025"]], character(0))
})

test_that("Belgium-Luxembourg reaches its successors after the #835 re-sync", {
  # The upstream re-sync published `BLX-1850-1999`'s successors, so FAOSTAT area
  # 15 now bridges to BEL+LUX in `.federation_land_bridge()`. That is only
  # reached under `federation_land = "successor_union"` (the default is
  # `"none"`), but it is the one attribution this data change adds, so it is
  # pinned rather than left to a caller to discover.
  res <- whep:::.successor_iso3_map("BLX-1850-1999", c("BEL", "LUX"))
  expect_equal(res[["BLX-1850-1999"]], c("BEL", "LUX"))
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
  # Scoped to the explicit fold. WHEP now models the reporting members of
  # bucket 999 in their own right (#459), so there is no Rest-of-World fold
  # by default; what this pins is the fold behaviour itself, which still has
  # to work for anyone reproducing a published-before number.
  withr::local_options(whep.unfold_rest_of_world = "none")
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

# ---- out-of-span stand-ins on published output (whep#545) -------------------

testthat::test_that("the published reporting columns hide the stand-in", {
  # THE DEFECT, PINNED AS THE DEFAULT. `add_polity_code()` documents that a row
  # no mapped period covers is attributed to the NEAREST period and reported as
  # `out_of_span`, "a coverage gap". The reporting-column boundary every
  # area-keyed build output crosses deletes that column, so the two rows below
  # are indistinguishable in published data even though one is a stand-in.
  #
  # FAOSTAT area 51 "Czechoslovakia" is the live case: it keeps being asked for
  # after `F51-1947-1993` ends, so 2015 lands on a polity that ended in 1993.
  # It replaces bucket 206, which used to be this example and stopped being one
  # when whep#860 gave the bucket `F206-2011-2025` for 2012 onward -- the class
  # is unchanged, only its exemplar.
  rows <- tibble::tibble(
    area_code = 51L,
    year = c(1990L, 2015L),
    value = 1
  )

  resolved <- whep::add_polity_code(rows)
  testthat::expect_equal(resolved$polity_code, rep("F51-1947-1993", 2L))
  testthat::expect_equal(resolved$mapping_status, c("manual", "out_of_span"))

  published <- whep:::.add_reporting_polity_columns(rows)
  testthat::expect_setequal(
    setdiff(names(published), c("year", "area_code", "value")),
    c(
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry"
    )
  )
  # Same polity, same name, nothing saying one of them did not exist yet.
  testthat::expect_equal(
    published$reporting_polity_code,
    rep("F51-1947-1993", 2L)
  )
})

testthat::test_that("polity_coverage_gaps finds what the columns hide", {
  gaps <- whep::polity_coverage_gaps(
    tibble::tibble(area_code = 51L, year = c(1990L, 2015L, 2015L), value = 1)
  )

  testthat::expect_equal(nrow(gaps), 1L)
  testthat::expect_equal(gaps$year, 2015L)
  testthat::expect_equal(gaps$area_code, 51L)
  testthat::expect_equal(gaps$polity_code, "F51-1947-1993")
  testthat::expect_equal(gaps$polity_end_year, 1993L)
  # Row COUNTS, not distinct area-years: the caller wants to know how much of
  # its table is affected.
  testthat::expect_equal(gaps$n_rows, 2L)

  # A table with no stand-in gets zero rows and the same columns, so a caller
  # can bind or assert on the result without a special case.
  clean <- whep::polity_coverage_gaps(
    tibble::tibble(area_code = 2L, year = 2000L)
  )
  testthat::expect_equal(nrow(clean), 0L)
  testthat::expect_equal(names(clean), names(gaps))
})

testthat::test_that("polity_coverage_gaps agrees with the resolver", {
  # An invariant rather than a count: the gap set must be EXACTLY the
  # `out_of_span` set `add_polity_code()` reports, or the two answers to "did
  # this polity exist that year" disagree. The count itself moves with every
  # upstream re-sync; the agreement must not. Restricted to 1961 onwards so the
  # back-cast anchor floor is inert, as in the resolver's own test above.
  crosswalk <- whep::polity_area_crosswalk
  grid <- expand.grid(
    area_code = sort(unique(stats::na.omit(crosswalk$area_code))),
    year = 1961:2023
  )
  resolved <- whep::add_polity_code(grid)
  expected <- resolved[
    !is.na(resolved$mapping_status) &
      resolved$mapping_status == "out_of_span",
    c("area_code", "year")
  ]
  gaps <- whep::polity_coverage_gaps(grid)

  testthat::expect_gt(nrow(expected), 0L)
  testthat::expect_equal(nrow(gaps), nrow(expected))
  testthat::expect_setequal(
    paste(gaps$area_code, gaps$year),
    paste(expected$area_code, expected$year)
  )
  # Every reported gap really is outside its polity's period, in one direction
  # or the other.
  #
  # `>=` on the upper bound, not `>`: `polity_end_year` is EXCLUSIVE (#550, and
  # the resolver was made to read it that way in #577), so a row AT the end year
  # is already outside the period and is correctly reported as a gap. Written
  # with `>` this assertion silently encodes the old inclusive convention and
  # fails on exactly those rows -- which is the same convention mismatch #550 is
  # about, reappearing in a test rather than in the resolver.
  testthat::expect_true(all(
    gaps$year < gaps$polity_start_year | gaps$year >= gaps$polity_end_year
  ))
})

testthat::test_that("polity_coverage_gaps names the direction of the gap", {
  # The two directions are different defects and #414 is only one of them, so
  # a consumer counting "rows attributed to a polity that did not exist" needs
  # to be able to separate them. FAOSTAT area 51 after the dissolution is the
  # `"ended"` case: `F51-1947-1993` stopped in 1993 and nothing later is mapped
  # to that area. Area 1 Armenia before 1991 is the `"not_started"` case, which
  # is WHEP's documented back-cast convention.
  gaps <- whep::polity_coverage_gaps(
    tibble::tibble(area_code = c(51L, 1L), year = c(2015L, 1900L))
  )

  testthat::expect_equal(
    gaps$gap_kind[gaps$area_code == 51L],
    "polity_ended"
  )
  testthat::expect_equal(
    gaps$gap_kind[gaps$area_code == 1L],
    "polity_not_started"
  )
  testthat::expect_true(all(
    gaps$gap_kind %in% c("polity_ended", "polity_not_started")
  ))
})

testthat::test_that("the gap direction is read at the back-cast anchor", {
  # The load-bearing half: `gap_kind` is NOT `year < polity_start_year`, and
  # cannot be, because `.add_polity_columns_dt()` floors the lookup year at
  # `backcast_anchor`. FAOSTAT area 273 Montenegro in 1850 is matched as 1961
  # and lands on `MNE-1913-1918`, a polity that had ENDED by the year the
  # resolver used -- so the raw-year comparison would mislabel it. On a real
  # `get_primary_production()` that was 165 rows, areas 178 and 273.
  #
  # The stand-in rule that produced it is now opt-in: whep#705 made the
  # fallback prefer a not-yet-started period, which is why 273 no longer lands
  # there by default. Pinned under `"nearest"` because the divergence the
  # anchored comparison exists for is a property of the resolution, not of one
  # crosswalk vintage, and a later vintage can bring it back to the default.
  withr::local_options(whep.polity_stand_in = "nearest")
  anchored <- whep::polity_coverage_gaps(
    tibble::tibble(area_code = 273L, year = 1850L)
  )
  testthat::expect_equal(anchored$polity_code, "MNE-1913-1918")
  testthat::expect_lt(anchored$polity_end_year, 1961L)
  testthat::expect_equal(anchored$gap_kind, "polity_ended")
  # And the raw-year reading is what the same row gives once the anchor is
  # switched off, which is the pair of answers the column exists to keep apart.
  raw <- whep::polity_coverage_gaps(
    tibble::tibble(area_code = 273L, year = 1850L),
    backcast_anchor = -Inf
  )
  testthat::expect_gt(raw$polity_start_year, 1850L)
  testthat::expect_equal(raw$gap_kind, "polity_not_started")
})

testthat::test_that("gap_kind agrees with the anchored comparison", {
  # An invariant over the whole shipped crosswalk rather than two hand-picked
  # areas: whatever upstream re-syncs do to the polity set, `gap_kind` must
  # stay the answer to "was the matched year before this polity started?" --
  # for the STAND-INS. The back-cast class is deliberately not derivable that
  # way (whep#763): it matched a real period at the anchor, so it is read off
  # the resolution and asserted separately below.
  crosswalk <- whep::polity_area_crosswalk
  grid <- expand.grid(
    area_code = sort(unique(stats::na.omit(crosswalk$area_code))),
    year = c(1850L, 1900L, 1961L, 1990L, 2015L, 2025L)
  )
  gaps <- whep::polity_coverage_gaps(grid)
  stand_ins <- gaps[gaps$gap_kind != "backcast_anchor", ]
  expected <- ifelse(
    !is.na(stand_ins$polity_start_year) &
      pmax(stand_ins$year, 1961L) < stand_ins$polity_start_year,
    "polity_not_started",
    "polity_ended"
  )

  testthat::expect_gt(nrow(stand_ins), 0L)
  testthat::expect_equal(stand_ins$gap_kind, expected)
  # All three classes really occur, so no branch is untested by accident.
  testthat::expect_setequal(
    unique(gaps$gap_kind),
    c("polity_ended", "polity_not_started", "backcast_anchor")
  )
  # And the back-cast rows are exactly the pre-anchor ones whose polity was
  # resolved at the anchor: none of them sits at or after 1961.
  anchored <- gaps[gaps$gap_kind == "backcast_anchor", ]
  testthat::expect_gt(nrow(anchored), 0L)
  testthat::expect_true(all(anchored$year < 1961L))
  testthat::expect_true(all(anchored$polity_start_year > anchored$year))
})

# ---- the back-cast anchor is not a match (whep#763) --------------------------

# Every reporting area the crosswalk resolves, over the whole back-cast block.
# The invariants below are stated on this rather than on hand-picked areas
# because the population is the point: 12,208 of these 29,415 cells sit outside
# the lifetime of the polity they are labelled with.
.backcast_grid <- function() {
  crosswalk <- whep::polity_area_crosswalk
  expand.grid(
    area_code = sort(unique(stats::na.omit(crosswalk$area_code))),
    year = 1850:1960
  )
}

testthat::test_that("a matched row really was inside its polity's period", {
  # THE GUARD. `"matched"` is a claim about the YEAR -- the year fell inside
  # this polity's period -- and `.add_polity_columns_dt()` floors the lookup
  # year at `backcast_anchor` BEFORE the span check, so a pre-anchor row used to
  # come back `"matched"` for a polity that did not exist for another century.
  # FAOSTAT area 238's 1850 row read `ETH-1952-1993`, `matched`. This is the
  # restatement of the claim, over every area and every back-cast year, so it
  # fails on any resolution that makes the assertion without checking it.
  resolved <- whep::add_polity_code(.backcast_grid())
  claimed <- resolved[
    !is.na(resolved$mapping_status) &
      resolved$mapping_status %in% c("matched", "manual"),
  ]

  testthat::expect_gt(nrow(claimed), 0L)
  testthat::expect_true(all(claimed$year >= claimed$polity_start_year))
  # And the class the floored rows go to instead is really populated, so the
  # invariant above cannot be satisfied by nothing ever being matched.
  testthat::expect_true(any(resolved$mapping_status %in% "backcast_anchor"))
})

testthat::test_that("backcast_anchor is exactly the rows the anchor moved", {
  # The definition, pinned both ways: a row is `"backcast_anchor"` if and only
  # if it is pre-anchor AND its polity had not started in its own year AND a
  # period was found (a row no period covers at all stays `"out_of_span"`,
  # which is the stronger statement). An area whose anchor polity DOES cover
  # its own year keeps `"matched"` -- 125 of the 265 resolving areas are in
  # that position for all 111 back-cast years, and for them the floor changed
  # nothing.
  resolved <- whep::add_polity_code(.backcast_grid())
  anchored <- !is.na(resolved$mapping_status) &
    resolved$mapping_status == "backcast_anchor"
  expected <- !is.na(resolved$polity_code) &
    !is.na(resolved$polity_start_year) &
    resolved$year < resolved$polity_start_year &
    !is.na(resolved$mapping_status) &
    resolved$mapping_status != "out_of_span"

  testthat::expect_equal(anchored, expected)
  testthat::expect_true(any(anchored))
  testthat::expect_true(any(
    !is.na(resolved$mapping_status) & resolved$mapping_status == "matched"
  ))
})

testthat::test_that("the anchor status is inert from the anchor year on", {
  # The floor only fires below `backcast_anchor`, so nothing at or after it can
  # carry the status -- which is what makes this a labelling change with no
  # reach into the FAOSTAT era.
  crosswalk <- whep::polity_area_crosswalk
  grid <- expand.grid(
    area_code = sort(unique(stats::na.omit(crosswalk$area_code))),
    year = 1961:2025
  )

  testthat::expect_false(any(
    whep::add_polity_code(grid)$mapping_status %in% "backcast_anchor"
  ))
  # And switching the anchor off removes the class entirely: with no floor
  # there is nothing to distinguish from a real period hit. Area 238 in 1850
  # then resolves to `ETH-1800-1889`, the period that really covers 1850, and
  # is honestly `"matched"` -- which is the pair of readings the status keeps
  # apart, since the anchored answer is a period that starts a century later.
  eth_1850 <- tibble::tibble(area_code = 238L, year = 1850L)
  raw <- whep::add_polity_code(eth_1850, backcast_anchor = -Inf)
  testthat::expect_equal(raw$polity_code, "ETH-1800-1889")
  testthat::expect_equal(raw$mapping_status, "matched")
  testthat::expect_false(any(
    whep::add_polity_code(
      .backcast_grid(),
      backcast_anchor = -Inf
    )$mapping_status %in%
      "backcast_anchor"
  ))

  anchored <- whep::add_polity_code(eth_1850)
  testthat::expect_equal(anchored$polity_code, "ETH-1952-1993")
  testthat::expect_equal(anchored$mapping_status, "backcast_anchor")
})

testthat::test_that("polity_coverage_gaps sees what the floor used to hide", {
  # The defect was not only the label: the floor is applied BEFORE the span
  # check, so the instrument built to audit exactly this resolved an 1850 row as
  # 1961 and came back clean. Over the back-cast block the diagnostic saw 2,664
  # of the 12,208 misattributed cells; it must now see all of them.
  grid <- .backcast_grid()
  resolved <- whep::add_polity_code(grid)
  # OUTSIDE IN EITHER DIRECTION, AND IN EITHER YEAR. This used to test only
  # "the row's own year is before the polity started", because on the shipped
  # crosswalk the back-cast block held no other kind: an area whose periods had
  # all ENDED resolved to `ROW-1850-2025`, which covers 1850-2025 and is
  # therefore never outside anything. Promoting the Rest-of-World members
  # (whep#717) makes that class real -- FAOSTAT area 42 Christmas Island now
  # resolves to `CXR-1946-1958`, an upstream period that ends before FAOSTAT
  # starts reporting -- so restricting the expectation to one direction would
  # assert that the diagnostic MISSES it.
  #
  # The two years are the two gap classes, restated without reading the status
  # the diagnostic is being checked on: the resolver matches on the ANCHORED
  # year, so a period the anchored year misses is the stand-in (`out_of_span`),
  # and a period the anchored year hits but the row's OWN year does not is the
  # back-cast (`backcast_anchor`). `polity_end_year` is exclusive.
  in_span <- function(year, start, end) {
    !is.na(start) & !is.na(end) & year >= start & year < end
  }
  anchored <- pmax(resolved$year, 1961L)
  outside <- resolved[
    !is.na(resolved$polity_code) &
      (!in_span(
        anchored,
        resolved$polity_start_year,
        resolved$polity_end_year
      ) |
        !in_span(
          resolved$year,
          resolved$polity_start_year,
          resolved$polity_end_year
        )),
  ]
  gaps <- whep::polity_coverage_gaps(grid)

  testthat::expect_gt(nrow(outside), 0L)
  testthat::expect_setequal(
    paste(gaps$area_code, gaps$year),
    paste(outside$area_code, outside$year)
  )
  # All three classes are present, and the back-cast one is still the larger
  # half -- the part that was invisible.
  testthat::expect_setequal(
    unique(gaps$gap_kind),
    c("backcast_anchor", "polity_not_started", "polity_ended")
  )
  testthat::expect_gt(
    sum(gaps$gap_kind == "backcast_anchor"),
    sum(gaps$gap_kind != "backcast_anchor")
  )
})

testthat::test_that("gap_kind separates a back-cast row from a hole", {
  # The three classes on one call, on the canonical area for each. 238 in 1850
  # matched a real period AT THE ANCHOR and is WHEP's own convention; 1 Armenia
  # in 1900 matched nothing even at the anchor; 51 Czechoslovakia after 1993 is
  # the ended-polity case (it was bucket 206 until whep#860 labelled that bucket
  # `F206-2011-2025` for 2012 onward).
  gaps <- whep::polity_coverage_gaps(
    tibble::tibble(
      area_code = c(238L, 1L, 51L),
      year = c(1850L, 1900L, 2015L)
    )
  )

  testthat::expect_equal(
    gaps$gap_kind[gaps$area_code == 238L],
    "backcast_anchor"
  )
  testthat::expect_equal(
    gaps$gap_kind[gaps$area_code == 1L],
    "polity_not_started"
  )
  testthat::expect_equal(gaps$gap_kind[gaps$area_code == 51L], "polity_ended")
  # The polity really is the anchor year's, not the row's.
  testthat::expect_equal(
    gaps$polity_code[gaps$area_code == 238L],
    "ETH-1952-1993"
  )
  testthat::expect_gt(gaps$polity_start_year[gaps$area_code == 238L], 1850L)
})

testthat::test_that("a stand-in never splits one area between two entities", {
  # whep#705. The fallback used to rank candidate periods by distance in years
  # alone, so a reporting area with no period at the back-cast anchor was
  # attributed to whichever of its periods happened to be nearer -- and that
  # flipped mid-series, with nothing in the data marking the break. FAOSTAT
  # area 178 Eritrea read `ERI-1889-1952`, the Italian colonial administration,
  # through 1972 and `ERI-1993-2025` from 1973; area 273 Montenegro flipped at
  # 1961 on a margin of ONE year (1961 - 1918 + 1 = 44 against 2006 - 1961 =
  # 45). Preferring the not-yet-started period keeps each area on one entity.
  span <- expand.grid(area_code = c(178L, 273L), year = 1850:2023)
  resolved <- whep::add_polity_code(span)
  per_area <- tapply(
    resolved$polity_code,
    resolved$area_code,
    function(x) length(unique(x))
  )

  testthat::expect_equal(as.vector(per_area), c(1L, 1L))
  testthat::expect_equal(
    unique(resolved$polity_code[resolved$area_code == 178L]),
    "ERI-1993-2025"
  )
  testthat::expect_equal(
    unique(resolved$polity_code[resolved$area_code == 273L]),
    "MNE-2006-2025"
  )

  # `"nearest"` is the pre-#705 ranking and is what the split looked like, so
  # the option really selects the two behaviours rather than being inert.
  withr::local_options(whep.polity_stand_in = "nearest")
  split <- whep::add_polity_code(span)
  testthat::expect_setequal(
    unique(split$polity_code[split$area_code == 178L]),
    c("ERI-1889-1952", "ERI-1993-2025")
  )
  testthat::expect_equal(
    max(split$year[split$polity_code == "ERI-1889-1952"]),
    1972L
  )
  testthat::expect_setequal(
    unique(split$polity_code[split$area_code == 273L]),
    c("MNE-1913-1918", "MNE-2006-2025")
  )
})

testthat::test_that("no area ends on a dead polity and revives on a later one", {
  # The invariant behind the two areas above, asserted over the whole shipped
  # crosswalk so an upstream re-sync that reintroduces the shape at some other
  # area fails here rather than shipping. Once a reporting area is standing in
  # on a polity that had ENDED, it cannot go back to standing in on one that
  # has NOT STARTED in a later year: that ordering can only come from a
  # distance metric flipping, never from succession.
  crosswalk <- whep::polity_area_crosswalk
  grid <- expand.grid(
    area_code = sort(unique(stats::na.omit(crosswalk$area_code))),
    year = 1850:2025
  )
  revived <- function() {
    gaps <- whep::polity_coverage_gaps(grid)
    ended <- gaps |>
      dplyr::filter(.data$gap_kind == "polity_ended") |>
      dplyr::summarise(last_ended = max(.data$year), .by = "area_code")
    not_started <- gaps |>
      dplyr::filter(.data$gap_kind == "polity_not_started") |>
      dplyr::summarise(first_not_started = min(.data$year), .by = "area_code")
    ended |>
      dplyr::inner_join(not_started, by = "area_code") |>
      dplyr::filter(.data$first_not_started > .data$last_ended) |>
      dplyr::pull("area_code")
  }

  testthat::expect_equal(revived(), integer(0))

  # And the invariant is load-bearing: the pre-#705 ranking breaks it, at
  # exactly the two areas whep#705 measured.
  withr::local_options(whep.polity_stand_in = "nearest")
  testthat::expect_setequal(revived(), c(178L, 273L))
})

testthat::test_that("the stand-in option rejects a value it cannot honour", {
  withr::local_options(whep.polity_stand_in = "closest")
  testthat::expect_error(
    whep::add_polity_code(tibble::tibble(area_code = 178L, year = 1900L)),
    "whep.polity_stand_in"
  )
})

testthat::test_that("polity_coverage_gaps needs the area column", {
  testthat::expect_error(
    whep::polity_coverage_gaps(tibble::tibble(year = 2015L)),
    "area_code"
  )
  # A non-default code column is honoured, and a table with no year column
  # falls back to the current mapping, which has no stand-ins by construction.
  renamed <- tibble::tibble(bucket = 51L, year = 2015L)
  testthat::expect_equal(
    nrow(whep::polity_coverage_gaps(renamed, code_column = "bucket")),
    1L
  )
  testthat::expect_equal(
    nrow(whep::polity_coverage_gaps(tibble::tibble(area_code = 51L))),
    0L
  )
})

testthat::test_that("the mapping-status switch carries the signal, opt-in", {
  # Carrying it by default would change the schema of ~100 exported outputs at
  # once, which is an owner decision (#545), so both repairs are selectable and
  # neither is imposed. What is asserted here is that each mode adds EXACTLY the
  # column it promises and changes nothing else.
  rows <- tibble::tibble(
    area_code = 51L,
    year = c(1990L, 2015L),
    value = 1
  )
  base <- whep:::.add_reporting_polity_columns(rows)

  withr::local_options(whep.polity_mapping_status = "flag")
  flagged <- whep:::.add_reporting_polity_columns(rows)
  testthat::expect_equal(
    setdiff(names(flagged), names(base)),
    "reporting_polity_out_of_span"
  )
  testthat::expect_equal(
    flagged$reporting_polity_out_of_span,
    c(FALSE, TRUE)
  )
  testthat::expect_equal(
    as.data.frame(flagged[names(base)]),
    as.data.frame(base)
  )

  withr::local_options(whep.polity_mapping_status = "status")
  status <- whep:::.add_reporting_polity_columns(rows)
  testthat::expect_equal(
    setdiff(names(status), names(base)),
    "reporting_mapping_status"
  )
  testthat::expect_equal(
    status$reporting_mapping_status,
    c("manual", "out_of_span")
  )
  testthat::expect_equal(
    as.data.frame(status[names(base)]),
    as.data.frame(base)
  )
})

testthat::test_that("the switch reaches the partner columns too", {
  trade <- tibble::tibble(
    area_code = 2L,
    area_code_partner = 51L,
    year = c(1990L, 2015L)
  )
  base <- whep:::.add_partner_polity_columns(trade)

  withr::local_options(whep.polity_mapping_status = "flag")
  flagged <- whep:::.add_partner_polity_columns(trade)
  testthat::expect_equal(
    setdiff(names(flagged), names(base)),
    "partner_polity_out_of_span"
  )
  testthat::expect_equal(flagged$partner_polity_out_of_span, c(FALSE, TRUE))
})

testthat::test_that("re-running under the switch adds no duplicate column", {
  # Several builds attach the reporting columns to a frame that already has
  # them, so a mode's own column has to be dropped and rebuilt like the others
  # rather than appended a second time.
  rows <- tibble::tibble(area_code = 51L, year = c(1990L, 2015L))

  withr::local_options(whep.polity_mapping_status = "flag")
  once <- whep:::.add_reporting_polity_columns(rows)
  twice <- whep:::.add_reporting_polity_columns(once)
  testthat::expect_equal(names(twice), names(once))
  testthat::expect_equal(as.data.frame(twice), as.data.frame(once))

  # And switching the mode off again removes it, instead of leaving a stale
  # column behind that no longer tracks the resolution.
  withr::local_options(whep.polity_mapping_status = "none")
  back <- whep:::.add_reporting_polity_columns(once)
  testthat::expect_false("reporting_polity_out_of_span" %in% names(back))
})

testthat::test_that("a mistyped mapping-status option aborts", {
  withr::local_options(whep.polity_mapping_status = "out_of_span")
  testthat::expect_error(
    whep:::.add_reporting_polity_columns(
      tibble::tibble(area_code = 2L, year = 2000L)
    ),
    "whep.polity_mapping_status"
  )
  testthat::expect_error(
    whep:::.polity_status_mode("yes"),
    class = "rlang_error"
  )
})

# Keeping a carried identity instead of resolving it twice (whep#670) ---------
#
# `.aggregate_to_polities()` now emits the reporting identity, so the tail
# helper keeps it rather than re-deriving it over the whole frame. What has to
# be true for that to be safe is tested here: it only keeps an identity that
# still describes the frame's key, and it checks that claim rather than
# trusting it.

.carried_frame <- function(n_rows = 500L) {
  # What the fold emits: one identity per (area_code, year), repeated across the
  # many item rows that share it.
  # 40 is a plain area, 206 is the fold, and 51 is an area whose only polity has
  # ended -- so the frame carries a `matched`, a bucket-aggregate and an
  # `out_of_span` identity, which is what the status test below needs.
  keys <- tibble::tibble(
    area_code = c(40L, 206L, 51L),
    year = c(2015L, 2015L, 2015L)
  )
  base <- whep:::.add_reporting_polity_columns(keys)
  base[rep(seq_len(nrow(base)), length.out = n_rows), ] |>
    dplyr::mutate(value = seq_len(n_rows))
}

testthat::test_that("a carried identity is kept, not resolved row by row", {
  carried <- .carried_frame(500L)
  seen <- integer(0)
  # Held before mocking, because the mock replaces the namespace binding the
  # real helper would otherwise be reached through.
  resolve <- whep:::.add_polity_columns_dt
  testthat::local_mocked_bindings(
    .add_polity_columns_dt = function(data, ...) {
      seen <<- c(seen, nrow(data))
      resolve(data, ...)
    }
  )

  out <- whep:::.add_reporting_polity_columns(carried)
  testthat::expect_equal(as.data.frame(out), as.data.frame(carried))
  # The only resolution left is the check, over the 3 distinct keys rather than
  # the 500 rows. Dropping the carried columns puts the full resolution back.
  testthat::expect_equal(seen, 3L)

  seen <- integer(0)
  stripped <- dplyr::select(
    carried,
    -dplyr::all_of(
      whep:::.reporting_polity_cols()
    )
  )
  again <- whep:::.add_reporting_polity_columns(stripped)
  testthat::expect_equal(seen, 500L)
  testthat::expect_equal(as.data.frame(again), as.data.frame(carried))
})

testthat::test_that("a re-keyed frame is resolved again, not kept", {
  # Bucket codes are fixed points, so an identity that no longer matches the
  # key it sits next to is one someone re-keyed: 40 (Chile) relabelled 231.
  carried <- .carried_frame(4L)
  carried$area_code <- 231L

  out <- whep:::.add_reporting_polity_columns(carried)
  testthat::expect_equal(unique(out$reporting_polity_code), "USA-1959-2025")
  testthat::expect_equal(unique(out$polity_area_code), 231L)
})

testthat::test_that("a contradicting carried identity warns and re-resolves", {
  carried <- .carried_frame(4L)
  carried$reporting_polity_code <- "XXX-1900-2000"

  testthat::expect_warning(
    out <- whep:::.add_reporting_polity_columns(carried),
    "contradicts"
  )
  testthat::expect_equal(
    sort(unique(out$reporting_polity_code)),
    c("CHL-1902-2025", "F206-2011-2025", "F51-1947-1993")
  )
})

testthat::test_that("an incomplete carry re-resolves without warning", {
  # `bind_rows()` with rows the fold never saw leaves NA in the carried columns.
  # That is a gap, not a contradiction, so it is filled silently.
  carried <- .carried_frame(4L)
  carried$reporting_polity_code[2:3] <- NA_character_

  out <- testthat::expect_no_warning(
    whep:::.add_reporting_polity_columns(carried)
  )
  testthat::expect_false(any(is.na(out$reporting_polity_code)))
})

testthat::test_that("the status switch always re-resolves", {
  # `reporting_mapping_status` is not part of the carried set, so a run that
  # asks for it has to resolve rather than keep.
  withr::local_options(whep.polity_mapping_status = "status")
  out <- whep:::.add_reporting_polity_columns(.carried_frame(4L))
  testthat::expect_true("reporting_mapping_status" %in% names(out))
  testthat::expect_equal(
    unique(out$reporting_mapping_status[out$area_code == 51L]),
    "out_of_span"
  )
  # Bucket 206 used to be the `out_of_span` half of this pair and is the
  # `matched` half now: whep#860 gave it `F206-2011-2025`, a live aggregate.
  testthat::expect_equal(
    unique(out$reporting_mapping_status[out$area_code == 206L]),
    "matched"
  )
})

# ---- Where the stop rule under-reaches (whep#863) ---------------------------

# The stop rule assumes a polity's `iso3_code` is COEXTENSIVE with its
# territory. Upstream publishes ten partitions where it is not: the parent keeps
# the ISO3 of one part, so the walk stops on the parent and never sees the other
# parts. `SRB-2006-2008` "Serbia (including Kosovo)" is the case whep#863 is
# named for; the largest by population is `SUD-1956-2011`, whose `SDN` excludes
# South Sudan.
#
# ASSERTED AGAINST THE PUBLISHED RELATION, not against a list: the expectation
# below is a census of the shipped snapshot, so a new partition of this shape
# arriving upstream becomes a test failure rather than a silent under-reach.
testthat::test_that(".successor_code_reuse censuses the lossy partitions", {
  census <- whep:::.successor_code_reuse()
  testthat::expect_setequal(
    census$polity_code,
    c(
      "BCM-1916-1961",
      "DEU-1920-1938",
      "F248-1920-1991",
      "F248-1947-1991",
      "KOR-1945-1948",
      "NLD-1800-1830",
      "PAK-1949-1971",
      "SGP-1946-1963",
      "SRB-2006-2008",
      "SUD-1956-2011"
    )
  )
  serbia <- dplyr::filter(census, .data$polity_code == "SRB-2006-2008")
  testthat::expect_identical(serbia$iso3_code, "SRB")
  testthat::expect_identical(serbia$iso3_not_reached, "KOS")
  sudan <- dplyr::filter(census, .data$polity_code == "SUD-1956-2011")
  testthat::expect_identical(sudan$iso3_not_reached, "SSD")

  # Every row must really be a partition whose parent reuses a part's code, and
  # a temporal continuation (successors all carrying the parent's own ISO3) must
  # NOT be one.
  iso3 <- whep:::.polity_iso3_lookup()
  edges <- whep:::.polity_successor_edges()
  parts <- unname(iso3[edges[["SRB-2006-2008"]]])
  testthat::expect_true(all(c("SRB", "KOS") %in% parts))
  testthat::expect_false("SWE-1814-1905" %in% census$polity_code)
})

# THE LOSS IS SILENT AND IT IS NOT A MISSING EDGE. `SRB-2006-2008` publishes
# both parts, and the walk still returns only `SRB` -- even when the vocabulary
# carries `KOS` too. No warning, no `NA`, no abort: the caller gets a shorter
# non-empty answer and cannot tell.
testthat::test_that("the walk stops at Serbia-including-Kosovo either way", {
  vocab <- c("SRB", "MNE", "KOS")
  testthat::expect_identical(
    whep:::.polity_successor_edges()[["SRB-2006-2008"]],
    c("SRB-2008-2025", "KOS-2008-2025")
  )
  res <- whep:::.successor_iso3_map(c("SRB-2006-2008", "SCG-1992-2006"), vocab)
  testthat::expect_identical(res[["SRB-2006-2008"]], "SRB")
  testthat::expect_identical(res[["SCG-1992-2006"]], c("MNE", "SRB"))
})

# `.successor_stop_map()` is where the ISO3 codes come from, so a caller can ask
# WHICH polities a branch stopped on and cross them against the census above.
testthat::test_that(".successor_stop_map names the polities walked to", {
  stops <- whep:::.successor_stop_map(
    c("SCG-1992-2006", "BEL-1831-2025", "CEM-1800-2025"),
    c("SRB", "MNE", "BEL")
  )
  testthat::expect_setequal(
    stops[["SCG-1992-2006"]],
    c("MNE-2006-2025", "SRB-2006-2008")
  )
  # A polity already in the vocabulary stops on itself.
  testthat::expect_identical(stops[["BEL-1831-2025"]], "BEL-1831-2025")
  testthat::expect_identical(stops[["CEM-1800-2025"]], character(0))

  # and it must agree with the ISO3 map it backs
  iso3 <- whep:::.polity_iso3_lookup()
  testthat::expect_identical(
    sort(unique(unname(iso3[stops[["SCG-1992-2006"]]]))),
    whep:::.successor_iso3_map("SCG-1992-2006", c("SRB", "MNE", "BEL"))[[1]]
  )
})
