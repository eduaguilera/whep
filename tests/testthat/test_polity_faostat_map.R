# The area-to-polity mapping comes from upstream, not from string surgery.
#
# `whep-polities` publishes `faostat_area_polity_map.csv` -- 281 rows over 228
# FAOSTAT area codes, each with the year span it applies to and a `match_route`
# recording how it was decided. This package used to ignore it and infer the same
# mapping from the polity code STRING via `sub("-.*", "", polity_code)`, which is
# a different question with different answers. These tests pin the properties that
# distinguish the two, so a regression to inference fails here rather than moving
# published numbers quietly.

testthat::test_that("only the expected areas fall back to prefix inference", {
  # PINNED so the list can only shrink deliberately. Every area here is one the
  # upstream map does not cover, so its polity is inferred rather than declared.
  # All seven are statistical constructs rather than territories: 351 is the
  # FAOSTAT "China" aggregate, deliberately left unmapped so it cannot
  # double-count its own components, and 901-906 are the regional "Other" buckets
  # that resolve to WHEP's own regional aggregate polities.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  fallback <- sort(unique(cw$area_code[
    cw$mapping_source == "prefix_fallback" & !is.na(cw$area_code)
  ]))

  testthat::expect_equal(fallback, c(351L, 901L, 902L, 903L, 904L, 905L, 906L))
})

testthat::test_that("mapping_source accounts for every crosswalk row", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  testthat::expect_setequal(
    unique(cw$mapping_source),
    c("upstream_map", "prefix_outside_map", "fabio_row_fold", "prefix_fallback")
  )
  # The map is the authority for the FAOSTAT era, so it must resolve the bulk of
  # the reporting areas rather than a handful of exceptions.
  mapped_areas <- unique(cw$area_code[cw$mapping_source == "upstream_map"])
  testthat::expect_gte(length(mapped_areas), 197L)

  # Only map rows carry the upstream span and route; nothing else may claim them.
  from_map <- cw$mapping_source == "upstream_map"
  testthat::expect_true(all(!is.na(cw$map_match_route[from_map])))
  testthat::expect_true(all(is.na(cw$map_match_route[!from_map])))
  # Four of upstream's five routes reach here. All 18 `registry` rows are areas
  # FABIO folds into Rest-of-World (Andorra, Monaco, San Marino, Greenland and
  # the like), so the fold consumes that route entirely -- see the fold test.
  testthat::expect_setequal(
    unique(cw$map_match_route[from_map]),
    c("iso-equal", "manual-route", "manual-replace", "manual-span")
  )
  testthat::expect_equal(sum(from_map), 245L)
})

testthat::test_that("the map's spans partition each area's reporting years", {
  # The spans are INCLUSIVE on both ends, which is what makes an area's periods
  # contiguous rather than overlapping. Read as exclusive they would leave a
  # one-year hole at every transition, so this is the assertion that would fail if
  # the convention were ever mixed up.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw <- cw[cw$mapping_source == "upstream_map", ]
  cw <- cw[order(cw$area_code, cw$map_year_start), ]

  same_area <- c(FALSE, diff(cw$area_code) == 0L)
  previous_end <- c(NA_integer_, utils::head(cw$map_year_end, -1L))
  gap <- cw$map_year_start[same_area] - previous_end[same_area]

  testthat::expect_gt(length(gap), 40L)
  testthat::expect_equal(unique(gap), 1L)
  testthat::expect_true(all(cw$map_year_end >= cw$map_year_start))

  # And every span lies inside the validity of the polity it names, which is the
  # other half of the same check: `polity_end_year` is EXCLUSIVE while
  # `map_year_end` is inclusive, so a span may end on the polity's end year.
  testthat::expect_true(all(cw$map_year_start >= cw$polity_start_year))
  testthat::expect_true(all(cw$map_year_end <= cw$polity_end_year))
})

testthat::test_that("prefix inference cannot reach a non-canonical code", {
  # `sub("-.*", "")` turns `MMR-LWR-1852-1885` into `MMR`, which is not that
  # polity's family but the family of the entity it sat inside. Five polities
  # carry the four-part shape and every one of them used to enter the crosswalk
  # through that collapse. Inference must not reach them at all.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  codes <- unique(cw$polity_code[!is.na(cw$polity_code)])
  non_canonical <- codes[!grepl("^[^-]+-[0-9]{4}-[0-9]{4}$", codes)]

  testthat::expect_equal(non_canonical, character(0))

  # Three of the five were typed `subnational`. The three that remain are named
  # explicitly by the upstream map, each covering the whole of what the FAOSTAT
  # area reported in those years: Burundi and Rwanda for 1961, while both were
  # part of Ruanda-Urundi, and Singapore for 1963-1964, inside Malaysia. They are
  # curated decisions, not prefix accidents, so they are pinned rather than
  # excluded -- and every one of them must come from the map.
  subnational <- cw[
    !is.na(cw$polity_type) & cw$polity_type == "subnational",
  ]
  testthat::expect_setequal(
    unique(subnational$polity_code),
    c("BDI-1922-1962", "RWA-1922-1962", "SGP-1963-1965")
  )
  testthat::expect_true(all(subnational$mapping_source == "upstream_map"))
})

testthat::test_that("the map fixes the resolutions the prefix got wrong", {
  # Each of these is a measured before/after against the same polities vintage.
  mapped <- tibble::tibble(
    area_code = c(15L, 72L, 72L, 248L, 206L, 181L),
    year = c(1961L, 1970L, 1990L, 1980L, 1970L, 1962L)
  ) |>
    add_polity_code()

  # Belgium-Luxembourg: prefix `BLX` reached both `BLX-1850-1999` and
  # `BLX-1921-1999` and the tie-break picked the later start.
  testthat::expect_equal(mapped$polity_code[1], "BLX-1850-1999")
  # Djibouti resolved to NOTHING: no prefix derived from `DJI` reaches `FRS`.
  testthat::expect_equal(mapped$polity_code[2], "FRS-1884-1977")
  testthat::expect_equal(mapped$polity_code[3], "FRS-1977-2025")
  # Yugoslavia: `F248-1920-1991` is retired upstream.
  testthat::expect_equal(mapped$polity_code[4], "F248-1947-1991")
  # Sudan (former) stood in on post-secession Sudan; Southern Rhodesia was
  # unreachable because its period carries a different prefix from the area ISO3.
  testthat::expect_equal(mapped$polity_code[5], "SUD-1956-2011")
  testthat::expect_equal(mapped$polity_code[6], "SRH-1953-1964")
})

testthat::test_that("area 52 keeps pre-1991 coverage without a prefix collapse", {
  # `AZE-SSR-1920-1991` used to reach the crosswalk ONLY through the prefix
  # collapse, and it was the sole reason area 52 had any pre-1991 answer. It is
  # gone, and no row is dropped: a pre-1992 area-52 row still resolves, flagged
  # `out_of_span` because FAOSTAT reports area 52 only from 1992 (its own data
  # for earlier years is filed under area 228, the USSR). That is the honest
  # answer -- upstream would have to add a pre-1992 span for area 52 to make it a
  # period hit, and this package no longer invents one.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  testthat::expect_false("AZE-SSR-1920-1991" %in% cw$polity_code)

  mapped <- tibble::tibble(area_code = c(52L, 52L), year = c(1970L, 2000L)) |>
    add_polity_code()
  testthat::expect_false(any(is.na(mapped$polity_code)))
  testthat::expect_equal(mapped$mapping_status, c("out_of_span", "matched"))
})

testthat::test_that("the FABIO Rest-of-World fold still outranks the map", {
  # PINNED because lifting it moves every Rest-of-World figure and is tracked
  # separately (#419/#414), not because it is right. 31 areas the upstream map
  # names a real polity for -- Syria, North Macedonia, Eswatini, New Caledonia,
  # French Guiana, Palestine among them -- stay on `ROW-1850-2023` because FABIO
  # folds them into its single Rest-of-World row. Adopting the map deliberately
  # did NOT change that, so this number must move only on purpose.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  folded <- unique(cw$area_code[cw$mapping_source == "fabio_row_fold"])

  testthat::expect_equal(length(folded), 62L)
  testthat::expect_true(all(
    cw$polity_code[cw$mapping_source == "fabio_row_fold"] == "ROW-1850-2023"
  ))
  for (area in c(212L, 154L, 209L, 153L, 69L, 299L)) {
    testthat::expect_true(area %in% folded)
  }
})
