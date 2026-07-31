# `polity_area_crosswalk` lists one row per (FAOSTAT area, polity period). Two properties have
# to hold for that to be joinable, and neither was checked.
#
# EXACT DUPLICATES are unambiguously wrong: the same area and the same polity code twice means
# any join through the crosswalk returns that area's data twice. One exists today -- area 240
# carries `ROW-1850-2023` twice -- and it reaches bucket 999, the rest-of-world aggregate, which
# is the single largest bucket in the build.
#
# OVERLAPPING PERIODS are the subtler case. Periods for one area are meant to partition time, so
# a data row for a given year matches exactly one of them. Where two overlap, a year-aware join
# matches both. `main` has one such area; this tree has nine, and they arrive by three distinct
# routes:
#
#   1. The prefix regex. `data-raw/table_mappings.R` derives the join key with
#      `sub("-.*", "", polity_code)`, which stops at the FIRST dash -- so `MMR-LWR-1852-1885`
#      yields prefix `MMR` and is joined in as though it were a period of Myanmar itself. Five
#      crosswalk rows come in this way: three `subnational` (IDN-BLB, IDN-JVM, IDN-OTH),
#      one `colonial` (MMR-LWR) and one `national` (AZE-SSR).
#   2. Genuinely overlapping periods upstream -- area 170 has `PER-1825-1884`, `PER-1825-1909`
#      and `PER-1884-1909`, which cannot all be right. That is a polities-database question,
#      not a packaging one.
#   3. The exact duplicate above.
#
# PINNED BY IDENTITY, NOT COUNT, and deliberately not asserted to be zero. Nine is the state
# this tree is in; asserting zero would fail today and tell no one which of the three routes
# regressed. A tenth appearing is the signal worth having, and if one is fixed the pin fails and
# says which -- that is the intended way to remove an entry.
#
# NOT a claim that these explain any value movement. Measured: the areas involved are not the
# ones whose totals move most against `main`, and five crosswalk rows of 621 cannot account for
# `feed` moving 7%. They are wrong on their own terms.

.crosswalk_overlaps <- function() {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw <- cw[!is.na(cw$area_code) & !is.na(cw$polity_start_year), ]
  out <- list()
  for (a in unique(cw$area_code)) {
    r <- cw[cw$area_code == a, ]
    r <- r[order(r$polity_start_year), ]
    if (nrow(r) < 2L) {
      next
    }
    for (i in seq(2L, nrow(r))) {
      if (r$polity_start_year[[i]] < r$polity_end_year[[i - 1L]]) {
        out[[length(out) + 1L]] <- paste0(
          a,
          ":",
          r$polity_code[[i - 1L]],
          "|",
          r$polity_code[[i]]
        )
      }
    }
  }
  sort(unlist(out))
}

testthat::test_that("no area carries the same polity code twice", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  keyed <- cw[
    !is.na(cw$area_code) & !is.na(cw$polity_code),
    c("area_code", "polity_code")
  ]
  dups <- keyed[duplicated(keyed), ]

  # Known and wrong: area 240's `ROW-1850-2023`, which lands in bucket 999. Pinned rather than
  # asserted away so that fixing it fails this test and says so, and so that a SECOND duplicate
  # appearing is distinguishable from the one already here.
  testthat::expect_equal(
    paste0(dups$area_code, ":", dups$polity_code),
    "240:ROW-1850-2023"
  )
})

testthat::test_that("overlapping polity periods per area are the nine known ones", {
  testthat::expect_setequal(
    .crosswalk_overlaps(),
    c(
      "101:IDN-1949-1969|IDN-BLB-1949-1951",
      "101:IDN-BLB-1949-1951|IDN-JVM-1949-1951",
      "101:IDN-JVM-1949-1951|IDN-OTH-1949-1951",
      "15:BLX-1850-1999|BLX-1921-1999",
      "170:PER-1825-1884|PER-1825-1909",
      "170:PER-1825-1909|PER-1884-1909",
      "240:ROW-1850-2023|ROW-1850-2023",
      "273:MNE-1913-1915|MNE-1913-1918",
      "28:MMR-1852-1885|MMR-LWR-1852-1885"
    )
  )
})

testthat::test_that("subnational polities do not enter the crosswalk as national periods", {
  # The prefix-regex route, isolated. A code of the form `PREFIX-SUB-start-end` has more than
  # three dash-separated segments; `sub("-.*", "")` collapses it onto `PREFIX`, so it joins as a
  # period of the national entity. Three of the five that do this are typed `subnational`, which
  # a crosswalk mapping FAOSTAT reporting areas should not contain at all.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  codes <- unique(cw$polity_code[!is.na(cw$polity_code)])
  deep <- codes[vapply(strsplit(codes, "-"), length, integer(1)) > 3L]

  testthat::expect_setequal(
    deep,
    c(
      "MMR-LWR-1852-1885",
      "AZE-SSR-1920-1991",
      "IDN-BLB-1949-1951",
      "IDN-JVM-1949-1951",
      "IDN-OTH-1949-1951"
    )
  )

  # And the type split, so that "fixed by filtering subnational" is distinguishable from "fixed
  # by fixing the regex": filtering leaves MMR-LWR and AZE-SSR, fixing the regex removes all five.
  types <- sort(table(cw$polity_type[cw$polity_code %in% deep]))
  testthat::expect_equal(as.integer(types[["subnational"]]), 3L)
})
