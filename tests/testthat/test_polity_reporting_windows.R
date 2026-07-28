# `.production_reporting_windows` states, per FAOSTAT reporting area, the years
# in which that area is the correct reporter. It replaced a block of bare
# integer cutoffs in `.filter_dissolved_countries()`.
#
# The window is NOT derivable from the polity span — FAOSTAT reports Belgium
# inside Belgium-Luxembourg until 1999 even though BEL-1831-2025 spans the whole
# period — so the table cannot be replaced by a join. But the polity span is a
# hard UPPER bound on it: a window that admits a year no polity mapped to that
# area covers would attribute production to a polity that did not exist. That is
# the invariant here, and it is what caught the two bounds the integer version
# was missing (area 51 admitted pre-1918 rows, area 15 pre-1850 ones).

windows <- whep:::.production_reporting_windows

test_that("every reporting window names a real FAOSTAT area", {
  cw <- whep::polity_area_crosswalk
  unknown <- setdiff(windows$area_code, cw$polity_area_code)
  expect_equal(
    length(unknown),
    0L,
    info = paste0(
      "areas absent from polity_area_crosswalk$polity_area_code: ",
      paste(unknown, collapse = ", ")
    )
  )
})

test_that("no reporting window admits a year outside its polity spans", {
  cw <- as.data.frame(whep::polity_area_crosswalk)

  # The database marks an ongoing polity by setting `end_year` to the present
  # year rather than leaving it open, so the largest end year in the table IS
  # that sentinel. Derived rather than hardcoded: a 2026 upstream refresh must
  # not turn every live polity into a closed span and fail this test.
  present_year <- max(cw$polity_end_year, na.rm = TRUE)

  for (i in seq_len(nrow(windows))) {
    w <- windows[i, ]
    fam <- cw[cw$polity_area_code == w$area_code, ]
    testthat::skip_if(nrow(fam) == 0)

    # The union of spans mapped to this area. Gaps inside it are not checked:
    # area 167 legitimately maps to CZE-1804-1918 and CZE-1993-2025 with a
    # 75-year hole, and the window sits in the later segment.
    span_first <- min(fam$polity_start_year, na.rm = TRUE)
    span_last <- max(fam$polity_end_year, na.rm = TRUE)

    first <- if (is.na(w$first_year)) -Inf else w$first_year
    last <- if (is.na(w$last_year)) Inf else w$last_year

    expect_true(
      first >= span_first,
      info = paste0(
        "area ",
        w$area_code,
        " admits rows from ",
        first,
        " but its earliest polity starts ",
        span_first,
        " (",
        w$why,
        ")"
      )
    )
    # `end_year` 2025 is the database's "still ongoing" sentinel, not a real
    # terminal year, so an open upper bound is correct for a live polity and
    # only a CLOSED span may bound the window from above.
    ongoing <- span_last >= present_year
    expect_true(
      ongoing || last <= span_last,
      info = paste0(
        "area ",
        w$area_code,
        " admits rows to ",
        last,
        " but its latest polity ends ",
        span_last,
        " (",
        w$why,
        ")"
      )
    )
  }
})

test_that("the filter reproduces the integer cutoffs it replaced", {
  # Behaviour lock against the previous implementation, over a grid wide enough
  # to cover every cutoff. The two deliberate tightenings (area 51 before 1918,
  # area 15 before 1850) are asserted separately below and excluded here.
  grid <- expand.grid(
    area_code = c(
      51L,
      167L,
      199L,
      126L,
      119L,
      63L,
      198L,
      98L,
      15L,
      255L,
      256L,
      1L
    ),
    year = c(
      1700L,
      1800L,
      1860L,
      1910L,
      1917L,
      1918L,
      1930L,
      1960L,
      1991L,
      1992L,
      1993L,
      1999L,
      2000L,
      2020L
    )
  )
  grid$tonnes <- 1

  old_filter <- function(df) {
    dplyr::filter(
      df,
      !(area_code == 51L & year > 1992),
      !(area_code %in% c(167L, 199L) & year < 1993),
      !(area_code %in% c(126L, 119L, 63L, 198L, 98L) & year < 1992),
      !(area_code == 15L & year > 1999),
      !(area_code %in% c(255L, 256L) & year < 2000)
    )
  }

  new <- whep:::.filter_dissolved_countries(grid)
  old <- old_filter(grid)

  key <- function(d) paste(d$area_code, d$year)
  # Everything the new filter keeps was kept by the old one: no row is
  # readmitted, so no double-counting is introduced.
  expect_true(all(key(new) %in% key(old)))

  # And the only rows the new filter additionally drops are the pre-polity ones.
  extra <- setdiff(key(old), key(new))
  expect_setequal(
    extra,
    c(
      "51 1700",
      "51 1800",
      "51 1860",
      "51 1910",
      "51 1917",
      "15 1700",
      "15 1800"
    )
  )
})

test_that("unlisted areas pass through unconstrained", {
  df <- data.frame(area_code = 1L, year = c(1700L, 1900L, 2025L), tonnes = 1)
  expect_equal(nrow(whep:::.filter_dissolved_countries(df)), 3L)
})
