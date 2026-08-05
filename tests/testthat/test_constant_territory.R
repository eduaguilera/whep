# Deterministic tests for build_constant_territory_series() on synthetic
# rectangular polities in the equal-area CRS (EPSG:6933), where cell areas are
# exact and the expected estimates are hand-derivable.

.rect <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(matrix(
    c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
    ncol = 2,
    byrow = TRUE
  )))
}

# Geometry (metres in EPSG:6933):
#   SRC  : [0,100k] x [0,100k]   value 100 in year 1900   (active 1850-1950)
#   T_L  : [0, 50k] x [0,100k]   active in 2000           (left half of SRC)
#   T_R  : [50k,150k] x [0,100k] active in 2000           (right half of SRC
#          PLUS an uncovered strip [100k,150k] with no source)
.synthetic_polities <- function() {
  sf::st_sf(
    polity_code = c("SRC", "T_L", "T_R"),
    start_year = c(1850L, 1990L, 1990L),
    end_year = c(1950L, 2025L, 2025L),
    geometry = sf::st_sfc(
      .rect(0, 0, 100000, 100000),
      .rect(0, 0, 50000, 100000),
      .rect(50000, 0, 150000, 100000),
      crs = 6933
    )
  )
}

.reported <- data.frame(year = 1900L, polity_code = "SRC", value = 100)

test_that("uniform density: covered mass is conserved and split by area", {
  res <- build_constant_territory_series(
    .reported,
    ref_year = 2000,
    polities = .synthetic_polities(),
    resolution = 10000,
    verbose = FALSE
  )
  res <- res[order(res$target_polity_code), ]

  # T_L sits entirely inside SRC -> half the source area -> half the value.
  tl <- res[res$target_polity_code == "T_L", ]
  expect_equal(tl$value, 50, tolerance = 1e-6)
  expect_equal(tl$imputed_share, 0, tolerance = 1e-9)

  # T_R: covered half of SRC (50) + an equal-area uncovered strip imputed at
  # the same regional intensity (50) -> 100, with half its area imputed.
  tr <- res[res$target_polity_code == "T_R", ]
  expect_equal(tr$covered, 50, tolerance = 1e-6)
  expect_equal(tr$imputed, 50, tolerance = 1e-6)
  expect_equal(tr$value, 100, tolerance = 1e-6)
  expect_equal(tr$imputed_share, 0.5, tolerance = 1e-6)

  # Conservation: covered mass over all targets equals the reported total.
  expect_equal(sum(res$covered), 100, tolerance = 1e-6)
})

test_that("donor='none' performs no imputation but still reports the gap", {
  res <- build_constant_territory_series(
    .reported,
    ref_year = 2000,
    polities = .synthetic_polities(),
    resolution = 10000,
    donor = "none",
    verbose = FALSE
  )
  tr <- res[res$target_polity_code == "T_R", ]
  expect_equal(tr$imputed, 0, tolerance = 1e-9)
  expect_equal(tr$value, 50, tolerance = 1e-6) # covered only
  expect_equal(tr$imputed_share, 0.5, tolerance = 1e-6) # gap still disclosed
})

test_that("non-uniform covariate shifts mass dasymetrically (not by area)", {
  # density 1 west of x=50k, density 3 east of it. Conservation must still hold,
  # but the east (T_R covered part) should take 3x the mass of the west (T_L).
  cov_fn <- function(centroids, year) {
    x <- sf::st_coordinates(centroids)[, 1]
    ifelse(x < 50000, 1, 3)
  }
  res <- build_constant_territory_series(
    .reported,
    ref_year = 2000,
    polities = .synthetic_polities(),
    covariate = cov_fn,
    resolution = 10000,
    verbose = FALSE
  )
  res <- res[order(res$target_polity_code), ]
  tl <- res[res$target_polity_code == "T_L", ]
  tr <- res[res$target_polity_code == "T_R", ]

  # West gets 100 * (1*A)/(1*A + 3*A) = 25 ; east covered gets 75.
  expect_equal(tl$value, 25, tolerance = 1e-6)
  expect_equal(tr$covered, 75, tolerance = 1e-6)
  # Covered conservation independent of the density field.
  expect_equal(sum(res$covered), 100, tolerance = 1e-6)
  # Dasymetric, not areal: covered east/west ratio ~ 3.
  expect_equal(tr$covered / tl$value, 3, tolerance = 1e-6)
})

test_that("years with no usable source are skipped, output schema is stable", {
  res <- build_constant_territory_series(
    data.frame(year = 1700L, polity_code = "SRC", value = 5), # SRC not active 1700
    ref_year = 2000,
    polities = .synthetic_polities(),
    resolution = 10000,
    verbose = FALSE
  )
  expect_s3_class(res, "tbl_df")
  expect_named(
    res,
    c(
      "target_polity_code",
      "year",
      "value",
      "covered",
      "imputed",
      "imputed_share",
      "n_sources"
    )
  )
  expect_equal(nrow(res), 0)
})

# ---------------------------------------------------------------------------
# Epoch resolution: `start_year` inclusive, `end_year` EXCLUSIVE at a
# succession and INCLUSIVE at the open end (DA-24).
#
# The four blocks above use deliberately NON-adjacent epochs (SRC 1850-1950,
# targets 1990-2025), so they pass under either convention and prove nothing
# about the boundary. The blocks below assert the property over EVERY polity in
# the shipped table, and add the adjacent-epoch fixture the others avoid.
#
# These run without `sf`: `.active_polities()` only reads `polity_code`,
# `start_year` and `end_year`, so the geometry is dropped as a plain data frame.
# ---------------------------------------------------------------------------

.ct_polities_flat <- function() {
  flat <- as.data.frame(whep::polities)
  flat[["geom"]] <- NULL
  flat[, c("polity_code", "start_year", "end_year")]
}

.ct_domain_years <- function(flat) {
  seq.int(min(flat$start_year), max(flat$end_year))
}

test_that("no year resolves to two intervals of the same polity", {
  flat <- .ct_polities_flat()
  years <- .ct_domain_years(flat)
  resolved <- purrr::map(years, \(yr) {
    whep:::.polity_family(whep:::.active_polities(flat, yr)$polity_code)
  })
  names(resolved) <- as.character(years)

  offenders <- purrr::imap(resolved, \(family, yr) {
    if (anyDuplicated(family) == 0L) {
      return(character())
    }
    paste0(unique(family[duplicated(family)]), "@", yr)
  })
  # Every polity, every year in 1684-2025 -- not a hand-picked sample.
  expect_equal(sort(unique(unlist(offenders))), character())

  # DA-24: the property has to hold at the OPEN END too, and there it is only a
  # real assertion if the year resolves at all. Under a uniformly exclusive
  # read the last year of the domain is empty, which would make the line above
  # vacuous exactly where the current polities live.
  last <- as.character(max(flat$end_year))
  expect_gt(length(resolved[[last]]), 0L)
  expect_equal(anyDuplicated(resolved[[last]]), 0L)

  # No family has an interval STARTING on the open end, so nothing succeeds
  # there and the year before it must resolve to precisely the same polities.
  expect_equal(sum(flat$start_year == max(flat$end_year)), 0L)
  previous <- as.character(max(flat$end_year) - 1L)
  expect_setequal(resolved[[last]], resolved[[previous]])
  expect_equal(length(resolved[[last]]), length(resolved[[previous]]))
})

test_that("the open end is read from the data and admits no sibling", {
  flat <- .ct_polities_flat()
  family <- whep:::.polity_family(flat$polity_code)
  open <- whep:::.open_ended_intervals(flat$start_year, flat$end_year, family)
  domain_end <- max(flat$end_year)

  # Open-ended means BOTH conditions: the interval ends where the table's
  # coverage ends, AND nothing of its own polity starts later.
  expect_true(all(flat$end_year[open] == domain_end))
  expect_equal(max(table(family[open])), 1L)
  expect_setequal(family[open], unique(family[flat$end_year == domain_end]))

  # The successor half is what a bare "end_year is the maximum" test misses.
  # These seven polities each carry a SECOND interval ending on the domain end,
  # and opening them too would count the terminal year twice. Enumerated, as
  # C2's shared-start list is, so the next one cannot arrive silently.
  succeeded <- flat$polity_code[flat$end_year == domain_end & !open]
  expect_setequal(
    succeeded,
    c(
      "AGO-1816-2025",
      "ARG-1800-2025",
      "BLZ-1800-2025",
      "BRA-1800-2025",
      "GRC-1919-2025",
      "IRQ-1921-2025",
      "ROU-1940-2025"
    )
  )
  # Each of the seven really is succeeded: a later-starting interval of the
  # same polity exists, so this is a succession and not an open end.
  later <- purrr::map_lgl(succeeded, \(code) {
    i <- match(code, flat$polity_code)
    any(family == family[i] & flat$start_year > flat$start_year[i])
  })
  expect_true(all(later))
  # ...and none of them is active on the open end, while its successor is.
  at_end <- whep:::.active_polities(flat, domain_end)$polity_code
  expect_equal(intersect(succeeded, at_end), character())
  expect_setequal(
    intersect(whep:::.polity_family(succeeded), whep:::.polity_family(at_end)),
    whep:::.polity_family(succeeded)
  )
})

test_that("a later sibling that ends earlier does not close the open end", {
  # "X-1900-2025" runs to the domain end; "X-1950-1980" starts later but is
  # long gone by then, so it succeeds nothing there. Reading any later start as
  # a successor would leave X alive in 2024 and absent in 2025 -- a one-year
  # hole in a polity that is continuous.
  sibling <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "X-1900-2025", 1900L, 2025L,
    "X-1950-1980", 1950L, 1980L,
    "Y-1900-2025", 1900L, 2025L
  )
  expect_setequal(
    whep:::.active_polities(sibling, 2024)$polity_code,
    c("X-1900-2025", "Y-1900-2025")
  )
  expect_setequal(
    whep:::.active_polities(sibling, 2025)$polity_code,
    c("X-1900-2025", "Y-1900-2025")
  )
  # The overlap rule itself is untouched: while both X intervals are live the
  # later-starting one still wins.
  expect_setequal(
    whep:::.active_polities(sibling, 1960)$polity_code,
    c("X-1950-1980", "Y-1900-2025")
  )
})

test_that("the open end moves with the table, not with a hardcoded 2025", {
  # A table whose coverage ends in 2000. Its own open end is 2000; 2025 -- the
  # shipped vintage's open end -- resolves to nothing here.
  shifted <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "P-1800-1900", 1800L, 1900L,
    "P-1900-2000", 1900L, 2000L
  )
  expect_equal(
    whep:::.active_polities(shifted, 2000)$polity_code,
    "P-1900-2000"
  )
  expect_equal(nrow(whep:::.active_polities(shifted, 2025)), 0L)
  # The succession boundary stays exclusive: 1900 belongs to the successor, and
  # a terminal year is not reopened merely because an interval ends on it.
  expect_equal(
    whep:::.active_polities(shifted, 1900)$polity_code,
    "P-1900-2000"
  )

  # Extend the table forward and 2000 becomes a closed succession boundary
  # again, with the open end following upstream to 2100.
  extended <- rbind(
    shifted,
    tibble::tibble(
      polity_code = "P-2000-2100",
      start_year = 2000L,
      end_year = 2100L
    )
  )
  expect_equal(
    whep:::.active_polities(extended, 2000)$polity_code,
    "P-2000-2100"
  )
  expect_equal(
    whep:::.active_polities(extended, 2100)$polity_code,
    "P-2000-2100"
  )
  expect_equal(nrow(whep:::.active_polities(extended, 2101)), 0L)
})

test_that("every adjacent-epoch boundary year resolves to the successor", {
  flat <- .ct_polities_flat()
  flat$family <- whep:::.polity_family(flat$polity_code)
  pairs <- merge(
    flat,
    flat,
    by = "family",
    suffixes = c("_pred", "_succ")
  )
  pairs <- pairs[pairs$end_year_pred == pairs$start_year_succ, ]
  # The shipped table must actually contain adjacent epochs, or this block is
  # vacuous -- the failure mode AM-25 flagged in two other blocks.
  expect_gt(nrow(pairs), 100)

  resolved <- purrr::map(unique(pairs$start_year_succ), \(yr) {
    whep:::.active_polities(flat, yr)$polity_code
  })
  names(resolved) <- as.character(unique(pairs$start_year_succ))

  pred_alive <- purrr::map2_lgl(
    pairs$polity_code_pred,
    as.character(pairs$start_year_succ),
    \(code, yr) code %in% resolved[[yr]]
  )
  # THE fencepost property: the predecessor is dissolved ON its own `end_year`.
  expect_equal(pairs$polity_code_pred[pred_alive], character())

  # And the interval that does take the boundary year began on it -- so the
  # year resolves forward to the successor generation, never back.
  starts <- stats::setNames(flat$start_year, flat$polity_code)
  winner <- purrr::map2_chr(
    pairs$family,
    as.character(pairs$start_year_succ),
    \(fam, yr) {
      codes <- resolved[[yr]]
      codes[whep:::.polity_family(codes) == fam]
    }
  )
  expect_equal(unname(starts[winner]), pairs$start_year_succ)

  # The named successor is the winner except where its own family has a SECOND
  # interval starting the same year -- an upstream duplicate, 7 of the 8 being
  # a `superseded`/`retired` row the tie-break correctly passes over. Pinned as
  # an enumerated exception so the next one cannot hide inside a tolerance.
  expect_setequal(
    pairs$polity_code_succ[winner != pairs$polity_code_succ],
    c(
      "CAN-1866-1886",
      "CHN-1921-1945",
      "ETH-1907-1952",
      "F248-1920-1991",
      "GRC-1919-2025",
      "HUN-1938-1947",
      "MNE-1913-1918",
      "ROU-1940-2025"
    )
  )
})

test_that("2014 resolves to RUS-2014-2025, never RUS-1991-2014", {
  flat <- .ct_polities_flat()
  active <- whep:::.active_polities(flat, 2014)$polity_code
  expect_true("RUS-2014-2025" %in% active)
  expect_false("RUS-1991-2014" %in% active)
  # ...and the predecessor still owns every year up to its exclusive end.
  expect_true(
    "RUS-1991-2014" %in% whep:::.active_polities(flat, 2013)$polity_code
  )
  expect_false(
    "RUS-2014-2025" %in% whep:::.active_polities(flat, 2013)$polity_code
  )
})

test_that("overlapping intervals of one polity collapse to the later epoch", {
  overlapping <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "Q-1800-1900", 1800L, 1900L,
    "Q-1850-2000", 1850L, 2000L,
    "R-1800-2000", 1800L, 2000L
  )
  # Mid-interval: the later-starting epoch wins.
  expect_setequal(
    whep:::.active_polities(overlapping, 1860)$polity_code,
    c("Q-1850-2000", "R-1800-2000")
  )
  # On the later epoch's own start year: the exact-start epoch wins.
  expect_setequal(
    whep:::.active_polities(overlapping, 1850)$polity_code,
    c("Q-1850-2000", "R-1800-2000")
  )
  # Before it exists, the earlier epoch is the only candidate.
  expect_setequal(
    whep:::.active_polities(overlapping, 1840)$polity_code,
    c("Q-1800-1900", "R-1800-2000")
  )
  # PER-1825-1909 genuinely overlaps PER-1825-1884 upstream; one row, not two.
  flat <- .ct_polities_flat()
  per <- whep:::.active_polities(flat, 1850)$polity_code
  expect_equal(sum(whep:::.polity_family(per) == "PER"), 1L)
})

test_that("a hyphenated prefix is not mistaken for an epoch", {
  expect_equal(whep:::.polity_family("AZE-SSR-1920-1991"), "AZE-SSR")
  expect_equal(whep:::.polity_family("MMR-LWR-1852-1885"), "MMR-LWR")
  expect_equal(whep:::.polity_family("RUS-2014-2025"), "RUS")
  # `NNG-1949-1963` really ends in 1969: the code is never a date source.
  expect_equal(whep:::.polity_family("NNG-1949-1963"), "NNG")
  # A code carrying no epoch suffix is its own family.
  expect_equal(whep:::.polity_family(c("P1", "T_L")), c("P1", "T_L"))
})

# Adjacent epochs of ONE polity, end to end through the exported function.
# P-1800-1900 is the left unit square; P-1900-2000 doubles it eastward.
.adjacent_polities <- function() {
  sf::st_sf(
    polity_code = c("P-1800-1900", "P-1900-2000"),
    start_year = c(1800L, 1900L),
    end_year = c(1900L, 2000L),
    geometry = sf::st_sfc(
      .rect(0, 0, 100000, 100000),
      .rect(0, 0, 200000, 100000),
      crs = 6933
    )
  )
}

test_that("an adjacent-epoch boundary year lands wholly on the successor", {
  testthat::skip_if_not_installed("sf")
  res <- whep::build_constant_territory_series(
    data.frame(year = 1900L, polity_code = "P-1900-2000", value = 100),
    ref_year = 1900,
    polities = .adjacent_polities(),
    resolution = 10000,
    verbose = FALSE
  )
  # Under an inclusive `end_year` both epochs are active in 1900 and
  # `.assign_polity()` gives the western half to the DISSOLVED "P-1800-1900",
  # producing two rows of 50. One row, all of it, on the successor.
  expect_equal(nrow(res), 1L)
  expect_equal(res$target_polity_code, "P-1900-2000")
  expect_equal(res$value, 100, tolerance = 1e-6)
  expect_equal(res$imputed_share, 0, tolerance = 1e-9)
  expect_false("P-1800-1900" %in% res$target_polity_code)
})

test_that("a source reported on its own end_year is dissolved, not placed", {
  testthat::skip_if_not_installed("sf")
  expect_warning(
    res <- whep::build_constant_territory_series(
      data.frame(year = 1900L, polity_code = "P-1800-1900", value = 100),
      ref_year = 1950,
      polities = .adjacent_polities(),
      resolution = 10000,
      verbose = TRUE
    ),
    "no source polity"
  )
  expect_equal(nrow(res), 0L)
})

test_that("ref_year on the open end resolves end to end", {
  testthat::skip_if_not_installed("sf")
  # `.synthetic_polities()` ends its coverage in 2025, exactly as the shipped
  # vintage does. Under a uniformly exclusive read this call ABORTS with "no
  # polities ... active in ref_year = 2025", which is what DA-24 removes.
  open_end <- whep::build_constant_territory_series(
    .reported,
    ref_year = 2025,
    polities = .synthetic_polities(),
    resolution = 10000,
    verbose = FALSE
  )
  expect_setequal(open_end$target_polity_code, c("T_L", "T_R"))
  # ...and it agrees with a year safely inside the same intervals, so the open
  # end is the same territory rather than a differently-resolved one.
  inside <- whep::build_constant_territory_series(
    .reported,
    ref_year = 2000,
    polities = .synthetic_polities(),
    resolution = 10000,
    verbose = FALSE
  )
  expect_equal(open_end, inside)

  # A source reported ON the open end is placed, where a source reported on a
  # succession boundary is dissolved (the block below pins that complement).
  placed <- whep::build_constant_territory_series(
    data.frame(year = 2025L, polity_code = "T_L", value = 100),
    ref_year = 2025,
    polities = .synthetic_polities(),
    resolution = 10000,
    verbose = FALSE
  )
  expect_equal(sum(placed$covered), 100, tolerance = 1e-6)
})

test_that("a ref_year past the open end aborts, naming the covered range", {
  testthat::skip_if_not_installed("sf")
  expect_error(
    whep::build_constant_territory_series(
      .reported,
      ref_year = 2026,
      polities = .synthetic_polities(),
      resolution = 10000,
      verbose = FALSE
    ),
    "1850-2025"
  )
})

test_that("a reported source is never discarded by the same-polity tie-break", {
  testthat::skip_if_not_installed("sf")
  overlapping <- sf::st_sf(
    polity_code = c("P-1800-1900", "P-1850-2000"),
    start_year = c(1800L, 1850L),
    end_year = c(1900L, 2000L),
    geometry = sf::st_sfc(
      .rect(0, 0, 100000, 100000),
      .rect(0, 0, 200000, 100000),
      crs = 6933
    )
  )
  # "P-1800-1900" loses the target tie-break to "P-1850-2000" at 1860, but the
  # caller named it as the SOURCE, so its value must still be placed.
  res <- whep::build_constant_territory_series(
    data.frame(year = 1860L, polity_code = "P-1800-1900", value = 100),
    ref_year = 1860,
    polities = overlapping,
    resolution = 10000,
    verbose = FALSE
  )
  expect_equal(res$target_polity_code, "P-1850-2000")
  expect_equal(res$covered, 100, tolerance = 1e-6)
})
