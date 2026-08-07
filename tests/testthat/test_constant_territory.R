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
      "n_sources",
      "unallocated"
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
# `start_year`, `end_year` and the OPTIONAL `wiki_status`, so the geometry is
# dropped as a plain data frame.
#
# `.ct_polities_flat()` deliberately supplies only the three REQUIRED columns,
# so every block using it exercises the status-blind ordering a caller gets when
# it withholds `wiki_status`. `.ct_polities_status()` is the same table with the
# column, and the DA-29 blocks below compare the two.
# ---------------------------------------------------------------------------

.ct_polities_flat <- function() {
  .ct_polities_status()[, c("polity_code", "start_year", "end_year")]
}

.ct_polities_status <- function() {
  flat <- as.data.frame(whep::polities)
  flat[["geom"]] <- NULL
  flat[, c("polity_code", "start_year", "end_year", "wiki_status")]
}

# Winner per family at every year the shipped table covers.
.ct_resolve_domain <- function(flat) {
  years <- .ct_domain_years(flat)
  out <- lapply(years, \(yr) whep:::.active_polities(flat, yr)$polity_code)
  names(out) <- as.character(years)
  out
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
  # These eight polities each carry a SECOND interval ending on the domain end,
  # and opening them too would count the terminal year twice. Enumerated, as
  # C2's shared-start list is, so the next one cannot arrive silently. The
  # 740 -> 749 re-sync (#551) added the eighth, `CAN-1948-2025`, which is
  # exactly the silent growth this enumeration exists to catch.
  succeeded <- flat$polity_code[flat$end_year == domain_end & !open]
  expect_setequal(
    succeeded,
    c(
      "AGO-1816-2025",
      "ARG-1800-2025",
      "BLZ-1800-2025",
      "BRA-1800-2025",
      "CAN-1948-2025",
      "GRC-1919-2025",
      "IRQ-1921-2025",
      "ROU-1940-2025"
    )
  )
  # PR #662 adds one open interval without changing the eight succeeded ones:
  # 238 intervals end on the domain end and 230 are open, so the year test and
  # the successor test do NOT agree here and only the latter is right.
  expect_equal(sum(flat$end_year == domain_end), 230L + length(succeeded))

  # Upstream's own `successor` column is the independent witness: no live polity
  # ending on the domain end is without a successor's counterpart, and every
  # interval our data-derived predicate calls open is one `.open_polity_codes()`
  # also calls open. The two mechanisms must not drift apart.
  expect_equal(
    setdiff(flat$polity_code[open], whep:::.open_polity_codes()),
    character()
  )

  # Each of the eight really is succeeded: a later-starting interval of the
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
  # interval starting the same year -- an upstream duplicate. Nine such cases on
  # the 749-row table, up from eight on the 740-row one: #551 added
  # `CAN-1886-1949` beside the now-retired `CAN-1886-1948`. Pinned as an
  # enumerated exception so the next one cannot hide inside a tolerance.
  losers <- sort(unique(pairs$polity_code_succ[
    winner != pairs$polity_code_succ
  ]))
  expect_setequal(
    losers,
    c(
      "CAN-1866-1886",
      "CAN-1886-1949",
      "CHN-1921-1945",
      "ETH-1907-1952",
      "F248-1920-1991",
      "GRC-1919-2025",
      "HUN-1938-1947",
      "MNE-1913-1918",
      "ROU-1940-2025"
    )
  )
  # `flat` withholds `wiki_status`, so this is the STATUS-BLIND list, and two of
  # the nine losers are live rows that lose to a dead one: on a shared start
  # year nothing decides. That is the documented consequence of supplying only
  # the three required columns, pinned here so the two call styles cannot drift
  # apart unnoticed. The block below re-measures the same list WITH the column,
  # where both of these disappear.
  status <- stats::setNames(
    whep::polities$wiki_status,
    whep::polities$polity_code
  )
  expect_setequal(
    losers[!status[losers] %in% c("retired", "superseded")],
    c("CAN-1886-1949", "MNE-1913-1918")
  )
})

test_that("with `wiki_status`, no live interval loses its own start year", {
  # DA-29 re-measures the block above with the optional column supplied. The
  # list stays nine long, because each swap replaces a live loser with the dead
  # row it now beats -- but every remaining loser is `retired` or `superseded`,
  # i.e. a row the tie-break is SUPPOSED to pass over. Zero live losers is the
  # property; the enumeration is what makes a new one visible.
  flat <- .ct_polities_status()
  flat$family <- whep:::.polity_family(flat$polity_code)
  pairs <- merge(flat, flat, by = "family", suffixes = c("_pred", "_succ"))
  pairs <- pairs[pairs$end_year_pred == pairs$start_year_succ, ]
  expect_gt(nrow(pairs), 100)

  resolved <- purrr::map(unique(pairs$start_year_succ), \(yr) {
    whep:::.active_polities(flat, yr)$polity_code
  })
  names(resolved) <- as.character(unique(pairs$start_year_succ))
  winner <- purrr::map2_chr(
    pairs$family,
    as.character(pairs$start_year_succ),
    \(fam, yr) {
      codes <- resolved[[yr]]
      codes[whep:::.polity_family(codes) == fam]
    }
  )
  losers <- sort(unique(pairs$polity_code_succ[
    winner != pairs$polity_code_succ
  ]))
  expect_setequal(
    losers,
    c(
      "CAN-1866-1886",
      "CAN-1886-1948",
      "CHN-1921-1945",
      "ETH-1907-1952",
      "F248-1920-1991",
      "GRC-1919-2025",
      "HUN-1938-1947",
      "MNE-1913-1915",
      "ROU-1940-2025"
    )
  )
  status <- stats::setNames(
    whep::polities$wiki_status,
    whep::polities$polity_code
  )
  expect_equal(
    losers[!status[losers] %in% c("retired", "superseded")],
    character()
  )
  # The two that changed hands, named. Not implied by the set above: it would
  # still pass if the pair had swapped in the other direction.
  expect_false("CAN-1886-1949" %in% losers)
  expect_false("MNE-1913-1918" %in% losers)
})

test_that("`wiki_status` moves exactly 1,344 polity-years, and no more", {
  # The whole measured consequence of DA-29 on the shipped table, enumerated so
  # neither a shrink nor a growth can pass silently. `from` is what the
  # status-blind ordering resolves, `to` what the status-aware one does.
  blind <- .ct_resolve_domain(.ct_polities_flat())
  aware <- .ct_resolve_domain(.ct_polities_status())
  years <- .ct_domain_years(.ct_polities_flat())

  moved <- purrr::map(seq_along(years), \(i) {
    from <- setdiff(blind[[i]], aware[[i]])
    to <- setdiff(aware[[i]], blind[[i]])
    # A tie-break can only ever SWAP one interval of a family for another, so
    # the two sides must be the same size and the same families. If they are
    # not, the change has stopped being a tie-break.
    expect_equal(length(from), length(to))
    expect_setequal(
      whep:::.polity_family(from),
      whep:::.polity_family(to)
    )
    if (length(from) == 0L) {
      return(NULL)
    }
    # Paired by FAMILY, not by position: a year in which two families both flip
    # would otherwise zip one family's `from` to another's `to`. The dedupe
    # guarantees at most one interval per family per side, so the family is a
    # key on both.
    tibble::tibble(
      year = years[i],
      from = from[order(whep:::.polity_family(from))],
      to = to[order(whep:::.polity_family(to))]
    )
  }) |>
    purrr::list_rbind()

  expect_equal(nrow(moved), 1344L)
  pairs <- dplyr::summarise(
    moved,
    n = dplyr::n(),
    first = min(.data$year),
    last = max(.data$year),
    .by = c("from", "to")
  )
  pairs <- dplyr::arrange(pairs, .data$from)
  expect_equal(
    paste(pairs$from, pairs$to, pairs$n, pairs$first, pairs$last),
    c(
      "CAN-1886-1948 CAN-1886-1949 62 1886 1947",
      "IDN-1800-1889 IDN-1800-1945 89 1800 1888",
      "MNE-1913-1915 MNE-1913-1918 2 1913 1914",
      "RAFR-1850-2021 RAFR-1850-2025 171 1850 2020",
      "RASI-1850-2021 RASI-1850-2025 171 1850 2020",
      "REUR-1850-2021 REUR-1850-2025 171 1850 2020",
      "RLAM-1850-2013 RLAM-1850-2025 163 1850 2012",
      "RNAM-1850-2021 RNAM-1850-2025 171 1850 2020",
      "ROCE-1850-2021 ROCE-1850-2025 171 1850 2020",
      "ROW-1850-2023 ROW-1850-2025 173 1850 2022"
    )
  )
  # Every interval given up is dead and every interval gained is live. That is
  # the DIRECTION of the fix, and it is what an inverted tie-break breaks.
  status <- stats::setNames(
    whep::polities$wiki_status,
    whep::polities$polity_code
  )
  expect_true(all(status[pairs$from] %in% c("retired", "superseded")))
  expect_false(any(status[pairs$to] %in% c("retired", "superseded")))
})

test_that("only Montenegro moves territory; the rest is relabelling", {
  skip_if_not_installed("sf")

  # S-A10's rule applied to a tie-break: measure VALUES, not row counts. Of the
  # ten pairs DA-29 swaps, seven are reporting aggregates whose dead side has NO
  # POLYGON AT ALL, two are `st_equals()` duplicates, and exactly one moves
  # ground.
  area_mha <- function(code) {
    g <- whep::get_polity_geometries(code)
    if (sf::st_is_empty(g)) {
      return(NA_real_)
    }
    as.numeric(sf::st_area(sf::st_make_valid(g))) / 1e10
  }
  equal_polygons <- function(a, b) {
    ga <- whep::get_polity_geometries(a)
    gb <- whep::get_polity_geometries(b)
    length(sf::st_equals(ga, gb)[[1]]) > 0L
  }

  # The one that moves territory: 1913 and 1914 gain 0.5970 Mha of Montenegro.
  expect_equal(area_mha("MNE-1913-1915"), 0.9923, tolerance = 1e-4)
  expect_equal(area_mha("MNE-1913-1918"), 1.5893, tolerance = 1e-4)
  expect_false(equal_polygons("MNE-1913-1915", "MNE-1913-1918"))
  expect_equal(
    1 - area_mha("MNE-1913-1915") / area_mha("MNE-1913-1918"),
    0.3756,
    tolerance = 1e-3
  )
  status <- .ct_polities_status()
  expect_equal(
    whep:::.active_polities(status, 1913)$polity_code[
      whep:::.polity_family(
        whep:::.active_polities(status, 1913)$polity_code
      ) ==
        "MNE"
    ],
    "MNE-1913-1918"
  )

  # The two that are pure relabelling: identical polygons, so no quantity can
  # cross a boundary however the tie resolves.
  expect_true(equal_polygons("CAN-1886-1948", "CAN-1886-1949"))
  expect_true(equal_polygons("IDN-1800-1889", "IDN-1800-1945"))
  expect_equal(area_mha("CAN-1886-1948"), area_mha("CAN-1886-1949"))
  expect_equal(area_mha("IDN-1800-1889"), area_mha("IDN-1800-1945"))
  # PR #662 repairs the shared Canadian geometry; equality is the behavioural
  # property, while the refreshed snapshot's common area is 950.1444 Mha.
  expect_equal(area_mha("CAN-1886-1948"), 950.1444, tolerance = 1e-4)

  # The seven reporting aggregates: the dead side carries no polygon, so it
  # could never have received data. `build_constant_territory_series()` drops
  # empty geometries before resolving, so its own answer never depended on this.
  buckets <- c("RAFR", "RASI", "REUR", "RLAM", "RNAM", "ROCE", "ROW")
  dead <- c(
    "RAFR-1850-2021",
    "RASI-1850-2021",
    "REUR-1850-2021",
    "RLAM-1850-2013",
    "RNAM-1850-2021",
    "ROCE-1850-2021",
    "ROW-1850-2023"
  )
  expect_setequal(whep:::.polity_family(dead), buckets)
  expect_true(all(is.na(purrr::map_dbl(dead, area_mha))))
  expect_true(all(purrr::map_lgl(
    paste0(buckets, "-1850-2025"),
    \(code) !is.na(area_mha(code))
  )))
})

test_that("status ranks BELOW the start year, never above it", {
  # The placement DA-29 chose, and the one it rejected. A later start is a more
  # specific epoch; letting status outrank it would resurrect a superseded WIDER
  # period over the narrower one that replaced it -- C2's defect in reverse.
  # These two are the whole population where the two placements disagree, so
  # this block is exactly the difference between them.
  flat <- .ct_polities_status()
  at <- function(yr, fam) {
    codes <- whep:::.active_polities(flat, yr)$polity_code
    codes[whep:::.polity_family(codes) == fam]
  }

  # `BLX-1921-1999` is retired and `BLX-1850-1999` is draft, but the retired one
  # starts 71 years later, so it keeps the year.
  expect_equal(at(1950, "BLX"), "BLX-1921-1999")
  expect_equal(at(1998, "BLX"), "BLX-1921-1999")
  # ...and before it exists, the earlier interval answers, as always.
  expect_equal(at(1900, "BLX"), "BLX-1850-1999")

  # `IDN-1889-1945` is superseded and `IDN-1800-1945` is draft; the later start
  # still wins. The SAME family flips on its shared start year (1800-1888),
  # which is what makes this a placement test and not a status test.
  expect_equal(at(1900, "IDN"), "IDN-1889-1945")
  expect_equal(at(1800, "IDN"), "IDN-1800-1945")
  expect_equal(at(1888, "IDN"), "IDN-1800-1945")

  # And the start year still outranks everything below it: on `MNE`'s shared
  # start the live row wins, but a year inside the later epoch resolves to the
  # later epoch whatever its status.
  expect_equal(at(1914, "MNE"), "MNE-1913-1918")
  expect_equal(at(1916, "MNE"), "MNE-1913-1918")
})

test_that("`wiki_status` is optional and only ever breaks a tie", {
  flat <- .ct_polities_flat()
  status <- .ct_polities_status()
  expect_false("wiki_status" %in% names(flat))

  # WHICH YEARS an interval covers is never a function of status: the two call
  # styles resolve the same FAMILIES at every year, and differ only in which
  # interval of a family answers.
  blind <- .ct_resolve_domain(flat)
  aware <- .ct_resolve_domain(status)
  expect_equal(lengths(blind), lengths(aware))
  expect_true(all(purrr::map2_lgl(blind, aware, \(a, b) {
    setequal(whep:::.polity_family(a), whep:::.polity_family(b))
  })))

  # A table with the column but nothing dead in it resolves exactly as the
  # three-column table does, which is what "the key is constant" means.
  all_live <- status
  all_live$wiki_status <- "draft"
  expect_equal(.ct_resolve_domain(all_live), blind)

  # An unknown status is LIVE, not dead. Reading it as dead would make the
  # optional column mandatory by the back door.
  expect_true(whep:::.polity_is_live(NA_character_))
  expect_equal(
    whep:::.polity_is_live(c("draft", "reviewed", NA, "brand-new")),
    rep(TRUE, 4)
  )
  unknown <- status
  unknown$wiki_status <- NA_character_
  expect_equal(.ct_resolve_domain(unknown), blind)
})

test_that("`retired` and `superseded` are one kind of dead", {
  expect_equal(
    whep:::.polity_is_live(c("retired", "superseded")),
    c(FALSE, FALSE)
  )
  # Swapping one dead label for the other cannot change a single resolution.
  # Ranking them apart is the mutation this refutes, and it is checked over the
  # whole domain rather than on the two rows that happen to differ today.
  status <- .ct_polities_status()
  swapped <- status
  flip <- c(retired = "superseded", superseded = "retired")
  swapped$wiki_status <- dplyr::coalesce(
    unname(flip[status$wiki_status]),
    status$wiki_status
  )
  expect_gt(sum(swapped$wiki_status != status$wiki_status), 0L)
  expect_equal(.ct_resolve_domain(swapped), .ct_resolve_domain(status))
})

test_that("the exported series keeps `wiki_status` when it is given one", {
  skip_if_not_installed("sf")

  # `build_constant_territory_series()` narrows the table before resolving it,
  # and dropping the optional column there would silently downgrade the DEFAULT
  # `get_polity_geometries()` table to the status-blind ordering the docs
  # reserve for a caller who withholds it.
  #
  # The tie has to be on the TARGET side to reach the exported function: the
  # SOURCE set is restricted to the codes the caller reported before the year is
  # resolved, precisely so a tie can never discard a reported code.
  #
  # Two intervals of "T" share the start year 1913: the retired one is the left
  # half of the square, the live one the whole square. `SRC` reports 100 over
  # the whole square in 1900, so the live target collects all of it and the
  # retired half only half -- a real 50% value difference, not a relabelling.
  polities <- sf::st_sf(
    polity_code = c("SRC", "T-1913-1915", "T-1913-1918"),
    start_year = c(1850L, 1913L, 1913L),
    end_year = c(1910L, 1915L, 1918L),
    wiki_status = c("draft", "retired", "draft"),
    geometry = sf::st_sfc(
      .rect(0, 0, 100000, 100000),
      .rect(0, 0, 50000, 100000),
      .rect(0, 0, 100000, 100000),
      crs = 6933
    )
  )
  reported <- data.frame(year = 1900L, polity_code = "SRC", value = 100)
  run <- function(p) {
    build_constant_territory_series(
      reported,
      ref_year = 1914,
      polities = p,
      resolution = 10000,
      verbose = FALSE
    )
  }
  aware <- run(polities)
  expect_equal(aware$target_polity_code, "T-1913-1918")
  expect_equal(aware$value, 100)
  expect_equal(aware$covered, 100)

  # Withhold the column and the retired half wins the year, so the series
  # reports half the mass under a code upstream has already replaced.
  blind <- run(polities[, c("polity_code", "start_year", "end_year")])
  expect_equal(blind$target_polity_code, "T-1913-1915")
  expect_equal(blind$value, 50)
  expect_equal(blind$covered, 50)
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

test_that("a starved (zero-weight) source is not smeared and is accounted", {
  # SRC reports value 100, but the covariate density is zero everywhere over
  # SRC's own extent (x < 100k) and positive only over the uncovered strip
  # (x >= 100k). SRC therefore has cells but zero gridded weight: it cannot be
  # placed. Its value must NOT inflate the donor intensity for the gap strip,
  # and it must be reported via `unallocated`.
  cov_fn <- function(centroids, year) {
    x <- sf::st_coordinates(centroids)[, 1]
    ifelse(x >= 100000, 1, 0)
  }
  expect_warning(
    res <- build_constant_territory_series(
      .reported,
      ref_year = 2000,
      polities = .synthetic_polities(),
      covariate = cov_fn,
      resolution = 10000,
      verbose = TRUE
    ),
    "smaller than the grid resolution"
  )
  res <- res[order(res$target_polity_code), ]

  # No placeable source -> nothing is covered and nothing is smeared.
  expect_equal(sum(res$covered), 0, tolerance = 1e-9)
  expect_equal(sum(res$imputed), 0, tolerance = 1e-9)
  expect_equal(sum(res$value), 0, tolerance = 1e-9)
  expect_equal(res$n_sources, c(0, 0))

  # The unallocatable value is accounted for, not silently dropped.
  expect_equal(unique(res$unallocated), 100, tolerance = 1e-6)
})

test_that("numeric polity_code keys index by name, not position", {
  # Same synthetic geometry, but with numeric polity codes and the source (code
  # 30) listed LAST, so name- vs position-indexing give different answers.
  polys <- sf::st_sf(
    polity_code = c(10, 20, 30),
    start_year = c(1990L, 1990L, 1850L),
    end_year = c(2025L, 2025L, 1950L),
    geometry = sf::st_sfc(
      .rect(0, 0, 50000, 100000), # code 10  -> T_L
      .rect(50000, 0, 150000, 100000), # code 20  -> T_R
      .rect(0, 0, 100000, 100000), # code 30  -> SRC
      crs = 6933
    )
  )
  reported <- data.frame(year = 1900L, polity_code = 30, value = 100)
  res <- build_constant_territory_series(
    reported,
    ref_year = 2000,
    polities = polys,
    resolution = 10000,
    verbose = FALSE
  )
  res <- res[order(res$target_polity_code), ]

  # Same expectations as the character-keyed uniform-density case: name-based
  # indexing attaches the right intensity regardless of source ordering.
  tl <- res[res$target_polity_code == "10", ]
  tr <- res[res$target_polity_code == "20", ]
  expect_equal(tl$value, 50, tolerance = 1e-6)
  expect_equal(tr$covered, 50, tolerance = 1e-6)
  expect_equal(tr$value, 100, tolerance = 1e-6)
  expect_equal(sum(res$covered), 100, tolerance = 1e-6)
})

test_that("a polity is not active in its exclusive end year", {
  # `end_year` is EXCLUSIVE (see [polities]), so a polity and its successors
  # must not both be active in the hand-over year. Reading it inclusively made
  # them overlap -- on the shipped snapshot, 238 polities carry a polygon in
  # 1993 that way, Czechoslovakia sitting on top of Czechia and Slovakia --
  # and each grid cell goes to exactly one target, so the dissolved
  # predecessor captured the cells its successors should have received (#550).
  #
  # OLD covers the whole rectangle and ends in 1950; NEW_L and NEW_R split it
  # and start in 1950. `ref_year` is the hand-over year 1950, so the targets
  # must be exactly the two successors.
  polys <- sf::st_sf(
    polity_code = c("OLD", "NEW_L", "NEW_R"),
    start_year = c(1850L, 1950L, 1950L),
    end_year = c(1950L, 2025L, 2025L),
    geometry = sf::st_sfc(
      .rect(0, 0, 100000, 100000),
      .rect(0, 0, 50000, 100000),
      .rect(50000, 0, 100000, 100000),
      crs = 6933
    )
  )
  reported <- data.frame(year = 1900L, polity_code = "OLD", value = 100)
  res <- build_constant_territory_series(
    reported,
    ref_year = 1950,
    polities = polys,
    resolution = 10000,
    verbose = FALSE
  )
  res <- res[order(res$target_polity_code), ]

  expect_equal(res$target_polity_code, c("NEW_L", "NEW_R"))
  expect_equal(res$value, c(50, 50), tolerance = 1e-6)
  expect_equal(sum(res$covered), 100, tolerance = 1e-6)
})
