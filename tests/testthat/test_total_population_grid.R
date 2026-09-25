# build_total_population_grid(): UN WPP totals downscaled by HYDE popc shares.
#
# Every fixture is injected (`data$wpp`, `data$hyde`), so nothing here reads
# WHEP_HYDE_DIR or the WPP download, except the one test that writes its own
# HYDE-style archives to a temp directory to exercise the real read path.
#
# Area codes: 203 Spain and 68 France are their own polity_area_code buckets;
# 276 (Sudan) and 277 (South Sudan) are grid codes that fold into bucket 206,
# which is where read_wpp_population() keys both SDN and SSD.

.tp_cell_polity <- function() {
  tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~polity_frac,
    -3.75, 40.25, 203L,       1,
    -0.25, 42.75, 203L,       0.6,
    -0.25, 42.75, 68L,        0.4,
    2.25,  46.25, 68L,        1
  )
}

# Snapshots 2000 and 2010. Spain's pattern moves towards the border cell.
.tp_hyde <- function() {
  tibble::tribble(
    ~lon,  ~lat,  ~year, ~popc,
    -3.75, 40.25, 2000L, 3000,
    -0.25, 42.75, 2000L, 1000,
    2.25,  46.25, 2000L, 5000,
    -3.75, 40.25, 2010L, 2000,
    -0.25, 42.75, 2010L, 2000,
    2.25,  46.25, 2010L, 5000
  )
}

.tp_wpp <- function(years = 2000:2015) {
  tidyr::expand_grid(year = years, area_code = c(203L, 68L)) |>
    dplyr::mutate(
      population = dplyr::if_else(.data$area_code == 203L, 4e7, 6e7) +
        1e5 * (.data$year - 2000L)
    )
}

# `...` replaces whole inputs by name. Not utils::modifyList(), which would
# recurse into the tibbles and merge their columns.
.tp_build <- function(years, ...) {
  data <- list(
    cell_polity = .tp_cell_polity(),
    wpp = .tp_wpp(),
    hyde = .tp_hyde()
  )
  overrides <- list(...)
  data[names(overrides)] <- overrides
  whep::build_total_population_grid(years = years, data = data)
}

# Spain's 2000-snapshot share of each polycell: the border cell's 1000 is
# split 0.6 / 0.4, so Spain holds 3000 + 600 and France 400 + 5000.
.tp_es_share_2000 <- c(3000, 600) / 3600
.tp_es_share_2010 <- c(2000, 1200) / 3200

testthat::test_that("the share plan classifies exact, interpolated and held", {
  plan <- whep:::.tp_share_plan(c(2000L, 2004L, 2010L, 2013L), c(2000L, 2010L))
  testthat::expect_equal(
    plan$status,
    c("exact", "interpolated", "exact", "held")
  )
  testthat::expect_equal(plan$lower, c(2000L, 2000L, 2010L, 2010L))
  testthat::expect_equal(plan$upper, c(2000L, 2010L, 2010L, 2010L))
  testthat::expect_equal(plan$weight, c(0, 0.4, 0, 0))
})

testthat::test_that("a year before the first snapshot is refused, not extrapolated", {
  testthat::expect_error(
    .tp_build(1995L),
    class = "whep_total_population_uncovered"
  )
})

testthat::test_that("a year the WPP table does not carry is refused", {
  testthat::expect_error(
    .tp_build(2004L, wpp = .tp_wpp(2000:2003)),
    class = "whep_total_population_uncovered"
  )
})

testthat::test_that("every country's polycells sum to its WPP total", {
  years <- c(2000L, 2004L, 2010L, 2013L)
  out <- suppressMessages(.tp_build(years))
  totals <- out |>
    dplyr::summarise(
      population = sum(.data$population),
      .by = c("year", "area_code")
    ) |>
    dplyr::inner_join(
      .tp_wpp(),
      by = c("year", "area_code"),
      suffix = c("", "_wpp")
    )
  # Non-vacuous: both countries in all four years reached the comparison.
  testthat::expect_equal(nrow(totals), 8L)
  testthat::expect_equal(
    totals$population,
    totals$population_wpp,
    tolerance = 1e-12
  )
})

testthat::test_that("the share, not the count, is interpolated linearly", {
  out <- .tp_build(2004L) |>
    dplyr::filter(.data$area_code == 203L) |>
    dplyr::arrange(.data$lon)
  wpp_es <- 4e7 + 4e5
  share <- 0.6 * .tp_es_share_2000 + 0.4 * .tp_es_share_2010
  testthat::expect_equal(out$population, wpp_es * share, tolerance = 1e-12)
})

testthat::test_that("years past the last snapshot hold its pattern, audibly", {
  testthat::expect_message(
    held <- .tp_build(2013L),
    "past the last HYDE snapshot"
  )
  es <- held |>
    dplyr::filter(.data$area_code == 203L) |>
    dplyr::arrange(.data$lon)
  testthat::expect_equal(
    es$population,
    (4e7 + 1.3e6) * .tp_es_share_2010,
    tolerance = 1e-12
  )
  coverage <- attr(held, "coverage")
  testthat::expect_equal(coverage$held, 2013L)
  testthat::expect_equal(coverage$held_at, 2010L)
})

testthat::test_that("the coverage attribute records the basis and the plan", {
  out <- suppressMessages(.tp_build(c(2000L, 2005L, 2012L)))
  coverage <- attr(out, "coverage")
  testthat::expect_equal(coverage$population_basis, "total")
  testthat::expect_equal(coverage$level_basis, "total_population")
  testthat::expect_equal(coverage$pattern_variable, "popc")
  testthat::expect_equal(coverage$exact, 2000L)
  testthat::expect_equal(coverage$interpolated, 2005L)
  testthat::expect_equal(coverage$held, 2012L)
  testthat::expect_equal(coverage$snapshots_used, c(2000L, 2010L))
  testthat::expect_equal(nrow(coverage$unplaced), 0L)
})

testthat::test_that("a border cell keeps each country's own level", {
  # The border cell is one lon/lat carrying two rows, one per country, each
  # levelled to its own WPP total: summing them and re-splitting by
  # polity_frac would blend Spain's and France's levels.
  out <- .tp_build(2000L)
  border <- dplyr::filter(out, .data$lon == -0.25)
  testthat::expect_setequal(border$area_code, c(203L, 68L))
  testthat::expect_equal(
    border$population[border$area_code == 203L],
    4e7 * 600 / 3600
  )
  testthat::expect_equal(
    border$population[border$area_code == 68L],
    6e7 * 400 / 5400
  )
})

testthat::test_that("grid codes folded into one WPP bucket share its total", {
  # 2012, after South Sudan's 2011 independence, so both grid codes are
  # polities that existed in the row's year.
  cell_polity <- tibble::tribble(
    ~lon,  ~lat, ~area_code,
    30.25, 15.25, 276L,
    31.25, 7.25,  277L
  )
  hyde <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~popc,
    30.25, 15.25, 2012L, 3000,
    31.25, 7.25,  2012L, 1000
  )
  wpp <- tibble::tibble(year = 2012L, area_code = 206L, population = 4e7)
  out <- whep::build_total_population_grid(
    years = 2012L,
    data = list(cell_polity = cell_polity, wpp = wpp, hyde = hyde)
  )
  testthat::expect_equal(
    out$population[match(c(276L, 277L), out$area_code)],
    c(3e7, 1e7)
  )
})

testthat::test_that("a WPP total with no HYDE pattern is reported, not dropped", {
  wpp <- dplyr::bind_rows(
    .tp_wpp(2000L),
    tibble::tibble(year = 2000L, area_code = 132L, population = 3e5)
  )
  testthat::expect_warning(
    out <- .tp_build(2000L, wpp = wpp),
    "no HYDE population pattern"
  )
  unplaced <- attr(out, "coverage")$unplaced
  testthat::expect_equal(unplaced$bucket, 132L)
  testthat::expect_equal(unplaced$wpp_population, 3e5)
})

testthat::test_that("a country present at one snapshot only is renormalised, loudly", {
  # France has no HYDE population at 2010, so its 2005 shares sum to 0.5
  # before renormalisation.
  hyde <- dplyr::filter(
    .tp_hyde(),
    !(.data$year == 2010L & .data$lon > -1)
  )
  testthat::expect_warning(
    out <- .tp_build(2005L, hyde = hyde),
    "renormalised"
  )
  fr <- dplyr::filter(out, .data$area_code == 68L)
  testthat::expect_equal(sum(fr$population), 6e7 + 5e5)
  testthat::expect_equal(nrow(attr(out, "coverage")$share_gaps), 1L)
})

# Write one HYDE-style "{year}AD_pop.zip" holding a 12 x 6 fine-cell popc grid
# (two 0.5-degree blocks at the north-west corner, each block's 36 cells set
# to one of `values`) beside an all-zero urbc grid, so a read of the wrong
# member shows up as zero population.
.tp_write_hyde_zip <- function(dir, year, values) {
  header <- c(
    "ncols 12",
    "nrows 6",
    "xllcorner -180.0",
    "yllcorner -90.0",
    "cellsize 0.0833333",
    "NODATA_value -9999.0"
  )
  rows <- rep(paste(rep(values, each = 6), collapse = " "), 6)
  asc <- c(
    file.path(dir, paste0("popc_", year, "AD.asc")),
    file.path(dir, paste0("urbc_", year, "AD.asc"))
  )
  writeLines(c(header, rows), asc[1])
  writeLines(c(header, rep(paste(rep(0, 12), collapse = " "), 6)), asc[2])
  withr::with_dir(
    dir,
    zip::zip(paste0(year, "AD_pop.zip"), basename(asc))
  )
  unlink(asc)
}

testthat::test_that("the popc pattern is read from real HYDE-style archives", {
  dir <- withr::local_tempdir()
  .tp_write_hyde_zip(dir, 2000L, c(1, 3))
  .tp_write_hyde_zip(dir, 2010L, c(1, 1))
  cell_polity <- tibble::tribble(
    ~lon,    ~lat,  ~area_code,
    -179.75, 89.75, 203L,
    -179.25, 89.75, 203L
  )
  wpp <- tibble::tibble(year = 2005L, area_code = 203L, population = 800)
  out <- whep::build_total_population_grid(
    years = 2005L,
    hyde_dir = dir,
    data = list(cell_polity = cell_polity, wpp = wpp)
  ) |>
    dplyr::arrange(.data$lon)
  # Shares 1/4, 3/4 at 2000 and 1/2, 1/2 at 2010 give 3/8, 5/8 at 2005: the
  # popc members were read, not the all-zero urbc ones beside them.
  testthat::expect_equal(out$population, 800 * c(3, 5) / 8)
  testthat::expect_equal(
    attr(out, "coverage")$snapshots_available,
    c(2000L, 2010L)
  )
})

testthat::test_that("years are required and whole", {
  testthat::expect_error(
    whep::build_total_population_grid(
      data = list(cell_polity = .tp_cell_polity())
    ),
    "years"
  )
  testthat::expect_error(.tp_build(2000.5), "whole")
})

testthat::test_that("build_total_population_grid example fixture is schema-complete", {
  out <- whep::build_total_population_grid(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "area_code", "year", "population", "reporting_polity_code")
  )
  pointblank::expect_col_vals_gt(out, "population", 0)
})
