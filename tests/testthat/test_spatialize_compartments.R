# S-A5 -- the anti-centroid guard.
#
# `.normalize_country_grid()` used to default a missing share to
# `cell_area_frac = 1`. The deployed `spatialize-country-grid` pin carries only
# (lon, lat, area_code), so a border cell went WHOLLY to one polity and every
# conservation check still passed, because a whole-cell share still sums the
# national total correctly (EA4). Turning that default into an abort is what
# makes the centroid convention mechanically detectable.

# A centroid-resolved crosswalk: the exact shape of the deployed pin.
.scg_centroid_grid <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code,
    0.25, 50.25, 1L,
    0.75, 50.25, 2L
  )
}

.scg_landuse_args <- function(country_grid) {
  list(
    country_areas = tibble::tibble(
      year = 2000L,
      area_code = 1L,
      item_prod_code = 15L,
      harvested_area_ha = 100
    ),
    crop_patterns = tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      item_prod_code = 15L,
      harvest_fraction = 1
    ),
    gridded_cropland = tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      year = 2000L,
      cropland_ha = 1000
    ),
    country_grid = country_grid
  )
}

.scg_livestock_args <- function(country_grid) {
  list(
    livestock_data = tibble::tibble(
      year = 2000L,
      area_code = 1L,
      species_group = "cattle",
      heads = 100
    ),
    gridded_pasture = tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      year = 2000L,
      pasture_ha = 100,
      rangeland_ha = 0
    ),
    gridded_cropland = tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      year = 2000L,
      cropland_ha = 100
    ),
    country_grid = country_grid
  )
}

testthat::test_that("a centroid-resolved grid is refused, not defaulted to 1", {
  testthat::expect_error(
    whep:::.normalize_country_grid(.scg_centroid_grid()),
    "carries no polity share"
  )
  testthat::expect_error(
    whep:::.normalize_country_grid(.scg_centroid_grid()),
    "cell_area_frac"
  )
  # Declaring the whole-cell convention is still allowed; only inferring it is
  # not, so the abort cannot be satisfied by accident.
  declared <- dplyr::mutate(.scg_centroid_grid(), cell_area_frac = 1)
  testthat::expect_equal(
    whep:::.normalize_country_grid(declared)$cell_area_frac,
    c(1, 1)
  )
})

testthat::test_that("both spatialization engines refuse a centroid grid", {
  # The regression test S-A5 names: the centroid crosswalk must fail at the
  # public entry points, not only in the helper.
  testthat::expect_error(
    do.call(
      whep::build_gridded_landuse,
      .scg_landuse_args(
        .scg_centroid_grid()
      )
    ),
    "carries no polity share"
  )
  testthat::expect_error(
    do.call(
      whep::build_gridded_livestock,
      .scg_livestock_args(
        .scg_centroid_grid()
      )
    ),
    "carries no polity share"
  )
  # Both run once the share is declared, so the abort is about the share and
  # not about some other property of the fixture.
  declared <- dplyr::mutate(.scg_centroid_grid(), cell_area_frac = 1)
  testthat::expect_equal(
    sum(
      do.call(
        whep::build_gridded_landuse,
        .scg_landuse_args(declared)
      )$rainfed_ha
    ),
    100
  )
  testthat::expect_equal(
    sum(
      do.call(
        whep::build_gridded_livestock,
        .scg_livestock_args(declared)
      )$heads
    ),
    100
  )
})

testthat::test_that("a land fraction is refused, never read as a share", {
  # AM-5 risk 7. `landfrac` was IN the alias list. It is the LPJmL/GADM land
  # fraction: one value per cell, identical for every polity in it, so reading
  # it as a share hands each polity of a border cell the same fraction and the
  # cell is delivered once per polity. Aborting on a MISSING share is not
  # enough if a different quantity can still be guessed at.
  grid <- dplyr::mutate(.scg_centroid_grid(), landfrac = c(0.4, 0.4))
  testthat::expect_error(
    whep:::.normalize_country_grid(grid),
    "not a polity share"
  )
  testthat::expect_error(whep:::.normalize_country_grid(grid), "landfrac")

  # The same grid carrying a real share is accepted, and the share is the one
  # that is used -- not the land fraction sitting beside it.
  ok <- whep:::.normalize_country_grid(
    dplyr::mutate(grid, polity_frac = c(0.25, 0.75))
  )
  testthat::expect_equal(ok$cell_area_frac, c(0.25, 0.75))

  # Every other land-fraction spelling is refused too, so one more producer
  # name cannot slip through as a share.
  purrr::walk(
    c("land_frac", "land_fraction", "cell_land_frac", "icwtr"),
    \(nm) {
      other <- .scg_centroid_grid()
      other[[nm]] <- c(0.4, 0.4)
      testthat::expect_error(
        whep:::.normalize_country_grid(other),
        "not a polity share",
        info = nm
      )
    }
  )
})

testthat::test_that("every accepted alias is honoured, with a fixed precedence", {
  purrr::walk(c("polity_frac", "area_frac", "country_frac"), \(nm) {
    grid <- .scg_centroid_grid()
    grid[[nm]] <- c(0.25, 0.75)
    testthat::expect_equal(
      whep:::.normalize_country_grid(grid)$cell_area_frac,
      c(0.25, 0.75),
      info = nm
    )
  })
  # An explicit `cell_area_frac` wins over any other spelling present.
  both <- .scg_centroid_grid()
  both$polity_frac <- c(0.1, 0.9)
  both$cell_area_frac <- c(0.25, 0.75)
  testthat::expect_equal(
    whep:::.normalize_country_grid(both)$cell_area_frac,
    c(0.25, 0.75)
  )
})

testthat::test_that("an NA or out-of-range share is refused", {
  # `NA` used to become 1 -- the same whole-cell gift as a missing column, one
  # row at a time.
  na_grid <- dplyr::mutate(
    .scg_centroid_grid(),
    cell_area_frac = c(0.5, NA_real_)
  )
  testthat::expect_error(
    whep:::.normalize_country_grid(na_grid),
    "used to become 1"
  )

  # An absolute area substituted for a normalised weight: the substitution C5
  # existed to detect, caught here by the range instead of by a 11% signal.
  area_grid <- dplyr::mutate(
    .scg_centroid_grid(),
    cell_area_frac = c(0.5, 3e5)
  )
  testthat::expect_error(
    whep:::.normalize_country_grid(area_grid),
    "absolute area"
  )
  neg <- dplyr::mutate(.scg_centroid_grid(), polity_frac = c(-0.2, 0.5))
  testthat::expect_error(
    whep:::.normalize_country_grid(neg),
    "outside"
  )

  # Float noise is still clamped rather than refused, so a measured producer
  # is not rejected for a 1e-16 overshoot.
  noisy <- dplyr::mutate(
    .scg_centroid_grid(),
    cell_area_frac = c(1 + 1e-12, -1e-12)
  )
  testthat::expect_equal(
    whep:::.normalize_country_grid(noisy)$cell_area_frac,
    c(1, 0)
  )
})

testthat::test_that("a compartment join refuses to degrade to lon/lat", {
  # S-A6. `intersect()` alone silently drops a missing compartment id and joins
  # on (lon, lat), which across a polycell grid is many-to-many.
  allocated <- tibble::tibble(
    polycell_id = "a",
    area_code = 1L,
    lon = 0.25,
    lat = 50.25
  )
  full <- dplyr::mutate(allocated, rf_capacity = 1)
  testthat::expect_equal(
    whep:::.compartment_join_cols(allocated, full, "allocated", "capacity"),
    c("polycell_id", "area_code", "lon", "lat")
  )
  testthat::expect_error(
    whep:::.compartment_join_cols(
      allocated,
      dplyr::select(full, -"polycell_id"),
      "allocated",
      "capacity"
    ),
    "many-to-many"
  )
  testthat::expect_error(
    whep:::.compartment_join_cols(
      allocated,
      dplyr::select(full, -"area_code"),
      "allocated",
      "capacity"
    ),
    "area_code"
  )
})

# Year resolution for a time-varying country grid.
#
# `.filter_country_grid_year()` is the compartment path's epoch resolver. It is
# dead for every crosswalk shipped today (none carries validity columns) and
# goes live the moment a producer emits `start_year`/`end_year`, so the
# convention has to be pinned before that happens, not after.
#
# Convention (DA-24): start bound INCLUSIVE, end bound EXCLUSIVE at a
# succession and INCLUSIVE at the open end.

# Two adjacent epochs of one polity in one cell, under each accepted column
# naming. `RUS-1991-2014` / `RUS-2014-2025` is the real shape being modelled.
.scg_epochs <- function(start_col, end_col) {
  grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_code, ~from, ~to,
    0.25, 0.25, 1L, "RUS-1991-2014", 1991L, 2014L,
    0.25, 0.25, 1L, "RUS-2014-2025", 2014L, 2025L
  )
  names(grid)[names(grid) == "from"] <- start_col
  names(grid)[names(grid) == "to"] <- end_col
  grid
}

.scg_column_pairs <- function() {
  list(
    c("valid_from", "valid_to"),
    c("start_year", "end_year"),
    c("from_year", "to_year")
  )
}

test_that("every validity-column naming is recognised as dynamic", {
  dynamic <- purrr::map_lgl(.scg_column_pairs(), \(pair) {
    whep:::.country_grid_is_dynamic(.scg_epochs(pair[[1L]], pair[[2L]]))
  })
  expect_true(all(dynamic))
  # Each validity column ALONE must also mark the grid dynamic. Testing only
  # matched pairs lets one name fall out of the list undetected, and a grid
  # carrying an end bound the resolver does not see is returned whole for every
  # year -- every polity live in every year, silently.
  alone <- purrr::map_lgl(
    c(unlist(.scg_column_pairs()), "year"),
    \(col) {
      grid <- tibble::tibble(lon = 0.25, lat = 0.25, area_code = 1L, x = 2014L)
      names(grid)[names(grid) == "x"] <- col
      whep:::.country_grid_is_dynamic(grid)
    }
  )
  expect_true(all(alone))
  expect_false(
    whep:::.country_grid_is_dynamic(tibble::tibble(lon = 0, lat = 0))
  )
})

test_that("a boundary year selects the successor epoch, under every naming", {
  purrr::walk(.scg_column_pairs(), \(pair) {
    grid <- .scg_epochs(pair[[1L]], pair[[2L]])
    at_boundary <- whep:::.filter_country_grid_year(grid, 2014)
    # An inclusive end bound returns BOTH rows here and double-counts the cell.
    expect_equal(nrow(at_boundary), 1L, info = pair[[2L]])
    expect_equal(at_boundary$polity_code, "RUS-2014-2025", info = pair[[2L]])

    expect_equal(
      whep:::.filter_country_grid_year(grid, 2013)$polity_code,
      "RUS-1991-2014",
      info = pair[[2L]]
    )
    # `start_year` stays inclusive.
    expect_equal(
      whep:::.filter_country_grid_year(grid, 1991)$polity_code,
      "RUS-1991-2014",
      info = pair[[2L]]
    )
    # DA-24: the successor's own end bound is the open end of this grid, so it
    # covers 2014-2025 inclusive rather than stopping a year short.
    expect_equal(
      whep:::.filter_country_grid_year(grid, 2025)$polity_code,
      "RUS-2014-2025",
      info = pair[[2L]]
    )
  })
})

test_that("no year resolves a cell-polity to more than one interval", {
  # Three consecutive epochs, two polities, one physical cell -- a polycell
  # partition. Compartment 3 carries the upstream shape that a bare "end_year
  # is the maximum" open-end test gets wrong: two OVERLAPPING intervals both
  # ending on the domain end, as `AGO-1816-2025` does beside `AGO-1975-2025`
  # in the shipped table. Only the later-starting one is open.
  # Asserted over EVERY year of the domain, not a probe year.
  grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_code, ~start_year, ~end_year,
    0.25, 0.25, 1L, "AAA-1900-1950", 1900L, 1950L,
    0.25, 0.25, 1L, "AAA-1950-1990", 1950L, 1990L,
    0.25, 0.25, 1L, "AAA-1990-2025", 1990L, 2025L,
    0.25, 0.25, 2L, "BBB-1900-1960", 1900L, 1960L,
    0.25, 0.25, 2L, "BBB-1960-2025", 1960L, 2025L,
    0.25, 0.25, 3L, "CCC-1900-2025", 1900L, 2025L,
    0.25, 0.25, 3L, "CCC-1975-2025", 1975L, 2025L
  )
  # 1900-1974: compartment 3 has one live interval; from 1975 the overlap is
  # upstream's and both are live, so the count is asserted per span.
  spans <- list(seq.int(1900L, 1974L), seq.int(1975L, 2024L))
  expected_rows <- c(3L, 4L)
  purrr::walk2(spans, expected_rows, \(years, n) {
    per_year <- purrr::map(years, \(yr) {
      sel <- whep:::.filter_country_grid_year(grid, yr)
      tibble::tibble(
        year = yr,
        n_rows = nrow(sel),
        n_polities = dplyr::n_distinct(sel$area_code)
      )
    }) |>
      purrr::list_rbind()
    expect_equal(unique(per_year$n_rows), n)
    expect_equal(unique(per_year$n_polities), 3L)
  })

  # Each epoch owns exactly its own half-open span.
  owned <- purrr::map_chr(seq.int(1900L, 2024L), \(yr) {
    sel <- whep:::.filter_country_grid_year(grid, yr)
    sel$polity_code[sel$area_code == 1L]
  })
  expect_equal(sum(owned == "AAA-1900-1950"), 50L)
  expect_equal(sum(owned == "AAA-1950-1990"), 40L)
  expect_equal(sum(owned == "AAA-1990-2025"), 35L)

  # DA-24: the open end is covered too, so the partition does not lose its last
  # year. Every compartment resolves, and the one carrying two intervals to the
  # domain end still resolves to exactly ONE -- the later-starting interval.
  at_end <- whep:::.filter_country_grid_year(grid, 2025L)
  expect_equal(nrow(at_end), 3L)
  expect_setequal(at_end$area_code, c(1L, 2L, 3L))
  expect_setequal(
    at_end$polity_code,
    c("AAA-1990-2025", "BBB-1960-2025", "CCC-1975-2025")
  )
  # Nothing beyond the open end.
  expect_equal(nrow(whep:::.filter_country_grid_year(grid, 2026L)), 0L)
})

test_that("the open end is the grid's own last year, not a hardcoded 2025", {
  # Coverage ending in 2000: 2000 is this grid's open end and 2025 is nothing.
  grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_code, ~start_year, ~end_year,
    0.25, 0.25, 1L, "AAA-1900-1950", 1900L, 1950L,
    0.25, 0.25, 1L, "AAA-1950-2000", 1950L, 2000L
  )
  expect_equal(
    whep:::.filter_country_grid_year(grid, 2000L)$polity_code,
    "AAA-1950-2000"
  )
  expect_equal(nrow(whep:::.filter_country_grid_year(grid, 2025L)), 0L)
  # The succession boundary stays exclusive: 1950 is the successor's alone.
  expect_equal(
    whep:::.filter_country_grid_year(grid, 1950L)$polity_code,
    "AAA-1950-2000"
  )

  # Extend the grid and 2000 closes again, the open end moving with the data.
  extended <- rbind(
    grid,
    tibble::tibble(
      lon = 0.25,
      lat = 0.25,
      area_code = 1L,
      polity_code = "AAA-2000-2100",
      start_year = 2000L,
      end_year = 2100L
    )
  )
  expect_equal(
    whep:::.filter_country_grid_year(extended, 2000L)$polity_code,
    "AAA-2000-2100"
  )
  expect_equal(
    whep:::.filter_country_grid_year(extended, 2100L)$polity_code,
    "AAA-2000-2100"
  )
})

test_that("a later interval that ends earlier does not close the open end", {
  # The compartment-path twin of the same shape: a later-starting interval that
  # is long gone by the domain end succeeds nothing there, so the long interval
  # must still cover its terminal year rather than leaving a one-year hole.
  grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_code, ~start_year, ~end_year,
    0.25, 0.25, 1L, "AAA-1900-2025", 1900L, 2025L,
    0.25, 0.25, 1L, "AAA-1950-1980", 1950L, 1980L
  )
  expect_equal(
    whep:::.filter_country_grid_year(grid, 2024L)$polity_code,
    "AAA-1900-2025"
  )
  expect_equal(
    whep:::.filter_country_grid_year(grid, 2025L)$polity_code,
    "AAA-1900-2025"
  )
})

test_that("the open end is per compartment, not shared across the grid", {
  # Two cells whose intervals end on different years. Only the one reaching the
  # grid's coverage end is open; the other dissolves on its own end_year, so a
  # neighbouring cell's longer history cannot resurrect it.
  grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_code, ~start_year, ~end_year,
    0.25, 0.25, 1L, "AAA-1900-1990", 1900L, 1990L,
    0.75, 0.25, 2L, "BBB-1900-2025", 1900L, 2025L
  )
  at_1990 <- whep:::.filter_country_grid_year(grid, 1990L)
  expect_false("AAA-1900-1990" %in% at_1990$polity_code)
  expect_equal(at_1990$polity_code, "BBB-1900-2025")
  expect_equal(
    whep:::.filter_country_grid_year(grid, 2025L)$polity_code,
    "BBB-1900-2025"
  )
})

test_that("an open-ended or missing bound still resolves", {
  # Only a start column: everything from 1990 onwards.
  start_only <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~start_year,
    0.25, 0.25, 1L, 1990L
  )
  expect_equal(nrow(whep:::.filter_country_grid_year(start_only, 1989)), 0L)
  expect_equal(nrow(whep:::.filter_country_grid_year(start_only, 1990)), 1L)

  # Only an end column: everything up to and including 2014, which is this
  # grid's open end -- nothing succeeds the row, so DA-24 makes its terminal
  # year inclusive. 2015 is the assertion that keeps the alias list honest: a
  # name falling out of the resolver returns the grid whole for every year.
  purrr::walk(c("valid_to", "end_year", "to_year"), \(col) {
    end_only <- tibble::tibble(
      lon = 0.25,
      lat = 0.25,
      area_code = 1L,
      x = 2014L
    )
    names(end_only)[names(end_only) == "x"] <- col
    expect_equal(
      nrow(whep:::.filter_country_grid_year(end_only, 2013)),
      1L,
      info = col
    )
    expect_equal(
      nrow(whep:::.filter_country_grid_year(end_only, 2014)),
      1L,
      info = col
    )
    expect_equal(
      nrow(whep:::.filter_country_grid_year(end_only, 2015)),
      0L,
      info = col
    )
  })
  # Likewise for each start-bound name alone.
  purrr::walk(c("valid_from", "start_year", "from_year"), \(col) {
    start_only <- tibble::tibble(
      lon = 0.25,
      lat = 0.25,
      area_code = 1L,
      x = 1990L
    )
    names(start_only)[names(start_only) == "x"] <- col
    expect_equal(
      nrow(whep:::.filter_country_grid_year(start_only, 1989)),
      0L,
      info = col
    )
    expect_equal(
      nrow(whep:::.filter_country_grid_year(start_only, 1990)),
      1L,
      info = col
    )
  })

  # An NA end bound is open-ended, not a dissolved polity.
  na_end <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~start_year, ~end_year,
    0.25, 0.25, 1L, 1990L, NA_integer_
  )
  expect_equal(nrow(whep:::.filter_country_grid_year(na_end, 2500)), 1L)

  # An explicit `year` column takes precedence and is matched exactly.
  by_year <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year,
    0.25, 0.25, 1L, 2014L,
    0.25, 0.25, 1L, 2015L
  )
  expect_equal(whep:::.filter_country_grid_year(by_year, 2014)$year, 2014L)

  # A static grid is returned untouched.
  static <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    0.25, 0.25, 1L
  )
  expect_equal(whep:::.filter_country_grid_year(static, 2014), static)
})
