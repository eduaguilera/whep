# Year resolution for a time-varying country grid.
#
# `.filter_country_grid_year()` is the compartment path's epoch resolver. It is
# dead for every crosswalk shipped today (none carries validity columns) and
# goes live the moment a producer emits `start_year`/`end_year`, so the
# convention has to be pinned before that happens, not after.
#
# Convention: start bound INCLUSIVE, end bound EXCLUSIVE.

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
  })
})

test_that("no year resolves a cell-polity to more than one interval", {
  # Three consecutive epochs, two polities, one physical cell -- a polycell
  # partition. Asserted over EVERY year of the domain, not a probe year.
  grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_code, ~start_year, ~end_year,
    0.25, 0.25, 1L, "AAA-1900-1950", 1900L, 1950L,
    0.25, 0.25, 1L, "AAA-1950-1990", 1950L, 1990L,
    0.25, 0.25, 1L, "AAA-1990-2025", 1990L, 2025L,
    0.25, 0.25, 2L, "BBB-1900-1960", 1900L, 1960L,
    0.25, 0.25, 2L, "BBB-1960-2025", 1960L, 2025L
  )
  per_year <- purrr::map(seq.int(1900L, 2024L), \(yr) {
    sel <- whep:::.filter_country_grid_year(grid, yr)
    tibble::tibble(
      year = yr,
      n_rows = nrow(sel),
      n_polities = dplyr::n_distinct(sel$area_code)
    )
  }) |>
    purrr::list_rbind()

  # Exactly one interval per polity, every year, no gap and no double count.
  expect_equal(unique(per_year$n_rows), 2L)
  expect_equal(unique(per_year$n_polities), 2L)

  # Each epoch owns exactly its own half-open span.
  owned <- purrr::map_chr(seq.int(1900L, 2024L), \(yr) {
    sel <- whep:::.filter_country_grid_year(grid, yr)
    sel$polity_code[sel$area_code == 1L]
  })
  expect_equal(sum(owned == "AAA-1900-1950"), 50L)
  expect_equal(sum(owned == "AAA-1950-1990"), 40L)
  expect_equal(sum(owned == "AAA-1990-2025"), 35L)
})

test_that("an open-ended or missing bound still resolves", {
  # Only a start column: everything from 1990 onwards.
  start_only <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~start_year,
    0.25, 0.25, 1L, 1990L
  )
  expect_equal(nrow(whep:::.filter_country_grid_year(start_only, 1989)), 0L)
  expect_equal(nrow(whep:::.filter_country_grid_year(start_only, 1990)), 1L)

  # Only an end column: everything strictly before 2014. Each end-bound name is
  # checked alone, so one falling out of the resolver's alias list cannot pass
  # unnoticed as an unbounded interval.
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
