# Write a tiny HYDE-style ESRI ASCII grid ZIP covering exactly
# `n_lon_blocks` x `n_lat_blocks` WHEP 0.5-degree cells (each block is 6x6
# fine cells, since 0.5 / 0.0833333 = 6), zipped the same way as the real
# "{year}AD_pop.zip" archive (containing "urbc_{year}AD.asc", and any other
# `members`), so the test exercises the real zip-read-parse path, not a
# shortcut. `members` maps each member prefix to its fill value.
.hyde_fixture_zip <- function(
  year = 1900L,
  n_lon_blocks = 1L,
  n_lat_blocks = 1L,
  fill_value = 100,
  nodata_cells = integer(0),
  members = c(urbc = fill_value),
  .local_envir = parent.frame()
) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  cellsize <- 0.0833333
  ncols <- 6L * n_lon_blocks
  nrows <- 6L * n_lat_blocks
  header <- c(
    paste("ncols", ncols),
    paste("nrows", nrows),
    "xllcorner -180.0",
    "yllcorner -90.0",
    paste("cellsize", cellsize),
    "NODATA_value -9999.0"
  )
  # Only the northwest-most block is needed: place the fine grid starting at
  # the grid origin so the resulting block center matches the real-grid
  # formula (lon_block[1] = -180 + 0.25, lat_block[1] = 90 - 0.25).
  asc_names <- vapply(
    names(members),
    function(prefix) {
      values <- rep(members[[prefix]], ncols * nrows)
      values[nodata_cells] <- -9999
      mat <- matrix(values, nrow = nrows, ncol = ncols, byrow = TRUE)
      rows <- apply(mat, 1, paste, collapse = " ")
      asc_name <- paste0(prefix, "_", year, "AD.asc")
      writeLines(c(header, rows), file.path(dir, asc_name))
      asc_name
    },
    character(1)
  )

  zip_path <- file.path(dir, paste0(year, "AD_pop.zip"))
  withr::with_dir(dir, zip::zip(basename(zip_path), unname(asc_names)))

  list(
    dir = dir,
    n_cells = n_lon_blocks * n_lat_blocks,
    fine_cells_per_block = 36L,
    fill_value = fill_value
  )
}

testthat::test_that("read_hyde_population sums fine cells into one 0.5-deg cell", {
  fixture <- .hyde_fixture_zip(
    year = 1900L,
    n_lon_blocks = 1L,
    n_lat_blocks = 1L
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture$dir,
    years = 1900L,
    variable = "urban"
  )

  pointblank::expect_col_exists(result, c("lon", "lat", "year", "urban_pop"))
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(
    result$urban_pop,
    fixture$fine_cells_per_block * fixture$fill_value
  )
})

testthat::test_that("read_hyde_population maps block centers correctly", {
  fixture <- .hyde_fixture_zip(
    year = 1900L,
    n_lon_blocks = 1L,
    n_lat_blocks = 1L
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture$dir,
    years = 1900L,
    variable = "urban"
  )

  # xllcorner = -180 fixes the WEST edge of the grid (column 1), so the
  # single fine block's 0.5-degree block center has lon = -180 + 0.25. Row 1
  # of the data matrix is the NORTHERNMOST row (standard ESRI convention:
  # data starts at yllcorner + cellsize*nrows and decreases), so the single
  # fine block sits at the NORTH edge, giving lat = 90 - 0.25.
  testthat::expect_equal(result$lon, -179.75)
  testthat::expect_equal(result$lat, 89.75)
})

testthat::test_that("read_hyde_population keeps adjacent 0.5-deg cells separate", {
  fixture <- .hyde_fixture_zip(
    year = 1900L,
    n_lon_blocks = 2L,
    n_lat_blocks = 1L
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture$dir,
    years = 1900L,
    variable = "urban"
  )

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_setequal(result$lon, c(-179.75, -179.25))
  testthat::expect_true(all(
    result$urban_pop == fixture$fine_cells_per_block * fixture$fill_value
  ))
})

testthat::test_that("read_hyde_population drops NODATA cells", {
  fixture <- .hyde_fixture_zip(
    year = 1900L,
    n_lon_blocks = 1L,
    n_lat_blocks = 1L,
    nodata_cells = c(1L, 2L, 3L)
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture$dir,
    years = 1900L,
    variable = "urban"
  )

  testthat::expect_equal(
    result$urban_pop,
    (fixture$fine_cells_per_block - 3L) * fixture$fill_value
  )
})

testthat::test_that("all-NODATA blocks are absent rather than false zeros", {
  fixture <- .hyde_fixture_zip(
    year = 1900L,
    n_lon_blocks = 1L,
    n_lat_blocks = 1L,
    nodata_cells = seq_len(36L)
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture$dir,
    years = 1900L,
    variable = "urban"
  )

  testthat::expect_equal(nrow(result), 0L)
  testthat::expect_identical(
    names(result),
    c("lon", "lat", "year", "urban_pop")
  )
})

testthat::test_that("read_hyde_population reads multiple years", {
  fixture_1900 <- .hyde_fixture_zip(year = 1900L, fill_value = 100)
  fixture_1950 <- .hyde_fixture_zip(year = 1950L, fill_value = 200)
  file.copy(
    file.path(fixture_1950$dir, "1950AD_pop.zip"),
    file.path(fixture_1900$dir, "1950AD_pop.zip")
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture_1900$dir,
    years = c(1900L, 1950L),
    variable = "urban"
  )

  testthat::expect_setequal(result$year, c(1900L, 1950L))
  testthat::expect_equal(
    result$urban_pop[result$year == 1900L],
    36L * 100
  )
  testthat::expect_equal(
    result$urban_pop[result$year == 1950L],
    36L * 200
  )
})

testthat::test_that("read_hyde_population requires years to be specified", {
  testthat::expect_error(
    whep::read_hyde_population(hyde_dir = "some/dir"),
    "years"
  )
})

testthat::test_that("read_hyde_population aborts on BC years", {
  testthat::expect_error(
    whep::read_hyde_population(hyde_dir = "some/dir", years = -100L),
    "AD"
  )
})

testthat::test_that("read_hyde_population example fixture is schema-complete", {
  out <- whep::read_hyde_population(example = TRUE)
  pointblank::expect_col_exists(out, c("lon", "lat", "year", "total_pop"))
  pointblank::expect_col_vals_gte(out, "total_pop", 0)
  urban <- whep::read_hyde_population(variable = "urban", example = TRUE)
  testthat::expect_identical(names(urban), c("lon", "lat", "year", "urban_pop"))
})

testthat::test_that("a HYDE variable missing from the archive is refused by name", {
  # The fixture archive holds only urbc; asking it for popc must name the
  # member rather than fail inside unz() or read another variable.
  fixture <- .hyde_fixture_zip(year = 1900L)
  testthat::expect_error(
    whep:::.read_hyde_year(1900L, fixture$dir, variable = "popc"),
    "popc_1900AD.asc"
  )
})

# ---- variable: total (default), urban, rural (issue 1304) --------------------

.hyde_three_counts <- function(nodata_cells = integer(0)) {
  .hyde_fixture_zip(
    year = 1900L,
    n_lon_blocks = 2L,
    n_lat_blocks = 1L,
    nodata_cells = nodata_cells,
    members = c(popc = 100, urbc = 30, rurc = 70),
    .local_envir = parent.frame()
  )
}

testthat::test_that("read_hyde_population reads the total population by default", {
  fixture <- .hyde_three_counts()
  result <- whep::read_hyde_population(hyde_dir = fixture$dir, years = 1900L)
  testthat::expect_identical(
    names(result),
    c("lon", "lat", "year", "total_pop")
  )
  testthat::expect_equal(result$total_pop, rep(36 * 100, 2))
})

testthat::test_that("each variable reads its own HYDE member", {
  fixture <- .hyde_three_counts()
  read <- function(variable) {
    whep::read_hyde_population(
      hyde_dir = fixture$dir,
      years = 1900L,
      variable = variable
    )
  }
  testthat::expect_equal(read("urban")$urban_pop, rep(36 * 30, 2))
  testthat::expect_equal(read("rural")$rural_pop, rep(36 * 70, 2))
  testthat::expect_error(read("popc"), class = "rlang_error")
})

testthat::test_that("total equals urban plus rural in every 0.5-degree block", {
  # NODATA cells are dropped from every member alike, so the identity holds
  # per block even where a block is only partly populated.
  fixture <- .hyde_three_counts(nodata_cells = c(1L, 2L, 40L))
  read <- function(variable) {
    whep::read_hyde_population(
      hyde_dir = fixture$dir,
      years = 1900L,
      variable = variable
    )
  }
  joined <- read("total") |>
    dplyr::inner_join(read("urban"), by = c("lon", "lat", "year")) |>
    dplyr::inner_join(read("rural"), by = c("lon", "lat", "year"))
  testthat::expect_equal(nrow(joined), 2L)
  testthat::expect_equal(
    joined$total_pop,
    joined$urban_pop + joined$rural_pop
  )
})

testthat::test_that("the urban read reproduces the reader's former output", {
  # Before `variable` existed the reader always read urbc into `urban_pop`.
  # The same parse, block sum and column, byte for byte, under "urban".
  fixture <- .hyde_three_counts(nodata_cells = 5L)
  dir <- fixture$dir
  former <- whep:::.read_hyde_year(1900L, dir, variable = "urbc") |>
    tibble::as_tibble() |>
    dplyr::rename(urban_pop = "pop")
  testthat::expect_identical(
    whep::read_hyde_population(
      hyde_dir = dir,
      years = 1900L,
      variable = "urban"
    ),
    former
  )
})

testthat::test_that("aggregate = FALSE returns the native 5-arcmin cells", {
  fixture <- .hyde_three_counts(nodata_cells = 1L)
  native <- whep::read_hyde_population(
    hyde_dir = fixture$dir,
    years = 1900L,
    aggregate = FALSE
  )
  # 72 fine cells in two blocks, one of them NODATA and so absent.
  testthat::expect_equal(nrow(native), 71L)
  testthat::expect_true(all(native$total_pop == 100))
  # Their own centres, not the block centre: the first row is the
  # northwest-most fine cell after the dropped one.
  cellsize <- 0.0833333
  testthat::expect_equal(native$lon[1], -180 + cellsize * 1.5)
  testthat::expect_equal(native$lat[1], 90 - cellsize * 0.5)
  # And they sum back to the aggregated blocks.
  blocks <- whep::read_hyde_population(hyde_dir = fixture$dir, years = 1900L)
  testthat::expect_equal(sum(native$total_pop), sum(blocks$total_pop))
})
