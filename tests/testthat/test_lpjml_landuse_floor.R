# whep#985: the LPJmL landuse NetCDF is float32, and filtering its fractions
# on `value > 0` let arithmetic residue through -- down to 1.401e-45, the
# smallest float32 denormal. `.floor_landuse_fractions()` drops what float32
# cannot resolve at the cell's own scale and folds it back into the cell.

.flt_eps <- 2^-23
.flt_min <- 2^-126

# Round a double to float32 the way `ncvar_put(prec = "float")` stores it.
.as_float32 <- function(x) {
  readBin(writeBin(x, raw(), size = 4L), "double", n = length(x), size = 4L)
}

.lu_cells <- function() {
  tibble::tribble(
    ~year, ~pft, ~row, ~col, ~value,
    2000L,   1L,   1L,   1L,   0.4,
    2000L,   2L,   1L,   1L,   0.2,
    2000L,   3L,   1L,   1L,   1e-45,
    2000L,   4L,   1L,   1L,   1e-12,
    2000L,   5L,   1L,   1L,   0,
    2000L,   1L,   1L,   2L,   1e-30,
    2000L,   2L,   1L,   2L,   3e-31,
    2000L,   1L,   2L,   1L,   1e-40,
    2000L,   2L,   2L,   1L,   2e-40
  ) |>
    data.table::as.data.table()
}

.cell_sums <- function(lu) {
  lu[, .(total = sum(value)), by = .(year, row, col)][order(year, row, col)]
}

testthat::test_that("sub-resolution and denormal fractions are dropped", {
  out <- whep:::.floor_landuse_fractions(.lu_cells())

  # Cell (1, 1): 1e-45 is denormal and 1e-12 is below 0.6 * FLT_EPSILON; the
  # exact zero goes too. Cell (1, 2) is tiny but internally resolvable, so
  # both of its bands survive. Cell (2, 1) is denormal throughout and goes.
  kept <- out[order(row, col, pft), .(row, col, pft)]
  testthat::expect_equal(kept$row, c(1L, 1L, 1L, 1L))
  testthat::expect_equal(kept$col, c(1L, 1L, 2L, 2L))
  testthat::expect_equal(kept$pft, c(1L, 2L, 1L, 2L))
  testthat::expect_true(all(out$value >= .flt_min))
})

testthat::test_that("every surviving cell keeps its land-use total", {
  lu <- .lu_cells()
  out <- whep:::.floor_landuse_fractions(lu)
  before <- .cell_sums(lu)[.cell_sums(out), on = .(year, row, col)]

  testthat::expect_equal(before$i.total, before$total, tolerance = 1e-15)
  # The fold-back is of the order of float32 resolution, not a real shift.
  testthat::expect_lt(
    abs(out[pft == 1L & col == 1L, value] / 0.4 - 1),
    .flt_eps
  )
})

testthat::test_that("the floor conserves mass on a heavy-tailed random grid", {
  withr::local_seed(985)
  n_cells <- 2000L
  lu <- data.table::data.table(
    year = 2000L,
    row = rep(seq_len(n_cells), each = 32L),
    col = 1L,
    pft = rep(seq_len(32L), times = n_cells),
    # Log-uniform over 1e-46..1: the shape of the allocation tail in #985.
    value = 10^stats::runif(32L * n_cells, -46, 0)
  )
  lu[, value := value / max(1, sum(value)), by = row]
  out <- whep:::.floor_landuse_fractions(lu)
  totals <- .cell_sums(lu)[.cell_sums(out), on = .(year, row, col)]

  testthat::expect_lt(nrow(out), nrow(lu))
  testthat::expect_equal(nrow(totals), n_cells)
  testthat::expect_equal(totals$i.total, totals$total, tolerance = 1e-14)
  testthat::expect_equal(sum(out$value), sum(lu$value), tolerance = 1e-14)
  # Nothing survives that sits past the last significant bit of its cell.
  out[, cell_total := sum(value), by = row]
  testthat::expect_true(all(out$value >= out$cell_total * .flt_eps))
  # And after the float32 cast nothing is denormal.
  stored <- .as_float32(out$value)
  testthat::expect_false(any(stored > 0 & stored < .flt_min))
})

testthat::test_that("the float32 cast of a surviving cell keeps its total", {
  lu <- .lu_cells()
  out <- whep:::.floor_landuse_fractions(lu)
  stored <- .as_float32(out$value)
  # Against the double total the pre-floor file would have stored, the
  # difference is inside float32 rounding of the cell total.
  cell_11 <- out$row == 1L & out$col == 1L
  testthat::expect_lt(
    abs(sum(stored[cell_11]) - (0.6 + 1e-12 + 1e-45)),
    0.6 * .flt_eps
  )
})

testthat::test_that("method = 'denormal' drops only what float32 cannot hold", {
  out <- whep:::.floor_landuse_fractions(.lu_cells(), method = "denormal")

  # 1e-12 is sub-resolution for its cell but a normal float32, so it stays.
  testthat::expect_true(any(abs(out$value - 1e-12) < 1e-20))
  testthat::expect_true(any(out$value == 1e-30))
  testthat::expect_false(any(out$value < .flt_min))
  testthat::expect_equal(sum(out$row == 2L), 0L)
})

testthat::test_that("method = 'none' returns the input unchanged", {
  lu <- .lu_cells()
  testthat::expect_identical(
    whep:::.floor_landuse_fractions(lu, method = "none"),
    lu
  )
})

testthat::test_that("an empty input comes back empty", {
  empty <- .lu_cells()[0L]
  testthat::expect_equal(nrow(whep:::.floor_landuse_fractions(empty)), 0L)
})

testthat::test_that("bad input aborts with a classed condition", {
  testthat::expect_error(
    whep:::.floor_landuse_fractions(.lu_cells()[, !"value"]),
    class = "whep_landuse_floor_columns"
  )
  testthat::expect_error(
    whep:::.floor_landuse_fractions(.lu_cells()[1L, value := -1]),
    class = "whep_landuse_floor_values"
  )
  testthat::expect_error(
    whep:::.floor_landuse_fractions(.lu_cells()[1L, value := NA_real_]),
    class = "whep_landuse_floor_values"
  )
  testthat::expect_error(
    whep:::.floor_landuse_fractions(.lu_cells(), method = "hectare"),
    class = "rlang_error"
  )
})

# The write step itself: `.write_lu_nc_chunk()` lives at script scope in
# inst/scripts/prepare_spatialize_all.R, so this part sources the script and
# skips where the script is not in the build (whep#402).
.source_prepare_spatialize()

# `.pft_nc_write_chunk()` is swapped for a recorder, so the test sees exactly
# the band values the writer hands to `ncvar_put()` without needing ncdf4.
.captured_lu_chunk <- function(...) {
  script_env <- environment(.write_lu_nc_chunk)
  real_writer <- script_env$.pft_nc_write_chunk
  withr::defer(assign(".pft_nc_write_chunk", real_writer, envir = script_env))
  captured <- NULL
  assign(
    ".pft_nc_write_chunk",
    function(nc_info, data_dt, ...) captured <<- data_dt,
    envir = script_env
  )
  grid <- make_target_grid()
  row_area_ha <- cell_area_ha_by_lat(grid$lat)
  cft <- tibble::tribble(
    ~lon,    ~lat,  ~year, ~cft_name,           ~rainfed_ha, ~irrigated_ha,
    -179.75, 83.75, 2000L, "temperate_cereals", 0.3,         0,
    -179.75, 83.75, 2000L, "maize",             1e-45,       0,
    -179.75, 83.75, 2000L, "rice",              1e-12,       0.1
  ) |>
    dplyr::mutate(dplyr::across(c(rainfed_ha, irrigated_ha), \(x) {
      x * row_area_ha[1L]
    }))
  .write_lu_nc_chunk(
    list(),
    cft,
    NULL,
    2000L,
    2000L,
    grid,
    row_area_ha,
    ...
  )
  captured
}

testthat::test_that("the landuse writer emits no denormal and keeps the cell", {
  .need_spatialize_helper(".write_lu_nc_chunk")
  # The default, so this also runs against a writer with no floor at all.
  written <- .captured_lu_chunk()
  stored <- .as_float32(written$value)

  testthat::expect_false(any(stored > 0 & stored < .flt_min))
  testthat::expect_false(any(stored < sum(stored) * .flt_eps))
  testthat::expect_setequal(written$pft, c(1L, 18L))
  testthat::expect_equal(sum(written$value), 0.4 + 1e-12 + 1e-45)
})

testthat::test_that("landuse_floor = 'none' reproduces the unfloored writer", {
  .need_spatialize_helper(".write_lu_nc_chunk")
  written <- .captured_lu_chunk(landuse_floor = "none")

  # The pre-#985 file: the 1e-45 band reaches the float32 cast as a denormal.
  stored <- .as_float32(written$value)
  testthat::expect_true(any(stored > 0 & stored < .flt_min))
})
