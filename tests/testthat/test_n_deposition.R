# Write a tiny 5-arcmin HaNi-style NetCDF (one variable, native fine grid)
# covering exactly `n_lon_blocks` x `n_lat_blocks` WHEP 0.5-degree cells (each
# block is 6x6 fine cells, since 0.5 / (5/60) = 6), plus `n_years` annual
# layers, and return where it lives plus the expected 0.5-degree cell count.
.hani_fixture_cube <- function(
  var_name = "ndep_nhx",
  file = "ndep_nhx.nc",
  n_lon_blocks = 1L,
  n_lat_blocks = 1L,
  n_years = 2L,
  fill_value = 1
) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  fine_step <- 0.5 / 6
  fine_lon <- -0.5 +
    fine_step / 2 +
    (seq_len(6L * n_lon_blocks) - 1) *
      fine_step
  fine_lat <- -0.5 +
    fine_step / 2 +
    (seq_len(6L * n_lat_blocks) - 1) *
      fine_step
  time <- seq_len(n_years) - 1L
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", fine_lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", fine_lat)
  dim_time <- ncdf4::ncdim_def(
    "time",
    "years since 1850-01-01 00:00:00",
    time,
    calendar = "noleap"
  )
  var <- ncdf4::ncvar_def(
    var_name,
    "g N",
    list(dim_lon, dim_lat, dim_time),
    missval = -9999
  )
  path <- file.path(dir, file)
  nc <- ncdf4::nc_create(path, list(var))
  vals <- array(
    fill_value,
    dim = c(length(fine_lon), length(fine_lat), length(time))
  )
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::nc_close(nc)
  list(
    dir = dir,
    n_cells = n_lon_blocks * n_lat_blocks,
    fine_cells_per_block = 36L,
    fill_value = fill_value
  )
}

testthat::test_that("read_n_deposition sums fine cells into one 0.5-deg cell", {
  cube <- .hani_fixture_cube(n_lon_blocks = 1L, n_lat_blocks = 1L, n_years = 1L)
  result <- whep::read_n_deposition(
    species = "nhx",
    hani_dir = cube$dir,
    years = 1850L
  )

  pointblank::expect_col_exists(result, c("lon", "lat", "year", "value_g"))
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(
    result$value_g,
    cube$fine_cells_per_block * cube$fill_value
  )
  testthat::expect_equal(result$lon, -0.25)
  testthat::expect_equal(result$lat, -0.25)
})

testthat::test_that("read_n_deposition keeps adjacent 0.5-deg cells separate", {
  cube <- .hani_fixture_cube(n_lon_blocks = 2L, n_lat_blocks = 1L, n_years = 1L)
  result <- whep::read_n_deposition(
    species = "nhx",
    hani_dir = cube$dir,
    years = 1850L
  )

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_setequal(result$lon, c(-0.25, 0.25))
  testthat::expect_true(all(
    result$value_g == cube$fine_cells_per_block * cube$fill_value
  ))
})

testthat::test_that("read_n_deposition maps time index to calendar year", {
  cube <- .hani_fixture_cube(n_years = 3L)
  result <- whep::read_n_deposition(
    species = "nhx",
    hani_dir = cube$dir,
    years = c(1850L, 1852L)
  )

  testthat::expect_setequal(result$year, c(1850L, 1852L))
})

testthat::test_that("read_n_deposition returns its schema when requested years are absent", {
  cube <- .hani_fixture_cube(n_years = 1L)
  result <- whep::read_n_deposition(
    species = "nhx",
    hani_dir = cube$dir,
    years = 1900L
  )

  testthat::expect_equal(nrow(result), 0L)
  testthat::expect_named(result, c("lon", "lat", "year", "value_g"))
})

# ---- build_n_deposition() ---------------------------------------------

.example_cell_polity <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    -0.25, -0.25, 1L, 1, 300000
  )
}

testthat::test_that("build_n_deposition converts mass to a per-hectare rate", {
  nhx <- tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2020L, 2000000000
  )
  noy <- tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2020L, 1000000000
  )
  out <- whep::build_n_deposition(
    data = list(nhx = nhx, noy = noy, cell_polity = .example_cell_polity())
  )

  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "deposition_kgn_ha",
      "deposition_n_t",
      "method_deposition"
    )
  )
  pointblank::expect_col_vals_gte(out, "deposition_kgn_ha", 0)
  # (2e9 + 1e9) g / 1000 (g->kg) / 300000 ha = 10 kg N/ha
  testthat::expect_equal(out$deposition_kgn_ha, 10)
  # 10 kg/ha * 300000 ha * 1 (polity_frac) / 1000 (kg->t) = 3000 t
  testthat::expect_equal(out$deposition_n_t, 3000)
})

testthat::test_that("build_n_deposition filters preloaded inputs by years", {
  nhx <- tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2019L, 1000,
    -0.25, -0.25, 2020L, 2000
  )
  noy <- tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2019L, 100,
    -0.25, -0.25, 2020L, 200
  )

  out <- whep::build_n_deposition(
    years = 2020L,
    data = list(nhx = nhx, noy = noy, cell_polity = .example_cell_polity())
  )

  testthat::expect_equal(out$year, 2020L)
  testthat::expect_equal(nrow(out), 1L)
})

testthat::test_that("build_n_deposition example fixture is schema-complete", {
  out <- whep::build_n_deposition(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "deposition_kgn_ha",
      "deposition_n_t",
      "method_deposition"
    )
  )
  pointblank::expect_col_vals_gte(out, "deposition_kgn_ha", 0)
})

# ---- C0 characterisation baseline (polycell consumer migration) --------
#
# THESE ARE CHARACTERISATION TESTS, NOT CORRECTNESS ASSERTIONS. They pin
# what build_n_deposition() does TODAY, on unmodified pre-migration code,
# so that any value change the polycell consumer migration introduces is
# visible and attributable instead of silent.
#
# What they protect. R/n_deposition.R:196 divides the HaNi cell mass by
# `cell_area_ha` to form `deposition_kgn_ha`, and R/n_deposition.R:197-199
# multiplies it straight back by `cell_area_ha`. The two cancel EXACTLY,
# so today
#
#     deposition_n_t  is  value_g_total x polity_frac / 1e6
#
# and the ~11% whole-cell area over-count does NOT reach the deposition
# mass column. Mass is already conserved here.
#
# Why that is fragile. Substituting a land area at :198 while leaving :196
# alone moves the global deposition total about 10% DOWN -- the direction
# a reviewer expects a genuine fix to move it -- so the diff reads as a
# success while a property that holds exactly today is destroyed. The
# other trap is dividing by each polycell's own land area, which makes
# rate x area recover the whole cell mass for EVERY polity of a shared
# cell, emitting a 4-polity cell's mass four times. Both are invisible to
# a single-polity fixture, so every fixture below shares a cell between
# several polities.

# Three cells: one shared by three polities, one shared by two at a high
# latitude (so cell_area_ha differs across cells), one single-polity. The
# third cell carries NHx only, so the NOy full_join/coalesce-to-zero path
# is exercised inside the conservation check rather than assumed.
.nd_c0_nhx <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2000L, 2e9,
    0.25, 59.75, 2000L, 7e8,
    0.75, 0.25, 2000L, 5e8
  )
}

.nd_c0_noy <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2000L, 1e9,
    0.25, 59.75, 2000L, 3e8
  )
}

# cell_area_ha values are the right order for a 0.5-degree cell at these
# latitudes but deliberately NOT the package's own numbers, which are
# 309103.9 ha at 0.25 and 155720.0 ha at 59.75 (.cell_area_ha_lat()).
# Using approximations is the point: today deposition_n_t is indifferent
# to what sits in this column, so a fixture that had to carry the exact
# areas would be pinning something the code does not actually depend on.
# The two values must merely DIFFER between cells, so that no test can
# pass by accident on a single constant area.
.nd_c0_cell_polity <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    -0.25, -0.25, 1L, 0.5, 308000,
    -0.25, -0.25, 2L, 0.3, 308000,
    -0.25, -0.25, 3L, 0.2, 308000,
    0.25, 59.75, 4L, 0.6, 155000,
    0.25, 59.75, 5L, 0.4, 155000,
    0.75, 0.25, 6L, 1.0, 308000
  )
}

.nd_c0_build <- function(cell_polity = .nd_c0_cell_polity()) {
  whep::build_n_deposition(
    data = list(
      nhx = .nd_c0_nhx(),
      noy = .nd_c0_noy(),
      cell_polity = cell_polity
    )
  )
}

testthat::test_that("C0: deposition conserves the HaNi source mass exactly", {
  out <- .nd_c0_build()
  # 2e9 + 7e8 + 5e8 NHx plus 1e9 + 3e8 NOy = 4.5e9 g N in the fixture.
  source_g <- sum(.nd_c0_nhx()$value_g) + sum(.nd_c0_noy()$value_g)

  testthat::expect_equal(source_g, 4.5e9)
  # 1e6 g N = 1 t N. Tolerance is DA-18's locked 1e-9 relative bound for
  # mass conservation; the measured gap on this fixture today is exactly 0.
  testthat::expect_equal(
    sum(out$deposition_n_t) * 1e6,
    source_g,
    tolerance = 1e-9
  )
})

testthat::test_that("C0: deposition_n_t is value_g_total x polity_frac / 1e6", {
  out <- .nd_c0_build()
  cp <- .nd_c0_cell_polity()
  total_g <- dplyr::full_join(
    .nd_c0_nhx(),
    .nd_c0_noy(),
    by = c("lon", "lat", "year"),
    suffix = c("_nhx", "_noy")
  ) |>
    dplyr::mutate(
      value_g_total = dplyr::coalesce(value_g_nhx, 0) +
        dplyr::coalesce(value_g_noy, 0)
    )
  expected <- cp |>
    dplyr::inner_join(total_g, by = c("lon", "lat")) |>
    dplyr::mutate(expected_n_t = value_g_total * polity_frac / 1e6)

  joined <- dplyr::inner_join(
    out,
    dplyr::select(expected, lon, lat, area_code, expected_n_t),
    by = c("lon", "lat", "area_code")
  )
  testthat::expect_equal(nrow(joined), nrow(out))
  # The algebraic identity that holds today, pinned row by row. No area
  # term appears in it: cell_area_ha has already cancelled. The
  # cancellation is exact in real arithmetic but round-trips through
  # IEEE-754 division and multiplication, so at full crosswalk scale
  # 14,775 of 68,527 rows differ from this identity in the last bits (max
  # 3.22e-16 relative; measured by
  # inst/scripts/characterize_consumer_baseline.R). DA-18's 1e-9 is six
  # orders of magnitude above that noise and six below the ~10% move the
  # dangerous migration produces.
  testthat::expect_equal(
    joined$deposition_n_t,
    joined$expected_n_t,
    tolerance = 1e-9
  )
  pointblank::expect_col_vals_gte(out, "deposition_n_t", 0)
})

testthat::test_that("C0: cell_area_ha cancels exactly out of deposition_n_t", {
  base <- .nd_c0_build()
  tripled <- .nd_c0_build(
    dplyr::mutate(.nd_c0_cell_polity(), cell_area_ha = cell_area_ha * 3)
  )

  # cell_area_ha divides at :196 and multiplies back at :198, so the mass
  # column cannot see it. THIS IS THE GUARD against swapping :198 to a
  # land area while leaving :196 -- that change would drop these masses by
  # the land fraction. Bit-identity holds on these particular fixture
  # areas; at full crosswalk scale the same tripling moves 26,122 of
  # 68,527 rows by at most 3.9e-16 relative (~2 ulp) with the total bit-
  # identical, because (v / A) * A is not exactly v for every A. So the
  # claim is "exact algebraically, ~2 ulp in float", not "bitwise for all
  # inputs"; expect_identical() is used here because it is the sharpest
  # pin these fixture values support.
  testthat::expect_identical(tripled$deposition_n_t, base$deposition_n_t)
  # The rate column, by contrast, does depend on the area it is divided
  # by, and today that divisor is the whole cell. Pinned so a migration
  # that silently re-references the rate is also visible.
  testthat::expect_false(
    identical(tripled$deposition_kgn_ha, base$deposition_kgn_ha)
  )
  testthat::expect_equal(
    tripled$deposition_kgn_ha,
    base$deposition_kgn_ha / 3
  )
})

testthat::test_that("C0: deposition rate is shared by every polity of a cell", {
  out <- .nd_c0_build()
  per_cell <- dplyr::summarise(
    out,
    n_rates = dplyr::n_distinct(deposition_kgn_ha),
    n_polities = dplyr::n_distinct(area_code),
    .by = c(lon, lat)
  )

  testthat::expect_setequal(per_cell$n_polities, c(3L, 2L, 1L))
  # One rate per cell, however many polities share it: the rate is a
  # whole-cell mean today. THIS IS THE GUARD against dividing the
  # allocated mass by each polycell's own land area, which would give
  # every polity of a shared cell its own rate AND make rate x area
  # recover the full cell mass once per polity.
  testthat::expect_true(all(per_cell$n_rates == 1L))
  testthat::expect_equal(
    out$deposition_kgn_ha[out$area_code == 1L],
    2e9 / 1000 / 308000 + 1e9 / 1000 / 308000
  )
})

testthat::test_that("C0: deposition mass conservation is inherited, not enforced", {
  # Conservation above holds because the crosswalk's polity_frac is a
  # partition of the cell (sum to 1). build_n_deposition() does not check
  # that and does not renormalise: fractions summing to 0.9 lose exactly
  # 10% of the cell's mass, silently. Pinned so the migration knows the
  # partition property is load-bearing and must survive in whatever
  # replaces polity_frac.
  short <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    -0.25, -0.25, 1L, 0.5, 308000,
    -0.25, -0.25, 2L, 0.4, 308000
  )
  out <- whep::build_n_deposition(
    data = list(
      nhx = .nd_c0_nhx(),
      noy = .nd_c0_noy(),
      cell_polity = short
    )
  )

  testthat::expect_equal(sum(out$deposition_n_t) * 1e6, 0.9 * 3e9)
})

testthat::test_that("C0: a HaNi cell absent from the crosswalk is dropped silently", {
  # R/n_deposition.R:194 joins with dplyr::inner_join(), so deposition
  # mass over a cell the crosswalk does not carry disappears with no
  # warning. Today the crosswalk misses 1,294 LUH2 terrestrial cells
  # (49.5 Mha), so this path is live, not hypothetical. Pinned as current
  # behaviour; it is not asserted to be right.
  one_cell <- dplyr::filter(.nd_c0_cell_polity(), lon == -0.25)
  out <- testthat::expect_no_warning(
    whep::build_n_deposition(
      data = list(
        nhx = .nd_c0_nhx(),
        noy = .nd_c0_noy(),
        cell_polity = one_cell
      )
    )
  )

  testthat::expect_setequal(out$lon, -0.25)
  # 1.2e9 g of the fixture's 4.5e9 lands on cells the crosswalk lacks and
  # is lost without trace.
  testthat::expect_equal(sum(out$deposition_n_t) * 1e6, 3e9)
})
