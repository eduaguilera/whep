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

testthat::test_that("C0: the transitional key is still reachable by name", {
  # `split = "polity_frac"` is what an unmigrated consumer keeps, so the C0
  # numbers above must remain reproducible by asking for that key explicitly,
  # not only by being handed a support that carries nothing else.
  auto <- .nd_c0_build()
  named <- whep::build_n_deposition(
    data = list(
      nhx = .nd_c0_nhx(),
      noy = .nd_c0_noy(),
      cell_polity = .nd_c0_cell_polity()
    ),
    split = "polity_frac"
  )

  testthat::expect_identical(named, auto)
  testthat::expect_true(all(auto$method_polity_split == "polity_frac"))
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

# ---- C3a: the split key moves from polity_frac to polity_area_ha -------
#
# C3a swaps ONLY the partition the cell's deposited mass is split by: from
# `polity_frac`, a subcell count quantised to 1/36 of a cell, to the share of
# the cell's territory each polity holds, as build_polycell_support() measures
# it geodesically. No category split (that is C3b), no new denominator.
#
# The fixture below deliberately makes the two partitions DISAGREE, and makes
# the cell's territory sum to well under the whole cell -- 200,000 of 308,000
# ha in the three-polity cell -- because both of the migration's silent
# failure modes are invisible unless it does:
#
#   * dividing the allocated mass by each polycell's OWN territory gives every
#     polity of a shared cell its own plausible rate and lets rate x area
#     recover the whole cell mass once per polity;
#   * splitting by `polity_area_ha / cell_area_ha` instead of by the polity's
#     share of the cell's territory sheds the non-territorial fraction, which
#     moves the global total DOWN by about the land fraction -- the direction
#     a reviewer expects a genuine fix to move it.
#
# Mass conservation is the invariant that separates a partition swap from
# either of those: a partition redistributes and never creates or destroys.

.nd_c3a_cell_polity <- function() {
  # Same cells, same polity_frac and cell_area_ha as the C0 fixture, so the
  # legacy path stays comparable, plus the geodesic territory each polity
  # holds. Shares by area are 0.6/0.3/0.1, 0.25/0.75 and 1.0 against
  # polity_frac's 0.5/0.3/0.2, 0.6/0.4 and 1.0: two rows move up, two move
  # down, one multi-polity row (area_code 2) is deliberately unmoved, and the
  # single-polity cell cannot move at all.
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha, ~polity_area_ha,
    -0.25, -0.25, 1L, 0.5, 308000, 120000,
    -0.25, -0.25, 2L, 0.3, 308000, 60000,
    -0.25, -0.25, 3L, 0.2, 308000, 20000,
    0.25, 59.75, 4L, 0.6, 155000, 35000,
    0.25, 59.75, 5L, 0.4, 155000, 105000,
    0.75, 0.25, 6L, 1.0, 308000, 250000
  )
}

.nd_c3a_build <- function(cell_polity = .nd_c3a_cell_polity(), ...) {
  whep::build_n_deposition(
    data = list(
      nhx = .nd_c0_nhx(),
      noy = .nd_c0_noy(),
      cell_polity = cell_polity
    ),
    ...
  )
}

.nd_c3a_total_g <- function() {
  dplyr::mutate(
    dplyr::full_join(
      .nd_c0_nhx(),
      .nd_c0_noy(),
      by = c("lon", "lat", "year"),
      suffix = c("_nhx", "_noy")
    ),
    value_g_total = dplyr::coalesce(value_g_nhx, 0) +
      dplyr::coalesce(value_g_noy, 0)
  )
}

testthat::test_that("C3a: the area split conserves the source mass exactly", {
  out <- .nd_c3a_build()
  source_g <- sum(.nd_c0_nhx()$value_g) + sum(.nd_c0_noy()$value_g)

  testthat::expect_equal(source_g, 4.5e9)
  # DA-18's locked 1e-9 relative bound. A partition swap redistributes: any
  # movement in this total is a defect, not a result.
  testthat::expect_equal(
    sum(out$deposition_n_t) * 1e6,
    source_g,
    tolerance = 1e-9
  )
  # And the same total the transitional key gives, cell by cell. Splitting by
  # `polity_area_ha / cell_area_ha` would pass a per-cell-share test and fail
  # this one, having quietly shed the 35% of the three-polity cell that is
  # not territory.
  legacy <- .nd_c3a_build(split = "polity_frac")
  per_cell <- function(x) {
    dplyr::arrange(
      dplyr::summarise(x, m = sum(deposition_n_t), .by = c(lon, lat)),
      lon,
      lat
    )
  }
  testthat::expect_equal(per_cell(out)$m, per_cell(legacy)$m, tolerance = 1e-9)
  testthat::expect_equal(
    sum(out$deposition_n_t),
    sum(legacy$deposition_n_t),
    tolerance = 1e-9
  )
})

testthat::test_that("C3a: deposition_n_t is the polity share of its cell territory", {
  out <- .nd_c3a_build()
  expected <- .nd_c3a_cell_polity() |>
    dplyr::mutate(
      share = polity_area_ha / sum(polity_area_ha),
      .by = c(lon, lat)
    ) |>
    dplyr::inner_join(.nd_c3a_total_g(), by = c("lon", "lat")) |>
    dplyr::mutate(expected_n_t = value_g_total * share / 1e6)

  joined <- dplyr::inner_join(
    out,
    dplyr::select(expected, lon, lat, area_code, share, expected_n_t),
    by = c("lon", "lat", "area_code")
  )
  testthat::expect_equal(nrow(joined), nrow(out))
  testthat::expect_equal(
    joined$deposition_n_t,
    joined$expected_n_t,
    tolerance = 1e-9
  )
  # The shares this fixture actually exercises, pinned so the block cannot go
  # vacuous if the fixture is edited: the split really is by territory.
  testthat::expect_equal(
    dplyr::arrange(joined, area_code)$share,
    c(0.6, 0.3, 0.1, 0.25, 0.75, 1)
  )
  testthat::expect_true(all(out$method_polity_split == "polity_area_ha"))
})

testthat::test_that("C3a: the key changed, and only where the partitions differ", {
  new <- dplyr::arrange(.nd_c3a_build(), area_code)
  old <- dplyr::arrange(.nd_c3a_build(split = "polity_frac"), area_code)

  # 3000 t in the three-polity cell, 1000 t at high latitude, 500 t alone.
  testthat::expect_equal(old$deposition_n_t, c(1500, 900, 600, 600, 400, 500))
  testthat::expect_equal(new$deposition_n_t, c(1800, 900, 300, 250, 750, 500))
  moved <- new$deposition_n_t != old$deposition_n_t
  testthat::expect_identical(moved, c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE))
  # The single-polity cell (area_code 6) takes the whole cell either way: a
  # partition of one is 1 whatever measures it.
  testthat::expect_equal(new$deposition_n_t[6], old$deposition_n_t[6])
})

testthat::test_that("C3a: the rate stays a whole-cell mean shared by a cell", {
  out <- .nd_c3a_build()
  per_cell <- dplyr::summarise(
    out,
    n_rates = dplyr::n_distinct(deposition_kgn_ha),
    n_polities = dplyr::n_distinct(area_code),
    .by = c(lon, lat)
  )

  testthat::expect_setequal(per_cell$n_polities, c(3L, 2L, 1L))
  # AM-5 risk 1. Dividing the allocated mass by each polycell's own territory
  # would give area_codes 1, 2 and 3 three different rates, each of which
  # recovers the whole cell mass when multiplied back by that polity's area,
  # so the cell would be emitted three times. One rate per cell is what makes
  # that impossible.
  testthat::expect_true(all(per_cell$n_rates == 1L))
  # The rate does not depend on the split at all, so it is bit-identical to
  # the transitional path's.
  testthat::expect_identical(
    out$deposition_kgn_ha,
    .nd_c3a_build(split = "polity_frac")$deposition_kgn_ha
  )
  testthat::expect_equal(
    out$deposition_kgn_ha[out$area_code == 1L],
    3e9 / 1000 / 308000
  )
})

testthat::test_that("C3a: cell_area_ha still cancels out of the mass", {
  base <- .nd_c3a_build()
  tripled <- .nd_c3a_build(
    dplyr::mutate(.nd_c3a_cell_polity(), cell_area_ha = cell_area_ha * 3)
  )

  # AM-5 risk 5, carried onto the new key: substituting a territory area on
  # one side of the cancellation moves every total ~10% down, which reads as
  # the fix succeeding. The mass column must remain blind to cell_area_ha.
  testthat::expect_identical(tripled$deposition_n_t, base$deposition_n_t)
  testthat::expect_equal(tripled$deposition_kgn_ha, base$deposition_kgn_ha / 3)
})

testthat::test_that("C3a: the split is a share, not an area", {
  # Scaling a cell's territory leaves the partition alone. This fails the
  # moment an absolute hectare figure is substituted for the share, the
  # single most likely way to turn a partition into a multiplier.
  scaled <- .nd_c3a_build(
    dplyr::mutate(.nd_c3a_cell_polity(), polity_area_ha = polity_area_ha * 7.5)
  )

  testthat::expect_equal(scaled$deposition_n_t, .nd_c3a_build()$deposition_n_t)
})

testthat::test_that("C3a: split selection is explicit and recorded", {
  crosswalk <- .nd_c0_cell_polity()

  # A support carrying both keys splits geodesically unless told otherwise.
  testthat::expect_true(all(
    .nd_c3a_build()$method_polity_split == "polity_area_ha"
  ))
  testthat::expect_true(all(
    .nd_c3a_build(split = "polity_frac")$method_polity_split == "polity_frac"
  ))
  # Asking for a key the support does not carry aborts instead of quietly
  # falling back to the coarser partition. The abort must be the package's own
  # missing-column refusal naming the key, not an incidental error from
  # whatever reads the absent column first: those two are indistinguishable to
  # a bare expect_error(), and only the first is a contract.
  testthat::expect_error(
    whep::build_n_deposition(
      data = list(
        nhx = .nd_c0_nhx(),
        noy = .nd_c0_noy(),
        cell_polity = crosswalk
      ),
      split = "polity_area_ha"
    ),
    "Missing columns.*cell_polity"
  )
  testthat::expect_error(
    whep::build_n_deposition(
      data = list(
        nhx = .nd_c0_nhx(),
        noy = .nd_c0_noy(),
        cell_polity = crosswalk
      ),
      split = "polity_area_ha"
    ),
    "polity_area_ha"
  )
  testthat::expect_error(
    whep::build_n_deposition(
      data = list(
        nhx = .nd_c0_nhx(),
        noy = .nd_c0_noy(),
        cell_polity = dplyr::select(.nd_c3a_cell_polity(), -polity_frac)
      ),
      split = "polity_frac"
    ),
    "Missing columns.*cell_polity"
  )
  testthat::expect_error(
    whep::build_n_deposition(
      data = list(
        nhx = .nd_c0_nhx(),
        noy = .nd_c0_noy(),
        cell_polity = dplyr::select(.nd_c3a_cell_polity(), -polity_frac)
      ),
      split = "polity_frac"
    ),
    "polity_frac"
  )
  testthat::expect_error(.nd_c3a_build(split = "land_area_ha"))
})

testthat::test_that("C3a: an unusable territory column aborts", {
  cp <- .nd_c3a_cell_polity()

  testthat::expect_error(
    .nd_c3a_build(dplyr::mutate(
      cp,
      polity_area_ha = dplyr::if_else(area_code == 2L, NA_real_, polity_area_ha)
    )),
    "finite"
  )
  testthat::expect_error(
    .nd_c3a_build(dplyr::mutate(cp, polity_area_ha = -polity_area_ha)),
    "finite"
  )
  # A cell with no territory has no partition; dividing by its total would
  # hand every polity of it an NaN share that later sums away to nothing.
  testthat::expect_error(
    .nd_c3a_build(dplyr::mutate(
      cp,
      polity_area_ha = dplyr::if_else(lat == 59.75, 0, polity_area_ha)
    )),
    "no territory"
  )
})

testthat::test_that("C3a: the polity vocabulary is not folded into area_code", {
  # DA-23. build_polycell_support() keys on polity_code, and the area
  # crosswalk merges distinct polities into one area_code (Sudan and South
  # Sudan share 206) or leaves it NA. Deposition rows are keyed on area_code,
  # so the conversion has to happen at the caller's boundary where it is
  # visible, not here where it would look like a partition.
  cp <- .nd_c3a_cell_polity()
  folded <- dplyr::mutate(
    cp,
    area_code = dplyr::if_else(area_code == 3L, 1L, area_code)
  )
  unkeyed <- dplyr::mutate(
    cp,
    area_code = dplyr::if_else(area_code == 3L, NA_integer_, area_code)
  )

  testthat::expect_error(.nd_c3a_build(folded), "duplicated")
  testthat::expect_error(.nd_c3a_build(unkeyed), "duplicated|NA")
  # The transitional key is untouched by this: it is what unmigrated
  # consumers still read, and C3a must not change their behaviour.
  testthat::expect_no_error(.nd_c3a_build(folded, split = "polity_frac"))
})

testthat::test_that("C3a: each cell is partitioned on its own, not with its neighbours", {
  # The cell key is (lon, lat). Every other fixture here puts each cell on its
  # own longitude, so a share grouped by lon alone would pass them all while
  # normalising a whole meridian as if it were one cell -- mass would leak
  # between cells at the same longitude and only the global total would still
  # add up. Two cells on one meridian, carrying different masses and very
  # different territory totals, is what makes that visible.
  nhx <- tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2000L, 3e9,
    -0.25, 0.25, 2000L, 1e9
  )
  cp <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha, ~polity_area_ha,
    -0.25, -0.25, 1L, 0.5, 308000, 240000,
    -0.25, -0.25, 2L, 0.5, 308000, 60000,
    -0.25, 0.25, 3L, 0.5, 308000, 5000,
    -0.25, 0.25, 4L, 0.5, 308000, 15000
  )
  out <- dplyr::arrange(
    whep::build_n_deposition(
      data = list(nhx = nhx, noy = nhx[0, ], cell_polity = cp)
    ),
    area_code
  )

  # Territory shares are 0.8/0.2 within the southern cell and 0.25/0.75 within
  # the northern one. Pooled over the meridian they would be 0.774/0.194/0.016
  # /0.048, which conserves the global 4000 t while moving mass across a cell
  # boundary -- so the per-cell figures, not the total, are the assertion.
  testthat::expect_equal(out$deposition_n_t, c(2400, 600, 250, 750))
  per_cell <- dplyr::summarise(
    out,
    m = sum(deposition_n_t),
    .by = c(lon, lat)
  )
  testthat::expect_equal(sort(per_cell$m), c(1000, 3000))
})

# ---- C3b: the polycell's share decomposed over land, water and ice -----
#
# DA-10 treats the HaNi cell value as a mass of deposition to land in that
# cell, allocates it across the cell's polycells in proportion to territory
# (C3a), and reports the land, inland-water and ice shares SEPARATELY. C3b
# adds that decomposition. DA-14 leaves open whether the nitrogen ledger
# should then take only the terrestrial share; nothing here decides it.
#
# The fixture is built so that the two failure modes this step can hide are
# both visible:
#
#   * computing the land fraction on the CELL instead of on the polycell.
#     Every polity of cell 1 would take 77.5% of its mass as land, which
#     reproduces the correct GLOBAL land total to the tonne (3625 t either
#     way) while moving 405 t between three polities. Only the per-polity
#     figures separate the two, so those are what is asserted.
#   * a decomposition that does not sum back to the polycell's own share,
#     which loses or creates mass behind three individually plausible
#     categories.
#
# So the polities of cell 1 hold territory that is 100%, 50% and 25% land,
# and cell 2's hold 60% and 100%.

.nd_c3b_cell_polity <- function() {
  # The C3a fixture, with each polity's territory decomposed. polity_frac,
  # cell_area_ha and polity_area_ha are unchanged, so the C3a numbers stay
  # directly comparable and any movement is the decomposition's alone.
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~polity_frac,
    ~cell_area_ha,
    ~polity_area_ha,
    ~land_area_ha,
    ~inland_water_ha,
    ~ice_area_ha,
    -0.25, -0.25, 1L, 0.5, 308000, 120000, 120000, 0, 0,
    -0.25, -0.25, 2L, 0.3, 308000, 60000, 30000, 30000, 0,
    -0.25, -0.25, 3L, 0.2, 308000, 20000, 5000, 5000, 10000,
    0.25, 59.75, 4L, 0.6, 155000, 35000, 21000, 7000, 7000,
    0.25, 59.75, 5L, 0.4, 155000, 105000, 105000, 0, 0,
    0.75, 0.25, 6L, 1.0, 308000, 250000, 200000, 50000, 0
  )
}

.nd_c3b_build <- function(cell_polity = .nd_c3b_cell_polity(), ...) {
  whep::build_n_deposition(
    data = list(
      nhx = .nd_c0_nhx(),
      noy = .nd_c0_noy(),
      cell_polity = cell_polity
    ),
    ...
  )
}

# One column per category, ordered so a pinned vector stays readable.
.nd_c3b_wide <- function(out) {
  out |>
    dplyr::select(area_code, area_category, deposition_n_t) |>
    tidyr::pivot_wider(
      names_from = area_category,
      values_from = deposition_n_t
    ) |>
    dplyr::arrange(area_code)
}

testthat::test_that("C3b: the three categories conserve the source mass", {
  out <- .nd_c3b_build()
  source_g <- sum(.nd_c0_nhx()$value_g) + sum(.nd_c0_noy()$value_g)

  testthat::expect_setequal(
    unique(out$area_category),
    c("land", "inland_water", "ice")
  )
  testthat::expect_identical(nrow(out), 18L)
  # DA-18's locked 1e-9 relative bound, on the same source mass C3a's single
  # category conserved. A decomposition redistributes within the polycell.
  testthat::expect_equal(
    sum(out$deposition_n_t) * 1e6,
    source_g,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    sum(out$deposition_n_t),
    sum(.nd_c3a_build()$deposition_n_t),
    tolerance = 1e-9
  )
  # And it conserves per polycell, not merely in the global total: each
  # polity's three categories add back to exactly the share C3a gave it.
  per_polycell <- dplyr::summarise(
    out,
    m = sum(deposition_n_t),
    .by = c(lon, lat, area_code)
  )
  testthat::expect_equal(
    dplyr::arrange(per_polycell, area_code)$m,
    dplyr::arrange(.nd_c3a_build(), area_code)$deposition_n_t,
    tolerance = 1e-9
  )
})

testthat::test_that("C3b: each category is the polycell's share, not the cell's", {
  wide <- .nd_c3b_wide(.nd_c3b_build())

  testthat::expect_equal(wide$land, c(1800, 450, 75, 150, 750, 400))
  testthat::expect_equal(wide$inland_water, c(0, 450, 75, 50, 0, 100))
  testthat::expect_equal(wide$ice, c(0, 0, 150, 50, 0, 0))
  # The trap this fixture exists for: a land fraction taken over the whole
  # cell gives 0.775 to all three polities of cell 1 (1395 / 697.5 / 232.5)
  # and 0.9 to both of cell 2 (225 / 675). Those reproduce the global land
  # total of 3625 t exactly, so a test on the total alone passes while
  # 405 t sits in the wrong polities.
  testthat::expect_equal(sum(wide$land), 3625)
  testthat::expect_false(isTRUE(all.equal(
    wide$land,
    c(1395, 697.5, 232.5, 225, 675, 400)
  )))
  testthat::expect_equal(sum(wide$inland_water), 675)
  testthat::expect_equal(sum(wide$ice), 200)
})

testthat::test_that("C3b: the rate is untouched by the decomposition", {
  out <- .nd_c3b_build()
  undecomposed <- .nd_c3a_build()

  # AM-5 risk 1, in its C3b form: referencing the rate to the land the mass
  # falls on would give a cell's polities three different rates AND make the
  # ledger's rate x area recover the cell mass once per polity. The rate is
  # a whole-cell mean, so it is the same on every category row of every
  # polity of a cell -- and bit-identical to the undecomposed path's.
  per_cell <- dplyr::summarise(
    out,
    n_rates = dplyr::n_distinct(deposition_kgn_ha),
    .by = c(lon, lat)
  )
  testthat::expect_true(all(per_cell$n_rates == 1L))
  joined <- dplyr::inner_join(
    dplyr::filter(out, area_category == "land"),
    dplyr::select(
      undecomposed,
      lon,
      lat,
      area_code,
      rate_c3a = deposition_kgn_ha
    ),
    by = c("lon", "lat", "area_code")
  )
  testthat::expect_identical(joined$deposition_kgn_ha, joined$rate_c3a)
  testthat::expect_equal(
    unique(out$deposition_kgn_ha[out$area_code == 1L]),
    3e9 / 1000 / 308000
  )
})

testthat::test_that("C3b: the decomposition is a share, not an area", {
  # Scaling every category and the territory together leaves the categories
  # alone. This fails the moment an absolute hectare figure is substituted
  # for the fraction, the same way C3a's scaling test guards the split key.
  scaled <- .nd_c3b_build(
    dplyr::mutate(
      .nd_c3b_cell_polity(),
      dplyr::across(
        c(polity_area_ha, land_area_ha, inland_water_ha, ice_area_ha),
        function(x) x * 7.5
      )
    )
  )

  testthat::expect_equal(scaled$deposition_n_t, .nd_c3b_build()$deposition_n_t)
})

testthat::test_that("C3b: category selection is explicit and recorded", {
  decomposed <- .nd_c3b_build()
  undecomposed <- .nd_c3a_build()

  # A support carrying the three columns decomposes unless told otherwise.
  testthat::expect_true(all(decomposed$method_area_split == "land_water_ice"))
  testthat::expect_identical(
    .nd_c3b_build(categories = "land_water_ice"),
    decomposed
  )
  # A support that cannot be decomposed says so rather than calling its one
  # undecomposed row "land", which would be a claim it cannot support.
  testthat::expect_true(all(undecomposed$method_area_split == "none"))
  testthat::expect_true(all(undecomposed$area_category == "territory"))
  # "none" is reachable by name on a support that could decompose, so the
  # two views are comparable on identical input.
  none <- .nd_c3b_build(categories = "none")
  testthat::expect_true(all(none$area_category == "territory"))
  testthat::expect_equal(none$deposition_n_t, undecomposed$deposition_n_t)
  # Asking for the decomposition without the columns aborts instead of
  # quietly returning one undecomposed row per polycell, which would be
  # indistinguishable from a decomposition whose water and ice were zero.
  testthat::expect_error(
    .nd_c3a_build(categories = "land_water_ice"),
    "Missing columns.*cell_polity"
  )
  testthat::expect_error(
    .nd_c3a_build(categories = "land_water_ice"),
    "land_area_ha"
  )
  testthat::expect_error(.nd_c3b_build(categories = "terrestrial"))
})

testthat::test_that("C3b: a decomposition not summing to the territory aborts", {
  cp <- .nd_c3b_cell_polity()

  # S-A1. Categories that do not add up to polity_area_ha are not a
  # decomposition of it: the shares would not sum to the polycell's own, so
  # the cell's mass would be silently lost or duplicated behind three
  # individually plausible numbers.
  testthat::expect_error(
    .nd_c3b_build(dplyr::mutate(cp, ice_area_ha = ice_area_ha + 1)),
    "must sum to"
  )
  testthat::expect_error(
    .nd_c3b_build(dplyr::mutate(cp, land_area_ha = land_area_ha * 0.5)),
    "must sum to"
  )
  # 1e-9 relative is DA-18's bound, and it is a bound rather than a
  # formality: float noise at that scale passes.
  testthat::expect_no_error(
    .nd_c3b_build(dplyr::mutate(
      cp,
      land_area_ha = land_area_ha * (1 + 1e-12)
    ))
  )
})

testthat::test_that("C3b: an unusable category column aborts", {
  cp <- .nd_c3b_cell_polity()

  testthat::expect_error(
    .nd_c3b_build(dplyr::mutate(
      cp,
      land_area_ha = dplyr::if_else(area_code == 2L, NA_real_, land_area_ha)
    )),
    "land_area_ha.*finite"
  )
  # A negative category still summing to the territory would let one
  # category borrow mass from another with conservation intact, so the sum
  # check alone cannot see it.
  testthat::expect_error(
    .nd_c3b_build(dplyr::mutate(
      cp,
      inland_water_ha = inland_water_ha - 1000,
      ice_area_ha = ice_area_ha + 1000
    )),
    "inland_water_ha.*finite"
  )
})

testthat::test_that("C3b: a polycell holding no territory yields no NaN", {
  # A zero-territory polycell takes no mass, so its category fraction is
  # 0 / 0. Left as NaN it would turn every downstream sum into NA while the
  # per-row values still looked fine.
  cp <- dplyr::mutate(
    .nd_c3b_cell_polity(),
    dplyr::across(
      c(polity_area_ha, land_area_ha, inland_water_ha, ice_area_ha),
      function(x) dplyr::if_else(area_code == 3L, 0, x)
    )
  )
  out <- .nd_c3b_build(cp)

  testthat::expect_false(anyNA(out$deposition_n_t))
  testthat::expect_equal(out$deposition_n_t[out$area_code == 3L], c(0, 0, 0))
  # Cell 1's territory is now 180,000 ha over two polities, so its 3000 t
  # goes 2000 / 1000 and none of it disappears.
  testthat::expect_equal(sum(out$deposition_n_t) * 1e6, 4.5e9)
  testthat::expect_equal(
    dplyr::filter(out, area_code == 1L, area_category == "land")$deposition_n_t,
    2000
  )
})

testthat::test_that("C3b: a scope resolves to categories, per decomposition", {
  # .n_inputs_deposition() reads the labels from the table's own
  # method_area_split rather than hardcoding them, so a table produced under
  # either decomposition resolves to the categories its scope covers.
  #
  # DA-14, decided 2026-08-06: "territory" is the DEFAULT and covers all
  # three. Deposition on inland water and on ice is real flux driving
  # indirect N2O and the eutrophication pathway; it is not discardable.
  testthat::expect_identical(
    whep:::.nd_scope_categories("territory", "land_water_ice"),
    c("land", "inland_water", "ice")
  )
  testthat::expect_identical(
    whep:::.nd_scope_categories("territory", "none"),
    "territory"
  )
  testthat::expect_identical(
    whep:::.nd_scope_categories("land", "land_water_ice"),
    "land"
  )
  # No silent fallback: a support that never decomposed its territory cannot
  # serve the land scope, and returning its undecomposed row under a "land"
  # label would be a claim it cannot support.
  testthat::expect_error(
    whep:::.nd_scope_categories("land", "none"),
    "needs a decomposed territory"
  )
  # An unrecognised method aborts. Silently returning NA would make the
  # ledger's filter match nothing, which under .ni_empty() semantics reads
  # as "no deposition input" rather than as a defect.
  testthat::expect_error(
    whep:::.nd_scope_categories("territory", "land"),
    "Unknown.*method_area_split"
  )
  testthat::expect_error(
    whep:::.nd_scope_categories(
      "territory",
      c("none", "land_water_ice")
    ),
    "Unknown.*method_area_split"
  )
})

testthat::test_that("C3b: the land scope reproduces the measured land share", {
  # The mirror of the default. On the C3b fixture the three categories carry
  # 3625 / 675 / 200 t of the 4500 t source, so the land scope is 80.556% of
  # the territory scope. The same construction measured 60.7385 Tg of 61.6285
  # Tg on real 2014 HaNi input (AM-30), a 1.444% fall -- the number DA-14
  # declined to take, pinned here so both scopes stay reproducible.
  wide <- .nd_c3b_wide(.nd_c3b_build())
  total <- sum(wide$land) + sum(wide$inland_water) + sum(wide$ice)

  testthat::expect_equal(total, 4500)
  testthat::expect_equal(sum(wide$land) / total, 0.8055555555555556)
  testthat::expect_equal(1 - sum(wide$land) / total, 0.19444444444444442)
})
# ---- polity_validity (#675) -------------------------------------------

# Area 277 (South Sudan) exists only from 2011, so a cell the present-day
# crosswalk labels 277 names a state that did not exist in any earlier year.
.nd_out_of_span_cell_polity <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    -0.25, -0.25, 277L, 1, 300000
  )
}

.nd_out_of_span_data <- function() {
  species <- tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2000L, 1000000,
    -0.25, -0.25, 2020L, 1000000
  )
  list(
    nhx = species,
    noy = species,
    cell_polity = .nd_out_of_span_cell_polity()
  )
}

testthat::test_that("build_n_deposition names an anachronistic polity", {
  testthat::expect_warning(
    out <- whep::build_n_deposition(data = .nd_out_of_span_data()),
    "did not exist in that row's year"
  )

  # "keep" is the default and keeps both rows, so nothing published moves.
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(
    out$reporting_polity_code[out$year == 2000L],
    "SSD-2011-2025"
  )
})

testthat::test_that("build_n_deposition honours drop and flag", {
  testthat::expect_warning(
    dropped <- whep::build_n_deposition(
      data = .nd_out_of_span_data(),
      polity_validity = "drop"
    )
  )
  testthat::expect_warning(
    flagged <- whep::build_n_deposition(
      data = .nd_out_of_span_data(),
      polity_validity = "flag"
    )
  )

  testthat::expect_equal(dropped$year, 2020L)
  testthat::expect_equal(
    flagged$reporting_polity_out_of_span,
    flagged$year == 2000L
  )
})

testthat::test_that("build_n_deposition is silent when every year is in span", {
  testthat::expect_no_warning(
    whep::build_n_deposition(years = 2020L, data = .nd_out_of_span_data())
  )
})
