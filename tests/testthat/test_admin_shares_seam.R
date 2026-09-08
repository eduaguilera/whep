# Tests for R/admin_shares_seam.R (plan T28, the seam back-cast core).
#
# The three new functions are reached through `whep:::` rather than
# `whep::`: roxygen has not run on this branch yet, so they are not in
# NAMESPACE, and the dispatcher's documentation pass exports them.
#
# Fixtures: `helper_level1_grid.R` (T37) for the main path and
# `helper_seam_1890.R` for the reduction and seam-agnosticism proofs
# T29 reuses.

# --- shared fixture builders --------------------------------------------------

# Country A's granted-depth compartments only. B is level 0 and carries no
# `level_polity_code`, so it is not a unit of this back-cast.
seam_grid_a <- function() {
  dplyr::filter(.level1_country_grid(), level == 1L)
}

# The T37 cropland with cell 1 halved and cell 4 doubled in 1974, so A1
# and A2 move in opposite directions and a common factor cannot cancel in
# the renormalisation.
seam_tilted_cropland <- function() {
  .level1_gridded_cropland() |>
    dplyr::mutate(
      cropland_ha = dplyr::case_when(
        year == 1974L & lon == 10.25 & lat == 40.25 ~ cropland_ha * 0.5,
        year == 1974L & lon == 11.75 & lat == 40.25 ~ cropland_ha * 2,
        .default = cropland_ha
      )
    )
}

seam_extent_a <- function(cropland = seam_tilted_cropland()) {
  whep:::aggregate_unit_extent(cropland, seam_grid_a(), "cropland_ha")
}

# A minimal shares table: one container, one item, the binding indicator.
seam_shares <- function(units, years, share, area_code = 920L) {
  tibble::tibble(
    area_code = as.integer(area_code),
    level = 1L,
    item_prod_code = 15L,
    indicator_used = "area_harvested",
    level_polity_code = units,
    year = as.integer(years),
    share = share,
    treatment_year = "observed"
  )
}

# A minimal extent table in `aggregate_unit_extent()`'s output shape.
seam_extent <- function(units, years, extent_ha, area_code = 920L) {
  tibble::tibble(
    area_code = as.integer(area_code),
    level_polity_code = units,
    level = 1L,
    year = as.integer(years),
    extent_ha = extent_ha,
    extent_basis = "cropland_ha"
  )
}

seam_count <- function(out, name) {
  out$counters$n[out$counters$diagnostic == name]
}

seam_share_at <- function(out, unit, yr) {
  out$shares |>
    dplyr::filter(level_polity_code == unit, year == yr) |>
    dplyr::pull(share)
}

# --- aggregate_unit_extent ----------------------------------------------------

test_that("the unit extent is the compartment-weighted cell sum", {
  extent <- seam_extent_a(.level1_gridded_cropland())

  # A1 holds cell 1 (1000 ha) and cell 3 (800) whole and 0.3 of cell 2
  # (1200); A2 holds 0.5 of cell 2, cell 4 (900) and cell 6 (700).
  a1 <- extent |>
    dplyr::filter(level_polity_code == "A-A1-1900-2100", year == 1975L)
  a2 <- extent |>
    dplyr::filter(level_polity_code == "A-A2-1975-2100", year == 1975L)

  expect_equal(a1$extent_ha, 1000 + 0.3 * 1200 + 800)
  expect_equal(a2$extent_ha, 0.5 * 1200 + 900 + 700)
  expect_equal(unique(extent$extent_basis), "cropland_ha")
  expect_named(
    extent,
    c(
      "area_code",
      "level_polity_code",
      "level",
      "year",
      "extent_ha",
      "extent_basis"
    )
  )
})

test_that("t0 geometry is held fixed: an edge starting later still counts", {
  # A2's containment edge starts in 1975 (`.level1_country_grid()`), and
  # the extent is still computed for 1974 -- the plan's t0-geometry
  # convention, which is what lets a 1974 back-cast row exist for A2.
  extent <- seam_extent_a(.level1_gridded_cropland())
  a2_1974 <- extent |>
    dplyr::filter(level_polity_code == "A-A2-1975-2100", year == 1974L)

  expect_equal(nrow(a2_1974), 1L)
  expect_gt(a2_1974$extent_ha, 0)
})

test_that("the livestock proxy classes are expressible as quantities", {
  grid <- seam_grid_a()
  pasture <- .level1_gridded_pasture()

  # `pasture` class: pasture_ha + rangeland_ha (spatialize_livestock.R).
  ext_pasture <- whep:::aggregate_unit_extent(
    pasture,
    grid,
    c("pasture_ha", "rangeland_ha")
  )
  a1 <- ext_pasture |>
    dplyr::filter(level_polity_code == "A-A1-1900-2100", year == 1975L)
  expect_equal(a1$extent_ha, (200 + 100) + 0.3 * (150 + 250) + (100 + 50))
  expect_equal(unique(ext_pasture$extent_basis), "pasture_ha+rangeland_ha")

  # `rangeland` class: rangeland_ha alone.
  ext_range <- whep:::aggregate_unit_extent(pasture, grid, "rangeland_ha")
  expect_equal(
    ext_range |>
      dplyr::filter(level_polity_code == "A-A1-1900-2100", year == 1975L) |>
      dplyr::pull(extent_ha),
    100 + 0.3 * 250 + 50
  )
})

test_that("a weighted lookup expresses the mixed class and per-item bases", {
  grid <- seam_grid_a()
  wide <- dplyr::left_join(
    .level1_gridded_pasture(),
    .level1_gridded_cropland(),
    by = c("lon", "lat", "year")
  )
  lookup <- tibble::tribble(
    ~species_group, ~quantity,      ~weight,
    "pigs",         "cropland_ha",  1,
    "other",        "pasture_ha",   0.5,
    "other",        "rangeland_ha", 0.5,
    "other",        "cropland_ha",  0.5
  )

  extent <- whep:::aggregate_unit_extent(
    wide,
    grid,
    lookup,
    extent_by = "species_group"
  )
  mixed <- extent |>
    dplyr::filter(
      species_group == "other",
      level_polity_code == "A-A1-1900-2100",
      year == 1975L
    )
  # 0.5 * (pasture + rangeland) + 0.5 * cropland, per compartment.
  expect_equal(
    mixed$extent_ha,
    0.5 *
      ((200 + 100) + 0.3 * (150 + 250) + (100 + 50)) +
      0.5 * (1000 + 0.3 * 1200 + 800)
  )
  expect_equal(
    mixed$extent_basis,
    "0.5*cropland_ha+0.5*pasture_ha+0.5*rangeland_ha"
  )
  expect_equal(
    extent |>
      dplyr::filter(species_group == "pigs") |>
      dplyr::pull(extent_basis) |>
      unique(),
    "cropland_ha"
  )
})

test_that("a gridded key column becomes an extent key", {
  typed <- tidyr::crossing(
    .level1_gridded_cropland(),
    luh2_type = c("c3ann", "c4ann")
  ) |>
    dplyr::mutate(
      type_ha = dplyr::if_else(luh2_type == "c3ann", 0.75, 0.25) * cropland_ha
    ) |>
    dplyr::select(lon, lat, year, luh2_type, type_ha)

  extent <- whep:::aggregate_unit_extent(
    typed,
    seam_grid_a(),
    "type_ha",
    extent_by = "luh2_type"
  )
  expect_true("luh2_type" %in% names(extent))
  expect_equal(
    extent |>
      dplyr::filter(
        luh2_type == "c3ann",
        level_polity_code == "A-A1-1900-2100",
        year == 1975L
      ) |>
      dplyr::pull(extent_ha),
    0.75 * (1000 + 0.3 * 1200 + 800)
  )
})

test_that("the residual unit is the container minus its units", {
  extent <- whep:::aggregate_unit_extent(
    .level1_gridded_cropland(),
    seam_grid_a(),
    "cropland_ha",
    container_frac = .level1_level0_shares(),
    residual_code = "A-RESIDUAL"
  )
  year <- dplyr::filter(extent, year == 1975L)
  residual <- dplyr::filter(year, level_polity_code == "A-RESIDUAL")
  units <- dplyr::filter(year, level_polity_code != "A-RESIDUAL")

  # Container A holds cells 1, 3, 4, 6 whole and 0.8 of cell 2.
  container <- 1000 + 800 + 900 + 700 + 0.8 * 1200
  expect_equal(residual$extent_ha, container - sum(units$extent_ha))
  expect_equal(residual$level, 1L)
})

test_that("a residual without container fractions aborts", {
  expect_error(
    whep:::aggregate_unit_extent(
      .level1_gridded_cropland(),
      seam_grid_a(),
      "cropland_ha",
      residual_code = "A-RESIDUAL"
    ),
    "container_frac"
  )
})

test_that("a residual code that names a real unit aborts", {
  expect_error(
    whep:::aggregate_unit_extent(
      .level1_gridded_cropland(),
      seam_grid_a(),
      "cropland_ha",
      container_frac = .level1_level0_shares(),
      residual_code = "A-A1-1900-2100"
    ),
    "already a unit"
  )
})

test_that("duplicate compartments and missing quantities abort", {
  doubled <- dplyr::bind_rows(seam_grid_a(), seam_grid_a()[1, ])
  expect_error(
    whep:::aggregate_unit_extent(
      .level1_gridded_cropland(),
      doubled,
      "cropland_ha"
    ),
    "more than"
  )

  holed <- .level1_gridded_cropland()
  holed$cropland_ha[1] <- NA_real_
  expect_error(
    whep:::aggregate_unit_extent(holed, seam_grid_a(), "cropland_ha"),
    "missing value"
  )
})

test_that("an unknown quantity column aborts naming it", {
  expect_error(
    whep:::aggregate_unit_extent(
      .level1_gridded_cropland(),
      seam_grid_a(),
      "grassland_ha"
    ),
    "grassland_ha"
  )
})

# --- check_extent_jumps -------------------------------------------------------

test_that("an isolated extent collapse is flagged, extent_basis aside", {
  extent <- seam_extent(
    rep("Z1", 4),
    1900:1903,
    c(1000, 1010, 5, 1020)
  )
  flags <- whep:::check_extent_jumps(extent)

  expect_equal(flags$year, c(1902L, 1903L))
  expect_false("extent_basis" %in% names(flags))
  expect_true(all(
    c("area_code", "level_polity_code", "level") %in%
      names(
        flags
      )
  ))
})

test_that("a smooth extent is not flagged", {
  extent <- seam_extent(rep("Z1", 4), 1900:1903, c(1000, 1010, 1020, 1030))
  expect_equal(nrow(whep:::check_extent_jumps(extent)), 0L)
})

# --- backcast_admin_shares: the main path -------------------------------------

test_that("an empty back-cast window is a no-op on the observed rows", {
  # t0 resolves to 1974, the extent's first year, so there is nothing
  # before the seam. Every observed row must survive untouched.
  shares <- .level1_admin_shares()
  out <- whep:::backcast_admin_shares(shares, seam_extent_a())

  observed <- dplyr::filter(out$shares, treatment == "observed")
  expect_equal(nrow(observed), nrow(shares))
  expect_equal(
    dplyr::arrange(observed, year, item_prod_code, level_polity_code)$share,
    dplyr::arrange(shares, year, item_prod_code, level_polity_code)$share
  )
  expect_equal(seam_count(out, "seam_supersedes_observed"), 0L)
  expect_false(any(out$shares$treatment %in% "backcast_t0_geometry"))
})

test_that("the T37 fixture back-casts 1974 on t0 = 1975 geometry", {
  shares <- .level1_admin_shares()
  extent <- seam_extent_a()
  seam <- tibble::tibble(
    area_code = 900L,
    item_prod_code = c(15L, 44L),
    t0 = 1975L
  )
  out <- whep:::backcast_admin_shares(shares, extent, seam = seam)

  back <- out$shares |>
    dplyr::filter(year == 1974L, item_prod_code == 15L) |>
    dplyr::arrange(level_polity_code)

  # A2's edge starts in 1975, so its 1974 row exists only because the
  # unit set and the geometry are both held at t0.
  expect_equal(
    back$level_polity_code,
    c("A-A1-1900-2100", "A-A2-1975-2100")
  )
  expect_equal(back$treatment, rep("backcast_t0_geometry", 2))
  expect_true(all(is.na(back$value)))
  expect_true(all(is.na(back$treatment_year)))

  # The plan's formula, written out: s_u(t0) * E_u(t) / E_u(t0),
  # renormalised over the t0 unit set.
  e <- function(unit, yr) {
    extent$extent_ha[extent$level_polity_code == unit & extent$year == yr]
  }
  s0 <- c(650 / 1100, 450 / 1100)
  tilde <- c(
    s0[1] * e("A-A1-1900-2100", 1974L) / e("A-A1-1900-2100", 1975L),
    s0[2] * e("A-A2-1975-2100", 1974L) / e("A-A2-1975-2100", 1975L)
  )
  expect_equal(back$share, tilde / sum(tilde), tolerance = 1e-12)
  expect_equal(sum(back$share), 1, tolerance = 1e-12)

  # The tilt is real: the back-cast vector is not the t0 vector.
  expect_gt(max(abs(back$share - s0)), 0.1)
})

test_that("the superseded observations of a seam override are counted", {
  shares <- .level1_admin_shares()
  seam <- tibble::tibble(
    area_code = 900L,
    item_prod_code = c(15L, 44L),
    t0 = 1976L
  )
  out <- whep:::backcast_admin_shares(shares, seam_extent_a(), seam = seam)

  # 1974 (A1 only, both items) and 1975 (both units, both items) sit
  # below t0 = 1976: 2 + 4 = 6 observed rows superseded.
  expect_equal(seam_count(out, "seam_supersedes_observed"), 6L)
  expect_equal(
    nrow(dplyr::filter(out$shares, treatment == "backcast_t0_geometry")),
    8L
  )
})

test_that("shares sum to 1 per container, item and back-cast year", {
  seam <- tibble::tibble(
    area_code = 900L,
    item_prod_code = c(15L, 44L),
    t0 = 1975L
  )
  out <- whep:::backcast_admin_shares(
    .level1_admin_shares(),
    seam_extent_a(),
    seam = seam
  )
  sums <- out$shares |>
    dplyr::filter(!is.na(share)) |>
    dplyr::summarise(
      total = sum(share),
      .by = c(area_code, item_prod_code, year)
    )
  expect_equal(sums$total, rep(1, nrow(sums)), tolerance = 1e-12)
})

test_that("the observed subset still conforms to the T33 contract", {
  seam <- tibble::tibble(
    area_code = 900L,
    item_prod_code = c(15L, 44L),
    t0 = 1975L
  )
  out <- whep:::backcast_admin_shares(
    .level1_admin_shares(),
    seam_extent_a(),
    seam = seam
  )
  observed <- out$shares |>
    dplyr::filter(treatment == "observed") |>
    dplyr::select(dplyr::all_of(names(whep::admin_shares_prototype())))

  expect_equal(
    nrow(whep::check_table_schema(observed, whep::admin_shares_schema())),
    0L
  )
  # The produced rows deliberately leave the closed schema: no reported
  # value, and `treatment_year` reserved for the interior-gap rule.
  produced <- dplyr::filter(out$shares, treatment != "observed")
  expect_true(all(is.na(produced$value)))
  expect_true(all(is.na(produced$treatment_year)))
})

# --- Tier A identity, reduction and seam-agnosticism --------------------------

test_that("at t0 the back-cast equals the observed shares exactly", {
  out <- whep:::backcast_admin_shares(
    .seam1890_shares(),
    .seam1890_extent()
  )
  at_t0 <- out$shares |>
    dplyr::filter(year == .seam1890_first_observed()) |>
    dplyr::arrange(level_polity_code)
  expected <- .seam1890_shares() |>
    dplyr::filter(year == .seam1890_first_observed()) |>
    dplyr::arrange(level_polity_code)

  expect_equal(at_t0$share, expected$share, tolerance = 1e-8)
  expect_equal(at_t0$treatment, rep("observed", nrow(at_t0)))
})

test_that("shares proportional to the extent reduce to the LUH2 vector", {
  out <- whep:::backcast_admin_shares(
    .seam1890_shares(),
    .seam1890_extent()
  )
  compared <- out$shares |>
    dplyr::select(level_polity_code, year, share) |>
    dplyr::inner_join(
      .seam1890_luh2_shares(),
      by = c("level_polity_code", "year")
    )

  expect_equal(nrow(compared), 3L * length(.seam1890_years()))
  expect_equal(compared$share, compared$luh2_share, tolerance = 1e-12)
})

test_that("moving t0 moves treatment, not the shares", {
  shares <- .seam1890_shares()
  extent <- .seam1890_extent()
  early <- whep:::backcast_admin_shares(shares, extent)
  late <- whep:::backcast_admin_shares(
    shares,
    extent,
    seam = tibble::tibble(area_code = 910L, item_prod_code = 15L, t0 = 1905L)
  )

  key <- c("level_polity_code", "year")
  joined <- dplyr::inner_join(
    dplyr::select(early$shares, dplyr::all_of(key), share, treatment),
    dplyr::select(late$shares, dplyr::all_of(key), share, treatment),
    by = key,
    suffix = c("_early", "_late")
  )
  expect_equal(nrow(joined), nrow(early$shares))
  expect_equal(joined$share_early, joined$share_late, tolerance = 1e-8)

  moved <- dplyr::filter(joined, treatment_early != treatment_late)
  expect_equal(sort(unique(moved$year)), 1900:1904)
  expect_equal(unique(moved$treatment_early), "observed")
  expect_equal(unique(moved$treatment_late), "backcast_t0_geometry")
  expect_equal(seam_count(late, "seam_supersedes_observed"), 15L)
})

test_that("the chained fill and the plan's direct ratio agree", {
  out <- whep:::backcast_admin_shares(
    .seam1890_shares(),
    .seam1890_extent()
  )
  extent <- .seam1890_extent()
  t0 <- .seam1890_first_observed()
  anchor <- .seam1890_shares() |>
    dplyr::filter(year == t0) |>
    dplyr::select(level_polity_code, share_t0 = share)

  direct <- extent |>
    dplyr::inner_join(anchor, by = "level_polity_code") |>
    dplyr::inner_join(
      extent |>
        dplyr::filter(year == t0) |>
        dplyr::select(level_polity_code, extent_t0 = extent_ha),
      by = "level_polity_code"
    ) |>
    dplyr::mutate(tilde = share_t0 * extent_ha / extent_t0) |>
    dplyr::mutate(expected = tilde / sum(tilde), .by = year) |>
    dplyr::filter(year < t0)

  compared <- dplyr::inner_join(
    direct,
    dplyr::select(out$shares, level_polity_code, year, share),
    by = c("level_polity_code", "year")
  )
  expect_equal(nrow(compared), 30L)
  expect_equal(compared$share, compared$expected, tolerance = 1e-12)
  expect_equal(seam_count(out, "direct_ratio_repair"), 0L)
})

# --- the four zero cases ------------------------------------------------------

test_that("case (a) holds a zero-extent t0 share outside the renormalisation", {
  shares <- seam_shares(
    c("Z", "Y", "X"),
    rep(1902L, 3),
    c(0.2, 0.5, 0.3)
  )
  extent <- seam_extent(
    rep(c("Z", "Y", "X"), each = 3),
    rep(1900:1902, times = 3),
    c(50, 50, 0, 100, 130, 160, 100, 100, 100)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  expect_equal(seam_count(out, "zero_extent_t0_held"), 1L)
  expect_equal(seam_share_at(out, "Z", 1900L), 0.2)
  expect_equal(seam_share_at(out, "Z", 1901L), 0.2)
  # Siblings share the remaining 0.8 in proportion to their own modulated
  # values: Y = 0.5 * 100/160, X = 0.3 * 100/100 = 0.3.
  sib <- c(0.5 * 100 / 160, 0.3)
  expect_equal(
    seam_share_at(out, "Y", 1900L),
    sib[1] * 0.8 / sum(sib),
    tolerance = 1e-12
  )
  expect_equal(
    seam_share_at(out, "X", 1900L),
    sib[2] * 0.8 / sum(sib),
    tolerance = 1e-12
  )
  expect_equal(
    sum(dplyr::filter(out$shares, year == 1900L)$share),
    1,
    tolerance = 1e-12
  )
})

test_that("case (b) zeroes a zero-extent year and gives its mass away", {
  shares <- seam_shares(c("P", "Q"), rep(1902L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    rep(c("P", "Q"), each = 3),
    rep(1900:1902, times = 2),
    c(0, 100, 150, 100, 100, 100)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  expect_equal(seam_count(out, "zero_extent_year_zeroed"), 1L)
  expect_equal(seam_share_at(out, "P", 1900L), 0)
  expect_equal(seam_share_at(out, "Q", 1900L), 1)
  # 1901 is untouched by the zero case: 0.6 * 100/150 = 0.4 against 0.4.
  expect_equal(
    seam_share_at(out, "P", 1901L),
    0.4 / 0.8,
    tolerance = 1e-12
  )
})

test_that("a zero year later in the chain still yields the plan's ratio", {
  # The growth chain breaks at the zero, so `fill_proxy_growth()` leaves
  # 1900 NA; the direct ratio supplies it and the repair is counted.
  shares <- seam_shares(c("R", "Q"), rep(1902L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    rep(c("R", "Q"), each = 3),
    rep(1900:1902, times = 2),
    c(150, 0, 200, 100, 100, 100)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  expect_equal(seam_count(out, "direct_ratio_repair"), 1L)
  expect_equal(seam_count(out, "zero_extent_year_zeroed"), 1L)
  expect_equal(seam_share_at(out, "R", 1901L), 0)
  expect_equal(
    seam_share_at(out, "R", 1900L),
    0.45 / (0.45 + 0.4),
    tolerance = 1e-12
  )
})

test_that("case (c) refuses a year whose whole unit set has no extent", {
  shares <- seam_shares(c("P", "Q"), rep(1902L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    rep(c("P", "Q"), each = 3),
    rep(1900:1902, times = 2),
    c(0, 100, 150, 0, 100, 100)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  expect_equal(seam_count(out, "zero_sum_year_refused"), 1L)
  expect_equal(seam_count(out, "zero_extent_year_zeroed"), 2L)
  refused <- dplyr::filter(out$shares, year == 1900L)
  expect_true(all(is.na(refused$share)))
  expect_true(all(is.na(refused$treatment)))
  expect_equal(nrow(refused), 2L)
})

test_that("case (d) contributes nothing and is counted", {
  shares <- seam_shares(c("W", "V"), rep(1902L, 2), c(0, 1))
  extent <- seam_extent(
    rep(c("W", "V"), each = 3),
    rep(1900:1902, times = 2),
    c(10, 10, 0, 100, 100, 100)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  expect_equal(seam_count(out, "zero_extent_t0_no_share"), 1L)
  expect_equal(seam_count(out, "zero_extent_t0_held"), 0L)
  expect_equal(seam_share_at(out, "W", 1900L), 0)
  expect_equal(seam_share_at(out, "V", 1900L), 1)
  expect_equal(seam_count(out, "incomplete_unit_set"), 0L)
})

test_that("every diagnostic in the vocabulary has a counter row", {
  out <- whep:::backcast_admin_shares(
    .seam1890_shares(),
    .seam1890_extent()
  )
  expect_setequal(out$counters$diagnostic, whep:::.backcast_diag_vocabulary())
  expect_true(all(out$counters$n >= 0))
  expect_equal(sum(out$counters$n), nrow(out$diagnostics))

  # The same identity where the diagnostics are not empty: a diagnostic
  # name outside the vocabulary would be dropped by the counter join and
  # would show up here as a shortfall.
  noisy <- whep:::backcast_admin_shares(
    seam_shares(c("Z", "Y"), rep(1902L, 2), c(0.2, 0.8)),
    seam_extent(
      rep(c("Z", "Y"), each = 3),
      rep(1900:1902, times = 2),
      c(50, 50, 0, 100, 130, 160)
    )
  )
  expect_gt(nrow(noisy$diagnostics), 0L)
  expect_equal(sum(noisy$counters$n), nrow(noisy$diagnostics))
  expect_true(all(
    noisy$diagnostics$diagnostic %in% whep:::.backcast_diag_vocabulary()
  ))
})

# --- the residual unit --------------------------------------------------------

test_that("a residual unit is renormalised with the reporting units", {
  shares <- seam_shares(
    c("P", "Q", "A-RESIDUAL"),
    rep(1902L, 3),
    c(0.5, 0.3, 0.2)
  )
  extent <- seam_extent(
    rep(c("P", "Q", "A-RESIDUAL"), each = 3),
    rep(1900:1902, times = 3),
    c(100, 100, 100, 200, 200, 200, 100, 140, 200)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  # The residual's own extent shrinks fastest going back, so its share
  # falls and the reporting units take the mass.
  tilde <- c(0.5 * 1, 0.3 * 1, 0.2 * 100 / 200)
  expect_equal(
    seam_share_at(out, "A-RESIDUAL", 1900L),
    tilde[3] / sum(tilde),
    tolerance = 1e-12
  )
  expect_equal(
    sum(dplyr::filter(out$shares, year == 1900L)$share),
    1,
    tolerance = 1e-12
  )
})

# --- gap rule, max_gap and the clamped slice ----------------------------------

test_that("interior and trailing gaps are refused and counted", {
  shares <- seam_shares(
    c("P", "Q", "P", "Q"),
    c(1901L, 1901L, 1903L, 1903L),
    c(0.6, 0.4, 0.5, 0.5)
  )
  extent <- seam_extent(
    rep(c("P", "Q"), each = 5),
    rep(1900:1904, times = 2),
    rep(100, 10)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  # 1902 sits between two observed years; 1904 after the last one.
  expect_equal(seam_count(out, "interior_gap_refused"), 2L)
  expect_equal(seam_count(out, "trailing_gap_refused"), 2L)
  expect_true(all(is.na(seam_share_at(out, "P", 1902L))))
  expect_equal(unique(out$shares$gap_rule), "refuse_interior")
  expect_equal(out$settings$gap_rule, "refuse_interior")
})

test_that("max_gap bounds the back-cast run and max_gap_linear is inert", {
  shares <- seam_shares(c("P", "Q"), rep(1904L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    rep(c("P", "Q"), each = 5),
    rep(1900:1904, times = 2),
    rep(100, 10)
  )
  bounded <- whep:::backcast_admin_shares(
    shares,
    extent,
    settings = list(max_gap = 2)
  )

  # Four back-cast years against a two-year budget: nothing is filled,
  # every year is refused for want of a complete unit set.
  expect_equal(seam_count(bounded, "max_gap_exceeded"), 8L)
  expect_equal(seam_count(bounded, "incomplete_unit_set"), 4L)
  expect_true(all(is.na(dplyr::filter(bounded$shares, year < 1904L)$share)))
  expect_equal(bounded$settings$max_gap, 2)

  # `max_gap_linear` cannot bite while the window ends at t0.
  a <- whep:::backcast_admin_shares(
    shares,
    extent,
    settings = list(max_gap_linear = 0)
  )
  b <- whep:::backcast_admin_shares(
    shares,
    extent,
    settings = list(max_gap_linear = 3)
  )
  expect_equal(a$shares$share, b$shares$share)
  expect_equal(b$settings$max_gap_linear, 3)
})

test_that("a t0 beyond the extent uses the clamped slice", {
  shares <- seam_shares(
    c("P", "Q"),
    rep(1910L, 2),
    c(0.6, 0.4)
  )
  extent <- seam_extent(
    rep(c("P", "Q"), each = 3),
    rep(1900:1902, times = 2),
    c(100, 150, 200, 100, 100, 100)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  expect_equal(seam_count(out, "luh2_clamped_t0"), 2L)
  produced <- dplyr::filter(out$shares, year <= 1902L)
  expect_equal(unique(produced$treatment), "luh2_clamped")
  # E_u(t0) is E_u(1902); the clamped anchor year reproduces the t0 share.
  expect_equal(seam_share_at(out, "P", 1902L), 0.6, tolerance = 1e-12)
  expect_equal(
    seam_share_at(out, "P", 1900L),
    (0.6 * 100 / 200) / (0.6 * 100 / 200 + 0.4),
    tolerance = 1e-12
  )
})

# --- the extent guard ---------------------------------------------------------

test_that("a flagged extent refuses the series by default", {
  shares <- seam_shares(c("P", "Q"), rep(1903L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    rep(c("P", "Q"), each = 4),
    rep(1900:1903, times = 2),
    c(1000, 1010, 5, 1020, 100, 100, 100, 100)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  expect_gt(seam_count(out, "extent_jump_refused"), 0L)
  expect_equal(seam_count(out, "extent_jump_repaired"), 0L)
  # The refused series keeps its observed rows and gains no others.
  expect_equal(nrow(out$shares), 2L)
  expect_equal(unique(out$shares$treatment), "observed")
})

test_that("repair = TRUE mends an isolated collapse and back-casts on", {
  shares <- seam_shares(c("P", "Q"), rep(1903L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    rep(c("P", "Q"), each = 4),
    rep(1900:1903, times = 2),
    c(1000, 1010, 5, 1020, 100, 100, 100, 100)
  )
  out <- whep:::backcast_admin_shares(
    shares,
    extent,
    settings = list(repair = TRUE)
  )

  expect_equal(seam_count(out, "extent_jump_repaired"), 1L)
  expect_equal(seam_count(out, "extent_jump_refused"), 0L)
  # 1902 is repaired to the mean of 1010 and 1020, so P's 1902 share is
  # 0.6 * 1015/1020 against Q's 0.4.
  tilde <- c(0.6 * 1015 / 1020, 0.4)
  expect_equal(
    seam_share_at(out, "P", 1902L),
    tilde[1] / sum(tilde),
    tolerance = 1e-12
  )
  expect_equal(out$settings$repair, TRUE)
})

# --- refusals and guards ------------------------------------------------------

test_that("a series with no observed binding indicator is refused", {
  shares <- seam_shares(c("P", "Q"), rep(1902L, 2), c(0.6, 0.4)) |>
    dplyr::mutate(indicator_used = "production")
  extent <- seam_extent(
    rep(c("P", "Q"), each = 3),
    rep(1900:1902, times = 2),
    rep(100, 6)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  expect_equal(seam_count(out, "no_binding_anchor"), 1L)
  expect_equal(nrow(out$shares), 2L)
})

test_that("a seam t0 with no observation there is refused", {
  shares <- seam_shares(c("P", "Q"), rep(1902L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    rep(c("P", "Q"), each = 3),
    rep(1900:1902, times = 2),
    rep(100, 6)
  )
  out <- whep:::backcast_admin_shares(
    shares,
    extent,
    seam = tibble::tibble(area_code = 920L, item_prod_code = 15L, t0 = 1901L)
  )

  expect_equal(seam_count(out, "seam_t0_not_observed"), 1L)
  expect_equal(nrow(out$shares), 2L)
})

test_that("a unit unresolved at t0 refuses its whole series", {
  shares <- seam_shares(c(NA_character_, "Q"), rep(1902L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    rep(c("P", "Q"), each = 3),
    rep(1900:1902, times = 2),
    rep(100, 6)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  expect_equal(seam_count(out, "unresolved_unit_at_t0"), 1L)
  expect_equal(nrow(out$shares), 2L)
})

test_that("a unit-year without an extent row refuses that year", {
  shares <- seam_shares(c("P", "Q"), rep(1902L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    c(rep("P", 3), "Q", "Q"),
    c(1900:1902, 1901L, 1902L),
    c(100, 100, 100, 100, 100)
  )
  out <- whep:::backcast_admin_shares(shares, extent)

  expect_equal(seam_count(out, "missing_extent"), 1L)
  expect_equal(seam_count(out, "incomplete_unit_set"), 1L)
  expect_true(all(is.na(dplyr::filter(out$shares, year == 1900L)$share)))
})

test_that("a bad zero_policy, repair or duplicated key aborts", {
  shares <- seam_shares(c("P", "Q"), rep(1902L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    rep(c("P", "Q"), each = 3),
    rep(1900:1902, times = 2),
    rep(100, 6)
  )

  expect_error(
    whep:::backcast_admin_shares(
      shares,
      extent,
      settings = list(zero_policy = "hold_inside")
    )
  )
  expect_error(
    whep:::backcast_admin_shares(
      shares,
      extent,
      settings = list(repair = NA)
    ),
    "repair"
  )
  # A knob that does not exist is a typo, not a setting: it must abort
  # rather than be dropped, which is the one thing a bundle can lose that
  # eight formals could not.
  expect_error(
    whep:::backcast_admin_shares(
      shares,
      extent,
      settings = list(max_gaps = 2)
    ),
    "max_gaps"
  )
  expect_error(
    whep:::backcast_admin_shares(shares, extent, settings = list(2)),
    "named list"
  )
  expect_error(
    whep:::backcast_admin_shares(dplyr::bind_rows(shares, shares), extent),
    "row key"
  )
  expect_error(
    whep:::backcast_admin_shares(
      dplyr::mutate(shares, treatment = "observed"),
      extent
    ),
    "treatment"
  )
  expect_error(
    whep:::backcast_admin_shares(
      dplyr::mutate(shares, extent_ha = 1),
      extent
    ),
    "extent_ha"
  )
  expect_error(
    whep:::backcast_admin_shares(shares, extent[0, ]),
    "empty"
  )
})

# --- no hardcoded years, no LUH2 read -----------------------------------------

test_that("the seam core hardcodes no calendar year and reads no LUH2", {
  ns <- asNamespace("whep")
  nms <- unique(c(
    "aggregate_unit_extent",
    "check_extent_jumps",
    "backcast_admin_shares",
    ".repair_extent_collapse",
    grep("^\\.(aue|backcast)_", ls(ns, all.names = TRUE), value = TRUE)
  ))
  src <- unlist(lapply(nms, function(nm) deparse(get(nm, envir = ns))))

  expect_false(any(grepl("\\b(18|19|20)[0-9]{2}\\b", src)))
  expect_false(any(grepl("read_luh2_landuse", src)))

  path <- testthat::test_path("..", "..", "R", "admin_shares_seam.R")
  skip_if_not(file.exists(path), "package sources not in the check dir")
  text <- readLines(path, warn = FALSE)
  expect_equal(grep("\\b1961\\b|\\b1850\\b", text), integer(0))
  code <- text[!grepl("^[[:space:]]*#", text)]
  expect_equal(grep("read_luh2_landuse", code), integer(0))
})

test_that("a non-binding indicator at t0 is not an anchor", {
  # A production row sitting beside the binding area row at t0 must not
  # become a second anchor for the same unit: it would double the unit in
  # the renormalisation and halve every back-cast share.
  area <- seam_shares(c("P", "Q"), rep(1902L, 2), c(0.6, 0.4))
  production <- area |>
    dplyr::mutate(indicator_used = "production", share = c(0.7, 0.3))
  extent <- seam_extent(
    rep(c("P", "Q"), each = 3),
    rep(1900:1902, times = 2),
    c(100, 150, 200, 100, 100, 100)
  )
  out <- whep:::backcast_admin_shares(
    dplyr::bind_rows(area, production),
    extent
  )

  back <- dplyr::filter(out$shares, year == 1900L)
  expect_equal(nrow(back), 2L)
  expect_equal(unique(back$indicator_used), "area_harvested")
  expect_equal(
    seam_share_at(out, "P", 1900L),
    (0.6 * 100 / 200) / (0.6 * 100 / 200 + 0.4),
    tolerance = 1e-12
  )
  # The production rows pass through untouched.
  passed <- dplyr::filter(out$shares, indicator_used == "production")
  expect_equal(nrow(passed), 2L)
  expect_equal(passed$share, c(0.7, 0.3))
})

test_that("an extent keyed more finely than the shares aborts", {
  shares <- seam_shares(c("P", "Q"), rep(1902L, 2), c(0.6, 0.4))
  extent <- seam_extent(
    rep(c("P", "Q"), each = 3),
    rep(1900:1902, times = 2),
    rep(100, 6)
  ) |>
    tidyr::crossing(luh2_type = c("c3ann", "c4ann"))

  expect_error(
    whep:::backcast_admin_shares(shares, extent),
    "more than one"
  )
})

test_that("an empty shares table and a one-year extent are no-ops", {
  proto <- tibble::tibble(
    area_code = integer(),
    level = integer(),
    item_prod_code = integer(),
    indicator_used = character(),
    level_polity_code = character(),
    year = integer(),
    share = double(),
    treatment_year = character()
  )
  empty <- whep:::backcast_admin_shares(
    proto,
    seam_extent("U1", 1900L, 100)
  )
  expect_equal(nrow(empty$shares), 0L)
  expect_equal(nrow(empty$diagnostics), 0L)
  expect_equal(
    nrow(empty$counters),
    length(whep:::.backcast_diag_vocabulary())
  )

  # One year of extent, with t0 on it: nothing precedes the seam.
  out <- whep:::backcast_admin_shares(
    seam_shares(c("U1", "U2"), rep(1900L, 2), c(0.6, 0.4)),
    seam_extent(c("U1", "U2"), rep(1900L, 2), c(100, 200))
  )
  expect_equal(out$shares$share, c(0.6, 0.4))
  expect_equal(out$shares$treatment, rep("observed", 2))
  expect_equal(nrow(out$diagnostics), 0L)
})

test_that("a residual is refused where the units sit at two depths", {
  mixed_depth <- seam_grid_a() |>
    dplyr::mutate(
      level = dplyr::if_else(
        level_polity_code == "A-A2-1975-2100",
        2L,
        level
      )
    )
  expect_error(
    whep:::aggregate_unit_extent(
      .level1_gridded_cropland(),
      mixed_depth,
      "cropland_ha",
      container_frac = .level1_level0_shares(),
      residual_code = "A-RESIDUAL"
    ),
    "different"
  )
})
