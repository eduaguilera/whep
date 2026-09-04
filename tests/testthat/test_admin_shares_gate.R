# Tests for R/admin_shares_gate.R (plan T29, the seam gate).
#
# Reached through `whep:::`: roxygen has not run on this branch yet, so
# `seam_gate()` and `seam_gate_tolerances()` are not in NAMESPACE and the
# dispatcher's documentation pass exports them.
#
# Fixtures: `helper_seam_1890.R` (T28's three-unit reduction fixture,
# t0 = 1900) drives the identity, agnosticism and no-op legs. Tier B's
# container gate is a proportion test, and a three-unit container is
# below the count at which it can be pronounced on at all (see the
# `min_gated` test), so the pass/fail discrimination is shown on a wider
# reduction fixture built here to the same rule: shares EXACTLY
# proportional to the unit extents in every year.

# --- shared fixture builders --------------------------------------------------

gate_seams <- function(
  seam_year = 1900L,
  seam_kind = "start",
  area_code = 910L,
  item_prod_code = 15L
) {
  tibble::tibble(
    area_code = as.integer(area_code),
    level = 1L,
    item_prod_code = as.integer(item_prod_code),
    seam_year = as.integer(seam_year),
    seam_kind = seam_kind,
    previous_year = NA_integer_,
    value_from = NA_character_,
    value_to = "fixture"
  )
}

# The T28 back-cast of the 1890 helper: three units, observed from 1900,
# back-cast over 1890:1899.
gate_backcast_1890 <- function(seam = NULL) {
  whep:::backcast_admin_shares(
    .seam1890_shares(),
    .seam1890_extent(),
    seam = seam
  )$shares
}

# A wide reduction fixture: `units` units over `years`, extents on a
# five-year cycle with a per-unit phase, and shares that are exactly the
# extent shares in every year -- so the back-cast a real run would
# produce is the LUH2 share vector itself and the seam is invisible by
# construction.
gate_wide_shares <- function(
  units = 12L,
  years = 1890:1930,
  t0 = 1900L,
  amplitude = 0.08,
  area_code = 700L
) {
  grid <- tidyr::expand_grid(unit = seq_len(units), year = years) |>
    dplyr::mutate(
      extent_ha = (100 + 20 * unit) *
        (1 + amplitude * sin(2 * pi * year / 5 + unit))
    ) |>
    dplyr::mutate(share = extent_ha / sum(extent_ha), .by = year)
  tibble::tibble(
    area_code = as.integer(area_code),
    level = 1L,
    item_prod_code = 15L,
    level_polity_code = sprintf("U%02d", grid$unit),
    year = as.integer(grid$year),
    share = grid$share,
    treatment = dplyr::if_else(
      grid$year < t0,
      "backcast_t0_geometry",
      "observed"
    )
  )
}

# Push every odd unit's pre-seam share down by `factor` and renormalise,
# which is a step of that size at the seam and nowhere else.
gate_inject_step <- function(shares, t0 = 1900L, factor = 1.3) {
  shares |>
    dplyr::mutate(
      share = dplyr::if_else(
        year < t0 & (as.integer(sub("U", "", level_polity_code)) %% 2L) == 1L,
        share / factor,
        share
      )
    ) |>
    dplyr::mutate(
      share = share / sum(share),
      .by = c("area_code", "level", "item_prod_code", "year")
    )
}

# A tiny crop-level cell table in the engine's output shape: one
# container, one crop, `n_cells` cells whose areas grow smoothly, so
# every cell's share of the national total is near-constant.
gate_cells <- function(
  years = 1898:1901,
  n_cells = 6L,
  area_code = 910L,
  regime = NULL
) {
  grid <- tidyr::expand_grid(cell = seq_len(n_cells), year = years) |>
    dplyr::mutate(
      lon = 10.25 + 0.5 * (cell - 1L),
      lat = 40.25,
      rainfed_ha = 1000 * cell * (1 + 0.01 * (year - min(years))),
      irrigated_ha = 0
    )
  out <- tibble::tibble(
    lon = grid$lon,
    lat = grid$lat,
    year = as.integer(grid$year),
    area_code = as.integer(area_code),
    crop_name = "wheat",
    rainfed_ha = grid$rainfed_ha,
    irrigated_ha = grid$irrigated_ha
  )
  if (is.null(regime)) {
    return(out)
  }
  dplyr::mutate(out, regime = regime(grid$cell, grid$year))
}

# --- tolerances ---------------------------------------------------------------

test_that("tolerances carry the plan's numbers and derive the rest", {
  tol <- whep:::seam_gate_tolerances()

  expect_equal(tol$identity_rel, 1e-8)
  expect_equal(tol$reference_quantile, 0.95)
  expect_equal(tol$null_rate, 0.05)
  expect_equal(tol$binomial_sigma, 2)
  expect_equal(tol$cell_min_ha, 100)
  expect_equal(tol$cell_excess, 0.01)
  expect_equal(tol$cell_ratio_bounds, c(0.55, 1.6))
})

test_that("min_gated is the count below which one pair cannot pass", {
  tol <- whep:::seam_gate_tolerances()
  n <- seq_len(10L)
  band <- tol$null_rate +
    tol$binomial_sigma * sqrt(tol$null_rate * (1 - tol$null_rate) / n)

  # Below min_gated a single exceeding pair is over the band by
  # arithmetic; from min_gated on it is not.
  expect_true(all((1 / n > band)[n < tol$min_gated]))
  expect_true(all((1 / n <= band)[n >= tol$min_gated]))
  expect_equal(tol$min_gated, 4L)

  # It follows the band rather than being pinned to it.
  wider <- whep:::seam_gate_tolerances(binomial_sigma = 4)
  expect_lt(wider$min_gated, tol$min_gated)
})

test_that("tolerances round-trip through seam_gate", {
  tol <- whep:::seam_gate_tolerances(identity_rel = 1e-3, cell_excess = 0.2)
  shares <- gate_backcast_1890()

  # The list a caller gets back is accepted unchanged, derived members
  # included, and is not silently replaced by the defaults.
  out <- whep:::seam_gate(shares, gate_seams(), tolerances = tol)
  expect_type(out$verdict, "logical")
  expect_setequal(
    names(out$verdict),
    c("tier_a", "tier_b", "tier_c", "overall")
  )

  expect_error(
    whep:::seam_gate(shares, gate_seams(), tolerances = list(nonsense = 1)),
    "unknown element"
  )
  expect_error(
    whep:::seam_gate_tolerances(reference_quantile = 1),
    "strictly in"
  )
  expect_error(
    whep:::seam_gate_tolerances(cell_ratio_bounds = c(2, 1)),
    "low then high"
  )
})

test_that("a tightened identity tolerance is the one that decides", {
  shares <- gate_backcast_1890() |>
    dplyr::mutate(
      share = dplyr::if_else(
        year == 1900L & level_polity_code == "S1",
        share * (1 + 1e-6),
        share
      )
    )

  loose <- whep:::seam_gate(
    shares,
    gate_seams(),
    tolerances = whep:::seam_gate_tolerances(identity_rel = 1e-3)
  )
  tight <- whep:::seam_gate(
    shares,
    gate_seams(),
    tolerances = whep:::seam_gate_tolerances(identity_rel = 1e-12)
  )

  expect_true(loose$tier_a$pass)
  expect_false(tight$tier_a$pass)
  expect_equal(tight$tier_a$reason, "anchor_shares_not_unit_sum")
})

# --- tier A -------------------------------------------------------------------

test_that("tier A passes the 1890 back-cast at its anchor", {
  out <- whep:::seam_gate(gate_backcast_1890(), gate_seams())

  expect_equal(nrow(out$tier_a), 1L)
  expect_true(out$tier_a$pass)
  expect_equal(out$tier_a$t0, .seam1890_first_observed())
  expect_equal(out$tier_a$n_units, 3L)
  expect_equal(out$tier_a$n_observed, 3L)
  expect_equal(out$tier_a$share_sum, 1, tolerance = 1e-12)
  expect_equal(out$tier_a$reason, "")
  # The helper's rows carry `value`, so the share column is checked
  # against the reported value and not only against itself.
  expect_equal(out$tier_a$basis, "value")
  expect_equal(out$tier_a$max_rel_diff, 0, tolerance = 1e-8)
  expect_true(out$tier_a$matches_seam_start)
})

test_that("tier A falls back to the share column with no value", {
  shares <- dplyr::select(gate_backcast_1890(), -"value")
  out <- whep:::seam_gate(shares, gate_seams())

  expect_equal(out$tier_a$basis, "share_only")
  expect_true(is.na(out$tier_a$max_rel_diff))
  expect_true(out$tier_a$pass)
})

test_that("tier A fails a share that disagrees with its own value", {
  shares <- gate_backcast_1890() |>
    dplyr::mutate(
      share = dplyr::if_else(
        year == 1900L & level_polity_code == "S1",
        share + 0.05,
        share
      ),
      share = dplyr::if_else(
        year == 1900L & level_polity_code == "S2",
        share - 0.05,
        share
      )
    )
  out <- whep:::seam_gate(shares, gate_seams())

  # The unit total is untouched, so only the value comparison can catch
  # this one.
  expect_equal(out$tier_a$share_sum, 1, tolerance = 1e-12)
  expect_false(out$tier_a$pass)
  expect_equal(out$tier_a$reason, "anchor_share_disagrees_with_value")
})

test_that("tier A reports an anchor that is not the seam list's start", {
  out <- whep:::seam_gate(gate_backcast_1890(), gate_seams(seam_year = 1899L))

  expect_false(out$tier_a$matches_seam_start)
  expect_false(out$tier_a$pass)
  expect_equal(out$tier_a$reason, "anchor_not_seam_start")
})

test_that("tier A is unevaluated when nothing is observed", {
  shares <- dplyr::mutate(gate_backcast_1890(), treatment = "luh2_clamped")
  out <- whep:::seam_gate(shares, gate_seams())

  expect_equal(nrow(out$tier_a), 0L)
  expect_true(is.na(out$verdict[["tier_a"]]))
})

# --- tier B -------------------------------------------------------------------

test_that("the pure-LUH2 reduction passes tier B", {
  shares <- gate_wide_shares()
  out <- whep:::seam_gate(shares, gate_seams(area_code = 700L))

  gate <- dplyr::distinct(
    out$tier_b,
    n_gated,
    n_beyond,
    frac_beyond,
    threshold,
    gate_status,
    pass
  )
  expect_equal(nrow(gate), 1L)
  expect_equal(gate$n_gated, 12L)
  expect_equal(gate$gate_status, "gated")
  expect_lt(gate$frac_beyond, gate$threshold)
  expect_true(gate$pass)
  expect_true(out$verdict[["tier_b"]])
  expect_true(all(out$tier_b$status == "gated"))
})

test_that("an injected 30% step at t0 fails tier B", {
  shares <- gate_inject_step(gate_wide_shares())
  out <- whep:::seam_gate(shares, gate_seams(area_code = 700L))

  gate <- dplyr::distinct(
    out$tier_b,
    n_gated,
    n_beyond,
    frac_beyond,
    threshold,
    pass
  )
  expect_equal(gate$n_gated, 12L)
  expect_false(gate$pass)
  expect_gt(gate$frac_beyond, gate$threshold)
  expect_false(out$verdict[["tier_b"]])

  # Half the container's mass moves at the seam, so the renormalisation
  # puts an opposite step of about log(1.3) - log(S) on the units that
  # were not pushed: the injection is a change of SHAPE and every unit
  # feels it. Which units end up beyond the quantile therefore depends
  # on each unit's own phase, and the evidence is the container rate,
  # not the identity of the movers.
  expect_equal(gate$n_beyond, 6L)
  expect_equal(gate$frac_beyond, 0.5)
  expect_gt(max(out$tier_b$log_ratio), 0.2)
  expect_lt(unique(out$tier_b$q_reference), 0.15)

  # Nothing else moved: the same fixture without the injection passes.
  clean <- whep:::seam_gate(gate_wide_shares(), gate_seams(area_code = 700L))
  expect_true(unique(clean$tier_b$pass))
})

test_that("a three-unit container is reported, not pronounced on", {
  out <- whep:::seam_gate(gate_backcast_1890(), gate_seams())

  expect_equal(nrow(out$tier_b), 3L)
  expect_true(all(out$tier_b$status == "gated"))
  expect_equal(unique(out$tier_b$n_gated), 3L)
  expect_equal(unique(out$tier_b$gate_status), "too_few_pairs")
  expect_true(all(is.na(out$tier_b$pass)))
  expect_true(is.na(out$verdict[["tier_b"]]))
})

test_that("every seam kind is gated, not just the start", {
  kinds <- c(
    "start",
    "source_switch",
    "grain_switch",
    "nuts_version_switch",
    "coverage_change",
    "indicator_switch"
  )
  years <- c(1900L, 1902L, 1904L, 1906L, 1908L, 1910L)
  seams <- purrr::map2(years, kinds, gate_seams, area_code = 700L) |>
    purrr::list_rbind()
  out <- whep:::seam_gate(gate_wide_shares(), seams)

  expect_setequal(out$tier_b$seam_year, years)
  expect_setequal(unique(out$tier_b$seam_kinds), kinds)
  expect_equal(nrow(out$tier_b), 12L * length(years))
  expect_equal(unique(out$tier_b$n_gated), 12L * length(years))
})

test_that("a year that is two seams at once is gated once", {
  seams <- dplyr::bind_rows(
    gate_seams(1902L, "source_switch", area_code = 700L),
    gate_seams(1902L, "coverage_change", area_code = 700L)
  )
  out <- whep:::seam_gate(gate_wide_shares(), seams)

  expect_equal(nrow(out$tier_b), 12L)
  expect_equal(
    unique(out$tier_b$seam_kinds),
    "coverage_change|source_switch"
  )
})

test_that("unjudgeable pairs are named, never counted as passes", {
  shares <- gate_wide_shares() |>
    dplyr::mutate(
      share = dplyr::if_else(
        year == 1899L & level_polity_code == "U01",
        NA_real_,
        share
      ),
      share = dplyr::if_else(
        year == 1899L & level_polity_code == "U02",
        0,
        share
      )
    ) |>
    dplyr::filter(!(year == 1899L & level_polity_code == "U03"))
  out <- whep:::seam_gate(shares, gate_seams(area_code = 700L))

  status <- dplyr::select(out$tier_b, level_polity_code, status)
  expect_equal(status$status[status$level_polity_code == "U01"], "na_share")
  expect_equal(status$status[status$level_polity_code == "U02"], "zero_share")
  expect_equal(
    status$status[status$level_polity_code == "U03"],
    "no_previous_row"
  )
  expect_equal(unique(out$tier_b$n_gated), 9L)
})

test_that("a reference too small to have a Q95 is refused, not judged", {
  shares <- dplyr::filter(gate_wide_shares(), year <= 1901L)
  out <- whep:::seam_gate(shares, gate_seams(area_code = 700L))

  # 12 units x 1 observed pair = 12 reference pairs, under the default
  # minimum of 20.
  expect_equal(unique(out$tier_b$n_reference), 12L)
  expect_true(all(out$tier_b$status == "no_reference"))
  expect_equal(unique(out$tier_b$gate_status), "no_pairs")
  expect_true(is.na(out$verdict[["tier_b"]]))
})

test_that("gated pairs are held out of their own reference", {
  shares <- gate_wide_shares()
  one <- whep:::seam_gate(shares, gate_seams(area_code = 700L))
  two <- whep:::seam_gate(
    shares,
    dplyr::bind_rows(
      gate_seams(area_code = 700L),
      gate_seams(1905L, "source_switch", area_code = 700L)
    )
  )

  # Gating a second year removes its 12 observed pairs from the pool.
  expect_equal(
    unique(two$tier_b$n_reference),
    unique(one$tier_b$n_reference) - 12L
  )
})

# --- seam agnosticism and the empty back-cast ---------------------------------

test_that("moving t0 moves treatment, and the gate says which", {
  early <- gate_backcast_1890()
  late <- gate_backcast_1890(
    seam = tibble::tibble(area_code = 910L, item_prod_code = 15L, t0 = 1905L)
  )

  key <- c("level_polity_code", "year")
  joined <- dplyr::inner_join(
    dplyr::select(early, dplyr::all_of(key), share, treatment),
    dplyr::select(late, dplyr::all_of(key), share, treatment),
    by = key,
    suffix = c("_early", "_late")
  )
  # Only `treatment` moves: the shares are seam-agnostic because the
  # helper's observed vector is the extent vector at every year.
  expect_equal(joined$share_early, joined$share_late, tolerance = 1e-8)
  moved <- dplyr::filter(joined, treatment_early != treatment_late)
  expect_equal(sort(unique(moved$year)), 1900:1904)

  # The gate reads t0 off the table, so it sees the move against a seam
  # list that still says 1900.
  gated <- whep:::seam_gate(late, gate_seams())
  expect_equal(gated$tier_a$t0, 1905L)
  expect_equal(gated$tier_a$seam_start_year, 1900L)
  expect_equal(gated$tier_a$reason, "anchor_not_seam_start")
})

test_that("a t0 at the first fixture year is a no-op the gate reads", {
  # The helper's shares start at 1900; observed rows for every extent
  # year instead put t0 at the first year there is, so the back-cast
  # window is empty.
  first <- min(.seam1890_years())
  every_year <- .seam1890_extent() |>
    dplyr::mutate(share = extent_ha / sum(extent_ha), .by = year) |>
    dplyr::mutate(
      item_prod_code = 15L,
      indicator_used = "area_harvested",
      treatment_year = "observed"
    ) |>
    dplyr::select(
      "area_code",
      "level",
      "item_prod_code",
      "indicator_used",
      "level_polity_code",
      "year",
      "share",
      "treatment_year"
    )
  full <- whep:::backcast_admin_shares(every_year, .seam1890_extent())$shares

  expect_equal(nrow(full), nrow(every_year))
  expect_true(all(full$treatment == "observed"))

  out <- whep:::seam_gate(full, gate_seams(seam_year = first))
  expect_equal(out$tier_a$t0, first)
  expect_true(out$tier_a$pass)
  # The seam is the first year there is, so it has no pair to score.
  expect_equal(nrow(out$tier_b), 3L)
  expect_true(all(out$tier_b$status == "no_previous_row"))
  expect_equal(unique(out$tier_b$gate_status), "no_pairs")
  expect_true(is.na(out$verdict[["tier_b"]]))
})

# --- tier C -------------------------------------------------------------------

test_that("tier C is unevaluated without cells", {
  out <- whep:::seam_gate(gate_backcast_1890(), gate_seams())

  expect_equal(nrow(out$tier_c), 0L)
  expect_true(is.na(out$verdict[["tier_c"]]))
})

test_that("tier C passes a smooth cell table", {
  out <- whep:::seam_gate(
    gate_backcast_1890(),
    gate_seams(),
    cells = gate_cells()
  )

  expect_equal(nrow(out$tier_c), 1L)
  expect_equal(out$tier_c$seam_year, 1900L)
  expect_equal(out$tier_c$n_series_seam, 6L)
  expect_equal(out$tier_c$n_flag_seam, 0L)
  expect_equal(out$tier_c$flag_rate_seam, 0)
  expect_equal(out$tier_c$excess, 0)
  expect_equal(out$tier_c$reason, "")
  expect_true(out$tier_c$pass)
  expect_false(out$tier_c$regime_checked)
})

test_that("tier C fails a seam pair that flags where its neighbours do not", {
  cells <- gate_cells() |>
    dplyr::mutate(
      rainfed_ha = dplyr::if_else(
        year >= 1900L & lon %in% c(10.25, 10.75, 11.25),
        rainfed_ha * 4,
        rainfed_ha
      )
    )
  out <- whep:::seam_gate(gate_backcast_1890(), gate_seams(), cells = cells)

  expect_equal(out$tier_c$n_flag_prev, 0L)
  expect_equal(out$tier_c$n_flag_next, 0L)
  expect_gt(out$tier_c$n_flag_seam, 0L)
  expect_gt(out$tier_c$excess, 0.01)
  expect_equal(out$tier_c$reason, "seam_flag_rate_excess")
  expect_false(out$tier_c$pass)
  expect_false(out$verdict[["overall"]])
})

test_that("tier C fails a regime flip on its own", {
  steady <- gate_cells(regime = function(cell, year) "type_aware")
  flipped <- gate_cells(
    regime = function(cell, year) {
      dplyr::if_else(cell == 1L & year >= 1900L, "uniform", "type_aware")
    }
  )

  ok <- whep:::seam_gate(gate_backcast_1890(), gate_seams(), cells = steady)
  bad <- whep:::seam_gate(gate_backcast_1890(), gate_seams(), cells = flipped)

  expect_true(ok$tier_c$regime_checked)
  expect_equal(ok$tier_c$n_regime_mismatch, 0L)
  expect_true(ok$tier_c$pass)

  expect_equal(bad$tier_c$n_regime_mismatch, 1L)
  expect_equal(bad$tier_c$reason, "regime_flip")
  expect_false(bad$tier_c$pass)
  # The flip alone decides: the flag rates are the steady table's.
  expect_equal(bad$tier_c$flag_rate_seam, ok$tier_c$flag_rate_seam)
})

test_that("tier C drops cells under the hectare floor before scanning", {
  # A cell of 10 ha that triples at the seam: a wild ratio on a
  # quantity too small to mean anything.
  tiny <- tibble::tibble(
    lon = 20.25,
    lat = 40.25,
    year = 1898:1901,
    area_code = 910L,
    crop_name = "wheat",
    rainfed_ha = c(10, 10, 30, 30),
    irrigated_ha = 0
  )
  cells <- dplyr::bind_rows(gate_cells(), tiny)

  floored <- whep:::seam_gate(
    gate_backcast_1890(),
    gate_seams(),
    cells = cells
  )
  raised <- whep:::seam_gate(
    gate_backcast_1890(),
    gate_seams(),
    cells = cells,
    tolerances = whep:::seam_gate_tolerances(cell_min_ha = 1)
  )

  expect_equal(floored$tier_c$n_series_seam, 6L)
  expect_equal(floored$tier_c$n_flag_seam, 0L)
  expect_equal(raised$tier_c$n_series_seam, 7L)
  expect_equal(raised$tier_c$n_flag_seam, 1L)
})

test_that("tier C scans exactly the three pairs of each seam", {
  # Cells reach 1901, so (t0, t0+1) is scannable; drop 1897 and the
  # (t0-2, t0-1) pair still is, because it needs 1898 and 1899 only.
  out <- whep:::seam_gate(
    gate_backcast_1890(),
    gate_seams(),
    cells = gate_cells(years = 1898:1901)
  )
  expect_equal(
    c(out$tier_c$n_series_prev, out$tier_c$n_series_seam),
    c(6L, 6L)
  )
  expect_equal(out$tier_c$n_series_next, 6L)

  # With no year after t0 the next pair cannot be scanned and the
  # neighbour mean falls back to the one pair that could be.
  short <- whep:::seam_gate(
    gate_backcast_1890(),
    gate_seams(),
    cells = gate_cells(years = 1898:1900)
  )
  expect_equal(short$tier_c$n_series_next, 0L)
  expect_true(is.na(short$tier_c$flag_rate_next))
  expect_equal(short$tier_c$neighbour_rate, short$tier_c$flag_rate_prev)

  # With no neighbour at all the gate reports itself unevaluable.
  none <- whep:::seam_gate(
    gate_backcast_1890(),
    gate_seams(),
    cells = gate_cells(years = 1899:1900)
  )
  expect_true(is.na(none$tier_c$neighbour_rate))
  expect_equal(none$tier_c$reason, "no_neighbour_pair")
  expect_true(is.na(none$tier_c$pass))
})

test_that("tier C says so when it drops a cell row it cannot use", {
  holed <- gate_cells() |>
    dplyr::mutate(
      rainfed_ha = dplyr::if_else(
        year == 1900L & lon == 10.25,
        NA_real_,
        rainfed_ha
      )
    )
  expect_warning(
    out <- whep:::seam_gate(gate_backcast_1890(), gate_seams(), cells = holed),
    "harvested area is missing"
  )
  # The dropped row leaves its series with only one of the seam pair's
  # two years, so the series is not scanned rather than scanned wrong.
  expect_equal(out$tier_c$n_series_seam, 5L)
  expect_equal(out$tier_c$n_series_prev, 6L)
})

test_that("tier C keys on the compartment, so one cell's units differ", {
  base <- gate_cells(n_cells = 2L)
  split <- dplyr::bind_rows(
    dplyr::mutate(base, level_polity_code = "A1"),
    dplyr::mutate(base, level_polity_code = "A2")
  )
  out <- whep:::seam_gate(gate_backcast_1890(), gate_seams(), cells = split)

  # Four compartment series, not two: the same (lon, lat) held by two
  # units is two series.
  expect_equal(out$tier_c$n_series_seam, 4L)
})

# --- contracts and invariants -------------------------------------------------

test_that("seam_gate refuses malformed input but never a failed gate", {
  shares <- gate_backcast_1890()

  expect_error(
    whep:::seam_gate(dplyr::select(shares, -"share"), gate_seams()),
    "missing column"
  )
  expect_error(
    whep:::seam_gate(shares, dplyr::select(gate_seams(), -"seam_kind")),
    "missing column"
  )
  expect_error(
    whep:::seam_gate(dplyr::bind_rows(shares, shares), gate_seams()),
    "repeats"
  )
  expect_error(
    whep:::seam_gate(
      shares,
      gate_seams(),
      cells = dplyr::select(gate_cells(), -"rainfed_ha")
    ),
    "no harvested area"
  )

  # A failing gate is a return value, not a condition.
  failing <- expect_no_error(
    whep:::seam_gate(
      gate_inject_step(gate_wide_shares()),
      gate_seams(area_code = 700L)
    )
  )
  expect_false(failing$verdict[["overall"]])
})

test_that("the gate reports a summary and returns four elements", {
  expect_message(
    out <- whep:::seam_gate(gate_wide_shares(), gate_seams(area_code = 700L)),
    "seam_gate"
  )
  expect_setequal(
    names(out),
    c("tier_a", "tier_b", "tier_c", "verdict")
  )
  expect_s3_class(out$tier_a, "tbl_df")
  expect_s3_class(out$tier_b, "tbl_df")
  expect_s3_class(out$tier_c, "tbl_df")
  expect_type(out$verdict, "logical")
})

test_that("an empty seam list leaves every tier's shape intact", {
  empty <- gate_seams()[0, ]
  out <- whep:::seam_gate(gate_backcast_1890(), empty)

  expect_equal(nrow(out$tier_b), 0L)
  expect_equal(nrow(out$tier_c), 0L)
  expect_true(is.na(out$verdict[["tier_b"]]))
  expect_setequal(names(out$tier_b), whep:::.sg_tier_b_cols())
})

test_that("no seam year is hardcoded anywhere in the gate", {
  # Deparsing the function bodies is the check that survives the built
  # tarball, where `R/` is absent -- and it drops comments for free, so
  # a year named in prose is not a false positive.
  objects <- c(
    "seam_gate",
    "seam_gate_tolerances",
    grep("^\\.sg_", ls(asNamespace("whep"), all.names = TRUE), value = TRUE)
  )
  code <- unlist(lapply(objects, function(nm) {
    deparse(get(nm, envir = asNamespace("whep")))
  }))

  # `L?` is load-bearing: an R year literal is written `1961L`, and
  # `\\b` finds no boundary between the digit and the suffix, so a
  # pattern without it is blind to exactly the form being hunted.
  any_year <- "\\b(1[6-9][0-9]{2}|20[0-9]{2})L?\\b"
  named_year <- "\\b(1961|1850|1962)L?\\b"

  # Positive control first: both patterns fire on lines that do hold a
  # year, so the assertions below are evidence rather than a regex that
  # silently matches nothing.
  planted <- c("  t0 <- 1961L", "  years <- 1850:1962", "  x <- seam_year")
  expect_equal(grepl(named_year, planted), c(TRUE, TRUE, FALSE))
  expect_equal(
    unlist(regmatches(planted, gregexpr(any_year, planted))),
    c("1961L", "1850", "1962")
  )

  expect_gt(length(objects), 20L)
  expect_false(any(grepl(named_year, code)))
  # Nothing that could be a year at all: the gate's only literals are
  # tolerances, quantiles and small counts.
  expect_equal(unlist(regmatches(code, gregexpr(any_year, code))), character(0))
})
