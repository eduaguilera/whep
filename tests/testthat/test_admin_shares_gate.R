# Tests for R/admin_shares_gate.R (plan T29, the seam gate).
#
# Reached through `whep:::`: roxygen has not run on this branch yet, so
# `seam_gate()` and `seam_gate_tolerances()` are not in NAMESPACE and the
# dispatcher's documentation pass exports them.
#
# T29b adds the tier-B basis, and the hold-out leg that stands in where
# tier B is vacuous. Its evidence is the two-history fixture below: one
# panel, two extent proxies with opposite per-unit trends, which tier B
# scores identically and the hold-out separates.
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

# The two-history fixture (T29b). One panel, two extent proxies with
# EQUAL AND OPPOSITE per-unit trends, so back-casting the same
# observations under each gives two materially different histories. It is
# the reduction fixture above with two things added that a real series
# has and it did not: noise, and a proxy that can be wrong.
#
# `slope` runs from -trend to +trend across the units, so unit 1's extent
# falls as fast under the first table as it rises under the second.
gate_trend_extent <- function(
  sign = 1,
  units = 12L,
  years = 1890:1930,
  trend = 0.035,
  area_code = 700L
) {
  grid <- tidyr::expand_grid(unit = seq_len(units), year = years)
  slope <- seq(-trend, trend, length.out = units)
  tibble::tibble(
    area_code = as.integer(area_code),
    level_polity_code = sprintf("U%02d", grid$unit),
    level = 1L,
    year = as.integer(grid$year),
    extent_ha = (100 + 20 * grid$unit) *
      exp(sign * slope[grid$unit] * (grid$year - min(years))),
    extent_basis = "cropland_ha"
  )
}

# Observations from `t0` on: the extent shares of whichever table is
# passed, times iid lognormal noise. The noise is drawn at
# `log_sd / sqrt(2)` so that the CONSECUTIVE log-ratios -- the quantity
# both tier B and the hold-out are scored against -- have a log-sd of
# about `log_sd`, which is what the fixture is specified by.
gate_noisy_observed <- function(extent, t0 = 1900L, log_sd = 0.15, seed = 1L) {
  set.seed(seed)
  extent |>
    dplyr::filter(year >= t0) |>
    dplyr::mutate(
      weight = extent_ha *
        exp(stats::rnorm(dplyr::n(), sd = log_sd / sqrt(2)))
    ) |>
    dplyr::mutate(share = weight / sum(weight), .by = year) |>
    dplyr::mutate(
      item_prod_code = 15L,
      indicator_used = "area_harvested",
      treatment_year = "observed",
      value = weight
    ) |>
    dplyr::select(
      "area_code",
      "level",
      "item_prod_code",
      "indicator_used",
      "level_polity_code",
      "year",
      "share",
      "value",
      "treatment_year"
    )
}

# The back-cast of that panel under one of the two proxies.
gate_trend_backcast <- function(extent, observed) {
  whep:::backcast_admin_shares(observed, extent)$shares
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
  expect_equal(tol$holdout_k, 10L)

  expect_error(whep:::seam_gate_tolerances(holdout_k = 0), "whole number")
  expect_error(whep:::seam_gate_tolerances(holdout_k = 2.5), "whole number")
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
    c("tier_a", "tier_b", "tier_b_holdout", "tier_c", "overall")
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

test_that("tier A fails a back-cast row standing at the anchor", {
  # The identity tier exists to refuse exactly this: the anchor is the
  # first year carrying an observation, so a unit whose statistics begin
  # later leaves a reconstructed row beside the observed ones at `t0`.
  # `.backcast_produced_rows()` labels such a row
  # "backcast_t0_geometry", which is what is injected here.
  shares <- gate_backcast_1890() |>
    dplyr::mutate(
      treatment = dplyr::if_else(
        year == .seam1890_first_observed() & level_polity_code == "S1",
        "backcast_t0_geometry",
        treatment
      )
    )
  out <- whep:::seam_gate(shares, gate_seams())

  # Only `treatment` moved, so every other tier-A check still passes and
  # the observed-row check is the only thing that can fail this.
  expect_equal(out$tier_a$t0, .seam1890_first_observed())
  expect_equal(out$tier_a$share_sum, 1, tolerance = 1e-12)
  expect_equal(out$tier_a$max_rel_diff, 0, tolerance = 1e-8)
  expect_true(out$tier_a$matches_seam_start)

  expect_equal(out$tier_a$n_units, 3L)
  expect_equal(out$tier_a$n_observed, 2L)
  expect_false(out$tier_a$pass)
  expect_equal(out$tier_a$reason, "anchor_row_not_observed")
  expect_false(out$verdict[["tier_a"]])
  expect_false(out$verdict[["overall"]])
})

test_that("a series with no observed row is a row, not a silence", {
  # There is no anchor, so every anchor measurement is `NA` -- but the
  # series still gets a row. A series that never reached the tier at all
  # is the one state a table of tier-A rows cannot otherwise show, and
  # it is what a mislabelled `treatment` column produces.
  shares <- dplyr::mutate(gate_backcast_1890(), treatment = "luh2_clamped")
  out <- whep:::seam_gate(shares, gate_seams())

  expect_equal(nrow(out$tier_a), 1L)
  expect_equal(out$tier_a$reason, "no_observed_anchor")
  expect_true(is.na(out$tier_a$pass))
  expect_true(is.na(out$tier_a$t0))
  expect_true(is.na(out$tier_a$share_sum))
  expect_true(is.na(out$tier_a$basis))
  expect_true(is.na(out$verdict[["tier_a"]]))
})

test_that("a passing tier C does not certify an unanchored table", {
  # Tier C is keyed on (container, seam year) and carries no item, so it
  # cannot stand in for a series. With nothing observed, tier A, tier B
  # and the hold-out all judge nothing, and a run no tier judged is
  # uncertified rather than passed.
  shares <- dplyr::mutate(gate_backcast_1890(), treatment = "luh2_clamped")
  expect_message(
    out <- whep:::seam_gate(shares, gate_seams(), cells = gate_cells()),
    "Coverage: 0 of 1"
  )

  expect_true(out$verdict[["tier_c"]])
  expect_true(is.na(out$verdict[["overall"]]))
})

test_that("a series no seam names is reported ungated, never passed", {
  # An empty seam list gates nothing at all, and a gate that was handed
  # nothing to check must not read as a pass.
  out <- whep:::seam_gate(gate_backcast_1890(), gate_seams()[0, ])

  expect_equal(nrow(out$tier_a), 1L)
  expect_equal(out$tier_a$reason, "no_start_seam")
  expect_true(is.na(out$tier_a$pass))
  expect_true(is.na(out$verdict[["tier_a"]]))
  expect_true(is.na(out$verdict[["overall"]]))
  # The identity numbers are still measured and still reported: what is
  # withheld is the verdict, not the evidence.
  expect_equal(out$tier_a$share_sum, 1, tolerance = 1e-12)
  expect_equal(out$tier_a$n_observed, 3L)
})

test_that("tier A withholds a pass where only a non-start seam names it", {
  # Tier A's one seam-derived check is that `t0` is the year the
  # `"start"` seam names, so a series with no `"start"` seam has that
  # check withheld, not satisfied. Deciding "gated" on "some seam names
  # the series" while checking "a start seam names the series" is what
  # let a source switch certify an anchor nothing had looked at.
  out <- whep:::seam_gate(
    gate_backcast_1890(),
    gate_seams(seam_kind = "source_switch")
  )

  expect_true(is.na(out$tier_a$seam_start_year))
  expect_true(is.na(out$tier_a$matches_seam_start))
  expect_equal(out$tier_a$reason, "no_start_seam")
  expect_true(is.na(out$tier_a$pass))
  # The identity numbers are still measured and still reported.
  expect_equal(out$tier_a$share_sum, 1, tolerance = 1e-12)
  expect_equal(out$tier_a$n_observed, 3L)

  # The seam it does carry is tier B's, and the year before `t0` is a
  # back-cast row, so tier B is vacuous here too: nothing anywhere
  # judged this series, and the gate must not read as a pass.
  expect_true(all(out$tier_b$basis == "vacuous_by_construction"))
  expect_true(is.na(out$verdict[["tier_a"]]))
  expect_true(is.na(out$verdict[["overall"]]))
})

test_that("a non-start seam tier B can judge does gate its series", {
  # The mirror of the test above, and the reason tier A's withheld pass
  # is not a failure: where the seam lands between two observations,
  # tier B judges the series and the run is certifiable on that.
  out <- whep:::seam_gate(
    gate_wide_shares(),
    gate_seams(seam_year = 1910L, seam_kind = "source_switch", area_code = 700L)
  )

  expect_equal(out$tier_a$reason, "no_start_seam")
  expect_true(is.na(out$tier_a$pass))
  expect_true(all(out$tier_b$basis == "observed_both_sides"))
  expect_true(out$verdict[["tier_b"]])
  expect_true(out$verdict[["overall"]])
})

test_that("an ungated series never hides an identity failure", {
  broken <- gate_backcast_1890() |>
    dplyr::mutate(
      share = dplyr::if_else(
        year == .seam1890_first_observed() & level_polity_code == "S1",
        share + 0.05,
        share
      )
    )
  out <- whep:::seam_gate(broken, gate_seams()[0, ])

  expect_equal(out$tier_a$reason, "anchor_shares_not_unit_sum")
  expect_false(out$tier_a$pass)
  expect_false(out$verdict[["overall"]])
})

test_that("one gated series does not certify an ungated sibling", {
  two <- dplyr::bind_rows(
    gate_backcast_1890(),
    dplyr::mutate(gate_backcast_1890(), item_prod_code = 44L)
  )
  expect_message(out <- whep:::seam_gate(two, gate_seams()), "Coverage: 1 of 2")

  expect_equal(nrow(out$tier_a), 2L)
  gated <- dplyr::filter(out$tier_a, item_prod_code == 15L)
  ungated <- dplyr::filter(out$tier_a, item_prod_code == 44L)
  expect_true(gated$pass)
  expect_equal(gated$reason, "")
  expect_true(is.na(ungated$pass))
  expect_equal(ungated$reason, "no_start_seam")

  # Tier A passed everything it judged, so its own verdict is TRUE --
  # and `overall` is still withheld, because one of the two series was
  # judged by nothing.
  expect_true(out$verdict[["tier_a"]])
  expect_true(is.na(out$verdict[["overall"]]))
})

test_that("the series key is compared by value, not by how it prints", {
  # `as.character(1e5)` is "1e+05" and `as.character(100000L)` is
  # "100000", so a key compared as formatted text disagrees with the
  # same key compared by a join. Whichever side carries the double, it
  # is one series and the gate must judge it.
  shares <- dplyr::mutate(gate_backcast_1890(), area_code = 1e5)
  seams <- gate_seams(area_code = 100000L)
  expect_false(identical(
    as.character(unique(shares$area_code)),
    as.character(unique(seams$area_code))
  ))

  out <- whep:::seam_gate(shares, seams)
  expect_equal(out$tier_a$seam_start_year, .seam1890_first_observed())
  expect_true(out$tier_a$matches_seam_start)
  expect_equal(out$tier_a$reason, "")
  expect_true(out$tier_a$pass)
  expect_true(out$verdict[["overall"]])

  # The mirror image: the double on the seam side instead.
  mirrored <- whep:::seam_gate(
    dplyr::mutate(gate_backcast_1890(), area_code = 100000L),
    dplyr::mutate(gate_seams(), area_code = 1e5)
  )
  expect_equal(mirrored$tier_a$reason, "")
  expect_true(mirrored$tier_a$pass)
})

# --- tier B -------------------------------------------------------------------

test_that("a start seam is reported vacuous, never passed", {
  # `s_u(t0 - 1)` here is the back-cast's own output, so the pair scores
  # one year of smooth extent change against a reference built from
  # observed year-to-year moves. It is a comparison the tier cannot
  # fail, and the numbers are reported saying so rather than as a pass.
  shares <- gate_wide_shares()
  out <- whep:::seam_gate(shares, gate_seams(area_code = 700L))

  gate <- dplyr::distinct(
    out$tier_b,
    basis,
    n_gated,
    n_beyond,
    frac_beyond,
    threshold,
    gate_status,
    pass
  )
  expect_equal(nrow(gate), 1L)
  expect_equal(gate$basis, "vacuous_by_construction")
  expect_equal(gate$gate_status, "vacuous_by_construction")
  expect_equal(gate$n_gated, 12L)
  expect_true(all(out$tier_b$status == "gated"))
  # The numbers are still there, and they are the numbers that used to
  # decide: comfortably inside the band.
  expect_lt(gate$frac_beyond, gate$threshold)
  expect_true(is.na(gate$pass))
  expect_true(is.na(out$verdict[["tier_b"]]))
})

test_that("the pure-LUH2 reduction passes tier B at an observed seam", {
  # Every year observed, so a source switch mid-record has an
  # observation on both sides and the tier means what it says.
  shares <- gate_wide_shares(t0 = 1890L)
  out <- whep:::seam_gate(
    shares,
    gate_seams(1902L, "source_switch", area_code = 700L)
  )

  gate <- dplyr::distinct(
    out$tier_b,
    basis,
    n_gated,
    n_beyond,
    frac_beyond,
    threshold,
    gate_status,
    pass
  )
  expect_equal(nrow(gate), 1L)
  expect_equal(gate$basis, "observed_both_sides")
  expect_equal(gate$n_gated, 12L)
  expect_equal(gate$gate_status, "gated")
  expect_lt(gate$frac_beyond, gate$threshold)
  expect_true(gate$pass)
  expect_true(out$verdict[["tier_b"]])
  expect_true(all(out$tier_b$status == "gated"))
})

test_that("an injected 30% step at an observed seam fails tier B", {
  shares <- gate_inject_step(gate_wide_shares(t0 = 1890L), t0 = 1902L)
  seams <- gate_seams(1902L, "source_switch", area_code = 700L)
  out <- whep:::seam_gate(shares, seams)

  gate <- dplyr::distinct(
    out$tier_b,
    basis,
    n_gated,
    n_beyond,
    frac_beyond,
    threshold,
    pass
  )
  expect_equal(gate$basis, "observed_both_sides")
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
  expect_equal(gate$n_beyond, 8L)
  expect_equal(gate$frac_beyond, 8 / 12)
  expect_gt(max(out$tier_b$log_ratio), 0.2)
  expect_lt(unique(out$tier_b$q_reference), 0.15)

  # Nothing else moved: the same fixture without the injection passes.
  clean <- whep:::seam_gate(gate_wide_shares(t0 = 1890L), seams)
  expect_true(unique(clean$tier_b$pass))
})

test_that("a three-unit container is reported, not pronounced on", {
  # A source switch inside the observed run, so the pair is real and the
  # only thing stopping a verdict is how few pairs there are.
  out <- whep:::seam_gate(
    gate_backcast_1890(),
    gate_seams(1905L, "source_switch")
  )

  expect_equal(nrow(out$tier_b), 3L)
  expect_true(all(out$tier_b$status == "gated"))
  expect_equal(unique(out$tier_b$basis), "observed_both_sides")
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

  # The start seam's 12 pairs are pooled with each other and with
  # nothing else: one gate for the vacuous basis and one for the five
  # switches, which is what keeps a vacuous rate out of a real one.
  gates <- dplyr::distinct(out$tier_b, basis, n_gated, gate_status)
  expect_equal(nrow(gates), 2L)
  expect_equal(
    gates$n_gated[gates$basis == "vacuous_by_construction"],
    12L
  )
  expect_equal(gates$n_gated[gates$basis == "observed_both_sides"], 60L)
  expect_equal(
    sort(unique(out$tier_b$seam_kinds[
      out$tier_b$basis == "vacuous_by_construction"
    ])),
    "start"
  )
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
  shares <- dplyr::filter(gate_wide_shares(), year <= 1902L)
  out <- whep:::seam_gate(
    shares,
    gate_seams(1902L, "source_switch", area_code = 700L)
  )

  # Observed 1900:1902, so two observed pairs per unit, of which the
  # gated one is held out: 12 units x 1 pair = 12 reference pairs, under
  # the default minimum of 20.
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

# --- tier B hold-out ----------------------------------------------------------

test_that("tier B cannot tell two opposite extent proxies apart", {
  # The defect T29b exists for, as a measurement. One panel, two extent
  # proxies with opposite per-unit trends: the histories they produce
  # differ by a factor of two where it matters, and tier B scores both
  # at ZERO exceedances.
  right <- gate_trend_extent(1)
  wrong <- gate_trend_extent(-1)
  observed <- gate_noisy_observed(right)
  seams <- gate_seams(area_code = 700L)

  a <- gate_trend_backcast(right, observed)
  b <- gate_trend_backcast(wrong, observed)

  first <- min(a$year)
  ratio <- dplyr::inner_join(
    dplyr::select(
      dplyr::filter(a, year == first),
      "level_polity_code",
      share_right = "share"
    ),
    dplyr::select(
      dplyr::filter(b, year == first),
      "level_polity_code",
      share_wrong = "share"
    ),
    by = "level_polity_code"
  )
  expect_lt(min(ratio$share_wrong / ratio$share_right), 0.5)
  expect_gt(max(ratio$share_wrong / ratio$share_right), 1.6)

  ga <- whep:::seam_gate(a, seams)
  gb <- whep:::seam_gate(b, seams)
  expect_equal(unique(ga$tier_b$n_beyond), 0L)
  expect_equal(unique(gb$tier_b$n_beyond), 0L)
  # The seam step is a single year of smooth extent change; the
  # reference is the panel's own year-to-year noise, an order larger.
  expect_lt(max(ga$tier_b$log_ratio), 0.1)
  expect_gt(unique(ga$tier_b$q_reference), 0.25)
  # So neither is passed: both report why the number decides nothing.
  expect_equal(unique(ga$tier_b$gate_status), "vacuous_by_construction")
  expect_true(is.na(ga$verdict[["tier_b"]]))
  expect_true(is.na(gb$verdict[["tier_b"]]))
})

test_that("the hold-out separates the two histories tier B cannot", {
  right <- gate_trend_extent(1)
  wrong <- gate_trend_extent(-1)
  observed <- gate_noisy_observed(right)
  seams <- gate_seams(area_code = 700L)

  ga <- whep:::seam_gate(
    gate_trend_backcast(right, observed),
    seams,
    holdout = list(extent = right)
  )
  gb <- whep:::seam_gate(
    gate_trend_backcast(wrong, observed),
    seams,
    holdout = list(extent = wrong)
  )

  # Same panel, same withheld years, same reference: the extent proxy is
  # the only thing that differs between the two runs.
  expect_equal(unique(ga$tier_b_holdout$anchor_year), 1910L)
  expect_equal(unique(gb$tier_b_holdout$anchor_year), 1910L)
  expect_equal(nrow(ga$tier_b_holdout), 12L * 10L)
  expect_true(all(ga$tier_b_holdout$status == "gated"))
  expect_true(all(gb$tier_b_holdout$status == "gated"))
  expect_equal(
    unique(ga$tier_b_holdout$q_reference),
    0.290442,
    tolerance = 1e-5
  )

  fa <- unique(ga$tier_b_holdout$frac_beyond)
  fb <- unique(gb$tier_b_holdout$frac_beyond)
  band <- unique(ga$tier_b_holdout$threshold)

  # Measured at seed 1: 1 of 120 pairs beyond for the right proxy, 46 of
  # 120 for the wrong one, against a band of 0.176. The separation is
  # not a knife edge, and it holds for every seed tried (1:12).
  expect_lt(fa, band)
  expect_gt(fb, band)
  expect_gt(fb - fa, 0.2)
  expect_true(unique(ga$tier_b_holdout$pass))
  expect_false(unique(gb$tier_b_holdout$pass))
  expect_true(ga$verdict[["tier_b_holdout"]])
  expect_false(gb$verdict[["tier_b_holdout"]])
  expect_false(gb$verdict[["overall"]])

  # The row says what was tested and what is being certified by it.
  expect_equal(unique(ga$tier_b_holdout$holdout_k), 10L)
  expect_equal(unique(ga$tier_b_holdout$n_backcast_years), 10L)
  expect_setequal(unique(ga$tier_b_holdout$horizon), 1:10)
})

test_that("a hold-out shallower than the error says nothing, and says so", {
  # The leg's power comes from the horizon: a wrong per-unit trend
  # accumulates linearly while the yardstick stays one year's move. At
  # k = 3 the same two proxies are NOT separated -- both pass. That is
  # the reason `holdout_k` defaults to 10 and the reason a pass carries
  # `n_backcast_years` beside it.
  right <- gate_trend_extent(1)
  wrong <- gate_trend_extent(-1)
  observed <- gate_noisy_observed(right)
  seams <- gate_seams(area_code = 700L)
  shallow <- whep:::seam_gate_tolerances(holdout_k = 3L)

  ga <- whep:::seam_gate(
    gate_trend_backcast(right, observed),
    seams,
    holdout = list(extent = right),
    tolerances = shallow
  )
  gb <- whep:::seam_gate(
    gate_trend_backcast(wrong, observed),
    seams,
    holdout = list(extent = wrong),
    tolerances = shallow
  )

  expect_equal(nrow(ga$tier_b_holdout), 12L * 3L)
  expect_equal(unique(ga$tier_b_holdout$anchor_year), 1903L)
  expect_true(unique(ga$tier_b_holdout$pass))
  expect_true(unique(gb$tier_b_holdout$pass))
})

test_that("the hold-out band counts units, not pairs", {
  # A unit's ten errors all carry its anchor year's own noise, so they
  # are one cluster and not ten independent draws. Dividing by pairs
  # instead of units narrows the band by sqrt(10) and flags proxies that
  # are right.
  right <- gate_trend_extent(1)
  observed <- gate_noisy_observed(right)
  out <- whep:::seam_gate(
    gate_trend_backcast(right, observed),
    gate_seams(area_code = 700L),
    holdout = list(extent = right)
  )
  tol <- whep:::seam_gate_tolerances()

  expect_equal(unique(out$tier_b_holdout$n_gated), 120L)
  expect_equal(unique(out$tier_b_holdout$n_units_gated), 12L)
  expect_equal(
    unique(out$tier_b_holdout$threshold),
    tol$null_rate +
      tol$binomial_sigma * sqrt(tol$null_rate * (1 - tol$null_rate) / 12)
  )
})

test_that("the hold-out holds its own window out of the reference", {
  right <- gate_trend_extent(1)
  observed <- gate_noisy_observed(right)
  shares <- gate_trend_backcast(right, observed)
  seams <- gate_seams(area_code = 700L)

  ten <- whep:::seam_gate(shares, seams, holdout = list(extent = right))
  twelve <- whep:::seam_gate(
    shares,
    seams,
    holdout = list(extent = right),
    tolerances = whep:::seam_gate_tolerances(holdout_k = 12L)
  )

  # Observed 1900:1930. With the anchor at 1910 the reference is the 20
  # pairs above it, per unit; two more withheld years take two more.
  expect_equal(unique(ten$tier_b_holdout$n_reference), 12L * 20L)
  expect_equal(unique(twelve$tier_b_holdout$n_reference), 12L * 18L)
})

test_that("a series too short to withhold is not applicable, not a pass", {
  right <- gate_trend_extent(1)
  observed <- gate_noisy_observed(right, t0 = 1925L)
  shares <- gate_trend_backcast(right, observed)
  out <- whep:::seam_gate(
    shares,
    gate_seams(1925L, area_code = 700L),
    holdout = list(extent = right)
  )

  # Six observed years, ten asked for: the leg refuses rather than
  # quietly running at a shorter horizon.
  expect_equal(nrow(out$tier_b_holdout), 1L)
  expect_equal(out$tier_b_holdout$status, "not_applicable_short_series")
  expect_equal(out$tier_b_holdout$gate_status, "not_applicable")
  expect_equal(out$tier_b_holdout$n_observed_years, 6L)
  expect_true(is.na(out$tier_b_holdout$anchor_year))
  expect_true(is.na(out$tier_b_holdout$pass))
  expect_true(is.na(out$verdict[["tier_b_holdout"]]))
})

test_that("without an extent the start seam is left with no evidence", {
  right <- gate_trend_extent(1)
  observed <- gate_noisy_observed(right)
  out <- whep:::seam_gate(
    gate_trend_backcast(right, observed),
    gate_seams(area_code = 700L)
  )

  expect_equal(nrow(out$tier_b_holdout), 1L)
  expect_equal(out$tier_b_holdout$status, "not_applicable_no_extent")
  expect_equal(out$tier_b_holdout$gate_status, "not_applicable")
  expect_true(is.na(out$tier_b_holdout$pass))
  expect_true(is.na(out$verdict[["tier_b_holdout"]]))
  # Neither tier B nor its hold-out has anything to say about this seam,
  # and the verdict says NA twice rather than TRUE once.
  expect_true(is.na(out$verdict[["tier_b"]]))
})

test_that("a series with no start seam is not the hold-out's business", {
  right <- gate_trend_extent(1)
  observed <- gate_noisy_observed(right)
  out <- whep:::seam_gate(
    gate_trend_backcast(right, observed),
    gate_seams(1905L, "source_switch", area_code = 700L),
    holdout = list(extent = right)
  )

  expect_equal(nrow(out$tier_b_holdout), 0L)
  expect_true(is.na(out$verdict[["tier_b_holdout"]]))
  expect_equal(unique(out$tier_b$basis), "observed_both_sides")
})

test_that("the hold-out refuses to be handed its own anchor", {
  right <- gate_trend_extent(1)
  observed <- gate_noisy_observed(right)
  shares <- gate_trend_backcast(right, observed)
  seams <- gate_seams(area_code = 700L)

  expect_error(
    whep:::seam_gate(shares, seams, holdout = list(args = list())),
    "must be a list with an"
  )
  expect_error(
    whep:::seam_gate(shares, seams, holdout = list(extent = right, k = 3)),
    "unknown element"
  )
  expect_error(
    whep:::seam_gate(
      shares,
      seams,
      holdout = list(extent = dplyr::select(right, -"extent_ha"))
    ),
    "missing column"
  )
  expect_error(
    whep:::seam_gate(
      shares,
      seams,
      holdout = list(extent = right, args = list(seam = "mine"))
    ),
    "which the leg sets"
  )
  # The back-cast reads two provenance columns the gate's own contract
  # does not ask for; without them the leg cannot run at all.
  expect_error(
    whep:::seam_gate(
      dplyr::select(shares, -"indicator_used"),
      seams,
      holdout = list(extent = right)
    ),
    "missing column"
  )
})

test_that("an override that does not take is unscored, not a pass", {
  # The trap the leg has to avoid: when the re-run anchors on nothing --
  # here because no observed row carries the binding indicator asked for
  # -- `backcast_admin_shares()` hands the observations straight back. A
  # leg that scored those would report a perfect reconstruction of the
  # very years it was supposed to withhold.
  right <- gate_trend_extent(1)
  observed <- gate_noisy_observed(right)
  out <- whep:::seam_gate(
    gate_trend_backcast(right, observed),
    gate_seams(area_code = 700L),
    holdout = list(
      extent = right,
      args = list(binding_indicator = "production")
    )
  )

  expect_true(all(out$tier_b_holdout$status == "not_reconstructed"))
  expect_true(all(is.na(out$tier_b_holdout$log_ratio)))
  expect_equal(unique(out$tier_b_holdout$n_gated), 0L)
  expect_true(is.na(out$verdict[["tier_b_holdout"]]))
})

test_that("the hold-out passes its settings to the back-cast", {
  # `args` is how a run's own settings reach the re-run. A `max_gap` of
  # zero leaves every withheld year unfilled, which the leg reports as
  # unscored rather than as a pass.
  right <- gate_trend_extent(1)
  observed <- gate_noisy_observed(right)
  out <- whep:::seam_gate(
    gate_trend_backcast(right, observed),
    gate_seams(area_code = 700L),
    holdout = list(
      extent = right,
      args = list(settings = list(max_gap = 0))
    )
  )

  expect_true(all(out$tier_b_holdout$status == "na_share"))
  expect_equal(unique(out$tier_b_holdout$n_gated), 0L)
  expect_equal(unique(out$tier_b_holdout$gate_status), "no_pairs")
  expect_true(is.na(out$verdict[["tier_b_holdout"]]))
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
  # The seam is the first year there is, so it has no pair to score:
  # vacuous by construction in the other way a start seam can be, with
  # nothing before it rather than a back-cast row before it.
  expect_equal(nrow(out$tier_b), 3L)
  expect_true(all(out$tier_b$status == "no_previous_row"))
  expect_equal(unique(out$tier_b$basis), "vacuous_by_construction")
  expect_equal(unique(out$tier_b$gate_status), "vacuous_by_construction")
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

test_that("tier C says when it had no regime column to check", {
  # Nothing in the package writes a `regime` column today, so on the
  # shipped crop-level output this axis is never evaluated. A gate that
  # passed while saying nothing about it would read as evidence that no
  # regime flipped: the report says how many gates are in that state.
  expect_message(
    out <- whep:::seam_gate(
      gate_backcast_1890(),
      gate_seams(),
      cells = gate_cells()
    ),
    "1 with the regime axis unchecked"
  )
  expect_false(out$tier_c$regime_checked)
  # `NA`, not `0`: an axis that was not evaluated has no count of
  # mismatches, and a zero there reads as a measurement.
  expect_true(is.na(out$tier_c$n_regime_mismatch))
  expect_true(out$tier_c$pass)

  expect_message(
    whep:::seam_gate(
      gate_backcast_1890(),
      gate_seams(),
      cells = gate_cells(regime = function(cell, year) "type_aware")
    ),
    "0 with the regime axis unchecked"
  )
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
      gate_inject_step(gate_wide_shares(t0 = 1890L), t0 = 1902L),
      gate_seams(1902L, "source_switch", area_code = 700L)
    )
  )
  expect_false(failing$verdict[["overall"]])
})

test_that("a seam naming a series the shares do not carry is refused", {
  shares <- gate_backcast_1890()

  # The whole seam list from another run: every tier evaluates nothing
  # and the gate used to pronounce the run passed.
  expect_error(
    whep:::seam_gate(shares, gate_seams(area_code = 901L)),
    class = "whep_seam_gate_seams_unmatched"
  )
  # The realistic shape is partial: most seams match, one names a series
  # the table does not carry and so is gated by nothing.
  expect_error(
    whep:::seam_gate(
      shares,
      dplyr::bind_rows(gate_seams(), gate_seams(area_code = 902L))
    ),
    "902"
  )
  # A seam for an item the share table does not carry is the same
  # mismatch, on a key the container code cannot show.
  expect_error(
    whep:::seam_gate(shares, gate_seams(item_prod_code = 44L)),
    class = "whep_seam_gate_seams_unmatched"
  )

  # A matched pair is untouched, and so is the documented tier-C-only
  # call, which passes a zero-row share table on purpose.
  expect_no_error(whep:::seam_gate(shares, gate_seams()))
  expect_no_error(
    whep:::seam_gate(shares[0, ], gate_seams(), cells = gate_cells())
  )
})

test_that("the gate reports a summary and returns five elements", {
  expect_message(
    out <- whep:::seam_gate(gate_wide_shares(), gate_seams(area_code = 700L)),
    "seam_gate"
  )
  expect_setequal(
    names(out),
    c("tier_a", "tier_b", "tier_b_holdout", "tier_c", "verdict")
  )
  expect_s3_class(out$tier_a, "tbl_df")
  expect_s3_class(out$tier_b, "tbl_df")
  expect_s3_class(out$tier_b_holdout, "tbl_df")
  expect_s3_class(out$tier_c, "tbl_df")
  expect_type(out$verdict, "logical")
})

test_that("an empty seam list leaves every tier's shape intact", {
  empty <- gate_seams()[0, ]
  out <- whep:::seam_gate(gate_backcast_1890(), empty)

  expect_equal(nrow(out$tier_b), 0L)
  expect_equal(nrow(out$tier_b_holdout), 0L)
  expect_equal(nrow(out$tier_c), 0L)
  expect_true(is.na(out$verdict[["tier_b"]]))
  expect_true(is.na(out$verdict[["tier_b_holdout"]]))
  expect_setequal(names(out$tier_b), whep:::.sg_tier_b_cols())
  expect_setequal(names(out$tier_b_holdout), whep:::.sg_holdout_cols())
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
