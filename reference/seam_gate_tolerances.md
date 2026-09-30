# Tolerances of the admin-shares seam gate

The user-settable numbers
[`seam_gate()`](https://eduaguilera.github.io/whep/reference/seam_gate.md)
judges with, in one place so a run records what it was gated against.
Each is a **proposal** of the subnational-spatialization plan's T29 and
may be replaced by its T31 allocation-policy decision; none is a
measured quantity and none is derived from data, so each is stated here
with the reason it has the value it has rather than left at a call site.

- `identity_rel` (1e-8): tier A's relative tolerance on a unit-share
  total, the plan's figure. The back-cast leaves the anchor year's
  observed rows untouched, so the only thing between the two sides is
  floating-point summation of at most a few hundred shares, which 1e-8
  is orders of magnitude above.

- `identity_floor` (1e-12): denominator guard for that relative
  comparison, so a unit whose observed share rounds to zero does not
  turn a difference of 1e-18 into a ratio of 1e6. A numerical guard, not
  a scientific threshold.

- `reference_quantile` (0.95): tier B's reference point, the plan's Q95
  of the observed consecutive log-ratio distribution. The null
  exceedance rate `null_rate` follows from it as `1 - quantile` and is
  returned rather than set, so the two cannot disagree.

- `binomial_sigma` (2): how many standard errors of slack the tier-B
  gate allows above `null_rate`, giving the plan's
  `0.05 + 2 * sqrt(0.05 * 0.95 / n)`. Two standard errors is a ~95%
  one-sided band under the null that a seam pair is an ordinary pair.

- `min_reference` (20): the smallest reference sample a quantile is
  taken from. Below it the Q95 is essentially the largest one or two
  observed ratios, so the series is reported unevaluable instead of
  being judged against a threshold that is itself noise. Assumed,
  unverified: no measurement fixes it, and T31 may.

- `cell_ratio_bounds` (`c(0.55, 1.6)`): tier C's plausible band,
  [`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md)'s
  own default, kept so the cell smoke test speaks the same language as
  the rest of the `check_*` library.

- `cell_min_ha` (100): tier C's hectare floor. A cell holding under a
  square kilometre of a crop in both years of a pair is dropped before
  the scan, its share being numerically tiny and its ratio dominated by
  rounding. The same 100 ha
  [`backcast_admin_shares()`](https://eduaguilera.github.io/whep/reference/backcast_admin_shares.md)
  inherits from `.fix_luh2_crop_collapse()`'s `min_neighbor_mha = 0.001`
  Mha.
  [`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md)'s
  `min_value` cannot express it: that applies to the scanned column,
  which here is a share, not an area.

- `cell_excess` (0.01): the plan's one percentage point. Tier C passes
  when the seam pair's flag rate exceeds the mean of its two
  neighbouring pairs' rates by no more than this.

- `holdout_k` (10): how many of a series' earliest observed years the
  tier-B hold-out withholds. The leg's power grows with the horizon,
  because a wrong per-unit trend in the extent proxy accumulates
  linearly with the years back-cast while the yardstick stays a single
  year's move: on this package's own two-history fixture
  (`test_admin_shares_gate.R`), two extent tables whose reconstructions
  differ by a factor of 0.40 to 1.63 at ten years are **not** separated
  at `holdout_k = 3` (both pass) and are separated decisively at 10
  (0.008 against 0.383 of pairs beyond the reference quantile). Each
  withheld year is also an observation removed from both the anchor and
  the reference, so `holdout_k` cannot approach the length of the
  record; ten leaves two decades of reference pairs on a thirty-year
  series. Assumed, unverified: no measurement fixes it, and it is a
  choice about how deep an extrapolation the gate certifies, not a
  property of the data.

Two further numbers are **derived** from those, and returned rather than
set, so they cannot disagree with them:

- `null_rate`, the tier-B null exceedance rate `1 - quantile`.

- `min_gated`, the smallest number of gated pairs a container gate is
  pronounced on. Below it the gate is arithmetically incapable of
  passing a container with a single exceeding pair: one pair out of `n`
  is a rate of `1/n`, and `1/n` exceeds the band whenever
  `1/n > null_rate + binomial_sigma * sqrt(...)`. At the defaults that
  is every `n` up to 3, so `min_gated` is 4 and a container with three
  gated pairs is reported `"too_few_pairs"` rather than failed. This is
  a property of the band, not a preference.

## Usage

``` r
seam_gate_tolerances(
  identity_rel = 1e-08,
  identity_floor = 1e-12,
  reference_quantile = 0.95,
  binomial_sigma = 2,
  min_reference = 20L,
  cell_ratio_bounds = c(0.55, 1.6),
  cell_min_ha = 100,
  cell_excess = 0.01,
  holdout_k = 10L
)
```

## Arguments

- identity_rel:

  Relative tolerance for tier A.

- identity_floor:

  Denominator guard for tier A.

- reference_quantile:

  Reference quantile for tier B, in (0, 1).

- binomial_sigma:

  Standard errors of slack in tier B's gate.

- min_reference:

  Smallest reference sample tier B will judge against.

- cell_ratio_bounds:

  Length-2 plausible band for tier C, passed to
  [`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md).

- cell_min_ha:

  Hectare floor applied to tier C's cells before the scan.

- cell_excess:

  Largest flag-rate excess tier C accepts, as a fraction.

- holdout_k:

  How many of a series' earliest observed years the tier-B hold-out
  withholds.

## Value

A named list of the arguments plus `null_rate` and `min_gated`.

## Examples

``` r
seam_gate_tolerances()$null_rate
#> [1] 0.05
seam_gate_tolerances(reference_quantile = 0.99)$null_rate
#> [1] 0.01
```
