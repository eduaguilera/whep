# Footprint/Production Validation Log

Append-only record of every iteration. The score is `n_flag` (lower is better)
over the deterministic grid; `correctness = n_pass / (n_pass + n_flag)`. Record
research (corpus growth), diagnoses, and fixes — including dead ends.

## Baseline

Grid: top-5 producers x {Wheat, Rice} x {1970, 2000, 2010} x {production, area}
= 60 cases. Corpus seeded with 5 hand-anchored, cited rows.

| Run | n_flag | n_pass | n_uncovered | correctness | Action | Notes |
|-----|--------|--------|-------------|-------------|--------|-------|
| 0 | 1 | 3 | 55 | 0.75 | baseline | 60-case grid vs 5-row seed corpus. Only flag: `prod-CHN-rice-1970`. |

## Settled questions (don't re-litigate)

- **Rice is milled-equivalent in WHEP.** `R/build_production.R` `.fix_rice_milled_equiv()`
  converts paddy rice x~0.67 to match FABIO's milled rice. Any rice probe must
  pin ground truth on the **milled** basis. `prod-IND-rice-2000` was re-pinned
  paddy->milled (DES 84.98 Mt, independent) and now passes. `prod-CHN-rice-1970`
  still flags only because no independent *milled* source for 1970 China is
  pinned yet — it is a `definitional_mismatch`/`known_convention`, NOT a bug.
- **Country entity matters.** WHEP `CHN` = FAO area 41 (mainland), not the FAO
  "China" aggregate (area 351). Ground truth must match the mainland entity.
