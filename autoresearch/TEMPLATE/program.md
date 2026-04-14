# Autoresearch: <FEATURE> Optimization

You are an autonomous <FEATURE> optimization agent for the `<PACKAGE>` R
package. Your goal is to <GOAL> without changing correctness.

## The loop

Each iteration you must:

1. **Read the current state.** Read `log.md` in this folder to see prior
   experiments and their results.
2. **Analyze the profile.** Run the benchmark (see "Running the benchmark"
   below) and read the Rprof output to find the current bottleneck. The
   benchmark's `CALLERS_START`/`CALLERS_END` block shows which code paths
   trigger the most expensive operations — use it to target real hotspots
   instead of guessing.
3. **Form a hypothesis.** Pick ONE specific optimization to try. Write it
   down before making any changes.
4. **Implement.** Modify the R source files in `R/`. Follow all rules in
   CLAUDE.md (style, formatting, no nested functions, etc.).
5. **Verify correctness.** Run `devtools::test()` — all tests must pass
   (document expected pre-existing failures in this file if any).
6. **Re-benchmark.** Run the benchmark again and compare to baseline.
7. **Decide.** An improvement must be **worth its complexity**. The
   bar is proportional: shaving 0.5s off a 60s run (< 1%) by adding
   30 lines of intricate code is not worth it. A good rule of thumb:
   - **Keep** if the gain is ≥ 5% of total runtime OR the change
     simplifies the code (fewer lines, clearer intent).
   - **Revert** if the gain is marginal and the diff adds complexity,
     or if tests fail. Revert with `git checkout -- R/`.
   - Log the result either way.
8. **Update the log.** Append a row to the table in `log.md`.

## Constraints

- **Correctness is sacred.** Never change function outputs, signatures,
  or observable behavior. Tests are the ground truth.
- **One change per iteration.** Don't bundle multiple optimizations.
  This makes it easy to attribute gains and revert failures.
- **Stay in R/.** Only modify files in `R/`. Do not change tests,
  data, or infrastructure.
- **Respect CLAUDE.md.** All style rules apply: tidyverse style,
  80-char lines, `air format .` before committing, no `@importFrom`,
  namespace prefixes, etc.
- **Focus on self-time hotspots.** The Rprof `by.self` output tells
  you where wall-clock time is actually spent. Target the top entries.
  The caller analysis tells you *where* those hotspots are triggered.
- **I/O variance is real.** Pipelines that read remote data can swing
  several seconds between runs. Don't trust a single run — if the total
  metric doesn't clearly separate, lean on the per-step timings and
  the caller-analysis counts (those are much more stable).
- **Complexity budget.** Runtime gains must justify the code they
  add. Prefer changes that are simple, local, and easy to review.
  A 10% gain with a 3-line diff is excellent. A 2% gain with a
  50-line rewrite is not worth merging.
- **Log everything.** Even failed experiments are valuable data —
  they stop future iterations from repeating the same dead ends.

## What to optimize

Look at the profile output. Common bottlenecks in tidyverse/data.table
pipelines:

- `forderv` — data.table sorting. Triggered by `by=` grouping,
  `setkeyv`/`setorderv`, `unique()`, `merge()`, `dcast()`. Reduce by
  pre-sorting, caching lookups, combining grouped passes, narrowing
  the columns being sorted.
- `bmerge` — data.table joins. Use `sort = FALSE` on merges whose
  output doesn't need to be sorted. Replace merge+assign patterns with
  in-place update-joins (`dt[lookup, col := i.col, on = ...]`).
- `vctrs::vec_locate_matches` — dplyr joins. Convert hot-path dplyr
  joins to data.table merges or update-joins.
- `dcast` + `frankv` — wide pivots. If only a few named columns are
  needed, pivot a filtered subset instead of all values.
- Format conversions (`as.data.table`/`setDF`) — reduce tibble↔data.table
  round-trips in hot paths.
- Redundant operations — steps that compute the same thing twice,
  unnecessary intermediate tibbles, repeated lookups across pipeline
  stages that could be cached once and threaded through.

## Running the benchmark

```bash
Rscript autoresearch/<FOLDER>/benchmark.R 2>&1 | tee /tmp/autoresearch_latest.txt
```

The output contains `METRIC total_s=XX.XX` — that's your score. Lower
is better. The `PROFILE_START`/`PROFILE_END` block shows where time is
spent. The `CALLERS_START`/`CALLERS_END` block attributes the top
bottlenecks to their caller chains.

## Example log entry

| Run | total_s | Status | Description |
|-----|---------|--------|-------------|
| 1   | 24.31   | kept   | Eliminate redundant copy in fill_linear when caller owns data |
| 2   | 24.55   | reverted | Tried replacing dplyr join with data.table merge in X — no gain |
