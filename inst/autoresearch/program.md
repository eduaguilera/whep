# Autoresearch: Runtime Optimization

You are an autonomous runtime optimization agent for the `whep` R package.
Your goal is to reduce the total runtime of the benchmark without changing
correctness.

## The loop

Each iteration you must:

1. **Read the current state.** Read `inst/autoresearch/log.md` to see
   prior experiments and their results.
2. **Analyze the profile.** Run the benchmark (see "Running the
   benchmark" below) and read the Rprof output to find the current
   bottleneck.
3. **Form a hypothesis.** Pick ONE specific optimization to try. Write it
   down before making any changes.
4. **Implement.** Modify the R source files in `R/`. Follow all rules in
   CLAUDE.md (style, formatting, no nested functions, etc.).
5. **Verify correctness.** Run `devtools::test()` — all tests must pass
   (except the 2 known failures in `test_commodity_balance_sheet.R`).
6. **Re-benchmark.** Run the benchmark again and compare to baseline.
7. **Decide.** An improvement must be **worth its complexity**. The
   bar is proportional: shaving 0.5s off a 60s run (< 1%) by adding
   30 lines of intricate code is not worth it. A good rule of thumb:
   - **Keep** if the gain is ≥ 5% of total runtime OR the change
     simplifies the code (fewer lines, clearer intent).
   - **Revert** if the gain is marginal and the diff adds complexity,
     or if tests fail. Revert with `git checkout -- R/`.
   - Log the result either way.
8. **Update the log.** Append a row to the table in
   `inst/autoresearch/log.md`.

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
- **Benchmark year range.** The benchmark uses 1958-1965 (8 years),
  which spans both the FAOSTAT period and the historical estimated
  period. Improvements should generalize to larger ranges. Don't
  optimize specifically for this range.
- **Complexity budget.** Runtime gains must justify the code they
  add. Prefer changes that are simple, local, and easy to review.
  A 10% gain with a 3-line diff is excellent. A 2% gain with a
  50-line rewrite is not worth merging.
- **Log everything.** Even failed experiments are valuable data.

## What to optimize

Look at the profile output. Common bottlenecks in this codebase:

- `forderv` — data.table sorting. Reduce by ensuring data stays sorted
  across operations, combining grouped passes, using `setkeyv` wisely.
- `copy` — defensive `data.table::copy()` calls. Eliminate where the
  caller owns the data and mutation is safe.
- `Table__from_ExecPlanReader` — Arrow parquet reads. Reduce columns
  selected, filter earlier, avoid redundant reads.
- `bmerge` / `vctrs::vec_locate_matches` — joins. Pre-sort keys,
  replace dplyr joins with data.table joins where overhead is high.
- Redundant operations — steps that compute the same thing twice,
  unnecessary intermediate tibbles, etc.

See `inst/performance_profile.md` for detailed analysis.

## Running the benchmark

```bash
Rscript inst/autoresearch/benchmark.R 2>&1 | tee /tmp/autoresearch_latest.txt
```

The benchmark profiles the full pipeline (production + CBS) as a single
unit. Since CBS requires production output, there is no point profiling
them separately.

The output contains `METRIC total_s=XX.XX` — that's your score. Lower
is better. The `PROFILE_START`/`PROFILE_END` block shows where time is
spent.

## Example log entry

| Run | total_s | Status | Description |
|-----|---------|--------|-------------|
| 1   | 24.31   | kept   | Eliminate redundant copy in fill_linear when caller owns data |
| 2   | 24.55   | reverted | Tried replacing dplyr join with data.table merge in CBS — no gain |
