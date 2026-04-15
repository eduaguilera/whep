# autoresearch

Autonomous optimization loops for this package, driven by Claude Code.
Each subfolder targets one optimization goal (a function, a pipeline, a
specific bottleneck). This directory is `.Rbuildignore`d — nothing in
here ships with the package.

## How it works

Each use case is a self-contained folder with three files:

- **`program.md`** — instructions the agent follows on every iteration:
  what to optimize, what constraints to respect, how to judge whether a
  change is worth keeping.
- **`benchmark.R`** — a reproducible harness that runs the code under
  `Rprof` and prints three machine-readable blocks:
  - `METRIC total_s=<seconds>` — the optimization score (lower = better).
  - `PROFILE_START/END` — top 15 self-time entries.
  - `CALLERS_START/END` — which code paths trigger the main hotspots.
- **`log.md`** — append-only table of every experiment (kept and
  reverted). This is how the loop accumulates knowledge across runs and
  stops re-trying dead ends.

On each iteration the agent reads the log, runs the benchmark, forms one
hypothesis, makes one targeted change in `R/`, runs tests, re-benchmarks,
then either keeps or reverts — and always appends to the log.

## Running a use case

Drive the loop with Claude Code:

```
/loop check autoresearch/<FOLDER>/program.md
```

The agent self-paces between iterations. Run the benchmark directly any
time to sanity-check state:

```bash
Rscript autoresearch/<FOLDER>/benchmark.R
```

## Starting a new use case

1. Copy `TEMPLATE/` to a new folder named for what you're optimizing
   (e.g. `autoresearch/my_feature/`).
2. Edit `program.md`: fill in `<FEATURE>`, `<PACKAGE>`, `<GOAL>`,
   `<FOLDER>`. Note any pre-existing test failures so the agent doesn't
   chase them.
3. Edit `benchmark.R`: set the config block (inputs sized for ~20-60s
   runs) and add the actual function calls to profile. Adjust the
   `targets` vector if the hotspots you expect aren't forderv/bmerge/copy.
4. Run the benchmark once and record the baseline in `log.md`.
5. Kick off the loop:
   `/loop check autoresearch/<FOLDER>/program.md`.

## Existing use cases

- [`build_functions/`](build_functions/) — runtime optimization of
  `build_primary_production()` and `build_commodity_balances()`. Focuses
  on tidyverse/data.table pipeline bottlenecks (forderv, bmerge, format
  conversions).
