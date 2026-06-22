# Autoresearch: Footprint/Production Validation

You are an autonomous **correctness** agent for the `whep` R package. Unlike the
other autoresearch cases (which optimize runtime), your goal is to make WHEP's
outputs agree with **trusted external ground truth** at specific points, and to
fix real divergences without breaking anything else.

The unit of work is a **probe**: one factual point like "USA wheat production
2000" or "China rice production 1970". The harness builds a deterministic grid
of probes (top-N producers x years x crops x layers), looks up ground truth in a
pinned corpus, and reports which probes pass, flag, or are uncovered.

## The loop

1. **Read the log.** `log.md` records prior iterations (what was fixed, what was
   a definitional issue, what was a dead end). Don't repeat settled questions.
2. **Run the benchmark.** `Rscript autoresearch/footprint_validation/benchmark.R`
   prints `METRIC n_flag=.. n_pass=.. n_uncovered=.. correctness=..` plus
   `FLAGGED_JSON` and `UNCOVERED_JSON` blocks. The score is `n_flag` (lower is
   better); `correctness = n_pass / (n_pass + n_flag)`.
3. **Close research gaps first.** For `UNCOVERED` probes, research the ground
   truth (web), **fetching** values from trusted sources (never recalling them;
   see `research_prompt.md`), and append cited rows to `ground_truth.csv`. This
   is "research once, pin forever" - re-runs are then deterministic and free.
4. **Diagnose before fixing.** For each `FLAGGED` probe, decide *why* it
   disagrees BEFORE touching code. Classify:
   - `real_bug` — WHEP computes the wrong number (unit, mapping, gap-fill).
   - `definitional_mismatch` — both right, different bases (e.g. rice paddy vs
     milled: WHEP applies `.fix_rice_milled_equiv` x0.67 in `R/build_production.R`;
     carcass vs live weight; area harvested vs planted; FAO aggregate vs mainland).
   - `ground_truth_error` — the pinned value is wrong/misread.
   - `known_convention` — an intentional, documented WHEP choice.
   - `needs_human` — genuinely ambiguous.
   A discrepancy is **not** assumed to be a bug. Most early flags are
   definitional - fix the corpus/probe basis, not the code.
5. **Fix only real bugs, guarded.** If and only if `real_bug`:
   - Make the smallest change in `R/` that fixes the root cause (CLAUDE.md style).
   - Run `devtools::test()` — all tests pass.
   - Re-run the benchmark. **Keep only if** the target probe now passes AND no
     previously-passing probe regresses AND tests pass. Otherwise
     `git checkout -- R/` and revert.
6. **Update the log.** Append a row either way.

## Constraints (read these every time)

- **A flag is a question, not a verdict.** The diagnose step is mandatory. The
  rice paddy/milled case is the canonical example: "fixing" it would have ripped
  out an intentional conversion.
- **Never edit the test to pass.** Fixes may touch only `R/`. `ground_truth.csv`,
  the probe grid, and `benchmark.R` are off-limits to a code fix. Correcting a
  *definitional* or *ground-truth* issue means editing the **corpus/probe**, and
  is logged as such — never both in one iteration.
- **No regressions.** Always re-run the full grid after a change. A fix that
  resolves one probe but flags another is rejected.
- **Fetch, don't recall.** Every ground-truth value needs a real source + URL.
  If you can't fetch it, mark the probe uncovered (confidence none) — never guess.
- **Prefer FAOSTAT-independent sources.** WHEP ingests FAOSTAT; checking against
  FAOSTAT only validates plumbing. USDA NASS, EUROSTAT, national stats are
  stronger.
- **Human-gated merges.** Leave accepted fixes uncommitted for review. Don't
  commit or push.
- **Respect CLAUDE.md.** Tidyverse style, 80-char lines, `air format .`,
  namespaces, no nested functions.

## Two ways to run

- **Code-driven (preferred):** the Workflow orchestrates the whole loop with
  schema-validated subagents (detect -> research -> diagnose -> guarded fix):
  `Workflow({ scriptPath: "autoresearch/footprint_validation/harness.workflow.js",
  args: { research_cap: 6, do_fix: false } })`. Set `do_fix: true` to let it
  attempt guarded fixes in isolated worktrees.
- **Single-agent (`/loop`):** `/loop check autoresearch/footprint_validation/program.md`
  — you drive the steps above yourself, self-pacing between iterations.
