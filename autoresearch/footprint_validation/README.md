# footprint_validation

Point-level correctness validation for WHEP, as an autoresearch case. Instead of
scoring against one aggregate reference (e.g. FABIO), it samples concrete points
— "USA wheat production 2000", "China rice production 1970" — and checks each
against a value fetched from a source the field already trusts (USDA NASS,
EUROSTAT, FAOSTAT, national statistics). A failing probe is a concrete,
debuggable claim.

## Files

| File | Role |
|---|---|
| `cases.R` | Deterministic grid generator: top-N producers (per crop-year) x years x crops x layers. Keeps only real ISO3 countries (drops FAO aggregates). |
| `validate.R` | Deterministic core: resolve names→codes, extract WHEP values, judge vs corpus, corpus I/O, build cache. Offline, no network. |
| `ground_truth.csv` | The pinned, citation-backed corpus. Grown by research; then checks are deterministic and free. |
| `golden_probes.csv` | Hand-curated anchor probes (optional union with the grid). |
| `benchmark.R` | Runs the grid + judge; prints `METRIC` + `FLAGGED_JSON` / `UNCOVERED_JSON`. The autoresearch "score". |
| `research_prompt.md` | Subagent template: fetch-don't-recall, prefer FAOSTAT-independent sources, capture definitions. |
| `harness.workflow.js` | Workflow orchestration: detect → research → re-detect → diagnose → guarded fix. |
| `program.md` | Loop instructions + constraints (diagnose-before-fix, regression guard, human-gated). |
| `log.md` | Append-only experiment log + settled questions. |

## Run

```bash
# Deterministic score any time:
Rscript autoresearch/footprint_validation/benchmark.R
```

Code-driven loop (schema-validated subagents), via Claude Code:

```
Workflow({ scriptPath: "autoresearch/footprint_validation/harness.workflow.js",
           args: { research_cap: 6, do_fix: false } })
```

`do_fix: true` lets it attempt guarded fixes (isolated worktree, regression-checked,
left uncommitted for review). Or drive it as a single agent:
`/loop check autoresearch/footprint_validation/program.md`.

## Verdicts

`pass`, `flag_high`/`flag_low` (WHEP outside tolerance), `uncovered` (no ground
truth pinned — a research gap, never a pass), `missing_whep` (WHEP produced no
value), `unit_mismatch`/`unknown_unit`, `unknown_area`/`unknown_item`.

## Design notes

- **Build, don't pin.** WHEP values come from `build_primary_production()` (public
  pins via `whep_read_file()`), cached under `.whep_cache/` keyed by year range.
  `WHEP_VALIDATE_REFRESH=1` rebuilds.
- **A flag is a question, not a bug.** Diagnosis classifies real_bug vs
  definitional_mismatch vs ground_truth_error vs known_convention. The rice
  paddy/milled case is why this stage is mandatory (see `log.md`).
- **Fixes touch only `R/`** and must regress nothing; the corpus/probes are never
  edited to make a test pass.
