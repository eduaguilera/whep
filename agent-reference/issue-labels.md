# WHEP agent reference — issue and PR labels

Read before opening or labelling an issue or PR.

Moved out of `AGENTS.md` on 2026-09-27 to keep that file under Codex's 32 KiB
default instruction-file read cap (`project_doc_max_bytes`). `AGENTS.md`
carries the pointer; this file carries the detail.

---

### Labels — the triage surface

Labels are how the maintainer decides at a glance what can be batch-merged and
what needs a domain expert. Label **every** issue extensively:

- **Triage axis (required, exactly one)**: `mechanical` or `needs-expert`.
  Reference-value and coefficient changes are always `needs-expert`, even at
  one line. Infra, packaging, CRAN, docs, testing, and pure crash/identity
  fixes are `mechanical`. When in doubt, `needs-expert`.
- **Subsystem** (one or more, where applicable): `area:cbs`,
  `area:production`, `area:livestock`, `area:trade`, `area:footprint`,
  `area:nitrogen`, `area:soc`, `area:gapfilling`, `area:lmdi-decomp`,
  `area:data-io`, `area:spatialize`, `area:regions`, plus cross-cutting
  `fabio`, `lpjml`, `footprint-extension`. Cross-cutting infra/meta issues
  legitimately carry none — do not force one.
- **Type**: `bug` / `enhancement` / `documentation` / `dev-infra` /
  `testing` / `release`.
- **Priority**: a `priority:*` label.

Apply the full set when opening an issue; backfill missing labels when you
touch an old one.

**A missing axis or an understated priority makes a real defect invisible.**
The 2026-08-27 sweep found ~15 issues with no triage axis at all, and one that
sat at `priority:low` for months while describing a coefficient table that is
live in the Tier 2 emissions path — it was then rediscovered four times by
work that could not see it. Priority is not a guess at effort; it is what
decides whether anyone looks. If an issue describes something that moves a
published number today, it is not `priority:low`, however small the diff.

PRs do not need the labels duplicated when they close an already-labelled
issue.

#### Contributor-facing labels

Three further labels exist for people outside the team, who pick work by them.
Guidelines live in `.github/CONTRIBUTING.md` (that path keeps them out of the
package build — `^\.github$` is in `.Rbuildignore`).

- **`good first issue`** + **`help wanted`**: small, self-contained and
  precisely specified. **Verify the defect still exists in current code before
  applying it.** Several audit-era issues had already been fixed by a later
  commit while the issue stayed open; sending a newcomer to one of those is
  worse than leaving it unlabelled.
- **`no-data-needed`**: the issue can be reproduced, fixed **and verified**
  using only a clone. Package data (`data/*.rda`, `inst/extdata/`), hand-built
  `tribble()` fixtures and injected arguments all count as available; pins,
  `WHEP_*_DIR` rasters and any network read do not.

`no-data-needed` exists because the data barrier, not the science, is what
usually blocks an outside contributor: the test suite is fully offline, but a
real pipeline build needs inputs that cannot be handed out. Two rules keep it
worth having — apply it only after checking the verification path really is
offline, and never apply it to code that is not on `main` (work living on an
unmerged feature branch cannot be picked up from a fresh clone).
