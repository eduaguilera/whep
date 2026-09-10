# validation/ — WHEP ground-truth validation

> **Prerequisites — this needs external data and network.** Nothing under
> `validation/cache/` is committed; it is (re)generated on first run. WHEP's own
> values come from public pins (`whep_read_file()`), and most ground truth is
> **downloaded on demand** (GAEZ from the FAO bucket via `gaez_potential.R`, USDA
> PSD, MapSPAM) or **pinned/committed** (Poore & Nemecek occupation,
> `gt_occupation.json`). Two sources need **manual setup**: the multi-GB USDA
> NASS bulk for USA production (point `cache/local_paths.json` at it — see below)
> and GGCMI for cycle length (Zenodo). So a first run will download files and
> read pins; it is not offline. See **`SOURCES.md`** for the full manifest of
> every source, where it lives, and its access terms.
>
> **Layout:** all data we compare against lives under the gitignored
> `validation/cache/` (`data/`/`files/` raw sources, `ground_truth/` pinned
> values, `findings/` WHEP-side, `local_paths.json`). Run the whole
> multi-variable sweep with `Rscript validation/validate_all.R`.

A standalone script (no `autoresearch/` ceremony) that validates WHEP's
**national** crop production against independently-compiled **subnational**
(state/province) statistics. For each top-N production country, a subagent
searches for subnational data, sums it to a national total per crop-year, and we
compare that against WHEP. `.Rbuildignore`d — never ships.

The idea: subnational statistics are gathered separately from the national
headline, so their sum is a genuine independent check on the figure WHEP ingests
— and it catches whatever crops/years that source happens to cover, rather than
a hardcoded grid.

## Pieces

| File | Role |
|---|---|
| `subnational.workflow.js` | The script. Iterates one subagent per top-N country (rank → discover → compare). Run via the Workflow tool. |
| `rank_countries.R` | Deterministic: build/cache WHEP production, rank top-N producers of the crops, emit `COUNTRIES_JSON`. |
| `compare_findings.R` | Deterministic: take discovered findings, pull WHEP national values, judge ratios, emit `VERDICTS_JSON`. |
| `validate.R` | Shared deterministic core (resolve/extract/judge, unit canonicalization, build cache). |
| `variables.R` | Variable registry (production, area, occupation, cycle_length, cropping_intensity, stability) + their WHEP-side extractors. |
| `stability.R` | Time-series stability check (internal archetype): flags year-over-year discontinuities in WHEP's own series. No external data. |
| `year_scoping.R` | Year-scoping equivalence check (internal archetype): a scoped `build_x(years = Y)` must equal the full-range build filtered to `Y`. See below. |
| `memory_floor.R` | Resident-memory floor left by the production/CBS chain (internal archetype): splits what a fat process is holding into live objects, glibc blocks freed but not returned, and another allocator's pages. See below. |
| `gt_year_scoping.json` | Recorded per-unit divergence budget for that check. **Committed** — a ratchet, meant to be tightened as each filed defect is fixed. |
| `compare_variable.R` | Variable-aware comparator: extracts WHEP's value, joins ground truth on the registry grain, judges (ratio or bound mode). |
| `validate_all.R` | One-shot sweep: runs every variable's check and prints a combined scorecard. |
| `citation_dois.R` | Asserts every DOI cited anywhere in the repo is **registered**. `R CMD check --as-cran` only sees `man/*.Rd`, and 34 of the repo's 71 DOIs appear only outside it (#900). Needs network. |
| `gaez_potential.R` | Builds the per-country GAEZ potential cropping-intensity ceiling. **Downloads** GAEZ v4 multiple-cropping zones (rainfed `mcr` + irrigated `mci`) from the open FAO bucket → `cache/files/GAEZ/`. No local copy needed (`WHEP_GAEZ_DIR` overrides). |
| `gt_occupation.json` | Pinned LCA occupation ground truth (Poore & Nemecek 2018), m²·yr/kg per crop. Committed. |
| `cache/sources.json` | Registry of subnational datasets discovered so far (per country: source, URL, what it covers, basis). **Committed** — shared knowledge of where the data lives. |
| `cache/files/<iso3>/` | The raw downloaded datasets (PDFs, xls, csv, API JSON). **Gitignored** (can be large); re-downloaded from the registry URL if missing. |
| `cache/findings/<iso3>.json` | Each discover agent writes its extracted, converted national totals here. The compare step reads these files directly — **bulk data never travels through a prompt**. Doubles as a findings cache. Gitignored. |
| `nass_sum.R` | Deterministic USA extractor: sums STATE rows from the local USDA NASS bulk CSV → national tonnes, writes `cache/findings/USA.json`. No web, no PDF. |
| `lpjml_faostat_crops.R` | Checks a finished **LPJmL run's** crop output against FAOSTAT per CFT and country. See below. |
| `lpjml_globalflux.R` | Checks a finished **LPJmL run's** global fluxes for spinup equilibration and against published observational estimates. See below. |
| `lpjml_pins.R` | Guards the four **LPJmL-derived input pins** against their recorded contract, physical invariants and magnitudes, so a pin swap cannot pass silently. See below. |
| `gt_lpjml_pins.json` | Recorded magnitudes for those pins. **Committed** — it is the tripwire, and it is meant to fail when the pins change. |
| `lpjml_forcing_pins.R` | Guards the six **climate-forcing pins** (the NetCDF grids that feed *into* LPJmL) against their grid contract and physical impossibility bounds. Its sibling above excludes them by design; #824 is why they still need a check. See below. |
| `gt_lpjml_forcing_pins.json` | Recorded state of those pins, including the **known** negative-radiation count of #824. **Committed** — compared bidirectionally, so a count that falls is as loud as one that rises. |
| `lpjml_wind_provenance.R` | Audits `lpjml-wind-isimip-1901-2019` against the ISIMIP2a files it claims to come from, by rebuilding the monthly means with `cdo` and requiring exact equality. The sibling above answers "is this pin corrupt?"; this answers "is it the dataset its name says?" (#371). Needs the daily chunks on disk via `WHEP_ISIMIP_WIND_DIR`; no baseline file, because the source *is* the baseline. |
| `temp_grassland_6633.R` | Checks modelled CBS 3002 (temporary grassland, the quantity PR #349 nets out of the FAO arable target) against FAOSTAT RL item 6633, **official rows only** — 68% of that series is FAO-imputed, including outright imputed zeros for Greece and Poland. See below. |
| `gt_temp_grassland_6633.json` | Recorded state of that comparison per modelled concept. **Committed** — a tripwire, meant to fail when the fodder reconstruction moves. |
| `admin_drift_tvd.R` | Sizes the within-country geography a spatialization pattern frozen at one reference year cannot represent (#1000): total variation distance between an admin-unit share vector and its reference year, on observed subnational statistics. Also emits the per-source discrepancy and gap-year distributions the allocation policy is decided against. Needs `WHEP_SUBNATIONAL`; skips with a message when unset. See below. |
| `gt_admin_drift.json` | Recorded state of that measurement per country × indicator × reference year × year, plus the gap-year and discrepancy summaries. **Committed** — a tripwire, meant to fail when the compiled panel changes. |
| `admin_seam_gate.R` | Judges one `run_spatialize()` output for seam continuity (#1000): the three-tier `seam_gate()` — anchor identity, seam log-ratios against the observed distribution, cell-share jump rates around each seam — plus a derived window scan. Reads one local output directory; needs `WHEP_SPATIALIZE_OUT_DIR`, skips with a message when unset. See below. |
| `gt_admin_seam_gate.json` | Recorded state of that gate per container × seam year. **Committed once a real run has been recorded** — a tripwire, meant to fail when the allocation moves. Absent until then. |
| `japan_pilot.R` | Runs **one whole constrained spatialization** end to end (#1000): Japan at depth 1, its national crop totals split across 46 prefectures by MAFF's reported areas, 1961–2022, six crops. The first `run_spatialize(level = 1)` on real administrative statistics rather than a fixture. Reproduces a hand-computed anchor (Hokkaido, paddy rice, 2000) and reports coverage, discrepancy, capacity breaches, straddling, interior gaps and the seam gate on real numbers. Needs `WHEP_POLYCELL_SUPPORT_PATH` (the Japan pilot support) and `VAL_JP_INPUT_DIR`; skips loudly when either is absent, which a fresh clone always does. See below. |
| `gt_japan_pilot.json` | Recorded state of that run: the pre-scan per item over the full stated scope, the per-item coverage and breach summary, the seam-gate tiers at their own gate grain, the interior-gap summary, and the anchor. **Committed** — a tripwire, meant to fail when the allocation, the pin or the support moves. |
| `spatialize_grid_vintage.R` | Sizes what changes when the level-0 country grid stops being the 2015 snapshot and becomes year-aware (#1000, T39): support coverage, the national tables with no cell to land in, and optionally the gridded crop output itself. A **measurement behind an open decision**, not a gate. Reads the polycell-support pin (network on a machine with no pins cache) and caches the year-aware grid it builds; the sweep runs it only once that cache exists. See below. |

## Temporary grassland vs FAO 6633 (`temp_grassland_6633.R`)

PR #349 nets modelled CBS 3002 out of the FAO Arable land target, because FAO
already counts temporary grassland inside arable land (#342). FAOSTAT also
measures that quantity directly, as Land Use item **6633 "Temporary meadows and
pastures"**, so the premise is checkable — but only if the comparison respects
where FAO's numbers come from.

**Most of 6633 is not an observation.** Over 2001–2023 the `faostat-landuse`
pin carries 1100 rows flagged `A` (official value) against 4001 flagged `I`
(imputed) — ~19% of the series is reported. 900 of the imputed rows are zeros,
which is FAO's documented convention:

> In case of a missing value replaced by FAO with a 0 because the phenomenon is
> assumed negligible for the considered unit, the flag to use is "I" (imputed)
> and NOT "N – not significant".
>
> — FAO, *Statistical Standard Series: Observation Status Code List, Version 4*,
> endorsed by DCG-T on 10 July 2025, guidance for flag "I".

Greece and Poland are imputed zeros for **every** year 2001–2023 while WHEP
models 2.10 and 4.78 Mha of temporary grassland there. A comparison that scores
those pairs is measuring FAO's gap-filling. So the judged set is official rows
only, and the imputed and caveated classes are printed beside it. The same rule
applies when several FAOSTAT reporting areas collapse into one WHEP polity: the
polity-year counts as official only when every contributing raw row does, which
is FAO's own composition rule for derived figures.

**Measured state, 2001–2023, official rows only (416 country-years, 24
polities):**

| modelled concept | Σ model | Σ FAO 6633 | Σ ratio | median ratio |
|---|---|---|---|---|
| `cbs_3002` (what #349 nets) | 158 Mha | 276 Mha | **0.572** | 0.905 |
| `green_on_3002` (whole green-fodder group, same country-years) | 403 Mha | 276 Mha | **1.459** | 1.48 |

The aggregate shortfall is real and it is against *official* FAO figures —
excluding the imputed rows does not rescue it, it slightly worsens it (pooling
every class gives 0.609). But it is a **scope** difference, not a broken
reconstruction, and the per-country table is what shows that:

- **6633 == CBS 3002 to the digit** for Ireland, Sweden, the United Kingdom,
  the Netherlands, Belgium, Luxembourg and Czechia (ratios 0.997–1.002).
- **6633 is 3–40× CBS 3002** for Romania (0.024), Bulgaria (0.037), Germany
  (0.130), Italy (0.214), Spain (0.265), Austria (0.392) and Denmark (0.555) —
  and for every one of those the whole green-fodder group (CBS 3002 + 2000 +
  2001 + 2002 + 2003) lands at 1.00–1.70 instead.

So FAO books a different set of arable green fodder as "temporary meadows"
depending on the reporting country, and **which concept #349 should net is a
methodological decision, not a bug to be tuned away** (#354).

Two further things the check prints every run:

- **Coverage.** Modelled CBS 3002 exists for 26 EU polities and stops at
  **2019** (FAOSTAT production item 996, its only source, ends there). #349
  therefore nets nothing from 2020 on, and nothing outside the EU.
- **Ireland is a FAO break, not a model error.** From 2007 modelled CBS 3002
  equals 6633 exactly, every year. Before 2007 the model carries 748–786 kha
  against FAO's 89–100 kha — and every FAO value there is flagged `I`,
  back-filled at the post-2007 level. The discontinuity lives in FAOSTAT item
  996, which WHEP passes through.

```bash
Rscript validation/temp_grassland_6633.R              # judge against baseline
Rscript validation/temp_grassland_6633.R --record     # re-record
Rscript validation/temp_grassland_6633.R --refresh    # rebuild the cache
VAL_TG_PERTURB=1.2 Rscript validation/temp_grassland_6633.R   # must FAIL
```

`gt_temp_grassland_6633.json` is a recorded **measurement**, not a tolerance:
the only slack is `1e-6` relative, for build-order floating point. `--record`
after a deliberate change; a concept that was never recorded fails on its first
run by design. `VAL_TG_PERTURB` scales the modelled side without touching the
baseline, which is how the check was shown to fire rather than merely to pass.

The production build (~130 s, ~4.5 GB peak for 2001–2023) is cached under the
gitignored `.whep_cache/`, so only the first run is slow. `validate_all.R` runs
this check only when that cache already exists, or when `VAL_TG_FORCE` is set.

## Admin-unit drift (`admin_drift_tvd.R`)

`run_spatialize()` splits a national total across 0.5-degree cells in proportion
to a weight frozen at circa 2000 — the Monfreda `harvest_fraction` for crops, a
LUH2 land-use proxy for livestock — so the *shape* of the allocation inside a
country is the year-2000 shape in every year of the run. This script measures
what that costs, and is the evidence Phase 3 of the subnational-spatialization
plan (#1000) is asked with.

For one country, indicator and item, with `s_u(t)` = admin unit `u`'s share of
the national quantity in year `t`:

```text
TVD(t, r) = 0.5 * sum_u | s_u(t) - s_u(r) |
```

is the fraction of the national total sitting in a different unit than the
reference-year pattern implies. Measured on **harvested area** (the quantity
the plan's decision 8 binds; a production share would embed a within-unit
yield) and on livestock head counts, area-weighted over items:

| country | 1961 | 1970 | 1980 | 1990 | 2010 | 2020 |
|---|---|---|---|---|---|---|
| Spain | 28.0 | 24.5 | 16.5 | 12.5 | 10.3 | 12.1 |
| Japan | 23.9 | 13.8 | 7.1 | 4.6 | 3.3 | 5.7 |
| USA | 16.6 | 13.9 | 16.0 | 8.5 | 4.8 | – |
| France | – | 19.9 | – | – | – | 9.2 |
| Italy | – | 18.7 | – | – | – | 10.0 |
| Australia | 12.5 | 11.6 | 6.1 | 5.9 | 6.2 | 12.9 |

Per crop it reaches 63.6% (Spain maize 1961) and 60.0% (Japan barley 1961);
French livestock against a 2010 reference reaches 48.7% (goats) and 42.8%
(sheep). Drift is monotone in distance from the reference year and V-shaped
about it. A `–` is a country-year where no item had at least five admin units
common to both years, not a zero: the USA's 2020 field-crop rows in this panel
are almost entirely flagged `duplicate_cell`, and France and Italy are compiled
at 1970 / 2000 / 2020 only.

The score is a **lower bound** on the allocation error — aggregating cells back
to units cannot see misallocation between cells inside one unit.

```bash
WHEP_SUBNATIONAL=<panel.parquet> Rscript validation/admin_drift_tvd.R
WHEP_SUBNATIONAL=... Rscript validation/admin_drift_tvd.R --record  # re-record
WHEP_SUBNATIONAL=... Rscript validation/admin_drift_tvd.R --perturb # must FAIL
```

`--perturb` (or `VAL_ADT_PERTURB=<factor>`) scales every other admin unit inside
each country-indicator-item-year. A *flat* scale would cancel in the shares and
prove nothing, so the tripwire distorts the shape instead; 83 of the 90 recorded
rows move under it.

Two companion outputs feed the allocation-policy decisions and are written to
the gitignored `cache/`, with their per-source summaries recorded in the JSON:

- `cache/admin_drift_discrepancy.csv` — `admin_sum / national_total - 1` per
  (country, item, year). Needs `VAL_ADT_NATIONAL_CSV` pointing at a
  `build_primary_production()` export (`year`, `area_code`, `item_prod_code`,
  `unit`, `value`); the section is skipped with a message when unset.
- `cache/admin_drift_gap_years.csv` — interior missing years per series, with
  run lengths, which is what `fill_proxy_growth()`'s `max_gap` settings are
  chosen against.

The zero-pattern-unit count (unit × crop pairs with reported area > 0 and no
positive Monfreda cell) is **not** produced here: it needs a cell-to-unit
crosswalk that does not exist until the level-1 grid lands.

This **extends** `subnational.workflow.js` / `compare_findings.R` rather than
replacing them. Those score WHEP's *national* value against subnational
statistics summed to a national total; this one never uses a national value as
skill, and reads a different artifact.

## Seam gate (`admin_seam_gate.R`)

The subnational-spatialization plan's decision 6 is that a series must be
continuous across every **seam** — the year a country's admin constraint
starts, and every source, grain, NUTS-version, coverage or indicator switch
after it. `seam_gate()` is the judge, in three tiers documented on the function
itself:

- **A, identity at the anchor.** At each series' `t0` the table still holds the
  observation: every unit's row is observed, the shares sum to 1 within 1e-8,
  each share equals its own reported value's share, and `t0` is the `"start"`
  seam the resolver named. A series with no `"start"` seam to compare `t0`
  with, or with no observed row to anchor on at all, comes back `pass = NA`
  with the reason on its row rather than passed; the METRIC line's
  `tier_a_ungated` counts them, and a series no tier judged holds the gate's
  `overall` verdict at `NA`.
- **B, the governed quantity.** The seam log-ratio
  `|log(s_u(t0) / s_u(t0 - 1))|` of every unit at every seam year, against the
  empirical distribution of that series' **observed** consecutive log-ratios
  pooled over its units, gated pairs held out. The statistic is the fraction of
  gated pairs beyond Q95, per container, judged against
  `0.05 + 2 * sqrt(0.05 * 0.95 / n)` — the upper band of a binomial proportion
  under the null that a seam pair is an ordinary pair.
- **C, cell smoke.** `check_series_jumps()` on each cell's share of the national
  total across `(t0-2, t0-1)`, `(t0-1, t0)` and `(t0, t0+1)`, one pair at a
  time, cells under 100 ha in both years dropped first. The gate is that the
  seam pair's flag rate exceeds its neighbours' mean by at most one percentage
  point. Where the cells carry a `regime` column it must be identical on both
  sides of the seam.

This script is the live leg. It reads **one existing** `run_spatialize()`
output directory and nothing else — no pin, no network, no rebuild:

```bash
WHEP_SPATIALIZE_OUT_DIR=<run dir> Rscript validation/admin_seam_gate.R
WHEP_SPATIALIZE_OUT_DIR=... Rscript validation/admin_seam_gate.R --record
WHEP_SPATIALIZE_OUT_DIR=... Rscript validation/admin_seam_gate.R --perturb
```

`gridded_landuse_crops.parquet` supplies tier C's cells (never the CFT
aggregation, whose rows pool several items) and `admin_coverage.csv` supplies
the seam list, rebuilt with the resolver's own `.admin_seam_list()`. That file
carries no indicator and no NUTS version, so **two of the six seam kinds cannot
be recovered from it**; point `VAL_ASG_SEAMS` at the resolver's own `seams`
table to gate those too. Tiers A and B additionally need the back-cast share
table (`VAL_ASG_SHARES`), which `run_spatialize()` does not write today — with
it unset the script runs tier C alone and says so.

Beyond the seam pairs, the script runs a **window scan**: tier B over every
consecutive pair from `VAL_ASG_WINDOW` years (default 2) before each series'
first seam to one year after it. The window is derived from that series' own
first constrained year, so a country whose statistics start in 1974 and one
that starts in 1850 each get their own; no year is written down anywhere in the
gate or the script.

`--perturb` (or `VAL_ASG_PERTURB=<factor>`) scales every other cell of each
container and crop from the seam year onwards. A *flat* scale would cancel in
the cell share and prove nothing, so the tripwire distorts the within-country
shape at the seam and nowhere else, which is exactly the artefact tier C
exists to catch. The default factor is 3 rather than something smaller because
scaling half the mass by `f` renormalises the shares by about `(f + 1) / 2`,
leaving the untouched half's ratio at about `2 / (f + 1)`, which only clears
`check_series_jumps()`'s low bound of 0.55 once `f > 2.64`. The script aborts
if a perturbed run moves no recorded row: a tripwire that fires on nothing has
proved nothing.

`gt_admin_seam_gate.json` does not exist until someone records a real run —
`--record` writes it, and the plain run then judges every measured row against
it and exits non-zero when one moves.

## Japan depth-1 pilot (`japan_pilot.R`)

The other two subnational legs each look at one slice of the depth chain:
`admin_drift_tvd.R` measures the panel before any run, `admin_seam_gate.R`
judges a run that already exists. This one **is** the run: it drives
`run_spatialize(level = 1, granted_containers = 110L)` over Japan for
1961–2022 and six crops, and it is the first time that path has met real
administrative statistics instead of a fixture.

**The anchor.** Hokkaido, paddy rice (`item_prod_code` 27), year 2000. MAFF
reports 134,900 ha; the 46 prefectures report 1,762,002 ha between them;
FAOSTAT's national total is 1,770,000 ha. Coverage is complete, so no residual
unit is raised and the reported units rescale proportionally —
134,900 × 1,770,000 / 1,762,002 = **135,512.33 ha**. The script pins that
number, recomputes it from the pin and the allocator, and exits non-zero if it
moves by more than 1e-9 relative. It is judged harder than everything else
because it is an arithmetic identity over three integers, not a measurement.

**Three things are approximate, and the script says so in its own header.**

1. *The support is Japan-only.* It carries the 46 prefecture polycells and no
   `area_code` at all, so `read_level_country_grid(level = 0)` returns zero
   rows against it and the allocation layer is the depth grid alone. Every
   layer row is reported as `unit_outside_level0` ragged coverage. The leg
   scopes its inputs to container 110 and the six MAFF items rather than let
   ~200 countries be dropped for having no cell; with no non-Japanese cell in
   the grid, that changes nothing about Japan's answer.
2. *The world pin was unsound and has been repinned* (whep#1010, fixed by
   whep#1012). The defective version `20260827T190201Z-f82a2` carried zero
   inland water and zero ice, which `territory == land + water + ice` cannot
   see because zero satisfies it. **`whep_inputs.csv` now registers
   `20260907T111653Z-e654d`, which carries both layers and is the version to
   use.** Do not repin backwards onto `20260825T102349Z-1a0eb`: it is sound on
   the layers but predates the bucket `area_code` work (whep#907).
   **A pilot support generated before whep#1012 must be regenerated**, and
   this is the one case where the guard's fallback gets the wrong answer for
   the right reason. `read_polycell_support()` defaults to
   `require_layers = TRUE` and aborts (`whep_polycell_absent_layers`) on a
   support whose layers it cannot vouch for. Where the `layers_supplied` stamp
   is present the *label* answers; where it is absent the fallback asks whether
   the column is zero in every row — and **Japan's ice genuinely is**, because
   Japan has no glaciers in the source. So an unstamped pilot support is
   refused for having a correct measurement. Re-running
   `inst/scripts/build_pilot_polycell_support.R` fixes it: it calls
   `build_polycell_support()`, which stamps `layers_supplied` itself.
3. *The alias rows are injected.* `resolve_admin_units()` resolves
   `JPN-HOKKAIDO` under the code system `whep-lab-japan`, and
   `polity_label_aliases` ships none of those rows: they are a whep-polities
   deliverable that has not landed, and without them every unit resolves to
   `NA` and the run aborts with `whep_run_admin_unresolved`. The script builds
   the 46-row identity map and installs it in the package namespace, so the
   production call path — `.admin_resolve_units()`, which passes no `aliases`
   — resolves it exactly as it will once the rows are published. **The alias
   `source` must be the code-system slug `whep-lab-japan`, never the pin's own
   `admin-stats-japan`:** measured, the first resolves 32,095 of 32,095 rows
   and the second 0 of 32,095, because the resolver builds the slug from
   `code_system` and never reads the rows' own `source`. The script asserts
   both numbers so the trap cannot come back silently.

**The pre-scan.** Decision T31(d) refuses a group whose units all report and
whose areas still miss the national total by more than both tolerances, and
`.alloc_refuse_discrepancy()` aborts the whole run on the first one, naming
only the worst. So the leg evaluates the same rule first, over the full stated
scope, prints every refused group, and runs the engine without their national
rows — their admin-share rows stay in the constraint, so the seam gate and the
gap report still see them. Nothing is repaired and no tolerance is widened:
which side of a refused group is wrong is a methodological question the leg
does not answer, and the refused groups are printed, written to
`cache/pilot_japan/prescan.csv`, counted in the METRIC line and recorded per
item in the baseline over the full scope.

**Two diagnostics the driver drops.** `allocate_level_crops()` returns a
`straddle` table and a `bridges` table and `reconcile_admin_allocation()`
returns a third, but `.write_admin_outputs()` writes eleven CSVs and none of
them is any of the three, nor does `.admin_run_record()` carry them — so a
depth run cannot be audited for cell straddling or carried years from its own
output directory. This leg recomputes both. That is a workaround for a defect
in `R/run_spatialize.R`, not a design.

**Tier C is always empty here.** `.admin_seam_gate()` calls `seam_gate()` with
the shares and the seams and no cells, so the cell tier has nothing to judge on
any `run_spatialize()` depth run. Tier C on this run's grid is exactly what
`admin_seam_gate.R` is for: point its `WHEP_SPATIALIZE_OUT_DIR` at
`validation/cache/pilot_japan/run`.

```bash
WHEP_POLYCELL_SUPPORT_PATH=<pilot support.parquet> \
  VAL_JP_INPUT_DIR=<prepared inputs> Rscript validation/japan_pilot.R
... Rscript validation/japan_pilot.R --record    # re-record the baseline
... Rscript validation/japan_pilot.R --refresh   # rebuild the scoped inputs
```

| Variable | Meaning |
|---|---|
| `WHEP_POLYCELL_SUPPORT_PATH` | Required. The Japan-only pilot support, from `inst/scripts/build_pilot_polycell_support.R`. A support with no `JPN-*` prefecture polycell — which is what the registered world pin is — skips the leg. |
| `VAL_JP_INPUT_DIR` | Required. A directory of prepared spatialization parquets, as `run_spatialize(paths = list(input_dir = ))` takes them. Unset skips the leg. |
| `VAL_JP_YEARS` | Optional. Comma-separated years. Default 1961–2022, the span the MAFF family covers. |
| `VAL_JP_ITEMS` | Optional. Comma-separated `item_prod_code`s. Default 15, 27, 44, 116, 122, 236 — the six the family ships. |

## Level-0 grid vintage (`spatialize_grid_vintage.R`)

`run_spatialize()` allocates a national total into a **country grid**: one row
per 0.5° cell and reporting `area_code`, carrying that unit's share of the
cell's land. Until #1000 T39 that grid was always the polycell support read at
one reference year (`.carbon_support_year()` = 2015), so every year of a run —
1851 included — was allocated into the present-day cell-to-country map. T31(j)
made the grid year-aware and `read_level_country_grid(grid_vintage = )` now
selects between the two. They are two different geographies, not two
precisions of one, and this script sizes the difference so the choice between
them is made on numbers.

It measures three things, in increasing distance from the support and
increasing cost:

- **A, the support itself.** Per year: how many cells and reporting areas each
  vintage carries, how much land, and — weighting each cell by the LUH2
  cropland and pasture it holds that year — how much of that land each country
  **loses** and **gains** between the vintages, and how much falls in a cell no
  polity claims that year and is therefore attributed to nobody. Both
  directions are reported: a one-sided metric cannot see a defect whose
  signature is absorption.
- **B, the national tables that have to land in it.** Per year: which reporting
  areas of `country_areas` / `livestock_country_data` have no cell at all under
  each vintage, and the harvested area and head count they carry.
  `.warn_grid_missing_reporters()` warns about exactly this set at run time,
  and the whole national total is dropped from the gridded output.
- **C, the gridded output.** For a small year set, `build_gridded_landuse()`
  runs twice on identical inputs differing only in the grid, and the outputs
  are differenced per cell. Crops only, and **off by default** because it is
  minutes per year.

A and B are the honest bound on C: nothing the engine does can put a national
total into a cell the grid does not offer it.

It does **not** reach the SOC or nitrogen chains, which read the same polycell
support through `.carbon_cell_support()` rather than through
`read_level_country_grid()`, so `grid_vintage` does not reach them either
(whep#1002).

```bash
Rscript validation/spatialize_grid_vintage.R
VAL_GV_ENGINE_YEARS=1961,2015 Rscript validation/spatialize_grid_vintage.R
VAL_GV_REBUILD=1 Rscript validation/spatialize_grid_vintage.R
```

| Variable | Meaning |
|---|---|
| `WHEP_POLYCELL_SUPPORT_PATH` | Optional. A local support parquet overriding the pin, as `read_polycell_support()` reads it. |
| `VAL_GV_YEARS` | Optional. Comma-separated years for A and B. Default: every decade 1851–2021 plus 1961 and 2015. |
| `VAL_GV_ENGINE_YEARS` | Optional. Comma-separated years for C. **Unset (the default) skips C.** |
| `VAL_GV_ENGINE_ITEMS` | Optional. Comma-separated `item_prod_code`s for C. Default: six large-area crops. |
| `VAL_GV_REBUILD` | Optional. `"1"` forces the year-aware grid to be rebuilt instead of read from its cache. |

The pinned inputs (`polycell-support`, `spatialize-gridded-cropland`,
`spatialize-gridded-pasture`, `spatialize-country-areas`,
`spatialize-livestock-country-data`, and for C `spatialize-crop-patterns`) are
read through `whep_read_file()`, so a machine with no pins cache needs the
network. The script exits 0 with `METRIC status=skipped` when the support
cannot be resolved, and says which input failed.

Outputs land under `validation/cache/`, which is gitignored in full:

| File | Contents |
|---|---|
| `grid_vintage_support.csv` | A, one row per year. |
| `grid_vintage_reporters.csv` | B, one row per year and national table. |
| `grid_vintage_missing_areas.csv` | B2, the ten largest losers per year. |
| `grid_vintage_engine.csv` | C, one row per year and component (only when C runs). |
| `grid_vintage_year_aware.rds` | The year-aware grid itself, which costs minutes to build (152 epoch denominators over ~484k rows). Rebuilt on `VAL_GV_REBUILD=1`. |

`validate_all.R` parses the script's `METRIC` line into a `grid_vintage` row.
That row leaves `ok` and `flag` empty on purpose: which vintage is right is the
open question the measurement exists to inform, so there is no pass criterion
to score, and inventing a threshold would be a methodological choice hidden in
a scorecard. Because building the year-aware grid costs minutes and reads a
pin — and the sweep does not start a build without being asked — the row reads
`not run` until `grid_vintage_year_aware.rds` exists, i.e. until the script has
been run once by hand.

## Year-scoping equivalence (`year_scoping.R`)

A `years =` window is a request for a **subset**, not for a different method, so
the invariant is arithmetic:

```
build_x(years = Y)  ==  build_x(full range) |> filter(year in Y)
```

`tests/` cannot check it — the full builds are ~170 s / 14 GB (primary
production) and ~250 s / 23 GB (wide CBS) and read pins. That is precisely how
three violations shipped green on CI: **#623** (`.fill_fodder_gaps()`
interpolates along the year axis, so a narrow window lost *every* forage crop —
1.16% of production tonnes, 1.85% of `t_ha`, 1.36% of wide-CBS `feed`, while
PR #570 was 10/10 green), the **#625** residual underneath it (since split into
#665/#666/#667), and the trade/stock cross-year dependency that #570 papered
over with the `.context_years()` margin.

```bash
Rscript validation/year_scoping.R                       # production, 2010
Rscript validation/year_scoping.R wide_cbs 2010
Rscript validation/year_scoping.R production 2010 --record   # re-record
Rscript validation/year_scoping.R production 2010 --refresh  # rebuild the cache
```

One layer per process, so the session build cache never holds the full
production *and* the full CBS at once. Only the **full** build is cached (under
the gitignored `.whep_cache/`); the scoped build is redone every run, so a stale
cache cannot mask a regression. Pass `--refresh` when WHEP code or its input
pins change what the full build reads.

Comparison is **per unit** (production) and **per quantity column** (wide CBS,
melted so one comparator serves both). An aggregate over everything would have
hidden #625 behind #623. Three things are compared at that grain: keys present
in only one build (either direction), the total, and the worst per-key value
difference among keys both builds have — #625's second half was 20 *shared* rows
whose values differed while the row sets agreed.

**The tolerance is not a measurement tolerance.** The target is exact equality;
two slacks are allowed and both are explicit:

- **Floating point** — the builds sum the same numbers in a different order.
  `1e-9` relative, and nothing larger.
- **Known, filed defects** — `gt_year_scoping.json` records the measured state
  per layer, **per year** and per unit. Exceeding it plus the floor fails. So
  the check is a regression net today and a **ratchet**: fix a filed defect,
  re-record, and the tighter number becomes the ceiling. The script says so
  itself when a unit comes in well inside its budget. A recorded number is a
  budget for a defect with an issue number, never a claim that the difference
  is fine.

The budget is keyed on the **year** because the residual is not year-invariant:
#666 measured its own cluster at 2.18e-04 at 2010 and 3.06e-03 at 1995, ~14x
worse. Sharing one budget across years would let a recording at 1995 raise the
ceiling a later 2010 run is judged against. A year that has never been recorded
is therefore required to be exact, and fails loudly the first time — record it
deliberately. 2010 is what ships recorded, and it is the *favourable* year, so
`VAL_SCOPING_YEAR=1995` is the more searching run.

`validate_all.R` reports both layers, but only *runs* a layer whose cached full
build already exists — the sweep deliberately does not start a multi-minute
build unasked. Force one with `VAL_SCOPING_LAYERS=production,wide_cbs`.

## Resident-memory floor (`memory_floor.R`)

A finished production/CBS build leaves the process far larger than its live
data, and `gc()` does not bring it back — #777 measured 20.6 GB resident for
1.42 GB live at 1901-2022 and read that as a leak. It is not one thing, and the
three things it is have three different remedies:

| component | what it is | how to see it | how to get rid of it |
|---|---|---|---|
| live | reachable objects: the build cache, the returned tables | `gc()`'s `used` column | `whep_clear_cache()`, or hold fewer tables |
| glibc arena | blocks `free()`d but not returned to the OS | `mallinfo2()$fordblks` | `malloc_trim(0)` — 0.25 s here, no live object touched |
| foreign allocator | pages held by an allocator that is not glibc; `arrow`'s default pool is **mimalloc** and keeps its segments after `bytes_allocated` returns to 0 | the pool's `max_memory` while `bytes_allocated` is 0 | `ARROW_DEFAULT_MEMORY_POOL=system` |

Only the first is WHEP's. Reporting the three as one number is what makes an
allocator floor look like a retention bug, which is the whole reason this
script exists.

`Rscript validation/memory_floor.R` re-derives the split at a configurable
window (`VAL_MEM_YEAR_MIN`/`VAL_MEM_YEAR_MAX`, default `2005`-`2015`;
`VAL_MEM_TRIM=0` skips the trim). It prints a `METRIC` line plus
`CHECKPOINTS_JSON`, and builds the two-function `malloc_trim`/`mallinfo2` shim
itself — with no compiler, or off glibc, those columns come back `NA` and the
live-vs-resident pair still works.

The script's own output at the default window, for reference:

```
03 after 3x gc(full)         resident   6.79 peak   9.05 live   1.46
04 after malloc_trim(0)      resident   3.06 peak   9.05 live   1.46
06 after malloc_trim(0)      resident   1.94 peak   9.05 live   0.33
METRIC years=2005-2015 build_seconds=226 peak_gb=9.05 live_gb=1.46
  floor_gb=5.34 reclaimable_gb=3.74 trim_seconds=0.250 arrow_max_gb=1.49
  cache_gb=0.83 cache_slots=4 shim=TRUE
```

Three independent runs of the shipped configuration put `resident` after `gc`
at 6.33, 6.79 and 7.10 GB and after the trim at 3.01, 3.06 and 3.08 GB, so the
reclaimable share is not a one-off.

The four rows below are one run each, at `2005-2015`,
`get_primary_production()` then `get_wide_cbs()`:

| configuration | build | peak | resident after `gc` | after `malloc_trim(0)` |
|---|---|---|---|---|
| as shipped | 241.9 s | 9.41 GB | 7.10 GB (live 1.45) | 3.01 GB |
| `ARROW_DEFAULT_MEMORY_POOL=system` | 253.4 s | 8.79 GB | 7.98 GB | 2.50 GB |
| `MALLOC_MMAP_THRESHOLD_=MALLOC_TRIM_THRESHOLD_=131072` | 356.3 s | 7.76 GB | 3.91 GB | 2.91 GB |
| both | 301.2 s | 6.12 GB | 2.90 GB | 1.72 GB |

The machine was running nine other agents throughout, so read the time column
as indicative (the shipped configuration came in at 196.9, 226 and 241.9 s
across three runs) and the memory columns as solid — those deltas are far
larger than the spread.

Three things follow that are easy to get backwards:

- Pinning glibc's thresholds lowers the **peak**, because large blocks go to
  `mmap` and come back on `free`. It pays for that in mmap churn.
- A trim lowers the **resting** size for nothing, and does not touch the peak.
- Putting arrow on the system allocator does not shrink the floor — it makes it
  *reclaimable*, by moving the residue out of mimalloc and into the arena a
  trim can return. Hence the higher `after gc` and lower `after trim`.

Whether the floor is a floor under the **next** phase depends on the size class
that phase allocates, and this is where #777 overreaches. Standing in a 4 GB
phase 2 after the chain (resident 7.27 GB, 4.46 GB free inside the arena):
256 x 16 MB blocks grow the process by only 1.22 GB, because 2.79 GB comes out
of those free blocks; 8 x 0.5 GB blocks grow it by the full 3.50 GB, because
glibc serves anything that large with a fresh `mmap` and cannot reuse the arena
at all. So a tabular consumer mostly does not pay the floor, and a gridded one
pays all of it. Trimming between the two phases held the overall peak at the
chain's own 8.92 GB instead of letting phase 2 push it to 10.77 GB.

The `MALLOC_*` variables have to be set in the shell that launches R: glibc
reads them before `.Renviron` is ever parsed, so putting them there does
nothing.

`ARROW_DEFAULT_MEMORY_POOL` is the exception — arrow reads it when its C++
library initialises, so `~/.Renviron` does work for that one.

## Validating an LPJmL run (different target)

Everything above validates WHEP's *own* datasets against independent statistics.
The two `lpjml_*.R` scripts validate something else — a finished **LPJmL model
run** — so they stand apart: no `autoresearch/` ceremony, no subagents, no
network. Both are standalone `Rscript`s and both are deliberately
self-contained, including reading `cft_mapping` from its CSV rather than from
the installed package: a check that imports the package it is checking is not
an independent check, and these have to run without `whep` on the library path.

```bash
# Crops: harvested area, yield level, implied carbon content, spatial pattern,
# and a run-vs-run diff. Caches the NetCDF read under cache/lpjml_cfts/.
Rscript validation/lpjml_faostat_crops.R <run_dir> [baseline_run_dir] 2000,2005,2010

# Global fluxes: spinup equilibration per pool, transient fluxes vs
# observations, and a run-vs-run diff. Reads one CSV; seconds to run.
Rscript validation/lpjml_globalflux.R <run_dir> [baseline_run_dir] 2000,2010
```

`<run_dir>` is a run's `output/scenario_1`.

### Guarding the LPJmL-derived pins

Separate from the two above, which check a *run*. This one checks the four
**pins** WHEP actually feeds on:

```bash
Rscript validation/lpjml_pins.R            # check against the recorded baseline
Rscript validation/lpjml_pins.R --record   # rewrite the baseline, deliberately
```

It exists because repointing those pins from LPJmL 5.9.7 to 6.1.1 raised
natural-land carbon input ~31% — moving every downstream SOC number — and
`validate_all.R` ran clean straight through it. It had to: every variable that
sweep covers reads FAOSTAT/GAEZ/MapSPAM/PSD and none reads an LPJmL pin (#559).

Three tiers, and they mean different things when they fail:

- **Contract** — required columns, row count, year span. A mismatch means the pin
  is not the layer its consumers expect.
- **Invariant** — physical impossibility, not expectation: a fractional
  saturation outside `[0, 1]`, a negative carbon density, a monthly rainfall
  above any observed value. These hold for any model version, so they never need
  updating, and a violation is corruption rather than a model difference.
- **Baseline** — recorded magnitudes in `gt_lpjml_pins.json`. **This tier is
  designed to fail when the pins change.** The tolerance is `1e-5`, not a few
  percent, because these are deterministic model outputs rather than
  measurements — at 2% the real 5.9.7→6.1.1 hydrology shift (1.7%) slipped
  through while the larger carbon shifts were caught, which is the silent pass
  the script exists to prevent.

So a baseline failure is a prompt, not a verdict: find out what moved, then
re-record with `--record` and say in the commit message why.

### Climate-forcing pins — `lpjml_forcing_pins.R`

```
Rscript validation/lpjml_forcing_pins.R            # check
Rscript validation/lpjml_forcing_pins.R --record   # rewrite the baseline
```

`lpjml_pins.R` above guards the four pins carrying LPJmL *output* and
deliberately excludes the *forcing* pins, on the reasoning that forcing does not
change with the model version. That is right for its magnitude tier and wrong
for its invariant tier: forcing can still be **corrupt**. #824 is the proof —
`lpjml-rsds-era5-2017-2023` ships 1,823,843 negative shortwave values because
#536 fixed the script that builds it and nobody rebuilt the artifact. Nothing in
the repo could see it: this script excluded the pin, and
`test_data_raw_freshness.R` gates `data/*.rda` against `data-raw/`, not a pin
against its generating script.

Six pins, three variables, and the bounds are impossibility limits: a
downwelling radiative flux and a wind speed cannot be negative, and the ceilings
(1500 W/m² for `rsds`, 1000 for `rlds`, 100 m/s for `wind`) sit far above the
observed maxima of 468, 485 and 21.2. **The floor is inclusive**:
`lpjml-rsds-isimip-1901-2019` has a minimum of exactly 0 — night — so a
positivity test would fail on a clean pin, and a clamp must land on 0 rather
than nudge to epsilon.

The recorded-state tier is what makes this usable while #824 is open: the
violation is *recorded* rather than suppressed, so the script is green on the
known-bad state, reports it as `KNOWN` on every run, and is loud about anything
new. The comparison is **bidirectional** — a count that rises is a new
corruption, and a count that falls means somebody rebuilt the pin, which is
exactly the event #824 exists because nobody noticed. Both stop the check and
demand a re-record, so the fix gets written down instead of quietly changing
what consumers receive.

**Still not covered:** how a pin change propagates into `build_carbon_balance()`
output. That needs local raster paths which are unset in a fresh checkout —
`WHEP_HWSD_DIR`, `WHEP_CRU_DIR`, `WHEP_LUH2_DIR`, `WHEP_POLITY_FRACTION_PATH`,
`WHEP_TYPE_CROPLAND_PATH`, `WHEP_GRIDDED_PASTURE_PATH`,
`WHEP_CROP_PATTERNS_PATH` — and the repo's tracked `.Renviron` shadows
`~/.Renviron` (#456), so they cannot come from a home file either. Tracked in
#559.

**What these can and cannot establish.** LPJmL only matches FAO yields once its
maximum LAI has been calibrated per country ([Fader et al.
2010](https://doi.org/10.1016/j.jhydrol.2010.04.011)), so an uncalibrated run
sits *above* FAO and an absolute comparison cannot validate a model change. The
checks are therefore ordered by how few assumptions each needs, and the ones
that can carry a verdict are constant-free: the Spearman correlation of
national production (invariant to any per-CFT factor) and the ratio between two
runs (which cancels the conversion and the calibration state alike). Harvested
area is the one hard absolute check — `cftfrac` × cell area *is* FAOSTAT "Area
harvested", same unit, no conversion.

Global GPP is genuinely contested, so `lpjml_globalflux.R` reports which family
of estimates a run agrees with rather than pretending there is one number:
satellite-optical products give 120–140 PgC/yr, while carbonyl-sulfide ([Lai et
al. 2024](https://doi.org/10.1038/s41586-024-08050-3), 157 ± 8.5) and ¹⁸O
(150–175) are higher, with the gap concentrated in tropical rainforest.

The equilibration table includes the two **mineral** nitrogen pools (`SoilNO3`,
`SoilNH4`) because on the 6.1.1 production run they are the only pools that
fail: every organic pool drifts 0.02–0.06 %/yr while nitrate drifts −0.146 %/yr
after 300 spinup years (#437). Runs whose output list omits them are unaffected
— absent columns are skipped. The `nlosses/ninflux` ratio below the table stays
a printed number with no verdict on purpose: it reads 2.011 on 6.1.1 and 1.094
on 5.9.7 at the end of their spinups, and no threshold between those two can be
justified without deciding how much imbalance is acceptable. It is also reported
for the **spinup only**, because over the transient the same run reads 1.103
(2004–2023) while its nitrate pool is still draining — rising deposition and
fertiliser nearly triple `ninflux`, so a transient ratio near 1 is not evidence
of a balanced nitrogen cycle.

## Local bulk datasets (USA / NASS)

Countries with a local bulk dataset are extracted **deterministically**, not by a
web agent. USA uses the USDA NASS QuickStats bulk export: `nass_sum.R` does one
`grep` pass over the multi-GB `crops.csv` (cached to a compact slice), sums
state-level production per crop-year, and converts to tonnes (rice ×0.67 milled).
The workflow's `LOCAL` map routes USA to this helper instead of web search.

The NASS data is **not committed**: its path lives in the gitignored
`cache/local_paths.json` (or the `WHEP_NASS_DIR` env var), and the slice + the
USA findings are gitignored. To enable on a new machine, point
`cache/local_paths.json` (`{"WHEP_NASS_DIR": "…/NASS"}`) at the folder holding
`crops.csv`.

## Variables & check archetypes

Validation generalizes beyond production to many **variables** (`variables.R`),
each tagged with an **archetype** that decides how it's checked:

| variable | archetype | grain | WHEP extractor | ground truth |
|---|---|---|---|---|
| production | external | country·crop·year | production tibble | USDA NASS (local) / EUROSTAT / national / FAOSTAT |
| area | external | country·crop·year | production (unit ha) | same |
| occupation | external | country·crop·year | `build_hayr_land_extension` ÷ production (ha·yr/t, **active** basis) | Poore & Nemecek 2018 (pinned, `gt_occupation.json`) |
| land_per_tonne | external | country·crop·year | CROPGRIDS physical ÷ production (ha·yr/t, **full-year** basis) | same — like-for-like with LCA m²·yr/kg |
| cropping_intensity | **bound** | country·crop | CROPGRIDS physical ÷ harvested | GAEZ v4 potential ceiling (auto-download, `gaez_potential.R`) |
| cycle_length | parameter | crop | `mirca_season.csv` (months) | FAO calendars / GGCMI (not pinned yet) |
| stability | internal | time series | WHEP series | none (self-consistency) |
| year_scoping | internal | unit / quantity column | scoped build vs full-range build | none (arithmetic identity) |
| memory_floor | internal | process | resident vs live after the production/CBS chain | none (allocator accounting) |

A third comparator, **bound** (one-sided), is for ceilings like GAEZ *potential*:
pass if WHEP's observed value stays at/below it. Occupation is split into two
variables on purpose: WHEP's `build_hayr` is **active** occupation (growing
season + fallow), which reads systematically *below* LCA's **full-year**
convention; `land_per_tonne` (physical land held all year) is the like-for-like
LCA comparison and lands inside the published range more often.

Run the whole sweep with `Rscript validation/validate_all.R` (one scorecard
across every variable). Per-variable: `validation/stability.R`,
`validation/compare_variable.R <var> <gt.json> [years] [bound]`,
`validation/gaez_potential.R` (rebuild the GAEZ ceiling).

- **external** — WHEP value vs an authoritative figure (ratio within tolerance).
- **parameter** — a coefficient/weight WHEP *uses* vs an authoritative coefficient.
- **internal** — WHEP's own consistency, no external source (`stability.R`,
  `year_scoping.R`, `memory_floor.R`).

All extractors run from packaged data + public pins (no LPJmL). The deterministic
comparators are proven; ground truth is now mostly automated — GAEZ (the
cropping-intensity ceiling) downloads from the FAO bucket, occupation is pinned
(Poore & Nemecek). The pieces still sourced manually are GGCMI (cycle length,
Zenodo) and the USDA NASS bulk (USA production, local path).

## Inter-agent communication

Agents are isolated (no shared memory); everything flows through the
orchestrator (JS variables) and through files. Two rules keep numeric data
trustworthy:

- **Structured returns, not prose.** Every data-bearing `agent()` call sets a
  `schema:`, so the runtime forces a validated JSON object back (numbers are
  typed, not parsed from text). Prose only appears in transcripts and in
  free-text fields like `notes`/`basis` (kept for definitional caveats).
- **Bulk data goes through files, not prompts.** Discover agents *write* their
  findings to `cache/findings/<iso3>.json` and return only a summary; the
  compare step's R reads those files and validates them at the point of use
  (`compare_findings.R::read_findings`). Nothing re-serializes a large array
  through an LLM prompt.

## Caching (cache-first discovery)

Discovery is the expensive part — finding which subnational dataset exists, then
downloading and parsing it. Both are cached so a re-run does neither again:

- **`cache/sources.json`** caches the *discovery*: which dataset + URL covers
  which crops/years for each country. A discover agent reads it first and skips
  the web search entirely when a registered source covers what it needs.
- **`cache/files/<iso3>/`** caches the *download*: the raw file. The agent parses
  it locally; it only re-downloads (from the registry URL) if the file is gone.

A discover agent only falls back to the web when no registered source covers a
crop/year, or when it deliberately wants an alternative/cross-check. New sources
it finds are saved to `cache/files/` and merged into `sources.json` (by a single
writer, after the parallel phase, to avoid races), so the registry grows every
run.

## Run

```
Workflow({ scriptPath: "validation/subnational.workflow.js",
           args: { n_countries: 5, crops: ["Wheat","Rice","Maize (corn)"],
                   year_min: 1970, year_max: 2010, rank_year: 2010,
                   model: "sonnet" } })
```

## Models / token cost

Subagents default to **Sonnet** at **medium** effort (`args.model` /
`args.effort`) to keep token cost down — the discovery fan-out is where the
tokens go. Setting effort explicitly also stops agents from inheriting the
session effort (which may be `xhigh`). Pass `args: { model: "opus" }` to run
everything on Opus, `"haiku"` for the cheapest, or `args: { effort: "high" }` to
think harder. (Note: `args` may arrive as a JSON string; the workflow parses it,
so the knobs bind either way.)

The deterministic halves run on their own too:

```bash
VAL_N_COUNTRIES=5 Rscript validation/rank_countries.R
Rscript validation/compare_findings.R <findings.json>
```

## How it works

1. **Rank** — `rank_countries.R` ranks the top-N producing countries (real ISO3
   only; FAO aggregates excluded) by total tonnes of the crops in `rank_year`.
2. **Discover** — one subagent per country searches for a subnational source
   (USDA NASS by state, India DES state-wise, China NBS provincial, EUROSTAT
   NUTS2, …), **fetches** values, sums units to a national total per crop-year it
   finds, and records `n_subnational_units`, basis, and the URL. Fetch, never
   recall; if no source exists it returns nothing for that country.
3. **Compare** — `compare_findings.R` extracts WHEP's national value for each
   `(country, crop, year)` and judges the ratio. Units are canonicalized
   (Mt↔tonnes); rice is compared on WHEP's milled-equivalent basis.

Verdicts: `pass`, `flag_high`/`flag_low`, `missing_whep`, `unknown_item`/`unknown_area`.

## Notes

- WHEP values come from `build_primary_production()` (public pins via
  `whep_read_file()`), cached in `.whep_cache/` keyed by year range.
- WHEP is national-only, so the comparison is **sum-of-subnational vs WHEP
  national**. The subnational breakdown is the evidence; the national total is
  what gets compared.
- `validate.R` is the shared deterministic core for this harness
  (resolve names→codes, extract WHEP values, judge with unit canonicalization).
