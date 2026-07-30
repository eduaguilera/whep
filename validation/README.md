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
| `compare_variable.R` | Variable-aware comparator: extracts WHEP's value, joins ground truth on the registry grain, judges (ratio or bound mode). |
| `validate_all.R` | One-shot sweep: runs every variable's check and prints a combined scorecard. |
| `gaez_potential.R` | Builds the per-country GAEZ potential cropping-intensity ceiling. **Downloads** GAEZ v4 multiple-cropping zones (rainfed `mcr` + irrigated `mci`) from the open FAO bucket → `cache/files/GAEZ/`. No local copy needed (`WHEP_GAEZ_DIR` overrides). |
| `gt_occupation.json` | Pinned LCA occupation ground truth (Poore & Nemecek 2018), m²·yr/kg per crop. Committed. |
| `cache/sources.json` | Registry of subnational datasets discovered so far (per country: source, URL, what it covers, basis). **Committed** — shared knowledge of where the data lives. |
| `cache/files/<iso3>/` | The raw downloaded datasets (PDFs, xls, csv, API JSON). **Gitignored** (can be large); re-downloaded from the registry URL if missing. |
| `cache/findings/<iso3>.json` | Each discover agent writes its extracted, converted national totals here. The compare step reads these files directly — **bulk data never travels through a prompt**. Doubles as a findings cache. Gitignored. |
| `nass_sum.R` | Deterministic USA extractor: sums STATE rows from the local USDA NASS bulk CSV → national tonnes, writes `cache/findings/USA.json`. No web, no PDF. |

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
- **internal** — WHEP's own consistency, no external source (`stability.R`).

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
