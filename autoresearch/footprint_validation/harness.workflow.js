export const meta = {
  name: 'footprint-validation',
  description: 'Validate WHEP production against external ground truth: detect -> research gaps -> diagnose flags -> propose guarded fixes',
  whenToUse: 'Run the point-level validation loop over the deterministic case grid (autoresearch/footprint_validation).',
  phases: [
    { title: 'Detect', detail: 'run benchmark.R, parse flagged + uncovered cases' },
    { title: 'Research', detail: 'one web subagent per uncovered case -> pin ground truth' },
    { title: 'Re-detect', detail: 're-run benchmark.R with the grown corpus' },
    { title: 'Diagnose', detail: 'classify each flag: real bug vs definitional vs gt-error' },
    { title: 'Fix', detail: 'guarded, propose-only fix per real bug (regression-checked)' },
  ],
}

// Args: { research_cap?: number, do_fix?: boolean, model?: string }
// args can arrive as a JSON string rather than a parsed object - normalise.
const ARGS = typeof args === 'string' ? JSON.parse(args || '{}') : (args || {})
const RESEARCH_CAP = ARGS.research_cap || 6
const DO_FIX = !!ARGS.do_fix
// Discovery/diagnosis default to Sonnet to save tokens; the opt-in fix agent
// edits scientific code, so it defaults to Opus for more headroom (overridable).
// Effort defaults to medium so agents don't inherit the (possibly xhigh) session
// effort; the rare, high-stakes fix agent runs higher.
const MODEL = ARGS.model || 'sonnet'
const FIX_MODEL = ARGS.fix_model || 'opus'
const EFFORT = ARGS.effort || 'medium'
const FIX_EFFORT = ARGS.fix_effort || 'high'
const DIR = 'autoresearch/footprint_validation'
const BENCH = `Rscript ${DIR}/benchmark.R`

// ---- schemas ----------------------------------------------------------------

const BENCH_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['metric_line', 'flagged', 'uncovered'],
  properties: {
    metric_line: { type: 'string', description: 'the full METRIC ... line verbatim' },
    flagged: { type: 'array', items: { type: 'object', additionalProperties: true } },
    uncovered: { type: 'array', items: { type: 'object', additionalProperties: true } },
  },
}

const GT_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['probe_id', 'found', 'gt_value', 'gt_unit', 'source', 'url', 'definition', 'tolerance_pct', 'confidence', 'independent_of_faostat', 'notes'],
  properties: {
    probe_id: { type: 'string' },
    found: { type: 'boolean' },
    gt_value: { type: ['number', 'null'] },
    gt_unit: { type: 'string' },
    source: { type: 'string' },
    url: { type: 'string' },
    definition: { type: 'string' },
    tolerance_pct: { type: 'number' },
    confidence: { type: 'string', enum: ['high', 'medium', 'low', 'none'] },
    independent_of_faostat: { type: 'boolean' },
    notes: { type: 'string' },
  },
}

const DIAGNOSE_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['probe_id', 'classification', 'is_real_bug', 'root_cause', 'evidence', 'proposed_fix', 'fix_scope', 'confidence'],
  properties: {
    probe_id: { type: 'string' },
    classification: { type: 'string', enum: ['real_bug', 'definitional_mismatch', 'ground_truth_error', 'known_convention', 'needs_human'] },
    is_real_bug: { type: 'boolean' },
    root_cause: { type: 'string' },
    evidence: { type: 'string', description: 'cite the R/ file:line or source that supports the classification' },
    proposed_fix: { type: 'string', description: 'for real_bug: the concrete code change. for definitional/gt: how to correct the corpus/probe instead.' },
    fix_scope: { type: 'string', enum: ['R_code', 'corpus_or_probe', 'none'] },
    confidence: { type: 'string', enum: ['high', 'medium', 'low'] },
  },
}

const FIX_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['probe_id', 'attempted', 'accepted', 'metric_before', 'metric_after', 'regressions', 'diff_summary', 'notes'],
  properties: {
    probe_id: { type: 'string' },
    attempted: { type: 'boolean' },
    accepted: { type: 'boolean', description: 'true only if the target probe now passes AND no other probe regressed AND devtools::test() passes' },
    metric_before: { type: 'string' },
    metric_after: { type: 'string' },
    regressions: { type: 'array', items: { type: 'string' }, description: 'probe_ids that newly flag/fail after the change' },
    diff_summary: { type: 'string' },
    notes: { type: 'string' },
  },
}

// ---- helpers ----------------------------------------------------------------

const runBenchmark = (label) => agent(
  `Run \`${BENCH}\` from the repo root. It prints a METRIC line and two JSON blocks delimited by FLAGGED_JSON_START/END and UNCOVERED_JSON_START/END. Return the verbatim METRIC line, the parsed FLAGGED array, and the parsed UNCOVERED array. Do not edit anything.`,
  { label, phase: label.startsWith('re') ? 'Re-detect' : 'Detect', schema: BENCH_SCHEMA, model: MODEL, effort: EFFORT }
)

// ---- Detect -----------------------------------------------------------------

phase('Detect')
const before = await runBenchmark('detect')
log(`baseline: ${before.metric_line} | flagged=${before.flagged.length} uncovered=${before.uncovered.length}`)

// ---- Research (cap to control cost; log what is deferred) -------------------

phase('Research')
const toResearch = before.uncovered.slice(0, RESEARCH_CAP)
if (before.uncovered.length > RESEARCH_CAP) {
  log(`researching ${RESEARCH_CAP} of ${before.uncovered.length} uncovered cases (cap); ${before.uncovered.length - RESEARCH_CAP} deferred`)
}

const researched = (await parallel(toResearch.map(c => () =>
  agent(
    `Resolve ONE validation probe into a ground-truth row by FETCHING a value from a trusted source. First load web tools: ToolSearch query "select:WebSearch,WebFetch", then use them.

PROBE: ${JSON.stringify(c)}
- layer "production" => total ${c.item_name} production in ${c.unit}; "area" with unit ha => harvested area.
- NOTE on rice: WHEP reports rice MILLED-equivalent (paddy x ~0.67). If the item is rice, pin the ground truth on the MILLED basis (or note the basis explicitly), else it will spuriously flag.

RULES:
- NEVER recall a number; fetch it and give the exact URL. If not found: found=false, confidence="none", gt_value=null.
- Prefer a source INDEPENDENT of FAOSTAT (USDA NASS for US, EUROSTAT/national stats for EU); set independent_of_faostat.
- Record the source's DEFINITION (basis, calendar vs marketing year). Convert to the probe unit; show original + factor in notes.
- Set tolerance_pct by source/era (5 modern primary; 15-30 pre-1961/sparse).`,
    { label: `research:${c.probe_id}`, phase: 'Research', schema: GT_SCHEMA, model: MODEL, effort: EFFORT }
  )
))).filter(Boolean)

const pins = researched.filter(r => r.found && r.confidence !== 'none' && r.gt_value != null)
log(`researched ${researched.length}, pinning ${pins.length} with a usable value`)

if (pins.length > 0) {
  await agent(
    `Append these researched rows to ${DIR}/ground_truth.csv WITHOUT overwriting existing rows. Steps:
1. Read the CSV header: probe_id,gt_value,gt_unit,source,url,definition,tolerance_pct,confidence,retrieved_on,researcher,notes
2. For each row below, append a line (quote any field containing a comma). Set retrieved_on to today, researcher to "footprint-validation workflow".
3. Keep one row per probe_id (newest wins). Do not touch R/ or any other file.

ROWS: ${JSON.stringify(pins)}`,
    { label: 'persist-corpus', phase: 'Research', model: MODEL, effort: EFFORT }
  )
}

// ---- Re-detect with grown corpus -------------------------------------------

phase('Re-detect')
const after = pins.length > 0 ? await runBenchmark('re-detect') : before
log(`after research: ${after.metric_line} | flagged=${after.flagged.length} uncovered=${after.uncovered.length}`)

// ---- Diagnose every flag ----------------------------------------------------

phase('Diagnose')
const diagnoses = (await parallel(after.flagged.map(f => () =>
  agent(
    `Diagnose ONE flagged validation case: WHEP disagrees with pinned ground truth. Decide WHY before anyone touches code. You may read R/ source and the web.

FLAG: ${JSON.stringify(f)}

Classify into exactly one:
- real_bug: WHEP code computes the wrong number (unit error, mis-mapping, bad gap-fill). fix_scope = R_code.
- definitional_mismatch: both numbers are "right" but on different bases (e.g. rice paddy vs milled - WHEP applies .fix_rice_milled_equiv x0.67 in R/build_production.R; carcass vs live weight; area harvested vs planted; FAO country aggregate vs mainland). fix_scope = corpus_or_probe.
- ground_truth_error: the pinned value is wrong/misread. fix_scope = corpus_or_probe.
- known_convention: an intentional, documented WHEP choice that legitimately differs. fix_scope = none.
- needs_human: genuinely ambiguous.

Cite file:line or a source URL in evidence. Do NOT edit anything. A discrepancy is NOT assumed to be a bug.`,
    { label: `diagnose:${f.probe_id}`, phase: 'Diagnose', schema: DIAGNOSE_SCHEMA, model: MODEL, effort: EFFORT }
  )
))).filter(Boolean)

const realBugs = diagnoses.filter(d => d.is_real_bug && d.classification === 'real_bug')
log(`diagnosed ${diagnoses.length} flag(s): ${realBugs.length} real bug(s); ${diagnoses.length - realBugs.length} definitional/convention/gt`)

// ---- Fix (guarded, opt-in) --------------------------------------------------

phase('Fix')
let fixes = []
if (!DO_FIX) {
  log(`fix phase skipped (do_fix=false). ${realBugs.length} real bug(s) reported for human review.`)
} else if (realBugs.length === 0) {
  log('no real bugs to fix - harness correctly proposes no code changes.')
} else {
  // One bug at a time, isolated worktree, regression-guarded.
  fixes = []
  for (const bug of realBugs) {
    const fix = await agent(
      `Fix ONE real WHEP bug, guarded. Target case: ${JSON.stringify(bug)}

PROCESS (do all, in order):
1. Record metric_before = the METRIC line from \`${BENCH}\`.
2. Make the SMALLEST change in R/ that fixes the root cause. Follow CLAUDE.md (style, air format, namespaces). You may ONLY edit files under R/. NEVER edit ${DIR}/ground_truth.csv, the probes, or the benchmark - changing the test to pass is forbidden.
3. Run \`devtools::test()\`. All tests must pass (allow only documented pre-existing failures).
4. Re-run \`${BENCH}\` -> metric_after. Compute regressions = probe_ids that pass before but flag/fail after (compare FLAGGED arrays).
5. accepted = (target probe now passes) AND (regressions is empty) AND (tests pass). If not accepted, REVERT with \`git checkout -- R/\` and report accepted=false.
Leave accepted changes in the working tree (uncommitted) for human review - do NOT commit.`,
      { label: `fix:${bug.probe_id}`, phase: 'Fix', schema: FIX_SCHEMA, isolation: 'worktree', model: FIX_MODEL, effort: FIX_EFFORT }
    )
    fixes.push(fix)
  }
}

return {
  metric_before: before.metric_line,
  metric_after: after.metric_line,
  researched: researched.length,
  pinned: pins.length,
  diagnoses,
  real_bugs: realBugs.map(b => b.probe_id),
  fixes,
}
