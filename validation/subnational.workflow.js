export const meta = {
  name: 'subnational-validation',
  description: 'For each top-N production country, a subagent searches for subnational (state/province) crop data, sums to national totals, and we compare against WHEP national production',
  whenToUse: 'Cross-check WHEP national production against independently-compiled subnational statistics.',
  phases: [
    { title: 'Rank', detail: 'rank top-N production countries from WHEP' },
    { title: 'Discover', detail: 'one subagent per country searches subnational data' },
    { title: 'Compare', detail: 'sum-to-national vs WHEP, judge ratios' },
  ],
}

// Args: { crops?: string[], n_countries?: number, year_min?: number, year_max?: number, rank_year?: number }
// args can arrive as a JSON string rather than a parsed object - normalise.
const ARGS = typeof args === 'string' ? JSON.parse(args || '{}') : (args || {})
const CROPS = ARGS.crops || ['Wheat', 'Rice', 'Maize (corn)']
const N = ARGS.n_countries || 5
const Y0 = ARGS.year_min || 1970
const Y1 = ARGS.year_max || 2010
const RANK_YEAR = ARGS.rank_year || 2010
// Harness subagents default to Sonnet to keep token cost down (override per run
// with args.model, e.g. {"model":"opus"} or {"model":"haiku"}). Effort defaults
// to medium so agents don't inherit the (possibly xhigh) session effort.
const MODEL = ARGS.model || 'sonnet'
const EFFORT = ARGS.effort || 'medium'
// Bounded benchmark years (don't make agents cover a 40-year span source-by-source).
const YEARS = ARGS.years || [1990, 2000, 2010]
// Countries with a LOCAL bulk dataset are extracted deterministically (a helper
// that sums state-level rows), so they never web-search or parse PDFs. The
// helper resolves the (gitignored) data path itself.
const LOCAL = { USA: `Rscript validation/nass_sum.R ${YEARS.join(',')}` }
const ENV = `VAL_CROPS=${CROPS.join(',')} VAL_N_COUNTRIES=${N} VAL_YEAR_MIN=${Y0} VAL_YEAR_MAX=${Y1} VAL_RANK_YEAR=${RANK_YEAR}`
log(`config: N=${N}, crops=[${CROPS.join(', ')}], window=${Y0}-${Y1}; raw args=${JSON.stringify(args)}`)

const COUNTRIES_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['crops', 'year_min', 'year_max', 'countries'],
  properties: {
    crops: { type: 'array', items: { type: 'string' } },
    year_min: { type: 'number' }, year_max: { type: 'number' },
    countries: {
      type: 'array',
      items: {
        type: 'object', additionalProperties: true,
        required: ['iso3', 'name', 'area_code'],
        properties: { iso3: { type: 'string' }, name: { type: 'string' }, area_code: { type: 'number' } },
      },
    },
  },
}

const SOURCE_ENTRY = {
  type: 'object', additionalProperties: false,
  required: ['iso3', 'source', 'url', 'local_file', 'covers_crops', 'covers_years', 'basis_notes', 'notes'],
  properties: {
    iso3: { type: 'string' },
    source: { type: 'string' },
    url: { type: 'string' },
    local_file: { type: 'string', description: 'path under validation/cache/data/<iso3>/ where the raw dataset is saved' },
    covers_crops: { type: 'array', items: { type: 'string' } },
    covers_years: { type: 'array', items: { type: 'number' } },
    basis_notes: { type: 'string' },
    notes: { type: 'string' },
  },
}

// Discover agents WRITE their findings to a per-country file and return only
// this summary - bulk data never travels back through a prompt. The findings
// file schema (documented in the prompt) is validated downstream in R by
// compare_findings.R::read_findings(), at the point of use.
const DISCOVER_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['country_iso3', 'subnational_source_found', 'used_cache', 'findings_file', 'n_findings', 'crops_covered', 'new_sources', 'notes'],
  properties: {
    country_iso3: { type: 'string' },
    subnational_source_found: { type: 'boolean' },
    used_cache: { type: 'boolean', description: 'true if at least one finding came from a cached source/file rather than a fresh web search' },
    findings_file: { type: 'string', description: 'path written: validation/cache/findings/<iso3>.json' },
    n_findings: { type: 'number' },
    crops_covered: { type: 'array', items: { type: 'string' } },
    new_sources: { type: 'array', items: SOURCE_ENTRY, description: 'sources discovered THIS run via web fallback, to add to the registry (empty if all from cache)' },
    notes: { type: 'string' },
  },
}

const VERDICTS_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['verdicts'],
  properties: { verdicts: { type: 'array', items: { type: 'object', additionalProperties: true } } },
}

// ---- Rank --------------------------------------------------------------------
phase('Rank')
const ranking = await agent(
  `Run \`${ENV} Rscript validation/rank_countries.R\` from the repo root. It prints a COUNTRIES_JSON_START/END block. Return the parsed crops, year_min, year_max, and countries arrays. Do not edit anything.`,
  { label: 'rank', phase: 'Rank', schema: COUNTRIES_SCHEMA, model: MODEL, effort: EFFORT }
)
if (!ranking || !ranking.countries) {
  log('rank agent failed (e.g. session limit) - aborting; resume after it resets.')
  return { error: 'rank_failed', countries: [], verdicts: [] }
}
log(`top ${ranking.countries.length} countries: ${ranking.countries.map(c => c.iso3).join(', ')} | crops: ${ranking.crops.join(', ')}`)

// ---- Discover (one subagent per country) ------------------------------------
phase('Discover')
const perCountry = (await parallel(ranking.countries.map(c => () => {
  const localCmd = LOCAL[c.iso3]
  const prompt = localCmd
    ? `A LOCAL bulk dataset is registered for ${c.name} (${c.iso3}). Run \`${localCmd}\` from the repo root. It deterministically sums state-level production per (crop, year), converts to metric tonnes (rice on WHEP's milled basis), and WRITES validation/cache/findings/${c.iso3}.json itself. Then read that file to count rows and RETURN the summary: findings_file="validation/cache/findings/${c.iso3}.json", n_findings, crops_covered, used_cache=true, new_sources=[], subnational_source_found=true, notes. Do NOT web-search, download, or parse PDFs.`
    : `You are validating WHEP's NATIONAL crop production for ONE country using INDEPENDENT SUBNATIONAL (state/province-level) statistics summed to a national total.

COUNTRY: ${c.name} (${c.iso3})
CROPS (use these exact names): ${ranking.crops.join(', ')}
TARGET YEARS: ${YEARS.join(', ')} (only these; skip a year if not readily available)

CACHE-FIRST (before any web search):
1. Read validation/sources.json, take entries with iso3 == "${c.iso3}"; \`ls validation/cache/data/${c.iso3}/\`.
2. If a cached entry covers a crop: parse its local_file in R (see below). If the file is missing, download the entry's url once, then parse. Sum subnational units -> national total; from_cache=true.
3. Use web tools (ToolSearch "select:WebSearch,WebFetch") ONLY if no cached entry covers a crop/year. Save any NEW dataset to validation/cache/data/${c.iso3}/ and add it to new_sources. from_cache=false for those.

USE R FOR PARSING + ARITHMETIC (more reliable than grep/regex/eyeballing):
- You have a full R toolchain via \`Rscript -e '...'\`: data.table (fread, for big CSVs), readxl + openxlsx (.xls/.xlsx), readr, nanoparquet + arrow (.parquet), jsonlite, curl/httr. Read the dataset and do the state/province sum + unit conversion in R, not by hand.
- The whep package itself is loadable with devtools::load_all('.') if you want its helpers (e.g. whep_read_file()).
- For a .pdf with no structured alternative, pdftotext is fine, but a CSV/xls/API is always preferred.

STRUCTURED-FIRST + HARD BUDGET (avoid rabbit holes):
- Strongly prefer ONE structured source (government API / CSV / xls) that covers multiple years in a single fetch.
- HARD LIMIT: at most ~8 web fetches total for this whole country. If you pass that, STOP and report whatever you have.
- Do NOT parse scanned PDFs. Do NOT scrape statistical-yearbook HTML pages, and NEVER use the Wayback Machine / archive.org - if a national statistics office doesn't offer a clean API/CSV, that year is simply unavailable.
- If a target year isn't covered by a cached source and isn't available from ONE clean structured fetch, SKIP it immediately (omit from findings). Returning only the cached years (e.g. just 1990) is the correct, expected outcome - missing years are fine, a rabbit hole is not.

VALUES:
- \`value\` in METRIC TONNES, unit="tonnes". Convert: corn/maize 1 bu=25.4012 kg; wheat/soybean 1 bu=27.2155 kg; rough rice 1 cwt=45.3592 kg; short ton=0.907185 t; "1000 tonnes" x1000.
- RICE is MILLED-equivalent in WHEP: if the source is paddy/rough rice, x0.67; record original + factor in basis. n_subnational_units = states/provinces summed.

OUTPUT - write a file, not rows:
- Write findings to validation/cache/findings/${c.iso3}.json (a JSON ARRAY; each item: country_iso3, crop, year, value, unit, basis, n_subnational_units, source, url, from_cache, confidence, notes).
- RETURN only: findings_file, n_findings, crops_covered, used_cache, new_sources, subnational_source_found, notes.

RULES: FETCH or read locally, never recall. If nothing exists, subnational_source_found=false and write [] to the file.`
  return agent(
    prompt,
    { label: `subnational:${c.iso3}`, phase: 'Discover', schema: DISCOVER_SCHEMA, model: MODEL, effort: EFFORT }
  )
}))).filter(Boolean)

// Persist any newly-discovered sources into the registry (single writer to
// avoid concurrent races; the per-country file downloads already happened).
const newSources = perCountry.flatMap(r => r.new_sources || [])
if (newSources.length > 0) {
  await agent(
    `Merge these newly-discovered subnational sources into validation/sources.json (a JSON array). Read the file, append each entry, dedup by (iso3,url) keeping the newest, and write it back as pretty JSON. Touch nothing else.

NEW SOURCES: ${JSON.stringify(newSources)}`,
    { label: 'persist-sources', phase: 'Discover', model: MODEL, effort: EFFORT }
  )
}
const cacheHits = perCountry.filter(r => r.used_cache).length
log(`discover: ${perCountry.length} countries, ${cacheHits} used cache, ${newSources.length} new source(s) registered`)

const totalFindings = perCountry.reduce((n, r) => n + (r.n_findings || 0), 0)
log(`collected ${totalFindings} findings across ${perCountry.filter(r => r.subnational_source_found).length} countries with a source`)

// ---- Compare (deterministic, via R reading the findings files) --------------
// No findings travel through this prompt - the agent only runs the R command,
// which globs validation/cache/findings/*.json and validates them.
phase('Compare')
let verdicts = []
if (totalFindings === 0) {
  log('no findings written to compare.')
} else {
  const out = await agent(
    `Run \`${ENV} Rscript validation/compare_findings.R validation/cache/findings\` from the repo root. It reads every per-country findings file, judges each against WHEP, and prints a VERDICTS_JSON_START/END block. Return the parsed verdicts array. Do not edit any repo files.`,
    { label: 'compare', phase: 'Compare', schema: VERDICTS_SCHEMA, model: MODEL, effort: EFFORT }
  )
  verdicts = (out && out.verdicts) || []
}

return {
  countries: ranking.countries.map(c => c.iso3),
  crops: ranking.crops,
  discovered: perCountry.map(r => ({ iso3: r.country_iso3, n: r.n_findings, used_cache: r.used_cache, file: r.findings_file })),
  verdicts,
}
