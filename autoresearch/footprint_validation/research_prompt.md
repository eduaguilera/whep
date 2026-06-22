# Ground-truth research prompt (subagent template)

One research subagent resolves **one probe** into a pinned ground-truth row. It
must **fetch** the value from a real source — never recall a number from memory
(see the project rule against hallucinated references). The agent has web tools
(WebSearch, WebFetch).

## Inputs given to the agent

A probe: `{probe_id, layer, area (ISO3 + name), item_name, year, element, unit}`.

## What the agent must do

1. Identify the **most authoritative** source for this exact point. Prefer a
   source **independent of FAOSTAT** where one exists, because WHEP ingests
   FAOSTAT — checking WHEP against FAOSTAT only validates the data plumbing, not
   an independent fact. Priority by region/item:
   - US crops/livestock → USDA NASS QuickStats.
   - EU → EUROSTAT.
   - Otherwise → national statistical office, then FAOSTAT (flag as
     non-independent), then peer-reviewed datasets.
2. **Fetch** the figure for the exact (country, item, year, element, unit).
3. Record the source's **definition** of the number (e.g. "all wheat, grain
   production, marketing year" vs "winter wheat only", "carcass weight" vs
   "live weight", "area harvested" vs "area planted"). Definitional mismatch is
   the main cause of false discrepancies — capture it.
4. Convert nothing; report the value in the source's own unit and give the unit
   string. The harness canonicalises units.
5. Set a `tolerance_pct` appropriate to source/era (e.g. 5 for a primary
   national statistic in the modern era, 15–30 for pre-1961 or sparse-reporting
   regions).
6. Set `confidence`: `high` (primary source, exact match), `medium` (secondary
   or slight definitional gap), `low` (proxy/derived), `none` (no source found —
   then `gt_value` is empty and the probe stays *uncovered*).

## Output (one row, structured)

```
probe_id, gt_value, gt_unit, source, url, definition, tolerance_pct,
confidence, notes
```

- Never invent a URL or a number. If you cannot fetch it, return
  `confidence = none` and explain in `notes`.
- `notes` should flag: non-independent source (FAOSTAT), definitional caveats,
  or unit ambiguity.

## Adversarial second pass (for flagged probes only)

When the harness flags a probe (WHEP vs ground truth outside tolerance), a
**second** agent re-researches the same point from a **different** source. A
discrepancy is only believed when two independent sources agree against WHEP;
if the two sources disagree with each other, the verdict is `source_conflict`
and a human decides.
