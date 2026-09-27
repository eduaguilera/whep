# WHEP agent reference — area codes and polity columns

Read before writing code that groups, joins or reduces on `reporting_polity_code`, `polity_area_code` or `area_code`.

Moved out of `AGENTS.md` on 2026-09-27 to keep that file under Codex's 32 KiB
default instruction-file read cap (`project_doc_max_bytes`). `AGENTS.md`
carries the pointer; this file carries the detail.

---

### Area codes and polity columns

There are **three** territorial columns, they do three different jobs, and
using one for another job silently misattributes data:

- `reporting_polity_code` — **the identity.** The polity itself
  (`"ESP-1846-1914"`), year-aware: the same `area_code` resolves to different
  polities in different years. This is what says which territory a row is
  about, and it is the column a published output must carry.
- `polity_area_code` — **the aggregation key.** `coalesce(fabio_code,
  area_code)`: FABIO's country numbering, adopted so WHEP's builds sum on the
  same grain FABIO does. It is a *bucket, not an identity* — several reporting
  areas can share one. Legitimate in a `by =`; never a statement about who a
  row is.
- `area_code` — **the provenance.** The FAOSTAT-style area key the source
  used. Belongs at ingestion; it is not an identity either, because FAOSTAT
  retires and re-cuts codes as history happens.

The bucket is **not** required by the input-output model: `build_io_model()`
sizes each year's matrix from that year's own areas
(`.get_io_dims(su, cbs_yr)`), so every year is solved independently and no
fixed country list is needed. The folding is inherited FABIO numbering, and
almost all of it is gone — the Rest-of-World fold was removed in #628 and is
un-folded by default. Measured live through `.polity_crosswalk()`, exactly
three area codes still fold into a different bucket (276 and 277 into 206,
62 into 238), and `polity_bucket_coverage()` reports **one** bucket summing
two live territories: 206, Sudan and South Sudan, from 2012. Un-folding it is
#680.

#### A published row is a polity-period

**Decided 2026-09-22 (#1192).** One published row answers for the entity that
existed that year, not for a reporting area across all time. A territorial
handover is a real discontinuity and the series steps at it.

The comparable-across-time view is **derived, not the default**: call the
exported `build_constant_territory_series()`, which reallocates onto a
reference year's boundaries and reports `imputed_share` so the estimate says
how much of itself was filled in.

This matters at every reduction, because a `by =` answers it implicitly. 50
area codes carry more than one polity in or after 1961 (119 across all time),
overwhelmingly decolonisation handovers — area 4 Algeria splits at 1962, 7
Angola at 1975, 16 Bangladesh at 1971. Grouping on the polity splits those
series; grouping on the area does not.

**Verify such a change by row and distinct-key counts, never by totals.**
Mass is conserved either way — it just spreads over more rows — so a totals
diff reads as success. That is exactly how #561/#563 shipped a bucket that
stopped summing without one value moving.

`R/polity_columns_doc.R` documents these once; inherit that section instead of
writing a fresh, subtly different description. Rows that resolve to no polity
keep `NA` rather than being dropped, so gaps stay visible.
