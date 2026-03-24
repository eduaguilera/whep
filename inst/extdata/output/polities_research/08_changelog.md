# Polities Research: Changes Made (Cumulative)

## Date: 2026-03-24

## Summary

| # | Fix | Before | After | Files |
|---|-----|--------|-------|-------|
| 1 | Manchukuo end year | MAN-1932-2025 | MAN-1932-1945 | whep_fixes, common_names, polity_codes |
| 2 | Ionian Islands end year | ION-1815-1862 | ION-1815-1864 | whep_fixes, common_names, polity_codes |
| 3 | Kokand end year | KOK-1800-1883 | KOK-1800-1876 | whep_fixes, common_names, polity_codes |
| 4 | Two Sicilies duplicate | 2 entries (KIN + TWO) | 1 entry (TWO-1800-1860) | common_names, polity_codes |
| 5 | Orange Free State duplicate | 2 entries (ORA-1848-1900 + ORA-1800-1910) | 1 entry (ORA-1848-1910) | common_names, polity_codes |

**Net result**: 603 polities -> 601 polities (2 duplicates removed, 3 dates corrected)

---

## Detailed Changes

### Fix 1: Manchukuo (Iteration 1)
- `whep_fixes.csv`: Added `Manchukuo,1932,1945,"...dissolved with Japanese surrender..."`
- `common_names.csv`: Added `Manchukuo,whep,Manchukuo`
- `polity_codes.csv`: Changed `MAN-1932-2025` to `MAN-1932-1945`

### Fix 2: Ionian Islands (Iteration 2)
- `whep_fixes.csv`: Added `Ionian islands,1815,1864,"...FT has end_year=1862 but incorrect..."`
- `common_names.csv`: Added `Ionian islands,whep,Ionian islands`
- `polity_codes.csv`: Changed `ION-1815-1862` to `ION-1815-1864`

### Fix 3: Kokand (Iteration 2)
- `whep_fixes.csv`: Added `Kokand,NA,1876,"...dissolved by Russia on February 19 1876..."`
- `common_names.csv`: Added `Kokand,whep,Kokand`
- `polity_codes.csv`: Changed `KOK-1800-1883` to `KOK-1800-1876`

### Fix 4: Two Sicilies Duplicate (Iteration 2)
- `common_names.csv`: Changed `Kingdom Two sicilies,federico_tena,Kingdom Two sicilies` to
  `Kingdom Two sicilies,federico_tena,Two Sicilies` (both FT entries now map to same common name)
- `polity_codes.csv`: Removed `Kingdom Two sicilies,KIN-1800-1860`

### Fix 5: Orange Free State Duplicate (Iteration 2, late)
- `common_names.csv`: Changed `Orange Free State,federico_tena,Orange Free State` to
  `Orange Free State,federico_tena,Orange Free State (to 1910)` (matching Transvaal pattern)
- `polity_codes.csv`: Removed `Orange Free State,ORA-1848-1900`, updated
  `Orange Free State (to 1910),ORA-1800-1910` to `Orange Free State (to 1910),ORA-1848-1910`

---

## Files Created (Research Reports)

15 files in `inst/extdata/output/polities_research/` (total ~540 KB):

| # | File | Content |
|---|------|---------|
| 01 | methodology_and_sources.md | Data sources and methodology |
| 02 | era_analysis_1860_1920.md | Italian/German unification, Balkans, Africa, WWI |
| 03 | era_analysis_1920_1991.md | Interwar, WWII, decolonization, Cold War |
| 04 | era_analysis_1991_present.md | Yugoslavia, post-Cold War, Crimea, 2022+ |
| 05 | technical_issues.md | Code collisions, duplicates, gaps |
| 06 | cow_crossreference.md | 381 COW territorial changes mapped |
| 07 | final_summary_and_decisions.md | First pass summary |
| 08 | changelog.md | This file (cumulative) |
| 09 | research_1860_1920.md | Deep: 150+ territorial changes |
| 10 | research_1920_1991.md | Deep: 130+ new states |
| 11 | research_1991_present.md | Deep: post-Cold War + databases |
| 12 | iteration2_findings.md | Root cause analysis |
| 13 | additional_databases.md | 24 additional databases with URLs |
| 14 | iteration2_agent_findings.md | Targeted agent results |
| -- | polities_database_verified.csv | Full 601-entry verified export |
