# Polities Research: Iteration 2 Findings

**Date**: 2026-03-24 (second pass)

## Summary of Changes from First Pass

The second-pass deep-dive confirmed the first-pass findings and uncovered additional
issues through detailed source tracing and systematic scanning.

## New Findings

### CONFIRMED BUG: Two Sicilies Duplicate (UPGRADED from "investigate" to "confirmed")

**Root cause**: The Federico-Tena source dataset contains TWO rows for the same entity:
- Row 1: "Kingdom Two sicilies" (start=NA, end=1860, notes="1861-1938 Italy; joint estimation with Italy")
- Row 2: "Two Sicilies" (start=NA, end=1860, notes="1861-1938 Italy; joint estimation with Italy")

Both rows have IDENTICAL dates and notes. Neither appears in any other source (CShapes,
FAOSTAT, M49). The common_names.csv maps each to a different common name, producing
two separate polity codes:
- KIN-1800-1860 ("Kingdom Two sicilies")
- TWO-1800-1860 ("Two Sicilies")

**This is a data duplication bug in the FT source that propagated through the pipeline.**

**Fix required**: Remove one entry from common_names.csv and polity_codes.csv.
Recommend keeping TWO-1800-1860 and removing KIN-1800-1860.

### NEW BUG: Ionian Islands End Year (UPGRADED from "minor" to "confirmed error")

**Root cause**: Federico-Tena has end_year=1862 for Ionian Islands, but the FT notes
themselves say "1864-1938 Greece." This is an internal contradiction in the FT data.

**Historical facts**:
- Ionian Parliament votes for union with Greece: October 7, 1863
- Treaty of London signed: November 14, 1863
- Handover to Greece: May 28, 1864

The year 1862 doesn't correspond to any known event. It is likely a transcription error
in the original FT dataset. The correct end year should be **1864** (year of actual handover).

**Fix required**: Add to whep_fixes.csv with end_year=1864. Update polity_codes.csv
from ION-1815-1862 to ION-1815-1864.

### NEW BUG: Kokand End Year (NEW FINDING)

**Root cause**: Federico-Tena has end_year=1883 for Kokand, but the Khanate of Kokand
was conquered by Russia in **1876**. The FT notes say "1873-1938 Russia and Soviet Union"
which adds to the confusion.

**Historical facts**:
- Russian military campaigns begin: 1873
- Khanate of Kokand formally dissolved by Russian Empire: February 19, 1876
- Incorporated into Russian Turkestan as Fergana Oblast

The year 1883 is 7 years after the actual dissolution. The notes saying "1873" refer to
the start of Russian military campaigns, not the actual end of Kokand. COW data records
Kokand conquered in 1873 at 92,463 km².

**Fix required**: Add to whep_fixes.csv with end_year=1876. Update polity_codes.csv
from KOK-1800-1883 to KOK-1800-1876.

### CLARIFIED: Orange Free State (Downgraded from "issue" to "design ambiguity")

**Root cause**: Two separate sources, correctly producing two separate polities:
- ORA-1848-1900: FT trade data for the independent republic (ends at Boer War)
- ORA-1800-1910: CShapes territorial entity (includes British colony period 1902-1910)

These overlap from 1848-1900 but have different common names and represent genuinely
different perspectives (trade entity vs. territorial entity).

**Decision**: No fix needed. This is the same pattern as other "aggregate + detailed"
entries (like Ethiopia (old) + Ethiopia periods). Document as design choice.

### CLARIFIED: Sweden (Confirmed correct)

**Root cause**: CShapes produces two periods (pre/post 1905 Norway independence).
The FAOSTAT/M49/FT sources produce a single "Sweden" entity spanning 1800-2025.
The pipeline correctly aggregates them into:
- SWE-1800-1905: CShapes pre-Norway-separation territory (761,932 km²)
- SWE-1800-2025: Modern Sweden aggregate (M49/FAOSTAT codes + post-1905 geometry, 443,303 km²)

**Decision**: Correct by design. No fix needed.

### CLARIFIED: Denmark 1864 (Confirmed as CShapes limitation)

**Root cause**: CShapes 2.0 starts at 1886. Denmark lost Schleswig-Holstein in 1864
(33% territory loss: 57,086 km² → 38,483 km²). By 1886, this change had already occurred.

The colleague's "Europe from 1850" sheet has the pre-1864 Denmark data (57,086 km²)
from a different source, but this data is not integrated into the WHEP pipeline.

**Decision**: Cannot fix without adding a new data source for pre-1886 geometry.
Document as known limitation of CShapes 2.0 coverage.

## Additional Scan Results

### FT Date Warnings (non-issues)
The systematic scan found many FT entries where the notes mention a successor year
that is exactly end_year + 1. These are all CORRECT:
- Duchy Modena: end=1860, notes "1861-1938 Italy" (1860+1=1861, correct)
- Duchy Parma: end=1860, notes "1861-1938 Italy" (correct)
- Sardinia: end=1860, notes "1861-1938 Italy" (correct)
- Papal States: end=1870, notes "1871-1938 Italy" (correct)
- Tuscany: end=1860, notes "1861-1938 Italy" (correct)
- Crete: end=1913, notes "1914-1938 Greece" (correct)

Only Ionian Islands (end=1862, notes "1864-1938") and Kokand (end=1883, notes "1873-1938")
have genuine mismatches.

### New Guinea Entries (non-issue)
The scan found similar-named FT entries:
- "British New Guinea" (1884-NA): Present-day Papua New Guinea
- "New Guinea" (1884-NA): German colony
- "Dutch new Guinea" (1898-NA): Present-day Indonesian Papua

These are CORRECTLY three different entities controlled by different colonial powers.

### CShapes Mapping Completeness
- All 187 unique CShapes country names have rename_cshapes.csv mappings
- All 315 CShapes polity-period entries have common_names.csv mappings
- **100% CShapes coverage confirmed**

### Area Comparison
CShapes areas match the colleague's "Europe from 1850" data within <1% for all
overlapping periods (post-1886). This confirms data integrity.

## Updated Issue Tracker

| # | Issue | Severity | Status | Action |
|---|-------|----------|--------|--------|
| 1 | Manchukuo end year 2025→1945 | IMPORTANT | **FIXED** (pass 1) | whep_fixes.csv updated |
| 2 | Two Sicilies duplicate (FT) | IMPORTANT | **CONFIRMED BUG** | Need to remove duplicate |
| 3 | Ionian Islands end 1862→1864 | IMPORTANT | **CONFIRMED BUG** | Need whep_fixes + code update |
| 4 | Kokand end 1883→1876 | IMPORTANT | **NEW BUG** | Need whep_fixes + code update |
| 5 | Denmark 1864 missing | MINOR | CShapes limitation | Document only |
| 6 | Orange Free State duplicate | IMPORTANT | **CONFIRMED BUG** | Merged (like Transvaal) |
| 7 | Sweden two entries | NOT A BUG | Correct by design | Document only |
