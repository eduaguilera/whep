# Iteration 2: Targeted Research Agent Findings

## Agent 1: Denmark 1864

**Key findings (confirmed by web research)**:
- Treaty of Vienna signed **October 30, 1864**
- Denmark lost Schleswig (~9,400 km2), Holstein (~8,417 km2), Lauenburg (~1,183 km2) = **~19,000 km2 total**
- Denmark before: **~58,000 km2** (European territory), after: **~39,000 km2**
- Loss: **~33% of territory, ~40% of population**
- North Schleswig returned 1920 via plebiscite: **+3,984 km2**
- CShapes starts at 1886, so the 1864 change is already past by then

**Conclusion**: Confirmed as CShapes limitation. The 1864 change was massive (33%) and
would warrant a new polity period per WHEP rules, but requires pre-1886 geometry data
that CShapes doesn't have. The colleague's "Europe from 1850" sheet has the data
(Denmark 1816-1864: 57,086 km2; Denmark 1865-1885: 38,483 km2).

**Additional finding**: Denmark also lost Norway to Sweden in 1814 (Treaty of Kiel),
sold the Danish West Indies to the US in 1917, and Iceland became sovereign in 1918
(full independence 1944). None of these additional changes affect the current database.

## Agent 2: Two Sicilies Duplicate

**Key findings (confirmed by web research)**:
- "Kingdom of the Two Sicilies" and "Two Sicilies" are the **full name and short name
  for the EXACT same entity**
- COW codes it as "Two Sicilies" (SIC, #329) — **single entry**
- CShapes does NOT include it (ended 1860, before CShapes 1886 start)
- The FT source paper (Federico & Tena-Junguito 2019) draws its polity list from COW
- The duplicate arose from two separate entries in the FT CSV for the same entity

**Conclusion**: Our fix is confirmed correct. Both FT entries now map to common name
"Two Sicilies" and the duplicate polity_code KIN-1800-1860 has been removed.

## Agent 3: Ionian Islands Date

**Key findings (confirmed by web research)**:
- Treaty of London signed **November 14, 1863** (not 1864 as some sources say)
- Actual handover: **May 28, 1864**
- CShapes does NOT include the Ionian Islands at all (transfer predates CShapes 1886 coverage)
- The 1862 date comes **exclusively from Federico-Tena**
- What happened in 1862: **King Otto of Greece was deposed** in October 1862, triggering
  negotiations that led to Britain offering the Ionians to the new Greek government
- The FT data itself is contradictory: end_year=1862 but notes="1864-1938 Greece"

**Nuance**: The agent noted that FT may be using 1862 as "last year of separate trade
data availability" rather than the political transfer date. This is plausible — trade
statistics may have been disrupted after the 1862 Greek revolution.

**Decision**: Our fix (end_year=1864) is historically accurate for the political
transfer. If FT specifically tracks trade data cessation, 1862 might be defensible,
but the FT notes saying "1864-1938 Greece" suggest they intended 1864 as the transition.
**Keep the fix.**

## Agent 4: Additional Databases

Found **24 additional datasets** beyond the ones already known. Most relevant:

| Dataset | Geometry? | Coverage | Free? |
|---------|-----------|----------|-------|
| **Aourednik Historical Basemaps** | Yes (GeoJSON) | 1880-1960 snapshots | Yes |
| **MPIDR Census Mosaic** (Max Planck) | Yes (Shapefiles) | 1860-2003, Europe | Yes |
| **COW Territorial Change v6** | No (tabular) | 1816-2018 | Yes |
| **Wimmer & Min Empire-to-Nation** | No (tabular) | 1816-2001 | Yes |
| **V-Dem Historical** | No (tabular) | 1789-present | Yes |
| **Thenmap API** | Yes (GeoJSON) | 1945-present | Yes |
| **IBAD (Border Agreements)** | No (tabular) | 1816-2001 | Yes |
| **Euratlas** | Yes (Shapefiles) | 1CE-2000CE | Paid |
| **IPE Data Resource** | No (tabular) | 1800-2018 | Yes |

**Recommendation for the Denmark 1864 gap**: The **Aourednik Historical Basemaps**
have an 1880 snapshot with GeoJSON polygons that would include pre-1920 Denmark
(already without Schleswig-Holstein but correct for that period). The **MPIDR Census
Mosaic** may have the actual 1860-era Danish boundaries. These could potentially
provide the missing geometry.

Full database survey saved in `13_additional_databases.md`.

## Agent 5: Orange Free State

**Confirmed our analysis**: Two entries from different sources (FT: 1848-1900, CShapes:
pre-1886 to 1910) represent different perspectives. The independent republic ended at
the Boer War (1900/1902), but the territory continued as British Orange River Colony
until the Union of South Africa (1910). CShapes tracks the territory, FT tracks trade.

**No action needed.** The overlap is by design.

## Agent 6: Sweden

**Key findings**:
- Norway separated in **1905** (dissolution of union June 7, 1905)
- Sweden before: **761,932 km2** (including Norway), after: **443,303 km2**
- Loss: **41.8%** of territory
- CShapes correctly has two periods at exactly 1905
- FAOSTAT, M49, and FT all treat Sweden as a single entity with no temporal split

**Important nuance**: The agent identified a **semantic inconsistency**:
- SWE-1800-2025 claims to cover 1800-2025 but its geometry is post-1905 only (443K km2)
- SWE-1800-1905 correctly has the larger pre-1905 geometry (762K km2)
- For the period 1800-1905, SWE-1800-2025's geometry doesn't match Sweden's actual territory

The agent suggests this could be resolved by adding a whep_fix to set Sweden's start
to 1905 (like was done for Russia 2014 and Serbia 2008), but this would break the
FAOSTAT/M49/FT linkage which uses "Sweden" as a continuous entity back to 1800.

**Decision**: Keep current design. The SWE-1800-2025 entity serves FAOSTAT/M49
data linkage (trade data attributed to "Sweden" regardless of territorial extent).
SWE-1800-1905 provides the correct larger geometry for spatial analysis. Downstream
users should use the appropriate polity depending on whether they need trade data
linkage or territorial accuracy. **Document this clearly.**
