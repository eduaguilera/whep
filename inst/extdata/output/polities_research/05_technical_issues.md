# Polities Research Report: Technical Issues and Recommendations

## 1. Code Prefix Collisions

Multiple unrelated polities share the same 3-letter prefix. This is NOT a bug
(the full polity_code is unique), but can cause confusion when filtering by prefix.

| Prefix | Polities | Issue |
|--------|----------|-------|
| BEL | Belgium (BEL-1831-2025), Belgian Congo (BEL-1885-2025) | Same colonizer code |
| CAN | Canary Islands, Canton/Enderbury Is., Canada (x2) | Unrelated entities |
| DAN | Danish India, Danish Virgin Islands | Same colonizer code |
| DUT | Dutch West Indies, Dutch New Guinea | Same colonizer code |
| GER | Germany/Zollverein, German colonies Oceania | Same country code |
| NET | Netherlands (old), Netherlands Antilles (original) | Related entities |
| ORA | Orange Free State, Orange Free State (to 1910) | POTENTIAL DUPLICATE |
| PAL | Palmyra Island, Palestine/Jordan | Unrelated entities |
| PAN | Panama (before Canal Zone), Panama | Related (same country) |
| PRI | Prince Edward Island, Puerto Rico | Unrelated entities |
| SAR | Sardinia, Sarawak | Unrelated entities |
| SPA | Spanish North Africa, Spanish Guinea | Same colonizer code |
| ST. | St. Helena, St. Lucia, St. Vincent | Different islands |
| SUD | Sudan (Anglo-Egyptian), Sudan (1956-2011) | Related (same country) |
| SWE | Sweden, Sweden (to 1905) | POTENTIAL DUPLICATE (see below) |
| SYR | Syria and Lebanon, Syrian Arab Republic (x2) | Related entities |
| TIB | Tibet (old), Tibet (1913-1914), Tibet (1914-1950) | Related entities |
| VAN | Van Diemen's Land, Vancouver's Island | Unrelated entities |

**Recommendation**: No action needed. The full polity_code (XXX-yyyy-YYYY) is
always unique. Document this in the polities documentation.

## 2. Potential Duplicate Entries

### 2.1 "Kingdom Two sicilies" vs "Two Sicilies"
- KIN-1800-1860: "Kingdom Two sicilies" (from Federico-Tena?)
- TWO-1800-1860: "Two Sicilies" (from CShapes?)

**Investigation needed**: Check common_names.csv to see if these map to different
source entries or if one should be removed. They cover the exact same territory
and time period.

### 2.2 "Sweden" vs "Sweden (to 1905)"
- SWE-1800-2025: Appears to be an aggregate FT entity
- SWE-1800-1905: CShapes-based pre-Norway-independence Sweden

**Investigation needed**: If SWE-1800-2025 is a FAOSTAT aggregate, this is by
design. But verify these don't both feed into the same polity after aggregation.

### 2.3 "Orange Free State" vs "Orange Free State (to 1910)"
- ORA-1848-1900: From FT (ends at Boer War)
- ORA-1800-1910: Broader period including pre-independence and annexation to SA

**Investigation needed**: These overlap significantly.

### 2.4 "Ethiopia (old)" vs Ethiopia periods
- ETH-1800-2025: FAOSTAT aggregate entity
- ETH-1800-1889 through ETH-1993-2025: CShapes-based periods

Same pattern as other "old" aggregate entities. By design.

## 3. Timeline Gaps (Intentional vs Problematic)

### 3.1 Intentional Gaps (polity absorbed into larger entity)
| Polity | Gap | Explanation |
|--------|-----|-------------|
| Estonia | 1940-1991 | Absorbed into USSR |
| Latvia | 1940-1991 | Absorbed into USSR |
| Lithuania | 1940-1991 | Absorbed into USSR |
| Montenegro | 1915-2006 | Absorbed into Yugoslavia/Serbia |
| Serbia | 1915-2006 | Absorbed into Yugoslavia |
| Germany | 1945-1990 | Split into FRG/GDR |
| Vietnam | 1893-1954 | Under French Indochina, then split |
| Mayotte | 1914-2002 | Absorbed into Madagascar, then France |

These are all correct - the "missing" polity periods are covered by the
parent/absorbing entity.

### 3.2 Gap Requiring Attention
| Polity | Gap | Issue |
|--------|-----|-------|
| Afghanistan | 1888-1919 | 31-year gap. No CShapes data. Buffer state status. |

**Recommendation**: This gap is documented in the colleague's whep_equivalences
but has no geometry. Consider adding an Afghanistan (1888-1919) entry without
geometry, or documenting the gap explicitly.

## 4. Date Accuracy Issues

### 4.1 Ionian Islands (ION-1815-1862)
End year is 1862 but historical consensus is 1864 (Treaty of London signed
March 29, 1864; handover May 1864). CShapes may use 1862 for a different
boundary event. **Verify against CShapes source data.**

### 4.2 Denmark Missing 1864 Split
Denmark (DNK-1800-1920) doesn't capture the 1864 loss of Schleswig-Holstein,
which was approximately 33% of Denmark's territory (57,086 km2 -> 38,483 km2).

CShapes data clearly shows this change. The colleague's "Europe from 1850"
sheet confirms separate periods at 1864.

**Recommendation**: Consider splitting:
- Denmark (to 1864): DNK-1800-1864
- Denmark (1864-1920): DNK-1864-1920
- Denmark (1920-2025): DNK-1920-2025 (exists)

This would require updates to: polity_codes.csv, common_names.csv, and
potentially rename_cshapes.csv.

### 4.3 Japan as Single Period
Japan (JPN-1800-2025) spans the entire study period despite significant
territorial changes:
- 1873: Bonin Islands annexed
- 1879: Ryukyu Kingdom annexed (Okinawa)
- 1895: Taiwan from China (Treaty of Shimonoseki)
- 1905: South Sakhalin from Russia (Treaty of Portsmouth)
- 1910: Korea annexed
- 1945: Lost all colonial territories

CShapes doesn't differentiate Japan periods (likely because the core home
islands remained the same). COW data shows the changes but they are colonial
acquisitions rather than changes to Japan proper's borders.

**Recommendation**: No change needed. The colonial acquisitions are tracked
as separate polities (Taiwan, Korea) rather than as Japan territory changes.

### 4.4 India Missing 1961 Goa Period
India (IND-1949-2025) doesn't capture the 1961 annexation of Goa (3,702 km2)
from Portugal. However, Goa represented only 0.1% of India's area, which is
well below the 10% threshold for a new polity period.

**Recommendation**: No change needed per WHEP area threshold rules.

## 5. Manchukuo End Year
Manchukuo (MAN-1932-2025) has end year 2025, but Manchukuo ceased to exist in
1945. The 2025 end year comes from the FT source having NA for end_year, which
defaults to k_polity_last_year (2025).

**Recommendation**: Add an entry to whep_fixes.csv:
```
Manchukuo,1932,1945,"Manchukuo dissolved with Japanese surrender in 1945"
```

## 6. Aggregate vs Detailed Entries

Several polities exist in both aggregate form (from FAOSTAT) and detailed
period form (from CShapes). This is by design - the aggregation logic in
R/polities.R handles this through the common_names mapping. The aggregate
entries allow linking modern FAO data while CShapes entries provide
territorial precision.

Examples of this pattern:
- "China, mainland" (FAOSTAT) vs China periods (CShapes)
- "Ethiopia (old)" (FAOSTAT) vs Ethiopia periods (CShapes)
- "Sudan (old)" (FAOSTAT) vs Sudan periods (CShapes)
- "Nigeria (old)" (FAOSTAT) vs Nigeria periods (CShapes)
- "Germany/Zollverein" (FT) vs Germany periods (CShapes)

These aggregate entries are correctly handled and should NOT be removed.

## 7. Summary of Recommended Actions

### CRITICAL (affects data accuracy)
None found. The database is remarkably accurate.

### IMPORTANT (should consider fixing)
1. **Manchukuo end year**: Fix from 2025 to 1945 via whep_fixes.csv
2. **Denmark 1864 split**: Add period split for Schleswig-Holstein loss
3. **"Kingdom Two sicilies" / "Two Sicilies" duplicate**: Investigate and resolve

### MINOR (documentation/cleanup)
4. Ionian Islands end year: Verify 1862 vs 1864
5. Afghanistan 1888-1919 gap: Document or add entry without geometry
6. Code prefix collisions: Document in polities documentation
7. Sweden duplicate entries: Verify aggregation handles correctly

### NO ACTION NEEDED
8. Japan single period: Colonial acquisitions tracked separately
9. India missing 1961 Goa: Below area threshold
10. German states before 1871: FT aggregates as Zollverein by design
11. Post-2022 Russia-Ukraine: Wait for conflict resolution
12. Aggregate vs detailed entries: Working correctly
