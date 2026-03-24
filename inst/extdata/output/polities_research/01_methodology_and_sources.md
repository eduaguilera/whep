# Polities Research Report: Methodology and Sources

**Date**: 2026-03-24
**Branch**: lbm364dl/refactor-polities
**Scope**: Polities from 1860 to present day
**Author**: Automated analysis cross-referencing multiple datasets

## 1. Objective

Systematically verify the WHEP polities database (polity_codes.csv, 603 entries) against
historical records, identifying:
- Incorrect start/end years
- Missing polities (entities with distinct territories not represented)
- Unnecessary duplicates
- Timeline gaps and overlaps
- Potential territorial changes not captured

## 2. Data Sources Consulted

### 2.1 Internal Sources (this branch)
| Source | File | Entries | Coverage |
|--------|------|---------|----------|
| Federico-Tena | federico_tena_polities.csv | 244 | 1800-1938 |
| FAOSTAT | faostat_regions.csv | 344 | 1961-present |
| M49 (UN) | historical_m49.csv | 286 | 1970-present |
| CShapes 2.0 | cshapes.csv | 316 | 1886-2019 |
| WHEP fixes | whep_fixes.csv | 17 | Manual overrides |
| Polity codes | polity_codes.csv | 603 | Combined output |
| Common names | common_names.csv | ~1200 | Source mappings |

### 2.2 Colleague's Database ('/home/catalin/Downloads/polities data full.xlsx')
| Sheet | Entries | Content |
|-------|---------|---------|
| whep_equivalences | 1226 rows, 179 unique polities | FT + CShapes + COW merged |
| COWs full entities changes | 381 | Territorial transfers with areas |
| COWs edited to merge with CShapes | ~1000 | Adapted COW for CShapes merge |
| Europe from 1850 | ~240 polity-periods | Detailed European state periods with areas |
| Cliopatria | 2905 (2694 from 1860+) | Historical polities with areas |
| Colonies_ft_cshapes | ~1000 | Colonial periods mapped to modern states |
| COW-country-codes | 244 | Correlates of War country codes |
| COWs Entities info | 2707 | Full COW entity list |
| British/French/Ottoman empire sheets | Various | Empire-specific entity histories |

### 2.3 External References
- **Correlates of War (COW) Project**: Territorial Change dataset v6
- **CShapes 2.0**: ETH Zurich territorial mapping (1886-2019)
- **Cliopatria**: Historical polities database (via colleague's Excel)
- **UN M49**: United Nations Standard Country Codes
- **FAOSTAT**: FAO country definitions
- **CIA World Factbook** (area references in colleague's notes)

## 3. Methodology

### 3.1 Definition of "Polity"
Per WHEP project: A polity is a **fixed territory over a continuous period of time**.
- Territory change = new polity (even if same country name)
- Name change only (no territory change) = same polity
- Most colonies tracked separately if they have individual trade data

### 3.2 Polity Code Format
`XXX-yyyy-YYYY` where:
- `XXX` = ISO3 code or artificial code
- `yyyy` = start year
- `YYYY` = end year

### 3.3 Verification Process
1. Parse all 603 polity codes and extract year ranges
2. Group by ISO prefix to detect overlaps/gaps
3. Cross-reference each era (1860-1920, 1920-1945, 1945-1975, 1975-1991, 1991-2025)
   against COW territorial changes, CShapes boundaries, and Cliopatria records
4. Compare with colleague's whep_equivalences working sheet
5. Flag discrepancies with severity levels:
   - **CRITICAL**: Wrong dates affecting data joins
   - **IMPORTANT**: Missing polity periods for significant territorial changes
   - **MINOR**: Design choices that could be revisited
   - **NOTE**: Observations for documentation

### 3.4 Area Threshold
Per colleague's delimitations sheet:
- Territorial change >= 10% of total polity area = new polity boundary
- Islands: change >= 100 km2 = new boundary
- Islands assumed same area through history if single polity
