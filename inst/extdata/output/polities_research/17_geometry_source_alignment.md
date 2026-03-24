# Geometry Source Alignment Analysis

## How CShapes Currently Works in the Pipeline

### Data Flow
```
cshapes::cshp()          -> sf tibble with MULTIPOLYGON geometry (WGS84/EPSG:4326)
  |
  .load_cshapes()        -> adds area (km²), extracts start/end years
  |
  .clean_cshapes()       -> filters for relevant area changes, removes boundary years
  |                          Saved to k_cshapes (sysdata.rda WITH geometry)
  |                          Also saved to cshapes.csv (WITHOUT geometry - too large)
  |
  .prepare_cshapes()     -> selects: original_name, source, start_year, end_year, geometry
  |
  .merge_datasets()      -> bind_rows with FT, FAOSTAT, M49, WHEP (no geometry)
  |                          Non-CShapes rows get EMPTY MULTIPOLYGON
  |
  .add_common_names()    -> join with common_names mapping
  |
  .aggregate_cols()      -> .check_unique_geometry(): keeps CShapes geom if present,
  |                          or returns EMPTY if no CShapes source for this polity
  |
  get_polities()         -> sf tibble: 601 polities, some with geometry, some empty
```

### Key Technical Properties of CShapes 2.0
- **CRS**: WGS84 / EPSG:4326
- **Geometry type**: MULTIPOLYGON
- **Coding system**: Gleditsch-Ward (GW) codes + COW codes
- **Coverage**: 1886-2019 (monthly resolution internally, aggregated to yearly)
- **Status types**: Includes both SOVEREIGN states and some DEPENDENT territories
  (dependencies coded with `status` field: 1=state in GW, 2=state in COW only,
  3=dependency/colony). The `cshp()` function returns ALL types by default.
- **Total entries**: ~930 country-periods (many per country across time)

### What CShapes DOES Include (with geometry)
- All sovereign states 1886-2019
- Major dependencies: British India, French Indochina (as whole units)
- BUT NOT: individual colonies within larger colonial federations
  (e.g., no separate "British Malaya" vs "Straits Settlements" distinction)

### What CShapes DOES NOT Include
- Pre-1886 entities (Italian pre-unification states, etc.)
- Small dependent territories (Bermuda, Guam, Cook Islands, etc.)
- Colonial sub-divisions (British East Africa as separate from Kenya+Uganda)
- Post-2019 changes (South Sudan 2011 IS covered; but nothing after 2019)
- Micro-states below CShapes' threshold (some very small entities)

## Alignment of Alternative Sources with CShapes

### Source 1: GADM 4.1

| Property | CShapes | GADM | Compatible? |
|----------|---------|------|-------------|
| CRS | WGS84 (EPSG:4326) | WGS84 (EPSG:4326) | YES - same CRS |
| Geometry type | MULTIPOLYGON | MULTIPOLYGON | YES |
| Temporal | 1886-2019 (historical) | Current snapshot only | DIFFERENT |
| Admin levels | L0 only (country) | L0-L5 (country to village) | GADM has more detail |
| Dependencies | Some | ALL (separate entries for all territories) | GADM has more |
| Accuracy | ~1:1M scale | ~1:100K scale (more detailed) | GADM more detailed |

**Alignment strategy**: GADM polygons can be DIRECTLY used alongside CShapes polygons
in an sf tibble because they share the same CRS and geometry type. No transformation
needed. However:
- GADM provides CURRENT borders only. For historical polities, GADM polygons represent
  the modern equivalent, not the exact historical boundary.
- GADM polygons are more detailed (higher resolution coastlines). This means they won't
  perfectly align with CShapes boundaries at edges. For visualization purposes this is
  fine. For area calculations, there will be small discrepancies.

**Usage**: Best for modern small territories, subnational units (emirates, provinces).

### Source 2: Natural Earth (1:10m)

| Property | CShapes | Natural Earth | Compatible? |
|----------|---------|--------------|-------------|
| CRS | WGS84 (EPSG:4326) | WGS84 (EPSG:4326) | YES |
| Geometry type | MULTIPOLYGON | MULTIPOLYGON | YES |
| Temporal | Historical | Current snapshot | DIFFERENT |
| Coverage | Sovereign + some deps | Sovereign + ALL deps + disputed | NE has more |
| Scale | ~1:1M | 1:10M (most detailed) | NE more detailed |

**Alignment strategy**: Same CRS, directly compatible. Natural Earth's
`ne_10m_admin_0_countries` layer includes dependent territories and disputed areas
that CShapes lacks. The `ne_10m_admin_0_map_subunits` layer further disaggregates.

**Usage**: Best for minor islands, disputed territories, and as a fallback.

### Source 3: Aourednik Historical Basemaps

| Property | CShapes | Aourednik | Compatible? |
|----------|---------|-----------|-------------|
| CRS | WGS84 (EPSG:4326) | WGS84 (EPSG:4326) | YES |
| Geometry type | MULTIPOLYGON | POLYGON/MULTIPOLYGON | YES (with conversion) |
| Temporal | 1886-2019 | Snapshots at specific years | COMPLEMENTARY |
| Accuracy | Academic dataset | Community project, variable quality | LOWER |

**Available snapshots relevant to WHEP (1800-2025)**:
- 1800, 1815, 1830, 1850, 1880, 1900, 1914, 1920, 1930, 1938, 1945, 1960, 1970,
  1980, 1994, 2010

**What's included in snapshots**: Sovereign states AND colonial empires with
approximate boundaries. Each polygon has `NAME`, `SUBJECTO` (colonial power),
and `PARTOF` fields. **However**: Colonies are often shown as part of the
metropole (big empire polygon), NOT as separate colonial territories.

**CRITICAL LIMITATION**: Aourednik shows "British Empire" as one polygon, not
"British Malaya" + "British East Africa" + etc. as separate polygons. This is
the SAME problem as Cliopatria.

**Usage**: Best for pre-unification Italian states (1850 snapshot has Sardinia,
Two Sicilies, Papal States, Tuscany, Modena, Parma as SEPARATE polygons), and
for Central Asian khanates. NOT useful for colonial disaggregation.

### Source 4: Cliopatria

Already noted by user: Cliopatria shows empire-level polygons, not individual
colonies. This makes it unsuitable for WHEP's colony-level tracking.

### Source 5: MPIDR Census Mosaic

| Property | CShapes | MPIDR | Compatible? |
|----------|---------|-------|-------------|
| CRS | WGS84 | Varies (check per file) | NEED TO VERIFY |
| Coverage | Global | Europe only | Europe only |
| Temporal | 1886-2019 | 1837-2003 at census dates | PRE-1886 coverage! |

**Key value**: The MPIDR dataset can provide pre-1886 European boundaries, including
Germany before 1871 and Denmark before 1864. However, these are ADMINISTRATIVE
boundaries at census dates, not necessarily political boundaries.

## Cliopatria vs What We Need

### The Colony Problem

The user correctly identifies that Cliopatria (and Aourednik) show empires as monolithic
polygons. For example:

**What Cliopatria shows**: "British Empire" = one giant polygon covering UK + India +
Australia + Africa + etc.

**What WHEP needs**: Separate polygons for each colonial territory that has its own
trade data:
- British East Africa (Kenya+Uganda) -> one polygon
- British Malaya -> one polygon
- Belgian Congo -> one polygon
- French West Africa -> one polygon
- etc.

### Why This Matters

WHEP tracks polities because each polity has TRADE DATA. A colonial territory like
"Belgian Congo" has its own trade statistics in Federico-Tena. To visualize or
spatially analyze this trade data, we need a polygon for "Belgian Congo" specifically,
not just "Belgian Empire."

### Solution for Colonial Polygons

The best approach is to **construct colonial polygons from GADM modern boundaries**:

1. **Most colonial borders became modern country borders** at independence. This is
   because the colonial powers drew borders, and newly independent states inherited them
   (the OAU "uti possidetis" principle).

2. **GADM level 0** (country borders) already gives us most colonial territories:
   - Belgian Congo ≈ modern DRC
   - German East Africa ≈ modern Tanzania mainland
   - Gold Coast ≈ modern Ghana
   - Nyasaland ≈ modern Malawi
   - etc.

3. **GADM level 1** (provinces/states) gives us sub-national colonial entities:
   - British Malaya ≈ Peninsular Malaysian states
   - Sabah/Sarawak ≈ Malaysian states
   - Manchukuo ≈ NE Chinese provinces

4. **For colonial federations** (French West Africa, French Equatorial Africa), we
   dissolve multiple GADM countries into one polygon.

The resulting polygons are **historically approximate** (colonial borders sometimes
differed from modern borders, especially in Africa where post-independence adjustments
occurred), but are accurate to within ~95% of territory for most cases.

## Recommended Polygon Integration Architecture

### Option A: Add a New Source (Recommended)

Add a new source alongside CShapes in the pipeline — call it `gadm` or `geometry_fixes`.
This would be a pre-processed sf object (like `k_cshapes`) containing polygons for
polities that CShapes doesn't cover.

```
.prepare_cshapes()       -> CShapes polygons (288 polities)
.prepare_gadm_geometry() -> GADM/NatEarth polygons (195 polities) [NEW]
.merge_datasets()        -> bind_rows all sources including new geometry source
```

This requires:
1. A new CSV/GeoPackage file with polity_name, geometry for each missing polity
2. A new `.prepare_*()` function
3. Adding the new source to `.merge_datasets()`
4. Common names entries for the new source

### Option B: Attach Geometry Post-Pipeline

After `get_polities()` produces the 601-row sf tibble, run a second pass that fills
empty geometries from GADM/NatEarth. This is simpler but breaks the single-pipeline
design.

### Option C: Expand CShapes-like Pre-Processing

Create a comprehensive `k_all_geometries` object in `sysdata.rda` that contains ALL
polity geometries from all sources, pre-processed to match CShapes format. This is
the cleanest approach but requires the most upfront work.

## Priority Polygon Gaps

Based on trade data importance (Federico-Tena polities with trade data but no polygon):

### Critical (have trade data, no polygon)
1. India (to 1947) - IND-1800-1947: Huge trade volume
2. Egypt (to 1922) - EGY-1800-1922: Major trade entity
3. China, mainland - CHN-1800-2025: FAOSTAT aggregate
4. Germany/Zollverein - GER-1800-2025: FT aggregate
5. Dutch East Indies - DEI-1800-2025: Major colonial trade
6. Belgian Congo - BEL-1885-2025: Major colonial trade
7. British East Africa - BEA-1895-2025: Colonial trade
8. French West Africa - FWA-1895-2025 (has CShapes? check)
9. Gold Coast - GOL-1843-2025: Colonial trade
10. Sudan (Anglo-Egyptian) - SUD-1882-2025: Colonial trade

### Important (smaller trade entities)
- All Gulf emirates (Abu Dhabi, Ajman, etc.)
- All Italian pre-unification states
- British Malaya, Sarawak, Sabah
- Various Pacific island territories

### Lower Priority
- Aggregate/old FAOSTAT entities (Ethiopia old, Nigeria old, etc.)
- Very small islands with negligible trade

## Summary

| Source | CRS Match? | Colony Polygons? | Pre-1886? | Free? | Effort |
|--------|-----------|-----------------|-----------|-------|--------|
| CShapes 2.0 | Baseline | Some deps | No | Yes | Already done |
| GADM 4.1 | YES | Via modern borders | No | Yes | LOW-MEDIUM |
| Natural Earth | YES | Some deps/islands | No | Yes | LOW |
| Aourednik | YES | Empires only | YES (1800+) | Yes | MEDIUM |
| MPIDR Census | Check | No | YES (1837+) | Yes | HIGH |
| Cliopatria | Check | Empires only | YES | Yes | Not suitable |

**Bottom line**: GADM is the primary solution for colonial polygons. Use modern
country borders as proxies. Aourednik for pre-1886 European states. All use WGS84
so they're directly compatible with CShapes.
