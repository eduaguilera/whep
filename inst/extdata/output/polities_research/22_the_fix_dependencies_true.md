# THE FIX: `cshapes::cshp(dependencies = TRUE)`

## The Root Cause

Line 497 of `R/polities.R`:
```r
countries <- cshapes::cshp() |>
```

The `cshp()` function defaults to `dependencies = FALSE`, which EXCLUDES:
- Colonies (status = 2)
- Protectorates (status = 3)
- Leased territories (status = 4)
- Occupied territories (status = 5)

This single default parameter is why 195 polities lack geometry.

## The Fix

Change line 497 to:
```r
countries <- cshapes::cshp(dependencies = TRUE) |>
```

This will return ALL CShapes entries (~930 rows instead of ~315), including
colonial territories with their actual historical boundaries and geometries.

## What This Unlocks

With `dependencies = TRUE`, CShapes 2.0 should provide polygons for:

### Colonial territories (confirmed in CShapes 2.0):
- British Malaya, British East Africa, British India, British Somaliland
- French Indochina, French West Africa, French Equatorial Africa
- Belgian Congo
- German East Africa, German South West Africa, German Kamerun, German Togoland
- Italian Somaliland, Italian Libya
- Dutch East Indies
- Portuguese colonies (Mozambique, Angola, Portuguese India, etc.)
- Spanish colonies (Spanish Morocco, Spanish Guinea, etc.)
- Ottoman Empire dependencies
- League of Nations mandates (Palestine, Syria, Iraq, Tanganyika, etc.)

### Pre-1886 European entities (via CShapes-Europe extension):
- Papal States (1816-1870)
- Kingdom of Two Sicilies (1816-1860)
- Kingdom of Sardinia (1816-1861)
- Grand Duchy of Tuscany (1816-1860)
- Duchy of Modena (1816-1860)
- Duchy of Parma (1816-1860)
- Pre-unification German states (dozens)
- Pre-1864 Denmark (with Schleswig-Holstein)

## Pipeline Impact

After enabling `dependencies = TRUE`:

1. `.load_cshapes()` will return ~930 rows instead of ~315
2. `.filter_relevant_area_changes()` will group and filter these
3. `.clean_cshapes()` will produce a larger `k_cshapes` object
4. `rename_cshapes.csv` will need NEW entries for the colonial names
5. `common_names.csv` will need NEW entries mapping CShapes colonial names
   to WHEP common names
6. `polity_codes.csv` may need adjustments for polities that now have
   CShapes-derived year ranges

## CShapes Columns Available

The `cshp()` function returns these columns:

| Column | Use in WHEP |
|--------|-------------|
| `country_name` | -> `polity_name` (via rename_cshapes.csv) |
| `start` | -> `start_year` (extract year) |
| `end` | -> `end_year` (extract year) |
| `status` | NEW: can distinguish sovereign from dependent |
| `owner` | NEW: GW code of colonial power (e.g., 200 = UK) |
| `gwcode` | Gleditsch-Ward code |
| `cowcode` | Correlates of War code |
| `geometry` | MULTIPOLYGON in WGS84/EPSG:4326 |

## Estimated Remaining Gaps After the Fix

Even with `dependencies = TRUE`, some polities will still lack CShapes geometry:
- Modern micro-territories (Cook Islands, Niue, Tokelau, etc.) -> use GADM/Natural Earth
- Very small historical entities not in GW/COW lists -> use GADM subnational
- FAOSTAT aggregate entities (regions) -> dissolve member countries

Estimated remaining without geometry: ~50-80 (down from 195)

## CRS Compatibility

CShapes uses WGS84 (EPSG:4326). GADM and Natural Earth also use WGS84.
All sources can be combined directly in sf without reprojection.
