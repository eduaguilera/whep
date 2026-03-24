# Polities Geometry Gap Analysis

**Date**: 2026-03-24

## The Problem

The WHEP polities database needs polygon geometries for all 601 polities. Currently,
only CShapes 2.0 provides geometry, covering 288 polities. **195 non-region polities
lack geometry.** (118 are aggregate regions that may not need polygons.)

## Current Geometry Coverage

| Category | Count | % |
|----------|-------|---|
| **Have CShapes geometry** | 288 | 47.9% |
| No geometry (non-region) | 195 | 32.4% |
| Aggregate regions (no polygon expected) | 118 | 19.6% |
| **Total** | **601** | 100% |

## Strategy for Filling Geometry Gaps

The 195 missing polities can be addressed through 4 strategies:

### Strategy 1: GADM / Natural Earth (96 polities)

**Source**: GADM v4.1 (https://gadm.org/) or Natural Earth 1:10m
(https://www.naturalearthdata.com/)

**What**: These are modern territories that still exist today (small islands, overseas
territories, microstates). Their boundaries haven't changed significantly, so current
polygons are adequate.

**Format**: Shapefiles, GeoJSON, GeoPackage (free download)

**Examples**:
- Andorra, Liechtenstein, Monaco, San Marino, Holy See
- Bermuda, Cayman Islands, British Virgin Islands, Guam, American Samoa
- Cook Islands, Niue, Tokelau, Pitcairn
- Aruba, Curaçao, Sint Maarten, Bonaire/St.Eustatius/Saba
- Åland Islands, Guernsey, Jersey, Isle of Man
- French Guiana, French Polynesia, Martinique, Guadeloupe, Réunion, Mayotte
- Christmas Island, Cocos Islands, Norfolk Island, etc.
- Greenland, Svalbard, Faroe Islands
- Hong Kong, Macao, Puerto Rico, US Virgin Islands
- Palestine, Western Sahara
- Seychelles, Nauru, Tuvalu, Tonga, Samoa, Palau, Marshall Islands

**Effort**: LOW. Download GADM level 0 for each territory. Can be scripted.

**GADM coverage**: GADM has separate entries for virtually ALL of these territories
at level 0. Natural Earth's `ne_10m_admin_0_countries` layer also includes most of
them, plus disputed areas and dependent territories.

**Key tool**: The `rnaturalearth` R package can pull these directly:
```r
rnaturalearth::ne_countries(scale = 10, type = "countries")
rnaturalearth::ne_countries(scale = 10, type = "sovereignty") # includes dependents
```

### Strategy 2: Combine Modern Country Borders (30 polities)

**Source**: GADM level 0 (country borders) or level 1 (admin subdivisions)

**What**: Colonial territories whose borders roughly match combinations of modern
countries or subnational units. The colonial territory polygon can be approximated
by dissolving/merging modern boundaries.

**Examples**:

| Colonial Polity | Modern Proxy |
|----------------|-------------|
| Belgian Congo | DRC (GADM level 0) |
| British East Africa | Kenya + Uganda |
| French West Africa | Senegal + Mali + Niger + Guinea + Côte d'Ivoire + Burkina Faso + Benin + Mauritania |
| French Equatorial Africa | Gabon + Congo + CAR + Chad |
| German East Africa (Tanganyika) | Tanzania mainland |
| Sudan (Anglo-Egyptian) | Sudan + South Sudan |
| Syria and Lebanon | Syria + Lebanon |
| Palestine/Jordan | Palestine + Jordan |
| Rwanda and Burundi | Rwanda + Burundi |
| Nyasaland Protectorate | Malawi |
| Basutoland | Lesotho |
| Bechuanaland Protectorate | Botswana |
| Gold Coast | Ghana |
| British Malaya | Peninsular Malaysia (GADM level 1) |
| Newfoundland | Newfoundland province (GADM Canada level 1) |
| Sarawak | Sarawak state (GADM Malaysia level 1) |
| Sabah (British Borneo) | Sabah state (GADM Malaysia level 1) |
| Manchukuo | Heilongjiang+Jilin+Liaoning+Inner Mongolia east (GADM China level 1) |
| Dutch East Indies | Indonesia |
| Dodecanese Islands | South Aegean region (GADM Greece level 2) |
| Italian Libia Cyrenaica | Libya |
| Spanish Guinea | Equatorial Guinea |
| New Hebrides | Vanuatu |
| British Protectorate (Somaliland) | Somaliland region of Somalia |
| Cameroon (Kamerun) | Cameroon (roughly) |

**Effort**: MEDIUM. Requires selecting the right GADM units, merging polygons in R/sf.
Some colonial borders don't perfectly match modern borders (e.g., German Kamerun
included parts of modern Chad/Congo/Nigeria that differ from modern Cameroon).

**Accuracy caveat**: Colonial boundaries were often different from modern boundaries.
This approach gives ~80-95% accuracy depending on the territory. For higher accuracy,
use the Aourednik Historical Basemaps or MPIDR Census Mosaic.

### Strategy 3: Historical GIS Data (25 polities)

**Source**: Multiple historical GIS datasets needed

**What**: These are polities that existed before 1886 (pre-CShapes) and whose
territory doesn't correspond to any modern entity. They need actual historical
boundary data.

**Polities requiring historical GIS**:

**Pre-unification Italian states** (ended 1860-1870):
- Papal States (PAP-1800-1870): ~41,000 km2 (central Italy)
- Two Sicilies (TWO-1800-1860): ~111,900 km2 (southern Italy + Sicily)
- Sardinia (SAR-1800-1860): ~74,000 km2 (NW Italy + Sardinia island)
- Tuscany (TUS-1800-1860): ~22,000 km2 (central Italy)
- Duchy Modena (DMO-1800-1860): ~6,400 km2
- Duchy Parma (DPA-1800-1860): ~6,200 km2

**Best source for Italian states**:
- Aourednik Historical Basemaps (1800 and 1850 snapshots have pre-unification states)
- Euratlas GIS (1800 and 1900 snapshots, paid)
- The MPIDR Census Mosaic may have Italian admin boundaries

**Pre-unification/pre-annexation Canadian provinces** (ended 1866):
- Lower Quebec, New Brunswick, Nova Scotia, Ontario, Prince Edward Island,
  Vancouver's Island, British Columbia
**Source**: GADM Canada level 1 (modern provinces) is close enough

**Central Asian khanates** (ended 1873-1920):
- Badakhshan (1800-1873), Khiva (1800-1873), Kokand (1800-1876), Bukhara (1800-1920)
**Source**: Aourednik 1850/1880 basemaps may include these

**Other historical entities**:
- Ionian Islands (1815-1864): Use GADM Greece level 2 for Ionian Islands region
- Herat (1800-1862): Historical Afghan principality, ~70,000 km2
- Yunnan (1856-1871): Modern Yunnan province, GADM China level 1
- Danish India (1800-1845): Tranquebar + Serampore, very small areas
- Bonin Islands (1846-1873): Modern Ogasawara, GADM Japan level 2
- St Martin/St Barthélemy (1816-1878): Modern island polygons from Natural Earth

**Effort**: HIGH for Italian states and Central Asian khanates. MEDIUM for others
(can use modern subnational boundaries as proxies).

### Strategy 4: Other Sources for CShapes-Range Dependents (44 polities)

**Source**: Natural Earth, GADM, Aourednik, or custom creation

**What**: These polities existed within the CShapes coverage period (1886-2019) but
CShapes doesn't include them because they were dependent territories, not sovereign states.

**Key subcategories**:

**Gulf Emirates (pre-UAE, pre-1971)**:
- Abu Dhabi, Ajman, Fujairah, Ras al Khaimah, Sharjah, Umm al Qawain
**Source**: GADM UAE level 1 gives modern emirate boundaries. These haven't changed
much since the Trucial States period.

**Small colonial concessions**:
- Kiautschou (German China lease, 1898-1922): ~552 km2 around Qingdao
- Kwang-Chou-Wan (French China lease): ~1,300 km2
- Kwantung/Port Arthur (Japanese lease): ~3,500 km2
**Source**: Would need manual GIS creation or historical maps

**Pacific territories**:
- Gilbert and Ellice Islands, Caroline Islands, etc.
**Source**: Natural Earth or GADM for the modern island groups

**African sub-territories**:
- British Bechuanaland (1885-1895): Northern Cape Province of South Africa (GADM)
- Gold Coast dependencies

**Other**:
- Crete (1898-1913): GADM Greece level 2 (Crete region)
- Manchukuo (1932-1945): Already covered in Strategy 2
- Netherlands Antilles: GADM for individual islands

**Effort**: MEDIUM. Most can be sourced from GADM subnational data.

## Available Polygon Data Sources (Detailed)

### Primary Sources (Free, Ready-to-Use)

| Source | URL | Format | Coverage | Territories? |
|--------|-----|--------|----------|-------------|
| **CShapes 2.0** | icr.ethz.ch/data/cshapes/ | Shapefile, R pkg | 1886-2019, sovereign + some dependent | YES (some) |
| **GADM 4.1** | gadm.org | Shapefile, GeoJSON, GeoPackage | Current, all countries, L0-L5 | YES (all) |
| **Natural Earth 10m** | naturalearthdata.com | Shapefile | Current, world | YES (dependents, disputed) |
| **Aourednik** | github.com/aourednik/historical-basemaps | GeoJSON | 54 snapshots, BCE-2010 | YES (empires, colonies) |
| **rnaturalearth** R pkg | CRAN | sf objects | Current | YES |

### Secondary Sources (Free, Require Processing)

| Source | URL | Coverage | Notes |
|--------|-----|----------|-------|
| **MPIDR Census Mosaic** | mosaic.ipums.org | Europe 1837-2003 | Registration required |
| **COW Territorial Change v6** | correlatesofwar.org | 1816-2018 (tabular) | No polygons, but areas |
| **Cliopatria** | cliopatria.io | All history | Areas only, no polygons |
| **Thenmap API** | thenmap.net | 1945-present | REST API, GeoJSON |
| **IPUMS IHGIS** | ihgis.ipums.org | Various census dates | Boundary shapefiles |
| **Wimmer Empire-to-Nation** | awimmer.com/data | 1816-2001 (tabular) | No polygons |

### Historical Map Sources (Images for Georeferencing)

| Source | URL | Content |
|--------|-----|---------|
| **David Rumsey Map Collection** | davidrumsey.com | 150,000+ historical maps, many georeferenced |
| **Old Maps Online** | oldmapsonline.org | Aggregator of many map collections |
| **Library of Congress** | loc.gov/maps/ | Extensive historical map collection |
| **British Library Maps** | bl.uk/collection-guides/maps | Colonial-era maps |
| **Gallica (BnF)** | gallica.bnf.fr | French colonial maps |

## Recommended Implementation Plan

### Phase 1: Quick Wins (96 polities, ~1 week effort)
Use GADM/Natural Earth for all modern small territories.
- Download GADM level 0 for each territory
- Match to polity codes via ISO3 codes
- Script in R using `geodata::gadm()` or `rnaturalearth`

### Phase 2: Colonial Proxies (30 polities, ~1-2 weeks effort)
Combine modern country/subnational borders for colonial territories.
- Build a mapping table: colonial polity -> list of modern GADM units
- Dissolve boundaries in R/sf
- Manual verification against historical maps

### Phase 3: CShapes-Range Dependents (44 polities, ~2-3 weeks effort)
Use GADM subnational data + Natural Earth for dependent territories.
- Gulf emirates from GADM UAE level 1
- Pacific islands from Natural Earth
- Historical concessions (Kiautschou, etc.) may need manual creation

### Phase 4: Historical States (25 polities, ~3-4 weeks effort)
Use Aourednik Historical Basemaps + Euratlas + custom GIS.
- Italian pre-unification states from Aourednik 1850 snapshot
- Central Asian khanates from Aourednik 1850/1880
- Canadian provinces from GADM Canada level 1
- Remaining entities: manual creation from historical maps

### Phase 5: Regions (118 polities, optional)
Aggregate regions (Africa, Asia, EU, etc.) can be built by dissolving
member country polygons using M49/FAOSTAT grouping definitions.

## Total Effort Estimate

| Phase | Polities | Effort | Source |
|-------|----------|--------|--------|
| Phase 1 | 96 | LOW (scriptable) | GADM + Natural Earth |
| Phase 2 | 30 | MEDIUM | GADM merge/dissolve |
| Phase 3 | 44 | MEDIUM-HIGH | GADM L1/L2 + manual |
| Phase 4 | 25 | HIGH | Aourednik + manual GIS |
| Phase 5 | 118 | LOW (scriptable) | Dissolve member countries |
| **Total** | **313** | **~8-10 weeks** | Multiple sources |

With all phases complete, **100% of polities would have geometry**.
