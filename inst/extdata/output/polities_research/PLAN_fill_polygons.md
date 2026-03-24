# PLAN: Fill All Missing Polity Polygons

**Run on a machine with enough RAM (the cshapes R package loads large sf objects)**

## Overview

| Step | Polities Covered | Source | Effort |
|------|-----------------|--------|--------|
| Step 1: CShapes `dependencies = TRUE` | ~49 colonial + ~? pre-1886 European | CShapes R package | Small code change + mapping work |
| Step 2: GADM direct download | 61 modern territories | GADM 4.1 | Script + ISO3 lookup |
| Step 3: GADM manual match | 26 FAOSTAT/FT aggregates | GADM L0/L1 | Manual mapping |
| Step 4: GADM subnational | ~36 historical/sub-entities | GADM L1/L2 | Manual mapping |
| Step 5: Historical entities | ~24 pre-1886 non-European | GADM L1 or Aourednik | Medium effort |
| **Total** | **~195 + bonus pre-1886** | | |

---

## STEP 1: Enable CShapes Dependencies (HIGH PRIORITY)

### 1.1 Change the code

File: `R/polities.R`, line 497

```r
# BEFORE:
countries <- cshapes::cshp() |>
    sf::st_make_valid()

# AFTER:
countries <- cshapes::cshp(dependencies = TRUE) |>
    sf::st_make_valid()
```

### 1.2 Explore what CShapes returns

Run this FIRST to understand the new data before integrating:

```r
library(cshapes)
library(dplyr)
library(sf)

# Load with dependencies
cs_all <- cshp(dependencies = TRUE) |> st_make_valid()
cs_ind <- cshp(dependencies = FALSE) |> st_make_valid()

cat("Independent only:", nrow(cs_ind), "\n")
cat("With dependencies:", nrow(cs_all), "\n")
cat("Dependencies:", nrow(cs_all) - nrow(cs_ind), "\n")

# See all dependent territories
deps <- cs_all |> filter(status != "independent")
cat("\nDependent territory statuses:\n")
print(table(deps$status))

# List all unique dependency names
dep_names <- deps |>
  distinct(country_name) |>
  arrange(country_name)
cat("\nAll dependency names:\n")
print(dep_names, n = 200)

# Check if pre-1886 entries exist (European extension)
pre_1886 <- cs_all |>
  filter(lubridate::year(start) < 1886)
cat("\nEntries starting before 1886:", nrow(pre_1886), "\n")
if (nrow(pre_1886) > 0) {
  cat("Pre-1886 names:\n")
  pre_1886 |>
    distinct(country_name, .keep_all = TRUE) |>
    select(country_name, start, end, status) |>
    print(n = 100)
}

# SAVE the full list for mapping work
deps |>
  st_drop_geometry() |>
  select(country_name, start, end, status, owner, gwcode, cowcode) |>
  write.csv("data-raw/polities_inputs/cshapes_dependencies_inventory.csv",
            row.names = FALSE)
```

### 1.3 Build the mappings

After running 1.2, you'll have a CSV of all CShapes dependency names. For each one
that matches a WHEP polity:

**`rename_cshapes.csv`**: Add a row mapping the CShapes `country_name` to the
name used in `common_names.csv`. Example:
```csv
country_name,common_name
Belgian Congo,Belgian Congo
British Malaya,British Malaya
...
```

**`common_names.csv`**: The `.prepare_cshapes()` function appends year ranges to
names via `.add_years_in_name()`, so the CShapes name for "Belgian Congo" becomes
e.g. "Belgian Congo (to 1960)" if it has an end year. Add entries like:
```csv
Belgian Congo (to 1960),cshapes,Belgian Congo
```

Match each to the appropriate existing `common_name` in `polity_codes.csv`.

### 1.4 Handle .filter_relevant_area_changes()

The current filter groups by `country_name` and aggregates entries with no area change.
This works fine for sovereign states but verify it works for dependencies too.
Check that:
- Dependencies aren't accidentally merged with their owner state
- The area-change filter doesn't discard dependencies with stable borders

### 1.5 Rebuild sysdata.rda

After all mapping changes, rerun `data-raw/constants.R` to regenerate the internal
data. The `k_cshapes` object will now be larger (includes dependency geometries).

---

## STEP 2: GADM Direct Download (61 modern small territories)

These polities have direct GADM ISO3 codes. Download level 0 for each.

### 2.1 R Script

```r
library(geodata)  # or use direct download from gadm.org
library(sf)
library(dplyr)

# ISO3 codes for all 61 territories
gadm_iso3 <- c(
  "AND", "AIA", "ATA", "ATG", "BMU", "BVT", "IOT", "VGB", "CYM",
  "CXR", "CCK", "COK", "DMA", "GUF", "PYF", "ATF", "GIB", "GRL",
  "GLP", "GUM", "HMD", "VAT", "IMN", "KIR", "LIE", "MAC", "MTQ",
  "MCO", "MSR", "PRI", "SMR", "STP", "SJM", "TON", "TCA", "TUV",
  "ESH", "REU", "PCN", "WLF", "NCL", "MHL", "TKL", "NRU", "ASM",
  "NIU", "WSM", "SYC", "ABW", "MNP", "PLW", "PSE", "MYT", "ALA",
  "GGY", "JEY", "BLM", "MAF", "BES", "CUW", "SXM"
)

# Download and combine
gadm_polys <- lapply(gadm_iso3, function(iso) {
  tryCatch({
    g <- geodata::gadm(country = iso, level = 0, path = tempdir())
    st_as_sf(g) |>
      select(geometry) |>
      mutate(iso3 = iso) |>
      st_cast("MULTIPOLYGON")
  }, error = function(e) {
    message("Failed: ", iso, " - ", e$message)
    NULL
  })
})

gadm_sf <- bind_rows(Filter(Nonnull, gadm_polys))
st_write(gadm_sf, "data-raw/polities_inputs/gadm_small_territories.gpkg")
```

### 2.2 ISO3 to Polity Mapping

Create a CSV mapping ISO3 -> polity_name for the pipeline:

```csv
iso3,polity_name
AND,Andorra
AIA,Anguilla
ATA,Antarctica
ATG,Antigua and Barbuda
...
```

Save as `data-raw/polities_inputs/gadm_polity_mapping.csv`.

### 2.3 Fallback for missing GADM entries

If any ISO3 code fails (Bouvet Island, Heard/McDonald, some tiny islands):
```r
# Use Natural Earth instead
library(rnaturalearth)
ne <- ne_countries(scale = 10, type = "map_units", returnclass = "sf")
# Filter for the missing territory by name or ISO code
```

---

## STEP 3: GADM Manual Match (26 aggregate/special entities)

These are FAOSTAT/FT aggregate entities that need manual GADM matching.

| Polity | GADM Source | Details |
|--------|-----------|---------|
| Canary Islands | GADM ESP L1 | Filter: NAME_1 = "Islas Canarias" |
| Ceuta Y Melilla | GADM ESP L2 | Filter: NAME_2 in ("Ceuta", "Melilla") |
| China, mainland | GADM CHN L0 | Minus TWN |
| Danish Virgin Islands | GADM VIR L0 | (now US Virgin Islands) |
| Dutch West Indies | GADM ABW+CUW+SXM+BES L0 | Dissolve all 4 |
| Ethiopia (old) | GADM ETH+ERI L0 | Dissolve |
| Germany/Zollverein | GADM DEU L0 | Modern Germany |
| Leeward Islands | GADM ATG+DMA+KNA+MSR+VGB L0 | Dissolve |
| Luxembourg (old) | GADM LUX L0 | |
| Mariana Island | GADM MNP L0 | |
| Netherlands (old) | GADM NLD L0 | |
| Nigeria (old) | GADM NGA L0 | |
| Portuguese India | GADM IND L2 | Filter: Goa, Daman, Diu districts |
| Saint Helena etc. | GADM SHN L0 | |
| Saint Kitts and Nevis | GADM KNA L0 | |
| Saint Lucia | GADM LCA L0 | |
| Saint Pierre and Miquelon | GADM SPM L0 | |
| Saint Vincent etc. | GADM VCT L0 | |
| Sharjah | GADM ARE L1 | Filter: NAME_1 = "Sharjah" |
| Sikkim | GADM IND L1 | Filter: NAME_1 = "Sikkim" |
| South Georgia etc. | GADM SGS L0 | |
| St. Helena | GADM SHN L0 | (same as Saint Helena) |
| Tibet (old) | GADM CHN L1 | Filter: NAME_1 = "Xizang" |
| US Minor Outlying Islands | GADM UMI L0 | |
| US Virgin Islands | GADM VIR L0 | |
| Vanuatu | GADM VUT L0 | |

### R Script Template

```r
# For subnational units:
gadm_esp <- geodata::gadm(country = "ESP", level = 1, path = tempdir()) |> st_as_sf()
canary <- gadm_esp |> filter(NAME_1 == "Islas Canarias")

# For multi-country dissolves:
gadm_eth <- geodata::gadm(country = "ETH", level = 0, path = tempdir()) |> st_as_sf()
gadm_eri <- geodata::gadm(country = "ERI", level = 0, path = tempdir()) |> st_as_sf()
ethiopia_old <- st_union(gadm_eth, gadm_eri)
```

---

## STEP 4: Other Entities Needing GADM Subnational (36 polities)

| Polity | GADM Source | Details |
|--------|-----------|---------|
| Abu Dhabi | GADM ARE L1 | Filter: NAME_1 = "Abu Zaby" |
| Ajman | GADM ARE L1 | Filter: NAME_1 = "'Ajman" |
| Umm al Qawain | GADM ARE L1 | Filter: NAME_1 = "Umm al Qaywayn" |
| Ras al Khaimah | GADM ARE L1 | Filter: NAME_1 = "Ra's al Khaymah" |
| Canton/Enderbury Islands | GADM KIR L1 | Phoenix Islands subset |
| Channel Islands | GADM GGY+JEY L0 | Dissolve |
| Dronning Maud Land | NatEarth ATA | Antarctic claim sector |
| East Berlin | GADM DEU L2 | Eastern Berlin districts |
| West Berlin | GADM DEU L2 | Western Berlin districts |
| Gaza Strip | NatEarth PSX | Gaza polygon |
| Johnston Island | GADM UMI L1 | Filter by name |
| Midway Islands | GADM UMI L1 | Filter by name |
| Netherlands Antilles (original) | GADM ABW+CUW+SXM+BES L0 | Dissolve all |
| Netherlands Antilles | GADM CUW+SXM+BES L0 | Dissolve (minus Aruba) |
| Neutral Zone | Manual | Saudi-Kuwait neutral zone polygon |
| Norfolk Island | GADM NFK L0 | |
| Panama (before Canal Zone) | GADM PAN L0 | (same as modern Panama) |
| Ryukyu Islands | GADM JPN L1 | Filter: NAME_1 = "Okinawa" |
| Saint Kitts-Nevis-Anguilla | GADM KNA+AIA L0 | Dissolve |
| Spanish North Africa | GADM ESP L2 | Ceuta + Melilla |
| US misc Pacific Islands | GADM UMI L0 | |
| British settlement Oceania | Multiple | Fiji+Solomons+etc |
| US settlement Oceania | Multiple | US Pacific possessions |
| Mayotte and Noissi-be | GADM MYT+MDG L2 | Mayotte + Nosy Be |
| Marquesas Island | GADM PYF L2 | Marquesas subdivision |
| Society Islands | GADM PYF L2 | Tahiti subdivision |
| Gambier Island | GADM PYF L2 | Gambier subdivision |
| Caroline Island | GADM FSM+PLW L0 | Dissolve |
| Gilbert and Ellice Island | GADM KIR+TUV L0 | Dissolve |
| Palmyra Island | GADM UMI L1 | Palmyra subset |
| Wake Island | GADM UMI L1 | Wake subset |
| Kwang-Chou-Wan | Manual | Guangzhou Bay ~1300 km2 |
| Saudi Arabia (1924-1932) | CShapes or GADM SAU L0 | Pre-unification Hejaz+Nejd |
| Developed/Developing regions | Skip | Statistical categories, no polygon |

---

## STEP 5: Historical Entities (24 pre-1886 non-European)

Most of these can use GADM subnational boundaries of their modern successor.

| Polity | Proxy Source | Details |
|--------|-------------|---------|
| Badakhshan | GADM AFG L1 | Badakhshan province |
| Khiva | Aourednik 1850 | Or GADM UZB L1 Khorezm region |
| Kokand | Aourednik 1850 | Or GADM UZB+KGZ L1 Fergana regions |
| Danish India | Manual | Tranquebar (~15 km2), Serampore (~1 km2) |
| Grenada | GADM GRD L0 | |
| St. Lucía | GADM LCA L0 | |
| St. Vincent | GADM VCT L0 | |
| Brunei (greater) | GADM BRN+MYS L0/L1 | Brunei + parts Sabah/Sarawak |
| St Martin and St Barthélemy | GADM MAF+BLM L0 | Dissolve |
| Van Diemen's Land (Tasmania) | GADM AUS L1 | Tasmania state |
| South Australia | GADM AUS L1 | South Australia state |
| Western Australia | GADM AUS L1 | Western Australia state |
| Victoria | GADM AUS L1 | Victoria state |
| Queensland | GADM AUS L1 | Queensland state |
| Bonin island | GADM JPN L2 | Ogasawara subprefecture |
| Vancouver's Island | GADM CAN L2 | Vancouver Island portion of BC |
| British Columbia | GADM CAN L1 | British Columbia |
| Lower Quebec | GADM CAN L1 | Quebec |
| New Brunswick | GADM CAN L1 | New Brunswick |
| Nova Scotia/Cape Breton | GADM CAN L1 | Nova Scotia |
| Ontario | GADM CAN L1 | Ontario |
| Prince Edward Island | GADM CAN L1 | Prince Edward Island |
| Yunnan | GADM CHN L1 | Yunnan province |
| British Bechuanaland | GADM ZAF L1 | North West province (approx) |

---

## STEP 6: Pipeline Integration

### Option A: New Geometry Source (Recommended)

Create a new source `gadm` alongside the existing 5 sources:

1. Create `data-raw/polities_inputs/gadm_geometries.gpkg` — GeoPackage with
   all GADM/NatEarth polygons keyed by polity_name
2. Create `.prepare_gadm_geometries()` function in `R/polities.R`
3. Add to `.merge_datasets()`:
   ```r
   .merge_datasets <- function() {
     dplyr::bind_rows(
       .prepare_historical_m49(),
       .prepare_faostat(),
       .prepare_federico_tena(),
       .prepare_cshapes(),
       .prepare_whep_fixes(),
       .prepare_gadm_geometries()  # NEW
     )
   }
   ```
4. Add `common_names.csv` entries with source `gadm`
5. The aggregation logic will pick up the geometry automatically

### Option B: Post-Pipeline Fill

After `get_polities()`, fill empty geometries:
```r
polities <- get_polities()
gadm_fill <- st_read("data-raw/polities_inputs/gadm_geometries.gpkg")
# Replace empty geometries with GADM ones
```

This is simpler but less integrated.

### Recommendation

**Option A** is cleaner because it keeps everything in one pipeline. The GADM
geometries flow through the same aggregation logic as CShapes geometries.

---

## Execution Checklist

- [ ] Step 1.2: Run CShapes exploration script, save dependency inventory
- [ ] Step 1.3: Build rename_cshapes.csv and common_names.csv entries for dependencies
- [ ] Step 1.4: Verify .filter_relevant_area_changes() works for dependencies
- [ ] Step 1.5: Rebuild sysdata.rda
- [ ] Step 1.6: Run tests, verify pipeline still works
- [ ] Step 2.1: Download GADM L0 for 61 ISO3 codes
- [ ] Step 2.2: Create ISO3-to-polity mapping CSV
- [ ] Step 3: Download and extract GADM subnational for 26 entities
- [ ] Step 4: Download and extract GADM subnational for 36 entities
- [ ] Step 5: Handle 24 historical entities (GADM subnational or Aourednik)
- [ ] Step 6: Integrate into pipeline (new source or post-fill)
- [ ] Final: Run tests, verify all 601 polities have non-empty geometry
- [ ] Final: Verify area calculations are reasonable
- [ ] Final: Visual spot-check in a map viewer

## Memory/Disk Notes for Tomorrow

- `cshapes::cshp(dependencies = TRUE)` loads ALL CShapes data into memory (~100-200 MB)
- GADM level 0 files are ~1-10 MB each; total for 61 territories: ~200 MB download
- GADM level 1 for large countries (China, India, Australia): ~50 MB each
- The final GeoPackage with all polygons will be ~50-100 MB
- Recommend at least **8 GB RAM** for the R session
- Use `tempdir()` for GADM downloads to avoid cluttering the repo
- The final sysdata.rda will be larger (currently 4 MB, may grow to 10-20 MB)
