# Validation sources & layout

All data we compare WHEP against lives under **`validation/cache/`**, which is
**gitignored in full** — nothing here is committed. Only code, this manifest,
and the registries are tracked.

```
validation/
  *.R, README.md            committed — harness code
  sources.json              committed — subnational source registry (grown by the workflow)
  SOURCES.md                committed — this manifest
  cache/                    GITIGNORED (all data)
    data/<SOURCE>/          raw downloaded source data
    ground_truth/<var>.json pinned values WHEP is compared against
    findings/<iso3>.json    WHEP-side extracted values (per country)
    local_paths.json        machine paths to external local datasets
```

Reproduce anything under `cache/` from the sources below; none of it needs to be
committed.

## Ground truth in use (per variable)

| Variable | Source dataset | Access | Lives in | Built by |
|---|---|---|---|---|
| production / area | USDA NASS QuickStats bulk (USA); SEDAC, IBGE, BPS (others, via workflow) | local / open | `cache/data/<iso3>/`, `cache/findings/` | `nass_sum.R`, `subnational.workflow.js` |
| cropping_intensity | GAEZ v3 multi-cropping suitability (LandInG) | local | `cache/ground_truth/cropping_intensity.json` | `gaez_potential.R` |
| occupation / land_per_tonne | Poore & Nemecek 2018 (LCA, m²·yr/kg) | open | `cache/ground_truth/occupation.json` | manual (cited) |
| cycle_length | GGCMI Phase 3 crop calendar | open (Zenodo 5062513) | `cache/ground_truth/cycle_length.json`, `cache/data/GGCMI/` | manual + `ncdf4` |
| stability | none (WHEP's own series) | — | — | `stability.R` |

## External local datasets (paths in `cache/local_paths.json`, gitignored)

| Key | Dataset | Location |
|---|---|---|
| `WHEP_NASS_DIR` | USDA NASS QuickStats bulk (crops.csv ~8 GB) | `~/Nextcloud/WHEP_ERC 2025/.../NASS` |
| `WHEP_GAEZ_DIR` | GAEZ v3 multi-cropping suitability rasters | `~/LandInG/landuse/tmp/work_30min` |

## Open datasets queued / wired (no license needed)

| Dataset | For | Access | Status |
|---|---|---|---|
| USDA FAS PSD | global production / area cross-check | open CSV (auto) | **wired** (`psd_production.R`) → `production_psd` |
| MapSPAM (SPAM2010 v2r0) | observed cropping intensity (harvested/physical) | open via Dataverse **API** (the guestbook only gates the HTML UI; `/api/access/datafile/{id}?format=original` → S3, no login) | **wired** (`spam_intensity.R`) → `cropping_intensity_obs` (362/377). file IDs 3984973 (phys) / 3984976 (harv) |
| Agribalyse 3.2 | 2nd LCA occupation source | data.gouv (summary) open; LCI gated portal | **not wired** — open files have only EF *points* / a blank methodology template. The per-crop m²·yr flow is in the ecospold2/OpenLCA LCI datasets on agribalyse.ademe.fr (obtain those, extract "Occupation, arable land"). Lower priority — P&N + land_per_tonne already cover occupation. |

## Licensed (you obtain)

ecoinvent, WFLDB (paid); GFLI (free email license). Only needed to add more LCA
occupation references beyond Poore & Nemecek + Agribalyse.
