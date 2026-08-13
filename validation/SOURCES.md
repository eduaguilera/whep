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
| cropping_intensity | GAEZ v4 multiple cropping zones (mcr/mci) | open (auto-download) | `cache/ground_truth/cropping_intensity.json` | `gaez_potential.R` |
| occupation / land_per_tonne | Poore & Nemecek 2018 (LCA, m²·yr/kg) | open | `cache/ground_truth/occupation.json` | manual (cited) |
| cycle_length | GGCMI Phase 3 crop calendar | open (Zenodo 5062513) | `cache/ground_truth/cycle_length.json`, `cache/data/GGCMI/` | manual + `ncdf4` |
| stability | none (WHEP's own series) | — | — | `stability.R` |

## Packaged BNF coefficient provenance

`inst/extdata/coefs/bnf_provenance.csv` is the tracked, cell-level registry for
the 60 non-missing numeric cells in `bnf.csv`. Publication locators and exact
source identities are stored with each cell; unresolved citations remain
explicit and do not authorize a coefficient correction.

| Source ID | Exact identity | DOI | Provenance role |
|---|---|---|---|
| `anglade_2015` | Anglade et al. (2015), *Relationships for estimating N2 fixation in legumes: incidence for N balance of legume-based cropping systems in Europe*, Ecosphere 6(3):37 | 10.1890/ES14-00353.1 | Direct Table 1, discussion and Table 5 assertions |
| `herridge_2008` | Herridge et al. (2008), *Global inputs of biological nitrogen fixation in agricultural systems*, Plant and Soil 311:1-18 | 10.1007/s11104-008-9668-3 | Direct Table 2 and Table 6 assertions; Table 4 is dry-matter HI, not NHI |
| `lassaletta_2014_erl_s1` | Lassaletta et al. (2014), *50 year trends in nitrogen use efficiency of world cropping systems: the relationship between yield and nitrogen input to cropland*, Environmental Research Letters 9:105011, Supplementary Methods | 10.1088/1748-9326/9/10/105011 | Immediate Table S1-2 authority; Table S1-3 supplies adjacent non-symbiotic context |
| `salvagiotti_2008` | Salvagiotti et al. (2008), *Nitrogen uptake, fixation and response to fertilizer N in soybeans: a review* | 10.1016/j.fcr.2008.03.001 | Underlying soybean citation; immediate stored-value authority remains Lassaletta Table S1-2 |
| `luscher_2014` | Luscher et al. (2014), *Potential of legume-based grassland-livestock systems in Europe: a review* | 10.1111/gfs.12124 | Mixed-sward/meadow context; no exact WHEP vector derived |
| `suter_2015` | Suter et al. (2015), *Nitrogen yield advantage from grass-legume mixtures is robust over a wide range of legume proportions and environmental conditions* | 10.1111/gcb.12880 | Mixed-sward context; no universal 0.25 share asserted |
| `nyfeler_2011` | Nyfeler et al. (2011), *Grass-legume mixtures can yield more nitrogen than legume pure stands due to mutual stimulation of nitrogen uptake from symbiotic and non-symbiotic sources* | 10.1016/j.agee.2010.11.022 | Mixed-sward context; no exact WHEP vector derived |
| `ledgard_2001_ambiguous` | Candidate identities: *Nitrogen fixation by white clover in pastures grazed by dairy cows*; *Nitrogen cycling in low input legume-based agriculture* | 10.1023/A:1004833804002; 10.1023/A:1004810620983 | Citation remains ambiguous; meadow cells unresolved |
| `roscher_2011` | Roscher et al. (2011), *N2 fixation and performance of 12 legume species in a 6-year grassland biodiversity experiment* | 10.1007/s11104-010-0647-0 | Meadow context; no universal coefficient asserted |
| `espigares_peco_1993` | Espigares & Peco (1993), *Mediterranean annual pasture dynamics: the role of germination* | not established | Fallow context; cells unresolved |
| `cirujeda_2011_candidate` | Best candidate: Cirujeda et al. (2011), *Remarkable changes of weed species in Spanish cereal fields from 1976 to 2007* | 10.1007/s13593-011-0030-4 | Underspecified fallow/weed citation; cells unresolved |
| `storkey_2012` | Storkey et al. (2012), *The impact of agricultural intensification and land-use change on the European arable flora* | 10.1098/rspb.2011.1686 | Weed-flora context; no exact 0.05 share asserted |
| `fried_2009_ambiguous` | Candidate identities: *A functional analysis of shifts in sunflower weed assemblages*; *Arable weed decline in Northern France* | 10.1111/j.1654-1103.2009.05284.x; 10.1016/j.biocon.2008.09.029 | Citation remains ambiguous; weed cells unresolved |

## Commodity mass-basis and conversion sources

Cited at the point of use in `R/read_raw_inputs.R`, and exercised by
`validation/rice_mass_basis.R`.

| Source | Exact identity | Access | Provenance role |
|---|---|---|---|
| FAO TCF | FAO, *Technical Conversion Factors for Agricultural Commodities* (provisional issue; rates are 1992-1996 national annual averages, stated in REMARKS item 1 on PDF p.2) | open, <https://www.fao.org/fileadmin/templates/ess/documents/methodology/tcf.pdf> | Authority for the paddy-to-milled rice extraction rate. National rates under "Milled Paddy Rice": median 65%, range 60-73 (China mainland 67, India 66, Bangladesh 67, Pakistan 67, Thailand 66, USA 72). WHEP applies a single global 0.67. |
| FAO FBS Handbook | FAO (2001), *Food Balance Sheets: A Handbook*, Section III | open, <https://www.fao.org/4/x9892e/X9892e03.htm> | Method authority: the worked example uses a 67% rice extraction rate and a 75% wheat-flour rate, and computes nutrients from food-composition percentages rather than from nitrogen. |

Mass basis by FAOSTAT vintage, verified directly against the pins at India 2010
production, and the reason the two must be treated differently (#751):

| Pin | Item | Name | India 2010 production | Basis |
|---|---|---|---|---|
| `faostat-fbs-new` | 2807 | Rice and products | 143,963 kt | paddy |
| `faostat-fbs-old` | 2805 | Rice (Milled Equivalent) | 96,023 kt | milled |
| `faostat-cbs-old-crops` | 2804 / 2805 | both | 143,963,008 / 96,023,326 t | both |

## Nourishment requirement sources

Cited at the point of use in `R/protein_requirement.R` and carried as the
packaged coefficient table `inst/extdata/coefs/protein_requirement.csv`.

| Source | Exact identity | Access | Provenance role |
|---|---|---|---|
| WHO/FAO/UNU TRS 935 | WHO/FAO/UNU (2007), *Protein and amino acid requirements in human nutrition*, WHO Technical Report Series 935, ISBN 9241209356 | open, <https://iris.who.int/handle/10665/43411> | The whole requirement side. **Table 46** (report p.243) adult safe level, 0.83 g/kg per day at PDCAAS 1.0, 46 g/day at 55 kg. **Table 47** (p.244) per-class safe level in g/day at WHO reference weights. **Tables 33a/33b** (pp.176-177) the *average* requirement per kg by single year of age, which is the anchor the report names for population use (p.41). |

Two derivation notes, both load-bearing:

- The packaged `avg_req_g_day` is **Table 47's published g/day times the
  average-to-safe ratio from Tables 33a/33b**, not `avg_req_g_kg_day` times the
  reference weight. The latter reproduces Table 47 for every child class but
  fails for adolescents (15-18 boys: 55.5 against 57.9 published), because body
  weight rises steeply inside an adolescent class so mean(g/kg) x mean(weight)
  is not mean(g/kg x weight), and TRS 935 does not publish the per-year weights
  it used.
- The average-to-safe ratio is **per class**, spanning 0.8077-0.8550 for child
  classes against 0.7952 for adults. Applying the adult ratio uniformly, as a
  first pass did, carries up to 7% error.

TRS 935 forbids the construction WHEP previously used: "reference intake or safe
intake levels defined as above for individuals have been **incorrectly applied
to populations**" (p.41), and "a safe population intake **cannot be defined as a
simple function of the mean requirement**" (p.241).

## External local datasets (paths in `cache/local_paths.json`, gitignored)

| Key | Dataset | Location |
|---|---|---|
| `WHEP_NASS_DIR` | USDA NASS QuickStats bulk (crops.csv ~8 GB) | `~/Nextcloud/WHEP_ERC 2025/.../NASS` |

(GAEZ is no longer a required local dataset: `gaez_potential.R` auto-downloads the
GAEZ v4 multiple-cropping-zone layers from the open FAO bucket; `WHEP_GAEZ_DIR`
remains an optional override pointing at locally-held layers by basename.)

## Open datasets queued / wired (no license needed)

| Dataset | For | Access | Status |
|---|---|---|---|
| USDA FAS PSD | global production / area cross-check | open CSV (auto) | **wired** (`psd_production.R`) → `production_psd` |
| MapSPAM (SPAM2010 v2r0) | observed cropping intensity (harvested/physical) | open via Dataverse **API** (the guestbook only gates the HTML UI; `/api/access/datafile/{id}?format=original` → S3, no login) | **wired** (`spam_intensity.R`) → `cropping_intensity_obs` (362/377). file IDs 3984973 (phys) / 3984976 (harv) |
| Agribalyse 3.2 | 2nd LCA occupation source | data.gouv (summary) open; LCI gated portal | **not wired** — open files have only EF *points* / a blank methodology template. The per-crop m²·yr flow is in the ecospold2/OpenLCA LCI datasets on agribalyse.ademe.fr (obtain those, extract "Occupation, arable land"). Lower priority — P&N + land_per_tonne already cover occupation. |

## Licensed (you obtain)

ecoinvent, WFLDB (paid); GFLI (free email license). Only needed to add more LCA
occupation references beyond Poore & Nemecek + Agribalyse.
