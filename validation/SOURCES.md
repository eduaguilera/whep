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

## Nourishment floor composition sources

Cited at the point of use in `R/nourishment_floor.R`. The floor composes the
requirement, dispersion and loss-wedge terms; these are the parameters the
composition itself adds.

| Source | Exact identity | Access | Provenance role |
|---|---|---|---|
| TRS 935 Box 1 | WHO/FAO/UNU (2007), TRS 935, Box 1, p.44 | open, <https://iris.who.int/handle/10665/43411> | The whole model. Verbatim: log(deficit) is normal with mean `M_D = M_I - M_R` and SD `S_D = sqrt(S_I^2 + S_R^2 - 2 R S_I S_R)`; prevalence of deficit is `Phi(-M_D/S_D)`. |
| TRS 935 `S_R` | Same, p.38 section 3.2.4: "ln (requirement) ~ normal (mean = 4.654, SD = 0.12)" | open | `requirement_sd = 0.12`, the log-scale spread of the adult requirement on a per-kilogram basis. The report also states (p.109 section 7.3, p.123 section 7.9) that this is only about a fifth of observed between-individual variance, ">80% ... could reflect a lack of energy balance" — which is why the argument is exposed rather than fixed. |
| TRS 935 `R = 0` | Same, p.44 and Table 2 heading "Zero correlation assumed" | open | The zero-correlation case of Box 1, reducing `S_D` to the quadrature sum. It is TRS 935's **assumption**, not a measurement: a positive intake-requirement correlation would shrink `S_D` and lower every floor. |
| TRS 935 `p*` | Same, Figure 7 header (p.46): "Safe population intake ie. risk<2.5%" | open | The 2.5% default tolerated shortfall. |
| FAO SDG 2.1.1 metadata | FAO, SDG indicator 2.1.1 metadata: "2.5% is the lowest feasible target that can be set for the PoU indicator" | open | The independent second anchor on 2.5%. Two sources fix it, so departing from 2.5% is the science decision and adopting it follows the source. |

### Protein quality

Cited at the point of use in `R/protein_quality.R`, with the animal/plant
classification packaged as `inst/extdata/coefs/protein_digestibility.csv`.

| Source | Exact identity | Access | Provenance role |
|---|---|---|---|
| TRS 935 quality anchor | Same, section 14.2 (printed p.242) and Table 46 footnote b (p.243) | open | Why the correction exists at all: the 0.83 g/kg per day safe level is issued "for proteins with a protein digestibility-corrected amino acid score value of **1.0**". No real diet reaches 1.0, so every uncorrected band is low by at least `1/D`. |
| TRS 935 Table 43 | Same, footnote b (printed p.214) | open | The two rates. Diet digestibility is "the weighted mean of **95%** and **80%** for **animal** and **plant** protein sources respectively". |
| TRS 935 Table 6 | Same, printed p.100 | open | That digestibility is additive over protein: it is computed there as "sum of digestible protein/total protein". Confirmed empirically for standardized ileal AA digestibility by Fanelli, Bailey, Guardiola & Stein, *J Nutr* 2021, \doi{10.1093/jn/nxaa398} — with the caveat, from the same paper, that **apparent** digestibility is "not always additive", so the basis must be true faecal or standardized ileal. |
| FAOSTAT FBS grouping | FAOSTAT Food Balance Sheets, items 2941 Animal Products and 2903 Vegetal Products | open | The animal/plant split itself, so it is FAO's own and not WHEP's opinion. It reconciles: on the 2010 world basket the packaged classification sums to 108.165 Mt animal against FAO's published 108.239, and 160.327 Mt plant against 160.398 — within 0.07% on each side. Note aquatic plants (2775) sit inside FAO's Animal Products grouping; following FAO is what makes the totals reconcile. |

This is **tier 1b of four** on the fidelity ladder, and it is a *provable lower
bound* on the full correction, because PDCAAS is `min(1, AAS) x D` which never
exceeds `D`. So it is conservative about the **size of the correction** and
therefore anti-conservative about adequacy: it under-corrects, and classifies
fewer countries as deficient than the full amino acid score would. Tier 2 needs
a per-item amino acid composition table WHEP does not have (~88 items x 4
amino acids); tier 3, true ileal DIAAS, FAO states is not obtainable — FNP 92
p.4: "currently available data are insufficient to support the application in
practice ... of true ileal amino acid digestibility".

**Never average per-item scores.** FAO forbids it twice in words — TRS 935 p.99
"the amino acid score for food mixtures should be calculated from the weighted
average digestible amino acid content", FNP 92 p.17 the same — and FNP 51 p.37
gives the reason: "the score of a mixture cannot always be calculated with
certainty from a knowledge of the individual scores of the components. Because
of the complementary potential between proteins". Digestibility is a genuine
protein-weighted mean and is computed as one; the amino acid score is not.

### Protein quality, tier 2 (amino acid scoring)

Cited at the point of use in `R/protein_score.R`, with TRS 935 Table 5 packaged
verbatim as `inst/extdata/coefs/protein_digestibility_trs935.csv`.

| Source | Exact identity | Access | Provenance role |
|---|---|---|---|
| TRS 935 Table 6 | Same, printed p.100 | open | The aggregation itself, as FAO's own worked example for a wheat / chickpea / milk-powder mixture. `tests/testthat/test_protein_score.R` reproduces its printed digestible amino acid totals (lysine 3241, sulfur 2326, threonine 2483, tryptophan 851 mg), its aggregate profile (44 / 32 / 34 / 12 mg per g of digestible protein) and its digestible protein (73 g of 85.9 g crude) exactly. |
| TRS 935 Table 5 | Same, printed p.96 | open | 35 measured true-digestibility values: 26 single foods and 9 mixed diets. The input tier 1a needs. It records the milling spread CBS cannot observe — wheat whole 0.86 / refined 0.96 / flour white 0.96 / cereal 0.77 / gluten 0.99, and maize 0.85 / corn whole 0.87 / corn cereal 0.70. Do not collapse those into one value. |
| TRS 935 p.99 | Same | open | The prohibition on averaging item scores: "the amino acid score for food mixtures should be calculated from the weighted average digestible amino acid content", and that four amino acids suffice — "in calculating scores it is usually only necessary to use a pattern based on these four amino acids". |
| FNP 92 | FAO Food and Nutrition Paper 92 (2013), *Dietary protein quality evaluation in human nutrition* | open | The competing truncation convention (DIAAS truncated at 100%, ceiling 1.0) that WHEP does **not** use, and p.4's statement that ileal data are insufficient, which is why tier 3 is unreachable. |

`inst/extdata/coefs/protein_digestibility_items.csv` maps FBS items onto Table
5's single-food rows for tier 1a (`method = "trs935_item"`). Every mapped row
resolves to a `single_food` entry, never one of the nine mixed diets — a
mixture's digestibility already aggregates a basket and would double-count the
aggregation the function performs. Coverage on the 2010 world basket is **84.5%**
of food protein; the remainder falls back to the tier 1b class rate, because
Table 5 prints no fruit, vegetable, root, tuber or sugar row at all.

The one judgement in that mapping is which **form** of a commodity was eaten,
which CBS cannot say. It is carried per item as `source_low` / `source_high`
rather than swept, because the processing direction is not uniform:

| item | low | default | high |
|---|---|---|---|
| Wheat | cereal 0.77 | whole 0.86 | flour white 0.96 |
| Maize | corn cereal 0.70 | maize 0.85 | corn whole 0.87 |
| Rice | cereal 0.75 | polished 0.88 | polished 0.88 |
| Oats | cereal 0.72 | oatmeal 0.86 | oatmeal 0.86 |

Refining **raises** wheat by removing bran and **lowers** maize, rice and oats
through extrusion and Maillard damage, so no single "processed" arm exists. The
default takes the least-processed form, which is the consistent partner for
WHEP's own whole-commodity agronomic nitrogen; on the 2010 basket the bracket
spans a median diet quality of 0.853 to 0.913 against a default of 0.891.

**Table 6's printed score and PDCAAS columns are rounded to two decimals and are
not all self-consistent at that precision.** Measured from the table's own
inputs, with the profile at 44.339 mg/g and digestibility 0.851048:

| pattern | score exact → | printed | PDCAAS exact → | printed |
|---|---|---|---|---|
| adult | 0.9853 → 0.99 | 0.99 | 0.83855 → 0.84 | 0.84 |
| older child | 0.9237 → 0.92 | **0.93** | 0.78614 → 0.79 | 0.79 |
| preschool | 0.8527 → 0.85 | 0.85 | 0.72566 → **0.73** | 0.72 |
| infant | 0.7779 → 0.78 | 0.78 | 0.66201 → **0.66** | 0.67 |

Three of the eight printed cells disagree with exact arithmetic on the table's
own inputs, in both directions. Only the cells that follow are asserted as
golden; the intermediates above, which reproduce exactly, carry the validation.
The infant row is independently suspect — its label reads "Infants (0–5 years)"
while its pattern is Table 43's 0.5-year one.

### The ceiling anchor

| Source | Exact identity | Access | Provenance role |
|---|---|---|---|
| TRS 935 upper limit | Same, section 13.7 (printed p.233) and section 14.2 (printed p.242) | open | The `ceiling$multiple` of **2**. Section 14.2: "No safe upper limit has been identified, and it is unlikely that intakes of **twice the safe level** are associated with any risk. However, caution is advised to those contemplating the very high intakes of **3-4 times** the safe intake, since such intakes **approach the tolerable upper limit** and cannot be assumed to be risk-free." Section 13.7: "we can be reasonably confident that an intake of **twice the recommended intake, previously identified as a safe upper limit**, is likely to be safe". So 2 is the sourced default and 3-4 the sourced sensitivity; the report sets no formal UL. |

Two things about the ceiling that are **not** sourced and are labelled as WHEP's
own construction at the point of use:

- **`ceiling$share = 0.5`.** Unlike the floor's 2.5%, which TRS 935 Figure 7 and
  FAO's PoU target fix independently, the tolerated share above the upper limit
  has no source. 0.5 reads "Over" as *the typical member of this population
  exceeds the limit*.
- **Why it is not 2.5%.** Applying the floor's tolerance to the upper tail puts
  the ceiling **below** the floor for 162 of 167 country-years on the 2010
  build. That is the arithmetic reflecting TRS 935's own asymmetry — below
  requirement is harmful, twice the safe level is "unlikely to be associated
  with any risk" — so the two tails cannot carry the same tolerance. At 0.5 the
  band never inverts: the lowest ceiling (74.81) exceeds the highest floor
  (73.75).

Golden values pinned by `tests/testthat/test_nourishment_band.R`: TRS 935
Table 2 (p.45) reports that a population whose median intake sits at the 0.83
safe level still has **7.9%** below requirement at `S_I = 0.12` and **18.2%** at
the printed `S_I` of 0.24. Both reproduce exactly from `Phi(-M_D/S_D)` with
`M_D = 0.24` and `S_R = 0.12`. Note the printed 0.24 is a rounding of
`1.96 x 0.12 = 0.2352`, and only the unrounded value reproduces the table.

Two things the composition deliberately does **not** do. It does not use FAO's
PoU inversion, which needs a class **minimum** requirement TRS 935 never
publishes, and which is a cut-point method whose validity condition (IOM Box
4-2 condition 4) requires true prevalence of 8-10% against targets here of
2.5-10%. And it does not import FAO's within-requirement CV, which is generated
by a physical-activity gap that the per-kilogram protein requirement has no
analogue for.

## Nourishment loss-wedge sources

Cited at the point of use in `R/loss_wedge.R` and carried as the packaged
coefficient tables `inst/extdata/coefs/food_loss_wedge.csv` (rates) and
`inst/extdata/coefs/food_loss_item_groups.csv` (item to commodity group).

| Source | Exact identity | Access | Provenance role |
|---|---|---|---|
| Gustavsson et al. 2011 | Gustavsson, J., Cederberg, C., Sonesson, U., van Otterdijk, R. & Meybeck, A. (2011), *Global food losses and food waste: extent, causes and prevention*, FAO, Rome, ISBN 978-92-5-107205-9 | open, <https://www.fao.org/4/mb060e/mb060e00.pdf> | **Annex 4** (PDF pp.33-34) is the whole rate side: weight percentages of loss and waste for 7 commodity groups in 7 world regions at 5 food-chain steps. Only the `Distribution` and `Consumption` columns are packaged. **Annex 2** (p.31) defines the 7 commodity groups by FBS item, and is the sole authority for `food_loss_item_groups.csv`. **Annex 1** (p.30) lists the countries in each region. **Annex 3** (p.32) records that the percentages are on an edible basis, with explicit conversion factors (wheat/rye 0.78, rice 1.0, fruit and vegetable peeling 0.77). |
| FAO, New Food Balances | FAO, *New Food Balances* methodology, section II p.4 and section VIII p.19 | open, <https://www.fao.org/faostat/en/#data/FBS> | Why only the two at-or-after-retail steps are composed: FBS food availability is measured "at the retail level" and "also includes any loss or waste at the retail or consumer level". Section VIII places element 5123 `Losses` pre-retail, which is why it is **not** subtracted -- doing so would double-count. The 2001 *FBS Handbook* ch. II section 9 conflicts, placing the Waste element's end point at "the household"; it is superseded, and only *New Food Balances* is cited. |
| FAOSTAT FS item 21059 | FAOSTAT Suite of Food Security Indicators, *Food loss percentage* | open | Magnitude cross-check only, never an input. Global median 2.83% of dietary energy (2020, n=203). SOFI 2026 Annex 1B states it is built by applying Gustavsson's `Distribution` column to FBS kilocalories, so it covers the retail step alone and is not a competing estimate of this wedge. |

`food_loss_regions.csv` transcribes **Annex 1** verbatim: 152 countries in seven
regions, covering 99.0% of 2010 world food protein. Three things about it are
load-bearing. Annex 1 places **China** in Region 3 alongside Japan and South
Korea, i.e. on Industrialized Asia's rates, and does not say whether it means
the mainland or the aggregate; WHEP splits them (`CHN` is area 41 "China,
mainland" while the aggregate area 351 carries no `iso3c` and is what the FBS
pin reports food on), so both codes are listed and a table keyed on `iso3c`
alone would drop a fifth of world food protein. **Ethiopia** and **Sudan**
each resolve to two WHEP areas across a polity split (238/62 and 276/206);
Annex 1's 2011 entries are pre-partition and legitimately cover both.
**South Sudan, Taiwan, Hong Kong, Macao, DPRK, Madagascar, Papua New Guinea**
and roughly a hundred small territories appear in no Annex 1 region at all.

Four derivation notes, all load-bearing:

- The two steps are composed **multiplicatively**,
  `1 - (1 - d/2)(1 - c/2)`, not added: the consumption step acts on what
  survives distribution.
- The across-region **consumption** minimum is sub-Saharan Africa in every one
  of the seven commodity groups (cereals 1%, roots 2%, oilseeds and pulses 1%,
  fruit and vegetables 5%, meat 2%, fish 2%, milk 0.1%). Those are scarcity
  figures, not efficiency figures. The **distribution** minima are genuine best
  practice (Europe, North America and Oceania, and Industrialized Asia for milk
  and meat). The halving does not repair that asymmetry, which is why the
  default is documented as a lower bound rather than an estimate of achievable
  loss.
- Annex 2's enumeration leaves eggs, sugar and sweeteners, vegetable and fish
  oils, stimulants and spices, alcoholic beverages, animal fats, butter, honey
  and `Miscellaneous` in no commodity group. On the 2010 world basket they are
  5.0% of food protein, of which eggs alone are 3.7%. They are excluded from the
  weighting rather than assigned a neighbouring group's rate; assigning eggs to
  meat or to dairy instead moves the wedge by under 0.1 percentage points.

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
