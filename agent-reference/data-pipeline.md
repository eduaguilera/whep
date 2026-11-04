# WHEP agent reference — data pipeline function catalogue

Read before touching a pipeline stage, a pin, or a producer function whose output a pin caches.

Moved out of `AGENTS.md` on 2026-09-27 to keep that file under Codex's 32 KiB
default instruction-file read cap (`project_doc_max_bytes`). `AGENTS.md`
carries the pointer; this file carries the detail.

---

## Data pipeline

- **Primary production**: `build_primary_production()` — FAOSTAT + LUH2
  extension (1850–2023).
- **CBS**: `build_commodity_balances()` — long format output with `source` and
  `fao_flag` columns.
- **Processing coefficients**: `build_processing_coefs()` — cascades from CBS.
- **Soil water balance**: `build_water_balance()` — gridded (0.5° cell ×
  polity) annual water budget from LPJmL hydrology (drainage for N leaching);
  `get_soc_climate_drivers()` emits the monthly SOC climate drivers.
- **Soil carbon (SOC)**: `build_carbon_balance()` — historical gridded SOC
  dynamics (equilibrium init + LUH2-driven march + LUC transfer), yielding
  ΔSOC → ΔSON. `calculate_soc_dynamics(model = c("hsoc","rothc","icbm","amg",
  "century"))` wraps the five SOC models (default `"hsoc"`);
  `build_soil_carbon_inputs()` assembles humified C inputs.
- **Soil nitrogen balance**: `build_nitrogen_balance()` — full gridded N
  balance (inputs − outputs, NUE indicators, GWP/CO2e). `build_n_inputs()`
  assembles the input terms; `calculate_nh3()` / `calculate_soil_n2o()` /
  `calculate_n_leaching()` are the selectable loss methods;
  `build_n_deposition()` / `build_urban_n()` read gridded deposition and
  urban/human N.
- **Footprints**: `build_footprint()` over the FABIO-style IO core
  (`build_io_model()`, `leontief.R`), with extensions wired per stressor
  (`crop_land_extension.R`, `grassland_land_extension.R`,
  `livestock_ghg_extension.R`, `energy_co2_extension.R`,
  `crop_soil_n2o_extension.R`, `n_exceedance_extension.R`). Conservation
  checks live in `R/conservation.R` — a footprint change should exercise them.
- **Source labels**: use dataset-specific names (`FAOSTAT_prod`,
  `FAOSTAT_FBS_New`, etc.).
- **New data sources**: see [Where input data comes
  from](../AGENTS.md#where-input-data-comes-from) for which of the three mechanisms to
  use.
- **LPJmL outputs are the one input a user cannot obtain**, so unlike the
  third-party rasters they are **pinned, and `WHEP_LPJML_RUN_DIR` is
  optional**. `build_grass_natural_carbon_inputs()` reads
  `lpjml-grass-natural-net-c` and `get_soc_climate_drivers()` reads
  `lpjml-soc-hydrology` by default; set the env var (or pass `run_dir`) only
  to derive those layers from a local run instead. Both artifacts hold **only**
  LPJmL-derived quantities — grazing excreta, humification fractions, CRU air
  temperature and the HWSD texture products are always computed locally, so
  the pinned and run-derived paths cannot silently disagree.
- **Regenerate the complete LPJmL output family from one run.** WHEP consumes
  seven model-output artifacts: `lpjml-grass-availability`,
  `lpjml-grass-productivity`, `lpjml-grass-natural-net-c`,
  `lpjml-soc-hydrology`, `lpjml-crop-regime-yield`,
  `lpjml-crop-irrig-depth` and `lpjml-seepage`. Use the single entry point
  `regenerate_whep_lpjml_pins()` in `~/whep_inputs`. Its shared output registry
  (`lpjml_pin_compare_specs()`) and full consumer contracts
  (`lpjml_pin_consumer_columns()`) must cover the complete family.
  `lpjml_pin_cohort()` checks common run provenance (`source`, `GIT_hash`,
  `history`), supported no-leap day axes, native continuity and starts,
  actual month membership and requested-year coverage before generation.
  Explicit sparse or pre-1850 years must survive unchanged.
  `validate_lpjml_pin_family()` requires every member, nonempty requested-year
  coverage, full consumer schemas and consistent required vintage fields
  before comparison or upload. A representative comparison value alone does
  not establish the complete consumer schema. The local manifest records
  actual year sets, the input cohort and artifact checksums. See
  [whep#1639](https://github.com/eduaguilera/whep/issues/1639) and
  [whep_inputs#9](https://github.com/eduaguilera/whep_inputs/pull/9).
  Generation is a dry-run by default. Inspect the manifest, compare numerical
  changes and validate consumers before separately authorized publication
  with `upload = TRUE`; passing cohort/schema guards alone does not validate
  the resulting numbers. The grazing-water artifact discussed in
  [PR #116](https://github.com/eduaguilera/whep/pull/116) remains proposed.
  Register it in this same family, with cohort and full-schema checks, before
  adoption; it is not one of the seven artifacts above. The six
  `lpjml-wind-*` / `lpjml-rsds-*` / `lpjml-rlds-*` pins are climate **forcing**
  (they feed *into* LPJmL, so they do not change with the model version) and
  must never go through that path; they come from
  `inst/scripts/prepare_spatialize_all.R`.
- **LPJmL output variable names are version-dependent.** 6.x renames some
  outputs to their CF short names — `mprec.nc` holds `pr` where 5.x held
  `prec`. Readers resolve the name against what the file actually contains
  (`.hydro_var_aliases()`), because a run directory carries no version stamp
  and both versions' output can sit side by side on one machine. Add the next
  rename there; the reader aborts listing the file's actual variables rather
  than failing on a `NULL` lookup.

#### Fixing the reader is half the job: the pin it feeds is now stale

A pin is a **frozen output of code in this repository**. So whenever a change
alters what a producer function emits, the pin that function made is wrong
from that moment, and **the fix moves no published number until the pin is
regenerated and re-uploaded**. A merged PR whose effect is still sitting
behind a stale pin is not done; it is latent.

This is not hypothetical. whep#1092 restored FAOSTAT's `1000 Head` live-animal
trade -- 76.1 billion head of poultry the reader had been dropping. The reader
fix merged as PR #1113 and changed nothing anyone can see, because
`build_detailed_trade()` is the `bilateral_trade` **producer** and has no
caller in `R/`: every published figure still comes from the pin built by the
old, filtering reader.

So when you touch a producer:

1. **Say so in the PR body**, explicitly: name the pin, and state that the
   change is latent until it is regenerated. "No published value changes" is
   the right measurement and the wrong conclusion if the reason is a stale pin
   -- distinguish "this genuinely moves nothing" from "this moves nothing
   *yet*".
2. **Open a follow-up issue for the regeneration** if you cannot do it in the
   PR, and link it. Regenerating usually needs board credentials and a long
   run, so it is legitimately separate work -- but it must be tracked, not
   assumed.
3. **Check whether the function you changed is a producer at all.** `git grep`
   its name across `R/`: no caller outside `inst/scripts/` is the signature of
   one.

The LPJmL-derived pins carry an extra constraint -- regenerate the complete
registered output family together, from one run, via
`regenerate_whep_lpjml_pins()`. See the data pipeline section for its seven
current members and the separate publication gate.
