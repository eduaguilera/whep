# Issue 423: whole-territory spatial accounting plan

- Status: draft for review
- Branch: `edu/issue-423-whole-territory`
- Base: `main` at `3f479f7a`
- Primary issue: [#423](https://github.com/eduaguilera/whep/issues/423)

Related work: [#276](https://github.com/eduaguilera/whep/issues/276),
[#422](https://github.com/eduaguilera/whep/pull/422),
[#311](https://github.com/eduaguilera/whep/issues/311),
[#331](https://github.com/eduaguilera/whep/issues/331), and
[#382](https://github.com/eduaguilera/whep/issues/382)

## Outcome

Make the canonical WHEP spatial analysis cover the complete terrestrial area of
each grid-cell--polity intersection. Preserve land use through carbon and
nitrogen accounting, including forest and other non-agricultural land. Produce
agriculture-only results only as a documented downstream filter of that full
ledger.

The completed implementation must:

- conserve the physical area represented by every cell--polity--year;
- retain mutually exclusive land-use classes and their provenance;
- keep class-level rows even when no crop item can be assigned;
- estimate gross NPP separately from harvest, biomass return, and stock change;
- quantify standing vegetation C and N accumulation and disaccumulation without
  counting harvest, fire, land-use transfer, or soil return twice;
- retain `land_use` through grid and polity carbon and N balances;
- reproduce the agricultural view by filtering the full result; and
- expose unavailable inputs and unresolved residuals explicitly rather than
  dropping or silently extrapolating them.

## Readiness and boundaries

### In scope

- A WHEP-owned full-territory land-support contract.
- Forest and other natural-land NPP from LPJmL outputs, after validating their
  PFT/stand semantics and units.
- Vegetation C and N stocks and annual stock changes from LPJmL where suitable
  outputs exist.
- Full-territory carbon and nitrogen input/balance schemas.
- A shared post-hoc scope filter for agricultural outputs.
- Tests at cell, border-cell, polity, land-class, and territory levels.
- Documentation, provenance, coverage diagnostics, and migration notes.

### Out of scope unless separately approved

- Treating LUH2 as the authoritative WHEP agricultural-area estimate. LUH2 may
  provisionally allocate the non-agricultural remainder.
- Reimplementing the Spain_Hist forest-inventory reconstruction as a WHEP
  runtime dependency.
- Inventing forest N from a fixed C:N ratio when LPJmL PFT vegetation-N output
  can be generated.
- Extending forestry product trade, processing, or MRIO accounts.
- Backcasting or forecasting missing LPJmL/LUH2 years without a named,
  documented method and an explicit decision.
- Changing scientific reconciliation rules merely to force a balance to close.

Large LPJmL and LUH2 files remain external data addressed through configuration
or environment variables. No generated raster/netCDF data belongs in Git.

## Evidence and reuse

The ref-pinned AFSE capability registry was checked before proposing new code.
It pins WHEP at `0800c15a` and afsetools at `9d4143aa`. Reusable registered
capabilities include the WHEP grass-productivity pipeline and afsetools crop-NPP
helpers. The registry has no full-territory support builder or forest
vegetation-stock-change capability. Current WHEP `main` is ahead of that pin and
adds two components that should be generalized rather than replaced:

- `R/lpjml_npp.R::read_lpjml_npp()` reads annual, named PFT NPP and harvest-C
  bands and subsets requested years at read time.
- `R/grass_natural_carbon_inputs.R::build_grass_natural_carbon_inputs()`
  documents the current natural-PFT summation, managed-grass stand weighting,
  and the conversion from g C m^-2 to Mg C ha^-1.
- `R/luh2_landuse.R::read_luh2_landuse()` already produces border-aware
  cell--polity land-use rows, but currently collapses forest and non-forest
  natural states to `natural` and must remain an interim remainder input.
- `R/n_balance_spatialize.R::build_cell_polity()` provides a reusable
  `(lon, lat, area_code, polity_frac, cell_area_ha)` crosswalk. The preparation
  script currently drops unassigned/ocean subcells and renormalizes surviving
  polity fractions, so `polity_frac` alone is not a terrestrial-area
  denominator. The new producer must retain an unnormalized land intersection.
- `R/spatialize.R::build_gridded_landuse()` provides crop allocation, not a
  complete mutually exclusive physical land surface.

PR #422 contains useful agricultural-support and deposition work. Its current
agriculture-only output is not the target contract: issue #423 must integrate or
refactor it after #422 lands, then retain non-agricultural rows and `land_use`.
The implementation branch must rebase and perform a semantic diff before code
work begins.

Spain_Hist is a methodological oracle, not a dependency. Its reusable principle
is that positive vegetation-stock change is retained biomass, while negative
change releases old biomass. Its exact inventory interpolation and routing
assumptions are Spain-specific and must not be copied without validation.

This registry miss is intentional and documented here. After merge, the AFSE
wiki and capability registry should record any stable public WHEP functions.

## Proposed contracts

### 1. Canonical land support

Add `build_land_use_support()` as the only producer consumed by spatial balance
builders. Its primary key is:

```text
(lon, lat, area_code, year, land_use)
```

Keep support at three explicit grains so cell diagnostics are never repeated and
accidentally summed as class area:

```text
cell support:    cell_support_id, lon, lat, year, cell_area_ha,
                 terrestrial_area_ha, ocean_area_ha,
                 unmapped_terrestrial_area_ha
polity support:  polity_support_id, cell_support_id, area_code, year,
                 polity_support_area_ha
class support:   polity_support_id, lon, lat, area_code, year,
                 land_use, area_ha
```

Minimum class/provenance columns are:

```text
polity_support_id, lon, lat, area_code, year, land_use, area_ha,
area_source, allocation_status, coverage_status
```

`area_code` remains the reporting-territory field. The support contract must
choose fixed reporting geometry or year-valid historical geometry. Under the
latter, polity support also carries the registry-agreed polity identity and
validity fields (for example `polity_code`, `geometry_valid_from`, and
`geometry_valid_through`), and those fields participate in uniqueness.

For terrestrial accounting, define the support before allocating land classes:

```text
polity_support_area_ha = cell_area_ha
                         * unnormalized_polity_land_fraction

sum(polity_support_area_ha across polities) <= cell_area_ha
terrestrial_area_ha = sum(mapped polity support)
                      + unmapped_terrestrial_area_ha
ocean_area_ha = cell_area_ha - terrestrial_area_ha
```

The unnormalized fraction must come from a geodesic/equal-area land intersection
or assigned subcells weighted by each subcell's geodesic area, not raw counts or
a polity fraction renormalized after ocean cells were dropped. Ocean stays
outside the land taxonomy. Unmapped terrestrial area remains explicit with a
missing polity/status and must be resolved or represented as `unclassified`;
it cannot disappear into ocean or from the territory total. LPJmL `grid_scaled`
settings and all other density/mass inputs must be detected and converted to
this same support before they are combined.

The builder should:

1. construct and validate the unnormalized terrestrial support;
2. use the accepted WHEP agricultural physical-area estimates;
3. calculate the remaining physical area rather than replacing WHEP estimates
   with raw LUH2 absolute areas;
4. allocate that remainder among non-agricultural classes using normalized
   source fractions, provisionally from LUH2;
5. retain missing terrestrial weights as `unclassified`; and
6. abort with a diagnostic table when agricultural area exceeds support area,
   unless an approved reconciliation policy says otherwise.

Crop attribution is a child view, not part of the class-level primary key. Its
optional key adds `item_cbs_code`; unmatched physical cropland remains present
with `item_cbs_code = NA` and an explicit `allocation_status`. Multiple cropping
and harvested area must not be mistaken for additional physical land.

The conservative default taxonomy is:

```text
land_use: cropland, grassland, natural, urban, unclassified
vegetation_group within natural: forest, other_natural
```

The agriculture filter selects `cropland` and `grassland`. A mutually exclusive
`forest`/`other_natural` area split may replace `natural` only after an
independent area/stand mapping is validated. LPJmL tree and grass PFTs coexist in
one natural stand and are not forest-area fractions. Do not fabricate area from
PFT abundance. Dehesa/agroforestry and grazed-forest membership remain a
decision gate.

### 2. Productivity ledger

Generalize the current LPJmL reader and carbon-input builder into two related
tables. The unique class ledger is keyed by the canonical land-support key and
is the only productivity table consumed by balances. A child component ledger
adds `stand`, `pft`, and/or `vegetation_group` to its key. Component rows carry
allocation weights/provenance, not independently summable copies of support
area, and must re-aggregate exactly to the class ledger. Preserve at least:

```text
gross_npp_c_mg_ha_yr, harvest_c_mg_ha_yr, returned_c_mg_ha_yr,
gross_npp_c_mg_yr, harvest_c_mg_yr, returned_c_mg_yr,
method, source_file, source_variable, coverage_status
```

Gross NPP must never be labelled with the current `NPP - harvest` soil-input
quantity. A returned/turnover field requires a direct source or named derivation;
it is not the automatic residual of NPP minus harvest. Natural PFTs that coexist
in one LPJmL stand are summed as components; managed rainfed and irrigated grass
stands are weighted by their validated fractions. Urban NPP is explicit
zero/not-applicable, not a missing row.

Readers must join by PFT names, not band positions, validate calendars, grids,
units, scale factors, missing values, and time coverage, and aggregate requested
years/PFT groups while reading. The implementation must empirically reconstruct
an LPJmL aggregate output before accepting any assumed stand weighting.

### 3. Vegetation stocks and biomass change

Add a vegetation-stock reader for LPJmL PFT vegetation C and N outputs. Prefer
direct PFT vegetation N to inferred C:N ratios. Aggregate vegetation C/N outputs
remain independent validation totals. If the necessary PFT outputs were not
selected, the pipeline must report a rerun prerequisite rather than substitute
NPP for stock. As for NPP, expose a unique class-stock ledger for balances and a
separate PFT/stand component ledger with a tested re-aggregation identity.

Retain both density and extensive mass, with explicit units, for example:

```text
stock_c_mg_ha, stock_n_mg_ha, stock_c_mg, stock_n_mg,
previous_year, interval_years, coverage_status
```

Mass-balance flows are differences in extensive stock on the canonical support,
not differences in per-hectare density. Density fields remain available for
diagnostics and the optional area/density decomposition.

For each stable spatial/class key:

```text
net_stock_change_mg_interval = stock_mg_t - stock_mg_previous
accumulated_biomass_mg_interval = max(net_stock_change_mg_interval, 0)
disaccumulated_biomass_mg_interval = max(-net_stock_change_mg_interval, 0)
```

`accumulated_biomass` and `disaccumulated_biomass` are non-negative magnitudes;
`net_stock_change` is signed. The first observed year is `NA`, not zero. Gaps and
irregular intervals carry the interval length and are not silently interpreted
as annual changes. Any annualized rate is a separate derived field with its
method recorded.

Evaluate closure over the complete interval `(previous_year, year]`. Sum every
annual extensive flux across all interior years before comparing it with the
extensive interval stock difference. If any required interior year is missing,
closure is unavailable. Never mix an annual rate with a multi-year stock change.

The single control-volume identity is:

```text
NPP_interval + disaccumulation_interval + inbound_transfer_interval
  = harvest_interval + fire_interval + litter_soil_return_interval
    + other_export_interval + accumulation_interval
    + outbound_transfer_interval + diagnostic_residual_interval
```

This identity explains stock loss; it must not manufacture harvest, fire, or
litter fluxes by routing the same decline a second time. Use direct model/source
variables for those fluxes and land transitions. Where they are absent, preserve
the unexplained amount as `unattributed_release` and mark closure partial. A
diagnostic residual is calculated from observed terms and is never a plug used
to force closure. Only observed litter/soil return is recycled as an N input.

Choose one land-transfer convention. Either use raw class stock change with
transitions as explanatory terms only in this identity, or conservatively remap
previous stock to current support before differencing and omit the same
transitions from that density-change identity. Never add both a transfer-adjusted
stock change and the same transfer to the accounting balance. Coordinate this
choice with the existing soil-carbon land-transfer term.

Store authoritative total stock change first. Any decomposition into persistent
land density change and area-change transfer is diagnostic until its exact
method is approved and its identity is tested.

### 4. Full-territory balances

Refactor `build_n_inputs()` so all spatial terms retain `land_use`, `area_ha`,
coverage, and method fields. Add item codes only to crop-attributed agricultural
rows. A class-wide term must not be repeated once per crop item.

Refactor `build_nitrogen_balance()` to compute the canonical balance at land
class grain before optional item allocation and to retain `land_use` at grid and
polity resolution. Class applicability is explicit: a scientifically
unavailable term is `NA` plus status, whereas a structurally impossible term is
zero plus method.

Define an expected/applicable term matrix per land class. Aggregation must never
turn all-`NA` or partly unavailable required terms into zero via
`sum(..., na.rm = TRUE)`. Emit `balance_status` as `complete`, `partial`, or
`unavailable`, plus the missing required terms. Calculate a closure residual only
for a complete balance; it cannot conceal missing inputs.

Specific semantic safeguards are:

- deposition is area-weighted over every represented class;
- soil organic matter terms are not silently restricted to cropland;
- vegetation N stock gain is an output/sink;
- only source-observed or explicitly derived litter/soil return is an N input;
- human/excreta N transported to agriculture keeps its destination as cropland
  and records a separate origin/process field; and
- subtotal/total rows cannot coexist with their components in the same summing
  path.

Apply the same grain rules to the carbon balance and keep land-use and nested
vegetation-group rows through polity aggregation.

### 5. Downstream scope filter

Add one shared helper, provisionally:

```r
filter_land_scope(x, scope = c("all", "agriculture"))
```

`scope = "agriculture"` selects the approved agricultural land classes from
the already-computed full ledger. Every downstream footprint or summary records
`land_scope`, included classes, excluded area, and the support version. Remove
parallel agriculture-only balance logic after parity is demonstrated.

Keep item attribution as a separate child transform, provisionally
`allocate_agriculture_items()`. Its weights sum to one within every eligible
class key; unmatched support remains as an `item_cbs_code = NA` row. Filtering
therefore has pure subset semantics, while item allocation has a separately
tested re-aggregation contract.

## Decisions requiring approval

These gates precede their dependent implementation tasks. The repository owner
or delegated domain maintainer approves each decision in #423 or the PR. The
decision record must name the selected option, evidence, rationale, affected
schema/years, compatibility impact, and acceptance test. A gate is unblocked
only when that written record is approved and its contract appears in a failing
test; `[ASK]` is a gate marker, not an allocation level.

1. **[ASK] Land taxonomy and agricultural membership.** Start from `natural`
   plus nested vegetation groups. Approve a mutually exclusive forest split only
   if the preflight validates an independent area/stand mapping. Define
   dehesa/agroforestry, grazed forest, item nullability, and whether legacy grass
   item `3000L` remains only a compatibility view.
2. **[ASK] Terrestrial denominator and conflicts.** Choose the exact/unnormalized
   land-intersection method, identify the agricultural physical-area source
   after #422, require one common support for all density inputs, and approve
   aborting rather than proportionally shrinking overfull cells. Select fixed
   reporting geometry or year-valid historical polity geometry. Ocean must stay
   outside the land taxonomy; unmapped terrestrial area must remain explicit.
3. **[ASK] Temporal coverage.** Decide how to handle the LUH2 end year and LPJmL
   run bounds and how periodized polity identities align to those years.
   Recommended default: no silent extrapolation; restrict output or mark
   unavailable until a stamped carry-forward/rerun method is approved.
4. **[ASK] LPJmL forest mapping and stock source.** Recommended default: rerun
   LPJmL with PFT vegetation C and N outputs, validate stand weighting against
   aggregate vegetation stocks, and do not use NPP as a stock proxy.
5. **[ASK] Biomass-loss and transfer convention.** Name the direct harvest,
   fire, litter/turnover, and land-transition variables that are available;
   approve the single control-volume identity, missing-flux behavior, and either
   raw-stock or transfer-adjusted stock change. Approve a density/area
   decomposition before it affects balances.
6. **[ASK] Non-agricultural N terms.** Define the required/applicable term matrix
   for forest/natural, other natural vegetation, and urban land; distinguish
   structural zero from unavailable data; and approve transported-N
   origin/destination semantics.

## Execution plan

Tasks labelled `[critical]` require an independent adversarial review of their
contracts, scientific identities, and tests before completion. Tasks also
labelled `[ASK]` stop at the decision gate defined above.

1. **T00 [critical] Integrate the moving base.** Wait for or explicitly resolve
   #422, rebase on `main`, and compare #311, #331, and #382 semantically. Record
   which support/deposition changes are reused or superseded. Explicitly replace
   #422 behavior that drops crop-pattern-unmatched physical cropland with an
   `item_cbs_code = NA` row. Dependencies: none.
2. **T01 [critical] Run the evidence preflight.** Before fixing taxonomy, inspect
   the terrestrial crosswalk, unnormalized intersection data, LPJmL model mask
   and `grid_scaled` metadata, PFT/stand fractions, flux/stock variables, time
   bounds, #382 periodized polity identities, and land-transition inputs.
   Reconstruct aggregate outputs and record which forest-area, harvest, fire,
   litter/turnover, and C/N stock quantities are actually available.
   Dependencies: T00.
3. **T02 [critical][ASK] Freeze the support contract.** Resolve decisions 1--3
   using T01 evidence. Record taxonomy, denominator, item semantics, time policy,
   overfull-cell failure policy, and compatibility guarantees. Dependencies: T01.
4. **T03 [critical] Encode failing area-contract tests.** Add ordinary, border,
   coastal-land-fraction-below-one, missing-weight, overfull-agriculture,
   multi-cropping, historical-polity-boundary, and temporal-coverage fixtures.
   Dependencies: T02.
5. **T04 [critical] Build canonical land support.** Implement the unnormalized
   terrestrial support, `build_land_use_support()`, provenance diagnostics,
   remainder allocation, `unclassified`, and child compatibility views.
   Dependencies: T03.
6. **T05 [critical] Migrate spatial consumers.** Make carbon, deposition, and
   other area consumers use one support and scaling convention. Preserve class
   grain through cell--polity allocation and aggregation. Dependencies: T04.
7. **T06 [critical] Build the NPP ledger.** Generalize named PFT reading, validate
   metadata and stand semantics established by T01, emit coverage diagnostics,
   and separate gross NPP, observed harvest, and observed/derived return.
   Dependencies: T02, T04.
8. **T07 [critical][ASK] Freeze the biomass method.** Resolve decisions 4--5,
   including output reruns, flux variables, stock units, interval closure,
   transfer convention, missing-flux behavior, and any area/density
   decomposition. Dependencies: T01, T06.
9. **T08 [critical] Implement the vegetation-stock ledger.** Read PFT C/N stocks,
   compute signed interval change and non-negative gain/loss magnitudes, evaluate
   the one control-volume identity from observed terms, and expose missing terms
   and diagnostic residuals. Dependencies: T07.
10. **T09 [critical][ASK] Freeze the class-by-term N matrix.** Resolve decision 6
    and approve completeness/status rules. Dependencies: T02, T08.
11. **T10 [critical] Refactor full-territory N inputs.** Retain class, support,
    status, and method through all terms; prevent item/class duplication; add
    vegetation stock change under the approved convention. Dependencies: T05,
    T09.
12. **T11 [critical] Refactor the N balance.** Balance class rows first, retain
    land use at grid and polity resolution, propagate missing required terms,
    and verify only complete balances. Dependencies: T10.
13. **T12 [standard] Add filtering and child item allocation.** Implement the
    pure scope subset, separate `allocate_agriculture_items()` transform, scope
    metadata, re-aggregation parity, and downstream-call migration. Remove any
    route that recomputes an agriculture-only balance. Dependencies: T05, T11.
14. **T13 [critical] Validate science, geography, and scale.** Run synthetic and
    real-data checks, inspect rendered maps/tables, profile representative years,
    and obtain adversarial verification of conservation and double-counting
    controls. Dependencies: T08, T11, T12.
15. **T14 [specified] Document and announce.** Update function docs, examples,
    LPJmL output instructions, schema migration notes, NEWS, and relevant pkgdown
    pages. Dependencies: T13.
16. **T15 [standard] Complete repository gates.** Run formatting, targeted
    tests, documentation generation, full tests/check, coverage as appropriate,
    and all PR CI. Dependencies: T14.

After T04 establishes support, T05 consumer migration and T06 NPP work can
progress in parallel. T12 follows the canonical balance so the agricultural view
cannot become a second pipeline.

Post-merge follow-up, outside the implementation PR completion DAG: update the
AFSE wiki and refresh the ref-pinned capability registry for stable exported
functions.

## Model allocation

Abstract levels in task labels are binding as follows for this plan. Critical
verification is assigned independently from the implementing agent.

| Level | Assigned model | Effort | Use |
|---|---|---:|---|
| mechanical | `gpt-5.6-terra` | medium | Mechanical inventories or edits |
| specified | `gpt-5.6-terra` | high | Bounded documentation work |
| standard | `gpt-5.6-sol` | high | Integration and routine implementation |
| critical | `gpt-5.6-sol` | max | Scientific contracts and balance logic |
| verifier | `gpt-5.6-sol` | max | Independent adversarial review |

## Expected file map

Exact names may be adjusted after T00, but ownership should remain separated.

- `R/land_use_support.R`: canonical land-support producer and validators.
- `R/lpjml_npp.R`: generalized LPJmL flux reader/preflight.
- `R/lpjml_vegetation_stock.R`: PFT vegetation C/N reader and stock changes.
- `R/land_productivity.R`: class-level NPP/harvest/return ledger.
- `R/n_balance_inputs.R`: full-territory N term assembly.
- `R/n_balance.R`: class-first N balance and aggregation.
- `R/carbon_balance.R` and current carbon helpers: class retention and biomass
  integration without land-use-change duplication.
- `R/land_scope.R`: common all/agriculture filter and metadata.
- `R/agriculture_item_allocation.R`: optional child item allocation with
  re-aggregation guarantees.
- `tests/testthat/`: contract, conservation, metadata, coverage, integration,
  and regression tests mirroring the modules above.
- `inst/scripts/`: LPJmL output preflight and representative validation run.
- `vignettes/`, `man/`, `_pkgdown.yml`, and `NEWS.md`: user-facing migration
  and data preparation guidance.

## Verification matrix

### Area and geography

- The canonical key is unique, land-use vocabulary is controlled, and areas are
  finite and non-negative.
- Class area sums exactly to `polity_support_area_ha` for every
  cell--polity--year within a declared tolerance.
- Summing polity support does not exceed physical cell area. A coastal fixture
  with land fraction below one preserves ocean outside the taxonomy, retains
  unmapped terrestrial area explicitly, and does not inflate mass.
- Assigned subcells are weighted by geodesic area; a latitude-sensitive fixture
  fails under raw subcell counts and passes under area weighting.
- `ocean_area_ha` and `unmapped_terrestrial_area_ha` are separate. The latter is
  retained as explicit unresolved terrestrial support and cannot disappear.
- Cell-, polity-, and class-grain support tables cannot double-count repeated
  diagnostic area fields.
- Summing terrestrial polity pieces restores independently measured land area;
  summing cells restores territory land area without border double counting.
- Agricultural areas reproduce the accepted pre-refactor totals before the
  remainder is added.
- Missing remainder weights produce `unclassified`; overfull agricultural cells
  fail with inspectable keys and magnitudes.
- Multi-crop attribution preserves physical cropland area and total mass.
- The last available LUH2 year and first unsupported year exercise the selected
  coverage policy explicitly.
- A historical boundary-change fixture applies the approved fixed or year-valid
  polity geometry and preserves land area across the transition.

### NPP and LPJmL

- Reordered PFT bands give identical results because joins use names.
- The unit conversion `1 g C m^-2 = 0.01 Mg C ha^-1` is tested.
- Natural coexisting PFT components sum as specified; managed-grass stands are
  fraction weighted; aggregate reconstruction passes tolerance.
- PFT/stand/vegetation-group child rows re-aggregate to one unique class row,
  while class support area is counted exactly once.
- Gross NPP, harvested C, and returned C are separate fields and satisfy their
  annual-rate units; return is not inferred as an unnamed remainder.
- Requested missing years/PFTs/files fail or carry explicit coverage status;
  joins do not silently remove border cells or land classes.
- Urban land is present with an explicit structural-zero/not-applicable method.

### Vegetation stocks

- First-year change is `NA`; consecutive and irregular intervals have correct
  signs, magnitudes, and interval metadata.
- Positive change populates accumulation only; negative change populates
  disaccumulation only; C and N are independently conserved.
- PFT-weighted stocks reconstruct aggregate LPJmL validation outputs.
- PFT/stand stock components re-aggregate to the unique class-stock ledger.
- The recurrence from starting stock plus changes reconstructs ending stock.
- Annual extensive fluxes summed over `(previous_year, year]` have the same time
  dimension as interval stock change. Missing interior years make closure
  unavailable; an irregular-gap fixture proves dimensional closure.
- NPP, disaccumulation, inbound/outbound transfer, harvest, fire, soil return,
  other export, accumulation, and diagnostic residual satisfy the single
  control-volume identity without duplicated flows.
- Only source-observed release destinations are populated; unexplained release
  remains explicit and cannot generate a balancing flux.
- Land-use-change transfers sum to zero across origin and destination classes.

### Nitrogen and carbon balances

- `land_use` survives every grid and polity result.
- Deposition mass by class equals deposition density times class area; class
  sums recover deposition over the represented terrestrial support.
- Item allocation does not change class or territory mass.
- Vegetation gain is counted once as a sink; only explicitly routed release is
  counted once as an input.
- Human N transported to agriculture has a cropland destination and separate
  origin/process metadata.
- SOM treatment is explicit for every class rather than silently cropland-only.
- Class balances and territory balances close with named residuals; no subtotal
  is summed alongside its components.
- All-`NA` aggregation remains `NA`; mixed required-term coverage produces
  `partial`/`unavailable`, lists missing terms, and cannot report false closure.
- The agriculture class ledger is a pure filter of the full ledger, including
  the same values and status metadata. The item child view re-aggregates to that
  class ledger with mass parity; row-for-row item parity is not required.

### Real-data and operational checks

- Use the #422 2010 deposition audit (approximately 63.4 Tg N full territory and
  27.9 Tg N agricultural support, including about 12.2 crop and 15.7 grass) as
  a pre-support-fix review diagnostic, not a brittle unit-test constant. Expect
  coastal-support corrections to change it and re-baseline with provenance.
- Render representative maps for forest, other natural land, border cells,
  `unclassified`, NPP, stock gain, and stock loss and inspect them visually.
- Profile a representative multi-year run; process requested years/PFT groups
  at read time rather than materializing the entire global cube.
- `air format .`, targeted tests, full `devtools::test()`, `devtools::check()`,
  documentation generation, and PR CI pass before merge.

## PR and commit sequence

This draft-plan PR is based directly on `main`, independent of #422's branch.
It relates to #423 and #276 but does not close either issue.

Proposed implementation commit sequence, stopping whenever its next ASK gate is
unresolved:

1. record the evidence preflight and failing support-contract tests;
2. implement canonical land support;
3. migrate area-dependent carbon/deposition consumers;
4. add LPJmL NPP and vegetation-stock ledgers;
5. refactor full-territory N inputs and balances;
6. add the downstream scope filter, child item allocation, and caller migration;
7. add real-data validation, documentation, and migration notes; and
8. format, check, obtain independent scientific review, and resolve CI.

Request `@lbm364dl` as reviewer on this plan PR and the implementation PR unless
he is the author. Do not merge implementation while #422 semantics remain
unresolved or any critical verifier finding is open.

## Run log

| task | level | model | effort | agent | date | result |
|---|---|---|---|---|---|---|
