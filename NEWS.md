# whep (development version)

* **`validation/lpjml_forcing_pins.R`: the climate-forcing pins now have an
  impossibility check, and it records #824 rather than suppressing it.**
  `validation/lpjml_pins.R` guards the four pins carrying LPJmL *output* and
  deliberately excludes the *forcing* pins, reasoning that forcing does not
  change with the model version. True of its magnitude tier, false of its
  invariant tier: forcing can still be corrupt.
  `lpjml-rsds-era5-2017-2023` ships **1,823,843 negative shortwave values**
  because #536 fixed the builder and nobody rebuilt the artifact, and nothing in
  the repo could see it — that script excluded the pin, and
  `test_data_raw_freshness.R` gates `data/*.rda` against `data-raw/`, not a pin
  against its generating script. A census of all six forcing pins shows the
  defect is confined to that one: `rlds` and `wind` come off the same builder
  and the same ERA5 tail and are clean, because shortwave is the only one of the
  three that legitimately reaches exactly zero. The floor is therefore
  **inclusive** — `lpjml-rsds-isimip-1901-2019` has a minimum of exactly 0, so a
  positivity test would fail on a clean pin. The recorded count is compared
  **bidirectionally**: a rise is a new corruption, and a fall means the pin was
  rebuilt, which is the event #824 exists because nobody noticed. Refs #824,
  #536, #384.

* **A missing `water` or `ice` layer no longer zero-fills silently in
  `build_polycell_support()` (#885).** Both arguments default to `NULL`, the
  column is filled with zeros, and the producer's identity
  `polity_area_ha == land_area_ha + inland_water_ha + ice_area_ha` still holds —
  so every check passes while every lake, river and glacier inside a polity is
  booked as land. The deployed pin `20260818T105426Z-a0330` was built that way,
  confirmed by reading it: all **482,605** rows carry `inland_water_ha == 0` and
  `ice_area_ha == 0`, and `land_area_ha` equals `polity_area_ha` in every one,
  which puts 2015 land 536.0 Mha (+4.15%) above the pin built from all four
  layers. `.pcs_add_water()` and `.pcs_add_ice()` now warn, on distinct condition
  classes (`whep_polycell_absent_water`, `whep_polycell_absent_ice`), naming the
  consequence rather than the absence. Zero-filling still happens — this changes
  no value, only its visibility — and remains correct for a smoke build. Does not
  fix the deployed pin, which needs regenerating with all four layers.
  Refs #885, #802.
* **The three polity tables are re-synced to upstream `whep-polities` again,
  and this time the re-sync closes an identity gap rather than moving one
  (#890, #458).** `polity_area_crosswalk`, `polities` and
  `polity_label_aliases` are committed build products of
  `data-raw/table_mappings.R`, whose inputs live outside this repo, so they
  drift between re-syncs and `test_data_raw_freshness.R` had begun failing on
  exactly that. They are rebuilt here from `eduaguilera/whep-polities` at
  `e10c7421` ("Merge pull request #597 from
  eduaguilera/docs/state-inventory-dead-name"), that repository's `main`.

  What moved. `polities` goes 779 → 781 rows: two additions, no removals, and
  both are territories upstream previously modelled as no polity at all —
  `SXM-2010-2025` Sint Maarten and `BES-2010-2025` Bonaire, Sint Eustatius and
  Saba. With them `ANT-1961-2010` Netherlands Antilles finally publishes a
  `successor` (`CUW-2010-2025; SXM-2010-2025; BES-2010-2025`) and a
  `predecessor` (`ANT-1816-1961`); `CUW-2010-2025` gains its predecessor too.
  Beyond that the 779 shared rows move in 5 `polygon_source` values, 2
  `polygon_status` (`assigned` → `proxy`) and 6 `polygon_area_km2`, and in
  nothing else — `last_ingest` does not change on a single shared row, so the
  snapshot's new maximum of 2026-08-24 is carried by the two new rows alone.
  The crosswalk goes 649 → 648 rows over the **same 267 areas** and one more
  polity (560 → 561): Sint Maarten used to fall back to BOTH Dutch periods
  (`NLD-1800-1830` and `NLD-1830-2025`) and now resolves to the single polity of
  its own, which is why `prefix_fallback` goes 27 → 26 and the
  `not_a_reporting_area` rows go 20 → 19 while still covering the same six
  territories. `polity_label_aliases` goes 995 → 1,003: eight new `fao1952`
  labels, plus two rules re-pointed off the joint Ruanda-Urundi entity onto its
  constituents — `burundi | iia | 1922-1961` from `RWB-1922-1962` to
  `BDI-1922-1962` "Burundi (within Ruanda-Urundi)", and `rwanda | mitchell |
  1953-1960` to `RWA-1922-1962`.

  **No published value moves, and no territorial identity moves either.** Over
  the full `(area_code, year)` grid 1850–2023 (46,284 pairs), `polity_code`,
  `polity_area_code`, `mapping_status` and `has_geometry` are unchanged for
  **every single pair** — the previous re-sync moved 126 of them, this one
  moves none. `polity_mapping_provenance()` over the crosswalk's own grid
  reports the same 46,816 resolutions in the same five `mapping_source` classes
  and the same four `authority` classes. A real `get_primary_production()`
  returns the same 6,310,171 rows × 12 columns with **zero keys added and zero
  removed**, and all twelve columns — `value` included, at 5.7635e12 summed —
  agree row for row. One `area_code` still carries exactly one `area` label
  (whep#563), on 0 violations.

  Two behaviours do change, and both are the gap closing. Area 151 Netherlands
  Antilles stops being unreachable: `population_source_reach()` now classifies
  it `"successor"` reaching `BES, CUW, SXM`, and **no** reporting area outside
  the Rest-of-World bucket is unreachable any more. And
  `.dependency_sovereign_iso3()` briefly lost Sint Maarten — see the next entry.

* **A crown dependency no longer loses its sovereign the moment upstream gives
  it a polity of its own (#407).** `.dependency_sovereign_iso3()` is what lets
  `dependency_land = "sovereign"` book Jersey's, Guernsey's, the Isle of Man's,
  Åland's, Saint Barthélemy's and Sint Maarten's LUH2 land under the state that
  reports for them, none of the six having a FAOSTAT `area_code`. It found the
  sovereign by asking which reporting area **shared** the dependency's
  `polity_code` — a relation that exists only while the dependency has no
  polity of its own. The re-sync above gave Sint Maarten `SXM-2010-2025`, so
  nothing shared its polity, the bridge silently went from six territories to
  five, and Sint Maarten's land went from counted under `NLD` to counted
  nowhere. It now falls back to `legacy_polity_prefix`, the same ISO3-stem-to-
  bucket bridge `.read_fodder_euadb()` uses, which names the sovereign whether
  or not the dependency has its own polity. The fallback fires only where the
  polity route finds nothing, so it cannot move an answer that route still
  gives: the two agree on all five it resolves, and the sixth is recovered.
  Default behaviour is untouched — `dependency_land` still defaults to
  `"drop"`.

* **The `polycell_support` pin is regenerated: the aggregate overlap layer is
  published, and the inland-water and glacier layers are restored (#873,
  #885).** `20260825T102349Z-1a0eb` replaces `20260818T105426Z-a0330`, and it
  moves published land areas. Three things change together, because they are
  one regeneration.

  **Inland water and ice are measured again.** The pin published on
  2026-08-18 was rebuilt without the optional `water` and `ice` layers, so
  `inland_water_ha` and `ice_area_ha` were zero in all 482,605 rows and
  `land_area_ha` equalled `polity_area_ha` exactly: every lake, river and
  glacier inside a territory was booked as land. At 2015, land was 13.4613
  Gha; it is now **12.9281 Gha**, with 303.21 Mha of inland water and 231.69
  Mha of ice measured separately. The largest single movers are Greenland
  (-178.6 Mha, almost all ice), Canada (-146.2 Mha, 122.6 Mha water and 23.6
  Mha ice) and Russia (-64.8 Mha). Consumers that read `land_area_ha` move
  with it: `read_luh2_landuse(area_basis = "polycell_land")`,
  `build_polycell_land_uses()` and `build_carbon_balance()`. Most visibly,
  `build_n_deposition()` splits deposition between land, inland water and ice
  again instead of booking all of it to land.

  **The aggregate overlap layer is published.** 16,182 rows over 12,438
  polycells and the 19 live aggregate polities now carry
  `support_role == "overlap"`, so `read_polycell_support(role = "overlap")`
  resolves instead of aborting and the ten pre-1962 reporting buckets whose
  only territory is an aggregate have territory again. The default
  `role = "partition"` is unchanged, so no consumer picks the layer up
  without asking: verified cell for cell at 1850, 1900, 1950, 1961, 2000,
  2015 and 2025, where territory, land, water and ice all differ from a
  partition-only build by 0.

  **The polity table is resynced.** The pin is built against `whep::polities`
  as it stands after the 2026-08-21 upstream sync, which the previous pin
  predates: 702 partition polities against 685, 29 gained and 12 lost (mostly
  period-boundary corrections such as `SEN-1886-1959` to `SEN-1886-1960`).
  2015 territory moves by +1.7 Mha (+0.012%), and 2,613 of 411,397 shared
  polity-cells move by more than 1 ha, concentrated in the colonial
  federations `AEF-1910-1960` and `AOF-1895-1960`.

* **Seven of the seventeen EarthStat fertilizer crop codes named the wrong
  crop, in two independent copies (#889).** `.earthstat_fertilizer_mapping()`
  keys each crop-specific fertilizer raster to an `item_prod_code`. Compared
  against `inst/extdata/earthstat_mapping.csv` — the same script's own answer for
  the same raster on the harvested-area layer — seven disagreed, and only two
  were detectable as bad codes: `cassava` 340 and `cotton` 274 are absent from
  `items_prod`, while `oilpalm` 217, `potato` 328, `rapeseed` 223, `sugarcane`
  780 and `sunflower` 222 are all real item codes naming **cashews, seed cotton,
  pistachios, jute and walnuts**. A code that exists and names another crop joins
  successfully to the wrong crop, so an existence check — the obvious guard —
  passes on five of the seven. `.read_west_manure_local()` carried an independent
  copy of the same tribble with the same seven errors, so the manure-N layer was
  misattributed too and a fix to the fertilizer mapping alone would not have
  reached it; it now reads the one shared mapping. The new guard compares codes
  against the CSV, asserts existence for rasters the CSV does not carry, and
  fails if any call site stops sharing the mapping. Regenerating
  `crop_fertilizer_patterns` is still required for the fix to take effect (same
  shape as #877). Refs #889, #876, #877, #888.

* **New `population_source_reach()` reports which areas a population source
  keyed on present-day ISO3 can reach, and area 151 is the one it cannot
  (#787).** Both population sources WHEP reads — the `gdp-population` pin and
  UN WPP 2024 — are keyed on a present-day ISO3 code, so neither can carry a
  territory that no longer exists. This classifies each of the 297 reporting
  periods in `polity_area_crosswalk` as `"direct"`, `"successor"` (the polities
  database's `successor` relation reaches present-day codes) or
  `"unreachable"`. Measured against UN WPP 2024 when this landed, 283 were
  direct, 8 resolved through successors, and exactly one reporting area outside
  the Rest-of-World bucket was unreachable: `ANT-1961-2010`, area 151
  Netherlands Antilles, which carries commodity-balance food in every year from
  1961 to 2010. Upstream published no successor for it and modelled neither
  Sint Maarten nor the BES islands as polities, so reconstructing it was an
  upstream identity gap rather than a missing value — **and the polity re-sync
  above closed that gap in this same release**, so area 151 now reads
  `"successor"` and no reporting area outside the Rest-of-World bucket is
  unreachable at all. No published value changes: this reports coverage and
  builds no denominator. It deliberately does **not** fill area 151 or any
  other area — a successor sum over UN WPP falls 17.5% short of the pin's own
  figure for the Yugoslav SFR, because WPP reports Kosovo separately and the
  successor relation does not name it.
* **`build_polycell_support()` can now emit aggregate polities as an explicit,
  non-partitioning overlap layer (#803).** An aggregate -- `BLX-1850-1999`
  Belgium-Luxembourg, `F249-1918-1990` Yemen, the six residual `"Other"`
  regions -- is the only territory some FAOSTAT reporting buckets ever have:
  bucket 15 carries data before 2000 where Belgium (255) and Luxembourg (256)
  carry none. Excluding them left ten buckets with no territory in at least one
  pre-1962 year (15, 151, 237 over 1954-1961, 249 and 901-906). They stay
  excluded by default, because an aggregate's polygon covers its members' and
  the support must partition each cell; `aggregates = "overlap_layer"` clips
  them too and marks every row `support_role`, `"partition"` or `"overlap"`.
  The partition is unchanged either way -- measured polity by polity and year
  by year on the real Belgium/Luxembourg geometries, the maximum difference is
  0 -- because a cell's inland water is apportioned over the partition's
  territory alone and every diagnostic describing the partition is measured on
  it alone. `read_polycell_support()` gains `role`, defaulting to
  `"partition"`, so no existing consumer can pick up an overlapping row by
  accident. **No published value changes**: the default is the status quo, no
  pin is regenerated, and the published `polycell_support` has no aggregate
  rows to return.
* **`build_commodity_balances()` gained `trade_recovery`, which lets the CBS
  keep an import it has no row for (#762).** `.cbs_impute_trade()` left-joins
  the crosswalked FAOSTAT/FishStat trade onto the already-pivoted CBS, so it
  can fill an `(area, item, year)` the CBS already carries but never create
  one. An area whose balance sheet omits an item it demonstrably imports loses
  that import outright. Measured at 2010 on the real pins, 3,336 crosswalked
  import records and 2,119 export records have no row to land on; among
  tonnes-denominated items 1,197 `(area, item)` pairs are net imports the CBS
  never sees. Singapore alone loses 72 items and 2.56 Mt of net imports.

  `trade_recovery = "net_import"` creates those rows first, so the join lands.
  It is restricted to what can be balanced without inventing anything:
  tonnes-denominated items only (live-animal trade is counted in heads and
  reaches the wide CBS through `get_livestock_cbs()`), net importers only (a
  created row has no production, so a net-exported one would force the cascade
  to invent some), and areas the CBS already covers in that year (so the
  `area` label is read from the CBS bucket rather than a year-free lookup).
  Recovered rows carry `source = "FAOSTAT_trade"`.

  **The default is `"none"`, which is exactly today's behaviour**, and
  `get_wide_cbs()` always uses it. Nothing published moves unless the argument
  is passed.

  **What moves when it is passed.** On a 2010 build (wide CBS, context window
  2005-2015): 1,164 keys are added and none removed (17,685 to 18,849 rows),
  1,138 of them created directly from the trade record and 26 more processed
  products the cascade then derives from them; 200 areas before and after.
  Over tonnes items, world imports rise 53.7 Mt (+3.4%), exports 12.3 Mt
  (+0.8%), domestic supply 41.7 Mt (+0.2%), feed 36.7 Mt (+0.4%) and food
  4.1 Mt (+0.09%); 34.6 Mt of the domestic supply is fodder (items 2000, 2001,
  2002), which is missing from 97 areas' balance sheets and present in the
  trade record for all of them. Every recovered import reaches the output
  unchanged (0 of 1,138 keys differ). No pre-existing row's supply-use
  residual worsens by more than 0.05 t. 205 created rows do end up with
  production, 210 kt in total, and all of them are processed products (beer,
  wine, oils, cakes, DDGS, sugar, butter) whose output the processing cascade
  derives from the newly recovered inputs.

  Downstream, on the nourishment axis at 2010: areas classified `Under` fall
  from 33 to 31. Singapore goes from 30.5 to 65.7 g protein/cap/day
  (`Under` to `Adequate`), Bahrain from 33.7 to 79.0 (`Under` to `Adequate`)
  and Qatar from 81.0 to 127.7 (`Adequate` to `Over`). Equatorial Guinea
  (11.4 to 18.3) and Puerto Rico (12.5, no trade record at all) stay `Under`:
  those two are a genuine data absence, not a join artifact. Spain, the USA,
  Niger, Kiribati and Saint Kitts and Nevis do not move at all.

  **Two things are deliberately not settled here**, which is why the default
  is unchanged. A created row has no destiny of its own, so the existing
  fallback assigns its whole domestic supply to the item's default destiny —
  processing for rice, not food — and that is an allocation rule, not an
  identity. And 144 net-exported pairs / 28.9 Mt at 2010 stay uncreated,
  because balancing them would fabricate supply. Both are open in #762.
* **`regions_full` and `polities_cats` no longer ship the `region_test`
  column, and their remaining regional groupings now document which ones WHEP
  reads (#386).** `region_test` held two values, `"Europe"` and `"Other"`, and
  had no consumer anywhere in the package; it was a working column left in two
  published tables. Both datasets drop from 39 columns to 38; no other cell
  changes and no published value moves. The `regions_full` help page gains a
  *Which regional groupings WHEP reads* section recording that six of the
  remaining groupings have an in-tree consumer, that `region_code` is a 1:1
  relabelling of `region`, and that the eleven unconsumed ones are shipped as
  reference without a re-validation promise -- with the gap a consumer would
  inherit stated explicitly: over the 202 `cbs` reporters the present-day
  taxonomies are `NA` for exactly the four dissolved federations (codes 51,
  186, 228, 248). Two `region_labour_mech` cells that hold a sub-region name
  rather than a mechanisation class are documented rather than repaired, since
  the correct class is not recoverable from anything the package ships.

* **New: `check_table_schema()` and `assert_table_schema()` validate a table
  against a serializable declarative schema (#373).** The schema is plain
  data — an ordered list of column specifications carrying type, presence,
  bounds, an allowed-value vocabulary, uniqueness and severity, plus a
  table-level key, extra-column policy, column-order policy and empty-table
  policy — so it round-trips through `yaml`/`jsonlite` and can be stored next
  to the artifact it describes. Project vocabularies and scientific bounds
  stay in the caller's schema; the validator hard-codes none of them.
  `check_table_schema()` returns one deterministic diagnostic row per
  violation (`row`, `column`, `rule`, `value`, `severity`, `detail`) and never
  touches the input; `assert_table_schema()` is the build-time gate over the
  same schema and returns its input unchanged. This complements
  `ensure_columns()`, which *coerces* a table to a typed prototype: use
  `ensure_columns()` to reach a schema and `check_table_schema()` to prove a
  table is already there. No published value changes — both functions are new
  and nothing in the pipeline calls them yet.

* **New `write_table_checked()` writes a table atomically and verifies it
  before it replaces anything (#375).** It creates the parent directory,
  writes to a temporary file beside the target, reads that file back
  (`assert_parquet_integrity()` plus a row and column-name check for Parquet,
  a header and row-count re-read for CSV) and only then renames it into
  place, so an interrupted, failed or corrupt write leaves the previous
  artifact untouched instead of overwriting it with a partial file.
  `overwrite = FALSE` refuses an existing target, and `sidecars` optionally
  writes `<path>.schema.yaml` and `<path>.provenance.yaml`. It also closes a
  silent failure mode: `nanoparquet::write_parquet()` given a path whose
  parent directory does not exist returns `NULL` and writes nothing at all,
  which `write_parquet_checked()` only reported as a confusing "Parquet file
  not found". No published value changes: this is a new function, and no
  existing call site was moved onto it.

* **The package no longer calls `dplyr::case_match()`, which dplyr 1.2.0
  deprecated (#850).** The five call sites — the `fert_type` bridge in the
  nitrogen balance, the `manure_type` bridge in its inputs, the GLEAM
  continent and method-label helpers in the energy CO2 extension, and the
  live-animal unit rename in the production assembly — now use
  `dplyr::case_when()`. This is deliberately not `recode_values()`, dplyr's
  named successor: that function does not exist before dplyr 1.2.0, so using
  it would force a `dplyr (>= 1.2.0)` bound in `DESCRIPTION` and break
  installation for anyone on an older dplyr, while `case_when()` behaves
  identically on both. No published value changes: every affected helper was
  run over its whole input vocabulary plus `NA`, an unmatched value and an
  empty input, under dplyr 1.1.4 and 1.2.1, and the output is identical in
  all cases. What does change is that a build on dplyr >= 1.2.0 no longer
  emits deprecation warnings from these paths.

* **The GLEAM coefficient tables now cite the FAO workbook they were actually
  read from, not an unregistered DOI (#607).** All fifteen GLEAM `@source`
  tags in `R/livestock_coefs.R` credited "MacLeod et al. (2018) GLEAM 3.0
  Supplement S1", four of them with an IOP-prefixed DOI that is not
  registered at all (a 404 at doi.org, "Resource not found" in Crossref),
  which is what produced the `--as-cran` "possibly invalid DOIs" NOTE.
  MacLeod et al. (2018) is the *Animal* position paper on GLEAM
  (`10.1017/S1751731117001847`), which publishes none of these tables. The
  twelve tables parsed from `data-raw/GLEAM_3.0_Supplement_S1.xlsx` are now
  cited as FAO (2022) GLEAM version 3.0 Supplement S1 by title and URL, the
  committed workbook having been confirmed byte-identical to the one FAO
  publishes; FAO issues no DOI for it. The other five -- `gleam_mms_shares`,
  `gleam_animal_weights`, `gleam_milk_production`,
  `gleam_livestock_categories` and `gleam_feed_categories` -- are hardcoded
  in `data-raw/livestock_coefficients.R`, could not be traced to any GLEAM
  document, and their documentation now says so instead of naming a source
  they do not have; `gleam_animal_weights` feeds the Tier 2 energy
  calculation, so #881 tracks sourcing it. Documentation only: no data value
  and no published number changes.

* **The `ipcc_2019_*` livestock coefficient tables now document what edition
  each of their values actually comes from (#601).** No stored value changed,
  so no published number changes. Every one of the ten objects was checked
  cell by cell against the published PDFs of both the 2019 Refinement and the
  2006 Guidelines, Vol 4, Ch 10 (and Ch 11 Table 11.1 for the
  pasture/range/paddock N2O factor). Only `ipcc_2019_bo` and `ipcc_2019_cfi`
  hold 2019 Refinement values throughout; `ipcc_2019_ym` is split between the
  two editions; the enteric, manure-CH4, MCF, nitrogen-excretion and direct-N2O
  tables are 2006 values, values from no IPCC table at all, or a per-head
  quantity the Refinement does not publish. The `@source` of each says which,
  names the specific cells, and gives the published alternative. Whether to
  revalue them, rename them or expose both editions is the open decision in
  #601: measured on the 2020 Tier 1 livestock chain, moving the cattle enteric
  table to the 2019 Refinement's Table 10.11 raises enteric CH4 from 109.4 to
  121.8 Tg (+11.3%, and +67% for Africa alone), and moving EF3 to the 2019
  Table 10.21 lowers direct manure N2O from 1.65 to 1.15 Tg (-30.6%).

* **User-supplied historical rice no longer arrives on two different mass
  bases (#778).** `.read_historical_production()` is the single reader behind
  the public `historical_data` argument of both `build_primary_production()`
  and `build_commodity_balances()`, but only the production side treated the
  rows as paddy: one 100 t paddy rice row reached item 2807 as 67 t through
  `build_primary_production()` and as 100 t through
  `build_commodity_balances()`, a 1.49x disagreement on a major staple.
  Item 2807 is milled equivalent throughout WHEP, so the CBS ingest now keys
  the paddy test on the row's `source` -- the item label cannot carry the basis
  once `items_full` has relabelled every 2807 row "Rice and products" -- using
  the same source list the production pipeline uses. Both paths therefore agree
  by construction. **No published value changes:** `historical_data` defaults
  to `NULL` and nothing in the repository passes rice through it, so this only
  affects a user who supplies a historical rice series, whose pre-1961 CBS rice
  is now milled equivalent (0.67x its previous level) rather than paddy. The
  paddy assumption is now documented on both `historical_data` parameters;
  pre-divide by the extraction rate if the series is already milled.

* **`build_water_balance()` now warns when the per-CFT consumptive-water
  inputs carry the LPJmL 6.x green/blue defect (#737).** The default
  `blue_green = "cft_native"` method partitions evapotranspiration with the
  `cft_consump_water_b` / `cft_consump_water_g` cubes. LPJmL 6.x before
  `lbm364dl/LPJmL#3` books infiltrating rain as blue water, so those cubes
  split it badly wrong: on the current 6.1.1 production run, rainfed
  grassland comes out green 134.2 / blue 382.2 mm where the same cells of a
  run built with the fix give 516.4 / 0.0. A rainfed crop band receives no
  irrigation, so blue water on one is proof of the defect, and that is what
  the check tests — measured over every rainfed band and cell of year 2005,
  the blue share is 0.899 on the affected run against 0.0199 (6.1.1 with the
  fix) and 0.0002 (5.9.7), so the warning fires above 0.10. It is detected
  from the data rather than from a version number because a run directory
  carries no version stamp. **No published value changes**: the split is
  still computed and returned exactly as before, and
  `blue_green = "irrig_share"` never reads these cubes. Nothing in the
  package consumes `blue_consump_mm` / `green_consump_mm` /
  `aet_blue_mm` / `aet_green_mm` yet, so this makes a trap visible rather
  than correcting a live error.

* **The `spatialize-crop-patterns` pin was regenerated: barley is back, and
  the gridded harvested-area round trip closes 6 points tighter (#877).**
  `inst/extdata/earthstat_mapping.csv` is the crosswalk
  `prepare_crop_patterns()` iterates -- one EarthStat harvested-area raster
  per row -- and it shipped with 169 rows against the 172 crop directories
  the EarthStat `HarvestedAreaYield175Crops` tree contains. The three
  missing names were `barley`, `greencorn` and `hempseed`, all three of
  which `cft_mapping.csv` already expected, so `crop_patterns` carried no
  cell for them and `build_gridded_landuse()` dropped their entire world
  total in every country and every year. The rows are added and the pin
  rebuilt from the whole EarthStat tree (version
  `20260825T092111Z-8690d`, 2,297,621 rows over 147 crops, up from
  2,247,239 over 144); the regeneration reproduces the 144 crops already
  in the deployed pin bit-for-bit, so the only change is the three crops
  it adds.
  **This moves published gridded values.** Spatializing with the `whep`
  preset and re-aggregating the cells to their reporting areas, the world
  harvested-area round trip goes from 0.9327 to 0.9898 at 1961 (+54.46
  Mha, of which barley 53.70 Mha) and from 0.9474 to 0.9832 at 2015
  (+48.96 Mha, barley 47.85 Mha); the share of areas round-tripping within
  1% goes from 42.3% to 78.9% at 1961 and 34.9% to 72.3% at 2015. No other
  crop's allocation moves by more than 6e-08 ha. Everything downstream of
  `build_gridded_landuse()` inherits the change, including
  `build_crop_land_extension()` and the gridded nitrogen balance. Hempseed
  contributes cells but no area yet: the national table carries no
  harvested area under item 336.
  `Chillies and peppers, dry` (689) and `Leeks and other alliaceous
  vegetables` (407) stay unallocated, and are meant to -- EarthStat
  publishes no raster for either.

* **`biomass_coefs` now has one source, and the Spanish nitrogen chain
  moves with it (#489).** The name resolved to two different tables: the
  packaged `whep::biomass_coefs` (63 columns, rebuilt from
  `inst/extdata/harmonization/biomass_coefs.csv`) and a `biomass_coefs` pin
  frozen at `20250728T082553Z` (41 columns). They disagreed on 12 of their 36
  shared columns, so `build_food_supply()` and `create_n_prov_destiny()` ran
  on different nitrogen coefficients for the same commodity. The packaged
  table is authoritative — it reproduces the upstream `Biomass_coefs.xlsx`
  `Coefs` sheet cell-for-cell on all 63 columns, whereas the pin differs from
  upstream in 126 cells — so the pin entry has been removed from
  `whep_inputs` and `create_n_prov_destiny()`,
  `validate_national_trade()` and the Josette typologies now read
  `whep::biomass_coefs`.
  Published values change. On the columns those callers use, 23 cells moved:
  `Residue_kgN_kgDM` for the 17 wood and forest items rises from
  0.00095 to 0.0030–0.0045 kg N per kg DM, and `Lysine` and `Methionine`
  gain product dry-matter and nitrogen contents the pin had at roughly half
  and at zero. Measured end to end on `create_n_prov_destiny()`
  (1961–2023, all provinces): total nitrogen 410.78 to 415.42 Tg,
  **+1.13%**, concentrated in `Firewood` (+4.35%), `Wood` (+216%),
  `Lysine` (+281%) and `Methionine` (0 to 239,619 Mg N). By destiny,
  `export` +6.48%, `population_other_uses` +19.41%, `livestock_mono`
  +1.92%; by box, `semi_natural_agroecosystems` +5.80%, `Cropland`
  +1.01%. No other function changed: nothing read the five below-ground
  columns the pin carried and the packaged table does not.

* **The six patchwork panel plots now say which package is missing
  instead of failing inside `loadNamespace()` (#431).**
  `plot_typology_indicators_panel()`, `plot_typology_periods_panel()`,
  `plot_loss_decomp_rolling_panel()`, `plot_loss_decomp_periods_panel()`,
  `plot_compart_factor_roll_panel()` and `plot_compart_factor_periods()`
  compose their figures with `patchwork`, and every panel is a `ggplot2`
  object, but both packages are `Suggests`. Without them the functions
  aborted with `there is no package called 'patchwork'` only after minutes
  of pin reads and decomposition work; their examples failed outright on any
  machine that lacks either package. They now call
  `rlang::check_installed(c("ggplot2", "patchwork"))` as their first
  statement, and their examples are wrapped in a `requireNamespace()` guard.
  No published value changes: the guard is a no-op whenever both packages
  are installed, which is the case on all CI platforms.

* **`build_primary_production()` now reports same-source duplicate rows
  instead of dropping them silently (#650).** `.dedup_production()` exists to
  arbitrate between competing *sources* for one
  `(year, area_code, item_prod_code, unit)` key, keeping the better-ranked
  one. Two rows carrying the *same* source are not that case: they are either
  an exact duplicate or, as in #633, two territories that should have been
  summed upstream, and keeping one silently lost the other's mass. The
  arbitration is unchanged -- summing here would double-count a FAOSTAT
  aggregate that legitimately arrives alongside its own components -- but such
  a collision now raises a warning naming the affected keys and the value
  discarded per unit, silenceable with
  `options(whep.warn_prod_dupes = FALSE)`. `show_duplicates = TRUE` likewise
  flags keys that repeat one source, which is why that source's cell holds a
  list rather than a number. No published value changes: on a full 1850-2023
  build (6,310,171 rows) every key is already unique, so the warning does not
  fire and dedup drops nothing.

* **`read_lpjml_hydrology()` checks the run's year coverage, and no longer
  documents a last year it does not read (#598).** Requesting a year outside
  the run's own time axis used to fail deep inside `ncdf4` with
  `NetCDF: Start+count exceeds dimension bound`, which names neither the run,
  the years at fault, nor the coverage the run does have; it now aborts with
  all three. Which years exist is a property of `run_dir`, not of the reader:
  WHEP's LPJmL runs end in 2009, 2018 and 2023 and sit side by side in one
  folder, and the file header claimed a fixed `lastyear 2009` (1308 monthly
  steps) that stopped being true when the reference run became
  `global_1901-2023_spinup_300_our_inputs_lpjml611` (LPJmL 6.1.1, 123 years,
  1476 monthly steps, the run the four LPJmL-derived pins came from, #558).
  The reader never used that constant — the span has always been read from
  the file's time dimension — so **no published value changes**: every call
  this now rejects already aborted, only less legibly, and every call that
  succeeded returns byte-identical output. The check reads the *written* time
  axis, so it still cannot see a run configured past the end of its forcing
  (the `global_1901-2018_spinup_200_our_inputs` trap of #340).

* **An `area_code`-keyed frame now resolves its GLEAM/IPCC region instead of
  silently taking the Global emission factor (#678).** The livestock emission
  helpers advertise `area_code` as an accepted territory key, but the shared
  resolver had no leg for it: it matched an explicit `iso3` column, then a
  `polity_area_code` override table covering only dissolved federations. An
  `area_code`-only frame therefore resolved 8 of the 266 reporting areas and
  every other row fell through to the Global default. The resolver now derives
  the ISO3 from `area_code` as a middle leg, taking `area_code` from 8 resolved
  areas to 214 — the same 214 an explicit ISO3 resolves. The 52 that stay
  unresolved are the dependencies, micro-states and residual aggregates GLEAM
  itself does not list, and they keep the Global default as before.

  **No published value changes.** Every in-package caller reaches the resolver
  through `prepare_livestock_emissions()`, which already attaches `iso3`:
  MEASURED on the full 241,434-row livestock frame from
  `build_primary_production()`, the resolved region is bit-identical before and
  after, with no unresolved row, so `build_livestock_ghg_extension()` and the
  manure chain are unmoved. What changes is the answer a *user* gets when
  calling `calculate_livestock_emissions()`, `calculate_enteric_ch4()`,
  `calculate_manure_emissions()` or `estimate_energy_demand()` on a frame keyed
  by `area_code` alone. MEASURED on the 2020 national head counts at Tier 1
  (AR6 GWP100): global livestock CO2e moves from 4,277 to 4,158 Mt (-2.8%), but
  the per-country change ranges from -31% (Madagascar, Tanzania, Eswatini and
  the rest of Sub-Saharan Africa, whose IPCC dairy factor is 46 against the
  Global 80) to +21% (Luxembourg, Lithuania, Latvia, Switzerland), and 176 of
  the 195 territories move by more than 1%.

* **`read_critical_n()`'s first-run fetch works from a path with a space in
  it, and its download-failure message no longer crashes (#451).** Two defects
  on the on-demand Zenodo path, both found by finally exercising it against an
  offline 7z fixture. The 7-Zip binary back-end passed the extraction
  directory to `7z` unquoted, so a cache path containing a space (a user name
  is enough on macOS or Windows) split at the space: 7-Zip read the tail as a
  member filter, extracted nothing, exited 0, and the reader then aborted with
  "did not unpack as expected" after a successful 18.4 MB download. And when
  the download itself failed, the abort interpolated the URL as
  `{.critn_archive_url()}`, which cli reads as a style name and rejects, so
  the intended message was replaced by "Invalid cli literal". Both back-ends
  (the `archive` package and a `7z`/`7za`/`7zr`/`7zz` binary) and the
  back-end selection are now covered by tests. No published value changes: the
  layers read out of a successfully extracted archive are unchanged.

* **The gridded crops layer has one key now, and it is a code (#788).**
  `build_livestock_nutrient_flows()`'s `gridded$crops` tibble was read two
  incompatible ways. `.sci_manure_crop_layer()`, the only in-package producer of
  a real layer and the one the carbon balance uses, sets
  `crop = as.character(item_prod_code)`. The nitrogen side resolved the same
  column by crop *name* only, and aborted on anything else — so the layer the
  carbon path builds could not be handed to `build_nitrogen_balance()` at all:
  measured on real data, every one of the 9,298 rows for 2010 (all 171 crops,
  1.383e9 ha) failed to match, and 1,102,005 rows over the full 1850–2023 span.

  The code is now the canonical key, documented on
  `build_livestock_nutrient_flows()` and `allocate_manure_to_land()`, and both
  consumers honour it. A crop name still resolves, as a deprecated compatibility
  bridge that warns — a name is not a unique key (`Fallow` names two
  `item_prod_code`s, and three codes carry no name), which is why the code
  direction is the one kept.

  **No published value changes.** Over the full-span layer the code-to-name map
  is a bijection (172 codes, 172 distinct names, none missing) and both keys
  resolve to the identical `item_cbs_code` for every one of them, so the
  name-round-trip workaround `inst/scripts/run_nitrogen_balance.R` carried was
  numerically lossless; it is now deleted and the driver passes
  `.sci_manure_crop_layer()`'s output straight through.

* **`build_polycell_support()` now aborts on overlapping validity intervals,
  not only on repeated keys (#758).** The existing guard keyed on
  `(cell_id, polity_code, start_year, end_year)`, so it saw only the subset of
  overlapping validity in which two intervals of one polity in one cell are
  identical. Supplying the same polity at `[2000, 2015)` and `[2010, 2020)`
  through the `geometries` argument completed with no abort and no warning, and
  emitted the shared interval `[2010, 2015)` twice per cell, doubling that
  polity's territory over those years. The producer now checks the intervals of
  each `(cell_id, polity_code)` for genuine overlap and aborts with class
  `whep_pcs_overlapping_interval`. The comparison is strict, so a succession —
  `[2000, 2010)` then `[2010, 2020)`, `end_year` being exclusive at a
  succession — is not an overlap and still splits as before. No published value
  changes: the shipped `polities` table (767 rows) carries no `polity_code`
  twice and no overlapping intervals, so no production build reached this, and
  a full offline `build_polycell_support()` from it finds zero overlapping
  pairs.

* **Parquet artifacts over 4 GiB were written corrupt, and now cannot be
  (#531).** `nanoparquet` before 0.5.0 stored column-chunk file offsets and
  sizes as 32-bit integers, so past 4 GiB (2^32 bytes) they wrapped around.
  The footer still declared every row group and row, but pointed at the wrong
  bytes for everything after the first 4 GiB: a reader returned the readable
  prefix and threw thrift "Deserializing page header failed" on the rest, so a
  consumer that did not open each row group individually silently got
  truncated data. Reproduced here on a 5.16 GiB file: 7 of 11 row groups
  readable under 0.4.2, 11 of 11 under 0.5.1, with the offsets of row group 7
  short by exactly 2^32. `nanoparquet (>= 0.5.0)` is now required, and the new
  `check_parquet_integrity()` / `assert_parquet_integrity()` verify a file's
  layout in milliseconds regardless of size (`deep = TRUE` also decodes every
  row group). `write_parquet_checked()` writes and then verifies, and the
  gridded landuse, livestock, yield and nitrogen cubes of
  `build_gridded_landuse()` and `inst/scripts/run_spatialize.R` go through it,
  so a bad write now aborts the build instead of shipping. The bytes written
  are unchanged, so no published value changes; only files that were already
  corrupt behave differently, and they now fail loudly.

* **The three polity tables are re-synced to upstream `whep-polities`
  (#835, #384).** `polity_area_crosswalk`, `polities` and
  `polity_label_aliases` are committed build products of
  `data-raw/table_mappings.R`, whose inputs live outside this repo, so
  `test_data_raw_freshness.R` could not check them and they had been shipping
  from a superseded upstream revision. They are rebuilt here from
  `eduaguilera/whep-polities` at `8e2bb78` ("Merge pull request #550 from
  eduaguilera/fix/retest-reunion-baseline"), which is that repository's `main`.

  What moved: the crosswalk goes 647 → 649 rows (upstream added
  `CPV-1800-1886` Cape Verde and `SUR-1800-1886` Dutch Guiana); `polities`
  goes 767 → 779 rows (21 new codes, 9 retired) and gains a
  `polygon_feature_date` column; `polity_label_aliases` goes 903 → 995 rows
  over 523 labels (up from 479), mostly British West Indies trade labels plus
  a new `federico_tena` source. Nine polity codes were re-dated by a year
  (Chad, Côte d'Ivoire, Ghana, Hungary, Kenya, Laos, Senegal, Syria and
  Antilles), and Indonesia's post-independence boundary moved from 1969 to
  1963.

  What that does to resolution: over the full `(area_code, year)` grid
  1850–2023 (46,284 pairs), **`polity_area_code` — the bucket the matrix
  workflows aggregate on — does not change for a single pair**, and neither
  does `mapping_status` or `has_geometry`. `polity_code` changes for 126 pairs,
  all of them FAOSTAT area 101 Indonesia, which now reports
  `IDN-1949-1963`/`IDN-1963-1976` where it reported
  `IDN-1949-1969`/`IDN-1969-1976`. Outputs carrying `reporting_polity_code`
  for Indonesia therefore change label; no aggregate changes value.
  `harmonization_tables.R` and `balance_coefficients.R` both read the
  crosswalk and their rebuilt tables are byte-for-byte unchanged, so
  `regions_full`, `polities_cats` and `urban_n_reference` do not move.

  Two behaviour changes ride along, both from upstream filling a field that
  was `NA`. `BLX-1850-1999` now publishes its successors, so FAOSTAT area 15
  Belgium-Luxembourg reaches `BEL`+`LUX` in `.federation_land_bridge()` and
  its pre-1962 production is back-cast under
  `federation_land = "successor_union"` — an opt-in; the default is `"none"`,
  so nothing changes unless it is asked for. And the three USSR periods
  (`F228-*`) gained `iso3_code = "SUN"`. The `"FSU"` alias itself is unchanged
  (`FSU` -> `F228-1945-1991`, 1961-1991); what changed is that the
  polity -> ISO3 -> area bridge behind it no longer dead-ends on `NA`, so the
  `"alias_map"` route of `inst/scripts/prepare_spatialize_all.R`'s
  grassland-share reader resolves 6,713 of 6,909 Lassaletta rows instead of
  6,682. That route is not the default either. The same fill makes
  `.successor_iso3_map("F228-1945-1991", vocab)` answer `"SUN"` rather than the
  15 republics for any caller whose `vocab` contains `"SUN"`; the LUH2
  vocabulary the one production caller passes does not.

* **`table_mappings.R` is now checked against upstream wherever upstream is
  checked out (#835).** `test_data_raw_freshness.R` gains a block that re-runs
  the builder and compares all five of its tables with the committed `.rda`,
  guarded on the three `whep-polities` files existing. It skips on CI,
  r-universe and CRAN, where they do not, so the suite still reads no `WHEP_*`
  path and touches no network — but a maintainer who *can* re-sync now finds
  out from `devtools::test()` rather than from a manual audit.

* **The cell-polity crosswalk is a pin now, so no user regenerates it
  (#694, #461).** `cell_polity_fraction.parquet` was the only one of the ten
  artefacts `inst/scripts/prepare_spatialize_all.R` produces that was not
  published: its nine siblings are pins, and it was gated behind
  `WHEP_POLITY_FRACTION_PATH`. But env-var gating is for the multi-GB
  third-party archives a user cannot be handed, and this is a 62 KB table WHEP
  builds itself from Natural Earth plus its own `inst/extdata/regions.csv`, so
  every user had to run the producer — which is how the retired-vocabulary copy
  that deleted Ethiopia and Sudan came to be the one everybody read.

  It is published as `spatialize-cell-polity-fraction`
  (`20260821T095211Z-a4952`, 62,784 rows over 58,791 cells and 178 area codes),
  and `build_cell_polity()` reads that pin **by default**.
  `WHEP_POLITY_FRACTION_PATH` and the new `polity_fraction_path` /
  `version` arguments are overrides for a local producer build, exactly the
  shape `read_polycell_support()` already had. Calling it with neither set no
  longer aborts. The published pin payload is byte-identical to the regenerated
  parquet #694 verified, so **no published value changes**.

  The #694 stale-vocabulary guard stays on, and now covers both routes: the
  override, which is the one that can go stale, and an explicitly pinned older
  `version`. `build_cell_polity()` also gained `example = TRUE`, so its example
  runs offline instead of being skipped for want of an environment variable.

* **`build_primary_production()` is now `identical()` to itself across
  sessions (#747).** The four commodity-balance extracts it carries as its
  `.cb_extracts` attribute (`fbs_new`, `fbs_old`, `cbs_crops`, `cbs_animals`)
  came back in a session-dependent row order, because the parquet reads go
  through arrow's multi-threaded scanner and nothing downstream pinned an
  order. The published frame was unaffected, but the object as a whole was not
  reproducible, so the natural `identical(build_primary_production(),
  baseline)` reproducibility check failed on a change that moved nothing.
  `.extract_cb()` now sorts on its aggregation key
  (`year`, `area_code`, `item_cbs_code`, `item_cbs`, `element`, `unit`).
  **No published value changes**: the four extracts hold the same rows with
  bitwise-identical values before and after (verified at 2010-2013 on the real
  pins, 1,070,446 rows in total), and the CBS production aggregate derived from
  them is bitwise identical, totals included.

* **Historical trade rows no longer carry a dissolved country's label
  (#719).** `.resolve_hist_trade_polities()` built its ISO3 -> area bridge by
  keeping the first row per ISO3, and the area lookup orders by `area_code`, so
  an ISO3 that names two FAOSTAT reporting areas entered as the lower code:
  `ETH` as 62 ("Ethiopia PDR", dissolved 1993) rather than 238 ("Ethiopia").
  The bridge now goes through `.iso3_area_code_bridge()`, which breaks that tie
  on the polities database (#586, #718), and the `area` label is attached from
  the resolved aggregation bucket rather than carried in from the member row --
  the rule `.aggregate_to_polities()` and `.read_crop_residues()` already
  follow.

  **No published value moves.** On the real historical-trade pins the row keys,
  `value`, `area_code` and `polity_code` are all identical before and after
  (248,508 rows, 18,639,792,136.89 t). Only the `area` label changes, on 81,388
  of those rows across 70 `area_code`s, from the plain FAOSTAT area name to the
  year-aware polity name that every other CBS source already emits -- so the
  feed stops offering a second `area` vocabulary for the same `area_code`
  (the split that dropped 702,166 rows in #382). Two identity defects are
  fixed along the way: Ethiopia is no longer labelled "Ethiopia PDR" on
  `area_code` 238, and a post-1993 Ethiopian row no longer resolves to the
  ended polity `ETH-1952-1993`; neither case occurs in the current pins, whose
  Ethiopian rows are all 1961 and whose only consumer keeps years before 1961.
  Bucket 206 also stops carrying two labels in one year ("Sudan (former)" from
  `SDN` and "South Sudan" from `SSD`).

* **Four `polity_area_code` bridges now read the crosswalk the pipeline
  resolves through, so 61 reporting areas stop being summed into bucket 999
  (#716).** `get_arable_permanent_land()`'s FAO and LUH2 legs, the crop/soil
  N2O extension's country-N bridge and the feed redistribution's cell bridge
  all built their bucket from the shipped `polity_area_crosswalk` instead of
  `.polity_crosswalk()`, where `.unfold_rest_of_world()` is applied. Since the
  Rest-of-World un-fold (#628) made promotion the default, those 61 areas --
  Syria 212, Greenland 85, Bermuda 17 and 58 more -- carry their own code
  everywhere else in the pipeline, so each bridge was aggregating onto a bucket
  the side it joins against no longer has. Measured on real inputs:
  `get_arable_permanent_land(years = 1850:2022)` goes from 194 to 227 areas and
  moves 786,562,273 ha-yr of cropland out of bucket 999 onto the areas that
  reported it (Syria alone 693.6 M ha-yr); the total falls 0.017%, entirely
  pre-1961, because each territory now splices its LUH2 back-cast on its own
  FAO-1961 anchor rather than on the aggregate's. In the crop/soil N2O
  extension the nitrogen that reaches crop shares rises 0.048% for synthetic
  fertiliser and 0.085% for applied manure (2015-2020) -- bucket 999 has no
  crop shares, so that mass was previously dropped outright. In the feed
  redistribution 151 cell-polity rows (0.18% of gridded land) keep their own
  code instead of being re-keyed to a bucket the demand does not carry.

* **`polity_area_crosswalk` no longer names an ISO3 stem after a polity
  (#711).** Two of its columns were `reporting_polity_code` and
  `reporting_polity_name`, the package's own published names for "the polity
  itself" (`?whep_polity_columns`), but held the ISO3-like stems and legacy
  labels this package vendors from `regions_full.csv`: `"ARM"`, `"ROCE"`,
  `"REUR"`, and **0 of the 641 non-`NA` values was a `polities$polity_code`**.
  The table's own documentation sent readers there to ask which territory a row
  belongs to, so following it returned a column that answers nothing -- the same
  trap #687 removed from `regions_full`, in the opposite column. They are now
  `legacy_polity_prefix` and `legacy_polity_name`, matching the vocabulary #687
  settled on, and the `@format` block documents both as explicitly *not* an
  identity. **This is a published schema break** for anything reading those two
  names off the crosswalk; the answer they appeared to promise is the table's
  `polity_code`/`polity_name`, and in a WHEP *output* it is
  `reporting_polity_code`, which is unchanged and still a real periodized code.
  **No published values change**: every cell of the table is byte-identical and
  nothing in the package computed from the renamed pair except
  `resolve_polity_label()`'s refusal list, which reads the same values under the
  new name and returns the same result.

* **`cell_polity_fraction.parquet` regenerated, and a stale copy is now
  refused (#694).** The deployed fractional cell-to-polity crosswalk had been
  rasterized through an older `inst/extdata/regions.csv` than the centroid
  `country_grid` beside it: it keyed Ethiopia `62` and Sudan (former) `206`,
  plus `6`, `125`, `192` and `205`, where today's lookup uses `238` / `276` and
  folds the other four upstream. Any consumer that adopted it deleted Ethiopia
  and Sudan outright -- 27.10 Mha of harvested area (2.0 % of the global
  1,365.9 Mha) and 332.0 M head (1.16 % of 28.638 bn), measured at 2015 in
  #461. Re-running sections 1 and 1b of
  `inst/scripts/prepare_spatialize_all.R` against the same Natural Earth
  polygons rebuilds it in the current vocabulary: 62,784 rows over 58,791
  cells and 178 area codes, exactly the 178 the centroid grid carries, so the
  `setdiff()` between the two grids is now empty in both directions. The
  centroid `country_grid` re-derives row-for-row identically (58,795 cells,
  178 codes, every code equal), so only the
  fractional artefact moved.

  `build_cell_polity()` now aborts with class `whep_stale_cell_polity_grid`
  when the parquet it is handed carries an area code today's `regions.csv` no
  longer has, naming the codes, the cells affected and the producer re-run.
  This is deliberately fatal rather than a warning: the #461 warning already
  made the loss visible and did not stop it, because the deletion lives in the
  artefact. **No published value changes** -- the `"centroid"` /
  `"polycell"` crosswalks that back published runs are untouched; what changes
  is that `country_grid = "fraction"` no longer loses two countries, and that
  a stale local copy fails instead of quietly deleting them.

* **`resolve_polity_label()` now covers the current year (#712).** Its year
  filter read `polity_end_year` strictly exclusively, so a polity whose interval
  ends at the open-period sentinel stopped covering its own terminal year: at
  2025 only 1 of the 204 ISO3 codes in `gleam_geographic_hierarchy` resolved,
  against 204 at 2024, while `add_polity_code()` resolved them normally because
  the numeric route already goes through `.polity_join_end_year()`. The label
  route now applies the same convention -- exclusive at a succession, inclusive
  at an open end (#577) -- and a period upstream records no successor for also
  covers its last year away from the sentinel (`ANT-1961-2010` in 2010).
  Declared containment still outranks that widening, so a succession year keeps
  resolving to exactly one polity and cannot become ambiguous (#720).

  **What changes for callers.** Resolutions are only ADDED, never moved: over
  1,020 identifiers x 1850:2026 the fix turns 700 `NA`s into codes (680 at the
  2025 sentinel, 20 in the last year of a terminated period) and changes no
  answer that already resolved. Any consumer that asked the label route about
  the snapshot's last year -- `mueller_synthetic_n`, `crops_manure_n`, the GLEAM
  tables, `R/sources.R` -- got `NA` for essentially every country and now gets
  the polity. No packaged value changes: the two in-package callers resolve
  below the sentinel (`expand_trade_sources()` stops at 2014,
  `add_present_day_polity()` asks `max(end_year) - 1`), so both are unmoved.

* **The destiny-share interpolation is keyed on `area_code`, not on the `area`
  label (#691).** `.interpolate_destiny_shares()` named the label beside the
  code in its skeleton join, its anti-join and its dedup — the last year-free
  territorial join in the package that read a label, and the shape behind #589
  (a shared label diluted Syria's livestock by 12x) and #563. It could not
  disagree in the current build, because `balance` and `destiny` are two
  filters of one frame, but the guarantee was the caller's rather than the
  function's and an unmatched key here drops a row silently instead of
  aborting. The keys are the code now and the one display label per code is
  re-attached at the end. **No published value changes**: on the real
  1850-2023 frames the old and new function return 20,314,086 rows with the
  same `(year, area_code, item_cbs_code, element)` key set (0 keys either
  side), the same 2,845,173 summed `dest_share`, a maximum per-key difference
  of 0 and one label per code in both. Measured on a fixture where the two
  sides disagree about the label, the old keys turned a 6-row skeleton into 2
  and lost two years of shares entirely.

* **`inst/scripts/prepare_spatialize_all.R` no longer reuses a production
  cache built under an older area model (#657).** The on-disk
  `.prod_cache.parquet` was invalidated on the requested year span alone, so a
  cache written before the polity restructure (#628, published areas
  195 -> 216) kept being reused for as long as its years covered the request,
  and every spatialize pin derived from it inherited the old `area_code`
  vocabulary (codes `276`/`277` instead of `206`, and no cell of their own for
  the 21 areas promoted out of bucket 999). The cache is now keyed on a
  content hash of the package's whole data payload plus the cached table's
  own column set
  and sorted `area_code` domain, recorded in a `.prod_cache.meta.rds` sidecar
  written next to it. A cache with no sidecar -- which is every cache deployed
  today -- is discarded with a warning naming the reason. No published values
  change from this commit alone; the next run of the prep script rebuilds
  production instead of reading a stale table, which is where the spatialize
  pins pick up today's area model.

* **`build_n_inputs()` and `build_nitrogen_balance()` now take
  `polity_validity` (#727).** Both gained the same
  `polity_validity = c("keep", "flag", "drop")` argument the four gridded
  builders they call already had, and forward it to all of them:
  `build_ag_land_support()`, `build_n_deposition()`, `build_urban_n()` and
  `spatialize_country_n_to_crops()`. The choice is then applied to the
  assembled inputs and to the balance rows themselves, so one call decides the
  fate of every row whose `(area_code, year)` resolves to a polity that did not
  exist in that year, instead of each builder deciding on its own key space.
  `"keep"` stays the default, so **no published value changes**: a default
  build is byte-identical and still only warns. `"flag"` now adds
  `reporting_polity_out_of_span` to both outputs, and `"drop"` removes those
  rows throughout the chain. Under `"drop"` a non-item input supplied directly
  (a `carbon_balance` table, say) can outlive the support rows it must be
  allocated over; that aborts in the existing mass check, whose message now
  names `polity_validity` as the cause.

* **`fill_proxy_growth()` no longer lags or smooths a proxy across the
  boundary between two series (#608).** The proxy lag and the
  `proxy_smooth_window` moving average were taken within the *aggregation*
  group of a `"variable:group"` proxy spec rather than within the individual
  series, so when two members of one group had non-overlapping but adjacent
  year coverage, the first year of the later series took the last value of the
  earlier one as its own previous observation. The `year == lag_yr + 1`
  adjacency guard cannot see the difference. In the issue's fixture (ESP with
  `gdp` to 2002, FRA from 2003, both in region `eu`) FRA's 2003 growth rate
  came out as 7.264463 -- FRA's 2003 `gdp` over ESP's 2002 `gdp` -- where the
  correct answer is `NA`, and a third member of the group with a 2002
  observation was inflated 8.26-fold (200 to 1652.893). Both are now `NA`.
  **No published values change**: every `fill_proxy_growth()` call in the
  package passes a plain numeric proxy column, for which the aggregation group
  already equals `.by`, and a 11,333-row randomised fixture on that form is
  bit-identical before and after.

* **Fodder is now built on `area_code` alone, and no longer counts a country
  once per historical name (#655).** `.merge_euadb_fodder()` and
  `.fill_fodder_gaps()` carried the periodized area *label* in their join and
  grouping keys, so one `area_code` whose label changes over the series became
  several independent series, and `.fill_fodder_gaps()`'s cross join then gave
  each of them the full year span. Egypt (59) ended up with three full-area
  copies of every fodder item — 308.6 Mt of clover in 2017 where FAOSTAT-scale
  production is 55.4 Mt. The label is now resolved once, at the end, from the
  polity crosswalk for the row's own year, the same rule
  `.aggregate_to_polities()` labels a bucket by. **Published values move**: the
  fodder table drops from 63,484 to 60,556 rows, total harvested area by 5.37%
  (9.353 to 8.851 Gha summed over 1961-2019) and total production by 7.36%
  (226.3 to 209.6 Gt); 2,574 duplicated `(year, area_code, item, unit)` keys
  become none, and 1,709 further keys change because `ha_share` and the
  year-axis interpolation now run over one series per country instead of one
  per label (Czechoslovakia, Germany, Romania, Poland, Hungary, Bulgaria,
  Greece, Finland). A side effect is that a fodder row now always carries the
  same `area` label as the FAOSTAT crop row it is bound to: 745 of 5,769
  `(year, area_code)` pairs disagreed before, none do now.

* **Three input pins were refreshed to their current upstream releases, and
  processing coefficients now reach 2023 with real data instead of a 7%
  stub (#449).** `faostat-fbs-new` had been carrying a pre-October-2025 FBS
  vintage: 4,660,700 rows ending in 2022. Its version stamp
  (`20260325T113807Z`) recorded when the file was uploaded, not when FAO
  published it, so the staleness was invisible. The current FBS release
  (2025-10-28) has 4,820,497 rows and runs to 2023, with a complete
  `Processing` element for that year (5,769 rows, 211 areas, 83 items).
  `faostat-cbs-new` moves to the 2026-06-15 CB release, growing from 58,107
  rows and 11 items to 127,558 rows and 13 items. `population_yg` moves from
  1860-2021 to 1860-2023, taking the two new years from Spain_Hist's own
  output rather than repeating 2021 (Spanish national population 47.37,
  47.79 and 48.33 million over 2021-2023).

  **Published values move.** `get_processing_coefs()` for 2023 goes from 359
  rows, 108 Mt and 14 of 45 input items to 5,986 rows, 1,548 Mt and all 45 --
  16.7x the rows and 14.3x the tonnage. That year previously held only what
  could be inferred from downstream production (oilseed crush, sugar and the
  milk-to-butter path); cereal and fruit processing, so beer, wine and flour,
  were absent entirely. The overlap years move much less: total processed
  tonnage shifts -0.374% (2019), -0.511% (2020), -0.104% (2021) and +0.607%
  (2022). Those are far smaller than the underlying FBS revision, which moves
  summed `Processing` by -6.3% to +2.4% across 2010-2022, because the
  per-country calibration in `build_processing_coefs()` absorbs most of it.

  The 2026-06-15 CB release also adds a `Processed` element (code 5023) for
  rubber, wool and silk. `.harmonize_element_names()` has no entry for it, so
  it passes through unmapped and `.extract_fao()` filters it out before
  `.get_fiber_tobacco()` runs, even though `cbs_trade_codes` maps all three
  onto CBS items (Rubber 2672, Wool (Clean Eq.) 2746, Silk 2747). Behaviour
  is therefore unchanged by this release, but the flow is not negligible --
  in 2023 reporting countries processed 14.66 Mt of the 15.62 Mt of natural
  rubber they produced, and 0.591 Mt of 0.606 Mt of silk-worm cocoons.
  Consuming it would give those items a processing flow they have never
  carried, so it is tracked as #811 rather than folded in here.

  `inst/scripts/prepare_faostat_balances.R` fetches these domains from the
  FAOSTAT bulk endpoint and reports the year span each file actually
  contains, so the next refresh is traceable to a dated FAO release rather
  than to whoever last downloaded a file by hand.

* **`build_grass_natural_carbon_inputs()` now sums all fourteen natural plant
  functional types LPJmL 6.x writes, not the eleven LPJmL 5.x had, raising
  natural-land soil carbon input by ~7%.** The natural-land carbon input selects
  PFTs by name, and the list was written for 5.x. LPJmL 6.1.1 adds `tropical
  broadleaved evergreen tree floodtolerant`, `C3 graminoid flood tolerant` and
  `Sphagnum moss`, so on a 6.x run those three matched nothing and their net
  primary production was dropped with no error and no warning. Measured on
  `global_1901-2023_spinup_300_our_inputs_lpjml611` at 2010: the mean natural
  carbon-input density rises from 6.739 to 7.218 MgC/ha (**+7.12%**), changing
  33,510 of 58,795 cells, of which 10,835 rise by more than 10%; the largest
  single-cell change is +16.25 MgC/ha. The excluded share grows over time, from
  3.31% of natural net primary production in 1901 to 8.92% in 2023, so the old
  behaviour biased trends as well as levels. The two flood-tolerant types carry
  most of it (3.33% and 3.39% of natural NPP at 2010); Sphagnum moss is the
  smallest at 0.58%, though it appears in the most cells.

  Downstream, this raises the carbon input to `build_carbon_balance()` for
  natural land and therefore its equilibrium SOC, so it moves in the *opposite*
  direction to the excess-natural-SOC question in #799 — it is a correctness fix
  to the PFT set, not a calibration change, and #799 still needs its own answer.

  **The pinned default path is not yet affected.** `lpjml-grass-natural-net-c`
  stores the already-summed natural density, so a caller without a run directory
  still receives the eleven-band numbers. The pin must be regenerated from a
  6.x run for the two paths to agree; until then they differ by the ~7% above.

* **A new warning fires when an LPJmL run writes natural PFT bands the list does
  not cover.** The band selection is a name match, so a future LPJmL adding a
  PFT would silently drop it exactly as 6.x's three were dropped.
  `build_grass_natural_carbon_inputs()` now names every unmatched natural band
  instead. It warns rather than aborting, so a newer LPJmL still runs.

* **Exported functions now work when the package is loaded but not attached.**
  With `LazyData: true` the shipped datasets live in the namespace's lazydata
  environment, which `library(whep)` puts on the search path but package code
  cannot see by bare name, so calling e.g. `whep::get_polity_geometries()` or
  `whep::add_polity_code()` from a script without `library(whep)` aborted with
  `object 'polities' not found`. `.onLoad()` now binds every lazy-loaded
  dataset into the namespace as a promise, so bare-name references resolve in
  both states. No published value changes: the bindings are the same objects
  `whep::<dataset>` returns, and they stay lazy (#641).

* **Every polycell-year is now partitioned into land uses, so a territorial
  quantity can be attributed to a land class instead of being assumed
  agricultural or dropped (#423).** `build_polycell_land_uses()` splits each
  polycell's `land_area_ha` into `cropland`, `grassland`, `urban`, `natural` and
  `unclassified`. The *level* of each agricultural class comes from the
  statistical record, which is authoritative; LUH2 supplies only the
  within-country spatial pattern, taken from its `fraction` (LUH2's share of the
  whole cell) so the classes tile the polycell's measured land by construction
  rather than to a tolerance. `level_source` and `pattern_source` are separate
  columns and their per-polycell difference is emitted as
  `statistical_pattern_disagreement_ha` rather than absorbed into `natural`;
  that column is the criterion for retiring LUH2 as a source. Inland water and
  ice are never land uses.

  Two conventions are worth knowing. FAO counts temporary meadows and pastures
  (Land Use item 6633) inside arable land while LUH2 books that ground as
  grassland, so that component keeps its FAO class but is spread over the LUH2
  grassland pattern. And because FAOSTAT land use starts in 1961, the pasture
  level is back-cast before then by carrying the FAO 1961 level on LUH2's own
  national trend, mirroring what `get_arable_permanent_land()` already does for
  cropland, so the gridded grassland series does not step at the splice. A
  back-cast row is labelled in `level_source` and excluded from the
  statistical-versus-pattern diagnostic, since it would otherwise measure LUH2
  against itself.

  A national total spread by the LUH2 pattern can give a polycell more
  agricultural land than it has, driven by countries where FAO and LUH2 disagree
  about how much land is permanent pasture. Measured on the function itself at
  2020: 63.50 Mha, 1.33% of the anchored agricultural area, of which Saudi
  Arabia is 35.10 Mha and Sudan (former) 14.20 Mha. `overfull_method` selects
  the treatment and is recorded in `method_overfull`. `"spillover"` (default)
  places the excess on same-country neighbours, widening the search ring until
  it is absorbed and taking non-forested natural land before forest: at 2020 it
  places 63.45 Mha of it across 3,878 receiving polycells, at a median ring of 2
  and a maximum of 22, and names the remaining 42,765 ha in
  `unplaceable_statistical_ha`. A neighbour can only receive a class it has a
  row for, so land the pattern classified nowhere is reported rather than
  credited to a row that does not exist. `"cap"` caps pro rata and leaves the
  whole 63.50 Mha in `unplaceable_statistical_ha`. The two are alternatives,
  never fallbacks, and both close the same ledger: 4,716.99 + 63.50 =
  4,780.44 + 0.04 Mha of anchored area in.

  On a real 2020 build the five classes sum to each polycell's `land_area_ha`
  to a maximum relative deviation of 1.7e-10 over 73,873 polycells, with none
  off by more than 1e-6. Global class areas come out at natural 7,985.3 Mha,
  grassland 3,225.3, cropland 1,565.7, urban 77.9 and unclassified 71.2.

  No published value changes: this adds a producer and does not alter any
  existing output. The ledger anchors grassland on FAO item 6655 by passing
  `source = "faostat_pasture"` explicitly, which differs from
  `build_grassland_land_extension()`'s own `"luh2"` default; that divergence is
  tracked in #759 and deliberately not resolved here, because three consumers
  rely on the current default and one of them is this ledger's own cropland
  anchor.

* **`create_n_prov_destiny()` derives processed items itself, and the
  substitution now conserves nitrogen exactly.** Processing shares and
  processed amounts come from the `processing_coefs` pin instead of the
  externally built `processed_prov_fixed` pin, and a processed item (wine,
  oil, flour) now subtracts its share from the primary crop's own production
  rather than being added on top of it. Applying every output's `cf` to the
  same input mass did not conserve N — the sum of coefficients per input item
  reaches 5.22 for maize and 5.11 for wheat, because beer and starch are
  water-diluted — so the substitution added a net +1.875 Mt N (+1.56%) over
  1860-2023, ranging from -2.75% to +7.84% by year. Output is now capped at
  the input's N and only accounted N is removed, balancing to 2.1e-16 every
  year. Where the primary-crop N of a zero-N processed item (wine, olive oil,
  sugar) should go is still open (whep#432).

* **The N left over from a zero-N processed-item substitution (wine, olive
  oil, sugar) now gets its own `processing_losses` destiny instead of
  staying folded into the primary crop's own destinies.**
  Previously, `removal_scale` only ever removed the N actually credited to
  a named output, so a near-zero-N output (wine from grapes, oil from
  olives) left most of the diverted mass sitting with the primary crop,
  inflating that crop's own `export` residual. The full processed mass is
  now always removed, the credited share still goes to the processed item,
  and the remainder is booked as `destiny = "processing_losses"`,
  `origin = "Cropland"`. No downstream surplus calculation (GRAFS plots,
  LMDI decomposition) tracks this destiny by name, so it falls into
  whatever each of them already treats as surplus, the same way
  `no_tracked_output` items already do. Concretely,
  `.create_land_surplus_df()` computes cropland surplus as inputs minus
  tracked outputs, so reported cropland N surplus rises by exactly the
  amount removed from `export`. That is the methodological choice this
  destiny embodies: the residue (grape pomace, olive cake, beet pulp) is
  counted as surplus rather than as product, pending explicit by-product
  items.

  Fixed two latent bugs surfaced while doing this. `create_n_nat_destiny()`
  re-derived national production as the sum of every `Origin == Box` row,
  which now includes `processing_losses` too, reinflating the national
  `export` residual by exactly the amount this fix removes provincially;
  `processing_losses` is now excluded from that sum and re-added as its own
  row. And `.combine_destinies()` gave every row in a multi-row
  `(Year, Province_name, Item)` group a full `production_share = 1` when
  their combined production was zero, instead of splitting evenly, so an
  item processed away entirely for a year duplicated its consumption once
  per remaining row (41 province-years, all Grapes in 1983, where fuller
  removal now reaches exactly zero where partial removal rarely did).

  **Published values move.** Over 1860-2023, the new `processing_losses`
  destiny totals 1,899,115 Mg, averaging 2.97% of Cropland-origin flows and
  rising from 2.53% in 1860 to 5.34% by 2020; olives (1,460,624 Mg), barley
  (230,838 Mg) and grapes (139,855 Mg) account for essentially all of it.
  `export` falls by the same order in both outputs: from 44,226,105 Mg to
  42,400,855 Mg (-1.83 Mt) in `create_n_prov_destiny()`, and from
  17,691,588 Mg to 15,897,066 Mg (-1.79 Mt) in `create_n_nat_destiny()`,
  whose `export` is a net residual on a different basis. Reported cropland N
  surplus rises by 1,899,115 Mg, exactly the amount the new destiny carries.
  The remaining destinies move only by what the `.combine_destinies()` fix
  stops double-counting: `livestock_rum` -35,742 Mg, `population_food`
  -21,094 Mg, `livestock_mono` -12,812 Mg, `population_other_uses` -542 Mg.
  `Cropland` and `semi_natural_agroecosystems` soil inputs are unchanged.
  National totals close to +34,403 Mg (+0.0090% of total N).

* **The GRAFS provincial chain runs to 2023 instead of stopping at 2021.**
  The `n_balance_ygpit_all`, `npp_ygpit`, `intake_ygiac` and `n_excretion_ygs`
  pins now cover 1860-2023, so the internal 2021 clip and its coverage warning
  are gone. Processing coefficients now come from `get_processing_coefs()`
  rather than the frozen July-2025 pin, so they track FAOSTAT as it is
  republished (whep#449), and with the pin refresh in whep#812 they now reach
  2023 in full: 5,999 coefficient rows globally and 82 for Spain, against the
  358 and 1 a stale `faostat-fbs-new` vintage had allowed. `population_yg`
  covers 1860-2023 as well, so its forward-fill is now a no-op. Three items
  whose processing is observed only to 2002 (coconuts), 2008 (sugar cane) and
  2018 (palm kernels) are still held flat from those years, and all three are
  negligible in Spanish production. **Published values move**: the two new
  years are structurally complete (50 provinces,
  every origin and destiny) but national N falls from 5,096,664 Mg in 2021 to
  4,607,682 Mg in 2022 and 4,203,893 Mg in 2023. That decline is in the input
  pin, not in this code — synthetic N drops 29.7% between 2021 and 2022 while
  cropland area stays flat at 50.56 Mha — and it has not been cross-checked
  against an independent source. Note also that the four analysis eras end at
  2010-2020, so 2021-2023 appear in yearly and evolution views but in no
  period aggregate.

* **The cropland destiny panel no longer silently drops items that have area
  but no tracked output.** `.allocate_by_destiny_share()` joined destiny
  shares onto item values with a `left_join`, so an item with cropland area
  and N inputs but no output in the destiny table vanished from the
  compartment-factor decomposition entirely. Those rows are now kept and
  tagged `no_tracked_output`. **Published values move**: the recovered rows
  are 1.32% of cropland area and 1.55% of N inputs over the full span, rising
  from 0.05% in the 1860s to about 5% by the 2020s, so the trend changes and
  not only the level. They are almost entirely the oilseed complex —
  sunflower seed, rape and mustardseed, cottonseed and sugar beet — whose
  output the processed-item change reassigns, which makes them a quantified
  instance of whep#432.

* **Three smaller accounting fixes in the typology and decomposition
  analysis.** The Finn flow matrix was built transposed, so `.calculate_finn()`
  read outflow where it needed inflow; mean FCI moves 0.1105 to 0.1102, but up
  to 0.035 absolute for a single province-year, and the 1860-versus-2020 trend
  tilts (whep#430). `decompose_manure_losses()` compared applied manure against
  an excretion total covering only the livestock-unit species in the panel, so
  `loss_frac` was measured against a base 0.16% to 1.26% too small, a gap that
  widens over time. The N surplus panel filtered out negative values as noise;
  a negative surplus is a real soil deficit, and keeping it lowers pre-1950
  decade means by up to 6% while affecting 86 of 8,200 province-years, all of
  them before 1950.


* **`build_historical_land_areas()` no longer rasterises its own cell-by-polity
  intersection; it reads the polycell support.** whep#776 built a second answer
  to a question whep#619 had already answered better: `.polity_cell_cover()`
  ran `terra::extract(exact = TRUE)` over every polity polygon, where
  `read_polycell_support()` is the same intersection measured geodesically with
  `sf::st_area()` on s2, keyed on each polity's validity interval, conserving by
  construction, and unable to give one cell to two overlapping polities at once.
  The rasteriser, its grid template, its lon/lat lookup and the `sf`/`terra`
  package assertion are all gone; this path now touches neither package. The
  weight is `polity_area_ha`, the polity's territory in the cell, renormalised
  to one per cell exactly as before — not `land_area_ha`, because
  `build_polycell_support()` apportions inland water pro rata by
  `polity_area_ha`, so within a cell the water cancels in that renormalisation
  except where its cap bites, and there 1,502 polycells covering 62.4 Mha
  (Canada on the Great Lakes and Hudson Bay, the USSR on the Caspian and Arctic
  shores) carry `land_area_ha == 0` and would lose their claim on the cell
  outright (whep#800).

  **No published value moves on this commit**, because
  `land_method = "present_day"` is still the default and the
  `"historical_polity"` path reads the `historical-land-areas` pin rather than
  recomputing. What moves is what `data-raw/historical_land_areas.R` now
  produces, and the pin has to be regenerated and re-uploaded for any of it to
  reach a user. Regenerated over 1850-1961: 18,922 rows / 215 buckets becomes
  17,187 / 198, global cropland −0.007% at 1850, −0.004% at 1900, −0.108% at
  1950 and −0.440% at 1961, and Ethiopia is unchanged to four decimals at every
  checkpoint. 84% of shared bucket-years move by less than 0.1% and 87% by less
  than 1%. The large movers are territories the old route was **halving**: an
  aggregate's polygon overlaps its members', so a cell claimed by both was split
  between them, and Belgium came out at 0.567 Mha of 1961 cropland instead of
  1.015, Luxembourg at 0.037 instead of 0.063, New Caledonia at exactly half and
  American Samoa at 51%.

  **The loss of coverage is the other side of that, and it is the part to
  review.** `build_polycell_support()` excludes `polity_type == "aggregate"`,
  because the support must be a partition and an aggregate's polygon overlaps
  its members'. Nine reporting buckets whose only pre-1962 territory is such an
  aggregate therefore drop out — Belgium-Luxembourg, Yemen, the Netherlands
  Antilles and the six "Other" residual regions, together 2.04 Mha of 1961
  cropland as the old route measured it — and **Viet Nam keeps 1886-1953
  unchanged but loses 1954-1961**, the span its combined-reporting entity
  covers. The other eight (Cayman, Gibraltar, Mayotte, Anguilla, Turks and
  Caicos, Wallis and Futuna, South Georgia, the French Southern Territories,
  0.001 Mha between them) carry a polygon in `polities` but no row in the
  *published* polycell pin, which predates the 2026-08-13 polity ingest; a
  refreshed polycell pin restores those. `build_historical_land_areas()` warns
  with the codes and separates the two causes, so neither loss is silent.

* **The milk FAOSTAT reports as churned into butter is no longer counted as
  milk eaten.** `cb_processing` gained the one dairy pathway it lacked,
  "Milk - Excluding Butter" to "Butter, Ghee". Without it, item 2848 carried a
  `processing` destiny with nowhere to go, so
  `.cbs_redistribute_notprocessed()` split that mass pro-rata across food,
  feed, other uses and export and deleted the `processing` row. The current
  behaviour was not an omission but a claim about diets: that 198 Mt of milk
  a year is drunk as milk (#757).

  **Published values move, from 2010 onwards only.** The old Food Balances do
  not report a `processing` destiny for milk at all — 1.0 Mt over 2010-2013
  against the new series' 837.5 Mt — so no year before 2010 changes. World
  2010 across the 180 areas shared with the `faostat-fbs-new` pin, Mt, WHEP
  before to WHEP after against FAOSTAT: milk food 649.1 to 497.3 against
  497.2; feed 74.3 to 60.6 against 60.6; export 120.6 to 89.0 against 89.0;
  `processing` 0.0 to 198.2 against 198.5. Milk food protein falls by 5.0 Mt,
  which is the whole of the milk discrepancy reported in #500 section 5.
  Butter is unchanged, production 9.27 Mt against FAOSTAT's 9.37. The
  remaining 3.2% gap in milk domestic supply is the dropped
  losses/residuals/tourist renormalisation of #412, which this does not touch.

  The fraction is 0.045, the median "Butter of Cow Milk" extraction rate over
  the 69 countries reporting one in FAO (1997), *Technical Conversion Factors
  for Agricultural Commodities* (range 3.3-7.3%). Per-area calibration lifts
  it to an effective 0.0468 for 2010, against the 0.047 the FBS itself implies
  (global butter production over milk processing, 0.044-0.047 across
  2010-2019). Every country reporting butter production also reports milk
  processing, and none reports butter without it.

  `.cbs_add_processed()` gained `.resolve_processed_production()`, because
  butter is the first processing output outside the "Crop products" group,
  whose read production is dropped wholesale on the grounds that the pathway
  always replaces it. For butter the pathway is silent before 2010, so a
  positive pathway estimate now supersedes the read production and a zero or
  absent one leaves it standing. Without that distinction the trace of milk
  processing the old FBS records in some areas emits an empty butter row that
  cancels the observed one, taking world 2000 butter production from 7.378 to
  3.527 Mt.

  **Items other than milk still lose their processing destiny.** Sugar (Raw
  Equivalent), animal fats, coconut oil and 13 smaller items have no pathway
  either, and roughly 17 Mt a year is still redistributed onto food and feed:
  2010 coconut oil food is 58% above FAOSTAT's, ricebran oil 45% and
  cottonseed oil 15%. Those carry almost no protein, so the nourishment axis
  is largely unaffected, but the mass accounting is not. That residue is
  unchanged here.

* **The polycell is now WHEP's spatial support unit, and it carries a measured
  territory instead of a whole grid cell.** `build_polycell_support()` returns
  one row per 0.5-degree cell intersected with a polity over that polity's
  validity interval, with the territory decomposed into
  `polity_area_ha = land_area_ha + inland_water_ha + ice_area_ha`, all geodesic
  from a spherical (`s2`) intersection of the polity polygons. Aggregating
  polycells to a polity changes no absolute value and no quantity crosses a
  border it does not belong to, which neither of the two conventions it
  replaces could offer: centroid assignment gave a whole border cell to one
  polity, and the fractional crosswalk multiplied a valid partition of the land
  by the **whole cell's** area. That last defect over-counted the global land
  base by **11.0%** -- 14.3195 Gha of whole cells against 12.9931 Gha of LUH2
  terrestrial area -- and it is the mechanism behind the inflated per-hectare
  deposition rates. New: `build_polycell_support()`, `expand_polycell_years()`,
  `read_polycell_support()`, `read_glwd_water()`, `read_glaciated_areas()`,
  `read_luh2_terrestrial()` and `polycell_example_geometries()`.
  * **Four definitions of "land" are live and they disagree by up to 10%**, so
    a global area is only interpretable next to the one it was measured on. At
    2015: whole 0.5-degree cells **14.3195 Gha**, HaNi's own land mask
    **13.5977 Gha**, the union of the live polity polygons **13.4267 Gha**,
    LUH2 terrestrial `(1 - icwtr) * carea` **12.9931 Gha**. The support table's
    territory is the third, but *summing* `polity_area_ha` does not reproduce
    it: the union is unique ground, while a sum counts shared ground once per
    claiming polity, so the sum at 2015 is **13.4599 Gha**, above the union by
    the **0.0332 Gha** two live polities both claim. The fourth is a
    validation layer whose disagreement is emitted in the `"unassigned"`
    attribute and never silently reconciled; the first is the convention
    being replaced. A fifth mask (the
    GLWD water layer's CRU mask, 67,420 cells) is reconciled in
    `"water_unmatched"` rather than joined away. Re-derivable with
    `inst/scripts/diagnose_polycell_support.R`; the polygon row moves with the
    polity vintage and is measured by `inst/scripts/reconcile_polity_areas.R`.
  * **`ice_area_ha` does not vary historically.** It comes from
    `ne_10m_glaciated_areas`, a coarse present-day snapshot, so a historical
    run carries today's ice extent and land that lay under ice in 1850 is
    credited to `land_area_ha`. That is accepted **only** because ice is a
    reporting category and not a driver: nothing divides by `ice_area_ha` or
    drives a flux with it. If ice ever becomes a driver the source has to be
    reopened. Inland water comes from the GLWD lakes-and-rivers layer at
    30 arcmin (Ostberg et al. 2023,
    <https://doi.org/10.5194/gmd-16-3375-2023>), not from
    `ne_10m_lakes`, which carries roughly half of global inland water and omits
    the Caspian.
  * **The table keys on `polity_code` and nothing else.** `area_code` rides
    along as a label. `polity_area_crosswalk` folds 505 polity codes into 201
    reporting buckets, 113 of which hold more than one polity and one of which
    (206) holds Sudan and South Sudan simultaneously, so a table whose purpose
    is correct territorial attribution is not keyed on it. Consumers convert at
    their own boundary, and **that conversion is where the lossy fold
    happens** -- visible at the consumer rather than hidden in the support.
    `build_n_deposition()` refuses an unconverted support instead of converting
    one silently.
  * The default grain is interval-keyed, one row per polycell per interval,
    because no area column varies by year; `expand_polycell_years()` gives the
    per-year view on demand. `start_year` is inclusive and `end_year` is
    **exclusive at a succession** but **inclusive at the open end**, so a
    handover year resolves to the successor alone and the current year still
    resolves to the polity nothing succeeds.
  * **A repeated polycell key now aborts instead of losing territory in
    silence.** The interval split reads the next breakpoint with
    `dplyr::lead()`, which is the next breakpoint only while
    `(cell_id, polity_code, start_year, end_year)` is unique. Two rows sharing
    it interleave, every second row comes back with `end_year == start_year`,
    and an empty interval resolves to no year at all: measured on a two-piece
    fixture, 70 of a polycell's 100 ha resolved to nothing at every year of its
    life, with no error, no warning and every conservation check still passing.
    `build_polycell_support()` now aborts with class
    `"whep_pcs_repeated_key"`, naming the count and up to three offending keys.
    It does not sum the duplicates: a repeated key means the geometry table is
    not one row per polity interval, and repairing the arithmetic would leave
    the fan-out that produced it invisible. **No published value changes**: the
    shipped 753-row polity table repeats no
    `(polity_code, start_year, end_year)` among the 666 rows that get clipped,
    so no production build reaches the guard, and any input that did not carry
    a repeated key returns exactly the table it returned before.

* **Atmospheric deposition is now split as a mass over territory, and its two
  land definitions are separated.** `build_n_deposition()` splits each cell's
  HaNi mass across the polities holding the cell in proportion to
  `polity_area_ha` (`split = "auto"` takes it when the support carries it and
  the old `polity_frac` otherwise; either can be demanded explicitly, and a
  demand that cannot be met aborts), then decomposes each polity's share over
  land, inland water and ice (`categories = "auto"`). Both choices are
  recorded in `method_polity_split` and `method_area_split`, so a table's
  split is readable from the table.
  * **WHEP's territory governs *placement*; HaNi's land mask governs the
    *total*.** The mass placed is HaNi's block sum, and HaNi is referenced to
    the whole 5 arcmin cell inside a land-masked domain whose mask is a third
    land definition at 13.5977 Gha. Nothing re-references the mass to WHEP's
    land: forming a rate on the whole cell and multiplying by `land_area_ha`
    would shed about 9% of the source mass, and re-referencing to HaNi's own
    mask would move the global total by about 4.5%. A global sum out of this
    function is therefore HaNi's total redistributed onto WHEP's territory,
    conserved exactly against the source (34.77 Tg NHx in 2014). Source: Tian
    et al. 2022, <https://doi.org/10.5194/essd-14-4551-2022>.
  * **Deposition scope is selectable and defaults to the whole territory.**
    `build_n_inputs(data = list(deposition_scope = ))` takes `"territory"`
    (default: land plus inland water plus ice) or `"land"`, recorded in
    `method_deposition_scope`. The default is a scientific choice, not a
    conservative one: nitrogen deposited on a lake or a glacier still drives
    indirect N2O and still reaches the eutrophication pathway, so restricting
    the ledger to the terrestrial share would discard 0.89 Tg N of real flux
    that the impact terms have to account for. `"land"` remains available for
    the purposes that want it and aborts if the support cannot be decomposed,
    rather than silently returning the whole territory. Under the default the
    ledger output is bit-identical to before the split.
  * **Known limitation, not a rounding error:** **eight** reporting areas the
    deployed crosswalk carries -- 61 Equatorial Guinea, 153 New Caledonia, 154
    North Macedonia, 209 Eswatini, 212 Syria, 299 Palestine, 276 Sudan and 277
    South Sudan -- receive no deposition through the polycell path. The first
    six fold onto `ROW-1850-2025` (`polity_area_code` 999, `fabio_row_fold`)
    while their own `GNQ-`, `NCL-`, `MKD-`, `SWZ-`, `SYR-` and `PSE-` codes
    resolve onto that same bucket 999 through the `fabio_row_promoted` rows
    added in #785, so their territory is folded into Rest of World rather than
    dropped: measured on this snapshot, `GNQ-1968-2025` builds 18 polycells
    (2,702,545 ha) and `MKD-1991-2025` 21 (2,539,428 ha), every row stamped
    `area_code` 999. Before #785 these codes carried no crosswalk row and were
    dropped outright, so the territory is now retained but still not attributed
    to the reporting area. Sudan and South Sudan do resolve, but both onto 206,
    Sudan (former), so neither 276 nor 277 is reachable on its own. **The gap is
    identity, not extent**: of the six with a directly comparable official
    area, all sit within 3.7% of it (Syria +0.90%, North Macedonia -1.23%,
    Eswatini -1.30%, New Caledonia +1.17%, Palestine +3.22%, Equatorial Guinea
    -3.65%). **Fiji is no longer among them**: since the polities refresh in
    #662 the crosswalk maps area 66 onto `FJI-1800-2025` through
    `upstream_map`, and the polycell build returns 60 polycells holding
    1,871,003 ha, all measured on `s2`, reproducing the polity's own polygon
    area exactly. How this ranks against the ledger's other open terms has not
    been measured, so no claim is made about it.

* **Migrating a consumer onto the polycell: what to change and what moved.**
  The transitional shim that let `build_polycell_support()` masquerade as the
  old crosswalk (a `polity_frac` column plus padding rows for cells the
  intersection did not reproduce) is **gone**. A consumer that used to
  multiply a rate by `cell_area_ha * polity_frac` now multiplies it by
  `polity_area_ha`, or by `land_area_ha` when the quantity is genuinely
  terrestrial, and converts `polity_code` to its own reporting vocabulary
  before joining. Migrated here: deposition (`build_n_deposition()`), the
  synthetic-N grid split, the carbon path (`build_carbon_balance()` and its
  land inputs) and the compartment keying in `spatialize()` /
  `spatialize_livestock()`, which now **abort** on a support carrying no
  polity share instead of defaulting `cell_area_frac = 1` and handing a border
  cell wholly to one polity.
  * **Measured movement, at polity grain**, is entirely in the deposition
    input term: `n_input_full_t` -0.504%, `n_balance_t` -2.252%,
    `surplus_t` -1.055%, `total_gwp_co2e_kg` -0.137%. Of the -678,612.5 t N,
    **95.6% (-648,491.3 t) is unreachable reporting areas** and only
    -30,121.2 t (0.107% of the term) is geometry, dominated by Canada
    (-1.03%). The split key itself moves 27 of 28 ledger quantities by exactly
    zero and the 28th by one ulp; the synthetic term moves by exactly 0 t.
    **Basis, because it has since moved:** this was measured on the polity
    vintage *before* #662, on which Fiji was unreachable too, so the
    unreachable share above spans **nine** areas rather than the eight that
    remain. Fiji's part of it has not been re-measured -- doing so needs the
    HaNi deposition rasters, which the measuring environment does not carry --
    so the figure is left as measured and its basis named rather than restated
    over a population it was not measured on.
  * **The island states are fixed.** Against official land areas, Kiribati
    goes from **34.3x** to **1.18**, Micronesia 17.5x to 1.00, French Polynesia
    15.3x to 1.16, Maldives 10.3x to 0.58 -- they used to draw a whole
    0.5-degree cell each while carrying no LUH2 terrestrial area at all.
  * **Greenland reads as +419% against FAOSTAT and is not a defect**: FAO's
    country area for Greenland "refers to area free from ice", so the
    comparable quantity is WHEP's territory minus its 177.5 Mha of ice, which
    reads -12.9%.
  * **Six `polity_frac` call sites remain and are deliberate.** Dropping
    `"polity_frac"` from `utils::globalVariables()` was used as a detector, and
    it named exactly six unqualified uses: `.wb_finalise()`,
    `.wb_drop_polity_cols()` and `.wb_aggregate_polity()` in `water_balance.R`,
    `aggregate_grass_to_polity()` in `feed_lpjml.R`, `.grass_to_cells()` in
    `feed_intake_redistribute.R`, and `.read_fraction_country_grid()` in
    `run_spatialize.R`. All four files are out of scope for this migration --
    the water balance is owned elsewhere, the feed path is frozen, and
    `.read_fraction_country_grid()` reads the deployed crosswalk on purpose --
    and they are **not** an oversight or an unfinished migration. The detector
    has done its job, so `"polity_frac"` is **restored** to
    `utils::globalVariables()` and `R CMD check` is back to `Status: OK`.
    Without it the check reported `Status: 1 NOTE` where merge-base main was
    `Status: OK`, on a check CI cannot fail: `check-r-package@v2` defaults
    `error-on: warning`.
* **A traded item with no production row now balances instead of vanishing.**
  `.reestimate_domestic_supply()` derives a last-resort domestic supply from
  `production + import - export` for rows that report neither a supply nor a
  destiny. `production` is deliberately still `NA` at that point, so the
  imputation further down can derive it (#142), but reading it raw made the
  residual `NA`, and `dplyr::if_else(NA, ...)` is `NA`, so both
  `domestic_supply` and `stock_variation` came out `NA`. Those rows were then
  dropped by the `value != 0` filters downstream rather than balancing. A
  missing production now counts as zero in that residual only; the imputation
  itself is untouched.

  **Published values move, slightly and in one direction.** On a 2010 build:
  12 rows are recovered and none is lost (17,648 to 17,660); 81 rows gain a
  domestic supply that was wrongly zero, the largest being Ireland
  "Miscellaneous" at 79,000 t, Switzerland at 22,000 t and Yemen tea at
  17,000 t; world domestic supply rises 212 kt on 63,388 Mt (+0.0003%) and food
  181 kt on 4,836 Mt (+0.004%). Every change is upward from zero. The
  supply-use identity improves sharply: rows off by more than 1 t fall from 144
  to 67, the worst residual from 160,000 t to 29 t, and the 12 `NA` residuals
  disappear.

* **`build_sjos_nitrogen()` gains `nourishment_band`, which makes every band
  choice selectable from the driver.** The quality tier
  (`quality_method` / `quality_variant`), the loss wedge (`wedge_method` /
  `wedge_coverage`) and the band's own `shortfall`, `ceiling` and
  `requirement_sd` were all reachable on their builders but frozen at their
  defaults inside the driver, so a sensitivity could not be run end to end.
  `ceiling` in particular is the knob the band's documentation names as WHEP's
  own criterion and asks callers to sweep.

  An option the list does not recognise **aborts**. A sensitivity analysis is
  the worst place for a silently ignored argument: the run completes, nothing
  moves, and the sweep gets reported as showing insensitivity. Nothing is
  defaulted in the driver either — an option the caller omits is simply not
  passed on, so each builder's own default applies and the two cannot drift.

* **Three guards against silent row multiplication and silent gaps**, all found
  by review of this branch rather than in the field:

  * `normalize_nourishment()` now **aborts** when a data-frame `thresholds`
    carries two rows for one `year`/`area_code`. It previously duplicated the
    country, once per candidate band and each with its own class, so every
    headcount downstream counted it twice. `build_nourishment_band()` output is
    unique by construction, but the argument accepts any data frame.
  * `normalize_nourishment()` now also warns when a matched band has a missing
    **ceiling**. The check tested the floor alone, so such a row scored `NA` and
    disappeared from the classification without a word.
  * `read_wpp_population()` now drops and **names** ISO3 codes the crosswalk
    does not resolve, instead of returning them on a missing `area_code` for
    `build_protein_requirement()` to weight into an `NA`-keyed country. On WPP
    2024 that is 7 codes at 0.03% of world population, Kosovo the largest.

  `build_loss_wedge()` additionally asserts one Annex 1 region per area. The
  packaged tables satisfy it, but `data$food_loss_regions` is injectable and two
  regions for one area would weight its whole basket twice, at two rates.

* **`build_protein_quality()` gains tier 1a and makes it the default:
  per-item measured digestibility instead of a two-rate class split.**
  `method = "trs935_item"` uses the true digestibility TRS 935 Table 5 publishes
  for each commodity — now packaged verbatim as `protein_digestibility_trs935` —
  and falls back to the tier 1b class rate for items the report does not
  measure. Table 5 prints **no fruit, vegetable, root, tuber or sugar row at
  all**, so the fallback is not a corner case: on the 2010 world basket
  **84.5%** of food protein carries a measured value and the rest takes the
  class rate. `protein_measured_share` reports it per row.

  On that basket the diet quality moves from a tier 1b median of 0.867 to
  **0.891** (0.818–0.940), lowering the floor from 67.77 to **66.23** and the
  ceiling from 98.13 to **96.04**. Against the flat band **50 of 167 countries
  change class** (58 under tier 1b), and world headcounts are **216 million
  below requirement** and 2,438 million above twice the safe level.

  `variant` brackets the one judgement tier 1a makes. Table 5 prints several
  forms of the same commodity and CBS cannot say which was eaten. **The
  processing direction is not uniform** — refining raises wheat (whole 0.86 →
  flour white 0.96, bran removed) and lowers maize, rice and oats (0.85 → 0.70,
  0.88 → 0.75, 0.86 → 0.72, through extrusion and Maillard damage) — so there is
  no single axis to sweep and the bracket is carried per item. `"default"` takes
  the least-processed form, the consistent partner for WHEP's whole-commodity
  agronomic nitrogen; `"low"` and `"high"` give a diet-quality span of 0.853 to
  0.913 at the median country. The choice is stamped in `method_quality`
  (`"trs935_item_default"`), so a sensitivity is self-labelling.

* **New `build_protein_score()`: tier 2 of the protein-quality ladder, the full
  aggregate PDCAAS.** It implements the aggregation FAO prints as a worked
  example in WHO/FAO/UNU TRS 935 Table 6 — digestible protein per item, the
  digestible-protein-weighted amino acid profile, the minimum ratio against the
  age reference pattern, truncated at 1 and multiplied by diet digestibility.

  **Averaging per-item scores is not an approximation of this.** FAO forbids it
  in words twice (TRS 935 p.99, FNP 92 p.17) and FNP 51 p.37 gives the reason.
  Because `min()` is concave, the average of item scores is a rigorous lower
  bound on diet quality and so a rigorous upper bound on the floor. The
  digestible-protein weighting is the correction TRS 935 makes to its own 1991
  report; on Table 6 it moves the lysine profile from 44.14 to 44.34 mg/g.

  Truncation follows the **TRS 935** convention — score truncated at 1, *then*
  multiplied by digestibility, so the ceiling is the diet's digestibility — not
  FNP 92's, which truncates the DIAAS itself at 1.0. For a diet at score 1.4 and
  digestibility 0.85 the two differ by 18% of the floor, and it bites on exactly
  the animal-rich diets that truncate.

  The function is **code-complete and validated but not yet wired**: it needs a
  per-item amino acid composition table WHEP does not have. It ships now because
  the aggregation is the part that is easy to get wrong, and it can be validated
  today against FAO's own example.

* **New packaged table `protein_digestibility_trs935`:** TRS 935 Table 5's 35
  measured true-digestibility values (26 single foods, 9 mixed diets),
  transcribed verbatim. It is the input tier 1a needs, and it records the
  milling spread that CBS cannot observe — wheat whole 0.86 against refined
  0.96, and three distinct maize rows at 0.85 / 0.87 / 0.70.

* **PUBLISHED VALUES MOVE: the SJOS-N nourishment axis now classifies against a
  composed, per-country-year band instead of a flat 62.1 / 85.05.**
  `build_sjos_nitrogen()` gains `nourishment_thresholds`, defaulting to
  `"composed"`. `normalize_nourishment()` accepts a data frame of per-row bounds
  — a `build_nourishment_band()` output passes straight through — alongside the
  scalar pair it always took.

  **On the 2010 build, 58 of 167 countries change nourishment class**, 21 of
  them Adequate → Under and 37 Over → Adequate. The floor moves from a flat 62.1
  to a median 67.77 (58.79–88.81) and the ceiling from a flat 85.05 to a median
  98.13 (84.84–107.70). Where the axis had one number for every country it now
  has a distribution, built from four sourced terms: the demographic
  requirement, within-country intake dispersion, the unavoidable-loss wedge and
  diet protein quality.

  Those figures are measured with **tier 1b** protein quality, the default when
  this landed. Tier 1a became the default later in the same release and is what
  ships: 50 of 167 change class, the floor median is 66.23 and the ceiling
  96.04. See the tier 1a entry at the top for the full comparison.

  `nourishment_thresholds = "flat"` restores the old pair for continuity and
  sensitivity. It is not a peer of the default: of its five underlying numbers
  only the 46 g/cap/day floor was ever sourced, and the 1.35 multiplier behind
  both bounds was a preliminary presentation figure (whep#753).

  A row that matches no band is classified `NA` and named in a warning — it
  never falls back to the flat pair, which would mix two threshold vintages
  inside one classification.

  One number in the composed band remains **WHEP's own criterion rather than a
  sourced value**: `ceiling$share`, the tolerated fraction of a population above
  twice the safe level, default 0.5. TRS 935 declines to set a tolerable upper
  intake, so nothing external fixes it. It is selectable, stamped in
  `method_ceiling`, and any published use should carry a sensitivity across it.

* **New `build_protein_quality()`: the band is no longer on crude protein.**
  TRS 935 issues its safe level "for proteins with a protein
  digestibility-corrected amino acid score value of **1.0**" (section 14.2). No
  real diet reaches 1.0, so every uncorrected band was low by at least `1/D` —
  for every country, in one direction.

  `method = "digestibility_share"` takes the diet's digestibility as the
  protein-weighted mean of **0.95 for animal and 0.80 for plant protein**, which
  is how TRS 935 Table 43 footnote b computes it. The animal/plant split follows
  FAO's own Food Balance Sheet grouping and reconciles against FAOSTAT's
  published aggregates to within 0.07% on each side.

  This is **tier 1b of four**, and a *provable lower bound* on the full
  correction, since PDCAAS is `min(1, AAS) × D ≤ D`. It is conservative about
  the **size of the correction**, not about adequacy: it under-corrects and so
  classifies fewer countries deficient than the full amino acid score would.
  Tier 2 needs a per-item composition table WHEP does not have; when it lands it
  becomes a new method rather than silently changing this one.

  Quality **divides both bounds**, which is algebraically the diet-side
  correction TRS 935 section 14.1.5 prefers: it keeps the published supply
  series untouched and moves floor and ceiling together, where correcting only
  the floor would leave the ceiling on crude protein.

  **On the 2010 build** the diet quality runs 0.82–0.91 (median 0.87), lifting
  the floor from a crude median of 58.60 to **67.77 g/cap/day** and the ceiling
  from 85.68 to **98.13**. That reverses the headline: the composed floor now
  sits *above* the retired flat 62.1, not below it. Against the flat band **58
  of 167 countries change class**, up from 21 without the correction, with 21
  moving Adequate → Under. World headcounts move from 99 to **266 million
  people below requirement** and from 3,278 to 2,258 million above twice the
  safe level.

  These are the **tier 1b** figures, measured when nothing yet composed the
  term. Both statements were superseded within the same release: the composed
  band became the default of `build_sjos_nitrogen()`, so the values do move, and
  tier 1a became the default quality method. The shipped figures are in the tier
  1a entry at the top.

* **`read_population()` can now fill its coverage gaps from UN WPP, and always
  reports where each row came from (#644).** The `gdp-population` pin does not
  reach every area WHEP models, and the two per-capita consumers inner-join it,
  so an uncovered area is absent from their output rather than wrong in it. The
  new `population_source = "pin_wpp_fallback"` fills **only** the country-years
  the pin does not reach, from `read_wpp_population()`: on the real inputs that
  is 44 areas the pin has no row for at all (Réunion, Bhutan, Comoros, Western
  Sahara, New Caledonia, the French overseas departments and the small island
  states) and 4,755 country-years inside the pin's own year span.

  The pin wins wherever both have a value, so turning the fallback on cannot
  move a denominator that was already published — it can only add one that was
  missing. It is a gap-filler rather than a replacement because the two sources
  disagree where they overlap: across 12,309 shared country-years by a median
  0.64%, a 95th percentile of 4.4% and a maximum of 81%.

  The output gains `source_pop`, carrying the pin's own vocabulary
  (`"Original"`, `"Linear interpolation"`, `"First value carried backwards"`),
  joined with `" + "` where an `area_code` bucket sums ISO3 codes of differing
  provenance, or `"UN WPP 2024"` for a filled row.

  **No published value changes**: the default is `"pin"`.

* **`nourishment_thresholds` now says which of its numbers are sourced, and its
  upper bound is renamed `"ceiling"`.** Four of the five values the shipped
  nourishment axis runs on had no source and nothing said so. A new
  `provenance` column records it per row: only the 46 g/cap/day protein floor
  is cited (WHO/FAO/UNU TRS 935 Table 46, the safe intake of a 55 kg adult —
  itself a 97.5th-percentile *individual* level that TRS 935 p.41 says is
  incorrect to apply to a population). The 63 ceiling, the 2300 and 2900 energy
  bounds and the 1.35 factor are labelled `inherited_unsourced`.

  **Breaking for anyone filtering the table**: `bound == "target"` is now
  `bound == "ceiling"` and returns zero rows under the old name.
  `normalize_nourishment()` uses that value as the top of the Adequate band,
  above which a country is classified Over, so "target" read as something to
  aim at — the opposite of its role.

  `normalize_nourishment()` also stops presenting protein and dietary energy as
  interchangeable. The arithmetic is shared, the bases are not: the energy
  bounds are unsourced, and WHEP's energy column is gross combustion energy
  where a dietary kcal threshold is metabolisable. Nothing in the package reads
  the energy path.

  **No published value changes**: the floor and ceiling are numerically
  unchanged at 62.1 and 85.05 g/cap/day and every classification is identical.

* **New `build_nourishment_band()`: both SJOS-N bounds are now composed from
  sourced terms, and the 1.35 multiplier is gone from each.** It implements
  WHO/FAO/UNU TRS 935 Box 1 — log-deficit normal with
  `S_D = sqrt(S_I^2 + S_R^2)`, prevalence `Phi(-M_D/S_D)` — and inverts it twice
  to give a floor and a ceiling on mean per-capita protein supply:

  ```
  bound = anchor * exp(z * S_D + S_I^2 / 2) / (1 - omega)
  ```

  The **floor** anchors on the demographically weighted *average* requirement at
  `z = qnorm(1 - shortfall)`; the **ceiling** on `multiple` times the
  demographically weighted *safe level* at `z = qnorm(share)`. `multiple`
  defaults to 2, which TRS 935 section 13.7 calls "twice the recommended intake,
  previously identified as a safe upper limit … likely to be safe"; 3–4× is the
  report's own sensitivity ("approach the tolerable upper limit").

  **The two tails do not take the same tolerance, and the model says so.**
  Applying the floor's 2.5% to the upper tail puts the ceiling *below* the floor
  for 162 of 167 country-years, because TRS 935 calls intakes below requirement
  harmful while calling twice the safe level "unlikely to be associated with any
  risk". `share` therefore defaults to 0.5 — "Over" means the typical member
  exceeds the limit — and that 0.5 is WHEP's construction, not a sourced value.
  At 0.5 the band never inverts: the lowest ceiling (74.81) exceeds the highest
  floor (73.75).

  **On the 2010 build**, floor median 58.60 (53.0–73.8) against the flat 62.1,
  and ceiling median 85.68 (74.8–91.5) against the flat 85.05. The ceiling's
  agreement with the retired number at the world median is a coincidence worth
  noting and not a justification: 85.05 was 63 × 1.35 and flat, this varies by
  country through demography, inequality and loss.

  It also reports **how many people**, not only the country's class:
  `prevalence_protein_deficit`, `prevalence_protein_excess`, `people_under` and
  `people_over`. A country is not uniformly under or over — on the 2010 build
  **99 million people are below requirement and 3,278 million above twice the
  safe level**, and the share below requirement ranges 0% to 48.9% *within* the
  countries the flat band called Adequate.

  The anchor is the **average** requirement from `build_protein_requirement()`,
  not the safe level, because TRS 935 says applying an individual safe level to
  a population is incorrect (p.41) and a safe population intake "cannot be
  defined as a simple function of the mean requirement" (p.241). Passing a
  safe-level requirement warns, because the formula adds its own population
  margin and would count the requirement margin twice. `shortfall` defaults to
  2.5%, fixed independently by TRS 935 Figure 7 and by FAO's stated lowest
  feasible PoU target. `requirement_sd` defaults to TRS 935's `S_R = 0.12` and
  is exposed because the report itself notes that captures only about a fifth
  of observed between-individual variance.

  **On the 2010 build the median country floor is 58.6 g/cap/day against the
  shipped flat 62.1**, ranging 53.0 to 73.8, with 39 of 167 countries above
  62.1. Where the axis had one number for every country it now has a
  distribution: demography pulls the requirement down to a median 32.0
  g/cap/day, and the dispersion margin (median `S_D` 0.29) puts most of it back.

  **The protein-quality term is not built**, and the floor is a known
  understatement without it — TRS 935's safe level is defined for a PDCAAS of
  1.0 and real diets score below that, a level shift the evidence record puts
  at +11% to +36%. `quality = "none"` is stamped in `method_quality` so the
  method name cannot silently change meaning when the term lands.

  **No published value changes.** Nothing calls this function yet;
  `normalize_nourishment()` still uses the flat threshold.

* **New `build_loss_wedge()`: the nourishment floor can now allow for the food
  that never becomes intake.** The floor asks whether supply *can* meet needs,
  so it has to account for loss between the retail shelf and the mouth — but
  only for the part no food system avoids. Avoidable waste belongs to the
  over-nourishment problem, and inflating the floor by it would turn a behaviour
  problem into an apparent adequacy failure; `omega = 0`, meanwhile, asserts
  that all edible loss is eliminable, which SDG target 12.3 does not even aim
  at.

  The wedge is built from Gustavsson et al. (2011) Annex 4, composing only the
  two steps at or after retail — `Distribution` and `Consumption` — because FBS
  food availability is already measured at the retail level and includes retail
  and consumer loss. The default `"gustavsson_half_min"` takes each rate's
  minimum across the seven world regions and halves it, giving roughly 2.5% of
  protein on the 2010 world basket (floor divisor 1.026). It is documented as a
  **deliberate lower bound, not an estimate of achievable loss**: the
  consumption-step minimum is sub-Saharan Africa in every commodity group, and
  those are scarcity figures rather than efficiency figures. `"gustavsson_min"`
  (roughly 4.9%) and `"none"` are selectable, and the choice is stamped in
  `method_loss_wedge`.

  `"gustavsson_regional_actual"` is the sensitivity arm, giving each country its
  Annex 1 region's own observed rates: 14.2% on the same basket, divisor 1.166,
  spanning 4.1% to 21.4% across countries. It is not an unavoidable-loss
  estimate, and its country structure is contested — Gustavsson's rich-high
  gradient runs opposite to UNEP's Food Waste Index — so it quantifies that
  disagreement rather than settling it. Annex 1's 152 countries cover 99.0% of
  2010 world food protein; the rest take the mean rate across the seven regions
  and are stamped `method_region = "global_mean"`, or return nothing under
  `coverage = "annex1_only"`.

  FBS element 5123 `Losses` is deliberately not used: it is pre-retail and
  already netted out of the Food element, so subtracting it would double-count.

  **No published value changes.** Nothing calls this function yet; the axis
  still uses the flat 1.35 multiplier until the remaining terms land. Two
  packaged coefficient tables are new
  (`inst/extdata/coefs/food_loss_wedge.csv`, the Annex 4 rates, and
  `inst/extdata/coefs/food_loss_item_groups.csv`, the Annex 2 item-to-group
  mapping); both are recorded in `validation/SOURCES.md`.

* **New `build_protein_requirement()`: the nourishment floor can now account
  for a population's age and sex structure.** The SJOS-N "just" axis has always
  compared per-capita protein supply against a flat 46 g/cap/day, which is
  WHO/FAO/UNU TRS 935's safe intake for a 55 kg **adult** applied to whole
  populations including children. Children need far less in absolute terms
  (17.1 g/day at ages 4-6), so the flat value overstates every population's
  requirement, and most in the youngest. The new function weights the TRS 935
  per-class requirements by an injected population-by-age-and-sex table.

  It defaults to `requirement = "average"`, the class average requirement,
  because TRS 935 states that applying an individual safe level to a population
  is incorrect (p.41) and that a safe population intake "cannot be defined as a
  simple function of the mean requirement" (p.241); `"safe"` remains selectable
  for continuity. This does **not** lower the eventual threshold — the margin
  that turns an average requirement into a supply floor is applied downstream,
  once, over the convolution of requirement and intake variability.

  **No published value changes yet.** Nothing calls this function; the axis
  still uses the flat floor until the remaining terms land. The packaged
  requirement table is new (`inst/extdata/coefs/protein_requirement.csv`), and
  its derivation and the TRS 935 tables behind it are recorded in
  `validation/SOURCES.md`.

* **Rice from the new FAOSTAT Food Balances is now converted to milled
  equivalent, so CBS item 2807 is on one mass basis.** FAOSTAT publishes rice on
  two bases depending on vintage: the historic series carry item 2805 "Rice
  (Milled Equivalent)" and 2804 "Rice (Paddy Equivalent)", while the new Food
  Balances carry item 2807 "Rice and products" in **paddy** (rough-rice)
  equivalent. `.fix_item_codes()` selected rows for the paddy-to-milled
  conversion by item name, and "Rice and products" was in neither of the two
  names it matched, so new-FBS rice was never converted. Since
  `build_primary_production()` does convert its own rice, a single item mixed
  milled production with paddy utilisation, and the difference was absorbed by
  the residual `stock_variation` plug. The extract path now recognises the
  new-FBS name as paddy; frames that have already been through the `items_full`
  lookup keep the previous behaviour, because there "Rice and products" is the
  canonical label and carries no basis information (#751).

  **Published values move.** Every element of item 2807 sourced from
  `faostat-fbs-new` falls by the 0.67 extraction rate. World 2010, tonnes:
  food 570,038,000 to 381,925,460; production 694,377,000 to 465,232,590;
  domestic supply 684,012,000 to 458,288,040; imports, exports, feed, seed,
  processing and other uses likewise. The corrected figures land close to
  FAOSTAT's own published milled-equivalent series: India 2010 production is
  96,455,210 against FAOSTAT item 2805's 96,023,000, a 0.45% difference which
  is the gap between WHEP's global 0.67 and FAO's implied 0.667. Every
  downstream consumer of rice tonnage inherits the change, including the
  nourishment axis, where rice protein per tonne of food moves from 1.550x
  FAOSTAT to 1.039x and which was how the defect was found (#500).

  **The historic series moves too, and by more than the FBS-new years.** The
  old-to-new FBS harmonisation derives its scaling ratio from the 2010-2013
  overlap, so with the new series on paddy and the old series on milled it was
  computing a median ratio of **1.4981** (= 1/0.667) for rice and scaling every
  FBS_Old rice year up by it — well inside the [0.2, 5] band
  `.clamp_fbs_scale_ratio()` allows, so nothing flagged it. That ratio is now
  **1.0037**: the two vintages agree on rice to 0.4% instead of disagreeing by
  50%. Wheat, which uses one basis in both vintages, is unchanged at 1.016 and
  serves as the control. `validation/rice_mass_basis.R` is the real-data guard.
* **A promoted Rest-of-World member now publishes under its own territory, not
  under the bucket's aggregate polity.** Lifting the FABIO Rest-of-World fold
  had promoted a member's numeric `polity_area_code` and nothing else, so all
  62 folded areas reported as themselves (`area_code == polity_area_code`) while
  still carrying `polity_code == "ROW-1850-2025"`, `polity_type "aggregate"`,
  `continent "World"` and no geometry -- a row that reports as itself and is
  identified as somewhere else. `data-raw/table_mappings.R` no longer discards
  the upstream FAOSTAT map's answer for those areas: 36 map rows over 31 areas
  that reached no crosswalk row at all are now carried as
  `mapping_source == "fabio_row_promoted"`, and `.unfold_rest_of_world()`
  chooses between them and the fold row per mode. **This is an identity change,
  and it moves quantities only at the third decimal place of a percent.** Over a
  full `get_primary_production()` (6,310,390 rows) and `get_wide_cbs()`
  (2,184,850 rows) no row and no key is added or removed;
  `reporting_polity_code` / `reporting_polity_name` change on 212,163 production
  rows across 22 areas -- Syria to
  `SYR-1946-1967` before 1967 and `SYR-1967-2025` after it, Eswatini to
  `SWZ-1894-2025`, New Caledonia to `NCL-1800-2025`, Palestine to
  `PSE-1948-2025`, and 27 more. The resolution is year-aware, so a 1950 row and
  a 2020 row of the same area need not agree. The 30 members the upstream map
  names nowhere stay on `ROW-1850-2025`; the new `row_promotion_status()`
  reports which is which and why, splitting them into `own_polity` (31),
  `polity_unmapped` (6 -- a live polity exists upstream and only the map row is
  missing) and `no_polity` (24, three of which are not territories at all).
  `options(whep.unfold_rest_of_world = "none")` still restores the fold
  crosswalk exactly, column for column.

  The quantities that do move are these, and both are pre-1961. 64 rows and
  1,722,000 t of historical trade for Guadeloupe and Martinique are
  **recovered**: their pre-1850 rows used to be dropped because `ROW-1850-2025`
  begins in 1850 and `add_polity_code()` refuses to extend an aggregate, and
  they now land on `GLP-1816-2025` / `MTQ-1816-2025` (historical trade feed
  +0.0093%). And 430 CBS rows (0.02% of the table, 340,474 t of movement, or
  3.5e-6% of its tonnage) shift between columns in 10 areas, 96% of it Eswatini
  reclassifying export as seed; `production`, `stock_addition` and
  `stock_withdrawal` are identical to the last bit. In
  `get_primary_production()` 338 rows (0.005%) move by at most 5.6e-4 in
  `t_LU`, in Italy, the Netherlands and Belgium, through the global-yield
  denominator: `.fill_yields()` keys on the `area` LABEL as well as the code,
  and that label is resolved per year, so an area whose polity changes
  mid-series has its rows completed under both labels. 39 area codes already
  did that before this change and 2 more (Syria, Equatorial Guinea) now do;
  the pre-existing defect is filed separately.

  Two further consequences worth naming: `polity_coverage_gaps()` now reports
  FAOSTAT areas 42, 88, 154, 180 and 187 as coverage gaps, because their
  upstream periods do not span the years FAOSTAT reports them -- the fold hid
  that behind a period running to 2025 -- and the energy CO2 extension's opt-in
  `unclassified = "polity_region"` treatment reaches 16 live areas instead of 2,
  resolving the second half of #415/#646. Its default (`"drop"`) is unchanged
  and moves no number.

* **A back-cast row no longer reports `mapping_status == "matched"` for a polity
  that was not alive in its year.** `add_polity_code()` floors the polity-lookup
  year at `backcast_anchor` (1961), because a pre-1961 WHEP value is a
  reconstruction on the anchor year's territory -- that convention is unchanged.
  What was wrong is that the row then claimed the polity had existed then, and
  for 12,208 of the 29,415 pre-1961 `(area, year)` cells it had not: FAOSTAT area
  238's 1850 row read `ETH-1952-1993`, `matched`, 102 years before that polity
  began. Those rows now report `mapping_status == "backcast_anchor"`, and
  `polity_coverage_gaps()` reports them as `gap_kind == "backcast_anchor"`
  alongside `polity_ended` / `polity_not_started`. The floor was applied before
  the span check, so the diagnostic could previously see only 2,664 of the
  12,208 cells; it now sees all of them, 9,544 of which are new.
  `polity_bucket_coverage()` surfaces the same resolver column, so its
  `bucket_mapping_status` would read `"backcast_anchor"` for a pre-1961
  `years =` argument; on the shipped crosswalk no bucket folds more than one
  polity before 1961, so it emits no such row today, and its `coverage`
  classification is unchanged either way. **No published value changes** -- a
  full `get_primary_production()` (6,310,390 rows) is `identical()` across the
  change, `mapping_status` is not on any published schema by default, and the
  `polity_validity` argument keeps its current scope, so `"drop"` still drops
  only nearest-period stand-ins (#763).
* **The polities snapshot is re-synced to `whep-polities` `2830fb7`, and no
  published value moves.** `polities` gains four rows (`ATF-1800-2025`,
  `SGS-1800-2025`, `WLF-1800-2025` and `FEZ-1943-1951`) and ten geometries,
  four wrong `cow_code` values are corrected (Albania 400 to 339, Comoros 403 to
  581, Sao Tome and Principe 411 to 403, Sardinia 338 to 325), and six
  predecessor/successor edges are filled in. `polity_label_aliases` gains the
  `Libya Fezzan` alias and three corrected `year_start` bounds.
  `gleam_geographic_hierarchy` resolves all 204 territories: `ATF`, `SGS` and
  `WLF` carried `NA` for want of any upstream polity and now carry one.
  `polity_area_crosswalk` keeps all 595 rows with **every routing column
  bit-identical** -- only three `cow_code` cells and one `polygon_status` cell
  change -- so a full `get_primary_production()` (6,310,390 rows, 1850-2023)
  comes back identical in all twelve columns, with no key added or removed, no
  `(area, year)` re-attributed and a zero delta in all eight units. Note for
  anyone reading #745: the 31 areas the upstream map names but the crosswalk
  resolves through `ROW-1850-2025` are **not** a stale-map artefact and this
  re-sync does not move them; they are the FABIO Rest-of-World fold, which
  outranks the map on purpose and is tracked separately (#717, #740) (#745).
* **The pre-1962 back-cast can now measure its hectares on each year's own
  borders.** `tonnes = ha * t_ha`: the yield half has always been historical
  (`.fill_yields()` back-casts `t_ha` against 1,058,295 pre-1962 observations),
  while the area half came from the `luh2-areas` pin, which is LUH2 land
  pre-aggregated to *present-day* ISO3. A row labelled with the 1961 entity was
  therefore measured on the borders that entity has today. The new
  `build_primary_production(land_method = "historical_polity")` measures it with
  `build_historical_land_areas()` instead: gridded LUH2 summed inside the
  polygon of the polity `area_code` resolves to in that year, resolved unfloored.
  How a change of territory reaches the back-cast is itself selectable, because
  `fill_proxy_growth()` reads only ratios: `boundary_step = "level_step"`
  (default) lets a change of territory through as a level step, because a
  different polity is a different thing being measured, and `"relink"`
  re-measures the previous year inside the *incoming* polygon so only
  within-territory growth is ever used. On Ethiopia in 1952, when Eritrea joins,
  the 1952 land ratio is +8.0% under the default and +1.9% under `"relink"`.
  `"relink"` suits a FIXED-territory series and is not the conservative choice
  here: suppressing that channel also suppresses the correction, and Ethiopia's
  1850 cropland comes back to 3.24 Mha against a present-day 3.22 -- the figure
  this method exists to replace. Under the default it is 1.52 Mha (whep#761).
  **No published values move by default**: `land_method = "present_day"` is
  unchanged and is what the pipeline still runs. Measured over 1850-1961 against
  the present-day series, the historical method moves 19.2% of back-cast crop
  tonnage at 1850 (net -17.2%), 6.5% at 1900 and 0.2% at 1961 under `"relink"`;
  31.3% / 22.9% / 0.2% under `"level_step"`. Under the new method pre-1962 rows
  are labelled `LUH2_polity_cropland` / `LUH2_polity_agriland` in `source`. It
  reaches all four dissolved federations without
  `federation_land = "successor_union"` -- Czechoslovakia, the USSR, Yugoslavia
  and Belgium-Luxembourg all have polygons of their own, and the USSR walks its
  own three-period chain back to 1850. It also declines to measure a bucket
  whose polity that year is a residual standing in for dozens of areas, or a
  resolver stand-in from outside its period: 5 buckets carrying 1961 crop
  tonnage lose their back-cast entirely, 0.1% of the 1961 total, the largest
  being Syria (#761).

* **The SOC climate driver read releases the LPJmL hydrology pin once it has
  been used.** The pin carries `swc_topsoil`, `prec_mm` and `irrig_mm` for every
  requested year -- ~12 GB at 1901-2022 -- and nothing reads it after the
  soil-water and monthly-climate series are derived from it, but it stayed
  referenced through the joins in `.assemble_soc_drivers()`, which is where the
  read peaks. Peak for a full-span `.cb_read_climate()` goes from 43.0 GB to
  36.0 GB at unchanged runtime (#624).

* **The SOC climate drivers assemble a year at a time, and stop decorating an
  86-million-row table.** Two changes to the same read. `.socd_monthly_climate()`
  joined four full-span monthly series on `(lon, lat, year, month)`; the joins
  and the water balance are all within-year, so they now run per year. And
  `build_carbon_balance()` no longer routes through the reporting-polity
  decoration: those four columns -- two of them character -- cost ~28 GB on the
  table a full span produces, and the carbon balance never reads them, keying its
  climate modifier on `(lon, lat, area_code, year, month)` and adding its own
  reporting columns to its own output. Polity validity still applies, since it
  can drop rows. Peak for an 80-year `.cb_read_climate()` goes from 56.5 GB to
  28.1 GB and it is a third faster (205 s vs 303 s), with all 17 shared columns
  `identical()`. The exported `get_soc_climate_drivers()` still returns the
  polity columns (#624).

* **`build_carbon_inputs()` collapses each year's gridded cropland inputs before
  gridding the next.** The gridded table is ~1.25e6 rows per simulated year and
  its only consumer, `.ci_cropland_class()`, keeps about one row in forty-two --
  so accumulating every year first built a 1.5e8-row table at 1901-2022 and then
  copied it again to bind. Reducing inside the per-year loop keeps only the
  collapsed years. Peak for a 40-year `build_carbon_inputs()` goes from 25.4 GB
  to 14.4 GB (37.8 GB before #738), with output `identical()` across all
  5,275,974 rows and 12 columns. The exported `build_soil_carbon_inputs()` still
  returns the full per-crop detail (#624).

* **The HWSD readers aggregate in latitude bands instead of one whole-grid
  pass.** Classifying the 30-arcsec HWSD grid in one go materialised ~11 GB of
  full-resolution intermediates to produce a few MB, and `terra::crop()` pulled
  the whole grid into memory before any aggregation began. Every aggregated cell
  draws only on the source pixels beneath it, so the work splits by latitude
  band with no cross-band dependency as long as each band is a whole number of
  target rows. Peak per call goes from ~16.8 GB to ~2.6 GB and each call is
  faster (clay 20 s to 23 s, hydraulic 60 s to 67 s, soil pH 23 s to 26 s on a
  loaded machine; ~2.5x faster when measured alone). Output is `identical()` at
  all three call sites -- `.cb_hwsd_clay()`, `read_soil_hydraulic()` and
  `read_soil_ph()`. This supersedes the per-call-site reclaim added in #735,
  which only covered one of the three (#624).

* **`polity_area_crosswalk` no longer gives an area a polity the upstream map
  awarded outside its fold (#741).** The prefix expansion removed a candidate
  only when it overlapped a map span of *its own* area, so nothing ever asked
  whether upstream had already named that polity elsewhere. FAOSTAT area 62
  Ethiopia PDR was therefore handed `ETH-1993-2025`, which area 238 owns, and
  it escaped the same-area test on a boundary year (1993 is not `<= 1992`).
  The exclusion now also fires when the map's owner sits outside the
  candidate's fold, and the crosswalk goes from 596 to 595 rows. The mirror-
  image row `(238, ETH-1952-1993)` is deliberately kept: area 62 folds into
  bucket 238, `reporting_polity_code` is resolved from the bucket code, and
  that row is the bucket's whole pre-1993 coverage. **One published identity
  moves, no quantity does.** `regions_full`'s row for area 62 "Ethiopia PDR"
  now carries `reporting_polity_code = "ETH-1952-1993"` / "Ethiopia
  (1952-1993)" instead of `"ETH-1993-2025"` / "Ethiopia" -- that area
  dissolved in 1993 and never was the modern republic, so this is a
  correction. It is the only row of the only dataset that moves. Resolving all
  266 area/bucket codes over 1850-2025 (46,816 pairs) moves 33, all of them
  area 62 in 1993-2025, years in which area 62 both published nothing and no
  longer existed. Confirmed on a real 6,310,390-row
  `get_primary_production()`: area 62 contributes 0 rows, and Ethiopia's
  bucket-238 rows still split 35,558 pre-1993 to `ETH-1952-1993` and 10,057
  from 1993 to `ETH-1993-2025`. One consequence is now visible rather than
  hidden: `.bucket_year_polity_conflicts()` reports bucket 238 alongside
  bucket 206 for 1993-2025, because the removed row was manufacturing
  agreement between a dead reporting area and a live one.
* **New diagnostic `polity_mapping_provenance()` says which authority a row's
  territorial identity rests on (#740).** `polity_area_crosswalk` is not the
  upstream FAOSTAT-to-polity map: it is that map (245 of 596 rows) plus rows
  WHEP manufactures by ISO3-prefix match (`prefix_outside_map`, 262;
  `prefix_fallback`, 27) and WHEP's own Rest-of-World bucket
  (`fabio_row_fold`, 62). Nothing said which of them a published number came
  through. The new function resolves `(area_code, year)` through the same
  lookup the builds use and reports the class of the crosswalk row that
  answered, plus an `authority` column collapsing it to `"upstream"`,
  `"whep_prefix"`, `"whep_bucket"` or `"unresolved"`. Measured on a real
  6,310,390-row `get_primary_production()`: 96.06% of rows and 99.76% of tonnes
  resolve through an upstream map row, 3.37% through the Rest-of-World bucket
  (the 24 reporting members #628 promoted), and 0.56% through a manufactured
  prefix row -- every one of them FAOSTAT area 238 Ethiopia before 1993, on
  `ETH-1952-1993`. Over the crosswalk's own 1850-2025 grid, 257 of the 262
  `prefix_outside_map` rows are the resolution of no `(area_code, year)` at
  all, because the back-cast anchor floors every lookup at 1961. **No published
  value changes**: the function is a read-only diagnostic and no build path
  calls it.

* **`build_carbon_inputs()` no longer attaches reporting polity columns to an
  intermediate that discards them.** `build_soil_carbon_inputs()` produced a
  5.0e7-row gridded table for a 40-year span, and adding the four reporting
  polity columns to it cost +20.4 GB and 20 s -- two of them are character
  columns. `.ci_cropland_class()` then collapsed that table 42-fold, to 1.2e6
  rows, discarding all four, and `build_carbon_inputs()` re-added them to its own
  output. The internal path now skips them and only the exported
  `build_soil_carbon_inputs()` pays. Peak for a 40-year `build_carbon_inputs()`
  goes from 37.8 GB to 25.4 GB, slightly faster (725 s vs 742 s), with output
  `identical()` across all 5,275,974 rows and 12 columns (#624).

* **BNF coefficients now ship with cell-level provenance (#497).** The long
  `bnf_provenance` sidecar is readable with
  `whep_coef_table("bnf_provenance")` and accounts for every one of the 60
  non-missing numeric cells in `bnf.csv` exactly once: 32 are asserted against
  a publication, 15 are explicit derivations, and 13 retain their existing
  values with genuinely unresolved authority and no guessed source
  attribution. It distinguishes nitrogen harvest index from Herridge's
  dry-matter harvest index and identifies Lassaletta et al.
  (2014) as the *Environmental Research Letters* 9:105011 Supplementary
  Methods authority. **No published value changes:** `bnf.csv` is byte-
  identical and BNF runtime outputs are unchanged. Two invariants that the
  provenance rewrite would otherwise have dropped are kept: a mixed stand's
  `leguminous_share` must stay strictly inside 0 and 1, and no Anglade-cited
  coefficient may coincide with a sample size reported on its own Table 1 row.

* **Breaking: `whep::biomass_coefs` no longer exposes five unused legacy
  below-ground fields (#524).** `BG_Biomass_kgDM_ha`, `Root_Shoot_ratio`,
  `Root_kgC_kgDM`, `Rhizodeposits_mass_kgC_kgDM`, and
  `Rhizodeposits_N_kgN_kgRootN` have been physically removed. Modern
  calculations use the item-keyed `bio_coefs` fields
  `bg_biomass_dm_kg_ha`, `root_shoot_ratio`, `root_c_kgdm`,
  `rhizodeposit_mass_c_kgdm`, and `rhizodeposit_n_kgn_krootn` under their
  existing contract, respectively. The first two are fallbacks when
  `ipcc_root_coefs$bg_ref_dm_t_ha` and `ipcc_root_coefs$rs_default`,
  respectively, are unavailable; `rhizodeposit_mass_c_kgdm` is an integrity
  and documentation component already included in `root_c_kgdm`, not a
  separate calculation input. **No published number changes:** the removed
  columns had no runtime consumer, both modern coefficient tables are
  byte-identical, and representative NPP, BNF, SOC, and nitrogen-input outputs
  are unchanged.
* **`read_soil_hydraulic()` no longer holds three full-resolution HWSD rasters
  at once.** It classifies the 30-arcsec HWSD grid once per hydraulic property
  (`t_field`, `t_wilt`, `porosity`), each costing ~11 GB of transient raster for
  a 3 MB result. That memory is reclaimable, but nothing triggered the collector
  between passes, so they accumulated (11.4 -> 22.1 -> 32.8 GB). Reclaiming
  between passes takes the reader's peak from 38.2 GB to 16.8 GB, with output
  `identical()` and no time cost (64 s vs 68 s). This is a fixed cost on every
  `build_carbon_balance()` and `get_soc_climate_drivers()` call, independent of
  how many years are requested (#624).

* **A year-scoped production build no longer depends on the window for which
  livestock stock combinations exist.** `.build_livestock_stocks()` read the
  stock series scoped to the caller's window, but `.combine_livestock()`
  completes the year axis against the (area, item) combinations that read
  produced — so a combination absent from the window was absent from the
  completion, and the completed rows are what give a livestock-product row its
  unit downstream. The series is now read over its full span and trimmed
  afterwards; full-range output is unchanged. This narrows the remaining
  year-scoping gap (`t_LU` at 2010: 2.18e-04 to 1.61e-04) but does not close
  #666 — a scoped build still derives `LU` as NA where a full build derives 0.
* **`build_carbon_balance()` no longer grows its memory with the length of the
  span.** The RothC/HSOC climate modifier is now reduced one year at a time.
  Attaching soil cover crosses the monthly climate table with every land-use
  class, which measures 0.452 GB per simulated year against 0.097 GB for the
  drivers themselves, and that intermediate used to be held for the whole span.
  Measured peaks: a 40-year build went from 61 GB (OOM-killed before finishing)
  to 49.9 GB (completed), while a 20-year build is unchanged at ~51 GB -- the
  fix removes the per-year slope, not the fixed plateau. Runtime is unchanged
  (362 s vs 365 s at five years). Output is identical: `identical()` holds
  across all 1,166,220 rows and 17 columns of a five-year build, row order
  included (#624).
* **A grid cell now spends its critical-nitrogen allowance once, instead of
  handing the whole allowance to every crop that shares the cell.**
  `build_n_boundary_exceedance()` compared each crop's per-hectare pressure
  against the cell's single critical value independently, so a cell with *n*
  crops was measured against *n* copies of one allowance and the crop
  exceedances did not add up to anything the source defines. The calculation is
  now cell-first: every crop and polity contribution in a source cell is
  aggregated, that one pressure is compared with that one allowance, and the
  resulting cell allowance, signed margin and positive overshoot are only then
  attributed back to crops — by input shares for `metric = "input"` and by
  signed surplus-contribution shares for `metric = "surplus"` (which may be
  negative or exceed one). **Published exceedance values move, and the two
  metrics move differently.** For `metric = "input"`, where crop pressure
  cannot be negative, the cell overshoot `max(sum(a) - c, 0)` is greater than
  or equal to the old per-crop sum `sum(max(a - c, 0))` for every input, so
  reported
  overshoot rises (or is unchanged when one crop holds the cell, or when
  neither form overshoots): on the package's two-crop fixture, two crops of
  4 t N against a 5 t N allowance went from 0 to 3 t N. For
  `metric = "surplus"` it can move either way, because a negative crop
  contribution now offsets a positive one inside the cell instead of being
  clipped to zero crop by crop. The global magnitude is not quantified here —
  that needs the restricted archive and a full gridded surplus build, not
  something CI can reach. The new `resolution = "cell"` grain returns the
  undivided source-cell result, and the crop grains reconcile back to it
  algebraically.
* **The critical surface is pinned to the deposited rasters by checksum.**
  `read_critical_n()` checks every raster a call actually reads — the selected
  critical surface plus the three shared input layers — against
  `inst/extdata/critical_n_source_manifest.csv` before parsing, on byte count,
  MD5 *and* SHA-256, and aborts naming the file and the Zenodo record (6395016)
  if any differs. The manifest pins all 27 files of the archive (3 input layers,
  12 critical-input and 12 critical-surplus surfaces, all 27 checksums
  distinct), so a partially substituted archive cannot go unnoticed on the path
  that consumes it. The layer also carries the
  deposited `source_area_ha` and `image_region` per cell, so IMAGE membership
  and source land area now arrive on the canonical integer cell key from the
  archive itself rather than through a year-free country-to-IMAGE join —
  which is why that join leaves the territorial-join baseline (whep#669).
* **Unsupported boundary modes hard-error rather than resolve to something
  else.** Only the source-exact `allocation_scenario = "yield_gap"` is
  implemented on the grid; `"no_increase"` and `"new_fixation"` abort. An
  annual actual pressure requires an explicit `actual_year`, and
  `critical_reference_year` must be `2010`, matching the fixed deposited
  reference surface — the year is a stated selector, not an inferred one. Where
  a cell's pressure denominator is exactly or near zero, the cell result is
  kept whole and an explicit `cell_residual` record carries the unallocated
  allowance, margin and overshoot; callers that require complete crop
  attribution raise a typed undefined-attribution error instead of discarding
  or inventing the residual. Country equal-per-capita allocation is unchanged
  and separate, and dynamic critical values remain out of scope (whep#702).
  Urban N stays provisionally inside WHEP actual pressure, and
  manure-management boundary comparability and intensive-grass scope remain
  recorded provenance rather than settled choices.

* **Six more gridded builds now say when a cell-year names a polity that did
  not exist.** `build_water_balance()` and `get_soc_climate_drivers()` gained
  `polity_validity = c("keep", "flag", "drop")` in whep#462; every other
  consumer of the same year-less `data$cell_polity` grid had the same defect
  silently. `build_n_deposition()`, `build_urban_n()`,
  `build_ag_land_support()`, `aggregate_grass_to_polity()`,
  `spatialize_country_n_to_crops()` and `build_carbon_balance()` now take the
  same argument, with the same three values, the same `"keep"` default and the
  same warning, routed through one shared helper so the eight entry points
  cannot drift apart. The HWSD clay/pH readers (`read_soil_ph()`,
  `read_soil_hydraulic()`) are documented as exempt: they use the crosswalk as
  a spatial extent and their output has neither `year` nor `area_code`.
  **No published value changes on the default path** — `"keep"` reproduces
  today's rows and numbers and only adds a warning, and `"flag"` adds one
  logical column. `"drop"` does move values and is opt-in: measured on the real
  58,795-cell country grid, it removes 3,181 of 30,438 `(area_code, year)` keys
  over 1850-2020 (22 of 178 area codes, 21.4% of cell-years), and 34 of 3,738
  keys over 2000-2020 alone.
* **An ISO3 code naming two FAOSTAT areas no longer resolves by row order, so
  Ethiopian ISO3-keyed input stops being stamped with a country that dissolved
  in 1993.** FAOSTAT keeps a pre-split entity beside its successor, so `ETH`
  names both 62 ("Ethiopia PDR") and 238 ("Ethiopia"), and `SDN` names both 206
  and 276. `.iso3_to_fao_area_code()` broke that tie with
  `unique(bridge, by = "iso3c")` — row order, which kept the lowest code, i.e.
  the dissolved 62 for `ETH`, in every year. The tie is now broken on the
  polities database instead: the area code that IS its polity's
  `polity_area_code` wins, which picks 238 for `ETH` and leaves `SDN` at 206;
  an ISO3 still ambiguous after that rule aborts rather than being guessed.
  Exactly one of the 263 ISO3 codes changes. **No published values move**: both
  live callers reduce to `polity_code`, which is the same either way, and the
  population totals the historical CBS proxy fill sees are byte-identical.

* **`gleam_geographic_hierarchy` now carries the polity of each country it
  lists.** The table is GLEAM's own registry of the countries that exist today
  — it has a row for South Sudan and none for any dissolved entity — but it
  carried no polity column at all, so every consumer resolved one ad hoc and
  joined on the bare `iso3`. That join has no year, and 38 of the 204 `iso3`
  values name a *different* polity at 1961 than at 2010, so which one an
  unyeared join picked was decided by nothing. The new
  `reporting_polity_code` / `reporting_polity_name` columns hold the polity the
  present day resolves each `iso3` to, and
  `polity_identity_conventions()` moves the table from `"recommended"` to
  `"carried"`. 201 of the 204 resolve, every one of them to a period that
  reaches the snapshot's open end with nothing succeeding it. Three keep `NA`
  and stay visible: `ATF`, `SGS` and `WLF` are territories whep-polities has no
  polity for at all (upstream whep-polities#187). **No published value
  changes** — the seven existing columns are byte-identical and no consumer
  reads the new ones yet; switching a consumer's join from `iso3` to the polity
  would move values and is deliberately not done here.
* **`regions_full` and `polities_cats` no longer carry a column named
  `polity_code` that is not a polity code.** Both shipped a legacy ISO3-like
  stem (`"AFG"`, `"ROW"`, `"RAFR"`) under that name, of which 0 of 271 non-`NA`
  values was a `polities$polity_code`, so a join from either table to `polities`
  or `polity_area_crosswalk` on the one column whose name promised identity came
  back completely empty and nothing warned. The column is now
  `legacy_polity_prefix`, which claims nothing; the real carrier remains
  `reporting_polity_code` (259/259 and 198/198 non-`NA`, all real). **This is a
  breaking schema change for any caller reading `regions_full$polity_code` or
  `polities_cats$polity_code`** — rename the read, and if the intent was a
  polity, switch to `reporting_polity_code`. **No published value changes**: the
  two rebuilt tables are `identical()` to their predecessors once the column is
  renamed back, and the one join in `R/` that used the old name
  (`.read_fodder_euadb()`'s EU AgriDB bridge, which was really an ISO3 join
  wearing a polity name) resolves the same 28 ADB regions to the same area
  codes.
* **The last site reading `polity_end_year` as inclusive has been removed.**
  `data-raw/balance_coefficients.R` stamped `urban_n_reference` with a polity
  code through its own copy of the year resolver, and that copy matched
  `polity_end_year >= year` while the column is exclusive at a succession
  everywhere else. Over the shipped crosswalk the two readings disagree on 313
  `(ISO3, year)` pairs; 299 of those abort with two candidates, and 14 resolved
  silently to the interval that had *ended* on that year, booking a coefficient
  to a polity that no longer existed. The resolver now lives in `R/polities.R`
  as `.iso3_year_to_polity_code()`, takes its upper bound from
  `.polity_join_end_year()` like every other call site (exclusive at a
  succession, inclusive at an open end), and aborts rather than answer with a
  dissolved polity. **No published value changes**: the one dataset the builder
  stamps is Spain over 1860–2022, covered by the single interval
  `ESP-1800-2025` on either reading, and all 23 tables the builder writes come
  back `identical()` to the committed ones.
* **Upstream's succession relation is now read in both directions, so a period
  whose successor is only recorded on the successor's side is no longer widened
  into that successor's first year.** `.polity_join_end_year()` extends an OPEN
  period by one year, and "open" was read from `polities$successor` alone.
  `AGO-1975-2025` names `ANG-1905-1975` as its predecessor while colonial
  Angola names no successor, so ANG was widened into 1975 and FAOSTAT area 7
  had two resolution candidates for that year, separated only by the
  `polity_start_year DESC` tie-break. A period another period both names as its
  predecessor and begins exactly at the end of now counts as succeeded; the
  begin-at-end test is what distinguishes a hand-over from a partial carve-out
  such as `TRS-1947-1954` out of `ITA-1919-2025`, whose predecessor goes on
  existing. **No published value changes**: measured over every
  `(area_code, year)` pair of the crosswalk for 1961–2025 and for 1850–2025,
  0 pairs change `polity_code` and 0 change `mapping_status`; the joined-span
  conflict count goes from 1 to 0.
* **A row with no mapped period now stands in on a polity that has not started
  yet rather than on one that had already ended.** When `add_polity_code()`
  finds no period covering a row's (anchored) year it falls back to another
  period of the same reporting area; that fallback ranked candidates purely by
  distance in years, which split a single reporting area's series between two
  entities at whatever year the arithmetic flipped. FAOSTAT area 178 Eritrea
  read `ERI-1889-1952`, the Italian colonial administration, for 1850-1972 and
  `ERI-1993-2025` from 1973; area 273 Montenegro split at 1961 between
  `MNE-1913-1918` and `MNE-2006-2025`, on a margin of one year (44 against
  45). A not-yet-started period is now preferred over an ended one, so each of
  those areas resolves to one entity across 1850-2023, which is also what the
  other 22 areas with no period at the back-cast anchor — the post-Soviet and
  post-Yugoslav ones — already did. **No published quantity changes**; 235 of
  the crosswalk's 46,640 `(area, year)` pairs over 1850-2025 change which
  polity they name, all of them areas 178 and 273. On a real full-range
  `get_primary_production()` (6,310,390
  rows) the out-of-span set is unchanged at 2,301 pairs / 7,247 rows, and 347
  of those rows move from `polity_coverage_gaps()`'s `"polity_ended"` class to
  `"polity_not_started"`, WHEP's documented back-cast convention, leaving
  `"polity_ended"` as FAOSTAT area 206 alone. `options(whep.polity_stand_in =
  "nearest")` restores ranking by distance alone.
* **The pre-1962 CBS proxy fill no longer reads a territory's identity out of
  its label.** `.fill_with_proxies()` recovered the frame's polity by matching
  its `(area_code, area)` pair — the bucket AND the LABEL — against the
  crosswalk's `(polity_area_code, polity_name)`, while the population and land
  proxies were resolved from the code, year-aware. The two sides disagreed.
  Measured on a real `build_commodity_balances(prim, 1955, 1965)` run (121,191
  frame rows, 1,267 `(area_code, area, year)` keys): 35 keys resolved to a
  *different* polity through the label than through the code, and 70 to no
  polity at all. Both proxies are now keyed on the reporting bucket the frame
  already carries, so no label is consulted and no resolution is needed on the
  frame side. **Published values move, in 1955-1960 only** (1961 onwards is
  byte-identical): total tonnage -0.064% to -0.068% a year, -0.0289% over the
  1955-1965 build; 9,623 of 528,769 cells change, 234 appear and 696 vanish.
  Burundi, Equatorial Guinea, French Guiana, Papua New Guinea, Singapore,
  Syria and Oman gain a population proxy they never had, and eleven areas gain
  an agricultural-land proxy. Eswatini stops growing on 5,409 thousand people
  and grows on its own 353 thousand: it, Bermuda and New Caledonia carry the
  shared `"Rest of World"` label, which used to join them onto one
  `ROW-1850-2025` proxy row holding the SUM of four promoted members'
  populations — the whep#589 shape. Bermuda and the Rest-of-World bucket have
  no proxy of their own and so lose their pre-1961 rows entirely (12
  `(area_code, year)` pairs, 92.99 Mt) rather than keep a series synthesised
  from other territories' populations; what an artificial aggregate's proxy
  should be stays open (whep#493). Sudan (former, area 206) is the one bucket
  whose agricultural land was split across two polities: it now sums Sudan and
  South Sudan, which is what its CBS numerator already did, and its `agriland`
  proxy rises 1.47x. No `(area_code, year)` changes its reporting polity and
  no `area_code` gains a second label.

* **The pre-1962 CBS extension is keyed on `area_code`, not on the `area`
  label.** `area` is the *periodized* polity name ("Algeria (1919-1962)") and
  it was a key in five places in the historical extension, including the year
  skeleton, which is crossed with the year axis. Two labels for one code
  therefore gave that code two full year skeletons rather than only a wrong
  name. `build_commodity_balances(historical_data = )` reaches exactly that:
  `.prepare_historical_cbs()` names its rows from the crosswalk's static
  `area_name` while the FAOSTAT rows carry the periodized polity name, and for
  97 of the 262 codes in that lookup the static name is not any of the code's
  polity names, so the two can never agree. Measured on a fixture, one such
  overlap turned 77 keys into 154 rows and had the cell's two candidate values
  summed downstream instead of reconciled — 240 t where the answer is 140 t.
  The extension now reconciles on the code, takes the best source as it always
  intended to, and re-attaches the code's one display label afterwards. **No
  published value changes** without `historical_data`: `1850–2023` is identical
  before and after, key for key.
* **The polity a row belongs to is now carried from where it is resolved
  instead of re-derived at the end of every output.**
  `.aggregate_to_polities()` has always resolved the bucket's polity in order
  to label the fold and then discarded the code, leaving the ~70 call sites of
  `.add_reporting_polity_columns()` to resolve it a second time from the same
  crosswalk. The fold now emits `polity_area_code`, `reporting_polity_code`,
  `reporting_polity_name` and `reporting_polity_has_geometry` — the published
  names, so no new vocabulary and no schema change — and the tail helper keeps
  a carried identity rather than resolving it again. It keeps it only after
  checking it: the identity must still match the key it sits next to (a bucket
  code resolves to itself, so a re-keyed frame fails that test), and the
  distinct `(area_code, year)` pairs are re-resolved and compared, which costs
  a fraction of the full resolution it replaces. Two non-`NA` answers for one
  key now warn instead of one of them being published silently. **No published
  value changes**: `get_primary_production()` (6,310,390 rows) and
  `get_wide_cbs()` (2,098,818 rows) are identical before and after, column for
  column.
* **`polity_coverage_gaps()` now says which direction a stand-in fell in, and
  the two directions are not the same defect.** The new `gap_kind` column takes
  `"polity_ended"` (the polity had ended by the row's year, so the value covers
  a territory that entity no longer describes — whep#414's case) or
  `"polity_not_started"` (the polity begins later, which is mostly WHEP's
  documented pre-1961 back-cast onto the anchor-year territory). No published
  value changes; this is a diagnostic gaining a column.
  Measured on a real full-range `get_primary_production()` (6.3M rows), 7,247
  rows — 0.115% — are attributed to a polity that was not live in the row's
  year, and they split **3,285 rows across 3 areas** `"polity_ended"` against
  **3,962 rows across 16 areas** `"polity_not_started"`. Bucket 206 is 2,938 of
  the ended ones, 89%; the other two, FAOSTAT area 178 Eritrea (123 rows,
  `ERI-1889-1952`) and area 273 Montenegro (224 rows, `MNE-1913-1918`), were
  not previously named anywhere.
  The classification is read at the year the resolver actually matched on, not
  the row's year, because `backcast_anchor` floors the lookup year: a
  pre-anchor row is matched as 1961 and can land on a polity that had already
  ended by then. That is exactly 165 rows of areas 178 and 273, which the
  raw-year comparison a caller could write for itself would label
  `"polity_not_started"` instead.
* **The `area` label a country carries through the commodity-balance build no
  longer depends on row order.** `.select_best_source()` reduced the long CBS
  input to one human-readable `area` per numeric code by keeping whichever row
  came first. `area` is the *periodized* polity name and a code legitimately
  changes it at a period boundary, so one code offers several labels over a
  multi-year build: on a real 1850-2023 run, 75 of the 216 codes carry more than
  one (up to four), and shuffling the input rows flipped the label for 13 of
  them. The label is also a join key — a second `area` vocabulary for one bucket
  once dropped 702,166 rows (#382) — so nothing pinned a key the build depends
  on. The pick is now a stated total order: the source that reports the code
  earliest in the order `.assemble_cbs_sources()` binds them in, that source's
  earliest year, then the label alphabetically. **No published value changes**:
  the rule reproduces all 216 of today's labels exactly, which is deliberate,
  because that same label is what the pre-1962 proxy fill reads a polity out of
  (#698) and changing it would silently redistribute which countries find a
  population and land proxy. Fixes #580.

* **Every join that keys on a territory but not on a year is now classified,
  and the list can only shrink.** A key of `area_code` with no `year` spans
  every period of a territory's history, so it asserts that the area means one
  thing for all time. Usually that is right -- 57 of the package's 163
  territorial joins carry no year, and nearly all are a single-year scope, a
  table with no time dimension (a coefficient, a single-vintage map, a grid
  mask), an identity lookup or a diagnostic -- but nothing said which, so a
  decision and an oversight looked alike. `.territorial_join_baseline()` now
  records the verdict and the reason for each, and `test_join_audit.R` fails
  when a new year-free territorial join appears unclassified, when a classified
  one disappears without its entry, or when a further join starts keying on the
  `area` label. Classifying them turned up one real defect, filed as #698 with
  its measurement rather than fixed here, because removing it needs #493's
  decision first. No published value changes (#669).
* **Four documented examples could not say which territory their rows belong
  to.** `build_supply_use(example = TRUE)` shipped a row
  with no `area_code` at all (an epsilon `3.33e-14` husbandry use) and
  `get_feed_intake(example = TRUE)` two more, so their polity columns came out
  `NA`; `build_feed_intake_local(example = TRUE)` and
  `build_grass_natural_carbon_inputs(example = TRUE)` keyed cells by an
  ISO-3166 numeric code (724 Spain, 300 Greece) where the FAOSTAT area code
  belongs, which resolves to nothing -- and one sibling row's ISO code for
  Argentina, 32, is FAOSTAT's Cameroon, so a cell in the pampas was labelled
  Cameroon. The two feed fixtures also predated the redistribute-feed
  migration and showed a 10% feed loss the current allocator cannot produce.
  All four fixtures are now sampled from real builds (the gridded ones keyed
  by the code the cell grid actually assigns: 203, 84, 9), and
  `build_supply_use(example = TRUE)` now covers all five documented process
  groups instead of three. **No published value changes** -- these are
  documentation fixtures, not pipeline outputs (#417).

* **`split_manure_management()` can now use the region-specific MMS shares, via
  the new `mms_source = "region_specific"`. The default is unchanged, so no
  published value moves.** `regional_mms_distribution` ships 33 rows: 18 for
  `region == "Global"` and 15 for North America (cattle, swine), Western Europe
  (cattle) and Latin America (cattle). The function filtered the table to
  `"Global"` unconditionally and never read the excretion's `territory`, so
  those 15 rows were unreachable and every territory got the global split —
  a global default, not a drop and not a silent zero. The territory is now
  resolved to its IPCC region through the same GLEAM lookup the emission-factor
  tables use (`.gleam_region_of()`, whep#465), with the Global rows as the
  fallback for every region and species the table does not cover.

  **What flipping the default would move**, measured on the real 2020 national
  chain (90.06 Mt excreted N, 195 territories): 66 territories and 5.40 Mt N
  (6.0% of the excreted nitrogen) change management system. The in-situ grazing
  stream falls from 41.36 to 40.71 Mt N (−1.6%) and the collected stream rises
  correspondingly; applied N moves −0.24%, volatilized N +0.94%, leached N
  −4.67%, direct N2O-N +0.30% and indirect N2O-N +0.84%. Rows, keys and
  territories are identical between the two sources, and mass is conserved per
  input row under both. The default stays `"regional_default"` because the
  region-specific rows are a coarse four-pair table (`data-raw` documents them
  as "GLEAM 3.0 / FAO statistics (simplified)") whose provenance has not been
  verified against a published GLEAM table; whether they are better than the
  global average is the maintainer's call (#466).
* **`build_gridded_landuse()` and `build_gridded_livestock()` now name the
  reporting areas their `country_grid` cannot represent at all**, once per
  call, with the national total at stake. The existing diagnostics fire per
  (country, crop) per year and per (species, year), so a country the grid has
  no cell for anywhere was reported as one more line in a list that already
  names 178 codes. On today's pinned centroid grid at 2015 the new warning
  reads 18 reporting areas carrying 0.109 Mha — all island or city states.
  Substitute the fractional crosswalk and it reads 20 areas carrying 28.90
  Mha, because that parquet still keys Ethiopia `62` and Sudan `206` where the
  centroid grid and today's `regions.csv` use `238` and `276`. No published
  value changes: this is a diagnostic only (#461).
* **`run_spatialize()` gains the `country_grid` override**, `"centroid"`
  (default, today's `spatialize-country-grid` pin) or `"fraction"`
  (`cell_polity_fraction.parquet`, which splits each border cell by fractional
  coverage instead of giving it whole to one polity). The engines already read
  `polity_frac` as `cell_area_frac`, so this is data wiring, not an engine
  change, and the resolved choice is recorded in `run_metadata.yaml`. The
  default is unchanged, so no published value moves. Measured at 2015, the
  alternative moves 6,828 of 7,557 (country, crop) cell-share vectors, by a
  median L1 of 0.060 and a harvested-area-weighted mean of 0.040, and raises
  the compartments receiving an allocation from 33,614 to 36,226 (#461).
* **`polity_bucket_coverage()` reported bucket 206 as a three-way fold in all
  65 years and called its label an extent mismatch; both were wrong (#414).**
  No published value changes — this is a diagnostic and the warning it drives.

  The fold runs **2012-2025, not 1961-2025**. Measured on the FAOSTAT
  production pin, area 206 "Sudan (former)" carries 13,759 rows over 1961-2011
  and areas 276 Sudan / 277 South Sudan carry 3,467 and 2,170 rows over
  2012-2024: the three never report in the same year. The year-aware resolver
  answers for every `(area_code, year)` pair regardless, standing in with the
  nearest period, and counting those stand-ins invented two members in every
  pre-secession year. A member now counts only when its polity is in span
  **and** the upstream map reports the area that year, which takes the report
  from 65 rows to 14.

  The label is **not** an extent mismatch. Bucket 206 resolves to
  `SUD-1956-2011`, whose published `successor` set is exactly
  `SDN-2011-2025; SSD-2011-2025` — the two polities the bucket folds — so that
  polity's territory *is* the sum. What is wrong is the period: it had ended.
  That is now its own class, `"predecessor"`, and `"partial"` is reserved for a
  label covering less than the value does. No bucket is `"partial"` today. The
  build-time warning says which of the two a bucket has instead of asserting
  the wrong one.

  The open decision in #414 is unchanged and unmade: no **live** polity means
  "Sudan and South Sudan". Minting one upstream is proposed in
  lbm364dl/whep-polities#139; un-folding the two areas instead is costed in
  #680.
* **`build_water_balance()` can now charge a single crop's water, and the
  per-CFT consumptive-water cubes are readable at all.** `read_lpjml_hydrology()`
  gains `"cft_consump_water_b"` / `"cft_consump_water_g"`, and
  `build_water_balance(bands = )` restricts the consumptive-water and
  `cft_nir` terms to named crop-functional-type bands, e.g.
  `bands = "rainfed grassland"` to charge a grazing footprint the grassland
  water alone rather than every crop in the cell. Bands are selected by the
  `band_name` the file itself carries, never by index, so a run configured with
  a different band set aborts instead of silently charging the wrong crop.
  `bands = NULL` (the default) totals every band, so existing callers are
  unaffected. Three fixes were needed to get there, each of which would have
  produced wrong numbers rather than an error:
  * The `cft_nir` map entry named `mcft_nir.nc` holding a monthly `cft_nir`
    variable. **No WHEP run has ever written that file**: all nine runs, 5.9.7
    and 6.1.1 alike, write `cft_nir.nc` holding annual `nir`. Reading it would
    simply have failed; nothing called it yet.
  * The reader assumed twelve time steps per year for every variable. The
    per-CFT consumptive-water cubes are annual (`nstep` 1, mm/yr), so their
    time axis was decoded as months, mapping year *y* to year 1901 + (y-1901)/12
    and slicing the wrong years out of the file entirely.
  * `ncvar_get()` drops length-1 dimensions, so slicing one year out of an
    annual per-CFT cube returned a 3-D slab whose *band* axis was then decoded
    as *time* — scrambling crops into years. Now read with
    `collapse_degen = FALSE`. Monthly cubes never hit this, because a one-year
    slice is still twelve steps.

* **New `polity_identity_conventions()` states, per object, what territorial
  identity a WHEP table with no year dimension carries (#671).** A polity code
  is year-scoped, so for a year-less object "attach the polity code" has no
  single answer: measured on the deployed `spatialize-country-grid` pin, 52,420
  of its 58,795 cells (89.2%) sit under an `area_code` that
  `polity_area_crosswalk` maps to more than one polity over time, and 33 of
  `mueller_synthetic_n`'s 156 `iso3c` labels, 37 of `crops_manure_n`'s 184
  `ISO` labels and 38 of `gleam_geographic_hierarchy`'s 204 `iso3` labels name
  a *different* polity at 1961 than at 2020. The register records which of
  #458's three answers each object takes — present-day polity, polity-period
  rows, or deliberately identity-free — and the new
  `tests/testthat/test_territorial_identity.R` checks each claim against the
  object it is made about, so a year-less territory-keyed dataset can no longer
  arrive without one. No published value moves; nothing but the register and
  its guards is added.

  Two things it makes visible. `regions_full` and `polities_cats` really do
  carry the present-day polity, in `reporting_polity_code`, and it is exactly
  what `add_polity_code(year_column = NULL)` resolves for all 272 and 198 rows
  respectively — now asserted rather than assumed. And their column literally
  named `polity_code` is **not** a polity code: none of its 271 values appears
  in `polities`, because it is a legacy ISO3-like prefix, which is pinned so
  the two vocabularies cannot be quietly conflated.
* **`build_water_balance()` and `get_soc_climate_drivers()` now say when a
  cell-year is attributed to a polity that did not yet exist, and can refuse to
  do it (#462).** The cell-polity crosswalk is a present-day rasterization with
  no year dimension, while polity validity is year-scoped, so a cell labelled
  `area_code` 52 carried that label in 1901 as readily as in 2009 and the
  polity resolution silently substituted the nearest period, `AZE-1991-2025`.
  Measured on the deployed `cell_polity_fraction.parquet` over the 1901-2009
  LPJmL run: **1,948 of 19,838 `(area_code, year)` keys, 21 of 182 area codes,
  14,761 of 58,791 cells** — the post-Soviet and post-Yugoslav successors plus
  South Sudan. Both functions gain `polity_validity`: `"keep"` (default) is the
  previous behaviour plus a warning naming the rows, years and area codes;
  `"flag"` adds the per-row logical `reporting_polity_out_of_span`; `"drop"`
  removes those rows. **No published value changes on the default**, and
  `"flag"` is numerically identical to it. `"drop"` removes 20.4% of the run's
  cell-years and makes South Sudan disappear from it entirely, which is why it
  is opt-in.

* **A year-scoped production build now agrees far more closely with the
  full-range build.** `.fill_yields()` interpolates `yield_c` along the year
  axis, so a window with no neighbouring years cannot reconstruct values the
  full series reconstructs, and `.finalise_primary()` drops those rows when it
  melts. Requested windows are now widened by 3 years either side for the read
  and trimmed back afterwards. Measured against the full-range build, the
  largest relative difference across all units falls from **1.67e-02 to
  7.21e-04 at 2015** and from 2.93e-04 to 2.84e-04 at 2010, for roughly +13 s
  on a scoped build. A full-range request is unaffected (#667).

* **Year-scoped builds no longer drop split-species slaughter counts.**
  `.compute_stock_shares()` read the livestock stock series scoped to the
  caller's window, but those shares are carried along the year axis precisely
  because the `faostat-emissions-livestock` pin lags QCL slaughter by 1-2 years.
  A narrow window left the carry-forward nothing to fill from, and the join in
  `.split_slaughter_by_shares()` then dropped the slaughter row entirely. At
  2010 that was 2 Singapore rows (Pigs, Hogs); `slaughtered_heads` now agrees
  exactly with the full-range build instead of by 4.7e-06. The stock series is
  read over its full span; full-range output is unchanged (#665).
* **`build_carbon_balance()` is about a quarter faster, with output unchanged
  to the last bit.** The RothC/HSOC climate modifier is now computed for every
  cell-year at once instead of once per (cell, year, land use) -- roughly 1.2e6
  separate calls over five years, each of which allocated a list and accumulated
  over twelve months. The deficit recurrence is sequential over months but
  independent across cells, so the loop inverts. Measured on
  `years = 1901:1905`: 820.4 s to 612.1 s. Peak memory is unaffected.

  The per-group path stays in place as the reference and still runs for models
  that do not use this modifier. The two agree exactly, not approximately:
  `identical()` holds across all 1,166,220 rows and 17 columns of the five-year
  build, so no result changes (#630).
* **`build_energy_co2_extension(unclassified = "historical_region")` prices the
  dissolved federations instead of losing them (#553).** Measured on the real
  `get_primary_production()` output (6,305,656 rows, 1850-2023), 569.4 Mt of
  meat carcass production — 3.33% of all of it, and 15.2% of the world's 1961
  tonnage — gets no energy intensity and leaves the extension, because
  `gleam_geographic_hierarchy` is a present-day country table with no row for
  the USSR, Belgium-Luxembourg, Czechoslovakia, the Yugoslav SFR or Serbia and
  Montenegro. Those five are now 99.998% of the loss: since the Rest-of-World
  fold was lifted (#628) bucket 999 no longer contributes to it at all. The new
  treatment groups them by running GLEAM's own scheme rules on the OECD and EU
  membership they themselves held while they existed — Belgium and Luxembourg
  were OECD founding members and EEC founders, so Belgium-Luxembourg is OECD/EU
  27; no successor of the other four was in either body before the entity
  dissolved, so they are non-OECD, non-EU. Rows carry
  `method_energy = "GLEAM_3.0_energy_meat_historical_region"`, and the option
  is a superset of `"polity_region"`.

  **No published value changes**: the default `unclassified = "drop"` is
  bit-identical on the full real input (181,831 rows, `sum(impact_u) =
  6.530863856531e12` before and after, `identical()` TRUE). Opting in adds
  1,190 rows and 7 areas, moves no shared row by any amount, and raises total
  energy CO2e by **+2.40%** over 1850-2023 — **+12.0% in 1961**, +11.3% in
  1990, +0.26% in 2000 and 0% from 2010 on.
* **`polity_area_crosswalk$mapping_status` now uses the value it documented but
  never shipped, and the confidence of a mapping is documented as the pair
  `mapping_status` x `mapping_source`.** `not_a_reporting_area` sat below
  `matched` in the build's `case_when`, so it could only fire for a row with
  neither an `area_code` nor a `polity_code` — no such row exists, and it
  shipped on 0 of 596 rows. The 20 rows it was written for (Aland, Saint
  Barthelemy, Guernsey, Jersey, the Isle of Man and Sint Maarten, which
  `regions_full` carries without a FAOSTAT code, plus the six regional
  aggregate polities) match a polity and so read `matched`, indistinguishable
  from a real area mapping even though they carry `NA` in both `area_code` and
  `polity_area_code` and no consumer can join to them. Status counts move from
  manual 27 / matched 568 / unmapped 1 to manual 27 / matched 548 /
  not_a_reporting_area 20 / unmapped 1. No `polity_code`, `polity_area_code` or
  any other column moves, and no code in the package filters the crosswalk on
  `mapping_status == "matched"`, so no published number changes. A consumer that
  does filter that way loses 20 unjoinable rows.

  `mapping_status` says whether a polity was found, not how far to trust it:
  `matched` covers a curated hit in upstream's FAOSTAT map (233 rows), a
  prefix-inferred historical period (247), a prefix guess for an area the map
  never mentions (6) and the FABIO Rest-of-World fold (62). `mapping_source`
  already separates those and is non-`NA` on every row, so the fix for #544 is
  to document the pair rather than add a third vocabulary that would duplicate
  it (#544).
* **`get_polity_geometries(polity_codes = )` now returns a usable `sf` object
  in a session that has not loaded `sf`.** The row subset ran through
  `[.data.frame` whenever the suggested `sf` namespace was not loaded, which
  keeps class `sf` and `attr(, "sf_column")` but strips `sfc` off the column
  they point at; the result passed every cheap structural check and then
  aborted inside the first `sf` call, complaining about a column nobody had
  renamed. The function now loads `sf` before subsetting, and aborts with class
  `whep_sf_required` if `sf` is not installed instead of returning the broken
  object. No published values change: the argument-less call is untouched, and
  both in-package callers use it.
* **`build_gridded_landuse()` and `build_gridded_livestock()` take an
  `area_key`, and say when their output cannot join a national table.** The
  spatialize chain allocates on the raw reporting codes its `country_areas`
  and `country_grid` are keyed on, while whep's polity-keyed national tables
  are aggregated on `polity_area_code`. A reporting code that is not itself a
  bucket therefore left every output row carrying two territorial keys that
  disagree — `area_code = 276` beside `polity_area_code = 206` — so whether a
  consumer joined on one or the other decided whether Sudan existed in its
  result (#582). Measured against the deployed pins, `country_grid` holds 831
  such cells under 2 codes (276 Sudan, 277 South Sudan) and `country_areas`
  0.64% of its harvested area; the other six codes the issue listed are no
  longer off-bucket, because #628 gave Syria, North Macedonia, Eswatini,
  Equatorial Guinea, New Caledonia and Palestine their own published codes.
  The default `area_key = "grid"` is unchanged bit-for-bit and now warns
  naming the codes that cannot join; `"polity_area"` re-keys the output on the
  bucket before the polity columns are attached, so the two keys agree in
  every row. **No published value changes** unless `"polity_area"` is asked
  for: on a 2020 Sudan/South Sudan run it conserved 21,894,526 ha and 230.7 M
  head exactly, kept the row count, and moved 13,447 crop rows and 3,671
  livestock rows from a key no national table carries onto `206`. Under
  `"polity_area"` the raw code is carried, not replaced, as `grid_area_code`,
  the shape `build_cell_polity()` adopted in #579. `run_spatialize()` accepts
  `area_key` in `overrides`.

* **`estimate_energy_demand()` now warns when `work_hours_day` is supplied
  without a work coefficient.** `whep` ships `cw = 0` for every species, so
  draft work is opt-in per call via `work_coef` — passing only the hours
  produced `ne_work = 0` with no indication that the input had been ignored
  (#210). The numbers are unchanged; only the silence is. Hours filled in from
  `livestock_production_defaults` never warn, since several species carry a
  non-zero default and warning about those would fire on ordinary runs.
* **The FABIO comparison's EU aggregate is derived, and now covers the
  dissolved predecessors.** `inst/scripts/compare_fabio_footprints.R` carried a
  28-element ISO3 literal for EU28. It is now built by `.eu_aggregate_iso3()`
  from the published `regions_full$EU27` flag plus `GBR`, the one membership
  fact no table in the package states, selected through the new
  `WHEP_EU_AGGREGATE` environment variable
  (`"eu28_territory"`, the default, `"eu27_territory"`, `"eu28_states"`,
  `"eu27_states"`). The literal omitted `BLX` (Belgium-Luxembourg) and `CSK`
  (Czechoslovakia), under which FABIO *and* WHEP's own CBS both book Belgium,
  Luxembourg, Czechia and Slovakia before those successions, so all four read
  as exactly zero in the 1986 benchmark year on both sides of the comparison
  and normally in 2000 and 2013. **This moves a published number:** the FABIO
  EU land footprint for 1986 goes from 210.4 Mha to 222.7 Mha (+12.3 Mha,
  +5.9%); 2000 and 2013 are bit-identical, because the predecessors carry no
  demand there. `WHEP_EU_AGGREGATE=eu28_states`
  reproduces the old list, and the old numbers, exactly. Whether the comparison
  should report EU28 or EU27 at all is left open (#421).
* **`build_food_supply()` and `build_n_percapita()` now name the areas they
  drop for having no population denominator.** Both inner-join the
  `read_population()` table, so an area the `gdp-population` pin does not cover
  was absent from their per-capita output rather than wrong in it, and nothing
  said so. Measured on a real `get_wide_cbs(years = c(2010, 2015, 2021))` plus
  the real pin, `build_food_supply()` silently lost **16 areas over 43
  area-years** — Bhutan, Comoros, New Caledonia, Tonga, Micronesia, Seychelles,
  the Faroe Islands, bucket 999 and others — carrying 0.0304% of the food
  protein in range. They are still dropped (no denominator is invented) but
  each is now named in a warning, with the share of the quantity that leaves
  with it. `options(whep.warn_missing_population = FALSE)` silences it.
  **No published value changes**: with the warning suppressed a real
  `build_food_supply()` run is `identical()` to the one before this change.

  This closes #543, whose measurement it also corrects. That issue reported the
  area-999 denominator as covering 6 of its 62 territories and so overstating
  every per-capita quantity keyed on 999 by 15-43%. Since the Rest-of-World
  un-fold (#628) that is no longer the shape of the defect: the 6 covered
  members have their own area codes, `read_population()` emits **no 999 row at
  all**, and CBS 999 carries **zero food** in 2010, 2015 and 2021 — so nothing
  per-capita is keyed on 999 and nothing is overstated. Routing the pin's five
  continental "Other" residuals into 999, the fix the issue proposed first,
  would now be wrong: it would give a bucket with no food a denominator of
  5.9 M people (2010) and attribute Reunion's, Greenland's and New Caledonia's
  population to a code that no longer carries their food.
* **LUH2 land is no longer discarded when one aggregation bucket holds two
  territories, and the pre-1962 yield back-cast no longer mixes countries that
  share a polity label.** Two sites keyed on the `area` *label* where an
  `area_code` was available, which is whep#632's defect at two further sites.
  (1) The LUH2 area bridge paired each bucket's code with its *member's* name,
  so bucket 206 reached grassland construction as two rows — "Sudan (former)"
  and "South Sudan" — under one `area_code`; `.dedup_production()` reads that
  as competing sources and kept one, dropping the other's pasture. The bucket
  now carries one label derived from its own code, so the two are summed.
  (2) The `t_ha` proxy-growth fill grouped its series on the label, which is
  wrong both ways: "Rest of World" covers 62 reporting `area_code`s, so growth
  rates were taken between different countries, and the label is year-aware, so
  one country's own series was cut in two at every periodization boundary
  (`area_code` 79 is "Germany (divided, 1949-1990)" through 1989 and "Germany"
  from 1990). **Published values move**: `ha`
  +1.04% (+5.56e9 ha-years, all of it bucket 206's recovered pasture over
  1850-2022) and `t_ha` -0.036% (464 cross-country fills removed, 5 real ones
  gained); `tonnes`, `heads`, `LU`, `t_head`, `t_LU` and `slaughtered_heads`
  are bit-identical. Comparing the Rest-of-World fold against the default
  un-folded build over its 62 areas, `ha` goes from 2.38x to 0.99x and
  `tonnes` from 0.59x to 0.82x, with the whole remainder in the pre-1962
  back-cast and the observed 1962-2023 era conserved to 0.04% (#633).
* **EU AgriDB fodder now reaches Austria and the United Kingdom.**
  `.read_fodder_euadb()` resolves the source's `Region` through
  `regions_full$ADB_Region`, and that column had a key for 26 of the pin's 28
  regions: `AT` and `GB` were missing, so 2030 rows (8.8% of the input,
  1961-2019) resolved to no area and were discarded without a message. Those
  two countries had their fodder estimated from dry-matter yields
  (`source = "DM_yield_estimate"`) while their 26 EU peers used the source.
  Adding the two keys moves published values for area codes 11 and 229 only,
  and for no other area: harvested-area totals over 1850-2023 rise 6.4%
  (Austria) and 14.0% (United Kingdom), fodder tonnage 33.4% and 65.7%; the
  global harvested-area total moves +0.07% and global tonnage +1.2%. Fodder
  production is copied one-for-one into `feed` by `.primary_to_cbs()`, so
  those areas' `feed` moves by the same tonnage. A region the source adds in
  future that `regions_full` does not key now raises a warning naming it,
  instead of vanishing (#585).
* **The last two ad-hoc country-label joins in the spatialization script are
  gone (#576).** `inst/scripts/prepare_spatialize_all.R` matched
  `whep::crops_manure_n` on a raw `iso3c` join and
  `whep::lassaletta_grassland_share` on a country *name*. The manure reader now
  goes through `whep::polity_label_aliases` like the Mueller reader does, read
  at the vintage of its own labels rather than at Mueller's circa-2000 base
  year: `crops_manure_n` names Serbia, Montenegro and South Sudan separately
  and names no Serbia and Montenegro, Sudan (former), Czechoslovakia or Zaire,
  so its vocabulary is post-2011, and every year from 2011 on maps all 183 of
  its country labels exactly as the retired join did. **No published value
  changes**: same 31,476 rows, same 183 area codes, maximum difference 0 Mg.
  West et al.'s `RoW` aggregate is still dropped rather than equated with
  WHEP's residual bucket 999, which since #628 means something else.

  The grassland-share reader gains a `grass_share_route` argument on
  `prepare_nitrogen_inputs()` and `prepare_spatialize_all()`, recorded in
  `nitrogen_inputs.parquet` as `method_grass_share`. The default,
  `"area_name"`, is the existing name join and is byte-identical to it.
  `"alias_map"` resolves each label at its own row's year instead: 6,633 rows
  against 6,370 and 137 area codes against 130, gaining China, Cote d'Ivoire,
  DPRepublic of Korea, Cape Verde, Swaziland, Ethiopia PDR, Belgium-Luxemburg
  and Occupied Palestinian Territory, and losing South Sudan and the years in
  which Yugoslav SFR, Czechoslovakia, Viet Nam and Botswana had no polity.
  Which route is right is an open question (#576); nothing switches by itself.

* **`get_primary_production()`, `get_wide_cbs()` and `get_processing_coefs()`
  take a `years` argument.** A scoped request now builds only that window
  instead of building 1850-2023 and discarding the rest. Measured for 2010:
  wide CBS 256 s / 23.3 GB peak to 12.6 s / 6.8 GB, primary production 168 s /
  14.7 GB to 29.5 s / 2.9 GB. The full wide-CBS build peaking above 16 GB is
  what had been failing the r-universe check on macOS and Linux. `years = NULL`
  is unchanged in every respect, including its cache slot, so existing callers
  keep today's behaviour and today's numbers. The primary-production to CBS to
  processing-coefficient chain and its cache keys are now shared with
  `build_io_model()`, which previously carried a private copy (#367).

  A scoped window is close to, but not identical with, building the full range
  and filtering. Against the full range at 2010, wide-CBS quantity totals agree
  to 3.8e-04 and primary-production totals to 3.0e-04, with `ha`, `t_ha`, `LU`
  and `heads` exact. The residual sits in `import` and in the livestock ratios
  (#625). Use `years = NULL` when exact agreement with the published series
  matters.

* **Year-scoped production builds no longer drop every forage crop.** Fodder
  rows are synthesised across the whole year axis — `.fill_fodder_gaps()` takes
  the union of (area, item) groups over all years and interpolates between them
  — so a narrow window silently lost all six forage items (`Forage and silage,
  *`, `Cabbage for fodder`, `Forage products`). At 2010 that was 137 rows,
  **1.16% of production tonnes, 1.85% of `t_ha` and 1.36% of wide-CBS `feed`**,
  and it affected `build_io_model(years = )` on every release that had it. The
  fodder chain now runs over the full span and trims afterwards. Full-range
  output is unchanged (#623).
* **Livestock stocks are split on the area CODE, not the area label (#589).**
  `.split_stock_share()` divides a parent item's production across its sub-items
  in proportion to their stocks, grouped by `(year, area, item_prod_code)`. When
  several reporting areas share one label the group spans all of them, the share
  denominator sums across areas, and each area keeps only its own fraction.
  That became live when the Rest-of-World fold was lifted: `.unfold_rest_of_world()`
  promotes `polity_area_code` but leaves `polity_code`/`polity_name` alone, so all
  13 reporting members came out with their own `area_code` and the shared label
  `"Rest of World"`. Measured: Syria's 2000 livestock read **3,408,857** head
  against **38,048,415** after the fix, with fractional animals (`1227745.45`) as
  the visible symptom of a share that should have been 1. `slaughtered_heads` was
  never affected, because it does not pass through this splitter — which is what
  made the defect look like a unit-conversion bug.

  The stock join, the carry-forward and the row-count grouping are re-keyed the
  same way. Globally this moves `heads` **+0.22%** and `LU` +0.13%; `ha`,
  `tonnes` and `slaughtered_heads` are bit-identical, because only areas sharing
  a label were ever affected.

* **`fill_linear()` no longer depends on the order its rows arrive in.** Without
  `.by`, it never sorted: carrying a value forward or backward and the
  `value_smooth_window` moving average are all positional, so an unsorted input
  filled the wrong way round. On a 2015-2020 series anchored at 2016 and 2019,
  reversing the rows swapped the two carry labels, and interleaving them left
  both outer gaps unfilled and moved two interpolated values. Both paths now
  sort by `.by` and then `time_col` first, and **rows come back in that order**
  — the grouped path already did through `setkeyv()`, the ungrouped one now
  matches it. Grouped output is unchanged for already-sorted input, which is
  every caller inside the package.
  Three further gaps in the same file are closed. A `value_smooth_window` that
  leaves a group with no valid anchor (gaps one year apart, or a window wider
  than the group) aborted with `missing value where TRUE/FALSE needed`; both
  paths now share one filling core, leave those gaps as `"Gap not filled"`, and
  cannot diverge again. `fill_linear()` used to trust a `.whep_sorted_by`
  attribute it had stamped on a previous call, which a `setorderv()` in between
  does not clear, so a reordered data.table was filled in the wrong direction
  and came back carrying a `sorted` key its rows did not obey; the sort is now
  verified against the rows. And in `fill_proxy_growth()`, the documented
  weighted proxy syntax (`"gdp:region[population]"`) aborted in `setnames()` on
  every call, so it had never run; with that fixed, its weights are lagged
  before the rows without a growth rate are dropped, which is what makes them
  the previous period's weights rather than the previous surviving row's.
* **One unvaluable 1:n split no longer erases observed data.**
  `harmonize_interpolate()` summed the split 1:n contributions together with
  the already-correct `"simple"` component using `sum()` without `na.rm`, so a
  single contribution with a missing `value`, or with a share that could
  neither be computed nor interpolated (every year of the group totalling
  zero makes the shares `NaN`), turned the whole harmonized
  `(item_code, year)` cell into `NA`/`NaN` — including the observed values
  summed into it. Unvaluable contributions are now dropped with a warning
  naming the affected cells, and the observed values survive. Published values
  change only where the old output was `NA`/`NaN`: such a cell now holds its
  observed `"simple"` sum, or disappears if it had none. No cell that was a
  number before changes.

* **`build_cbs_prices()` no longer drops crop residues into an NA bucket.**
  The residue routing in `.add_residue_prices()` read
  `Herb_Woody == "Woody"` inside a nested `fifelse()`, so every item whose
  herbaceous/woody habit is missing got `NA` as its residue item. Those rows
  were pooled into one `NA`-keyed group and then dropped, and the pool mixed
  the mass and value of unrelated items on the way. On the real
  `faostat-trade-bilateral` pin (1986-2021) that silently discarded **72 rows**
  (36 years x 2 elements) of residue value. Residues are now generated only for
  primary crops and grassland — processed and animal products never had a crop
  residue — and a crop with no recorded habit takes the herbaceous default,
  reported in a warning naming the items (currently Cottonseed, Palm kernels
  and Palm Oil, whose `Name` is unset in `items_prod_full_raw.csv`).
  **Published values move for one item, `Other crop residues` (2106)**: its
  tonnage basis grows by 15.0% on average (2020 exports 345.6 Mt to 401.9 Mt)
  and its price shifts by -2.6% on average (2020 exports -1.2%, largest single
  move +6.8%). `Straw` (2105), `Firewood` (2107) and every non-residue item are
  unchanged to the digit.
* **`calculate_soc_dynamics()` returns one schema for all five SOC models.**
  It used to hand back whichever shape the selected model happened to produce:
  `hsoc` (the default) came back long as `pool` / `year` / `stock_mgc_ha` /
  `rate_mgc_ha` with **no** `soc_total`, while `rothc`, `icbm`, `amg` and
  `century` came back wide with `soc_total` and their own mutually exclusive
  pool columns (`dpm`/`rpm`/`bio`/`hum`/`iom`, `y`/`o`, `ca`/`cs`,
  `str`/`met`/`act`/`slw`/`pas`) — no two of the five agreed, so a caller had
  to branch on `model`. The selector now reshapes whichever model ran to the
  long schema `year`, `pool`, `stock_mgc_ha`, `soc_total`, `method_soc`: pool
  detail is kept, the model-specific part sits in the values of `pool` instead
  of in column names, and the five runs of a sensitivity analysis stack with a
  plain `dplyr::bind_rows()`. Total-only callers read
  `dplyr::distinct(out, year, soc_total)`. `calculate_soc_hsoc()` itself is now
  wide like its four siblings (`year`, `fresh`, `humus`, `iom`, `soc_total`) and
  no longer returns the per-pool `rate_mgc_ha`, which was exactly the forward
  annual difference of `stock_mgc_ha` and is recoverable from it. **No published
  value changes**: every pool stock and every `build_carbon_balance()`
  equilibrium is bit-identical before and after (checked across nine HSOC
  parameterisations and the spin-up of all five models).
* **WHEP now models the Rest-of-World reporting members in their own right.**
  FABIO folds 61 FAOSTAT reporting areas into its single `Rest of World` column,
  and `polity_area_code` inherited that fold, so any territory outside FABIO's
  192-country layout was published as `ROW`. FABIO's layout is a methodology this
  package compares against, not a constraint on which territories it represents,
  and the country set is WHEP's own decision (#459).

  The fold was also not doing what its name suggests. Of the 61 members only
  about a third report anything; the rest contribute no rows, so folding them is
  arithmetically a no-op. Everything the bucket carried came from the members
  that DO file returns, and the fold discarded whose data it was — Syria's
  production was published as "Rest of World" despite Syria filing its own
  FAOSTAT returns. Promotion is therefore self-limiting: an area with no rows is
  unaffected either way, so no hand-maintained list of "which ones to promote"
  is needed.

  Measured on two full-range `get_wide_cbs()` builds (1850-2023): the published
  area count goes **195 → 216**, and 21 territories become standalone —
  Bermuda, Cayman Islands, Cook Islands, Equatorial Guinea, Faroe Islands,
  French Guiana, Greenland, Guadeloupe, Martinique, New Caledonia, North
  Macedonia, Niue, Réunion, Eswatini, Syria, Palestine and five more. Global
  totals move by at most **0.99%** (`stock_addition`), every other column inside
  0.4%. **Bucket 999 survives** as a genuine residual for the territories that
  report nothing, shrinking from 15,507 rows to 516.

  `options(whep.unfold_rest_of_world = "none")` restores the fold, which is what
  reproducing a number published before this change requires; it warns on every
  crosswalk read, because such a run no longer matches the published series.
  The `"successor_state"` folds (Sudan/South Sudan into bucket 206) are
  untouched — those are territorial identities, not a FABIO convention, and
  remain the subject of #414.

  An earlier measurement in #419 put this change at up to 13.7x on `feed`. That
  comparison predates the `dcast()` duplicate-key fix (#425/#429) and does not
  reproduce; #555 re-measured it at 1.0000.

* `create_typologies_of_josette()` and `create_typologies_grafs_spain()` gained
  an `example = FALSE` argument, so both now have runnable examples like the
  rest of the package's remote-data functions. Their documented `@return` was
  also wrong and is corrected: `create_typologies_of_josette()` returns a named
  list of three tibbles plus a `ggplot`, not a single tibble, and
  `create_typologies_grafs_spain()` returns `Province_name` and `Typologie` for
  `map_year` only, not a seven-column series over all years. No published value
  changes — the only new code path is the `example = TRUE` early return.
* **An aggregation bucket now sums, and comes out under one name.** The reader
  aggregation grouped rows by the member's polity **name** as well as by
  `polity_area_code`, so a bucket folding members that resolve to different
  polities was never actually summed: it came back as several rows under one
  `area_code`, carrying different `area` labels. That is live on the shipped
  crosswalk, not hypothetical — bucket 206 "Sudan (former)" folds FAOSTAT areas
  276 Sudan and 277 South Sudan, which resolve to two polities from 2012 on.
  Measured over the real pins, four sources came out split: `faostat-fbs-new`
  (2,056 duplicate `(area_code, year, item, element, unit)` keys),
  `faostat-trade-totals` (3,739), `faostat-production` (2,000) and
  `faostat-emissions-livestock` (144). The label is now derived after the sum
  from the **bucket's own** code — the same polity `polity_bucket_coverage()`
  reports and the reporting columns resolve — so one `area_code` has one `area`
  in one year. Each reader's total is **unchanged to the digit** and its row
  count falls by exactly its duplicate-key count.
  **Published values do move, for bucket 206 only**, because the duplicated keys
  were mishandled downstream in both directions: `build_primary_production()`
  changes 1,673 of 6,170,595 keys, all in 2012-2023, and against the raw pin the
  new value is the right one — bucket 206 goats in 2018 were 14,449,249 head
  (South Sudan alone, Sudan's 40,846,000 dropped) and are now 55,295,249, while
  2019 sugar cane was 10,898,000 t against 5,449,000 t reported and is now
  5,449,000 t. On a real 2005-2020 `build_commodity_balances()` the effect is
  678 changed keys, 559 of them area 206; every other area moves by **43.4 t in
  total across 119 keys** (largest single move 3.91 t, 4e-9% of the build).
  Element totals over that range move by 1.79% on `stock_variation` and by less
  than 0.03% on everything else. `reporting_polity_code` for bucket 206 is
  `SUD-1956-2011` before and after.
* `polity_end_year` / `end_year` is now read as **exclusive** everywhere, which
  is the convention upstream `whep-polities` publishes: a successor's
  `start_year` equals its predecessor's `end_year`, and 240 of the 245
  FAOSTAT-map rows in `polity_area_crosswalk` carry
  `polity_end_year == map_year_end + 1`. `add_polity_code()` used to join on
  `polity_end_year >= year`, so a period answered for one year past its end.
  Over the 1961-2024 grid for all 266 crosswalk areas that put **7** area-years
  on their period's end year, of which **3 landed in a state that had already
  dissolved** and still read `"matched"`/`"manual"`: 1993 Czechoslovakia
  (`F51-1947-1993`), 2006 Serbia and Montenegro (`SCG-1992-2006`) and 1992
  Yugoslav SFR (`F248-1991-1992`). Those three now report `"out_of_span"`. The
  other four are years the upstream map explicitly declares the area reports
  (`map_year_end`, inclusive), and the resolver keeps them: a reported year is
  never dropped for being one past a polity's end. **No published value moves**:
  over 1850-2024 x 266 areas, `polity_area_code` is unchanged on every row, the
  resolved-row count is unchanged (46,336 with the default back-cast anchor),
  and the only `polity_code` that moves is area 273 Montenegro in 1962, from
  `MNE-1913-1918` to `MNE-2006-2025` -- a nearest-period stand-in either way,
  now landing on the nearer period. `build_constant_territory_series()` reads
  the same convention, so a dissolved polity no longer sits on top of its
  successors in the hand-over year (238 polities carried a polygon in 1993 on
  the old reading against 236, and 453 extra active polity-years over
  1850-2024), where each grid cell goes to exactly one target and the
  predecessor was capturing the ones its successors should have received. Note
  that `ref_year = 2025` now aborts: the vintage's open periods carry 2025 as
  their exclusive end, so they stop at 2024.
* `inst/scripts/prepare_spatialize_all.R` no longer repairs
  `mueller_synthetic_n`'s FAO-style legacy ISO codes with a hand-maintained
  14-entry `recode()` list. The mapping now comes from
  `whep::polity_label_aliases` through `resolve_polity_label()`, bridged back to
  the country grid's numeric `area_code` through the polity's `iso3_code` and the
  same `regions.csv` lookup the grid is rasterised from. **No published values
  change**: the resulting `crop_synthetic` table is byte-identical, 5,043 rows
  resolving to the same 156 area codes with a maximum rate difference of 0. Four
  of the 14 list entries (`BHA`, `BAR`, `DMI`, `STL`) named codes the dataset
  never uses.
* New `polity_coverage_gaps()` reports the rows of a built table whose
  `reporting_polity_code` is a nearest-period stand-in, i.e. a polity that did
  not exist in that row's year. `add_polity_code()` has always reported these as
  `mapping_status == "out_of_span"`, but the reporting-column boundary every
  area-keyed output crosses dropped that column, so the documented uncertainty
  was invisible in published data. Measured on the real FAOSTAT production path
  (`.read_input("faostat-production")` aggregated to polities, 1961-2024),
  **5,637 of 3,011,912 rows (0.19%)** are stand-ins, all of them bucket 206
  "Sudan (former)" over 2012-2024 on `SUD-1956-2011`; on `faostat-fbs-old` it is
  972 of 5,331,877 (0.018%), the same bucket over 2012-2013. Across the whole
  crosswalk over 1961-2023 it is 922 of 16,658 resolved area-years in 28 areas,
  in both directions. **No published value or column changes**: the new function
  is a separate query, and carrying the signal on the outputs themselves is
  opt-in through `options(whep.polity_mapping_status = "flag")` for a logical
  `reporting_polity_out_of_span`, or `"status"` for the full
  `reporting_mapping_status`. The default, `"none"`, is today's schema. Which of
  the two to adopt as the default is an open decision (#545).
* `build_energy_co2_extension()` gains a third `unclassified` treatment,
  `"polity_region"`, for the **live** reporting areas `gleam_geographic_hierarchy`
  has no row for. On today's crosswalk that is Nauru (area 148) and Tuvalu (227):
  they exist, report under their own area codes, and their meat production left
  the extension unpriced. `"polity_region"` groups them by running GLEAM's own
  scheme rules on the continent their polity carries -- no grouping label is
  added to the package -- so Tuvalu now lands on `"Least developed countries"`,
  the classification `.energy_ldc_iso3()` already asserted for TUV while joining
  against a table with no TUV row. Those rows are labelled
  `"GLEAM_3.0_energy_meat_polity_region"` in `method_energy`. **No published
  value changes**: the default is still `"drop"`, and the full 1850-2023 build is
  bit-identical under both `"drop"` and `"global_mean"`. Measured, for the
  decision: `"polity_region"` adds 366 rows and 2 areas (61,149 to 61,515),
  moves no existing row by any amount, and raises total energy CO2e by
  0.0000155%; it puts Nauru at 288,502 kg CO2e and Tuvalu at 424,851 kg over
  1961-2023, against 664,412 and 1,775,719 under `"global_mean"`. Whether the
  default should move is left open in whep#415.

* `folded_reporting_areas()` no longer calls all 61 Rest-of-World folds a FABIO
  convention, because for four of them it is not one. FABIO's own published
  region list -- `io_codes.csv` of the v1.1 release (Zenodo record 2577067),
  192 areas x 125 commodities, the file `inst/scripts/compare_fabio.R` already
  downloads -- gives **153 New Caledonia, 154 North Macedonia, 209 Eswatini and
  212 Syria** each their own commodity block, distinct from area 999 `RoW`; the
  FABIO source repository marks all four `current == TRUE`, which is exactly the
  flag its `replace_RoW()` keeps out of bucket 999. `regions_full` nonetheless
  gives them `fabio_code` 999 while flagging them `cbs` `TRUE`, and Syria is the
  single largest contributor to the fold (24,426 `faostat-production` rows).
  Those four now come back as a third `fold_kind`, `"cbs_reporter_folded"`,
  separating them from the 57 folds FABIO does make; a new
  `options(whep.unfold_rest_of_world = "cbs_reporters")` promotes only those
  four, alongside the existing `TRUE` (equivalently `"all"`) for all 61. **No
  published value changes**: `fabio_code`, `polity_area_code` and every polity
  label are untouched on the default path, which the suite pins against the
  committed crosswalk. Whether to correct `regions_full` is an open decision
  (#556); doing so is the numeric un-fold of #563's option 3, and must not be
  done at the polity level alone (#480, reverted in #561).
* `build_feed_demand()` gains `region_fallback`, which decides how a reporting
  bucket the crosswalk leaves without a Bouwman feed region gets one. Rest of
  World (`area_code` 999) folds 62 FAOSTAT reporting areas, 58 of which have a
  region of their own, and kept none of them; every region-keyed join therefore
  missed and the bucket's feed demand went nowhere. The new default,
  `"member_mix"`, splits the bucket across its members' regions weighted by the
  livestock those members carry (Middle East 0.69, Southern Africa 0.18,
  Oceania 0.045, Eastern Europe 0.045, then five smaller regions). `"none"`
  restores the previous behaviour. **This moves published values, for area 999
  only.** Measured over a full 1850-2023 `get_primary_production()`: with
  `by = "feed_type"` the mix gains 5,500 keys and 926,327,446 t of dry matter
  (world total +0.151%) where 808,638,528 t had been dropped outright, and no
  key that existed before changes by more than 1e-6 t; at `by = "category"`
  area 999 goes from 808,638,528 to 926,327,446 t (+14.6%) with
  `demand_tier = "ipcc"`, and from 0 to 2,035,462,034 t with
  `demand_tier = "fcr"`. All 191 other areas are bit-identical in both tiers.
  The five continent residuals `901`-`905` stay unmapped on purpose: they span
  several Bouwman regions each and carry no production row at all.
* `build_cell_polity()` gains `area_key`, choosing which code the shared
  cell-area grid every gridded consumer keys on. The grid is rasterized from
  present-day polygons through `regions.csv`, so its `area_code` is a raw
  reporting-area code, not the `polity_area_code` bucket every polity-keyed
  national table in whep is aggregated on. On the deployed
  `cell_polity_fraction` parquet **12 of its 182 codes (819 cells) are not a
  bucket** -- Syria, Palestine, Eswatini, Equatorial Guinea, North Macedonia,
  New Caledonia, Western Sahara, Andorra, Liechtenstein and San Marino (all
  folded into `999` Rest of World), `62` Ethiopia PDR (into `238`) and `277`
  South Sudan (into `206`) -- so their cells match nothing on either side of
  the join. Measured against real 2010 harvested area, **21.09 Mha of national
  cropland, 1.525% of the world total, cannot be placed on any grid cell**;
  15.30 Mha of that is the whole of Ethiopia and 5.67 Mha is Rest of World.
  `area_key = "polity_area"` re-keys the grid on the bucket and cuts that to
  0.12 Mha (0.009%). **No published value changes**: the default `"grid"`
  reproduces today's output bit-for-bit and only adds a warning naming the
  codes, because switching moves every gridded consumer's territorial
  attribution at once. Whether it should become the default is issue #460.
* `polities` and `polity_area_crosswalk` are re-synced against upstream
  `whep-polities` at `eb02dcb` (740 rows to **749**), which retired or superseded
  **14** codes this package had been treating as live and published a replacement
  for each. The user-visible consequence is that `reporting_polity_code` values
  change: `ROW-1850-2023` becomes `ROW-1850-2025`, the six regional buckets
  `RAFR/RASI/REUR/RNAM/ROCE-1850-2021` and `RLAM-1850-2013` become `-1850-2025`,
  and `CAN-1948-2025` becomes `CAN-1949-2025`. Newfoundland acceded on 31 March
  **1949**, so calendar 1948 now resolves to pre-accession Canada
  (`CAN-1886-1949`, 9,379,600 km2) instead of post-accession Canada
  (9,774,537 km2) -- a 394,937 km2 correction visible only where the back-cast
  anchor is off, i.e. historical trade sources reported under their own borders.
  The bucket extensions recover **88 previously unresolvable area-years** over
  1961-2024 (`RLAM` alone had lost 2014-2024), of which 20 fall inside the
  default `1850:2023` build range. **No published value is expected to move**:
  `polity_area_code`, the numeric bucket every matrix workflow aggregates on, is
  byte-identical for all 267 reporting areas, and the recovered area-years are
  either year 2024 (outside the default range) or areas 901-906, which are WHEP
  reporting labels no source dataset carries. That is a crosswalk-level
  measurement, not a full-pipeline one.
* `read_population()` now reports the `area_code` rows that are aggregates of
  several territories, alongside the message it already emitted for the dropped
  regional residuals. `area_code` is `polity_area_code`, a bucket rather than an
  identity, so with the real `gdp-population` pin eight ISO3 codes fold into two
  rows: 999 "Rest of World" (Syria, North Macedonia, Palestine, Eswatini,
  Equatorial Guinea, French Guiana) and, from 2012, 206 "Sudan (former)"
  (Sudan + South Sudan). That is 0.35% of the population over 1850-2021 and
  1.05% in 2015, against the 0.07% the existing message covered. The fold is
  deliberate -- those are the codes the commodity balances are keyed on, so a
  finer key would leave their food supply with no denominator -- and the
  `@return` documentation now says a row is an area code rather than a country.
  **No published value changes**: the output of a full real-pin read is
  byte-identical before and after (28,255 rows, 530,970,330,534 person-years).
* `build_energy_co2_extension()` now **reports the meat production it cannot
  price** instead of dropping it in silence. Reporting areas with no row in
  `gleam_geographic_hierarchy` get no energy intensity, so their carcass
  production used to leave the extension without a word: measured on the full
  FAOSTAT production input, that is **595 Mt of carcass weight, 3.48% of
  1850-2023** and **15.3% of 1961**, over eight areas -- the USSR (436.8 Mt),
  Belgium-Luxembourg (43.9), Czechoslovakia (38.1), the Yugoslav SFR (37.8), the
  Rest-of-World bucket 999 (25.5), Serbia and Montenegro (12.8), Tuvalu and
  Nauru. A warning now names them with their tonnage and share. A new
  `unclassified` argument selects the treatment: `"drop"` (default) keeps the
  historical behaviour, and `"global_mean"` prices those areas at the unweighted
  world mean of the published GLEAM factors, marking the affected rows
  `"GLEAM_3.0_energy_meat_global_mean"` in `method_energy`. **No published value
  changes on the default path** (verified bit-identical on the full input);
  `"global_mean"` raises total energy CO2e by 4.4% over 1850-2023, 14.3% in 1961
  and 0.17% in 2023. Which treatment is right is an open decision (#492).
* New `polity_bucket_coverage()` reports every FABIO reporting bucket
  (`polity_area_code`) that folds more than one polity in a year, and says
  whether the polity the bucket itself resolves to covers the fold
  (`"aggregate"`), covers only part of it (`"partial"`), or is absent
  (`"unlabelled"`). Exactly one bucket in the shipped crosswalk is `"partial"`:
  206, which folds FAOSTAT areas 276 Sudan and 277 South Sudan while no live
  polity means "Sudan and South Sudan". Measured on real FAOSTAT production for
  2015, that bucket carries 53,124,088 t for Sudan plus 14,876,146 t for South
  Sudan -- 21.9% of the bucket -- under one polity label.
  `.aggregate_to_polities()` now warns when it builds such a bucket; silence it
  with `options(whep.warn_polity_folds = FALSE)`. **No published value changes:**
  the fold, the numeric bucket and every polity label are exactly as before, and
  the only new behaviour is the warning and the new function.
* The FABIO Rest-of-World fold is now **reported instead of silent**, and the
  measurement that was blocking a decision on it has been redone. New
  `folded_reporting_areas()` lists every reporting area whose `polity_area_code`
  is not its own `area_code`: 61 areas folded into Rest of World, of which 14
  carry observed data (Syria 24,426 `faostat-production` rows, Eswatini 12,196,
  Réunion 11,970, Palestine 9,606, the Faroe Islands 2,458 and nine more, 130,103
  rows in total), plus 3 successor-state folds (62 into 238, 276 and 277 into
  206). `.aggregate_to_polities()` now warns per source, naming the areas and the
  rows it folded, because these areas resolve with `mapping_status == "matched"`
  and so no coverage count could show them. **No published value changes.**
  `options(whep.unfold_rest_of_world = TRUE)` promotes each member to its own
  code for sensitivity work; it warns on every crosswalk read and is not a
  production mode. Measured on a full-range `get_wide_cbs()` (1850-2023),
  promoting all 61 members moves global totals by at most 1.2%
  (`stock_addition`) and under 0.1% for `feed`, `production` and `processing` —
  the 13.7x feed inflation recorded in issue #419 does not reproduce, because
  that comparison predates the `dcast()` duplicate-key fix in
  `.select_best_source()` (#425).
* `build_primary_production()` gains `federation_land`, controlling how the
  pre-1962 LUH2 back-cast reaches an area whose territory is a dissolved
  federation. LUH2 land use is keyed on present-day ISO3, so 15
  Belgium-Luxembourg, 51 Czechoslovakia, 228 USSR and 248 Yugoslav SFR have no
  land record of their own and their pre-1962 production has never been
  back-cast at all -- 14.3% of 1961-62 FAOSTAT production tonnage, USSR alone
  12.2%. `federation_land = "successor_union"` rebuilds each federation's land
  series as the sum of its successor states' LUH2 land, resolved from the
  `successor` relation published in `polities`, and reduces the unmatched areas
  from 4 to 1 (only Belgium-Luxembourg, which upstream publishes no successor
  for). **No published value changes by default**: `"none"` keeps current
  behaviour. Measured on a 1850-1965 build, `"successor_union"` raises global
  pre-1962 production tonnage by 13.9% (1850) to 19.4% (1960), moves exactly
  three area codes (51, 228, 248) and moves no row at or after 1961; it also
  closes the hard 0-to-704 Mt discontinuity USSR had at the 1961 splice
  (1960/1961 now differ by 1.2%).

* `polity_area_crosswalk` now takes its area-to-polity mapping from
  **upstream's published map** (`faostat_area_polity_map.csv`, read via
  `WHEP_POLITIES_FAOSTAT_MAP`, 281 rows over 228 FAOSTAT area codes) instead of
  inferring it from the polity-code string with `sub("-.*", "", polity_code)`.
  The build aborts if the map is absent rather than falling back silently. Seven
  reporting areas gain a mapping no prefix could reach -- Djibouti (72) had
  resolved to **nothing**, and areas 7, 20, 181, 237, 249 and 251 reach
  `ANG-1905-1975`, `BEC-1885-1966`, `SRH-1953-1964`, `F237-1954-1975`,
  `F249-1918-1990` and `NRH-1953-1964`. Area 15 resolves to `BLX-1850-1999`
  rather than `BLX-1921-1999`, and area 206 "Sudan (former)" to `SUD-1956-2011`
  rather than standing in on post-secession `SDN-2011-2025`. Prefix inference is
  kept, labelled in a new `mapping_source` column, only where the map is silent:
  seven areas it does not cover (351 and 901-906) and periods outside the spans
  it declares, which is what keeps pre-1961 history resolvable for sources
  reported under their own borders. The four-part codes that used to enter
  through the prefix collapse (`AZE-SSR-1920-1991`, `IDN-BLB/JVM/OTH-1949-1951`,
  `MMR-LWR-1852-1885`) are gone, taking crosswalk `subnational` rows from 6 to 3
  and ambiguous `(area, year)` resolutions from 199 to 86. **This moves published
  values and no magnitude comparison has been run.**
* `polities` is refreshed from upstream, 603 rows to **740**, because the
  published map names 43 polity codes the old snapshot did not contain. This is
  the refresh #485 drafted.

* The manure/nutrient chain now documents **one vocabulary for `territory`**: a
  stringified `area_code`, what `redistribute_feed()` emits and what the
  pipeline has always passed. The `@examples` of `estimate_n_excretion()`,
  `split_manure_management()`, `apply_management_losses()`,
  `allocate_manure_to_land()`, `allocate_manure_transport()` and
  `build_livestock_nutrient_flows()` used ISO literals instead (`"ESP"`, and
  `"ES"`, which the chain's own resolver rejects outright). Passing an `iso3c`
  still resolves, as a bridge for existing fixtures, but now warns: it can only
  answer with `polity_area_code`, a FABIO aggregation bucket, which for 62 of
  the 257 ISO3 codes in `regions_full` is not that territory's own code (61 land
  on 999, Rest of World; `"SSD"` lands on 206, Sudan (former), where the numeric
  form `"277"` keeps South Sudan). No published value changes: the pipeline
  itself never took the ISO3 branch.

* `urban_n_reference` now carries a **`polity_code`** column
  (`"ESP-1800-2025"`) alongside its numeric `area_code`, so the benchmark series
  names the territory it measures instead of only the FAOSTAT aggregation bucket
  `203`. The code is resolved per benchmark year against the polity active in
  that year. Additive: `area_code` and every measured value are unchanged, and
  no exported function reads this dataset at runtime. This sets the convention
  for the other territory-keyed coefficient tables.

* `add_polity_code()` no longer presents a **nearest-period stand-in as a real
  match**. When no mapped period covers a row's year the row still resolves to
  the nearest period of the same area, but `mapping_status` now reports
  `"out_of_span"` instead of inheriting the crosswalk's `"matched"`/`"manual"`,
  so a figure attributed to a polity that did not exist in that year is
  visible. Over the FAOSTAT era this covers 993 of 16638 resolved area-years
  across 36 areas, in both directions: FAOSTAT area 206 "Sudan (former)" for
  1961-2010 resolved to `SDN-2011-2025` (post-secession Sudan, which excludes
  the territory those figures cover) and area 51 Czechoslovakia for 1994-2023
  resolved to `F51-1947-1993`, a state that had dissolved. Relabelling only:
  no `polity_code` assignment changes (0 of 16638), and no exported table
  carries `mapping_status`, so no published value moves.
* **Crop-residue feed-use fractions are live again.** The
  `residue_feed_fraction` coefficient table's region column was named
  `region_hanpp` but held UN M49 sub-regions, and
  `calculate_residue_destinies(method = "krausmann_regional")` joined it against
  a `region_hanpp` column the pipeline filled from `regions_full$region_HANPP`.
  The two vocabularies share no label, so the join matched nothing and every
  polity silently took the `"Global"` default of `0.20` — a table spanning
  `0.05` to `0.45`, dead in full. The column is renamed to `region_un_sub`
  (values unchanged, apart from `South-Eastern Asia` -> `South-eastern Asia` to
  match `regions_full`), and the method now requires a `region_un_sub` input
  instead of `region_hanpp`; 230 of 261 areas receive a region-specific
  fraction, the rest (Micronesia, Polynesia, RoW and areas with no M49
  sub-region) keep the `0.20` fallback. This **moves published values**:
  `residue_feed_dm_t` and `residue_burn_dm_t` change, and with them
  `build_residue_feed_avail()` and the nitrogen balance's `used_residue_n_t` /
  `burnt_residue_n_t`. `residue_soil_dm_t` and their sum do not change (neither
  depends on `feed_use_fraction`), so `build_soil_carbon_inputs()`'s residue
  carbon is unaffected.
* The pre-1962 commodity-balance fills now key their **proxies on the polity**
  rather than on an area name. Three name vocabularies met at that join: the
  frame carries the periodized `polity_name` (FAO area 3 arrives as
  `"Albania (1913-2025)"`), the gdp/population pin carries its own labels
  (`"Albania"`) and the LUH2 land table the crosswalk's static `area_name`. 57
  of the pin's 196 names (8,263 rows, 27.8%) and 96 of the LUH2 labels (41.7% of
  land rows) were names no builder emits, so those territories silently kept
  their gaps. **This moves published values**: proxy coverage of the pre-1962
  frame's (year, polity) cells rises from 13,664 to 18,480 of 22,624 for
  population (43 polities gain a proxy, none lose one) and from 402 to 567 of
  606 for agricultural land over 1900-1902 (55 gain, none lose). Aggregates that
  are only reached by folding other territories into them (Rest of World, 999)
  are still left without a proxy: what an aggregate's proxy should be is an open
  methodological question (#493).
* Every area-keyed exported output now carries the **reporting-polity columns**
  (`polity_area_code`, `reporting_polity_code`, `reporting_polity_name`,
  `reporting_polity_has_geometry`), so a caller can tell which territory a row
  belongs to, and whether it has a polygon, without re-joining the crosswalk.
  This widens the contract from 10 exports to 35, covering the balances
  (`build_nitrogen_balance()`, `build_carbon_balance()`, `build_water_balance()`,
  ...), the footprint extensions (`get_crop_land_extension()`,
  `build_livestock_ghg_extension()`, ...) and the gridded readers
  (`read_luh2_landuse()`, `get_soc_climate_drivers()`). The columns are
  additive: no existing column or value changes. `get_faostat_data()` keeps
  returning raw FAOSTAT area names (it is the pre-resolution reader), and
  `build_grazing_feed_footprint()`/`build_land_balance_footprint()` are
  aggregated over time and have no year to resolve a polity against.
  `build_urban_n()` now returns the numeric WHEP `area_code` rather than the
  character territory key its manure-transport reuse works in; an ISO3 input
  still resolves through the same checked resolver as the manure path.
* Add gridded soil **water, carbon and nitrogen balances** (0.5° cell × polity
  fragment). `build_water_balance()` closes the annual cell water budget from
  LPJmL hydrology and exposes drainage (for N leaching) plus footprint terms;
  `get_soc_climate_drivers()` emits the monthly SOC climate drivers from CRU.
  `build_carbon_balance()` runs historical SOC dynamics (equilibrium init,
  LUH2-driven land-use march, LUC C-transfer) and derives ΔSON via asymmetric
  C:N; `calculate_soc_dynamics(model = c("hsoc","rothc","icbm","amg",
  "century"))` wraps the five SOC models (default `"hsoc"`), fed by
  `build_soil_carbon_inputs()`. `build_nitrogen_balance()` assembles inputs
  (`build_n_inputs()`, incl. `build_n_deposition()`/`build_urban_n()`) minus the
  selectable losses (`calculate_nh3()`/`calculate_soil_n2o()`/
  `calculate_n_leaching()`, with the process-based MANNER NH3 model) into
  surplus, NUE and GWP/CO2e indicators. New readers: `read_lpjml_hydrology()`,
  `read_cru_climate()`, `read_luh2_landuse()`, `read_hyde_population()`,
  `read_lpjml_wind()`, `read_soil_ph()`, `read_soil_hydraulic()` (per-cell
  field capacity, wilting point and porosity from HWSD texture, feeding the
  ICBM SOC moisture modifier). New coefficient datasets for SOC turnover,
  humification, C:N, N2O EFs, MANNER and denitrification, the crop
  growth-stage soil-cover curve (`soc_soil_cover_curve`), and the USDA
  texture-class soil hydraulic properties (`soil_hydraulic_by_texture`) with
  the HWSD texture-code crosswalk (`hwsd_texture_usda`).

* Add `ensure_columns()` to complete tibbles from typed zero-row prototypes,
  with safe casts, scalar defaults, deterministic ordering, and explicit
  extra-column handling.
* Add `decompose_weighted_ratio()` for exact Kitagawa/Shapley, additive LMDI,
  and sequential-polar decomposition of changing aggregate ratios.
* `build_io_model()` and `build_footprint()` gain a `method = c("mass",
  "value")` argument for co-product allocation. `"value"` splits a multi-output
  process's inputs (and the pressures embodied in them) across its products by
  economic value (mass times export price from `build_cbs_prices()`) instead of
  mass, falling back to mass for any process whose co-products lack prices. The
  default `"mass"` preserves previous results (#100).

* Add `build_livestock_ghg_extension()`: aggregate the IPCC enteric and manure
  emissions pipeline into a greenhouse-gas footprint extension (kg CO2e per
  `area_code` x `item_cbs_code`), with selectable IPCC tier and GWP100 standard,
  ready to feed `compute_footprint()`. `inst/scripts/footprints.R` gains a
  `WHEP_FOOTPRINT_PRESSURE` switch to trace GHG instead of land (#100).
* `calculate_livestock_emissions(tier = 1)` now also computes manure N2O
  (direct and indirect) from IPCC default per-head excretion rates, so the
  default Tier-1 GHG footprint covers enteric CH4 + manure CH4 + manure N2O
  rather than methane alone (#100).
* Add `build_crop_soil_n2o_extension()`: IPCC 2019 Tier 1 nitrous-oxide from
  nitrogen applied to managed soils -- synthetic fertiliser (F_SN), applied
  manure (F_ON, FAOSTAT "Manure applied to soils"), and above-ground crop
  residues (F_CR, residue dry matter times Table 11.1a N content, net of
  removal) -- as a CO2e footprint extension keyed by `(area_code,
  item_cbs_code)`. F_SN and F_ON are FAOSTAT country totals allocated to crops
  by harvested area. Direct (EF1), indirect volatilisation (FracGASF for
  synthetic, FracGASM for manure; residues excluded) and leaching per Ch. 11;
  EFs verified against the IPCC source. Grazing deposition (F_PRP) and
  below-ground residue N are documented follow-ups (#100).
* Add `build_footprint()` and `align_extension()`: trace a long-format
  extension table through the supply chain end to end, de-duplicating the
  per-year alignment glue previously repeated across the footprint driver
  scripts (#100).
* Fix non-dairy cattle being misclassified as dairy in the livestock emission
  factors: a case-insensitive `"dairy"` match also matched the `"Cattle,
  non-dairy"` item name, so beef cattle received the much larger dairy enteric
  and manure factors. Non-dairy cattle CH4 (Tier 1 and Tier 2) is now classified
  correctly.
* Fix `prepare_livestock_emissions()` failing on real production data when
  `live_anim_code` is supplied as an integer.
* Fix `build_io_model()` not passing the now-required `feed_mode` argument to
  the feed-redistribution step.
* Fix mojibake area labels in `regions_full`, `polities_cats` and
  `polity_area_crosswalk`: Curaçao, Côte d'Ivoire and "Netherlands Antilles /
  Curaçao" shipped with their accented letters read as pairs of Latin-1
  characters. The vendored harmonization CSVs are now repaired on read in
  `data-raw` (#399).
* `polities_cats` is now derived from `regions_full` rather than vendored as a
  second hand-maintained copy of the same 39 columns, so the two can no longer
  drift. They had: 17 columns disagreed over the 198 shared area codes, and 95
  of the differing cells were the literal string `"0"` in `eia`, `iea` and
  eleven `region_*` columns where `regions_full` leaves `NA`. Those 95 cells are
  now `NA`; the row set, row order, column names and column types are unchanged,
  and the deliberate fold of Bhutan into `RASI` and Comoros into `RAFR` is kept
  as an explicit override (#406).
* `consolidate_sources()` gains two opt-in `tie_break` options for panels whose
  sources report exact zeros or several quality variants of one cell.
  `coverage = "positive"` counts the coverage tie-break over strictly positive
  values instead of non-missing ones, so a zero-padded series no longer wins on
  inflated coverage; `quality_variants = TRUE` collapses a source's several
  `quality_col` variants of a cell to its best-ranked one instead of aborting.
  Both default to the previous behaviour (#139).
* `get_faostat_data()` no longer attaches and then unloads `FAOSTAT` to make
  `FAOSTAT::fillCountryCode()` see its lazily loaded `FAOcountryProfile`. The
  ISO3 lookup now loads that dataset explicitly and matches area names itself,
  reproducing `fillCountryCode()`'s rule (exact match against the six profile
  name columns, unresolved when several profile rows match). Verified identical
  on all 232 FAOSTAT area names in `regions_full` and upstream's
  `faostat_area_polity_map`: 215 resolve, 0 differences. Two side effects are
  gone -- the user's `FAOSTAT` session state is left alone, and rows keep their
  input order instead of being sorted by area name by an internal `merge()`
  (#520).
* `citation("whep")` now returns two entries -- the package itself, carrying its
  CRAN DOI and all five authors, and the FABIO paper the model builds on --
  where it returned only the generated `DESCRIPTION` default before. The package
  entry takes its year from `Date/Publication` rather than a hardcoded one. The
  machine-readable equivalents, `CITATION.cff` and `codemeta.json`, ship
  alongside it, and the package gained a
  [code of conduct](https://ropensci.org/code-of-conduct/) and a link from the
  README to the contributing guide. Groundwork for rOpenSci peer review (#75).
* The HWSD readers now say which column a local `hwsd_data.csv` is missing.
  `read_soil_ph()`, `read_soil_hydraulic()` and the soil-carbon clay driver
  check the extract against the columns they are about to read and abort naming
  the absent ones plus the script that re-exports a complete extract, where a
  partial extract previously surfaced as a `dplyr` error (`Column t_clay not
  found in .data`) that read as a code fault rather than a stale input.
  `inst/scripts/export_hwsd_attributes.R` now exports `t_clay`, so a re-run
  produces an extract the clay driver can read. No published value changes: a
  complete extract is read exactly as before (#596).
* **`propagate_fp_uncertainty()` no longer reseeds the calling session.** Given
  `options = list(seed = )` it called `set.seed()` and left it set, so every
  random number drawn afterwards depended on having made the call, and in a
  session that had not yet used the RNG it created `.Random.seed` where there
  was none. The seed is now scoped to the call and the previous RNG state (or
  its absence) is restored on return. Seeded results are bit-identical to
  before; unseeded runs still consume the caller's stream, so consecutive
  unseeded runs remain independent draws (#188).
* A failed Natural Earth download now reports how to recover instead of dying
  on its own error message. The abort interpolated the layer URL as
  `{.url {.natural_earth_url(layer)}}`, and cli >= 3.4.0 reads a `{}`
  expression starting with a dot as a style name, so the branch raised
  `Invalid cli literal` and the instructions never reached the user. The
  province typologies (`create_typologies_grafs_spain()`,
  `create_typologies_of_josette()`) are the callers that reach it. No published
  value changes (#594).

# whep 0.3.0

* Add `fill_proxy_growth()` and `calculate_lmdi()` (@jinfama, #65).
* Build datasets for GRAFS model in Spain (@AliceBeckmann, #18).
* Add harmonization functions (@justin-morgan-csic, #66).

# whep 0.2.0

* Add gapfilling functions `fill_linear()`, `fill_sum()` (@eduaguilera, #11).
* Now examples can't fail because of unavailable Internet resources (#58).

# whep 0.1.0

* Work in Progress FABIO model implementation:
  - Build supply-use tables (`build_supply_use()`) (#17).
  - Balance bilateral trade (`get_bilateral_trade()`) (#8, #9).
* Create article `Follow the workflow` for new contributors (#1, #2, #29).
* Download large datasets with `whep_read_file()` and `pins` package (#29, #43).
* Get raw FAOSTAT data with `get_faostat_data()` wrapper (#3).
* Initial CRAN submission.
