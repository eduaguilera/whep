# whep (development version)

* **Gridded outputs change for Comoros and Mayotte** (#404). `inst/extdata/cow_to_lpjml.csv`
  maps FAOSTAT `area_code` to LPJmL's country index and carries its own `iso3c` and
  `area_name` per row -- a second copy of area identity sitting next to the polities
  database rather than derived from it, and nothing compared the two. Comparing them found
  `45,"MYT","Mayotte",153`: FAOSTAT area 45 is **Comoros**, Mayotte is 270 and was absent
  from the file entirely. `prepare_spatialize_all.R` joins on `area_code` alone, so every
  Comoros grid cell carried Mayotte's LPJmL index while Mayotte's own cells fell to 0.
  Corrected to `45 -> COM` with index 46 and a new `270 -> MYT` row.

  Which field was wrong came from the file's own structure rather than assumption: rows are
  ordered alphabetically by `iso3c` with `lpjml_code` tracking that order (MWI 151, MYS 152,
  MYT 153, NAM 154), so 153 is where MYT belongs -- the index was right and the `area_code`
  was wrong. Comoros' index 46 was left unassigned until it could be read from LPJmL's own
  `managepar.h` (`#define Comoros 46`) rather than inferred from the ordering. With that
  header available the whole mapping was checked, not just the one row: all 190 indices exist
  in it, 6.0.5 and 6.1.1 define all 257 identically, and the 22 rows whose names differ from
  the header are the same countries under another convention ("Laos" against "Lao People's
  Democratic Republic").

* **`polities_cats` values change: a literal `"0"` becomes `NA`.** That table was exported
  with `"0"` wherever a value is absent, in 13 character columns -- `eia`, `iea` and every
  `region_*` classification -- while `regions_full`, which carries the same 40 columns over a
  superset of the rows, leaves them blank and so reads as `NA`. `"0"` reads as data:
  `!is.na(iea)` kept all 198 rows instead of the 139 with an IEA name, a join on `iea` would
  have matched 59 rows to each other as one country, and grouping by `region_UN` produced a
  `"0"` region. Cleared in character columns only, since a numeric 0 is a real value in `EU27`
  and `cbs`. Code that tested those columns against `"0"` must now test `is.na()`; nothing in
  this package did.

* `resolve_polity_label()` now falls back to a polity's own `polity_name` when no
  alias applies, mirroring upstream's "alias, then ISO/name family + year
  containment". Previously a caller passing the database's own name for a polity
  got `NA` -- `resolve_polity_label("Netherlands")` found nothing while `polities`
  carried a polity named exactly that. Two guards bound it: a name resolves only
  when exactly one polity carries it in the year asked about (52 pairs share a
  normalised name and overlap in years), and an alias covering that year outranks
  the name whatever its source. Sweeping 3,760 `(label, year)` pairs against the
  previous behaviour: 1,509 answers filled in, none lost, none changed. Names of
  deliberately-unmapped reporting areas are refused, so FAOSTAT's `"China"`
  (area 351, the aggregate of mainland, Hong Kong, Macao and Taiwan) does not
  resolve to the mainland polity and double-count against its own components.

* `add_area_code()` routes names the crosswalk cannot match exactly through the
  polities database instead of returning `NA`. It fills `NA` only, so the
  crosswalk stays authoritative wherever it has an answer, and it is year-aware
  (`"Czech Republic"` is Czechia from 1993 and Czechoslovakia before it).
  `get_primary_residues()`, the one builder that resolves areas by name, went
  from 44,985 of 475,688 rows (9.5%) with no area code to 200 (0.04%).

* **No value changes.** Every column total of a full-range `get_wide_cbs()` is
  within 1% of the previous release, and the numeric aggregation key
  (`polity_area_code`) is identical for all 267 area codes. Two earlier attempts
  in this cycle to improve on that were withdrawn after measurement, and are
  recorded here because both looked like fixes:

  - Summing the FAOSTAT areas that fold into one reporting bucket. The bucket
    holds several rows when its members' polities are differently named -- 1,525
    duplicate keys at 1990-2023, all in FABIO's Sudan region 206 -- which reads
    as a defect. It is not: the name distinguishes territory-periods, and rows
    for a member and rows already aggregating that member both land in the
    bucket, so summing double-counts. Measured: `food` 266x, `domestic_supply`
    1.9x, `feed` 12x. Grouping by `(bucket, polity_name)` is correct.
  - Promoting the 16 areas FABIO folds into rest-of-world that report data of
    their own to their own numeric key. Measured: `feed` 13.7x, `export` 13.2x,
    `production` 1.8x, with the entire `feed` increase landing on one area
    (212 Syria, at twelve times the world total). See #419; the attribution
    problem it was written for is still open, and its cost is now also measured
    -- 0.21% of livestock feed demand is dropped for want of a Bouwman region on
    RoW.

* `build_commodity_balances()` now **warns** if duplicate keys reach the source
  cast, naming the area codes, sources and years, rather than silently replacing
  values with row counts -- `dcast()` without `fun.aggregate` answers a duplicate
  key with `length()`, so those values become row COUNTS in any build that does
  not happen to die on the resulting type clash. A warning rather than an abort
  because these duplicates are pre-existing and shared with the previous release,
  so aborting refuses to build a pipeline that has always had them. Choosing
  sum-vs-first at the cast changes published numbers: #418.

* FAOSTAT ISO3 codes are corrected from the polities crosswalk rather than from a
  hand-maintained list. `.populate_iso3_code()` carried seven patches introduced as
  "manually fix some crazy countries/ISO3_CODE" -- China mainland, Türkiye, Netherlands
  (Kingdom of the), Sudan, South Sudan, Czechia and Lao PDR. All seven agree with
  `area_iso3c` in the crosswalk, so the list was a copy of something already published,
  and one that covered only the names somebody had hit: the next FAOSTAT rename lands as
  a silent `NA`. Now every reporting area is checked, disagreements are corrected and
  reported rather than applied silently, and names the crosswalk does not know keep
  whatever `FAOSTAT::fillCountryCode()` returned. Restricted to rows with an `area_code`,
  which is what makes the lookup unambiguous -- unrestricted, "France" maps to both FRA
  and BLM, and "Finland" to FIN and ALA.

* **`urban_n_reference$area_code` is now an integer FAOSTAT code, not the ISO3 string
  `"ESP"`** (#401). Every other `area_code` in the package is a numeric FAOSTAT code --
  the same workflow's own toy example uses `203L` for Spain -- so one concept was keyed
  two ways and this series could not be joined to any area-keyed table without a hand
  conversion the column name gave no hint was needed. A breaking type change, and listed
  as one, though nothing in the package joins this series: it is a benchmark a reader
  compares against by hand, which is why the column could hold a string for as long as
  it did and why the fix changes no output. Converted in `data-raw/` rather than in the
  vendored CSV, which would diverge from its source, and resolved through the crosswalk
  rather than by writing `203` as a literal -- so a renamed or re-coded territory
  surfaces as an error instead of a wrong join.

* Missing `iso3_code` and `cow_code` in `polities` are now real `NA`. They were the
  literal string `"NA"`, which upstream has since normalised to NULL at the source,
  so this now holds for `iso3_code` (82 rows), `cow_code` (216) and
  `polygon_feature_id` (29) rather than only where this package could patch it. The GeoPackage round-trip writes missing text as
  `"NA"`, so 79 rows of `iso3_code` and 185 of `cow_code` looked present:
  `is.na(iso3_code)` found 3 missing codes when 82 were missing, and any
  `!is.na(iso3)` guard treated those rows as carrying a valid ISO3. Converted at
  the read for every character column, since `"NA"` is not a legitimate value for
  any of them (Namibia is `NAM`).

* `resolve_polity_label()` normalises labels the way the upstream matcher does:
  folding accents, dropping parenthesised qualifiers (`"Sudan (former)"` is
  `"sudan"`), stripping a leading `"the"`, and mapping punctuation to spaces. The
  first version only lowercased and squished whitespace, which made it resolve 25
  of 6,627 cross-checked cases differently from upstream.

* New `resolve_polity_label()` and the `polity_label_aliases` dataset resolve a
  source's country **label** to a polity. `add_polity_code()` handles numeric
  FAOSTAT area codes, and nothing handled labels — so datasets carrying them had
  no supported path to a polity: `mueller_synthetic_n$iso3c` holds FAO-style
  legacy codes (`"BZE"`, `"ROM"`, `"ZAR"`) and
  `lassaletta_grassland_share$Country` holds name variants (`"Cape Verde"`,
  `"Swaziland"`). Resolution is source- and year-aware, since the same label can
  mean different things per source and its referent changes over time (`"Cape
  Verde"` is the Portuguese colony before 1975 and Cabo Verde after). The mapping
  is a copy of the contract published by whep-polities, not a lookup built here.

* Pre-independence FAOSTAT years no longer resolve to the modern polity. The
  crosswalk mapped each reporting area to a single polity prefix, and upstream
  gives the colonial and modern polities of seven chains different prefixes
  (Angola `ANG-1905-1975` then `AGO-1975-2025`, Sudan `SUD` then `SDN`, Zimbabwe
  `SRH` then `ZWE`, Botswana `BEC`/`BWA`, Zambia `NRH`/`ZMB`, Viet Nam `F237`,
  Yemen `F249`). The colonial polity was unreachable, so `add_polity_code()` fell
  back to the modern one: 1965 Angola resolved to "Angola (independent,
  1975-2025)", and 1970 Sudan to post-secession `SDN-2011-2025`, which excludes
  the South Sudanese territory those reports covered. `manual_area_prefixes` now
  lists both prefixes per area. 118 area-years change attribution, all of which
  previously had NO span-covering polity.

* `.iso3_to_fao_area_code()` no longer breaks an ambiguous ISO3 on row order. An
  ISO3 can name two FAOSTAT reporting areas because the pre-split entity is kept
  beside its successor (`ETH` is both 62 "Ethiopia PDR" and 238 "Ethiopia"), and
  `unique(bridge, by = "iso3c")` was resolving `ETH` to the **dissolved** area 62
  for every year. The tie is now broken on the polities database — prefer the
  area that is its polity's `polity_area_code` — and the function aborts rather
  than guessing if that leaves a choice.

* `.filter_dissolved_countries()` in `build_production()` no longer hardcodes
  FAOSTAT area codes with bare year cutoffs. The bounds now live in
  `.production_reporting_windows`, which separates the two things the integer
  version conflated: **polity existence** (Slovakia is `SVK-1993-2025`, so a
  1970 Slovakia row is attributed to a polity that did not exist) and **FAOSTAT
  reporting convention** (Belgium is `BEL-1831-2025`, but the source files it
  inside Belgium-Luxembourg until 1999, so admitting Belgium rows earlier would
  double-count). Only the first is derivable from the polities database, which
  is why the table is not replaced by a join — but the polity span now bounds
  every window, checked by `test_polity_reporting_windows.R`. That check found
  two missing lower bounds: the old filter admitted Czechoslovakia before 1918
  and Belgium-Luxembourg before 1850, which matters now that `historical_data`
  feeds pre-1961 rows through the same path. No row that previously survived is
  readmitted.

* **Breaking:** the `polity_code` column of `regions_full` and `polities_cats`
  is renamed `polity_prefix`. It never held polity codes — all 271 values were
  bare ISO3-shaped family prefixes (`"AFG"`, `"ROW"`), so joining it to
  `polities`, whose codes are periodized (`"AFG-1919-2025"`), silently matched
  nothing. Join on `reporting_polity_code` instead, which carries the real
  code. `inst/extdata/harmonization/regions_full.csv` and `polities_cats.csv`
  are renamed to match, and `test_polity_prefix_naming.R` now fails if either
  column regains the misleading name.

* `regions_full` and `polities_cats` are rebuilt against the current
  whep-polities database. The committed copies were stale and 12 area codes
  still resolved to superseded polities — 8 of them explicitly retired
  upstream (Angola to `AGO-1816-2025` rather than `AGO-1975-2025`, Brazil to
  the collapsed row rather than the post-Acre split).

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
