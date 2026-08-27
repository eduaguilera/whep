# Territorial join audit -------------------------------------------------------
#
# A join keyed on a territory column but not on `year` asserts that the
# territory means one thing for all of history. That is often right -- a
# coefficient table, a grid mask or a label lookup genuinely does not vary in
# time, and plenty of joins run inside a scope that is already one year -- but
# nothing in the source distinguishes a deliberate "this does not vary in time"
# from an oversight that hands 1961 Czechoslovakia the coefficient of 1993
# Czechia. whep#669 is that unverifiability, not a known wrong number.
#
# `.territorial_joins()` enumerates every join in the package namespace whose
# key mentions a territory column, and `.territorial_join_baseline()` records
# the classification of each one that carries no year. The pair is checked by
# `test_join_audit.R`, so a new year-free territorial join fails the
# suite until its author classifies it, and an entry that stops matching a real
# join fails too, which makes the list able only to shrink.
#
# It reads the NAMESPACE rather than the source files: `R/` is not shipped to
# where the tests run under `R CMD check`, whereas the parsed bodies always are.
# The cost is that the reason lives here rather than as a comment at each site;
# the benefit is that it is checked, and a comment never is.

# The classification of every join in the package that keys on a territory and
# not on a year, one row per distinct (owner, join function, key) signature with
# the number of calls carrying it. Four verdicts, and none of them may name a
# LABEL in the key:
#
#   single_year      the call site is already scoped to one year, so the year is
#                    a constant there and adding it to the key changes nothing.
#   time_invariant   the joined table has no time dimension by design -- a
#                    coefficient, a single-vintage map, a grid mask, a
#                    membership list. Adding a year would need a source that
#                    varies in time, which is a methodological change, not a fix.
#   identity_lookup  the join IS the identity resolution (code -> bucket, code
#                    -> ISO3, code -> label, polity -> attribute), where the
#                    year-scoped step is elsewhere or the key is already the
#                    year-scoped polity.
#   diagnostic       a warning or comparison path that reports on keys; it moves
#                    no published value.
# Two verdicts are GONE and are deliberately not in the vocabulary any more.
# `label_identity` covered one join, `.polity_code_from_labels()`, which read
# the pre-1962 CBS frame's polity out of its `area` label; whep#698 keyed that
# frame on the reporting bucket it already carries and deleted the function.
# `label_redundant` covered the last one, `.interpolate_destiny_shares()`, whose
# skeleton join named `area` beside `area_code`: both sides were two filters of
# one frame, so the label could not disagree there, but the guarantee was the
# caller's and an unmatched key was a dropped row rather than an error. whep#691
# keyed it on the code alone and re-attached the label once at the end, so no
# year-free join in the package names a label at all. Re-introducing either
# class means re-arguing that a label may be a key, which is the whole thing
# this audit exists to stop.
#
# Adding a row is not a way to pass the gate: it is a statement, reviewed like
# any other, that the join means the same thing in 1850 and in 2023.
.territorial_join_baseline <- function() {
  tibble::tribble(
    ~owner, ~join_fn, ~key, ~n, ~class, ~why,
    ".add_federation_land_rows", "merge", "iso3c", 1L, "identity_lookup",
    "ISO3 names the territory, not one of its periods; the year rides along in
     the joined land rows.",
    ".add_land_bucket_label", "merge", "area_code", 2L, "identity_lookup",
    "A bucket must carry exactly one label (whep#563); a year-varying label
     would re-split it.",
    ".add_polity_columns_dt", "[", "area_code", 2L, "identity_lookup",
    "The resolver itself: the code join proposes the candidate periods and the
     year predicate that follows disposes of them.",
    ".add_polity_columns_dt", "[", "area_code, <dynamic>", 1L, "identity_lookup",
    "Same resolver, joining back the caller's own rows by row id.",
    ".adjust_food_for_leftovers", "left_join", "area_code, item_cbs_code", 1L,
    "single_year", "`cbs_yr` is one year of the CBS; the IO model is built per
     year.",
    ".allocate_livestock_to_grid", "inner_join", "area_code", 1L, "single_year",
    "Called inside the per-year, per-species-group loop that stamps `year`
     afterwards.",
    ".attach_cbs_area_label", "[", "area_code", 1L, "identity_lookup",
    "Puts a code's ONE display label back on a frame that has already been
     keyed on the code (whep#709). It exists because the label came OUT of the
     pre-1962 year skeleton and the observed-source join, where a second label
     for one code doubled the skeleton rather than only mislabelling it: the
     label's identity role is now this one lookup instead of four keys.",
    ".plu_bind_pasture_backcast", "inner_join", "area_code", 1L, "single_year",
    "Carries the FAO 1961 pasture level onto LUH2's pre-1961 rows so the
     gridded grassland series does not step at FAOSTAT's start. The anchor side
     is filtered to 1961 before joining, so the year is fixed by construction
     rather than missing -- which is what an anchored back-cast is. The LUH2
     side's own 1961 value is a grouped lookup on the same table, not a second
     join.",
    ".attach_mapping_source", "left_join",
    "area_code, polity_code, polity_start_year, polity_end_year", 1L,
    "diagnostic",
    "Reads the provenance label off the crosswalk row the resolver ALREADY
     picked, for `polity_mapping_provenance()` (whep#740). The key is that
     row's own identity -- the area plus the polity PERIOD -- so the year
     scoping happened inside `.add_polity_columns_dt()` one step earlier and
     re-stating it here would constrain nothing: `polity_start_year` and
     `polity_end_year` ARE the years. It moves no published value; it is the
     instrument that measures which authority a published value rests on.",
    ".build_fd_flat", "left_join", "area_code, item_cbs_code", 1L, "single_year",
    "`cbs_yr` is one year of the CBS.",
    ".build_feed_demand_fcr", "left_join", "area_code", 1L, "time_invariant",
    "Bouwman region membership; the published table has no year, the FCRs it
     leads to are joined on (year, region).",
    ".build_feed_demand_head", "left_join", "area_code", 1L, "time_invariant",
    "Bouwman region membership, as above.",
    ".build_feed_mix", "left_join", "area_code", 1L, "time_invariant",
    "Bouwman region membership, as above.",
    ".build_output_vector", "left_join", "area_code, item_cbs_code", 1L,
    "single_year", "`cbs_yr` is one year of the CBS.",
    ".build_sw_domestic", "left_join", "area_code, item_cbs_code", 1L,
    "single_year", "`cbs_yr` is one year of the CBS.",
    ".carbon_warn_fold", "semi_join", "lon, lat, area_code", 1L, "diagnostic",
    "Both sides are the SAME carbon support, already filtered to
     `.carbon_support_year()` by `.carbon_support_at_year()`, so there is no
     second year for a key to disagree about. The join only selects which
     `polity_code`s to name in the DA-23 fold warning; it reaches no value and
     cannot move one.",
    ".cb_apply_equilibrium_climate", "left_join",
    "lon, lat, area_code, land_use", 1L, "time_invariant",
    "The equilibrium modifier is one number per cell and land use by
     construction -- it is what the SOC march starts from, not a year of it.",
    ".cell_polity_to_bucket", "left_join", "area_code", 1L, "identity_lookup",
    "area_code -> polity_area_code is functionally determined: no crosswalk row
     disagrees, so a year predicate cannot change the answer.",
    ".cells_to_polity_area", "left_join", "area_code", 1L, "identity_lookup",
    "area_code -> polity_area_code, as above.",
    ".compute_su_used", "full_join", "area_code, item_cbs_code", 1L,
    "single_year", "Both sides are one year's supply-use.",
    ".dependency_sovereign_iso3", "merge", "polity_code", 1L, "identity_lookup",
    "Keyed on the polity, which is the year-scoped identity itself.",
    ".dependency_sovereign_iso3", "merge", "legacy_polity_prefix", 1L,
    "identity_lookup",
    "ISO3-like stem -> bucket bridge, the same one `.read_fodder_euadb()` uses,
     and the fallback for a dependency upstream has given its own polity so
     that no reporting area shares it (Sint Maarten from the 2026-08-25
     re-sync). The stem is a present-day sovereign, which has no time
     dimension to key on; the dependency's own period is already picked before
     this join.",
    ".energy_co2e_by_group", "inner_join", "area_code", 1L, "identity_lookup",
    "area_code -> ISO3 through the current-area lookup.",
    ".energy_co2e_by_group", "left_join", "iso3, grp", 1L, "time_invariant",
    "GLEAM energy intensities are one published vintage applied to every year;
     `method_energy`/`ef_scope` record which factor a row used.",
    ".energy_dissolved_rows", "inner_join", "polity_area_code", 1L,
    "time_invariant",
    "OECD/EU-27 membership of the dissolved entities, a fixed list.",
    ".energy_intensity_by_country", "full_join", "iso3", 1L, "time_invariant",
    "GLEAM embedded and direct factors, one vintage.",
    ".energy_intensity_by_country", "left_join", "iso3", 1L, "time_invariant",
    "GLEAM scheme scope per country, one vintage.",
    ".energy_join_dressing", "left_join", "iso3", 1L, "time_invariant",
    "GLEAM dressing fractions, one vintage.",
    ".fabio_bridge_fabio_side", "left_join", "iso3c", 1L, "diagnostic",
    "Resolves each FABIO published region to the WHEP bucket carrying its ISO3,
     for `inst/scripts/compare_fabio.R` only (whep#264). ISO3 names the
     territory rather than one of its periods, and FABIO's own region list has
     no time dimension -- it is one published vintage of 192 codes. Nothing
     downstream of it is published: the bridge exists to stop a comparison
     silently matching two different Rest-of-World residuals.",
    ".fold_bucket_labels", "left_join", "bucket_polity_code, polity_code", 1L,
    "identity_lookup", "Polity type keyed on the polity code.",
    ".handed_over_polity_codes", "inner_join", "predecessor, polity_code", 1L,
    "identity_lookup",
    "Succession, from `polities` to itself. The key IS the year-scoped polity
     period -- `ANG-1905-1975` carries its own years -- and the join exists to
     READ a year off it: whether the successor begins where the predecessor
     ends. Adding a `year` would be adding a year to a relation between two
     periods, which has none. This is the one row that RAISED the cap (57 ->
     58 -> 59), and it does so for a join that makes the resolver more
     year-aware, not less: it is what stops `.polity_join_end_year()` widening
     colonial Angola into 1975, the year `AGO-1975-2025` starts (whep#683).",
    ".gn_grass_area", "inner_join", "lon, lat, area_code", 1L, "time_invariant",
    "The country grid is one polity assignment per cell; the land-use side
     carries the year and keeps it.",
    ".infer_target_labels", "left_join", "area_code", 1L, "identity_lookup",
    "Reporting-polity labels for the IO model's final-demand columns.",
    ".interpolate_destiny_shares", "[",
    "area_code, item_cbs, item_cbs_code", 1L, "identity_lookup",
    "The last label-keyed join in the package until whep#691 dropped `area`
     from it. What is left is the skeleton crossing itself: the joined side is
     `ds_keys`, the (year, area_code, item) triples where domestic_supply
     exists, so
     the YEAR comes out of the join rather than being missing from it, and the
     year-free half is the SET of destiny elements the area-item ever reports --
     which is year-free on purpose, because carrying a share into a year that
     did not report it is what the interpolation is for. The label is
     re-attached once from the code afterwards.",
    ".land_in_polygons", "merge", "polity_code", 1L, "time_invariant",
    "A polity code already names its own period (`ETH-1952-1993`), so the
     territory it is joined to cannot vary within it. Since whep#800 that
     territory is the polycell's `polity_area_ha`, and time-invariance is
     MEASURED rather than argued: a maximum relative standard deviation of 0
     over the 33,433 (cell, polity) pairs the shipped support splits into more
     than one interval. This is the join that makes the pre-1962 LAND half
     year-aware at all (whep#761): the caller has already resolved
     (area_code, year) -> polity_code unfloored, and every step after this one
     carries `year`.",
    ".luh2_perennial_backcast", "merge", "area_code", 2L, "single_year",
    "Both joined tables are the anchor year alone; the back-cast rescales the
     pre-anchor years onto it.",
    ".lw_area_regions", "inner_join", "iso3c", 1L, "time_invariant",
    "Gustavsson's Annex 1 is one 2011 snapshot with no time dimension, so the
     region a country's loss rates come from cannot vary by year. The join is
     year-free ON PURPOSE: where one ISO3 spans two WHEP areas across a polity
     split (Ethiopia 238/62, Sudan 276/206) Annex 1's pre-partition entry
     covers both, and a year-keyed join would give the successor no region.",
    ".lw_assign_regions", "left_join", "area_code", 1L, "time_invariant",
    "Attaches that same year-free region to each area, plus the `method_region`
     stamp that says whether it came from Annex 1 or from the global mean.",
    ".lw_weight", "left_join", "area_code", 1L, "time_invariant",
    "Puts the region on the item rows so the wedge can be looked up per region
     and commodity group. Same year-free assignment as above, read once.",
    ".n_country_to_polity", "inner_join", "area_code", 1L, "identity_lookup",
    "area_code -> polity_area_code, checked against the year-aware route over
     the real pins to 0 differences.",
    ".prepare_historical_cbs", "merge", "area_code", 1L, "identity_lookup",
    "Attaches the one label the code carries; the value keeps its own year.",
    ".prepare_historical_production", "merge", "area_code", 1L,
    "identity_lookup", "Attaches the one label the code carries.",
    ".read_fodder_euadb", "left_join", "legacy_polity_prefix, area_iso3c", 1L,
    "identity_lookup",
    "ISO3 -> bucket bridge. It read as a polity join until whep#687 renamed
     `regions_full$polity_code`, which was never a polity code but the vendored
     ISO3-like stem; the key now names both vocabularies it actually bridges.",
    ".read_land_areas", "merge", "iso3c", 1L, "identity_lookup",
    "ISO3 -> bucket bridge; the LUH2 rows keep their year.",
    ".luh2_bridge_iso3c", "merge", "iso3c", 1L, "identity_lookup",
    "ISO3 names the territory, not one of its periods; the year rides on the
     LUH2 rows being bridged. Extracted from `.read_luh2_cft` so the LUH2
     national readers share ONE bridge: this row used to be that function's,
     and the move is why the count did not rise with the pasture reader.",
    ".reconcile_fao_arable_fallow", "merge", "area_code", 1L, "single_year",
    "Inside the per-year fallow attribution loop.",
    ".reconcile_fao_arable_fallow", "merge", "area_code, item_cbs_code", 1L,
    "single_year", "Inside the per-year fallow attribution loop.",
    ".redistribute_countries_dt", "[", "area_code", 2L, "single_year",
    "Per-country subsets inside one year's redistribution.",
    ".resolve_all_area_years", "left_join", "area_code", 1L, "time_invariant",
    "The first year the upstream FAOSTAT map reports each area at all: one
     number per area by construction, and the year bound the predicate right
     after it applies. It is what makes `polity_bucket_coverage()` year-aware,
     and it cannot be year-keyed because the resolver does not carry it --
     `polity_start_year` is the polity's, not the map's (area 276: 2011 against
     2012).",
    ".resolve_hist_trade_polities", "merge", "iso3c", 1L, "identity_lookup",
    "ISO3 -> area bridge, immediately followed by the year-aware polity
     resolution.",
    ".sci_crop_prod_wide", "left_join", "area_code", 1L, "time_invariant",
    "The Krausmann/HANPP/UN sub-region groupings the crop-NPP coefficients are
     published by; none of them varies in time.",
    ".sci_join_weights", "inner_join", "area_code, item_prod_code", 1L,
    "time_invariant",
    "`crop_patterns` is a single-vintage gridded map, applied to every year on
     purpose.",
    ".sci_warn_unspatialized", "anti_join", "area_code, item_prod_code", 1L,
    "diagnostic", "Reports the carbon the join above cannot spatialize.",
    ".select_best_source", "[", "area_code", 1L, "identity_lookup",
    "Re-attaches one label per code after selection, deliberately not keyed on
     the label the sources disagree about.",
    ".spatialize_to_bucket", "[", "area_code", 1L, "identity_lookup",
    "area_code -> polity_area_code, keeping the raw code alongside.",
    ".spatialize_year", "[", "area_code, item_prod_code", 1L, "single_year",
    "One year of the spatialization.",
    ".warn_orphan_land", "anti_join", "item_cbs_code, area_code", 1L,
    "diagnostic",
    "Reports extension rows no production or trade key supports.",
    "align_extension", "right_join", "area_code, item_cbs_code", 1L,
    "single_year", "The extension is filtered to `year` in the line above.",
    "attribute_fallow_to_crops", "left_join", "area_code", 1L, "single_year",
    "Its documented contract is one year's cropgrids, fallow and weights.",
    "attribute_fallow_to_crops", "left_join", "area_code, item_cbs_code", 1L,
    "single_year", "One year's tables, as above.",
    "build_cropgrids_land_extension", "left_join", "area_code, item_cbs_code",
    1L, "time_invariant",
    "The CROPGRIDS physical-to-harvested ratio is one map vintage applied to
     every year; making it vary would need a time series that does not exist.",
    "compare_footprint_methods", "full_join", "area_code, item_cbs_code", 1L,
    "diagnostic",
    "Compares two already-summarised footprints; its documented contract has no
     year column.",
    "prepare_livestock_emissions", "left_join", "area_code", 1L,
    "identity_lookup", "area_code -> ISO3 through the current-area lookup."
  )
}

# Grouping keys ----------------------------------------------------------------
#
# The other half of whep#669, filed as whep#692. whep#669's grep of `by =`
# matched dplyr GROUPING keys as well as join keys, and only the joins were
# classified. The two are different hazards:
#
#   * a JOIN key without `year` reads a value for the wrong period;
#   * a GROUPING key without `year` collapses ACROSS years, summing a 1961 value
#     into a 2023 one -- or, under `mutate()`/`filter()`, computing a window
#     statistic over the whole series where one year was meant.
#
# So this second baseline classifies every year-free territorial GROUP, on the
# same terms: `.territorial_groupings()` enumerates them, the gate in
# `test_join_audit.R` fails on one that is not in this table, and a row that
# stops matching a real grouping fails too, so the list can only shrink.
#
# The four join verdicts carry over, and grouping needs two more, because a
# group can be year-free for reasons a join never is:
#
#   single_year      the call site is already scoped to one year.
#   time_invariant   the grouped table has no time dimension by design.
#   identity_lookup  the group IS the identity resolution -- a code -> bucket
#                    fold, a crosswalk dedup, an ISO3 bridge.
#   diagnostic       a warning, check or comparison; it moves no published
#                    value.
#   year_axis        `year` is deliberately absent because it is the AXIS the
#                    operation runs ALONG or reduces OVER: a first/last year, a
#                    cumulative chain-link, a whole-series total, an all-zero
#                    test. Adding `year` to the key would make every group one
#                    point long and destroy the statistic. This is the class
#                    with no join analogue, and it is why a year-free group is
#                    not simply a defect.
#   row_wise         the expression under `.by` is row-wise, so the group
#                    changes nothing at all.
#
# WHICH GROUPING KEYS ARE ACCEPTABLE is a judgement, not a fact, so it is stated
# here rather than left implicit in a registry: a territorial group may omit
# `year` when (a) the scope is one year already, (b) the table has no year, (c)
# the group is the identity re-keying itself, (d) it feeds a diagnostic, or (e)
# the year axis is what is being reduced. Any other year-free territorial group
# is an oversight, and the gate makes an author say which of the five it is.
# (`row_wise` is the sixth and is not a reason a group may be year-free: it is
# the observation that one particular group is not a group at all.)
#
# Adding a row is not a way to pass the gate: it is a statement, reviewed like
# any other, that the group means the same thing in 1850 and in 2023.
.territorial_grouping_baseline <- function() {
  tibble::tribble(
    ~owner, ~group_fn, ~key, ~n, ~class, ~why,
    ".area_first_reported_year", "summarise", "area_code", 1L, "year_axis",
    "`min(map_year_start)` IS the reduction over the crosswalk's periods: one
     first-reported year per area, the bound that makes
     `polity_bucket_coverage()` year-aware. Keying on the year would return the
     year itself.",
    ".areas_gleam_cannot_group", "distinct",
    "area_code, area_name, area_iso3c, polity_area_code, continent", 1L,
    "diagnostic",
    "Lists the still-open areas GLEAM's country grouping cannot place, for the
     energy-coverage report. The four columns after the code are its
     attributes, carried through so the report can name them.",
    ".bind_area_label_sources", "distinct", "area_code", 1L, "identity_lookup",
    "One row per area_code across the label sources, `.keep_all` so its
     reporting-polity columns ride along. It is the label lookup itself.",
    ".build_trade_matrix", "summarise", "from_code, to_code", 1L, "single_year",
    "`.process_bilateral_trade()` calls it on one element of
     `.nest_by_year_item_code()`, i.e. one (year, item) group, so the year is a
     constant inside the matrix being built.",
    ".carbon_check_support_key", "count", "lon, lat, area_code", 1L,
    "diagnostic",
    "Counts rows per (cell, area_code) and ABORTS on a duplicate, so the carbon
     support cannot fold two polities into one cell silently. The guard, not a
     value.",
    ".carbon_fold_area_code", "summarise", "lon, lat, area_code", 1L,
    "single_year",
    "Folds the polities of one cell into its reporting bucket on a support
     already filtered to `.carbon_support_year()` -- the same frame the
     `.carbon_warn_fold` join row describes.",
    ".carbon_warn_unkeyed", "summarise", "polity_code", 1L, "diagnostic",
    "Ranks the polities named in the DA-23 unkeyed-land warning. It reaches no
     value.",
    ".cb_init_density", "mutate", "lon, lat, area_code", 1L, "single_year",
    "`first` is the earliest year of each cell, selected one step earlier; the
     equilibrium density sums the land-use classes WITHIN that one year.",
    ".cb_initialise", "filter", "lon, lat, area_code", 1L, "year_axis",
    "`year == min(year)` per cell: the group is year-free because picking the
     first year is what it does. A `year` in the key would make every year its
     own minimum and initialise the SOC march from all of them.",
    ".cb_read_cell_polity", "distinct", "lon, lat, area_code", 1L,
    "time_invariant",
    "The distinct cells of the carbon cell support, which is one polity
     assignment per cell with no time dimension.",
    ".cell_polity_bucket_lookup", "distinct",
    "area_code, polity_area_code, <dynamic>", 1L, "identity_lookup",
    "One row per area_code in the polity-area crosswalk, with the ambiguity
     check right after it. The crosswalk has no year.",
    ".cell_polity_to_bucket", "summarise", "lon, lat, area_code", 1L,
    "identity_lookup",
    "The bucket fold itself: border cells holding two areas of one bucket
     (Sudan/South Sudan) re-sum their `polity_frac` and keep both raw codes in
     `grid_area_code`. The polycell support is one polity assignment per cell,
     with no year to collapse.",
    ".cells_to_polity_area", "distinct", "area_code", 1L, "identity_lookup",
    "area_code -> polity_area_code, one row per code; same crosswalk, same
     absence of a year.",
    ".chain_link_land", "[", "area_code, land_use", 2L, "year_axis",
    "The chain-link: after `setorder(area_code, land_use, year)` the group is
     the SERIES and the operation runs along it -- a log ratio between
     consecutive years, then a suffix sum. `year` in the key would give every
     year a series of one and flatten the reconstruction to today's borders,
     which is the defect whep#761 fixed.",
    ".combine_livestock", "mutate",
    "area, area_code, item_cbs, item_cbs_code, Livestock_name", 1L, "row_wise",
    "`if_else(!is.na(value_comb), value_comb, if_else(n > 40, NA, 0))` is
     row-wise -- `n` was computed by the grouped `mutate()` above -- so this
     group aggregates nothing. The label rides beside its code.",
    ".combine_livestock", "mutate", "area_code, item_cbs, item_cbs_code", 1L,
    "year_axis",
    "`n = n()` counts how many YEARS the series has, which is the test the line
     below applies (`n > 40`). The count is the reduction over the year axis.",
    ".compute_su_used", "summarise", "area_code, item_cbs_code", 2L,
    "single_year",
    "`su` is one year's supply-use; the IO model is built per year, as the
     matching join row says.",
    ".cropgrids_to_polity_area", "summarise", "area_code, item_cbs_code", 1L,
    "identity_lookup",
    "The bucket fold of the CROPGRIDS map. `add_polity_code(year_column =
     NULL)` is deliberate: CROPGRIDS is ONE map vintage applied to every year,
     so there is no year to resolve against and nothing to collapse.",
    ".energy_area_iso3", "distinct", "area_code, iso3, area_iso3c", 1L,
    "identity_lookup", "area_code -> ISO3, one row per pair, off the energy
     crosswalk.",
    ".energy_dissolved_areas", "summarise",
    "area_code, area_name, area_iso3c, polity_area_code, continent", 1L,
    "year_axis",
    "`max(polity_end_year)` per area: the LAST year each area reports for
     itself, compared with the crosswalk's open end to decide which areas are
     dissolved. The other four columns are attributes of `area_code`, carried
     through rather than keys.",
    ".energy_intensity_by_country", "distinct", "iso3, ef_scope", 1L,
    "time_invariant",
    "Which GLEAM scheme scope each country's factors came from. One published
     vintage, as the matching join rows say.",
    ".energy_unpriced_summary", "summarise", "area_code, iso3", 1L,
    "diagnostic",
    "Ranks the carcass tonnage the GLEAM price scope cannot cover, for the
     unpriced-share warning.",
    ".fabio_bridge_fabio_side", "distinct", "area_code, iso3c, <dynamic>", 1L,
    "diagnostic",
    "De-duplicates FABIO's published region list to one row per (code, ISO3)
     before it is used as a comparison key (whep#264). The list is a single
     vintage with no year to group by.",
    ".fabio_bridge_fabio_side", "distinct",
    "iso3c, whep_bucket, area_iso3c, <dynamic>", 1L, "diagnostic",
    "The ISO3-to-bucket side of the same bridge. It deliberately collapses the
     crosswalk's polity PERIODS, because the question is which bucket carries a
     territory at all, not which period it reported in; a year here would
     multiply one FABIO region into one row per period and abort the
     `many-to-one` join that follows.",
    ".fabio_bridge_whep_buckets", "distinct", "area_code, bucket, <dynamic>",
    1L, "diagnostic",
    "The WHEP half of the same bridge: which bucket an area's rows are summed
     into. Year-free for the same reason the FABIO half is -- the fold is a
     property of the area, not of a year -- and it is what stops reporting area
     276 being keyed apart from the 206 bucket FABIO's own 276 resolves to.",
    ".fabio_bridge_whep_buckets", "count", "area_code", 1L, "diagnostic",
    "The guard on that lookup: aborts if one area folds into two buckets, which
     would make the comparison key ambiguous. Counting the rows per area is
     the check, so a year in the key would defeat it.",
    ".fao_area_iso3_lookup", "distinct",
    "fao_area_name, iso3_code, area_name, area_iso3c", 1L, "identity_lookup",
    "The FAOSTAT area name -> ISO3 lookup `get_faostat_data()` resolves
     `ISO3_CODE` through (whep#541). It is the identity re-keying itself: one
     row per FAOSTAT area name, off a crosswalk whose `area_iso3c` is a
     property of the area and not of a year. Collapsing the crosswalk's polity
     periods is the point -- an area has one ISO3 code across all of them, and
     a year in the key would return one row per period for the same name and
     make the `match()` below pick whichever came first.",
    ".federation_land_bridge", "[", "area_code, area", 1L, "identity_lookup",
    "Expands each dissolved polity to its successor ISO3 codes off
     `.current_area_lookup()`, which has no year. The label is one per code
     here (the lookup is already `unique(by = \"area_code\")`) and is carried,
     not keyed on alone.",
    ".feed_region_lookup", "distinct", "area_code", 1L, "time_invariant",
    "Bouwman region membership, one row per area; the published table has no
     year and the FCRs it leads to are joined on (year, region).",
    ".iso3_area_code_bridge", "[", "iso3c", 1L, "identity_lookup",
    "Picks the canonical FAOSTAT area for each ISO3, and aborts rather than let
     row order decide when the rule leaves two. A year cannot break the tie:
     the crosswalk has none.",
    ".iso3_polity_spans", "distinct",
    "area_iso3c, polity_code, polity_start_year, polity_end_year", 1L,
    "identity_lookup",
    "The key IS the polity PERIOD -- `polity_start_year` and `polity_end_year`
     are the years -- and the spans it dedups are what the year predicate is
     then applied to.",
    ".iso3c_area_code_lookup", "distinct", "iso3c, area_code, <dynamic>", 1L,
    "identity_lookup", "ISO3 -> bucket, one row per pair, off `regions_full`.",
    ".label_reporting_polity_lookup", "distinct", "area_code", 1L,
    "identity_lookup",
    "One row per area_code with its reporting-polity columns; the same lookup
     as `.bind_area_label_sources()`, for a single frame.",
    ".land_balance_production", "summarise", "area_code, item_cbs_code", 1L,
    "single_year",
    "`get_primary_production()` is filtered to `.env$year` in the same pipe.",
    ".land_in_polygons", "[", "area_code, land_use", 1L, "single_year",
    "Sums gridded land into buckets for ONE year: `.measure_land_year()` passes
     `polity_areas[year == yr]`, so the polygons are the ones live that year
     and the sum is within it.",
    ".lw_area_regions", "distinct", "iso3c, area_code, <dynamic>", 1L,
    "identity_lookup",
    "ISO3 -> area bridge for Gustavsson's Annex 1 regions; the snapshot it
     serves has no time dimension at all (see the matching join row).",
    ".lw_assign_regions", "distinct", "area_code", 1L, "time_invariant",
    "The set of areas needing a loss region. Gustavsson's Annex 1 is a single
     2011 snapshot with no time dimension, so this cannot be year-keyed without
     leaving every successor area unregioned (see the matching join row).",
    ".lw_check_one_region", "summarise", "area_code", 1L, "diagnostic",
    "Counts regions per area and aborts on more than one. It is the guard, not
     a value.",
    ".missing_denominator_areas", "summarise", "area_code", 1L, "diagnostic",
    "Totals the mass and the area-year count that have no population
     denominator, for the warning that names them.",
    ".n_check_area_key", "count", "lon, lat, area_code", 1L, "diagnostic",
    "The same duplicate-cell guard as the carbon one, for the gridded nitrogen
     support; it aborts rather than fold two polities into one cell.",
    ".n_country_to_polity", "distinct",
    "area_code, polity_area_code, <dynamic>", 1L, "identity_lookup",
    "One row per (area_code, polity_area_code) off the crosswalk; the summarise
     that follows it DOES carry `year`.",
    ".nd_check_area_key", "count", "lon, lat, area_code", 1L, "diagnostic",
    "The same guard again, for the deposition support.",
    ".pcs_abort_interval_overlap", "mutate", "cell_id, polity_code", 1L,
    "year_axis",
    "`lag(start_year)` / `lag(end_year)` over the intervals of one polity in
     one cell IS the reduction over the year axis: the group has to hold the
     whole interval sequence for the previous interval to exist. Keying on a
     year would compare each interval with itself.",
    ".pcs_area_code", "distinct", "polity_code", 1L, "identity_lookup",
    "polity_code -> polity_area_code, one row per polity, for the polycell
     support's area key.",
    ".pcs_area_code", "distinct", "polity_code, polity_area_code", 1L,
    "identity_lookup", "The inner dedup of the same crosswalk pair.",
    ".pcs_footprint_diff", "distinct", "lon, lat, area_code", 1L, "diagnostic",
    "The cell footprint of each crosswalk source, compared to report where they
     disagree. It moves no value.",
    ".pcs_polycell_footprint", "distinct", "lon, lat, area_code", 1L,
    "single_year",
    "The support is subset to the rows covering `data$crosswalk_year` in the
     same call, so the cells are one year's.",
    ".plu_bind_pasture_backcast", "mutate", "area_code", 1L, "year_axis",
    "`luh2_ha[match(anchor_year, year)]` reads the anchor year's value out of
     the series, which needs the whole series in the group. The anchor is 1961
     by construction, so this is the fixed-year lookup the back-cast rescales
     onto, not a collapse.",
    ".polity_cell_cover", "[", "polity_code, lon, lat", 1L, "time_invariant",
    "`max(polity_area_ha)` over the successive INTERVALS of one polycell, which
     repeat the same geometry -- measured at a maximum relative standard
     deviation of 0 in whep#800. `max()` rather than `unique()` so a polity
     cannot be emitted twice in one cell and double its weight.",
    ".polity_type_lookup", "distinct",
    "polity_code, bucket_polity_type, polity_type", 1L, "identity_lookup",
    "Polity type keyed on the polity code, which already names its own
     period.",
    ".pop_folded_buckets", "summarise", "area_code", 1L, "diagnostic",
    "Counts the ISO3 cells folded into each bucket, to label them in the
     population fold report.",
    ".read_fodder_euadb", "distinct", "area_iso3c", 1L, "identity_lookup",
    "ISO3 -> bucket bridge, one row per ISO3; the fodder rows keep their own
     year.",
    ".reporting_periods", "summarise", "area_code, polity_code", 1L,
    "year_axis",
    "`min(map_year_start)` and `max(map_year_end)` ARE the reduction over the
     crosswalk's rows for one period: the output is that period's reporting
     span, so keying on the year would return the year itself. The period is
     already the year-scoped identity.",
    ".sci_crop_regions", "distinct", "area_code", 1L, "time_invariant",
    "The Krausmann/HANPP/UN sub-region groupings the crop-NPP coefficients are
     published by; none of them varies in time.",
    ".sci_grid_weights", "mutate", "area_code, item_prod_code", 1L,
    "time_invariant",
    "Renormalises cell crop area within (area, crop). `crop_patterns` is a
     single-vintage gridded map applied to every year on purpose -- the same
     source the `.sci_join_weights` join row rests on -- so the frame has no
     year to collapse.",
    ".sci_warn_unspatialized", "distinct", "area_code, item_prod_code", 1L,
    "diagnostic",
    "The (area, crop) pairs the crop-pattern weights cover, so the warning can
     name the carbon they cannot spatialize.",
    ".spatialize_year", "[", "area_code, item_prod_code", 2L, "single_year",
    "Both are inside `.spatialize_year(yr, ...)`, which stamps `year = yr` at
     the end.",
    ".summarise_folded_rows", "[", "area_code, polity_area_code, <dynamic>", 1L,
    "diagnostic",
    "Counts the rows each area folds into its bucket, for the fold warning's
     label. `area_name` joins the key only when the frame carries it, which is
     why the third element reads `<dynamic>`.",
    ".suppress_empty_series", "mutate",
    "area, area_code, item_prod, item_prod_code", 1L, "year_axis",
    "`sum(t) + sum(fu)` over the WHOLE series, to drop series that are zero
     everywhere. A year in the key would test each year separately and delete
     every legitimate zero year. The label rides beside its code.",
    ".trade_matrix", "summarise", "from_code, to_code", 1L, "single_year",
    "`compute_footprint_balance()` runs per item within one year;
     `.land_balance_trade(year)` supplies the rows.",
    ".warn_bucket_coverage", "summarise",
    "polity_area_code, member_polity_codes, bucket_polity_code, coverage", 1L,
    "diagnostic",
    "`paste(min(year), max(year))` is the year RANGE it reports -- the group
     spans years so the warning can say which, and it moves no value.",
    ".warn_orphan_land", "distinct", "area_code, item_cbs_code, from_code", 1L,
    "diagnostic", "Builds the (item, area) support set the orphan-land warning
     compares against.",
    ".warn_orphan_land", "distinct", "area_code, item_cbs_code, to_code", 1L,
    "diagnostic", "The import side of the same support set.",
    ".warn_orphan_land", "distinct", "item_cbs_code, area_code", 1L,
    "diagnostic",
    "The production side of the same support set; all three are one year's
     footprint inputs and none of them reaches a value.",
    ".warn_unallocated_crops", "[", "area_code, item_prod_code", 1L,
    "diagnostic",
    "Reports the (country, crop) pairs with national area but no allocatable
     cell, inside one year of the spatialization.",
    ".weight_supply_by_value", "mutate", "area_code, proc_group, proc_cbs_code",
    1L, "single_year",
    "`all(price_ok | type != \"supply\")` over one year's supply-use, so a
     process group is value-weighted only when every one of its supplies has a
     price.",
    ".zero_proxy_land_areas", "summarise", "area_key, area, proxy_class", 1L,
    "diagnostic",
    "`all(zero_proxy)` over the pre-1962 series, for the warning that names the
     areas whose LUH2 land proxy is zero in every year. It is also the one
     year-free group in the package whose key names the `area` LABEL without a
     code the audit can SEE: `area_key` holds whichever column the caller has,
     and `.fill_pre_faostat()` falls back to `\"area\"` when the LUH2 land
     table carries no `area_code`. That fallback is whep#584's name-vocabulary
     problem, not something this gate can fix; it is recorded here so it stays
     visible.",
    "align_extension", "summarise", "area_code, item_cbs_code", 1L,
    "single_year", "The extension is filtered to `.env$year` in the same pipe.",
    "build_grazing_feed_footprint", "summarise", "area_code, item_cbs_code", 1L,
    "single_year", "`production` is `.grazing_production(year)`.",
    "compare_footprint_methods", "summarise", "area_code, item_cbs_code", 2L,
    "diagnostic",
    "Totals each method before differencing them; its documented contract has
     no year column, as the matching join row says.",
    "folded_reporting_areas", "distinct",
    paste0(
      "area_code, area_name, area_iso3c, polity_area_code, ",
      "polity_code, polity_name, fold_kind"
    ),
    1L, "diagnostic",
    "One row per (area, bucket, polity) fold for the fold report; the polity
     code already names its own period, and the report is a description of the
     crosswalk rather than a value read out of it.",
    "gridded_fallow_weights", "summarise", "area_code, item_cbs_code", 1L,
    "single_year",
    "Its documented `gridded_crops` contract is `lon`, `lat`, `area_code`,
     `item_cbs_code`, `rainfed_ha` -- one year's grid in, one weight table out,
     called per year by `attribute_fallow_to_crops()`.",
    "prepare_livestock_emissions", "distinct", "area_code", 1L,
    "identity_lookup",
    "area_code -> ISO3 through the current-area lookup, one row per code -- the
     dedup behind the join row of the same name.",
    "row_promotion_status", "summarise", "area_code, area_name, area_iso3c", 1L,
    "diagnostic",
    "`n_periods = n()` over an area's crosswalk periods, for the promotion
     report. The two name columns are attributes of the code, carried through."
  )
}

.territorial_key_cols <- function() {
  c(
    "area",
    "area_code",
    "area_name",
    "area_iso3c",
    "polity_area_code",
    "polity_code",
    "reporting_polity_code",
    # Not a polity code: `regions_full`'s vendored ISO3-like stem (whep#687).
    # Listed so renaming a column cannot be a way out of the audit.
    "legacy_polity_prefix",
    "grid_area_code",
    "iso3",
    "iso3c",
    "territory",
    "from_code",
    "to_code"
  )
}

# One row per join call in `ns` whose key mentions a territory column.
.territorial_joins <- function(ns = asNamespace("whep")) {
  .audit_key_calls(ns, .join_record) |>
    dplyr::rename(join_fn = "key_fn")
}

# One row per grouping call in `ns` whose key mentions a territory column. The
# verb is kept under its own name rather than the join detector's `join_fn`, so
# a baseline row cannot be moved between the two tables by accident.
.territorial_groupings <- function(ns = asNamespace("whep")) {
  .audit_key_calls(ns, .grouping_record) |>
    dplyr::rename(group_fn = "key_fn")
}

# Walk every function in `ns` with `recorder`, which returns zero or one row per
# call node. Both detectors share this and emit the same columns, so the two
# gates in `test_join_audit.R` read the same way.
.audit_key_calls <- function(ns, recorder) {
  out <- unlist(
    lapply(ls(ns, all.names = TRUE), function(nm) {
      .keys_in_object(ns, nm, recorder)
    }),
    recursive = FALSE
  )
  if (length(out) == 0L) {
    return(.empty_key_audit())
  }
  out <- dplyr::bind_rows(out)
  out |>
    dplyr::filter(.data$is_territorial) |>
    dplyr::arrange(.data$owner, .data$key_fn, .data$key) |>
    dplyr::select("owner", "key_fn", "key", "has_year", "has_label", "has_code")
}

.empty_key_audit <- function() {
  tibble::tibble(
    owner = character(),
    key_fn = character(),
    key = character(),
    has_year = logical(),
    has_label = logical(),
    has_code = logical()
  )
}

.keys_in_object <- function(ns, nm, recorder) {
  obj <- tryCatch(get(nm, envir = ns), error = function(e) NULL)
  if (!is.function(obj)) {
    return(list())
  }
  .walk_for_keys(body(obj), nm, recorder)
}

# Depth-first walk of one function body, collecting keyed calls. `lapply()` and
# `unlist()` rather than purrr here: this runs once per call node of the whole
# package, hundreds of thousands of them, where the per-call overhead shows.
.walk_for_keys <- function(e, owner, recorder) {
  if (!is.call(e)) {
    return(list())
  }
  here <- recorder(e, owner)
  kids <- unlist(
    lapply(as.list(e), function(x) {
      if (is.call(x)) .walk_for_keys(x, owner, recorder) else list()
    }),
    recursive = FALSE
  )
  c(here, kids)
}

# One record, shared by both detectors so a join key and a grouping key are
# described in the same vocabulary.
.key_record <- function(keys, owner, key_fn) {
  cols <- .territorial_key_cols()
  labels <- .territorial_label_cols()
  list(list(
    owner = owner,
    key_fn = key_fn,
    key = paste(keys, collapse = ", "),
    is_territorial = any(keys %in% cols),
    has_year = "year" %in% keys,
    has_label = any(keys %in% labels),
    has_code = any(keys %in% setdiff(cols, labels))
  ))
}

# The two columns that are a DISPLAY name rather than a key. Keying on one is
# the shape behind whep#589 and whep#563.
.territorial_label_cols <- function() {
  c("area", "area_name")
}

# A join call is `*_join()`, `merge()`, or `x[i, on = ]` (data.table).
#
# WHAT THIS DOES NOT SEE, stated so the gate is not read as stronger than it is:
# a key computed at run time. `by = key_cols` resolves to `<dynamic>` and, with
# nothing territorial written in it, is not audited. There are 55 such joins,
# nearly all of them generic helpers parameterised by their caller's key, so
# enumerating them would say nothing about territory. A literal territorial key
# is what a new join normally writes, and that is what is gated.
.join_record <- function(e, owner) {
  fname <- .call_name(e)
  key_arg <- .join_key_arg(e, fname)
  if (is.null(key_arg)) {
    return(list())
  }
  .key_record(.static_key_cols(key_arg), owner, fname)
}

# A grouping call is a dplyr verb with `.by = `, a verb whose positional
# arguments ARE the group (`distinct()`, `count()`, `group_by()`), or
# `x[, j, by = ]` (data.table).
#
# WHAT THIS DOES NOT SEE, on top of the computed keys the join detector already
# misses: the `.by` of whep's own series helpers -- `fill_linear()`,
# `fill_sum()`, `fill_proxy_growth()`, `consolidate_sources()`,
# `check_series_jumps()`, `calculate_lmdi()`, `decompose_weighted_ratio()`. They
# are left out on purpose, not overlooked. Each takes the time axis as its OWN
# argument (`time_col = year`), so its `.by` is the series identity and being
# year-free is the contract rather than a hazard: putting `year` in it would
# make every group one point long and stop the interpolation working.
.grouping_record <- function(e, owner) {
  keys <- .grouping_key_cols(e)
  if (is.null(keys)) {
    return(list())
  }
  .key_record(keys, owner, .call_name(e))
}

.call_name <- function(e) {
  fn <- e[[1L]]
  if (is.name(fn)) {
    return(as.character(fn))
  }
  if (is.call(fn) && identical(as.character(fn[[1L]]), "::")) {
    return(as.character(fn[[3L]]))
  }
  ""
}

.join_key_arg <- function(e, fname) {
  nms <- names(e)
  arg <- if (grepl("_join$", fname) || identical(fname, "merge")) {
    "by"
  } else if (identical(fname, "[")) {
    "on"
  } else {
    return(NULL)
  }
  if (is.null(nms) || !arg %in% nms) {
    return(NULL)
  }
  e[[which(nms == arg)[1L]]]
}

# dplyr verbs whose `.by = ` is a grouping key. `mutate()` and `filter()` are in
# the list beside `summarise()`: they do not collapse rows, but a window
# statistic over a year-free group reads the whole series just as a summarise
# would, which is the same hazard.
.dot_by_verbs <- function() {
  c(
    "summarise",
    "summarize",
    "mutate",
    "filter",
    "reframe",
    "count",
    "add_count",
    "tally",
    "add_tally",
    "slice",
    "slice_head",
    "slice_tail",
    "slice_min",
    "slice_max",
    "slice_sample"
  )
}

# Verbs whose positional arguments are themselves the group.
.positional_group_verbs <- function() {
  c("distinct", "count", "add_count", "group_by")
}

# Arguments of those verbs that are options rather than keys.
.non_key_args <- function() {
  c("wt", "sort", "name", ".drop", ".keep_all", ".add", ".by")
}

.grouping_key_cols <- function(e) {
  fname <- .call_name(e)
  nms <- names(e)
  has <- function(a) !is.null(nms) && a %in% nms
  if (fname %in% .dot_by_verbs() && has(".by")) {
    return(.static_key_cols(e[[which(nms == ".by")[1L]]]))
  }
  if (identical(fname, "[") && (has("by") || has("keyby"))) {
    return(.static_key_cols(e[[which(nms %in% c("by", "keyby"))[1L]]]))
  }
  if (fname %in% .positional_group_verbs()) {
    return(.positional_key_cols(e))
  }
  NULL
}

# The positional keys of `distinct(data, a, b)` and friends: every argument but
# the data and the verb's own options.
.positional_key_cols <- function(e) {
  parts <- as.list(e)[-1L]
  nms <- names(parts) %||% rep("", length(parts))
  parts <- parts[!nms %in% .non_key_args()]
  if (length(parts) < 2L) {
    return(NULL)
  }
  .static_key_cols(as.call(c(quote(c), parts[-1L])))
}

# The column names a `by = ` / `on = ` argument names, as written. `c("a", "b")`
# and `.(a, b)` resolve; `c(x = "y")` contributes both sides, so a renaming join
# is audited on both vocabularies; anything computed resolves to `<dynamic>`.
.static_key_cols <- function(arg) {
  if (is.character(arg)) {
    return(unique(c(names(arg), unname(arg))))
  }
  if (!is.call(arg)) {
    return("<dynamic>")
  }
  head_name <- .call_name(arg)
  if (!head_name %in% c("c", ".", "list", "join_by")) {
    return("<dynamic>")
  }
  parts <- as.list(arg)[-1L]
  vals <- parts |>
    purrr::map(.key_col_names) |>
    purrr::list_c()
  # `setdiff(, "")`: a partly named `c(a, iso3 = "b")` has an empty name for the
  # unnamed half, and an empty string is not a column.
  unique(setdiff(c(names(parts), vals), ""))
}

.key_col_names <- function(x) {
  if (is.character(x)) {
    return(as.character(x))
  }
  if (is.name(x)) {
    return(as.character(x))
  }
  if (is.call(x) && .call_name(x) %in% c("==", "=")) {
    return(c(.key_col_names(x[[2L]]), .key_col_names(x[[3L]])))
  }
  # `.data$area_code` names a column as plainly as `area_code` does, and
  # grouping keys are written that way far more often than join keys are. Not
  # resolving it would let `distinct(x, .data$area_code)` out of the audit.
  if (is.call(x) && identical(.call_name(x), "$")) {
    if (identical(x[[2L]], quote(.data)) && is.name(x[[3L]])) {
      return(as.character(x[[3L]]))
    }
  }
  "<dynamic>"
}
