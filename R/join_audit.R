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
  out <- unlist(
    lapply(ls(ns, all.names = TRUE), function(nm) .joins_in_object(ns, nm)),
    recursive = FALSE
  )
  if (length(out) == 0L) {
    return(.empty_join_audit())
  }
  out <- dplyr::bind_rows(out)
  out |>
    dplyr::filter(.data$is_territorial) |>
    dplyr::arrange(.data$owner, .data$join_fn, .data$key) |>
    dplyr::select("owner", "join_fn", "key", "has_year", "has_label")
}

.empty_join_audit <- function() {
  tibble::tibble(
    owner = character(),
    join_fn = character(),
    key = character(),
    has_year = logical(),
    has_label = logical()
  )
}

.joins_in_object <- function(ns, nm) {
  obj <- tryCatch(get(nm, envir = ns), error = function(e) NULL)
  if (!is.function(obj)) {
    return(list())
  }
  .walk_for_joins(body(obj), nm)
}

# Depth-first walk of one function body, collecting join calls. `lapply()` and
# `unlist()` rather than purrr here: this runs once per call node of the whole
# package, hundreds of thousands of them, where the per-call overhead shows.
.walk_for_joins <- function(e, owner) {
  if (!is.call(e)) {
    return(list())
  }
  here <- .join_record(e, owner)
  kids <- unlist(
    lapply(as.list(e), function(x) {
      if (is.call(x)) .walk_for_joins(x, owner) else list()
    }),
    recursive = FALSE
  )
  c(here, kids)
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
  keys <- .static_key_cols(key_arg)
  cols <- .territorial_key_cols()
  list(list(
    owner = owner,
    join_fn = fname,
    key = paste(keys, collapse = ", "),
    is_territorial = any(keys %in% cols),
    has_year = "year" %in% keys,
    has_label = any(keys %in% c("area", "area_name"))
  ))
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
  unique(c(names(parts), vals))
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
  "<dynamic>"
}
