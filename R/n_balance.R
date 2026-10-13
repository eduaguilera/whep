# Full nitrogen balance assembler (Module C, Task C7), ported from the balance
# and indicator equations of an earlier regional historical reconstruction.
# Combines the gridded N-input assembly (build_n_inputs(),
# R/n_balance_inputs.R) with the output side (production, residues, grazed
# weeds, soil organic-matter change) and the loss cascade
# (calculate_nh3()/calculate_soil_n2o()/
# calculate_n_leaching()/calculate_indirect_n2o_nh3(), R/n_balance_
# losses.R) into one balance-closing tibble with NUE indicators, an
# N-limitation cap on SOM sequestration and a GWP/CO2e indicator.
#
# The loss cascade (calculate_nh3()/calculate_soil_n2o()/
# calculate_indirect_n2o_nh3()) is run at the (key, fert_type) grain
# directly on build_n_inputs()'s long-format rows, per the task's "build x
# from the assembled N_input_for_N2O_MgN-style rows" instruction, then
# summed back to the balance key (year/area_code/item_cbs_code, plus
# lon/lat at resolution = "grid"). calculate_n_leaching() instead runs once
# per balance key (n_surplus_t is a balance-level quantity, not a
# per-fert_type one).
#
# "Agro_industry" and "accum_loss"/"Accum_gain_AG_MgN"/"Accum_gain_BG_MgN"
# (perennial-crop standing-biomass N accumulation, input AND output side)
# are documented gaps carried over unchanged from build_n_inputs(): their
# source computations were not available for this task, so they contribute
# zero rather than being fabricated. Both sides are treated consistently
# (neither is silently included while the other is skipped).
#
# fert_type vocabulary bridge: build_n_inputs() emits lowercase snake_case
# fert_type values ("bnf", "recycling", "manure_solid", "manure_liquid",
# "excreta", "deposition", "human", "som_mineralization", "synthetic"), but
# n_balance_losses.R's coefficient tables (fertiliser_n2o_modifiers,
# subsoil_no3_reduction) are keyed on a DIFFERENT Title-case vocabulary
# ("Synthetic", "Solid", "Liquid", "Recycling", "Excreta_cattle_monog",
# "Excreta_other", "SOM", "Human", "Deposition", "BNF") that additionally
# splits excreta by species. build_n_inputs() pools a cell's excreta into one
# row and carries its species mix beside it as `excreta_cpp_share`, the share
# of its N deposited by cattle, poultry and pigs (R/excreta_species_mix.R).
# .nb_loss_fert_type() therefore maps "excreta" to "Excreta", a label for
# excreta whose species are in that share rather than in the label, and
# .nb_loss_rows() carries the share (N-weighted) to the loss rows. The loss
# methods price it there: IPCC 2019 as the EF3PRP,CPP/EF3PRP,SO blend, and
# aguilera as the share-weighted blend of the Excreta_cattle_monog and
# Excreta_other modifiers. Every other term is a direct 1:1 rename.
# N_input_for_N2O_MgN's rows (Excreta, Liquid, Solid, SOM, Synthetic, Human,
# Recycling) are exactly the fert_types that resolve to a real
# fertiliser_n2o_modifiers row; "bnf" and "deposition" are excluded from
# that sum and therefore never reach calculate_soil_n2o()/calculate_nh3()
# here, consistent with the verified N_input_for_N2O_MgN formula.

#' Build the full nitrogen balance: inputs, outputs, losses and NUE.
#'
#' @description
#' Assembles [build_n_inputs()]'s long-format nitrogen inputs into the
#' aggregate `N_input_*` sums, adds the output side (crop production,
#' residue use/burning, grazed weeds, soil organic-matter sequestration),
#' runs the nitrogen-loss cascade ([calculate_nh3()], [calculate_soil_n2o()],
#' [calculate_n_leaching()], [calculate_indirect_n2o_nh3()]), closes the
#' balance (`N_input_full - N_output_full`), applies the N-limitation cap on
#' SOM sequestration, and derives nutrient-use-efficiency (NUE) indicators
#' plus a GWP/CO2e indicator for the nitrous-oxide streams. Ported from the
#' balance and indicator equations of an earlier regional historical
#' reconstruction.
#'
#' @param methods A named list of method choices: `nh3` (forwarded to
#'   [calculate_nh3()], default `"manner"`), `n2o` (forwarded to
#'   [calculate_soil_n2o()], default `"ipcc2019"`, the IPCC 2019 Tier 1
#'   wet/dry factors of Table 11.1 (Updated) p. 11.13; `"ipcc2019_atl_med"`,
#'   `"aguilera"` and `"ipcc2006"` are selectable), `n2o_indirect`
#'   (forwarded to [calculate_indirect_n2o_nh3()], default `"ipcc2019"`, the
#'   wet/dry EF4 of Table 11.3 (Updated) p. 11.26, or `"ipcc2019_atl_med"`),
#'   `leaching` (forwarded to [calculate_n_leaching()], default
#'   `"meisinger_drainage"`; `"ipcc_fracleach"`, the wet/dry
#'   FracLEACH-(H) of Section 11.2.2.2 p. 11.25; or `"image_gnm"`, the
#'   IMAGE-GNM root zone of Beusen et al. 2015, eqs. 10-14 p. 4052),
#'   `leaching_transport` and
#'   `leaching_none_class` (forwarded to [calculate_n_leaching()] as
#'   `transport`, default `"swat_mixing"`, and `none_class`, default
#'   `"retention"`; read only under `"meisinger_drainage"`, which threads
#'   the retained nitrate through the years of the balance on each key
#'   without its year) and `regime`, how each grid
#'   row is split into a rainfed and an irrigated part (see Details):
#'   `"yield_split"` (default), `"area_split"` or `"none"`. The IPCC 2019
#'   wet/dry methods read `moisture_regime` from the driver tables in `data`.
#' @param resolution `"grid"` (default) or `"polity"`, as in
#'   [build_n_inputs()]; `"polity"` sums every term (and re-derives every
#'   indicator) over cells.
#' @inheritParams build_water_balance
#' @param data Named list of pre-loaded upstream inputs. `n_inputs`
#'   ([build_n_inputs()]'s output) is used directly when supplied, else
#'   [build_n_inputs()] is called with this SAME `data` list (so its nested
#'   readers, e.g. `bnf_input`/`npp_n_input`/`livestock_intake`, propagate
#'   through). Also required:
#'   * `npp_n_input`: shared with [build_n_inputs()]'s `"recycling"` term
#'     (see [calculate_npp_carbon_nitrogen()]); used here for `prod_n_t`.
#'   * `ag_land_support`: the [build_ag_land_support()] table, supplying
#'     `area_ha` (the per-hectare boundary denominator) on a physical-land
#'     basis. Derived natively when absent and derivable, and shared with
#'     [build_n_inputs()] so both see one table; when neither supplied nor
#'     derivable, `area_ha` falls back to the harvested area the NPP input
#'     carries.
#'   * `residue_destiny_input`: [calculate_residue_destinies()]'s required
#'     input (`item_prod_code`, `residue_dm_t`, plus whatever the chosen
#'     `residue_destiny_method` needs), for `used_residue_n_t`/
#'     `bedding_residue_n_t`/`burnt_residue_n_t`. `residue_destiny_method`
#'     selects the method (default `"recovery_regional"`) and
#'     `residue_bedding_fraction` the share of the recovered non-feed residue
#'     used as bedding (default `0`; see [calculate_residue_destinies()]).
#'     The bedded straw also comes back as a manure input, through
#'     [build_n_inputs()].
#'   * `livestock_intake`: shared with [build_n_inputs()]'s manure term;
#'     its `"grass"` `feed_quality` rows drive `grazed_weeds_n_t`.
#'   * `carbon_balance`: shared with [build_n_inputs()]'s `"som_
#'     mineralization"` term; its `son_change_kgn_ha` drives `som_
#'     sequestration_n_t`.
#'   * `n_balance_drivers` and `n_balance_leaching_drivers`: the loss
#'     drivers. At grid resolution each one left `NULL` is built by
#'     [build_n_loss_drivers()] on the split balance's own rows (the IPCC 2019
#'     wet/dry class and the ATL/MED drivers mapped from it; see its table of
#'     which methods it supplies), and `method_drivers` says so. That needs
#'     the rainfed/irrigated split (`methods$regime` other than `"none"`).
#'     The leaching drivers are built for `methods$leaching`; the
#'     Meisinger and IMAGE-GNM ones read the balance's own
#'     `data$carbon_balance`. The MANNER `nh3` methods read drivers it does
#'     not supply and are refused before any input is assembled unless their
#'     table is supplied. At polity resolution both must be supplied.
#'   * `n_loss_driver_inputs`: optional list passed as `data` to
#'     [build_n_loss_drivers()] when it runs (`climate`, `crop_depth`,
#'     `lpjml_years`, `lpjml_grid`, `regime_areas`, `moisture_regime`, and
#'     the leaching layers it lists); the balance's own `country_grid` and
#'     `carbon_balance` are shared with it. `n_loss_driver_methods`: optional
#'     list of its `methods` (`som_source`, `som_factor`, `root_depth`); its
#'     `leaching` is always `methods$leaching`.
#'   * `n_balance_drivers`: additional per-`(key, fert_type)` driver
#'     columns [calculate_nh3()]/[calculate_soil_n2o()]/
#'     [calculate_indirect_n2o_nh3()] need beyond `n_input_t`/`fert_type`
#'     (`moisture_regime` for the IPCC 2019 wet/dry methods, `climate` and
#'     `irrig_type` for the ATL/MED ones, plus every MANNER driver column
#'     when `methods$nh3 == "manner"`), joined by the balance key AND
#'     `fert_type` (the Title-case vocabulary of the loss functions:
#'     `"Synthetic"`, `"Solid"`, `"Liquid"`, `"Excreta"`, `"Human"`,
#'     `"Recycling"`, `"SOM"`). The excreta rows' species mix,
#'     `excreta_cpp_share`, comes from [build_n_inputs()], N-weighted over
#'     the rows pooled into each loss row. A second set of
#'     balance-key-only driver columns (`moisture_regime`, `fert_type`,
#'     `climate`, `irrig_cat`, `tillage`, `som_share`, `cn_input`,
#'     `land_use`) for [calculate_n_leaching()] is read from
#'     `n_balance_leaching_drivers`, which must hold at most one row per
#'     balance key (a many-to-one join aborts on duplicate keys rather than
#'     fanning the rows out and misaligning `drainage_mm`). Under
#'     `"meisinger_drainage"` it carries no `fert_type`, which the balance
#'     sets on each input part (see Details), and a `c_input_t` (t C per
#'     year) it carries in place of `cn_input` is divided by the item's
#'     `n_input_full_nosom_t`, summed over the water regimes. Missing required
#'     drivers abort inside the called `calculate_*()` function, naming the
#'     exact column; for the MANNER `nh3` methods the check runs on
#'     `n_balance_drivers` before any input is assembled, because no
#'     function in the package supplies those drivers (only `windspeed_ms`
#'     has a reader, [read_lpjml_wind()], not wired in).
#'   * `drainage_mm`: annual drainage (mm) for [calculate_n_leaching()]'s
#'     `"meisinger_drainage"` (`"image_gnm"` reads `runoff_mm` instead), as
#'     a numeric vector aligned to the balance-key rows, or already present
#'     as a `drainage_mm` column via `n_balance_leaching_drivers`. At grid
#'     resolution the rainfed/irrigated split doubles the rows, so an aligned
#'     vector only fits with `methods$regime = "none"`; otherwise supply
#'     drainage as a column of `n_balance_leaching_drivers`.
#' @param gwp 100-year global warming potential standard for N2O, `"ar6"`
#'   (default), `"ar5"` or `"ar4"`, matching [build_crop_soil_n2o_extension()].
#' @param example If `TRUE`, return a small fixture instead of assembling
#'   real data. Defaults to `FALSE`.
#' @return A tibble keyed by `year`/`area_code`/`item_cbs_code` (plus
#'   `lon`/`lat` at `resolution = "grid"`, and `water_regime`, `"rainfed"` or
#'   `"irrigated"`, at grid resolution unless `methods$regime = "none"`) with
#'   `area_ha` (each crop's harvested hectares in the cell, summed over cells
#'   at
#'   `resolution = "polity"`; used downstream to convert tonnes N to a
#'   per-hectare rate), the input aggregates
#'   (`n_input_full_t`, `n_input_full_nosom_t`, `n_input_std_t`,
#'   `n_input_som_t`, `n_input_for_n2o_t`), the output aggregates
#'   (`n_output_residues_t`, `n_output_som_t`, `n_output_useful_t`,
#'   `n_output_std_t`, `n_output_full_t`), the loss terms (`nh3_n_t`,
#'   `n2o_direct_n_t`, `no3_n_t`, `denitrification_n_t`,
#'   `retained_profile_n_t`, `n2o_indirect_no3_n_t`, `n2o_indirect_nh3_n_t`;
#'   `no3_n_t + denitrification_n_t + retained_profile_n_t` is the surplus
#'   plus, under `"meisinger_drainage"`, the nitrate the profile carried in
#'   from the key's previous year, `retained_carried_in_n_t`, and less,
#'   under `"image_gnm"`, the `surface_runoff_n_t` it computes from the
#'   inputs, to at most the surplus; on grassland and natural rows with a
#'   total runoff below 3 mm/yr `"image_gnm"` holds the whole surplus as
#'   inactive N in `retained_profile_n_t` and books its surface runoff beside
#'   it; `"image_gnm"` also reports the eroded
#'   soil N, `erosion_n_t`, which no budget pays for; see
#'   [calculate_n_leaching()]), the (post-cap)
#'   `som_sequestration_n_t`, `n_balance_t`, `surplus_t`, `surplus_share`,
#'   the five NUE ratios (`nue_std`, `nue_residues`, `nue_som`,
#'   `nue_useful`, `nue_full`), `total_gwp_co2e_kg`, and the `method_nh3`/
#'   `method_soil_n2o`/`method_n2o_indirect_nh3`/`method_leaching`
#'   provenance columns (the per-row stamps of the loss functions; a balance
#'   row pooling loss rows priced differently lists them joined by `"|"`,
#'   and a row with no N2O-relevant input, which emits no direct or
#'   NH3-indirect N2O, carries the method chosen suffixed `"_no_loss_row"`,
#'   e.g. `"ipcc2019_no_loss_row"`),
#'   `method_drivers` (`"whep_built"`, `"supplied"` or
#'   `"supplied|whep_built"`), and, when the leaching drivers carry them (as
#'   built ones do), the row's wet/dry `moisture_regime`, how it was obtained
#'   (`method_moisture_regime`) and `climate_fill_km`,
#'   `method_regime`
#'   and `method_regime_split` when the split ran (`"yield_ratio"`;
#'   `"area_no_production"` where the crop's production could not be
#'   expressed in fresh weight and the area share was used for everything;
#'   `"regime_shares"` for shares supplied as `data$regime_shares`; or
#'   `"no_regime_share"` for a row booked wholly rainfed because the regime
#'   layer does not cover it), plus the polity columns below. When the
#'   supplied `n_inputs` carry them, the
#'   `method_recycling_n`, `method_synthetic`, `method_deposition`,
#'   `method_deposition_scope`, `method_human_population`,
#'   `method_human_kgn_cap`, `method_unsupported`, `method_manure` and
#'   `method_unattributed` stamps from
#'   [build_n_inputs()] are carried through as well, so a balance names the
#'   input conventions that produced it. Gains
#'   `reporting_polity_out_of_span` when `polity_validity = "flag"`.
#'
#' @details
#' **Rainfed/irrigated split.** At grid resolution each crop row is split
#' into a rainfed and an irrigated part before the losses are computed, so
#' regime-specific loss drivers can attach. Two shares per row drive it:
#' the irrigated share of the crop's cell area, from the spatialization
#' chain's rainfed/irrigated hectares ([build_gridded_landuse()]), and the
#' irrigated share of its production, from [split_regime_yield()] on the
#' ratio of [build_regime_yield_ratio()]. Synthetic N, production N and the
#' residue destinies (used, bedding, burnt) split by the production share,
#' on the assumption that N availability per unit of yield is the same in
#' both regimes; every other input, the harvested area, grazed weeds and SOM
#' sequestration split by the area share. Summing the two parts reproduces
#' the unsplit balance for every additive column. `"area_split"` uses the
#' area share for everything and `"none"` returns the unsplit balance. The
#' shares can be supplied as `data$regime_shares` (`lon`, `lat`,
#' `area_code`, `item_cbs_code`, `year`, `irrigated_area_share`,
#' `irrigated_yield_share`). Loss-driver tables without a `water_regime`
#' column apply to both parts.
#'
#' `polity_validity` is forwarded to [build_n_inputs()] -- which forwards it in
#' turn to [build_ag_land_support()], [build_n_deposition()], [build_human_n()]
#' and [spatialize_country_n_to_crops()] -- and then applied to the balance rows
#' themselves, so one choice governs the whole build (whep#727). A
#' `data$n_inputs` table supplied directly is left alone: it was built by its
#' own call with its own choice, and the balance rows derived from it still get
#' this argument's fate at the tail.
#'
#' @section Loss drivers:
#' Which driver columns each loss method reads, and whether
#' [build_n_loss_drivers()] supplies them. At grid resolution that function
#' builds each driver table left `NULL`; at polity resolution both tables must
#' be supplied.
#'
#' |Method|Driver columns|Supplied|Status|
#' |---|---|---|---|
#' |`nh3`: `"ipcc"`|`fert_type`|yes|built|
#' |`nh3`: `"manner"`, `"manner_default"`|MANNER's|no|note 1|
#' |`n2o`: `"ipcc2019"`|`moisture_regime`|yes|built|
#' |`n2o`: `"ipcc2019_atl_med"`|`climate`|yes|built|
#' |`n2o`: `"aguilera"`, `"ipcc2006"`|`climate`, `irrig_type`|yes|built|
#' |`n2o_indirect`: `"ipcc2019"`|`moisture_regime`|yes|built|
#' |`n2o_indirect`: `"ipcc2019_atl_med"`|`climate`, `irrig_type`|yes|built|
#' |`leaching`: `"ipcc_fracleach"`|`moisture_regime`, `water_regime`|yes|built|
#' |`leaching`: `"meisinger_drainage"`|Meisinger's|yes|built, note 2|
#' |`leaching`: `"image_gnm"`|IMAGE-GNM's|yes|built, note 3|
#'
#' - Note 1: MANNER reads `manner_fertiliser`, `soil_ph`, `rate_kg_ha`,
#'   `rainfall_mm`, `irrigated`, `temp_c`, `temp_c_annual_mean`, `windspeed_ms`,
#'   `technique`, `system`, `incorporation_delay_h` and `species` (see
#'   [calculate_nh3()]). Drivers must be supplied; global drivers planned.
#' - Note 2: Meisinger reads `fert_type`, `climate`, `irrig_cat`, `land_use`,
#'   `cn_input`, `tillage`, `som_share`, `drainage_mm`, `root_depth_cm`
#'   and, by transport, `soil_porosity` (`"swat_mixing"`) or
#'   `soil_field_capacity` (`"burns"`); see [calculate_n_leaching()]. The
#'   builder supplies them from LPJmL, HWSD, FAO-56 and the carbon balance
#'   (see its Leaching drivers section).
#' - Note 3: IMAGE-GNM reads `runoff_mm` (the year's total runoff, LPJmL
#'   `mrunoff.nc`), `surface_runoff_fraction`,
#'   `slope_m_per_km` (both from [read_image_gnm_runoff()]), `land_use`,
#'   `temp_c_annual_mean`, `soil_field_capacity`, `soil_wilting_point`,
#'   `soil_texture_class`, `soil_drainage_class` and
#'   `soil_organic_carbon_pct`, plus the balance's own `n_input_std_t`,
#'   `nh3_n_t` and `area_ha`; see [calculate_n_leaching()]. The builder
#'   supplies them, as for note 2.
#'
#' [build_nitrogen_balance()] refuses a method of note 1 before assembling
#' any input unless its driver table is supplied.
#'
#' **Meisinger per input part.** Under `"meisinger_drainage"` each balance
#' key's surplus is split over the inputs of `n_input_full_t` (biological
#' fixation, excreta, liquid and solid manure, synthetic fertiliser, human N,
#' deposition and soil organic matter mineralisation) in proportion to their
#' N, `n_surplus_t * N_f / n_input_full_t`, and each part is leached on its
#' own Meisinger rows (Synthetic or Manure topsoil denitrification, its own
#' subsoil reduction), as the regional reconstruction this cascade is ported
#' from does (its surplus share is surplus over the full input, recycled
#' residues excluded). Each part carries its own retained nitrate from year
#' to year, on the key and the part's `fert_type`, and the parts' flows sum to
#' the key, so `no3_n_t + denitrification_n_t + retained_profile_n_t` equals
#' the surplus plus `retained_carried_in_n_t` on every key. A key with no
#' input carries no surplus and is stamped `meisinger_drainage_no_input`.
#'
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_nitrogen_balance(example = TRUE)
build_nitrogen_balance <- function(
  methods = list(
    nh3 = "manner",
    n2o = "ipcc2019",
    n2o_indirect = "ipcc2019",
    leaching = "meisinger_drainage"
  ),
  resolution = c("grid", "polity"),
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  gwp = c("ar6", "ar5", "ar4"),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  polity_validity <- rlang::arg_match(polity_validity)
  gwp <- rlang::arg_match(gwp)
  if (isTRUE(example)) {
    return(.resolve_polity_validity(
      .example_nitrogen_balance(),
      polity_validity
    ))
  }
  m <- .nb_methods(methods)
  .nh3_check_drivers_supplied(
    m$nh3,
    data$n_balance_drivers,
    "data$n_balance_drivers"
  )
  .nb_check_driver_sources(data, m, resolution)
  key <- .nb_key(resolution)

  # Compute calculate_npp_carbon_nitrogen() once and cache it on data, so
  # build_n_inputs()'s "recycling" term and this function's own prod_n_t
  # term (both calling .n_balance_npp()) share one result instead of each
  # re-running the full NPP-N pipeline.
  data$.npp_cache <- .n_balance_npp(data)
  # Materialise the agricultural land support once here so this function's
  # area_ha denominator and build_n_inputs()'s non-item allocation share ONE
  # table: derived twice they could disagree, and area_ha would silently fall
  # back to harvested area even though physical support was available.
  data$.polity_validity <- polity_validity
  data$ag_land_support <- .ni_resolve_land_support(data, years = NULL)
  n_inputs <- (data$n_inputs %||%
    build_n_inputs(
      resolution = resolution,
      polity_validity = polity_validity,
      data = data
    )) |>
    .human_upgrade_legacy_inputs()
  data <- .human_upgrade_legacy_drivers(data)
  .nb_validate_input_grain(n_inputs, resolution)

  split <- .nb_split_balance(n_inputs, data, key, resolution, m$regime)
  drivers <- .nb_resolve_drivers(data, split, m)
  data[names(drivers$tables)] <- drivers$tables
  split$x |>
    .nb_losses(split$n_inputs, split$key, m, data) |>
    .nb_indicators_pass1() |>
    .nb_cap_som() |>
    .nb_indicators_pass2(m, data, split$key, gwp) |>
    dplyr::mutate(method_drivers = drivers$stamp) |>
    .nb_finalise() |>
    .resolve_polity_validity(polity_validity)
}

# ---- Private helpers: loss drivers ----------------------------------------

# The two driver tables the loss cascade joins.
.nb_driver_tables <- function() {
  c("n_balance_drivers", "n_balance_leaching_drivers")
}

.nb_drivers_to_build <- function(data) {
  .nb_driver_tables()[purrr::map_lgl(
    .nb_driver_tables(),
    \(name) is.null(data[[name]])
  )]
}

# Refused before any input is assembled: drivers the balance would have to
# build but cannot. build_n_loss_drivers() works on the grid key per water
# regime, and supplies no Meisinger driver yet.
.nb_check_driver_sources <- function(data, m, resolution) {
  absent <- .nb_drivers_to_build(data)
  if (length(absent) == 0L) {
    return(invisible(NULL))
  }
  if (resolution == "polity") {
    cli::cli_abort(
      c(
        "A polity balance needs its loss drivers supplied:
         {.code data${absent}} {?is/are} missing.",
        i = "{.fn build_n_loss_drivers} builds them on the grid key only.
             Build the balance at grid resolution, or supply driver tables
             keyed by {.field area_code}, {.field item_cbs_code} and
             {.field year}."
      ),
      class = "whep_n_loss_drivers_polity"
    )
  }
  if (m$regime == "none") {
    cli::cli_abort(
      c(
        "WHEP-built loss drivers are per water regime, and
         {.code methods$regime = \"none\"} leaves the rows unsplit.",
        i = "Use {.val yield_split} or {.val area_split}, or supply
             {.code data${absent}}."
      ),
      class = "whep_n_loss_drivers_regime"
    )
  }
  invisible(NULL)
}

# Builds the driver tables the caller did not supply, from the split
# balance's own keys, and says where the drivers came from: "whep_built",
# "supplied", or "supplied|whep_built" when one table was supplied and the
# other built. The builder reads `data$n_loss_driver_inputs` and shares the
# balance's cell support (`country_grid`).
.nb_resolve_drivers <- function(data, split, m) {
  absent <- .nb_drivers_to_build(data)
  if (length(absent) == 0L) {
    return(list(tables = list(), stamp = "supplied"))
  }
  inputs <- data$n_loss_driver_inputs %||% list()
  inputs$country_grid <- inputs$country_grid %||% data$country_grid
  inputs$carbon_balance <- inputs$carbon_balance %||% data$carbon_balance
  driver_methods <- utils::modifyList(
    as.list(data$n_loss_driver_methods),
    list(leaching = m$leaching)
  )
  tables <- build_n_loss_drivers(
    .nb_driver_rows(split),
    data = inputs,
    methods = driver_methods
  )
  stamp <- if (length(absent) == length(.nb_driver_tables())) {
    "whep_built"
  } else {
    "supplied|whep_built"
  }
  list(tables = tables[absent], stamp = stamp)
}

# The balance keys the drivers must cover: every balance row (for leaching)
# and, with its loss fert_type, every loss row .nb_loss_rows() forms. Each
# carries its N, which build_n_loss_drivers() needs to tell an irrigated row
# without irrigated land of its item from one that carries no irrigated N.
.nb_driver_rows <- function(split) {
  key <- split$key
  loss <- split$n_inputs |>
    dplyr::filter(.data$fert_type %in% .nb_n2o_fert_types()) |>
    dplyr::summarise(
      n_input_t = sum(.data$n_input_t),
      .by = dplyr::all_of(c(key, "fert_type"))
    ) |>
    dplyr::mutate(fert_type = .nb_loss_fert_type(.data$fert_type))
  key_n <- dplyr::summarise(
    split$n_inputs,
    n_input_t = sum(.data$n_input_t),
    .by = dplyr::all_of(key)
  )
  # A balance key with no input row in build_n_inputs()' own ledger (one
  # that only an output term opened) received no input N: a structural zero.
  balance <- split$x |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(key))) |>
    dplyr::left_join(key_n, by = key, relationship = "one-to-one") |>
    dplyr::mutate(
      n_input_t = dplyr::coalesce(.data$n_input_t, 0),
      fert_type = NA_character_
    )
  dplyr::bind_rows(loss, balance)
}

# ---- Private helpers: method validation ----------------------------------

.nb_methods <- function(methods) {
  valid_names <- c(
    "nh3",
    "n2o",
    "n2o_indirect",
    "leaching",
    "leaching_transport",
    "leaching_none_class",
    "regime"
  )
  unknown <- setdiff(names(methods), valid_names)
  if (length(unknown) > 0L) {
    # qty() pinned: the marker sat ahead of both interpolations, so cli had to
    # defer it to post-processing, which refuses a message carrying more than
    # one candidate quantity -- every unknown name aborted with "Multiple
    # quantities for pluralization" instead of naming the name (#621).
    cli::cli_abort(
      "Unknown {.arg methods} {cli::qty(length(unknown))}name{?s}: \\
       {.val {unknown}}. Use {.val {valid_names}}."
    )
  }
  nh3 <- methods$nh3 %||% "manner"
  n2o <- methods$n2o %||% "ipcc2019"
  n2o_indirect <- methods$n2o_indirect %||% "ipcc2019"
  leaching <- methods$leaching %||% "meisinger_drainage"
  transport <- methods$leaching_transport %||% "swat_mixing"
  none_class <- methods$leaching_none_class %||% "retention"
  regime <- methods$regime %||% "yield_split"
  list(
    nh3 = rlang::arg_match(nh3, c("manner", "ipcc", "manner_default")),
    n2o = rlang::arg_match(
      n2o,
      c("ipcc2019", "ipcc2019_atl_med", "aguilera", "ipcc2006")
    ),
    n2o_indirect = rlang::arg_match(
      n2o_indirect,
      c("ipcc2019", "ipcc2019_atl_med")
    ),
    leaching = rlang::arg_match(
      leaching,
      c("meisinger_drainage", "ipcc_fracleach", "image_gnm")
    ),
    leaching_transport = rlang::arg_match(
      transport,
      c("swat_mixing", "burns")
    ),
    leaching_none_class = rlang::arg_match(
      none_class,
      c("retention", "waterlogged")
    ),
    regime = rlang::arg_match(
      regime,
      c("yield_split", "area_split", "none")
    )
  )
}

.nb_key <- function(resolution) {
  if (resolution == "grid") {
    c("lon", "lat", "area_code", "item_cbs_code", "year")
  } else {
    c("area_code", "item_cbs_code", "year")
  }
}

# ---- Step 1: input aggregates ---------------------------------------------

# The four N_input_* sums, as in the source implementation. "recycling" is
# deliberately excluded (it feeds the residue OUTPUT terms, not an input
# sum) and n_input_for_n2o_t is a separate sum that DOES include it, as the
# source's N2O input sum does.
.nb_inputs <- function(n_inputs, key) {
  provenance <- .nb_input_methods(n_inputs, key)
  wide <- n_inputs |>
    dplyr::summarise(
      n_input_t = sum(.data$n_input_t, na.rm = TRUE),
      .by = dplyr::all_of(c(key, "fert_type"))
    ) |>
    tidyr::pivot_wider(
      names_from = "fert_type",
      values_from = "n_input_t",
      values_fill = 0
    ) |>
    .nb_ensure_fert_cols()
  out <- .nb_input_totals(wide)
  if (is.null(provenance)) {
    return(out)
  }
  dplyr::left_join(out, provenance, by = key)
}

# The input aggregates, recomputed from the per-fert_type columns wherever
# those change (after the rainfed/irrigated split, which weights synthetic
# N differently from the other inputs).
.nb_input_totals <- function(wide) {
  wide |>
    dplyr::mutate(
      n_input_full_t = .data$bnf +
        .data$excreta +
        .data$manure_liquid +
        .data$manure_solid +
        .data$synthetic +
        .data$human +
        .data$deposition +
        .data$som_mineralization,
      n_input_full_nosom_t = .data$bnf +
        .data$excreta +
        .data$manure_liquid +
        .data$manure_solid +
        .data$synthetic +
        .data$human +
        .data$deposition,
      n_input_std_t = .data$n_input_full_nosom_t,
      n_input_som_t = .data$bnf +
        .data$excreta +
        .data$manure_liquid +
        .data$manure_solid +
        .data$synthetic +
        .data$human +
        .data$som_mineralization,
      n_input_for_n2o_t = .data$excreta +
        .data$manure_liquid +
        .data$manure_solid +
        .data$som_mineralization +
        .data$synthetic +
        .data$human +
        .data$recycling
    )
}

.nb_validate_input_grain <- function(n_inputs, resolution) {
  if (resolution != "grid") {
    return(invisible(n_inputs))
  }
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  .check_columns(n_inputs, key, "n_inputs")
  incomplete <- !stats::complete.cases(n_inputs[, key, drop = FALSE])
  if (any(incomplete)) {
    cli::cli_abort(c(
      "Grid nitrogen inputs must carry complete grid and item keys.",
      i = "{sum(incomplete)} row{?s} are incomplete; allocate non-item inputs
           over agricultural support before building the balance."
    ))
  }
  invisible(n_inputs)
}

.nb_input_methods <- function(n_inputs, key) {
  method_cols <- intersect(
    .nb_input_method_cols(),
    names(n_inputs)
  )
  if (length(method_cols) == 0L) {
    return(NULL)
  }
  dplyr::summarise(
    n_inputs,
    dplyr::across(dplyr::all_of(method_cols), .nb_method_stamp),
    .by = dplyr::all_of(key)
  )
}

.nb_method_stamp <- function(x) {
  values <- sort(unique(x[!is.na(x) & nzchar(x)]))
  if (length(values) == 0L) NA_character_ else paste(values, collapse = "|")
}

# fert_type columns not present after the pivot (a source with zero rows)
# contribute 0 to every sum rather than erroring; "recycling" is read too
# (for n_input_for_n2o_t); "accum_loss" is the documented gap and is always
# absent from build_n_inputs()'s output, so it never needs a column here.
.nb_ensure_fert_cols <- function(wide) {
  needed <- c(
    "bnf",
    "recycling",
    "manure_solid",
    "manure_liquid",
    "excreta",
    "deposition",
    "human",
    "som_mineralization",
    "synthetic"
  )
  missing <- setdiff(needed, names(wide))
  if (length(missing) > 0) {
    wide[missing] <- 0
  }
  wide
}

# ---- Step 2: output aggregates ---------------------------------------------

.nb_outputs <- function(x, data, key) {
  x |>
    .nb_add_prod_n(data, key) |>
    .nb_add_residue_destiny(data, key) |>
    .nb_add_grazed_weeds(data, key) |>
    .nb_add_som_sequestration(data, key)
}

# prod_n_t: the SAME calculate_npp_carbon_nitrogen() result build_n_inputs()
# already computes for its "recycling" term, via the shared .n_balance_npp()
# helper (R/n_balance_inputs.R) -- not a second, separate NPP-N call.
# area_ha (each crop's harvested hectares, needed downstream by
# calculate_n_surplus()/build_n_pathway_exceedance() to convert tonnes N to a
# per-hectare rate) is summed from the SAME NPP rows onto the balance key: at
# resolution = "grid" per (lon, lat, area_code, item_cbs_code, year), at
# "polity" over cells. Several item_prod_code crops can map to one
# item_cbs_code, so the harvested areas are summed the same way prod_n_t is.
.nb_add_prod_n <- function(x, data, key) {
  npp <- .n_balance_npp(data)
  if (is.null(npp)) {
    area <- .nb_area_support(data, key)
    if (is.null(area)) {
      return(dplyr::mutate(x, prod_n_t = 0, area_ha = NA_real_))
    }
    return(.nb_merge_output_term(x, dplyr::mutate(area, prod_n_t = 0), key))
  }
  prod_n <- npp |>
    .nb_npp_with_area() |>
    dplyr::summarise(
      prod_n_t = sum(.data$product_n_t, na.rm = TRUE),
      .by = dplyr::all_of(key)
    )
  support <- .nb_area_support(data, key)
  prod <- if (is.null(support)) {
    npp |>
      .nb_npp_with_area() |>
      dplyr::summarise(
        area_ha = .sum_if_any(.data$area_ha),
        .by = dplyr::all_of(key)
      ) |>
      dplyr::full_join(prod_n, by = key)
  } else {
    dplyr::full_join(prod_n, support, by = key)
  }
  .nb_merge_output_term(x, prod, key)
}

# Prefer explicit physical agricultural support for the per-hectare boundary
# denominator. Harvested area can exceed physical land under multicropping and
# leaves grass rows without an area; it remains a compatibility fallback only.
.nb_area_support <- function(data, key) {
  if (is.null(data$ag_land_support)) {
    return(NULL)
  }
  support <- .ni_land_support(data)
  support |>
    dplyr::summarise(
      area_ha = sum(.data$area_ha),
      .by = dplyr::all_of(key)
    )
}

# The NPP result carries each crop's harvested area (area_ha) when the upstream
# npp_n_input supplied it (the real crop-NPP chain, calculate_crop_npp() etc.,
# preserves it). When a caller assembled npp_n_input without area_ha, the
# harvested area is genuinely unknown, so an NA column is added instead of a
# guessed value. That NA collapses (na.rm) to 0 on the balance key, which
# downstream yields an NA per-hectare rate (the same guard as a true zero-area
# cell) rather than a spurious finite rate.
.nb_npp_with_area <- function(npp) {
  if (rlang::has_name(npp, "area_ha")) {
    return(npp)
  }
  dplyr::mutate(npp, area_ha = NA_real_)
}

# used_residue_n_t / bedding_residue_n_t / burnt_residue_n_t:
# calculate_residue_destinies() splits residue_dm_t into feed/bedding/burn/soil
# destinies; the three removed shares are converted to N with the SAME
# residue_n_kgdm coefficient calculate_npp_carbon_nitrogen() uses internally
# (whep::whep_coef_table("bio_coefs"), joined on item_prod_code).
#
# Bedding needs its own term rather than being left inside the burn share it is
# carved from: all three leave the field and so all three belong in
# n_output_full_t, but bedding is the only one that comes back as manure, so a
# reader has to be able to see it separately. Folding it into burnt_residue_n_t
# would keep the balance closed and mislabel the flow.
.nb_add_residue_destiny <- function(x, data, key) {
  if (is.null(data$residue_destiny_input)) {
    return(dplyr::mutate(
      x,
      used_residue_n_t = 0,
      bedding_residue_n_t = 0,
      burnt_residue_n_t = 0
    ))
  }
  n_kgdm <- whep::whep_coef_table("bio_coefs") |>
    dplyr::transmute(
      item_prod_code = as.character(.data$item_prod_code),
      residue_n_kgdm = .data$residue_n_kgdm
    )
  destiny <- data |>
    .nb_residue_destinies() |>
    dplyr::left_join(n_kgdm, by = "item_prod_code") |>
    .nb_check_residue_n_joined() |>
    dplyr::summarise(
      used_residue_n_t = sum(
        .data$residue_feed_dm_t * .data$residue_n_kgdm,
        na.rm = TRUE
      ),
      bedding_residue_n_t = sum(
        .data$residue_bedding_dm_t * .data$residue_n_kgdm,
        na.rm = TRUE
      ),
      burnt_residue_n_t = sum(
        .data$residue_burn_dm_t * .data$residue_n_kgdm,
        na.rm = TRUE
      ),
      .by = dplyr::all_of(key)
    )
  .nb_merge_output_term(x, destiny, key)
}

# The one residue-destiny split of a balance. The manure input reads the same
# split (.ni_manure_bedding(), R/n_balance_inputs.R), so the bedding booked
# here as leaving the field is the bedding that comes back as manure.
.nb_residue_destinies <- function(data) {
  data$residue_destiny_input |>
    calculate_residue_destinies(
      method = data$residue_destiny_method %||% "recovery_regional",
      bedding_fraction = data$residue_bedding_fraction %||% 0
    ) |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code))
}

# The N content is joined on item_prod_code, and a key that does not match
# (a code space or a spelling bio_coefs does not share) leaves residue_n_kgdm
# NA on every row. The three sums below then drop it through na.rm, so the
# used, bedding and burnt residue removals all ship as zero, the surplus rises
# by their whole N, and the balance still closes (whep#1034). One crop without
# a coefficient is a partial absence and passes; none at all is refused.
.nb_check_residue_n_joined <- function(destiny) {
  check_inputs_supplied(
    destiny,
    c("residue N content" = "residue_n_kgdm"),
    details = c(
      i = "No {.field item_prod_code} of {.arg data$residue_destiny_input}
           matched {.code whep_coef_table(\"bio_coefs\")}, so no residue
           removal could be converted to N."
    )
  )
}

# grazed_weeds_n_t: real grazed-forage intake from data$livestock_intake
# (redistribute_feed()'s feed_quality == "grass" rows), converted to N with
# whep::whep_coef_table("weed_coefs")$residue_n_kgdm_weed -- the SAME
# above-ground weed N-content coefficient crop_npp.R's .npp_cn_weeds() uses
# for weed_ag_n_t, the closest analogous purpose in the package (grass and
# spontaneous weed above-ground biomass share the one weed_coefs table).
# Territory/sub_territory resolve to area_code/lon/lat via the SAME private
# helpers build_n_inputs()'s manure term uses (R/n_balance_inputs.R).
.nb_add_grazed_weeds <- function(x, data, key) {
  if (is.null(data$livestock_intake)) {
    return(dplyr::mutate(x, grazed_weeds_n_t = 0))
  }
  weed_n_kgdm <- whep::whep_coef_table("weed_coefs")$residue_n_kgdm_weed
  .nb_check_grazed_label(data$livestock_intake)
  grazed <- data$livestock_intake |>
    dplyr::filter(.data$feed_quality == "grass") |>
    dplyr::summarise(
      intake_dm_t = sum(.data$intake_dm_t, na.rm = TRUE),
      .by = c("year", "territory", "sub_territory")
    ) |>
    .nb_grazed_coords() |>
    dplyr::summarise(
      grazed_weeds_n_t = sum(.data$intake_dm_t * weed_n_kgdm, na.rm = TRUE),
      .by = dplyr::all_of(key)
    ) |>
    check_inputs_supplied(
      c("grazed forage N" = "grazed_weeds_n_t"),
      details = .nb_grazed_remedy()
    )
  .nb_merge_output_term(x, grazed, key)
}

# `"grass"` is redistribute_feed()'s vocabulary, not a contract this function
# controls. A supplied intake whose label has moved matches no row, the grazed
# term is then an empty frame, and .nb_merge_output_term() zero-fills it into
# every balance row: n_output_full_t loses its whole grazing removal, the
# surplus rises by the same amount, and n_balance_t = input - output still
# closes exactly, because zero satisfies it (whep#1034). The label is asserted
# before the filter; check_inputs_supplied() after it catches an intake whose
# grass rows exist but carry no dry matter.
.nb_check_grazed_label <- function(intake) {
  check_labels_supplied(
    intake,
    "feed_quality",
    "grass",
    details = .nb_grazed_remedy()
  )
}

.nb_grazed_remedy <- function() {
  c(
    i = "{.arg data$livestock_intake} is the {.fn redistribute_feed} result;
         its {.val grass} rows are the grazed forage the balance removes as
         {.field grazed_weeds_n_t}.",
    i = "Leave {.arg data$livestock_intake} out to build a balance with no
         grazing removal at all."
  )
}

# Reuse build_n_inputs()'s manure territory/coordinate resolution verbatim
# (same package, private helpers callable directly) rather than
# reimplementing it a third time. Grazed forage is grass, so item_cbs_code is
# 3000L, the SAME grass sentinel build_n_inputs()'s manure engine assigns to
# grassland rows (.ni_manure_item_cbs()): this keys grazed weeds onto the
# grass N rows they belong with instead of the NA "no specific item" code
# (which never matches the 3000L grass input rows).
.nb_grazed_coords <- function(x) {
  coords <- .ni_manure_coords(x$sub_territory)
  x |>
    dplyr::mutate(
      lon = coords$lon,
      lat = coords$lat,
      area_code = .manure_territory_to_area_code(.data$territory),
      item_cbs_code = 3000L
    )
}

# som_sequestration_n_t: pmax(0, -son_change_kgn_ha) * area_ha / 1000, the
# SAME clamp direction as build_n_inputs()'s "som_mineralization" term
# (which keeps son_change_kgn_ha > 0), using build_carbon_balance()'s own
# area_ha directly (the same way .n_inputs_som() does). carbon_balance is
# per-land-use, not per-crop, so it carries no item_cbs_code (like
# .n_inputs_som()'s deposition/human/SOM rows); the NA_integer_
# "not crop-specific" sentinel is used. A cell in net carbon GAIN
# (son_change_kgn_ha < 0) emits no som_mineralization input row, so .nb_merge_
# output_term()'s full join is what keeps the sequestration output when no
# NA-item input row (deposition/human/SOM) exists at that cell to attach to.
.nb_add_som_sequestration <- function(x, data, key) {
  if (is.null(data$carbon_balance)) {
    return(dplyr::mutate(x, som_sequestration_n_t = 0))
  }
  seq_n <- data$carbon_balance |>
    # Crop GROUPS are cropland too
    # (crop_groups = list(method = "rotation_groups")), so this keys on the
    # prefix, not the literal.
    dplyr::filter(.soc_is_cropland(.data$land_use)) |>
    dplyr::mutate(item_cbs_code = NA_integer_) |>
    dplyr::summarise(
      som_sequestration_n_t = sum(
        pmax(0, -.data$son_change_kgn_ha) * .data$area_ha / 1000,
        na.rm = TRUE
      ),
      .by = dplyr::all_of(key)
    )
  .nb_merge_output_term(x, seq_n, key)
}

# Merge an output term keyed by `key` onto x. Unlike a left join (which drops
# any term row whose key is absent from x -- the failure mode when a cell in
# net carbon gain, or a grass cell with no manure input row, has no
# pre-existing balance-key row to attach to), a full join keeps the term's
# rows and creates the balance-key row when it is missing. Every non-key
# numeric column (the newly-added output column and the input/output
# aggregates already on x) is coalesced to 0 on those output-only rows, which
# would otherwise carry NA from the join and poison the balance closure.
.nb_merge_output_term <- function(x, term, key) {
  dplyr::full_join(x, term, by = key) |>
    dplyr::mutate(dplyr::across(
      !dplyr::all_of(key) & dplyr::where(is.numeric),
      \(v) tidyr::replace_na(v, 0)
    ))
}

# ---- Step 3a: loss cascade -------------------------------------------------

# Runs calculate_nh3(), then calculate_soil_n2o() + calculate_indirect_n2o_
# nh3() (which needs nh3_n_t already on the row), at the (key, fert_type)
# grain (n_inputs, restricted to the N_input_for_N2O_MgN fert_types, per
# the verified formula), then sums nh3_n_t/n2o_direct_n_t/n2o_indirect_
# nh3_n_t back to the balance key. Recycling/SOM rows get nh3_n_t = 0 (IPCC
# Tier 1's own "Recycling"/"SOM" fraction is 0; MANNER has no residue/SOM-N
# path), consistent with calculate_nh3(method = "ipcc")'s own treatment,
# but still flow through calculate_soil_n2o()/calculate_indirect_n2o_nh3(),
# matching fertiliser_n2o_modifiers's coverage.
.nb_losses <- function(x, n_inputs, key, m, data) {
  rows <- .nb_loss_rows(n_inputs, key, data)
  no_loss <- .nb_no_loss_stamps(m)
  if (nrow(rows) == 0L) {
    return(dplyr::mutate(
      x,
      nh3_n_t = 0,
      n2o_direct_n_t = 0,
      n2o_indirect_nh3_n_t = 0,
      method_nh3 = m$nh3,
      method_soil_n2o = no_loss$soil,
      method_n2o_indirect_nh3 = no_loss$indirect
    ))
  }
  no_volat <- c("Recycling", "SOM")
  with_nh3 <- dplyr::bind_rows(
    rows |>
      dplyr::filter(!.data$fert_type %in% no_volat) |>
      calculate_nh3(method = m$nh3),
    rows |>
      dplyr::filter(.data$fert_type %in% no_volat) |>
      dplyr::mutate(nh3_n_t = 0, method_nh3 = m$nh3)
  )
  # The soil-N2O and indirect stamps name the factor each loss row took, so
  # a balance row pooling several of them lists every one ("|"-joined).
  losses <- with_nh3 |>
    calculate_soil_n2o(method = m$n2o) |>
    calculate_indirect_n2o_nh3(method = m$n2o_indirect) |>
    dplyr::summarise(
      nh3_n_t = sum(.data$nh3_n_t, na.rm = TRUE),
      n2o_direct_n_t = sum(.data$n2o_direct_n_t, na.rm = TRUE),
      n2o_indirect_nh3_n_t = sum(.data$n2o_indirect_nh3_n_t, na.rm = TRUE),
      method_soil_n2o = .nb_method_stamp(.data$method_soil_n2o),
      method_n2o_indirect_nh3 = .nb_method_stamp(
        .data$method_n2o_indirect_nh3
      ),
      .by = dplyr::all_of(key)
    ) |>
    dplyr::mutate(method_nh3 = m$nh3)
  # A balance row with no loss row (no N2O-relevant input) emits nothing;
  # its zeros are structural, and it is stamped with the method chosen and
  # the reason, apart from the rows that method priced.
  x |>
    dplyr::left_join(losses, by = key) |>
    dplyr::mutate(
      nh3_n_t = tidyr::replace_na(.data$nh3_n_t, 0),
      n2o_direct_n_t = tidyr::replace_na(.data$n2o_direct_n_t, 0),
      n2o_indirect_nh3_n_t = tidyr::replace_na(
        .data$n2o_indirect_nh3_n_t,
        0
      ),
      method_nh3 = tidyr::replace_na(.data$method_nh3, m$nh3),
      method_soil_n2o = tidyr::replace_na(.data$method_soil_n2o, no_loss$soil),
      method_n2o_indirect_nh3 = tidyr::replace_na(
        .data$method_n2o_indirect_nh3,
        no_loss$indirect
      )
    )
}

# The N2O stamps of a balance row with no loss row. Several methods stamp
# their priced rows with the bare method name ("ipcc2019_atl_med",
# "ipcc2006", "aguilera" on non-excreta rows), so the bare name alone would
# read as a priced row; the suffix says no factor was applied. method_nh3
# stays the bare method on every row, as calculate_nh3() stamps it.
.nb_no_loss_stamps <- function(m) {
  list(
    soil = paste0(m$n2o, "_no_loss_row"),
    indirect = paste0(m$n2o_indirect, "_no_loss_row")
  )
}

# N_input_for_N2O_MgN's own fert_types (Excreta, Liquid, Solid, SOM,
# Synthetic, Human, Recycling), mapped to the loss cascade's Title-case
# vocabulary (see file header) and joined against data$n_balance_drivers on
# (key, fert_type) for the moisture_regime/climate/irrig_type/MANNER driver
# columns; never invents driver values, missing columns abort inside
# calculate_*().
.nb_loss_rows <- function(n_inputs, key, data) {
  rows <- n_inputs |>
    dplyr::filter(.data$fert_type %in% .nb_n2o_fert_types()) |>
    .nb_pool_loss_rows(key) |>
    dplyr::mutate(fert_type = .nb_loss_fert_type(.data$fert_type))
  if (is.null(data$n_balance_drivers)) {
    return(rows)
  }
  dplyr::left_join(
    rows,
    data$n_balance_drivers,
    by = c(.nb_driver_key(key, data$n_balance_drivers), "fert_type"),
    relationship = "many-to-one"
  )
}

# N_input_for_N2O_MgN's own fert_types, in build_n_inputs()' vocabulary.
.nb_n2o_fert_types <- function() {
  c(
    "excreta",
    "manure_liquid",
    "manure_solid",
    "som_mineralization",
    "synthetic",
    "human",
    "recycling"
  )
}

# Sum the N of the rows sharing a (key, fert_type). The excreta species mix
# travels with it: the share is the N-weighted mean of the pooled rows, and
# undefined (NA) when any pooled row carrying N has none, so a row of
# unknown species is never priced as though it had the mix of the rest
# (.excreta_weighted_share()). The share is computed before n_input_t is
# replaced by its sum. Excreta labelled only as the fallback of a manure-type
# split is collected manure, priced as applied manure, so it pools apart from
# the grazing excreta of its key rather than erasing their mix: a key can
# then carry two "Excreta" loss rows. build_n_inputs() already keeps the two
# apart; this holds the rule for a supplied input table too.
.nb_pool_loss_rows <- function(rows, key) {
  by <- c(key, "fert_type")
  if (!rlang::has_name(rows, "excreta_cpp_share")) {
    return(dplyr::summarise(
      rows,
      n_input_t = sum(.data$n_input_t, na.rm = TRUE),
      .by = dplyr::all_of(by)
    ))
  }
  if (!rlang::has_name(rows, "method_excreta_species")) {
    rows$method_excreta_species <- NA_character_
  }
  rows$.fallback <- .excreta_is_fallback(rows$method_excreta_species)
  dplyr::summarise(
    rows,
    method_excreta_species = .excreta_method_stamp(
      .data$method_excreta_species,
      .data$excreta_cpp_share,
      .data$n_input_t
    ),
    excreta_cpp_share = .excreta_weighted_share(
      .data$excreta_cpp_share,
      .data$n_input_t
    ),
    n_input_t = sum(.data$n_input_t, na.rm = TRUE),
    .by = dplyr::all_of(c(by, ".fallback"))
  ) |>
    dplyr::select(-".fallback")
}

# ---- Step 3b/c: balance closure, SOM cap, NUE ------------------------------

# First-pass balance from the input/output aggregates, BEFORE leaching (the
# leaching call needs n_surplus_t from this pass, per the source
# implementation's ordering).
.nb_indicators_pass1 <- function(x) {
  x |>
    .nb_output_aggregates() |>
    .nb_balance()
}

.nb_output_aggregates <- function(x) {
  x |>
    dplyr::mutate(
      n_output_residues_t = .data$prod_n_t + .data$used_residue_n_t,
      n_output_som_t = .data$prod_n_t + .data$som_sequestration_n_t,
      n_output_useful_t = .data$prod_n_t +
        .data$used_residue_n_t +
        .data$grazed_weeds_n_t +
        .data$som_sequestration_n_t,
      n_output_std_t = .data$prod_n_t + .data$grazed_weeds_n_t,
      n_output_full_t = .data$prod_n_t +
        .data$used_residue_n_t +
        .data$bedding_residue_n_t +
        .data$burnt_residue_n_t +
        .data$grazed_weeds_n_t +
        .data$nh3_n_t +
        .data$som_sequestration_n_t
    )
}

.nb_balance <- function(x) {
  x |>
    dplyr::mutate(
      n_balance_t = .data$n_input_full_t - .data$n_output_full_t,
      surplus_t = pmax(0, .data$n_balance_t),
      surplus_share = .nb_ratio(.data$surplus_t, .data$n_input_full_t)
    )
}

# N-limitation cap (as in the source implementation): a soil already in
# deficit cannot additionally sequester N it doesn't have. Only engages
# when n_balance_t < 0 AND som_sequestration_n_t > 0.
.nb_cap_som <- function(x) {
  dplyr::mutate(
    x,
    som_sequestration_n_t = dplyr::if_else(
      .data$n_balance_t < 0 & .data$som_sequestration_n_t > 0,
      pmax(0, .data$som_sequestration_n_t + .data$n_balance_t),
      .data$som_sequestration_n_t
    )
  )
}

# Second pass: run leaching with the (capped-SOM) surplus, recompute every
# output/balance term from the capped som_sequestration_n_t, then the NUE
# ratios and the GWP/CO2e indicator.
.nb_indicators_pass2 <- function(x, m, data, key, gwp) {
  x |>
    dplyr::mutate(n_surplus_t = .data$surplus_t) |>
    .nb_run_leaching(m, data, key) |>
    .nb_output_aggregates() |>
    .nb_balance() |>
    .nb_nue() |>
    .nb_gwp(gwp)
}

# calculate_n_leaching() runs on the balance rows joined to
# data$n_balance_leaching_drivers at the balance key, with data$drainage_mm (a
# numeric vector aligned to x, or a "drainage_mm" column already present via
# n_balance_leaching_drivers). "ipcc_fracleach" and "image_gnm" run once per
# balance key. "meisinger_drainage" runs once per input part of each key
# (.nb_meisinger_parts()) and carries the retained nitrate of each part from
# year to year on the balance key without its year (cell, polity, item and
# water regime) and the part's fert_type, so every year of the balance is
# threaded in order on that key.
.nb_run_leaching <- function(x, m, data, key) {
  joined <- if (is.null(data$n_balance_leaching_drivers)) {
    x
  } else {
    .nb_leaching_join(x, data$n_balance_leaching_drivers, key)
  }
  if (m$leaching == "meisinger_drainage") {
    return(
      joined |>
        .nb_attach_drainage(data) |>
        .nb_derive_cn_input(key) |>
        .nb_meisinger_parts(m, key)
    )
  }
  # calculate_n_leaching() stamps each row with the fraction it took (for
  # ipcc_fracleach, wet, dry irrigated or dry rainfed), so the stamp is kept.
  calculate_n_leaching(
    joined,
    drainage_mm = data$drainage_mm,
    method = m$leaching,
    transport = m$leaching_transport,
    none_class = m$leaching_none_class,
    carry_over_by = setdiff(key, "year")
  )
}

# The drainage as a column of the balance rows: the drivers' own, else
# data$drainage_mm, a single value or a vector aligned to the rows. Absent,
# it stays absent and calculate_n_leaching() refuses the run.
.nb_attach_drainage <- function(joined, data) {
  if (rlang::has_name(joined, "drainage_mm") || is.null(data$drainage_mm)) {
    return(joined)
  }
  joined$drainage_mm <- vctrs::vec_recycle(
    data$drainage_mm,
    nrow(joined),
    x_arg = "data$drainage_mm"
  )
  joined
}

# The input C:N Meisinger's attenuation reads (cn_input), from the C input
# the WHEP-built drivers carry (c_input_t, t C per year) over the item's non-SOM
# N input, n_input_full_nosom_t, the definition the A_CN span was calibrated
# with. Both regime rows of an item carry the item's whole C, so the ratio
# divides by the item's whole N, summed over the water regimes, and both rows
# are stamped `item_level_cn`: the soil carbon inputs carry no water regime
# here, so the rainfed and irrigated parts share the item's C:N. An item with no
# non-SOM N has no input C:N (NA, which calculate_n_leaching() reads as the
# minimum C:N, no attenuation), stamped. A supplied cn_input is kept.
.nb_derive_cn_input <- function(joined, key) {
  if (
    rlang::has_name(joined, "cn_input") ||
      !rlang::has_name(joined, "c_input_t")
  ) {
    return(joined)
  }
  joined |>
    dplyr::mutate(
      .item_n_t = sum(.data$n_input_full_nosom_t),
      .by = dplyr::all_of(setdiff(key, "water_regime"))
    ) |>
    dplyr::mutate(
      cn_input = dplyr::if_else(
        .data$.item_n_t > 0,
        .data$c_input_t / .data$.item_n_t,
        NA_real_
      ),
      method_c_input = dplyr::case_when(
        is.na(.data$c_input_t) ~ .data$method_c_input,
        .data$.item_n_t > 0 ~ paste0(.data$method_c_input, ";item_level_cn"),
        .default = paste0(.data$method_c_input, ";no_non_som_n")
      )
    ) |>
    dplyr::select(-".item_n_t")
}

# The input fert_types a balance key's surplus is split over, in
# build_n_inputs()' vocabulary and the loss cascade's: every term of
# n_input_full_t. Recycled residues are not an input of the surplus, as in
# the ported source, which books their surplus at 0.
.nb_surplus_fert_types <- function() {
  c(
    bnf = "BNF",
    excreta = "Excreta",
    manure_liquid = "Liquid",
    manure_solid = "Solid",
    synthetic = "Synthetic",
    human = "Human",
    deposition = "Deposition",
    som_mineralization = "SOM"
  )
}

# The Meisinger cascade per input part. Each key's surplus is split over its
# input fert_types in proportion to their N, n_surplus_t * N_f /
# n_input_full_t, as the ported source does (Surplus_MgN = MgN *
# Surplus_share, Surplus_share = Surplus / N_input_full), and each part takes
# its own topsoil denitrification class (Synthetic or Manure) and subsoil
# reduction. A key's parts are the fert_types it carries in any year of the
# run, so a part's carried pool keeps cascading in a year without its input.
# The parts' flows sum back to the key, so the key's partition closes:
# n_surplus_t + retained_carried_in_n_t = no3_n_t + denitrification_n_t +
# retained_profile_n_t.
.nb_meisinger_parts <- function(joined, m, key) {
  .nb_check_no_fert_type(joined)
  types <- .nb_surplus_fert_types()
  joined$.row <- seq_len(nrow(joined))
  stay <- setdiff(key, "year")
  long <- joined |>
    dplyr::select(dplyr::all_of(c(
      ".row",
      key,
      "n_surplus_t",
      "n_input_full_t",
      names(types)
    ))) |>
    tidyr::pivot_longer(
      dplyr::all_of(names(types)),
      names_to = ".input",
      values_to = ".part_n_t"
    )
  carried <- long |>
    dplyr::filter(.data$.part_n_t > 0) |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(c(stay, ".input"))))
  drivers <- setdiff(
    names(joined),
    c(names(types), "n_surplus_t", "n_input_full_t", key, "fert_type")
  )
  parts <- long |>
    dplyr::semi_join(carried, by = c(stay, ".input")) |>
    dplyr::transmute(
      .row = .data$.row,
      dplyr::across(dplyr::all_of(key)),
      fert_type = unname(types[.data$.input]),
      n_surplus_t = dplyr::if_else(
        .data$n_input_full_t > 0,
        .data$n_surplus_t * .data$.part_n_t / .data$n_input_full_t,
        0
      )
    ) |>
    dplyr::left_join(
      joined[c(".row", .nb_meisinger_driver_cols(joined, drivers))],
      by = ".row",
      relationship = "many-to-one"
    )
  flows <- calculate_n_leaching(
    parts,
    drainage_mm = "drainage_mm",
    method = "meisinger_drainage",
    transport = m$leaching_transport,
    none_class = m$leaching_none_class,
    carry_over_by = c(stay, "fert_type")
  ) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(.nb_leaching_flow_cols()), sum),
      method_leaching = .nb_method_stamp(.data$method_leaching),
      .by = ".row"
    )
  .nb_join_part_flows(joined, flows)
}

# The flows a Meisinger part returns, summed to its key.
.nb_leaching_flow_cols <- function() {
  c(
    "no3_n_t",
    "denitrification_n_t",
    "retained_profile_n_t",
    "retained_carried_in_n_t",
    "n2o_indirect_no3_n_t"
  )
}

# The columns of the joined balance a part carries into the cascade: the
# drivers Meisinger reads (and the drainage), no other balance term.
.nb_meisinger_driver_cols <- function(joined, drivers) {
  intersect(
    c(
      "climate",
      "irrig_cat",
      "land_use",
      "cn_input",
      "tillage",
      "som_share",
      "drainage_mm",
      "root_depth_cm",
      "soil_porosity",
      "soil_field_capacity"
    ),
    drivers
  )
}

# A key with no input part has no surplus (the surplus is at most the input)
# and no pool: its flows are structural zeros, stamped.
.nb_join_part_flows <- function(joined, flows) {
  joined |>
    dplyr::left_join(flows, by = ".row", relationship = "one-to-one") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(.nb_leaching_flow_cols()),
        \(v) tidyr::replace_na(v, 0)
      ),
      method_leaching = tidyr::replace_na(
        .data$method_leaching,
        "meisinger_drainage_no_input"
      )
    ) |>
    dplyr::select(-".row")
}

# Under the input-part split the balance gives each part its own fert_type;
# a fert_type in the supplied leaching drivers would name one source for a
# surplus of many.
.nb_check_no_fert_type <- function(joined) {
  if (!rlang::has_name(joined, "fert_type")) {
    return(invisible(joined))
  }
  cli::cli_abort(
    c(
      "{.code data$n_balance_leaching_drivers} carries {.field fert_type}.",
      i = "Meisinger leaching splits each key's surplus over its input
           fert_types itself; drop the column."
    ),
    class = "whep_leaching_fert_type_supplied"
  )
}

# n_balance_leaching_drivers is caller-supplied and documented as one row per
# balance key. Enforce that with a many-to-one join: a duplicate key would
# otherwise fan x out to more rows than the caller's balance-key-aligned
# drainage_mm vector, and calculate_n_leaching() would then either abort with
# a size-mismatch error far from the cause or (when the fanned count happens
# to match) bind drainage to the wrong cells silently, corrupting the
# leaching/denitrification partition. dplyr aborts here at the join instead,
# naming the offending row.
.nb_leaching_join <- function(x, drivers, key) {
  dplyr::left_join(
    x,
    drivers,
    by = .nb_driver_key(key, drivers),
    relationship = "many-to-one"
  )
}

# Five NUE ratios (the source implementation's balance parameters and NUE
# calculation): nue_std/nue_residues divide by N_input_std, nue_som/nue_useful
# by N_input_full, nue_full by N_input_SOM. These denominators are NOT
# collapsed into one shared variable -- the source genuinely uses two
# different conventions and this keeps them independently traceable.
.nb_nue <- function(x) {
  dplyr::mutate(
    x,
    nue_std = .nb_ratio(.data$prod_n_t, .data$n_input_std_t),
    nue_residues = .nb_ratio(
      .data$n_output_residues_t,
      .data$n_input_std_t
    ),
    nue_som = .nb_ratio(.data$n_output_som_t, .data$n_input_full_t),
    nue_useful = .nb_ratio(.data$n_output_useful_t, .data$n_input_full_t),
    nue_full = .nb_ratio(.data$n_output_full_t, .data$n_input_som_t)
  )
}

# A nutrient-use ratio is undefined when its input denominator is zero. Return
# NA rather than leaking NaN/Inf from output-only balance rows.
.nb_ratio <- function(numerator, denominator) {
  dplyr::if_else(denominator > 0, numerator / denominator, NA_real_)
}

# GWP/CO2e indicator: reuses .ghg_gwp_factors() (livestock_ghg_extension.R)
# and the 44/28 N2O-N to N2O conversion (crop_soil_n2o_extension.R's own
# .soil_n2o_factors()$n_to_n2o), matching this codebase's own kg-CO2e
# convention for impact_u/co2e_kg (tonnes N x 1000 -> kilograms).
.nb_gwp <- function(x, gwp) {
  gwp_n2o <- .ghg_gwp_factors(gwp)[["n2o"]]
  n_to_n2o <- .soil_n2o_factors()$n_to_n2o
  dplyr::mutate(
    x,
    total_gwp_co2e_kg = (.data$n2o_direct_n_t +
      .data$n2o_indirect_nh3_n_t +
      .data$n2o_indirect_no3_n_t) *
      n_to_n2o *
      gwp_n2o *
      1000
  )
}

# ---- Final resolution aggregation / column selection -----------------------

.nb_finalise <- function(x) {
  cols <- c(
    .nb_present_key(x),
    "area_ha",
    "n_input_full_t",
    "n_input_full_nosom_t",
    "n_input_std_t",
    "n_input_som_t",
    "n_input_for_n2o_t",
    "prod_n_t",
    "used_residue_n_t",
    "bedding_residue_n_t",
    "burnt_residue_n_t",
    "grazed_weeds_n_t",
    "som_sequestration_n_t",
    "n_output_residues_t",
    "n_output_som_t",
    "n_output_useful_t",
    "n_output_std_t",
    "n_output_full_t",
    "nh3_n_t",
    "n2o_direct_n_t",
    "no3_n_t",
    "denitrification_n_t",
    "retained_profile_n_t",
    intersect(
      c("retained_carried_in_n_t", "surface_runoff_n_t", "erosion_n_t"),
      names(x)
    ),
    "n2o_indirect_no3_n_t",
    "n2o_indirect_nh3_n_t",
    "n_balance_t",
    "surplus_t",
    "surplus_share",
    "nue_std",
    "nue_residues",
    "nue_som",
    "nue_useful",
    "nue_full",
    "total_gwp_co2e_kg",
    "method_nh3",
    "method_soil_n2o",
    "method_n2o_indirect_nh3",
    "method_leaching",
    "method_drivers",
    intersect(.nb_driver_cols(), names(x)),
    intersect(c("method_regime", "method_regime_split"), names(x)),
    intersect(.nb_input_method_cols(), names(x))
  )
  dplyr::select(x, dplyr::all_of(cols))
}

# The wet/dry class each balance row was priced on and how it was obtained,
# and the stamp of every other leaching driver, kept when the leaching drivers
# carry them (build_n_loss_drivers() does).
.nb_driver_cols <- function() {
  c(
    "moisture_regime",
    "method_moisture_regime",
    "climate_fill_km",
    "method_climate",
    "method_tillage",
    "method_som",
    "method_c_input",
    "method_drainage",
    "method_root_depth",
    "method_soil_hydraulic",
    "method_runoff",
    "method_image_gnm_runoff",
    "method_soil_texture_fao",
    "method_soil_drainage"
  )
}

# The method columns build_n_inputs() stamps on its rows, carried through to
# the balance so a published balance names the conventions that produced it.
# `method_deposition_scope` is here because DA-14 made deposition scope a
# choice: a territory-scope balance and a land-scope balance differ by about
# 1.4% of the deposition term and would otherwise be indistinguishable.
# `method_deposition` is the other deposition axis (whep#1105): which product
# the field itself came from. HaNi is measurably biased over Europe and the
# bias grows backwards in time (whep#1097/#1121), so a balance built on a
# corrected field and one built on raw HaNi have to be tellable apart.
#
# `method_human_population` and `method_human_kgn_cap` are here because the
# human term's population basis is a choice: per inhabitant on the WPP total,
# or per urban inhabitant on HYDE's urban count, which differ by the ratio of
# each country's urban share to the calibration one.
#
# `method_unattributed` is here for the same reason (whep#532): the nitrogen
# that reached agricultural land but no single crop is a real mass, and whether
# a balance spread it over cropland, over all agricultural land, or dropped it
# is not recoverable from the numbers.
#
# `intersect()`ed against the actual columns by the caller, so an `n_inputs`
# table supplied from an older build that predates a stamp still balances.
.nb_input_method_cols <- function() {
  c(
    "method_recycling_n",
    "method_synthetic",
    "method_deposition",
    "method_deposition_scope",
    "method_human_population",
    "method_human_kgn_cap",
    "method_unsupported",
    "method_manure",
    "method_unattributed"
  )
}

.nb_present_key <- function(x) {
  intersect(
    c("lon", "lat", "area_code", "item_cbs_code", "water_regime", "year"),
    names(x)
  )
}

# ---- fert_type vocabulary bridge (documented mapping, see file header) ----

.nb_loss_fert_type <- function(fert_type) {
  dplyr::case_when(
    fert_type == "bnf" ~ "BNF",
    fert_type == "recycling" ~ "Recycling",
    fert_type == "manure_solid" ~ "Solid",
    fert_type == "manure_liquid" ~ "Liquid",
    fert_type == "excreta" ~ "Excreta",
    fert_type == "deposition" ~ "Deposition",
    fert_type == "human" ~ "Human",
    fert_type == "som_mineralization" ~ "SOM",
    fert_type == "synthetic" ~ "Synthetic"
  )
}

# ---- Example fixture -------------------------------------------------------

# Constructed to close exactly by design: n_input_full_t == n_output_full_t
# (n_balance_t = 0, surplus_t = 0), so no SOM cap engages, and
# n_input_full_t != n_input_som_t so nue_som/nue_useful vs nue_full exercise
# different denominators.
.example_nitrogen_balance <- function() {
  tibble::tribble(
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~area_ha,
    ~n_input_full_t,
    ~n_input_full_nosom_t,
    ~n_input_std_t,
    ~n_input_som_t,
    ~n_input_for_n2o_t,
    ~prod_n_t,
    ~used_residue_n_t,
    ~bedding_residue_n_t,
    ~burnt_residue_n_t,
    ~grazed_weeds_n_t,
    ~som_sequestration_n_t,
    ~n_output_residues_t,
    ~n_output_som_t,
    ~n_output_useful_t,
    ~n_output_std_t,
    ~n_output_full_t,
    ~nh3_n_t,
    ~n2o_direct_n_t,
    ~no3_n_t,
    ~denitrification_n_t,
    ~retained_profile_n_t,
    ~retained_carried_in_n_t,
    ~n2o_indirect_no3_n_t,
    ~n2o_indirect_nh3_n_t,
    ~n_balance_t,
    ~surplus_t,
    ~surplus_share,
    ~nue_std,
    ~nue_residues,
    ~nue_som,
    ~nue_useful,
    ~nue_full,
    ~total_gwp_co2e_kg,
    ~method_nh3,
    ~method_soil_n2o,
    ~method_n2o_indirect_nh3,
    ~method_leaching,
    ~method_drivers,
    10L,
    2511L,
    2020L,
    120,
    100,
    98,
    98,
    99,
    97,
    40,
    10,
    0,
    5,
    8,
    2,
    50,
    42,
    60,
    48,
    100,
    35,
    1.5,
    12,
    3,
    0,
    0,
    0.132,
    0.49,
    0,
    0,
    0,
    40 / 98,
    50 / 98,
    42 / 100,
    60 / 100,
    100 / 99,
    (1.5 + 0.49 + 0.132) * (44 / 28) * 273 * 1000,
    "manner",
    "ipcc2019_wet_dry",
    "ipcc2019_wet_dry",
    paste0(
      "meisinger_drainage;swat_mixing;none_retention;",
      "initial_pool_steady_state"
    ),
    "supplied"
  ) |>
    .add_reporting_polity_columns()
}
