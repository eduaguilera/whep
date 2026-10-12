# Nitrogen loss cascade: ammonia volatilisation, direct soil N2O, nitrate
# leaching + denitrification, and indirect N2O (Module C, Task C5), ported
# from an earlier regional historical reconstruction.
#
# These four functions are PURE, TESTABLE row-wise transforms: they take a
# tibble x (one row per N-input record) plus explicit driver columns already
# present on x (or, for calculate_n_leaching(), a separate drainage_mm
# argument) and append new columns. None of them read any gridded raster
# themselves; wiring real gridded drivers into x is a later task's job.

#' Estimate ammonia-N volatilisation from applied nitrogen.
#'
#' @description
#' Three independent methods for the fraction of applied nitrogen
#' volatilised as ammonia. `"ipcc"` (IPCC 2019 Tier 1) needs only `fert_type`
#' and applies a single global fraction. `"manner"` (the default) dispatches
#' each row through the process-based [calculate_manner_nh3()] MANNER model
#' (Task C4), which requires far more
#' driver detail (see Details); this asymmetry in input requirements is
#' intentional, not an oversight. `"manner_default"` dispatches each row
#' through [calculate_manner_nh3_default()] instead, the same process-based
#' organic-manure model but with `technique`/`incorporation_delay_h` filled
#' in from a documented gross-assumption blend rather than required as
#' driver columns.
#'
#' @details
#' `method = "manner"` requires `x` to already carry a `manner_fertiliser`
#' column holding the exact [calculate_manner_nh3()] `fertiliser` key
#' (`"Urea"`, `"AN"`, `"CAN"`, `"AS"`, `"cattle_slurry"`, `"pig_slurry"`,
#' `"FYM"`, `"poultry_manure"` or `"urban"`) plus every driver column that
#' key's path needs: `soil_ph`, `rate_kg_ha`, `rainfall_mm`, `irrigated`,
#' `temp_c`, `temp_c_annual_mean` for the synthetic path; `rainfall_mm`,
#' `irrigated`, `windspeed_ms`, `technique`, `system`, `temp_c`,
#' `incorporation_delay_h`, `species` (unless `manner_fertiliser ==
#' "urban"`) for the organic path. This function does not infer
#' `manner_fertiliser` from `fert_type` (e.g. which synthetic sub-type
#' `"Synthetic"` maps to is not determined by `fert_type` alone) and does
#' not silently fall back to `"ipcc"` or invent driver values: a missing
#' required column aborts naming exactly which column is absent. Each row is
#' dispatched to [calculate_manner_nh3()] individually (MANNER's dispatch is
#' inherently per-row categorical, not vectorizable across the coefficient
#' joins); this row-iteration is isolated to a small private helper.
#'
#' `method = "manner_default"` requires the same `manner_fertiliser` column
#' (restricted to the organic-manure keys, since the gross default only
#' covers that path) plus `rainfall_mm`, `irrigated`, `windspeed_ms`,
#' `system`, `temp_c` and `species` (unless `manner_fertiliser == "urban"`).
#' It does NOT require `technique` or `incorporation_delay_h`: those are
#' filled in from [manner_default_technique_mix] (see
#' [calculate_manner_nh3_default()]'s Details for the gross-assumption
#' reasoning), never invented per-row.
#'
#' Where the MANNER drivers come from: nothing in the package produces them
#' on any build path, so a caller supplies them. The one exception with a
#' reader is `windspeed_ms`, which [read_lpjml_wind()] returns as monthly
#' 0.5-degree GSWP3-W5E5 wind speed (m/s, the forcing LPJmL is driven with,
#' from the `lpjml-wind-isimip-1901-2019` input via `WHEP_WIND_DIR`). It is
#' not joined onto the N-loss rows by any builder: the grain of that join,
#' the averaging window (growing season, application month or year) and the
#' rule for years outside the wind record are open modelling choices
#' (whep#1078). [build_nitrogen_balance()] therefore refuses a MANNER
#' `nh3` method before assembling anything unless its
#' `data$n_balance_drivers` carries the columns listed above. `"ipcc"`
#' needs none of them.
#'
#' @param x A tibble with `n_input_t` (numeric, tonnes N) and `fert_type`.
#'   `method = "manner"` additionally requires `manner_fertiliser` and the
#'   driver columns listed in Details. `method = "manner_default"`
#'   additionally requires `manner_fertiliser` and the driver columns listed
#'   in Details, but NOT `technique`/`incorporation_delay_h`.
#' @param method `"manner"` (default, process-based, per-row), `"ipcc"`
#'   (Tier 1, global fraction) or `"manner_default"` (process-based organic
#'   path with a gross-assumption technique/incorporation-delay blend, no
#'   `technique`/`incorporation_delay_h` columns required).
#' @param example If `TRUE`, return a small fixture instead of computing
#'   from `x`. Defaults to `FALSE`.
#' @return `x` with `nh3_n_t` and `method_nh3` appended.
#' @export
#' @examples
#' calculate_nh3(example = TRUE)
calculate_nh3 <- function(x, method = "manner", example = FALSE) {
  method <- rlang::arg_match(method, c("manner", "ipcc", "manner_default"))
  if (isTRUE(example)) {
    return(.example_nh3())
  }
  if (method == "ipcc") {
    .nh3_ipcc(x)
  } else if (method == "manner_default") {
    .nh3_manner_default(x)
  } else {
    .nh3_manner(x)
  }
}

#' Estimate direct soil N2O emissions from applied nitrogen.
#'
#' @description
#' Four emission-factor regimes for direct nitrous oxide from nitrogen
#' applied to soil, `n2o_direct_n_t = n_input_t * ef`.
#'
#' `"ipcc2019"` (the default) is the IPCC 2019 Tier 1 wet/dry
#' disaggregation, keyed on each row's `moisture_regime` (`"wet"` or
#' `"dry"`), with every factor read from [ipcc_2019_soil_n_ef]:
#' * `EF1` (IPCC 2019 Refinement Vol 4 Ch 11 Table 11.1 (Updated) p. 11.13):
#'   0.016 for synthetic fertiliser and 0.006 for every other N input in wet
#'   climates, 0.005 for all N inputs in dry climates. Stamped
#'   `"ipcc2019_wet_dry"`.
#' * `EF1FR` (same table): 0.004, the aggregated flooded-rice factor, on
#'   every rice row in both moisture and both water regimes. No drainage
#'   layer exists for its continuous-flooding (0.003) versus drained (0.005)
#'   split, and upland rice, which the table prices with `EF1`, has no layer
#'   of its own either. Rice rows are those whose `item_cbs_code` is the CBS
#'   item of a production item classed `Crop_irrig == "Rice"` in
#'   [items_prod_full] (2807, rice and products). Stamped `"ipcc2019_ef1fr"`.
#' * `EF3PRP` (same table) on excreta deposited by grazing animals, as the
#'   N-weighted blend `excreta_cpp_share * EF3PRP,CPP + (1 -
#'   excreta_cpp_share) * EF3PRP,SO`, with `EF3PRP,CPP` 0.006 wet and 0.002
#'   dry for cattle, poultry and pigs and `EF3PRP,SO` 0.003 for sheep and
#'   other animals. Excreta keep this factor on a rice row too: Equation 11.1
#'   prices urine and dung deposited by grazing animals apart from the N
#'   applied to the crop. A row labelled by species reads its share from the
#'   label: 1 on `"Excreta_cattle_monog"`, 0 on `"Excreta_other"`. Stamped
#'   `"ipcc2019_ef3prp_blend"`.
#' * Excreta labelled so only as the fallback of a manure-type split
#'   (`method_excreta_species == "fallback_label_no_species"`) is collected
#'   manure applied to the land, not grazing deposition: it takes the factor
#'   of applied manure on its row (`EF1` for other N inputs, `EF1FR` on a
#'   rice item), whatever its missing species mix. Stamped
#'   `"ipcc2019_fallback_applied_manure"`.
#'
#' `"ipcc2019_atl_med"` is the earlier IPCC 2019 Tier 1 option keyed on the
#' Atlantic/Mediterranean `climate` instead: the [n2o_efs_disaggregated]
#' table's two climate-level rows (`irrig_type == "Tier_1"` for ATL,
#' `irrig_type == "Med_average"` for MED, 0.010 and 0.005), whatever the
#' input. `"aguilera"` is a finer Mediterranean-calibrated disaggregation
#' (Cayuela et al. 2017), selectable where its `irrig_type`/`fert_type`
#' granularity is available and its regional emission factors apply:
#' `ef * mf`, `ef` from [n2o_efs_disaggregated] on `(irrig_type, climate)`,
#' `mf` from [fertiliser_n2o_modifiers] on `(fert_type, climate)`. An
#' `"Excreta"` row, whose species are carried as a share rather than in its
#' label, takes `excreta_cpp_share * mf(Excreta_cattle_monog) + (1 -
#' excreta_cpp_share) * mf(Excreta_other)`, stamped
#' `"aguilera_excreta_blend"`; a fallback-labelled `"Excreta"` row, which is
#' collected manure applied to the land and not grazing deposition, takes
#' `mf(Solid)` (0.38 on MED, 0.60 on ATL), stamped
#' `"aguilera_fallback_applied_manure"`.
#' `"ipcc2006"` uses the [n2o_efs_ipcc2006]
#' table (IPCC 2006 Tier 1 defaults, flat 0.010 except flooded rice 0.003),
#' keyed like `"aguilera"` on `(irrig_type, climate)` with no `mf`.
#'
#' @details
#' `method = "ipcc2019"` aborts when `moisture_regime` is absent
#' (`whep_missing_moisture_regime`), missing on any row
#' (`whep_moisture_regime_na`, naming the count) or neither `"wet"` nor
#' `"dry"` (`whep_moisture_regime_value`): which class a row belongs to is
#' decided where the drivers are built, not here. An excreta row carrying N
#' without an `excreta_cpp_share` aborts (`whep_excreta_share_missing`), as
#' does a share outside 0 to 1 (`whep_excreta_share_invalid`) and a share on
#' a species-labelled row that contradicts its label
#' (`whep_excreta_share_label_conflict`); a row with no excreta N emits
#' nothing whatever its mix. A `fert_type` with no direct
#' factor in Equation 11.1 (`"BNF"`, `"Deposition"`) aborts
#' (`whep_soil_n2o_fert_type`).
#'
#' @param x A tibble with `n_input_t`. `method = "ipcc2019"` requires
#'   `fert_type` (`"Synthetic"`, `"Solid"`, `"Liquid"`, `"Human"`,
#'   `"Recycling"`, `"SOM"` or an excreta label: `"Excreta"`,
#'   `"Excreta_cattle_monog"`, `"Excreta_other"`), `item_cbs_code` and
#'   `moisture_regime`, plus `excreta_cpp_share` on `"Excreta"` rows.
#'   `"ipcc2019_atl_med"` requires `climate`. `"aguilera"` and `"ipcc2006"`
#'   require `climate` and `irrig_type`, and aguilera `fert_type` (and
#'   `excreta_cpp_share` on `"Excreta"` rows).
#' @param method `"ipcc2019"` (default, IPCC 2019 Tier 1 wet/dry),
#'   `"ipcc2019_atl_med"` (IPCC 2019 Tier 1 on ATL/MED), `"aguilera"`
#'   (Mediterranean-calibrated) or `"ipcc2006"` (IPCC 2006 Tier 1).
#' @param example If `TRUE`, return a small fixture instead of computing
#'   from `x`. Defaults to `FALSE`.
#' @return `x` with `n2o_direct_n_t` and `method_soil_n2o` appended.
#'   `method_soil_n2o` names the factor each row took: the method name, or
#'   for `"ipcc2019"` one of `"ipcc2019_wet_dry"`, `"ipcc2019_ef1fr"`,
#'   `"ipcc2019_ef3prp_blend"`, `"ipcc2019_fallback_applied_manure"`, and for
#'   `"aguilera"` `"aguilera_excreta_blend"` on blended excreta rows and
#'   `"aguilera_fallback_applied_manure"` on fallback-labelled ones.
#' @source IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for
#'   National Greenhouse Gas Inventories, Vol 4, Ch 11, Table 11.1
#'   (Updated), p. 11.13.
#' @export
#' @examples
#' calculate_soil_n2o(example = TRUE)
calculate_soil_n2o <- function(
  x,
  method = c("ipcc2019", "ipcc2019_atl_med", "aguilera", "ipcc2006"),
  example = FALSE
) {
  method <- rlang::arg_match(method)
  if (isTRUE(example)) {
    return(.example_soil_n2o())
  }
  switch(
    method,
    ipcc2019 = .soil_n2o_ipcc2019(x),
    ipcc2019_atl_med = .soil_n2o_ipcc2019_atl_med(x),
    aguilera = .soil_n2o_aguilera(x),
    ipcc2006 = .soil_n2o_ipcc2006(x)
  )
}

#' Estimate nitrate leaching, topsoil denitrification and indirect N2O.
#'
#' @description
#' Three methods for partitioning a nitrogen surplus into leached nitrate,
#' denitrified nitrogen and nitrate retained in the soil profile.
#'
#' `"meisinger_drainage"` (the default) is a transport-limited Meisinger
#' cascade. Each year the surplus `S` and the nitrate the profile retained the
#' year before, `C_in` (`retained_carried_in_n_t`), pass through it together:
#' the nitrate available for transport is
#' `A = (S + C_in) (1 - d) (1 - a_cn)`, with `d` the topsoil denitrification
#' share looked up from [meisinger_denitrification] by drainage class, soil
#' organic matter class, fertiliser class and climate, and `a_cn` the
#' carbon-to-nitrogen leaching attenuation. A transport factor `f` of the
#' annual drainage `D` sets how much of `A` leaves the root zone, and the
#' subsoil reduction `red` ([subsoil_no3_reduction]) acts on what leaves:
#' `no3_n_t = A f (1 - red)`, `retained_profile_n_t = A (1 - f)`, and
#' `denitrification_n_t = S + C_in - no3_n_t - retained_profile_n_t` (all the
#' gaseous and subsoil sinks). The retained nitrate is next year's `C_in` on
#' the same row key, so every row closes
#' `n_surplus_t + retained_carried_in_n_t = no3_n_t + denitrification_n_t +
#' retained_profile_n_t`.
#'
#' `"ipcc_fracleach"` is the IPCC 2019 Tier 1 leached fraction, keyed on
#' `moisture_regime` and `water_regime`, with every factor read from
#' [ipcc_2019_soil_n_ef]: `FracLEACH-(H)` is 0.24 on wet rows and on dry
#' irrigated rows and 0 on dry rainfed rows (IPCC 2019 Refinement Vol 4 Ch 11
#' Section 11.2.2.2 p. 11.25: 0.24 "for wet climates or in dry climate
#' regions where irrigation (other than drip irrigation) is used"), and `EF5`
#' is 0.011 (Table 11.3 (Updated) p. 11.26). All irrigation is treated as
#' non-drip, since no layer says which is drip: an upper-bound assumption for
#' dry irrigated land. On wet and dry irrigated rows the rest of the surplus
#' is booked as `denitrification_n_t`. On dry rainfed rows, where FracLEACH is
#' 0, it is booked as `retained_profile_n_t`, nitrogen left in the soil
#' profile, and `denitrification_n_t` is 0: FracLEACH says that surplus does
#' not leach, not that it denitrifies, and no method here estimates its fate.
#' That retained term is a flux of the year, not carried into the next one.
#' Each row is stamped `"ipcc_fracleach_wet"`,
#' `"ipcc_fracleach_dry_irrigated"` or
#' `"ipcc_fracleach_dry_rainfed_retained"`, and
#' `no3_n_t + denitrification_n_t + retained_profile_n_t` equals
#' `n_surplus_t`.
#'
#' `"image_gnm"` is the root-zone leaching and denitrification of IMAGE-GNM
#' (Beusen et al. 2015, Geosci. Model Dev. 8:4045-4067,
#' doi:10.5194/gmd-8-4045-2015, eqs. 8-14, pp. 4051-4052, and Table 2,
#' p. 4053). Surface runoff first carries off `surface_runoff_n_t`, a share of
#' the year's nitrogen input (eq. 9). The budget left leaches in the share
#' `f_leach`, `no3_n_t = f_leach (n_surplus_t - surface_runoff_n_t)`
#' (eq. 10), and the rest of it denitrifies (`f_den = 1 - f_leach`), with
#' `f_leach = [1 - min(f_climate + f_text + f_drain + f_soc, 1)] f_landuse`
#' (eq. 11). Eroded soil carries `erosion_n_t` from the soil's own organic
#' nitrogen (eq. 8), which no budget pays for. See the IMAGE-GNM section under
#' Details. IMAGE-GNM carries no nitrate from year to year, so every row
#' closes `max(n_surplus_t, surface_runoff_n_t) = surface_runoff_n_t +
#' no3_n_t + denitrification_n_t`.
#'
#' @details
#' ## Transport factor
#'
#' `transport` selects `f(D)`, with `D` the annual drainage (mm), `h` the
#' root-zone depth `root_depth_cm`, `phi` the porosity `soil_porosity` and
#' `theta_fc` the volumetric field capacity `soil_field_capacity`:
#'
#' - `"swat_mixing"` (default): the SWAT well-mixed layer,
#'   `f = 1 - exp(-D / ((1 - theta_e) phi h))` with `h` in mm and the anion
#'   exclusion fraction `theta_e = 0.5` (Neitsch et al. 2011, SWAT 2009
#'   Theoretical Documentation, TR-406, eqs. 4:2.1.2 to 4:2.1.8, pp. 269-270;
#'   `theta_e` default from the SWAT Input/Output File Documentation,
#'   Version 2012, ch. 22, p. 304).
#' - `"burns"`: Burns' displacement for a solute incorporated uniformly
#'   through the root zone, `f = (P / (P + theta_fc))^(h / 2)`, with `P` the
#'   drainage and `h` the depth, both in cm (Burns 1976, J. Agric. Sci.
#'   86:305-313, doi:10.1017/S0021859600054769).
#'
#' Every constant is read from [n_attenuation_constants]. The columns a
#' transport reads must be present and non-missing on every row:
#' `"swat_mixing"` reads `root_depth_cm` and `soil_porosity`, and `"burns"`
#' `root_depth_cm` and `soil_field_capacity`.
#'
#' ## Zero drainage and deep vadose storage
#'
#' Where `D <= 0` nothing is transported (`f = 0`). The year's own available
#' nitrate `S (1 - d) (1 - a_cn)` is retained, and the carried pool `C_in`
#' is held as deep vadose storage: it is neither denitrified nor leached that
#' year and re-enters the cascade in the next year that drains (Walvoord et
#' al. 2003, Science, doi:10.1126/science.1086435; Ascott et al. 2017, Nat.
#' Commun., doi:10.1038/s41467-017-01321-w). Such rows carry the field
#' `vadose_storage`.
#'
#' ## Drainage classes
#'
#' Drainage and soil organic matter bins are matched with the source's
#' strictly-open `s_min < s < s_max` filter: a value exactly on a shared bin
#' edge, or below the lowest bin, matches no bin and aborts via the
#' unmatched-row check rather than being pulled into an adjacent bin. The
#' top drainage bin is open: drainage of 3,000 mm and above takes the
#' `Very_high` share. Meisinger's `None` class (drainage below 0.1 mm/yr) is
#' set by `none_class`: `"retention"` (default) takes the `Very_low` row's
#' share, so what does not denitrify is retained or stored; `"waterlogged"`
#' keeps the table's literal `None` rows, which denitrify the whole surplus.
#'
#' Manure/organic rows (`fert_cat == "Manure"`, i.e. every `fert_type`
#' other than `"Synthetic"`) always join the Meisinger table's
#' `tillage == "Not_specified"` row regardless of `x$tillage`. Synthetic
#' rows join on `x$tillage`, but only `"Tillage"` exists in the source's
#' synthetic block: a synthetic row with `tillage == "No_tillage"` aborts.
#'
#' ## Carry-over across years
#'
#' With `carry_over_by`, rows are threaded per key in year order: each row's
#' `C_in` is the `retained_profile_n_t` of its key's previous row. A key's
#' first row starts at the steady state of that year's fluxes,
#' `C_in = S r / (1 - r)` with `r = (1 - d) (1 - a_cn) (1 - f)` the share of
#' the pool's input that stays (field `initial_pool_steady_state`); a key
#' whose first year does not drain has no finite steady state and starts at
#' 0 (field `initial_vadose_zero`). Later rows carry the field
#' `carried_pool`. A key absent from a year of the run keeps its pool on the
#' key: the pool is neither moved to another key nor dropped, and re-enters
#' when the key reappears. The row whose pool waits that way, the key's last
#' row before a run year it lacks, carries the field `pool_held_absent_key`.
#' Over a run, the sum of `n_surplus_t` equals the sums of `no3_n_t` and
#' `denitrification_n_t`, plus each key's last `retained_profile_n_t`, minus
#' each key's first `retained_carried_in_n_t`. Without `carry_over_by` every
#' row is its own key's first and only year.
#'
#' `method_leaching` joins these fields with `;`: `meisinger_drainage`; the
#' transport; `none_retention` or `none_waterlogged`; the pool field; and,
#' optionally, `vadose_storage` and `pool_held_absent_key`.
#'
#' ## IMAGE-GNM
#'
#' `method = "image_gnm"` follows Beusen et al. (2015), eqs. 8-14
#' (pp. 4051-4052) and Table 2 (p. 4053). Where the paper is silent or
#' ambiguous, or disagrees with the code, it follows the authors' model code
#' published with the paper (doi:10.5194/gmd-8-4045-2015-supplement,
#' `N_model/trunk/tools/`, the code that produced their results), as noted
#' below.
#'
#' - `f_climate = f_K T_r` (eq. 12), with
#'   `f_K = 7.94e12 exp(-74830 / (R K))` (eq. 13), `R = 8.3144` J/(mol K) and
#'   `K` the mean annual temperature `temp_c_annual_mean` + 273.15 (the
#'   authors' code adds 273). `f_K` is bounded at 1, as in the authors' code
#'   (line 97); the bound binds above about 30 degC.
#' - `T_r = tawc / q_eff` years (eq. 14). `tawc` is the total available
#'   water capacity of the top 1 m, `(soil_field_capacity -
#'   soil_wilting_point)` x 1,000 mm. The source takes the top 1 m "or less
#'   if thinner"; WHEP has no soil-thickness layer, so the full 1 m is used
#'   everywhere. `q_eff` is the excess water flow of eq. 5 (p. 4049), the
#'   water that infiltrates and recharges shallow and deep groundwater, here
#'   `drainage_mm`: WHEP's annual drainage is the deep seepage below the
#'   soil column, which is that excess water. Where `q_eff` is 0 the
#'   authors' code takes `T_r = 100` years (`trzd()`, lines 179-182).
#' - On cropland `T_r` is at most 1 year, as in the authors' code (lines
#'   377-378: crops must have enough water, for example from irrigation).
#'   The printed sentence (p. 4052) reads as raising a `T_r` below 1 to 1 on
#'   dry agricultural land; the code lowers every cropland `T_r` above 1 to
#'   1, and the code is followed.
#' - `f_text`, `f_drain` and `f_soc` are read from
#'   [image_gnm_leaching_factors] (Table 2) on `soil_texture_class`,
#'   `soil_drainage_class` and the topsoil organic carbon
#'   `soil_organic_carbon_pct` (% by weight; each organic-carbon class holds
#'   its lower bound, as in the authors' class code). `f_landuse` is 1 on
#'   `"Cropland"` and 0.36 on `"Grassland"` and `"Natural"` (p. 4052, citing
#'   Keuskamp et al. 2012).
#' - `surface_runoff_n_t` is `N_sro,rec` of eq. 9 (p. 4052), the nitrogen
#'   surface runoff carries off from the year's inputs before the budget
#'   leaches: `f_cal f_qsro(landuse) f_qsro N_inp`, with `f_cal = 0.3` and
#'   `f_qsro(landuse)` 1 on cropland, 0.25 on grassland and 0.125 on natural
#'   vegetation (Table 3, p. 4056; the authors' code,
#'   `n_surface_runoff.py` lines 98, 114 and 128, uses the same values).
#'   `f_qsro`, the product `f_qsro(slope) f_qsro(texture)` of eqs. 3-4
#'   (p. 4049), is the column `surface_runoff_fraction`: the authors' own
#'   0.5-degree layer, which their code reads instead of computing eq. 4
#'   (see [read_image_gnm_runoff()]). `N_inp` follows the authors' code: on
#'   cropland and grassland the input net of ammonia, `n_input_std_t -
#'   nh3_n_t`, floored at 0 (`n_surface_runoff.py` lines 57-58, 96 and 112);
#'   on natural vegetation the surplus `n_surplus_t` (line 128). Surface
#'   runoff is a share of the input, so it can exceed the surplus; eq. 10 is
#'   then negative, and the authors' code sets the budget to 0 (lines 106,
#'   122 and 136), as here.
#' - `erosion_n_t` is `N_sro,mem` of eq. 8 (p. 4052), the nitrogen of soil
#'   lost to erosion, computed as the authors' code does (`n_erosion.py`
#'   lines 47-55 and 88-135): an erodibility
#'   `(-1.5 + 17 / (1 + exp(2.3 - 6.1 S / 1000)))` times a texture factor
#'   (1.5 on medium and fine, 0.5 on the others), with `S` the median slope
#'   `slope_m_per_km`; a soil loss of 1,151, 99 and 29 t per km2 per unit
#'   erodibility on cropland, grassland and natural vegetation over `area_ha`;
#'   and N = soil loss x `soil_organic_carbon_pct` / C:N, with C:N 12, 14
#'   and 14 (p. 4052). The paper subtracts the whole of eq. 8 in eq. 10; the
#'   authors' code subtracts only the runoff from the recent inputs and adds
#'   the eroded N to the river load (`N-model.py` lines 233-243 and 307), so
#'   `erosion_n_t` is reported and taken from no budget.
#' - IMAGE-GNM keeps no root-zone nitrate from one year to the next, so
#'   `retained_profile_n_t` and `retained_carried_in_n_t` are 0 by
#'   construction, and `transport`, `none_class` and `carry_over_by` do not
#'   apply.
#'
#' Three parts of the authors' code are not reproduced. Its separate water
#' capacities of dominant and suitable soils: WHEP has one soil per cell.
#' Its excess water flow of eq. 5, `(1 - f_qsro(landuse) f_qsro)` times the
#' total runoff (`n_soil_denitrification.py` lines 356, 376 and 399): WHEP's
#' drainage is already below the surface. And its 3 mm net
#' precipitation below which the excess water is set to 0: net
#' precipitation is not a WHEP quantity. The code also interpolates `f_soc`
#' linearly from 0 at 0% to 0.3 at 6% organic carbon instead of the printed
#' classes; the printed Table 2 is used.
#'
#' `method_leaching` is `image_gnm;tawc_top_1m`, followed where they apply
#' by `no_drainage_residence_100yr` (`q_eff` of 0), `crop_residence_cap_1yr`
#' (a cropland `T_r` above 1 year held at 1), `temperature_effect_cap_1`
#' (an `f_K` above 1 held at 1) and `surface_runoff_exceeds_budget` (a
#' surface runoff above `n_surplus_t`, leaving a budget of 0), joined with
#' `;`.
#'
#' ## Errors
#'
#' - `whep_missing_leaching_driver` (also `whep_absent_input`): a driver
#'   column the transport reads is absent or holds a missing value, or
#'   `drainage_mm` is absent or missing.
#' - `whep_leaching_driver_value`: a non-positive `root_depth_cm`, a porosity
#'   or field capacity outside (0, 1], or a drainage at or below -0.1 mm.
#' - `whep_leaching_carry_key`: `carry_over_by` names an absent column, `x`
#'   has no `year`, or a key occurs twice in one year.
#'
#' For `method = "image_gnm"`:
#'
#' - `whep_missing_leaching_driver` (also `whep_absent_input`): a column it
#'   reads is absent or holds a missing value, or `drainage_mm` is absent or
#'   missing.
#' - `whep_leaching_driver_value`: a field capacity outside (0, 1], a
#'   wilting point outside (0, field capacity), a negative organic carbon,
#'   drainage, slope, area, input, ammonia or surplus, a non-finite
#'   temperature, or a surface runoff fraction outside [0, 1].
#' - `whep_gnm_runoff_supplied`: `x` already holds `surface_runoff_n_t`,
#'   which `"image_gnm"` computes; drop it rather than have it overwritten.
#' - `whep_leaching_class_value`: a texture, drainage or land-use label
#'   [image_gnm_leaching_factors] does not hold. The authors' code books an
#'   unknown texture or drainage class as 0; here it aborts, because a class
#'   that cannot be read is not a class.
#'
#' For `method = "ipcc_fracleach"`, a missing or invalid `moisture_regime`
#' aborts as in [calculate_soil_n2o()], and a dry row without a
#' `water_regime` aborts (`whep_missing_water_regime` for an absent column,
#' `whep_water_regime_value` for a missing or unknown value, naming the
#' count): dry irrigated and dry rainfed land leach 0.24 and 0 of their
#' surplus, so a dry row of unknown regime has no defensible fraction. Wet
#' rows need no `water_regime`. `transport`, `none_class` and
#' `carry_over_by` do not apply to it.
#'
#' @param x A tibble with `n_surplus_t`. `method = "meisinger_drainage"`
#'   requires `fert_type`, `climate`, `irrig_cat`, `land_use`, `cn_input`
#'   (may be `NA`), `tillage` (checked only for synthetic rows), `som_share`
#'   and the soil columns its `transport` reads (see Details), plus `year`
#'   with `carry_over_by`; `method = "ipcc_fracleach"` requires
#'   `moisture_regime` (`"wet"` or `"dry"`) and, on dry rows, `water_regime`
#'   (`"rainfed"` or `"irrigated"`); `method = "image_gnm"` requires
#'   `n_input_std_t`, `nh3_n_t`, `area_ha`, `surface_runoff_fraction`,
#'   `slope_m_per_km` (both from [read_image_gnm_runoff()]), `land_use`
#'   (`"Cropland"`, `"Grassland"` or `"Natural"`), `temp_c_annual_mean`,
#'   `soil_field_capacity`, `soil_wilting_point`, `soil_texture_class`,
#'   `soil_drainage_class` and `soil_organic_carbon_pct`, and no
#'   `surface_runoff_n_t` (see Details).
#' @param drainage_mm A numeric vector aligned to `x`'s rows giving annual
#'   drainage (mm), or a single string naming a column of `x` to use
#'   instead. Kept as a separate argument (rather than a static `x` column)
#'   because in the full pipeline it flows in from Module A's gridded water
#'   balance.
#' @param method `"meisinger_drainage"` (default), `"ipcc_fracleach"` or
#'   `"image_gnm"`.
#' @param transport The transport factor of `"meisinger_drainage"`:
#'   `"swat_mixing"` (default) or `"burns"`.
#' @param none_class How `"meisinger_drainage"` prices Meisinger's `None`
#'   drainage class: `"retention"` (default) or `"waterlogged"`.
#' @param carry_over_by Character vector of the columns that key a row
#'   across years (every key column but `year`), or `NULL` (default) to treat
#'   each row as a single year.
#' @param example If `TRUE`, return a small fixture instead of computing
#'   from `x`. Defaults to `FALSE`.
#' @return `x` with `no3_n_t`, `denitrification_n_t`,
#'   `retained_profile_n_t`, `n2o_indirect_no3_n_t` and `method_leaching`
#'   appended, plus `retained_carried_in_n_t` under `"meisinger_drainage"`
#'   and `"image_gnm"`, and `surface_runoff_n_t` and `erosion_n_t` under
#'   `"image_gnm"`.
#' @source IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for
#'   National Greenhouse Gas Inventories, Vol 4, Ch 11, Section 11.2.2.2,
#'   p. 11.25 (`FracLEACH-(H)`) and Table 11.3 (Updated), p. 11.26 (`EF5`).
#'   Beusen, A. H. W., Van Beek, L. P. H., Bouwman, A. F. et al. (2015).
#'   Coupling global models for hydrology and
#'   nutrient loading to simulate nitrogen and phosphorus retention in
#'   surface water - description of IMAGE-GNM and analysis of performance.
#'   Geosci. Model Dev. 8, 4045-4067. \doi{10.5194/gmd-8-4045-2015}.
#'   The transport sources are cited under Details.
#' @export
#' @examples
#' calculate_n_leaching(example = TRUE)
calculate_n_leaching <- function(
  x,
  drainage_mm = NULL,
  method = c("meisinger_drainage", "ipcc_fracleach", "image_gnm"),
  transport = c("swat_mixing", "burns"),
  none_class = c("retention", "waterlogged"),
  carry_over_by = NULL,
  example = FALSE
) {
  method <- rlang::arg_match(method)
  transport <- rlang::arg_match(transport)
  none_class <- rlang::arg_match(none_class)
  if (isTRUE(example)) {
    return(.example_n_leaching())
  }
  if (method == "ipcc_fracleach") {
    return(.leaching_ipcc_fracleach(x))
  }
  drainage <- .leaching_drainage_values(x, drainage_mm)
  if (method == "image_gnm") {
    return(.leaching_image_gnm(x, drainage))
  }
  options <- list(transport = transport, none_class = none_class)
  .leaching_meisinger(x, drainage, options, carry_over_by)
}

#' Estimate indirect N2O from volatilised ammonia.
#'
#' @description
#' Converts the ammonia-N already volatilised ([calculate_nh3()]'s
#' `nh3_n_t`) into indirect nitrous oxide. `"ipcc2019"` (the default) is
#' the IPCC 2019 Tier 1 wet/dry `EF4`, read from [ipcc_2019_soil_n_ef] on
#' each row's `moisture_regime`: 0.014 wet and 0.005 dry (IPCC 2019
#' Refinement Vol 4 Ch 11 Table 11.3 (Updated) p. 11.26), stamped
#' `"ipcc2019_wet_dry"`. `"ipcc2019_atl_med"` is the Atlantic/Mediterranean
#' option: Atlantic rows use the flat EF4 wet factor (`ef4_nh3_to_n2o_atl`,
#' 0.014) and touch no emission-factor lookup; Mediterranean rows use the
#' disaggregated [n2o_efs_disaggregated] `ef` on `(irrig_type, climate)`
#' alone (`NH3_MgN * N2O_EF`), WITHOUT the [fertiliser_n2o_modifiers] `mf`
#' that [calculate_soil_n2o()]'s `method = "aguilera"` applies to direct
#' N2O. It is stamped `"ipcc2019_atl_med"`.
#'
#' @details
#' `method = "ipcc2019"` aborts on an absent, missing or invalid
#' `moisture_regime` exactly as [calculate_soil_n2o()] does.
#'
#' @param x A tibble with `nh3_n_t`. `method = "ipcc2019"` requires
#'   `moisture_regime` (`"wet"` or `"dry"`); `method = "ipcc2019_atl_med"`
#'   requires `climate` and, for MED rows, the `irrig_type` column
#'   [n2o_efs_disaggregated] is keyed on.
#' @param method `"ipcc2019"` (default, IPCC 2019 Tier 1 wet/dry) or
#'   `"ipcc2019_atl_med"` (ATL flat EF4, MED disaggregated by irrigation).
#' @param example If `TRUE`, return a small fixture instead of computing
#'   from `x`. Defaults to `FALSE`.
#' @return `x` with `n2o_indirect_nh3_n_t` and `method_n2o_indirect_nh3`
#'   appended.
#' @source IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for
#'   National Greenhouse Gas Inventories, Vol 4, Ch 11, Table 11.3
#'   (Updated), p. 11.26.
#' @export
#' @examples
#' calculate_indirect_n2o_nh3(example = TRUE)
calculate_indirect_n2o_nh3 <- function(
  x,
  method = c("ipcc2019", "ipcc2019_atl_med"),
  example = FALSE
) {
  method <- rlang::arg_match(method)
  if (isTRUE(example)) {
    return(.example_indirect_n2o_nh3())
  }
  if (method == "ipcc2019") {
    .indirect_nh3_ipcc2019(x)
  } else {
    .indirect_nh3_atl_med(x)
  }
}

# ---- Private helpers: calculate_indirect_n2o_nh3 -------------------------

.indirect_nh3_ipcc2019 <- function(x) {
  .n_check_moisture_regime(x)
  ef4 <- dplyr::if_else(
    x$moisture_regime == "wet",
    .ipcc2019_soil_ef("EF4", "wet"),
    .ipcc2019_soil_ef("EF4", "dry")
  )
  x |>
    dplyr::mutate(
      n2o_indirect_nh3_n_t = .data$nh3_n_t * .env$ef4,
      method_n2o_indirect_nh3 = "ipcc2019_wet_dry"
    )
}

# The Atlantic/Mediterranean option, as before the wet/dry method existed.
.indirect_nh3_atl_med <- function(x) {
  .n_check_climate(x)
  med_rows <- which(x$climate == "MED")
  ef_med <- rep(NA_real_, nrow(x))
  if (length(med_rows) > 0L) {
    ef_med[med_rows] <- .indirect_nh3_ef_med(x[med_rows, , drop = FALSE])
  }
  ef4_atl <- .n_constant("ef4_nh3_to_n2o_atl")
  x |>
    dplyr::mutate(
      n2o_indirect_nh3_n_t = dplyr::if_else(
        .data$climate == "ATL",
        .data$nh3_n_t * ef4_atl,
        .data$nh3_n_t * .env$ef_med
      ),
      method_n2o_indirect_nh3 = "ipcc2019_atl_med"
    )
}

# ---- Private helpers: calculate_nh3 ------------------------------------

# Where each MANNER driver can come from, said wherever a missing one aborts.
# Only windspeed_ms has an in-package reader, and it is not wired into any
# builder (whep#1078): which grain, season and out-of-record rule to join it
# on are open modelling choices. Every other driver has no producer at all.
.nh3_driver_provenance <- function() {
  c(
    i = "Only {.field windspeed_ms} has a reader in whep,
         {.fn read_lpjml_wind} (monthly, 0.5-degree, needs
         {.envvar WHEP_WIND_DIR}); no function supplies the other MANNER
         drivers.",
    i = "{.fn build_n_loss_drivers} supplies the wet/dry and ATL/MED
         drivers, not these.",
    i = "Supply them yourself, or use the IPCC Tier 1 method,
         {.code method = \"ipcc\"}, which needs only {.field fert_type}."
  )
}

# Entry-time form of the driver check, for a caller that would otherwise
# reach calculate_nh3() only after an expensive assembly: build_nitrogen_
# balance() used to build every input (the NPP chain, build_n_inputs()) and
# only then abort inside calculate_nh3() on the default method (whep#1078).
# `drivers` is the table the caller will join onto the loss rows. Recycling
# and SOM rows are dropped first because .nb_losses() never sends them to
# calculate_nh3(), so an NA manner_fertiliser there must not demand drivers.
.nh3_check_drivers_supplied <- function(method, drivers, arg = "drivers") {
  if (method == "ipcc") {
    return(invisible(NULL))
  }
  if (is.null(drivers)) {
    cli::cli_abort(
      c(
        "{.code nh3 = {.val {method}}} needs MANNER driver columns, but
         {.arg {arg}} was not supplied.",
        .nh3_driver_provenance()
      ),
      class = "whep_error_nh3_drivers_absent"
    )
  }
  if (rlang::has_name(drivers, "fert_type")) {
    drivers <- dplyr::filter(
      drivers,
      !.data$fert_type %in% c("Recycling", "SOM")
    )
  }
  if (method == "manner") {
    .nh3_manner_require_columns(drivers, arg)
  } else {
    .nh3_manner_default_req_cols(drivers, arg)
  }
  invisible(NULL)
}

# IPCC Tier 1: a single global fraction per fert_type, from
# n_attenuation_constants.
.nh3_ipcc <- function(x) {
  frac_synth <- .n_constant("nh3_frac_synthetic")
  frac_org <- .n_constant("nh3_frac_organic")
  x |>
    dplyr::mutate(
      nh3_n_t = data.table::fcase(
        .data$fert_type == "Synthetic"             ,
        .data$n_input_t * frac_synth               ,
        .data$fert_type %in% c("Recycling", "SOM") ,
                                                 0 ,
        default = .data$n_input_t * frac_org
      ),
      method_nh3 = "ipcc"
    )
}

# MANNER dispatch: requires manner_fertiliser plus every driver column its
# fertiliser path needs, aborting with the exact missing column rather than
# guessing a mapping or falling back silently. Read nh3_n_t directly from the
# model result: for organic fertilisers it includes the manure-specific
# inorganic-N fraction and therefore is not simply ef * n_input_t.
.nh3_manner <- function(x) {
  .nh3_manner_require_columns(x)
  nh3_n_t <- purrr::pmap_dbl(x, .nh3_manner_row_nh3)
  x |>
    dplyr::mutate(
      nh3_n_t = .env$nh3_n_t,
      method_nh3 = "manner"
    )
}

.nh3_manner_synthetic_cols <- c(
  "soil_ph",
  "rate_kg_ha",
  "rainfall_mm",
  "irrigated",
  "temp_c",
  "temp_c_annual_mean"
)
.nh3_manner_organic_cols <- c(
  "rainfall_mm",
  "irrigated",
  "windspeed_ms",
  "technique",
  "system",
  "temp_c",
  "incorporation_delay_h"
)

.nh3_manner_require_columns <- function(x, arg = "x") {
  if (!rlang::has_name(x, "manner_fertiliser")) {
    cli::cli_abort(
      c(
        "{.arg {arg}} is missing required column {.field manner_fertiliser}.",
        i = paste0(
          "calculate_nh3(method = \"manner\") requires the exact ",
          "calculate_manner_nh3() fertiliser key on every row."
        ),
        .nh3_driver_provenance()
      ),
      class = "whep_error_nh3_missing_driver"
    )
  }
  synthetic <- c("Urea", "AN", "CAN", "AS")
  needs_species <- !all(x$manner_fertiliser %in% c(synthetic, "urban"))
  required <- c(
    if (any(x$manner_fertiliser %in% synthetic)) {
      .nh3_manner_synthetic_cols
    },
    if (any(!x$manner_fertiliser %in% synthetic)) .nh3_manner_organic_cols,
    if (needs_species) "species"
  )
  missing <- required[!purrr::map_lgl(required, \(col) rlang::has_name(x, col))]
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} is missing required MANNER driver column{?s} {.field {missing}}.",
        i = "calculate_nh3(method = \"manner\") never invents driver values.",
        .nh3_driver_provenance()
      ),
      class = "whep_error_nh3_missing_driver"
    )
  }
}

# One MANNER call per row; dispatch stays isolated here rather than leaking
# a row-by-row style into the rest of this file.
.nh3_manner_row_nh3 <- function(...) {
  row <- list(...)
  synthetic <- c("Urea", "AN", "CAN", "AS")
  drivers <- if (row$manner_fertiliser %in% synthetic) {
    row[.nh3_manner_synthetic_cols]
  } else if (row$manner_fertiliser == "urban") {
    row[.nh3_manner_organic_cols]
  } else {
    c(row[.nh3_manner_organic_cols], list(species = row$species))
  }
  calculate_manner_nh3(
    n_applied_t = row$n_input_t,
    fertiliser = row$manner_fertiliser,
    drivers = drivers
  )$nh3_n_t
}

# manner_default dispatch: same organic-path driver contract as .nh3_manner
# MINUS technique/incorporation_delay_h, which calculate_manner_nh3_default()
# fills in from the gross-assumption blend instead. Reads nh3_n_t directly
# off each row's calculate_manner_nh3_default() call rather than
# re-deriving via ef * n_input_t: the blended ef already excludes the
# inorganic_n_fraction scaling (matching calculate_manner_nh3()'s organic
# path, where ef and nh3_n_t are not related by a plain n_input_t product).
.nh3_manner_default <- function(x) {
  .nh3_manner_default_req_cols(x)
  nh3_n_t <- purrr::pmap_dbl(x, .nh3_manner_default_row_nh3)
  x |>
    dplyr::mutate(
      nh3_n_t = .env$nh3_n_t,
      method_nh3 = "manner_default"
    )
}

.nh3_manner_default_cols <- c(
  "rainfall_mm",
  "irrigated",
  "windspeed_ms",
  "system",
  "temp_c"
)

.nh3_manner_default_req_cols <- function(x, arg = "x") {
  if (!rlang::has_name(x, "manner_fertiliser")) {
    cli::cli_abort(
      c(
        "{.arg {arg}} is missing required column {.field manner_fertiliser}.",
        i = paste0(
          "calculate_nh3(method = \"manner_default\") requires the exact ",
          "calculate_manner_nh3_default() fertiliser key on every row."
        ),
        .nh3_driver_provenance()
      ),
      class = "whep_error_nh3_missing_driver"
    )
  }
  needs_species <- !all(x$manner_fertiliser == "urban")
  required <- c(
    .nh3_manner_default_cols,
    if (needs_species) "species"
  )
  missing <- required[!purrr::map_lgl(required, \(col) rlang::has_name(x, col))]
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} is missing required MANNER driver column{?s} {.field {missing}}.",
        i = "calculate_nh3(method = \"manner_default\") never invents driver values.",
        .nh3_driver_provenance()
      ),
      class = "whep_error_nh3_missing_driver"
    )
  }
}

# One calculate_manner_nh3_default() call per row.
.nh3_manner_default_row_nh3 <- function(...) {
  row <- list(...)
  drivers <- if (row$manner_fertiliser == "urban") {
    row[.nh3_manner_default_cols]
  } else {
    c(row[.nh3_manner_default_cols], list(species = row$species))
  }
  calculate_manner_nh3_default(
    n_applied_t = row$n_input_t,
    fertiliser = row$manner_fertiliser,
    drivers = drivers
  )$nh3_n_t
}

# ---- Private helpers: calculate_soil_n2o -------------------------------

# aguilera: full disaggregated ef * mf, exposed separately so
# calculate_indirect_n2o_nh3() can reuse the raw ef*mf product without a
# second EF join.
.soil_n2o_aguilera <- function(x) {
  fallback <- .n_excreta_fallback(x, x$fert_type)
  blended <- x$fert_type %in% "Excreta" & !fallback
  x |>
    dplyr::mutate(
      n2o_direct_n_t = .data$n_input_t * .soil_n2o_ef_mf_aguilera(x),
      method_soil_n2o = dplyr::case_when(
        .env$fallback ~ "aguilera_fallback_applied_manure",
        .env$blended ~ "aguilera_excreta_blend",
        .default = "aguilera"
      )
    )
}

# An "Excreta" row carries its species as `excreta_cpp_share` rather than in
# its label, so its modifier is the N-weighted blend of the cattle and
# monogastric row (the CPP animals of IPCC 2019 Table 11.1) and the
# other-animal row of fertiliser_n2o_modifiers. Rows already labelled by
# species keep their own modifier. A fallback-labelled "Excreta" row carries
# no species at all and is collected manure applied to the land, not grazing
# deposition: it takes the Solid modifier of fertiliser_n2o_modifiers.
.soil_n2o_ef_mf_aguilera <- function(x) {
  ef <- .soil_n2o_ef_disaggregated(x)
  .soil_n2o_check_ef(ef)
  fert_type <- .human_legacy_fert_type(x$fert_type)
  fallback <- .n_excreta_fallback(x, fert_type)
  blended <- fert_type == "Excreta" & !fallback
  share <- .n_excreta_share(x, blended, fert_type)
  mf_cpp <- .soil_n2o_mf("Excreta_cattle_monog", x$climate)
  mf_other <- .soil_n2o_mf("Excreta_other", x$climate)
  mf_applied <- .soil_n2o_mf("Solid", x$climate)
  mf <- dplyr::case_when(
    fallback ~ mf_applied,
    blended ~ share * mf_cpp + (1 - share) * mf_other,
    .default = .soil_n2o_mf(fert_type, x$climate)
  )
  .soil_n2o_check_mf(x, mf)
  ef * mf
}

.soil_n2o_mf <- function(fert_type, climate) {
  tibble::tibble(fert_type = fert_type, climate = climate) |>
    dplyr::left_join(
      whep::fertiliser_n2o_modifiers,
      by = c("fert_type", "climate"),
      relationship = "many-to-one"
    ) |>
    dplyr::pull("mf")
}

# The disaggregated (irrig_type, climate) ef, joined without any NA guard so
# both the aguilera direct-N2O path (which adds mf) and the MED indirect-NH3
# path (which uses ef alone) can share one join.
.soil_n2o_ef_disaggregated <- function(x) {
  x |>
    dplyr::select("irrig_type", "climate") |>
    dplyr::left_join(
      whep::n2o_efs_disaggregated,
      by = c("irrig_type", "climate")
    ) |>
    dplyr::pull("ef")
}

# MED indirect NH3-N2O uses the disaggregated ef alone (no mf), so the ATL
# branch never touches an emission factor; ATL rows keep their NA ef here and
# it is discarded by calculate_indirect_n2o_nh3()'s ATL if_else branch.
.indirect_nh3_ef_med <- function(x) {
  if (!rlang::has_name(x, "irrig_type")) {
    cli::cli_abort(c(
      "Mediterranean rows require {.field irrig_type}.",
      i = paste0(
        "Atlantic rows use the flat EF4 and do not require this column; ",
        "Mediterranean rows are keyed by irrig_type and climate."
      )
    ))
  }
  ef <- .soil_n2o_ef_disaggregated(x)
  if (anyNA(ef)) {
    cli::cli_abort(c(
      "{.field n2o_efs_disaggregated} has no factor for a Mediterranean row.",
      i = "Check irrig_type/climate against whep::n2o_efs_disaggregated."
    ))
  }
  ef
}

# Any missing factor in the direct-N2O path is unsupported. The source table's
# ATL non-Tier_1/Flooded rows deliberately carry NA, while every supported MED
# irrigation key has a finite factor; neither case may silently propagate NA.
# The separate indirect-NH3 path still bypasses this lookup entirely for ATL.
.soil_n2o_check_ef <- function(ef) {
  if (anyNA(ef)) {
    cli::cli_abort(c(
      "{.field n2o_efs_disaggregated} has no direct N2O factor for a row.",
      i = "Check irrig_type/climate against whep::n2o_efs_disaggregated."
    ))
  }
}

# An NA mf must abort, not multiply the ef into a silent NA that a downstream
# na.rm sum would drop: a missing modifier is a real data gap
# (fertiliser_n2o_modifiers) that has to be visible.
.soil_n2o_check_mf <- function(x, mf) {
  if (anyNA(mf)) {
    bad <- unique(x$fert_type[is.na(mf)])
    cli::cli_abort(c(
      "{.field fertiliser_n2o_modifiers} has no modifier for \\
       fert_type{?s} {.val {bad}}.",
      i = paste0(
        "calculate_soil_n2o(method = \"aguilera\") never treats a missing ",
        "modifier as zero; add the {.field mf} value to ",
        "{.code whep::fertiliser_n2o_modifiers}."
      )
    ))
  }
}

# ipcc2019: the IPCC 2019 Tier 1 wet/dry factors of Table 11.1 (Updated),
# p. 11.13, read from ipcc_2019_soil_n_ef. Excreta deposited by grazing
# animals take the EF3PRP blend of their species share, every other input on
# a rice item takes EF1FR, and the rest take EF1 split by moisture regime
# (and, in wet climates, synthetic versus other N). Excreta labelled so only
# as the fallback of a manure-type split is collected manure applied to the
# land, so it is priced as applied manure, an "other" N input.
.soil_n2o_ipcc2019 <- function(x) {
  .n_check_moisture_regime(x)
  .soil_n2o_check_ipcc2019_cols(x)
  fert_type <- .human_legacy_fert_type(x$fert_type)
  .soil_n2o_check_fert_type(fert_type)
  fallback <- .n_excreta_fallback(x, fert_type)
  excreta <- fert_type %in% .n_excreta_fert_types() & !fallback
  rice <- !excreta & x$item_cbs_code %in% .n_rice_item_cbs_codes()
  share <- .n_excreta_share(x, excreta, fert_type)
  wet <- x$moisture_regime == "wet"
  ef <- dplyr::case_when(
    excreta ~ .soil_n2o_ef3prp(share, wet),
    rice ~ .ipcc2019_soil_ef("EF1FR", "aggregated"),
    wet & fert_type == "Synthetic" ~ .ipcc2019_soil_ef("EF1", "synthetic_wet"),
    wet ~ .ipcc2019_soil_ef("EF1", "other_wet"),
    .default = .ipcc2019_soil_ef("EF1", "all_dry")
  )
  method <- dplyr::case_when(
    fallback ~ "ipcc2019_fallback_applied_manure",
    excreta ~ "ipcc2019_ef3prp_blend",
    rice ~ "ipcc2019_ef1fr",
    .default = "ipcc2019_wet_dry"
  )
  x |>
    dplyr::mutate(
      n2o_direct_n_t = .data$n_input_t * .env$ef,
      method_soil_n2o = .env$method
    )
}

# The N-weighted EF3PRP of a row whose share `share` of excreta N comes from
# cattle, poultry and pigs (EF3PRP,CPP, wet or dry) and the rest from sheep
# and other animals (EF3PRP,SO, one value for both climates).
.soil_n2o_ef3prp <- function(share, wet) {
  cpp <- dplyr::if_else(
    wet,
    .ipcc2019_soil_ef("EF3PRP_CPP", "wet"),
    .ipcc2019_soil_ef("EF3PRP_CPP", "dry")
  )
  share * cpp + (1 - share) * .ipcc2019_soil_ef("EF3PRP_SO", "aggregated")
}

.soil_n2o_check_ipcc2019_cols <- function(x) {
  missing <- setdiff(c("fert_type", "item_cbs_code"), names(x))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "{.arg x} is missing required column{?s} {.field {missing}}.",
        i = "{.code method = \"ipcc2019\"} needs {.field fert_type} to tell
             synthetic from other N and excreta, and {.field item_cbs_code}
             to find rice."
      ),
      class = "whep_soil_n2o_columns"
    )
  }
}

# The inputs Equation 11.1 prices directly. BNF has no direct factor since
# the 2006 Guidelines, and atmospheric deposition is an indirect source.
.soil_n2o_check_fert_type <- function(fert_type) {
  known <- c(
    "Synthetic",
    "Solid",
    "Liquid",
    "Human",
    "Recycling",
    "SOM",
    .n_excreta_fert_types()
  )
  bad <- unique(fert_type[is.na(fert_type) | !fert_type %in% known])
  if (length(bad) > 0L) {
    cli::cli_abort(
      c(
        "No IPCC 2019 direct N2O factor for {.field fert_type}
         {.val {bad}}.",
        i = "Expected one of {.val {known}}."
      ),
      class = "whep_soil_n2o_fert_type"
    )
  }
}

# ipcc2019_atl_med: reuse n2o_efs_disaggregated's two climate-level rows
# (Tier_1 for ATL, Med_average for MED) as the single source of truth for the
# flat 0.010/0.005 factors -- the same 0.010 documented as EF1 in
# build_crop_soil_n2o_extension(), not re-hardcoded here.
.soil_n2o_ipcc2019_atl_med <- function(x) {
  .n_check_climate(x)
  ef_atl <- .n2o_disaggregated_row("Tier_1", "ATL")
  ef_med <- .n2o_disaggregated_row("Med_average", "MED")
  x |>
    dplyr::mutate(
      n2o_direct_n_t = .data$n_input_t *
        dplyr::if_else(.data$climate == "ATL", ef_atl, ef_med),
      method_soil_n2o = "ipcc2019_atl_med"
    )
}

.n2o_disaggregated_row <- function(.irrig_type, .climate) {
  whep::n2o_efs_disaggregated |>
    dplyr::filter(.data$irrig_type == .irrig_type, .data$climate == .climate) |>
    dplyr::pull("ef")
}

# ipcc2006: flat IPCC 2006 Tier 1 defaults, keyed like aguilera but with no
# mf multiplier.
.soil_n2o_ipcc2006 <- function(x) {
  ef <- x |>
    dplyr::select("irrig_type", "climate") |>
    dplyr::left_join(whep::n2o_efs_ipcc2006, by = c("irrig_type", "climate")) |>
    dplyr::pull("ef")
  if (anyNA(ef)) {
    cli::cli_abort(c(
      "{.field n2o_efs_ipcc2006} has no direct N2O factor for a row.",
      i = "Check irrig_type/climate against whep::n2o_efs_ipcc2006."
    ))
  }
  x |>
    dplyr::mutate(
      n2o_direct_n_t = .data$n_input_t * .env$ef,
      method_soil_n2o = "ipcc2006"
    )
}

# ---- Private helpers: calculate_n_leaching -----------------------------

.leaching_drainage_values <- function(x, drainage_mm) {
  if (is.character(drainage_mm) && length(drainage_mm) == 1) {
    x[[drainage_mm]]
  } else {
    drainage_mm
  }
}

# ipcc_fracleach: FracLEACH-(H) of IPCC 2019 Section 11.2.2.2 (p. 11.25) and
# EF5 of Table 11.3 (p. 11.26), read from ipcc_2019_soil_n_ef. Dry land
# leaches only where it is irrigated, and every irrigated row is taken as
# non-drip, the upper bound, because no layer says which irrigation is drip.
.leaching_ipcc_fracleach <- function(x) {
  .n_check_moisture_regime(x)
  wet <- x$moisture_regime == "wet"
  dry_irrigated <- .leaching_dry_irrigated(x)
  frac_leach <- dplyr::case_when(
    wet ~ .ipcc2019_soil_ef("FracLEACH_H", "wet"),
    dry_irrigated ~ .ipcc2019_soil_ef("FracLEACH_H", "dry_irrigated_non_drip"),
    .default = .ipcc2019_soil_ef("FracLEACH_H", "dry")
  )
  method <- dplyr::case_when(
    wet ~ "ipcc_fracleach_wet",
    dry_irrigated ~ "ipcc_fracleach_dry_irrigated",
    .default = "ipcc_fracleach_dry_rainfed_retained"
  )
  dry_rainfed <- !wet & !dry_irrigated
  ef5 <- .ipcc2019_soil_ef("EF5", "aggregated")
  # FracLEACH 0 on dry rainfed land says the surplus does not leach, not that
  # it denitrifies: no method here estimates its fate, so it is booked as
  # retained in the profile and its denitrification is 0, a term not
  # estimated rather than a measured zero. Elsewhere the non-leached surplus
  # is denitrification, as in the Meisinger cascade, and nothing is retained.
  no3 <- x$n_surplus_t * frac_leach
  retained <- dplyr::if_else(dry_rainfed, x$n_surplus_t - no3, 0)
  x |>
    dplyr::mutate(
      no3_n_t = .env$no3,
      denitrification_n_t = .data$n_surplus_t - .env$no3 - .env$retained,
      retained_profile_n_t = .env$retained,
      n2o_indirect_no3_n_t = .data$no3_n_t * ef5,
      method_leaching = .env$method
    )
}

# TRUE on the dry irrigated rows. A dry row must say which water regime it
# is: the two leach 0.24 and 0 of their surplus, so no default is defensible.
.leaching_dry_irrigated <- function(x) {
  dry <- x$moisture_regime == "dry"
  if (!any(dry)) {
    return(dry)
  }
  if (!rlang::has_name(x, "water_regime")) {
    cli::cli_abort(
      c(
        "{.arg x} has {sum(dry)} dry row{?s} but no {.field water_regime}.",
        i = "IPCC 2019 leaches dry land only where it is irrigated; build
             the balance at grid resolution with the rainfed/irrigated
             split, or supply {.field water_regime}."
      ),
      class = c("whep_missing_water_regime", "whep_absent_input")
    )
  }
  regime <- x$water_regime
  bad <- dry & !regime %in% c("rainfed", "irrigated")
  if (any(bad)) {
    cli::cli_abort(
      c(
        "{sum(bad)} dry row{?s} of {.arg x} {?has/have} no valid
         {.field water_regime}: {.val {unique(regime[bad])}}.",
        i = "Expected {.val rainfed} or {.val irrigated}."
      ),
      class = "whep_water_regime_value"
    )
  }
  dry & regime == "irrigated"
}

# meisinger_drainage: the transport-limited cascade. Per row, the surplus and
# the carried-in pool lose the topsoil denitrification share and the C:N
# attenuation; the transport factor moves part of the rest out of the root
# zone, where subsoil reduction acts on it, and the remainder is retained and
# carried into the key's next year.
.leaching_meisinger <- function(x, drainage_mm, options, carry_over_by) {
  drainage_mm <- .leaching_check_drainage(drainage_mm, nrow(x))
  .leaching_check_soil(x, options$transport)
  rates <- .leaching_rates(x, drainage_mm, options)
  pool <- .leaching_thread_pool(x, rates, carry_over_by)
  flows <- .leaching_cascade(x$n_surplus_t, pool$carried_in, rates)
  ef5 <- .n_constant("ef5_no3_to_n2o")
  x |>
    dplyr::mutate(
      no3_n_t = flows$leached,
      denitrification_n_t = flows$denitrified,
      retained_profile_n_t = flows$retained,
      retained_carried_in_n_t = pool$carried_in,
      n2o_indirect_no3_n_t = .data$no3_n_t * ef5,
      method_leaching = .leaching_stamp(options, rates$drained, pool)
    )
}

# Synthetic rows must supply tillage == "Tillage" (the Meisinger table's
# synthetic block has no No_tillage rows); manure rows always join
# Not_specified regardless of x$tillage, so their tillage value is never
# checked.
.leaching_check_tillage <- function(x, fert_cat) {
  bad <- fert_cat == "Synthetic" & x$tillage != "Tillage"
  if (any(bad)) {
    cli::cli_abort(c(
      "Synthetic-fertiliser rows require {.code tillage == \"Tillage\"}.",
      i = paste0(
        "meisinger_denitrification has no {.val No_tillage} rows for ",
        "{.val Synthetic}; {.val No_tillage} is unsupported here."
      )
    ))
  }
}

.leaching_denit_share <- function(x, fert_cat, drainage_mm, none_class) {
  tillage_join <- dplyr::if_else(
    fert_cat == "Synthetic",
    x$tillage,
    "Not_specified"
  )
  som_content <- .bin_range(
    x$som_share,
    whep::som_ranges,
    "som_content",
    "som_min",
    "som_max"
  )
  drainage_rate <- .bin_range(
    drainage_mm,
    .leaching_drainage_bins(),
    "drainage_rate",
    "s_min",
    "s_max"
  ) |>
    .leaching_none_class(none_class)
  tibble::tibble(
    fert_cat = fert_cat,
    tillage = tillage_join,
    som_content = som_content,
    drainage_rate = drainage_rate,
    climate = x$climate
  ) |>
    dplyr::left_join(
      whep::meisinger_denitrification,
      by = c(
        "fert_cat",
        "tillage",
        "som_content",
        "drainage_rate",
        "climate"
      )
    ) |>
    dplyr::pull("denit_share") |>
    .leaching_check_na(
      "meisinger_denitrification",
      "fert_cat/tillage/som_content/drainage_rate/climate"
    )
}

# The top drainage bin is open: drainage at or beyond its tabulated 3,000 mm
# upper bound is still Very_high rather than an unmatched row. The other
# bins keep the source's strictly-open edges.
.leaching_drainage_bins <- function() {
  dplyr::mutate(
    whep::drainage_ranges,
    s_max = dplyr::if_else(
      .data$s_max == max(.data$s_max),
      Inf,
      .data$s_max
    )
  )
}

# Meisinger's None class (drainage below 0.1 mm/yr) denitrifies the whole
# surplus in the literal table ("waterlogged"). Under "retention" it takes
# the Very_low row's share, so what does not denitrify is retained or
# stored rather than booked as denitrified.
.leaching_none_class <- function(drainage_rate, none_class) {
  if (none_class == "waterlogged") {
    return(drainage_rate)
  }
  dplyr::if_else(drainage_rate %in% "None", "Very_low", drainage_rate)
}

# meisinger_denitrification and subsoil_no3_reduction are lookup tables with
# real coverage gaps (e.g. fert_type "Recycling" is absent from
# subsoil_no3_reduction); an unmatched row must abort, not silently
# propagate NA through no3_n_t/denitrification_n_t/n2o_indirect_no3_n_t.
.leaching_check_na <- function(values, table_name, join_desc) {
  if (anyNA(values)) {
    cli::cli_abort(c(
      "{.field {table_name}} has no matching row for {sum(is.na(values))} \\
       row{?s} of {.arg x}.",
      i = "Check the {join_desc} combination against {.code whep::{table_name}}."
    ))
  }
  values
}

# Bin a numeric vector into a labelled class via the source's strictly-open
# min < v < max filter: a value on a shared bin edge or
# outside the covered range matches no bin and returns NA (the source drops
# such rows), rather than being pulled into an adjacent or ceiling bin.
.bin_range <- function(values, ranges, label_col, min_col, max_col) {
  purrr::map_chr(values, function(v) {
    hit <- ranges[
      v > ranges[[min_col]] & v < ranges[[max_col]],
      ,
      drop = FALSE
    ]
    hit[[label_col]][1]
  })
}

.leaching_a_cn <- function(x) {
  min_cn <- .n_constant("a_cn_min_cn")
  max_a_cn <- .n_constant("a_cn_max")
  span <- dplyr::if_else(
    x$land_use == "Other",
    .n_constant("a_cn_span_other"),
    .n_constant("a_cn_span")
  )
  cn <- dplyr::coalesce(x$cn_input, min_cn)
  pmin(max_a_cn, pmax(0, (cn - min_cn) / span))
}

.leaching_no3_red <- function(x) {
  keys <- dplyr::select(x, "fert_type", "climate", "irrig_cat")
  keys$fert_type <- .human_legacy_fert_type(keys$fert_type)
  keys |>
    dplyr::left_join(
      whep::subsoil_no3_reduction,
      by = c("fert_type", "climate", "irrig_cat")
    ) |>
    dplyr::pull("no3_red") |>
    .leaching_check_na("subsoil_no3_reduction", "fert_type/climate/irrig_cat")
}

# ---- Private helpers: Meisinger transport and carry-over -----------------

# Drainage is the transport driver, so a row without it has no partition. The
# lowest Meisinger bin (None) starts above -0.1 mm: a drainage at or below it
# is outside the method. A single value applies to every row.
.leaching_check_drainage <- function(drainage_mm, n) {
  if (is.null(drainage_mm) || anyNA(drainage_mm)) {
    cli::cli_abort(
      c(
        "{.code meisinger_drainage} needs an annual drainage on every row.",
        i = "{.arg drainage_mm} is absent or missing on
             {sum(is.na(drainage_mm))} row{?s}; supply it as a vector aligned
             to {.arg x} or as the name of a column of {.arg x}."
      ),
      class = c("whep_missing_leaching_driver", "whep_absent_input")
    )
  }
  drainage <- vctrs::vec_recycle(drainage_mm, n, x_arg = "drainage_mm")
  floor_mm <- min(whep::drainage_ranges$s_min)
  low <- drainage <= floor_mm
  if (any(low)) {
    cli::cli_abort(
      c(
        "{sum(low)} drainage value{?s} {?is/are} at or below
         {floor_mm} mm, outside every Meisinger drainage class.",
        i = "Lowest value: {.val {min(drainage)}} mm."
      ),
      class = "whep_leaching_driver_value"
    )
  }
  drainage
}

# The soil columns each transport reads (see calculate_n_leaching()).
.leaching_soil_cols <- function(transport) {
  switch(
    transport,
    swat_mixing = c("root_depth_cm", "soil_porosity"),
    burns = c("root_depth_cm", "soil_field_capacity")
  )
}

# A transport without its soil columns has no defensible value, so an absent
# column or a missing value aborts here instead of becoming a zero transport.
.leaching_check_soil <- function(x, transport) {
  needed <- .leaching_soil_cols(transport)
  present <- needed[rlang::has_name(x, needed)]
  with_na <- present[purrr::map_lgl(present, \(col) anyNA(x[[col]]))]
  missing <- c(setdiff(needed, present), with_na)
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "The {.val {transport}} transport reads {.field {missing}}, absent
         from {.arg x} or missing on some row.",
        i = "Supply {.field {needed}} with the leaching drivers."
      ),
      class = c("whep_missing_leaching_driver", "whep_absent_input")
    )
  }
  bad <- needed[purrr::map_lgl(
    needed,
    \(col) any(.leaching_soil_value_bad(x[[col]], col))
  )]
  if (length(bad) > 0L) {
    cli::cli_abort(
      c(
        "{.field {bad}} {?holds/hold} a value outside its physical range.",
        i = "{.field root_depth_cm} must be positive and
             {.field soil_porosity} and {.field soil_field_capacity} in
             (0, 1]."
      ),
      class = "whep_leaching_driver_value"
    )
  }
  invisible(x)
}

.leaching_soil_value_bad <- function(values, col) {
  switch(
    col,
    root_depth_cm = values <= 0,
    soil_porosity = values <= 0 | values > 1,
    soil_field_capacity = values <= 0 | values > 1
  )
}

# The per-row shares of the cascade: topsoil denitrification `denit_share`,
# C:N attenuation `a_cn`, subsoil reduction `no3_red`, transport factor
# `transport`, and whether the row drains at all.
.leaching_rates <- function(x, drainage_mm, options) {
  drained <- drainage_mm > 0
  c(
    .leaching_rates_meisinger(x, drainage_mm, options),
    list(
      drained = drained,
      a_cn = .leaching_a_cn(x),
      no3_red = .leaching_no3_red(x)
    )
  )
}

.leaching_rates_meisinger <- function(x, drainage_mm, options) {
  fert_cat <- dplyr::if_else(x$fert_type == "Synthetic", "Synthetic", "Manure")
  .leaching_check_tillage(x, fert_cat)
  list(
    denit_share = .leaching_denit_share(
      x,
      fert_cat,
      drainage_mm,
      options$none_class
    ),
    transport = if (options$transport == "swat_mixing") {
      .leaching_f_swat(drainage_mm, x$soil_porosity, x$root_depth_cm)
    } else {
      .leaching_f_burns(drainage_mm, x$soil_field_capacity, x$root_depth_cm)
    }
  )
}

# SWAT 2009 well-mixed layer (Neitsch et al. 2011, TR-406, eqs. 4:2.1.2 to
# 4:2.1.8, pp. 269-270): the percolate mixes with the mobile water, the
# (1 - theta_e) share of the layer's saturated water content (porosity x
# depth, mm), and carries 1 - exp(-percolate / mobile water) of its nitrate.
# theta_e is the ANION_EXCL default 0.5 (SWAT Input/Output File
# Documentation, Version 2012, ch. 22, p. 304). No drainage, no transport.
.leaching_f_swat <- function(drainage_mm, soil_porosity, root_depth_cm) {
  theta_e <- .n_constant("swat_anion_exclusion")
  mobile_mm <- (1 - theta_e) * soil_porosity * root_depth_cm * 10
  dplyr::if_else(drainage_mm > 0, 1 - exp(-drainage_mm / mobile_mm), 0)
}

# Burns (1976, J. Agric. Sci. 86:305-313, doi:10.1017/S0021859600054769),
# solute incorporated uniformly through the root zone: the share displaced
# below depth h (cm) by P cm of drainage through soil of volumetric field
# capacity theta_fc is (P / (P + theta_fc))^(h / 2). No drainage, no
# transport.
.leaching_f_burns <- function(drainage_mm, soil_field_capacity, root_depth_cm) {
  drainage_cm <- drainage_mm / 10
  displaced <- (drainage_cm /
    (drainage_cm + soil_field_capacity))^(root_depth_cm / 2)
  dplyr::if_else(drainage_mm > 0, displaced, 0)
}

# One year of the cascade. Where the row drains, the carried pool joins the
# surplus; where it does not, the pool is held as deep vadose storage
# (Walvoord et al. 2003, doi:10.1126/science.1086435; Ascott et al. 2017,
# doi:10.1038/s41467-017-01321-w): neither denitrified nor leached that year,
# it stays retained. Denitrification is the residual of every sink.
.leaching_cascade <- function(surplus, carried_in, rates) {
  through <- surplus + dplyr::if_else(rates$drained, carried_in, 0)
  available <- through * (1 - rates$denit_share) * (1 - rates$a_cn)
  leached <- available * rates$transport * (1 - rates$no3_red)
  held <- dplyr::if_else(rates$drained, 0, carried_in)
  retained <- available * (1 - rates$transport) + held
  list(
    leached = leached,
    retained = retained,
    denitrified = surplus + carried_in - leached - retained
  )
}

# The pool a key starts with: the steady state of its first year's fluxes,
# pool = S r / (1 - r), r the share of the pool's input that stays. Every
# meisinger_denitrification share is at least 0.02, so r < 1 on a drained
# row. A store with no exit (no drainage) has no finite steady state and
# starts at 0.
.leaching_steady_pool <- function(surplus, rates) {
  stays <- (1 - rates$denit_share) * (1 - rates$a_cn) * (1 - rates$transport)
  dplyr::if_else(rates$drained, surplus * stays / (1 - stays), 0)
}

# Each row's carried-in pool, its pool field and whether its pool waits on an
# absent key. Without `carry_over_by` every row is a first year.
.leaching_thread_pool <- function(x, rates, carry_over_by) {
  steady <- .leaching_steady_pool(x$n_surplus_t, rates)
  n <- nrow(x)
  if (is.null(carry_over_by)) {
    return(list(
      carried_in = steady,
      field = .leaching_pool_field(rep(TRUE, n), rates$drained),
      held = rep(FALSE, n)
    ))
  }
  key_id <- .leaching_carry_key(x, carry_over_by)
  context <- list(
    key_id = key_id,
    surplus = x$n_surplus_t,
    steady = steady,
    rates = rates
  )
  state <- purrr::reduce(
    split(seq_len(n), x$year),
    \(state, rows) .leaching_thread_year(state, rows, context),
    .init = list(
      pool = rep(NA_real_, max(key_id, 0L)),
      carried_in = rep(NA_real_, n),
      first = rep(FALSE, n)
    )
  )
  list(
    carried_in = state$carried_in,
    field = .leaching_pool_field(state$first, rates$drained),
    held = .leaching_held_rows(key_id, x$year)
  )
}

# One run year: each key present takes its pool from its previous row, or its
# steady state if it has none, and leaves this year's retained nitrate as the
# pool. Keys absent this year keep their pool untouched.
.leaching_thread_year <- function(state, rows, context) {
  ids <- context$key_id[rows]
  pooled <- state$pool[ids]
  first <- is.na(pooled)
  carried_in <- dplyr::if_else(first, context$steady[rows], pooled)
  rates <- purrr::map(context$rates, \(values) values[rows])
  flows <- .leaching_cascade(context$surplus[rows], carried_in, rates)
  state$pool[ids] <- flows$retained
  state$carried_in[rows] <- carried_in
  state$first[rows] <- first
  state
}

# The key of every row across years, refusing a key that is absent, has a
# missing year, or occurs twice in one year (two rows would race for one
# pool).
.leaching_carry_key <- function(x, carry_over_by) {
  needed <- c(carry_over_by, "year")
  absent <- needed[!rlang::has_name(x, needed)]
  if (!is.character(carry_over_by) || length(absent) > 0L) {
    cli::cli_abort(
      c(
        "{.arg carry_over_by} must name columns of {.arg x}, which must also
         carry {.field year}.",
        x = "Absent: {.field {absent}}."
      ),
      class = "whep_leaching_carry_key"
    )
  }
  key_id <- vctrs::vec_group_id(x[carry_over_by])
  key_year <- tibble::tibble(key_id = key_id, year = x$year)
  if (anyNA(x$year) || vctrs::vec_duplicate_any(key_year)) {
    cli::cli_abort(
      c(
        "Each {.field {carry_over_by}} key needs one row per {.field year},
         with no missing year.",
        x = "{sum(vctrs::vec_duplicate_detect(key_year))} row{?s} share a
             key and a year; {sum(is.na(x$year))} {?has/have} no year."
      ),
      class = "whep_leaching_carry_key"
    )
  }
  key_id
}

# TRUE on a key's row when the next year of the run lacks that key: its pool
# waits on the key instead of entering a cascade that year.
.leaching_held_rows <- function(key_id, year) {
  years <- sort(unique(year))
  next_year <- years[match(year, years) + 1L]
  present <- vctrs::vec_in(
    tibble::tibble(key_id = key_id, year = next_year),
    tibble::tibble(key_id = key_id, year = year)
  )
  !is.na(next_year) & !present
}

.leaching_pool_field <- function(first, drained) {
  dplyr::case_when(
    !first ~ "carried_pool",
    drained ~ "initial_pool_steady_state",
    .default = "initial_vadose_zero"
  )
}

# method_leaching fields joined by ";": method, transport, None-class rule,
# pool field, then the optional vadose_storage and pool_held_absent_key.
.leaching_stamp <- function(options, drained, pool) {
  base <- paste(
    c(
      "meisinger_drainage",
      options$transport,
      paste0("none_", options$none_class)
    ),
    collapse = ";"
  )
  paste0(
    base,
    ";",
    pool$field,
    dplyr::if_else(drained, "", ";vadose_storage"),
    dplyr::if_else(pool$held, ";pool_held_absent_key", "")
  )
}

# ---- Private helpers: IMAGE-GNM root-zone leaching -----------------------

# image_gnm: Beusen et al. (2015), Geosci. Model Dev. 8:4045-4067, eqs. 8-14
# (pp. 4051-4052) and Table 2 (p. 4053). Surface runoff carries off a share of
# the year's inputs (eq. 9); the budget left leaches in the share f_leach
# (eq. 10) and the rest denitrifies (f_den = 1 - f_leach). Eroded soil N
# (eq. 8) is reported but, as in the authors' code, taken from no budget.
# IMAGE-GNM keeps no root-zone nitrate from one year to the next, so the
# retained and carried-in pools are structural zeros of the method, not
# estimates.
.leaching_image_gnm <- function(x, drainage_mm) {
  drainage_mm <- .gnm_check_drivers(x, drainage_mm)
  factors <- .gnm_factors(x, drainage_mm)
  runoff <- .gnm_surface_runoff(x)
  # Eq. 10 with a runoff above the surplus is negative; the authors' code
  # floors the remaining balance at 0 (n_surface_runoff.py lines 106, 122
  # and 136).
  budget <- pmax(0, x$n_surplus_t - runoff)
  flows <- list(
    leached = factors$f_leach * budget,
    denitrified = (1 - factors$f_leach) * budget
  )
  stamp <- paste0(
    factors$stamp,
    dplyr::if_else(
      runoff > x$n_surplus_t,
      ";surface_runoff_exceeds_budget",
      ""
    )
  )
  ef5 <- .n_constant("ef5_no3_to_n2o")
  x |>
    dplyr::mutate(
      surface_runoff_n_t = runoff,
      erosion_n_t = .gnm_erosion(x),
      no3_n_t = flows$leached,
      denitrification_n_t = flows$denitrified,
      retained_profile_n_t = 0,
      retained_carried_in_n_t = 0,
      n2o_indirect_no3_n_t = .data$no3_n_t * ef5,
      method_leaching = stamp
    )
}

# Beusen et al. (2015) eq. 9 (p. 4052): N_sro,rec = f_cal f_qsro N_inp, with
# f_qsro = f_qsro(slope) f_qsro(texture) f_qsro(landuse) (eq. 3, p. 4049).
# The slope and texture product is the authors' 0.5-degree layer
# (surface_runoff_fraction); the land-use factor is Table 3's (p. 4056).
# N_inp as the authors' code takes it (n_surface_runoff.py): the input net
# of ammonia on crops and grassland, floored at 0 (lines 57-58, 96 and 112),
# and the balance on natural vegetation (line 128).
.gnm_surface_runoff <- function(x) {
  natural <- x$land_use == "Natural"
  n_inp <- dplyr::if_else(
    natural,
    x$n_surplus_t,
    pmax(0, x$n_input_std_t - x$nh3_n_t)
  )
  .n_constant("image_gnm_runoff_calibration") *
    .gnm_runoff_factor(x$land_use, "runoff_land_use") *
    x$surface_runoff_fraction *
    n_inp
}

# N_sro,mem of eq. 8 (p. 4052), the N of eroded soil, as the authors' code
# computes it (n_erosion.py): erodibility = slope factor x texture factor
# (lines 47-55), soil loss = erodibility x the land use's loss per km2 x the
# area (lines 88-116), and N = topsoil organic carbon x soil loss / C:N
# (lines 120-135). area_ha / 100 is the area in km2; the soil loss is in
# tonnes, so N is in tonnes.
.gnm_erosion <- function(x) {
  soil_loss_t <- .gnm_erodibility(x) *
    .gnm_runoff_factor(x$land_use, "soil_loss") *
    x$area_ha /
    100
  soil_loss_t *
    x$soil_organic_carbon_pct /
    100 /
    .gnm_runoff_factor(x$land_use, "erosion_cn")
}

# The authors' erodibility (n_erosion.py lines 47-55): the slope factor
# a + b / (1 + exp(c - d S / 1000)) of the median slope S in m/km (S / 1000
# in m/m), times the texture factor of texture_erosion.vat.
.gnm_erodibility <- function(x) {
  slope_m_per_m <- x$slope_m_per_km / 1000
  slope_factor <- .n_constant("image_gnm_erosion_slope_offset") +
    .n_constant("image_gnm_erosion_slope_range") /
      (1 +
        exp(
          .n_constant("image_gnm_erosion_slope_intercept") -
            .n_constant("image_gnm_erosion_slope_rate") * slope_m_per_m
        ))
  slope_factor *
    .gnm_runoff_factor(x$soil_texture_class, "erosion_texture")
}

# One factor of image_gnm_runoff_factors per label. A label the table does
# not hold aborts rather than take a factor the source never gave it (the
# authors' texture_erosion.vat gives its undefined class 0 a factor of 1,
# which would read as a measured soil).
.gnm_runoff_factor <- function(labels, factor) {
  rows <- dplyr::filter(
    whep::image_gnm_runoff_factors,
    .data$factor == .env$factor
  )
  hit <- match(labels, rows$class)
  if (anyNA(hit)) {
    cli::cli_abort(
      c(
        "{sum(is.na(hit))} row{?s} carr{?ies/y} a {factor} class that
         {.code image_gnm_runoff_factors} does not hold:
         {.val {unique(labels[is.na(hit)])}}.",
        i = "Expected one of {.val {rows$class}}."
      ),
      class = "whep_leaching_class_value"
    )
  }
  rows$value[hit]
}

# Eqs. 11-14 per row: f_leach = [1 - min(f_climate + f_text + f_drain +
# f_soc, 1)] f_landuse, f_climate = f_K T_r, with the stamp fields of the
# bounds that held a factor. Two bounds come from the authors' model code
# (N_model/trunk/tools/n_soil_denitrification.py), which governs where the
# paper is silent or ambiguous: f_K at most 1 (line 97) and, on cropland,
# T_r at most 1 year (lines 377-378, "the residence time of the water is
# maximal 1 year (for example irrigation)"); the printed sentence (p. 4052)
# reads as a floor of 1 year on dry agricultural land instead.
.gnm_factors <- function(x, drainage_mm) {
  f_landuse <- .gnm_class_factor(x$land_use, "land_use")
  cropland <- x$land_use == "Cropland"
  f_k_eq13 <- .gnm_f_k(x$temp_c_annual_mean)
  f_k <- pmin(f_k_eq13, .n_constant("image_gnm_max_temperature_effect"))
  residence <- .gnm_residence_years(x, drainage_mm)
  crop_cap <- .n_constant("image_gnm_crop_max_residence_yr")
  t_r <- dplyr::if_else(cropland, pmin(residence, crop_cap), residence)
  f_climate <- f_k * t_r
  f_sum <- f_climate +
    .gnm_class_factor(x$soil_texture_class, "texture") +
    .gnm_class_factor(x$soil_drainage_class, "drainage") +
    .gnm_soc_factor(x$soil_organic_carbon_pct)
  list(
    f_leach = (1 - pmin(f_sum, 1)) * f_landuse,
    stamp = .gnm_stamp(
      no_drainage = drainage_mm == 0,
      crop_capped = cropland & residence > crop_cap,
      f_k_capped = f_k_eq13 > f_k
    )
  )
}

# Beusen et al. (2015) eq. 13: the Arrhenius temperature effect on root-zone
# denitrification, per year of residence, with the mean annual temperature in
# kelvin (Celsius + 273.15; the authors' code adds 273). Unbounded here; the
# caller applies the bound at 1.
.gnm_f_k <- function(temp_c) {
  .n_constant("beusen_arrhenius_factor") *
    exp(
      -.n_constant("beusen_activation_energy") /
        (.n_constant("molar_gas_constant") * (temp_c + 273.15))
    )
}

# Beusen et al. (2015) eq. 14: T_r = tawc / q_eff years. tawc is the total
# available water capacity (field capacity - wilting point) of the top 1 m,
# in mm; the source takes the top 1 m "or less if thinner", and WHEP has no
# soil-thickness layer, so the full 1 m is used. q_eff, the excess water flow
# of eq. 5 (p. 4049) that recharges groundwater, is WHEP's annual drainage,
# the deep seepage below the soil column. Eq. 14 is undefined without
# drainage; the authors' code takes 100 years there (trzd(),
# n_soil_denitrification.py lines 179-182).
.gnm_residence_years <- function(x, drainage_mm) {
  tawc_mm <- (x$soil_field_capacity - x$soil_wilting_point) *
    .n_constant("image_gnm_tawc_depth_m") *
    1000
  dplyr::if_else(
    drainage_mm > 0,
    tawc_mm / drainage_mm,
    .n_constant("image_gnm_no_drainage_residence_yr")
  )
}

# The Table 2 texture or drainage factor, or f_landuse, of each label, from
# image_gnm_leaching_factors. A label the table does not hold aborts: the
# authors' code books an unknown texture or drainage class as 0, which would
# read as a measured coarse or well-drained soil.
.gnm_class_factor <- function(labels, factor) {
  rows <- dplyr::filter(
    whep::image_gnm_leaching_factors,
    .data$factor == .env$factor
  )
  hit <- match(labels, rows$class)
  if (anyNA(hit)) {
    cli::cli_abort(
      c(
        "{sum(is.na(hit))} row{?s} carr{?ies/y} a {factor} class that
         {.code image_gnm_leaching_factors} does not hold:
         {.val {unique(labels[is.na(hit)])}}.",
        i = "Expected one of {.val {rows$class}}."
      ),
      class = "whep_leaching_class_value"
    )
  }
  rows$value[hit]
}

# Table 2's soil organic carbon factor. Each class holds its lower bound
# (1-3% starts at 1), as in the authors' class code (n_soil_denitrification.py
# lines 111-118: oc < 1, oc < 3, oc < 6, else). Negative values are refused
# before this point.
.gnm_soc_factor <- function(soc_pct) {
  rows <- dplyr::filter(whep::image_gnm_leaching_factors, .data$factor == "soc")
  rows$value[findInterval(soc_pct, rows$soc_min_pct)]
}

# method_leaching for image_gnm: the method and its 1 m water-capacity depth,
# then each bound that held a factor on the row, joined by ";".
.gnm_stamp <- function(no_drainage, crop_capped, f_k_capped) {
  paste0(
    "image_gnm;tawc_top_1m",
    dplyr::if_else(no_drainage, ";no_drainage_residence_100yr", ""),
    dplyr::if_else(crop_capped, ";crop_residence_cap_1yr", ""),
    dplyr::if_else(f_k_capped, ";temperature_effect_cap_1", "")
  )
}

# The columns image_gnm reads from `x` (drainage arrives separately): the
# balance's own terms, then the drivers a caller supplies.
.gnm_driver_cols <- function() {
  c(.gnm_balance_cols(), .gnm_external_cols())
}

# The image_gnm columns a nitrogen balance row already carries.
.gnm_balance_cols <- function() {
  c("n_surplus_t", "n_input_std_t", "nh3_n_t", "area_ha")
}

# The image_gnm drivers a nitrogen balance does not hold, supplied in its
# leaching driver table.
.gnm_external_cols <- function() {
  c(
    "surface_runoff_fraction",
    "slope_m_per_km",
    "land_use",
    "temp_c_annual_mean",
    "soil_field_capacity",
    "soil_wilting_point",
    "soil_texture_class",
    "soil_drainage_class",
    "soil_organic_carbon_pct"
  )
}

# An absent or missing driver has no defensible value, so it aborts here
# rather than becoming a zero factor; then every value must be physical.
# Returns the drainage recycled to the rows of `x`.
.gnm_check_drivers <- function(x, drainage_mm) {
  if (rlang::has_name(x, "surface_runoff_n_t")) {
    cli::cli_abort(
      c(
        "{.code image_gnm} computes {.field surface_runoff_n_t} (eq. 9), but
         {.arg x} already holds it.",
        i = "Drop it, and supply {.field surface_runoff_fraction} from
             {.fn read_image_gnm_runoff} instead."
      ),
      class = "whep_gnm_runoff_supplied"
    )
  }
  needed <- .gnm_driver_cols()
  present <- needed[rlang::has_name(x, needed)]
  with_na <- present[purrr::map_lgl(present, \(col) anyNA(x[[col]]))]
  no_drainage <- is.null(drainage_mm) || anyNA(drainage_mm)
  missing <- c(setdiff(needed, present), with_na, "drainage_mm"[no_drainage])
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "{.code image_gnm} reads {.field {missing}}, absent from {.arg x} or
         missing on some row.",
        i = "Supply {.field {c(needed, 'drainage_mm')}}."
      ),
      class = c("whep_missing_leaching_driver", "whep_absent_input")
    )
  }
  drainage <- vctrs::vec_recycle(drainage_mm, nrow(x), x_arg = "drainage_mm")
  .gnm_check_values(x, drainage)
  drainage
}

.gnm_check_values <- function(x, drainage_mm) {
  capacity <- x$soil_field_capacity
  wilting <- x$soil_wilting_point
  fraction <- x$surface_runoff_fraction
  non_negative <- c(
    "soil_organic_carbon_pct",
    "slope_m_per_km",
    "area_ha",
    "n_input_std_t",
    "nh3_n_t",
    "n_surplus_t"
  )
  bad <- c(
    soil_field_capacity = any(capacity <= 0 | capacity > 1),
    soil_wilting_point = any(wilting <= 0 | wilting >= capacity),
    purrr::map_lgl(rlang::set_names(non_negative), \(col) any(x[[col]] < 0)),
    temp_c_annual_mean = any(!is.finite(x$temp_c_annual_mean)),
    drainage_mm = any(drainage_mm < 0),
    surface_runoff_fraction = any(fraction < 0 | fraction > 1)
  )
  if (any(bad)) {
    cli::cli_abort(
      c(
        "{.field {names(bad)[bad]}} {?holds/hold} a value outside its
         physical range.",
        i = "Field capacity in (0, 1], wilting point in (0, field capacity),
             non-negative organic carbon, drainage, slope, area, input,
             ammonia and surplus, a finite temperature, and a surface runoff
             fraction in [0, 1]."
      ),
      class = "whep_leaching_driver_value"
    )
  }
  invisible(x)
}

# ---- Private helpers: shared ---------------------------------------------

.n_constant <- function(name) {
  whep::n_attenuation_constants |>
    dplyr::filter(.data$constant == name) |>
    dplyr::pull("value")
}

# Takes the whole table: a missing column must abort here, not pass as
# unique(NULL) and resurface in if_else() as "object 'climate' not found".
.n_check_climate <- function(x) {
  if (!rlang::has_name(x, "climate")) {
    cli::cli_abort(
      c(
        "{.arg x} is missing required column {.field climate}.",
        i = "Expected {.val ATL} or {.val MED} on every row."
      ),
      class = "whep_missing_climate"
    )
  }
  climate <- x$climate
  valid <- c("ATL", "MED")
  bad <- unique(climate[is.na(climate) | !climate %in% valid])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Unexpected or missing {.field climate} value{?s}: {.val {bad}}.",
      i = "Expected {.val ATL} or {.val MED}."
    ))
  }
  invisible(NULL)
}

# One IPCC 2019 Tier 1 factor from ipcc_2019_soil_n_ef, by its parameter and
# disaggregation. A lookup that does not return exactly one value is a broken
# table, never a factor to guess.
.ipcc2019_soil_ef <- function(parameter, disaggregation) {
  table <- .ipcc2019_soil_ef_table()
  value <- table$value[
    table$parameter == parameter & table$disaggregation == disaggregation
  ]
  if (length(value) != 1L || is.na(value)) {
    cli::cli_abort(
      "{.code ipcc_2019_soil_n_ef} has {length(value)} value{?s} for
       {.val {parameter}} / {.val {disaggregation}}; expected one.",
      class = "whep_ipcc2019_ef_lookup"
    )
  }
  value
}

# The factor table every IPCC 2019 Tier 1 lookup reads, one binding so a test
# can hand the lookup a broken table.
.ipcc2019_soil_ef_table <- function() {
  whep::ipcc_2019_soil_n_ef
}

# The wet/dry class every IPCC 2019 wet/dry factor is keyed on. Its absence,
# on the table or on a row, is refused rather than filled: which class a row
# belongs to is decided where the drivers are built (build_cell_moisture_
# regime()), and a default here would price the row as though it had been.
.n_check_moisture_regime <- function(x) {
  if (!rlang::has_name(x, "moisture_regime")) {
    cli::cli_abort(
      c(
        "{.arg x} is missing required column {.field moisture_regime}.",
        i = "The IPCC 2019 wet/dry factors need {.val wet} or {.val dry} on
             every row; {.fn build_cell_moisture_regime} classifies cells.",
        i = "The Atlantic/Mediterranean option, {.val ipcc2019_atl_med},
             reads {.field climate} instead."
      ),
      class = c("whep_missing_moisture_regime", "whep_absent_input")
    )
  }
  regime <- x$moisture_regime
  missing <- is.na(regime)
  if (any(missing)) {
    cli::cli_abort(
      c(
        "{sum(missing)} row{?s} of {.arg x} {?has/have} no
         {.field moisture_regime}.",
        i = "Each row needs {.val wet} or {.val dry}; the driver builder
             decides how a cell without a class is filled."
      ),
      class = c("whep_moisture_regime_na", "whep_absent_input")
    )
  }
  bad <- setdiff(unique(regime), c("wet", "dry"))
  if (length(bad) > 0L) {
    cli::cli_abort(
      c(
        "Unexpected {.field moisture_regime} value{?s}: {.val {bad}}.",
        i = "Expected {.val wet} or {.val dry}."
      ),
      class = "whep_moisture_regime_value"
    )
  }
  invisible(NULL)
}

# fert_type labels of excreta deposited by grazing animals: the balance's own
# "Excreta" (species carried as excreta_cpp_share) and the two species labels
# of fertiliser_n2o_modifiers.
.n_excreta_fert_types <- function() {
  c("Excreta", "Excreta_cattle_monog", "Excreta_other")
}

# CBS items of the production items classed as rice in items_prod_full's
# irrigation class (`Crop_irrig == "Rice"`): item 27 "Rice", whose CBS item
# is 2807 "Rice and products", the code the balance key carries. The label
# is asserted first, so a renamed class cannot silently empty the rice rule.
.n_rice_item_cbs_codes <- function() {
  items <- whep::items_prod_full
  check_labels_supplied(items, "Crop_irrig", "Rice")
  items$item_cbs_code[items$Crop_irrig %in% "Rice"] |>
    stats::na.omit() |>
    unique() |>
    as.integer()
}

# TRUE on the excreta rows labelled excreta only as the fallback of a
# manure-type split (`method_excreta_species == "fallback_label_no_species"`,
# R/excreta_species_mix.R): collected manure with no type and no species,
# which is not grazing deposition.
.n_excreta_fallback <- function(x, fert_type) {
  stamp <- if (rlang::has_name(x, "method_excreta_species")) {
    x$method_excreta_species
  } else {
    rep(NA_character_, nrow(x))
  }
  fert_type %in% .n_excreta_fert_types() & .excreta_is_fallback(stamp)
}

# `excreta_cpp_share` on the rows flagged `excreta`, the share of their N
# deposited by cattle, poultry and pigs (R/excreta_species_mix.R). A row
# labelled by species (`fert_type`) takes the share its label states, and a
# supplied share that contradicts the label aborts. A row with excreta N and
# no share has no defensible factor and aborts; so does a share outside
# [0, 1]. A row with no excreta N emits nothing whatever its mix, so its share
# is structurally irrelevant and set to 0 to keep the product finite. Rows not
# flagged keep NA.
.n_excreta_share <- function(x, excreta, fert_type) {
  share <- if (rlang::has_name(x, "excreta_cpp_share")) {
    x$excreta_cpp_share
  } else {
    rep(NA_real_, nrow(x))
  }
  share <- .n_excreta_label_share(share, excreta, fert_type)
  no_n <- excreta & x$n_input_t %in% 0
  unknown <- excreta & !no_n & is.na(share)
  if (any(unknown)) {
    .abort_excreta_share_missing(x, unknown)
  }
  invalid <- excreta & !is.na(share) & (share < 0 | share > 1)
  if (any(invalid)) {
    cli::cli_abort(
      "{sum(invalid)} excreta row{?s} carr{?ies/y} an
       {.field excreta_cpp_share} outside 0 to 1:
       {.val {unique(share[invalid])}}.",
      class = "whep_excreta_share_invalid"
    )
  }
  dplyr::if_else(no_n & is.na(share), 0, share)
}

# The CPP share a species label states: all of an "Excreta_cattle_monog"
# row's N comes from cattle, poultry and pigs (EF3PRP,CPP) and none of an
# "Excreta_other" row's. Rows flagged `excreta` with a species label take it;
# a supplied share that says otherwise is a contradiction with no defensible
# resolution, so it aborts.
.n_excreta_label_share <- function(share, excreta, fert_type) {
  label <- dplyr::case_when(
    fert_type == "Excreta_cattle_monog" ~ 1,
    fert_type == "Excreta_other" ~ 0,
    .default = NA_real_
  )
  labelled <- excreta & !is.na(label)
  conflict <- labelled &
    !is.na(share) &
    abs(share - label) > sqrt(.Machine$double.eps)
  if (any(conflict)) {
    cli::cli_abort(
      c(
        "{sum(conflict)} excreta row{?s} carr{?ies/y} an
         {.field excreta_cpp_share} that contradicts {?its/their} species
         label: {.val {unique(fert_type[conflict])}}.",
        i = "{.val Excreta_cattle_monog} states a share of 1 and
             {.val Excreta_other} a share of 0."
      ),
      class = "whep_excreta_share_label_conflict"
    )
  }
  dplyr::if_else(labelled, label, share)
}

.abort_excreta_share_missing <- function(x, unknown) {
  reasons <- if (rlang::has_name(x, "method_excreta_species")) {
    unique(stats::na.omit(x$method_excreta_species[unknown]))
  } else {
    character()
  }
  cli::cli_abort(
    c(
      "{sum(unknown)} excreta row{?s} carr{?ies/y} N but no
       {.field excreta_cpp_share} ({signif(sum(x$n_input_t[unknown]), 6)} t
       N).",
      x = "IPCC 2019 prices grazing excreta by species (EF3PRP,CPP versus
           EF3PRP,SO), so a row without the mix has no defensible factor.",
      i = if (length(reasons) > 0L) {
        "{.field method_excreta_species}: {.val {reasons}}."
      },
      i = "{.fn build_n_inputs} carries the share on its excreta rows."
    ),
    class = c("whep_excreta_share_missing", "whep_absent_input")
  )
}

# ---- Private helpers: examples -------------------------------------------

.example_nh3 <- function() {
  tibble::tribble(
    ~n_input_t, ~fert_type, ~nh3_n_t, ~method_nh3,
    10, "Synthetic", 1.1, "ipcc"
  )
}

# Wet synthetic N at EF1 0.016, dry other N at 0.005, flooded rice at EF1FR
# 0.004, and grazing excreta at the EF3PRP blend 0.5 * 0.006 + 0.5 * 0.003.
.example_soil_n2o <- function() {
  tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime,
    ~excreta_cpp_share, ~n2o_direct_n_t, ~method_soil_n2o,
    10, "Synthetic", 2511L, "wet", NA, 0.16, "ipcc2019_wet_dry",
    10, "Solid", 2511L, "dry", NA, 0.05, "ipcc2019_wet_dry",
    10, "Synthetic", 2807L, "wet", NA, 0.04, "ipcc2019_ef1fr",
    10, "Excreta", 3000L, "wet", 0.5, 0.045, "ipcc2019_ef3prp_blend"
  )
}

.example_n_leaching <- function() {
  tibble::tribble(
    ~n_surplus_t,
    ~moisture_regime,
    ~no3_n_t,
    ~denitrification_n_t,
    ~retained_profile_n_t,
    ~n2o_indirect_no3_n_t,
    ~method_leaching,
    100,
    "wet",
    24,
    76,
    0,
    0.264,
    "ipcc_fracleach_wet"
  )
}

.example_indirect_n2o_nh3 <- function() {
  tibble::tribble(
    ~nh3_n_t, ~moisture_regime, ~n2o_indirect_nh3_n_t,
    ~method_n2o_indirect_nh3,
    1.1, "wet", 0.0154, "ipcc2019_wet_dry",
    1.1, "dry", 0.0055, "ipcc2019_wet_dry"
  )
}
