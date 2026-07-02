# Full nitrogen balance assembler (Module C, Task C7), ported from Spain_
# Hist's Balance_parameters()/N_Figs.R equations. Combines the gridded N-
# input assembly (build_n_inputs(), R/n_balance_inputs.R) with the output
# side (production, residues, grazed weeds, soil organic-matter change) and
# the loss cascade (calculate_nh3()/calculate_soil_n2o()/
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
# "excreta", "deposition", "urban", "som_mineralization", "synthetic"), but
# n_balance_losses.R's coefficient tables (fertiliser_n2o_modifiers,
# subsoil_no3_reduction) are keyed on a DIFFERENT Title-case vocabulary
# ("Synthetic", "Solid", "Liquid", "Recycling", "Excreta_cattle_monog",
# "Excreta_other", "SOM", "Urban", "Deposition", "BNF") that additionally
# splits excreta by species. build_n_inputs()'s manure term aggregates
# species away entirely (see .manure_to_n_inputs()), so a faithful
# cattle/monogastric split is not recoverable at this stage without
# re-deriving it from a different upstream source -- this is a genuine
# methodological choice, not a mechanical rename, surfaced here for review
# rather than silently picked. .nb_loss_fert_type() maps "excreta" to the
# generic "Excreta_other" (the conservative choice that does not assume a
# cattle-heavy herd); every other term is a direct 1:1 rename.
# N_input_for_N2O_MgN's rows (Excreta, Liquid, Solid, SOM, Synthetic, Urban,
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
#' plus a GWP/CO2e indicator for the nitrous-oxide streams. Ported from
#' Spain_Hist's `Balance_parameters()`/`N_Figs.R` equations.
#'
#' @param methods A named list of method choices: `nh3` (forwarded to
#'   [calculate_nh3()], default `"manner"`), `n2o` (forwarded to
#'   [calculate_soil_n2o()], default `"aguilera"`) and `leaching` (forwarded
#'   to [calculate_n_leaching()], default `"meisinger_drainage"`).
#' @param resolution `"grid"` (default) or `"polity"`, as in
#'   [build_n_inputs()]; `"polity"` sums every term (and re-derives every
#'   indicator) over cells.
#' @param data Named list of pre-loaded upstream inputs. `n_inputs`
#'   ([build_n_inputs()]'s output) is used directly when supplied, else
#'   [build_n_inputs()] is called with this SAME `data` list (so its nested
#'   readers, e.g. `bnf_input`/`npp_n_input`/`livestock_intake`, propagate
#'   through). Also required:
#'   * `npp_n_input`: shared with [build_n_inputs()]'s `"recycling"` term
#'     (see [calculate_npp_carbon_nitrogen()]); used here for `prod_n_t`.
#'   * `residue_destiny_input`: [calculate_residue_destinies()]'s required
#'     input (`item_prod_code`, `residue_dm_t`, plus whatever the chosen
#'     `residue_destiny_method` needs), for `used_residue_n_t`/
#'     `burnt_residue_n_t`. `residue_destiny_method` selects the method
#'     (default `"krausmann_regional"`).
#'   * `livestock_intake`: shared with [build_n_inputs()]'s manure term;
#'     its `"grass"` `feed_quality` rows drive `grazed_weeds_n_t`.
#'   * `carbon_balance`: shared with [build_n_inputs()]'s `"som_
#'     mineralization"` term; its `son_change_kgn_ha` drives `som_
#'     sequestration_n_t`.
#'   * `n_balance_drivers`: additional per-`(key, fert_type)` driver
#'     columns [calculate_nh3()]/[calculate_soil_n2o()]/
#'     [calculate_indirect_n2o_nh3()] need beyond `n_input_t`/`fert_type`
#'     (`climate`, `irrig_type`, plus every MANNER driver column when
#'     `methods$nh3 == "manner"`), joined by the balance key AND
#'     `fert_type` (the Title-case vocabulary from [calculate_nh3()]'s own
#'     documentation, e.g. `"Synthetic"`, `"Solid"`, `"Excreta_other"`; see
#'     the file-level "fert_type vocabulary bridge" note for the exact
#'     mapping from [build_n_inputs()]'s lowercase values). A second set of
#'     balance-key-only driver columns (`irrig_cat`, `tillage`, `som_share`,
#'     `cn_input`, `land_use`) for [calculate_n_leaching()] is read from
#'     `n_balance_leaching_drivers`. Missing required drivers abort inside
#'     the called `calculate_*()` function, naming the exact column.
#'   * `drainage_mm`: annual drainage (mm) for [calculate_n_leaching()], as
#'     a numeric vector aligned to the balance-key rows, or already present
#'     as a `drainage_mm` column via `n_balance_leaching_drivers`.
#' @param gwp 100-year global warming potential standard for N2O, `"ar6"`
#'   (default), `"ar5"` or `"ar4"`, matching [build_crop_soil_n2o_extension()].
#' @param example If `TRUE`, return a small fixture instead of assembling
#'   real data. Defaults to `FALSE`.
#' @return A tibble keyed by `year`/`area_code`/`item_cbs_code` (plus
#'   `lon`/`lat` at `resolution = "grid"`) with the input aggregates
#'   (`n_input_full_t`, `n_input_full_nosom_t`, `n_input_std_t`,
#'   `n_input_som_t`, `n_input_for_n2o_t`), the output aggregates
#'   (`n_output_residues_t`, `n_output_som_t`, `n_output_useful_t`,
#'   `n_output_std_t`, `n_output_full_t`), the loss terms (`nh3_n_t`,
#'   `n2o_direct_n_t`, `no3_n_t`, `denitrification_n_t`,
#'   `n2o_indirect_no3_n_t`, `n2o_indirect_nh3_n_t`), the (post-cap)
#'   `som_sequestration_n_t`, `n_balance_t`, `surplus_t`, `surplus_share`,
#'   the five NUE ratios (`nue_std`, `nue_residues`, `nue_som`,
#'   `nue_useful`, `nue_full`), `total_gwp_co2e_kg`, and the `method_nh3`/
#'   `method_soil_n2o`/`method_leaching` provenance columns.
#' @export
#' @examples
#' build_nitrogen_balance(example = TRUE)
build_nitrogen_balance <- function(
  methods = list(
    nh3 = "manner",
    n2o = "aguilera",
    leaching = "meisinger_drainage"
  ),
  resolution = c("grid", "polity"),
  data = list(),
  gwp = c("ar6", "ar5", "ar4"),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  gwp <- rlang::arg_match(gwp)
  if (isTRUE(example)) {
    return(.example_nitrogen_balance())
  }
  m <- .nb_methods(methods)
  key <- .nb_key(resolution)

  n_inputs <- data$n_inputs %||%
    build_n_inputs(resolution = resolution, data = data)

  .nb_inputs(n_inputs, key) |>
    .nb_outputs(data, key) |>
    .nb_losses(n_inputs, key, m, data) |>
    .nb_indicators_pass1() |>
    .nb_cap_som() |>
    .nb_indicators_pass2(m, data, key, gwp) |>
    .nb_finalise()
}

# ---- Private helpers: method validation ----------------------------------

.nb_methods <- function(methods) {
  list(
    nh3 = methods$nh3 %||% "manner",
    n2o = methods$n2o %||% "aguilera",
    leaching = methods$leaching %||% "meisinger_drainage"
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

# The four N_input_* sums (Balance_parameters, verified). "recycling" is
# deliberately excluded (it feeds the residue OUTPUT terms, not an input
# sum) and n_input_for_n2o_t is a separate sum that DOES include it
# (N_Figs.R:496).
.nb_inputs <- function(n_inputs, key) {
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
  wide |>
    dplyr::mutate(
      n_input_full_t = .data$bnf +
        .data$excreta +
        .data$manure_liquid +
        .data$manure_solid +
        .data$synthetic +
        .data$urban +
        .data$deposition +
        .data$som_mineralization,
      n_input_full_nosom_t = .data$bnf +
        .data$excreta +
        .data$manure_liquid +
        .data$manure_solid +
        .data$synthetic +
        .data$urban +
        .data$deposition,
      n_input_std_t = .data$n_input_full_nosom_t,
      n_input_som_t = .data$bnf +
        .data$excreta +
        .data$manure_liquid +
        .data$manure_solid +
        .data$synthetic +
        .data$urban +
        .data$som_mineralization,
      n_input_for_n2o_t = .data$excreta +
        .data$manure_liquid +
        .data$manure_solid +
        .data$som_mineralization +
        .data$synthetic +
        .data$urban +
        .data$recycling
    )
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
    "urban",
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
.nb_add_prod_n <- function(x, data, key) {
  npp <- .n_balance_npp(data)
  if (is.null(npp)) {
    return(dplyr::mutate(x, prod_n_t = 0))
  }
  prod <- npp |>
    dplyr::summarise(
      prod_n_t = sum(.data$product_n_t, na.rm = TRUE),
      .by = dplyr::all_of(key)
    )
  x |>
    dplyr::left_join(prod, by = key) |>
    dplyr::mutate(prod_n_t = tidyr::replace_na(.data$prod_n_t, 0))
}

# used_residue_n_t / burnt_residue_n_t: calculate_residue_destinies() splits
# residue_dm_t into feed/burn/soil destinies; the feed and burn shares are
# converted to N with the SAME residue_n_kgdm coefficient
# calculate_npp_carbon_nitrogen() uses internally (whep::whep_coef_table
# ("bio_coefs"), joined on item_prod_code).
.nb_add_residue_destiny <- function(x, data, key) {
  if (is.null(data$residue_destiny_input)) {
    return(dplyr::mutate(x, used_residue_n_t = 0, burnt_residue_n_t = 0))
  }
  n_kgdm <- whep::whep_coef_table("bio_coefs") |>
    dplyr::transmute(
      item_prod_code = as.character(.data$item_prod_code),
      residue_n_kgdm = .data$residue_n_kgdm
    )
  destiny <- data$residue_destiny_input |>
    calculate_residue_destinies(
      method = data$residue_destiny_method %||% "krausmann_regional"
    ) |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code)) |>
    dplyr::left_join(n_kgdm, by = "item_prod_code") |>
    dplyr::summarise(
      used_residue_n_t = sum(
        .data$residue_feed_dm_t * .data$residue_n_kgdm,
        na.rm = TRUE
      ),
      burnt_residue_n_t = sum(
        .data$residue_burn_dm_t * .data$residue_n_kgdm,
        na.rm = TRUE
      ),
      .by = dplyr::all_of(key)
    )
  x |>
    dplyr::left_join(destiny, by = key) |>
    dplyr::mutate(
      used_residue_n_t = tidyr::replace_na(.data$used_residue_n_t, 0),
      burnt_residue_n_t = tidyr::replace_na(.data$burnt_residue_n_t, 0)
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
    )
  x |>
    dplyr::left_join(grazed, by = key) |>
    dplyr::mutate(
      grazed_weeds_n_t = tidyr::replace_na(
        .data$grazed_weeds_n_t,
        0
      )
    )
}

# Reuse build_n_inputs()'s manure territory/coordinate resolution verbatim
# (same package, private helpers callable directly) rather than
# reimplementing it a third time. item_cbs_code is NA_integer_ (grazed
# forage is not attributed to a single crop item), matching the "not
# crop-specific" sentinel used package-wide.
.nb_grazed_coords <- function(x) {
  coords <- .ni_manure_coords(x$sub_territory)
  x |>
    dplyr::mutate(
      lon = coords$lon,
      lat = coords$lat,
      area_code = .manure_territory_to_area_code(.data$territory),
      item_cbs_code = NA_integer_
    )
}

# som_sequestration_n_t: pmax(0, -son_change_kgn_ha) * area_ha / 1000, the
# SAME clamp direction as build_n_inputs()'s "som_mineralization" term
# (which keeps son_change_kgn_ha > 0), using build_carbon_balance()'s own
# area_ha directly (the same way .n_inputs_som() does). carbon_balance is
# per-land-use, not per-crop, so it carries no item_cbs_code (like
# .n_inputs_som()'s deposition/urban/SOM rows); the NA_integer_
# "not crop-specific" sentinel is added before grouping so the join matches
# x's own NA_integer_ som_mineralization rows.
.nb_add_som_sequestration <- function(x, data, key) {
  if (is.null(data$carbon_balance)) {
    return(dplyr::mutate(x, som_sequestration_n_t = 0))
  }
  seq_n <- data$carbon_balance |>
    dplyr::filter(stringr::str_to_lower(.data$land_use) == "cropland") |>
    dplyr::mutate(item_cbs_code = NA_integer_) |>
    dplyr::summarise(
      som_sequestration_n_t = sum(
        pmax(0, -.data$son_change_kgn_ha) * .data$area_ha / 1000,
        na.rm = TRUE
      ),
      .by = dplyr::all_of(key)
    )
  x |>
    dplyr::left_join(seq_n, by = key) |>
    dplyr::mutate(
      som_sequestration_n_t = tidyr::replace_na(
        .data$som_sequestration_n_t,
        0
      )
    )
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
  no_volat <- c("Recycling", "SOM")
  with_nh3 <- dplyr::bind_rows(
    rows |>
      dplyr::filter(!.data$fert_type %in% no_volat) |>
      calculate_nh3(method = m$nh3),
    rows |>
      dplyr::filter(.data$fert_type %in% no_volat) |>
      dplyr::mutate(nh3_n_t = 0, method_nh3 = m$nh3)
  )
  losses <- with_nh3 |>
    calculate_soil_n2o(method = m$n2o) |>
    calculate_indirect_n2o_nh3() |>
    dplyr::summarise(
      nh3_n_t = sum(.data$nh3_n_t, na.rm = TRUE),
      n2o_direct_n_t = sum(.data$n2o_direct_n_t, na.rm = TRUE),
      n2o_indirect_nh3_n_t = sum(.data$n2o_indirect_nh3_n_t, na.rm = TRUE),
      .by = dplyr::all_of(key)
    ) |>
    dplyr::mutate(method_nh3 = m$nh3, method_soil_n2o = m$n2o)
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
      method_soil_n2o = tidyr::replace_na(.data$method_soil_n2o, m$n2o)
    )
}

# N_input_for_N2O_MgN's own fert_types (Excreta, Liquid, Solid, SOM,
# Synthetic, Urban, Recycling), mapped to the loss cascade's Title-case
# vocabulary (see file header) and joined against data$n_balance_drivers on
# (key, fert_type) for the climate/irrig_type/MANNER driver columns; never
# invents driver values, missing columns abort inside calculate_*().
.nb_loss_rows <- function(n_inputs, key, data) {
  n2o_fert_types <- c(
    "excreta",
    "manure_liquid",
    "manure_solid",
    "som_mineralization",
    "synthetic",
    "urban",
    "recycling"
  )
  rows <- n_inputs |>
    dplyr::filter(.data$fert_type %in% n2o_fert_types) |>
    dplyr::summarise(
      n_input_t = sum(.data$n_input_t, na.rm = TRUE),
      .by = dplyr::all_of(c(key, "fert_type"))
    ) |>
    dplyr::mutate(fert_type = .nb_loss_fert_type(.data$fert_type))
  if (is.null(data$n_balance_drivers)) {
    return(rows)
  }
  dplyr::left_join(rows, data$n_balance_drivers, by = c(key, "fert_type"))
}

# ---- Step 3b/c: balance closure, SOM cap, NUE ------------------------------

# First-pass balance from the input/output aggregates, BEFORE leaching (the
# leaching call needs n_surplus_t from this pass, per n_fun.r:978-985's
# ordering).
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
      surplus_share = .data$surplus_t / .data$n_input_full_t
    )
}

# N-limitation cap (N_balance.R:169-188, verified): a soil already in
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

# calculate_n_leaching() runs once per balance key (n_surplus_t is a
# balance-level quantity, not per-fert_type): joins data$n_balance_
# leaching_drivers (fert_type, climate, irrig_cat, land_use, cn_input,
# tillage, som_share) at the balance key, and data$drainage_mm (a numeric
# vector aligned to x, or a "drainage_mm" column already present via
# n_balance_leaching_drivers).
.nb_run_leaching <- function(x, m, data, key) {
  joined <- if (is.null(data$n_balance_leaching_drivers)) {
    x
  } else {
    dplyr::left_join(x, data$n_balance_leaching_drivers, by = key)
  }
  drainage <- if (rlang::has_name(joined, "drainage_mm")) {
    "drainage_mm"
  } else {
    data$drainage_mm
  }
  calculate_n_leaching(joined, drainage_mm = drainage, method = m$leaching) |>
    dplyr::mutate(method_leaching = m$leaching)
}

# Five NUE ratios (Balance_parameters n_fun.r:375-402 + NUE_calc N_Figs.R:
# 338-344): nue_std/nue_residues divide by N_input_std, nue_som/nue_useful
# by N_input_full, nue_full by N_input_SOM. These denominators are NOT
# collapsed into one shared variable -- the source genuinely uses two
# different conventions and this keeps them independently traceable.
.nb_nue <- function(x) {
  dplyr::mutate(
    x,
    nue_std = .data$prod_n_t / .data$n_input_std_t,
    nue_residues = .data$n_output_residues_t / .data$n_input_std_t,
    nue_som = .data$n_output_som_t / .data$n_input_full_t,
    nue_useful = .data$n_output_useful_t / .data$n_input_full_t,
    nue_full = .data$n_output_full_t / .data$n_input_som_t
  )
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
    "n_input_full_t",
    "n_input_full_nosom_t",
    "n_input_std_t",
    "n_input_som_t",
    "n_input_for_n2o_t",
    "prod_n_t",
    "used_residue_n_t",
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
    "method_leaching"
  )
  dplyr::select(x, dplyr::all_of(cols))
}

.nb_present_key <- function(x) {
  intersect(c("lon", "lat", "area_code", "item_cbs_code", "year"), names(x))
}

# ---- fert_type vocabulary bridge (documented mapping, see file header) ----

.nb_loss_fert_type <- function(fert_type) {
  dplyr::case_match(
    fert_type,
    "bnf" ~ "BNF",
    "recycling" ~ "Recycling",
    "manure_solid" ~ "Solid",
    "manure_liquid" ~ "Liquid",
    "excreta" ~ "Excreta_other",
    "deposition" ~ "Deposition",
    "urban" ~ "Urban",
    "som_mineralization" ~ "SOM",
    "synthetic" ~ "Synthetic"
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
    ~n_input_full_t,
    ~n_input_full_nosom_t,
    ~n_input_std_t,
    ~n_input_som_t,
    ~n_input_for_n2o_t,
    ~prod_n_t,
    ~used_residue_n_t,
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
    ~method_leaching,
    10L,
    2511L,
    2020L,
    100,
    98,
    98,
    99,
    97,
    40,
    10,
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
    0.132,
    0.56,
    0,
    0,
    0,
    40 / 98,
    50 / 98,
    42 / 100,
    60 / 100,
    100 / 99,
    (1.5 + 0.56 + 0.132) * (44 / 28) * 273 * 1000,
    "manner",
    "aguilera",
    "meisinger_drainage"
  )
}
