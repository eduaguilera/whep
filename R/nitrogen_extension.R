#' Build the nitrogen footprint extension.
#'
#' @description
#' Aggregate the cropland nitrogen balance into a footprint extension keyed by
#' `(year, area_code, item_cbs_code)`, expressed in kilograms of nitrogen
#' (kg N). This is the nitrogen analogue of [build_crop_soil_n2o_extension()]
#' and [build_livestock_ghg_extension()] and feeds [build_footprint()] /
#' [compute_footprint()] the same way.
#'
#' The footprint engine traces one impact column at a time, so the nitrogen
#' metric is selected with `method` (recorded in `method_nitrogen`):
#' - `"surplus"` (default): the soil nitrogen surplus, `inputs - removal`, the
#'   canonical nitrogen pressure (the net load available for leaching,
#'   volatilisation and runoff). Inputs reuse the soil-N assembly of
#'   [build_crop_soil_n2o_extension()] -- synthetic fertiliser (F_SN), applied
#'   manure (F_ON) and returned crop residue (F_CR), each allocated to crops by
#'   harvested-area share -- plus biological nitrogen fixation (BNF). Removal is
#'   the nitrogen exported in the harvested product. A sector whose removal
#'   exceeds its inputs (net soil-nitrogen mining) is floored to zero rather
#'   than reported as a negative load, because the footprint engine's
#'   conservation step ([compute_footprint()] with `conserve_extensions = TRUE`)
#'   clamps negative extensions and drops non-positive flows, so only a positive
#'   nitrogen load can be traced. Atmospheric deposition and below-ground
#'   residue nitrogen are further inputs not yet included.
#' - `"bnf"`: biological nitrogen fixation alone, the nitrogen fixed by
#'   symbiotic crop legumes ([calculate_crop_bnf()]). It is an input pressure on
#'   its own and a sensitivity reference for the surplus.
#'
#' Biological fixation and product removal come from whep's own crop
#' net-primary-production chain: [calculate_crop_npp()] and
#' [calculate_npp_carbon_nitrogen()] derive per-crop product and residue
#' nitrogen from production and harvested area, and [calculate_crop_bnf()]
#' derives the legume fixation. Non-legumes fix no symbiotic nitrogen, so they
#' contribute to the surplus only through fertiliser, manure and residues.
#'
#' All terms are nitrogen masses, so no global-warming-potential or other
#' characterisation factor is applied; the extension is wholly in-repo and needs
#' no coefficient pins beyond the FAOSTAT fertiliser and manure inputs already
#' read by [build_crop_soil_n2o_extension()].
#'
#' @param method Nitrogen metric, `"surplus"` (default) or `"bnf"`.
#' @param residue_removed_frac Fraction of above-ground crop residue removed
#'   from the field and therefore not returned to soil, passed through to the
#'   F_CR input. Defaults to `0.45`. Used only for `method = "surplus"`.
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` ([get_primary_production()]), `fertilizer` (the
#'   `faostat-fertilizer-nutrients` pin), `manure` (the
#'   `faostat-emissions-livestock` pin) and `primary_residues`
#'   ([get_primary_residues()]). Each falls back to its reader when absent; the
#'   `"bnf"` method needs only `primary_prod`.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (nitrogen in kilograms) and `method_nitrogen` (e.g.
#'   `"soil_n_surplus"`).
#'
#' @export
#'
#' @examples
#' build_nitrogen_extension(example = TRUE)
build_nitrogen_extension <- function(
  method = c("surplus", "bnf"),
  residue_removed_frac = 0.45,
  data = list(),
  example = FALSE
) {
  method <- rlang::arg_match(method)
  .check_removed_frac(residue_removed_frac)
  if (isTRUE(example)) {
    return(.example_nitrogen_extension())
  }

  primary_prod <- .resolve_input(data, "primary_prod", get_primary_production)
  npp_n <- .crop_npp_nitrogen(primary_prod)
  if (method == "bnf") {
    return(.nitrogen_bnf_extension(.crop_bnf_by_sector(npp_n), method))
  }
  .nitrogen_surplus(
    .surplus_input_streams(npp_n, primary_prod, residue_removed_frac, data),
    .crop_n_removal(npp_n),
    method
  )
}

# Use an injected input when present, otherwise call its reader thunk.
.resolve_input <- function(data, name, reader) {
  if (is.null(data[[name]])) reader() else data[[name]]
}

# The four surplus input streams (tonnes N per crop sector): synthetic
# fertiliser, applied manure and returned residue (reusing the soil-N2O
# assembly), plus biological nitrogen fixation.
.surplus_input_streams <- function(npp_n, primary_prod, removed_frac, data) {
  shares <- .crop_area_shares(primary_prod)
  fertilizer <- .resolve_input(
    data,
    "fertilizer",
    function() whep_read_file("faostat-fertilizer-nutrients")
  )
  manure <- .resolve_input(
    data,
    "manure",
    function() whep_read_file("faostat-emissions-livestock")
  )
  residues <- .resolve_input(data, "primary_residues", get_primary_residues)
  list(
    .synthetic_n_inputs(fertilizer, shares),
    .manure_n_inputs(manure, shares),
    .residue_n_inputs(residues, removed_frac),
    .crop_bnf_by_sector(npp_n)
  )
}

# Per-crop product and total-NPP nitrogen (tonnes N) from whep's crop-NPP
# chain. Crops are the production rows that also have a harvested-area row, so
# the inner join selects them and pairs production with area for the chain.
.crop_npp_nitrogen <- function(primary_prod) {
  crops <- .crop_production_area(primary_prod)
  if (nrow(crops) == 0L) {
    return(.empty_npp_nitrogen())
  }
  crops |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code)) |>
    calculate_crop_npp() |>
    calculate_npp_carbon_nitrogen()
}

# Pair crop production (tonnes) with harvested area (hectares) per crop item,
# excluding grassland. Only items with both rows survive the inner join, which
# is exactly the crop set the NPP chain needs.
.crop_production_area <- function(primary_prod) {
  grass <- c(3000L, 3002L, 3003L)
  prod <- primary_prod |>
    dplyr::filter(
      .data$unit == "tonnes",
      !is.na(.data$item_cbs_code),
      !.data$item_cbs_code %in% grass
    ) |>
    dplyr::summarise(
      production_t = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, item_prod_code, item_cbs_code)
    )
  area <- primary_prod |>
    dplyr::filter(
      .data$unit == "ha",
      !is.na(.data$item_cbs_code),
      !.data$item_cbs_code %in% grass
    ) |>
    dplyr::summarise(
      area_ha = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, item_prod_code, item_cbs_code)
    )
  prod |>
    dplyr::inner_join(
      area,
      by = c("year", "area_code", "item_prod_code", "item_cbs_code")
    ) |>
    dplyr::filter(.data$production_t > 0, .data$area_ha > 0)
}

# Symbiotic crop legume fixation (tonnes N) summed to the IO grain.
.crop_bnf_by_sector <- function(npp_n) {
  if (nrow(npp_n) == 0L) {
    return(.empty_n_stream())
  }
  npp_n |>
    calculate_crop_bnf() |>
    dplyr::summarise(
      n_t = sum(.data$crop_bnf_t, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    )
}

# Nitrogen removed in harvested product (tonnes N) summed to the IO grain.
.crop_n_removal <- function(npp_n) {
  npp_n |>
    dplyr::summarise(
      removal_t = sum(.data$product_n_t, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    )
}

# Sum the input streams, subtract removal and convert tonnes N to kg N. The
# full join keeps sectors that have inputs but no removal (and vice versa);
# missing terms are zero.
.nitrogen_surplus <- function(inputs, removal, method) {
  inputs |>
    dplyr::bind_rows() |>
    dplyr::summarise(
      inputs_t = sum(.data$n_t, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::full_join(
      removal,
      by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(
      impact_u = (dplyr::coalesce(.data$inputs_t, 0) -
        dplyr::coalesce(.data$removal_t, 0)) *
        1000
    ) |>
    .nitrogen_finalise(method)
}

# Biological fixation alone, as a standalone pressure (tonnes N to kg N).
.nitrogen_bnf_extension <- function(bnf, method) {
  bnf |>
    dplyr::mutate(impact_u = .data$n_t * 1000) |>
    .nitrogen_finalise(method)
}

# Drop empty sectors, coerce keys to integer and label the method. A negative
# surplus (net soil-N mining) is floored to zero: the footprint engine's
# conservation step clamps negative extensions and drops non-positive flows, so
# only a positive nitrogen load can be traced (see build_nitrogen_extension()).
.nitrogen_finalise <- function(extension, method) {
  extension |>
    dplyr::filter(.data$impact_u > 0) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code),
      method_nitrogen = .nitrogen_method_label(method)
    ) |>
    dplyr::select(year, area_code, item_cbs_code, impact_u, method_nitrogen)
}

.nitrogen_method_label <- function(method) {
  switch(method, surplus = "soil_n_surplus", bnf = "biological_n_fixation")
}

.empty_n_stream <- function() {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    item_cbs_code = integer(),
    n_t = numeric()
  )
}

.empty_npp_nitrogen <- function() {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    item_prod_code = character(),
    item_cbs_code = integer(),
    product_n_t = numeric(),
    crop_npp_n_t = numeric()
  )
}
