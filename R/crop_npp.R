#' Estimate potential net primary production.
#'
#' Estimates potential net primary production per unit area, used downstream to
#' scale weed biomass on cropland. Several methods are available; the default
#' `lpjml` reads gross managed-grassland NPP from a finished LPJmL run, while the
#' climate methods compute potential NPP from temperature, water input and
#' actual evapotranspiration.
#'
#' @param x A tibble. The climate methods require `temp_c` (mean annual
#'   temperature, degrees C), `water_input_mm` (precipitation plus irrigation)
#'   and `aet_mm` (actual evapotranspiration). The `lpjml` method instead needs
#'   a spatial key to join the gridded grass NPP (see `lpjml`).
#' @param method Potential-NPP method, one of `"lpjml"` (default), `"miami"`,
#'   `"nceas"` or `"rosenzweig"`.
#' @param lpjml A named list of options for the `lpjml` method (passed to the
#'   LPJmL grass reader). Ignored by the climate methods.
#' @return The input tibble with `npp_potential_dm_t_ha` (potential NPP, t DM per
#'   hectare) and `method_npp_potential` (the method used).
#' @export
#' @examples
#' calculate_potential_npp(
#'   tibble::tibble(temp_c = 15, water_input_mm = 800, aet_mm = 700),
#'   method = "miami"
#' )
calculate_potential_npp <- function(
  x,
  method = c("lpjml", "miami", "nceas", "rosenzweig"),
  lpjml = list()
) {
  method <- rlang::arg_match(method)
  out <- switch(
    method,
    lpjml = .potential_npp_lpjml(x, lpjml),
    miami = .potential_npp_miami(x),
    nceas = .potential_npp_nceas(x),
    rosenzweig = .potential_npp_rosenzweig(x)
  )
  dplyr::mutate(out, method_npp_potential = method)
}

#' Estimate crop above-ground residue biomass.
#'
#' Estimates crop residue dry matter from production and area using an ensemble
#' of an IPCC 2019 linear model and a `bio_coefs` residue:product ratio. The
#' irrigation and modern-variety adjustments activate only when their driver
#' columns are present in `x`.
#'
#' @param x A tibble with `item_prod_code`, `production_t` (fresh matter) and
#'   `area_ha`. Optional `water_regime` enables the irrigation adjustment;
#'   optional `year` plus `region_hanpp` enable the modern-variety adjustment.
#' @param method Residue method: `"ensemble"` (default, weighted IPCC + ratio),
#'   `"ipcc"` (IPCC linear only) or `"ratio"` (`bio_coefs` ratio only).
#' @param weights Named list; `w_ipcc` is the IPCC weight in the ensemble (0-1,
#'   default 0.5), used only when `method = "ensemble"`.
#' @return The input tibble with `product_dm_t`, `yield_dm_t_ha`, `residue_dm_t`
#'   and `method_residue`.
#' @export
#' @examples
#' calculate_crop_residues(
#'   tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
#' )
calculate_crop_residues <- function(
  x,
  method = c("ensemble", "ipcc", "ratio"),
  weights = list(w_ipcc = 0.5)
) {
  method <- rlang::arg_match(method)
  .crop_npp_validate(
    x,
    c("item_prod_code", "production_t", "area_ha"),
    "calculate_crop_residues"
  )
  w_ipcc <- .residue_weight(weights)
  x |>
    .residue_dm_conversions() |>
    .residue_ipcc() |>
    .residue_ratio() |>
    .residue_irrigation_factor() |>
    .residue_variety_factor() |>
    .residue_combine(method, w_ipcc) |>
    dplyr::mutate(method_residue = method) |>
    .residue_cleanup()
}

#' Estimate crop below-ground (root) biomass.
#'
#' Estimates root dry matter from above-ground biomass using an ensemble of an
#' IPCC root:shoot ratio and a reference below-ground biomass per hectare. The
#' nitrogen-input and irrigation adjustments activate only when their driver
#' columns are present in `x`.
#'
#' @param x A tibble with `item_prod_code`, `product_dm_t`, `residue_dm_t` and
#'   `area_ha`. Optional `n_input_kg_ha` enables the nitrogen adjustment;
#'   optional `water_regime` enables the irrigation adjustment.
#' @param method Root method: `"ensemble"` (default, weighted root:shoot +
#'   reference), `"root_shoot"` (root:shoot ratio only) or `"reference"`
#'   (reference below-ground biomass only).
#' @param weights Named list; `w_ref` is the reference weight in the ensemble
#'   (0-1, default 0.5), used only when `method = "ensemble"`.
#' @return The input tibble with `root_dm_t` and `method_root`.
#' @export
#' @examples
#' calculate_crop_roots(
#'   tibble::tibble(
#'     item_prod_code = "15",
#'     product_dm_t = 87.9,
#'     residue_dm_t = 135.75,
#'     area_ha = 40
#'   )
#' )
calculate_crop_roots <- function(
  x,
  method = c("ensemble", "root_shoot", "reference"),
  weights = list(w_ref = 0.5)
) {
  method <- rlang::arg_match(method)
  .crop_npp_validate(
    x,
    c("item_prod_code", "product_dm_t", "residue_dm_t", "area_ha"),
    "calculate_crop_roots"
  )
  w_ref <- .root_weight(weights)
  x |>
    .root_coefs_join() |>
    .root_n_factor() |>
    .root_irrigation_factor() |>
    .root_estimates() |>
    .root_combine(method, w_ref) |>
    dplyr::mutate(method_root = method) |>
    .root_cleanup()
}

#' Estimate total crop net primary production.
#'
#' Assembles total crop net primary production (product plus residue plus root
#' dry matter) by running [calculate_crop_residues()] then
#' [calculate_crop_roots()].
#'
#' @param x A tibble with `item_prod_code`, `production_t` and `area_ha`, plus
#'   any optional adjustment columns used by the residue and root steps.
#' @param residue_method Residue method passed to [calculate_crop_residues()].
#' @param root_method Root method passed to [calculate_crop_roots()].
#' @param weights Named list of ensemble weights; `w_ipcc` for residues and
#'   `w_ref` for roots (each 0-1, default 0.5).
#' @return The input tibble with `product_dm_t`, `yield_dm_t_ha`, `residue_dm_t`,
#'   `root_dm_t`, `crop_npp_dm_t`, `method_residue` and `method_root`.
#' @export
#' @examples
#' calculate_crop_npp(
#'   tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
#' )
calculate_crop_npp <- function(
  x,
  residue_method = "ensemble",
  root_method = "ensemble",
  weights = list(w_ipcc = 0.5, w_ref = 0.5)
) {
  x |>
    calculate_crop_residues(method = residue_method, weights = weights) |>
    calculate_crop_roots(method = root_method, weights = weights) |>
    dplyr::mutate(crop_npp_dm_t = product_dm_t + residue_dm_t + root_dm_t)
}

#' Estimate cropland NPP components including weeds.
#'
#' Assembles full cropland net primary production: scales weed biomass from
#' potential NPP and the `weed_npp_scaling` factors, then partitions crop and
#' weed biomass into dry matter, carbon and nitrogen via
#' [calculate_npp_carbon_nitrogen()].
#'
#' The `weed_npp_scaling` table is taken from Spain_Hist and is flagged
#' `to_be_revised`: it is Spain-specific and not validated for WHEP's global
#' scope. A `weed_scaling_to_be_revised` column records this on the output and a
#' one-time warning is emitted.
#'
#' @param x A tibble with `item_prod_code`, `area_ha`, `year`, `product_dm_t`,
#'   `residue_dm_t` and `root_dm_t` (e.g. the output of [calculate_crop_npp()]).
#'   When `npp_potential_dm_t_ha` is absent it is computed via
#'   [calculate_potential_npp()] (which, for the default `lpjml` method, needs
#'   `lon`/`lat`).
#' @param .by Optional character vector of grouping columns used to fill missing
#'   weed-scaling factors with the group mean.
#' @param potential A named list selecting the potential-NPP source: `method`
#'   (default `"lpjml"`) and `lpjml` options.
#' @return The input tibble with weed dry matter, the dry-matter / nitrogen /
#'   carbon partition for crop, weeds and total, and `weed_scaling_to_be_revised`.
#' @export
#' @examples
#' tibble::tibble(
#'   item_prod_code = "15", production_t = 100, area_ha = 40,
#'   year = 2000, npp_potential_dm_t_ha = 5
#' ) |>
#'   calculate_crop_npp() |>
#'   calculate_crop_npp_components()
calculate_crop_npp_components <- function(
  x,
  .by = NULL,
  potential = list(method = "lpjml")
) {
  .crop_npp_validate(
    x,
    c(
      "item_prod_code",
      "area_ha",
      "year",
      "product_dm_t",
      "residue_dm_t",
      "root_dm_t"
    ),
    "calculate_crop_npp_components"
  )
  x |>
    .components_potential(potential) |>
    .components_weeds(.by) |>
    calculate_npp_carbon_nitrogen()
}

#' Partition crop and weed NPP into dry matter, carbon and nitrogen.
#'
#' Converts crop NPP components (product, residue, root) and weed biomass to
#' nitrogen and carbon using the `bio_coefs` per-component coefficients and the
#' `weed_coefs` scalars. Root and weed below-ground nitrogen include
#' rhizodeposits. The residue-to-soil split is computed only when a
#' `residue_soil_dm_t` column (from [calculate_residue_destinies()]) is present.
#'
#' @param x A tibble with `item_prod_code`, `product_dm_t`, `residue_dm_t` and
#'   `root_dm_t`. Optional `weed_ag_dm_t` (above-ground weed dry matter; treated
#'   as 0 when absent), `crop_npp_dm_t` (kept when present) and
#'   `residue_soil_dm_t` (enables the soil-residue nitrogen and carbon split).
#' @return The input tibble with weed dry matter, and nitrogen (`*_n_t`) and
#'   carbon (`*_c_t`) for product, residue, root, weeds, crop NPP and total NPP.
#' @export
#' @examples
#' tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40) |>
#'   calculate_crop_npp() |>
#'   calculate_npp_carbon_nitrogen()
calculate_npp_carbon_nitrogen <- function(x) {
  .crop_npp_validate(
    x,
    c("item_prod_code", "product_dm_t", "residue_dm_t", "root_dm_t"),
    "calculate_npp_carbon_nitrogen"
  )
  x |>
    .npp_cn_join_coefs() |>
    .npp_cn_weeds() |>
    .npp_cn_crop() |>
    .npp_cn_soil_residue() |>
    .npp_cn_cleanup()
}

.npp_coef <- function(coefs, model, param, component = NULL) {
  mask <- coefs$model == model & coefs$parameter == param
  if (!is.null(component)) {
    mask <- mask & coefs$component == component
  }
  val <- coefs$value[mask]
  if (length(val) != 1L) {
    cli::cli_abort(
      "npp_model_coefs: no unique match for model {.val {model}}, \\
       parameter {.val {param}}."
    )
  }
  val
}

.potential_npp_miami <- function(x) {
  coefs <- whep::whep_coef_table("npp_model_coefs")
  t_max <- .npp_coef(coefs, "Miami", "Max_gCm2yr", "F_MAT")
  t_mid <- .npp_coef(coefs, "Miami", "Midpoint", "F_MAT")
  t_rate <- .npp_coef(coefs, "Miami", "Rate", "F_MAT")
  p_max <- .npp_coef(coefs, "Miami", "Max_gCm2yr", "F_MAP")
  p_rate <- .npp_coef(coefs, "Miami", "Rate", "F_MAP")
  dplyr::mutate(
    x,
    npp_potential_dm_t_ha = pmin(
      (t_max / (1 + exp(t_mid - t_rate * temp_c))) / 100,
      (p_max * (1 - exp(-p_rate * water_input_mm))) / 100
    )
  )
}

.potential_npp_nceas <- function(x) {
  coefs <- whep::whep_coef_table("npp_model_coefs")
  carbon_fraction <- whep::whep_coef_table("weed_coefs")$residue_c_kgdm_weed
  a_max <- .npp_coef(coefs, "NCEAS_nontree_ANPP", "Max_gCm2yr")
  a_rate <- .npp_coef(coefs, "NCEAS_nontree_ANPP", "Rate")
  dplyr::mutate(
    x,
    npp_potential_dm_t_ha = a_max *
      (1 - exp(-a_rate * water_input_mm)) /
      (100 * carbon_fraction)
  )
}

.potential_npp_rosenzweig <- function(x) {
  coefs <- whep::whep_coef_table("npp_model_coefs")
  slope <- .npp_coef(coefs, "Rosenzweig", "Slope")
  intercept <- .npp_coef(coefs, "Rosenzweig", "Intercept")
  dplyr::mutate(
    x,
    npp_potential_dm_t_ha = 10^(slope * log10(aet_mm) + intercept) / 100
  )
}

.potential_npp_lpjml <- function(x, options) {
  if (!all(c("lon", "lat", "year") %in% names(x))) {
    cli::cli_abort(
      "calculate_potential_npp(method = \"lpjml\") needs {.field lon}, \\
       {.field lat} and {.field year} to join the gridded grass NPP."
    )
  }
  # Gross above-ground grass NPP: grass_access_shares() keeps grazable = 1, so
  # .lpjml_grass_to_dm() applies only the above-ground (0.46) and carbon-to-DM
  # conversion, not the grazing access reduction.
  grass <- do.call(read_lpjml_grass_productivity, options) |>
    dplyr::mutate(
      lon = round(lon, 2),
      lat = round(lat, 2),
      npp_potential_dm_t_ha = .lpjml_grass_to_dm(
        grass_npp,
        grass_access_shares()
      )
    ) |>
    dplyr::select(lon, lat, year, npp_potential_dm_t_ha)
  x |>
    dplyr::mutate(lon = round(lon, 2), lat = round(lat, 2)) |>
    dplyr::inner_join(grass, by = c("lon", "lat", "year"))
}

.crop_npp_validate <- function(x, required, fn) {
  missing <- required[
    !purrr::map_lgl(required, function(col) {
      rlang::has_name(x, col)
    })
  ]
  if (length(missing) > 0) {
    cli::cli_abort("{fn}: missing required column{?s} {.field {missing}}.")
  }
  invisible(x)
}

.residue_weight <- function(weights) {
  w <- weights[["w_ipcc"]]
  if (is.null(w)) {
    w <- 0.5
  }
  if (w < 0 || w > 1) {
    cli::cli_abort("{.arg w_ipcc} must be between 0 and 1, not {w}.")
  }
  w
}

.residue_dm_conversions <- function(x) {
  coefs <- whep::whep_coef_table("bio_coefs") |>
    dplyr::select(
      item_prod_code,
      product_dm_kgfm,
      residue_dm_kgfm,
      residue_kg_product_fm_kg
    )
  x |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::left_join(coefs, by = "item_prod_code") |>
    dplyr::mutate(
      production_t = tidyr::replace_na(as.numeric(production_t), 0),
      product_dm_t = production_t * product_dm_kgfm,
      yield_dm_t_ha = dplyr::if_else(area_ha > 0, product_dm_t / area_ha, 0)
    )
}

.residue_ipcc <- function(x) {
  mapping <- whep::whep_coef_table("ipcc_crop_mapping") |>
    dplyr::select(item_prod_code, ipcc_crop, crop_group)
  coefs <- whep::whep_coef_table("ipcc_residue_coefs") |>
    dplyr::select(ipcc_crop, slope_ag, intercept_ag_dm_t_ha)
  x |>
    dplyr::left_join(mapping, by = "item_prod_code") |>
    dplyr::left_join(coefs, by = "ipcc_crop") |>
    dplyr::mutate(
      residue_ipcc_t_ha = dplyr::if_else(
        !is.na(slope_ag),
        slope_ag * yield_dm_t_ha + intercept_ag_dm_t_ha,
        NA_real_
      ),
      residue_ipcc_t = pmax(residue_ipcc_t_ha, 0) * area_ha
    )
}

.residue_ratio <- function(x) {
  dplyr::mutate(
    x,
    residue_ratio_t = production_t * residue_kg_product_fm_kg * residue_dm_kgfm
  )
}

.residue_irrigation_factor <- function(x) {
  if (!rlang::has_name(x, "water_regime")) {
    return(dplyr::mutate(x, residue_ratio_factor = 1))
  }
  irr <- whep::whep_coef_table("irrigation_adj") |>
    dplyr::select(water_regime, residue_ratio_factor)
  sens <- whep::whep_coef_table("irr_residue_crop_adj") |>
    dplyr::select(crop_group, irr_residue_sensitivity)
  x |>
    dplyr::left_join(irr, by = "water_regime") |>
    dplyr::left_join(sens, by = "crop_group") |>
    dplyr::mutate(
      residue_ratio_factor = tidyr::replace_na(residue_ratio_factor, 1),
      irr_residue_sensitivity = tidyr::replace_na(irr_residue_sensitivity, 0),
      residue_ratio_factor = 1 +
        irr_residue_sensitivity * (residue_ratio_factor - 1)
    )
}

.residue_variety_factor <- function(x) {
  if (!all(c("year", "region_hanpp") %in% names(x))) {
    return(dplyr::mutate(x, hi_correction_factor = 1))
  }
  adoption <- whep::whep_coef_table("modern_variety_adoption") |>
    dplyr::select(region_hanpp, crop_group, year, modern_share)
  ranges <- whep::whep_coef_table("hi_crop_ranges") |>
    dplyr::select(crop_group, hi_gap_factor)
  x |>
    dplyr::left_join(adoption, by = c("region_hanpp", "crop_group", "year")) |>
    dplyr::left_join(ranges, by = "crop_group") |>
    dplyr::mutate(
      modern_share = tidyr::replace_na(modern_share, 1),
      hi_gap_factor = tidyr::replace_na(hi_gap_factor, 1),
      hi_correction_factor = 1 + (1 - modern_share) * (hi_gap_factor - 1)
    )
}

.residue_combine <- function(x, method, w_ipcc) {
  dplyr::mutate(
    x,
    residue_ratio_adj_t = residue_ratio_t *
      residue_ratio_factor *
      hi_correction_factor,
    residue_ipcc_adj_t = residue_ipcc_t *
      residue_ratio_factor *
      hi_correction_factor,
    residue_dm_t = .residue_pick(
      method,
      w_ipcc,
      residue_ipcc_adj_t,
      residue_ratio_adj_t
    ),
    residue_dm_t = pmax(tidyr::replace_na(residue_dm_t, 0), 0)
  )
}

.residue_pick <- function(method, w_ipcc, ipcc, ratio) {
  switch(
    method,
    ipcc = ipcc,
    ratio = ratio,
    ensemble = dplyr::if_else(
      !is.na(ipcc),
      w_ipcc * ipcc + (1 - w_ipcc) * ratio,
      ratio
    )
  )
}

.residue_cleanup <- function(x) {
  dplyr::select(
    x,
    -dplyr::any_of(c(
      "product_dm_kgfm",
      "residue_dm_kgfm",
      "residue_kg_product_fm_kg",
      "ipcc_crop",
      "crop_group",
      "slope_ag",
      "intercept_ag_dm_t_ha",
      "residue_ipcc_t_ha",
      "residue_ipcc_t",
      "residue_ipcc_adj_t",
      "residue_ratio_t",
      "residue_ratio_adj_t",
      "residue_ratio_factor",
      "irr_residue_sensitivity",
      "hi_correction_factor",
      "modern_share",
      "hi_gap_factor"
    ))
  )
}

.root_weight <- function(weights) {
  w <- weights[["w_ref"]]
  if (is.null(w)) {
    w <- 0.5
  }
  if (w < 0 || w > 1) {
    cli::cli_abort("{.arg w_ref} must be between 0 and 1, not {w}.")
  }
  w
}

.root_coefs_join <- function(x) {
  bio <- whep::whep_coef_table("bio_coefs") |>
    dplyr::select(item_prod_code, root_shoot_ratio, bg_biomass_dm_kg_ha)
  mapping <- whep::whep_coef_table("ipcc_crop_mapping") |>
    dplyr::select(item_prod_code, ipcc_crop, crop_group)
  root <- whep::whep_coef_table("ipcc_root_coefs") |>
    dplyr::select(ipcc_crop, rs_default, bg_ref_dm_t_ha)
  x |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::select(
      -dplyr::any_of(c(
        "root_shoot_ratio",
        "bg_biomass_dm_kg_ha",
        "ipcc_crop",
        "crop_group"
      ))
    ) |>
    dplyr::left_join(bio, by = "item_prod_code") |>
    dplyr::left_join(mapping, by = "item_prod_code") |>
    dplyr::left_join(root, by = "ipcc_crop")
}

.root_n_factor <- function(x) {
  if (!rlang::has_name(x, "n_input_kg_ha")) {
    return(dplyr::mutate(x, n_rs_factor = 1))
  }
  adj <- whep::whep_coef_table("n_input_rs_adj")
  resp <- whep::whep_coef_table("crop_rs_n_response") |>
    dplyr::select(crop_group, rs_n_sensitivity)
  x |>
    dplyr::mutate(
      n_rs_idx = findInterval(n_input_kg_ha, vec = adj$n_input_min),
      n_rs_idx = pmax(1L, pmin(n_rs_idx, nrow(adj))),
      n_rs_factor_raw = adj$rs_adjustment[n_rs_idx]
    ) |>
    dplyr::left_join(resp, by = "crop_group") |>
    dplyr::mutate(
      rs_n_sensitivity = tidyr::replace_na(rs_n_sensitivity, 1),
      n_rs_factor = 1 + (n_rs_factor_raw - 1) * rs_n_sensitivity
    )
}

.root_irrigation_factor <- function(x) {
  if (!rlang::has_name(x, "water_regime")) {
    return(dplyr::mutate(x, rs_ratio_factor = 1))
  }
  irr <- whep::whep_coef_table("irrigation_adj") |>
    dplyr::select(water_regime, rs_ratio_factor)
  x |>
    dplyr::left_join(irr, by = "water_regime") |>
    dplyr::mutate(rs_ratio_factor = tidyr::replace_na(rs_ratio_factor, 1))
}

.root_estimates <- function(x) {
  dplyr::mutate(
    x,
    rs_base = dplyr::if_else(!is.na(rs_default), rs_default, root_shoot_ratio),
    rs_effective = rs_base * n_rs_factor * rs_ratio_factor,
    aerial_dm_t = product_dm_t + residue_dm_t,
    root_rs_t = aerial_dm_t * rs_effective,
    bg_ref_used = dplyr::case_when(
      !is.na(bg_ref_dm_t_ha) ~ bg_ref_dm_t_ha,
      !is.na(bg_biomass_dm_kg_ha) ~ bg_biomass_dm_kg_ha / 1000,
      TRUE ~ NA_real_
    ),
    root_ref_t = dplyr::if_else(
      !is.na(bg_ref_used),
      bg_ref_used * area_ha,
      NA_real_
    )
  )
}

.root_combine <- function(x, method, w_ref) {
  dplyr::mutate(
    x,
    root_dm_t = .root_pick(method, w_ref, root_rs_t, root_ref_t),
    root_dm_t = .root_cap(root_dm_t, aerial_dm_t, rs_base),
    root_dm_t = pmax(tidyr::replace_na(root_dm_t, 0), 0)
  )
}

.root_pick <- function(method, w_ref, rs, ref) {
  switch(
    method,
    root_shoot = rs,
    reference = ref,
    ensemble = dplyr::case_when(
      !is.na(rs) & !is.na(ref) ~ (1 - w_ref) * rs + w_ref * ref,
      !is.na(rs) ~ rs,
      !is.na(ref) ~ ref,
      TRUE ~ 0
    )
  )
}

.root_cap <- function(root, aerial, rs_base) {
  dplyr::case_when(
    is.na(rs_base) | aerial <= 0 ~ root,
    root / aerial > rs_base * 3 ~ aerial * rs_base * 3,
    TRUE ~ root
  )
}

.root_cleanup <- function(x) {
  dplyr::select(
    x,
    -dplyr::any_of(c(
      "ipcc_crop",
      "crop_group",
      "rs_default",
      "bg_ref_dm_t_ha",
      "root_shoot_ratio",
      "bg_biomass_dm_kg_ha",
      "n_rs_idx",
      "n_rs_factor_raw",
      "n_rs_factor",
      "rs_n_sensitivity",
      "rs_ratio_factor",
      "rs_base",
      "rs_effective",
      "aerial_dm_t",
      "root_rs_t",
      "bg_ref_used",
      "root_ref_t"
    ))
  )
}

.npp_cn_coef_cols <- function() {
  c(
    "product_n_kgdm",
    "residue_n_kgdm",
    "root_n_kgdm",
    "product_c_kgdm",
    "residue_c_kgdm",
    "root_c_kgdm",
    "rhizodeposit_n_kgn_krootn",
    "residue_dm_kgfm"
  )
}

.npp_cn_join_coefs <- function(x) {
  coefs <- whep::whep_coef_table("bio_coefs") |>
    dplyr::select(dplyr::all_of(c("item_prod_code", .npp_cn_coef_cols())))
  x |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::select(-dplyr::any_of(.npp_cn_coef_cols())) |>
    dplyr::left_join(coefs, by = "item_prod_code")
}

.npp_cn_weeds <- function(x) {
  weed <- whep::whep_coef_table("weed_coefs")
  if (!rlang::has_name(x, "weed_ag_dm_t")) {
    x <- dplyr::mutate(x, weed_ag_dm_t = 0)
  }
  dplyr::mutate(
    x,
    weed_ag_dm_t = tidyr::replace_na(weed_ag_dm_t, 0),
    weed_bg_dm_t = weed_ag_dm_t * weed$root_shoot_ratio_weed,
    weed_npp_dm_t = weed_ag_dm_t + weed_bg_dm_t,
    weed_ag_n_t = weed_ag_dm_t * weed$residue_n_kgdm_weed,
    weed_bg_n_t = weed_bg_dm_t *
      weed$root_n_kgdm_weed *
      (1 + weed$rhizodeposit_n_weed),
    weed_npp_n_t = weed_ag_n_t + weed_bg_n_t,
    weed_ag_c_t = weed_ag_dm_t * weed$residue_c_kgdm_weed,
    weed_bg_c_t = weed_bg_dm_t * weed$root_c_kgdm_weed,
    weed_npp_c_t = weed_ag_c_t + weed_bg_c_t
  )
}

.npp_cn_crop <- function(x) {
  has_crop_npp <- rlang::has_name(x, "crop_npp_dm_t")
  dplyr::mutate(
    x,
    crop_npp_dm_t = if (has_crop_npp) {
      crop_npp_dm_t
    } else {
      product_dm_t + residue_dm_t + root_dm_t
    },
    total_npp_dm_t = crop_npp_dm_t + weed_npp_dm_t,
    product_n_t = product_dm_t * product_n_kgdm,
    residue_n_t = residue_dm_t * residue_n_kgdm,
    root_n_t = root_dm_t * root_n_kgdm * (1 + rhizodeposit_n_kgn_krootn),
    crop_npp_n_t = product_n_t + residue_n_t + root_n_t,
    total_npp_n_t = crop_npp_n_t + weed_npp_n_t,
    product_c_t = product_dm_t * product_c_kgdm,
    residue_c_t = residue_dm_t * residue_c_kgdm,
    root_c_t = root_dm_t * root_c_kgdm,
    crop_npp_c_t = product_c_t + residue_c_t + root_c_t,
    total_npp_c_t = crop_npp_c_t + weed_npp_c_t,
    residue_fm_t = dplyr::if_else(
      residue_dm_kgfm > 0,
      residue_dm_t / residue_dm_kgfm,
      0
    )
  )
}

.npp_cn_soil_residue <- function(x) {
  if (!rlang::has_name(x, "residue_soil_dm_t")) {
    return(x)
  }
  dplyr::mutate(
    x,
    residue_soil_n_t = residue_soil_dm_t * residue_n_kgdm,
    residue_soil_c_t = residue_soil_dm_t * residue_c_kgdm
  )
}

.npp_cn_cleanup <- function(x) {
  dplyr::select(x, -dplyr::any_of(.npp_cn_coef_cols()))
}

.components_potential <- function(x, potential) {
  if (rlang::has_name(x, "npp_potential_dm_t_ha")) {
    return(x)
  }
  calculate_potential_npp(
    x,
    method = potential$method %||% "lpjml",
    lpjml = potential$lpjml %||% list()
  )
}

.components_weeds <- function(x, .by) {
  cli::cli_warn(
    c(
      "Weed scaling uses the Spain-specific {.field weed_npp_scaling} table.",
      i = "It is flagged {.field to_be_revised}: not validated for global scope."
    ),
    .frequency = "once",
    .frequency_id = "weed_npp_scaling_provisional"
  )
  scaling <- whep::whep_coef_table("weed_npp_scaling") |>
    dplyr::select(item_prod_code, year, weed_scaling)
  x |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::left_join(scaling, by = c("item_prod_code", "year")) |>
    .components_fill_scaling(.by) |>
    dplyr::mutate(
      weed_ag_dm_t = area_ha * weed_scaling * npp_potential_dm_t_ha,
      weed_scaling_to_be_revised = TRUE
    )
}

.components_fill_scaling <- function(x, .by) {
  if (is.null(.by)) {
    return(dplyr::mutate(
      x,
      weed_scaling = tidyr::replace_na(
        weed_scaling,
        mean(weed_scaling, na.rm = TRUE)
      )
    ))
  }
  dplyr::mutate(
    x,
    weed_scaling = dplyr::if_else(
      is.na(weed_scaling),
      mean(weed_scaling, na.rm = TRUE),
      weed_scaling
    ),
    .by = dplyr::all_of(.by)
  )
}
