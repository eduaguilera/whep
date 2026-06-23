#' Estimate symbiotic biological nitrogen fixation by crop legumes.
#'
#' Estimates symbiotic BNF from leguminous crops via two methods (an NPP-based
#' estimate and the Anglade product-based estimate). Environmental modifiers
#' (nitrogen inhibition, temperature, water) are applied only when their driver
#' columns are present in `x`; an absent driver leaves that modifier at 1.
#'
#' @param x A tibble with `item_prod_code`, `crop_npp_n_t` and `product_n_t`.
#'   Optional driver columns activate the modifiers: `n_synth_kg_ha` /
#'   `n_org_kg_ha` (nitrogen inhibition), `temp_c` (temperature),
#'   `water_input_mm` (or `precip_mm` + `irrig_mm`) and `pet_mm` (water).
#' @param symbiotic_params Named list overriding the symbiotic-BNF parameters
#'   `k_n_synth`, `k_n_org`, `t_opt`, `t_sigma`, `ai_threshold`.
#' @return The input tibble with the environmental factors, `ndfa_adj`,
#'   `crop_bnf_t` (NPP method), `crop_bnf_anglade_t` (Anglade method) and the
#'   per-product ratios `bnf_product_ratio_npp` / `bnf_product_ratio_anglade`.
#' @export
#' @examples
#' calculate_crop_bnf(
#'   tibble::tibble(item_prod_code = "176", crop_npp_n_t = 10, product_n_t = 5)
#' )
calculate_crop_bnf <- function(x, symbiotic_params = list()) {
  .bnf_validate_input(
    x,
    c("item_prod_code", "crop_npp_n_t", "product_n_t"),
    "calculate_crop_bnf"
  )
  if (nrow(x) == 0) {
    return(x)
  }
  p <- .bnf_symbiotic_params(symbiotic_params)
  x |>
    .bnf_ensure_env_cols() |>
    .bnf_join_params() |>
    .bnf_crop_factors(p) |>
    .bnf_crop_estimates()
}

#' Estimate symbiotic biological nitrogen fixation by weeds and cover crops.
#'
#' Estimates symbiotic BNF from leguminous weeds and seeded cover crops. The
#' legume fraction is a weighted average of spontaneous weeds and seeded cover
#' crops. Environmental modifiers activate only when their driver columns are
#' present.
#'
#' @param x A tibble with `weed_npp_n_t`, `land_use`, `legumes_seeded` and
#'   `seeded_cover_crop_share`. Optional `legumes_spontaneous` overrides the
#'   land-use default; the same environmental driver columns as
#'   [calculate_crop_bnf()] are supported.
#' @param symbiotic_params Named list overriding the symbiotic-BNF parameters
#'   (see [calculate_crop_bnf()]).
#' @return The input tibble with `weed_ndfa_ref`, `weed_ndfa`, `weed_leg_share`,
#'   `f_env_weed` and `weed_bnf_t`.
#' @export
#' @examples
#' calculate_weed_bnf(
#'   tibble::tibble(
#'     weed_npp_n_t = 10, land_use = "Cropland",
#'     legumes_seeded = 0, seeded_cover_crop_share = 0
#'   )
#' )
calculate_weed_bnf <- function(x, symbiotic_params = list()) {
  .bnf_validate_input(
    x,
    c("weed_npp_n_t", "land_use", "legumes_seeded", "seeded_cover_crop_share"),
    "calculate_weed_bnf"
  )
  if (nrow(x) == 0) {
    return(x)
  }
  p <- .bnf_symbiotic_params(symbiotic_params)
  x |>
    .bnf_ensure_env_cols() |>
    .bnf_weed_spont_legumes() |>
    .bnf_weed_estimates(p)
}

#' Estimate non-symbiotic biological nitrogen fixation.
#'
#' Estimates free-living and associative BNF in agricultural soils from a base
#' rate (crop-specific from `bnf`, or a default) scaled by environmental
#' modifiers for nitrogen, temperature, water, soil organic matter, pH and clay.
#' Each modifier activates only when its driver column is present.
#'
#' @param x A tibble with `area_ha`. Optional `nonsymbiotic_base_kg_ha` (or
#'   `item_prod_code` to join the crop-specific base rate), plus the nitrogen,
#'   temperature, water and soil (`som_pct`, `soil_ph`, `clay_pct`) drivers.
#' @param nonsymbiotic_params Named list overriding `nsbnf_default_kg_ha`,
#'   `k_n_synth`, `k_n_org`, `t_opt`, `t_sigma`, `ai_threshold`.
#' @param soil_params Named list overriding `k_som`, `som_ref`, `ph_opt`,
#'   `ph_sigma`, `k_clay`, `clay_ref`.
#' @return The input tibble with `nonsymbiotic_base_kg_ha`, the six
#'   `f_*_nonsymbiotic` modifiers, `f_env_nonsymbiotic` and `nonsymbiotic_bnf_t`.
#' @export
#' @examples
#' calculate_nonsymbiotic_bnf(tibble::tibble(area_ha = 40))
calculate_nonsymbiotic_bnf <- function(
  x,
  nonsymbiotic_params = list(),
  soil_params = list()
) {
  .bnf_validate_input(x, "area_ha", "calculate_nonsymbiotic_bnf")
  if (nrow(x) == 0) {
    return(x)
  }
  ns <- .bnf_nonsymbiotic_params(nonsymbiotic_params)
  soil <- .bnf_soil_params(soil_params)
  x |>
    .bnf_ensure_env_cols() |>
    .bnf_ensure_base_rate() |>
    .bnf_nonsymbiotic_estimates(ns, soil)
}

#' Estimate total biological nitrogen fixation.
#'
#' Sums the three BNF components: symbiotic crop legumes, symbiotic weeds/cover
#' crops, and non-symbiotic free-living fixation, by running
#' [calculate_crop_bnf()], [calculate_weed_bnf()] and
#' [calculate_nonsymbiotic_bnf()]. When a `climate_type` column is present, the
#' climate-specific parameters from `bnf_climate_params` override the relevant
#' defaults per climate type.
#'
#' @param x A tibble carrying the required columns of all three component
#'   functions, optionally with a `climate_type` column.
#' @param symbiotic_params,nonsymbiotic_params,soil_params Named lists passed to
#'   the component functions (see those functions).
#' @return The input tibble with all component columns plus `fert_type` (`"BNF"`)
#'   and `bnf_t` (total BNF).
#' @export
#' @examples
#' calculate_bnf(
#'   tibble::tibble(
#'     item_prod_code = "176", crop_npp_n_t = 10, product_n_t = 5,
#'     weed_npp_n_t = 4, land_use = "Cropland", legumes_seeded = 0,
#'     seeded_cover_crop_share = 0, area_ha = 40
#'   )
#' )
calculate_bnf <- function(
  x,
  symbiotic_params = list(),
  nonsymbiotic_params = list(),
  soil_params = list()
) {
  if (rlang::has_name(x, "climate_type")) {
    return(.bnf_by_climate(
      x,
      symbiotic_params,
      nonsymbiotic_params,
      soil_params
    ))
  }
  .bnf_combine_components(
    x,
    symbiotic_params,
    nonsymbiotic_params,
    soil_params
  )
}

#' Summarise biological nitrogen fixation results.
#'
#' Aggregates the per-row BNF components into group totals, shares and mean
#' modifiers.
#'
#' @param x A tibble with `crop_bnf_t`, `weed_bnf_t`, `nonsymbiotic_bnf_t` and
#'   `bnf_t` (the output of [calculate_bnf()]).
#' @param group_by Character vector of grouping columns (default
#'   `"item_prod_code"`); use `NULL` for an overall summary.
#' @return A tibble with per-group counts, BNF totals, component percentages and
#'   mean environmental factors.
#' @export
#' @examples
#' tibble::tibble(
#'   item_prod_code = "176", crop_npp_n_t = 10, product_n_t = 5,
#'   weed_npp_n_t = 4, land_use = "Cropland", legumes_seeded = 0,
#'   seeded_cover_crop_share = 0, area_ha = 40
#' ) |>
#'   calculate_bnf() |>
#'   summarize_bnf()
summarize_bnf <- function(x, group_by = "item_prod_code") {
  .bnf_validate_input(
    x,
    c("crop_bnf_t", "weed_bnf_t", "nonsymbiotic_bnf_t", "bnf_t"),
    "summarize_bnf"
  )
  x <- .summarize_bnf_prepare(x)
  groups <- intersect(group_by, names(x))
  if (length(groups) > 0) {
    x <- dplyr::group_by(x, dplyr::across(dplyr::all_of(groups)))
  }
  x |>
    dplyr::summarise(
      n = dplyr::n(),
      legume_category = dplyr::if_else(
        dplyr::n_distinct(legume_category) == 1L,
        dplyr::first(legume_category),
        NA_character_
      ),
      total_crop_bnf_t = sum(crop_bnf_t, na.rm = TRUE),
      total_weed_bnf_t = sum(weed_bnf_t, na.rm = TRUE),
      total_nonsymbiotic_bnf_t = sum(nonsymbiotic_bnf_t, na.rm = TRUE),
      total_bnf_t = sum(bnf_t, na.rm = TRUE),
      mean_ndfa_adj = mean(ndfa_adj, na.rm = TRUE),
      mean_f_env_symbiotic = mean(f_env_symbiotic, na.rm = TRUE),
      mean_f_env_nonsymbiotic = mean(f_env_nonsymbiotic, na.rm = TRUE),
      .groups = "drop"
    ) |>
    .summarize_bnf_pct()
}

# ---- Private helpers --------------------------------------------------

.bnf_symbiotic_params <- function(p) {
  utils::modifyList(
    list(
      k_n_synth = 0.0035,
      k_n_org = 0.0018,
      t_opt = 22,
      t_sigma = 12,
      ai_threshold = 0.45
    ),
    p
  )
}

.bnf_validate_input <- function(x, required, fn) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{fn}: input must be a data frame.")
  }
  if (nrow(x) == 0) {
    cli::cli_warn("{fn}: input has zero rows.")
    return(invisible(x))
  }
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

.bnf_ensure_env_cols <- function(x) {
  if (!rlang::has_name(x, "n_synth_kg_ha")) {
    x[["n_synth_kg_ha"]] <- 0
  }
  if (!rlang::has_name(x, "n_org_kg_ha")) {
    x[["n_org_kg_ha"]] <- 0
  }
  if (!rlang::has_name(x, "temp_c")) {
    x[["temp_c"]] <- NA_real_
  }
  if (!rlang::has_name(x, "pet_mm")) {
    x[["pet_mm"]] <- NA_real_
  }
  if (!rlang::has_name(x, "som_pct")) {
    x[["som_pct"]] <- NA_real_
  }
  if (!rlang::has_name(x, "soil_ph")) {
    x[["soil_ph"]] <- NA_real_
  }
  if (!rlang::has_name(x, "clay_pct")) {
    x[["clay_pct"]] <- NA_real_
  }
  .bnf_ensure_water(x)
}

.bnf_ensure_water <- function(x) {
  if (rlang::has_name(x, "water_input_mm")) {
    return(x)
  }
  if (!rlang::has_name(x, "precip_mm")) {
    x[["water_input_mm"]] <- NA_real_
    return(x)
  }
  irrig <- if (rlang::has_name(x, "irrig_mm")) x[["irrig_mm"]] else 0
  x[["water_input_mm"]] <- x[["precip_mm"]] + irrig
  x
}

.bnf_join_params <- function(x) {
  if (all(c("ndfa", "nonsymbiotic_base_kg_ha") %in% names(x))) {
    return(x)
  }
  names_bnf <- whep::whep_coef_table("names_bnf") |>
    dplyr::select(item_prod_code, name_bnf)
  bnf <- whep::whep_coef_table("bnf") |>
    dplyr::select(
      name_bnf,
      ndfa,
      n_harvest_index,
      below_ground_n_ratio,
      nonsymbiotic_base_kg_ha,
      leguminous_share
    )
  params <- dplyr::left_join(names_bnf, bnf, by = "name_bnf")
  x |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::left_join(params, by = "item_prod_code")
}

.bnf_f_n_symbiotic <- function(
  n_synth,
  n_org,
  k_synth = 0.0035,
  k_org = 0.0018
) {
  exp(-k_synth * n_synth - k_org * n_org)
}

.bnf_f_n_nonsymbiotic <- function(
  n_synth,
  n_org,
  k_synth = 0.005,
  k_org = 0.0025
) {
  exp(-k_synth * n_synth - k_org * n_org)
}

.bnf_f_temperature <- function(temp_c, t_opt = 22, sigma = 12) {
  sigma_eff <- dplyr::if_else(temp_c <= t_opt, sigma, sigma * 0.5)
  exp(-((temp_c - t_opt)^2) / (2 * sigma_eff^2))
}

.bnf_f_water <- function(water_mm, pet_mm, ai_threshold = 0.45, floor = 0.35) {
  ai <- dplyr::if_else(pet_mm > 0, water_mm / pet_mm, 1)
  pmin(1, floor + (1 - floor) * pmin(ai, ai_threshold) / ai_threshold)
}

.bnf_f_som <- function(som_pct, k_som = 2.0, som_ref = 2.5) {
  (som_pct / (som_pct + k_som)) / (som_ref / (som_ref + k_som))
}

.bnf_f_ph <- function(ph, ph_opt = 6.8, sigma = 1.5) {
  exp(-((ph - ph_opt)^2) / (2 * sigma^2))
}

.bnf_f_clay <- function(clay_pct, k_clay = 20, clay_ref = 25) {
  (clay_pct / (clay_pct + k_clay)) / (clay_ref / (clay_ref + k_clay))
}

.bnf_dampen_product <- function(...) {
  factors <- list(...)
  n <- length(factors)
  if (n == 0) {
    return(1)
  }
  # Soft-dampen the product (exponent 2/n, deliberately NOT the geometric mean's
  # 1/n) so several moderate stresses do not collapse the combined factor toward
  # zero. Calibrated against Keuter et al. (2014) and Ladha et al. (2022).
  Reduce(`*`, factors)^(2 / n)
}

.bnf_crop_factors <- function(x, p) {
  dplyr::mutate(
    x,
    n_total_kg_ha = n_synth_kg_ha + n_org_kg_ha,
    f_nitrogen_symbiotic = .bnf_f_n_symbiotic(
      n_synth_kg_ha,
      n_org_kg_ha,
      p$k_n_synth,
      p$k_n_org
    ),
    f_temperature_symbiotic = dplyr::if_else(
      !is.na(temp_c),
      .bnf_f_temperature(temp_c, p$t_opt, p$t_sigma),
      1
    ),
    f_water_symbiotic = dplyr::if_else(
      !is.na(water_input_mm) & !is.na(pet_mm) & pet_mm > 0,
      .bnf_f_water(water_input_mm, pet_mm, p$ai_threshold),
      1
    ),
    f_env_symbiotic = .bnf_dampen_product(
      f_nitrogen_symbiotic,
      f_temperature_symbiotic,
      f_water_symbiotic
    ),
    ndfa_adj = dplyr::if_else(
      !is.na(ndfa),
      pmin(ndfa * f_env_symbiotic, ndfa),
      NA_real_
    )
  )
}

.bnf_combine_components <- function(x, sp, nsp, soil) {
  x |>
    calculate_crop_bnf(symbiotic_params = sp) |>
    calculate_weed_bnf(symbiotic_params = sp) |>
    calculate_nonsymbiotic_bnf(nonsymbiotic_params = nsp, soil_params = soil) |>
    dplyr::mutate(
      fert_type = "BNF",
      bnf_t = crop_bnf_t + weed_bnf_t + nonsymbiotic_bnf_t
    )
}

.bnf_by_climate <- function(x, sp, nsp, soil) {
  clim <- whep::whep_coef_table("bnf_climate_params")
  x |>
    dplyr::mutate(.bnf_row = dplyr::row_number()) |>
    dplyr::group_split(climate_type) |>
    purrr::map(function(grp) {
      p <- .bnf_climate_override(clim, grp$climate_type[1], sp, nsp, soil)
      .bnf_combine_components(grp, p$sp, p$nsp, p$soil)
    }) |>
    purrr::list_rbind() |>
    dplyr::arrange(.bnf_row) |>
    dplyr::select(-".bnf_row")
}

.bnf_climate_override <- function(clim, climate_type, sp, nsp, soil) {
  row <- clim[clim$climate_type == climate_type, ]
  if (nrow(row) == 0) {
    return(list(sp = sp, nsp = nsp, soil = soil))
  }
  sp <- utils::modifyList(
    sp,
    list(t_sigma = row$t_sigma_symb, ai_threshold = row$ai_threshold)
  )
  nsp <- utils::modifyList(
    nsp,
    list(t_sigma = row$t_sigma_ns, ai_threshold = row$ai_threshold)
  )
  soil <- utils::modifyList(
    soil,
    list(ph_opt = row$ph_opt, ph_sigma = row$ph_sigma, som_ref = row$som_ref)
  )
  list(sp = sp, nsp = nsp, soil = soil)
}

.summarize_bnf_prepare <- function(x) {
  if (!rlang::has_name(x, "ndfa_adj")) {
    x[["ndfa_adj"]] <- NA_real_
  }
  if (!rlang::has_name(x, "f_env_symbiotic")) {
    x[["f_env_symbiotic"]] <- NA_real_
  }
  if (!rlang::has_name(x, "f_env_nonsymbiotic")) {
    x[["f_env_nonsymbiotic"]] <- NA_real_
  }
  if (rlang::has_name(x, "legume_category")) {
    return(x)
  }
  if (!rlang::has_name(x, "name_bnf")) {
    x[["legume_category"]] <- NA_character_
    return(x)
  }
  pure_legs <- whep::whep_coef_table("pure_legs") |>
    dplyr::select(name_bnf, legume_category)
  dplyr::left_join(x, pure_legs, by = "name_bnf")
}

.summarize_bnf_pct <- function(x) {
  dplyr::mutate(
    x,
    pct_crop_bnf = .bnf_pct(total_crop_bnf_t, total_bnf_t),
    pct_weed_bnf = .bnf_pct(total_weed_bnf_t, total_bnf_t),
    pct_nonsymbiotic_bnf = .bnf_pct(total_nonsymbiotic_bnf_t, total_bnf_t)
  )
}

.bnf_pct <- function(part, total) {
  dplyr::if_else(total > 0, 100 * part / total, NA_real_)
}

.bnf_nonsymbiotic_params <- function(p) {
  utils::modifyList(
    list(
      nsbnf_default_kg_ha = 5,
      k_n_synth = 0.005,
      k_n_org = 0.0025,
      t_opt = 22,
      t_sigma = 14,
      ai_threshold = 0.45
    ),
    p
  )
}

.bnf_soil_params <- function(p) {
  utils::modifyList(
    list(
      k_som = 2.0,
      som_ref = 2.5,
      ph_opt = 6.8,
      ph_sigma = 1.5,
      k_clay = 20,
      clay_ref = 25
    ),
    p
  )
}

.bnf_ensure_base_rate <- function(x) {
  if (rlang::has_name(x, "nonsymbiotic_base_kg_ha")) {
    return(x)
  }
  if (rlang::has_name(x, "item_prod_code")) {
    return(.bnf_join_params(x))
  }
  dplyr::mutate(x, nonsymbiotic_base_kg_ha = NA_real_)
}

# Apply an environmental modifier only where its driver is present (an absent
# driver leaves the factor at 1). The modifier returns NA for NA drivers, which
# if_else then replaces with 1.
.bnf_modifier <- function(driver, value) {
  dplyr::if_else(!is.na(driver), value, 1)
}

.bnf_nonsymbiotic_factors <- function(x, ns, soil) {
  dplyr::mutate(
    x,
    f_nitrogen_nonsymbiotic = .bnf_f_n_nonsymbiotic(
      n_synth_kg_ha,
      n_org_kg_ha,
      ns$k_n_synth,
      ns$k_n_org
    ),
    f_temperature_nonsymbiotic = .bnf_modifier(
      temp_c,
      .bnf_f_temperature(temp_c, ns$t_opt, ns$t_sigma)
    ),
    f_water_nonsymbiotic = dplyr::if_else(
      !is.na(water_input_mm) & !is.na(pet_mm) & pet_mm > 0,
      .bnf_f_water(water_input_mm, pet_mm, ns$ai_threshold),
      1
    ),
    f_som_nonsymbiotic = .bnf_modifier(
      som_pct,
      .bnf_f_som(som_pct, soil$k_som, soil$som_ref)
    ),
    f_ph_nonsymbiotic = .bnf_modifier(
      soil_ph,
      .bnf_f_ph(soil_ph, soil$ph_opt, soil$ph_sigma)
    ),
    f_clay_nonsymbiotic = .bnf_modifier(
      clay_pct,
      .bnf_f_clay(clay_pct, soil$k_clay, soil$clay_ref)
    ),
    f_env_nonsymbiotic = .bnf_dampen_product(
      f_nitrogen_nonsymbiotic,
      f_temperature_nonsymbiotic,
      f_water_nonsymbiotic,
      f_som_nonsymbiotic,
      f_ph_nonsymbiotic,
      f_clay_nonsymbiotic
    )
  )
}

.bnf_nonsymbiotic_estimates <- function(x, ns, soil) {
  x |>
    dplyr::mutate(
      nonsymbiotic_base_kg_ha = dplyr::coalesce(
        nonsymbiotic_base_kg_ha,
        ns$nsbnf_default_kg_ha
      )
    ) |>
    .bnf_nonsymbiotic_factors(ns, soil) |>
    dplyr::mutate(
      nonsymbiotic_bnf_t = nonsymbiotic_base_kg_ha *
        f_env_nonsymbiotic *
        area_ha /
        1000
    )
}

.bnf_weed_spont_legumes <- function(x) {
  if (rlang::has_name(x, "legumes_spontaneous")) {
    return(x)
  }
  legs <- whep::whep_coef_table("legs_spontweeds")
  cropland <- legs$legumes_spontaneous[legs$land_use == "Cropland"]
  other <- legs$legumes_spontaneous[legs$land_use == "Other"]
  dplyr::mutate(
    x,
    legumes_spontaneous = dplyr::if_else(
      land_use == "Cropland",
      cropland,
      other
    )
  )
}

.bnf_weed_estimates <- function(x, p) {
  weed_ndfa_ref <- whep::whep_coef_table("bnf") |>
    dplyr::filter(name_bnf == "Weeds") |>
    dplyr::pull(ndfa)
  dplyr::mutate(
    x,
    f_env_weed = .bnf_dampen_product(
      .bnf_f_n_symbiotic(n_synth_kg_ha, n_org_kg_ha, p$k_n_synth, p$k_n_org),
      dplyr::if_else(
        !is.na(temp_c),
        .bnf_f_temperature(temp_c, p$t_opt, p$t_sigma),
        1
      ),
      dplyr::if_else(
        !is.na(water_input_mm) & !is.na(pet_mm) & pet_mm > 0,
        .bnf_f_water(water_input_mm, pet_mm, p$ai_threshold),
        1
      )
    ),
    weed_ndfa_ref = weed_ndfa_ref,
    weed_ndfa = pmin(weed_ndfa_ref * f_env_weed, weed_ndfa_ref),
    legumes_seeded = tidyr::replace_na(legumes_seeded, 0),
    seeded_cover_crop_share = dplyr::if_else(
      land_use == "Cropland",
      seeded_cover_crop_share,
      0
    ),
    weed_leg_share = (legumes_spontaneous * (1 - seeded_cover_crop_share)) +
      (legumes_seeded * seeded_cover_crop_share),
    weed_bnf_t = tidyr::replace_na(
      weed_npp_n_t * weed_ndfa * weed_leg_share,
      0
    )
  )
}

.bnf_crop_estimates <- function(x) {
  dplyr::mutate(
    x,
    crop_bnf_t = dplyr::if_else(
      !is.na(ndfa_adj),
      crop_npp_n_t * ndfa_adj * leguminous_share,
      0
    ),
    crop_bnf_anglade_t = dplyr::if_else(
      !is.na(ndfa_adj) & !is.na(n_harvest_index) & n_harvest_index > 0,
      product_n_t *
        leguminous_share *
        ndfa_adj *
        below_ground_n_ratio /
        n_harvest_index,
      0
    ),
    bnf_product_ratio_npp = dplyr::if_else(
      product_n_t > 0,
      crop_bnf_t / product_n_t,
      NA_real_
    ),
    bnf_product_ratio_anglade = dplyr::if_else(
      product_n_t > 0,
      crop_bnf_anglade_t / product_n_t,
      NA_real_
    )
  )
}
