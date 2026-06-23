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
