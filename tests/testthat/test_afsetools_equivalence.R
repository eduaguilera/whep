# Live numerical-equivalence regression against the afsetools originals.
#
# Compares the migrated whep functions and the afsetools originals on shared
# fixtures across the happy path AND every conditional adjustment branch
# (residue irrigation / modern-variety, root nitrogen / irrigation, BNF
# environmental modifiers, weed seeded cover crops, non-symbiotic soil factors),
# plus a multi-row case. afsetools is the ground truth, so a faithfully ported
# branch matches and a mis-ported one fails here.
#
# Skips unless a full afsetools is installed with the source BNF/NPP functions
# and its load_general_data() data (true on a dev machine, not on CI). The
# afsetools functions are not exported, so they are reached via getFromNamespace.

.afse <- function(n) utils::getFromNamespace(n, "afsetools")

.afse_ready <- function() {
  if (!requireNamespace("afsetools", quietly = TRUE)) {
    return(FALSE)
  }
  fns <- c(
    "calculate_crop_residues",
    "calculate_crop_roots",
    "calc_crop_bnf",
    "calc_weed_bnf",
    "calc_nonsymbiotic_bnf",
    "calc_bnf",
    "load_general_data"
  )
  all(vapply(
    fns,
    function(f) exists(f, where = asNamespace("afsetools")),
    logical(1)
  ))
}

.eq <- function(wh, af, pairs) {
  for (p in pairs) {
    testthat::expect_equal(
      wh[[p[[1]]]],
      af[[p[[2]]]],
      tolerance = 1e-6,
      info = paste0(p[[1]], " vs ", p[[2]])
    )
  }
}

test_that("whep ports match afsetools across all branches", {
  testthat::skip_if_not(
    .afse_ready(),
    "afsetools with source functions not installed"
  )
  loaded <- tryCatch(
    {
      eval(
        quote(utils::getFromNamespace("load_general_data", "afsetools")()),
        envir = globalenv()
      )
      TRUE
    },
    error = function(e) FALSE
  )
  testthat::skip_if_not(loaded, "afsetools::load_general_data() unavailable")

  ## ---- residues: plain, +irrigation, +variety, +both, multi-row ----
  res <- function(
    af_extra = NULL,
    wh_extra = NULL,
    items = "Wheat",
    codes = "15",
    prod = 100,
    area = 40
  ) {
    af_in <- data.frame(
      Name_biomass = items,
      Prod_ygpit_Mg = prod,
      Area_ygpit_ha = area
    )
    wh_in <- tibble::tibble(
      item_prod_code = codes,
      production_t = prod,
      area_ha = area
    )
    if (!is.null(af_extra)) {
      af_in <- cbind(af_in, af_extra)
    }
    if (!is.null(wh_extra)) {
      wh_in <- dplyr::bind_cols(wh_in, wh_extra)
    }
    list(
      wh = whep::calculate_crop_residues(wh_in),
      af = .afse("calculate_crop_residues")(af_in)
    )
  }
  pair_res <- list(
    c("residue_dm_t", "Residue_MgDM"),
    c("product_dm_t", "Prod_MgDM")
  )

  r0 <- res()
  .eq(r0$wh, r0$af, pair_res)

  # Use crops/years where the adjustment factor is genuinely != 1: maize is
  # irrigation-sensitive (wheat is not); 1960 has partial modern-variety
  # adoption (2000 is fully adopted, so its factor would be 1).
  r_irr <- res(
    items = "Maize",
    codes = "56",
    prod = 200,
    area = 60,
    af_extra = data.frame(Water_regime = "Irrigated"),
    wh_extra = tibble::tibble(water_regime = "Irrigated")
  )
  .eq(r_irr$wh, r_irr$af, pair_res)

  r_var <- res(
    af_extra = data.frame(Year = 1960, region_HANPP = "West Europe"),
    wh_extra = tibble::tibble(year = 1960, region_hanpp = "West Europe")
  )
  .eq(r_var$wh, r_var$af, pair_res)

  r_both <- res(
    items = "Maize",
    codes = "56",
    prod = 200,
    area = 60,
    af_extra = data.frame(
      Water_regime = "Irrigated",
      Year = 1960,
      region_HANPP = "West Europe"
    ),
    wh_extra = tibble::tibble(
      water_regime = "Irrigated",
      year = 1960,
      region_hanpp = "West Europe"
    )
  )
  .eq(r_both$wh, r_both$af, pair_res)

  r_multi <- res(
    items = c("Wheat", "Beans"),
    codes = c("15", "176"),
    prod = c(100, 50),
    area = c(40, 20)
  )
  .eq(r_multi$wh, r_multi$af, pair_res)

  ## ---- roots: plain, +nitrogen, +irrigation, +both ----
  pr <- r0$wh$product_dm_t
  re <- r0$wh$residue_dm_t
  root <- function(af_extra = NULL, wh_extra = NULL) {
    af_in <- data.frame(
      Name_biomass = "Wheat",
      Prod_MgDM = pr,
      Residue_MgDM = re,
      Area_ygpit_ha = 40
    )
    wh_in <- tibble::tibble(
      item_prod_code = "15",
      product_dm_t = pr,
      residue_dm_t = re,
      area_ha = 40
    )
    if (!is.null(af_extra)) {
      af_in <- cbind(af_in, af_extra)
    }
    if (!is.null(wh_extra)) {
      wh_in <- dplyr::bind_cols(wh_in, wh_extra)
    }
    list(
      wh = whep::calculate_crop_roots(wh_in),
      af = .afse("calculate_crop_roots")(af_in)
    )
  }
  pair_root <- list(c("root_dm_t", "Root_MgDM"))

  .eq(root()$wh, root()$af, pair_root)
  k_n <- root(
    af_extra = data.frame(N_input_kgha = 150),
    wh_extra = tibble::tibble(n_input_kg_ha = 150)
  )
  .eq(k_n$wh, k_n$af, pair_root)
  k_irr <- root(
    af_extra = data.frame(Water_regime = "Irrigated"),
    wh_extra = tibble::tibble(water_regime = "Irrigated")
  )
  .eq(k_irr$wh, k_irr$af, pair_root)
  k_both <- root(
    af_extra = data.frame(Water_regime = "Irrigated", N_input_kgha = 150),
    wh_extra = tibble::tibble(water_regime = "Irrigated", n_input_kg_ha = 150)
  )
  .eq(k_both$wh, k_both$af, pair_root)

  ## ---- crop BNF: plain + full environmental modifiers ----
  pair_bnf <- list(
    c("crop_bnf_t", "CropBNF"),
    c("crop_bnf_anglade_t", "CropBNF2")
  )
  b0_af <- .afse("calc_crop_bnf")(
    data.frame(Name_biomass = "Beans", Crop_NPP_MgN = 10, Prod_MgN = 5)
  )
  b0_wh <- whep::calculate_crop_bnf(
    tibble::tibble(item_prod_code = "176", crop_npp_n_t = 10, product_n_t = 5)
  )
  .eq(b0_wh, b0_af, pair_bnf)

  b_env_af <- .afse("calc_crop_bnf")(data.frame(
    Name_biomass = "Beans",
    Crop_NPP_MgN = 10,
    Prod_MgN = 5,
    N_synth_kgha = 100,
    TMP = 18,
    WaterInput_mm = 400,
    PET_mm = 800
  ))
  b_env_wh <- whep::calculate_crop_bnf(tibble::tibble(
    item_prod_code = "176",
    crop_npp_n_t = 10,
    product_n_t = 5,
    n_synth_kg_ha = 100,
    temp_c = 18,
    water_input_mm = 400,
    pet_mm = 800
  ))
  .eq(b_env_wh, b_env_af, c(pair_bnf, list(c("f_env_symbiotic", "f_env_symb"))))

  ## ---- weed BNF: plain + seeded cover crops + env ----
  pair_weed <- list(
    c("weed_bnf_t", "WeedsBNF"),
    c("weed_leg_share", "Weeds_leg_share")
  )
  w_af <- .afse("calc_weed_bnf")(data.frame(
    Weeds_NPP_MgN = 10,
    LandUse = "Cropland",
    Legs_Seeded = 0.5,
    Seeded_CC_share = 0.3,
    N_synth_kgha = 50,
    TMP = 16
  ))
  w_wh <- whep::calculate_weed_bnf(tibble::tibble(
    weed_npp_n_t = 10,
    land_use = "Cropland",
    legumes_seeded = 0.5,
    seeded_cover_crop_share = 0.3,
    n_synth_kg_ha = 50,
    temp_c = 16
  ))
  .eq(w_wh, w_af, pair_weed)

  ## ---- non-symbiotic BNF: plain + soil + env ----
  n_af <- .afse("calc_nonsymbiotic_bnf")(data.frame(
    Area_ygpit_ha = 40,
    N_synth_kgha = 80,
    TMP = 20,
    WaterInput_mm = 500,
    PET_mm = 900,
    SOM_pct = 3,
    soil_pH = 6.5,
    clay_pct = 30
  ))
  n_wh <- whep::calculate_nonsymbiotic_bnf(tibble::tibble(
    area_ha = 40,
    n_synth_kg_ha = 80,
    temp_c = 20,
    water_input_mm = 500,
    pet_mm = 900,
    som_pct = 3,
    soil_ph = 6.5,
    clay_pct = 30
  ))
  .eq(
    n_wh,
    n_af,
    list(c("nonsymbiotic_bnf_t", "NSBNF"), c("f_env_nonsymbiotic", "f_env_ns"))
  )

  ## ---- master calc_bnf: total BNF ----
  master_in_af <- data.frame(
    Name_biomass = "Beans",
    Crop_NPP_MgN = 10,
    Prod_MgN = 5,
    Weeds_NPP_MgN = 4,
    LandUse = "Cropland",
    Legs_Seeded = 0,
    Seeded_CC_share = 0,
    Area_ygpit_ha = 40
  )
  master_in_wh <- tibble::tibble(
    item_prod_code = "176",
    crop_npp_n_t = 10,
    product_n_t = 5,
    weed_npp_n_t = 4,
    land_use = "Cropland",
    legumes_seeded = 0,
    seeded_cover_crop_share = 0,
    area_ha = 40
  )
  .eq(
    whep::calculate_bnf(master_in_wh),
    .afse("calc_bnf")(master_in_af),
    list(c("bnf_t", "BNF"))
  )

  ## ---- master calc_bnf with climate_type + environmental drivers ----
  # climate_type only bites when the env/soil drivers are present (they set the
  # climate-specific t_sigma / ai_threshold / pH / SOM-ref). afsetools applies
  # these per row; whep applies them per climate-type group.
  clim_af <- cbind(
    master_in_af,
    data.frame(
      climate_type = "Mediterranean",
      TMP = 28,
      WaterInput_mm = 300,
      PET_mm = 1000,
      SOM_pct = 2,
      soil_pH = 7.5,
      clay_pct = 20
    )
  )
  clim_wh <- dplyr::mutate(
    master_in_wh,
    climate_type = "Mediterranean",
    temp_c = 28,
    water_input_mm = 300,
    pet_mm = 1000,
    som_pct = 2,
    soil_ph = 7.5,
    clay_pct = 20
  )
  .eq(
    whep::calculate_bnf(clim_wh),
    .afse("calc_bnf")(clim_af),
    list(c("bnf_t", "BNF"))
  )
})
