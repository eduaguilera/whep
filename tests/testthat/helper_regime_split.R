# Shared fixtures for the yield-weighted rainfed/irrigated split of the
# carbon chain (test_carbon_inputs.R) and the nitrogen chain
# (test_n_balance_regime.R). One cell-crop-year grain, both chains: the
# fixtures are built so the two chains see the same production, the same
# irrigated share and the same ratio table for every cell-crop-year they
# share, which is what lets a test compare the ratio each one applies.
#
# The golden outputs in fixtures/carbon_inputs_regime_golden.rds were written
# from these fixtures by the code before the yield split existed (whep main
# at b1da02e4), so they must not change without regenerating that file. The
# split now re-evaluates the residue model on each part, irrigation terms
# included, so the golden is reproduced with those terms switched off
# (`.rs_neutral_irrigation()`).

# The per-crop cropland layer, as build_soil_carbon_inputs() returns it at
# grid resolution: four components whose sum is the total, and a humified
# fraction that is their carbon-weighted mean at the package coefficients.
.rs_cropland <- function() {
  h <- whep:::.sci_humification_lookup(whep::residue_humification)
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year, ~crop_area_ha,
    ~residue_c_mgc_ha_yr, ~root_c_mgc_ha_yr, ~weed_c_mgc_ha_yr,
    ~manure_c_mgc_ha_yr, ~input_cn,
    0.25, 40.25, 203L, "15",  2010L, 40, 1.20, 0.80, 0.10, 0.50, 25,
    0.25, 40.25, 203L, "56",  2010L, 30, 1.60, 0.90, 0.00, 0.30, 30,
    0.25, 40.25, 203L, "260", 2010L, 20, 0.70, 0.50, 0.20, 0.00, 40,
    0.25, 40.25, 203L, "44",  2010L, 10, 1.00, 0.60, 0.00, 0.20, 28,
    0.75, 40.25, 203L, "15",  2010L, 50, 1.10, 0.70, 0.05, 0.40, 26,
    0.75, 40.25, 203L, "27",  2010L, 25, 1.30, 0.90, 0.00, 0.60, 22
  ) |>
    dplyr::mutate(
      total_c_input_mgc_ha_yr = .data$residue_c_mgc_ha_yr +
        .data$root_c_mgc_ha_yr +
        .data$weed_c_mgc_ha_yr +
        .data$manure_c_mgc_ha_yr,
      humified_fraction = (.data$residue_c_mgc_ha_yr *
        h$crop_residue +
        .data$root_c_mgc_ha_yr * h$root +
        .data$weed_c_mgc_ha_yr * h$weed +
        .data$manure_c_mgc_ha_yr * h$manure) /
        .data$total_c_input_mgc_ha_yr,
      method_c_input = "humified_weighted",
      method_unspatialized = "fodder_pattern",
      method_crop_weights = "spatialized"
    )
}

# Irrigated share of each crop's cell area: partial (wheat, rice), none
# (maize), whole (olives); barley has no row at all.
.rs_shares <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year, ~irrigated_share,
    0.25, 40.25, 203L, 15L,  2010L, 0.25,
    0.25, 40.25, 203L, 56L,  2010L, 0,
    0.25, 40.25, 203L, 260L, 2010L, 1,
    0.75, 40.25, 203L, 15L,  2010L, 0.6,
    0.75, 40.25, 203L, 27L,  2010L, 0.9
  )
}

.rs_grass <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use,
    ~c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    0.25, 40.25, 203L, 2010L, "grassland", 1.0, 0.12, "g",
    0.75, 40.25, 203L, 2010L, "natural",   1.5, 0.30, "n"
  )
}

# National production in the get_primary_production() layout. Country 203
# reaches the fixture's 2010 yields (wheat 4, maize 6, olives 2, barley 3.5,
# rice 5 t/ha); countries 100 and 101 sit at 3x and 0.2x of it, so the
# pooled 99th and 1st percentiles bound the split well away from the
# fixture's yields unless a test pushes the ratio on purpose.
.rs_production <- function() {
  tidyr::expand_grid(
    tibble::tribble(
      ~item_prod_code, ~yield_2010,
      15L,  4,
      56L,  6,
      260L, 2,
      44L,  3.5,
      27L,  5
    ),
    tibble::tribble(
      ~area_code, ~factor,
      203L, 1,
      100L, 3,
      101L, 0.2
    ),
    year = 2001:2010
  ) |>
    dplyr::mutate(
      tonnes = 1000 *
        .data$yield_2010 *
        .data$factor *
        (1 - 0.02 * (2010L - .data$year)),
      ha = 1000
    ) |>
    tidyr::pivot_longer(
      c("tonnes", "ha"),
      names_to = "unit",
      values_to = "value"
    ) |>
    dplyr::select("year", "area_code", "item_prod_code", "unit", "value")
}

# A build_regime_yield_ratio()-shaped table for the share rows; `value`
# replaces every ratio (1 gives the equal-yield golden case).
.rs_ratio <- function(value = NULL) {
  out <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year, ~ratio_unbounded,
    0.25, 40.25, 203L, 15L,  2010L, 1.8,
    0.25, 40.25, 203L, 56L,  2010L, 2.0,
    0.25, 40.25, 203L, 260L, 2010L, 1.5,
    0.75, 40.25, 203L, 15L,  2010L, 1.6,
    0.75, 40.25, 203L, 27L,  2010L, 1.3
  )
  if (!is.null(value)) {
    out$ratio_unbounded <- value
  }
  out
}

# The inputs build_carbon_inputs() reads, every one injected.
.rs_carbon_data <- function(ratio = .rs_ratio()) {
  list(
    cropland = .rs_cropland(),
    grass_natural = .rs_grass(),
    crop_regime_share = .rs_shares(),
    regime_ratio = ratio,
    regime_production = .rs_production()
  )
}

# The residue model's irrigation terms switched off for the calling test: no
# crop group's residue responds to irrigation and an irrigated root keeps the
# rainfed root:shoot ratio, so a re-evaluated part differs from its crop only
# through its yield. Mocks the coefficient-table reader, so the residue and
# root functions themselves run unchanged.
.rs_neutral_irrigation <- function(env = parent.frame()) {
  read_coef <- whep:::.read_coef
  testthat::local_mocked_bindings(
    .read_coef = function(name) {
      out <- read_coef(name)
      if (name == "irr_residue_crop_adj") {
        out$irr_residue_sensitivity <- 0
      }
      if (name == "irrigation_adj") {
        out$rs_ratio_factor <- 1
      }
      out
    },
    .package = "whep",
    .env = env
  )
}

# The per-crop layer with the nitrogen of its components, as
# build_soil_carbon_inputs() returns it: residue and root N per hectare and
# the N of every component whose N is known (here residue, root and manure;
# the weed N is unknown), so the input C:N is the ratio of the carbon and
# nitrogen of those three.
.rs_cropland_with_n <- function() {
  .rs_cropland() |>
    dplyr::mutate(
      residue_n_mgn_ha_yr = .data$residue_c_mgc_ha_yr / 60,
      root_n_mgn_ha_yr = .data$root_c_mgc_ha_yr / 30,
      manure_n = .data$manure_c_mgc_ha_yr / 12,
      input_n_mgn_ha_yr = .data$residue_n_mgn_ha_yr +
        .data$root_n_mgn_ha_yr +
        .data$manure_n,
      input_cn = (.data$residue_c_mgc_ha_yr +
        .data$root_c_mgc_ha_yr +
        .data$manure_c_mgc_ha_yr) /
        .data$input_n_mgn_ha_yr
    ) |>
    dplyr::select(-"manure_n")
}

# Residue and root dry matter per hectare of maize (item 56) in Spain
# (area code 203) in 2010, written out from the coefficient tables rather
# than through the package's residue and root functions: the IPCC
# slope-intercept residue term, the CBS's cereal line (whep#1519), times the
# irrigation factor; the root as the root:shoot estimate
# (times the irrigated root:shoot factor) averaged with the reference
# below-ground biomass, capped at three times the root:shoot ratio. Spain's
# HANPP region has a modern-variety share of 1 for maize in 2010, so the
# harvest-index correction is 1. `yield` is fresh t/ha.
.rs_maize_by_hand <- function(yield, residue_factor = 1, rs_factor = 1) {
  bio <- whep::whep_coef_table("bio_coefs")
  bio <- bio[bio$item_prod_code == "56", ]
  res <- whep::whep_coef_table("ipcc_residue_coefs")
  res <- res[res$ipcc_crop == "Maize", ]
  root <- whep::whep_coef_table("ipcc_root_coefs")
  root <- root[root$ipcc_crop == "Maize", ]
  yield_dm <- yield * bio$product_dm_kgfm
  ipcc <- res$slope_ag * yield_dm + res$intercept_ag_dm_t_ha
  # Maize is a cereal: the soil chain prices it with the CBS's IPCC line
  # alone (whep#1519), not the ensemble with the residue:product ratio.
  residue <- residue_factor * ipcc
  aerial <- yield_dm + residue
  root_dm <- 0.5 *
    aerial *
    root$rs_default *
    rs_factor +
    0.5 * root$bg_ref_dm_t_ha
  list(
    residue = residue,
    root = min(root_dm, 3 * root$rs_default * aerial)
  )
}
