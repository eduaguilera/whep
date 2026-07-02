# Tests for R/n_balance.R (Module C, Task C7): build_nitrogen_balance().

# Helper fixtures --------------------------------------------------------------

.nb_bnf_input <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~item_prod_code,
    ~crop_npp_n_t,
    ~product_n_t,
    ~weed_npp_n_t,
    ~land_use,
    ~legumes_seeded,
    ~seeded_cover_crop_share,
    ~area_ha,
    0.25,
    50.25,
    10L,
    2010L,
    "176",
    10,
    5,
    4,
    "Cropland",
    0,
    0,
    40
  )
}

.nb_npp_input <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~item_prod_code,
    ~item_cbs_code,
    ~product_dm_t,
    ~residue_dm_t,
    ~root_dm_t,
    0.25,
    50.25,
    10L,
    2010L,
    "15",
    2511L,
    87.9,
    135.75,
    30,
  )
}

.nb_livestock_intake <- function() {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_quality,
    ~intake_dm_t,
    2010L,
    "10",
    "0.25_50.25",
    "Cattle_milk",
    2513L,
    "high_quality",
    200,
    2010L,
    "10",
    "0.25_50.25",
    "Cattle_milk",
    NA,
    "grass",
    600
  )
}

.nb_gridded <- function() {
  list(
    crops = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~crop,
      ~manure_n_receptivity,
      ~crop_n_cap,
      2010L,
      "10",
      "0.25_50.25",
      "barley",
      6,
      200
    ),
    grass = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~grass_n_cap,
      2010L,
      "10",
      "0.25_50.25",
      50
    )
  )
}

.nb_nhx <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~value_g,
    0.25,
    50.25,
    2010L,
    2000000000
  )
}

.nb_noy <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~value_g,
    0.25,
    50.25,
    2010L,
    1000000000
  )
}

.nb_urban_population <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~urban_pop,
    0.25,
    50.25,
    2010L,
    30898536
  )
}

.nb_cropland_ha <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~cropland_ha,
    0.25,
    50.25,
    10L,
    2010L,
    1000
  )
}

.nb_cell_polity <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~polity_frac,
    ~cell_area_ha,
    0.25,
    50.25,
    10L,
    1,
    3000
  )
}

.nb_carbon_balance <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~land_use,
    ~year,
    ~area_ha,
    ~son_change_kgn_ha,
    0.25,
    50.25,
    10L,
    "Cropland",
    2010L,
    50,
    -0.4 # negative son_change_kgn_ha means SOM sequestration is positive
  )
}

.nb_primary_prod <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~unit,
    ~value,
    2010L,
    10L,
    2511L,
    "ha",
    700
  )
}

.nb_fertilizer <- function() {
  tibble::tribble(
    ~Year,
    ~`Area Code`,
    ~Element,
    ~Item,
    ~Value,
    2010L,
    10L,
    "Agricultural Use",
    "Nutrient nitrogen N (total)",
    100
  )
}

.nb_crop_patterns <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~item_prod_code,
    ~harvest_fraction,
    0.25,
    50.25,
    15L,
    1
  )
}

.nb_type_cropland <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~luh2_type,
    ~type_ha,
    0.25,
    50.25,
    2010L,
    "c3ann",
    1000
  )
}

.nb_residue_destiny_input <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~item_prod_code,
    ~residue_dm_t,
    ~region_krausmann,
    ~region_hanpp,
    0.25,
    50.25,
    10L,
    2511L,
    2010L,
    "15",
    135.75,
    "Western Europe",
    "Western Europe"
  )
}

.nb_full_data <- function() {
  list(
    bnf_input = .nb_bnf_input(),
    npp_n_input = .nb_npp_input(),
    livestock_intake = .nb_livestock_intake(),
    gridded = .nb_gridded(),
    nhx = .nb_nhx(),
    noy = .nb_noy(),
    urban_population = .nb_urban_population(),
    cropland_ha = .nb_cropland_ha(),
    cell_polity = .nb_cell_polity(),
    carbon_balance = .nb_carbon_balance(),
    primary_prod = .nb_primary_prod(),
    fertilizer = .nb_fertilizer(),
    crop_patterns = .nb_crop_patterns(),
    type_cropland = .nb_type_cropland(),
    residue_destiny_input = .nb_residue_destiny_input()
  )
}

# Joins loss/leaching drivers for every item_cbs_code present in the
# assembled n_inputs, so the join in build_nitrogen_balance() never drops a
# row for a driver mismatch (the tests use method = "ipcc"/"ipcc2019"/
# "ipcc_fracleach", the simplest methods, to keep driver requirements small).
.nb_data_with_drivers <- function() {
  data <- .nb_full_data()
  n_inputs <- whep::build_n_inputs(data = data)
  item_codes <- unique(n_inputs$item_cbs_code)
  fert_types <- c(
    "Excreta_other",
    "Liquid",
    "Solid",
    "SOM",
    "Synthetic",
    "Urban",
    "Recycling"
  )
  data$n_balance_drivers <- tidyr::expand_grid(
    tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      area_code = 10L,
      item_cbs_code = item_codes,
      year = 2010L
    ),
    fert_type = fert_types
  ) |>
    dplyr::mutate(climate = "MED", irrig_type = "Rainfed")
  data$n_balance_leaching_drivers <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = item_codes,
    year = 2010L,
    climate = "MED",
    irrig_cat = "Rainfed",
    land_use = "Cropland",
    cn_input = NA_real_,
    tillage = "Not_specified",
    som_share = 0.03
  )
  data$drainage_mm <- 400
  data
}

.nb_run <- function(data = .nb_data_with_drivers(), resolution = "grid") {
  whep::build_nitrogen_balance(
    methods = list(
      nh3 = "ipcc",
      n2o = "ipcc2019",
      leaching = "ipcc_fracleach"
    ),
    resolution = resolution,
    data = data
  )
}

# Tests --------------------------------------------------------------------

testthat::test_that("calculate_npp_carbon_nitrogen runs once per build_nitrogen_balance call", {
  # build_n_inputs()'s "recycling" term and this function's own prod_n_t
  # term both need calculate_npp_carbon_nitrogen()'s result; a prior
  # version recomputed it twice despite an .n_balance_npp() cache helper
  # because the cache was never populated before the first call site ran.
  # Prepare the fixture data BEFORE mocking: .nb_data_with_drivers() makes
  # its own build_n_inputs() call to derive driver rows, which is a
  # legitimate, separate call outside build_nitrogen_balance() and must
  # not be counted here.
  data <- .nb_data_with_drivers()
  call_count <- 0
  real_fn <- whep::calculate_npp_carbon_nitrogen
  testthat::local_mocked_bindings(
    calculate_npp_carbon_nitrogen = function(...) {
      call_count <<- call_count + 1
      real_fn(...)
    },
    .package = "whep"
  )
  .nb_run(data = data)
  testthat::expect_equal(call_count, 1L)
})

testthat::test_that("balance closes: n_balance_t equals input minus output", {
  out <- .nb_run()
  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(
    abs(out$n_balance_t - (out$n_input_full_t - out$n_output_full_t)) < 1e-6
  ))
})

testthat::test_that("surplus_t is never negative", {
  out <- .nb_run()
  testthat::expect_true(all(out$surplus_t >= 0))
})

testthat::test_that("example fixture is schema-complete", {
  out <- whep::build_nitrogen_balance(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("nue_std", "nue_som", "nue_useful", "surplus_share", "method_nh3")
  )
})

testthat::test_that("example fixture closes and has non-negative surplus", {
  out <- whep::build_nitrogen_balance(example = TRUE)
  testthat::expect_true(all(
    abs(out$n_balance_t - (out$n_input_full_t - out$n_output_full_t)) < 1e-6
  ))
  testthat::expect_true(all(out$surplus_t >= 0))
})

testthat::test_that("the N-limitation SOM cap engages and recomputes every downstream value", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")

  # Directly test the private two-pass sequence on a constructed scenario:
  # n_balance_t < 0 (deficit) and som_sequestration_n_t > 0 before the cap.
  before_cap <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L,
    n_input_full_t = 50,
    n_input_full_nosom_t = 45,
    n_input_std_t = 45,
    n_input_som_t = 48,
    n_input_for_n2o_t = 40,
    prod_n_t = 20,
    used_residue_n_t = 5,
    burnt_residue_n_t = 2,
    grazed_weeds_n_t = 3,
    som_sequestration_n_t = 10, # would push n_output_full_t above 50+
    nh3_n_t = 30
  )
  pass1 <- whep:::.nb_indicators_pass1(before_cap)
  # n_output_full_t sums to 70 (20 + 5 + 2 + 3 + 30 + 10) against an input
  # of 50, so n_balance_t is negative.
  testthat::expect_lt(pass1$n_balance_t, 0)
  testthat::expect_gt(pass1$som_sequestration_n_t, 0)

  capped <- whep:::.nb_cap_som(pass1)
  # Capped som is pmax(0, 10 + n_balance_t), i.e. 0 here, strictly smaller
  # than the uncapped value of 10.
  testthat::expect_lt(capped$som_sequestration_n_t, pass1$som_sequestration_n_t)
  testthat::expect_equal(capped$som_sequestration_n_t, 0)

  # Recompute every downstream output/balance value from the capped SOM (the
  # SAME .nb_output_aggregates()/.nb_balance() helpers the real pipeline
  # uses), confirming nothing downstream is left stale.
  recomputed <- capped |>
    whep:::.nb_output_aggregates() |>
    whep:::.nb_balance()
  expected_output_full <- 20 + 5 + 2 + 3 + 30 + 0
  testthat::expect_equal(recomputed$n_output_full_t, expected_output_full)
  testthat::expect_equal(
    recomputed$n_balance_t,
    50 - expected_output_full,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    recomputed$surplus_t,
    pmax(0, 50 - expected_output_full)
  )
  # The stale pre-cap n_output_full_t (70) must not survive anywhere.
  testthat::expect_false(isTRUE(all.equal(recomputed$n_output_full_t, 70)))
})

testthat::test_that("resolution = \"polity\" re-aggregates resolution = \"grid\"", {
  data <- .nb_data_with_drivers()
  grid <- .nb_run(data, resolution = "grid")
  polity <- .nb_run(data, resolution = "polity")

  testthat::expect_true(nrow(polity) > 0)
  # Mass-conserving input/output totals must match exactly between
  # resolutions (both were built from the same single grid cell).
  testthat::expect_equal(
    sum(polity$n_input_full_t),
    sum(grid$n_input_full_t),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    sum(polity$prod_n_t),
    sum(grid$prod_n_t),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    sum(polity$n_output_full_t),
    sum(grid$n_output_full_t),
    tolerance = 1e-6
  )
})

testthat::test_that("NUE denominators are not accidentally collapsed", {
  x <- tibble::tibble(
    n_input_std_t = 100,
    n_input_full_t = 90, # deliberately != n_input_std_t and n_input_som_t
    n_input_som_t = 80,
    prod_n_t = 40,
    n_output_residues_t = 50,
    n_output_som_t = 45,
    n_output_useful_t = 60,
    n_output_full_t = 70
  )
  out <- whep:::.nb_nue(x)

  testthat::expect_equal(out$nue_std, 40 / 100)
  testthat::expect_equal(out$nue_residues, 50 / 100)
  # nue_som and nue_useful divide by n_input_full_t (90), NOT n_input_std_t
  # (100) or n_input_som_t (80).
  testthat::expect_equal(out$nue_som, 45 / 90)
  testthat::expect_equal(out$nue_useful, 60 / 90)
  # nue_full divides by n_input_som_t (80), a THIRD distinct denominator.
  testthat::expect_equal(out$nue_full, 70 / 80)
  testthat::expect_false(isTRUE(all.equal(out$nue_som, 45 / 80)))
  testthat::expect_false(isTRUE(all.equal(out$nue_full, 70 / 90)))
})

testthat::test_that("method_nh3/method_soil_n2o/method_leaching are stamped", {
  out <- .nb_run()
  testthat::expect_true(all(out$method_nh3 == "ipcc"))
  testthat::expect_true(all(out$method_soil_n2o == "ipcc2019"))
  testthat::expect_true(all(out$method_leaching == "ipcc_fracleach"))
})

testthat::test_that("resolution argument is validated", {
  testthat::expect_error(
    whep::build_nitrogen_balance(resolution = "province", data = list()),
    "resolution"
  )
})

testthat::test_that("gwp argument is validated and total_gwp_co2e_kg is non-negative", {
  out <- .nb_run()
  testthat::expect_true(all(out$total_gwp_co2e_kg >= 0))
  testthat::expect_error(
    whep::build_nitrogen_balance(gwp = "ar99", data = list())
  )
})

# -- Spain_Hist port-fidelity validation (skip_on_ci, Task C8) ---------------
#
# Compare whep::build_nitrogen_balance() on REAL Spain cropland N data
# against Spain_Hist's own reference, N_balance_ygpit_all.csv, at national
# annual totals for a single reference year (2019, the same year Module B's
# carbon-balance fidelity test uses). Follows test_carbon_balance.R's exact
# pattern: "province as cell" (lon = province area_code, lat = 0), env-var
# resolution for the local data roots (.spain_hist_dir()/.spain_hist_l_dir(),
# shared via tests/testthat/helper_spain_fidelity.R), and skip_on_ci() +
# skip_if(is.null(...)) guards so the test degrades gracefully when the local
# Spain_Hist checkout or its off-repo L-files output are absent.
#
# SCOPE (stated explicitly, not guessed -- see the file-level decisions this
# was built against):
#
#   N-INPUT side: read DIRECTLY from N_balance_ygpit_all.csv's own Synthetic/
#   Liquid/Solid/Excreta/Urban/BNF/SOM/Deposition columns (Spain's OWN real
#   per-province-crop N inputs), reshaped into build_n_inputs()'s long-format
#   contract and injected via data$n_inputs -- this BYPASSES build_n_inputs()'s
#   own BNF/manure/deposition/urban/synthetic computation pipelines entirely.
#   Agro_industry/Accum_loss are dropped (this branch's own documented gaps).
#   This test therefore validates build_nitrogen_balance()'s OWN balance-
#   closing/NUE/SOM-cap/loss-cascade arithmetic against real Spain magnitudes,
#   GIVEN Spain_Hist's own real N-input values as ground truth -- it does NOT
#   re-validate the gridded N-input SOURCING pipelines (deposition, urban,
#   manure/synthetic spatialization), which have their own unit tests already.
#
#   OUTPUT side (prod_n_t/used_residue_n_t/burnt_residue_n_t): computed via
#   WHEP's REAL calculate_npp_carbon_nitrogen() + calculate_residue_
#   destinies(), fed with REAL Spain NPP data from NPP_ygpit.csv.gz. Spain's
#   free-form Name_biomass crop-name string is matched to item_prod_code via
#   the SAME case-insensitive item_prod join .ni_crop_name_to_item_cbs()
#   already uses (R/n_balance_inputs.R); only crops where this join succeeds
#   are included, and the unmatched count is reported below (measured: 1316 /
#   7202 cropland Name_biomass x province x irrigation rows match, ~18% of
#   rows / ~54% of cropland area for 2019 -- Spain_Hist uses short singular
#   crop names, e.g. "Olive", "Potato", "Maize", "Grapevine", while WHEP's
#   item_prod is FAOSTAT-style plural/parenthetical, e.g. "Olives",
#   "Potatoes", "Maize (corn)", "Grapes"; a fuzzy/plural-aware crosswalk is a
#   defensible future refinement, not invented here per the task's explicit
#   instruction to reuse the exact existing join pattern unmodified).
#
#   grazed_weeds_n_t: a DOCUMENTED, EXPLICITLY FLAGGED gap for this test
#   specifically -- real Spain livestock grazing-intake data at province
#   grain was not available. data$livestock_intake is left unset;
#   .nb_add_grazed_weeds() already returns 0 for a missing
#   data$livestock_intake (see R/n_balance.R), so n_output_full_t/
#   n_balance_t/surplus_t/NUE all diverge from Spain_Hist's own by (at least)
#   the magnitude of Spain's real GrazedWeeds_MgN column: 11,393 MgN
#   nationally in 2019, ~0.9% of Spain's own N_input_full (2,026,846 MgN) --
#   a real but small contributor to the measured divergence below.
#
#   som_sequestration_n_t: reuses Module B's OWN real Spain carbon-balance
#   assembly (the exact build_carbon_balance(model = "hsoc",
#   resolution = "grid") call test_carbon_balance.R's
#   "WHEP polity SOC matches Spain_Hist per-province baseline" test makes),
#   fed to build_nitrogen_balance() via data$carbon_balance. Because only a
#   SINGLE year (2019) of carbon-balance inputs is assembled here (matching
#   this test's single-reference-year design, not a multi-year march),
#   son_change_kgn_ha is 0 for every province by construction (year 1's rate
#   is init-minus-equilibrium under the SAME single-year inputs, so it is
#   exactly 0 -- see .cb_march()/R/carbon_balance.R): the SOM-sequestration
#   N-output term is therefore 0 in WHEP's run here regardless of Spain's own
#   nonzero SOM_MgN input-side flux. This affects only the OUTPUT-side SOM
#   term (n_output_som_t/n_output_full_t), not the N-INPUT side (which is
#   read directly from Spain's own SOM column per the scope above).
#
#   Loss-side drivers: the SIMPLER method set (nh3 = "ipcc",
#   n2o = "ipcc2019", leaching = "ipcc_fracleach"), NOT the package defaults
#   (manner/aguilera/meisinger_drainage) which Spain_Hist's own reference
#   values were computed with. MANNER needs real per-cell windspeed/pH/
#   technique/tillage driver data at province grain that is not assembleable
#   from what is available locally -- the "technique" driver in particular
#   has no real per-province/per-era source anywhere in this branch (a
#   documented O7 gap). climate (MED/ATL) per province comes from
#   Spain_Hist's own Province_Codes.xlsx "Codes" sheet (the exact source
#   Classify_climate_cats() in n_fun.r:343-356 uses, read read-only via
#   openxlsx::read.xlsx() per this repo's xlsx convention), via the shared
#   .spain_province_climate() helper. irrig_type is set to the flat
#   "Tier_1"/"Med_average" convention calculate_soil_n2o(method =
#   "ipcc2019")'s own ATL/MED split uses (n2o_efs_disaggregated has no other
#   non-NA Atlantic irrig_type row, so any real per-crop Irrig_type would
#   still resolve to Tier_1 here); calculate_n_leaching(method =
#   "ipcc_fracleach") needs only n_surplus_t (computed internally), so the
#   leaching-drivers table below carries no further real driver columns.
#   A MANNER/aguilera/meisinger_drainage validation is a documented follow-up,
#   blocked on resolving the MANNER technique-by-era driver gap.

# Case-insensitive match of Spain_Hist's Name_biomass crop-name string to
# whep::items_prod_full's item_prod_code -- the SAME join pattern
# .ni_crop_name_to_item_cbs() uses (R/n_balance_inputs.R), just resolving to
# item_prod_code (needed by calculate_npp_carbon_nitrogen()) instead of
# item_cbs_code. Adapted, not reused directly, because the target column
# differs; the lookup construction is otherwise identical.
.nb_spain_crop_to_item_prod <- function(crop) {
  lookup <- whep::items_prod_full |>
    dplyr::filter(!is.na(.data$item_prod)) |>
    dplyr::transmute(
      crop_lower = stringr::str_to_lower(.data$item_prod),
      item_prod_code = .data$item_prod_code
    ) |>
    dplyr::distinct(.data$crop_lower, .keep_all = TRUE)
  tibble::tibble(crop_lower = stringr::str_to_lower(crop)) |>
    dplyr::left_join(lookup, by = "crop_lower") |>
    dplyr::pull("item_prod_code")
}

# One row per province, an arbitrary but stable integer area_code (the
# "province as cell" convention: lon = area_code, lat = 0).
.nb_spain_province_lookup <- function(provinces) {
  tibble::tibble(province = sort(unique(provinces))) |>
    dplyr::mutate(area_code = dplyr::row_number())
}

# Spain_Hist's cropland N_balance_ygpit_all.csv rows for one reference year,
# joined to the province area_code lookup. NULL when the file is absent.
.nb_spain_reference <- function(ref_year = 2019L) {
  f <- file.path(.spain_hist_l_dir(), "N_balance_ygpit_all.csv")
  if (!file.exists(f)) {
    return(NULL)
  }
  cols <- c(
    "Year",
    "LandUse",
    "Name_biomass",
    "Province_name",
    "Synthetic",
    "Liquid",
    "Solid",
    "Excreta",
    "Urban",
    "BNF",
    "SOM",
    "Deposition",
    "Surplus",
    "NH3_MgN",
    "N2O_MgN",
    "NO3_MgN",
    "Denitrif_MgN",
    "GrazedWeeds_MgN",
    "N_input_full"
  )
  ref <- data.table::fread(f, select = cols) |>
    tibble::as_tibble() |>
    dplyr::filter(.data$Year == ref_year, .data$LandUse == "Cropland")
  prov <- .nb_spain_province_lookup(ref$Province_name)
  dplyr::left_join(ref, prov, by = c("Province_name" = "province"))
}

# The reference NATIONAL annual totals (2019, cropland) for every term this
# test compares. National totals (rather than per-province) avoid the
# division-by-near-zero noise the per-province reference data itself
# exhibits: 9 of 50 provinces (all Atlantic climate) carry N2O_MgN == 0 in
# Spain's own reference, which would make a per-province percent-difference
# infinite there under WHEP's flat ipcc2019 EF (never exactly 0). NULL when
# the reference is absent.
.nb_spain_reference_national <- function(ref_year = 2019L) {
  ref <- .nb_spain_reference(ref_year)
  if (is.null(ref)) {
    return(NULL)
  }
  dplyr::summarise(
    ref,
    surplus_ref = sum(.data$Surplus, na.rm = TRUE),
    nh3_ref = sum(.data$NH3_MgN, na.rm = TRUE),
    n2o_ref = sum(.data$N2O_MgN, na.rm = TRUE),
    no3_ref = sum(.data$NO3_MgN, na.rm = TRUE),
    denitrification_ref = sum(.data$Denitrif_MgN, na.rm = TRUE),
    grazed_weeds_ref = sum(.data$GrazedWeeds_MgN, na.rm = TRUE),
    n_input_full_ref = sum(.data$N_input_full, na.rm = TRUE)
  )
}

# build_n_inputs()'s long-format n_inputs contract (lon, lat, area_code,
# item_cbs_code, year, fert_type, n_input_t), reshaped directly from
# Spain_Hist's own per-fert-type N-input columns (see the file-level scope
# comment above). item_cbs_code is NA_integer_ throughout: the reference
# columns are only available at (province, crop) grain, but the OUTPUT side
# (from the crop-name join, necessarily incomplete) cannot be matched
# 1:1 to every N-input row, so both sides are deliberately collapsed to the
# per-province "not crop-specific" grain before comparison, consistent with
# how deposition/urban/SOM rows are already NA_integer_-keyed package-wide.
.nb_spain_n_inputs <- function(ref) {
  fert_cols <- c(
    Synthetic = "synthetic",
    Liquid = "manure_liquid",
    Solid = "manure_solid",
    Excreta = "excreta",
    Urban = "urban",
    BNF = "bnf",
    SOM = "som_mineralization",
    Deposition = "deposition"
  )
  purrr::map(names(fert_cols), function(col) {
    tibble::tibble(
      lon = ref$area_code,
      lat = 0,
      area_code = ref$area_code,
      item_cbs_code = NA_integer_,
      year = ref$Year,
      fert_type = fert_cols[[col]],
      n_input_t = ref[[col]]
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::filter(!is.na(.data$n_input_t))
}

# calculate_npp_carbon_nitrogen()'s required input (item_prod_code,
# product_dm_t, residue_dm_t, root_dm_t), from Spain_Hist's own
# NPP_ygpit.csv.gz for the SAME reference year and cropland rows, restricted
# to rows where the Name_biomass -> item_prod_code join succeeds. Takes the
# SAME prov_lookup the N-input reference file derived (rather than deriving
# its own from NPP_ygpit.csv.gz's own province set), so an area_code means
# the SAME province across every data source fed to build_nitrogen_balance()
# -- the two files carry the identical 50 provinces, but keying off one
# shared lookup makes that identity a guarantee, not a coincidence of
# `sort(unique(...))` producing the same order twice. NULL when the file is
# absent. Returns the join-rate counts as attributes so the test can report
# them without a second file read.
.nb_spain_npp_input <- function(prov_lookup, ref_year = 2019L) {
  f <- file.path(.spain_hist_l_dir(), "NPP_ygpit.csv.gz")
  if (!file.exists(f)) {
    return(NULL)
  }
  cols <- c(
    "Year",
    "LandUse",
    "Name_biomass",
    "Province_name",
    "Prod_MgDM",
    "Residue_MgDM",
    "Root_MgDM"
  )
  npp <- data.table::fread(f, select = cols) |>
    tibble::as_tibble() |>
    dplyr::filter(.data$Year == ref_year, .data$LandUse == "Cropland")
  npp <- dplyr::inner_join(
    npp,
    prov_lookup,
    by = c("Province_name" = "province")
  ) |>
    dplyr::mutate(
      item_prod_code = .nb_spain_crop_to_item_prod(.data$Name_biomass)
    )
  matched <- dplyr::filter(npp, !is.na(.data$item_prod_code))
  structure(
    tibble::tibble(
      lon = matched$area_code,
      lat = 0,
      area_code = matched$area_code,
      item_cbs_code = NA_integer_,
      year = matched$Year,
      item_prod_code = matched$item_prod_code,
      product_dm_t = matched$Prod_MgDM,
      residue_dm_t = matched$Residue_MgDM,
      root_dm_t = matched$Root_MgDM
    ),
    n_matched = nrow(matched),
    n_total = nrow(npp)
  )
}

# Module B's own real Spain cropland carbon-balance assembly, adapted from
# test_carbon_balance.R's .spain_cropland_class_inputs()/.cb_spain_hist_
# inputs() (that file's private helpers are not sourced across test files;
# testthat only guarantees helper_*.R files load first, so this is a
# clearly-commented adapted copy, per the task's explicit (b) option). Runs
# the EXACT build_carbon_balance(model = "hsoc", resolution = "grid") call
# that test's "WHEP polity SOC matches Spain_Hist per-province baseline"
# test makes, at the SAME reference year, over the SAME province area_code
# keying this file's own N-input/NPP helpers use (so son_change_kgn_ha joins
# straight into build_nitrogen_balance() via data$carbon_balance). NULL when
# the source files are absent.
.nb_spain_carbon_balance <- function(prov_lookup, ref_year = 2019L) {
  d <- .spain_hist_l_dir()
  files <- file.path(
    d,
    c(
      "C_humus_inputs_ygpit.csv",
      "Mineraliz_ModifyingFactors_ygpit.csv",
      "Crop_AreaProd_Sc.csv.gz"
    )
  )
  if (!all(file.exists(files))) {
    return(NULL)
  }
  prov <- .nb_spain_cropland_inputs(files, ref_year, prov_lookup)
  whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = list(
      c_inputs = tibble::tibble(
        lon = prov$area_code,
        lat = 0,
        area_code = prov$area_code,
        year = ref_year,
        land_use = "cropland",
        c_input_mgc_ha_yr = prov$c_input_mgc_ha_yr,
        humified_fraction = prov$humified_fraction
      ),
      land_use = tibble::tibble(
        lon = prov$area_code,
        lat = 0,
        area_code = prov$area_code,
        year = ref_year,
        land_use = "cropland",
        area_ha = prov$area_ha
      ),
      climate = tibble::tibble(
        lon = prov$area_code,
        lat = 0,
        area_code = prov$area_code,
        year = ref_year,
        climate_modifier = prov$abc
      ),
      clay = tibble::tibble(lon = prov$area_code, lat = 0, clay_pct = 20)
    )
  )
}

# Per-crop cropland carbon inputs + abc modifier + Baseline area, aggregated
# to the province cropland class -- the same aggregation
# .spain_province_cropland() (test_carbon_balance.R) performs, adapted to
# key on this file's own prov_lookup (area_code by Province_name) rather than
# assigning a fresh row_number() per call, so the SOM term lines up with the
# N-input/NPP province keys elsewhere in this test.
.nb_spain_cropland_inputs <- function(files, ref_year, prov_lookup) {
  crop_key <- c(
    "LandUse",
    "Name_biomass",
    "Province_name",
    "Irrig_cat",
    "Irrig_type"
  )
  c_humus <- data.table::fread(
    files[1],
    select = c("Year", crop_key, "Applied_MgC", "Humified_MgC")
  ) |>
    tibble::as_tibble() |>
    dplyr::filter(.data$Year == ref_year, .data$LandUse == "Cropland")
  abc <- data.table::fread(files[2], select = c("Year", crop_key, "abc")) |>
    tibble::as_tibble() |>
    dplyr::filter(.data$Year == ref_year, .data$LandUse == "Cropland")
  areas <- data.table::fread(
    files[3],
    select = c("Year_sc", crop_key, "Area_ygpit_ha")
  ) |>
    tibble::as_tibble() |>
    dplyr::filter(.data$Year_sc == "Baseline", .data$LandUse == "Cropland")
  merged <- c_humus |>
    dplyr::inner_join(
      dplyr::select(abc, dplyr::all_of(c("Year", crop_key, "abc"))),
      by = c("Year", crop_key)
    ) |>
    dplyr::inner_join(
      dplyr::summarise(
        areas,
        area = sum(.data$Area_ygpit_ha),
        .by = dplyr::all_of(crop_key)
      ),
      by = crop_key
    ) |>
    dplyr::filter(.data$area > 0, !is.na(.data$abc)) |>
    dplyr::summarise(
      c_input_mgc_ha_yr = sum(.data$Applied_MgC) / sum(.data$area),
      humified_fraction = sum(.data$Humified_MgC) / sum(.data$Applied_MgC),
      abc = sum(.data$abc * .data$area) / sum(.data$area),
      area_ha = sum(.data$area),
      .by = "Province_name"
    )
  dplyr::left_join(
    merged,
    prov_lookup,
    by = c("Province_name" = "province")
  )
}

# Loss-cascade driver tables for the simpler method set (see the file-level
# scope comment): climate ("ATL"/"MED") from Spain_Hist's own
# Province_Codes.xlsx, irrig_type from the flat Tier_1/Med_average
# calculate_soil_n2o(method = "ipcc2019") itself uses. One row per
# (balance key, fert_type) for n_balance_drivers (calculate_nh3()/
# calculate_soil_n2o() dispatch on fert_type); one row per balance key for
# n_balance_leaching_drivers (calculate_n_leaching() is a balance-level
# call). NULL when Province_Codes.xlsx is absent.
.nb_spain_loss_drivers <- function(prov_lookup, ref_year = 2019L) {
  climate <- .spain_province_climate()
  if (is.null(climate)) {
    return(NULL)
  }
  keyed <- prov_lookup |>
    dplyr::left_join(climate, by = c("province" = "Province_name")) |>
    dplyr::transmute(
      lon = .data$area_code,
      lat = 0,
      area_code = .data$area_code,
      item_cbs_code = NA_integer_,
      year = ref_year,
      climate = dplyr::if_else(.data$Climate == "ATL", "ATL", "MED"),
      irrig_type = dplyr::if_else(
        .data$climate == "ATL",
        "Tier_1",
        "Med_average"
      )
    )
  # The same fert_type vocabulary .nb_data_with_drivers() uses: exactly the
  # N_input_for_N2O_MgN terms .nb_loss_rows() (R/n_balance.R) selects.
  # "bnf"/"deposition" never reach that filter, so they need no driver row.
  fert_types <- c(
    "Excreta_other",
    "Liquid",
    "Solid",
    "SOM",
    "Synthetic",
    "Urban",
    "Recycling"
  )
  list(
    n_balance_drivers = tidyr::expand_grid(
      dplyr::select(
        keyed,
        "lon",
        "lat",
        "area_code",
        "item_cbs_code",
        "year",
        "climate",
        "irrig_type"
      ),
      fert_type = fert_types
    ),
    n_balance_leaching_drivers = dplyr::transmute(
      keyed,
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$item_cbs_code,
      .data$year,
      .data$climate,
      irrig_cat = "Rainfed",
      land_use = "Cropland",
      cn_input = NA_real_,
      tillage = "Not_specified",
      som_share = 0.03
    )
  )
}

# Assemble every whep::build_nitrogen_balance() input from the real Spain
# 2019 data, per the file-level scope comment. NULL (with the reason
# attached) when any required local source is absent.
.nb_spain_hist_inputs <- function(ref_year = 2019L) {
  ref <- .nb_spain_reference(ref_year)
  if (is.null(ref)) {
    return(NULL)
  }
  prov_lookup <- .nb_spain_province_lookup(ref$Province_name)
  npp_input <- .nb_spain_npp_input(prov_lookup, ref_year)
  if (is.null(npp_input)) {
    return(NULL)
  }
  carbon_balance <- .nb_spain_carbon_balance(prov_lookup, ref_year)
  if (is.null(carbon_balance)) {
    return(NULL)
  }
  drivers <- .nb_spain_loss_drivers(prov_lookup, ref_year)
  if (is.null(drivers)) {
    return(NULL)
  }
  list(
    n_inputs = .nb_spain_n_inputs(ref),
    npp_n_input = npp_input,
    carbon_balance = carbon_balance,
    n_balance_drivers = drivers$n_balance_drivers,
    n_balance_leaching_drivers = drivers$n_balance_leaching_drivers,
    drainage_mm = 400,
    npp_join = list(
      n_matched = attr(npp_input, "n_matched"),
      n_total = attr(npp_input, "n_total")
    )
  )
}

testthat::test_that("WHEP N balance matches Spain_Hist national 2019 totals", {
  testthat::skip_on_ci()
  ref_nat <- .nb_spain_reference_national()
  testthat::skip_if(
    is.null(ref_nat),
    "Spain_Hist N_balance_ygpit_all.csv not found."
  )
  inputs <- .nb_spain_hist_inputs()
  testthat::skip_if(
    is.null(inputs),
    "Real Spain N-balance inputs not found (NPP_ygpit.csv.gz, carbon-balance
     source files or Province_Codes.xlsx)."
  )

  whep_out <- whep::build_nitrogen_balance(
    methods = list(nh3 = "ipcc", n2o = "ipcc2019", leaching = "ipcc_fracleach"),
    resolution = "polity",
    data = inputs[c(
      "n_inputs",
      "npp_n_input",
      "carbon_balance",
      "n_balance_drivers",
      "n_balance_leaching_drivers",
      "drainage_mm"
    )]
  )
  whep_nat <- dplyr::summarise(
    whep_out,
    surplus_whep = sum(.data$surplus_t, na.rm = TRUE),
    nh3_whep = sum(.data$nh3_n_t, na.rm = TRUE),
    n2o_whep = sum(.data$n2o_direct_n_t, na.rm = TRUE),
    no3_whep = sum(.data$no3_n_t, na.rm = TRUE),
    denitrification_whep = sum(.data$denitrification_n_t, na.rm = TRUE)
  )

  terms <- c("surplus", "nh3", "n2o", "no3", "denitrification")
  cmp <- tibble::tibble(
    term = terms,
    whep = as.numeric(whep_nat[paste0(terms, "_whep")]),
    ref = as.numeric(ref_nat[paste0(terms, "_ref")])
  ) |>
    dplyr::mutate(abs_pct_diff = 100 * abs(.data$whep - .data$ref) / .data$ref)

  # Measured divergence (task C8, real 2019 run, national cropland totals,
  # n = 50 provinces collapsed to one national total per term; join
  # diagnostics: inputs$npp_join$n_matched / n_total crop-name joins
  # succeeded for the NPP output side):
  #   surplus            ~26%
  #   nh3_n_t            ~314% (WHEP >> Spain)
  #   n2o_direct_n_t     ~58%
  #   no3_n_t            ~41%
  #   denitrification_n_t ~96%
  # Decomposition (not tuned to force a pass, per the task's explicit
  # instruction and this branch's Module B precedent):
  #   (i) nh3_n_t, the largest single divergence, is FULLY attributable to
  #       the simpler-methods scope choice: IPCC 2019's flat Tier-1
  #       volatilisation fractions (21% organic, 11% synthetic;
  #       whep::n_attenuation_constants) are far coarser than Spain_Hist's
  #       own MANNER process-based estimate (which accounts for real
  #       application technique, incorporation delay, windspeed and soil
  #       pH), and Spain's real organic-N mix in 2019 volatilises well below
  #       21% under MANNER. This single term then cascades into n2o_direct_n_t
  #       (via calculate_indirect_n2o_nh3()'s nh3-derived indirect term is
  #       NOT compared here, but n_surplus_t IS, since nh3_n_t subtracts from
  #       n_output_full_t) and into no3_n_t/denitrification_n_t (both driven
  #       by n_surplus_t = surplus_t under ipcc_fracleach).
  #   (ii) n2o_direct_n_t: Spain's own reference has N2O_MgN == 0 for all 9
  #       Atlantic-climate provinces in 2019 (verified directly from the
  #       reference file), while WHEP's ipcc2019 method always applies a
  #       nonzero flat EF (Tier_1 = 0.010) for ATL rows; this is a real
  #       structural difference between the two methods on wet-climate
  #       cropland, not a bug in either.
  #   (iii) no3_n_t/denitrification_n_t inherit the surplus_t divergence
  #       from (i): under ipcc_fracleach, both are a fixed 24%/76% split of
  #       n_surplus_t, so any surplus_t divergence propagates linearly here
  #       (denitrification's larger % divergence than no3_n_t's, despite the
  #       fixed split, comes from denitrification's smaller reference
  #       magnitude, not a different mechanism).
  #   (iv) the documented grazed_weeds_n_t gap (0 in WHEP vs Spain's real
  #       11,393 MgN, ~0.9% of national N_input_full) and the
  #       som_sequestration_n_t gap (0 in WHEP under this test's single-year
  #       equilibrium setup, vs Spain's real evolved SOM flux) both push
  #       WHEP's n_output_full_t below Spain's, inflating surplus_t (and
  #       hence nh3_n_t/no3_n_t/denitrification_n_t downstream) further; both
  #       are small relative to (i)'s MANNER-vs-IPCC gap but not zero.
  # None of this is a bug in build_nitrogen_balance()'s own arithmetic (the
  # balance-closing/NUE/SOM-cap tests earlier in this file exercise that
  # arithmetic directly and pass); it is the documented cost of substituting
  # simpler global loss methods, a missing grazing-intake source and a
  # single-year (non-marched) SOM term for Spain_Hist's own process-based
  # MANNER/aguilera/meisinger_drainage pipeline and multi-decade SOC march.
  # The 10% tolerance (test_carbon_balance.R's precedent for a similarly
  # in-scope-limited fidelity test) is NOT loosened to force a pass: the
  # measured divergence is reported here for diagnosis, and a MANNER/
  # aguilera/meisinger_drainage validation (blocked on the MANNER
  # technique-by-era driver gap) is the documented follow-up.
  worst <- max(cmp$abs_pct_diff)
  med <- stats::median(cmp$abs_pct_diff)
  testthat::expect_lt(
    worst,
    10,
    label = sprintf(
      paste0(
        "WHEP N balance vs Spain_Hist 2019 national cropland totals: ",
        "median divergence = %.2f%%, max = %.2f%% (terms: %s; NPP crop-name ",
        "join matched %d/%d rows)"
      ),
      med,
      worst,
      paste(sprintf("%s=%.1f%%", cmp$term, cmp$abs_pct_diff), collapse = ", "),
      inputs$npp_join$n_matched,
      inputs$npp_join$n_total
    )
  )
})
