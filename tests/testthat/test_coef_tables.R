test_that("bio_coefs loads, is item-keyed and unique", {
  x <- whep::whep_coef_table("bio_coefs")
  testthat::expect_true(
    all(c("item_prod_code", "item_prod_name", "name_biomass") %in% names(x))
  )
  testthat::expect_equal(nrow(x), dplyr::n_distinct(x$item_prod_code))
})

test_that("residue_feed_fraction has a global fallback row", {
  x <- whep::whep_coef_table("residue_feed_fraction")
  testthat::expect_true(any(x$region_un_sub == "Global"))
})

test_that("natural-grain ipcc tables share the snake_case ipcc_crop key", {
  res <- whep::whep_coef_table("ipcc_residue_coefs")
  root <- whep::whep_coef_table("ipcc_root_coefs")
  testthat::expect_true("ipcc_crop" %in% names(res))
  testthat::expect_true("ipcc_crop" %in% names(root))
})

test_that("legacy below-ground fields stay retired from biomass_coefs", {
  legacy <- c(
    "BG_Biomass_kgDM_ha",
    "Root_Shoot_ratio",
    "Root_kgC_kgDM",
    "Rhizodeposits_mass_kgC_kgDM",
    "Rhizodeposits_N_kgN_kgRootN"
  )
  source_path <- system.file(
    "extdata",
    "harmonization",
    "biomass_coefs.csv",
    package = "whep"
  )
  source_coefs <- readr::read_csv(
    source_path,
    show_col_types = FALSE
  ) |>
    dplyr::select(!dplyr::starts_with("..."))

  # 422 rows since the Aquatic Plants row was added (whep#970).
  testthat::expect_equal(dim(source_coefs), c(422L, 63L))
  testthat::expect_identical(names(source_coefs), names(whep::biomass_coefs))
  testthat::expect_false(any(legacy %in% names(source_coefs)))
  testthat::expect_false(any(legacy %in% names(whep::biomass_coefs)))

  bio <- whep::whep_coef_table("bio_coefs")
  modern <- c(
    "bg_biomass_dm_kg_ha",
    "root_shoot_ratio",
    "root_c_kgdm",
    "rhizodeposit_mass_c_kgdm",
    "rhizodeposit_n_kgn_krootn"
  )
  testthat::expect_true(all(modern %in% names(bio)))

  mapping <- whep::whep_coef_table("ipcc_crop_mapping")
  ipcc <- whep::whep_coef_table("ipcc_root_coefs")
  input <- tibble::tibble(
    item_prod_code = c("15", "417"),
    product_dm_t = 10,
    residue_dm_t = 20,
    area_ha = 2
  )
  root_shoot <- whep::calculate_crop_roots(input, method = "root_shoot")
  reference <- whep::calculate_crop_roots(input, method = "reference")
  wheat_ipcc <- mapping$ipcc_crop[mapping$item_prod_code == "15"]
  wheat_root <- ipcc[ipcc$ipcc_crop == wheat_ipcc, ]
  fallback <- bio[bio$item_prod_code == "417", ]

  testthat::expect_equal(root_shoot$root_dm_t[1], 30 * wheat_root$rs_default)
  testthat::expect_equal(
    reference$root_dm_t[1],
    2 * wheat_root$bg_ref_dm_t_ha
  )
  testthat::expect_equal(
    root_shoot$root_dm_t[2],
    30 * fallback$root_shoot_ratio
  )
  testthat::expect_equal(
    reference$root_dm_t[2],
    2 * fallback$bg_biomass_dm_kg_ha / 1000
  )
})

test_that("no row has an out-of-range Edible_portion", {
  # Edible_portion is a fraction of fresh matter, so (0, 1] is the whole valid
  # range. One row used to break it: "ANIMAL PRODUCTS" at 4.0. That row is NOT
  # a data defect to clean upstream -- in afsetools' Biomass_coefs.xlsx it is
  # the VLOOKUP column-index vector the Coefs sheet depends on by absolute
  # address, so editing it there breaks the workbook (#752). It is dropped at
  # ingestion instead, so the packaged table now satisfies the bound outright.
  coefs <- whep::biomass_coefs
  out_of_range <- coefs[
    !is.na(coefs$Edible_portion) &
      (coefs$Edible_portion <= 0 | coefs$Edible_portion > 1),
  ]

  testthat::expect_identical(nrow(out_of_range), 0L)
  testthat::expect_false(
    "ANIMAL PRODUCTS" %in% whep::items_full$Name_biomass
  )
})

test_that("the proximate columns exceed dry matter on a known 69 rows", {
  # Protein + carbohydrate + lipid + fibre cannot exceed dry matter. 69 of 421
  # rows do (#752); Honey left the set in #1096. Lysine, Phytase and Enzimes
  # left it and Ammonium chloride joined it in #1074. Some are feed additives
  # where a composition block is not applicable (Urea, Ammonium chloride),
  # but ordinary foods are affected too, so the
  # columns cannot be used to reason about a row's basis. A tripwire, not a
  # target: it fails if a new row joins the set, and it fails when the set is
  # genuinely repaired -- at which point lower the number deliberately.
  coefs <- whep::biomass_coefs
  proximate <- rowSums(
    cbind(
      coefs$N_kgN_kgFM * 6.25 * 1000,
      coefs$Carbohydrates_g_kgFM,
      coefs$Lipids_g_kgFM,
      coefs$Fiber_g_kgFM
    ),
    na.rm = TRUE
  )
  dry_matter <- coefs$Product_kgDM_kgFM * 1000
  # Compare with a tolerance: three rows (Melon, Strawberry, Duck eggs) close
  # exactly against dry matter and exceed it only by ~1e-14, which is float
  # noise, not a data defect. Of the 69 that remain, 68 exceed by more than
  # 1 g/kg, so the set is not tolerance-sensitive in any other way.
  over <- !is.na(dry_matter) &
    proximate > 0 &
    proximate > dry_matter + 1e-6

  testthat::expect_equal(sum(over), 69L)
  testthat::expect_true("Barley" %in% coefs$Name_biomass[over])
})

test_that("unknown coefficient table errors", {
  testthat::expect_error(
    whep::whep_coef_table("does_not_exist"),
    "Unknown coefficient table"
  )
})

# Cereal root:shoot and N contents as printed in IPCC (2006) Vol. 4 Ch. 11
# Table 11.2 (pp. 11.17-11.18) and IPCC (2019) Table 11.1a (p. 11.18); the two
# agree on every value both list. NA marks a crop the tables give no value for
# (whep#1520). `Other_cereals` is the "Grains" / "Generic Grains" row.
.ipcc_cereal_reference <- function() {
  tibble::tribble(
    ~ipcc_crop,      ~rs,  ~n_ag, ~n_bg,
    "Wheat",         0.24, 0.006, 0.009,
    "Winter_wheat",  0.23, 0.006, 0.009,
    "Spring_wheat",  0.28, 0.006, 0.009,
    "Maize",         0.22, 0.006, 0.007,
    "Rice",          0.16, 0.007, NA,
    "Barley",        0.22, 0.007, 0.014,
    "Oats",          0.25, 0.007, 0.008,
    "Millet",        NA,   0.007, NA,
    "Sorghum",       NA,   0.007, 0.006,
    "Rye",           NA,   0.005, 0.011,
    "Other_cereals", 0.22, 0.006, 0.009
  )
}

.ipcc_cereal_long <- function() {
  res <- whep::whep_coef_table("ipcc_residue_coefs")
  columns <- tibble::tribble(
    ~quantity, ~value_col,      ~source_col,
    "rs",      "rs_ratio_ipcc", "source_rs_ratio",
    "n_ag",    "n_ag_residue",  "source_n_ag",
    "n_bg",    "n_bg_residue",  "source_n_bg"
  )
  purrr::pmap(columns, function(quantity, value_col, source_col) {
    .ipcc_cereal_reference() |>
      dplyr::select(ipcc_crop, ipcc = dplyr::all_of(quantity)) |>
      dplyr::left_join(
        dplyr::select(
          res,
          ipcc_crop,
          whep = dplyr::all_of(value_col),
          source = dplyr::all_of(source_col)
        ),
        by = "ipcc_crop"
      ) |>
      dplyr::mutate(quantity = quantity)
  }) |>
    purrr::list_rbind()
}

test_that("cereal root:shoot and N match the IPCC tables they cite", {
  long <- .ipcc_cereal_long()
  sourced <- dplyr::filter(long, !is.na(ipcc))
  testthat::expect_equal(sourced$whep, sourced$ipcc)
  testthat::expect_false(any(stringr::str_detect(sourced$source, "^fill")))
})

test_that("cereal values IPCC does not give are declared generic fills", {
  long <- .ipcc_cereal_long()
  filled <- dplyr::filter(long, is.na(ipcc))
  # IPCC (2019) Table 11.1a note e: "No estimate is available. The most
  # appropriate generic value can be used"; Generic Grains is 0.22 and 0.009.
  generic <- c(rs = 0.22, n_ag = 0.006, n_bg = 0.009)
  testthat::expect_equal(filled$whep, unname(generic[filled$quantity]))
  testthat::expect_true(all(stringr::str_detect(filled$source, "^fill")))
})

test_that("ipcc_residue_coefs labels every value with its own source", {
  res <- whep::whep_coef_table("ipcc_residue_coefs")
  cols <- c(
    "source_slope_intercept",
    "source_rs_ratio",
    "source_n_ag",
    "source_n_bg"
  )
  testthat::expect_true(all(cols %in% names(res)))
  testthat::expect_false("source" %in% names(res))
  for (col in cols) {
    testthat::expect_false(any(is.na(res[[col]]) | res[[col]] == ""))
  }
})

test_that("the live root table carries the corrected cereal root:shoot", {
  root <- whep::whep_coef_table("ipcc_root_coefs")
  res <- whep::whep_coef_table("ipcc_residue_coefs")
  cereals <- setdiff(.ipcc_cereal_reference()$ipcc_crop, "Other_cereals")
  joined <- dplyr::inner_join(
    dplyr::filter(root, ipcc_crop %in% cereals),
    res,
    by = "ipcc_crop"
  )
  testthat::expect_equal(nrow(joined), length(cereals))
  testthat::expect_equal(joined$rs_default, joined$rs_ratio_ipcc)
})
