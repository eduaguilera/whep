# Tests for grafs_plot_df.R functions
testthat::local_edition(3)


# helper fixtures --------------------------------------------------------------

.fixture_prov_destiny <- function() {
  tibble::tribble(
    ~Province_name, ~Year, ~Item, ~Irrig_cat, ~Box, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Wheat and products", "Irrigated", "Cropland", "Cropland", "export", 100,
    "Huesca", 2000, "Wheat and products", "Irrigated", "Cropland", "Cropland", "population_food", 40,
    "Huesca", 2000, "Wheat and products", "Rainfed", "Cropland", "Cropland", "livestock_rum", 30,
    "Huesca", 2000, "Barley and products", "Rainfed", "Cropland", "Cropland", "livestock_mono", 20,
    "Huesca", 2000, "Bovine Meat", NA, "Livestock", "Livestock", "population_food", 15,
    "Huesca", 2000, "Wool (Clean Eq.)", NA, "Livestock", "Livestock", "population_food", 5,
    "Huesca", 2000, "Bovine Meat", NA, "Livestock", "Livestock", "export", 8,
    "Huesca", 2000, "Grassland", NA, "semi_natural_agroecosystems", "semi_natural_agroecosystems", "livestock_rum", 25,
    "Huesca", 2000, "Wheat and products", "Irrigated", "Cropland", "Synthetic", "Cropland", 60,
    "Huesca", 2000, "Wheat and products", "Irrigated", "Cropland", "Fixation", "Cropland", 10,
    "Huesca", 2000, "Bovine Meat", NA, "Livestock", "Outside", "population_food", 3
  )
}


# .rename_destiny_pascal -------------------------------------------------------

test_that(".rename_destiny_pascal maps snake_case flow columns to PascalCase", {
  snake <- tibble::tribble(
    ~year, ~province_name, ~item, ~irrig_cat, ~box, ~origin, ~destiny, ~mg_n,
    2000, "Huesca", "Wheat", "Rainfed", "Cropland", "Cropland", "export", 1
  )

  out <- .rename_destiny_pascal(snake)

  expect_true(
    all(
      c(
        "Province_name",
        "Year",
        "Item",
        "Irrig_cat",
        "Box",
        "Origin",
        "Destiny",
        "MgN"
      ) %in%
        names(out)
    )
  )
  expect_equal(out$MgN, 1)
})


# .create_n_flow_df ------------------------------------------------------------

test_that(".create_n_flow_df labels and sums crop and livestock flows", {
  out <- .create_n_flow_df(.fixture_prov_destiny())

  expect_setequal(
    names(out),
    c("province", "year", "label", "data", "align")
  )

  crop_export <- out |>
    dplyr::filter(label == "{CROP_EXPORT}") |>
    dplyr::pull(data)
  expect_equal(crop_export, 100)

  crops_to_livestock <- out |>
    dplyr::filter(label == "{CROPS_TO_LIVESTOCK}") |>
    dplyr::pull(data)
  expect_equal(crops_to_livestock, 50)

  # all five flow labels are always present (completed with zeros)
  expect_setequal(
    unique(out$label),
    c(
      "{CROP_EXPORT}",
      "{CROPS_TO_POP}",
      "{CROPS_TO_LIVESTOCK}",
      "{LIVESTOCK_TO_HUMAN}",
      "{GRASS_TO_LIVESTOCK}"
    )
  )
})


# .create_livestock_df ---------------------------------------------------------

test_that(".create_livestock_df splits edible and non-edible products", {
  out <- .create_livestock_df(.fixture_prov_destiny())

  edible <- out |>
    dplyr::filter(label == "{LV_EDBL}") |>
    dplyr::pull(data)
  non_edible <- out |>
    dplyr::filter(label == "{LVSTCK_NOEDIBLE}") |>
    dplyr::pull(data)

  expect_equal(edible, 15)
  expect_equal(non_edible, 5)
})


# .create_feed_df --------------------------------------------------------------

test_that(".create_feed_df separates ruminant and monogastric feed", {
  out <- .create_feed_df(.fixture_prov_destiny())

  rum <- out |>
    dplyr::filter(label == "{RCRTOLVSTCK_R}") |>
    dplyr::pull(data)
  mono <- out |>
    dplyr::filter(label == "{MCRTOLVSTCK_M}") |>
    dplyr::pull(data)

  expect_equal(rum, 30)
  expect_equal(mono, 20)
})


# .create_livestock_total_df ---------------------------------------------------

test_that(".create_livestock_total_df sums livestock output to a single label", {
  out <- .create_livestock_total_df(.fixture_prov_destiny())

  expect_equal(unique(out$label), "{LVSTCKTOTN}")
  # two food rows and one export row for livestock sum to twenty-eight
  expect_equal(out$data, 28)
})

test_that(".create_livestock_total_df includes population_food_inedible", {
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Livestock", "population_food", 10,
    "Huesca", 2000, "Livestock", "population_food_inedible", 5,
    "Huesca", 2000, "Cropland", "population_food_inedible", 999
  )

  out <- .create_livestock_total_df(prov_destiny_df)

  expect_equal(out$data, 15)
})


# .create_land_surplus_df ------------------------------------------------------

test_that(".create_land_surplus_df computes inputs minus outputs", {
  out <- .create_land_surplus_df(.fixture_prov_destiny())

  crop_surplus <- out |>
    dplyr::filter(label == "{CROP_SURPLUS}") |>
    dplyr::pull(data)

  # crop inputs are synthetic and fixation; crop outputs are export, food,
  # ruminant feed and monogastric feed
  expect_equal(crop_surplus, (60 + 10) - (100 + 40 + 30 + 20))
})

test_that(".create_land_surplus_df counts population_food_inedible as output", {
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Synthetic", "Cropland", 100,
    "Huesca", 2000, "Cropland", "population_food", 30,
    "Huesca", 2000, "Cropland", "population_food_inedible", 10
  )

  out <- .create_land_surplus_df(prov_destiny_df) |>
    dplyr::filter(label == "{CROP_SURPLUS}")

  # 100 input - (30 + 10) output = 60, not 70.
  expect_equal(out$data, 60)
})


# .create_land_df helpers ------------------------------------------------------

test_that(".is_crop matches land use, biomass set and irrigation", {
  mask <- .is_crop(
    land_use = c("Cropland", "Cropland", "Forest_low"),
    biomass = c("Olive", "Wheat", "Olive"),
    biomass_set = c("Olive"),
    irrig_cat = c("Irrigated", "Irrigated", "Irrigated"),
    irrig = "Irrigated"
  )
  expect_equal(mask, c(TRUE, FALSE, FALSE))
})

test_that(".sum_land_n sums the three N components under a mask", {
  total <- .sum_land_n(
    prod_n = c(1, 10),
    residue_n = c(2, 20),
    grazed_n = c(3, 30),
    mask = c(TRUE, FALSE)
  )
  expect_equal(total, 6)
})


# .create_land_df --------------------------------------------------------------

test_that(".create_land_df aggregates area from n_balance and N from destiny data", {
  local_mocked_bindings(
    whep_read_file = function(alias) {
      if (alias == "n_balance_ygpit_all") {
        return(
          tibble::tribble(
            ~Province_name, ~Year, ~LandUse, ~Name_biomass, ~Irrig_cat, ~Area_ygpit_ha, ~Prod_MgN, ~UsedResidue_MgN, ~GrazedWeeds_MgN,
            "Huesca", 2000, "Cropland", "Olive", "Irrigated", 100, 5, 1, 0,
            "Huesca", 2000, "Cropland", "Wheat", "Rainfed", 200, 8, 2, 0,
            "Huesca", 2000, "Forest_low", "Oak", NA, 500, 3, 0, 1
          )
        )
      }
      if (alias == "grafs_crop_categories") {
        return(
          tibble::tribble(
            ~crop_type, ~Name_biomass,
            "permanent", "Olive",
            "non_permanent", "Wheat"
          )
        )
      }
      # codes_coefs_items_full: bridges prov_destiny_df$Item to Name_biomass.
      tibble::tribble(
        ~item, ~Name_biomass,
        "Olives (including preserved)", "Olive",
        "Wheat and products", "Wheat"
      )
    },
    # Both fixture items are primary crops (their own Name_biomass already
    # resolves), so no processed item needs a parent-crop share here.
    .processing_item_shares = function(...) {
      tibble::tibble(
        Item = character(),
        Name_biomass = character(),
        share = double()
      )
    }
  )

  # No residue-item rows here, so .compute_residue_shares()'s reallocation
  # contributes zero and the PERiN/NPErN totals below come only from the
  # matching Cropland rows.
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Item, ~Irrig_cat, ~Origin, ~MgN,
    "Huesca", 2000, "Olives (including preserved)", "Irrigated", "Cropland", 40,
    "Huesca", 2000, "Wheat and products", "Rainfed", "Cropland", 60
  )

  out <- dplyr::filter(.create_land_df(prov_destiny_df), province == "Huesca")

  peri_ha <- out |>
    dplyr::filter(label == "{PERiha}") |>
    dplyr::pull(data)
  expect_equal(peri_ha, 100)

  forn <- out |>
    dplyr::filter(label == "{FORN}") |>
    dplyr::pull(data)
  expect_equal(forn, 3 + 0 + 1)

  peri_n <- out |>
    dplyr::filter(label == "{PERiN}") |>
    dplyr::pull(data)
  expect_equal(peri_n, 40)

  nper_n <- out |>
    dplyr::filter(label == "{NPErN}") |>
    dplyr::pull(data)
  expect_equal(nper_n, 60)
})


# .create_crop_type_n_df: horticulture folding and byproduct remap ------------

test_that(".create_crop_type_n_df folds horticulture into non-permanent", {
  local_mocked_bindings(
    whep_read_file = function(alias) {
      tibble::tribble(
        ~item, ~Name_biomass,
        "Tomatoes and products", "Tomato"
      )
    },
    # Tomato is a primary crop (its own Name_biomass already resolves), so
    # no processed item needs a parent-crop share here.
    .processing_item_shares = function(...) {
      tibble::tibble(
        Item = character(),
        Name_biomass = character(),
        share = double()
      )
    }
  )
  crop_lookup <- tibble::tribble(
    ~crop_type, ~Name_biomass,
    "horticulture", "Tomato"
  )
  n_balance <- tibble::tibble(
    Province_name = character(),
    Year = numeric(),
    LandUse = character(),
    Name_biomass = character(),
    Irrig_cat = character(),
    Prod_MgN = numeric(),
    UsedResidue_MgN = numeric(),
    GrazedWeeds_MgN = numeric()
  )
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Item, ~Irrig_cat, ~Origin, ~MgN,
    "Huesca", 2000, "Tomatoes and products", "Irrigated", "Cropland", 70
  )

  out <- .create_crop_type_n_df(prov_destiny_df, crop_lookup, n_balance)
  huesca <- dplyr::filter(out, province == "Huesca")

  # Tomato has no permanent/non_permanent crop_type of its own -- only
  # "horticulture" -- so it must land in {NPEiN}, not vanish.
  expect_equal(
    dplyr::pull(dplyr::filter(huesca, label == "{NPEiN}"), data),
    70
  )
  expect_false(any(huesca$data[huesca$label == "{PERiN}"] > 0))
})

test_that(".create_crop_type_n_df remaps a processed byproduct to its parent crop", {
  local_mocked_bindings(
    whep_read_file = function(alias) {
      tibble::tribble(
        ~item, ~Name_biomass,
        "Soyabean Cake", "Soyabean cake biomass"
      )
    },
    # Real get_processing_coefs()-derived share: Soyabean Cake comes 100%
    # from Soyabeans. .processing_item_shares() itself is tested separately.
    .processing_item_shares = function(...) {
      tibble::tribble(
        ~Item, ~Name_biomass, ~share,
        "Soyabean Cake", "Soyabeans", 1
      )
    }
  )
  crop_lookup <- tibble::tribble(
    ~crop_type, ~Name_biomass,
    "non_permanent", "Soyabeans"
  )
  # Soyabeans production is 100% Irrigated for this province/year (the
  # explicit zero Rainfed row matters: an absent row, rather than a zero
  # one, would leave .compute_irrigation_shares() with nothing to join
  # against and fall back to 0.5 for that combination), so the byproduct
  # (which carries Irrig_cat = NA) must be allocated fully to {NPEiN}, none
  # to {NPErN}.
  n_balance <- tibble::tribble(
    ~Province_name, ~Year, ~LandUse, ~Name_biomass, ~Irrig_cat, ~Prod_MgN, ~UsedResidue_MgN, ~GrazedWeeds_MgN,
    "Huesca", 2000, "Cropland", "Soyabeans", "Irrigated", 50, 0, 0,
    "Huesca", 2000, "Cropland", "Soyabeans", "Rainfed", 0, 0, 0
  )
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Item, ~Irrig_cat, ~Origin, ~MgN,
    "Huesca", 2000, "Soyabean Cake", NA_character_, "Cropland", 20
  )

  out <- .create_crop_type_n_df(prov_destiny_df, crop_lookup, n_balance)
  huesca <- dplyr::filter(out, province == "Huesca")

  expect_equal(
    dplyr::pull(dplyr::filter(huesca, label == "{NPEiN}"), data),
    20
  )
  expect_equal(
    sum(dplyr::filter(huesca, label == "{NPErN}")$data),
    0
  )
})


# .processing_item_shares -------------------------------------------------------

test_that(".processing_item_shares computes real parent-crop shares from processed volumes", {
  local_mocked_bindings(
    get_processing_coefs = function(years) {
      tibble::tribble(
        ~area_code, ~item_cbs_code_to_process, ~item_cbs_code_processed, ~value_to_process,
        203L, 1L, 100L, 75,
        203L, 2L, 100L, 25,
        203L, 3L, 200L, 10,
        # A different country must not leak into Spain's shares.
        999L, 1L, 100L, 999
      )
    },
    add_item_cbs_name = function(table, code_column, name_column) {
      dplyr::mutate(
        table,
        "{name_column}" := paste0("code_", .data[[code_column]])
      )
    }
  )
  item_lookup <- tibble::tribble(
    ~Item, ~Name_biomass,
    "code_1", "CropA",
    "code_2", "CropB",
    "code_3", "CropC"
  )

  out <- .processing_item_shares(item_lookup, exclude_items = character())
  processed_100 <- dplyr::filter(out, Item == "code_100")

  expect_equal(nrow(processed_100), 2)
  expect_equal(
    dplyr::pull(dplyr::filter(processed_100, Name_biomass == "CropA"), share),
    0.75
  )
  expect_equal(
    dplyr::pull(dplyr::filter(processed_100, Name_biomass == "CropB"), share),
    0.25
  )
  # code_200's only parent gets share 1, same as a single-source byproduct.
  expect_equal(
    dplyr::pull(dplyr::filter(out, Item == "code_200"), share),
    1
  )
})

test_that(".processing_item_shares drops excluded output items and unresolved parents", {
  local_mocked_bindings(
    get_processing_coefs = function(years) {
      tibble::tribble(
        ~area_code, ~item_cbs_code_to_process, ~item_cbs_code_processed, ~value_to_process,
        203L, 1L, 100L, 50,
        203L, 1L, 300L, 50,
        # code_4 has no Name_biomass in item_lookup below -- must be
        # dropped rather than leaving a Name_biomass = NA row.
        203L, 4L, 400L, 999
      )
    },
    add_item_cbs_name = function(table, code_column, name_column) {
      dplyr::mutate(
        table,
        "{name_column}" := paste0("code_", .data[[code_column]])
      )
    }
  )
  item_lookup <- tibble::tribble(
    ~Item, ~Name_biomass,
    "code_1", "CropA"
  )

  out <- .processing_item_shares(item_lookup, exclude_items = "code_300")

  expect_false("code_300" %in% out$Item)
  expect_true("code_100" %in% out$Item)
  expect_false("code_400" %in% out$Item)
})


# .add_national_n_balance ------------------------------------------------------

test_that(".add_national_n_balance sums provinces into a Spain row", {
  n_balance <- tibble::tribble(
    ~Province_name, ~Year, ~LandUse, ~Name_biomass, ~Irrig_cat, ~Prod_MgN, ~UsedResidue_MgN, ~GrazedWeeds_MgN,
    "Huesca", 2000, "Cropland", "Wheat", "Irrigated", 10, 1, 0,
    "Teruel", 2000, "Cropland", "Wheat", "Irrigated", 5, 2, 0
  )

  out <- .add_national_n_balance(n_balance)
  spain <- dplyr::filter(out, Province_name == "Spain")

  expect_equal(nrow(spain), 1)
  expect_equal(spain$Prod_MgN, 15)
  expect_equal(spain$UsedResidue_MgN, 3)
  # Provincial rows survive unchanged alongside the new Spain row.
  expect_equal(nrow(out), 3)
})


# .compute_irrigation_shares ----------------------------------------------

test_that(".compute_irrigation_shares falls back to 0.5 with no production", {
  n_balance_full <- tibble::tribble(
    ~Province_name, ~Year, ~LandUse, ~Name_biomass, ~Irrig_cat, ~Prod_MgN,
    "Huesca", 2000, "Cropland", "Wheat", "Irrigated", 30,
    "Huesca", 2000, "Cropland", "Wheat", "Rainfed", 10,
    "Huesca", 2000, "Cropland", "Barley", "Irrigated", 0,
    "Huesca", 2000, "Cropland", "Barley", "Rainfed", 0
  )

  out <- .compute_irrigation_shares(n_balance_full)
  wheat_irrig <- out |>
    dplyr::filter(Name_biomass == "Wheat", Irrig_cat == "Irrigated") |>
    dplyr::pull(share)
  barley_irrig <- out |>
    dplyr::filter(Name_biomass == "Barley", Irrig_cat == "Irrigated") |>
    dplyr::pull(share)

  expect_equal(wheat_irrig, 0.75)
  # Zero total production for Barley -> 50/50 fallback, not NaN or 0.
  expect_equal(barley_irrig, 0.5)
})


# create_grafs_plot_df (example) -----------------------------------------------

test_that("create_grafs_plot_df(example = TRUE) returns the documented schema", {
  out <- whep::create_grafs_plot_df(example = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_setequal(
    names(out),
    c("province", "year", "label", "data", "align", "arrowColor")
  )
  expect_type(out$data, "character")
})


# .create_n_import_df ----------------------------------------------------------

test_that(".create_n_import_df labels, aggregates and aligns import flows", {
  testthat::local_mocked_bindings(
    whep_read_file = function(alias) {
      tibble::tribble(
        ~item, ~group,
        "Soybean cake", "Agro-industry",
        "Bovine Meat", "Livestock products",
        "Wheat and products", "Crop products"
      )
    }
  )

  prov <- tibble::tribble(
    ~Province_name, ~Year, ~Item, ~Irrig_cat, ~Box, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Soybean cake", NA, "Agro-industry", "Outside", "livestock_rum", 40,
    "Huesca", 2000, "Soybean cake", NA, "Agro-industry", "Outside", "livestock_mono", 20,
    "Huesca", 2000, "Bovine Meat", NA, "Livestock", "Outside", "population_food", 15,
    "Huesca", 2000, "Wheat and products", NA, "Cropland", "Outside", "livestock_rum", 10,
    "Huesca", 2000, "Wheat and products", NA, "Cropland", "Synthetic", "Cropland", 60,
    "Huesca", 2000, "Wheat and products", NA, "Cropland", "Deposition", "Cropland", 5,
    "Huesca", 2000, "Wheat and products", NA, "Cropland", "Fixation", "Cropland", 8,
    "Huesca", 2000, "Wheat and products", NA, "Cropland", "People", "Cropland", 3,
    "Huesca", 2000, "Wheat and products", NA, "Cropland", "People", "population_other_uses", 7,
    "Huesca", 2000, "Wheat and products", NA, "Cropland", "Outside", "population_food", 12
  )

  out <- .create_n_import_df(prov)
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)
  align_of <- function(l) dplyr::pull(dplyr::filter(out, label == l), align)

  expect_equal(pick("{IMANOTR}"), 40)
  expect_equal(pick("{IMANOTM}"), 20)
  expect_equal(pick("{IMANOT}"), 60)
  expect_equal(pick("{IMPHUMANMEAT}"), 15)
  expect_equal(pick("{IMPHMANA}"), 15)
  expect_equal(pick("{IMPORT_ANIMALCR_RUM}"), 10)
  expect_equal(pick("{IMPORT_ANIMALCR}"), 10)
  expect_equal(pick("{SYNTHF}"), 60)
  expect_equal(pick("{SYNTHF_TOTAL}"), 60)
  expect_equal(pick("{OXDEPCROPS}"), 5)
  expect_equal(pick("{FIXCR}"), 8)
  expect_equal(pick("{FIX_DEP_CR}"), 13)
  # People-origin N to Cropland specifically is {ORGOT} (organic-other reuse).
  # {WASTEWATER} is not a destiny lookup at all -- it's the residual
  # .create_wastewater_surplus_df() computes -- so a People-origin row to any
  # other destiny (population_other_uses here) is simply unclassified.
  expect_equal(pick("{ORGOT}"), 3)
  expect_equal(pick("{CROP_POPIMPORT}"), 12)

  expect_equal(align_of("{CROP_POPIMPORT}"), "R")
  expect_equal(align_of("{IMANOTR}"), "R")
  expect_equal(align_of("{IMPORT_ANIMALCR}"), "R")
  expect_equal(align_of("{SYNTHF}"), "L")
  expect_equal(align_of("{ORGOT}"), "R")
  expect_equal(align_of("{IMPHUMANMEAT}"), "L")

  expect_false("{IMPORT_ANIMALCR_MONOG}" %in% out$label)
  expect_false("{WASTEWATER}" %in% out$label)
})


# .create_animal_losses_df -----------------------------------------------------

test_that(".create_animal_losses_df sums excreted N as animal losses", {
  # n_excretion_ygs reports excreted N directly in MgN (N_excr_MgN); it no
  # longer carries the Gross_Prod_GgN / Net_Prod_GgN pair.
  testthat::local_mocked_bindings(
    whep_read_file = function(alias) {
      tibble::tribble(
        ~Year, ~Province_name, ~Livestock_cat, ~N_excr_MgN,
        2000, "Huesca", "Cattle", 500,
        2000, "Huesca", "Pigs", 200
      )
    }
  )

  prov <- tibble::tribble(
    ~Province_name, ~Year, ~Item, ~Irrig_cat, ~Box, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Bovine Meat", NA, "Livestock", "Livestock", "population_other_uses", 50
  )

  out <- .create_animal_losses_df(prov)
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)

  expect_equal(pick("{AN_LS}"), 700)
  expect_equal(pick("{AN_OTH}"), 50)
  expect_equal(pick("{AN_LS_OTH}"), 750)
  expect_true(all(out$align == "R"))
})


# .create_livestock_lu_df ------------------------------------------------------

test_that(".create_livestock_lu_df computes LU and million-LU per system", {
  testthat::local_mocked_bindings(
    whep_read_file = function(alias) {
      if (alias == "livestock_prod_ygps") {
        return(
          tibble::tribble(
            ~Province_name, ~Year, ~Livestock_cat, ~Stock_Number,
            "Huesca", 2000, "Cattle", 1000000,
            "Huesca", 2000, "Pigs", 2000000
          )
        )
      }
      tibble::tribble(
        ~Livestock_cat, ~LU_head, ~system,
        "Cattle", 0.8, "ruminant",
        "Pigs", 0.3, "monogastric"
      )
    }
  )

  out <- .create_livestock_lu_df()
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)

  expect_equal(pick("{RUMIANTSLU}"), 800000)
  expect_equal(pick("{RUMIANTSMLU}"), 0.8)
  expect_equal(pick("{MONOGLU}"), 600000)
  expect_equal(pick("{MONOGMLU}"), 0.6)
  expect_true(all(out$align == "R"))
})


# .create_n_input_df -----------------------------------------------------------

test_that(".create_n_input_df aggregates greenhouse, grass, area and km2", {
  n_balance <- tibble::tribble(
    ~Province_name, ~Year, ~LandUse, ~Irrig_cat, ~Area_ygpit_ha, ~Prod_MgN, ~UsedResidue_MgN, ~GrazedWeeds_MgN,
    "Huesca", 2000, "Cropland", "Greenhouse", 50, 4, 1, 0,
    "Huesca", 2000, "Cropland", "Irrigated", 100, 8, 2, 0,
    "Huesca", 2000, "Forest_low", NA, 500, 0, 0, 0
  )
  # {GREHN} now comes from prov_destiny_df (Origin == Cropland, Irrig_cat ==
  # Greenhouse), not from n_balance's Prod/Residue/Grazed columns.
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Origin, ~Irrig_cat, ~MgN,
    "Huesca", 2000, "Cropland", "Greenhouse", 9,
    "Huesca", 2000, "Cropland", "Irrigated", 999
  )

  out <- .create_n_input_df(n_balance, prov_destiny_df)
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)

  expect_equal(pick("{GREHha}"), 50)
  expect_equal(pick("{GREHN}"), 9)
  expect_equal(pick("{HAGRASS}"), 500)
  expect_equal(pick("{HACULT}"), 150)
  expect_equal(pick("{KM2_PROVINCE}"), 6.5)
  expect_equal(pick("{GREHMha}"), 5e-05)
  expect_true(all(out$align == "L"))
})


# .rescale_grafs_labels --------------------------------------------------------

test_that(".rescale_grafs_labels divides N labels by 1000, leaves others", {
  df_final <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align, ~arrowColor,
    "Huesca", 2000, "{ARAiN}", "2000", "R", "",
    "Huesca", 2000, "{POPULATIONM}", "0.22", "L", "",
    "Huesca", 2000, "{KM2_PROVINCE}", "6.5", "L", ""
  )

  out <- .rescale_grafs_labels(df_final)
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)

  expect_equal(pick("{ARAiN}"), "2")
  expect_equal(pick("{POPULATIONM}"), "0.22")
  expect_equal(pick("{KM2_PROVINCE}"), "6.5")
  expect_type(out$data, "character")
})


# .add_spain_totals ------------------------------------------------------------

test_that(".add_spain_totals sums provinces into Spain for missing_labels", {
  df_final <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align, ~arrowColor,
    "Huesca", 2000, "{FORN}", "10", "R", "",
    "Teruel", 2000, "{FORN}", "5", "R", "",
    "Huesca", 2000, "{POPULATIONM}", "0.2", "L", "",
    "Teruel", 2000, "{POPULATIONM}", "0.1", "L", "",
    "Huesca", 2000, "{CROP_EXPORT}", "100", "L", ""
  )

  out <- .add_spain_totals(df_final)
  spain <- function(l) {
    dplyr::pull(dplyr::filter(out, province == "Spain", label == l), data)
  }

  expect_equal(spain("{FORN}"), "15")
  expect_equal(spain("{POPULATIONM}"), "0.3")
  expect_equal(
    nrow(dplyr::filter(out, province == "Spain", label == "{CROP_EXPORT}")),
    0
  )
  expect_equal(nrow(out), 7)
})


# .collapse_grafs_labels -------------------------------------------------------

test_that(".collapse_grafs_labels takes first for non-additive, sums additive", {
  df_final <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align, ~arrowColor,
    "Spain", 2000, "{POPULATIONM}", "0.2", "L", "",
    "Spain", 2000, "{POPULATIONM}", "0.3", "L", "",
    "Spain", 2000, "{FORN}", "10", "R", "",
    "Spain", 2000, "{FORN}", "5", "R", "",
    "Huesca", 2000, "{WIDTH_MAX}", "1500", "L", ""
  )

  out <- .collapse_grafs_labels(df_final)
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)

  expect_equal(pick("{POPULATIONM}"), "0.2")
  expect_equal(pick("{FORN}"), "15")
  expect_equal(pick("{WIDTH_MAX}"), "1500")
  expect_equal(nrow(out), 3)
  expect_true(all(out$arrowColor == ""))
})


# .create_livestock_surplus_df -------------------------------------------------

test_that(".create_livestock_surplus_df computes LIVGASLOSS as inputs - outputs", {
  df_all_flows <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align,
    "Huesca", 2000, "{CROPS_TO_LIVESTOCK}", "50", "L",
    "Huesca", 2000, "{GRASS_TO_LIVESTOCK}", "25", "L",
    "Huesca", 2000, "{LIVESTOCK_TO_HUMAN}", "20", "L",
    "Huesca", 2000, "{AN_OTH}", "5", "R",
    "Huesca", 2000, "{CROP_EXPORT}", "999", "L"
  )
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Synthetic", "Cropland", 999
  )

  out <- .create_livestock_surplus_df(df_all_flows, prov_destiny_df)

  expect_equal(unique(out$label), "{LIVGASLOSS}")
  expect_equal(out$data, 50)
  expect_equal(out$align, "R")
})

test_that(".create_livestock_surplus_df adds the inedible remainder to output", {
  df_all_flows <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align,
    "Huesca", 2000, "{CROPS_TO_LIVESTOCK}", "50", "L",
    "Huesca", 2000, "{LIVESTOCK_TO_HUMAN}", "20", "L"
  )
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Livestock", "population_food_inedible", 5,
    # A Cropland-origin inedible row must not leak into the livestock total.
    "Huesca", 2000, "Cropland", "population_food_inedible", 999
  )

  out <- .create_livestock_surplus_df(df_all_flows, prov_destiny_df)

  # input 50 - output (20 + 5 inedible) = 25, not 30.
  expect_equal(out$data, 25)
})


# .create_wastewater_surplus_df -------------------------------------------------

test_that(".create_wastewater_surplus_df computes consumption minus returned N", {
  df_all_flows <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align,
    "Huesca", 2000, "{CROPS_TO_POP}", "80", "L",
    "Huesca", 2000, "{LIVESTOCK_TO_HUMAN}", "20", "L",
    "Huesca", 2000, "{CROP_EXPORT}", "999", "L"
  )
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "People", "Cropland", 30,
    "Huesca", 2000, "People", "semi_natural_agroecosystems", 5,
    "Huesca", 2000, "People", "population_other_uses", 999,
    "Huesca", 2000, "Synthetic", "Cropland", 999
  )

  out <- .create_wastewater_surplus_df(df_all_flows, prov_destiny_df)

  # input totals 100 (80 crops-to-pop plus 20 livestock-to-human); returned
  # totals 35 (30 Cropland plus 5 semi_natural), leaving a residual of 65.
  expect_equal(unique(out$label), "{WASTEWATER}")
  expect_equal(out$data, 65)
  expect_equal(out$align, "R")
})

test_that(".create_wastewater_surplus_df treats a missing side as zero", {
  df_all_flows <- tibble::tibble(
    province = character(),
    year = numeric(),
    label = character(),
    data = character(),
    align = character()
  )
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "People", "Cropland", 12
  )

  out <- .create_wastewater_surplus_df(df_all_flows, prov_destiny_df)

  # No consumption labels at all -> input = 0, returned = 12 -> data = -12.
  expect_equal(out$data, -12)
})


# .create_cropland_total_df ----------------------------------------------------

test_that(".create_cropland_total_df sums the four cropland-output labels", {
  df_flow <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align,
    "Huesca", 2000, "{CROP_EXPORT}", 100, "L",
    "Huesca", 2000, "{CROPS_TO_POP}", 40, "L",
    "Huesca", 2000, "{CROPS_TO_LIVESTOCK}", 50, "L",
    "Huesca", 2000, "{LIVESTOCK_TO_HUMAN}", 15, "L",
    "Spain", 2000, "{CROP_EXPORT}", 10, "L"
  )
  df_processing_losses <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align,
    "Huesca", 2000, "{CRP_PROCLOSS}", 20, "L"
  )
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Synthetic", "Cropland", 999
  )

  out <- .create_cropland_total_df(
    df_flow,
    df_processing_losses,
    prov_destiny_df
  )
  pick <- function(prov) {
    dplyr::pull(
      dplyr::filter(out, province == prov, label == "{CRPLNDTOTN}"),
      data
    )
  }

  # Huesca: export + pop + livestock + processing losses.
  expect_equal(pick("Huesca"), 210)
  expect_equal(pick("Spain"), 10)
  expect_true(all(out$align == "R"))
  expect_equal(unique(out$label), "{CRPLNDTOTN}")
})

test_that(".create_cropland_total_df adds the inedible remainder for Cropland origin only", {
  df_flow <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align,
    "Huesca", 2000, "{CROPS_TO_POP}", 40, "L"
  )
  df_processing_losses <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align,
    "Huesca", 2000, "{CRP_PROCLOSS}", 0, "L"
  )
  prov_destiny_df <- tibble::tribble(
    ~Province_name, ~Year, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Cropland", "population_food_inedible", 10,
    # A Livestock-origin inedible row must not leak into the cropland total.
    "Huesca", 2000, "Livestock", "population_food_inedible", 999
  )

  out <- .create_cropland_total_df(
    df_flow,
    df_processing_losses,
    prov_destiny_df
  )

  expect_equal(out$data[out$province == "Huesca"], 50)
})


# .fill_finalize_labels --------------------------------------------------------

test_that(".fill_finalize_labels injects meta rows and zero-fills missing", {
  df_combi <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align,
    "Huesca", 2000, "{CROP_EXPORT}", "100", "L"
  )

  out <- .fill_finalize_labels(df_combi)
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)
  align_of <- function(l) dplyr::pull(dplyr::filter(out, label == l), align)

  expect_equal(pick("{WIDTH_MAX}"), "1500")
  expect_equal(pick("{YEAR}"), "2000")
  expect_equal(pick("{PROVINCE_NAME}"), "Huesca")
  expect_equal(pick("{CRPNOLV}"), "0")
  expect_equal(pick("{IMPHUMHONEY}"), "0")
  expect_equal(pick("{CROP_EXPORT}"), "100")
  expect_equal(align_of("{WIDTH_MAX}"), "L")
  expect_equal(align_of("{NCONTCROP}"), "R")
})


# .combine_and_finalize_df -----------------------------------------------------

test_that(".combine_and_finalize_df binds flow dfs and finalizes", {
  empty <- tibble::tibble(
    province = character(),
    year = numeric(),
    label = character(),
    data = numeric(),
    align = character()
  )
  f1 <- tibble::tibble(
    province = "Huesca",
    year = 2000,
    label = "{CROP_EXPORT}",
    data = 100,
    align = "L"
  )
  f2 <- tibble::tibble(
    province = "Huesca",
    year = 2000,
    label = "{LV_EDBL}",
    data = 15,
    align = "L"
  )

  out <- .combine_and_finalize_df(
    crop_livestock_flows = f1,
    df_livestock = f2,
    df_lv_r_m = empty,
    df_crop_losses = empty,
    df_processing_losses = empty,
    df_animal_losses = empty,
    df_livestock_total = empty,
    df_livestock_surplus = empty,
    df_land_surplus = empty
  )
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)

  expect_equal(names(out), c("province", "year", "label", "data", "align"))
  expect_equal(pick("{WIDTH_MAX}"), "1500")
  expect_true("{YEAR}" %in% out$label)
  expect_true("{PROVINCE_NAME}" %in% out$label)
  expect_equal(pick("{CROP_EXPORT}"), "100")
  expect_equal(pick("{LV_EDBL}"), "15")
})


# .create_milk_df --------------------------------------------------------------

test_that(".create_milk_df sums dairy items to LVST_MILK", {
  prov <- tibble::tribble(
    ~Province_name, ~Year, ~Item, ~Irrig_cat, ~Box, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Milk - Excluding Butter", NA, "Livestock", "Livestock", "population_food", 12,
    "Huesca", 2000, "Butter, Ghee", NA, "Livestock", "Livestock", "population_food", 3,
    "Huesca", 2000, "Milk - Excluding Butter", NA, "Livestock", "Livestock", "export", 99,
    "Huesca", 2000, "Bovine Meat", NA, "Livestock", "Livestock", "population_food", 50
  )

  out <- .create_milk_df(prov)

  expect_equal(unique(out$label), "{LVST_MILK}")
  expect_equal(out$data, 15)
  expect_equal(out$align, "L")
})


# .create_livestock_export_df --------------------------------------------------

test_that(".create_livestock_export_df sums exported livestock N", {
  out <- .create_livestock_export_df(.fixture_prov_destiny())

  expect_equal(unique(out$label), "{LIVESTOCK_EXPORTED}")
  expect_equal(out$data, 8)
  expect_equal(out$align, "L")
})


# .create_crop_losses_df -------------------------------------------------------

test_that(".create_crop_losses_df sums cropland other-uses to CRP_OTHUSES", {
  prov <- tibble::tribble(
    ~Province_name, ~Year, ~Item, ~Irrig_cat, ~Box, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "Wheat and products", "Irrigated", "Cropland", "Cropland", "population_other_uses", 7,
    "Huesca", 2000, "Wheat and products", "Irrigated", "Cropland", "Cropland", "population_food", 40
  )

  out <- .create_crop_losses_df(prov)

  expect_equal(unique(out$label), "{CRP_OTHUSES}")
  expect_equal(out$data, 7)
  expect_equal(out$align, "L")
  expect_true(all(c("province", "year") %in% names(out)))
})


# .create_population_df --------------------------------------------------------

test_that(".create_population_df maps Pop_Mpeop_yg to POPULATIONM", {
  testthat::local_mocked_bindings(
    whep_read_file = function(alias) {
      tibble::tribble(
        ~Province_name, ~Year, ~Pop_Mpeop_yg,
        "Huesca", 2000, 0.22
      )
    }
  )

  out <- .create_population_df()

  expect_equal(unique(out$label), "{POPULATIONM}")
  expect_equal(out$data, 0.22)
  expect_equal(out$align, "L")
  expect_equal(names(out), c("province", "year", "label", "data", "align"))
})


# .select_flow_cols ------------------------------------------------------------

test_that(".select_flow_cols keeps the five canonical flow columns", {
  df <- tibble::tibble(
    province = "Huesca",
    year = 2000,
    label = "{X}",
    data = "1",
    align = "L",
    extra = "drop_me"
  )

  out <- .select_flow_cols(df)

  expect_equal(names(out), c("province", "year", "label", "data", "align"))
  expect_false("extra" %in% names(out))
})


# create_n_nat_destiny vocabulary contract -------------------------------------

test_that("create_n_nat_destiny(example) stays within the grafs filter vocab", {
  out <- whep::create_n_nat_destiny(example = TRUE)

  expect_true(all(
    unique(out$destiny) %in%
      c(
        "population_food",
        "population_other_uses",
        "livestock_rum",
        "livestock_mono",
        "export",
        "Cropland",
        "semi_natural_agroecosystems"
      )
  ))
  expect_true(all(
    unique(out$origin) %in%
      c(
        "Cropland",
        "semi_natural_agroecosystems",
        "Livestock",
        "Fish",
        "Agro-industry",
        "Deposition",
        "Fixation",
        "Synthetic",
        "People",
        "Outside"
      )
  ))
  expect_true(all(
    unique(out$box) %in%
      c(
        "Cropland",
        "semi_natural_agroecosystems",
        "Livestock",
        "Fish",
        "Agro-industry",
        NA
      )
  ))
})


# .create_n_flow_df noise handling ---------------------------------------------

test_that(".create_n_flow_df drops unmapped rows without leaking NA labels", {
  prov <- tibble::tribble(
    ~Province_name, ~Year, ~Item, ~Irrig_cat, ~Box, ~Origin, ~Destiny, ~MgN,
    "Huesca", 2000, "X", NA, "Cropland", "Cropland", "export", 100,
    "Huesca", 2000, "X", NA, "Fish", "Fish", "export", 999
  )

  out <- .create_n_flow_df(prov)

  expect_false(any(is.na(out$label)))
  expect_equal(
    dplyr::pull(dplyr::filter(out, label == "{CROP_EXPORT}"), data),
    100
  )
})


# .rescale_grafs_labels: text tokens preserved -------------------------------

test_that(".rescale_grafs_labels preserves non-numeric label values", {
  df_final <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align, ~arrowColor,
    "Huesca", 2000, "{PROVINCE_NAME}", "Huesca", "L", "",
    "Huesca", 2000, "{YEAR}", "2000", "L", "",
    "Huesca", 2000, "{ARAiN}", "12340", "R", "",
    "Huesca", 2000, "{POPULATIONM}", "0.22", "L", ""
  )

  out <- .rescale_grafs_labels(df_final)
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)

  # {PROVINCE_NAME} must survive (as.numeric("Huesca") would give NA)
  expect_equal(pick("{PROVINCE_NAME}"), "Huesca")
  expect_equal(pick("{YEAR}"), "2000")
  expect_equal(pick("{ARAiN}"), "12.34")
  expect_equal(pick("{POPULATIONM}"), "0.22")
})


# assemble chain keeps meta rows intact end-to-end ---------------------------

test_that("rescale -> add_spain_totals -> collapse keeps meta rows", {
  df_final <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align, ~arrowColor,
    "Huesca", 2000, "{PROVINCE_NAME}", "Huesca", "L", "",
    "Huesca", 2000, "{YEAR}", "2000", "L", "",
    "Huesca", 2000, "{FORN}", "10", "R", "",
    "Teruel", 2000, "{FORN}", "5", "R", ""
  )

  out <- df_final |>
    .rescale_grafs_labels() |>
    .add_spain_totals() |>
    .collapse_grafs_labels()
  pick <- function(prov, l) {
    dplyr::pull(dplyr::filter(out, province == prov, label == l), data)
  }

  expect_equal(pick("Huesca", "{PROVINCE_NAME}"), "Huesca")
  expect_equal(pick("Huesca", "{YEAR}"), "2000")
  expect_setequal(
    names(out),
    c("province", "year", "label", "data", "align", "arrowColor")
  )
})


# .collapse_grafs_labels: non-additive with leading NA -----------------------

test_that(".collapse_grafs_labels returns first non-NA for non-additive labels", {
  df_final <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align, ~arrowColor,
    "Spain", 2000, "{FORha}", NA_character_, "R", "",
    "Spain", 2000, "{FORha}", "10", "R", ""
  )

  out <- .collapse_grafs_labels(df_final)

  expect_equal(nrow(out), 1)
  expect_equal(out$data, "10")
})


# .create_livestock_lu_df: single system present ------------------------------

test_that(".create_livestock_lu_df tolerates a single livestock system", {
  testthat::local_mocked_bindings(
    whep_read_file = function(alias) {
      if (alias == "livestock_prod_ygps") {
        return(
          tibble::tribble(
            ~Province_name, ~Year, ~Livestock_cat, ~Stock_Number,
            "Huesca", 2000, "Cattle", 1000000
          )
        )
      }
      tibble::tribble(
        ~Livestock_cat, ~LU_head, ~system,
        "Cattle", 0.8, "ruminant"
      )
    }
  )

  out <- .create_livestock_lu_df()
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)

  expect_equal(pick("{RUMIANTSLU}"), 800000)
  expect_equal(pick("{MONOGLU}"), 0)
  expect_equal(pick("{MONOGMLU}"), 0)
})
