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

test_that(".create_land_df aggregates area and N by GRAFS land labels", {
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
      tibble::tribble(
        ~crop_type, ~Name_biomass,
        "permanent", "Olive",
        "non_permanent", "Wheat"
      )
    }
  )

  out <- .create_land_df()

  peri_ha <- out |>
    dplyr::filter(label == "{PERiha}") |>
    dplyr::pull(data)
  expect_equal(peri_ha, 100)

  forn <- out |>
    dplyr::filter(label == "{FORN}") |>
    dplyr::pull(data)
  expect_equal(forn, 3 + 0 + 1)

  npe_r_n <- out |>
    dplyr::filter(label == "{NPErN}") |>
    dplyr::pull(data)
  expect_equal(npe_r_n, 8 + 2 + 0)
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
  expect_equal(pick("{WASTEWATER}"), 3)
  expect_equal(pick("{CROP_POPIMPORT}"), 12)

  expect_equal(align_of("{CROP_POPIMPORT}"), "R")
  expect_equal(align_of("{IMANOTR}"), "R")
  expect_equal(align_of("{IMPORT_ANIMALCR}"), "R")
  expect_equal(align_of("{SYNTHF}"), "L")
  expect_equal(align_of("{WASTEWATER}"), "L")
  expect_equal(align_of("{IMPHUMANMEAT}"), "L")

  expect_false("{IMPORT_ANIMALCR_MONOG}" %in% out$label)
})


# .create_animal_losses_df -----------------------------------------------------

test_that(".create_animal_losses_df converts GgN to MgN and sums losses", {
  testthat::local_mocked_bindings(
    whep_read_file = function(alias) {
      tibble::tribble(
        ~Year, ~Province_name, ~Livestock_cat, ~Gross_Prod_GgN, ~Net_Prod_GgN,
        2000, "Huesca", "Cattle", 2, 1.5,
        2000, "Huesca", "Pigs", 1, 0.8
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

  out <- .create_n_input_df(n_balance)
  pick <- function(l) dplyr::pull(dplyr::filter(out, label == l), data)

  expect_equal(pick("{GREHha}"), 50)
  expect_equal(pick("{GREHN}"), 5)
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

  out <- .create_livestock_surplus_df(df_all_flows)

  expect_equal(unique(out$label), "{LIVGASLOSS}")
  expect_equal(out$data, 50)
  expect_equal(out$align, "R")
})


# .create_cropland_total_df ----------------------------------------------------

test_that(".create_cropland_total_df sums the three cropland-output labels", {
  df_flow <- tibble::tribble(
    ~province, ~year, ~label, ~data, ~align,
    "Huesca", 2000, "{CROP_EXPORT}", 100, "L",
    "Huesca", 2000, "{CROPS_TO_POP}", 40, "L",
    "Huesca", 2000, "{CROPS_TO_LIVESTOCK}", 50, "L",
    "Huesca", 2000, "{LIVESTOCK_TO_HUMAN}", 15, "L",
    "Spain", 2000, "{CROP_EXPORT}", 10, "L"
  )

  out <- .create_cropland_total_df(df_flow)
  pick <- function(prov) {
    dplyr::pull(
      dplyr::filter(out, province == prov, label == "{CRPLNDTOTN}"),
      data
    )
  }

  expect_equal(pick("Huesca"), 190)
  expect_equal(pick("Spain"), 10)
  expect_true(all(out$align == "R"))
  expect_equal(unique(out$label), "{CRPLNDTOTN}")
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
