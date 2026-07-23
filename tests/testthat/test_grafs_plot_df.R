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
