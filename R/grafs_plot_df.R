#' @title Create GRAFS plot dataset
#'
#' @description
#' Combines land input data and N flows from crops, livestock, imports, and
#' exports to generate a dataset of nitrogen (MgN) by province and year, to
#' create a GRAFS plot, offered by Alfredo Rodríguez.
#'
#' @return
#' A tibble containing province, year, label, data, and alignment.
#'
#' @export
create_grafs_plot_df <- function() {
  prov_destiny_df <- create_n_prov_destiny()
  n_balance <- whep_read_file("n_balance_ygpit_all")

  df_land <- .create_land_df()
  df_flow <- .create_n_flow_df(prov_destiny_df)
  df_import <- .create_n_import_df(prov_destiny_df)
  df_lu <- .create_livestock_lu_df()
  df_n_input <- .create_n_input_df(n_balance, df_land)
  df_livestock <- .create_livestock_df(prov_destiny_df)
  df_lv_r_m <- .create_feed_df(prov_destiny_df)
  df_crop_losses <- .create_crop_losses_df(n_balance, prov_destiny_df)
  df_animal_losses <- .create_animal_losses_df(prov_destiny_df)
  df_livestock_export <- .create_livestock_export_df(prov_destiny_df)
  df_milk <- .create_milk_df(prov_destiny_df)
  df_livestock_total <- .create_livestock_total_df(
    df_flow,
    df_livestock_export,
    df_animal_losses
  )
  df_livestock_gas_loss <- .create_livestock_gas_loss_df()

  df_combined <- .combine_and_finalize_df(
    crop_livestock_flows = df_flow,
    df_livestock = dplyr::bind_rows(df_livestock, df_milk, df_livestock_export),
    df_lv_r_m = df_lv_r_m,
    df_crop_losses = df_crop_losses,
    df_animal_losses = df_animal_losses,
    df_livestock_total = df_livestock_total,
    df_livestock_gas_loss = df_livestock_gas_loss
  )

  df_final <- dplyr::bind_rows(
    df_land |> dplyr::mutate(data = as.character(data)),
    df_combined,
    df_import |> dplyr::mutate(data = as.character(data)),
    df_lu |> dplyr::mutate(data = as.character(data)),
    df_n_input |> dplyr::mutate(data = as.character(data))
  ) |>
    dplyr::arrange(province, year, label) |>
    dplyr::filter(!is.na(province) & !is.na(year)) |>
    dplyr::mutate(arrowColor = "") |>
    dplyr::select(province, year, label, data, align, arrowColor) |>
    dplyr::distinct(province, year, label, .keep_all = TRUE)

  writexl::write_xlsx(
    df_final,
    "C:/PhD/GRAFS_plot/inst/extdata/GRAFS_spain_data.xlsx"
  )

  df_final
}

#' @title Create nitrogen import dataset by province
#'
#' @description
#' Generates a dataset of nitrogen imports (MgN) by province and year, splitting
#' imports, livestock, and population data into labels.
#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return
#' A tibble with columns `province`, `year`, `label`, `data`, and `align`.
#'
#' @keywords internal
.create_n_import_df <- function(prov_destiny_df = NULL) {
  if (is.null(prov_destiny_df)) {
    prov_destiny_df <- create_n_prov_destiny()
  }

  df_n_flows <- prov_destiny_df |>
    dplyr::mutate(
      label = dplyr::case_when(
        Origin == "Agro-industry" & Destiny == "livestock_rum" ~ "<IMANOTR>",
        Origin == "Agro-industry" & Destiny == "livestock_mono" ~ "<IMANOTM>",
        Item %in%
          c(
            "Milk - Excluding Butter",
            "Milk, lactation",
            "Whey",
            "Butter, Ghee"
          ) &
          Origin == "Outside" &
          Destiny %in% c("population_food", "population_other_uses") ~
          "<IMPHUMMILK>",
        Box == "Cropland" &
          Origin == "Outside" &
          Destiny %in% c("population_food", "population_other_uses") ~
          "<CROP_POPIMPORT>",
        Item %in%
          c(
            "Bovine Meat",
            "Mutton & Goat Meat",
            "Pigmeat",
            "Poultry Meat",
            "Meat, Other",
            "Offals, Edible"
          ) &
          Origin == "Outside" &
          Destiny %in% c("population_food", "population_other_uses") ~
          "<IMPHUMANMEAT>",
        Item == "Eggs" &
          Origin == "Outside" &
          Destiny %in% c("population_food", "population_other_uses") ~
          "<IMPHUMANEGGS>",
        Origin == "Fish" &
          Destiny %in% c("population_food", "population_other_uses") ~
          "<IMPHUMFISH>",
        Box == "Livestock" &
          Origin == "Outside" &
          Destiny %in% c("population_food", "population_other_uses") ~
          "<IMPHMANA>",
        Box == "Cropland" &
          Origin == "Outside" &
          Destiny %in% c("livestock_rum", "livestock_mono") ~
          "<IMPORT_ANIMALCR>",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(label)) |>
    dplyr::group_by(Province_name, Year, label) |>
    dplyr::summarise(data = sum(MgN, na.rm = TRUE), .groups = "drop")

  df_imanot <- df_n_flows |>
    dplyr::filter(label %in% c("<IMANOTR>", "<IMANOTM>")) |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(data = sum(data), .groups = "drop") |>
    dplyr::mutate(label = "<IMANOT>")

  df_n_flows <- dplyr::bind_rows(df_n_flows, df_imanot) |>
    dplyr::mutate(
      align = dplyr::case_when(
        label %in%
          c(
            "<CROP_POPIMPORT>",
            "<IMPORT_ANIMALCR>",
            "<IMANOTR>",
            "<IMANOTM>",
            "<IMANOT>"
          ) ~
          "R",
        TRUE ~ "L"
      ),
      province = Province_name,
      year = Year
    ) |>
    dplyr::select(province, year, label, data, align)

  df_n_flows
}


#' @title Create Livestock LU (Livestock Units) dataset
#'
#' @description
#' Calculated livestock units (LU) by province and year for ruminants and
#' monogastric animals.
#' Converts stock numbers into standardized LU values using conversion factors.
#'
#' @return
#' A tibble with columns `province`, `year`, `label`, `data`, and `align`.
#'
#' @keywords internal
.create_livestock_lu_df <- function() {
  livestock_lu <- whep_read_file("livestock_prod_ygps")

  lu_factors <- c(
    Cattle_meat = 0.8,
    Cattle_milk = 1.0,
    Goats = 1.0,
    Sheep = 1.0,
    Horses = 0.8,
    Donkeys_mules = 0.8,
    Rabbits = 0.02,
    Pigs = 0.3,
    Poultry = 0.02
  )

  df_lu <- livestock_lu |>
    dplyr::select(Province_name, Year, Livestock_cat, Stock_Number) |>
    dplyr::filter(Livestock_cat %in% names(lu_factors)) |>
    dplyr::mutate(
      LU = Stock_Number * dplyr::recode(Livestock_cat, !!!lu_factors),
      group = dplyr::case_when(
        Livestock_cat %in%
          c(
            "Cattle_meat",
            "Cattle_milk",
            "Goats",
            "Sheep",
            "Horses",
            "Donkeys_mules"
          ) ~
          "ruminant",
        Livestock_cat %in% c("Rabbits", "Pigs", "Poultry") ~ "monogastric"
      )
    ) |>
    dplyr::group_by(Province_name, Year, group) |>
    dplyr::summarise(LU = sum(LU, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = group,
      values_from = LU,
      values_fill = 0
    ) |>
    dplyr::mutate(
      `<RUMIANTSLU>` = ruminant,
      `<RUMIANTSMLU>` = ruminant / 1e6,
      `<MONOGLU>` = monogastric,
      `<MONOGMLU>` = monogastric / 1e6
    ) |>
    dplyr::select(
      province = Province_name,
      year = Year,
      `<RUMIANTSLU>`,
      `<RUMIANTSMLU>`,
      `<MONOGLU>`,
      `<MONOGMLU>`
    ) |>
    tidyr::pivot_longer(
      cols = c(`<RUMIANTSLU>`, `<RUMIANTSMLU>`, `<MONOGLU>`, `<MONOGMLU>`),
      names_to = "label",
      values_to = "data"
    ) |>
    dplyr::mutate(align = "R")

  df_lu
}

#' @title Create land dataset by province
#'
#' @description
#' Generates a dataset of land use by province and year of cropland (permanent
#' and non permanent), horticulture, and forest area for N and area (ha),
#' separated into irrigated and rainfed.
#'
#' @return
#' A tibble with columns `province`, `year`, `label`, `data`, and `align`.
#'
#' @keywords internal
.create_land_df <- function() {
  n_balance <- whep_read_file("n_balance_ygpit_all")

  permanent_biomass <- c(
    "Apple",
    "Chestnut",
    "Figs",
    "Grapevine",
    "Lemon",
    "Peach",
    "Pear",
    "Pomegranate",
    "Almonds",
    "Apricot",
    "Olive",
    "Plum",
    "Carob",
    "Cherries",
    "Grapefruit",
    "Orange",
    "Walnut",
    "Mandarin",
    "Hazelnut",
    "Crack willow",
    "Bananas, platains",
    "Fruits",
    "Dates",
    "Fruit, tropical fresh nes",
    "Quince",
    "Avocado",
    "Citrus",
    "Nuts nes",
    "Raspberries",
    "Kiwifruit",
    "Berries nes",
    "Pistachios",
    "Persimmon",
    "Blueberries",
    "Holm oak"
  )

  horticulture_biomass <- c(
    "Artichoke thistle",
    "Cabbage, Broccoli",
    "Carrot",
    "Cauliflower",
    "Chard",
    "Garlic",
    "Onion",
    "Pea, green, with pod",
    "Pepper",
    "Tomato",
    "Turnip",
    "Beans, green",
    "Melon",
    "Artichoke",
    "Celery",
    "Endive",
    "Asparagus",
    "Zucchini",
    "Caper",
    "Lettuce",
    "Vegetables, other",
    "Cucumber",
    "Squash, pumpkin",
    "Vegetables, leguminous nes",
    "Watermelon",
    "Aubergine",
    "Leek",
    "Spinach",
    "Beet",
    "Chili pepper",
    "Borage"
  )

  # --- LAND BALANCE FOR CROPLAND AND FOREST ---
  df_land <- n_balance |>
    dplyr::filter(
      LandUse %in% c("Cropland", "Forest_low", "Forest_high", "Dehesa")
    ) |>
    dplyr::group_by(Province_name, Year, Irrig_cat) |>
    dplyr::summarise(
      `<FORha>` = sum(
        Area_ygpit_ha[LandUse %in% c("Forest_low", "Forest_high", "Dehesa")],
        na.rm = TRUE
      ),
      `<FORMha>` = `<FORha>` / 1e6,
      `<FORN>` = sum(
        Prod_MgN[LandUse %in% c("Forest_low", "Forest_high", "Dehesa")] +
          UsedResidue_MgN[
            LandUse %in% c("Forest_low", "Forest_high", "Dehesa")
          ] +
          GrazedWeeds_MgN[
            LandUse %in% c("Forest_low", "Forest_high", "Dehesa")
          ],
        na.rm = TRUE
      ),
      `<PERha>` = sum(
        Area_ygpit_ha[
          LandUse == "Cropland" & Name_biomass %in% permanent_biomass
        ],
        na.rm = TRUE
      ),
      `<PERMha>` = `<PERha>` / 1e6,
      `<PERN>` = sum(
        Prod_MgN[LandUse == "Cropland" & Name_biomass %in% permanent_biomass] +
          UsedResidue_MgN[
            LandUse == "Cropland" & Name_biomass %in% permanent_biomass
          ] +
          GrazedWeeds_MgN[
            LandUse == "Cropland" & Name_biomass %in% permanent_biomass
          ],
        na.rm = TRUE
      ),
      `<HORha>` = sum(
        Area_ygpit_ha[
          LandUse == "Cropland" & Name_biomass %in% horticulture_biomass
        ],
        na.rm = TRUE
      ),
      `<HORMha>` = `<HORha>` / 1e6,
      `<HORN>` = sum(
        Prod_MgN[
          LandUse == "Cropland" & Name_biomass %in% horticulture_biomass
        ] +
          UsedResidue_MgN[
            LandUse == "Cropland" & Name_biomass %in% horticulture_biomass
          ] +
          GrazedWeeds_MgN[
            LandUse == "Cropland" & Name_biomass %in% horticulture_biomass
          ],
        na.rm = TRUE
      ),
      `<NPEha>` = sum(
        Area_ygpit_ha[
          LandUse == "Cropland" & !(Name_biomass %in% permanent_biomass)
        ],
        na.rm = TRUE
      ),
      `<NPEMha>` = `<NPEha>` / 1e6,
      `<NPEN>` = sum(
        Prod_MgN[
          LandUse == "Cropland" & !(Name_biomass %in% permanent_biomass)
        ] +
          UsedResidue_MgN[
            LandUse == "Cropland" & !(Name_biomass %in% permanent_biomass)
          ] +
          GrazedWeeds_MgN[
            LandUse == "Cropland" & !(Name_biomass %in% permanent_biomass)
          ],
        na.rm = TRUE
      ),
      `<ARAha>` = `<NPEha>`,
      `<ARAMha>` = `<ARAha>` / 1e6,
      `<ARAN>` = `<NPEN>`,
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      -c(Province_name, Year, Irrig_cat),
      names_to = "label_base",
      values_to = "data"
    ) |>
    dplyr::mutate(
      label = dplyr::case_when(
        Irrig_cat == "Irrigated" & grepl("^<FOR", label_base) ~
          gsub("<FOR", "<FORi", label_base),
        Irrig_cat == "Rainfed" & grepl("^<FOR", label_base) ~
          gsub("<FOR", "<FORr", label_base),
        Irrig_cat == "Irrigated" & grepl("^<PER", label_base) ~
          gsub("<PER", "<PERi", label_base),
        Irrig_cat == "Rainfed" & grepl("^<PER", label_base) ~
          gsub("<PER", "<PERr", label_base),
        Irrig_cat == "Irrigated" & grepl("^<HOR", label_base) ~
          gsub("<HOR", "<HORi", label_base),
        Irrig_cat == "Rainfed" & grepl("^<HOR", label_base) ~
          gsub("<HOR", "<HORr", label_base),
        Irrig_cat == "Irrigated" & grepl("^<NPE", label_base) ~
          gsub("<NPE", "<NPEi", label_base),
        Irrig_cat == "Rainfed" & grepl("^<NPE", label_base) ~
          gsub("<NPE", "<NPEr", label_base),
        Irrig_cat == "Irrigated" & grepl("^<ARA", label_base) ~
          gsub("<ARA", "<ARAi", label_base),
        Irrig_cat == "Rainfed" & grepl("^<ARA", label_base) ~
          gsub("<ARA", "<ARAr", label_base),
        TRUE ~ label_base
      )
    ) |>
    dplyr::filter(
      !label %in%
        c(
          "<FORha>",
          "<FORMha>",
          "<FORN>",
          "<PERha>",
          "<PERMha>",
          "<PERN>",
          "<HORha>",
          "<HORMha>",
          "<HORN>",
          "<NPEha>",
          "<NPEMha>",
          "<NPEN>",
          "<ARAha>",
          "<ARAMha>",
          "<ARAN>"
        )
    ) |>
    dplyr::select(province = Province_name, year = Year, label, data) |>
    dplyr::group_by(province, year, label) |>
    dplyr::summarise(data = sum(data, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(align = "R")

  df_land
}

#' @title Create dataset for greeonhouse, grassland, and N soil input
#'
#' @description
#' Generates dataset for greenhouse, grasslands, N inputs (manure, deposition,
#'  fixation, surplus, and wastewater).
#' Combines with crops/forest dataset.
#'
#' @return
#' A tibble with columns `province`, `year`, `label`, `data`, and `align`.
#'
#' @keywords internal
.create_n_input_df <- function(n_balance, df_land) {
  df_n_soil_inputs <- n_balance |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      `<GREHha>` = sum(Area_ygpit_ha[Irrig_cat == "Greenhouse"], na.rm = TRUE),
      `<GREHMha>` = `<GREHha>` / 1e6,
      `<GREHN>` = sum(
        Prod_MgN[Irrig_cat == "Greenhouse"] +
          UsedResidue_MgN[Irrig_cat == "Greenhouse"] +
          GrazedWeeds_MgN[Irrig_cat == "Greenhouse"],
        na.rm = TRUE
      ),
      # Is Grassland correctly filtered here with Pasture_Shrubland and Other?
      `<HAGRASS>` = sum(
        Area_ygpit_ha[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      `<GRASSMha>` = `<HAGRASS>` / 1e6,
      `<HACULT>` = sum(Area_ygpit_ha[LandUse == "Cropland"], na.rm = TRUE),
      `<KM2_PROVINCE>` = sum(Area_ygpit_ha, na.rm = TRUE) / 100,
      `<OXDEPCROPS>` = sum(Deposition[LandUse == "Cropland"], na.rm = TRUE),
      `<FIXCR>` = sum(BNF[LandUse == "Cropland"], na.rm = TRUE),
      `<LIVESTOCK_TO_CROPS>` = sum(Solid[LandUse == "Cropland"], na.rm = TRUE) +
        sum(Liquid[LandUse == "Cropland"], na.rm = TRUE),
      `<LIVESTOCK_TO_GRASS>` = sum(
        Solid[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      `<OXDEPGRASS>` = sum(
        Deposition[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      `<SYF_GRASS>` = sum(
        Synthetic[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      # COMMENT: what is the difference between SYNTHF_TOTAL and SYNTHF. Should
      # I filter LandUse == Cropland for SYNTHF?
      `<SYNTHF_TOTAL>` = sum(Synthetic, na.rm = TRUE),
      `<SYNTHF>` = sum(Synthetic, na.rm = TRUE),
      `<FIXGR>` = sum(
        BNF[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      `<FIX_DEP_GRASS>` = sum(
        BNF[LandUse %in% c("Pasture_Shrubland", "Other")] +
          Deposition[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      `<FIX_DEP_CR>` = sum(
        BNF[LandUse == "Cropland"] + Deposition[LandUse == "Cropland"],
        na.rm = TRUE
      ),
      `<CROP_SURPLUS>` = sum(Surplus[LandUse == "Cropland"], na.rm = TRUE),
      `<GRASS_SURPLUS>` = sum(
        Surplus[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      `<WASTEWATER>` = sum(Urban, na.rm = TRUE),
      `<CRPLNDTOTN>` = sum(Prod_MgN[LandUse == "Cropland"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      -c(Province_name, Year),
      names_to = "label",
      values_to = "data"
    ) |>
    dplyr::select(province = Province_name, year = Year, label, data) |>
    dplyr::mutate(
      align = "L"
    )

  # --- Combine ---
  df_all <- dplyr::bind_rows(
    df_land,
    df_n_soil_inputs
  )

  df_all
}

#' @title Create nitrogen flow dataset by province
#'
#' @description
#' Generates nitrogen flow data (MgN) by province and year, representing
#' #' @title Create nitrogen flow dataset by province
#'
#' @description
#' Generates nitrogen flow data (MgN) by province and year, representing
#' exchanges between cropland, livestock, grassland, population, and exports.
#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, and
#' `align`.
#'
#' @keywords internal

#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_n_flow_df <- function(prov_destiny_df = NULL) {
  if (is.null(prov_destiny_df)) {
    prov_destiny_df <- create_n_prov_destiny()
  }

  # --- Crop & livestock flows ---
  crop_livestock_flows <- prov_destiny_df |>
    dplyr::mutate(
      label = dplyr::case_when(
        Origin == "Cropland" & Destiny == "export" ~ "<CROP_EXPORT>",
        Origin == "Cropland" &
          Destiny == "population_food" ~
          "<CROPS_TO_POP>",
        Origin == "Cropland" &
          Destiny %in% c("livestock_rum", "livestock_mono") ~
          "<CROPS_TO_LIVESTOCK>",
        # Do I need to substract Milk from LIVESTOCK_TO_HUMAN, since it is
        # already covered in another label?
        Origin == "Livestock" &
          Destiny == "population_food" ~
          "<LIVESTOCK_TO_HUMAN>",
        Origin == "semi_natural_agroecosystems" &
          Destiny %in% c("livestock_rum", "livestock_mono") ~
          "<GRASS_TO_LIVESTOCK>",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(label)) |>
    dplyr::group_by(Province_name, Year, label) |>
    dplyr::summarise(data = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(align = "L") |>
    dplyr::rename(province = Province_name, year = Year) |>
    tidyr::complete(
      province = unique(prov_destiny_df$Province_name),
      year = unique(prov_destiny_df$Year),
      label = c(
        "<CROP_EXPORT>",
        "<CROPS_TO_POP>",
        "<CROPS_TO_LIVESTOCK>",
        "<LIVESTOCK_TO_HUMAN>",
        "<GRASS_TO_LIVESTOCK>"
      ),
      fill = list(data = 0, align = "L")
    )
  crop_livestock_flows
}

#' @title Create livestock production dataset
#'
#' @description
#' Generates nitrogen production from livestock destined for population
#' (food or other uses) by province and year, distinguishing edible and
#' non-edible products.
#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_livestock_df <- function(prov_destiny_df) {
  df_livestock <- prov_destiny_df |>
    dplyr::filter(
      Destiny %in% c("population_food", "population_other_uses"),
      Origin == "Livestock"
    ) |>
    dplyr::mutate(
      group_item = ifelse(
        Item %in% c("Hides and skins", "Wool (Clean Eq.)", "Silk"),
        "non_edible",
        "edible"
      )
    ) |>
    dplyr::group_by(Province_name, Year, group_item) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = dplyr::case_when(
        group_item == "non_edible" ~ "<LVSTCK_NOEDIBLE>",
        group_item == "edible" ~ "<LV_EDBL>"
      ),
      align = "L"
    ) |>
    dplyr::select(
      province = Province_name,
      year = Year,
      label,
      data = MgN,
      align
    )

  df_livestock
}

#' @title Create milk production dataset
#'
#' @description
#' Generates nitrogen data for milk and dairy products consumed by population.
#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_milk_df <- function(prov_destiny_df) {
  df_milk <- prov_destiny_df |>
    dplyr::filter(
      Origin == "Livestock",
      Destiny == "population_food",
      Item %in%
        c("Milk - Excluding Butter", "Milk, lactation", "Whey", "Butter, Ghee")
    ) |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "<LVST_MILK>",
      align = "L"
    ) |>
    dplyr::select(
      province = Province_name,
      year = Year,
      label,
      data = MgN,
      align
    )

  df_milk
}

#' @title Create livestock export dataset
#'
#' @description
#' Generates nitrogen flows associated with exported livestock products.
#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_livestock_export_df <- function(prov_destiny_df) {
  df_livestock_export <- prov_destiny_df |>
    dplyr::filter(
      Destiny == "export",
      Origin == "Livestock"
    ) |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(data = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "<LIVESTOCK_EXPORTED>",
      align = "L",
      province = Province_name,
      year = Year
    ) |>
    dplyr::select(province, year, label, data, align)

  df_livestock_export
}

#' @title Create feed from cropland dataset
#'
#' @description
#' Creates nitrogen data representing feed transfers from cropland to
#' ruminant and monogastric livestock.
#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_feed_df <- function(prov_destiny_df) {
  df_feed <- prov_destiny_df |>
    dplyr::filter(
      Origin == "Cropland",
      Destiny %in% c("livestock_rum", "livestock_mono")
    ) |>
    dplyr::group_by(Province_name, Year, Destiny) |>
    dplyr::summarise(
      data = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      label = dplyr::case_when(
        Destiny == "livestock_rum" ~ "<CRTOLVSTCK_R>",
        Destiny == "livestock_mono" ~ "<CRTOLVSTCK_M>"
      ),
      align = "L"
    ) |>
    dplyr::rename(province = Province_name, year = Year) |>
    dplyr::select(province, year, label, data, align)

  df_feed
}

#' @title Create crop losses dataset
#'
#' @description
#' Generates nitrogen loss data from croplands, including gaseous losses (NH₃,
#' N₂O, denitrification) and other uses.
#'
#' @param n_balance A data frame from the nitrogen balance dataset, including
#' N losses from cropland.
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_crop_losses_df <- function(n_balance, prov_destiny_df) {
  df_crop_gas <- n_balance |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      `<CRP_LS>` = sum(Denitrif_MgN + NH3_MgN + N2O_MgN, na.rm = TRUE),
      .groups = "drop"
    )

  df_crop_oth <- prov_destiny_df |>
    dplyr::filter(Origin == "Cropland", Destiny == "population_other_uses") |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      `<CRP_OTHUSES>` = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  df_crop_losses <- dplyr::left_join(
    df_crop_gas,
    df_crop_oth,
    by = c("Province_name", "Year")
  ) |>
    dplyr::mutate(
      `<CRP_OTHUSES>` = ifelse(is.na(`<CRP_OTHUSES>`), 0, `<CRP_OTHUSES>`),
      `<CRP_LS_OTHUSES>` = `<CRP_LS>` + `<CRP_OTHUSES>`
    ) |>
    tidyr::pivot_longer(
      c(`<CRP_LS>`, `<CRP_OTHUSES>`, `<CRP_LS_OTHUSES>`),
      names_to = "label",
      values_to = "data"
    ) |>
    dplyr::mutate(align = "L") |>
    dplyr::rename(province = Province_name, year = Year)
}

#' @title Create animal losses dataset
#'
#' @description
#' Generates nitrogen loss data from livestock, including metabolic losses and
#' livestock products used for other uses.
#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_animal_losses_df <- function(prov_destiny_df) {
  n_excretion <- whep_read_file("n_excretion_ygs") |>
    dplyr::select(
      Year,
      Province_name,
      Livestock_cat,
      Gross_Prod_GgN,
      Net_Prod_GgN
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      `<AN_LS>` = (Gross_Prod_GgN - Net_Prod_GgN) * 1e3
    ) |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      `<AN_LS>` = sum(`<AN_LS>`, na.rm = TRUE),
      .groups = "drop"
    )

  an_oth <- prov_destiny_df |>
    dplyr::filter(
      Origin == "Livestock",
      Destiny == "population_other_uses"
    ) |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      `<AN_OTH>` = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  df_animal_losses <- n_excretion |>
    dplyr::left_join(an_oth, by = c("Province_name", "Year")) |>
    dplyr::mutate(
      `<AN_OTH>` = ifelse(is.na(`<AN_OTH>`), 0, `<AN_OTH>`),
      `<AN_LS_OTH>` = `<AN_LS>` + `<AN_OTH>`
    ) |>
    tidyr::pivot_longer(
      cols = c(`<AN_LS>`, `<AN_OTH>`, `<AN_LS_OTH>`),
      names_to = "label",
      values_to = "data"
    ) |>
    dplyr::mutate(
      align = "R"
    ) |>
    dplyr::rename(
      province = Province_name,
      year = Year
    )

  df_animal_losses
}

#' @title Create combined livestock nitrogen dataset
#'
#' @description
#' Combines nitrogen data from livestock destined for humans, exports, and
#' losses to generate combined nitrogen output from livestock.
#'
#' @param crop_livestock_flows Data frame with livestock-to-human nitrogen data.
#' @param df_livestock_export Data frame with livestock export nitrogen data.
#' @param df_animal_losses Data frame with livestock loss nitrogen data.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_livestock_total_df <- function(
  crop_livestock_flows,
  df_livestock_export,
  df_animal_losses
) {
  df_livestock_total <- dplyr::bind_rows(
    crop_livestock_flows,
    df_livestock_export,
    df_animal_losses
  ) |>
    dplyr::filter(
      label %in%
        c("<LIVESTOCK_TO_HUMAN>", "<LIVESTOCK_EXPORTED>", "<AN_OTH>")
    ) |>
    dplyr::group_by(province, year) |>
    dplyr::summarise(data = sum(data, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "<LVSTCKTOTN>",
      align = "L"
    )

  df_livestock_total
}

#' @title Create livestock gaseous loss dataset
#'
#' @description
#' Calculates gaseous nitrogen losses from livestock excretion based on
#' excretion and loss share data.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_livestock_gas_loss_df <- function() {
  df_livestock_gas_loss <- whep_read_file("n_excretion_ygs") |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      data = sum(Excr_GgN * Loss_share * 1e3, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      province = Province_name,
      year = Year,
      label = "<LIVGASLOSS>",
      align = "R"
    ) |>
    dplyr::select(province, year, label, data, align)

  df_livestock_gas_loss
}

#' @title Combine and finalize nitrogen flow dataset
#'
#' @description
#' Merges all the created nitrogen datasets into a unified structure.
#' Adding missing labels and setting WIDTH_MAX to 1500. IMPHUMHONEY should be 0.
#' The other labels ("<UNKMANURE>", "<CRPNOLV>", "<NCONTCROP>","<ORGOT>") are
#' set to 0, since I don't know how to create them yet.
#'
#' @param crop_livestock_flows Data frame of crop-livestock nitrogen flows.
#' @param df_livestock Data frame of livestock nitrogen data.
#' @param df_lv_r_m Data frame of livestock feed data.
#' @param df_crop_losses Data frame of crop nitrogen losses.
#' @param df_animal_losses Data frame of animal nitrogen losses.
#' @param df_livestock_total Data frame of total livestock nitrogen.
#' @param df_livestock_gas_loss Data frame of livestock gaseous nitrogen losses.
#'
#' @return A tibble with standardized columns `province`, `year`, `label`,
#' `data`, and `align`.
#'
#' @keywords internal
.combine_and_finalize_df <- function(
  crop_livestock_flows,
  df_livestock,
  df_lv_r_m,
  df_crop_losses,
  df_animal_losses,
  df_livestock_total,
  df_livestock_gas_loss
) {
  df_combi <- dplyr::bind_rows(
    crop_livestock_flows |> dplyr::select(province, year, label, data, align),
    df_livestock |> dplyr::select(province, year, label, data, align),
    df_lv_r_m |> dplyr::select(province, year, label, data, align),
    df_crop_losses |> dplyr::select(province, year, label, data, align),
    df_animal_losses |> dplyr::select(province, year, label, data, align),
    df_livestock_total |> dplyr::select(province, year, label, data, align),
    df_livestock_gas_loss |> dplyr::select(province, year, label, data, align)
  ) |>
    dplyr::arrange(province, year, label) |>
    dplyr::mutate(
      data = as.character(data),
      align = as.character(align)
    )

  missing_labels <- c(
    "<IMPHUMHONEY>",
    "<UNKMANURE>",
    "<CRPNOLV>",
    "<NCONTCROP>",
    "<ORGOT>",
    "<WIDTH_MAX>"
  )

  df_combi <- df_combi |>
    tidyr::complete(
      province,
      year,
      label = c(unique(df_combi$label), missing_labels),
      fill = list(data = "0", align = "L")
    ) |>
    dplyr::mutate(
      align = dplyr::case_when(
        label == "<WIDTH_MAX>" ~ "L",
        label %in% c("<NCONTCROP>", "<ORGOT>") ~ "R",
        TRUE ~ align
      )
    ) |>
    dplyr::bind_rows(
      dplyr::distinct(df_combi, province, year) |>
        dplyr::mutate(
          label = "<YEAR>",
          data = as.character(year),
          align = "L"
        ) |>
        dplyr::select(province, year, label, data, align),
      dplyr::distinct(df_combi, province, year) |>
        dplyr::mutate(
          label = "<PROVINCE_NAME>",
          data = as.character(province),
          align = "L"
        ) |>
        dplyr::select(province, year, label, data, align)
    ) |>
    dplyr::mutate(
      data = dplyr::case_when(
        label == "<WIDTH_MAX>" ~ "1500",
        TRUE ~ data
      ),
      align = dplyr::case_when(
        label == "<WIDTH_MAX>" ~ "L",
        label %in%
          c(
            "<NCONTCROP>",
            "<ORGOT>"
          ) ~
          "R",
        TRUE ~ align
      )
    ) |>
    dplyr::arrange(province, year, label)

  df_combi
}
