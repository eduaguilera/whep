#' @title Create nitrogen flow dataset by province
#'
#' @description
#' Generates a dataset of nitrogen flows (MgN) by province and year, splitting
#' imports, livestock, and population data into labels.
#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return
#' A tibble with columns `province`, `year`, `label`, `data`, and `align`.
#'
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
          Destiny == "population_food" ~
          "<IMPHUMMILK>",
        Box == "Cropland" & Origin == "Outside" & Destiny == "population_food" ~
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
          Destiny == "population_food" ~
          "<IMPHUMANMEAT>",
        Item == "Eggs" & Origin == "Outside" & Destiny == "population_food" ~
          "<IMPHUMANEGGS>",
        Origin == "Fish" &
          Destiny %in% c("population_food", "population_other_uses") ~
          "<IMPHUMFISH>",
        Box == "Livestock" &
          Origin == "Outside" &
          Destiny == "population_food" ~
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

  return(df_n_flows)
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

#' @title Create land input dataset by province
#'
#' @description
#' Generates a dataset of land use, crop production, grasslands,
#' synthetic fertilizer, and nitrogen inputs by province and year.
#'
#' @return
#' A tibble with columns `province`, `year`, `label`, `data`, and `align`.
#'
#' @keywords internal
.create_land_input_df <- function() {
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
  df_split <- n_balance |>
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
    dplyr::summarise(data = sum(data, na.rm = TRUE), .groups = "drop")

  # --- Land balance for rest and N inputs ---
  df_rest <- n_balance |>
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
    dplyr::select(province = Province_name, year = Year, label, data)

  # --- Combine ---
  df_all <- dplyr::bind_rows(df_split, df_rest) |>
    dplyr::mutate(
      align = dplyr::if_else(
        label %in%
          c(
            "<OXDEPCROPS>",
            "<LIVESTOCK_TO_CROPS>",
            "<OXDEPGRASS>",
            "<SYF_GRASS>",
            "<FIXGR>",
            "<FIX_DEP_GRASS>",
            "<CROP_SURPLUS>",
            "<HAGRASS>",
            "<GRASSMha>",
            "<GRASS_SURPLUS>",
            "<WASTEWATER>",
            "<KM2_PROVINCE>"
          ),
        "L",
        "R"
      )
    )
}

#--- N flows ----------------------------------------------------------------
.create_n_flow_df <- function(prov_destiny_df = NULL) {
  if (is.null(prov_destiny_df)) {
    prov_destiny_df <- create_n_prov_destiny()
  }

  # --- Crop & livestock flows ---
  df_flows <- prov_destiny_df |>
    dplyr::mutate(
      label = dplyr::case_when(
        Origin == "Cropland" & Destiny == "export" ~ "<CROP_EXPORT>",
        Origin == "Cropland" & Destiny == "population_food" ~ "<CROPS_TO_POP>",
        Origin == "Cropland" &
          Destiny %in% c("livestock_rum", "livestock_mono") ~
          "<CROPS_TO_LIVESTOCK>",
        Origin == "Livestock" & Destiny == "population_food" ~
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

  # --- Livestock production ---
  livestock_items <- prov_destiny_df |>
    dplyr::filter(Box == "Livestock") |>
    dplyr::mutate(
      flow_type = dplyr::case_when(
        Destiny %in%
          c(
            "population_food",
            "livestock_mono",
            "livestock_rum",
            "other_uses",
            "export"
          ) ~
          "output",
        Origin == "Outside" ~ "import",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(flow_type))

  df_livestock <- livestock_items |>
    dplyr::mutate(
      group_item = ifelse(
        Item %in% c("Hides and skins", "Wool (Clean Eq.)", "Silk"),
        "non_edible",
        "edible"
      )
    ) |>
    dplyr::group_by(Province_name, Year, group_item, flow_type) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = flow_type,
      values_from = MgN,
      values_fill = 0
    ) |>
    dplyr::mutate(
      prod = output - import,
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
      data = prod,
      align
    )

  df_milk <- livestock_items |>
    dplyr::filter(
      Item %in%
        c(
          "Milk - Excluding Butter",
          "Milk, lactation",
          "Whey",
          "Butter, Ghee"
        )
    ) |>
    dplyr::group_by(Province_name, Year, flow_type) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = flow_type,
      values_from = MgN,
      values_fill = 0
    ) |>
    dplyr::mutate(
      prod = output - import,
      label = "<LVST_MILK>",
      align = "L"
    ) |>
    dplyr::select(
      province = Province_name,
      year = Year,
      label,
      data = prod,
      align
    )

  df_livestock_export <- livestock_items |>
    dplyr::filter(Destiny == "export") |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(data = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "<LIVESTOCK_EXPORTED>",
      align = "L",
      province = Province_name,
      year = Year
    ) |>
    dplyr::select(province, year, label, data, align)

  df_livestock <- dplyr::bind_rows(
    df_livestock,
    df_milk,
    df_livestock_export
  )

  # --- Feed from Cropland ---
  df_lv_r_m <- prov_destiny_df |>
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

  # --- Crop losses ---
  df_crop_losses <- prov_destiny_df |>
    dplyr::mutate(
      crop_inputs = dplyr::case_when(
        Origin %in% c("Fixation", "Synthetic", "Deposition") ~ MgN,
        Origin == "Livestock" ~ MgN,
        Origin == "People" ~ MgN,
        TRUE ~ 0
      ),
      crop_outputs = dplyr::case_when(
        Origin == "Cropland" &
          Destiny %in%
            c(
              "population_food",
              "population_other_uses",
              "livestock_mono",
              "livestock_rum",
              "export"
            ) ~
          MgN,
        Origin == "Outside" & Destiny == "Cropland" ~ -MgN,
        TRUE ~ 0
      ),
      crop_outputs_other = dplyr::case_when(
        Origin == "Cropland" & Destiny == "population_other_uses" ~ MgN,
        Origin == "Outside" & Destiny == "Cropland" ~ -MgN,
        TRUE ~ 0
      )
    ) |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      inputs = sum(crop_inputs, na.rm = TRUE),
      outputs = sum(crop_outputs, na.rm = TRUE),
      outputs_other = sum(crop_outputs_other, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `<CRP_LS>` = inputs - outputs,
      `<CRP_LS_OTHUSES>` = inputs - outputs_other,
      `<CRP_OTHUSES>` = outputs_other
    ) |>
    tidyr::pivot_longer(
      c(`<CRP_LS>`, `<CRP_LS_OTHUSES>`, `<CRP_OTHUSES>`),
      names_to = "label",
      values_to = "data"
    ) |>
    dplyr::mutate(align = "L") |>
    dplyr::rename(province = Province_name, year = Year)

  # --- Animal losses ---
  df_animal_losses <- prov_destiny_df |>
    dplyr::mutate(
      feed_inputs = ifelse(
        Destiny %in% c("livestock_rum", "livestock_mono"),
        MgN,
        0
      ),
      animal_outputs = ifelse(
        Origin == "Livestock" &
          Destiny %in%
            c("population_food", "population_other_uses", "export"),
        MgN,
        ifelse(
          Origin == "Outside" &
            Destiny %in% c("livestock_rum", "livestock_mono"),
          -MgN,
          0
        )
      ),
      animal_outputs_other = ifelse(
        Origin == "Livestock" & Destiny == "population_other_uses",
        MgN,
        0
      )
    ) |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      feed = sum(feed_inputs, na.rm = TRUE),
      outputs = sum(animal_outputs, na.rm = TRUE),
      outputs_other = sum(animal_outputs_other, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `<AN_LS>` = feed - outputs,
      `<AN_OTH>` = outputs_other,
      `<AN_LS_OTH>` = (feed - outputs) + outputs_other
    ) |>
    tidyr::pivot_longer(
      c(`<AN_LS>`, `<AN_OTH>`, `<AN_LS_OTH>`),
      names_to = "label",
      values_to = "data"
    ) |>
    dplyr::mutate(align = as.character("R")) |>
    dplyr::rename(province = Province_name, year = Year)

  df_livestock_total <- dplyr::bind_rows(
    df_flows,
    df_livestock_export,
    df_animal_losses
  ) |>
    dplyr::filter(
      label %in%
        c("<LIVESTOCK_TO_HUMAN>", "<LIVESTOCK_EXPORTED>", "<AN_LS_OTH>")
    ) |>
    dplyr::group_by(province, year) |>
    dplyr::summarise(data = sum(data, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "<LVSTCKTOTN>",
      align = "L"
    )

  # --- Combine ---
  df_combi <- dplyr::bind_rows(
    df_flows |> dplyr::select(province, year, label, data, align),
    df_livestock |> dplyr::select(province, year, label, data, align),
    df_lv_r_m |> dplyr::select(province, year, label, data, align),
    df_crop_losses |> dplyr::select(province, year, label, data, align),
    df_animal_losses |> dplyr::select(province, year, label, data, align),
    df_livestock_total |> dplyr::select(province, year, label, data, align)
  ) |>
    dplyr::arrange(province, year, label)

  # ---Finalize -------------------------------------------------------------
  missing_labels <- c(
    "<HORiha>",
    "<HORiMha>",
    "<HORiN>",
    "<HORrha>",
    "<HORrMha>",
    "<HORrN>",
    "<IMPHUMHONEY>",
    "<UNKMANURE>",
    "<LIVGASLOSS>",
    "<CRPNOLV>",
    "<NCONTCROP>",
    "<ORGOT>",
    "<WIDTH_MAX>"
  )

  df_combi <- df_combi |>
    dplyr::mutate(data = as.character(data)) |>
    tidyr::complete(
      province,
      year,
      label = c(unique(df_combi$label), missing_labels),
      fill = list(data = "0", align = "L")
    ) |>
    dplyr::mutate(
      align = dplyr::case_when(
        label %in% c("<AN_LS>", "<AN_OTH>", "<AN_LS_OTH>") ~ "R",
        label == "<WIDTH_MAX>" ~ "L",
        TRUE ~ align
      ),
      data = as.numeric(data),
      data = ifelse(data < 0, 0, data),
      data = as.character(data)
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
        TRUE ~ as.character(data)
      ),
      align = dplyr::case_when(
        label == "<WIDTH_MAX>" ~ "L",
        label %in%
          c(
            "<HORiha>",
            "<HORiMha>",
            "<HORiN>",
            "<HORrha>",
            "<HORrMha>",
            "<HORrN>",
            "<LIVGASLOSS>",
            "<NCONTCROP>",
            "<ORGOT>"
          ) ~
          "R",
        TRUE ~ align
      )
    ) |>
    dplyr::arrange(province, year, label)

  return(df_combi)
}

#' @title Create GRAFS plot dataset for provinces
#'
#' @description
#' Combines land input data and N flows from crops, livestock, imports, and
#' exports to generate a dataset of nitrogen (MgN) by province and year, to
#' create a GRAFS plot, offered by Alfredo Rodríguez.
#' The data includes labels for different land uses, livestock, animal supply,
#' imports, and population.
#'
#' @return
#' A tibble containing province, year, label, data, and alignment.
#'
#' @export
create_grafs_plot_df <- function() {
  df_land <- .create_land_input_df() |> dplyr::mutate(data = as.character(data))
  df_flow <- .create_n_flow_df() |> dplyr::mutate(data = as.character(data))
  df_import <- .create_n_import_df() |> dplyr::mutate(data = as.character(data))
  df_lu <- .create_livestock_lu_df() |> dplyr::mutate(data = as.character(data))

  df_final <- dplyr::bind_rows(df_land, df_flow, df_import, df_lu) |>
    dplyr::arrange(province, year, label) |>
    dplyr::filter(!is.na(province) & !is.na(year)) |>
    dplyr::mutate(arrowColor = "") |>
    dplyr::select(province, year, label, data, align, arrowColor)

  #openxlsx::write.xlsx(
  #df_final,
  #file = system.file("extdata", "GRAFS_spain_data.xlsx", package = "GRAFS")
  #)

  write_path <- "C:/PhD/GRAFS/GRAFS_spain_data.xlsx"
  openxlsx::write.xlsx(df_final, file = write_path)

  df_final
}
