#' @title Create nitrogen flow dataset by province
#'
#' @description
#' Generates a dataset of nitrogen flows (MgN) by province and year, splitting
#' imports, livestock, and population data into labels.
#'
#' @param prod_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return
#' A tibble with columns `province`, `year`, `label`, `data`, and `align`.
#'
#' @keywords internal
.create_n_flow_df <- function(prod_destiny_df = NULL) {
  if (is.null(prod_destiny_df)) {
    prod_destiny_df <- create_n_prov_destiny()$prod_destiny
  }

  destiny_shares <- prod_destiny_df |>
    dplyr::filter(Destiny %in% c("food", "feed", "other_uses")) |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(
      total_consumption = sum(MgN, na.rm = TRUE),
      food_share = sum(MgN[Destiny == "food"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      feed_share = sum(MgN[Destiny == "feed"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      other_share = sum(MgN[Destiny == "other_uses"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  import_destiny_df <- prod_destiny_df |>
    dplyr::filter(Destiny == "import") |>
    dplyr::left_join(
      destiny_shares,
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      import_food = MgN * food_share,
      import_feed = MgN * feed_share,
      import_other_uses = MgN * other_share
    ) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Box,
      Box_destiny,
      import_food,
      import_feed,
      import_other_uses
    ) |>
    tidyr::pivot_longer(
      cols = c(import_food, import_feed, import_other_uses),
      names_to = "trade_destiny",
      values_to = "MgN",
      values_drop_na = TRUE
    )

  df_n_flows <- import_destiny_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      label = list(c(
        if (
          Item %in%
            c(
              "Milk - Excluding Butter",
              "Milk, lactation",
              "Whey",
              "Butter, Ghee"
            ) &
            trade_destiny == "import_food"
        ) {
          "<IMPHUMMILK>"
        } else {
          NULL
        },
        if (Box == "Cropland" & trade_destiny == "import_food") {
          "<CROP_POPIMPORT>"
        } else {
          NULL
        },
        if (
          Item %in%
            c(
              "Bovine Meat",
              "Mutton & Goat Meat",
              "Pigmeat",
              "Poultry Meat",
              "Meat, Other",
              "Offals, Edible"
            ) &
            trade_destiny == "import_food"
        ) {
          "<IMPHUMANMEAT>"
        } else {
          NULL
        },
        if (Item %in% c("Eggs") & trade_destiny == "import_food") {
          "<IMPHUMANEGGS>"
        } else {
          NULL
        },
        if (
          Item %in%
            c(
              "Demersal Fish",
              "Pelagic Fish",
              "Marine Fish, Other",
              "Freshwater Fish",
              "Cephalopods",
              "Crustaceans",
              "Molluscs, Other",
              "Aquatic Animals, Others"
            ) &
            trade_destiny == "import_food"
        ) {
          "<IMPHUMFISH>"
        } else {
          NULL
        },
        if (trade_destiny == "import_food") "<IMPHMANA>" else NULL,
        if (trade_destiny == "import_feed" & Box == "Cropland") {
          "<IMPORT_ANIMALCR>"
        } else {
          NULL
        },
        if (trade_destiny == "import_other_uses" & Box == "Livestock") {
          "<IMPOTHANIM>"
        } else {
          NULL
        },
        if (trade_destiny == "import_other_uses") "<IMANOT>" else NULL
      ))
    ) |>
    tidyr::unnest(label) |>
    dplyr::filter(!is.na(label)) |>
    dplyr::ungroup() |>
    dplyr::group_by(Province_name, Year, label) |>
    dplyr::summarise(data = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      align = dplyr::case_when(
        label %in%
          c(
            "<CROP_POPIMPORT>",
            "<IMPORT_ANIMALCR>",
            "<IMPORT_ANIMALCR_RUM>",
            "<IMPORT_ANIMALCR_MONOG>",
            "<IMANOT>",
            "<IMANOTR>",
            "<IMANOTM>"
          ) ~
          "R",
        TRUE ~ "L"
      ),
      province = Province_name,
      year = Year
    ) |>
    tidyr::complete(
      Province_name = unique(import_destiny_df$Province_name),
      Year = unique(import_destiny_df$Year),
      label = c(
        "<CROP_POPIMPORT>",
        "<IMPHUMMILK>",
        "<IMPHUMANMEAT>",
        "<IMPHUMANEGGS>",
        "<IMPHUMFISH>",
        "<IMPHMANA>",
        "<IMPORT_ANIMALCR>",
        "<IMPOTHANIM>",
        "<IMANOT>"
      ),
      fill = list(data = 0, align = "L")
    ) |>
    dplyr::select(province, year, label, data, align)

  # --- Split import feed Ruminants / Monogastric ---
  df_feed <- import_destiny_df |>
    dplyr::filter(trade_destiny == "import_feed") |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      data_rum = sum(MgN, na.rm = TRUE) * 0.3,
      data_monog = sum(MgN, na.rm = TRUE) * 0.7,
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = c(data_rum, data_monog),
      names_to = "label_temp",
      values_to = "data"
    ) |>
    dplyr::mutate(
      label = dplyr::case_when(
        label_temp == "data_rum" ~ "<IMPORT_ANIMALCR_RUM>",
        label_temp == "data_monog" ~ "<IMPORT_ANIMALCR_MONOG>"
      ),
      align = "R",
      province = Province_name,
      year = Year
    ) |>
    dplyr::select(province, year, label, data, align)

  # --- Split IMANOT into Ruminants and Monogastric ---
  df_imanot_split <- df_n_flows |>
    dplyr::filter(label == "<IMANOT>")

  if (nrow(df_imanot_split) > 0) {
    df_imanot_split <- df_imanot_split |>
      dplyr::mutate(
        IMANOTR_val = data * 0.3,
        IMANOTM_val = data * 0.7
      ) |>
      dplyr::select(-data) |>
      tidyr::pivot_longer(
        cols = c(IMANOTR_val, IMANOTM_val),
        names_to = "label_temp",
        values_to = "data"
      ) |>
      dplyr::mutate(
        label = dplyr::case_when(
          label_temp == "IMANOTR_val" ~ "<IMANOTR>",
          label_temp == "IMANOTM_val" ~ "<IMANOTM>"
        ),
        align = "R"
      ) |>
      dplyr::select(province, year, label, data, align)
  }

  # --- Population label ---
  df_pop <- whep_read_file("population_yg")

  df_population_label <- df_pop |>
    dplyr::mutate(
      `<POPULATION>` = Pop_Mpeop_yg,
      `<POPULATIONM>` = Pop_Mpeop_yg / 1e6
    ) |>
    dplyr::select(
      province = Province_name,
      year = Year,
      `<POPULATION>`,
      `<POPULATIONM>`
    ) |>
    tidyr::pivot_longer(
      cols = c(`<POPULATION>`, `<POPULATIONM>`),
      names_to = "label",
      values_to = "data",
      names_repair = "unique"
    ) |>
    dplyr::mutate(align = "L")

  # Combine
  df_import_label <- dplyr::bind_rows(
    df_n_flows,
    df_feed,
    df_imanot_split,
    df_population_label
  )

  return(df_import_label)
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
    dplyr::distinct() |>
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

  return(df_lu)
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

  # --- LAND BALANCE ---
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
          "<NPEha>",
          "<NPEMha>",
          "<NPEN>",
          "<ARAha>",
          "<ARAMha>",
          "<ARAN>"
        )
    ) |>
    dplyr::select(province = Province_name, year = Year, label, data)

  df_rest <- n_balance |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      `<GREHha>` = sum(
        Area_ygpit_ha[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      `<GREHMha>` = `<GREHha>` / 1e6,
      `<GREHN>` = sum(
        Prod_MgN[LandUse %in% c("Pasture_Shrubland", "Other")] +
          UsedResidue_MgN[LandUse %in% c("Pasture_Shrubland", "Other")] +
          GrazedWeeds_MgN[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      `<HACULT>` = sum(Area_ygpit_ha[LandUse == "Cropland"], na.rm = TRUE),
      `<KM2_PROVINCE>` = sum(Area_ygpit_ha, na.rm = TRUE) / 100,
      `<OXDEPCROPS>` = sum(Deposition[LandUse == "Cropland"], na.rm = TRUE),
      `<FIXCR>` = sum(BNF[LandUse == "Cropland"], na.rm = TRUE),
      `<LIVESTOCK_TO_CROPS>` = sum(Solid[LandUse == "Cropland"], na.rm = TRUE) +
        sum(Liquid[LandUse == "Cropland"], na.rm = TRUE),
      `<LIVESTOCK_TO_GRASS>` = sum(
        Solid[LandUse == "Pasture_Shrubland"],
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
      `<HAGRASS>` = sum(
        Area_ygpit_ha[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      `<GRASSMha>` = `<HAGRASS>` / 1e6,
      `<KM2GRASS>` = `<HAGRASS>` / 100,
      `<GRASS_SURPLUS>` = sum(
        Surplus[LandUse %in% c("Pasture_Shrubland", "Other")],
        na.rm = TRUE
      ),
      `<WASTEWATER>` = sum(Urban, na.rm = TRUE),
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
            "<KM2GRASS>",
            "<GRASS_SURPLUS>",
            "<WASTEWATER>"
          ),
        "L",
        "R"
      )
    )

  # --- N flows ---
  grafs_prod_destiny_final <- create_n_prov_destiny()$prod_destiny

  destiny_shares <- grafs_prod_destiny_final |>
    dplyr::filter(Destiny %in% c("food", "feed", "other_uses")) |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(
      total_consumption = sum(MgN, na.rm = TRUE),
      food_share = sum(MgN[Destiny == "food"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      feed_share = sum(MgN[Destiny == "feed"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      other_share = sum(MgN[Destiny == "other_uses"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  grafs_prod_destiny_final <- grafs_prod_destiny_final |>
    dplyr::left_join(destiny_shares, by = c("Year", "Province_name", "Item"))

  df_crp_labels <- grafs_prod_destiny_final |>
    dplyr::filter(Box == "Cropland") |>
    dplyr::mutate(
      crp_ls_othuses = (MgN *
        (Destiny == "other_uses") +
        MgN * (Destiny == "export") * other_share -
        MgN * (Destiny == "import") * other_share),
      crp_ls = (MgN *
        (Destiny %in% c("other_uses", "food", "feed")) +
        MgN * (Destiny == "export") -
        MgN * (Destiny == "import"))
    ) |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      `<CRP_LS_OTHUSES>` = sum(crp_ls_othuses, na.rm = TRUE),
      `<CRP_LS>` = sum(crp_ls, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(province = Province_name, year = Year) |>
    tidyr::pivot_longer(
      cols = c(`<CRP_LS_OTHUSES>`, `<CRP_LS>`),
      names_to = "label",
      values_to = "data"
    ) |>
    dplyr::mutate(align = "L") |>
    dplyr::select(province, year, label, data, align)

  df_flows <- grafs_prod_destiny_final |>
    dplyr::mutate(
      CRPNOLV = ifelse(
        Box == "Cropland" & Destiny %in% c("food", "other_uses"),
        "<CRPNOLV>",
        NA_character_
      ),
      CRP_OTHUSES = ifelse(
        Box_destiny == "crops_to_pop" & Destiny == "other_uses",
        "<CRP_OTHUSES>",
        NA_character_
      ),
      CROP_EXPORT = ifelse(
        Box_destiny == "crop_export",
        "<CROP_EXPORT>",
        NA_character_
      ),
      CROPS_TO_POP = ifelse(
        Box_destiny == "crops_to_pop",
        "<CROPS_TO_POP>",
        NA_character_
      ),
      CROPS_TO_LIVESTOCK = ifelse(
        Box_destiny == "crops_to_livestock",
        "<CROPS_TO_LIVESTOCK>",
        NA_character_
      ),
      LIVESTOCK_TO_HUMAN = ifelse(
        Box_destiny == "livestock_to_pop",
        "<LIVESTOCK_TO_HUMAN>",
        NA_character_
      ),
      GRASS_TO_LIVESTOCK = ifelse(
        Box_destiny == "semi_natural_to_livestock",
        "<GRASS_TO_LIVESTOCK>",
        NA_character_
      )
    ) |>
    tidyr::pivot_longer(
      cols = CRPNOLV:GRASS_TO_LIVESTOCK,
      values_to = "label",
      values_drop_na = TRUE
    ) |>
    dplyr::group_by(Province_name, Year, label) |>
    dplyr::summarise(data = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(align = "L") |>
    dplyr::rename(province = Province_name, year = Year) |>
    tidyr::complete(
      province = unique(grafs_prod_destiny_final$Province_name),
      year = unique(grafs_prod_destiny_final$Year),
      label = c(
        "<CRPNOLV>",
        "<CRP_OTHUSES>",
        "<CROP_EXPORT>",
        "<CROPS_TO_POP>",
        "<CROPS_TO_LIVESTOCK>",
        "<LIVESTOCK_TO_HUMAN>",
        "<GRASS_TO_LIVESTOCK>"
      ),
      fill = list(data = 0, align = "L")
    )

  # --- Livestock ---
  livestock_items <- grafs_prod_destiny_final |>
    dplyr::filter(Box == "Livestock") |>
    dplyr::mutate(
      flow_type = dplyr::case_when(
        Destiny %in% c("food", "other_uses", "feed", "export") ~ "output",
        Destiny == "import" ~ "import",
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
    dplyr::mutate(prod = output - import, label = "<LVST_MILK>", align = "L") |>
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

  # Total livestock N
  df_livestock_total <- livestock_items |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      data = sum(
        MgN[Destiny %in% c("food", "feed", "other_uses", "export")],
        na.rm = TRUE
      ) -
        sum(MgN[Destiny == "import"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      label = "<LVSTCKTOTN>",
      align = "L",
      province = Province_name,
      year = Year
    ) |>
    dplyr::select(province, year, label, data, align)

  df_livestock <- dplyr::bind_rows(
    df_livestock,
    df_milk,
    df_livestock_export,
    df_livestock_total
  )

  # --- animal supply ---
  df_animal <- grafs_prod_destiny_final |>
    dplyr::filter(Box == "Livestock") |>
    dplyr::mutate(
      flow_type = dplyr::case_when(
        Destiny %in% c("food", "other_uses", "feed", "export") ~ "output",
        Destiny == "import" ~ "import"
      )
    ) |>
    dplyr::filter(!is.na(flow_type)) |>
    dplyr::group_by(Province_name, Year, flow_type) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = flow_type,
      values_from = MgN,
      values_fill = 0
    ) |>
    dplyr::mutate(prod = output - import) |>
    dplyr::transmute(
      province = Province_name,
      year = Year,
      label = "<AN_LS>",
      data = prod,
      align = "R"
    )

  df_animal_other <- grafs_prod_destiny_final |>
    dplyr::filter(Box == "Livestock", Destiny == "other_uses") |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(data = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "<AN_OTH>",
      align = "R",
      province = Province_name,
      year = Year
    ) |>
    dplyr::select(province, year, label, data, align)

  # --- livestock feed (Ruminants vs Monogastric) ---
  df_lv_r_m <- grafs_prod_destiny_final |>
    dplyr::filter(Box_destiny == "crops_to_livestock") |>
    dplyr::group_by(Province_name, Year) |>
    dplyr::summarise(
      `<CRTOLVSTCK_R>` = sum(MgN, na.rm = TRUE) * 0.3,
      `<CRTOLVSTCK_M>` = sum(MgN, na.rm = TRUE) * 0.7,
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      -c(Province_name, Year),
      names_to = "label",
      values_to = "data"
    ) |>
    dplyr::mutate(align = "L", province = Province_name, year = Year) |>
    dplyr::select(province, year, label, data, align)

  # --- combine and missing labels = 0 ---
  missing_labels <- c(
    "<AN_LS_OTH>",
    "<HORiha>",
    "<HORiMha>",
    "<HORiN>",
    "<HORrha>",
    "<HORrMha>",
    "<HORrN>",
    "<IMPHUMHONEY>",
    "<UNKMANURE>",
    "<LIVGASLOSS>",
    "<NCONTCROP>",
    "<ORGOT>",
    "<PROVINCE_NAME>",
    "<WIDTH_MAX>"
  )

  df_combi <- dplyr::bind_rows(
    df_all,
    df_crp_labels,
    df_flows,
    df_livestock,
    df_animal,
    df_animal_other,
    df_lv_r_m
  ) |>
    dplyr::bind_rows(
      dplyr::distinct(df_all, province, year) |>
        dplyr::mutate(label = "<YEAR>", data = year) |>
        dplyr::select(province, year, label, data)
    ) |>
    tidyr::complete(
      province = unique(province),
      year = unique(year),
      label = unique(c(label, missing_labels)),
      fill = list(data = 0)
    ) |>
    dplyr::mutate(
      align = dplyr::case_when(
        !is.na(align) ~ align,
        label %in%
          c(
            "<AN_LS_OTH>",
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
        TRUE ~ "L"
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
  df_land <- create_land_input_df()
  df_flow <- create_n_flow_df()
  df_lu <- create_livestock_lu_df()

  df_final <- dplyr::bind_rows(df_land, df_flow, df_lu) |>
    dplyr::arrange(province, year, label) |>
    dplyr::filter(!is.na(province) & !is.na(year)) |>
    dplyr::mutate(arrowColor = "")

  openxlsx::write.xlsx(
    df_final,
    file = "C:/PhD/GRAFS/Alfredos_package/GRAFS/inst/extdata/GRAFS_spain_data.xlsx"
  )

  return(df_final)
}
