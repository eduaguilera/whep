#' @title Create GRAFS plot dataset
#'
#' @description
#' Combines land input data and N flows from crops, livestock, imports, and
#' exports to generate a dataset of nitrogen (mg_n) by province and year, to
#' create a GRAFS plot, offered by Alfredo Rodríguez.
#'
#' @return
#' A tibble containing province, year, label, data, and alignment.
#'
#' @export
create_grafs_plot_df <- function() {
  prov_destiny_df <- create_n_prov_destiny()
  nat_destiny_df <- create_n_nat_destiny()

  nat_destiny_df <- nat_destiny_df |>
    dplyr::mutate(province_name = "Spain")

  prov_destiny_df <- prov_destiny_df |>
    dplyr::filter(province_name != "Spain") |>
    dplyr::bind_rows(nat_destiny_df)

  n_balance <- whep_read_file("n_balance_ygpit_all") |>
    dplyr::rename_with(tolower)

  df_land <- .create_land_df()
  df_flow <- .create_n_flow_df(prov_destiny_df)
  df_import <- .create_n_import_df(prov_destiny_df)
  df_lu <- .create_livestock_lu_df()
  df_population <- .create_population_df()
  df_n_input <- .create_n_input_df(n_balance)
  df_land_surplus <- .create_land_surplus_df(prov_destiny_df)
  df_livestock <- .create_livestock_df(prov_destiny_df)
  df_lv_r_m <- .create_feed_df(prov_destiny_df)
  df_crop_losses <- .create_crop_losses_df(n_balance, prov_destiny_df)
  df_animal_losses <- .create_animal_losses_df(prov_destiny_df)
  df_livestock_export <- .create_livestock_export_df(prov_destiny_df)
  df_milk <- .create_milk_df(prov_destiny_df)
  df_livestock_total <- .create_livestock_total_df(prov_destiny_df)

  df_crplndtot <- df_flow |>
    dplyr::filter(
      province != "Spain",
      label %in%
        c(
          "{CROP_EXPORT}",
          "{CROPS_TO_POP}",
          "{CROPS_TO_LIVESTOCK}"
        )
    ) |>
    dplyr::mutate(data = suppressWarnings(as.numeric(data))) |>
    dplyr::group_by(province, year) |>
    dplyr::summarise(
      data = sum(data, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      label = "{CRPLNDTOTN}",
      align = "R"
    )

  df_crplndtot_spain <- df_flow |>
    dplyr::filter(
      province == "Spain",
      label %in% c("{CROP_EXPORT}", "{CROPS_TO_POP}", "{CROPS_TO_LIVESTOCK}")
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(data = sum(data, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(province = "Spain", label = "{CRPLNDTOTN}", align = "R")

  df_crplndtot <- dplyr::bind_rows(
    df_crplndtot,
    df_crplndtot_spain
  )

  df_all_flows <- dplyr::bind_rows(
    df_flow,
    df_import,
    df_livestock,
    df_milk,
    df_livestock_export,
    df_lv_r_m,
    df_crop_losses,
    df_animal_losses
  )

  df_livestock_surplus <- .create_livestock_surplus_df(df_all_flows)
  df_wastewater_surplus <- .create_wastewater_surplus_df(
    df_all_flows,
    prov_destiny_df
  )

  df_combined <- .combine_and_finalize_df(
    crop_livestock_flows = df_flow,
    df_livestock = dplyr::bind_rows(df_livestock, df_milk, df_livestock_export),
    df_lv_r_m = df_lv_r_m,
    df_crop_losses = df_crop_losses,
    df_animal_losses = df_animal_losses,
    df_livestock_total = df_livestock_total,
    df_livestock_surplus = df_livestock_surplus,
    df_land_surplus = df_land_surplus
  )

  df_final <- dplyr::bind_rows(
    df_land |> dplyr::mutate(data = as.character(data)),
    df_combined,
    df_import |> dplyr::mutate(data = as.character(data)),
    df_lu |> dplyr::mutate(data = as.character(data)),
    df_n_input |> dplyr::mutate(data = as.character(data)),
    df_population |> dplyr::mutate(data = as.character(data)),
    df_crplndtot |> dplyr::mutate(data = as.character(data)),
    df_wastewater_surplus |> dplyr::mutate(data = as.character(data))
  ) |>
    dplyr::arrange(province, year, label) |>
    dplyr::filter(!is.na(province) & !is.na(year)) |>
    dplyr::mutate(arrowColor = "") |>
    dplyr::select(province, year, label, data, align, arrowColor)

  n_labels <- c(
    "{IMANOT}",
    "{IMANOTR}",
    "{IMANOTM}",
    "{IMPHUMANMEAT}",
    "{IMPHUMANEGGS}",
    "{IMPHUMFISH}",
    "{IMPHUMMILK}",
    "{IMPORT_ANIMALCR}",
    "{IMPORT_ANIMALCR_RUM}",
    "{IMPORT_ANIMALCR_MONOG}",
    "{CROP_POPIMPORT}",
    "{IMPHMANA}",
    "{CROP_EXPORT}",
    "{CROPS_TO_POP}",
    "{CROPS_TO_LIVESTOCK}",
    "{LIVESTOCK_TO_HUMAN}",
    "{GRASS_TO_LIVESTOCK}",
    "{RCRTOLVSTCK_R}",
    "{MCRTOLVSTCK_M}",
    "{CRP_OTHUSES}",
    "{AN_LS}",
    "{AN_OTH}",
    "{AN_LS_OTH}",
    "{LV_EDBL}",
    "{LVSTCK_NOEDIBLE}",
    "{LVST_MILK}",
    "{LIVESTOCK_EXPORTED}",
    "{LVSTCKTOTN}",
    "{OXDEPCROPS}",
    "{FIXCR}",
    "{LIVESTOCK_TO_CROPS}",
    "{LIVESTOCK_TO_GRASS}",
    "{OXDEPGRASS}",
    "{SYF_GRASS}",
    "{SYNTHF_TOTAL}",
    "{SYNTHF}",
    "{FIXGR}",
    "{FIX_DEP_GRASS}",
    "{FIX_DEP_CR}",
    "{CROP_SURPLUS}",
    "{GRASS_SURPLUS}",
    "{LIVGASLOSS}",
    "{WASTEWATER}",
    "{ORGOT}",
    "{CRPLNDTOTN}",
    "{GREHN}",
    "{FORN}",
    "{PERiN}",
    "{PERrN}",
    "{HORiN}",
    "{HORrN}",
    "{NPEiN}",
    "{NPErN}",
    "{ARAiN}",
    "{ARArN}"
  )

  df_final <- df_final |>
    dplyr::mutate(
      data = suppressWarnings(
        ifelse(
          label %in% n_labels,
          as.numeric(data) / 1000,
          as.numeric(data)
        )
      ),
      data = as.character(data)
    )

  missing_labels <- c(
    "{FORha}",
    "{FORMha}",
    "{FORN}",
    "{HAGRASS}",
    "{HACULT}",
    "{PERiN}",
    "{PERrN}",
    "{NPEiN}",
    "{NPErN}",
    "{GREHN}",
    "{GREHMha}",
    "{POPULATIONM}",
    "{PERiMha}",
    "{PERrMha}",
    "{NPEiMha}",
    "{NPErMha}",
    "{RUMIANTSMLU}",
    "{MONOGMLU}",
    "{GRASSMha}",
    "{HORiN}",
    "{HORrN}",
    "{ARAiN}",
    "{ARArN}",
    "{KM2_PROVINCE}",
    "{AN_LS}",
    "{AN_OTH}",
    "{AN_LS_OTH}"
  )

  df_missing_spain <- df_final |>
    dplyr::filter(province != "Spain") |>
    dplyr::filter(label %in% missing_labels) |>
    dplyr::mutate(data = as.numeric(data)) |>
    dplyr::group_by(year, label) |>
    dplyr::summarise(data = sum(data, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      province = "Spain",
      align = "R",
      data = as.character(data)
    )

  df_final <- dplyr::bind_rows(
    df_final,
    df_missing_spain
  )

  non_additive_labels <- c(
    "{YEAR}",
    "{PROVINCE_NAME}",
    "{WIDTH_MAX}",
    "{FORha}",
    "{FORMha}",
    "{HAGRASS}",
    "{HACULT}",
    "{GREHha}",
    "{GREHMha}",
    "{PERiha}",
    "{PERrha}",
    "{HORiha}",
    "{HORrha}",
    "{NPEiha}",
    "{NPErha}",
    "{ARAiha}",
    "{ARArha}",
    "{PERiMha}",
    "{PERrMha}",
    "{HORiMha}",
    "{HORrMha}",
    "{NPEiMha}",
    "{NPErMha}",
    "{ARAiMha}",
    "{ARArMha}",
    "{RUMIANTSLU}",
    "{RUMIANTSMLU}",
    "{MONOGLU}",
    "{MONOGMLU}",
    "{POPULATIONM}",
    "{KM2_PROVINCE}"
  )

  df_final <- df_final |>
    dplyr::mutate(data_num = suppressWarnings(as.numeric(data))) |>
    dplyr::group_by(province, year, label) |>
    dplyr::summarise(
      data = dplyr::case_when(
        label %in% non_additive_labels & any(!is.na(data_num)) ~
          as.character(dplyr::first(data_num[!is.na(data_num)])),
        label %in% non_additive_labels ~ dplyr::first(data),
        all(is.na(data_num)) ~ dplyr::first(data),
        TRUE ~ as.character(sum(data_num, na.rm = TRUE))
      ),
      align = dplyr::first(align),
      arrowColor = "",
      .groups = "drop"
    )

  #Path needs to be adjusted by user, until the final version can be uploaded
  #to SACO
  readr::write_csv(
    df_final,
    "C:/PhD/GRAFS_plot/inst/extdata/GRAFS_spain_data.csv"
  )

  df_final
}

#' @title Create nitrogen import dataset by province
#'
#' @description
#' Generates a dataset of nitrogen flows (mg_n) by province and year, including
#' N soil inputs, production data, imports.
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

  item_box_lookup <- whep_read_file("codes_coefs_items_full") |>
    dplyr::select(item, group) |>
    dplyr::filter(!is.na(item), !is.na(group)) |>
    dplyr::distinct() |>
    dplyr::group_by(item) |>
    dplyr::summarise(group = dplyr::first(group), .groups = "drop")

  df_n_flows <- prov_destiny_df |>
    dplyr::left_join(item_box_lookup, by = c("item" = "item")) |>
    dplyr::mutate(
      box_filled = dplyr::case_when(
        item %in% c("Holm oak", "Average wood") ~
          "semi_natural_agroecosystems",
        item == "Fallow" ~ "Cropland",
        group %in% c("Crop products", "Primary crops", "crop residue") ~
          "Cropland",
        group %in% c("Livestock products", "Livestock") ~ "Livestock",
        group %in% c("Agro-industry", "Fish") ~ group,
        !is.na(box) ~ box,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::mutate(
      label = dplyr::case_when(
        box_filled == "Agro-industry" &
          origin == "Outside" &
          destiny == "livestock_rum" ~
          "{IMANOTR}",
        box_filled == "Agro-industry" &
          origin == "Outside" &
          destiny == "livestock_mono" ~
          "{IMANOTM}",
        item %in%
          c(
            "Milk - Excluding Butter",
            "Milk, lactation",
            "Whey",
            "Butter, Ghee"
          ) &
          origin == "Outside" &
          destiny %in% c("population_food", "population_other_uses") ~
          "{IMPHUMMILK}",
        item %in%
          c(
            "Bovine Meat",
            "Mutton & Goat Meat",
            "Pigmeat",
            "Poultry Meat",
            "Meat, Other",
            "Offals, Edible"
          ) &
          origin == "Outside" &
          destiny %in% c("population_food", "population_other_uses") ~
          "{IMPHUMANMEAT}",
        item == "Eggs" &
          origin == "Outside" &
          destiny %in% c("population_food", "population_other_uses") ~
          "{IMPHUMANEGGS}",
        box_filled == "Fish" &
          origin == "Outside" &
          destiny %in% c("population_food", "population_other_uses") ~
          "{IMPHUMFISH}",
        box_filled != "Agro-industry" &
          origin == "Outside" &
          destiny == "livestock_rum" ~
          "{IMPORT_ANIMALCR_RUM}",
        box_filled != "Agro-industry" &
          origin == "Outside" &
          destiny == "livestock_mono" ~
          "{IMPORT_ANIMALCR_MONOG}",
        origin == "Livestock" &
          destiny == "Cropland" ~
          "{LIVESTOCK_TO_CROPS}",
        origin == "Livestock" &
          destiny == "semi_natural_agroecosystems" ~
          "{LIVESTOCK_TO_GRASS}",
        origin == "Deposition" &
          destiny == "Cropland" ~
          "{OXDEPCROPS}",
        origin == "Fixation" &
          destiny == "Cropland" ~
          "{FIXCR}",
        origin == "Synthetic" &
          destiny == "Cropland" ~
          "{SYNTHF}",
        origin == "Deposition" &
          destiny == "semi_natural_agroecosystems" ~
          "{OXDEPGRASS}",
        origin == "Fixation" &
          destiny == "semi_natural_agroecosystems" ~
          "{FIXGR}",
        origin == "Synthetic" &
          destiny == "semi_natural_agroecosystems" ~
          "{SYF_GRASS}",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(label)) |>
    dplyr::group_by(province_name, year, label) |>
    dplyr::summarise(data = sum(mg_n, na.rm = TRUE), .groups = "drop")

  df_crop_popimport <- prov_destiny_df |>
    dplyr::filter(
      box == "Cropland",
      origin == "Outside",
      destiny %in% c("population_food", "population_other_uses")
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(data = sum(mg_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "{CROP_POPIMPORT}",
      province = province_name,
      year = year,
      align = "R"
    ) |>
    dplyr::select(province, year, label, data, align)

  df_fix_dep_cr <- df_n_flows |>
    dplyr::filter(label %in% c("{OXDEPCROPS}", "{FIXCR}")) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(data = sum(data), .groups = "drop") |>
    dplyr::mutate(label = "{FIX_DEP_CR}")

  df_fix_dep_grass <- df_n_flows |>
    dplyr::filter(label %in% c("{OXDEPGRASS}", "{FIXGR}")) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(data = sum(data), .groups = "drop") |>
    dplyr::mutate(label = "{FIX_DEP_GRASS}")

  df_import_animalcr <- df_n_flows |>
    dplyr::filter(
      label %in% c("{IMPORT_ANIMALCR_RUM}", "{IMPORT_ANIMALCR_MONOG}")
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(data = sum(data), .groups = "drop") |>
    dplyr::mutate(label = "{IMPORT_ANIMALCR}")

  df_synth_total <- df_n_flows |>
    dplyr::filter(label == "{SYNTHF}") |>
    dplyr::mutate(label = "{SYNTHF_TOTAL}")

  df_imanot <- df_n_flows |>
    dplyr::filter(label %in% c("{IMANOTR}", "{IMANOTM}")) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(data = sum(data), .groups = "drop") |>
    dplyr::mutate(label = "{IMANOT}")

  df_n_flows <- dplyr::bind_rows(
    df_n_flows,
    df_fix_dep_cr,
    df_fix_dep_grass,
    df_import_animalcr,
    df_synth_total,
    df_imanot
  ) |>
    dplyr::mutate(
      province = province_name,
      year = year,
      align = "L"
    ) |>
    dplyr::select(province, year, label, data, align)

  right_labels <- c(
    "{CROP_POPIMPORT}",
    "{IMPORT_ANIMALCR_RUM}",
    "{IMPORT_ANIMALCR_MONOG}",
    "{IMPORT_ANIMALCR}",
    "{IMANOTR}",
    "{IMANOTM}",
    "{IMANOT}"
  )

  df_imphmana <- prov_destiny_df |>
    dplyr::filter(
      origin == "Outside",
      destiny %in% c("population_food", "population_other_uses"),
      box %in% c("Livestock", "Fish")
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(data = sum(mg_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "{IMPHMANA}",
      province = province_name,
      year = year,
      align = "L"
    ) |>
    dplyr::select(province, year, label, data, align)

  df_n_flows <- dplyr::bind_rows(
    df_n_flows,
    df_imphmana,
    df_crop_popimport
  )

  df_n_flows <- df_n_flows |>
    tidyr::complete(
      province,
      year,
      label,
      fill = list(data = 0, align = "L")
    ) |>
    dplyr::mutate(
      align = dplyr::case_when(
        label %in% right_labels ~ "R",
        TRUE ~ "L"
      )
    )

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
  livestock_lu <- whep_read_file("livestock_prod_ygps") |>
    dplyr::rename_with(tolower)
  lu_lookup <- whep_read_file("livestock_units") |>
    dplyr::rename_with(tolower)

  df_lu <- livestock_lu |>
    dplyr::select(
      province_name,
      year,
      livestock_cat,
      stock_number
    ) |>
    dplyr::distinct() |>
    dplyr::left_join(
      lu_lookup,
      by = "livestock_cat"
    ) |>
    dplyr::filter(!is.na(lu_head), system %in% c("ruminant", "monogastric")) |>
    dplyr::mutate(
      LU = stock_number * lu_head
    ) |>
    dplyr::group_by(province_name, year, system) |>
    dplyr::summarise(LU = sum(LU, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = system,
      values_from = LU,
      values_fill = 0
    ) |>
    dplyr::mutate(
      `{RUMIANTSLU}` = ruminant,
      `{RUMIANTSMLU}` = ruminant / 1e6,
      `{MONOGLU}` = monogastric,
      `{MONOGMLU}` = monogastric / 1e6
    ) |>
    dplyr::select(
      province = province_name,
      year = year,
      `{RUMIANTSLU}`,
      `{RUMIANTSMLU}`,
      `{MONOGLU}`,
      `{MONOGMLU}`
    ) |>
    tidyr::pivot_longer(
      cols = -c(province, year),
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
  n_balance <- whep_read_file("n_balance_ygpit_all") |>
    dplyr::rename_with(tolower)
  crop_lookup <- whep_read_file("grafs_crop_categories") |>
    dplyr::rename_with(tolower)

  permanent_biomass <- crop_lookup |>
    dplyr::filter(crop_type == "permanent") |>
    dplyr::pull(name_biomass)

  horticulture_biomass <- crop_lookup |>
    dplyr::filter(crop_type == "horticulture") |>
    dplyr::pull(name_biomass)

  non_permanent_biomass <- crop_lookup |>
    dplyr::filter(crop_type == "non_permanent") |>
    dplyr::pull(name_biomass)

  df_land <- n_balance |>
    dplyr::filter(
      landuse %in% c("Cropland", "Forest_low", "Forest_high", "Dehesa")
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(
      FORha = sum(
        area_ygpit_ha[landuse %in% c("Forest_low", "Forest_high", "Dehesa")],
        na.rm = TRUE
      ),
      FORMha = FORha / 1e6,
      FORN = sum(
        (prod_mgn + usedresidue_mgn + grazedweeds_mgn)[
          landuse %in% c("Forest_low", "Forest_high", "Dehesa")
        ],
        na.rm = TRUE
      ),

      PERiha = sum(
        area_ygpit_ha[
          landuse == "Cropland" &
            name_biomass %in% permanent_biomass &
            irrig_cat == "Irrigated"
        ],
        na.rm = TRUE
      ),
      PERrha = sum(
        area_ygpit_ha[
          landuse == "Cropland" &
            name_biomass %in% permanent_biomass &
            irrig_cat == "Rainfed"
        ],
        na.rm = TRUE
      ),
      PERiMha = PERiha / 1e6,
      PERrMha = PERrha / 1e6,
      PERiN = sum(
        (prod_mgn + usedresidue_mgn + grazedweeds_mgn)[
          landuse == "Cropland" &
            name_biomass %in% permanent_biomass &
            irrig_cat == "Irrigated"
        ],
        na.rm = TRUE
      ),
      PERrN = sum(
        (prod_mgn + usedresidue_mgn + grazedweeds_mgn)[
          landuse == "Cropland" &
            name_biomass %in% permanent_biomass &
            irrig_cat == "Rainfed"
        ],
        na.rm = TRUE
      ),

      HORiha = sum(
        area_ygpit_ha[
          landuse == "Cropland" &
            name_biomass %in% horticulture_biomass &
            irrig_cat == "Irrigated"
        ],
        na.rm = TRUE
      ),
      HORrha = sum(
        area_ygpit_ha[
          landuse == "Cropland" &
            name_biomass %in% horticulture_biomass &
            irrig_cat == "Rainfed"
        ],
        na.rm = TRUE
      ),
      HORiMha = HORiha / 1e6,
      HORrMha = HORrha / 1e6,
      HORiN = sum(
        (prod_mgn + usedresidue_mgn + grazedweeds_mgn)[
          landuse == "Cropland" &
            name_biomass %in% horticulture_biomass &
            irrig_cat == "Irrigated"
        ],
        na.rm = TRUE
      ),
      HORrN = sum(
        (prod_mgn + usedresidue_mgn + grazedweeds_mgn)[
          landuse == "Cropland" &
            name_biomass %in% horticulture_biomass &
            irrig_cat == "Rainfed"
        ],
        na.rm = TRUE
      ),

      NPEiha = sum(
        area_ygpit_ha[
          landuse == "Cropland" &
            name_biomass %in% non_permanent_biomass &
            irrig_cat == "Irrigated"
        ],
        na.rm = TRUE
      ),
      NPErha = sum(
        area_ygpit_ha[
          landuse == "Cropland" &
            name_biomass %in% non_permanent_biomass &
            irrig_cat == "Rainfed"
        ],
        na.rm = TRUE
      ),
      NPEiMha = NPEiha / 1e6,
      NPErMha = NPErha / 1e6,
      NPEiN = sum(
        (prod_mgn + usedresidue_mgn + grazedweeds_mgn)[
          landuse == "Cropland" &
            name_biomass %in% non_permanent_biomass &
            irrig_cat == "Irrigated"
        ],
        na.rm = TRUE
      ),
      NPErN = sum(
        (prod_mgn + usedresidue_mgn + grazedweeds_mgn)[
          landuse == "Cropland" &
            name_biomass %in% non_permanent_biomass &
            irrig_cat == "Rainfed"
        ],
        na.rm = TRUE
      ),

      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      -c(province_name, year),
      names_to = "var",
      values_to = "data"
    ) |>

    dplyr::mutate(
      label = paste0("{", var, "}"),
      province = province_name,
      year = year,
      align = "R"
    ) |>
    dplyr::select(province, year, label, data, align)

  df_land
}


#' @title Create dataset for greeonhouse, grassland, and N soil input
#'
#' @description
#' Generates dataset for greenhouse, grasslands, total km2, surpluses.
#' Combines with crops/forest dataset.
#'
#' @return
#' A tibble with columns `province`, `year`, `label`, `data`, and `align`.
#'
#' @keywords internal
.create_n_input_df <- function(n_balance) {
  n_balance |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(
      `{GREHha}` = sum(area_ygpit_ha[irrig_cat == "Greenhouse"], na.rm = TRUE),
      `{GREHMha}` = `{GREHha}` / 1e6,
      `{GREHN}` = sum(
        prod_mgn[irrig_cat == "Greenhouse"] +
          usedresidue_mgn[irrig_cat == "Greenhouse"] +
          grazedweeds_mgn[irrig_cat == "Greenhouse"],
        na.rm = TRUE
      ),
      `{HAGRASS}` = sum(
        area_ygpit_ha[
          landuse %in%
            c(
              "Dehesa",
              "Forest_high",
              "Forest_low",
              "Other",
              "Pasture_Shrubland"
            )
        ],
        na.rm = TRUE
      ),
      `{GRASSMha}` = `{HAGRASS}` / 1e6,
      `{HACULT}` = sum(area_ygpit_ha[landuse == "Cropland"], na.rm = TRUE),
      `{KM2_PROVINCE}` = sum(area_ygpit_ha, na.rm = TRUE) / 100,
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      -c(province_name, year),
      names_to = "label",
      values_to = "data"
    ) |>
    dplyr::select(province = province_name, year = year, label, data) |>
    dplyr::mutate(align = "L")
}


#' @title Create land nitrogen surplus dataset (cropland & semi-natural systems)
#'
#' @description
#' Calculates nitrogen surplus for cropland and semi-natural agroecosystems
#' (grassland) by province and year. The surplus is calculated as the difference
#' between total nitrogen inputs and outputs.
#' Inputs include nitrogen from synthetic fertilizers, biological fixation,
#' atmospheric deposition, and livestock manure.
#' Outputs include nitrogen flows from cropland to population, livestock,
#' exports, and other uses.
#'
#' @param prov_destiny_df A data frame containing nitrogen flows with columns
#' such as `origin`, `destiny`, `province_name`, `year`, and `mg_n`.
#'
#' @return
#' A tibble with columns: Province name, year, label (`{CROP_SURPLUS}` or `{GRASS_SURPLUS}`), nitrogen
#' surplus (mg_n), and text alignment.
#'
#' @keywords internal
.create_land_surplus_df <- function(prov_destiny_df) {
  inputs <- prov_destiny_df |>
    dplyr::filter(
      (origin %in%
        c("Synthetic", "Fixation", "Deposition", "Livestock") &
        destiny %in% c("Cropland", "semi_natural_agroecosystems"))
    ) |>
    dplyr::mutate(
      system = dplyr::case_when(
        destiny == "Cropland" ~ "crop",
        destiny == "semi_natural_agroecosystems" ~ "grass"
      )
    ) |>
    dplyr::group_by(province_name, year, system) |>
    dplyr::summarise(input = sum(mg_n, na.rm = TRUE), .groups = "drop")

  outputs <- prov_destiny_df |>
    dplyr::filter(
      (origin == "Cropland" &
        destiny %in%
          c(
            "population_food",
            "population_other_uses",
            "livestock_rum",
            "livestock_mono",
            "export"
          )) |
        (origin == "semi_natural_agroecosystems" &
          destiny %in%
            c(
              "population_food",
              "population_other_uses",
              "livestock_rum",
              "livestock_mono",
              "export"
            ))
    ) |>
    dplyr::mutate(
      system = dplyr::case_when(
        origin == "Cropland" ~ "crop",
        origin == "semi_natural_agroecosystems" ~ "grass"
      )
    ) |>
    dplyr::group_by(province_name, year, system) |>
    dplyr::summarise(output = sum(mg_n, na.rm = TRUE), .groups = "drop")

  dplyr::full_join(
    inputs,
    outputs,
    by = c("province_name", "year", "system")
  ) |>
    dplyr::mutate(
      input = dplyr::coalesce(input, 0),
      output = dplyr::coalesce(output, 0),
      surplus = input - output,
      label = dplyr::case_when(
        system == "crop" ~ "{CROP_SURPLUS}",
        system == "grass" ~ "{GRASS_SURPLUS}"
      ),
      province = province_name,
      year = year,
      align = "R"
    ) |>
    dplyr::select(province, year, label, data = surplus, align)
}


#' @title Create nitrogen flow dataset by province
#'
#' @description
#' Generates nitrogen flow data (mg_n) by province and year, representing
#' @title Create nitrogen flow dataset by province
#'
#' @description
#' Generates nitrogen flow data (mg_n) by province and year, representing
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
        origin == "Cropland" & destiny == "export" ~ "{CROP_EXPORT}",
        origin == "Cropland" &
          destiny %in% c("population_food", "population_other_uses") ~
          "{CROPS_TO_POP}",
        origin == "Cropland" &
          destiny %in% c("livestock_rum", "livestock_mono") ~
          "{CROPS_TO_LIVESTOCK}",
        origin == "Livestock" &
          destiny %in% c("population_food", "population_other_uses") ~
          "{LIVESTOCK_TO_HUMAN}",
        origin == "semi_natural_agroecosystems" &
          destiny %in% c("livestock_rum", "livestock_mono") ~
          "{GRASS_TO_LIVESTOCK}",
        origin == "People" & destiny == "Cropland" ~ "{ORGOT}",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(label)) |>
    dplyr::group_by(province_name, year, label) |>
    dplyr::summarise(data = sum(mg_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      align = dplyr::if_else(label == "{ORGOT}", "R", "L")
    ) |>
    dplyr::rename(province = province_name, year = year) |>
    tidyr::complete(
      province = unique(prov_destiny_df$province_name),
      year = unique(prov_destiny_df$year),
      label = c(
        "{CROP_EXPORT}",
        "{CROPS_TO_POP}",
        "{CROPS_TO_LIVESTOCK}",
        "{LIVESTOCK_TO_HUMAN}",
        "{GRASS_TO_LIVESTOCK}",
        "{ORGOT}"
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
      destiny %in% c("population_food", "population_other_uses"),
      origin == "Livestock"
    ) |>
    dplyr::mutate(
      group_item = ifelse(
        item %in% c("Hides and skins", "Wool (Clean Eq.)", "Silk"),
        "non_edible",
        "edible"
      )
    ) |>
    dplyr::group_by(province_name, year, group_item) |>
    dplyr::summarise(mg_n = sum(mg_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = dplyr::case_when(
        group_item == "non_edible" ~ "{LVSTCK_NOEDIBLE}",
        group_item == "edible" ~ "{LV_EDBL}"
      ),
      align = "L"
    ) |>
    dplyr::select(
      province = province_name,
      year = year,
      label,
      data = mg_n,
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
      origin == "Livestock",
      destiny == "population_food",
      item %in%
        c("Milk - Excluding Butter", "Milk, lactation", "Whey", "Butter, Ghee")
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(mg_n = sum(mg_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "{LVST_MILK}",
      align = "L"
    ) |>
    dplyr::select(
      province = province_name,
      year = year,
      label,
      data = mg_n,
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
      destiny == "export",
      origin == "Livestock"
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(data = sum(mg_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "{LIVESTOCK_EXPORTED}",
      align = "L",
      province = province_name,
      year = year
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
      origin == "Cropland",
      destiny %in% c("livestock_rum", "livestock_mono")
    ) |>
    dplyr::group_by(province_name, year, destiny) |>
    dplyr::summarise(
      data = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      label = dplyr::case_when(
        destiny == "livestock_rum" ~ "{RCRTOLVSTCK_R}",
        destiny == "livestock_mono" ~ "{MCRTOLVSTCK_M}"
      ),
      align = "L"
    ) |>
    dplyr::rename(province = province_name, year = year) |>
    dplyr::select(province, year, label, data, align)

  df_feed
}

#' @title Create crop losses dataset
#'
#' @description
#' Generates N data from other uses in cropland.
#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_crop_losses_df <- function(n_balance, prov_destiny_df) {
  df_crop_oth <- prov_destiny_df |>
    dplyr::filter(
      origin == "Cropland",
      destiny == "population_other_uses"
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(
      data = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      label = "{CRP_OTHUSES}",
      align = "L"
    ) |>
    dplyr::rename(
      province = province_name,
      year = year
    )
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
    dplyr::rename_with(tolower) |>
    dplyr::select(
      year,
      province_name,
      livestock_cat,
      n_excr_mgn
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      `{AN_LS}` = n_excr_mgn
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(
      `{AN_LS}` = sum(`{AN_LS}`, na.rm = TRUE),
      .groups = "drop"
    )

  an_oth <- prov_destiny_df |>
    dplyr::filter(
      origin == "Livestock",
      destiny == "population_other_uses"
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(
      `{AN_OTH}` = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  df_animal_losses <- n_excretion |>
    dplyr::left_join(an_oth, by = c("province_name", "year")) |>
    dplyr::mutate(
      `{AN_OTH}` = ifelse(is.na(`{AN_OTH}`), 0, `{AN_OTH}`),
      `{AN_LS_OTH}` = `{AN_LS}` + `{AN_OTH}`
    ) |>
    tidyr::pivot_longer(
      cols = c(`{AN_LS}`, `{AN_OTH}`, `{AN_LS_OTH}`),
      names_to = "label",
      values_to = "data"
    ) |>
    dplyr::mutate(
      align = "R"
    ) |>
    dplyr::rename(
      province = province_name,
      year = year
    )

  df_animal_losses
}

#' @title Create combined livestock nitrogen dataset
#'
#' @description
#' Combines nitrogen data from livestock destined for humans, exports, and
#' losses to generate combined nitrogen output from livestock.
#'
#' @param prov_destiny_df A data frame containing production and destiny
#' information.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_livestock_total_df <- function(prov_destiny_df) {
  prov_destiny_df |>
    dplyr::filter(
      origin == "Livestock",
      destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "export"
        )
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(data = sum(mg_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      label = "{LVSTCKTOTN}",
      align = "L"
    ) |>
    dplyr::select(
      province = province_name,
      year = year,
      label,
      data,
      align
    )
}

#' @title Create livestock loss dataset
#'
#' @description
#' Calculates nitrogen losses from livestock excretion based on
#' excretion and loss share data.
#'
#' @return A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_livestock_surplus_df <- function(df_all_flows) {
  input_labels <- c(
    "{CROPS_TO_LIVESTOCK}",
    "{GRASS_TO_LIVESTOCK}",
    "{IMANOTR}",
    "{IMANOTM}",
    "{IMPORT_ANIMALCR_RUM}",
    "{IMPORT_ANIMALCR_MONOG}"
  )

  output_labels <- c(
    "{LIVESTOCK_TO_HUMAN}",
    "{LIVESTOCK_EXPORTED}",
    "{LIVESTOCK_TO_CROPS}",
    "{LIVESTOCK_TO_GRASS}",
    "{AN_OTH}"
  )

  df_inputs <- df_all_flows |>
    dplyr::filter(label %in% input_labels) |>
    dplyr::group_by(province, year) |>
    dplyr::summarise(
      input = sum(as.numeric(data), na.rm = TRUE),
      .groups = "drop"
    )

  df_outputs <- df_all_flows |>
    dplyr::filter(label %in% output_labels) |>
    dplyr::group_by(province, year) |>
    dplyr::summarise(
      output = sum(as.numeric(data), na.rm = TRUE),
      .groups = "drop"
    )

  dplyr::full_join(df_inputs, df_outputs, by = c("province", "year")) |>
    dplyr::mutate(
      input = dplyr::coalesce(input, 0),
      output = dplyr::coalesce(output, 0),
      data = input - output,
      label = "{LIVGASLOSS}",
      align = "R"
    ) |>
    dplyr::select(province, year, label, data, align)
}

#' @title Create population dataset
#'
#' @description
#' Loads population data (in million inhabitants, MInhab) and converts it
#' into the GRAFS plot structure.
#'
#' @return
#' A tibble with columns `province`, `year`, `label`, `data`, `align`.
#'
#' @keywords internal
.create_population_df <- function() {
  population <- whep_read_file("population_yg") |>
    dplyr::rename_with(tolower)

  df_pop <- population |>
    dplyr::select(
      province = province_name,
      year = year,
      pop_mpeop_yg
    ) |>
    dplyr::mutate(
      label = "{POPULATIONM}",
      data = pop_mpeop_yg,
      align = "L"
    ) |>
    dplyr::select(province, year, label, data, align)

  df_pop
}


#' @keywords internal
.create_wastewater_surplus_df <- function(df_all_flows, prov_destiny_df) {
  input_labels <- c(
    "{CROPS_TO_POP}",
    "{CROP_POPIMPORT}",
    "{LIVESTOCK_TO_HUMAN}",
    "{IMPHMANA}"
  )

  df_inputs <- df_all_flows |>
    dplyr::filter(label %in% input_labels) |>
    dplyr::group_by(province, year) |>
    dplyr::summarise(
      input = sum(as.numeric(data), na.rm = TRUE),
      .groups = "drop"
    )

  df_returned <- prov_destiny_df |>
    dplyr::filter(
      origin == "People",
      destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(province_name, year) |>
    dplyr::summarise(returned = sum(mg_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::rename(province = province_name)

  dplyr::full_join(df_inputs, df_returned, by = c("province", "year")) |>
    dplyr::mutate(
      input = dplyr::coalesce(input, 0),
      returned = dplyr::coalesce(returned, 0),
      data = input - returned,
      label = "{WASTEWATER}",
      align = "R"
    ) |>
    dplyr::select(province, year, label, data, align)
}


#' @title Combine and finalize nitrogen flow dataset
#'
#' @description
#' Merges all the created nitrogen datasets into a unified structure.
#' Adding missing labels and setting WIDTH_MAX to 1500. IMPHUMHONEY should be 0.
#' The other labels (CRPNOLV", "NCONTCROP") are
#' set to 0, since I don't know how to create them yet.
#'
#' @param crop_livestock_flows Data frame of crop-livestock nitrogen flows.
#' @param df_livestock Data frame of livestock nitrogen data.
#' @param df_lv_r_m Data frame of livestock feed data.
#' @param df_crop_losses Data frame of crop nitrogen losses.
#' @param df_animal_losses Data frame of animal nitrogen losses.
#' @param df_livestock_total Data frame of total livestock nitrogen.
#' @param df_livestock_surplus Data frame of livestock surplus nitrogen.
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
  df_livestock_surplus,
  df_land_surplus
) {
  df_combi <- dplyr::bind_rows(
    crop_livestock_flows |> dplyr::select(province, year, label, data, align),
    df_livestock |> dplyr::select(province, year, label, data, align),
    df_lv_r_m |> dplyr::select(province, year, label, data, align),
    df_crop_losses |> dplyr::select(province, year, label, data, align),
    df_animal_losses |> dplyr::select(province, year, label, data, align),
    df_livestock_total |> dplyr::select(province, year, label, data, align),
    df_livestock_surplus |> dplyr::select(province, year, label, data, align),
    df_land_surplus |> dplyr::select(province, year, label, data, align)
  ) |>
    dplyr::arrange(province, year, label) |>
    dplyr::mutate(
      data = as.character(data),
      align = as.character(align)
    )

  missing_labels <- c(
    "{IMPHUMHONEY}",
    "{CRP_LS_OTHUSES}",
    "{CRP_LS}",
    "{CRPNOLV}",
    "{NCONTCROP}",
    "{WIDTH_MAX}"
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
        label %in% c("{NCONTCROP}", "{ORGOT}") ~ "R",
        TRUE ~ align
      )
    ) |>
    dplyr::bind_rows(
      dplyr::distinct(df_combi, province, year) |>
        dplyr::mutate(
          label = "{YEAR}",
          data = as.character(year),
          align = "L"
        ) |>
        dplyr::select(province, year, label, data, align),
      dplyr::distinct(df_combi, province, year) |>
        dplyr::mutate(
          label = "{PROVINCE_NAME}",
          data = as.character(province),
          align = "L"
        ) |>
        dplyr::select(province, year, label, data, align)
    ) |>
    dplyr::mutate(
      data = dplyr::case_when(
        label == "{WIDTH_MAX}" ~ "1500",
        TRUE ~ data
      ),
      align = dplyr::case_when(
        label == "{WIDTH_MAX}" ~ "L",
        label %in%
          c(
            "{NCONTCROP}",
            "{ORGOT}"
          ) ~
          "R",
        TRUE ~ align
      )
    ) |>
    dplyr::arrange(province, year, label)

  df_combi
}
