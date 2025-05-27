create_n_inputs_grafs_spain <- function() {
  # Define input directory
  inputs_dir <- "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs"

  # Load datasets
  data <- .load_inputs(inputs_dir)

  # Calculate N inputs and manure
  n_inputs_prepared <- .calculate_n_inputs(data$N_balance_ygpit_all, data$Codes_coefs)

  # Summarise inputs
  n_inputs_summary <- .summarise_inputs(n_inputs_prepared)

  # Summarise production
  n_inputs_combined <- .summarise_production(data$GRAFS_Prod_Destiny, n_inputs_summary)

  # Calculate NUE
  nue <- .calculate_nue(n_inputs_combined)

  # Create plots
  plot1 <- .plot_n_inputs_production_cropland_semi_natural_agroecosystems(data$GRAFS_Prod_Destiny, n_inputs_combined)
  plot2 <- .plot_nue_spain(nue)
  plot3 <- .plot_n_inputs_production_cropland(data$GRAFS_Prod_Destiny, n_inputs_combined)
  plot4 <- .plot_n_inputs_production_provinces(data$GRAFS_Prod_Destiny, n_inputs_combined)

  # Return the loaded datasets
  list(
    N_Inputs_combined = n_inputs_combined,
    NUE = nue,
    plots = list(
      cropland_seminatural = plot1,
      nue = plot2,
      cropland = plot3,
      provinces = plot4
    )
  )
}

# N Inputs -----------------------------------------------------------------------------------------------------------------------------------
# Prepare data -------------------------------------------------------------------------------------------------------------------------------
.load_inputs <- function(inputs_dir) {
  list(
    N_Excretion_ygs = readRDS(file.path(inputs_dir, "N_Excretion_ygs.rds")),
    N_balance_ygpit_all = readRDS(file.path(inputs_dir, "N_balance_ygpit_all.rds")),
    GRAFS_Prod_Destiny = readr::read_csv(file.path(inputs_dir, "GRAFS_Prod_Destiny.csv")),
    Codes_coefs = readxl::read_excel(file.path(inputs_dir, "Codes_coefs.xlsx"), sheet = "Names_biomass_CB")
  )
}

.calculate_n_inputs <- function(N_balance_ygpit_all, Codes_coefs) {
  Semi_natural_agroecosystems <- c("Dehesa", "Forest_high", "Forest_low", "Other", "Pasture_Shrubland")
  firewood_biomass <- c("Holm oak", "Mediterranean shrubland", "Conifers", "Holm oak forest")

  # Merge Name_biomass with Item
  Items <- Codes_coefs |>
    dplyr::select(Name_biomass, Item)

  # Combine all necessary N Inputs
  N_inputs_summary <- N_balance_ygpit_all |>
    dplyr::left_join(Items, by = "Name_biomass") |>
    dplyr::mutate(
      Item = ifelse(Name_biomass %in% firewood_biomass, "Firewood", Item),
      Box = ifelse(LandUse %in% Semi_natural_agroecosystems, "Semi_natural_agroecosystems", LandUse)
    ) |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, Box) |>
    dplyr::summarise(
      Deposition = sum(Deposition, na.rm = TRUE),
      BNF = sum(BNF, na.rm = TRUE),
      Synthetic = sum(Synthetic, na.rm = TRUE),
      Urban = sum(Urban, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Box, Deposition, BNF, Synthetic, Urban)

  # Calculation of Manure from Excreta, Solid, Liquid ----------------------------------------------------------------------------------------------------
  item_lookup <- Codes_coefs |>
    dplyr::select(Name_biomass, Item) |>
    dplyr::distinct()

  manure_selected <- N_balance_ygpit_all |>
    dplyr::left_join(item_lookup, by = "Name_biomass") |>
    dplyr::mutate(
      Item = ifelse(Name_biomass %in% firewood_biomass, "Firewood", Item),
      Box = ifelse(LandUse %in% Semi_natural_agroecosystems, "Semi_natural_agroecosystems", LandUse)
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Box, Excreta, Solid, Liquid)

  manure_summary <- manure_selected |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, Box) |>
    dplyr::summarise(Total_Manure = sum(Excreta + Solid + Liquid, na.rm = TRUE), .groups = "drop")

  list(
    N_inputs_summary = N_inputs_summary,
    manure_summary = manure_summary
  )
}

# Combine all Inputs -----------------------------------------------------------------------------------------------------------------------------------------------------
.summarise_inputs <- function(n_inputs_prepared) {
  N_Inputs <- dplyr::full_join(
    n_inputs_prepared$N_inputs_summary,
    n_inputs_prepared$manure_summary,
    by = c("Year", "Province_name", "Name_biomass", "Item", "Box")
  )

  # Summing Inputs for each Year, Province_name, Box
  N_Inputs_sum <- N_Inputs |>
    dplyr::group_by(Year, Province_name, Box) |>
    dplyr::summarise(
      MgN_dep = sum(Deposition, na.rm = TRUE),
      MgN_fix = sum(BNF, na.rm = TRUE),
      MgN_syn = sum(Synthetic, na.rm = TRUE),
      MgN_manure = sum(Total_Manure, na.rm = TRUE),
      MgN_urban = sum(Urban, na.rm = TRUE),
      .groups = "drop"
    )
  return(N_Inputs_sum)
}

# GRAFS_Prod_Destiny -------------------------------------------------------------------------------------------------------------------------------------------------
# Summarize and calculate new columns: Prod_MgN
# Spread the Destiny column to separate columns for Food, Feed, Other_uses, Export
.summarise_production <- function(GRAFS_Prod_Destiny, N_Inputs_sum) {
  GRAFS_Prod_Destiny_summary <- GRAFS_Prod_Destiny |>
    tidyr::pivot_wider(names_from = Destiny, values_from = MgN, values_fn = sum, values_fill = list(MgN = 0)) |>
    dplyr::mutate(
      Prod_MgN = (Food + Feed + Other_uses + Export) - Import,
      Import_MgN = Import,
      # Set Production to 0 for Fish Box
      Prod_MgN = ifelse(Box == "Fish", 0, Prod_MgN)
    ) |>
    dplyr::group_by(Year, Province_name, Box) |>
    dplyr::summarise(
      Import_MgN = sum(Import_MgN, na.rm = TRUE),
      Prod_MgN = sum(Prod_MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(Year, Province_name, Box) |>
    dplyr::select(Year, Province_name, Box, Import_MgN, Prod_MgN)

  # Combine with N_Inputs dataset
  N_Inputs_combined <- dplyr::full_join(N_Inputs_sum, GRAFS_Prod_Destiny_summary,
    by = c("Year", "Province_name", "Box")
  )
  return(N_Inputs_combined)
}

# NUE for Cropland and Semi-natural agroecosystems ------------------------------------------------------------------------------------------------------
.calculate_nue <- function(N_Inputs_combined) {
  NUE <- N_Inputs_combined |>
    dplyr::group_by(Year, Province_name, Box) |>
    dplyr::mutate(
      Inputs_MgN = sum(MgN_dep, MgN_fix, MgN_syn, MgN_manure, MgN_urban, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  NUE <- NUE |>
    dplyr::mutate(
      NUE = ifelse(Box %in% c("Semi_natural_agroecosystems", "Cropland"),
        Prod_MgN / Inputs_MgN * 100,
        NA_real_
      )
    )
  return(NUE)
}

# Plots --------------------------------------------------------------------------------------------------------------------------------------------------
# Plot for Cropland AND Semi_natural_agroecosystems ------------------------------------------------------------------------------------------------------
# Summarise Inputs + Production for Cropland & Semi_natural_agroecosystems
.plot_n_inputs_production_cropland_semi_natural_agroecosystems <- function(GRAFS_Prod_Destiny, N_Inputs_combined) {
  residue_items <- c("Other crop residues", "Straw", "Firewood")

  GRAFS_Prod_Destiny_Residues <- GRAFS_Prod_Destiny |>
    dplyr::filter(Box %in% c("Cropland", "Semi_natural_agroecosystems")) |>
    dplyr::mutate(
      Prod_type = ifelse(Item %in% residue_items, "Production_residues", "Production")
    ) |>
    dplyr::group_by(Year, Item, Destiny, Prod_type) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = Destiny,
      values_from = MgN,
      values_fill = 0
    ) |>
    dplyr::mutate(
      Prod_MgN = (Food + Feed + Other_uses + Export) - Import
    ) |>
    dplyr::group_by(Year, Prod_type) |>
    dplyr::summarise(Prod_MgN = sum(Prod_MgN, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = Prod_type,
      values_from = Prod_MgN,
      values_fill = 0
    )

  N_summary <- N_Inputs_combined |>
    dplyr::filter(Box %in% c("Cropland", "Semi_natural_agroecosystems")) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      Deposition = sum(MgN_dep, na.rm = TRUE),
      Fixation = sum(MgN_fix, na.rm = TRUE),
      Synthetic_fertilizer = sum(MgN_syn, na.rm = TRUE),
      Manure = sum(MgN_manure, na.rm = TRUE),
      Urban = sum(MgN_urban, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(GRAFS_Prod_Destiny_Residues, by = "Year") |>
    dplyr::mutate(
      Production = dplyr::coalesce(Production, 0),
      Production_residues = dplyr::coalesce(Production_residues, 0),
      Input_Total = Deposition + Fixation + Manure + Synthetic_fertilizer + Urban,
      Surplus = Input_Total - Production - Production_residues
    )

  N_long <- N_summary |>
    tidyr::pivot_longer(
      cols = c(Synthetic_fertilizer, Manure, Fixation, Deposition, Urban, Production, Production_residues),
      names_to = "Type",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      Value = Value / 1000,
      Value = dplyr::case_when(
        Type %in% c("Synthetic_fertilizer", "Manure", "Fixation", "Deposition", "Urban") ~ -Value,
        TRUE ~ Value
      )
    ) |>
    dplyr::bind_rows(
      N_summary |> dplyr::mutate(Type = "Surplus", Value = Surplus / 1000) |> dplyr::select(Year, Type, Value)
    ) |>
    dplyr::mutate(
      Type = factor(Type, levels = c(
        "Synthetic_fertilizer", "Manure", "Fixation", "Deposition", "Urban",
        "Surplus", "Production_residues", "Production"
      ))
    )

  ggplot2::ggplot(
    N_long,
    ggplot2::aes(x = Year, y = Value, fill = Type)
  ) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = "N Inputs, Production and Surplus in Spain (Cropland + Semi-natural agroecosystems)",
      x = "Year",
      y = "Gg N",
      fill = ""
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Synthetic_fertilizer" = "red4",
        "Manure" = "darkorange3",
        "Urban" = "darkorange4",
        "Fixation" = "olivedrab4",
        "Deposition" = "gray40",
        "Surplus" = "slategray",
        "Production_residues" = "darkgoldenrod1",
        "Production" = "orange3"
      )
    ) +
    ggplot2::theme_minimal()

  list(
    plot = plot,
    data_long = N_long,
    summary = N_summary,
    residues = GRAFS_Prod_Destiny_Residues
  )
}

# NUE plot -----------------------------------------------------------------------------------------------------------------------------------------------
.plot_nue_spain <- function(NUE) {
  NUE_spain <- NUE |>
    dplyr::filter(Box %in% c("Cropland", "Semi_natural_agroecosystems"), Province_name != "Sea") |>
    dplyr::group_by(Year, Box) |>
    dplyr::summarise(
      NUE_spain = mean(NUE, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot2::ggplot(NUE_spain, ggplot2::aes(x = Year, y = NUE_spain, color = Box)) +
    ggplot2::geom_line(size = 0.8) +
    ggplot2::labs(
      title = "Nitrogen Use Efficiency (NUE) in Spain",
      x = "Year",
      y = "NUE (%)",
      color = "Box"
    ) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 7)
    )

  list(
    plot = plot,
    data = NUE_spain
  )
}

# Plot Spain Input/Output no imports, but surpluses ---------------------------------------------------------------------------------------------------------
.plot_n_inputs_production_cropland <- function(GRAFS_Prod_Destiny, N_Inputs_combined) {
  residue_items <- c("Other crop residues", "Straw", "Firewood")

  GRAFS_Prod_Destiny_Residues <- GRAFS_Prod_Destiny |>
    dplyr::filter(Box == "Cropland") |>
    dplyr::mutate(
      Prod_type = ifelse(Item %in% residue_items, "Production_residues", "Production")
    ) |>
    dplyr::group_by(Year, Province_name, Item, Destiny, Prod_type) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = Destiny,
      values_from = MgN,
      values_fill = 0
    ) |>
    dplyr::mutate(
      Prod_MgN = (Food + Feed + Other_uses + Export) - Import
    ) |>
    dplyr::group_by(Year, Prod_type) |>
    dplyr::summarise(Prod_MgN = sum(Prod_MgN, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = Prod_type,
      values_from = Prod_MgN,
      values_fill = 0
    )

  N_summary_Residues <- N_Inputs_combined |>
    dplyr::filter(Box == "Cropland") |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      Deposition = sum(MgN_dep, na.rm = TRUE),
      Fixation = sum(MgN_fix, na.rm = TRUE),
      Synthetic_fertilizer = sum(MgN_syn, na.rm = TRUE),
      Manure = sum(MgN_manure, na.rm = TRUE),
      Urban = sum(MgN_urban, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(GRAFS_Prod_Destiny_Residues, by = "Year") |>
    dplyr::mutate(
      Production = dplyr::coalesce(Production, 0),
      Production_residues = dplyr::coalesce(Production_residues, 0),
      Input_Total = Deposition + Fixation + Manure + Synthetic_fertilizer + Urban,
      Surplus = Input_Total - Production - Production_residues
    )

  N_summary_Residues <- N_summary_Residues |>
    tidyr::pivot_longer(
      cols = c(Synthetic_fertilizer, Manure, Fixation, Deposition, Urban, Production, Production_residues),
      names_to = "Type",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      Value = Value / 1000,
      Value = dplyr::case_when(
        Type %in% c("Synthetic_fertilizer", "Manure", "Fixation", "Deposition", "Urban") ~ -Value,
        TRUE ~ Value
      )
    ) |>
    dplyr::bind_rows(
      N_summary_Residues |>
        dplyr::mutate(Type = "Surplus", Value = Surplus / 1000) |>
        dplyr::select(Year, Type, Value)
    ) |>
    dplyr::mutate(
      Type = factor(Type, levels = c(
        "Synthetic_fertilizer", "Manure", "Fixation", "Deposition", "Urban",
        "Surplus", "Production_residues", "Production"
      ))
    )

  ggplot2::ggplot(
    N_summary_Residues,
    ggplot2::aes(x = Year, y = Value, fill = Type)
  ) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = "N Inputs, Production and Surpluses in Spanish Cropland",
      x = "Year",
      y = "Gg N",
      fill = ""
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Synthetic_fertilizer" = "red4",
        "Manure" = "darkorange3",
        "Urban" = "darkorange4",
        "Fixation" = "olivedrab4",
        "Deposition" = "gray40",
        "Surplus" = "slategray",
        "Production_residues" = "darkgoldenrod1",
        "Production" = "orange3"
      )
    ) +
    ggplot2::theme_minimal()

  list(
    plot = plot,
    data_long = N_summary_Residues_long,
    summary = N_summary_Residues,
    residues = GRAFS_Prod_Destiny_Residues
  )
}


# Plot Spain Input/ no imports, but surpluses for Provinces ----------------------------------------------------------------------------------------------------------------
.plot_n_inputs_production_provinces <- function(GRAFS_Prod_Destiny, N_Inputs_combined) {
  N_Inputs_combined <- N_Inputs_combined |> dplyr::filter(Province_name != "Sea")

  GRAFS_Prod_Destiny_Residues_prov <- GRAFS_Prod_Destiny |>
    dplyr::filter(Box == "Cropland") |>
    dplyr::mutate(
      Prod_type = ifelse(Item %in% residue_items, "Production_residues", "Production")
    ) |>
    dplyr::group_by(Year, Province_name, Item, Destiny, Prod_type) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = Destiny,
      values_from = MgN,
      values_fill = 0
    ) |>
    dplyr::mutate(
      Prod_MgN = (Food + Feed + Other_uses + Export) - Import
    ) |>
    dplyr::group_by(Year, Province_name, Prod_type) |>
    dplyr::summarise(Prod_MgN = sum(Prod_MgN, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = Prod_type,
      values_from = Prod_MgN,
      values_fill = 0
    )

  N_summary_prov_residues <- N_Inputs_combined |>
    dplyr::filter(Box == "Cropland") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Deposition = sum(MgN_dep, na.rm = TRUE),
      Fixation = sum(MgN_fix, na.rm = TRUE),
      Synthetic_fertilizer = sum(MgN_syn, na.rm = TRUE),
      Manure = sum(MgN_manure, na.rm = TRUE),
      Urban = sum(MgN_urban, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(GRAFS_Prod_Destiny_Residues_prov, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      Production = dplyr::coalesce(Production, 0),
      Production_residues = dplyr::coalesce(Production_residues, 0),
      Input_Total = Deposition + Fixation + Manure + Synthetic_fertilizer + Urban,
      Surplus = Input_Total - Production - Production_residues
    )

  N_long_prov_residues <- N_summary_prov_residues |>
    tidyr::pivot_longer(
      cols = c(Synthetic_fertilizer, Manure, Fixation, Deposition, Urban, Production, Production_residues),
      names_to = "Type",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      Value = Value / 1000,
      Value = dplyr::case_when(
        Type %in% c("Synthetic_fertilizer", "Manure", "Fixation", "Deposition", "Urban") ~ -Value,
        TRUE ~ Value
      )
    ) |>
    dplyr::bind_rows(
      N_summary_prov_residues |>
        dplyr::mutate(Type = "Surplus", Value = Surplus / 1000) |>
        dplyr::select(Year, Province_name, Type, Value)
    ) |>
    dplyr::mutate(
      Type = factor(Type, levels = c(
        "Synthetic_fertilizer", "Manure", "Fixation", "Deposition", "Urban",
        "Surplus", "Production_residues", "Production"
      ))
    )

  plot_prov_residues <- ggplot2::ggplot(
    N_long_prov_residues,
    ggplot2::aes(x = Year, y = Value, fill = Type)
  ) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::facet_wrap(~Province_name, scales = "free_y") +
    ggplot2::labs(
      title = "N Inputs, Production and Surpluses by Province",
      x = "Year",
      y = "Gg N",
      fill = ""
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Synthetic_fertilizer" = "red4",
        "Manure" = "darkorange3",
        "Urban" = "darkorange4",
        "Fixation" = "olivedrab4",
        "Deposition" = "gray40",
        "Surplus" = "slategray",
        "Production_residues" = "darkgoldenrod1",
        "Production" = "orange3"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )
  list(
    plot = plot_prov_residues,
    data_long = N_long_prov_residues,
    summary = N_summary_prov_residues,
    residues = GRAFS_Prod_Destiny_Residues_prov
  )
}
