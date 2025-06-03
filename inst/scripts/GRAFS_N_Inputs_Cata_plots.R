# Plots --------------------------------------------------------------------------------------------------------------------------------------------------
# Plot for Cropland AND Semi_natural_agroecosystems ------------------------------------------------------------------------------------------------------
# Summarise Inputs + Production for Cropland & Semi_natural_agroecosystems
.plot_n_inputs_production_cropland_semi_natural_agroecosystems <- function(GRAFS_Prod_Destiny, N_Inputs_combined) {
  categories <- .assign_items()
  residue_items <- categories$residue_items

  GRAFS_Prod_Destiny_Residues <- GRAFS_Prod_Destiny |>
    dplyr::filter(Box %in% c("Cropland", "Semi_natural_agroecosystems")) |>
    dplyr::mutate(
      Prod_type = ifelse(Item %in% local(residue_items), "Production_residues", "Production")
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

  plot_Spain_cropland_seminatural <- ggplot2::ggplot(
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
    plot = plot_Spain_cropland_seminatural,
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

  plot_nue_Spain <- ggplot2::ggplot(NUE_spain, ggplot2::aes(x = Year, y = NUE_spain, color = Box)) +
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
    plot = plot_nue_Spain,
    data = NUE_spain
  )
}

# Plot Spain Input/Output no imports, but surpluses ---------------------------------------------------------------------------------------------------------
.plot_n_inputs_production_cropland <- function(GRAFS_Prod_Destiny, N_Inputs_combined) {
  categories <- .assign_items()
  residue_items <- categories$residue_items

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

  N_summary_Residues_long <- N_summary_Residues |>
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

  plot_spain_cropland <- ggplot2::ggplot(
    N_summary_Residues_long,
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
    plot = plot_spain_cropland,
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
