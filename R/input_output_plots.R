#' Plot N inputs, production and surplus
#'
#' @param system Character. One of "Cropland" or
#'   "semi_natural_agroecosystems".
#'
#' @return A ggplot object.
#' @export
plot_input_output <- function(
  system = c("Cropland", "semi_natural_agroecosystems")
) {
  system <- match.arg(system)
  data <- create_n_prov_destiny()

  df_system <- data |>
    dplyr::filter(
      Province_name != "Sea"
    )

  inputs <- df_system |>
    dplyr::filter(
      Destiny == system,
      Origin %in%
        c("Deposition", "Fixation", "Synthetic", "Livestock", "People")
    ) |>
    dplyr::group_by(Year, Origin) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      Type = dplyr::recode(
        Origin,
        "Deposition" = "Deposition",
        "Fixation" = "Fixation",
        "Synthetic" = "Synthetic_fertilizer",
        "Livestock" = "Manure",
        "People" = "Urban"
      )
    )

  production <- df_system |>
    dplyr::filter(
      Origin == system,
      Destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Type = "Production")

  input_sum <- inputs |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Input_Total = sum(MgN), .groups = "drop")

  prod_sum <- production |>
    dplyr::select(Year, Production = MgN)

  surplus <- input_sum |>
    dplyr::left_join(prod_sum, by = "Year") |>
    dplyr::mutate(
      Production = dplyr::coalesce(Production, 0),
      Surplus = Input_Total - Production
    ) |>
    dplyr::select(Year, Surplus) |>
    dplyr::mutate(Type = "Surplus")

  plot_df <- dplyr::bind_rows(
    inputs |> dplyr::select(Year, Type, MgN),
    production |> dplyr::select(Year, Type, MgN),
    surplus |> dplyr::rename(MgN = Surplus)
  ) |>
    dplyr::mutate(
      MgN = MgN / 1000,
      MgN = dplyr::case_when(
        Type %in%
          c(
            "Synthetic_fertilizer",
            "Manure",
            "Fixation",
            "Deposition",
            "Urban"
          ) ~ -MgN,
        TRUE ~ MgN
      ),
      Type = factor(
        Type,
        levels = c(
          "Synthetic_fertilizer",
          "Manure",
          "Fixation",
          "Deposition",
          "Urban",
          "Surplus",
          "Production"
        )
      )
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = Year, y = MgN, fill = Type)) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = paste("Spanish nitrogen inputs and outputs –", system),
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
        "Production" = "orange3"
      )
    ) +
    ggplot2::theme_minimal()
}


#' Plot N inputs, production and surplus for Livestock system
#'
#' @return A ggplot object.
#' @export
plot_input_output_livestock <- function() {
  data <- create_n_prov_destiny()

  df <- data |>
    dplyr::filter(Province_name != "Sea")

  inputs <- df |>
    dplyr::filter(
      Destiny %in% c("livestock_rum", "livestock_mono")
    ) |>
    dplyr::group_by(Year, Destiny) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      Type = dplyr::recode(
        Destiny,
        "livestock_rum" = "Feed_ruminants",
        "livestock_mono" = "Feed_monogastric"
      )
    )

  production <- df |>
    dplyr::filter(
      Origin == "Livestock",
      Destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "export",
          "livestock_rum",
          "livestock_mono"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Type = "Production")

  input_sum <- inputs |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Input_Total = sum(MgN), .groups = "drop")

  prod_sum <- production |>
    dplyr::select(Year, Production = MgN)

  surplus <- input_sum |>
    dplyr::left_join(prod_sum, by = "Year") |>
    dplyr::mutate(
      Production = dplyr::coalesce(Production, 0),
      Surplus = Input_Total - Production
    ) |>
    dplyr::select(Year, Surplus) |>
    dplyr::mutate(Type = "Surplus")

  plot_df <- dplyr::bind_rows(
    inputs |> dplyr::select(Year, Type, MgN),
    production |> dplyr::select(Year, Type, MgN),
    surplus |> dplyr::rename(MgN = Surplus)
  ) |>
    dplyr::mutate(
      MgN = MgN / 1000,
      MgN = dplyr::case_when(
        Type %in% c("Feed_ruminants", "Feed_monogastric") ~ -MgN,
        TRUE ~ MgN
      ),
      Type = factor(
        Type,
        levels = c(
          "Feed_ruminants",
          "Feed_monogastric",
          "Surplus",
          "Production"
        )
      )
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = Year, y = MgN, fill = Type)) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = "Spanish nitrogen inputs and ouputs – Livestock system",
      x = "Year",
      y = "Gg N",
      fill = ""
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Feed_ruminants" = "darkolivegreen3",
        "Feed_monogastric" = "#1b9e77",
        "Surplus" = "slategray",
        "Production" = "orange3"
      )
    ) +
    ggplot2::theme_minimal()
}


#' Plot N inputs and uses for full agro-food system (system level)
#'
#' @return A ggplot object.
#' @export
plot_input_output_system <- function() {
  data <- create_n_prov_destiny()

  df <- data |>
    dplyr::filter(Province_name != "Sea")

  soil_inputs <- df |>
    dplyr::filter(
      Origin %in% c("Synthetic", "Fixation", "Deposition"),
      Destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year, Origin) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::mutate(
      Type = dplyr::recode(
        Origin,
        "Synthetic" = "Synthetic_fertilizer",
        "Fixation" = "Fixation",
        "Deposition" = "Deposition"
      )
    )

  feed_import <- df |>
    dplyr::filter(
      Origin == "Outside",
      Destiny %in% c("livestock_rum", "livestock_mono")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::mutate(Type = "Feed_import")

  food_import <- df |>
    dplyr::filter(
      Origin == "Outside",
      Destiny %in% c("population_food", "population_other_uses")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::mutate(Type = "Food_import")

  inputs <- dplyr::bind_rows(
    soil_inputs |> dplyr::select(Year, Type, MgN),
    feed_import,
    food_import
  )

  livestock_ingestion <- df |>
    dplyr::filter(
      Destiny %in% c("livestock_rum", "livestock_mono"),
      Origin %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::mutate(Type = "Livestock_ingestion")

  human_ingestion <- df |>
    dplyr::filter(
      Destiny %in% c("population_food", "population_other_uses"),
      Origin %in% c("Cropland", "semi_natural_agroecosystems", "Livestock")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::mutate(Type = "Human_ingestion")

  exports <- df |>
    dplyr::filter(Destiny == "export") |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::mutate(Type = "Export")

  uses_core <- dplyr::bind_rows(
    livestock_ingestion,
    human_ingestion,
    exports
  )

  input_sum <- inputs |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Input_Total = sum(MgN), .groups = "drop")

  use_sum <- uses_core |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Use_Total = sum(MgN), .groups = "drop")

  surplus <- input_sum |>
    dplyr::left_join(use_sum, by = "Year") |>
    dplyr::mutate(
      Use_Total = dplyr::coalesce(Use_Total, 0),
      MgN = Input_Total - Use_Total,
      Type = "Surplus"
    ) |>
    dplyr::select(Year, Type, MgN)

  plot_df <- dplyr::bind_rows(
    inputs,
    uses_core,
    surplus
  ) |>
    dplyr::mutate(
      MgN = MgN / 1000,
      MgN = dplyr::case_when(
        Type %in%
          c(
            "Synthetic_fertilizer",
            "Fixation",
            "Deposition",
            "Feed_import",
            "Food_import"
          ) ~ -MgN,
        TRUE ~ MgN
      ),
      Type = factor(
        Type,
        levels = c(
          "Synthetic_fertilizer",
          "Fixation",
          "Deposition",
          "Feed_import",
          "Food_import",
          "Surplus",
          "Livestock_ingestion",
          "Human_ingestion",
          "Export"
        )
      )
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = Year, y = MgN, fill = Type)) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = "Spanish nitrogen inputs and outputs – Agro-food system",
      x = "Year",
      y = "Gg N",
      fill = ""
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Synthetic_fertilizer" = "red4",
        "Fixation" = "olivedrab4",
        "Deposition" = "gray40",
        "Feed_import" = "#1b9e77",
        "Food_import" = "darkolivegreen3",
        "Livestock_ingestion" = "darkorange3",
        "Human_ingestion" = "darkorange4",
        "Export" = "orange3",
        "Surplus" = "slategray"
      )
    ) +
    ggplot2::theme_minimal()
}
