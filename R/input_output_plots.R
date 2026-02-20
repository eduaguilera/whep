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
      title = "Spanish nitrogen flows – Livestock system (full system balance)",
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
