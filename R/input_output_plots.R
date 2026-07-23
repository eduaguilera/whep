#' Plot national nitrogen inputs, production, and surplus for a land system.
#'
#' @description
#' Builds a stacked-area plot of Spanish national nitrogen inputs (as negative
#' values), production, residues, and surplus over time for either cropland or
#' semi-natural agroecosystems.
#'
#' @param system Character. One of `"Cropland"` or
#'   `"semi_natural_agroecosystems"`.
#' @param example If `TRUE`, build the plot from a small example dataset without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' plot_input_output(example = TRUE)
plot_input_output <- function(
  system = c("Cropland", "semi_natural_agroecosystems"),
  example = FALSE
) {
  system <- match.arg(system)
  df_system <- .load_nat_destiny(example) |>
    dplyr::filter(Province_name != "Sea")

  inputs <- .system_inputs(df_system, system)
  production <- .system_production(df_system, system)
  surplus <- .surplus_from_totals(inputs, production, positive_only = TRUE)

  input_types <- c(
    "Synthetic_fertilizer",
    "Manure",
    "Fixation",
    "Deposition",
    "Urban"
  )
  plot_df <- .stack_plot_df(
    inputs,
    production,
    surplus,
    negative_types = input_types,
    type_levels = c(input_types, "Surplus", "Production", "Residues")
  )

  .stacked_area_plot(
    plot_df,
    title = paste("Spanish nitrogen inputs and outputs -", system),
    fill_values = c(
      "Synthetic_fertilizer" = "red4",
      "Manure" = "darkorange3",
      "Urban" = "darkorange4",
      "Fixation" = "olivedrab4",
      "Deposition" = "gray40",
      "Surplus" = "slategray",
      "Residues" = "goldenrod3",
      "Production" = "orange3"
    )
  )
}

#' Plot national nitrogen inputs, production, and surplus for livestock.
#'
#' @description
#' Builds a stacked-area plot of Spanish national livestock nitrogen feed
#' inputs (as negative values), production, and surplus over time.
#'
#' @param example If `TRUE`, build the plot from a small example dataset without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' plot_input_output_livestock(example = TRUE)
plot_input_output_livestock <- function(example = FALSE) {
  df <- .load_nat_destiny(example) |>
    dplyr::filter(Province_name != "Sea")

  inputs <- df |>
    dplyr::filter(Destiny %in% c("livestock_rum", "livestock_mono")) |>
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

  surplus <- .surplus_from_totals(inputs, production, positive_only = FALSE)

  feed_types <- c("Feed_ruminants", "Feed_monogastric")
  plot_df <- .stack_plot_df(
    inputs,
    production,
    surplus,
    negative_types = feed_types,
    type_levels = c(feed_types, "Surplus", "Production")
  )

  .stacked_area_plot(
    plot_df,
    title = "Spanish nitrogen inputs and outputs - Livestock system",
    fill_values = c(
      "Feed_ruminants" = "darkolivegreen3",
      "Feed_monogastric" = "#1b9e77",
      "Surplus" = "slategray",
      "Production" = "orange3"
    )
  )
}

#' Plot national nitrogen inputs and uses for the full agro-food system.
#'
#' @description
#' Builds a stacked-area plot of Spanish national nitrogen inputs (soil inputs
#' and imports, as negative values) against uses (feed, food, other uses,
#' exports) and surplus over time.
#'
#' @param example If `TRUE`, build the plot from a small example dataset without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' plot_input_output_system(example = TRUE)
plot_input_output_system <- function(example = FALSE) {
  df <- .load_nat_destiny(example) |>
    dplyr::filter(Province_name != "Sea")

  inputs <- .system_level_inputs(df)
  uses_core <- .system_level_uses(df)
  surplus <- .surplus_from_totals(inputs, uses_core, positive_only = TRUE)

  input_types <- c(
    "Synthetic_fertilizer",
    "Fixation",
    "Deposition",
    "Feed_import",
    "Food_import"
  )
  plot_df <- .stack_plot_df(
    inputs,
    uses_core,
    surplus,
    negative_types = input_types,
    type_levels = c(
      input_types,
      "Surplus",
      "Feed",
      "Food",
      "Other_uses",
      "Export"
    )
  )

  .stacked_area_plot(
    plot_df,
    title = "Spanish nitrogen inputs and outputs - Agro-food system",
    fill_values = c(
      "Synthetic_fertilizer" = "red4",
      "Fixation" = "olivedrab4",
      "Deposition" = "gray40",
      "Feed_import" = "#1b9e77",
      "Food_import" = "darkolivegreen3",
      "Feed" = "darkorange3",
      "Food" = "darkorange4",
      "Other_uses" = "sandybrown",
      "Export" = "orange3",
      "Surplus" = "slategray"
    )
  )
}

# Private helpers --------------------------------------------------------------

.load_nat_destiny <- function(example) {
  create_n_nat_destiny(example = example) |>
    .rename_destiny_pascal()
}

.system_inputs <- function(df_system, system) {
  df_system |>
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
}

.system_production <- function(df_system, system) {
  residue_items <- c("Straw", "Other crop residues")
  df_system |>
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
    dplyr::mutate(
      Type = dplyr::if_else(Item %in% residue_items, "Residues", "Production")
    ) |>
    dplyr::group_by(Year, Type) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop")
}

.system_level_inputs <- function(df) {
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

  feed_import <- .import_use(
    df,
    c("livestock_rum", "livestock_mono"),
    "Feed_import"
  )
  food_import <- .import_use(
    df,
    c("population_food", "population_other_uses"),
    "Food_import"
  )

  dplyr::bind_rows(
    soil_inputs |> dplyr::select(Year, Type, MgN),
    feed_import,
    food_import
  )
}

.import_use <- function(df, destinies, type) {
  df |>
    dplyr::filter(Origin == "Outside", Destiny %in% destinies) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::mutate(Type = type)
}

.system_level_uses <- function(df) {
  livestock_ingestion <- df |>
    dplyr::filter(
      Destiny %in% c("livestock_rum", "livestock_mono"),
      Origin %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::mutate(Type = "Feed")

  human_ingestion <- df |>
    dplyr::filter(
      Destiny %in% c("population_food", "population_other_uses"),
      Origin %in% c("Cropland", "semi_natural_agroecosystems", "Livestock")
    ) |>
    dplyr::mutate(
      Type = dplyr::if_else(Destiny == "population_food", "Food", "Other_uses")
    ) |>
    dplyr::group_by(Year, Type) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop")

  exports <- df |>
    dplyr::filter(Destiny == "export") |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::mutate(Type = "Export")

  dplyr::bind_rows(livestock_ingestion, human_ingestion, exports)
}

.surplus_from_totals <- function(inputs, uses, positive_only) {
  input_sum <- inputs |>
    dplyr::group_by(Year) |>
    dplyr::summarise(input_total = sum(MgN), .groups = "drop")

  use_sum <- uses |>
    dplyr::group_by(Year) |>
    dplyr::summarise(use_total = sum(MgN), .groups = "drop")

  input_sum |>
    dplyr::left_join(use_sum, by = "Year") |>
    dplyr::mutate(
      use_total = dplyr::coalesce(use_total, 0),
      net = input_total - use_total,
      MgN = if (positive_only) pmax(net, 0) else net,
      Type = "Surplus"
    ) |>
    dplyr::select(Year, Type, MgN)
}

.stack_plot_df <- function(
  inputs,
  outputs,
  surplus,
  negative_types,
  type_levels
) {
  dplyr::bind_rows(
    inputs |> dplyr::select(Year, Type, MgN),
    outputs |> dplyr::select(Year, Type, MgN),
    surplus |> dplyr::select(Year, Type, MgN)
  ) |>
    dplyr::mutate(
      MgN = MgN / 1000,
      MgN = dplyr::if_else(Type %in% negative_types, -MgN, MgN),
      Type = factor(Type, levels = type_levels)
    )
}

.stacked_area_plot <- function(plot_df, title, fill_values) {
  ggplot2::ggplot(plot_df, ggplot2::aes(x = Year, y = MgN, fill = Type)) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = title, x = "Year", y = "Gg N", fill = "") +
    ggplot2::scale_fill_manual(values = fill_values) +
    ggplot2::theme_minimal()
}
