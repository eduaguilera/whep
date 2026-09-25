#' Plot national nitrogen inputs, production, and surplus for a land system.
#'
#' @description
#' Builds a stacked-area plot of Spanish national nitrogen inputs (as negative
#' values), production, residues, and surplus over time for either cropland or
#' semi-natural agroecosystems.
#'
#' @param system Character. One of `"Cropland"` or
#'   `"semi_natural_agroecosystems"`.
#' @param per_ha Logical. If `TRUE`, express nitrogen flows per hectare of the
#'   system's land area (kg N/ha) instead of national totals (Gg N). Requires
#'   remote data, so it is ignored in example mode. Default is `FALSE`.
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
  per_ha = FALSE,
  example = FALSE
) {
  system <- match.arg(system)
  df_system <- .load_nat_destiny(example) |>
    dplyr::filter(Province_name != "Sea")

  n_balance <- .load_n_balance(example, needed = per_ha)
  lu_area <- .national_area(
    n_balance,
    per_ha,
    .system_landuse(n_balance, system)
  )
  per_ha <- per_ha && !is.null(lu_area)

  inputs <- .system_inputs(df_system, system)
  outputs <- .system_production(df_system, system)
  surplus <- .surplus_from_totals(inputs, outputs, positive_only = TRUE)

  input_types <- c(
    "Synthetic_fertilizer",
    "Manure",
    "Fixation",
    "Deposition",
    "Urban"
  )
  plot_df <- .stack_plot_df(
    inputs,
    outputs,
    surplus,
    negative_types = input_types,
    type_levels = c(
      input_types,
      "Surplus",
      "Production",
      "Residues"
    ),
    per_ha = per_ha,
    lu_area = lu_area
  )

  .stacked_area_plot(
    plot_df,
    fill_values = c(
      "Synthetic_fertilizer" = "red4",
      "Manure" = "darkorange3",
      "Urban" = "darkorange4",
      "Fixation" = "olivedrab4",
      "Deposition" = "gray40",
      "Surplus" = "slategray",
      "Residues" = "goldenrod3",
      "Production" = "orange3"
    ),
    breaks = c(
      "Surplus",
      "Production",
      "Residues",
      "Urban",
      "Deposition",
      "Fixation",
      "Manure",
      "Synthetic_fertilizer"
    ),
    labels = c(
      "Surplus",
      "Production",
      "Residues",
      "Urban",
      "Deposition",
      "Fixation",
      "Manure",
      "Synthetic fertilizer"
    ),
    annotate_label = if (system == "Cropland") {
      "Cropland"
    } else {
      "Semi-natural agroecosystems"
    },
    y_lab = if (per_ha) "kg N/ha" else "Gg N"
  )
}

#' Plot national nitrogen inputs, production, and surplus for livestock.
#'
#' @description
#' Builds a stacked-area plot of Spanish national livestock nitrogen feed
#' inputs (as negative values), production, and surplus over time. On real
#' data feed is broken down by origin (local grass, local crops, imports) and
#' production is split into ruminant and monogastric output using the
#' `stock_prod_ygps` pin. In example mode a simpler feed-by-destiny breakdown
#' is used so the plot builds offline.
#'
#' @param per_ha Logical. If `TRUE`, express nitrogen flows per hectare of
#'   agricultural land (kg N/ha) instead of national totals (Gg N). Requires
#'   remote data, so it is ignored in example mode. Default is `FALSE`.
#' @param example If `TRUE`, build the plot from a small example dataset without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' plot_input_output_livestock(example = TRUE)
plot_input_output_livestock <- function(per_ha = FALSE, example = FALSE) {
  df <- .load_nat_destiny(example) |>
    dplyr::filter(Province_name != "Sea")

  if (example) {
    return(.plot_livestock_example(df))
  }
  .plot_livestock_real(df, per_ha)
}

#' Plot national nitrogen inputs and uses for the full agro-food system.
#'
#' @description
#' Builds a stacked-area plot of Spanish national nitrogen inputs (soil inputs
#' and imports, as negative values) against uses (feed, food, other uses,
#' exports) and surplus over time.
#'
#' @param per_ha Logical. If `TRUE`, express nitrogen flows per hectare of
#'   agricultural land (kg N/ha) instead of national totals (Gg N). Requires
#'   remote data, so it is ignored in example mode. Default is `FALSE`.
#' @param example If `TRUE`, build the plot from a small example dataset without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' plot_input_output_system(example = TRUE)
plot_input_output_system <- function(per_ha = FALSE, example = FALSE) {
  df <- .load_nat_destiny(example) |>
    dplyr::filter(Province_name != "Sea")

  n_balance <- .load_n_balance(example, needed = per_ha)
  lu_area <- .national_area(n_balance, per_ha)
  per_ha <- per_ha && !is.null(lu_area)

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
    ),
    per_ha = per_ha,
    lu_area = lu_area
  )

  .stacked_area_plot(
    plot_df,
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
    ),
    breaks = c(
      "Surplus",
      "Feed",
      "Food",
      "Other_uses",
      "Export",
      "Food_import",
      "Feed_import",
      "Deposition",
      "Fixation",
      "Synthetic_fertilizer"
    ),
    labels = c(
      "Surplus",
      "Feed",
      "Food",
      "Other uses",
      "Export",
      "Food import",
      "Feed import",
      "Deposition",
      "Fixation",
      "Synthetic fertilizer"
    ),
    annotate_label = "Agro-food system",
    y_lab = if (per_ha) "kg N/ha" else "Gg N"
  )
}

#' Plot cropland and semi-natural input-output panels side by side
#'
#' @description
#' Combines [plot_input_output()] for `"Cropland"` and
#' `"semi_natural_agroecosystems"` into a single two-panel figure. When
#' `per_ha = TRUE`, each panel is normalized by its own land-use area
#' (cropland area for the left panel, semi-natural agroecosystem area for
#' the right) -- unlike [plot_input_output_total_panel()], whose two panels
#' share the same total agricultural area. The y-axis label and caption
#' only mention per-hectare units when normalization actually took effect
#' (it silently falls back to national totals if the area data could not
#' be loaded), so the figure never claims a unit it did not use.
#'
#' @param per_ha Logical. If `TRUE`, express nitrogen flows per hectare of
#'   each panel's own land-use area (kg N/ha) instead of national totals
#'   (Gg N). Requires remote data, so it is ignored in example mode.
#'   Default is `FALSE`.
#' @param example If `TRUE`, build both panels from a small example dataset
#'   without downloading remote data. Default is `FALSE`.
#'
#' @return A patchwork ggplot object.
#' @export
#'
#' @examples
#' if (
#'   requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)
#' ) {
#'   plot_input_output_land_panel(example = TRUE)
#' }
plot_input_output_land_panel <- function(per_ha = FALSE, example = FALSE) {
  rlang::check_installed(
    c("ggplot2", "patchwork"),
    "to draw the input-output land panel."
  )
  p_cropland <- plot_input_output("Cropland", per_ha, example)
  p_seminat <- plot_input_output(
    "semi_natural_agroecosystems",
    per_ha,
    example
  )
  .input_output_two_panel(
    p_cropland,
    p_seminat,
    area_labels = c("cropland", "semi-natural"),
    caption = "Each panel per hectare of its own land-use area."
  )
}

#' Plot livestock and agro-food system input-output panels side by side
#'
#' @description
#' Combines [plot_input_output_livestock()] and [plot_input_output_system()]
#' into a single two-panel figure. When `per_ha = TRUE`, both panels are
#' normalized by the same total national agricultural area -- unlike
#' [plot_input_output_land_panel()], whose two panels each use a different,
#' smaller land-use area. The y-axis label and caption only mention
#' per-hectare units when normalization actually took effect (it silently
#' falls back to national totals if the area data could not be loaded), so
#' the figure never claims a unit it did not use.
#'
#' @param per_ha Logical. If `TRUE`, express nitrogen flows per hectare of
#'   total national agricultural land (kg N/ha) instead of national totals
#'   (Gg N). Requires remote data, so it is ignored in example mode.
#'   Default is `FALSE`.
#' @param example If `TRUE`, build both panels from a small example dataset
#'   without downloading remote data. Default is `FALSE`.
#'
#' @return A patchwork ggplot object.
#' @export
#'
#' @examples
#' if (
#'   requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)
#' ) {
#'   plot_input_output_total_panel(example = TRUE)
#' }
plot_input_output_total_panel <- function(per_ha = FALSE, example = FALSE) {
  rlang::check_installed(
    c("ggplot2", "patchwork"),
    "to draw the input-output total-area panel."
  )
  p_livestock <- plot_input_output_livestock(per_ha, example)
  p_system <- plot_input_output_system(per_ha, example)
  .input_output_two_panel(
    p_livestock,
    p_system,
    area_labels = c("total agri. land", "total agri. land"),
    caption = "Both panels per hectare of total national agricultural land."
  )
}

#' Plot all four input-output panels together
#'
#' @description
#' Combines [plot_input_output()] for `"Cropland"` and
#' `"semi_natural_agroecosystems"`, [plot_input_output_livestock()], and
#' [plot_input_output_system()] into a single four-panel (2x2) figure of
#' national nitrogen totals (Gg N). There is no `per_ha` argument: per
#' hectare, the four systems do not share one area basis (Cropland and
#' semi-natural agroecosystems each use their own land-use area; Livestock
#' and the agro-food system use total agricultural area -- see
#' [plot_input_output_land_panel()] and [plot_input_output_total_panel()]),
#' so folding all four normalized panels into one grid would silently mix
#' two different denominators. Use those two functions for the per-hectare
#' view instead, kept as two separate figures for that reason.
#'
#' @param example If `TRUE`, build all four panels from a small example
#'   dataset without downloading remote data. Default is `FALSE`.
#'
#' @return A patchwork ggplot object.
#' @export
#'
#' @examples
#' if (
#'   requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)
#' ) {
#'   plot_input_output_four_panel(example = TRUE)
#' }
plot_input_output_four_panel <- function(example = FALSE) {
  rlang::check_installed(
    c("ggplot2", "patchwork"),
    "to draw the input-output four panel."
  )
  p_cropland <- plot_input_output("Cropland", example = example)
  p_seminat <- plot_input_output(
    "semi_natural_agroecosystems",
    example = example
  )
  p_livestock <- plot_input_output_livestock(example = example)
  p_system <- plot_input_output_system(example = example)

  .input_output_four_panel_cross(p_cropland, p_seminat, p_livestock, p_system)
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
          # The inedible remainder .split_food_inedible_loss() split out of
          # population_food (n_prov_destiny.R) still left the system as
          # production, so it belongs in this total too.
          "population_food_inedible",
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

.livestock_feed_inputs <- function(df) {
  df |>
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
}

.livestock_production <- function(df) {
  df |>
    dplyr::filter(
      Origin == "Livestock",
      Destiny %in%
        c(
          "population_food",
          "population_food_inedible",
          "population_other_uses",
          "export",
          "livestock_rum",
          "livestock_mono"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Type = "Production")
}

.plot_livestock_example <- function(df) {
  inputs <- .livestock_feed_inputs(df)
  production <- .livestock_production(df)
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
    fill_values = c(
      "Feed_ruminants" = "darkolivegreen3",
      "Feed_monogastric" = "#1b9e77",
      "Surplus" = "slategray",
      "Production" = "orange3"
    ),
    breaks = c("Surplus", "Production", "Feed_monogastric", "Feed_ruminants"),
    labels = c(
      "Surplus",
      "Production",
      "Feed monogastric",
      "Feed ruminants"
    ),
    annotate_label = "Livestock system"
  )
}

.plot_livestock_real <- function(df, per_ha) {
  lu_area <- .national_area(
    if (per_ha) whep_read_file("n_balance_ygpit_all") else NULL,
    per_ha
  )
  per_ha <- per_ha && !is.null(lu_area)

  item_to_type <- .livestock_prod_type_map(whep_read_file("stock_prod_ygps"))
  inputs <- .livestock_feed_by_origin(df)
  production <- .livestock_production_split(df, item_to_type)
  surplus <- .surplus_from_totals(inputs, production, positive_only = FALSE)

  feed_types <- c("Grass_local", "Crops_local", "Imports")
  plot_df <- .stack_plot_df(
    inputs,
    production,
    surplus,
    negative_types = feed_types,
    type_levels = c(
      "Imports",
      "Crops_local",
      "Grass_local",
      "Surplus",
      "Production_rum",
      "Production_mono"
    ),
    per_ha = per_ha,
    lu_area = lu_area
  )

  .stacked_area_plot(
    plot_df,
    fill_values = c(
      "Grass_local" = "darkolivegreen3",
      "Crops_local" = "#1b9e77",
      "Imports" = "steelblue3",
      "Surplus" = "slategray",
      "Production_rum" = "orange3",
      "Production_mono" = "darkorange3"
    ),
    breaks = c(
      "Surplus",
      "Production_rum",
      "Production_mono",
      "Grass_local",
      "Crops_local",
      "Imports"
    ),
    labels = c(
      "Surplus",
      "Production ruminants",
      "Production monogastric",
      "Grass local",
      "Crops local",
      "Imports"
    ),
    annotate_label = "Livestock system",
    y_lab = if (per_ha) "kg N/ha" else "Gg N"
  )
}

.livestock_feed_by_origin <- function(df) {
  df |>
    dplyr::filter(
      Destiny %in% c("livestock_rum", "livestock_mono"),
      Origin %in% c("semi_natural_agroecosystems", "Cropland", "Outside")
    ) |>
    dplyr::mutate(
      Type = dplyr::case_when(
        Origin == "semi_natural_agroecosystems" ~ "Grass_local",
        Origin == "Cropland" ~ "Crops_local",
        Origin == "Outside" ~ "Imports"
      )
    ) |>
    dplyr::group_by(Year, Type) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop")
}

.livestock_prod_type_map <- function(livestock_prod) {
  ruminant_cats <- c(
    "Cattle_meat",
    "Cattle_milk",
    "Goats",
    "Sheep",
    "Horses",
    "Donkeys_mules"
  )
  # Hogs/Other_birds are distinct Livestock_cat values from Pigs/Poultry
  # (see typologies_spain_plot.R weights); Other_birds is monogastric,
  # consistent with .add_feed() in n_prov_destiny.R. Without either, the
  # stock_prod_ygps rows for those categories fall through to NA and are
  # dropped below.
  monogastric_cats <- c(
    "Pigs",
    "Hogs",
    "Poultry",
    "Rabbits",
    "Bees",
    "Other_birds"
  )
  livestock_prod |>
    # stock_prod_ygps keys products as item_cbs; this file uses Item.
    dplyr::rename(Item = item_cbs) |>
    dplyr::distinct(Item, Livestock_cat) |>
    dplyr::mutate(
      prod_type = dplyr::case_when(
        Livestock_cat %in% ruminant_cats ~ "Production_rum",
        Livestock_cat %in% monogastric_cats ~ "Production_mono"
      )
    ) |>
    dplyr::filter(!is.na(prod_type)) |>
    dplyr::distinct(Item, prod_type)
}

.livestock_production_split <- function(df, item_to_type) {
  df |>
    dplyr::filter(
      Origin == "Livestock",
      Destiny %in%
        c(
          "population_food",
          "population_food_inedible",
          "population_other_uses",
          "export",
          "livestock_rum",
          "livestock_mono"
        )
    ) |>
    dplyr::left_join(item_to_type, by = c("Item" = "Item")) |>
    dplyr::mutate(prod_type = dplyr::coalesce(prod_type, "Production_rum")) |>
    dplyr::group_by(Year, Type = prod_type) |>
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
    # population_food_inedible is the remainder .split_food_inedible_loss()
    # (n_prov_destiny.R) split out of population_food; it still entered the
    # system as an import, so it belongs in this total too.
    c("population_food", "population_food_inedible", "population_other_uses"),
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

  # "Food" is deliberately edible-basis only here, matching {CROPS_TO_POP} /
  # {LIVESTOCK_TO_HUMAN} in the GRAFS plot: population_food_inedible
  # (.split_food_inedible_loss(), n_prov_destiny.R) is excluded, not added.
  # Surplus is a residual of the same national inputs total this Food figure
  # is subtracted from, so leaving it out of Food makes it surface as Surplus
  # automatically -- the same outcome {WASTEWATER} reaches in the GRAFS plot,
  # just via this function's own residual instead of an explicit add-back.
  human_ingestion <- df |>
    dplyr::filter(
      Destiny %in% c("population_food", "population_other_uses"),
      Origin %in% c("Cropland", "semi_natural_agroecosystems", "Livestock")
    ) |>
    dplyr::mutate(
      Type = dplyr::if_else(
        Destiny == "population_food",
        "Food",
        "Other_uses"
      )
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

  dplyr::full_join(input_sum, use_sum, by = "Year") |>
    dplyr::mutate(
      input_total = dplyr::coalesce(input_total, 0),
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
  type_levels,
  per_ha = FALSE,
  lu_area = NULL
) {
  dplyr::bind_rows(
    inputs |> dplyr::select(Year, Type, MgN),
    outputs |> dplyr::select(Year, Type, MgN),
    surplus |> dplyr::select(Year, Type, MgN)
  ) |>
    .normalize_mg_n(per_ha, lu_area) |>
    dplyr::mutate(
      MgN = dplyr::if_else(Type %in% negative_types, -MgN, MgN),
      Type = factor(Type, levels = type_levels)
    )
}

.stacked_area_plot <- function(
  plot_df,
  fill_values,
  breaks = NULL,
  labels = NULL,
  annotate_label = NULL,
  y_lab = "Gg N"
) {
  year_breaks <- seq(
    floor(min(plot_df$Year) / 20) * 20,
    ceiling(max(plot_df$Year) / 20) * 20,
    by = 20
  )

  plot <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = Year, y = MgN, fill = Type)
  ) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(x = NULL, y = y_lab, fill = "") +
    ggplot2::scale_x_continuous(breaks = year_breaks) +
    ggplot2::scale_fill_manual(
      breaks = breaks,
      labels = labels,
      values = fill_values
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 15),
      legend.key.size = ggplot2::unit(1.2, "cm"),
      axis.text = ggplot2::element_text(size = 13),
      axis.title = ggplot2::element_text(size = 16)
    )

  if (!is.null(annotate_label)) {
    plot <- plot +
      ggplot2::annotate(
        "text",
        x = -Inf,
        y = Inf,
        label = annotate_label,
        hjust = -0.05,
        vjust = 1.5,
        size = 7,
        fontface = "bold"
      )
  }

  plot
}

# Detects whether per-hectare normalization actually took effect by
# reading each panel's own y-axis label -- set to the literal "kg N/ha" by
# .stacked_area_plot() -- rather than trusting the caller's `per_ha`
# request, since plot_input_output()/plot_input_output_livestock()/
# plot_input_output_system() silently fall back to national totals when
# the area data could not be loaded. Only then does it substitute the
# area-specific label; otherwise both panels are returned unchanged, still
# in Gg N.
.label_input_output_panels <- function(p1, p2, area_labels) {
  applied <- identical(p1$labels$y, "kg N/ha") &&
    identical(p2$labels$y, "kg N/ha")
  if (applied) {
    p1 <- p1 + ggplot2::labs(y = paste("kg N / ha", area_labels[1]))
    p2 <- p2 + ggplot2::labs(y = paste("kg N / ha", area_labels[2]))
  }
  list(p1 = p1, p2 = p2, applied = applied)
}

# Combines two already-built input-output panels (from plot_input_output(),
# plot_input_output_livestock() or plot_input_output_system()) side by
# side, adding the caption only when .label_input_output_panels() found
# that per-hectare normalization actually applied to both -- so the
# caption never claims a unit the panels do not use.
.input_output_two_panel <- function(p1, p2, area_labels, caption) {
  labeled <- .label_input_output_panels(p1, p2, area_labels)
  patchwork::wrap_plots(labeled$p1, labeled$p2, nrow = 1) +
    patchwork::plot_annotation(
      caption = if (labeled$applied) caption else NULL
    )
}

# A thin grey rectangle used as the cross-shaped divider between the four
# panels in .input_output_four_panel_cross().
.input_output_divider <- function() {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "grey40", color = NA)
    )
}

# Composes the four input-output panels (top-left, top-right, bottom-left,
# bottom-right) into a 2x2 grid divided by a thin grey cross, matching
# plot_typology_periods_panel()'s .panel_periods_cross() layout.
.input_output_four_panel_cross <- function(p_tl, p_tr, p_bl, p_br) {
  divider <- .input_output_divider()
  design <- "
    AAAVBBB
    AAAVBBB
    AAAVBBB
    LLLVRRR
    CCCVDDD
    CCCVDDD
    CCCVDDD
  "
  patchwork::wrap_plots(
    A = p_tl,
    B = p_tr,
    C = p_bl,
    D = p_br,
    V = divider,
    L = divider,
    R = divider,
    design = design,
    widths = c(1, 1, 1, 0.015, 1, 1, 1),
    heights = c(1, 1, 1, 0.015, 1, 1, 1)
  )
}

.load_n_balance <- function(example, needed) {
  if (example || !needed) {
    return(NULL)
  }
  whep_read_file("n_balance_ygpit_all")
}

.system_landuse <- function(n_balance, system) {
  if (is.null(n_balance) || system == "Cropland") {
    return("Cropland")
  }
  unique(n_balance$LandUse[n_balance$LandUse != "Cropland"])
}

.national_area <- function(n_balance, per_ha, landuse = NULL) {
  if (!per_ha || is.null(n_balance)) {
    return(NULL)
  }
  .get_area_national(n_balance, landuse)
}

.get_area_national <- function(n_balance, landuse = NULL) {
  df <- n_balance
  if (!is.null(landuse)) {
    df <- dplyr::filter(df, LandUse %in% landuse)
  }
  df |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      area_ha = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )
}

.normalize_mg_n <- function(df, per_ha, lu_area) {
  if (per_ha) {
    df |>
      dplyr::left_join(lu_area, by = "Year") |>
      dplyr::mutate(MgN = MgN * 1000 / area_ha) |>
      dplyr::select(-area_ha)
  } else {
    dplyr::mutate(df, MgN = MgN / 1000)
  }
}
