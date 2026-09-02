#' Plot national nitrogen inputs, production, and surplus for a land system.
#'
#' @description
#' Builds a stacked-area plot of Spanish national nitrogen inputs (as negative
#' values), production, residues, and surplus over time for either cropland or
#' semi-natural agroecosystems. For the semi-natural system a nitrogen
#' "Accumulation" term (net soil/biomass N accumulation) is added when the
#' `n_balance_ygpit_all` pin is available.
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

  n_balance <- .load_n_balance(
    example,
    needed = per_ha || system == "semi_natural_agroecosystems"
  )
  accum <- .accum_for_system(n_balance, system)
  lu_area <- .national_area(
    n_balance,
    per_ha,
    .system_landuse(n_balance, system)
  )
  per_ha <- per_ha && !is.null(lu_area)

  inputs <- .system_inputs(df_system, system)
  outputs <- dplyr::bind_rows(.system_production(df_system, system), accum)
  surplus <- .surplus_from_totals(inputs, outputs, positive_only = TRUE)

  input_types <- c(
    "Synthetic_fertilizer",
    "Manure",
    "Fixation",
    "Deposition",
    "Urban"
  )
  accum_level <- if (nrow(accum) > 0) "Accumulation" else character()
  plot_df <- .stack_plot_df(
    inputs,
    outputs,
    surplus,
    negative_types = input_types,
    type_levels = c(
      input_types,
      "Surplus",
      accum_level,
      "Production",
      "Residues"
    ),
    per_ha = per_ha,
    lu_area = lu_area
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
      "Accumulation" = "steelblue4",
      "Residues" = "goldenrod3",
      "Production" = "orange3"
    ),
    breaks = c(
      "Surplus",
      "Accumulation",
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
      "Accumulation",
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
#' exports) and surplus over time. A nitrogen "Accumulation" term is added
#' when the `n_balance_ygpit_all` pin is available.
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

  n_balance <- .load_n_balance(example, needed = TRUE)
  accum <- if (is.null(n_balance)) {
    .empty_accum()
  } else {
    .calculate_n_accum(n_balance)
  }
  lu_area <- .national_area(n_balance, per_ha)
  per_ha <- per_ha && !is.null(lu_area)

  inputs <- .system_level_inputs(df)
  uses_core <- dplyr::bind_rows(.system_level_uses(df), accum)
  surplus <- .surplus_from_totals(inputs, uses_core, positive_only = TRUE)

  input_types <- c(
    "Synthetic_fertilizer",
    "Fixation",
    "Deposition",
    "Feed_import",
    "Food_import"
  )
  accum_level <- if (nrow(accum) > 0) "Accumulation" else character()
  plot_df <- .stack_plot_df(
    inputs,
    uses_core,
    surplus,
    negative_types = input_types,
    type_levels = c(
      input_types,
      "Surplus",
      accum_level,
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
    title = "Spanish nitrogen inputs and outputs - Agro-food system",
    fill_values = c(
      "Synthetic_fertilizer" = "red4",
      "Fixation" = "olivedrab4",
      "Deposition" = "gray40",
      "Feed_import" = "#1b9e77",
      "Food_import" = "darkolivegreen3",
      "Accumulation" = "steelblue4",
      "Feed" = "darkorange3",
      "Food" = "darkorange4",
      "Other_uses" = "sandybrown",
      "Export" = "orange3",
      "Surplus" = "slategray"
    ),
    breaks = c(
      "Surplus",
      "Accumulation",
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
      "Accumulation",
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
    title = "Spanish nitrogen inputs and outputs - Livestock system",
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
    title = "Spanish nitrogen inputs and outputs - Livestock system",
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

  human_ingestion <- df |>
    dplyr::filter(
      Destiny %in%
        c(
          "population_food",
          # The inedible remainder .split_food_inedible_loss()
          # (n_prov_destiny.R) split out of population_food is still part of
          # "Food" for this balance -- it left the producing system exactly
          # like the edible fraction did.
          "population_food_inedible",
          "population_other_uses"
        ),
      Origin %in% c("Cropland", "semi_natural_agroecosystems", "Livestock")
    ) |>
    dplyr::mutate(
      Type = dplyr::if_else(
        Destiny %in% c("population_food", "population_food_inedible"),
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
  title,
  fill_values,
  breaks = NULL,
  labels = NULL,
  annotate_label = NULL,
  y_lab = "Gg N"
) {
  plot <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = Year, y = MgN, fill = Type)
  ) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = title, x = "Year", y = y_lab, fill = "") +
    ggplot2::scale_fill_manual(
      breaks = breaks,
      labels = labels,
      values = fill_values
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 15),
      legend.key.size = ggplot2::unit(1.2, "cm"),
      axis.text = ggplot2::element_text(size = 13)
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

.accum_for_system <- function(n_balance, system) {
  if (system != "semi_natural_agroecosystems" || is.null(n_balance)) {
    return(.empty_accum())
  }
  landuse <- unique(n_balance$LandUse[n_balance$LandUse != "Cropland"])
  .calculate_n_accum(n_balance, landuse)
}

.empty_accum <- function() {
  tibble::tibble(Year = integer(), MgN = numeric(), Type = character())
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

.calculate_n_accum <- function(n_balance, landuse = NULL) {
  df <- n_balance
  if (!is.null(landuse)) {
    df <- dplyr::filter(df, LandUse %in% landuse)
  }
  df |>
    dplyr::mutate(
      Accum_net = Accum_gain_AG_MgN + Accum_gain_BG_MgN - Accum_loss
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MgN = sum(Accum_net, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Type = "Accumulation")
}
