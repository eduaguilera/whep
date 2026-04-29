#' Plot N inputs, production and surplus
#'
#' @param system Character. One of "Cropland" or
#'   "semi_natural_agroecosystems".
#'
#' @return A ggplot object.
#' @export
plot_input_output <- function(
  system = c("Cropland", "semi_natural_agroecosystems"),
  per_ha = FALSE
) {
  system <- match.arg(system)
  data <- create_n_nat_destiny()
  needs_n_balance <- per_ha || system == "semi_natural_agroecosystems"
  n_balance <- if (needs_n_balance) {
    whep_read_file("n_balance_ygpit_all")
  } else {
    NULL
  }
  accum <- if (system == "semi_natural_agroecosystems") {
    landuse_filter <- unique(n_balance$LandUse[n_balance$LandUse != "Cropland"])
    .calculate_n_accum(n_balance, landuse_filter)
  } else {
    tibble::tibble(year = integer(), mg_n = numeric(), Type = character())
  }

  df_system <- data |>
    dplyr::filter(
      province_name != "Sea"
    )

  inputs <- df_system |>
    dplyr::filter(
      destiny == system,
      origin %in%
        c("Deposition", "Fixation", "Synthetic", "Livestock", "People")
    ) |>
    dplyr::group_by(year, origin) |>
    dplyr::summarise(mg_n = sum(mg_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      Type = dplyr::recode(
        origin,
        "Deposition" = "Deposition",
        "Fixation" = "Fixation",
        "Synthetic" = "Synthetic_fertilizer",
        "Livestock" = "Manure",
        "People" = "Urban"
      )
    )

  residue_items <- c("Straw", "Other crop residues")

  production <- df_system |>
    dplyr::filter(
      origin == system,
      destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::mutate(
      Type = dplyr::if_else(item %in% residue_items, "Residues", "Production")
    ) |>
    dplyr::group_by(year, Type) |>
    dplyr::summarise(mg_n = sum(mg_n, na.rm = TRUE), .groups = "drop")

  input_sum <- inputs |>
    dplyr::group_by(year) |>
    dplyr::summarise(Input_Total = sum(mg_n), .groups = "drop")

  prod_sum <- production |>
    dplyr::group_by(year) |>
    dplyr::summarise(Production = sum(mg_n), .groups = "drop")

  surplus <- input_sum |>
    dplyr::left_join(prod_sum, by = "year") |>
    dplyr::left_join(
      accum |> dplyr::select(year, Accum = mg_n),
      by = "year"
    ) |>
    dplyr::mutate(
      Production = dplyr::coalesce(Production, 0),
      Accum = dplyr::coalesce(Accum, 0),
      Surplus = pmax(Input_Total - Production - Accum, 0)
    ) |>
    dplyr::select(year, Surplus) |>
    dplyr::mutate(Type = "Surplus")

  lu_area <- if (per_ha) {
    lu_filter <- if (system == "Cropland") {
      "Cropland"
    } else {
      unique(n_balance$LandUse[n_balance$LandUse != "Cropland"])
    }
    .get_area_national(n_balance, lu_filter)
  } else {
    NULL
  }

  plot_df <- dplyr::bind_rows(
    inputs |> dplyr::select(year, Type, mg_n),
    production |> dplyr::select(year, Type, mg_n),
    accum |> dplyr::select(year, Type, mg_n),
    surplus |> dplyr::rename(mg_n = Surplus)
  ) |>
    .normalize_mg_n(per_ha, lu_area) |>
    dplyr::mutate(
      mg_n = dplyr::case_when(
        Type %in%
          c(
            "Synthetic_fertilizer",
            "Manure",
            "Fixation",
            "Deposition",
            "Urban"
          ) ~ -mg_n,
        TRUE ~ mg_n
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
          "Accumulation",
          "Production",
          "Residues"
        )
      )
    )

  y_lab <- if (per_ha) "kg N/ha" else "Gg N"

  ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = mg_n, fill = Type)) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = if (system == "Cropland") "Cropland" else "Semi-natural agroecosystems",
      hjust = -0.05,
      vjust = 1.5,
      size = 3.5,
      fontface = "bold"
    ) +
    ggplot2::labs(x = NULL, y = y_lab, fill = "") +
    ggplot2::scale_fill_manual(
      values = c(
        "Synthetic_fertilizer" = "red4",
        "Manure" = "darkorange3",
        "Urban" = "darkorange4",
        "Fixation" = "olivedrab4",
        "Deposition" = "gray40",
        "Surplus" = "slategray",
        "Accumulation" = "steelblue4",
        "Residues" = "goldenrod3",
        "Production" = "orange3"
      )
    ) +
    ggplot2::theme_minimal()
}


#' Plot N inputs, production and surplus for Livestock system
#'
#' @return A ggplot object.
#' @export
plot_input_output_livestock <- function(per_ha = FALSE) {
  data <- create_n_nat_destiny()
  livestock_prod <- whep_read_file("stock_prod_ygps")
  lu_area <- if (per_ha) {
    .get_area_national(whep_read_file("n_balance_ygpit_all"))
  } else {
    NULL
  }

  df <- data |>
    dplyr::filter(province_name != "Sea")

  item_to_type <- livestock_prod |>
    dplyr::distinct(Item, Livestock_cat) |>
    dplyr::mutate(
      prod_type = dplyr::case_when(
        Livestock_cat %in%
          c(
            "Cattle_meat",
            "Cattle_milk",
            "Goats",
            "Sheep",
            "Horses",
            "Donkeys_mules"
          ) ~
          "Production_rum",
        Livestock_cat %in%
          c("Pigs", "Hogs", "Poultry", "Rabbits", "Bees") ~
          "Production_mono"
      )
    ) |>
    dplyr::filter(!is.na(prod_type)) |>
    dplyr::distinct(Item, prod_type)

  inputs <- df |>
    dplyr::filter(
      destiny %in% c("livestock_rum", "livestock_mono"),
      origin %in% c("semi_natural_agroecosystems", "Cropland", "Outside")
    ) |>
    dplyr::mutate(
      Type = dplyr::case_when(
        origin == "semi_natural_agroecosystems" ~ "Grass_local",
        origin == "Cropland" ~ "Crops_local",
        origin == "Outside" ~ "Imports"
      )
    ) |>
    dplyr::group_by(year, Type) |>
    dplyr::summarise(mg_n = sum(mg_n, na.rm = TRUE), .groups = "drop")

  production <- df |>
    dplyr::filter(
      origin == "Livestock",
      destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "export",
          "livestock_rum",
          "livestock_mono"
        )
    ) |>
    dplyr::left_join(item_to_type, by = c("item" = "Item")) |>
    dplyr::mutate(
      prod_type = dplyr::coalesce(prod_type, "Production_rum")
    ) |>
    dplyr::group_by(year, Type = prod_type) |>
    dplyr::summarise(mg_n = sum(mg_n, na.rm = TRUE), .groups = "drop")

  input_sum <- inputs |>
    dplyr::group_by(year) |>
    dplyr::summarise(Input_Total = sum(mg_n), .groups = "drop")

  prod_sum <- production |>
    dplyr::group_by(year) |>
    dplyr::summarise(Production = sum(mg_n), .groups = "drop")

  surplus <- input_sum |>
    dplyr::left_join(prod_sum, by = "year") |>
    dplyr::mutate(
      Production = dplyr::coalesce(Production, 0),
      Surplus = Input_Total - Production
    ) |>
    dplyr::select(year, Surplus) |>
    dplyr::mutate(Type = "Surplus")

  plot_df <- dplyr::bind_rows(
    inputs,
    production,
    surplus |> dplyr::rename(mg_n = Surplus)
  ) |>
    .normalize_mg_n(per_ha, lu_area) |>
    dplyr::mutate(
      mg_n = dplyr::case_when(
        Type %in% c("Grass_local", "Crops_local", "Imports") ~ -mg_n,
        TRUE ~ mg_n
      ),
      Type = factor(
        Type,
        levels = c(
          "Imports",
          "Crops_local",
          "Grass_local",
          "Surplus",
          "Production_rum",
          "Production_mono"
        )
      )
    )

  y_lab <- if (per_ha) "kg N/ha" else "Gg N"

  ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = mg_n, fill = Type)) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = "Livestock system",
      hjust = -0.05,
      vjust = 1.5,
      size = 3.5,
      fontface = "bold"
    ) +
    ggplot2::labs(x = NULL, y = y_lab, fill = "") +
    ggplot2::scale_fill_manual(
      values = c(
        "Grass_local" = "darkolivegreen3",
        "Crops_local" = "#1b9e77",
        "Imports" = "steelblue3",
        "Surplus" = "slategray",
        "Production_rum" = "orange3",
        "Production_mono" = "darkorange3"
      )
    ) +
    ggplot2::theme_minimal()
}


#' Plot N inputs and uses for full agro-food system (system level)
#'
#' @return A ggplot object.
#' @export
plot_input_output_system <- function(per_ha = FALSE) {
  data <- create_n_nat_destiny()
  n_balance <- whep_read_file("n_balance_ygpit_all")
  accum <- .calculate_n_accum(n_balance)
  lu_area <- if (per_ha) .get_area_national(n_balance) else NULL

  df <- data |>
    dplyr::filter(province_name != "Sea")

  soil_inputs <- df |>
    dplyr::filter(
      origin %in% c("Synthetic", "Fixation", "Deposition"),
      destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(year, origin) |>
    dplyr::summarise(mg_n = sum(mg_n), .groups = "drop") |>
    dplyr::mutate(
      Type = dplyr::recode(
        origin,
        "Synthetic" = "Synthetic_fertilizer",
        "Fixation" = "Fixation",
        "Deposition" = "Deposition"
      )
    )

  feed_import <- df |>
    dplyr::filter(
      origin == "Outside",
      destiny %in% c("livestock_rum", "livestock_mono")
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(mg_n = sum(mg_n), .groups = "drop") |>
    dplyr::mutate(Type = "Feed_import")

  food_import <- df |>
    dplyr::filter(
      origin == "Outside",
      destiny %in% c("population_food", "population_other_uses")
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(mg_n = sum(mg_n), .groups = "drop") |>
    dplyr::mutate(Type = "Food_import")

  inputs <- dplyr::bind_rows(
    soil_inputs |> dplyr::select(year, Type, mg_n),
    feed_import,
    food_import
  )

  livestock_ingestion <- df |>
    dplyr::filter(
      destiny %in% c("livestock_rum", "livestock_mono"),
      origin %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(mg_n = sum(mg_n), .groups = "drop") |>
    dplyr::mutate(Type = "Feed")

  human_ingestion <- df |>
    dplyr::filter(
      destiny %in% c("population_food", "population_other_uses"),
      origin %in% c("Cropland", "semi_natural_agroecosystems", "Livestock")
    ) |>
    dplyr::mutate(
      Type = dplyr::if_else(destiny == "population_food", "Food", "Other_uses")
    ) |>
    dplyr::group_by(year, Type) |>
    dplyr::summarise(mg_n = sum(mg_n), .groups = "drop")

  exports <- df |>
    dplyr::filter(destiny == "export") |>
    dplyr::group_by(year) |>
    dplyr::summarise(mg_n = sum(mg_n), .groups = "drop") |>
    dplyr::mutate(Type = "Export")

  uses_core <- dplyr::bind_rows(
    livestock_ingestion,
    human_ingestion,
    exports
  )

  input_sum <- inputs |>
    dplyr::group_by(year) |>
    dplyr::summarise(Input_Total = sum(mg_n), .groups = "drop")

  use_sum <- uses_core |>
    dplyr::group_by(year) |>
    dplyr::summarise(Use_Total = sum(mg_n), .groups = "drop")

  surplus <- input_sum |>
    dplyr::left_join(use_sum, by = "year") |>
    dplyr::left_join(
      accum |> dplyr::select(year, Accum = mg_n),
      by = "year"
    ) |>
    dplyr::mutate(
      Use_Total = dplyr::coalesce(Use_Total, 0),
      Accum = dplyr::coalesce(Accum, 0),
      mg_n = pmax(Input_Total - Use_Total - Accum, 0),
      Type = "Surplus"
    ) |>
    dplyr::select(year, Type, mg_n)

  y_lab <- if (per_ha) "kg N/ha" else "Gg N"

  plot_df <- dplyr::bind_rows(
    inputs,
    uses_core,
    accum |> dplyr::select(year, Type, mg_n),
    surplus
  ) |>
    .normalize_mg_n(per_ha, lu_area) |>
    dplyr::mutate(
      mg_n = dplyr::case_when(
        Type %in%
          c(
            "Synthetic_fertilizer",
            "Fixation",
            "Deposition",
            "Feed_import",
            "Food_import"
          ) ~ -mg_n,
        TRUE ~ mg_n
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
          "Accumulation",
          "Feed",
          "Food",
          "Other_uses",
          "Export"
        )
      )
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = mg_n, fill = Type)) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = "Agro-food system",
      hjust = -0.05,
      vjust = 1.5,
      size = 3.5,
      fontface = "bold"
    ) +
    ggplot2::labs(x = NULL, y = y_lab, fill = "") +
    ggplot2::scale_fill_manual(
      values = c(
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
      )
    ) +
    ggplot2::theme_minimal()
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
    ) |>
    dplyr::rename(year = Year)
}

.normalize_mg_n <- function(df, per_ha, lu_area) {
  if (per_ha) {
    df |>
      dplyr::left_join(lu_area, by = "year") |>
      dplyr::mutate(mg_n = mg_n * 1000 / area_ha) |>
      dplyr::select(-area_ha)
  } else {
    dplyr::mutate(df, mg_n = mg_n / 1000)
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
    dplyr::summarise(mg_n = sum(Accum_net, na.rm = TRUE), .groups = "drop") |>
    dplyr::rename(year = Year) |>
    dplyr::mutate(Type = "Accumulation")
}
