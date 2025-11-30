#' @title Create WHEP typologies for Spain
#'
#' @description Calculates all decision variables for the WHEP typology
#' using only production and consumption data (no import/export).
#'
#' @param prod_destiny Data frame with consumption data
#' (food, feed, other_uses).
#' @param prod_n Data frame with production data (production_n) from
#' grafs_prod_item_n.
#' @param years Numeric vector of years to include (default = 1860:2020).
#'
#' @return A data frame with Year, Province_name, decision variables,
#' and Category.
#' @export
create_typologies_whep <- function(
  prod_destiny = create_n_prov_destiny()$prod_destiny,
  prod_n = create_n_prov_destiny()$grafs_prod_item_n,
  years = 1860:2020
) {
  prod_destiny <- prod_destiny |>
    dplyr::filter(
      Year %in% years,
      Province_name != "Sea",
      Box != "Fish",
      Box != "Agro-industry"
    )

  prod_n <- prod_n |>
    dplyr::filter(Year %in% years, Province_name != "Sea")

  # --- Feed-Import ------------------------------------------------
  destiny_shares <- prod_destiny |>
    dplyr::filter(Destiny %in% c("food", "feed", "other_uses")) |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(
      total_consumption = sum(MgN, na.rm = TRUE),
      feed_share = sum(MgN[Destiny == "feed"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  import_feed_df <- prod_destiny |>
    dplyr::filter(Destiny == "import") |>
    dplyr::left_join(destiny_shares, by = c("Year", "Province_name", "Item")) |>
    dplyr::mutate(
      import_feed = MgN * feed_share
    ) |>
    dplyr::select(Year, Province_name, Item, Box, import_feed) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      feed_import = sum(import_feed, na.rm = TRUE),
      .groups = "drop"
    )

  # --- 1. Human share ------------------------------------------------------
  food_consumption <- prod_destiny |>
    dplyr::filter(Destiny %in% c("food", "other_uses")) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      food_consumption = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  production <- prod_n |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      production = sum(production_n, na.rm = TRUE),
      .groups = "drop"
    )

  human_share <- food_consumption |>
    dplyr::left_join(production, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      human_share = ifelse(production > 0, food_consumption / production, NA)
    )

  # --- 2. Woody share --------------------------------------------------------
  woody_share <- prod_destiny |>
    dplyr::filter(Box %in% c("Cropland", "Semi_natural_agroecosystems")) |>
    dplyr::mutate(
      woody = ifelse(Item %in% c("Firewood", "Acorns"), MgN, 0)
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      woody_prod = sum(woody, na.rm = TRUE),
      total_prod = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      woody_share = ifelse(total_prod > 0, woody_prod / total_prod, NA)
    )

  # --- 3. Cropland production ------------------------------------------------
  crop_feed_list <- prod_n |>
    dplyr::filter(Box == "Cropland") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      crop_prod = sum(production_n, na.rm = TRUE),
      .groups = "drop"
    )

  # --- 4. Animal ingestion / livestock feed --------------------------------
  animal_ingestion <- prod_destiny |>
    dplyr::filter(Destiny == "feed") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      animal_ingestion = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # --- 5. Grass vs crop feed -------------------------------------------------
  grass_feed <- prod_destiny |>
    dplyr::filter(
      Box == "Semi_natural_agroecosystems",
      Item == "Grassland",
      Destiny == "feed"
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      grass_feed_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  crop_feed <- prod_destiny |>
    dplyr::filter(Box == "Cropland", Destiny == "feed") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      crop_feed_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # --- Combine all decision variables ---------------------------------------
  whep_typologies <- human_share |>
    dplyr::left_join(woody_share, by = c("Year", "Province_name")) |>
    dplyr::left_join(crop_feed_list, by = c("Year", "Province_name")) |>
    dplyr::left_join(animal_ingestion, by = c("Year", "Province_name")) |>
    dplyr::left_join(import_feed_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(grass_feed, by = c("Year", "Province_name")) |>
    dplyr::left_join(crop_feed, by = c("Year", "Province_name"))

  # --- Assign WHEP Typologies ------------------------------------------------
  whep_typologies <- whep_typologies |>
    dplyr::mutate(
      import_share = feed_import / animal_ingestion,
      Category = dplyr::case_when(
        human_share > 0.9 ~ "Urban System",
        woody_share > 0.1 ~ "Woody-based system",
        import_share > 0.6 ~ "Imported feed-based system",
        crop_prod > animal_ingestion ~ "Cropland-based system",
        grass_feed_N > crop_feed_N ~ "Local grass-based livestock system",
        TRUE ~ "Local crop-based livestock system"
      )
    )

  whep_typologies
}


#' @title Plot of WHEP typology classification
#'
#' @description Generates a plot of province typologies over time based on
#' WHEP typology classification.
#'
#' @param whep_typologies A data frame returned by `create_typologies_whep()`.
#'
#' @return A plot showing province typology evolution from 1860 to 2020.
#' @keywords internal
.plot_whep_typologies <- function(whep_typologies = NULL) {
  if (is.null(whep_typologies)) {
    whep_typologies <- create_typologies_whep()
  }
  whep_typologies <- whep_typologies |>
    dplyr::filter(Province_name != "Sea") |>
    dplyr::mutate(
      human_share = tidyr::replace_na(human_share, 0),
      woody_share = tidyr::replace_na(woody_share, 0),
      Province_name = factor(
        Province_name,
        levels = sort(unique(Province_name), decreasing = TRUE)
      )
    )

  typology_colors <- c(
    "Urban System" = "#1E90FF",
    "Woody-based system" = "#8B4513",
    "Cropland-based system" = "#FFD700",
    "Local grass-based livestock system" = "#2E8B57",
    "Local crop-based livestock system" = "#C2B280",
    "Imported feed-based system" = "#FF6666"
  )

  ggplot2::ggplot(
    whep_typologies,
    ggplot2::aes(x = Year, y = Province_name, fill = Category)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_continuous(
      breaks = seq(
        min(whep_typologies$Year),
        max(whep_typologies$Year),
        by = 20
      ),
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(values = typology_colors) +
    ggplot2::labs(
      x = "Year",
      y = "Province",
      fill = "Typology",
      title = "Province type classification evolution (1860-2020)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8)
    )
}
