#' @title Alfredo's typology classification
#'
#' @description Calculates typologies for provinces based on grassland,
#' fertilizer, imported feed, and woody/herbaceous shares.
#'
#' @param soil_inputs A data frame containing soil nitrogen inputs.
#' @param prod_destiny A data frame containing production and destiny data.
#' @param years Years between 1860 and 2020.
#'
#' @return A data frame with the columns: Year, Province_name, grass_N,
#' fertilizer_N, feed_import_N, woody, herbaceous, woody_share, and Category.
#'
#' @export
create_alfredos_typologies <- function(
  soil_inputs = NULL,
  prod_destiny = NULL,
  years = 1860:2020
) {
  if (is.null(soil_inputs)) {
    soil_inputs <- create_n_soil_inputs()
  }

  if (is.null(prod_destiny)) {
    prod_destiny <- create_n_prov_destiny()
  }

  prod_destiny_mean <- prod_destiny |>
    dplyr::filter(Year %in% years) |>
    dplyr::group_by(Year, Province_name, Box, Item, Box_destiny, Destiny) |>
    dplyr::summarise(MgN = mean(MgN, na.rm = TRUE), .groups = "drop")

  # Grassland N
  grassland <- prod_destiny_mean |>
    dplyr::filter(
      Box == "Semi_natural_agroecosystems",
      Item == "Grassland",
      Box_destiny %in% c("semi_natural_to_livestock", "semi_natural_export")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(grass_N = sum(MgN, na.rm = TRUE), .groups = "drop")

  # Fertiliser N
  fertiliser <- soil_inputs |>
    dplyr::filter(Year %in% years) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      fertiliser_N = sum(synthetic, na.rm = TRUE),
      .groups = "drop"
    )

  # Feed and Food + Other_uses share
  destiny_shares <- prod_destiny_mean |>
    dplyr::filter(Destiny %in% c("feed", "food", "other_uses")) |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(
      total = sum(MgN, na.rm = TRUE),
      feed_share = sum(MgN[Destiny == "feed"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      human_share = sum(
        MgN[Destiny %in% c("food", "other_uses")],
        na.rm = TRUE
      ) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # Feed-import
  feed_imports <- prod_destiny_mean |>
    dplyr::filter(
      Destiny == "import",
      Box %in% c("Cropland", "Semi_natural_agroecosystems")
    ) |>
    dplyr::left_join(destiny_shares, by = c("Year", "Province_name", "Item")) |>
    dplyr::mutate(feed_import_N = MgN * feed_share) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      feed_import_N = sum(feed_import_N, na.rm = TRUE),
      .groups = "drop"
    )

  # Synthetic woody
  woody_share <- prod_destiny_mean |>
    dplyr::filter(
      Box %in% c("Cropland", "Semi_natural_agroecosystems"),
      Item %in% c("Firewood", "Acorns", "Grassland")
    ) |>
    dplyr::left_join(
      prod_destiny_mean |>
        dplyr::filter(Destiny == "import") |>
        dplyr::group_by(Year, Province_name, Item) |>
        dplyr::summarise(
          imported_MgN = sum(MgN, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      local_MgN = MgN - dplyr::coalesce(imported_MgN, 0),
      local_MgN = ifelse(local_MgN < 0, 0, local_MgN),
      woody = ifelse(Item %in% c("Firewood", "Acorns"), local_MgN, 0),
      herbaceous = ifelse(
        Item == "Grassland" & Box == "Semi_natural_agroecosystems",
        local_MgN,
        0
      )
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      woody = sum(woody, na.rm = TRUE),
      herbaceous = sum(herbaceous, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      woody_share = ifelse(
        woody + herbaceous > 0,
        woody / (woody + herbaceous),
        0
      )
    )

  alfredos_typologies <- grassland |>
    dplyr::left_join(fertiliser, by = c("Year", "Province_name")) |>
    dplyr::left_join(feed_imports, by = c("Year", "Province_name")) |>
    dplyr::left_join(woody_share, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      Category = dplyr::case_when(
        grass_N > fertiliser_N & grass_N > feed_import_N ~ "Grassland",
        fertiliser_N > feed_import_N & woody_share >= 0.5 ~ "Synthetic woody",
        fertiliser_N > feed_import_N & woody_share < 0.5 ~
          "Synthetic herbaceous",
        TRUE ~ "Imported feed"
      )
    )

  alfredos_typologies
}

#' @title Plot of Alfredo's typology classification
#'
#' @description Generates a plot of province typologies over time based on
#' Alfredo's typology classification.
#'
#' @param alfredos_typologies A data frame returned by
#' `create_alfredos_typologies()`.
#'
#' @return A plot showing province typology evolution from 1860 to 2020.
#'
#' @keywords internal
#' @noRd
.plot_province_typologies <- function(alfredos_typologies) {
  alfredos_typologies <- alfredos_typologies |>
    dplyr::mutate(
      fertiliser_N = tidyr::replace_na(fertiliser_N, 0),
      feed_import_N = tidyr::replace_na(feed_import_N, 0),
      woody_share = tidyr::replace_na(woody_share, 0),
      Province_name = factor(
        Province_name,
        levels = sort(unique(Province_name), decreasing = TRUE)
      )
    )

  typology_colors <- c(
    "Grassland" = "#2E8B57",
    "Synthetic woody" = "#8b4513",
    "Synthetic herbaceous" = "#9bb8e6",
    "Imported feed" = "#F2D16B"
  )

  ggplot2::ggplot(
    alfredos_typologies,
    ggplot2::aes(x = Year, y = Province_name, fill = Category)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_continuous(
      breaks = seq(
        min(alfredos_typologies$Year),
        max(alfredos_typologies$Year),
        by = 20
      ),
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(values = typology_colors) +
    ggplot2::labs(
      x = "Year",
      y = "Province",
      fill = "Typology",
      title = paste0(
        "Province type classification evolution (1860-2020)"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8)
    )
}
