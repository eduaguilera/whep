#' @title GRAFS Nitrogen (N) flows – National Spain
#'
#' @description
#' Provides N flows of the Spanish agro-food system on a national level
#' between 1860 and 2020. This dataset is the national equivalent of the
#' provincial GRAFS model and represents Spain as a single system without
#' internal trade between provinces. All production, consumption and soil
#' inputs are aggregated nationally before calculating trade with the
#' outside.
#'
#' @return
#' A final tibble containing national N flow data by origin and destiny.
#'
#' @export
#' @title GRAFS Nitrogen (N) flows – National Spain (clean)
#'
#' @description
#' National GRAFS N flows built by reusing the provincial pipeline,
#' aggregating to Spain BEFORE trade. Structure is identical to
#' create_n_prov_destiny().
#'
#' @export
create_n_nat_destiny <- function() {
  prov <- create_n_prov_destiny()

  nat_shares <- prov |>
    dplyr::filter(
      Destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono"
        )
    ) |>
    dplyr::group_by(Year, Item, Destiny) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(Year, Item) |>
    dplyr::mutate(share = MgN / sum(MgN, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(Year, Item, Destiny, share)

  nat_core <- prov |>
    dplyr::filter(Origin != "Outside", Destiny != "export") |>
    dplyr::group_by(
      Year,
      Item,
      Irrig_cat,
      Box,
      Origin,
      Destiny
    ) |>
    dplyr::summarise(
      MgN = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(Province_name = "Spain")

  nat_balance <- prov |>
    dplyr::group_by(Year, Item, Box, Irrig_cat) |>
    dplyr::summarise(
      production = sum(MgN[Origin == Box], na.rm = TRUE),
      consumption = sum(
        MgN[
          Destiny %in%
            c(
              "population_food",
              "population_other_uses",
              "livestock_rum",
              "livestock_mono"
            )
        ],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      export = pmax(production - consumption, 0),
      import = pmax(consumption - production, 0),
      Province_name = "Spain"
    )

  exports <- nat_balance |>
    dplyr::filter(export > 0) |>
    dplyr::transmute(
      Year,
      Province_name,
      Item,
      Irrig_cat,
      Box,
      Origin = Box,
      Destiny = "export",
      MgN = export
    )

  imports <- nat_balance |>
    dplyr::filter(import > 0) |>
    dplyr::left_join(nat_shares, by = c("Year", "Item")) |>
    dplyr::mutate(
      MgN = pmin(import, consumption) * share,
      Origin = "Outside",
      Irrig_cat = NA_character_
    ) |>
    dplyr::filter(MgN > 0) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Irrig_cat,
      Box,
      Origin,
      Destiny,
      MgN
    )

  dplyr::bind_rows(
    nat_core,
    exports,
    imports
  ) |>
    dplyr::arrange(Year, Item, Origin, Destiny)
}
