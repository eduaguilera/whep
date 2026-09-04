#' @title Typologies of Julia
#'
#' @description
#' Typologies of provinces in Spain based on nitrogen (N) production
#' data of crops and livestock, using various input datasets and generating
#' classification maps and data frames.
#'
#' @param make_map If TRUE a map of the typologies will be created.
#'
#' @param shapefile_path Optional path to a Natural Earth 10m admin-1
#'   states/provinces shapefile. When `NULL` (default) the layer is
#'   downloaded from <https://www.naturalearthdata.com> on first use and
#'   cached locally; set `options(whep.provinces_shapefile = )` to point at
#'   an existing copy instead.
#'
#' @param map_year The year for which the typology map is created.
#'
#' @param example If `TRUE`, return a small example output without reading the
#'   remote inputs or the Natural Earth layer. Default is `FALSE`.
#'
#' @returns
#' A tibble with the classification of Spanish provinces into typologies for
#' `map_year`. It contains the following columns:
#' - `Province_name`: The name of the Spanish province.
#' - `Typologie`: Assigned typology category for each province. This is based
#'                on thresholds in livestock density (livestock units per
#'                hectare of agricultural area), crop N productivity (kg N
#'                harvested per hectare of cropland), and the semi-natural and
#'                imported shares of feed. The typologies are:
#'   - `Specialized cropping system`
#'   - `Extensive cropping system`
#'   - `Extensive mixed crop-livestock system`
#'   - `Intensive mixed crop-livestock system`
#'   - `Specialized livestock-farming system`
#'
#' @export
#'
#' @examples
#' create_typologies_grafs_spain(example = TRUE)
create_typologies_grafs_spain <- function(
  make_map = TRUE,
  shapefile_path = NULL,
  map_year = 1980,
  example = FALSE
) {
  if (example) {
    return(.ex_typologies_grafs_spain())
  }
  shapefile_path <- .provinces_shapefile(shapefile_path)

  # Load datasets
  data <- .load_inputs_typologies_julia(shapefile_path)
  data$sf_provinces <- data$sf_provinces_spain

  data$sf_provinces$name <- stringi::stri_trans_general(
    data$sf_provinces$name,
    "Latin-ASCII"
  )
  data$Livestock_Prod_ygps$Province_name <- stringi::stri_trans_general(
    data$Livestock_Prod_ygps$Province_name,
    "Latin-ASCII"
  )
  data$sf_provinces$name <- gsub(" ", "_", data$sf_provinces$name)
  data$sf_provinces$name[data$sf_provinces$name == "La_Rioja"] <- "Rioja"
  data$sf_provinces$name[data$sf_provinces$name == "Alava"] <- "Araba"
  data$sf_provinces$name[data$sf_provinces$name == "Lerida"] <- "Lleida"
  data$sf_provinces$name[data$sf_provinces$name == "Castellon"] <- "Castello"
  data$sf_provinces$name[data$sf_provinces$name == "La_Coruna"] <- "A_Coruna"
  data$sf_provinces$name[data$sf_provinces$name == "Orense"] <- "Ourense"
  data$sf_provinces$name[data$sf_provinces$name == "Gerona"] <- "Girona"
  data$sf_provinces <- data$sf_provinces[
    !data$sf_provinces$name %in%
      c(
        "Las_Palmas",
        "Tenerife"
      ),
  ]

  # Prepare LU coefficients with Livestock_cat mapping
  lu_coefs_mapped <- .prepare_lu_coefs(data$livestock_units)

  # Merge livestock data with LU coefficients and calculate totals
  lu_totals_detailed <- .calculate_lu_totals(
    data$Livestock_Prod_ygps,
    lu_coefs_mapped
  )

  # Aggregate LU_total per Year, Province
  lu_aggregated <- .aggregate_lu_totals(lu_totals_detailed)

  # Aggregate Area
  area_aggregated <- .aggregate_area_aa(data$NPP_ygpit)

  # Calculate livestock density
  livestock_density <- .calculate_livestock_density(
    lu_aggregated,
    area_aggregated
  )

  # Calculate cropland productivity
  cropland_productivity <- .aggregate_crop_productivity(data$NPP_ygpit)

  # Aggregate feed from semi natural agroecosystems
  semi_natural_feed <- .aggregate_semi_nat_feed_mgn(data$GRAFS_Prod_Destiny_git)

  cropland_feed <- .aggregate_cropland_feed_mgn(data$GRAFS_Prod_Destiny_git)

  # Calculate feed share (semi_natural feed / total feed)
  feed_share <- .calculate_semi_nat_feed_share(data$GRAFS_Prod_Destiny_git)

  # Use feed supply from GRAFS_Prod_Destiny_git + LU data
  feed_domestic_prov <- .calculate_feed_domest_supply(
    data$GRAFS_Prod_Destiny_git,
    lu_aggregated
  )

  # Calculate feed import per province based on national imports & LU shares
  feed_import_by_province <- .calculate_feed_import_share(
    data$PIE_FullDestinies_FM,
    lu_aggregated
  )

  # Calculate imported feed share at province level
  feed_imported_share <- .calculate_imported_feed_share(
    feed_import_by_province,
    feed_domestic_prov
  )

  typologies_result <- .assign_decision_tree(
    livestock_density,
    cropland_productivity,
    feed_share,
    feed_imported_share,
    sf_provinces = data$sf_provinces_spain,
    year = map_year
  )

  if (make_map) {
    typologies_df <- typologies_result$Typologies
    map_plot <- typologies_result$Typologies_map
  } else {
    typologies_df <- typologies_result$Typologies
    map_plot <- NULL
  }

  typologies_df
}

#' @title Load input datasets --------------------------------------------------
#'
#' @param shapefile_path The local path where the input data are located.
#'
#' @keywords internal
#' @noRd
.load_inputs_typologies_julia <- function(shapefile_path) {
  list(
    Livestock_Prod_ygps = whep_read_file("livestock_prod_ygps"),
    livestock_units = whep_read_file("livestock_units"),
    NPP_ygpit = whep_read_file("npp_ygpit"),
    GRAFS_Prod_Destiny_git = .grafs_prod_destiny_legacy(),
    PIE_FullDestinies_FM = whep_read_file("pie_full_destinies_fm"),
    sf_provinces_spain = .read_spain_provinces(shapefile_path)
  )
}

#' @title Read the Spanish provinces layer -------------------------------------
#'
#' @param shapefile_path Path to a Natural Earth admin-1 shapefile.
#'
#' @keywords internal
#' @noRd
.read_spain_provinces <- function(shapefile_path) {
  layer_name <- tools::file_path_sans_ext(basename(shapefile_path))

  sf::st_read(
    shapefile_path,
    query = paste0("SELECT * FROM ", layer_name, " WHERE iso_a2 = 'ES'")
  )
}

#' @title Legacy view of the GRAFS N flows -------------------------------------
#'
#' @description
#' Both typologies were written against the precomputed
#' `GRAFS_Prod_Destiny_git.csv` file. `create_n_prov_destiny()` now computes
#' the same flows, but with a different schema and vocabulary, so the whole
#' translation lives here instead of in every downstream helper:
#'
#' - `destiny == "population_food"` (and its `population_food_inedible`
#'   remainder, see `.split_food_inedible_loss()`) becomes `Destiny ==
#'   "Food"`.
#' - `destiny == "population_other_uses"` becomes `Destiny == "Other_uses"`.
#' - `destiny` in `livestock_rum`/`livestock_mono` becomes `Destiny == "Feed"`.
#' - `destiny == "export"` becomes `Destiny == "Export"`.
#' - `box == "semi_natural_agroecosystems"` becomes
#'   `Box == "Semi_natural_agroecosystems"`.
#' - Soil-input rows (origin Deposition, Fixation, Synthetic, Livestock,
#'   People) are dropped: the legacy file did not contain them.
#'
#' The legacy file had no `"Import"` destiny either. Its Food/Feed/Other_uses
#' rows are gross use (imports included) and imports are repeated as separate
#' `"Import"` rows, which is why downstream code computes production as
#' `Food + Feed + Other_uses + Export - Import`. Imported flows are
#' `origin == "Outside"` here and already carry their real destiny, so they
#' are emitted twice, once under that destiny and once as `"Import"`, which
#' reproduces the legacy convention exactly rather than double counting.
#'
#' @param prod_destiny Tibble of N flows from `create_n_prov_destiny()`.
#'
#' @keywords internal
#' @noRd
.grafs_prod_destiny_legacy <- function(
  prod_destiny = create_n_prov_destiny()
) {
  legacy_destiny <- c(
    population_food = "Food",
    # population_food_inedible is the remainder .split_food_inedible_loss()
    # (n_prov_destiny.R) split out of population_food; the legacy vocabulary
    # predates that split, so fold it back into "Food" or it would be
    # silently dropped by the names(legacy_destiny) filter below.
    population_food_inedible = "Food",
    population_other_uses = "Other_uses",
    livestock_rum = "Feed",
    livestock_mono = "Feed",
    export = "Export"
  )

  flows <- prod_destiny |>
    .rename_destiny_pascal() |>
    dplyr::filter(Destiny %in% names(legacy_destiny)) |>
    dplyr::mutate(
      Box = dplyr::if_else(
        Box == "semi_natural_agroecosystems",
        "Semi_natural_agroecosystems",
        Box
      )
    )

  dplyr::bind_rows(
    flows |> dplyr::mutate(Destiny = unname(legacy_destiny[Destiny])),
    flows |>
      dplyr::filter(Origin == "Outside") |>
      dplyr::mutate(Destiny = "Import")
  ) |>
    dplyr::summarise(
      MgN = sum(MgN, na.rm = TRUE),
      .by = c("Year", "Province_name", "Item", "Box", "Destiny")
    )
}

#' @title Prepare LU coefficients with Livestock_cat mapping -------------------
#'
#' @description
#' The `livestock_units` pin already maps each `Livestock_cat` to its
#' `Animal_class` and livestock-unit coefficient, so no hardcoded mapping is
#' needed.
#'
#' @param livestock_units_df Tibble from the `livestock_units` pin.
#' @keywords internal
#' @noRd
.prepare_lu_coefs <- function(livestock_units_df) {
  livestock_units_df |>
    dplyr::select(Livestock_cat, Animal_class, LU_head) |>
    dplyr::distinct()
}

#' @title Calculate LU_total per row -------------------------------------------
#'
#' @param livestock_df A data frame containing livestock data.
#' @param lu_coefs_df A data frame with livestock unit coefficients.
#'
#' @return A tibble with columns 'Year', 'Province_name', 'Livestock_cat',
#' 'Animal_class', Stock_Number', 'LU_head', and 'LU_total'.
#' @keywords internal
#' @noRd
.calculate_lu_totals <- function(livestock_df, lu_coefs_df) {
  livestock_df |>
    dplyr::select(Year, Province_name, Livestock_cat, Stock_Number) |>
    dplyr::left_join(lu_coefs_df, by = "Livestock_cat") |>
    dplyr::mutate(LU_total = Stock_Number * LU_head) |>
    dplyr::select(
      Year,
      Province_name,
      Livestock_cat,
      Animal_class,
      Stock_Number,
      LU_head,
      LU_total
    ) |>
    dplyr::distinct()
}

#' @title Aggregate LU_total ---------------------------------------------------
#' @description Aggregates total land use (LU_total) by year and province.
#'
#' @param lu_detailed_df A data frame containing columns 'Year',
#' 'Province_name', and 'LU_total'.
#'
#' @return A tibble with total land use summed for each year and province,
#' sorted by year and province.
#' @keywords internal
#' @noRd
.aggregate_lu_totals <- function(lu_detailed_df) {
  lu_aggregated <- lu_detailed_df |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      LU_total = sum(
        LU_total,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(Year, Province_name)

  lu_aggregated
}

#' @title Aggregate Area AA ----------------------------------------------------
#' @description Aggregates the area (Area_ygpit_ha) by year and province.
#'
#' @param npp_df A data frame containing the columns 'Year', 'Province_name',
#' and 'Area_ygpit_ha'.
#'
#' @return A tibble with the sum of areas per year and province.
#' @keywords internal
#' @noRd
.aggregate_area_aa <- function(npp_df) {
  npp_df |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Area_ha = sum(
        Area_ygpit_ha,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

#' @title Calculate livestock density ------------------------------------------
#'
#' @param lu_totals_df A data frame containing livestock total data.
#' @param area_df A data frame containing area information.
#'
#' @return A tibble with columns 'Year', 'Province_name', 'LU_total', 'Area_ha',
#' and 'Livestock_density' (LU_total divided by Area_ha).
#' @keywords internal
#' @noRd
.calculate_livestock_density <- function(lu_totals_df, area_df) {
  lu_totals_df |>
    dplyr::left_join(area_df, by = c("Year", "Province_name")) |>
    dplyr::mutate(Livestock_density = LU_total / Area_ha) |>
    dplyr::select(Year, Province_name, LU_total, Area_ha, Livestock_density) |>
    dplyr::arrange(Year, Province_name)
}

#' @title Aggregate Productivity for Cropland ----------------------------------
#'
#' @param npp_df A data frame containing columns 'Year', 'Province_name',
#' 'LandUse','Prod_MgN', and 'Area_ygpit_ha'.
#'
#' @return A tibble grouped by year and province with total production,
#' total cropland area, and productivity in kg N per hectare.
#' @keywords internal
#' @noRd
.aggregate_crop_productivity <- function(npp_df) {
  cropland_prod <- npp_df |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Prod_MgN_total = sum(Prod_MgN, na.rm = TRUE),
      Area_ha_cropland = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Productivity_kgN_ha = Prod_MgN_total / Area_ha_cropland * 1000
    ) |>
    dplyr::arrange(Year, Province_name)

  cropland_prod
}

#' @title Aggregate Feed from Semi natural agroecosystems for Grassland > 60%
#' of Livestock intake from Grassland
#'
#' @param df A data frame containing columns'Year','Province_name','Box',
#'' Destiny', and'MgN'.
#'
#' @return A tibble grouped by year and province with the total feed nitrogen
#' (MgN) from semi-natural agroecosystems.
#' @keywords internal
#' @noRd
.aggregate_semi_nat_feed_mgn <- function(df) {
  df |>
    dplyr::filter(Box == "Semi_natural_agroecosystems", Destiny == "Feed") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Semi_nat_feed_MgN = sum(
        MgN,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

#' @title Aggregate Feed from Cropland ----------------------------------------
#'
#' @param df A data frame containing columns 'Year', 'Province_name',
#' 'Box', 'Destiny', and 'MgN'.
#'
#' @return A tibble grouped by year and province with the total cropland feed
#' nitrogen (MgN).
#' @keywords internal
#' @noRd
.aggregate_cropland_feed_mgn <- function(df) {
  df |>
    dplyr::filter(Box == "Cropland", Destiny == "Feed") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Cropland_feed_MgN = sum(
        MgN,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

#' Aggregate total feed from all boxes (Feed destiny) -------------------------
#'
#' @param df A data frame containing nitrogen data with columns including
#' 'Year', 'Province_name', 'Destiny', 'Box', and 'MgN'.
#'
#' @return A tibble with total feed nitrogen (MgN) summed by year and province.
#' @keywords internal
#' @noRd
.aggregate_total_feed_mgn <- function(df) {
  df |>
    dplyr::filter(
      Destiny == "Feed",
      Box %in% c("Semi_natural_agroecosystems", "Cropland")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Total_feed_MgN = sum(
        MgN,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(Year, Province_name)
}

#' @title Calculate Feed share (between semi natural agroecosystems and total
#' feed)
#'
#' @param df A data frame containing nitrogen data with columns including
#' 'Year', 'Province_name', 'Destiny', 'Box', and 'MgN'.
#'
#' @return A data frame including the share of feed from semi natural
#' agroecosystems
#' @keywords internal
#' @noRd
.calculate_semi_nat_feed_share <- function(df) {
  total_feed <- .aggregate_total_feed_mgn(df)
  semi_nat_feed <- .aggregate_semi_nat_feed_mgn(df)

  dplyr::left_join(
    total_feed,
    semi_nat_feed,
    by = c("Year", "Province_name")
  ) |>
    dplyr::mutate(
      Semi_nat_feed_MgN = ifelse(
        is.na(Semi_nat_feed_MgN),
        0,
        Semi_nat_feed_MgN
      ),
      Semi_nat_share = Semi_nat_feed_MgN / Total_feed_MgN
    ) |>
    dplyr::arrange(Year, Province_name)
}

#' @title Calculate feed domestic supply ---------------------------------------
#'
#' @param grafs_df A data frame containing GRAFS data with the columns Destiny',
#' 'Year', 'Province_name', and 'MgN'.
#' @param lu_df A data frame with land use data.
#'
#' @return A tibble with columns 'Year', 'Province_name', and
#' 'Domestic_feed_MgN' representing the total domestic feed supply in MgN.
#' @keywords internal
#' @noRd
.calculate_feed_domest_supply <- function(grafs_df, lu_df) {
  domestic_feed <- grafs_df |>
    dplyr::filter(Destiny == "Feed") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Domestic_feed_MgN = sum(
        MgN,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  # Add LU_total for use in further steps
  domestic_feed |>
    dplyr::left_join(
      lu_df |> dplyr::select(Year, Province_name, LU_total),
      by = c("Year", "Province_name")
    )
}

#' @title Calculate feed import per province -----------------------------------
#'
#' @param feed_df A data frame containing feed data.
#' @param lu_df A data frame with land use information.
#'
#' @return A tibble with columns 'Year', 'Province_name', 'LU_total',
#' 'LU_share', and 'Feed_import_MgN', where 'Feed_import_MgN' is the estimated
#'  feed import allocated to each province.
#' @keywords internal
#' @noRd
.calculate_feed_import_share <- function(feed_df, lu_df) {
  feed_filtered <- feed_df |>
    dplyr::filter(Element == "Import", Destiny == "Feed") |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      Total_feed_import = sum(
        Value_destiny,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  total_lu_spain <- lu_df |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      LU_total_spain = sum(
        LU_total,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  lu_with_share <- lu_df |>
    dplyr::left_join(total_lu_spain, by = "Year") |>
    dplyr::mutate(LU_share = LU_total / LU_total_spain)

  feed_import_by_province <- lu_with_share |>
    dplyr::left_join(feed_filtered, by = "Year") |>
    dplyr::mutate(Feed_import_MgN = LU_share * Total_feed_import) |>
    dplyr::select(Year, Province_name, LU_total, LU_share, Feed_import_MgN)

  feed_import_by_province
}

#' @title Calculate feed share of imported/consumed feed -----------------------
#'
#' @param feed_import_by_province A data frame containing imported feed data.
#' @param domestic_feed_by_province A data frame containing domestic feed data.
#'
#' @return A data frame with the imported feed share.
#' @keywords internal
#' @noRd
.calculate_imported_feed_share <- function(
  feed_import_by_province,
  domestic_feed_by_province
) {
  # Both inputs carry LU_total, derived from the same aggregated LU table, so
  # joining them unsuffixed would produce LU_total.x / LU_total.y and the
  # select() below would find no LU_total at all. Keep the import-side copy.
  feed_import_by_province |>
    dplyr::left_join(
      domestic_feed_by_province |> dplyr::select(-dplyr::any_of("LU_total")),
      by = c("Year", "Province_name")
    ) |>
    dplyr::mutate(
      Total_feed_MgN = Domestic_feed_MgN + Feed_import_MgN,
      Imported_feed_share = Feed_import_MgN / Total_feed_MgN,
      Imported_feed_share = ifelse(
        is.nan(Imported_feed_share),
        NA,
        Imported_feed_share
      )
    ) |>
    dplyr::select(
      Year,
      Province_name,
      LU_total = LU_total,
      Feed_import_MgN,
      Domestic_feed_MgN,
      Total_feed_MgN,
      Imported_feed_share
    )
}

#' @title Assign Typologies and optionally plot map----------------------------
#'
#' @param livestock_density A data frame with livestock density values.
#' @param productivity A data frame with productivity (kgN/ha) values.
#' @param semi_nat_share A data frame with semi-natural agroecosystem share.
#' @param imported_feed_share A data frame with share of imported feed.
#' @param sf_provinces An sf object with province geometries.
#' @param year Integer specifying the year for which typologies are assigned.
#'
#' @return A tibble with province names and their assigned farming system
#' typology for the specified year.
#' @keywords internal
#' @noRd
.assign_decision_tree <- function(
  livestock_density,
  productivity,
  semi_nat_share,
  imported_feed_share,
  sf_provinces,
  year
) {
  typologies <- livestock_density |>
    dplyr::inner_join(productivity, by = c("Year", "Province_name")) |>
    dplyr::inner_join(semi_nat_share, by = c("Year", "Province_name")) |>
    dplyr::inner_join(imported_feed_share, by = c("Year", "Province_name")) |>
    dplyr::filter(Year == year) |>
    dplyr::mutate(
      Typologie = dplyr::case_when(
        Livestock_density < 0.4 &
          Productivity_kgN_ha > 60 ~
          "Specialized cropping system",
        Livestock_density < 0.4 &
          Productivity_kgN_ha <= 60 ~
          "Extensive cropping system",
        Livestock_density >= 0.4 &
          Semi_nat_share > 0.6 ~
          "Extensive mixed crop-livestock system",
        Livestock_density >= 0.4 &
          Semi_nat_share <= 0.6 &
          Imported_feed_share < 0.5 ~
          "Intensive mixed crop-livestock system",
        Livestock_density >= 0.4 &
          Semi_nat_share <= 0.6 &
          Imported_feed_share >= 0.5 ~
          "Specialized livestock-farming system",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(Province_name, Typologie)

  sf_provinces_filtered <- sf_provinces |>
    dplyr::rename(Province_name = name) |>
    dplyr::inner_join(typologies, by = "Province_name")

  map <- ggplot2::ggplot(sf_provinces_filtered) +
    ggplot2::geom_sf(ggplot2::aes(fill = Typologie), color = "white") +
    ggplot2::scale_fill_manual(
      values = c(
        "Specialized cropping system" = "#FFD700",
        "Extensive cropping system" = "#FFFF99",
        "Extensive mixed crop-livestock system" = "#66a61e",
        "Intensive mixed crop-livestock system" = "#d95f02",
        "Specialized livestock-farming system" = "#7570b3"
      ),
      na.value = "grey80"
    ) +
    ggplot2::labs(
      title = paste("Typologies by Province -", year),
      fill = "Typologie"
    ) +
    ggplot2::theme_minimal()

  # Typologies for each year as a dataset
  typologies_all_years <- livestock_density |>
    dplyr::inner_join(productivity, by = c("Year", "Province_name")) |>
    dplyr::inner_join(semi_nat_share, by = c("Year", "Province_name")) |>
    dplyr::inner_join(imported_feed_share, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      Typologie = dplyr::case_when(
        Livestock_density < 0.4 &
          Productivity_kgN_ha > 60 ~
          "Specialized cropping system",
        Livestock_density < 0.4 &
          Productivity_kgN_ha <= 60 ~
          "Extensive cropping system",
        Livestock_density >= 0.4 &
          Semi_nat_share > 0.6 ~
          "Extensive mixed crop-livestock system",
        Livestock_density >= 0.4 &
          Semi_nat_share <= 0.6 &
          Imported_feed_share < 0.5 ~
          "Intensive mixed crop-livestock system",
        Livestock_density >= 0.4 &
          Semi_nat_share <= 0.6 &
          Imported_feed_share >= 0.5 ~
          "Specialized livestock-farming system",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(Year, Province_name, Typologie) |>
    dplyr::arrange(Year, Province_name)

  list(
    Typologies = typologies,
    Typologies_map = map,
    Typologies_all_years = typologies_all_years
  )
}
