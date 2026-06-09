#' Get land footprint data for local production.
#'
#' @description
#' Read and clean the `land_fp` input file, returning only land-use
#' entries from local production (`Impact == "Land"` and
#' `Origin == "Production"`).
#'
#' @param example If `TRUE`, return a small example output without
#'   downloading remote data. Default is `FALSE`.
#' @param grassland_metric Grassland land-extension metric. `"occupation"`
#'   returns the current LUH2 pasture+rangeland occupation area. `"active_grazing"`
#'   replaces grassland rows with the area required to supply modelled grazed
#'   grass intake at `usable_grass_yield_dm_t_ha`, capped by occupation area.
#'   `"both"` returns two scoped copies for comparing both metrics.
#' @param usable_grass_yield_dm_t_ha Usable grazed-grass dry-matter yield in
#'   tonnes per hectare. Only used when `grassland_metric` is `"active_grazing"`
#'   or `"both"`.
#'
#' @return A tibble with columns:
#' - `year`: Year.
#' - `area_code`: Numeric area code.
#' - `item_cbs_code`: Commodity balance sheet item code.
#' - `impact`: Impact category.
#' - `element`: Source element.
#' - `origin`: Origin type.
#' - `group`: Group category.
#' - `impact_u`: Land footprint value (TODO: find which unit).
#' - `extension_scope`: Land-extension scope. This is `"occupation"` by
#'   default and distinguishes the two copies when `grassland_metric = "both"`.
#'
#' @export
#'
#' @examples
#' get_land_fp_production(example = TRUE)
get_land_fp_production <- function(
  example = FALSE,
  grassland_metric = c("occupation", "active_grazing", "both"),
  usable_grass_yield_dm_t_ha = 2.06
) {
  grassland_metric <- match.arg(grassland_metric)

  land_fp <- if (example) {
    .example_land_fp_production()
  } else {
    whep_read_file("land_fp") |>
      dplyr::filter(Impact == "Land", Origin == "Production") |>
      .add_land_fp_area_code(name_column = "area", code_column = "code") |>
      dplyr::rename_with(tolower) |>
      dplyr::transmute(
        year = as.integer(year),
        area_code = as.integer(code),
        item_cbs_code = as.integer(item_code),
        impact,
        element,
        origin,
        group,
        impact_u
      )
  }

  .apply_grassland_metric(
    land_fp,
    grassland_metric,
    usable_grass_yield_dm_t_ha,
    example = example
  )
}

.example_land_fp_production <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact, ~element, ~origin, ~group, ~impact_u,
    2020L, 32L, 2511L, "Land", "Cropland", "Production", "Crops", 1000,
    2020L, 76L, 2514L, "Land", "Cropland", "Production", "Crops", 800,
    2020L, 32L, 3000L, "Land", "Production", "Production", "Grass", 50
  )
}

.apply_grassland_metric <- function(
  land_fp,
  grassland_metric,
  usable_grass_yield_dm_t_ha,
  example = FALSE
) {
  occupation <- .set_extension_scope(land_fp, "occupation")
  if (grassland_metric == "occupation") {
    return(occupation)
  }

  .check_usable_grass_yield(usable_grass_yield_dm_t_ha)
  feed_intake <- if (isTRUE(example)) {
    get_feed_intake(example = TRUE)
  } else {
    get_feed_intake()
  }
  active <- .active_grazing_land_fp(
    land_fp,
    feed_intake,
    usable_grass_yield_dm_t_ha
  )

  if (grassland_metric == "active_grazing") {
    return(active)
  }

  dplyr::bind_rows(occupation, active)
}

.active_grazing_land_fp <- function(
  land_fp,
  feed_intake,
  usable_grass_yield_dm_t_ha
) {
  grass_mask <- .grass_land_rows(land_fp)
  grass <- dplyr::filter(land_fp, grass_mask)
  other <- dplyr::filter(land_fp, !grass_mask)

  intake <- feed_intake |>
    dplyr::filter(.data$feed_type == "grass") |>
    dplyr::summarise(
      grazing_intake_dm_t = sum(.data$intake_dry_matter, na.rm = TRUE),
      .by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code)
    )

  grass_active <- grass |>
    dplyr::left_join(
      intake,
      by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(
      grazing_intake_dm_t = tidyr::replace_na(.data$grazing_intake_dm_t, 0),
      impact_u_occupation = .data$impact_u,
      usable_grass_yield_dm_t_ha = usable_grass_yield_dm_t_ha,
      impact_u = pmin(
        .data$impact_u_occupation,
        .data$grazing_intake_dm_t / .data$usable_grass_yield_dm_t_ha,
        na.rm = TRUE
      ),
      active_fraction = dplyr::if_else(
        .data$impact_u_occupation > 0,
        .data$impact_u / .data$impact_u_occupation,
        NA_real_
      )
    )

  dplyr::bind_rows(other, grass_active) |>
    .set_extension_scope("active_grazing")
}

.grass_land_rows <- function(land_fp) {
  tidyr::replace_na(land_fp$group == "Grass", FALSE) |
    land_fp$item_cbs_code %in% c(3000L, 3002L)
}

.set_extension_scope <- function(land_fp, extension_scope) {
  land_fp |>
    dplyr::mutate(extension_scope = extension_scope)
}

.check_usable_grass_yield <- function(usable_grass_yield_dm_t_ha) {
  if (
    !is.numeric(usable_grass_yield_dm_t_ha) ||
      length(usable_grass_yield_dm_t_ha) != 1L ||
      is.na(usable_grass_yield_dm_t_ha) ||
      usable_grass_yield_dm_t_ha <= 0
  ) {
    cli::cli_abort(
      "{.arg usable_grass_yield_dm_t_ha} must be one positive number."
    )
  }
}

.add_land_fp_area_code <- function(
  data,
  name_column = "area",
  code_column = "code"
) {
  bridge <- .land_fp_area_code_bridge(name_column, code_column)

  data |>
    dplyr::left_join(bridge, by = name_column)
}

.land_fp_area_code_bridge <- function(
  name_column = "area",
  code_column = "code"
) {
  alias_cols <- intersect(
    c("polity_name", "FAOSTAT_name", "name"),
    names(whep::regions_full)
  )
  polity_codes <- whep::polities |>
    dplyr::transmute(
      polity_code = .data$iso3c,
      !!code_column := as.integer(.data$area_code)
    )

  whep::regions_full |>
    dplyr::select(
      "polity_code",
      dplyr::all_of(alias_cols)
    ) |>
    dplyr::filter(!is.na(.data$polity_code)) |>
    dplyr::left_join(polity_codes, by = "polity_code") |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(alias_cols),
      values_to = name_column,
      names_to = "source"
    ) |>
    dplyr::filter(
      !is.na(.data[[name_column]]),
      nzchar(.data[[name_column]]),
      !is.na(.data[[code_column]])
    ) |>
    dplyr::mutate(
      !!code_column := as.integer(.data[[code_column]])
    ) |>
    dplyr::distinct(.data[[name_column]], .data[[code_column]]) |>
    dplyr::add_count(.data[[name_column]], name = "n_codes") |>
    dplyr::filter(.data$n_codes == 1L) |>
    dplyr::select(-"n_codes")
}
