#' Get land footprint data for local production.
#'
#' @description
#' Read and clean the `land_fp` input file, returning only land-use
#' entries from local production (`Impact == "Land"` and
#' `Origin == "Production"`). Derived product rows from `land_fp` are excluded
#' because they encode land impacts on product output rather than physical land
#' occupation; processed products and animal products should inherit crop and
#' grass land through the input-output model.
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
      ) |>
      .drop_derived_land_extensions()
  }

  primary_double_items <- .primary_double_land_target_items()
  if (
    !isTRUE(example) &&
      any(land_fp$item_cbs_code %in% primary_double_items$item_cbs_code)
  ) {
    land_fp <- .rebuild_primary_double_land(
      land_fp,
      primary_prod = get_primary_production()
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

.drop_derived_land_extensions <- function(land_fp) {
  land_fp |>
    dplyr::filter(
      !(.data$group %in% c(
        "Crop products",
        "Crop residues",
        "Draught",
        "Livestock products"
      ))
    )
}

.rebuild_primary_double_land <- function(land_fp, primary_prod) {
  target_items <- .primary_double_land_target_items()
  target_item_codes <- unique(target_items$item_cbs_code)
  rebuilt_land <- .primary_double_land(primary_prod)

  templates <- .primary_double_land_templates(land_fp, target_items)

  rebuilt <- rebuilt_land |>
    dplyr::left_join(
      templates,
      by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::left_join(
      target_items,
      by = "item_cbs_code"
    ) |>
    dplyr::mutate(
      impact = dplyr::coalesce(.data$impact, "Land"),
      element = dplyr::coalesce(.data$element, "Cropland"),
      origin = dplyr::coalesce(.data$origin, "Production"),
      group = dplyr::coalesce(.data$group, .data$item_group, "Primary crops")
    ) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      impact,
      element,
      origin,
      group,
      impact_u
    )

  land_fp |>
    dplyr::filter(!.data$item_cbs_code %in% target_item_codes) |>
    dplyr::bind_rows(rebuilt) |>
    dplyr::summarise(
      impact_u = sum(.data$impact_u, na.rm = TRUE),
      .by = c(
        year,
        area_code,
        item_cbs_code,
        impact,
        element,
        origin,
        group
      )
    )
}

.primary_double_land <- function(primary_prod) {
  product_map <- .primary_double_product_map()
  if (nrow(product_map) == 0L) {
    return(tibble::tibble(
      year = integer(),
      area_code = integer(),
      item_cbs_code = integer(),
      impact_u = numeric()
    ))
  }

  special_land <- .primary_double_special_land(primary_prod, product_map)
  regular_land <- .primary_double_regular_land(primary_prod, product_map)

  dplyr::bind_rows(special_land, regular_land) |>
    dplyr::summarise(
      impact_u = sum(.data$impact_u, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::filter(.data$impact_u > 0)
}

.primary_double_special_land <- function(primary_prod, product_map) {
  area_map <- .primary_double_area_map()
  if (nrow(area_map) == 0L) {
    return(tibble::tibble(
      year = integer(),
      area_code = integer(),
      item_cbs_code = integer(),
      impact_u = numeric()
    ))
  }

  field_area <- primary_prod |>
    dplyr::filter(.data$unit == "ha") |>
    dplyr::inner_join(
      area_map,
      by = c(
        "item_prod_code" = "area_item_prod_code",
        "item_cbs_code" = "area_item_cbs_code"
      )
    ) |>
    dplyr::summarise(
      field_area = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, family)
    ) |>
    dplyr::filter(.data$field_area > 0)
  if (nrow(field_area) == 0L) {
    return(tibble::tibble(
      year = integer(),
      area_code = integer(),
      item_cbs_code = integer(),
      impact_u = numeric()
    ))
  }

  family_outputs <- product_map |>
    dplyr::distinct(family, item_cbs_code) |>
    dplyr::add_count(family, name = "n_outputs")

  output_production <- primary_prod |>
    dplyr::filter(.data$unit == "tonnes") |>
    dplyr::inner_join(
      product_map,
      by = c("item_prod_code", "item_cbs_code")
    ) |>
    dplyr::summarise(
      output_value = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, family, item_cbs_code)
    )

  field_area |>
    dplyr::inner_join(
      family_outputs,
      by = "family",
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      output_production,
      by = c("year", "area_code", "family", "item_cbs_code")
    ) |>
    dplyr::mutate(
      output_value = tidyr::replace_na(.data$output_value, 0),
      total_output = sum(.data$output_value, na.rm = TRUE),
      share = dplyr::if_else(
        .data$total_output > 0,
        .data$output_value / .data$total_output,
        1 / .data$n_outputs
      ),
      impact_u = .data$field_area * .data$share,
      .by = c(year, area_code, family)
    ) |>
    dplyr::select(year, area_code, item_cbs_code, impact_u)
}

.primary_double_regular_land <- function(primary_prod, product_map) {
  target_item_codes <- unique(product_map$item_cbs_code)
  special_prod_codes <- unique(product_map$item_prod_code)

  regular_map <- whep::items_prod_full |>
    dplyr::transmute(
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      item_cbs_code = .as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::filter(
      .data$item_cbs_code %in% target_item_codes,
      !.data$item_prod_code %in% special_prod_codes,
      !is.na(.data$item_prod_code),
      !is.na(.data$item_cbs_code)
    ) |>
    dplyr::distinct()

  if (nrow(regular_map) == 0L) {
    return(tibble::tibble(
      year = integer(),
      area_code = integer(),
      item_cbs_code = integer(),
      impact_u = numeric()
    ))
  }

  primary_prod |>
    dplyr::filter(.data$unit == "ha") |>
    dplyr::inner_join(
      regular_map,
      by = c("item_prod_code", "item_cbs_code")
    ) |>
    dplyr::summarise(
      impact_u = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    )
}

.primary_double_land_target_items <- function() {
  .primary_double_product_map() |>
    dplyr::distinct(item_cbs_code) |>
    dplyr::left_join(
      whep::items_full |>
        dplyr::select(item_cbs_code, item_group = group) |>
        dplyr::distinct(),
      by = "item_cbs_code"
    )
}

.primary_double_land_templates <- function(land_fp, target_items) {
  land_fp |>
    dplyr::semi_join(target_items, by = "item_cbs_code") |>
    dplyr::group_by(.data$year, .data$area_code, .data$item_cbs_code) |>
    dplyr::slice(1L) |>
    dplyr::ungroup() |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      impact,
      element,
      origin,
      group
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
