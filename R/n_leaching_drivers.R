# The leaching drivers of the Meisinger drainage cascade and of IMAGE-GNM's
# root zone, built by WHEP on the balance grid key for build_n_loss_drivers().
#
# Every driver is a source layer joined on the cell (lon, lat), the polity
# (area_code), the commodity balance item and the year, never on a name:
# - LPJmL deep seepage (`drainage_mm`, Meisinger) and total runoff
#   (`runoff_mm`, IMAGE-GNM) are whole-cell quantities, so both water regime
#   rows of a cell share them. A balance cell off the LPJmL grid takes its
#   nearest LPJmL land cell's water, stamped with the distance.
# - The HWSD layers (hydraulic properties, FAO texture and drainage class,
#   topsoil mass) and IMAGE-GNM's runoff fraction and slope are static. A
#   cell a layer lacks takes its nearest neighbour's value, stamped with the
#   distance, as read_soil_hydraulic() does for its own target grid.
# - Soil organic carbon is the carbon balance's 0-30 cm stock of the row's
#   land use (cropland for crop rows, grassland for the grass item) over the
#   HWSD topsoil mass; soil organic matter is that carbon times the SOM:SOC
#   factor of n_attenuation_constants.
# - The C input of the C:N attenuation is the crop's own soil carbon input
#   (build_soil_carbon_inputs(), pooled to the commodity balance item) on crop
#   rows and the carbon balance's grassland input on the grass item; the
#   balance divides it by its own non-SOM N input (calculate_n_leaching()
#   reads the ratio as cn_input).
# - The root-zone depth is FAO-56 Table 22's maximum effective rooting depth
#   of the row's production items, pooled by WHEP's own hectares of each in
#   the row's water regime (the rule irrigation depth is pooled by).
# A row without a crop item carries no N; its drivers are declared values
# that cannot move a number, and a row of that kind carrying N is refused.

# ---- Methods ----------------------------------------------------------------

# The leaching method the drivers are built for and its options, validated.
.nld_methods <- function(methods) {
  methods <- as.list(methods)
  valid <- c("leaching", "som_source", "som_factor", "root_depth")
  unknown <- setdiff(names(methods), valid)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      "Unknown {.arg methods} {cli::qty(length(unknown))}name{?s}:
       {.val {unknown}}. Use {.val {valid}}.",
      class = "whep_n_loss_drivers_methods"
    )
  }
  leaching <- methods$leaching %||% "ipcc_fracleach"
  som_source <- methods$som_source %||% "carbon_balance"
  som_factor <- methods$som_factor %||% "pribyl_2010"
  root_depth <- methods$root_depth %||% "by_regime"
  list(
    leaching = rlang::arg_match(
      leaching,
      c("ipcc_fracleach", "meisinger_drainage", "image_gnm")
    ),
    som_source = rlang::arg_match(som_source, c("carbon_balance", "hwsd")),
    som_factor = rlang::arg_match(som_factor, c("pribyl_2010", "conventional")),
    root_depth = rlang::arg_match(
      root_depth,
      c("by_regime", "midpoint", "upper", "lower", "topsoil_30cm", "top_1m")
    )
  )
}

# The leaching driver columns each method's table carries, after the key.
.nld_leaching_cols <- function(leaching = "ipcc_fracleach") {
  base <- c("moisture_regime", "method_moisture_regime", "climate_fill_km")
  soil <- c(
    "land_use",
    "soil_field_capacity",
    "soil_wilting_point",
    "method_soil_hydraulic",
    "method_som"
  )
  switch(
    leaching,
    ipcc_fracleach = base,
    meisinger_drainage = c(
      base,
      "climate",
      "method_climate",
      "irrig_cat",
      "tillage",
      "method_tillage",
      soil,
      "soil_porosity",
      "som_share",
      "c_input_t",
      "c_input_mgc_ha",
      "method_c_input",
      "drainage_mm",
      "method_drainage",
      "root_depth_cm",
      "method_root_depth"
    ),
    image_gnm = c(
      base,
      soil,
      "soil_organic_carbon_pct",
      "runoff_mm",
      "method_runoff",
      "surface_runoff_fraction",
      "slope_m_per_km",
      "method_image_gnm_runoff",
      "temp_c_annual_mean",
      "soil_texture_class",
      "method_soil_texture_fao",
      "soil_drainage_class",
      "method_soil_drainage"
    )
  )
}

# The commodity balance sentinel of grassland rows, the one
# .nb_grazed_coords() and build_n_inputs()' manure engine give them.
.nld_grass_item <- function() {
  3000L
}

# ---- Assembly ---------------------------------------------------------------

# The leaching drivers of `opts$leaching` on the classified rows (one per
# balance key, with the normalised join columns of .nld_check_rows()).
.nld_leaching_drivers <- function(drivers, opts, data, spec) {
  if (opts$leaching == "ipcc_fracleach") {
    return(drivers)
  }
  .nld_check_no_item_n(drivers)
  cells <- dplyr::distinct(drivers, lon = .data$.lon, lat = .data$.lat)
  years <- sort(unique(drivers$.year))
  out <- drivers |>
    dplyr::mutate(
      land_use = dplyr::if_else(
        .data$.item %in% .nld_grass_item(),
        "Grassland",
        "Cropland"
      )
    ) |>
    .nld_join_hydraulic(cells, data) |>
    .nld_join_som(cells, data, opts)
  if (opts$leaching == "meisinger_drainage") {
    return(.nld_meisinger(out, cells, years, data, opts))
  }
  .nld_image_gnm(out, cells, years, data, spec)
}

.nld_meisinger <- function(out, cells, years, data, opts) {
  out |>
    dplyr::mutate(
      irrig_cat = dplyr::if_else(
        .data$water_regime == "irrigated",
        "Irrigated",
        "Rainfed"
      ),
      # The Meisinger table's synthetic block has only Tillage rows, so no
      # value could change it (calculate_n_leaching()).
      tillage = "Tillage",
      method_tillage = "constant_tillage"
    ) |>
    .nld_join_water("drainage", cells, years, data) |>
    .nld_join_root_depth(data, years, opts$root_depth) |>
    .nld_join_c_input(data, years)
}

.nld_image_gnm <- function(out, cells, years, data, spec) {
  out |>
    .nld_join_water("runoff", cells, years, data) |>
    .nld_join_gnm_runoff(cells, data) |>
    .nld_join_temperature(cells, years, data, spec) |>
    .nld_join_soil_classes(cells, data)
}

# A row without a crop item is a soil organic matter sequestration row or a
# row only an output term opened, and carries no input N: its leaching
# drivers are then declared values that move nothing. One that carries N, or
# whose N is not supplied, would be leached on drivers no crop defines.
.nld_check_no_item_n <- function(drivers) {
  bad <- is.na(drivers$.item) & !drivers$.n_input_t %in% 0
  if (!any(bad)) {
    return(invisible(drivers))
  }
  n_bad <- sum(bad)
  n_t <- sum(drivers$.n_input_t[bad], na.rm = TRUE)
  cli::cli_abort(
    c(
      "{n_bad} balance row{?s} without a crop item carr{?ies/y} N or no
       supplied {.field n_input_t}.",
      x = "Their leaching drivers (root depth, carbon input) are defined by a
           crop; {signif(n_t, 6)} t N would be leached on none.",
      i = "Allocate the N to crop items before building the drivers."
    ),
    class = "whep_n_loss_drivers_no_item_n"
  )
}

# ---- Nearest-neighbour fill -------------------------------------------------

# The layer's values on every cell of `cells`: a cell's own where the layer
# holds all `cols` there (every year, for a yearly layer), else the values of
# the nearest layer cell that holds them, by great-circle distance between
# cell centres, with the distance in `fill_km` (NA on a cell's own values).
.nld_nearest_fill <- function(layer, cells, cols, label) {
  by_year <- rlang::has_name(layer, "year")
  layer <- .nld_round_cells(layer)
  donors <- .nld_complete_cells(layer, cols, by_year)
  own <- dplyr::semi_join(layer, donors, by = c("lon", "lat")) |>
    dplyr::semi_join(cells, by = c("lon", "lat")) |>
    dplyr::mutate(fill_km = NA_real_)
  lacking <- dplyr::anti_join(cells, donors, by = c("lon", "lat"))
  if (nrow(lacking) == 0L) {
    return(own)
  }
  if (nrow(donors) == 0L) {
    cli::cli_abort(
      "The {label} layer holds {.field {cols}} on no cell.",
      class = c("whep_n_loss_drivers_layer", "whep_absent_input")
    )
  }
  # One row per lacking cell, but one donor can be the nearest cell of
  # several lacking cells, and a yearly layer holds one donor row per year:
  # many lacking cells meet many donor rows.
  nearest <- .cmr_nearest_cells(lacking, donors)
  filled <- nearest |>
    dplyr::select(
      "lon",
      "lat",
      "donor_lon",
      "donor_lat",
      fill_km = "climate_fill_km"
    ) |>
    dplyr::inner_join(
      dplyr::rename(layer, donor_lon = "lon", donor_lat = "lat"),
      by = c("donor_lon", "donor_lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::select(-"donor_lon", -"donor_lat")
  cli::cli_inform(
    "{nrow(lacking)} balance cell{?s} without the {label} layer took the
     nearest cell's value{?s} (at most {round(max(nearest$climate_fill_km))}
     km away).",
    class = "whep_n_loss_drivers_nearest"
  )
  dplyr::bind_rows(own, filled)
}

.nld_round_cells <- function(layer) {
  tibble::as_tibble(layer) |>
    dplyr::mutate(
      lon = round(as.numeric(.data$lon), 2),
      lat = round(as.numeric(.data$lat), 2)
    )
}

# The cells holding every column (in every year of a yearly layer).
.nld_complete_cells <- function(layer, cols, by_year) {
  ok <- stats::complete.cases(layer[cols]) &
    purrr::reduce(
      purrr::map(cols, \(col) {
        !is.numeric(layer[[col]]) | is.finite(layer[[col]])
      }),
      `&`
    )
  n_years <- if (by_year) dplyr::n_distinct(layer$year) else 1L
  tibble::tibble(lon = layer$lon, lat = layer$lat, ok = ok) |>
    dplyr::summarise(
      complete = all(.data$ok) & dplyr::n() == .env$n_years,
      .by = c("lon", "lat")
    ) |>
    dplyr::filter(.data$complete) |>
    dplyr::select("lon", "lat")
}

# The stamp field of a filled cell.
.nld_fill_field <- function(fill_km, field) {
  dplyr::if_else(
    is.na(fill_km),
    "",
    paste0(";", field, "=", round(fill_km, 1), "km")
  )
}

# ---- Water --------------------------------------------------------------------

# LPJmL's annual deep seepage ("drainage") or total runoff ("runoff"), per
# cell and year, onto every row of the cell. `data$lpjml_drainage` /
# `data$lpjml_runoff` replace the read; both are read_lpjml_hydrology()
# annual tables (`lon`, `lat`, `year`, `value`).
.nld_join_water <- function(rows, var, cells, years, data) {
  out_col <- if (var == "drainage") "drainage_mm" else "runoff_mm"
  method_col <- if (var == "drainage") "method_drainage" else "method_runoff"
  source <- if (var == "drainage") "lpjml_seepage" else "lpjml_total_runoff"
  table <- data[[paste0("lpjml_", var)]] %||%
    read_lpjml_hydrology(var, years = years, monthly = FALSE)
  table <- .nld_check_water(table, var, years)
  filled <- .nld_nearest_fill(table, cells, "value", paste("LPJmL", var)) |>
    dplyr::transmute(
      .lon = .data$lon,
      .lat = .data$lat,
      .year = as.integer(.data$year),
      !!out_col := .data$value,
      !!method_col := paste0(
        source,
        .nld_fill_field(.data$fill_km, "nearest_lpjml_cell")
      )
    )
  dplyr::left_join(
    rows,
    filled,
    by = c(".lon", ".lat", ".year"),
    relationship = "many-to-one"
  )
}

.nld_check_water <- function(table, var, years) {
  .check_columns(table, c("lon", "lat", "year", "value"), paste("LPJmL", var))
  table <- dplyr::filter(table, as.integer(.data$year) %in% years)
  supplied <- paste0("data$lpjml_", var)
  check_inputs_supplied(
    table,
    rlang::set_names("value", paste("LPJmL", var)),
    details = c(
      i = paste(
        "Set WHEP_LPJML_RUN_DIR to a finished LPJmL run, or supply",
        supplied,
        "(lon, lat, year, value; mm/yr)."
      )
    )
  )
  absent <- setdiff(years, as.integer(table$year))
  if (length(absent) > 0L) {
    cli::cli_abort(
      "The LPJmL {.val {var}} table holds no rows for {.val {absent}}.",
      class = c("whep_n_loss_drivers_layer", "whep_absent_input")
    )
  }
  table
}

# ---- Soil -------------------------------------------------------------------

# HWSD hydraulic properties by dominant USDA texture class
# (read_soil_hydraulic(): t_field, t_wilt, porosity), on every row of a cell.
.nld_join_hydraulic <- function(rows, cells, data) {
  cols <- c("t_field", "t_wilt", "porosity")
  layer <- data$soil_hydraulic %||% read_soil_hydraulic()
  .check_columns(layer, c("lon", "lat", cols), "soil_hydraulic")
  check_inputs_supplied(
    layer,
    c(
      "field capacity" = "t_field",
      "wilting point" = "t_wilt",
      "porosity" = "porosity"
    )
  )
  filled <- layer |>
    dplyr::select(dplyr::all_of(c("lon", "lat", cols))) |>
    .nld_nearest_fill(cells, cols, "HWSD hydraulic") |>
    dplyr::transmute(
      .lon = .data$lon,
      .lat = .data$lat,
      soil_field_capacity = .data$t_field,
      soil_wilting_point = .data$t_wilt,
      soil_porosity = .data$porosity,
      method_soil_hydraulic = paste0(
        "hwsd_usda_texture_class",
        .nld_fill_field(.data$fill_km, "nearest_hwsd_cell")
      )
    )
  dplyr::left_join(
    rows,
    filled,
    by = c(".lon", ".lat"),
    relationship = "many-to-one"
  )
}

# The FAO texture and drainage classes of IMAGE-GNM's Table 2, from HWSD.
.nld_join_soil_classes <- function(rows, cells, data) {
  texture <- (data$soil_texture_fao %||% read_soil_texture_fao()) |>
    .nld_class_layer("soil_texture_fao", "method_soil_texture_fao", cells) |>
    dplyr::rename(soil_texture_class = "soil_texture_fao")
  drainage <- (data$soil_drainage %||% read_soil_drainage()) |>
    .nld_class_layer("soil_drainage_class", "method_soil_drainage", cells)
  rows |>
    dplyr::left_join(
      texture,
      by = c(".lon", ".lat"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      drainage,
      by = c(".lon", ".lat"),
      relationship = "many-to-one"
    )
}

# One HWSD class layer on every balance cell, its stamp carrying the fill.
.nld_class_layer <- function(layer, class_col, method_col, cells) {
  .check_columns(layer, c("lon", "lat", class_col, method_col), class_col)
  layer |>
    dplyr::select(dplyr::all_of(c("lon", "lat", class_col, method_col))) |>
    .nld_nearest_fill(cells, class_col, paste("HWSD", class_col)) |>
    dplyr::transmute(
      .lon = .data$lon,
      .lat = .data$lat,
      !!class_col := .data[[class_col]],
      !!method_col := paste0(
        .data[[method_col]],
        .nld_fill_field(.data$fill_km, "nearest_hwsd_cell")
      )
    )
}

# Soil organic carbon over the HWSD topsoil mass: a mass fraction, as
# `soil_organic_carbon_pct` (percent) and, times the SOM:SOC factor, as the
# soil organic matter share `som_share` (fraction) Meisinger's bins read.
.nld_join_som <- function(rows, cells, data, opts) {
  factor_name <- if (opts$som_factor == "pribyl_2010") {
    "som_soc_factor_pribyl"
  } else {
    "som_soc_factor_conventional"
  }
  factor <- .n_constant(factor_name)
  mass <- .nld_soil_mass(cells, data)
  with_mass <- dplyr::left_join(
    rows,
    mass,
    by = c(".lon", ".lat"),
    relationship = "many-to-one"
  )
  soc <- if (opts$som_source == "carbon_balance") {
    .nld_carbon_stock(with_mass, data)
  } else {
    dplyr::mutate(
      with_mass,
      .soc_mgc_ha = .data$soc_obs_mgc_ha,
      .soc_source = "hwsd_topsoil_oc"
    )
  }
  soc |>
    .nld_check_som() |>
    dplyr::mutate(
      .oc_fraction = .data$.soc_mgc_ha / .data$soil_mass_mg_ha,
      som_share = .data$.oc_fraction * .env$factor,
      soil_organic_carbon_pct = 100 * .data$.oc_fraction,
      method_som = paste0(
        .data$.soc_source,
        ";som_factor=",
        .env$factor,
        ";",
        .data$.mass_method
      )
    ) |>
    dplyr::select(
      -".oc_fraction",
      -".soc_mgc_ha",
      -".soc_source",
      -"soil_mass_mg_ha",
      -"soc_obs_mgc_ha",
      -".mass_method"
    )
}

.nld_soil_mass <- function(cells, data) {
  cols <- c("soil_mass_mg_ha", "soc_obs_mgc_ha")
  layer <- data$soil_mass %||% read_hwsd_topsoil_mass()
  .check_columns(layer, c("lon", "lat", cols), "soil_mass")
  check_inputs_supplied(layer, c("topsoil mass" = "soil_mass_mg_ha"))
  layer |>
    dplyr::select(dplyr::all_of(c("lon", "lat", cols))) |>
    .nld_nearest_fill(cells, cols, "HWSD topsoil mass") |>
    dplyr::transmute(
      .lon = .data$lon,
      .lat = .data$lat,
      soil_mass_mg_ha = .data$soil_mass_mg_ha,
      soc_obs_mgc_ha = .data$soc_obs_mgc_ha,
      .mass_method = paste0(
        "hwsd_topsoil_mass",
        .nld_fill_field(.data$fill_km, "nearest_hwsd_cell")
      )
    )
}

# The carbon balance's 0-30 cm stock of each row's land use: the cell's
# cropland classes (area-weighted) for crop rows and rows without an item,
# its grassland for the grass item. A cell-polity the carbon balance lacks
# takes the mean stock of the same land use over the cell's other polities,
# stamped: the soil of a cell is one soil.
.nld_carbon_stock <- function(rows, data) {
  cb <- .nld_carbon_balance(data) |>
    dplyr::filter(!is.na(.data$land_use))
  stock <- dplyr::summarise(
    cb,
    .soc_mgc_ha = .nld_area_mean(.data$stock_mgc_ha, .data$area_ha),
    .by = c(".lon", ".lat", ".area", ".year", "land_use")
  )
  cell <- dplyr::summarise(
    stock,
    .cell_soc = mean(.data$.soc_mgc_ha),
    .by = c(".lon", ".lat", ".year", "land_use")
  )
  rows |>
    dplyr::left_join(
      stock,
      by = c(".lon", ".lat", ".area", ".year", "land_use"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      cell,
      by = c(".lon", ".lat", ".year", "land_use"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      .soc_source = dplyr::if_else(
        is.na(.data$.soc_mgc_ha) & !is.na(.data$.cell_soc),
        "carbon_balance_soc;cell_other_polity",
        "carbon_balance_soc"
      ),
      .soc_mgc_ha = dplyr::coalesce(.data$.soc_mgc_ha, .data$.cell_soc)
    ) |>
    dplyr::select(-".cell_soc") |>
    .nld_nearest_stock(cell)
}

# A cell the carbon balance holds no stock of the row's land use in (it runs
# on the CRU and LPJmL grids only, whep#1095) takes the stock of the nearest
# cell holding that land use in the same year, stamped with the distance
# (maintainer decision D77, 2026-10-14). A land use the carbon balance holds
# nowhere stays missing, for .nld_check_som() to judge.
.nld_nearest_stock <- function(rows, cell) {
  lacking <- rows |>
    dplyr::filter(is.na(.data$.soc_mgc_ha), !is.na(.data$land_use)) |>
    dplyr::distinct(.data$.lon, .data$.lat, .data$.year, .data$land_use)
  if (nrow(lacking) == 0L) {
    return(rows)
  }
  filled <- lacking |>
    dplyr::group_by(.data$.year, .data$land_use) |>
    dplyr::group_modify(\(group, k) {
      .nld_nearest_donor(
        dplyr::rename(group, lon = ".lon", lat = ".lat"),
        cell |>
          dplyr::filter(
            .data$.year == k$.year,
            .data$land_use == k$land_use
          ) |>
          dplyr::transmute(lon = .data$.lon, lat = .data$.lat, .data$.cell_soc)
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      .lon = .data$lon,
      .lat = .data$lat,
      .data$.year,
      .data$land_use,
      .near_soc = .data$.cell_soc,
      .near_km = .data$fill_km
    )
  rows |>
    dplyr::left_join(
      filled,
      by = c(".lon", ".lat", ".year", "land_use"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      .soc_source = dplyr::if_else(
        is.na(.data$.soc_mgc_ha) & !is.na(.data$.near_soc),
        paste0(
          "carbon_balance_soc;nearest_cell_same_land_use=",
          round(.data$.near_km, 1),
          "km"
        ),
        .data$.soc_source
      ),
      .soc_mgc_ha = dplyr::coalesce(.data$.soc_mgc_ha, .data$.near_soc)
    ) |>
    dplyr::select(-".near_soc", -".near_km")
}

# Each lacking cell with the values of its nearest donor cell and the
# distance in `fill_km`; no rows when there is no donor.
.nld_nearest_donor <- function(lacking, donors) {
  if (nrow(donors) == 0L) {
    return(dplyr::mutate(donors, fill_km = double()))
  }
  .cmr_nearest_cells(lacking, donors) |>
    dplyr::select(
      "lon",
      "lat",
      "donor_lon",
      "donor_lat",
      fill_km = "climate_fill_km"
    ) |>
    dplyr::inner_join(
      dplyr::rename(donors, donor_lon = "lon", donor_lat = "lat"),
      by = c("donor_lon", "donor_lat"),
      relationship = "many-to-one"
    ) |>
    dplyr::select(-"donor_lon", -"donor_lat")
}

# The carbon balance in the drivers' join form, its land use mapped to the
# drivers' Cropland/Grassland (crop groups are cropland too); other classes
# (natural, urban) carry no balance row and map to NA.
.nld_carbon_balance <- function(data) {
  cb <- data$carbon_balance
  if (is.null(cb)) {
    cli::cli_abort(
      c(
        "The Meisinger and IMAGE-GNM leaching drivers read the carbon
         balance, and {.code data$carbon_balance} is absent.",
        i = "Supply a gridded {.fn build_carbon_balance} result covering the
             balance years (the balance passes its own
             {.code data$carbon_balance} on), or use
             {.code methods$som_source = \"hwsd\"} for the soil carbon."
      ),
      class = c("whep_n_loss_drivers_carbon_balance", "whep_absent_input")
    )
  }
  cols <- c(
    "lon",
    "lat",
    "area_code",
    "land_use",
    "year",
    "stock_mgc_ha",
    "c_input_mgc_ha",
    "area_ha"
  )
  .check_columns(cb, cols, "carbon_balance")
  check_inputs_supplied(cb, c("soil organic carbon stock" = "stock_mgc_ha"))
  lower <- stringr::str_to_lower(cb$land_use)
  tibble::as_tibble(cb) |>
    dplyr::transmute(
      .lon = round(as.numeric(.data$lon), 2),
      .lat = round(as.numeric(.data$lat), 2),
      .area = as.integer(.data$area_code),
      .year = as.integer(.data$year),
      land_use = dplyr::case_when(
        .soc_is_cropland(.env$lower) ~ "Cropland",
        .env$lower == "grassland" ~ "Grassland"
      ),
      stock_mgc_ha = .data$stock_mgc_ha,
      c_input_mgc_ha = .data$c_input_mgc_ha,
      area_ha = .data$area_ha
    )
}

# An area-weighted mean, or the plain mean where the classes carry no area.
.nld_area_mean <- function(value, area) {
  ok <- !is.na(value)
  if (sum(area[ok], na.rm = TRUE) > 0) {
    stats::weighted.mean(value[ok], area[ok], na.rm = TRUE)
  } else {
    mean(value[ok])
  }
}

# A row carrying N without a soil carbon stock has no soil organic matter
# class, and is refused. A row without N leaches nothing, so its carbon is
# declared from HWSD's own topsoil carbon (the share's selectable source),
# stamped `zero_n_row`.
.nld_check_som <- function(rows) {
  missing <- is.na(rows$.soc_mgc_ha)
  bad <- missing & !rows$.n_input_t %in% 0
  if (any(bad)) {
    n_t <- signif(sum(rows$.n_input_t[bad], na.rm = TRUE), 6)
    cli::cli_abort(
      c(
        "{sum(bad)} balance row{?s} carrying N {?has/have} no soil organic
         carbon of {?its/their} land use ({n_t} t N).",
        i = "The carbon balance holds no stock of the row's land use in any
             cell of the year; supply one that covers it."
      ),
      class = c("whep_n_loss_drivers_soc", "whep_absent_input")
    )
  }
  dplyr::mutate(
    rows,
    .soc_source = dplyr::if_else(
      .env$missing,
      "hwsd_topsoil_oc;zero_n_row",
      .data$.soc_source
    ),
    .soc_mgc_ha = dplyr::if_else(
      .env$missing,
      .data$soc_obs_mgc_ha,
      .data$.soc_mgc_ha
    )
  )
}

# ---- Carbon input of the C:N attenuation --------------------------------------

# The C carried into the soil with the row's inputs (t C per year): the
# crop's own soil carbon input pooled to its commodity balance item on crop
# rows, the carbon balance's grassland input on the grass item. Both regime
# rows of an item take the item's whole C: the balance divides it by the
# item's whole non-SOM N, so both carry the crop's C:N.
.nld_join_c_input <- function(rows, data, years) {
  crop <- .nld_crop_carbon(data, years)
  cb_grass <- .nld_carbon_balance(data) |>
    dplyr::filter(.data$land_use %in% "Grassland", .data$.year %in% years)
  grass <- dplyr::summarise(
    cb_grass,
    .grass_c_t = sum(.data$c_input_mgc_ha * .data$area_ha),
    .by = c(".lon", ".lat", ".area", ".year")
  )
  grass_item <- .nld_grass_item()
  rows |>
    dplyr::left_join(
      crop,
      by = c(".lon", ".lat", ".area", ".item", ".year"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      grass,
      by = c(".lon", ".lat", ".area", ".year"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      c_input_t = dplyr::case_when(
        is.na(.data$.item) ~ NA_real_,
        .data$.item == .env$grass_item ~ .data$.grass_c_t,
        .default = .data$.crop_c_t
      ),
      method_c_input = dplyr::case_when(
        is.na(.data$.item) ~ "no_crop_item",
        is.na(.data$c_input_t) ~ "no_carbon_input",
        .data$.item == .env$grass_item ~ "carbon_balance_grassland",
        .default = "soil_carbon_inputs_per_crop"
      )
    ) |>
    dplyr::select(-".crop_c_t", -".grass_c_t") |>
    .nld_nearest_grass_c(cb_grass) |>
    .nld_inform_c_input()
}

# A grass row whose cell-polity the carbon balance holds no grassland in
# takes the grassland C input density (Mg C/ha/yr, its area-weighted mean)
# of the nearest cell holding grassland in the same year, stamped with the
# distance (maintainer decision D78, 2026-10-14, as D77 does for the stock).
# The density is carried as `c_input_mgc_ha`: the balance puts it on the
# key's own grass area (.nb_derive_cn_input()), which only the balance knows.
.nld_nearest_grass_c <- function(rows, cb_grass) {
  rows$c_input_mgc_ha <- NA_real_
  lacking <- rows$.item %in% .nld_grass_item() & is.na(rows$c_input_t)
  if (!any(lacking)) {
    return(rows)
  }
  density <- cb_grass |>
    dplyr::summarise(
      .density = sum(.data$c_input_mgc_ha * .data$area_ha) /
        sum(.data$area_ha),
      .by = c(".lon", ".lat", ".year")
    ) |>
    dplyr::filter(is.finite(.data$.density))
  cells <- dplyr::distinct(rows[lacking, ], .data$.lon, .data$.lat, .data$.year)
  filled <- cells |>
    dplyr::group_by(.data$.year) |>
    dplyr::group_modify(\(group, k) {
      .nld_nearest_donor(
        dplyr::rename(group, lon = ".lon", lat = ".lat"),
        density |>
          dplyr::filter(.data$.year == k$.year) |>
          dplyr::transmute(lon = .data$.lon, lat = .data$.lat, .data$.density)
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::rename(.lon = "lon", .lat = "lat", .near_km = "fill_km")
  rows |>
    dplyr::left_join(
      filled,
      by = c(".lon", ".lat", ".year"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      .take = .env$lacking & !is.na(.data$.density),
      c_input_mgc_ha = dplyr::if_else(.data$.take, .data$.density, NA_real_),
      method_c_input = dplyr::if_else(
        .data$.take,
        paste0(
          "carbon_balance_grassland;nearest_cell_same_land_use=",
          round(.data$.near_km, 1),
          "km"
        ),
        .data$method_c_input
      )
    ) |>
    dplyr::select(-".density", -".near_km", -".take")
}

# A crop row carrying N whose crop has no carbon input in its cell and
# polity keeps no C here (`no_carbon_input`): the balance gives it its
# item's C:N in the polity, else globally (maintainer decision D78,
# 2026-10-14; .nb_derive_cn_input()), and refuses an item with none. The
# count is reported so the fill is never silent.
.nld_inform_c_input <- function(rows) {
  open <- rows$method_c_input == "no_carbon_input" & !rows$.n_input_t %in% 0
  if (any(open)) {
    n_t <- signif(sum(rows$.n_input_t[open], na.rm = TRUE), 6)
    cli::cli_inform(
      "{sum(open)} balance row{?s} carrying N ({n_t} t N) {?has/have} no soil
       carbon input in {?its/their} cell and polity; the balance gives
       {?it/them} the item's polity C:N, stamped.",
      class = "whep_n_loss_drivers_c_input_fill"
    )
  }
  rows
}

# build_soil_carbon_inputs()' per-crop carbon mass, pooled to the commodity
# balance item.
.nld_crop_carbon <- function(data, years) {
  inputs <- data$carbon_inputs %||%
    build_soil_carbon_inputs(
      resolution = "grid",
      years = years,
      data = purrr::compact(list(country_grid = data$country_grid))
    )
  cols <- c(
    "lon",
    "lat",
    "area_code",
    "item_prod_code",
    "year",
    "total_c_input_mgc_ha_yr",
    "crop_area_ha"
  )
  .check_columns(inputs, cols, "carbon_inputs")
  check_inputs_supplied(
    inputs,
    c("soil carbon input" = "total_c_input_mgc_ha_yr")
  )
  tibble::as_tibble(inputs) |>
    dplyr::transmute(
      .lon = round(as.numeric(.data$lon), 2),
      .lat = round(as.numeric(.data$lat), 2),
      .area = as.integer(.data$area_code),
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      .year = as.integer(.data$year),
      c_t = .data$total_c_input_mgc_ha_yr * .data$crop_area_ha
    ) |>
    dplyr::filter(.data$.year %in% years) |>
    dplyr::inner_join(
      .lid_prod_cbs(),
      by = "item_prod_code",
      relationship = "many-to-one"
    ) |>
    dplyr::rename(.item = "item_cbs_code") |>
    dplyr::summarise(
      .crop_c_t = sum(.data$c_t),
      .by = c(".lon", ".lat", ".area", ".year", ".item")
    )
}

# ---- Root-zone depth ----------------------------------------------------------

# The root-zone depth (cm) of each row. Under the FAO-56 points it is the
# maximum effective rooting depth Zr of FAO-56 Table 22 (Allen et al. 1998,
# fao56_root_depth) of the row's production items, pooled by WHEP's own
# hectares of each in the row's water regime, the rule the irrigation depth
# of a pooled item follows. "by_regime" takes the upper end of each range on
# rainfed rows and the lower end on irrigated rows (Table 22, footnote 1:
# the smaller values hold where water is applied frequently). An item whose
# production items hold no hectares in the regime takes the plain mean over
# them, stamped. A row without a crop item takes the HWSD topsoil depth.
.nld_join_root_depth <- function(rows, data, years, point) {
  if (point %in% c("topsoil_30cm", "top_1m")) {
    depth <- if (point == "topsoil_30cm") {
      .n_constant("root_depth_topsoil_cm")
    } else {
      100 * .n_constant("image_gnm_tawc_depth_m")
    }
    return(dplyr::mutate(
      rows,
      root_depth_cm = .env$depth,
      method_root_depth = paste0("fixed_", .env$point)
    ))
  }
  prod <- .nld_prod_root_depth(point)
  topsoil <- .n_constant("root_depth_topsoil_cm")
  rows |>
    dplyr::left_join(
      .nld_pooled_root_depth(prod, data, years),
      by = c(".lon", ".lat", ".area", ".item", ".year", "water_regime"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      .nld_item_root_depth(prod),
      by = c(".item", "water_regime"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      root_depth_cm = dplyr::case_when(
        is.na(.data$.item) ~ .env$topsoil,
        !is.na(.data$.pooled_cm) ~ .data$.pooled_cm,
        .default = .data$.item_cm
      ),
      method_root_depth = dplyr::case_when(
        is.na(.data$.item) ~ "no_crop_item_topsoil_30cm",
        !is.na(.data$.pooled_cm) ~ paste0(
          "fao56_",
          .env$point,
          dplyr::if_else(.data$.pooled_proxy, ";proxy", "")
        ),
        !is.na(.data$.item_cm) ~ paste0(
          "fao56_",
          .env$point,
          dplyr::if_else(.data$.item_proxy, ";proxy", ""),
          ";unweighted_no_regime_area"
        )
      )
    ) |>
    .nld_check_root_depth(topsoil) |>
    dplyr::select(
      -".pooled_cm",
      -".pooled_proxy",
      -".item_cm",
      -".item_proxy"
    )
}

# Per production item and water regime, the depth (cm) of the chosen point
# of its Table 22 range, and whether the row is a stated proxy.
.nld_prod_root_depth <- function(point) {
  table <- whep::fao56_root_depth |>
    dplyr::filter(
      .data$method_root_depth != "none",
      !is.na(.data$depth_min_m),
      !is.na(.data$depth_max_m)
    ) |>
    dplyr::transmute(
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      min_cm = 100 * .data$depth_min_m,
      max_cm = 100 * .data$depth_max_m,
      proxy = .data$method_root_depth == "proxy"
    )
  rainfed <- switch(
    point,
    by_regime = table$max_cm,
    midpoint = (table$min_cm + table$max_cm) / 2,
    upper = table$max_cm,
    lower = table$min_cm
  )
  irrigated <- switch(
    point,
    by_regime = table$min_cm,
    midpoint = (table$min_cm + table$max_cm) / 2,
    upper = table$max_cm,
    lower = table$min_cm
  )
  dplyr::bind_rows(
    dplyr::mutate(table, water_regime = "rainfed", depth_cm = .env$rainfed),
    dplyr::mutate(table, water_regime = "irrigated", depth_cm = .env$irrigated)
  ) |>
    dplyr::select("item_prod_code", "water_regime", "depth_cm", "proxy")
}

# The depth of each cell's commodity balance item and water regime, pooled
# over its production items by WHEP's hectares of each in that regime.
.nld_pooled_root_depth <- function(prod, data, years) {
  areas <- data$regime_areas %||%
    .sci_engine_crops(
      years,
      data$country_grid %||% .sci_read_country_grid()
    )
  .lid_check_areas(areas) |>
    dplyr::filter(.data$year %in% years) |>
    tidyr::pivot_longer(
      c("rainfed_ha", "irrigated_ha"),
      names_to = "water_regime",
      values_to = "weight_ha"
    ) |>
    dplyr::mutate(
      water_regime = stringr::str_remove(.data$water_regime, "_ha$")
    ) |>
    dplyr::filter(.data$weight_ha > 0) |>
    dplyr::inner_join(
      prod,
      by = c("item_prod_code", "water_regime"),
      relationship = "many-to-one"
    ) |>
    dplyr::summarise(
      .pooled_cm = sum(.data$depth_cm * .data$weight_ha) /
        sum(.data$weight_ha),
      .pooled_proxy = any(.data$proxy),
      .by = c(
        "lon",
        "lat",
        "area_code",
        "item_cbs_code",
        "year",
        "water_regime"
      )
    ) |>
    dplyr::rename(
      .lon = "lon",
      .lat = "lat",
      .area = "area_code",
      .item = "item_cbs_code",
      .year = "year"
    )
}

# The plain mean depth over a commodity balance item's production items.
.nld_item_root_depth <- function(prod) {
  prod |>
    dplyr::inner_join(
      .lid_prod_cbs(),
      by = "item_prod_code",
      relationship = "many-to-many"
    ) |>
    dplyr::rename(.item = "item_cbs_code") |>
    dplyr::summarise(
      .item_cm = mean(.data$depth_cm),
      .item_proxy = any(.data$proxy),
      .by = c(".item", "water_regime")
    )
}

# A crop row carrying N whose item has no Table 22 depth has no root zone,
# and is refused. One without N leaches nothing and takes the topsoil depth,
# stamped.
.nld_check_root_depth <- function(rows, topsoil) {
  missing <- is.na(rows$root_depth_cm)
  bad <- missing & !rows$.n_input_t %in% 0
  if (any(bad)) {
    items <- sort(unique(rows$.item[bad]))
    cli::cli_abort(
      c(
        "{sum(bad)} balance row{?s} carrying N {?has/have} no root-zone
         depth.",
        i = "Commodity balance items no {.code whep::fao56_root_depth} row
             covers: {.val {items}}."
      ),
      class = c("whep_n_loss_drivers_root_depth", "whep_absent_input")
    )
  }
  dplyr::mutate(
    rows,
    root_depth_cm = dplyr::if_else(
      .env$missing,
      .env$topsoil,
      .data$root_depth_cm
    ),
    method_root_depth = dplyr::if_else(
      .env$missing,
      "no_fao56_row_zero_n_topsoil_30cm",
      .data$method_root_depth
    )
  )
}

# ---- IMAGE-GNM layers -----------------------------------------------------------

# IMAGE-GNM's surface runoff fraction and median slope (read_image_gnm_runoff()).
.nld_join_gnm_runoff <- function(rows, cells, data) {
  cols <- c("surface_runoff_fraction", "slope_m_per_km")
  layer <- data$image_gnm_runoff %||% read_image_gnm_runoff()
  .check_columns(layer, c("lon", "lat", cols), "image_gnm_runoff")
  check_inputs_supplied(layer, c("surface runoff fraction" = cols[[1]]))
  filled <- layer |>
    dplyr::select(dplyr::all_of(c("lon", "lat", cols))) |>
    .nld_nearest_fill(cells, cols, "IMAGE-GNM runoff") |>
    dplyr::transmute(
      .lon = .data$lon,
      .lat = .data$lat,
      surface_runoff_fraction = .data$surface_runoff_fraction,
      slope_m_per_km = .data$slope_m_per_km,
      method_image_gnm_runoff = paste0(
        "image_gnm_supplement",
        .nld_fill_field(.data$fill_km, "nearest_image_gnm_cell")
      )
    )
  dplyr::left_join(
    rows,
    filled,
    by = c(".lon", ".lat"),
    relationship = "many-to-one"
  )
}

# The year's CRU mean annual temperature of the cell, from the climate the
# moisture regime read (a cell off CRU's land mask takes its nearest CRU land
# cell's, as its class does).
.nld_join_temperature <- function(rows, cells, years, data, spec) {
  temp <- .cmr_climate(data, spec, cells)$table |>
    dplyr::transmute(
      .lon = round(as.numeric(.data$lon), 2),
      .lat = round(as.numeric(.data$lat), 2),
      .year = as.integer(.data$year),
      temp_c_annual_mean = .data$mat_c
    ) |>
    dplyr::filter(.data$.year %in% years) |>
    dplyr::distinct()
  out <- dplyr::left_join(
    rows,
    temp,
    by = c(".lon", ".lat", ".year"),
    relationship = "many-to-one"
  )
  missing <- is.na(out$temp_c_annual_mean)
  if (any(missing)) {
    cli::cli_abort(
      "{sum(missing)} balance row{?s} {?has/have} no CRU mean annual
       temperature in {?its/their} year.",
      class = c("whep_n_loss_drivers_temperature", "whep_absent_input")
    )
  }
  out
}
