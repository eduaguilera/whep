# Cropland manure carbon placed on the cells where the livestock are
# (whep#1307).
#
# Under `method_manure_placement = "livestock"` the manure carbon of
# build_soil_carbon_inputs() follows the herds, the way the gridded nitrogen
# balance's manure does since whep#1300 (R/n_balance_grid_manure.R): the feed
# intake is the local grain (.run_redistribute_local(), national demand spread
# to cells by gridded head shares on the carbon path's own cell support), and
# the manure engine runs at "subnational", so each cell's collected manure is
# allocated over that cell's crops under the fixed 170 kg N/ha ceiling and
# what a cell cannot hold is trucked to neighbouring cells with room, the
# rest over-applied on the cell itself (.sci_place_cell_manure()). The
# engine's crop layer is the very cell-crop area this file divides carbon by,
# that year's crop weights renormalised to the FAOSTAT harvested area, so the
# cap binds on the hectares the density is formed over.
#
# `"crop_area"` keeps the national grain: the polity's manure is allocated to
# its crops by harvested area and each polity-crop is fanned out to cells by
# crop area, like residue and roots. Livestock location never enters it.

# The placements build_soil_carbon_inputs() offers, default first.
.sci_manure_placements <- function() {
  c("livestock", "crop_area")
}

# The cell-grain inputs of the livestock placement, resolved once per build.
# A supplied `data$manure` must already be keyed on cells: a national stream
# carries no location to follow, and gridding it by crop area under this name
# would be the silent fallback the method argument exists to prevent.
.sci_cell_manure_inputs <- function(data, years) {
  if (!is.null(data$manure)) {
    return(list(manure = .sci_check_cell_manure(data$manure)))
  }
  list(intake_ctx = .sci_cell_intake_context(years))
}

.sci_check_cell_manure <- function(manure) {
  .check_columns(manure, c("year", "territory", "sub_territory"), "manure")
  if (anyNA(manure$sub_territory)) {
    cli::cli_abort(
      c(
        "{.code data$manure} has national rows ({.field sub_territory} is
         {.val {NA}}), which cannot be placed where the livestock are.",
        i = "Supply the {.code applied} stream of
             {.fun build_livestock_nutrient_flows} run at
             {.val subnational}, or pass
             {.code method_manure_placement = 'crop_area'}."
      ),
      class = "whep_sci_manure_grain_mismatch"
    )
  }
  manure
}

# The production and commodity balances the local feed engine reads, sliced
# to the requested years and normalised once for every year of the build.
.sci_cell_intake_context <- function(years) {
  .local_run_context(
    "ipcc",
    "historical",
    production = get_primary_production(years = years),
    cbs = get_wide_cbs(years = years)
  )
}

# One year's realised feed intake at the local grain, its heads and grass
# ceiling placed on `country_grid` (the carbon path's own cell support), with
# the settings the nitrogen balance uses (.n_livestock_intake()).
.sci_cell_intake <- function(yr, ctx, country_grid) {
  .local_year_engine(yr, ctx, country_grid)$result
}

# One year's cropland manure on cells, as gridded component rows, plus what
# could not be placed. NULL under "crop_area", whose manure is already among
# the polity-crop components.
.sci_year_cell_manure <- function(yr, d, weights) {
  if (d$manure_placement != "livestock") {
    return(NULL)
  }
  cell_area <- .sci_manure_cell_area(weights, yr, d$harvested_area)
  applied <- if (is.null(d$cell_manure$manure)) {
    .sci_cell_applied(yr, d, cell_area)
  } else {
    dplyr::filter(d$cell_manure$manure, as.integer(.data$year) == yr)
  }
  .sci_place_cell_manure(applied, cell_area)
}

# The year's cell-crop area: the crop weights renormalised to the FAOSTAT
# national harvested area exactly as .sci_join_weights() does, so the manure
# lands on the hectares the residue and root carbon of that cell-crop are
# divided by. With a national area supplied, a crop without one is left out,
# as it carries no other carbon (the NPP chain needs harvested area too) and
# the nitrogen layer leaves it out as well (.n_manure_crop_layer()).
.sci_manure_cell_area <- function(weights, yr, harvested_area) {
  cells <- dplyr::mutate(weights, year = yr)
  faostat <- .sci_faostat_area(harvested_area)
  if (!is.null(faostat)) {
    cells <- dplyr::semi_join(
      cells,
      faostat,
      by = c("area_code", "item_prod_code", "year")
    )
  }
  cells |>
    .sci_rescale_cell_area(harvested_area) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "item_prod_code",
      "year",
      "crop_area_ha"
    )
}

# The manure engine on one year's cell intake, with the allocation the
# nitrogen balance's driver uses (inst/scripts/run_nitrogen_balance.R): the
# fixed 170 kg N/ha ceiling, at "subnational", over the cell crop layer.
.sci_cell_applied <- function(yr, d, cell_area) {
  intake <- .sci_cell_intake(yr, d$cell_manure$intake_ctx, d$country_grid)
  crops <- dplyr::transmute(
    cell_area,
    year = .data$year,
    territory = as.character(.data$area_code),
    sub_territory = .cell_id(.data$lon, .data$lat),
    crop = .data$item_prod_code,
    manure_n_receptivity = .data$crop_area_ha,
    crop_area_ha = .data$crop_area_ha
  )
  build_livestock_nutrient_flows(
    intake,
    resolution = "subnational",
    methods = list(allocation = list(cap_method = "fixed_ceiling")),
    gridded = list(crops = crops)
  )$applied
}

# Put a cell-keyed `applied` stream's cropland manure on cell-crop rows.
#
# A row naming a crop the cell grows lands on that cell-crop. A row with no
# crop -- manure trucked in within the sink's room, or over-applied above the
# cap -- or naming a crop the cell does not grow is spread over the cell's
# crops by area, the rule the nitrogen balance applies to the same rows
# (`unattributed_method = "cropland_area"`, build_n_inputs()). So the
# over-cap manure the national grain drops (.sci_warn_dropped_manure_c(),
# whep#805) is kept here, on the cell the nitrogen balance keeps it on.
# Manure on a cell with no crop on the support is spread over its polity's
# cropland by area, the nitrogen driver's `method_unsupported =
# "reallocate_drop"` and this file's own rule for an unspatialized crop
# (.sci_reallocate()); only a polity with no cropland cell at all loses it,
# and that is reported.
.sci_place_cell_manure <- function(applied, cell_area) {
  rows <- .sci_cell_manure_rows(applied)
  keys <- c("lon", "lat", "area_code", "item_prod_code", "year")
  direct <- dplyr::inner_join(rows$kept, cell_area, by = keys)
  in_cell <- dplyr::anti_join(rows$kept, cell_area, by = keys) |>
    .sci_spread_manure(cell_area, c("lon", "lat", "area_code", "year"))
  in_polity <- in_cell$unplaced |>
    .sci_spread_manure(cell_area, c("area_code", "year"))
  list(
    rows = dplyr::bind_rows(direct, in_cell$rows, in_polity$rows) |>
      dplyr::mutate(input_type = "manure") |>
      dplyr::select(
        dplyr::all_of(keys),
        "input_type",
        "c_mass_mg",
        "n_mass_mg",
        "crop_area_ha"
      ),
    over_cap = rows$over_cap,
    reallocated = in_cell$unplaced,
    dropped = in_polity$unplaced
  )
}

# Cropland rows on cell keys, summed to cell-crop, plus the over-cap rows
# (kept, but sized for the report).
.sci_cell_manure_rows <- function(applied) {
  cropland <- applied |>
    ensure_columns(tibble::tibble(
      applied_n = numeric(),
      over_cap = logical()
    )) |>
    dplyr::filter(.data$land_use == "Cropland")
  cell <- .parse_cell_id(cropland$sub_territory)
  cropland <- cropland |>
    dplyr::mutate(
      lon = round(cell$lon, 2),
      lat = round(cell$lat, 2),
      area_code = .manure_territory_to_area_code(.data$territory),
      item_prod_code = .sci_manure_crop_prod_code(.data$crop),
      year = as.integer(.data$year)
    )
  kept <- dplyr::summarise(
    cropland,
    c_mass_mg = sum(.data$applied_c, na.rm = TRUE),
    # NOT na.rm, as in .sci_manure_components(): unknown nitrogen stays NA.
    n_mass_mg = sum(.data$applied_n),
    .by = c("lon", "lat", "area_code", "item_prod_code", "year")
  )
  list(
    kept = kept,
    over_cap = dplyr::filter(cropland, .data$over_cap %in% TRUE)
  )
}

# Spread manure over the cell-crops of each `by` group in proportion to their
# area; what finds no cell-crop in its group is returned as `unplaced`.
.sci_spread_manure <- function(rows, cell_area, by) {
  per_group <- dplyr::summarise(
    rows,
    c_mass_mg = sum(.data$c_mass_mg),
    n_mass_mg = sum(.data$n_mass_mg),
    .by = dplyr::all_of(by)
  )
  shares <- dplyr::mutate(
    cell_area,
    share = .data$crop_area_ha / sum(.data$crop_area_ha),
    .by = dplyr::all_of(by)
  )
  list(
    rows = per_group |>
      dplyr::inner_join(shares, by = by, relationship = "one-to-many") |>
      dplyr::mutate(
        c_mass_mg = .data$c_mass_mg * .data$share,
        n_mass_mg = .data$n_mass_mg * .data$share
      ),
    unplaced = dplyr::anti_join(per_group, shares, by = by)
  )
}

# Report, once over every year, the over-cap manure kept, the manure moved
# off a crop-less cell and the manure no cropland could take. `[[`, not `$`:
# under "crop_area" no year contributes a row and the bound tables have no
# columns at all.
.sci_report_cell_manure <- function(parts) {
  over <- .sci_sum_part(parts, "over_cap", "applied_c")
  if (over > 0) {
    cli::cli_inform(
      c(
        i = "{.val {round(over)}} t C of cropland manure above the 170 kg N/ha
             cap is kept on cropland, as the nitrogen balance keeps it
             (whep#805)."
      ),
      class = "whep_sci_manure_over_cap_kept"
    )
  }
  moved <- .sci_sum_part(parts, "reallocated", "c_mass_mg")
  if (moved > 0) {
    cli::cli_inform(
      c(
        i = "{.val {round(moved)}} t C of cropland manure sits on cells with no
             crop on the support and is spread over its polity's cropland by
             area."
      ),
      class = "whep_sci_manure_reallocated"
    )
  }
  .sci_warn_manure_no_cropland(dplyr::bind_rows(purrr::map(parts, "dropped")))
}

.sci_sum_part <- function(parts, part, column) {
  sum(dplyr::bind_rows(purrr::map(parts, part))[[column]], na.rm = TRUE)
}

.sci_warn_manure_no_cropland <- function(dropped) {
  lost <- sum(dropped[["c_mass_mg"]], na.rm = TRUE)
  if (lost <= 0) {
    return(invisible(NULL))
  }
  codes <- sort(unique(dropped$area_code))
  cli::cli_warn(
    c(
      "{.val {round(lost, 3)}} t C of cropland manure is in
       {cli::qty(length(codes))}polit{?y/ies} with no cropland cell on the
       support and is dropped from the carbon input.",
      i = "Affected {.field area_code}: {.val {codes}}."
    ),
    class = "whep_sci_manure_no_cropland"
  )
}
