# The livestock inputs of the nitrogen balance, at the grain its resolution
# needs (whep#1300).
#
# build_n_inputs(resolution = "grid") runs the manure engine at "subnational"
# (.ni_manure_resolution()), which reads `sub_territory` as a "lon_lat" cell
# id. So at grid resolution BOTH the intake and the crop layer its manure is
# allocated over must be keyed on cells: national intake (`sub_territory =
# NA`) leaves every manure row without a cell and the assembly aborts on
# "missing spatial keys"; a national crop layer never meets a cell and the
# allocation cap never binds.
#
# At grid resolution both are therefore placed on ONE cell support,
# `country_grid`, which the caller passes in: the livestock head shares and
# the grass ceiling (.local_spatial_inputs()) and the crop layer
# (.sci_grid_weights()) all read their cell-polity shares from it, so a border
# cell's animals and the hectares their manure lands on cannot be split
# between polities differently. At polity resolution nothing is placed on
# cells and the national grain is kept.

# The realised feed intake the manure and grazed-forage terms consume: the raw
# redistribute_feed() result (the estimate_n_excretion() contract), not the
# reshaped per-animal get_feed_intake() table.
#
# Grid: the local grain (.run_redistribute_local()) -- national demand spread
# to cells by gridded head shares, grazing capped by each cell's grass, plus
# border grazing -- with the engine's own settings (.local_intake_defaults()).
# Polity: the national grain (.run_redistribute_national()).
#
# Both use `feed_mode = "historical"` (no surplus distribution), the
# build_feed_intake_local() default and what build_soil_carbon_inputs() uses
# for the national grain.
.n_livestock_intake <- function(
  resolution,
  production,
  cbs,
  country_grid = NULL,
  demand_tier = "ipcc"
) {
  resolution <- rlang::arg_match0(resolution, c("grid", "polity"))
  if (resolution == "polity") {
    return(.run_redistribute_national(
      production = production,
      cbs = cbs,
      demand_tier = demand_tier,
      options = list(distribute_surplus = FALSE)
    ))
  }
  .n_check_cell_support(country_grid)
  ctx <- .local_run_context(
    demand_tier,
    "historical",
    production = production,
    cbs = cbs
  )
  years <- sort(unique(as.integer(ctx$production$year)))
  purrr::map(years, \(yr) {
    .local_year_engine(yr, ctx, country_grid)$result
  }) |>
    dplyr::bind_rows()
}

# The crop layer allocate_manure_to_land() spreads collected manure over:
# per polity-crop harvested area from primary production
# (.sci_manure_crop_layer(), the same layer build_soil_carbon_inputs() uses),
# with `manure_n_receptivity` equal to the harvested area and `crop_area_ha`
# for the fixed-ceiling cap.
#
# Grid: each polity-crop's area is spread to cells by .sci_grid_weights(), the
# per-cell share of the polity-crop's crop-pattern area on `country_grid` --
# the same weights the nitrogen driver grids crop NPP by. Cell areas sum back
# to the national one wherever the polity-crop has a cell; the rest is
# reported, not absorbed.
# Polity: the national layer unchanged.
.n_manure_crop_layer <- function(
  resolution,
  production,
  country_grid = NULL,
  crop_patterns = NULL
) {
  resolution <- rlang::arg_match0(resolution, c("grid", "polity"))
  national <- .sci_manure_crop_layer(production)
  if (resolution == "polity") {
    return(national)
  }
  .n_check_cell_support(country_grid)
  weights <- .sci_grid_weights(
    country_grid,
    crop_patterns %||% .sci_read_crop_patterns()
  )
  gridded <- national |>
    dplyr::mutate(area_code = as.integer(.data$territory)) |>
    dplyr::inner_join(
      dplyr::select(
        weights,
        "lon",
        "lat",
        "area_code",
        "item_prod_code",
        "area_weight"
      ),
      by = c("area_code", crop = "item_prod_code"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      sub_territory = .cell_id(.data$lon, .data$lat),
      crop = .data$crop,
      manure_n_receptivity = .data$manure_n_receptivity * .data$area_weight,
      crop_area_ha = .data$crop_area_ha * .data$area_weight
    )
  .n_report_unplaced_crop_area(national, gridded)
  gridded
}

.n_check_cell_support <- function(country_grid) {
  if (is.null(country_grid)) {
    cli::cli_abort(c(
      "Grid-resolution livestock inputs need a cell support.",
      i = "Pass {.arg country_grid}, the one support the heads, the grass
           ceiling and the crop layer are all placed on."
    ))
  }
  invisible(country_grid)
}

# Harvested area whose polity-crop has no cell on the support. It is not lost
# manure -- the manure sits where the animals are -- but hectares that cannot
# cap or receive it, so the share is said out loud.
.n_report_unplaced_crop_area <- function(national, gridded) {
  total <- sum(national$crop_area_ha)
  placed <- sum(gridded$crop_area_ha)
  if (!is.finite(total) || total <= 0) {
    return(invisible(NULL))
  }
  cli::cli_inform(
    c(
      i = "Manure crop layer: {signif(100 * (total - placed) / total, 3)}% of
           harvested area ({signif(total - placed, 4)} of {signif(total, 4)}
           ha) has no cell on the support."
    ),
    class = "whep_manure_crop_area_unplaced"
  )
  invisible(NULL)
}
