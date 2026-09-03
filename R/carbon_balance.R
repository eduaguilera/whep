# Historical gridded soil-organic-carbon balance (Module B, Task B2a-3). Ports
# the Spain_Hist SOC trajectory (R/SOC_Fun.R: Calc_equilibrium :220-233,
# Calc_SOC_evolution :315-418) to the WHEP cell x polity grain. The selected
# SOC turnover model (calculate_soc_dynamics()) is run to steady state under the
# first-year per-land-use carbon inputs to set per-class equilibrium densities;
# each cell is initialised by weighting those equilibria with the first-year
# land-use fractions; then stocks march forward year by year applying the
# model's annual mineralization-minus-input update and a land-use-change carbon
# transfer that conserves total cell carbon. Soil-organic-nitrogen change is
# derived from the annual carbon rate via the asymmetric soil C:N ratios.

#' Build the historical gridded soil-organic-carbon balance.
#'
#' @description
#' Reconstruct per-cell soil-organic-carbon stock trajectories: run the selected
#' turnover model to equilibrium under the earliest per-land-use carbon inputs,
#' initialise each cell by weighting those equilibria with the earliest
#' land-use fractions, march forward on yearly per-cell per-land-use areas
#' applying the model annual update plus a carbon-conserving land-use-change
#' transfer, and derive the soil-organic-nitrogen change from the carbon rate
#' via asymmetric soil carbon-to-nitrogen ratios.
#'
#' @details
#' \code{polity_validity} governs this function's own output. The internal
#' \code{\link{get_soc_climate_drivers}} read it falls back on always keeps its
#' rows: the march needs a climate modifier for every cell-year it steps
#' through, so dropping driver rows for an anachronistic polity label would
#' break the trajectory rather than relabel it. The driver read therefore warns
#' on its own key space (whep#462) while this argument decides the fate of the
#' balance rows.
#'
#' @param model Turnover model: one of \code{"hsoc"} (default), \code{"rothc"},
#'   \code{"icbm"}, \code{"amg"}, \code{"century"} or \code{"lpjml"}. The
#'   choice sets the equilibrium target, and through the time constant
#'   \code{soc_eq / c_input} the speed the stock relaxes toward it; the
#'   transient itself is a single exponential for every model.
#' @param init How each land-use class's opening stock is set.
#'   \code{"own_equilibrium"} (default) starts every class at the stock its own
#'   carbon input and climate support. \code{"cell_average"} starts every class
#'   in a cell at the fraction-weighted mean of the classes sharing it, the
#'   Spain historical behaviour: a proxy for land converted from something
#'   richer, at the cost of opening the lowest-input class far above its own
#'   target and draining it for decades, which the balance then reports as soil
#'   nitrogen mineralization. Recorded in \code{method_soc_init}.
#' @param resolution \code{"grid"} (default, per cell and land-use class) or
#'   \code{"polity"} (aggregated to \code{area_code} conserving carbon mass).
#' @param years Optional integer vector of calendar years to keep. \code{NULL}
#'   (default) keeps every year the inputs cover, but reading the full LUH2 range
#'   (850-2015) is infeasible turnkey, so a subset is strongly recommended when
#'   the default readers are used. Threaded into every default reader
#'   (\code{\link{read_luh2_landuse}}, \code{\link{get_soc_climate_drivers}} and
#'   \code{\link{build_carbon_inputs}}); ignored for inputs supplied via
#'   \code{data}.
#' @inheritParams build_water_balance
#' @param data Named list of pre-loaded inputs, each falling back to its reader
#'   when absent: \code{c_inputs} (per cell, land-use class and year, with
#'   \code{c_input_mgc_ha_yr} and \code{humified_fraction}); \code{land_use}
#'   (yearly per-cell per-class \code{lon}, \code{lat}, \code{area_code},
#'   \code{year}, \code{land_use}, \code{area_ha}); \code{climate} (either a
#'   precomputed per cell-year \code{climate_modifier}, applied to every
#'   land-use class alike, or the raw monthly drivers \code{temp_c} and
#'   \code{water_minus_pet_mm} keyed by \code{lon}, \code{lat},
#'   \code{area_code}, \code{year}, \code{month}, from which the selected
#'   model's native modifier is computed internally per land-use class: for the
#'   RothC/HSOC cover term the monthly vegetated soil-cover fraction is taken
#'   from the generic land-use curve \code{\link{soc_soil_cover_curve}} (a crop
#'   growth-stage canopy for cropland, sustained perennial cover for
#'   grassland/natural), so any \code{soil_cover} column supplied on the raw
#'   drivers is ignored); \code{clay} (per cell \code{clay_pct}); and an
#'   \code{natural_cover} (per cell and year, with \code{natural_cover}, the
#'   vegetated fraction of the natural stand, from
#'   \code{\link{read_lpjml_natural_cover}}); when supplied it replaces
#'   \code{\link{soc_soil_cover_curve}}'s constant for the NATURAL class only
#'   -- managed grassland has no measured cover to use and stays on the curve
#'   -- and when absent every class stays on the curve, which is the previous
#'   behaviour; \code{cropland_cover} (per cell, year and MONTH, from
#'   \code{\link{read_lpjml_crop_cover}}), which replaces the curve for the
#'   CROPLAND class with the cover its own crop calendar implies. The curve
#'   already gives cropland a season, but anchors it to the cell-year's
#'   warmest month: measured at 2010 the real crop mid-season falls there in
#'   only 5.2% of cropland cells and three or more months away in 51.0%, so
#'   the correction is one of timing rather than of annual mean (0.254 on the
#'   curve against 0.343 on the calendar); and an
#'   optional \code{equilibrium_climate} (the pre-industrial climatological
#'   normal, one representative monthly cycle per cell, used only for the
#'   equilibrium spin-up modifier while the forward march uses the year-specific
#'   drivers).
#' @param crop_groups How cropland is resolved into land-use classes; see
#'   [build_carbon_inputs()]. `list()` (default) keeps one `cropland` class.
#'   `list(method = "spain_hist")` marches crop GROUPS -- herbaceous crops
#'   pooled per irrigation regime, woody crops per species, rainfed and
#'   irrigated separate. Each cell-year's LUH2 cropland area is split over
#'   the groups in proportion to their crop-pattern area, so LUH2's total is
#'   kept. Herbaceous groups follow the annual crop cover (and the crop
#'   calendar); woody groups take a perennial cover of 0.85, an ASSUMED
#'   value with no sourced constant behind it yet. Soil cover is computed
#'   once per cover profile and joined to the classes, so the class count
#'   does not multiply the monthly climate table.
#' @param class_water How a cell's applied irrigation is shared among its
#'   land-use classes in the moisture term. `"cell"` (default) gives every
#'   class except natural land the cell-level water surplus, irrigation
#'   included, as before. `"regime"` concentrates the irrigation on the
#'   irrigated crop groups in proportion to their share of the cell and runs
#'   every other class on rain alone; the area-weighted mean over classes is
#'   the cell value either way. Needs `crop_groups`, because only groups
#'   carry a regime. Recorded in `method_class_water`.
#' @param density_basis Which crop area weights the per-crop carbon densities
#'   when they collapse to a class; see [build_carbon_inputs()]. `"static"`
#'   (default) keeps the crop-pattern weights, `"renormalised"` the yearly
#'   FAOSTAT-renormalised cell area the densities were computed on. Only read
#'   when the carbon inputs are built here rather than supplied.
#' @param example If \code{TRUE}, return a small fixture instead of reading
#'   remote data. Defaults to \code{FALSE}.
#' @section The land-use-change ledger closes on mass, not on density:
#' \code{luc_transfer_mgc_ha} is the carbon a class received (positive) or
#' gave up (negative) through land-use change, per hectare of the class's
#' CURRENT area. A class whose area falls to zero still gives up its whole
#' stock -- the balance carries the row at zero area and moves the carbon
#' into the growing classes -- but at zero hectares that outflow has no
#' per-hectare expression, so it is reported as 0 and
#' \code{sum(luc_transfer_mgc_ha * area_ha)} over a cell-year is then positive
#' by exactly the vanished stock. \code{luc_transfer_mgc} is the same
#' transfer as a signed mass in Mg C, on every row including the vanished
#' one, and sums to zero within every cell-year (and, at \code{"polity"}
#' resolution, is the summed mass). Check conservation on the mass column.
#'
#' @section Soil depth:
#' Every carbon and nitrogen density this function reports -- `stock_mgc_ha`,
#' `mineralization_mgc_ha`, `c_input_mgc_ha`, `luc_transfer_mgc_ha`,
#' `rate_mgc_ha` and `son_change_kgn_ha` -- is a **0-30 cm topsoil** quantity,
#' not a whole-profile one. The depth is a property of the model family, not a
#' free choice: HSOC comes from Aguilera et al. (2018), which states that "the
#' model was applied to the 0-30 cm layer of the soil"; the humification
#' fractions in [residue_humification] are that paper's Table 2; and the
#' RothC/HSOC climate modifier rescales RothC's own 0-23 cm maximum
#' topsoil-moisture-deficit expression to 30 cm
#' (`soc_rate_modifier_rothc(soil_depth_m = 0.3)`).
#'
#' Comparing this output against a whole-profile soil-carbon product is
#' therefore a category error. LPJmL's `soilc`, in particular, reports carbon
#' over its top 3 m and is roughly three times a topsoil stock; global
#' 0-30 cm references such as GSOCmap are the valid comparators. Stating this
#' is not pedantry -- an unstated depth convention is what made a chain of
#' contradictory diagnoses possible (whep#799).
#'
#' @section Spatial support:
#' Every default reader on the carbon path -- the land-use areas, the carbon
#' inputs, the climate drivers and the clay -- resolves its cell-to-polity table
#' through one polycell support ([read_polycell_support()]), read at a static
#' reference year. A cell shared between polities therefore delivers to each
#' only the land it holds there, and no reader can be left on a different
#' crosswalk: half the path on one footprint and half on another would surface
#' as an ordinary climate-coverage warning from the modifier join, not as an
#' error. Land the reporting vocabulary cannot key (no `area_code`) is reported
#' and dropped, never folded into another polity's.
#'
#' @return A tibble keyed by \code{(lon, lat, area_code, land_use, year)} at
#'   \code{"grid"} resolution (or \code{(area_code, year)} at \code{"polity"}),
#'   with \code{stock_mgc_ha}, \code{mineralization_mgc_ha}, \code{c_input_mgc_ha},
#'   \code{luc_transfer_mgc_ha}, \code{luc_transfer_mgc}, \code{rate_mgc_ha},
#'   \code{son_change_kgn_ha},
#'   \code{area_ha}, \code{method_soc} and \code{method_soc_init}, plus the
#'   polity columns below, plus
#'   \code{reporting_polity_out_of_span} when
#'   \code{polity_validity = "flag"}.
#' @inheritSection whep_polity_columns Polity columns
#' @source Aguilera, E., Guzman, G. I., Alvaro-Fuentes, J., Infante-Amate, J.,
#'   Garcia-Ruiz, R., Carranza-Gallego, G., Soto, D. & Gonzalez de Molina, M.
#'   (2018). A historical perspective on soil organic carbon in Mediterranean
#'   cropland (Spain, 1900-2008). *Science of the Total Environment*, 621,
#'   634-648. \doi{10.1016/j.scitotenv.2017.11.243}; land-use-change carbon
#'   transfer ported from the Spain historical pipeline.
#' @export
#' @examples
#' build_carbon_balance(example = TRUE)
build_carbon_balance <- function(
  model = c("hsoc", "rothc", "icbm", "amg", "century", "lpjml"),
  init = c("own_equilibrium", "cell_average"),
  resolution = c("grid", "polity"),
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  years = NULL,
  crop_groups = list(),
  class_water = c("cell", "regime"),
  density_basis = c("static", "renormalised"),
  example = FALSE
) {
  crop_groups <- .ci_group_config(crop_groups)
  class_water <- .cb_check_class_water(class_water, crop_groups)
  density_basis <- rlang::arg_match(density_basis)
  polity_validity <- rlang::arg_match(polity_validity)
  if (isTRUE(example)) {
    return(.resolve_polity_validity(
      .example_carbon_balance(),
      polity_validity
    ))
  }
  model <- rlang::arg_match(model)
  init <- rlang::arg_match(init)
  resolution <- rlang::arg_match(resolution)
  progress <- .cb_show_progress()
  if (progress) {
    cli::cli_progress_step("Reading model inputs (may read multi-GB rasters)")
  }
  d <- .cb_resolve_inputs(data, years, crop_groups, density_basis)
  d$class_water <- class_water
  if (progress) {
    cli::cli_progress_step("Computing per-class equilibrium")
  }
  classes <- .cb_class_table(d, model) |> .cb_attach_equilibrium(model)
  if (progress) {
    cli::cli_progress_step("Initialising soil-carbon pools")
  }
  init_stock <- .cb_initialise(classes, model, d, init)
  if (progress) {
    cli::cli_progress_done()
  }
  marched <- .cb_march(classes, init_stock)
  marched |>
    .cb_derive_son() |>
    dplyr::mutate(
      method_soc = model,
      method_soc_init = init,
      method_class_water = class_water
    ) |>
    .cb_finalise(resolution) |>
    .resolve_polity_validity(polity_validity)
}

# -- Input resolution ---------------------------------------------------------

.cb_resolve_inputs <- function(
  data,
  years = NULL,
  crop_groups = list(),
  density_basis = "static"
) {
  c_inputs <- data$c_inputs %||%
    .cb_read_c_inputs(years, crop_groups, density_basis)
  land_use <- data$land_use %||% .cb_read_land_use(years)
  climate <- data$climate %||% .cb_read_climate(years)
  # get_soc_climate_drivers() carries clay_pct in its own output, so a
  # turnkey (or clay_pct-bearing) climate table supplies the per-cell clay
  # directly; only fall back to the standalone HWSD clay reader when the
  # climate table lacks it (the precomputed climate_modifier path).
  clay <- data$clay %||% .cb_clay_from_climate(climate) %||% .cb_read_clay()
  list(
    c_inputs = c_inputs,
    land_use = land_use,
    climate = climate,
    clay = clay,
    natural_cover = data$natural_cover,
    cropland_cover = data$cropland_cover,
    equilibrium_climate = data$equilibrium_climate
  )
}

# Reuse the per-cell clay_pct the climate-driver table already carries, so the
# clay driving the turnover model and the clay driving the RothC/HSOC modifier
# come from one source. Returns NULL when the climate table has no clay_pct
# (the precomputed climate_modifier path), letting the caller fall back to the
# standalone HWSD clay reader.
.cb_clay_from_climate <- function(climate) {
  if (!rlang::has_name(climate, "clay_pct")) {
    return(NULL)
  }
  climate |>
    dplyr::select("lon", "lat", "clay_pct") |>
    dplyr::distinct()
}

# Join land-use areas, carbon inputs, the per-cell-year (and, for the raw-driver
# path, per-land-use) climate modifier and clay into one per-cell, per-land-use,
# per-year class table; add the cell-year land-use fraction. A land-use class
# with no carbon-input row (e.g. LUH2 `urban`, for which the input builders emit
# nothing) is kept as a zero-carbon class (`c_input` and `humified_fraction`
# coalesced to 0) rather than dropped, so `frac` still sums to 1 across the cell
# and that class's area share dilutes rather than deflates the cell equilibrium
# SOC. The climate modifier is either the precomputed `climate_modifier` column
# (back-compat, one value per cell-year, land-use-independent) or one derived
# from the raw monthly drivers via the selected model's native climate function,
# reduced PER LAND USE so the RothC/HSOC plant-cover term differs between
# cropland (crop growth-stage curve) and grassland/natural (perennial cover);
# see `.cb_climate_modifier_table()`. A cell-year with no modifier at all (no
# climate coverage) is dropped with a warning by `.cb_drop_uncovered_climate()`.
.cb_class_table <- function(d, model) {
  clay <- d$clay
  base <- .cb_split_cropland_groups(d$land_use, d$c_inputs) |>
    dplyr::mutate(
      frac = .data$area_ha / sum(.data$area_ha),
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    dplyr::left_join(
      d$c_inputs,
      by = c("lon", "lat", "area_code", "year", "land_use")
    ) |>
    dplyr::mutate(
      c_input_mgc_ha_yr = dplyr::coalesce(.data$c_input_mgc_ha_yr, 0),
      humified_fraction = dplyr::coalesce(.data$humified_fraction, 0)
    )
  modifiers <- .cb_climate_modifier_table(
    d$climate,
    clay,
    model,
    base$land_use,
    d$natural_cover,
    d$cropland_cover,
    class_water = .cb_class_water_spec(d$class_water, base)
  )
  base |>
    .cb_join_modifier(modifiers) |>
    dplyr::left_join(clay, by = c("lon", "lat")) |>
    .cb_drop_uncovered_climate()
}

# Split each cell-year's LUH2 cropland area over the crop groups the carbon
# inputs carry, in proportion to their `group_area_ha`. LUH2 knows how much
# cropland a cell has, not which crops are on it; the inputs know the crop
# areas but on the crop-pattern basis, not LUH2's. Proportional splitting
# keeps LUH2's total. A cell-year with cropland but no grouped inputs keeps
# its plain `cropland` row (zero carbon, as today), so nothing is dropped.
# An ungrouped run is returned untouched.
.cb_split_cropland_groups <- function(land_use, c_inputs) {
  groups <- c_inputs[
    .soc_is_cropland(c_inputs$land_use) & c_inputs$land_use != "cropland",
  ]
  if (nrow(groups) == 0L || !rlang::has_name(groups, "group_area_ha")) {
    return(land_use)
  }
  keys <- c("lon", "lat", "area_code", "year")
  shares <- groups |>
    dplyr::mutate(
      group_share = .data$group_area_ha / sum(.data$group_area_ha),
      .by = dplyr::all_of(keys)
    ) |>
    dplyr::filter(is.finite(.data$group_share)) |>
    dplyr::select(dplyr::all_of(keys), "land_use", "group_share")
  crop <- land_use[.soc_is_cropland(land_use$land_use), ]
  other <- land_use[!.soc_is_cropland(land_use$land_use), ]
  .cb_inform_undrawn_groups(groups, crop, keys)
  split <- crop |>
    dplyr::select(-"land_use") |>
    dplyr::inner_join(shares, by = keys, relationship = "many-to-many") |>
    dplyr::mutate(area_ha = .data$area_ha * .data$group_share) |>
    dplyr::select(-"group_share")
  unsplit <- dplyr::anti_join(crop, shares, by = keys)
  dplyr::bind_rows(other, split, unsplit)
}

# Grouped inputs whose cell-year has NO LUH2 cropland row draw no area and
# carry no carbon into the march: the crop patterns say crops are there, the
# land-use layer says no cropland is. That is a real disagreement between
# two sources, so it is counted and reported rather than dropped silently.
.cb_inform_undrawn_groups <- function(groups, crop, keys) {
  undrawn <- dplyr::anti_join(groups, crop, by = keys)
  if (nrow(undrawn) == 0L) {
    return(invisible(NULL))
  }
  cells <- dplyr::n_distinct(undrawn[keys])
  mha <- sum(undrawn$group_area_ha, na.rm = TRUE) / 1e6
  cli::cli_inform(c(
    "i" = "{nrow(undrawn)} grouped carbon-input row{?s} in {cells} cell-year{?s}
           ({format(mha, digits = 3)} Mha of crop-pattern area) fall where
           LUH2 has no cropland and draw no area: their carbon does not enter
           the march."
  ))
  invisible(NULL)
}

# Join the modifier table onto the class table. The raw-driver modifier table
# carries a `land_use` column (the modifier varies by class), so it is joined on
# the 5-key; the precomputed back-compat table has one modifier per cell-year,
# so it is joined on the 4-key and broadcast to every class.
.cb_join_modifier <- function(base, modifiers) {
  keys <- c("lon", "lat", "area_code", "year")
  if (rlang::has_name(modifiers, "land_use")) {
    dplyr::left_join(base, modifiers, by = c(keys, "land_use"))
  } else {
    dplyr::left_join(base, modifiers, by = keys)
  }
}

# Drop cell-years that have land-use and carbon-input coverage but no climate
# modifier. An NA here means the cell-year is missing from the climate table
# entirely (a real climate-data gap), never a merely driverless cell: the
# per-model native modifier path returns a legitimate 1 when only the raw driver
# columns are absent (soc_dynamics.R:80-81). Such cells cannot be modelled, so
# warn and drop them (surfacing the coverage loss) rather than aborting the whole
# run on a small gap, or silently running SOC turnover at an unmodified neutral 1.
.cb_drop_uncovered_climate <- function(classes) {
  missing <- classes |> dplyr::filter(is.na(.data$climate_modifier))
  if (nrow(missing) > 0) {
    gaps <- missing |>
      dplyr::distinct(.data$lon, .data$lat, .data$area_code, .data$year)
    cli::cli_warn(
      c(
        "!" = "Dropped {nrow(gaps)} cell-year{?s} with land-use/carbon-input
          coverage but no climate modifier (outside the climate-driver grid).",
        i = "Supply {.code data$climate} for these cell-years to retain them."
      )
    )
    classes <- classes |> dplyr::filter(!is.na(.data$climate_modifier))
  }
  classes
}

# -- Climate modifier resolution ----------------------------------------------

# Climate modifier the balance consumes. If `climate` already carries a
# `climate_modifier` column it is passed through unchanged (back-compat with the
# phase-2A injected path), one value per cell-year with no land-use dependence.
# Otherwise the raw monthly drivers are reduced to the selected model's native
# modifier PER (cell-year, land_use): each class's monthly `soil_cover` is
# attached first (see `.cb_attach_soil_cover()`), so the RothC/HSOC plant-cover
# term differs between cropland and perennial classes, then reduced via
# `.cb_year_climate_modifier()`. Models that do not consume `soil_cover` (ICBM,
# AMG, Century) get an identical modifier across classes. Clay is joined in
# because the RothC/HSOC modifier needs it.
.cb_climate_modifier_table <- function(
  climate,
  clay,
  model,
  land_use_classes,
  natural_cover = NULL,
  cropland_cover = NULL,
  class_water = NULL
) {
  keys <- c("lon", "lat", "area_code", "year")
  if (rlang::has_name(climate, "climate_modifier")) {
    return(dplyr::distinct(
      dplyr::select(climate, dplyr::all_of(c(keys, "climate_modifier")))
    ))
  }
  # One year at a time. Each cell-year's modifier is reduced from its own twelve
  # monthly rows, so nothing crosses years -- but attaching soil cover crosses
  # the MONTHLY table with every land-use class, which measures 0.452 GB per
  # simulated year against 0.097 GB for the drivers themselves. Held for the
  # whole span that intermediate is ~55 GB at 1901-2022, and it is what took the
  # full-span build to a 95.5 GB peak before it could reach the march (#624).
  groups <- .cb_year_row_groups(climate)
  parts <- lapply(groups, function(rows) {
    .cb_chunk_modifier(
      climate[rows, , drop = FALSE],
      clay,
      model,
      keys,
      land_use_classes,
      natural_cover,
      cropland_cover,
      class_water
    )
  })
  dplyr::bind_rows(parts)
}

# Row indices of each year, as ONE pass over the year column. Filtering the
# table per year instead (climate[climate$year == yr, ]) rescans every row once
# per year, which cost 12% of the build at a five-year span and would scale with
# the square of the span. Returning indices rather than frames also keeps the
# chunks lazy, so only one year is materialised at a time instead of a second
# copy of the whole table. A table with no year column is one group.
.cb_year_row_groups <- function(climate) {
  if (!rlang::has_name(climate, "year") || nrow(climate) == 0L) {
    return(list(seq_len(nrow(climate))))
  }
  split(seq_len(nrow(climate)), climate$year)
}

# The modifier for one chunk of the monthly climate table.
.cb_chunk_modifier <- function(
  climate,
  clay,
  model,
  keys,
  land_use_classes,
  natural_cover = NULL,
  cropland_cover = NULL,
  class_water = NULL
) {
  prepared <- climate |>
    .cb_join_clay(clay) |>
    .cb_arrange_by_month() |>
    .cb_attach_soil_cover(
      land_use_classes,
      natural_cover,
      cropland_cover
    ) |>
    .cb_attach_class_water(class_water)
  group_keys <- c(keys, "land_use")

  # Vectorised across cell-years where the shape allows it; NULL means fall
  # through to the per-group path below, which stays the reference.
  fast <- .cb_rothc_modifier_vectorised(prepared, model, group_keys)
  if (!is.null(fast)) {
    return(fast)
  }
  prepared |>
    dplyr::summarise(
      climate_modifier = .cb_year_climate_modifier(
        model,
        dplyr::pick(dplyr::everything()),
        dplyr::first(.data$clay_pct)
      ),
      .by = dplyr::all_of(group_keys)
    )
}

# The RothC/HSOC climate modifier for EVERY cell-year at once.
#
# The per-group path calls .cb_year_climate_modifier() once per
# (cell, year, land_use) -- ~1.2e6 groups over five years -- and each call
# allocates a list, dispatches rlang::has_name(), and runs purrr::accumulate()
# over twelve months. Profiling put ~20% of the march in tidyverse per-group
# machinery and ~9% in the accumulate, with no single line above 4.9%: the cost
# is dispatch, not arithmetic (#630).
#
# The topsoil-moisture-deficit recurrence is sequential over MONTHS but
# independent across CELLS, so the loop inverts: twelve vectorised steps over all
# groups, instead of ~1.2e6 twelve-step accumulations. The arithmetic below is
# the same as soc_rate_modifier_rothc() and .rothc_moisture_factor(), kept
# deliberately line-for-line comparable with them.
#
# Returns NULL -- deferring to the per-group path -- when this cannot be trusted:
# a non-RothC model, a missing driver, or ragged groups (unequal month counts),
# where the matrix reshape would silently misalign months across cells.
.cb_rothc_modifier_vectorised <- function(prepared, model, group_keys) {
  if (!model %in% c("hsoc", "rothc")) {
    return(NULL)
  }
  drivers <- c("temp_c", "water_minus_pet_mm", "clay_pct", "soil_cover")
  if (!all(purrr::map_lgl(drivers, \(d) rlang::has_name(prepared, d)))) {
    return(NULL)
  }
  if (!rlang::has_name(prepared, "month")) {
    return(NULL)
  }

  dt <- data.table::as.data.table(prepared)
  # Number the groups by FIRST APPEARANCE, and order by that rather than by the
  # key columns, so the output row order matches the per-group path exactly.
  # Sorted order would carry the same values, but it reaches the downstream
  # aggregates in a different sequence, and floating-point addition is not
  # associative: it perturbed mineralization/rate/son_change by ~1e-15 -- small,
  # but this change has to be a no-op, not nearly one.
  dt[, ".grp" := .GRP, by = group_keys]
  data.table::setorderv(dt, c(".grp", "month"))
  counts <- dt[, list(.n = .N), by = c(group_keys, ".grp")]
  data.table::setorderv(counts, ".grp")
  if (data.table::uniqueN(counts$.n) != 1L) {
    return(NULL)
  }
  n_months <- counts$.n[[1L]]
  if (n_months < 1L) {
    return(NULL)
  }

  # byrow: rows are groups, columns months, matching the sort above.
  as_mat <- function(x) matrix(x, ncol = n_months, byrow = TRUE)
  temp <- as_mat(dt$temp_c)
  balance <- as_mat(dt$water_minus_pet_mm)
  cover <- as_mat(dt$soil_cover)
  # clay is a per-group scalar; the per-group path takes dplyr::first().
  clay <- as_mat(dt$clay_pct)[, 1L]

  # Undefined at -18.27 C: below the asymptote the expression wraps back to ~47.91
  # instead of zero decomposition, so it is floored, exactly as in the scalar fn.
  a <- ifelse(temp <= -18.27, 0, 47.91 / (1 + exp(106.06 / (temp + 18.27))))

  # Same depth rescaling as the scalar soc_rate_modifier_rothc(); both read the
  # one accessor so a change to the topsoil layer cannot reach one path only.
  max_tsmd <- .soc_topsoil_depth_m() *
    100 *
    (-(20 + 1.3 * clay - 0.01 * clay^2)) /
    23

  # tsmd[, 1] = max(min(balance_1, 0), max_tsmd), then carried forward. pmin/pmax
  # propagate NA the same way min/max do here (both na.rm = FALSE), so an NA month
  # still poisons its group's later months as before.
  tsmd <- matrix(NA_real_, nrow = nrow(balance), ncol = n_months)
  tsmd[, 1L] <- pmax(pmin(balance[, 1L], 0), max_tsmd)
  for (m in seq_len(n_months)[-1L]) {
    tsmd[, m] <- pmax(pmin(tsmd[, m - 1L] + balance[, m], 0), max_tsmd)
  }

  threshold <- 0.444 * max_tsmd
  max_mat <- matrix(max_tsmd, nrow = nrow(tsmd), ncol = n_months)
  thr_mat <- matrix(threshold, nrow = nrow(tsmd), ncol = n_months)
  b <- ifelse(
    tsmd > thr_mat,
    1,
    0.2 + 0.8 * (max_mat - tsmd) / (max_mat - thr_mat)
  )
  b <- pmax(b, 0.2)

  cover_factor <- 0.6 + 0.4 * (1 - cover)

  # Reduce with the same mean() the scalar path calls, NOT rowMeans(). mean()
  # accumulates in long double and applies a second-pass correction that
  # rowMeans() omits, so the two disagree by 1 ulp roughly once in 3e5 rows --
  # rare enough to survive a 200-group test, but with ~1.2e6 cell-years it hits,
  # and the march amplifies it to ~1e-15 in mineralization and ~1e-13 in
  # son_change. Replicating the correction in R does not help: R arithmetic is
  # double, not long double, and lands further away than rowMeans does. At ~3 s
  # per million groups against the ~200 s this function saves, calling the real
  # mean() is the cheap way to stay exact. Do not "optimise" this to rowMeans().
  products <- a * b * cover_factor
  modifier <- vapply(
    seq_len(nrow(products)),
    function(i) mean(products[i, ], na.rm = TRUE),
    numeric(1)
  )

  out <- counts[, group_keys, with = FALSE]
  out[, "climate_modifier" := modifier]
  # as.data.frame() first: as_tibble() on a data.table carries its
  # .internal.selfref pointer out as an attribute, which makes the result
  # compare unequal to the per-group path under all.equal() despite every
  # column being identical.
  tibble::as_tibble(as.data.frame(out))
}

# get_soc_climate_drivers() already embeds clay_pct in its own output (RothC/
# HSOC need it as a climate driver too); joining the separately-supplied `clay`
# on top would silently suffix both to clay_pct.x/clay_pct.y and break the
# .data$clay_pct read, so only join `clay` in when `climate` lacks it.
.cb_join_clay <- function(climate, clay) {
  if (rlang::has_name(climate, "clay_pct")) {
    climate
  } else {
    dplyr::left_join(climate, clay, by = c("lon", "lat"))
  }
}

# Cross the monthly climate rows with every land-use class present in the
# cell-year and attach each class's monthly vegetated soil-cover fraction. For
# cropland the fraction follows the generic crop growth-stage canopy curve
# (`whep::soc_soil_cover_curve`) aligned so the peak-canopy (mid-season) month is
# the cell-year's warmest month, with the remaining fallow/off-season months at
# a low bare-soil cover; grassland and natural carry a sustained perennial cover
# year-round. A class absent from the curve table (e.g. urban) defaults to bare
# soil (soil_cover 0), preserving the prior behaviour for those classes.
.cb_attach_soil_cover <- function(
  climate,
  land_use_classes,
  natural_cover = NULL,
  cropland_cover = NULL
) {
  classes <- unique(land_use_classes)
  # Crossed by cover PROFILE, not by class. Crop groups can number eighty
  # (woody species x regime) and share five profiles between them; crossing
  # the monthly climate with every class would multiply the largest table in
  # the balance twentyfold. Each class is joined to its profile's rows after.
  profile_of <- tibble::tibble(
    land_use = classes,
    .cover_key = .cb_cover_profile(classes)
  )
  climate |>
    dplyr::select(-dplyr::any_of("soil_cover")) |>
    dplyr::mutate(
      months_from_peak = .cb_months_from_peak(.data$month, .data$temp_c),
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    tidyr::crossing(.cover_key = unique(profile_of$.cover_key)) |>
    dplyr::mutate(.curve_key = .cb_curve_key(.data$.cover_key)) |>
    dplyr::left_join(
      .cb_cover_curve(),
      by = c(".curve_key" = "land_use", "months_from_peak")
    ) |>
    dplyr::select(-".curve_key") |>
    dplyr::mutate(soil_cover = dplyr::coalesce(.data$soil_cover, 0)) |>
    .cb_apply_natural_cover(natural_cover, key = ".cover_key") |>
    .cb_apply_crop_cover(cropland_cover, key = ".cover_key") |>
    dplyr::inner_join(
      profile_of,
      by = ".cover_key",
      relationship = "many-to-many"
    ) |>
    dplyr::select(-".cover_key")
}

# The soil-cover profile a class follows. Plain cropland follows the annual
# crop curve (and the pooled crop-calendar override); herbaceous crop groups
# follow it per irrigation regime (`cropland_rainfed`, `cropland_irrigated`),
# which only differs from plain cropland once a per-regime crop calendar is
# supplied; woody crop groups follow a perennial cover; grassland and natural
# keep their own rows. Anything else keeps its lowercase label and, absent
# from the curve, runs bare -- the previous behaviour for urban. Idempotent:
# a profile maps to itself, so a lookup can be keyed on it twice.
.cb_cover_profile <- function(land_use) {
  key <- stringr::str_to_lower(land_use)
  crop <- .soc_is_cropland(key)
  herb <- crop & stringr::str_detect(key, "_herbaceous$")
  woody <- crop & !herb & key != "cropland"
  dplyr::case_when(
    key %in% .cb_cover_profiles ~ key,
    woody ~ "woody_cropland",
    herb & stringr::str_starts(key, "cropland_irrigated_") ~
      "cropland_irrigated",
    herb ~ "cropland_rainfed",
    crop ~ "cropland",
    TRUE ~ key
  )
}

.cb_cover_profiles <- c(
  "cropland",
  "cropland_rainfed",
  "cropland_irrigated",
  "woody_cropland"
)

# The curve row a profile reads: the two regime profiles share cropland's.
.cb_curve_key <- function(profile) {
  dplyr::if_else(
    profile %in% c("cropland_rainfed", "cropland_irrigated"),
    "cropland",
    profile
  )
}

# Replace natural land's constant soil cover with the cover LPJmL grew.
#
# `soc_soil_cover_curve` gives natural land 0.85 in every month of every
# cell, so the RothC plant-retainment term 0.6 + 0.4 * (1 - cover) is a fixed
# 0.66 in the Sahel and in the Amazon alike. Measured against LPJmL foliar
# projective cover, that constant is close on the MEAN (0.858 in 1901 rising
# to 0.884 in 2023) and wrong in the DISTRIBUTION: the median natural cell is
# fully covered at 1.000 and the 5th percentile is bare at ~0.00, so 0.85 sits
# between two states that between them hold most of the land.
#
# The tail is what matters. In a near-bare cell the retainment factor goes
# 0.66 -> 1.00, decomposition runs half again as fast and equilibrium carbon
# falls about a third -- and those arid cells are exactly where the model is
# furthest from observation (tropical grass 3.05x, temperate grass 2.55x,
# against tundra at 1.88x). Global mean effect ~1.8%; per cell 0.66x to 1.10x.
#
# Managed grassland necessarily stays on the curve: `fpc.nc` carries the
# natural stand only, so there is no measured cover for it to use.
#
# A NULL layer leaves every class on the curve, which is the previous
# behaviour exactly.
# Replace cropland's curve cover with the cover its crop calendar implies.
#
# soc_soil_cover_curve gives cropland a real season, but anchors it to the
# cell-year's WARMEST month as a stand-in for peak canopy. Measured at 2010
# over 18,548 cropland cells, the area-weighted crop mid-season falls in the
# warmest month in 5.2% of them and three or more months away in 51.0%, a
# median absolute offset of three months. That is a timing error, not a level
# one -- the curve averages 0.254 cover against the calendar's 0.343 -- so it
# shows up as modelled canopy over real fallow rather than as a wrong annual
# mean.
#
# Keyed on month as well as year, unlike the natural override: the natural
# layer is one cover per cell-year, this one is twelve.
.cb_apply_crop_cover <- function(prepared, cropland_cover, key = "land_use") {
  if (is.null(cropland_cover) || nrow(cropland_cover) == 0L) {
    return(prepared)
  }
  lookup <- .cb_crop_cover_lookup(cropland_cover)
  prepared |>
    dplyr::mutate(.crop_profile = .cb_cover_profile(.data[[key]])) |>
    dplyr::left_join(
      lookup,
      by = c("lon", "lat", "year", "month", ".crop_profile")
    ) |>
    dplyr::mutate(
      soil_cover = dplyr::if_else(
        !is.na(.data$cropland_cover),
        .data$cropland_cover,
        .data$soil_cover
      )
    ) |>
    dplyr::select(-"cropland_cover", -".crop_profile")
}

# One cover per cell-month and annual-crop profile. A pooled layer (no
# `regime` column, the `read_lpjml_crop_cover()` default) serves plain
# cropland and both herbaceous regime profiles alike. A per-regime layer
# (`by = "regime"`) serves `cropland_rainfed` and `cropland_irrigated` from
# their own bands, and plain cropland from the two pooled by cropped area --
# the same number the pooled read would have given.
.cb_crop_cover_lookup <- function(cropland_cover) {
  annual <- setdiff(.cb_cover_profiles, "woody_cropland")
  if (!rlang::has_name(cropland_cover, "regime")) {
    .check_columns(
      cropland_cover,
      c("lon", "lat", "year", "month", "cropland_cover"),
      "data$cropland_cover"
    )
    pooled <- cropland_cover |>
      dplyr::select("lon", "lat", "year", "month", "cropland_cover") |>
      dplyr::distinct()
    return(tidyr::crossing(pooled, .crop_profile = annual))
  }
  .check_columns(
    cropland_cover,
    c(
      "lon",
      "lat",
      "year",
      "month",
      "regime",
      "cropped_frac",
      "cropland_cover"
    ),
    "data$cropland_cover"
  )
  unknown <- setdiff(unique(cropland_cover$regime), c("rainfed", "irrigated"))
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{.arg data$cropland_cover} carries unknown regime{?s} {.val {unknown}}.",
      i = "Expected {.val rainfed} and {.val irrigated}, as
           {.fn read_lpjml_crop_cover} with {.code by = \"regime\"} writes."
    ))
  }
  per_regime <- cropland_cover |>
    dplyr::select(
      "lon",
      "lat",
      "year",
      "month",
      "regime",
      "cropped_frac",
      "cropland_cover"
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(.crop_profile = paste0("cropland_", .data$regime))
  pooled <- per_regime |>
    dplyr::summarise(
      cropland_cover = stats::weighted.mean(
        .data$cropland_cover,
        .data$cropped_frac
      ),
      .by = c("lon", "lat", "year", "month")
    ) |>
    dplyr::mutate(.crop_profile = "cropland")
  dplyr::bind_rows(
    dplyr::select(per_regime, -"regime", -"cropped_frac"),
    pooled
  )
}
.cb_apply_natural_cover <- function(prepared, natural_cover, key = "land_use") {
  if (is.null(natural_cover) || nrow(natural_cover) == 0L) {
    return(prepared)
  }
  .check_columns(
    natural_cover,
    c("lon", "lat", "year", "natural_cover"),
    "data$natural_cover"
  )
  prepared |>
    dplyr::left_join(
      dplyr::distinct(
        dplyr::select(natural_cover, "lon", "lat", "year", "natural_cover")
      ),
      by = c("lon", "lat", "year")
    ) |>
    dplyr::mutate(
      soil_cover = dplyr::if_else(
        stringr::str_to_lower(.data[[key]]) == "natural" &
          !is.na(.data$natural_cover),
        .data$natural_cover,
        .data$soil_cover
      )
    ) |>
    dplyr::select(-"natural_cover")
}

# Put natural land back on its rainfed water balance.
#
# `water_minus_pet_mm` arrives from the drivers as a CELL-level surplus that
# already includes the cell's irrigation -- `(precip_mm + irrig_mm) - pet_mm`
# at R/water_balance.R:829 -- while this modifier table is built per land-use
# class. Leaving it untouched therefore waters the natural vegetation of every
# irrigated cell with water it never received, raising its moisture term and
# so its decomposition rate. Natural land is returned to `precip_mm - pet_mm`,
# which the driver table carries because `precip_mm` is precipitation alone
# (R/water_balance.R:156-157).
#
# Cropland and managed grassland keep the cell-level value: dividing the
# cell's irrigation between them, and between each one's rainfed and irrigated
# stands, needs a per-crop irrigation layer, and the LPJmL run exposes
# irrigation either monthly (`mirrig`, no crop dimension) or per crop
# (`cft_nir`, annual), never both.
#
# The moisture term is concave -- capped at 1 once the soil is wet -- so
# spreading a cell's irrigation evenly is not neutral: it overstates the mean
# response relative to concentrating it where the water actually goes.
#
# A climate table lacking `precip_mm`/`pet_mm` cannot separate rain from
# irrigation (the precomputed-`climate_modifier` path, or a caller supplying
# only the RothC drivers). It is passed through exactly as supplied rather
# than guessed at.
#
# `class_water = "regime"` (a spec from `.cb_class_water_spec()`) goes one step
# further once crop GROUPS are marched: the cell's applied irrigation, which
# the driver carries as a cell-mean depth, is concentrated on the irrigated
# groups in proportion to their share of the cell (`irrigated_frac`), and
# every other class -- rainfed groups, grassland, natural -- runs on rain
# alone. The area-weighted mean over classes still equals the cell value, so
# no water is created or lost; it is only put where it was applied. A
# cell-year with irrigation but no irrigated class (cropland that stayed
# unsplit) keeps the `"cell"` rule, so its irrigation is not dropped.
.cb_attach_class_water <- function(prepared, class_water = NULL) {
  needed <- c("precip_mm", "pet_mm", "water_minus_pet_mm", "land_use")
  if (!all(purrr::map_lgl(needed, \(x) rlang::has_name(prepared, x)))) {
    return(prepared)
  }
  regime <- !is.null(class_water) && identical(class_water$method, "regime")
  prepared |>
    .cb_join_irrigated_frac(if (regime) class_water$irrigated_frac) |>
    dplyr::mutate(
      .rain = .data$precip_mm - .data$pet_mm,
      .irrig = .data$water_minus_pet_mm - .data$.rain,
      .natural = stringr::str_to_lower(.data$land_use) == "natural",
      .irrigated = .soc_is_irrigated_class(.data$land_use),
      water_minus_pet_mm = dplyr::case_when(
        .data$.irrigated_frac > 0 & .data$.irrigated ~
          .data$.rain + .data$.irrig / .data$.irrigated_frac,
        .data$.irrigated_frac > 0 ~ .data$.rain,
        .data$.natural ~ .data$.rain,
        TRUE ~ .data$water_minus_pet_mm
      )
    ) |>
    dplyr::select(
      -".rain",
      -".irrig",
      -".natural",
      -".irrigated",
      -".irrigated_frac"
    )
}

# Attach each cell-year's irrigated share of the cell as `.irrigated_frac`;
# zero everywhere when no spec is given (the `"cell"` rule) or where the
# cell-year has no irrigated class. Joined on whichever of the four keys the
# table carries, so the year-less equilibrium normal joins on the cell alone.
.cb_join_irrigated_frac <- function(prepared, irrigated_frac) {
  if (is.null(irrigated_frac)) {
    return(dplyr::mutate(prepared, .irrigated_frac = 0))
  }
  keys <- intersect(c("lon", "lat", "area_code", "year"), names(prepared))
  share <- irrigated_frac |>
    dplyr::summarise(
      .irrigated_frac = sum(.data$irrigated_frac),
      .by = dplyr::all_of(keys)
    )
  prepared |>
    dplyr::left_join(share, by = keys) |>
    dplyr::mutate(.irrigated_frac = dplyr::coalesce(.data$.irrigated_frac, 0))
}

# The per-class water rule as a spec the modifier chain carries: the method
# and, for `"regime"`, each cell-year's irrigated share of the cell from the
# class table's area fractions. NULL for `"cell"`, the status quo.
.cb_class_water_spec <- function(method, classes) {
  if (is.null(method) || method != "regime") {
    return(NULL)
  }
  irrigated <- classes |>
    dplyr::filter(.soc_is_irrigated_class(.data$land_use)) |>
    dplyr::summarise(
      irrigated_frac = sum(.data$frac),
      .by = c("lon", "lat", "area_code", "year")
    )
  list(method = "regime", irrigated_frac = irrigated)
}

# `"regime"` needs classes that can carry the irrigation; without crop groups
# there are none and the option would silently do nothing, so it is refused.
.cb_check_class_water <- function(class_water, crop_groups) {
  class_water <- rlang::arg_match(class_water, c("cell", "regime"))
  if (class_water == "regime" && identical(crop_groups$method, "none")) {
    cli::cli_abort(c(
      "{.arg class_water} = {.val regime} needs crop groups to carry the
       irrigation.",
      i = "Pass {.code crop_groups = list(method = \"spain_hist\")}, or keep
           {.arg class_water} = {.val cell}."
    ))
  }
  class_water
}
# Signed month offset of each month from the cell-year's warmest (peak-canopy)
# month, on a 12-month circle mapped to -5..6 (0 = the warmest month). Aligns
# the crop cover curve's mid-season peak to the growing-season temperature peak,
# which auto-handles both hemispheres from the temperature seasonality alone.
.cb_months_from_peak <- function(month, temp_c) {
  peak <- month[which.max(temp_c)]
  raw <- (month - peak) %% 12
  dplyr::if_else(raw <= 6, raw, raw - 12L)
}

# The generic land-use monthly soil-cover curve, matched to lowercase land-use
# labels so the LUH2 reader's classes (cropland, grassland, natural) resolve.
.cb_cover_curve <- function() {
  whep::soc_soil_cover_curve |>
    dplyr::mutate(land_use = stringr::str_to_lower(.data$land_use))
}

# Order the monthly climate rows by month within each cell-year so the RothC
# topsoil-moisture-deficit accumulation sees January-to-December sequence.
.cb_arrange_by_month <- function(climate) {
  if (rlang::has_name(climate, "month")) {
    dplyr::arrange(climate, .data$month)
  } else {
    climate
  }
}

# Reduce one cell-year's monthly raw climate drivers to the selected model's
# native climate rate modifier, reusing the same `.soc_climate_modifier()` path
# `calculate_soc_dynamics()` uses so the modifier always matches `model`. Monthly
# driver columns (e.g. temp_c, water_minus_pet_mm) ride in as vectors; the scalar
# covariates (clay_pct, and soil_cover defaulting to 0 = bare soil when absent)
# are supplied alongside. Returns 1 (neutral) when the required drivers are
# absent, mirroring `.soc_climate_modifier()`.
.cb_year_climate_modifier <- function(model, months, clay_pct) {
  drivers <- as.list(months)
  drivers$clay_pct <- clay_pct
  if (!rlang::has_name(months, "soil_cover")) {
    drivers$soil_cover <- 0
  }
  .soc_climate_modifier(model, drivers)
}

# -- Equilibrium + initialisation ---------------------------------------------

# Per-class equilibrium SOC density: run the selected model to steady state
# under each class's carbon input and climate. Reuses calculate_soc_dynamics();
# never reimplements the pool kinetics. One run per distinct input combination.
.cb_equilibrium <- function(model, classes) {
  combos <- classes |>
    dplyr::distinct(
      .data$land_use,
      .data$c_input_mgc_ha_yr,
      .data$humified_fraction,
      .data$climate_modifier,
      .data$clay_pct
    )
  # Models with a closed-form equilibrium compute it vectorised over the
  # distinct combinations rather than running a 5000-year spin-up per
  # combination. At global grain the near-continuous climate/clay values barely
  # dedupe, so the old per-combination trajectory dominated the whole run; the
  # closed form is the exact point that spin-up converges to (see #352).
  #
  # All six models have one wired now, so the spin-up below is unreachable in
  # production. It stays because it is the oracle `test_carbon_balance.R` runs
  # FIVE of the six closed forms against, which is what catches an edited rate
  # constant or pool structure quietly ceasing to be the fixed point of the
  # model's own kinetics -- an equality test on the formula alone cannot. The
  # fall-through stays for the next model added before its closed form is
  # derived.
  #
  # LPJmL is the sixth and is NOT checked this way: its slow pool e-folds in
  # 1,000 / response years, so a 5,000-year spin-up has not converged and
  # would fail the comparison for being the wrong oracle rather than for any
  # defect in the formula. It is guarded instead by the property that defines
  # an equilibrium -- that the trajectory started there stays there.
  #
  # The spin-up also stops being a usable oracle for the other five below a
  # climate modifier of roughly 0.3: at 5,000 years Century breaches its own
  # 1e-4 tolerance near cm 0.27 and ICBM its 1e-3 near cm 0.178, and in both
  # cases it is the trajectory that has not arrived, not the closed form that
  # is wrong. That regime is not exotic -- every closed form scales as
  # 1 / climate_modifier, so it is exactly where the equilibrium is largest,
  # and natural land sits in it.
  closed <- .cb_closed_form_equilibrium(model, combos)
  if (!is.null(closed)) {
    return(.cb_check_equilibrium(dplyr::mutate(combos, soc_eq_mgc_ha = closed)))
  }
  dplyr::mutate(
    combos,
    soc_eq_mgc_ha = purrr::pmap_dbl(
      list(
        .data$c_input_mgc_ha_yr,
        .data$humified_fraction,
        .data$climate_modifier,
        .data$clay_pct
      ),
      \(input, hf, cm, clay) .cb_steady_state(model, input, hf, cm, clay)
    )
  ) |>
    .cb_check_equilibrium()
}

# Every closed-form equilibrium is proportional to 1 / climate_modifier, and
# each model's modifier reaches exactly zero somewhere real: RothC/HSOC at or
# below -18.27 C, ICBM below -3.78 C, AMG below 0 C, Century at or above 45 C.
# A zero there yields Inf (or NaN at zero carbon input), which is not caught
# downstream -- `.cb_init_density()` spreads it over every land-use class in
# the cell through `sum(frac * soc_eq)`, and the march's `fifelse` then reads
# the Inf back as an effective rate of 0, so the cell accumulates carbon
# forever and mineralizes none. Failing here names the cells instead.
.cb_check_equilibrium <- function(eq) {
  bad <- !is.finite(eq$soc_eq_mgc_ha)
  if (!any(bad)) {
    return(eq)
  }
  cli::cli_abort(c(
    "Equilibrium soil carbon is not finite for \\
    {sum(bad)} row{?s}.",
    "i" = "The equilibrium scales as 1 / {.field climate_modifier}, which is \\
      {.val {signif(min(eq$climate_modifier[bad]), 3)}} at the worst of them.",
    "x" = "A non-finite equilibrium silently becomes a cell that never \\
      mineralizes, so it is refused rather than marched."
  ))
}

# Vectorised closed-form equilibrium SOC density for the models that have one,
# else NULL (caller falls back to the per-combination spin-up). Each formula is
# the fixed point of that model's dynamics under a constant carbon input and
# scalar climate modifier -- the exact stock its 5000-year spin-up relaxes to;
# climate_modifier scales the decomposition rates exactly as the model does.
.cb_closed_form_equilibrium <- function(model, combos) {
  input <- combos$c_input_mgc_ha_yr
  cm <- combos$climate_modifier
  switch(
    model,
    hsoc = .cb_hsoc_equilibrium(
      input,
      combos$humified_fraction,
      cm,
      combos$clay_pct
    ),
    icbm = .cb_icbm_equilibrium(input, cm),
    lpjml = .cb_lpjml_equilibrium(input, cm),
    amg = .cb_amg_equilibrium(input, cm),
    century = .cb_century_equilibrium(
      input,
      cm,
      combos$clay_pct,
      # Present only when the caller supplied a silt layer; NA falls back to
      # the tabulated placeholder inside .century_silt(). `has_name()` rather
      # than `$`: on a tibble a missing column WARNS rather than returning
      # NULL, so `%||%` would fire "Unknown or uninitialised column" on every
      # Century build that has no silt.
      if (rlang::has_name(combos, "silt_pct")) combos$silt_pct else NA
    ),
    rothc = .cb_rothc_equilibrium(
      input,
      cm,
      combos$clay_pct,
      combos$humified_fraction
    ),
    NULL
  )
}

# ICBM: young pool at input / k_y and old pool at h * input / k_o, both rates
# scaled by the climate modifier (see calculate_soc_icbm()). This is the true
# t -> infinity fixed point; it differs from the previous value by up to ~1e-4
# where the slow old pool had not fully converged in the 5000-year spin-up.
.cb_icbm_equilibrium <- function(input, climate_modifier) {
  k_young <- .soc_param("icbm", "young", "decomposition_rate") *
    climate_modifier
  k_old <- .soc_param("icbm", "old", "decomposition_rate") * climate_modifier
  h <- .soc_param("icbm", "transfer", "humification_coefficient")
  input / k_young + h * input / k_old
}

# LPJmL: the two mineral-soil pools at their fixed points. Of the litter
# carbon that DECOMPOSES, (1 - atmfrac) survives respiration and is split
# fastfrac / (1 - fastfrac) between pools decaying at k_fast and k_slow, both
# scaled by the response. Neither pool feeds the other and there is no inert
# term, so the total is just the two fixed points: the soil-bound input --
# the decomposed litter, less the share respired straight to the
# atmosphere -- divided between the pools, the fast share over its rate plus
# the slow share over its rate, all over the response. With the run's
# parameters that comes to 22.25 years.
#
# The distinction matters and this comment used to get it wrong. In LPJmL the
# fraction multiplies the flux LEAVING the litter pool, never the litterfall
# entering it; the two coincide only at litter steady state. Since this is an
# equilibrium expression, using it here is exact -- but the same wording was
# also on the trajectory function, where it is not, and it contradicted
# soc_turnover_params' own description of the parameter. The table was right.
#
# This is algebraically what LPJmL's own equilsoil() converges to once its
# per-layer c_shift weights are summed, because the layer weights are normalised
# to one and the decay rate cancels out of them (Schaphoff et al. 2018
# Eqs. 98-100). The slow pool takes 2% of the input and holds about 45% of the
# stock, which is why LPJmL solves this rather than spinning it up.
.cb_lpjml_equilibrium <- function(input, climate_modifier) {
  k_fast <- .cb_param("lpjml", "fast") * climate_modifier
  k_slow <- .cb_param("lpjml", "slow") * climate_modifier
  fast_share <- .soc_param("lpjml", "soil", "fast_fraction")
  soil_in <- input * (1 - .soc_param("lpjml", "litter", "atmosphere_fraction"))
  soil_in * fast_share / k_fast + soil_in * (1 - fast_share) / k_slow
}

# AMG: active pool at its steady state ca_ss = h * input / k (k scaled by the
# climate modifier); the total adds the inert stable share, giving
# ca_ss / (1 - f_iom) (see calculate_soc_amg()'s steady_state init).
.cb_amg_equilibrium <- function(input, climate_modifier) {
  k <- .soc_param("amg", "active", "decomposition_rate") * climate_modifier
  f_iom <- .soc_param("amg", "stable", "inert_fraction")
  (.amg_default_h() * input / k) / (1 - f_iom)
}

# Century: the 5-pool linear ODE (str, met, act, slw, pas) fixed point. The
# structural/metabolic pools sit at their inflow / rate; the active/slow/passive
# pools solve the 3-way transfer loop (act <-> slw <-> pas) analytically. This
# is the true t -> infinity steady state -- it differs from the previous
# 5000-year `deSolve` value where the very slow passive pool had not converged.
.cb_century_equilibrium <- function(
  input,
  climate_modifier,
  clay_pct,
  silt_pct = NA
) {
  p <- .cb_century_coefs(climate_modifier, clay_pct, silt_pct)
  out_str <- p$fs * input
  out_met <- p$fm * input
  denom <- 1 -
    p$a_slw_act * p$a_act_slw -
    p$a_pas_act * p$a_act_pas -
    p$a_pas_act * p$a_slw_pas * p$a_act_slw
  from_str_met <- p$a_str_act * out_str + p$a_met_act * out_met
  from_slw_loop <- (p$a_slw_act + p$a_pas_act * p$a_slw_pas) *
    p$a_str_slw *
    out_str
  out_act <- (from_str_met + from_slw_loop) / denom
  out_slw <- p$a_str_slw * out_str + p$a_act_slw * out_act
  out_pas <- p$a_act_pas * out_act + p$a_slw_pas * out_slw
  out_str /
    p$k_str +
    out_met / p$k_met +
    out_act / p$k_act +
    out_slw / p$k_slw +
    out_pas / p$k_pas
}

# Vectorised Century rates and inter-pool transfer fractions from the climate
# modifier and clay (mirrors .century_params/.century_texture/.century_rates/
# .century_transfers). fm/fs (metabolic/structural input split) depend only on
# the constant lignin:N ratio, so they are scalars.
.cb_century_coefs <- function(
  climate_modifier,
  clay_pct,
  silt_pct = NA
) {
  ls <- .soc_param("century", "defaults", "lignin_fraction")
  ln <- .soc_param("century", "defaults", "lignin_n_ratio")
  silt <- .century_silt(silt_pct)
  weeks <- .soc_param("century", "all", "weeks_per_year")
  base <- .soc_rates_named("century", "base_rate_weekly")
  txtr <- pmin(pmax(pmin(clay_pct, 100), 0) / 100 + silt / 100, 1)
  f_txtr <- .soc_param("century", "act", "texture_intercept") -
    .soc_param("century", "act", "texture_slope") * txtr
  es <- .soc_param("century", "act", "respiration_intercept") -
    .soc_param("century", "act", "respiration_texture_slope") * txtr
  fm <- .soc_param("century", "met", "metabolic_intercept") -
    .soc_param("century", "met", "metabolic_ln_slope") * ln
  a_act_pas <- .soc_param("century", "act_pas", "transfer_fraction")
  list(
    fm = fm,
    fs = 1 - fm,
    k_str = base[["str"]] * exp(-3 * ls) * weeks * climate_modifier,
    k_met = base[["met"]] * weeks * climate_modifier,
    k_act = base[["act"]] * f_txtr * weeks * climate_modifier,
    k_slw = base[["slw"]] * weeks * climate_modifier,
    k_pas = base[["pas"]] * weeks * climate_modifier,
    a_str_act = (1 - ls) *
      (1 - .soc_param("century", "str_act", "transfer_fraction_const")),
    a_str_slw = ls *
      (1 - .soc_param("century", "str_slw", "transfer_fraction_const")),
    a_met_act = 1 - .soc_param("century", "met_act", "transfer_fraction_const"),
    a_act_slw = 1 - es - a_act_pas,
    a_act_pas = a_act_pas,
    a_slw_act = .soc_param("century", "slw_act", "transfer_fraction"),
    a_slw_pas = .soc_param("century", "slw_pas", "transfer_fraction"),
    a_pas_act = 1 -
      .soc_param("century", "pas_act", "transfer_fraction_const")
  )
}

# RothC: exact fixed point of the monthly sub-step map (Coleman & Jenkinson
# 1996). DPM/RPM sit at their input over the per-sub-step decayed fraction; the
# BIO+HUM feedback closes because the total decomposition flux is
# (c_dpm + c_rpm) / (1 - frac_bio - frac_hum). The inert IOM pool is the Falloon
# (1998) function of the seed stock, matching calculate_soc_rothc(). Uses the
# same sub-step count as the run -- literally the same accessor,
# `.rothc_substeps()`, because two copies of the expression is exactly how
# they came to disagree: `x / 12` and `x * (1 / 12)` round differently, and
# over 49,991 modifiers in [0.001, 5] they split at cm = 4.8000000000000007
# for a 0.14% difference in the equilibrium.
.cb_rothc_equilibrium <- function(
  input,
  climate_modifier,
  clay_pct,
  humified_fraction
) {
  rates <- .soc_rates("rothc", c("dpm", "rpm", "bio", "hum"))
  n_sub <- .rothc_substeps(rates, climate_modifier, 1 / 12)
  step_dt <- 1 / (12 * n_sub)
  ratio <- .soc_param("rothc", "input", "dpm_rpm_ratio")
  frac_dpm <- ratio / (1 + ratio)
  x <- 1.67 * (1.85 + 1.60 * exp(-0.0786 * clay_pct))
  frac_bio <- 0.46 / (x + 1)
  frac_hum <- 0.54 / (x + 1)
  c_dpm <- input / 12 * frac_dpm / n_sub
  c_rpm <- input / 12 * (1 - frac_dpm) / n_sub
  survive <- \(k) 1 - exp(-k * climate_modifier * step_dt)
  dpm <- c_dpm / survive(rates[["dpm"]])
  rpm <- c_rpm / survive(rates[["rpm"]])
  total_dec <- (c_dpm + c_rpm) / (1 - frac_bio - frac_hum)
  bio <- total_dec * frac_bio / survive(rates[["bio"]])
  hum <- total_dec * frac_hum / survive(rates[["hum"]])
  k_fresh <- .cb_param("hsoc", "fresh")
  k_humus <- .cb_param("hsoc", "humus")
  seed <- pmax(
    input *
      (1 - humified_fraction) /
      (k_fresh * climate_modifier) +
      input * humified_fraction / (k_humus * climate_modifier),
    1
  )
  dpm + rpm + bio + hum + 0.049 * seed^1.139
}

# Closed-form HSOC steady state, vectorised over its inputs. The active fresh
# and humus pools sit at their fixed points input_pool / (k_pool *
# climate_modifier); the inert (IOM) pool is the Falloon (1998) function
# 0.049 * active^1.139 of the seed active stock (floored at 1, matching
# `.cb_seed_stock()`). This is the exact stock the 5000-year HSOC spin-up
# relaxes to (the pool series starts at the fixed point and is flat), so it
# replaces a 5000-step trajectory per input combination with an O(1)
# expression.
.cb_hsoc_equilibrium <- function(
  input,
  humified_fraction,
  climate_modifier,
  clay_pct
) {
  k_fresh <- .cb_param("hsoc", "fresh")
  k_humus <- .cb_param("hsoc", "humus")
  hf <- .cb_hsoc_hf(humified_fraction, clay_pct)
  active <- input *
    (1 - hf) /
    (k_fresh * climate_modifier) +
    input * hf / (k_humus * climate_modifier)
  active + 0.049 * pmax(active, 1)^1.139
}

# Aguilera et al. (2018) Eq. 5-6: the tabulated humification coefficient of an
# input type is the value for a reference soil, and the effective coefficient is
# H = h * d, with d falling on coarse soils that stabilise less carbon. The
# denominator is RothC's own clay function -- the same `x` already used to split
# decomposition between BIO and HUM in `.cb_rothc_equilibrium()` and
# `.rothc_splits()` -- and the 3.51 numerator normalises d to 1 at RothC's
# Rothamsted reference of 23.4% clay. It runs 0.72 at 5% clay to 1.13 at 60%.
# Omitting it was invisible in a Spain-only validation, where the national mean
# clay of about 21.8% puts d at roughly 0.97, and it matters most on the coarse
# soils much natural land sits on.
.cb_texture_modifier <- function(clay_pct) {
  3.51 / (1.67 * (1.85 + 1.60 * exp(-0.0786 * clay_pct)))
}

# The effective HSOC humification fraction: the tabulated coefficient scaled by
# the texture modifier and capped, since a fraction of the carbon input cannot
# exceed all of it. Used by both the closed form and the spin-up it replaces,
# which must agree.
.cb_hsoc_hf <- function(humified_fraction, clay_pct) {
  pmin(humified_fraction * .cb_texture_modifier(clay_pct), 1)
}

# Attach the equilibrium density to every class-year row.
#
# A closed form is already vectorised over the whole column, so there is
# nothing to dedupe FOR: it is evaluated in place and no join happens at all.
# Measured at global grain (58,800 cells x 3 classes x 20 years = 3.5e6 rows,
# HSOC): 9.86 s through the dedupe-and-join path against 1.84 s in place, for
# identical values to 1e-12. The dedup was not merely a poor trade, it was
# free of any benefit -- `distinct()` returned 100.0% of the rows at every
# scale tried, including with a deliberately discretised humification
# fraction and climate rounded to two decimals, because `c_input_mgc_ha_yr`
# and `clay_pct` are near-unique per cell on their own (#394).
#
# The join it removes was also an exact float-equality match on
# `climate_modifier` and `clay_pct`, which is a fragile thing to key on and
# is now simply absent.
#
# The spin-up fall-through keeps the dedup, and genuinely wants it: there the
# cost is one 5000-year trajectory per distinct combination, not one
# vectorised expression.
.cb_attach_equilibrium <- function(classes, model) {
  closed <- .cb_closed_form_equilibrium(model, classes)
  if (!is.null(closed)) {
    return(.cb_check_equilibrium(
      dplyr::mutate(classes, soc_eq_mgc_ha = closed)
    ))
  }
  classes |>
    dplyr::left_join(
      .cb_equilibrium(model, classes),
      by = c(
        "land_use",
        "c_input_mgc_ha_yr",
        "humified_fraction",
        "climate_modifier",
        "clay_pct"
      )
    )
}

.cb_steady_state <- function(model, input, humified_fraction, cm, clay) {
  # HSOC's humification is texture-dependent (Aguilera Eq. 5-6); the other
  # models carry their own texture terms, so only HSOC's fraction is scaled.
  # Scaled HERE for the analytic seed only. `calculate_soc_hsoc()` applies
  # the same modifier itself from the `clay_pct` passed below, so the
  # fraction handed to it has to be the unscaled tabulated one or the
  # texture term lands twice.
  hf <- if (model == "hsoc") {
    .cb_hsoc_hf(humified_fraction, clay)
  } else {
    humified_fraction
  }
  seed <- .cb_seed_stock(model, input, hf, cm)
  args <- list(
    initial_soc_mgc_ha = seed,
    c_input_mgc_ha_yr = input,
    years = 5000L,
    clay_pct = clay,
    climate_modifier = cm
  )
  if (model == "hsoc") {
    args$humification_fraction <- humified_fraction
  }
  if (model == "amg") {
    # fixed_iom would split the arbitrary analytic `seed` by a fixed stable
    # fraction; there is no real measured total here to split, so the
    # from-scratch equilibrium must derive both pools from ca_ss/f_iom
    # instead (see .amg_init()'s steady_state branch).
    args$init_mode <- "steady_state"
  }
  traj <- calculate_soc_dynamics(model = model, data = args)
  dplyr::last(.cb_total_stock(traj)$stock_mgc_ha)
}

# Seed the steady-state run with the analytic active equilibrium so the inert
# organic matter pool (a Falloon function of the initial stock) is consistent
# with the converged active stock. For HSOC the active equilibrium is the sum of
# the two pools' input/decay steady states; other models relax from a generic
# seed regardless, so the active HSOC form is a safe starting point.
.cb_seed_stock <- function(model, input, humified_fraction, cm) {
  k_fresh <- .cb_param("hsoc", "fresh")
  k_humus <- .cb_param("hsoc", "humus")
  fresh_eq <- input * (1 - humified_fraction) / (k_fresh * cm)
  humus_eq <- input * humified_fraction / (k_humus * cm)
  max(fresh_eq + humus_eq, 1)
}

.cb_param <- function(model_name, component_name) {
  whep::soc_turnover_params |>
    dplyr::filter(
      .data$model == model_name,
      .data$component == component_name,
      .data$parameter == "decomposition_rate"
    ) |>
    dplyr::pull(.data$value)
}

# Collapse the selector's long per-pool output to a single total stock per year.
# Every model reports the same `soc_total` on each of a year's pool rows (#350),
# so no per-model branch is needed here.
.cb_total_stock <- function(traj) {
  traj |>
    dplyr::distinct(.data$year, .data$soc_total) |>
    dplyr::rename(stock_mgc_ha = "soc_total")
}

# Initialise each cell from the earliest year: every class starts at the
# cell-weighted-mean equilibrium density (SOC_init = sum_lu(frac_lu * soc_eq_lu)).
# The equilibrium (spin-up) modifier optionally comes from a distinct
# pre-industrial climatological normal (`d$equilibrium_climate`, RESOLVED F3),
# so the initial stock reflects the equilibrium climate while the forward march
# uses the year-specific modifier already carried in `soc_eq_mgc_ha`.
.cb_initialise <- function(classes, model, d, init) {
  first <- dplyr::filter(
    classes,
    .data$year == min(.data$year),
    .by = c("lon", "lat", "area_code")
  )
  first <- .cb_apply_equilibrium_climate(first, model, d)
  .cb_init_density(first, init)
}

# Recompute the first-year per-class equilibrium densities under the
# equilibrium-climate normal when one is supplied, overwriting `soc_eq_mgc_ha`
# for the initialisation only. With no normal the first-year forward equilibrium
# is kept (the prior behaviour).
.cb_apply_equilibrium_climate <- function(first, model, d) {
  eq_climate <- d$equilibrium_climate
  if (is.null(eq_climate)) {
    return(first)
  }
  eq_mod <- .cb_equilibrium_modifier_table(
    eq_climate,
    d$clay,
    model,
    first$land_use,
    d$natural_cover,
    d$cropland_cover,
    class_water = .cb_class_water_spec(d$class_water, first)
  )
  first |>
    dplyr::left_join(eq_mod, by = c("lon", "lat", "area_code", "land_use")) |>
    dplyr::mutate(
      climate_modifier = dplyr::coalesce(
        .data$climate_modifier_eq,
        .data$climate_modifier
      )
    ) |>
    dplyr::select(-"climate_modifier_eq", -"soc_eq_mgc_ha") |>
    .cb_attach_equilibrium(model)
}

# Per-cell, per-land-use equilibrium-climate modifier from the pre-industrial
# normal. The normal carries the same monthly raw drivers as the forward climate
# but only one representative period per cell; each land-use class's monthly
# soil-cover is attached (as in the forward path) before reducing to one
# `climate_modifier_eq` per (lon, lat, area_code, land_use).
.cb_equilibrium_modifier_table <- function(
  eq_climate,
  clay,
  model,
  land_use_classes,
  natural_cover = NULL,
  cropland_cover = NULL,
  class_water = NULL
) {
  cell_keys <- c("lon", "lat", "area_code", "land_use")
  eq_climate |>
    .cb_join_clay(clay) |>
    .cb_arrange_by_month() |>
    .cb_attach_soil_cover(
      land_use_classes,
      natural_cover,
      cropland_cover
    ) |>
    .cb_attach_class_water(class_water) |>
    dplyr::summarise(
      climate_modifier_eq = .cb_year_climate_modifier(
        model,
        dplyr::pick(dplyr::everything()),
        dplyr::first(.data$clay_pct)
      ),
      .by = dplyr::all_of(cell_keys)
    )
}

# Initial SOC density per cell and class.
#
# `"own_equilibrium"` (default) starts each class at the stock its own carbon
# input and climate support, so a class opens on its own target and the march
# reports the trend its drivers imply.
#
# `"cell_average"` is the Spain_Hist behaviour: every class in a cell opens at
# the fraction-weighted mean `sum(frac * soc_eq)` of the classes sharing it. It
# is a proxy for land converted from something richer -- cropland broken out of
# forest does inherit a stock above its own equilibrium -- and it is defensible
# at the provincial grain it was written for. Carried to a 0.5-degree cell it
# also means the lowest-input class starts wherever its neighbours' equilibria
# put it and drains toward its own for decades: with cropland's time constant
# `soc_eq / c_input` near 11 years, that transient is read out as soil nitrogen
# mineralization, and it accounted for about a third of the spurious flux
# reaching the nitrogen balance (312 against 211 Tg N; whep#792, whep#799).
# Kept selectable because the inheritance it models is real, not because it is
# the safer default.
.cb_init_density <- function(classes, init) {
  classes |>
    dplyr::mutate(
      stock_mgc_ha = if (init == "cell_average") {
        sum(.data$frac * .data$soc_eq_mgc_ha)
      } else {
        .data$soc_eq_mgc_ha
      },
      .by = c("lon", "lat", "area_code")
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "land_use",
      "stock_mgc_ha"
    )
}

# -- Forward march ------------------------------------------------------------

# March every cell forward over its years, applying the model annual update then
# the land-use-change carbon transfer. Each cell is processed independently.
# Both tables are partitioned once by the cell key (an O(n) split) and the
# groups zipped, rather than re-filtering the whole table per cell (which was
# O(cells^2) and dominated the global run time). A cell absent from `init` gets
# an empty init slice, matching the previous per-cell zero-row filter.
.cb_march <- function(classes, init) {
  dt <- data.table::as.data.table(classes)
  dt[, `:=`(
    cell_key = paste(lon, lat, area_code, sep = "\r"),
    eff_rate = data.table::fifelse(
      soc_eq_mgc_ha > 0,
      c_input_mgc_ha_yr / soc_eq_mgc_ha,
      0
    )
  )]
  years <- sort(unique(dt$year))
  init_dt <- data.table::as.data.table(init)
  init_dt[, cell_key := paste(lon, lat, area_code, sep = "\r")]
  # state: transferred stock per (cell_key, land_use), carried across years.
  # lon/lat/area_code ride along so a class whose row vanishes in a later year
  # can be re-added at zero area (see .cb_keep_vanished()).
  state <- init_dt[, .(
    cell_key,
    land_use,
    lon,
    lat,
    area_code,
    prev_stock = stock_mgc_ha
  )]
  prev <- NULL
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    out[[i]] <- .cb_march_year(dt[year == years[i]], state, prev)
    state <- out[[i]]$state
    prev <- out[[i]]$prev
    out[[i]] <- out[[i]]$rows
  }
  # Match the previous per-cell order: cells by their string key, then year,
  # then land_use (the old split()/arrange order).
  res <- data.table::rbindlist(out)
  data.table::setorder(res, cell_key, year, land_use)
  res[, cell_key := NULL]
  tibble::as_tibble(as.data.frame(res))
}

# Advance one year for ALL cells at once, apply the land-use-change transfer
# vectorised across cells, and build the output rows. Returns the year's rows
# plus the carried state and the prev-year rate/input/area for the next step.
.cb_march_year <- function(cur, state, prev) {
  cur <- cur[, .(
    cell_key,
    lon,
    lat,
    area_code,
    land_use,
    year,
    area_ha,
    c_input_mgc_ha_yr,
    eff_rate
  )]
  cur <- .cb_keep_vanished(cur, state)
  cur <- state[cur, on = c("cell_key", "land_use")]
  cur[is.na(prev_stock), prev_stock := 0]
  if (is.null(prev)) {
    # First year: no prior rates, and old_area == new_area so no LUC transfer.
    cur[, `:=`(stepped = prev_stock, old_area = area_ha)]
  } else {
    cur <- prev[cur, on = c("cell_key", "land_use")]
    cur[is.na(k_prev), k_prev := 0]
    cur[is.na(input_prev), input_prev := 0]
    cur[is.na(old_area), old_area := 0]
    cur[, stepped := prev_stock - prev_stock * k_prev + input_prev]
  }
  cur <- .cb_luc_all(cur)
  cur[, `:=`(
    mineralization = new_stock * eff_rate,
    luc = data.table::fifelse(area_ha > 0, mass_moved / area_ha, 0)
  )]
  list(
    rows = cur[, .(
      lon,
      lat,
      area_code,
      land_use,
      year,
      area_ha,
      stock_mgc_ha = new_stock,
      mineralization_mgc_ha = mineralization,
      c_input_mgc_ha = c_input_mgc_ha_yr,
      luc_transfer_mgc_ha = luc,
      luc_transfer_mgc = mass_moved,
      rate_mgc_ha = c_input_mgc_ha_yr - mineralization,
      cell_key
    )],
    state = cur[, .(
      cell_key,
      land_use,
      lon,
      lat,
      area_code,
      prev_stock = new_stock
    )],
    prev = cur[, .(
      cell_key,
      land_use,
      k_prev = eff_rate,
      input_prev = c_input_mgc_ha_yr,
      old_area = area_ha
    )]
  )
}

# A class whose ROW disappears in a later year must not take its carbon with
# it. `state[cur, ...]` is a right join onto the current year, so a (cell,
# class) present last year but absent this year was silently dropped, and
# because `state` and `prev` are rebuilt from `cur`, its stock vanished from
# the ledger -- in both the vectorised march and the sequential twin. Found
# while preparing the per-crop-group balance, where classes (a woody species
# in a cell) legitimately come and go. The row is re-added at zero area, so
# the land-use-change transfer treats it as an ordinary shrink to zero and
# releases stock x lost hectares into the cell pool. A cell that vanishes
# entirely still loses its carbon; that is a support change, not a class
# change, and is out of scope here.
.cb_keep_vanished <- function(cur, state) {
  if (is.null(state) || nrow(state) == 0L) {
    return(cur)
  }
  gone <- state[!cur, on = c("cell_key", "land_use")]
  gone <- gone[cell_key %in% cur$cell_key]
  if (nrow(gone) == 0L) {
    return(cur)
  }
  yr <- cur$year[[1]]
  filler <- gone[, .(
    cell_key,
    lon,
    lat,
    area_code,
    land_use,
    year = yr,
    area_ha = 0,
    c_input_mgc_ha_yr = 0,
    eff_rate = 0
  )]
  data.table::rbindlist(list(cur, filler), use.names = TRUE)
}

# Vectorised land-use-change carbon transfer across all cells. Within a cell,
# shrinking classes (with positive stock) release stock * lost_area into a pool;
# the pool density carbon/area is constant while growing classes absorb from it,
# so each grower (in area-change-ascending order) draws
# min(gained_area, pool_area_remaining) at that fixed density. The remaining
# pool area is pool_area minus the cumulative gained area of earlier growers,
# which vectorises as an exclusive cumulative sum. Ordering by (cell, area
# change) before the per-cell sum/cumsum makes the arithmetic follow the same
# ascending order as the previous sequential loop.
.cb_luc_all <- function(d) {
  d[, area_change := area_ha - old_area]
  data.table::setorder(d, cell_key, area_change)
  d[, `:=`(
    is_shrink = area_ha < old_area & stepped > 0,
    is_grow = area_ha > old_area
  )]
  d[, `:=`(
    shrink_area = data.table::fifelse(is_shrink, old_area - area_ha, 0),
    shrink_carbon = data.table::fifelse(
      is_shrink,
      stepped * (old_area - area_ha),
      0
    ),
    gained = data.table::fifelse(is_grow, area_ha - old_area, 0)
  )]
  d[,
    `:=`(
      pool_area = sum(shrink_area),
      pool_carbon = sum(shrink_carbon)
    ),
    by = "cell_key"
  ]
  d[, dens := data.table::fifelse(pool_area > 0, pool_carbon / pool_area, 0)]
  d[, cum_prev := cumsum(gained) - gained, by = "cell_key"]
  d[, remaining := pool_area - cum_prev]
  # A grower only takes carbon when the pool still has area at its turn; if not,
  # its stock is unchanged (the old sequential code's `else` branch).
  d[, active_grow := is_grow & remaining > 0]
  d[,
    drawn_area := data.table::fifelse(
      active_grow,
      pmin(gained, remaining),
      0
    )
  ]
  d[, drawn_c := dens * drawn_area]
  # Every growing class re-averages the carbon it already holds over its new
  # area, plus whatever the pool still had at its turn -- which may be nothing.
  # Keeping the per-hectare density instead (the old sequential code's `else`
  # branch, gated on `active_grow`) spreads the same density over more hectares
  # and manufactures carbon: a class growing 10 -> 50 ha at 100 Mg C/ha against
  # an empty pool turned 1,000 Mg C into 5,000. It was invisible because
  # `mass_moved` still summed to zero across the cell, so the transfer looked
  # balanced while the stock it produced was not. `is_grow` implies
  # `area_ha > old_area >= 0`, so the divisor is positive.
  d[,
    new_stock := data.table::fifelse(
      is_grow,
      (stepped * old_area + drawn_c) / area_ha,
      stepped
    )
  ]
  d[,
    mass_moved := data.table::fifelse(
      active_grow,
      drawn_c,
      data.table::fifelse(is_shrink, -(stepped * (old_area - area_ha)), 0)
    )
  ]
  d
}

# March one cell forward year by year (Spain_Hist Calc_SOC_evolution
# :370-410). State is a named density vector indexed by land-use class. The
# first year keeps the equilibrium-weighted initial stock unchanged; each later
# year advances the previous year's stock with the PREVIOUS year's rate and
# input (soc - soc*K[i-1] + Input[i-1]) then redistributes released carbon via
# the land-use-change buffer. Reported diagnostics for a year use that year's
# own post-transfer stock, rate and input.
.cb_march_cell <- function(cell, init) {
  years <- sort(unique(cell$year))
  # Split once by year (base) instead of a dplyr::filter per year -- the march
  # calls this once per cell, so the per-year data-mask overhead adds up.
  by_year <- split(cell, cell$year)
  state <- stats::setNames(init$stock_mgc_ha, init$land_use)
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    cur <- by_year[[as.character(years[i])]]
    prev <- if (i == 1L) {
      NULL
    } else {
      by_year[[as.character(years[i - 1L])]]
    }
    step <- .cb_year_step(cur, prev, state)
    out[[i]] <- step$rows
    state <- step$state
  }
  dplyr::bind_rows(out)
}

# One year of evolution for a cell. The first year (`prev` NULL) leaves the
# initial stock in place with no transfer; a later year advances each class's
# previous-year stock with the previous year's rate and input, then applies the
# carbon-conserving land-use-change transfer driven by the previous-to-current
# area change. A class absent from the previous year starts from zero stock and
# zero area (a newly appearing class carries no carbon; Spain_Hist NaN guard,
# SOC_Fun.R:388-390).
.cb_year_step <- function(cur, prev, state) {
  cur <- .cb_year_keep_vanished(cur, state)
  # Base radix order matches dplyr::arrange(land_use)'s C-locale ordering
  # without the per-call data-mask overhead (this runs once per cell-year).
  cur <- cur[order(cur$land_use, method = "radix"), , drop = FALSE]
  stepped <- .cb_advance_stock(cur, prev, state)
  transferred <- .cb_luc_transfer(
    tibble::tibble(
      land_use = cur$land_use,
      stock_mgc_ha = stepped,
      old_area_ha = .cb_prev_areas(cur, prev),
      new_area_ha = cur$area_ha
    )
  )
  rows <- .cb_year_rows(cur, transferred)
  list(
    rows = rows,
    state = stats::setNames(transferred$stock_mgc_ha, transferred$land_use)
  )
}

# The sequential twin of .cb_keep_vanished(): a class carried in `state` but
# absent from this year's rows re-enters at zero area, so its stock is
# released through the transfer instead of silently surviving in `state`
# (or, in the vectorised march, vanishing outright).
.cb_year_keep_vanished <- function(cur, state) {
  gone <- setdiff(names(state), cur$land_use)
  if (length(gone) == 0L || nrow(cur) == 0L) {
    return(cur)
  }
  filler <- cur[rep(1L, length(gone)), , drop = FALSE]
  filler$land_use <- gone
  zero <- intersect(
    c("area_ha", "c_input_mgc_ha_yr", "eff_rate", "frac", "soc_eq_mgc_ha"),
    names(filler)
  )
  filler[zero] <- 0
  dplyr::bind_rows(cur, filler)
}

# Per-class stock entering the current year's transfer. The first year passes
# the initial stock through unchanged; a later year applies the previous year's
# decay and input to the previous year's stock. A class with no previous-year
# stock (absent last year) enters at zero.
.cb_advance_stock <- function(cur, prev, state) {
  prev_stock <- .cb_lookup(state, cur$land_use)
  if (is.null(prev)) {
    return(prev_stock)
  }
  k_prev <- .cb_lookup(.cb_rate_vec(prev), cur$land_use)
  input_prev <- .cb_lookup(.cb_input_vec(prev), cur$land_use)
  prev_stock - prev_stock * k_prev + input_prev
}

# Named lookup that maps classes absent from the source vector to 0 rather than
# propagating NA (Spain_Hist treats a class absent last year as zero stock).
.cb_lookup <- function(vec, land_use) {
  looked <- vec[land_use]
  dplyr::coalesce(unname(looked), 0)
}

# Previous-year effective decay rate per class as a named vector (0 for a class
# absent last year, so its stock does not decay before it exists).
.cb_rate_vec <- function(prev) {
  stats::setNames(.cb_effective_rate(prev), prev$land_use)
}

# Previous-year carbon input per class as a named vector.
.cb_input_vec <- function(prev) {
  stats::setNames(prev$c_input_mgc_ha_yr, prev$land_use)
}

# Previous-year area per class aligned to the current year's classes; a class
# absent last year has zero previous area, so it enters the transfer as a pure
# area gain drawing from the released-carbon buffer.
.cb_prev_areas <- function(cur, prev) {
  if (is.null(prev)) {
    return(cur$area_ha)
  }
  .cb_lookup(stats::setNames(prev$area_ha, prev$land_use), cur$land_use)
}

# Effective annual decay rate making the stock relax to the model equilibrium
# (K = input / soc_eq), the Spain_Hist Miner = Stock * K form (SOC_Fun.R:280).
.cb_effective_rate <- function(yr) {
  eq <- yr$soc_eq_mgc_ha
  dplyr::if_else(eq > 0, yr$c_input_mgc_ha_yr / eq, 0)
}

# Assemble the per-class output rows for one cell-year. `transferred` is keyed
# by land_use (it was reordered by area change inside the transfer), so it is
# matched back to the year's class order. Mineralization, rate and input are the
# year's own diagnostics on its post-transfer stock (Spain_Hist
# Calc_SOC_categories, SOC_Fun.R:275-283). luc_transfer_mgc_ha is the buffer
# mass exchanged per current hectare (sums to zero across the cell).
.cb_year_rows <- function(cur, transferred) {
  idx <- match(cur$land_use, transferred$land_use)
  stock <- transferred$stock_mgc_ha[idx]
  mineralization <- stock * .cb_effective_rate(cur)
  tibble::tibble(
    lon = cur$lon,
    lat = cur$lat,
    area_code = cur$area_code,
    land_use = cur$land_use,
    year = cur$year,
    area_ha = cur$area_ha,
    stock_mgc_ha = stock,
    mineralization_mgc_ha = mineralization,
    c_input_mgc_ha = cur$c_input_mgc_ha_yr,
    luc_transfer_mgc_ha = dplyr::if_else(
      cur$area_ha > 0,
      transferred$mass_moved[idx] / cur$area_ha,
      0
    ),
    luc_transfer_mgc = transferred$mass_moved[idx],
    rate_mgc_ha = cur$c_input_mgc_ha_yr - mineralization
  )
}

# Land-use-change carbon transfer (Spain_Hist Calc_SOC_evolution :377-408).
# Classes are processed by area change ascending (losses first): a shrinking
# class keeps its per-hectare density and releases the carbon on its abandoned
# hectares (density x lost area) into a shared cell buffer; a growing class
# draws carbon for its gained hectares from the buffer at the buffer's
# area-weighted density, re-averaging its density over its new total area. Total
# cell carbon (sum of density x area) is conserved.
.cb_luc_transfer <- function(before) {
  # Ordered by area_change ascending (shrinking classes first), matching the
  # previous data.table::setorder; base radix order is stable like setorder, so
  # ties keep input order. Operating on plain vectors avoids an
  # as.data.table()/as_tibble() round-trip on every cell-year (the march calls
  # this ~n_cells * n_years times).
  ord <- order(before$new_area_ha - before$old_area_ha, method = "radix")
  land_use <- before$land_use[ord]
  old_area <- before$old_area_ha[ord]
  new_area <- before$new_area_ha[ord]
  stocks <- before$stock_mgc_ha[ord]
  moved <- numeric(length(stocks))
  pool <- list(carbon = 0, area = 0)
  for (i in seq_along(stocks)) {
    res <- .cb_transfer_one(stocks[i], old_area[i], new_area[i], pool)
    stocks[i] <- res$stock
    moved[i] <- res$mass_moved
    pool <- res$pool
  }
  tibble::tibble(
    land_use = land_use,
    stock_mgc_ha = stocks,
    new_area_ha = new_area,
    mass_moved = moved
  )
}

# One class's transfer step: a loss deposits the carbon on its lost hectares
# into the buffer (density unchanged on remaining land); a gain draws carbon
# from the buffer at its area-weighted density and re-averages density over the
# new total area. `mass_moved` is the signed carbon mass exchanged with the
# buffer (negative leaving, positive arriving), so it sums to zero across the
# cell.
.cb_transfer_one <- function(stock, old_area, new_area, pool) {
  if (new_area < old_area && stock > 0) {
    lost_area <- old_area - new_area
    lost_c <- stock * lost_area
    pool$carbon <- pool$carbon + lost_c
    pool$area <- pool$area + lost_area
    list(stock = stock, mass_moved = -lost_c, pool = pool)
  } else if (new_area > old_area) {
    .cb_apply_gain(stock, old_area, new_area, pool)
  } else {
    list(stock = stock, mass_moved = 0, pool = pool)
  }
}

# A growing class draws carbon for its gained hectares from the buffer at the
# buffer density, then re-averages: new density = (existing carbon + drawn
# carbon) / new area, conserving total cell carbon.
.cb_apply_gain <- function(stock, old_area, new_area, pool) {
  gained_area <- new_area - old_area
  drawn_area <- min(gained_area, pool$area)
  # An empty buffer has no density. Guarding here rather than at the call site
  # keeps a grower that can draw nothing on the same path as one that can: it
  # re-averages what it already holds over its new area (drawn_c = 0) instead of
  # carrying its old density onto more hectares and manufacturing carbon.
  dens <- if (pool$area > 0) pool$carbon / pool$area else 0
  drawn_c <- dens * drawn_area
  list(
    stock = (stock * old_area + drawn_c) / new_area,
    mass_moved = drawn_c,
    pool = list(carbon = pool$carbon - drawn_c, area = pool$area - drawn_area)
  )
}

# -- Soil-organic-nitrogen change ---------------------------------------------

# Net carbon loss (rate < 0) mineralizes nitrogen at the cropland-class
# mineralization C:N (son_change > 0, a positive N-release flux into the mineral
# pool that Module C consumes as an N input); net gain immobilizes nitrogen at
# the sequestration C:N (son_change < 0). This is the N-release-flux convention
# (negated relative to the Spain_Hist SOC_Fun.R:278-283 delta-SON-stock sign) so
# downstream consumers add it directly. The asymmetric ratios come from
# whep::soil_cn_ratios (Conventional rows).
.cb_derive_son <- function(marched) {
  cn <- .cb_cn_lookup()
  marched |>
    dplyr::mutate(cropland_class = .cb_cropland_class(.data$land_use)) |>
    dplyr::left_join(cn, by = "cropland_class") |>
    dplyr::mutate(
      cn_used = dplyr::if_else(
        .data$rate_mgc_ha < 0,
        .data$cn_mineralization,
        .data$cn_sequestration
      ),
      son_change_kgn_ha = -.data$rate_mgc_ha * 1000 / .data$cn_used
    ) |>
    dplyr::select(
      -"cropland_class",
      -"cn_mineralization",
      -"cn_sequestration",
      -"cn_used"
    )
}

# Classify any land-use label as Cropland vs NonCropland for the C:N lookup.
# Case-insensitive so the LUH2 reader's lowercase classes (cropland, grassland,
# natural, urban) resolve: only "cropland" maps to the Cropland C:N pair, every
# other class maps to NonCropland.
.cb_cropland_class <- function(land_use) {
  dplyr::if_else(
    .soc_is_cropland(land_use),
    "Cropland",
    "NonCropland"
  )
}

# The asymmetric C:N pair per cropland class (Conventional management). Keyed by
# cropland_class so .cb_derive_son joins on the classified label, never on the
# raw land-use string.
#
# CONVENTIONAL IS HARDCODED ON PURPOSE, not pending a parameter (issue 809).
# `whep::soil_cn_ratios` also ships Organic rows, and nothing here can reach
# them. That is the intended state: this balance has no management dimension --
# it is global and gridded -- so an argument selecting management could only be
# set world-wide, and running the whole world as organic is not a meaningful
# request. The Organic rows are reference values for downstream consumers; see
# the `management` entry of ?soil_cn_ratios. `test_datasets_balances.R` pins
# both halves, so exposing management later has to be a deliberate edit.
.cb_cn_lookup <- function() {
  whep::soil_cn_ratios |>
    dplyr::filter(.data$management == "Conventional") |>
    dplyr::select(
      "cropland_class",
      "cn_mineralization",
      "cn_sequestration"
    )
}

# -- Finalisation -------------------------------------------------------------

# Grid output keeps the per-cell per-class rows; polity output aggregates to
# (area_code, year), area-weighting the per-hectare densities so total carbon
# mass (stock x area) is conserved.
.cb_finalise <- function(marched, resolution) {
  if (resolution == "grid") {
    return(tibble::as_tibble(marched))
  }
  marched |>
    dplyr::summarise(
      stock_mgc_ha = .cb_wmean(.data$stock_mgc_ha, .data$area_ha),
      mineralization_mgc_ha = .cb_wmean(
        .data$mineralization_mgc_ha,
        .data$area_ha
      ),
      c_input_mgc_ha = .cb_wmean(.data$c_input_mgc_ha, .data$area_ha),
      luc_transfer_mgc_ha = .cb_wmean(.data$luc_transfer_mgc_ha, .data$area_ha),
      dplyr::across(dplyr::any_of("luc_transfer_mgc"), sum),
      rate_mgc_ha = .cb_wmean(.data$rate_mgc_ha, .data$area_ha),
      son_change_kgn_ha = .cb_wmean(.data$son_change_kgn_ha, .data$area_ha),
      dplyr::across(
        dplyr::any_of(c("method_soc", "method_class_water")),
        \(x) x[1]
      ),
      area_ha = sum(.data$area_ha),
      .by = c("area_code", "year")
    ) |>
    tibble::as_tibble()
}

.cb_wmean <- function(value, weight) {
  if (sum(weight) == 0) {
    return(0)
  }
  sum(value * weight) / sum(weight)
}

# -- Default input readers ----------------------------------------------------

# The per-(cell, land-use class, year) carbon-input layer, assembled from the
# cropland (build_soil_carbon_inputs), grassland and natural
# (build_grass_natural_carbon_inputs) builders by build_carbon_inputs(). Grid
# grain is required: .cb_class_table() joins c_inputs onto the land-use areas
# per cell.
.cb_read_c_inputs <- function(
  years = NULL,
  crop_groups = list(),
  density_basis = "static"
) {
  build_carbon_inputs(
    resolution = "grid",
    years = years,
    crop_groups = crop_groups,
    density_basis = density_basis
  )
}

# Yearly per-cell per-class land-use areas from LUH2 v2h (read_luh2_landuse()
# emits lowercase cropland/grassland/natural/urban classes, matching the
# carbon-input builders).
.cb_read_land_use <- function(years = NULL) {
  read_luh2_landuse(resolution = "grid", years = years)
}

# The per cell-year monthly climate drivers get_soc_climate_drivers() produces
# (temperature, water surplus, soil moisture and clay_pct), from which
# .cb_climate_modifier_table() derives the selected model's native modifier.
# get_soc_climate_drivers() requires the per-cell clay and cell-polity
# crosswalk, supplied here from HWSD and the spatialization country grid.
.cb_read_climate <- function(years = NULL) {
  cell_polity <- .cb_read_cell_polity()
  # .socd_build() rather than get_soc_climate_drivers(): the reporting polity
  # columns the exported reader attaches cost ~27 GB on the 8.6e7-row table a
  # full span produces, and nothing here reads them -- the climate modifier keys
  # on (lon, lat, area_code, year, month), and this function's own output gets
  # its reporting columns added at the end regardless (#624).
  .socd_build(
    run_dir = NULL,
    years = years,
    polity_validity = "keep",
    data = list(
      clay = .cb_hwsd_clay(cell_polity),
      cell_polity = cell_polity
    )
  )
}

# Standalone per-cell clay reader (only reached when the resolved climate table
# carries no clay_pct); the HWSD clay cropped to the spatialization country grid.
.cb_read_clay <- function() {
  .cb_hwsd_clay(.cb_read_cell_polity())
}

# The cell -> polity footprint (lon, lat, area_code) the climate drivers and the
# HWSD clay are read on, from the same polycell support every other carbon
# reader uses. `get_soc_climate_drivers()` consumes this only to LABEL each cell
# with an area_code and to restrict the grid to it -- it never multiplies by an
# area -- so what this feed decides is the carbon path's FOOTPRINT, and handing
# it a different table from the one the land-use areas are measured on is what
# made one exported function carry two crosswalks (EA4, AM-1).
.cb_read_cell_polity <- function() {
  .carbon_cell_support() |>
    dplyr::distinct(.data$lon, .data$lat, .data$area_code)
}

# -- The carbon path's spatial support ----------------------------------------
#
# One support for the whole carbon path (S-A5): `read_luh2_landuse()`,
# `build_carbon_balance()`, `build_grass_natural_carbon_inputs()`,
# `build_soil_carbon_inputs()` and `build_carbon_inputs()` all resolve their
# cell-to-polity table through here, so the path cannot be half-migrated with
# one reader left on the centroid grid -- the failure `.cb_join_modifier()` and
# `.cb_drop_uncovered_climate()` would report as an ordinary climate gap.
#
# `cell_area_frac` is the polycell's share of the cell's LAND, never
# `land_area_ha / cell_area_ha`: everything this fraction splits (LUH2 class
# areas, crop-pattern hectares) is already land-only, so dividing by the whole
# cell would subtract the water a second time -- invisibly, because a share's
# denominator still makes the polity totals add up (AM-5 risk 3). The assertion
# that the shares sum to 1 per cell is what makes that structural.

# The reference year the static cell-to-polity assignment is read at. The carbon
# path has always used a present-day snapshot -- LUH2 carries no territorial
# history and `read_luh2_landuse()` documents a pre-modern year as "the
# present-day cell's area read at that year". Migrating the EXTENT (DA-26) does
# not migrate the ATTRIBUTION (DA-28, issue #549), so the snapshot is kept and
# made explicit instead of being implicit in an undated pin.
.carbon_support_year <- function() 2015L

# Resolve the polycell support to the carbon path's grain: one row per cell and
# `area_code`, carrying the cell's own area, the polycell's land, and the
# polycell's share of the cell's land.
.carbon_cell_support <- function(
  support = NULL,
  year = .carbon_support_year()
) {
  (support %||% read_polycell_support()) |>
    .carbon_support_at_year(year) |>
    .carbon_support_to_area_code()
}

# Take the interval covering `year`, using the package's own predicate so the
# exclusive-at-a-succession / inclusive-at-the-open-end rule (DA-24) is stated
# once. A support already expanded to one row per polycell-year is filtered on
# its `year` column by the same helper.
.carbon_support_at_year <- function(support, year) {
  .check_columns(support, c("lon", "lat", "area_code"), "country_grid")
  if (!.country_grid_is_dynamic(support)) {
    return(support)
  }
  out <- .filter_country_grid_year(support, year)
  if (nrow(out) == 0L) {
    cli::cli_abort(c(
      "No polycell support rows are valid at {year}.",
      i = "The carbon path reads the support at a single reference year."
    ))
  }
  out
}

# Collapse `polity_code` to the `area_code` the carbon path reports on.
#
# DA-23: the support keys on `polity_code` and the conversion to a reporting
# code is lossy -- some polities have no reporting area at all, and two can
# share one. Both losses are made visible here rather than absorbed: rows with
# no `area_code` are dropped with their land reported, and polycells that share
# an `area_code` inside one cell are summed with the fold reported. The sum is
# right for an EXTENT and would be wrong for a value, which is why it is done
# here, once, at the boundary that owns it, and refused by
# `.normalize_carbon_support()` everywhere else.
#
# The code the fold runs on is re-resolved from `polity_code` first
# (`.carbon_rekey_area_code()`), because the pinned support's own `area_code`
# column holds matrix BUCKET codes -- 206 for Sudan plus South Sudan, 999 for
# Syria and 42 other territories -- which is not the vocabulary this function's
# consumers are keyed on (whep#907).
.carbon_support_to_area_code <- function(support) {
  .check_columns(
    support,
    c("lon", "lat", "area_code", "cell_area_ha", "land_area_ha"),
    "country_grid"
  )
  support <- .carbon_rekey_area_code(support)
  # The share denominator is the cell's WHOLE measured land, taken before any
  # row is dropped. Taking it after would renormalise the survivors over a
  # smaller cell, handing an unkeyable polity's hectares to its neighbour --
  # the absorption S-A11 exists to forbid, and invisible because the shares
  # would still sum to 1.
  cell_land <- support |>
    dplyr::summarise(
      cell_land_ha = sum(.data$land_area_ha, na.rm = TRUE),
      .by = c("lon", "lat")
    )
  support |>
    .carbon_drop_unkeyed() |>
    .carbon_fold_area_code() |>
    .carbon_attach_land_share(cell_land)
}

# Re-resolve `area_code` from `polity_code` so the support is keyed on the
# REPORTING vocabulary its consumers use, not on the matrix bucket the pinned
# column carries. This is a relabelling and an un-folding: no row is added or
# dropped and no hectare moves, so `sum(land_area_ha)` is invariant.
#
# A support the caller built itself may not carry `polity_code` at all, and a
# polity the crosswalk does not know resolves to NA; in both cases the incoming
# code is left alone rather than being replaced by a guess. `.pcs_area_code()`
# writes the same codes at the producer, so this is a no-op on a support
# regenerated after whep#907 and the correction for every pin published before
# it.
.carbon_rekey_area_code <- function(support) {
  if (!rlang::has_name(support, "polity_code")) {
    return(support)
  }
  support$area_code <- as.integer(support$area_code)
  resolved <- .polity_reporting_area_code(support$polity_code)
  moved <- !is.na(resolved) &
    !is.na(support$area_code) &
    support$area_code != resolved
  support$area_code[!is.na(resolved)] <- resolved[!is.na(resolved)]
  if (any(moved)) {
    .carbon_inform_rekey(support[moved, , drop = FALSE], resolved[moved])
  }
  support
}

.carbon_inform_rekey <- function(moved, codes) {
  land <- round(sum(moved$land_area_ha, na.rm = TRUE) / 1e6, 2)
  n_codes <- dplyr::n_distinct(codes)
  cli::cli_inform(c(
    i = "Re-keyed {nrow(moved)} polycell{?s} ({land} Mha of land) from a matrix
         bucket onto {cli::qty(n_codes)}{n_codes} reporting
         {.field area_code}{?s}.",
    i = "The support is read by {.field area_code}-keyed callers; the bucket
         space folds Sudan with South Sudan and 43 territories into Rest of
         World (whep#907)."
  ))
}

# Rows the reporting vocabulary cannot express: no `area_code`, or no measured
# land. The second case was the DA-13 shim's `"crosswalk_only"` padding, which
# was NA throughout; C9 removed the padding, so `build_polycell_support()` no
# longer produces it, but the guard stays because this function takes a support
# from the caller and an NA land area silently deletes one polity's claim while
# the rest of the cell still looks like a complete partition.
.carbon_drop_unkeyed <- function(support) {
  keep <- !is.na(support$area_code) & !is.na(support$land_area_ha)
  if (!all(keep)) {
    lost <- support[!keep, , drop = FALSE]
    .carbon_warn_unkeyed(lost)
  }
  support[keep, , drop = FALSE]
}

.carbon_warn_unkeyed <- function(lost) {
  land <- sum(lost$land_area_ha, na.rm = TRUE)
  codes <- if (rlang::has_name(lost, "polity_code")) {
    lost |>
      dplyr::summarise(
        land = sum(.data$land_area_ha, na.rm = TRUE),
        .by = "polity_code"
      ) |>
      dplyr::slice_max(.data$land, n = 3L) |>
      dplyr::pull("polity_code")
  } else {
    character()
  }
  cli::cli_warn(c(
    "!" = "{nrow(lost)} polycell{?s} ({round(land / 1e6, 2)} Mha of land) carry
           no {.field area_code} and are outside the carbon ledger.",
    i = "Largest: {.val {codes}}. The reporting vocabulary has no bucket for
         them; they are dropped here rather than folded into one."
  ))
}

# Sum the land of polycells sharing an `area_code` in one cell (DA-23's fold),
# reporting it. `cell_area_ha` is a property of the cell, so it is taken once.
.carbon_fold_area_code <- function(support) {
  folded <- support |>
    dplyr::summarise(
      cell_area_ha = dplyr::first(.data$cell_area_ha),
      land_area_ha = sum(.data$land_area_ha),
      n_polities = dplyr::n(),
      .by = c("lon", "lat", "area_code")
    )
  .carbon_warn_fold(folded, support)
  dplyr::select(folded, -"n_polities")
}

.carbon_warn_fold <- function(folded, support) {
  hit <- dplyr::filter(folded, .data$n_polities > 1L)
  if (nrow(hit) == 0L) {
    return(invisible(NULL))
  }
  codes <- if (rlang::has_name(support, "polity_code")) {
    support |>
      dplyr::semi_join(hit, by = c("lon", "lat", "area_code")) |>
      dplyr::pull("polity_code") |>
      unique() |>
      sort()
  } else {
    character()
  }
  cli::cli_warn(c(
    "!" = "{nrow(hit)} cell-{.field area_code} group{?s}
           ({round(sum(hit$land_area_ha) / 1e6, 2)} Mha of land) fold more than
           one {.field polity_code}.",
    i = "Folded: {.val {codes}}. Their land is summed, which is correct for a
         territorial EXTENT and would not be for a value."
  ))
}

# The polycell's share of the cell's land. A cell whose polycells hold no land
# at all has no share to take, so it is dropped rather than divided by zero.
.carbon_attach_land_share <- function(support, cell_land) {
  out <- dplyr::left_join(support, cell_land, by = c("lon", "lat"))
  dry <- dplyr::filter(out, .data$cell_land_ha <= 0)
  if (nrow(dry) > 0L) {
    cli::cli_warn(
      "{dplyr::n_distinct(dry$lon, dry$lat)} cell{?s} hold no land and carry no
       carbon; dropped from the carbon support."
    )
  }
  out |>
    dplyr::filter(.data$cell_land_ha > 0) |>
    dplyr::mutate(cell_area_frac = .data$land_area_ha / .data$cell_land_ha) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "cell_area_ha",
      "land_area_ha",
      "cell_area_frac"
    )
}

# The single normaliser every carbon consumer runs its support through, whether
# it came from `.carbon_cell_support()` or straight from the caller. A support
# that is not already one row per cell and `area_code` is REFUSED here (the
# pattern C3a used at `.nd_check_area_key()`): folding it silently would merge
# two territories' land under one label, and the fold belongs at the boundary
# that can report it.
.normalize_carbon_support <- function(support, arg = "country_grid") {
  .check_columns(support, c("lon", "lat", "area_code"), arg)
  .carbon_check_support_key(support, arg)
  .normalize_country_grid(support)
}

.carbon_check_support_key <- function(support, arg = "country_grid") {
  dup <- support |>
    dplyr::count(.data$lon, .data$lat, .data$area_code, name = "n_rows") |>
    dplyr::filter(.data$n_rows > 1L | is.na(.data$area_code))
  if (nrow(dup) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{.arg {arg}} must hold one row per cell and {.field area_code}.",
    x = "{nrow(dup)} cell-{.field area_code} group{?s} {?is/are} duplicated
         or {.val NA}.",
    i = "Convert {.field polity_code} to {.field area_code} before calling;
         the carbon path reports on {.field area_code} and will not fold two
         polities into one silently."
  ))
}

# Per-cell topsoil clay percent from HWSD: the map-unit share-weighted mean of
# the HWSD topsoil clay fraction (t_clay), aggregated to the 0.5-degree grid
# (cropped to `cell_polity`) via the shared HWSD aggregation helper. Reuses the
# HWSD attribute/raster path read_soil_hydraulic() uses so the clay driver is
# consistent with the hydraulic drivers.
#
# EXEMPT from the `polity_validity` year-check (whep#675), for the reason given
# in R/soil_ph.R: `cell_polity` is a spatial extent here, the output has no
# `year` and no `area_code`, so no row can name a polity that did not exist.
.cb_hwsd_clay <- function(cell_polity) {
  rlang::check_installed("terra")
  hwsd_dir <- .resolve_hwsd_dir(NULL)
  mu_clay <- .read_hwsd_attributes_local(
    hwsd_dir,
    required = .hwsd_clay_columns()
  ) |>
    dplyr::filter(!is.na(.data$t_clay)) |>
    dplyr::summarise(
      clay_pct = stats::weighted.mean(.data$t_clay, .data$share),
      .by = "mu_global"
    )
  .aggregate_hwsd(
    hwsd_dir,
    mu_clay,
    target_res = 0.5,
    target_grid = cell_polity,
    value_col = "clay_pct",
    out_col = "clay_pct"
  )
}

# Whether to print phase-progress feedback. Real runs (including non-interactive
# Rscript batch runs, which are the common way this multi-minute model is run)
# should show progress so the user is never left staring at a silent process;
# under testthat it is suppressed so the test log stays clean. The march bar
# (purrr `.progress`) is separately gated by cli's show-after delay, so it never
# renders for the fast test fixtures and needs no explicit guard.
.cb_show_progress <- function() {
  !identical(Sys.getenv("TESTTHAT"), "true")
}
