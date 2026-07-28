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
#' @param model Turnover model: one of \code{"hsoc"} (default), \code{"rothc"},
#'   \code{"icbm"}, \code{"amg"} or \code{"century"}.
#' @param resolution \code{"grid"} (default, per cell and land-use class) or
#'   \code{"polity"} (aggregated to \code{area_code} conserving carbon mass).
#' @param years Optional integer vector of calendar years to keep. \code{NULL}
#'   (default) keeps every year the inputs cover, but reading the full LUH2 range
#'   (850-2015) is infeasible turnkey, so a subset is strongly recommended when
#'   the default readers are used. Threaded into every default reader
#'   (\code{\link{read_luh2_landuse}}, \code{\link{get_soc_climate_drivers}} and
#'   \code{\link{build_carbon_inputs}}); ignored for inputs supplied via
#'   \code{data}.
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
#'   optional \code{equilibrium_climate} (the pre-industrial climatological
#'   normal, one representative monthly cycle per cell, used only for the
#'   equilibrium spin-up modifier while the forward march uses the year-specific
#'   drivers).
#' @param example If \code{TRUE}, return a small fixture instead of reading
#'   remote data. Defaults to \code{FALSE}.
#' @return A tibble keyed by \code{(lon, lat, area_code, land_use, year)} at
#'   \code{"grid"} resolution (or \code{(area_code, year)} at \code{"polity"}),
#'   with \code{stock_mgc_ha}, \code{mineralization_mgc_ha}, \code{c_input_mgc_ha},
#'   \code{luc_transfer_mgc_ha}, \code{rate_mgc_ha}, \code{son_change_kgn_ha},
#'   \code{area_ha} and \code{method_soc}.
#' @source Aguilera, E. et al. (2018). Embodied energy in agricultural inputs.
#'   \doi{10.1016/j.scitotenv.2018.03.118}; land-use-change carbon transfer
#'   ported from the Spain historical pipeline.
#' @export
#' @examples
#' build_carbon_balance(example = TRUE)
build_carbon_balance <- function(
  model = c("hsoc", "rothc", "icbm", "amg", "century"),
  resolution = c("grid", "polity"),
  data = list(),
  years = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_carbon_balance())
  }
  model <- rlang::arg_match(model)
  resolution <- rlang::arg_match(resolution)
  progress <- .cb_show_progress()
  if (progress) {
    cli::cli_progress_step("Reading model inputs (may read multi-GB rasters)")
  }
  d <- .cb_resolve_inputs(data, years)
  if (progress) {
    cli::cli_progress_step("Computing per-class equilibrium")
  }
  classes <- .cb_class_table(d, model) |> .cb_attach_equilibrium(model)
  if (progress) {
    cli::cli_progress_step("Initialising soil-carbon pools")
  }
  init <- .cb_initialise(classes, model, d)
  if (progress) {
    cli::cli_progress_done()
  }
  marched <- .cb_march(classes, init)
  marched |>
    .cb_derive_son() |>
    dplyr::mutate(method_soc = model) |>
    .cb_finalise(resolution)
}

# -- Input resolution ---------------------------------------------------------

.cb_resolve_inputs <- function(data, years = NULL) {
  c_inputs <- data$c_inputs %||% .cb_read_c_inputs(years)
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
  base <- d$land_use |>
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
  modifiers <- .cb_climate_modifier_table(d$climate, clay, model, base$land_use)
  base |>
    .cb_join_modifier(modifiers) |>
    dplyr::left_join(clay, by = c("lon", "lat")) |>
    .cb_drop_uncovered_climate()
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
.cb_climate_modifier_table <- function(climate, clay, model, land_use_classes) {
  keys <- c("lon", "lat", "area_code", "year")
  if (rlang::has_name(climate, "climate_modifier")) {
    return(dplyr::distinct(
      dplyr::select(climate, dplyr::all_of(c(keys, "climate_modifier")))
    ))
  }
  climate |>
    .cb_join_clay(clay) |>
    .cb_arrange_by_month() |>
    .cb_attach_soil_cover(land_use_classes) |>
    dplyr::summarise(
      climate_modifier = .cb_year_climate_modifier(
        model,
        dplyr::pick(dplyr::everything()),
        dplyr::first(.data$clay_pct)
      ),
      .by = dplyr::all_of(c(keys, "land_use"))
    )
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
.cb_attach_soil_cover <- function(climate, land_use_classes) {
  classes <- unique(land_use_classes)
  climate |>
    dplyr::select(-dplyr::any_of("soil_cover")) |>
    dplyr::mutate(
      months_from_peak = .cb_months_from_peak(.data$month, .data$temp_c),
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    tidyr::crossing(land_use = classes) |>
    dplyr::mutate(.cover_key = stringr::str_to_lower(.data$land_use)) |>
    dplyr::left_join(
      .cb_cover_curve(),
      by = c(".cover_key" = "land_use", "months_from_peak")
    ) |>
    dplyr::mutate(soil_cover = dplyr::coalesce(.data$soil_cover, 0)) |>
    dplyr::select(-".cover_key")
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
  # closed form is the exact point that spin-up converges to (verified identical
  # to < 1e-9 relative). Models without a wired closed form (RothC, Century)
  # fall back to the one-trajectory-per-combination path (see #352).
  closed <- .cb_closed_form_equilibrium(model, combos)
  if (!is.null(closed)) {
    return(dplyr::mutate(combos, soc_eq_mgc_ha = closed))
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
  )
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
    hsoc = .cb_hsoc_equilibrium(input, combos$humified_fraction, cm),
    amg = .cb_amg_equilibrium(input, cm),
    NULL
  )
}

# AMG: active pool at its steady state ca_ss = h * input / k (k scaled by the
# climate modifier); the total adds the inert stable share, giving
# ca_ss / (1 - f_iom) (see calculate_soc_amg()'s steady_state init).
.cb_amg_equilibrium <- function(input, climate_modifier) {
  k <- .soc_param("amg", "active", "decomposition_rate") * climate_modifier
  f_iom <- .soc_param("amg", "stable", "inert_fraction")
  (.amg_default_h() * input / k) / (1 - f_iom)
}

# Closed-form HSOC steady state, vectorised over its inputs. The active fresh
# and humus pools sit at their fixed points input_pool / (k_pool *
# climate_modifier); the inert (IOM) pool is the Falloon (1998) function
# 0.049 * active^1.139 of the seed active stock (floored at 1, matching
# `.cb_seed_stock()`). This is the exact stock the 5000-year HSOC spin-up
# relaxes to (the pool series starts at the fixed point and is flat), so it
# replaces a 5000-step trajectory per input combination with an O(1)
# expression.
.cb_hsoc_equilibrium <- function(input, humified_fraction, climate_modifier) {
  k_fresh <- .cb_param("hsoc", "fresh")
  k_humus <- .cb_param("hsoc", "humus")
  active <- input *
    (1 - humified_fraction) /
    (k_fresh * climate_modifier) +
    input * humified_fraction / (k_humus * climate_modifier)
  active + 0.049 * pmax(active, 1)^1.139
}

# Attach the equilibrium density to every class-year row by joining on the
# input combination that drives it.
.cb_attach_equilibrium <- function(classes, model) {
  eq <- .cb_equilibrium(model, classes)
  classes |>
    dplyr::left_join(
      eq,
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
  seed <- .cb_seed_stock(model, input, humified_fraction, cm)
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

# Collapse any model's per-year output to a single total stock per year.
.cb_total_stock <- function(traj) {
  if (rlang::has_name(traj, "soc_total")) {
    return(dplyr::transmute(
      traj,
      year = .data$year,
      stock_mgc_ha = .data$soc_total
    ))
  }
  traj |>
    dplyr::summarise(
      stock_mgc_ha = sum(.data$stock_mgc_ha),
      .by = "year"
    )
}

# Initialise each cell from the earliest year: every class starts at the
# cell-weighted-mean equilibrium density (SOC_init = sum_lu(frac_lu * soc_eq_lu)).
# The equilibrium (spin-up) modifier optionally comes from a distinct
# pre-industrial climatological normal (`d$equilibrium_climate`, RESOLVED F3),
# so the initial stock reflects the equilibrium climate while the forward march
# uses the year-specific modifier already carried in `soc_eq_mgc_ha`.
.cb_initialise <- function(classes, model, d) {
  first <- dplyr::filter(
    classes,
    .data$year == min(.data$year),
    .by = c("lon", "lat", "area_code")
  )
  first <- .cb_apply_equilibrium_climate(first, model, d)
  .cb_init_density(first)
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
    first$land_use
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
  land_use_classes
) {
  cell_keys <- c("lon", "lat", "area_code", "land_use")
  eq_climate |>
    .cb_join_clay(clay) |>
    .cb_arrange_by_month() |>
    .cb_attach_soil_cover(land_use_classes) |>
    dplyr::summarise(
      climate_modifier_eq = .cb_year_climate_modifier(
        model,
        dplyr::pick(dplyr::everything()),
        dplyr::first(.data$clay_pct)
      ),
      .by = dplyr::all_of(cell_keys)
    )
}

# Cell-level initial SOC density: the fraction-weighted mean of the per-class
# equilibrium densities, applied uniformly to each class in the cell.
.cb_init_density <- function(classes) {
  classes |>
    dplyr::mutate(
      stock_mgc_ha = sum(.data$frac * .data$soc_eq_mgc_ha),
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
  cell_key <- \(x) paste(x$lon, x$lat, x$area_code, sep = "\r")
  classes_split <- split(classes, cell_key(classes))
  init_split <- split(init, cell_key(init))
  purrr::map(
    names(classes_split),
    \(k) .cb_march_cell(classes_split[[k]], init_split[[k]] %||% init[0, ]),
    .progress = "Marching cells"
  ) |>
    dplyr::bind_rows()
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
  state <- stats::setNames(init$stock_mgc_ha, init$land_use)
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    cur <- cell |> dplyr::filter(.data$year == years[i])
    prev <- if (i == 1L) {
      NULL
    } else {
      dplyr::filter(cell, .data$year == years[i - 1L])
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
  cur <- cur |> dplyr::arrange(.data$land_use)
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
  dt <- data.table::as.data.table(before)
  dt[, area_change := new_area_ha - old_area_ha]
  data.table::setorder(dt, area_change)
  pool <- list(carbon = 0, area = 0)
  stocks <- dt$stock_mgc_ha
  moved <- numeric(nrow(dt))
  for (i in seq_len(nrow(dt))) {
    res <- .cb_transfer_one(
      stocks[i],
      dt$old_area_ha[i],
      dt$new_area_ha[i],
      pool
    )
    stocks[i] <- res$stock
    moved[i] <- res$mass_moved
    pool <- res$pool
  }
  dt[, stock_mgc_ha := stocks]
  dt[, mass_moved := moved]
  tibble::as_tibble(
    dt[, c("land_use", "stock_mgc_ha", "new_area_ha", "mass_moved")]
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
  } else if (new_area > old_area && pool$area > 0) {
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
  dens <- pool$carbon / pool$area
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
    stringr::str_to_lower(land_use) == "cropland",
    "Cropland",
    "NonCropland"
  )
}

# The asymmetric C:N pair per cropland class (Conventional management). Keyed by
# cropland_class so .cb_derive_son joins on the classified label, never on the
# raw land-use string.
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
      rate_mgc_ha = .cb_wmean(.data$rate_mgc_ha, .data$area_ha),
      son_change_kgn_ha = .cb_wmean(.data$son_change_kgn_ha, .data$area_ha),
      method_soc = .data$method_soc[1],
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
.cb_read_c_inputs <- function(years = NULL) {
  build_carbon_inputs(resolution = "grid", years = years)
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
  get_soc_climate_drivers(
    years = years,
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

# The cell -> polity crosswalk (lon, lat, area_code) from the spatialization
# country grid, the same source grass_natural and LUH2 use.
.cb_read_cell_polity <- function() {
  whep_read_file("spatialize-country-grid") |>
    .normalize_country_grid() |>
    dplyr::select("lon", "lat", "area_code")
}

# Per-cell topsoil clay percent from HWSD: the map-unit share-weighted mean of
# the HWSD topsoil clay fraction (t_clay), aggregated to the 0.5-degree grid
# (cropped to `cell_polity`) via the shared HWSD aggregation helper. Reuses the
# HWSD attribute/raster path read_soil_hydraulic() uses so the clay driver is
# consistent with the hydraulic drivers.
.cb_hwsd_clay <- function(cell_polity) {
  rlang::check_installed("terra")
  hwsd_dir <- .resolve_hwsd_dir(NULL)
  mu_clay <- .read_hwsd_attributes_local(hwsd_dir) |>
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
