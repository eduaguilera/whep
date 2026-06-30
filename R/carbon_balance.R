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
#' @param data Named list of pre-loaded inputs, each falling back to its reader
#'   when absent: \code{c_inputs} (per cell, land-use class and year, with
#'   \code{c_input_mgc_ha_yr} and \code{humified_fraction}); \code{land_use}
#'   (yearly per-cell per-class \code{lon}, \code{lat}, \code{area_code},
#'   \code{year}, \code{land_use}, \code{area_ha}); \code{climate} (per cell-year
#'   \code{climate_modifier}); \code{clay} (per cell \code{clay_pct}).
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
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_carbon_balance())
  }
  model <- rlang::arg_match(model)
  resolution <- rlang::arg_match(resolution)
  d <- .cb_resolve_inputs(data)
  classes <- .cb_class_table(d) |> .cb_attach_equilibrium(model)
  init <- .cb_initialise(classes)
  marched <- .cb_march(classes, init)
  marched |>
    .cb_derive_son() |>
    dplyr::mutate(method_soc = model) |>
    .cb_finalise(resolution)
}

# -- Input resolution ---------------------------------------------------------

.cb_resolve_inputs <- function(data) {
  list(
    c_inputs = data$c_inputs %||% .cb_read_c_inputs(),
    land_use = data$land_use %||% .cb_read_land_use(),
    climate = data$climate %||% .cb_read_climate(),
    clay = data$clay %||% .cb_read_clay()
  )
}

# Join land-use areas, carbon inputs, climate and clay into one per-cell,
# per-land-use, per-year class table; add the cell-year land-use fraction.
.cb_class_table <- function(d) {
  d$land_use |>
    dplyr::mutate(
      frac = .data$area_ha / sum(.data$area_ha),
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    dplyr::inner_join(
      d$c_inputs,
      by = c("lon", "lat", "area_code", "year", "land_use")
    ) |>
    dplyr::left_join(d$climate, by = c("lon", "lat", "area_code", "year")) |>
    dplyr::left_join(d$clay, by = c("lon", "lat")) |>
    dplyr::mutate(climate_modifier = .data$climate_modifier %|% 1)
}

# -- Equilibrium + initialisation ---------------------------------------------

# Per-class equilibrium SOC density: run the selected model to steady state
# under each class's carbon input and climate. Reuses calculate_soc_dynamics();
# never reimplements the pool kinetics. One run per distinct input combination.
.cb_equilibrium <- function(model, classes) {
  classes |>
    dplyr::distinct(
      .data$land_use,
      .data$c_input_mgc_ha_yr,
      .data$humified_fraction,
      .data$climate_modifier,
      .data$clay_pct
    ) |>
    dplyr::mutate(
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
.cb_initialise <- function(classes) {
  first_year <- min(classes$year)
  classes |>
    dplyr::filter(.data$year == first_year) |>
    .cb_init_density()
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
.cb_march <- function(classes, init) {
  cells <- classes |>
    dplyr::distinct(.data$lon, .data$lat, .data$area_code)
  purrr::pmap(
    list(cells$lon, cells$lat, cells$area_code),
    \(lon, lat, ac) {
      .cb_march_cell(
        dplyr::filter(
          classes,
          .data$lon == lon,
          .data$lat == lat,
          .data$area_code == ac
        ),
        dplyr::filter(
          init,
          .data$lon == lon,
          .data$lat == lat,
          .data$area_code == ac
        )
      )
    }
  ) |>
    dplyr::bind_rows()
}

# March one cell forward year by year. State is a named density vector indexed
# by land-use class; per year compute the model annual update (mineralization
# and input), then redistribute released carbon via the land-use-change buffer.
.cb_march_cell <- function(cell, init) {
  years <- sort(unique(cell$year))
  state <- stats::setNames(init$stock_mgc_ha, init$land_use)
  prev_area <- .cb_year_areas(cell, years[1])
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    yr <- cell |> dplyr::filter(.data$year == years[i])
    step <- .cb_year_step(yr, state, prev_area)
    out[[i]] <- step$rows
    state <- step$state
    prev_area <- .cb_year_areas(cell, years[i])
  }
  dplyr::bind_rows(out)
}

# One year of evolution for a cell: model annual update per class, then the
# carbon-conserving land-use-change transfer driven by the area change.
.cb_year_step <- function(yr, state, prev_area) {
  yr <- yr |> dplyr::arrange(.data$land_use)
  prev_stock <- state[yr$land_use]
  k_eff <- .cb_effective_rate(yr)
  mineralization <- prev_stock * k_eff
  stepped <- prev_stock - mineralization + yr$c_input_mgc_ha_yr
  transferred <- .cb_luc_transfer(
    tibble::tibble(
      land_use = yr$land_use,
      stock_mgc_ha = stepped,
      old_area_ha = prev_area[yr$land_use],
      new_area_ha = yr$area_ha
    )
  )
  rows <- .cb_year_rows(yr, transferred, mineralization)
  list(
    rows = rows,
    state = stats::setNames(transferred$stock_mgc_ha, transferred$land_use)
  )
}

# Effective annual decay rate making the stock relax to the model equilibrium
# (K = input / soc_eq), the Spain_Hist Miner = Stock * K form (SOC_Fun.R:280).
.cb_effective_rate <- function(yr) {
  eq <- yr$soc_eq_mgc_ha
  dplyr::if_else(eq > 0, yr$c_input_mgc_ha_yr / eq, 0)
}

# Assemble the per-class output rows for one cell-year. `transferred` is keyed
# by land_use (it was reordered by area change inside the transfer), so it is
# matched back to the year's class order. luc_transfer_mgc_ha is the buffer mass
# exchanged per current hectare (sums to zero across the cell).
.cb_year_rows <- function(yr, transferred, mineralization) {
  idx <- match(yr$land_use, transferred$land_use)
  tibble::tibble(
    lon = yr$lon,
    lat = yr$lat,
    area_code = yr$area_code,
    land_use = yr$land_use,
    year = yr$year,
    area_ha = yr$area_ha,
    stock_mgc_ha = transferred$stock_mgc_ha[idx],
    mineralization_mgc_ha = mineralization,
    c_input_mgc_ha = yr$c_input_mgc_ha_yr,
    luc_transfer_mgc_ha = transferred$mass_moved[idx] / yr$area_ha,
    rate_mgc_ha = yr$c_input_mgc_ha_yr - mineralization
  )
}

.cb_year_areas <- function(cell, year) {
  yr <- cell |> dplyr::filter(.data$year == year)
  stats::setNames(yr$area_ha, yr$land_use)
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
# mineralization C:N (a positive N input); net gain immobilizes nitrogen at the
# sequestration C:N (SOC_Fun.R:278-283). The asymmetric ratios come from
# whep::soil_cn_ratios; Cropland uses the Conventional row.
.cb_derive_son <- function(marched) {
  cn <- .cb_cn_lookup()
  marched |>
    dplyr::left_join(cn, by = "land_use") |>
    dplyr::mutate(
      cn_used = dplyr::if_else(
        .data$rate_mgc_ha < 0,
        .data$cn_mineralization,
        .data$cn_sequestration
      ),
      son_change_kgn_ha = -.data$rate_mgc_ha * 1000 / .data$cn_used
    ) |>
    dplyr::select(-"cn_mineralization", -"cn_sequestration", -"cn_used")
}

# Map each land-use class to its asymmetric C:N pair. Cropland -> the Cropland
# Conventional row; every other class -> the NonCropland Conventional row.
.cb_cn_lookup <- function() {
  conv <- whep::soil_cn_ratios |>
    dplyr::filter(.data$management == "Conventional")
  crop <- conv |> dplyr::filter(.data$cropland_class == "Cropland")
  noncrop <- conv |> dplyr::filter(.data$cropland_class == "NonCropland")
  tibble::tibble(
    land_use = c("Cropland", "NonCropland"),
    cn_mineralization = c(
      crop$cn_mineralization,
      noncrop$cn_mineralization
    ),
    cn_sequestration = c(crop$cn_sequestration, noncrop$cn_sequestration)
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

`%|%` <- function(x, y) dplyr::if_else(is.na(x), y, x)

# -- Reader stubs (real readers are separate later tasks) ---------------------

.cb_read_c_inputs <- function() {
  cli::cli_abort(
    c(
      "No {.field c_inputs} reader is wired yet.",
      i = "Pass {.code data$c_inputs} from {.fun build_soil_carbon_inputs}."
    )
  )
}

.cb_read_land_use <- function() {
  cli::cli_abort(
    c(
      "No {.field land_use} reader is wired yet.",
      i = "Pass {.code data$land_use} (yearly per-cell per-class areas)."
    )
  )
}

.cb_read_climate <- function() {
  cli::cli_abort(
    "No {.field climate} reader is wired yet; pass {.code data$climate}."
  )
}

.cb_read_clay <- function() {
  cli::cli_abort(
    "No {.field clay} reader is wired yet; pass {.code data$clay}."
  )
}
