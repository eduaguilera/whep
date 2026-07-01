# Urban / human-excreta nitrogen input to agriculture (Module C, Task C3).
#
# The per-capita urban-N-to-agriculture rate is a documented placeholder:
# Spain's own historical benchmark series (urban_n_reference,
# urban_kgn_cap_reference) applied as a global default, consistent with
# this branch's other Module C defaults (also Spain/Mediterranean-calibrated
# coefficients applied globally). See build_urban_n()'s @details for the
# forward-looking refinement note (sewage N from reconstructed dietary N
# intake, plus food-waste N from historical loss/waste estimates) -- not
# implemented here.
#
# Each cell's urban N is generated 100% as "surplus" (population and
# cropland-N-need do not coincide 1:1) and spilled to neighbouring cells
# with cropland room via allocate_manure_transport() (R/manure_transport.R),
# the same king-move room-weighted transport used by the manure engine's
# .manure_subnational() (R/build_livestock_nutrient_flows.R).

#' Build gridded urban/human-excreta nitrogen inputs to agriculture.
#'
#' @description
#' Estimates the nitrogen from urban human excreta and municipal waste
#' applied to agricultural land, per WHEP 0.5-degree grid cell. Each cell's
#' urban population (from [read_hyde_population()]) is converted to a
#' nitrogen load via a per-capita rate interpolated from Spain's own
#' historical benchmark series (`urban_n_reference` /
#' `urban_kgn_cap_reference`; see Details), then spilled from cells with no
#' local cropland room to same-polity neighbouring cells with spare
#' capacity via [allocate_manure_transport()], the same buffering used by
#' the manure engine.
#'
#' @details
#' The current per-capita rate is a documented placeholder (Spain's own
#' historical urban-N series applied as a global default). For a future
#' refinement, urban N should instead be derived from two distinct, more
#' mechanistic streams: (1) sewage/human-excreta N estimated from actual
#' historical per-capita dietary protein/N intake (already reconstructable
#' in WHEP via its FAOSTAT/commodity-balance food-supply data, rather than a
#' fixed external per-capita constant), and (2) food-waste/municipal-solid-
#' waste N from actual historical food-loss and waste estimates. This is out
#' of scope for the current task and is not implemented here.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   keeps every year `data$urban_population` covers.
#' @param data Optional named list of pre-loaded inputs: `urban_population`
#'   (`lon`, `lat`, `year`, `urban_pop`, falling back to
#'   [read_hyde_population()] when absent), `cell_polity` (`lon`, `lat`,
#'   `area_code`, required) and `cropland_ha` (`lon`, `lat`, `area_code`,
#'   `year`, `cropland_ha`, required: the gridded cropland area used as the
#'   simple room proxy, `cropland_ha * 0.170` t N/ha, the same EU-Nitrates
#'   fixed ceiling used by [allocate_manure_to_land()]'s `fixed_ceiling_kg_ha`
#'   default).
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year`, `urban_n_t` and
#'   `method_urban`.
#' @export
#' @examples
#' build_urban_n(example = TRUE)
build_urban_n <- function(years = NULL, data = list(), example = FALSE) {
  if (isTRUE(example)) {
    return(.example_urban_n())
  }
  urban_pop <- data$urban_population %||% read_hyde_population(years = years)
  polity <- .wb_require_input(data$cell_polity, "cell_polity", "area_code")
  cropland <- .wb_require_input(
    data$cropland_ha,
    "cropland_ha",
    c("area_code", "year", "cropland_ha")
  )
  generated <- .urban_n_generated(urban_pop, polity)
  source_cells <- .urban_source_cells(generated)
  sink_cells <- .urban_sink_cells(cropland)
  flows <- allocate_manure_transport(source_cells, sink_cells)
  .urban_finalise(flows)
}

# ---- Private helpers --------------------------------------------------

# Urban N generated per cell-year: urban_pop * urban_kgn_cap(year) / 1000,
# joined to the polity crosswalk for area_code / territory.
.urban_n_generated <- function(urban_pop, polity) {
  rate <- .urban_kgn_cap_series(unique(urban_pop$year))
  urban_pop |>
    dplyr::inner_join(rate, by = "year") |>
    dplyr::inner_join(polity, by = c("lon", "lat")) |>
    dplyr::mutate(
      urban_n_generated_t = .data$urban_pop * .data$urban_kgn_cap / 1000
    )
}

# Interpolate the per-capita urban-N rate to the requested years:
# fill_linear between urban_kgn_cap_reference benchmark years, held constant
# (carried forward AND backward, since the series has no data before its
# first benchmark year; see data-raw/build_urban_kgn_cap.R for why) outside
# the benchmark range.
.urban_kgn_cap_series <- function(years) {
  all_years <- sort(unique(c(years, whep::urban_kgn_cap_reference$year)))
  tibble::tibble(year = all_years) |>
    dplyr::left_join(whep::urban_kgn_cap_reference, by = "year") |>
    fill_linear(
      urban_kgn_cap,
      time_col = year,
      fill_forward = TRUE,
      fill_backward = TRUE
    ) |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::select("year", "urban_kgn_cap")
}

# Every urban-N-generating cell is a source: 100% of its urban N is surplus
# needing placement (population and cropland-N-need do not coincide 1:1).
# No urban carbon/VS stream is modelled, so surplus_c and surplus_vs are 0.
.urban_source_cells <- function(generated) {
  generated |>
    dplyr::filter(.data$urban_n_generated_t > 0) |>
    dplyr::transmute(
      year = .data$year,
      territory = as.character(.data$area_code),
      sub_territory = paste0(.data$lon, "_", .data$lat),
      surplus_n = .data$urban_n_generated_t,
      surplus_c = 0,
      surplus_vs = 0
    )
}

# Every cell with cropland area is a possible sink: room_n is the simple
# EU-Nitrates fixed-ceiling proxy (170 kg N/ha, the same
# fixed_ceiling_kg_ha default as allocate_manure_to_land(), since Module C
# has no crop-N-demand table wired in yet).
.urban_sink_cells <- function(cropland) {
  fixed_ceiling_kg_ha <- 170
  cropland |>
    dplyr::filter(.data$cropland_ha > 0) |>
    dplyr::transmute(
      year = .data$year,
      territory = as.character(.data$area_code),
      sub_territory = paste0(.data$lon, "_", .data$lat),
      room_n = fixed_ceiling_kg_ha / 1000 * .data$cropland_ha
    )
}

# Parse sub_territory back to lon/lat, aggregate transported + residual flows
# to the final schema and stamp method_urban.
.urban_finalise <- function(flows) {
  coords <- .parse_cell_id(flows$sub_territory)
  flows |>
    dplyr::mutate(
      lon = coords$lon,
      lat = coords$lat,
      area_code = .data$territory
    ) |>
    dplyr::summarise(
      urban_n_t = sum(.data$applied_n),
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    dplyr::mutate(method_urban = "spain_hist_rate|room_weighted")
}

# Toy fixture for a runnable example (one cell, one polity, one year).
.example_urban_n <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~urban_n_t, ~method_urban,
    -0.25, -0.25, "ESP", 2020L, 4.5, "spain_hist_rate|room_weighted"
  )
}
