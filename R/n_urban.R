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
#' @inheritParams build_water_balance
#' @param data Optional named list of pre-loaded inputs: `urban_population`
#'   (`lon`, `lat`, `year`, `urban_pop`, falling back to
#'   [read_hyde_population()] when absent), `cell_polity` (`lon`, `lat`,
#'   `area_code`, plus optional `polity_frac`; a missing `polity_frac` is
#'   treated as 1 for backwards compatibility) and `cropland_ha` (`lon`,
#'   `lat`, `area_code`, `year`, `cropland_ha`, required: the gridded cropland
#'   area used as the simple room proxy, `cropland_ha * 0.170` t N/ha, the same
#'   EU-Nitrates fixed ceiling used by [allocate_manure_to_land()]'s
#'   `fixed_ceiling_kg_ha` default). Both frames' `area_code` must be the
#'   numeric WHEP area code, whole-numbered, as [build_cell_polity()] emits
#'   it. Anything else -- an ISO3 literal, an area name, a fractional value --
#'   aborts with class `whep_urban_area_code_unresolved`, naming the frame
#'   that carries it. It is not bridged: the two frames key the same transport
#'   partition, so one written in a different vocabulary from the other would
#'   silently strand a cell's load on a cell with no room instead of placing
#'   it, and an ISO3 resolves to a `polity_area_code` aggregation bucket that
#'   is not every territory's own code (`"SSD"` would become 206, Sudan
#'   (former)). Map to the code first, via [add_area_code()] or
#'   [regions_full].
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year`, `urban_n_t` and
#'   `method_urban`, plus the polity columns below, plus
#'   `reporting_polity_out_of_span` when `polity_validity = "flag"`.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_urban_n(example = TRUE)
build_urban_n <- function(
  years = NULL,
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE
) {
  polity_validity <- rlang::arg_match(polity_validity)
  if (isTRUE(example)) {
    return(.resolve_polity_validity(.example_urban_n(), polity_validity))
  }
  urban_pop <- data$urban_population %||% read_hyde_population(years = years)
  urban_pop <- .urban_filter_years(urban_pop, years)
  polity <- .wb_require_input(data$cell_polity, "cell_polity", "area_code") |>
    .urban_resolve_area_code("cell_polity")
  cropland <- .wb_require_input(
    data$cropland_ha,
    "cropland_ha",
    c("area_code", "year", "cropland_ha")
  ) |>
    .urban_filter_years(years) |>
    .urban_resolve_area_code("cropland_ha")
  generated <- .urban_n_generated(urban_pop, polity)
  source_cells <- .urban_source_cells(generated)
  sink_cells <- .urban_sink_cells(cropland)
  flows <- allocate_manure_transport(source_cells, sink_cells)
  .urban_finalise(flows) |>
    .resolve_polity_validity(polity_validity)
}

# ---- Private helpers --------------------------------------------------

# Require an input frame's `area_code` to BE the numeric WHEP area code, and
# fix its type once, at the input boundary, before it is stringified into the
# transport allocator's `territory` key by .urban_source_cells() /
# .urban_sink_cells().
#
# Two separate defects live here, and only the first is about ordering.
#
# 1. Resolving after transport (the shape #487 introduced and #597 reported)
#    left the resolution and the partition it keys disagreeing. Neither a
#    column-set census nor an area_code census can see it, because the output
#    schema and the output codes are identical either way and only the cell
#    the nitrogen lands on moves: two frames written in different
#    vocabularies for the SAME polity ("ESP" in cropland_ha, 203 in
#    cell_polity) produced `territory` keys that never met in
#    allocate_manure_transport(), so the source found no reachable sink and
#    its whole load stranded on its own room-less cell -- then was relabelled
#    onto area_code 203 anyway, silently, with no warning of any kind because
#    the ISO3 never reached a resolver at all.
#
# 2. Accepting an ISO3 is wrong for THIS function. The other four callers of
#    .manure_territory_to_area_code() receive `territory` from
#    build_livestock_nutrient_flows(), i.e. in another frame's vocabulary, so
#    a bridge is meaningful there. build_urban_n() manufactures the key
#    itself out of a column its own docs call `area_code`, so a bridge only
#    buys a chance of silently answering with a polity_area_code aggregation
#    bucket that is not the territory's own ("SSD" -> 206, Sudan (former)).
#    The column is refused instead: no bridge, no warn-and-continue, no
#    silent coercion of a label.
#
# The gridded pin build_cell_polity() emits is integer-keyed, so this is the
# identity on real input (asserted over the whole regions_full vocabulary in
# test_n_urban.R) and published values do not move.
.urban_resolve_area_code <- function(x, input) {
  codes <- x$area_code
  arg <- paste0("data$", input)
  if (!is.numeric(codes)) {
    shown <- utils::head(unique(stats::na.omit(as.character(codes))), 3)
    cli::cli_abort(
      c(
        "{.field area_code} in {.arg {arg}} must be the numeric WHEP area
         code.",
        x = "It is {.cls {class(codes)}}, e.g. {.val {shown}}.",
        i = "Pass the code itself, never an {.field iso3c} or a name:
             {.fun build_cell_polity} emits it and {.code whep::regions_full}
             maps an {.field iso3c} onto it. An {.field iso3c} would resolve
             to {.field polity_area_code}, an aggregation bucket that is not
             every territory's own code."
      ),
      class = "whep_urban_area_code_unresolved"
    )
  }
  .urban_check_whole_codes(codes, arg)
  dplyr::mutate(x, area_code = as.integer(codes))
}

# A numeric `area_code` that is not a whole number is a real key error -- a
# share or a fraction landing in the code column -- and as.integer() would
# truncate it into a DIFFERENT territory's code rather than fail.
.urban_check_whole_codes <- function(codes, arg) {
  bad <- unique(codes[!is.na(codes) & codes != trunc(codes)])
  if (length(bad) == 0) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.field area_code} in {.arg {arg}} must be a whole number.",
      x = "{cli::qty(length(bad))}Fractional value{?s}:
           {.val {utils::head(bad, 3)}}.",
      i = "Truncating would silently name a different territory."
    ),
    class = "whep_urban_area_code_unresolved"
  )
}

.urban_filter_years <- function(x, years) {
  if (is.null(years)) {
    return(x)
  }
  dplyr::filter(x, .data$year %in% years)
}

# Urban N generated per cell-polity-year: the cell load is split by
# polity_frac after joining the polity crosswalk. Simple one-polity crosswalks
# may omit polity_frac and retain the historical implicit value of 1.
.urban_n_generated <- function(urban_pop, polity) {
  rate <- .urban_kgn_cap_series(unique(urban_pop$year))
  if (!rlang::has_name(polity, "polity_frac")) {
    polity <- dplyr::mutate(polity, polity_frac = 1)
  }
  urban_pop |>
    dplyr::inner_join(rate, by = "year") |>
    dplyr::inner_join(polity, by = c("lon", "lat")) |>
    dplyr::mutate(
      urban_n_generated_t = .data$urban_pop *
        .data$urban_kgn_cap *
        .data$polity_frac /
        1000
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
      # `territory` is the character key the transport allocator works in. It
      # is `as.character()` of the numeric area_code .urban_resolve_area_code()
      # already produced at the input boundary, so recovering it is a plain
      # parse and cannot fold, bridge or fail (#597).
      area_code = as.integer(.data$territory)
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
    -0.25, -0.25, 203L, 2020L, 4.5, "spain_hist_rate|room_weighted"
  ) |>
    .add_reporting_polity_columns()
}
