# Human-population nitrogen input to agriculture (Module C, Task C3): the
# nitrogen a population returns to farmland through municipal solid waste,
# sewage sludge and human excreta. It is a function of the whole population,
# not of urban dwellers alone.
#
# POPULATION BASIS. The load is population x a per-capita rate, and the two
# must be on the same basis or the product is scaled by an inverse urban
# fraction that is itself a function of time (WPP total / HYDE urban measured
# 3.02 in 1960 and 1.91 in 2017, global sums). So `population_basis` selects
# the pair, never one side of it:
#   "total" -- (default) UN WPP total population on HYDE's total-population
#              pattern (build_total_population_grid()) with
#              human_kgn_cap_total_reference, kg N per TOTAL inhabitant;
#   "urban" -- HYDE urban count (`urbc`, read_hyde_population()) with
#              human_kgn_cap_reference, kg N per URBAN inhabitant.
# Both rates divide the same calibration nitrogen series by the calibration
# population on their own basis, so either one regenerates its calibration
# total in a benchmark year; what changes between the bases is how the rate
# transfers to populations whose urban share differs from the calibration
# one. The basis is stamped on every output row.
#
# The per-capita rate is a documented placeholder: one national historical
# series (human_n_reference, human_kgn_cap_reference) applied as a global
# default, consistent with this branch's other Module C defaults. See
# build_human_n()'s @details for the forward-looking refinement note (sewage
# N from reconstructed dietary N intake, plus food-waste N from historical
# loss/waste estimates) -- not implemented here.
#
# Each cell's human N is generated 100% as "surplus" (population and
# cropland-N-need do not coincide 1:1) and spilled to neighbouring cells
# with cropland room via allocate_manure_transport() (R/manure_transport.R),
# the same king-move room-weighted transport used by the manure engine's
# .manure_subnational() (R/build_livestock_nutrient_flows.R).

#' Build gridded human-population nitrogen inputs to agriculture.
#'
#' @description
#' Estimates the nitrogen the human population returns to agricultural land
#' through municipal solid waste, sewage sludge and human excreta, per WHEP
#' 0.5-degree grid cell. Each polycell's population is converted to a
#' nitrogen load via a per-capita rate interpolated from a national
#' historical benchmark series, the Spanish series taken as reference
#' ([human_n_reference]; see Details), then spilled from cells with no local
#' cropland room to same-polity neighbouring cells with spare capacity via
#' [allocate_manure_transport()], the same buffering used by the manure
#' engine.
#'
#' The population and the rate are chosen together by `population_basis`, so
#' a per-urban-inhabitant rate is never applied to a total population:
#'
#' * `"total"` (default): UN WPP total population downscaled by HYDE's
#'   total-population pattern ([build_total_population_grid()]) times
#'   [human_kgn_cap_total_reference], kg N per inhabitant.
#' * `"urban"`: HYDE's urban population count
#'   (`read_hyde_population(variable = "urban")`) times
#'   [human_kgn_cap_reference], kg N per urban inhabitant.
#'
#' Both rates are the calibration nitrogen divided by the calibration
#' population on the same basis, so either regenerates its calibration total.
#' Elsewhere they differ by how far a population's urban share departs from
#' the calibration one: in 2010 the global WPP total population is 2.0 times
#' HYDE's global urban count.
#'
#' `build_urban_n()` is the deprecated former name of this function. It
#' forwards every argument to `build_human_n()` and warns (class
#' `whep_build_urban_n_deprecated`, also `lifecycle_warning_deprecated`); it
#' will be removed in a future release. The output columns were renamed with
#' it: `urban_n_t` is now `human_n_t`, and `method_urban`,
#' `method_urban_population` and `method_urban_kgn_cap` are now
#' `method_human`, `method_human_population` and `method_human_kgn_cap`.
#'
#' @details
#' The current per-capita rate is a documented placeholder (one national
#' historical series applied as a global default). For a future refinement,
#' human N should instead be derived from two distinct, more mechanistic
#' streams: (1) sewage/human-excreta N estimated from actual historical
#' per-capita dietary protein/N intake (already reconstructable in WHEP via
#' its FAOSTAT/commodity-balance food-supply data, rather than a fixed
#' external per-capita constant), and (2) food-waste/municipal-solid-waste N
#' from actual historical food-loss and waste estimates. This is out of scope
#' for the current task and is not implemented here.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   keeps every year the supplied population covers; it is required when the
#'   population is read rather than supplied.
#' @param population_basis Which population, and the per-capita rate on the
#'   same basis, generates the load: `"total"` (default) or `"urban"`. See
#'   Description. Recorded in the `method_human_population` and
#'   `method_human_kgn_cap` output columns.
#' @inheritParams build_water_balance
#' @param data Optional named list of pre-loaded inputs: `total_population`
#'   (`lon`, `lat`, `area_code`, `year`, `population`, one row per polycell as
#'   [build_total_population_grid()] returns it; read under
#'   `population_basis = "total"`, falling back to that builder when absent;
#'   taken per polycell, not re-split by `polity_frac`, and every polycell
#'   must exist in `cell_polity`), `urban_population` (`lon`, `lat`, `year`,
#'   `urban_pop`; read under `population_basis = "urban"`, falling back to
#'   `read_hyde_population(variable = "urban")` when absent), `cell_polity`
#'   (`lon`, `lat`, `area_code`, plus optional `polity_frac`; a missing
#'   `polity_frac` is treated as 1 for backwards compatibility) and
#'   `cropland_ha` (`lon`, `lat`, `area_code`, `year`, `cropland_ha`,
#'   required: the gridded cropland area used as the simple room proxy,
#'   `cropland_ha * 0.170` t N/ha, the same EU-Nitrates fixed ceiling used by
#'   [allocate_manure_to_land()]'s `fixed_ceiling_kg_ha` default). Supplying
#'   only the other basis's population aborts with class
#'   `whep_human_n_population_basis_mismatch` rather than silently reading a
#'   default. Both frames' `area_code` must be the numeric WHEP area code,
#'   whole-numbered, as [build_cell_polity()] emits it. Anything else -- an
#'   ISO3 literal, an area name, a fractional value -- aborts with class
#'   `whep_human_n_area_code_unresolved` (also
#'   `whep_urban_area_code_unresolved`, its former name), naming the frame
#'   that carries it. It is not bridged: the two frames key the same transport
#'   partition, so one written in a different vocabulary from the other would
#'   silently strand a cell's load on a cell with no room instead of placing
#'   it, and an ISO3 resolves to a `polity_area_code` aggregation bucket that
#'   is not every territory's own code (`"SSD"` would become 206, Sudan
#'   (former)). Map to the code first, via [add_area_code()] or
#'   [regions_full].
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year`, `human_n_t`,
#'   `method_human`, `method_human_population` (`"total_population"` or
#'   `"urban_population"`) and `method_human_kgn_cap`
#'   (`"kg_n_per_total_inhabitant"` or `"kg_n_per_urban_inhabitant"`), plus
#'   the polity columns below, plus `reporting_polity_out_of_span` when
#'   `polity_validity = "flag"`.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_human_n(example = TRUE)
build_human_n <- function(
  years = NULL,
  population_basis = c("total", "urban"),
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE
) {
  population_basis <- rlang::arg_match(population_basis)
  polity_validity <- rlang::arg_match(polity_validity)
  if (isTRUE(example)) {
    return(.resolve_polity_validity(
      .example_human_n(population_basis),
      polity_validity
    ))
  }
  .human_check_population_slot(data, population_basis)
  polity <- .wb_require_input(data$cell_polity, "cell_polity", "area_code") |>
    .human_resolve_area_code("cell_polity")
  cropland <- .wb_require_input(
    data$cropland_ha,
    "cropland_ha",
    c("area_code", "year", "cropland_ha")
  ) |>
    .human_filter_years(years) |>
    .human_resolve_area_code("cropland_ha")
  population <- .human_polycell_population(
    population_basis,
    data,
    polity,
    years
  )
  generated <- .human_n_generated(population, population_basis)
  source_cells <- .human_source_cells(generated)
  sink_cells <- .human_sink_cells(cropland)
  flows <- allocate_manure_transport(source_cells, sink_cells)
  .human_finalise(flows, population_basis) |>
    .resolve_polity_validity(polity_validity)
}

#' @rdname build_human_n
#' @param ... For `build_urban_n()`, arguments passed on to `build_human_n()`.
#' @export
build_urban_n <- function(...) {
  cli::cli_warn(
    c(
      "{.fn build_urban_n} is deprecated; use {.fn build_human_n}.",
      i = "The term is nitrogen from the whole human population. Its
           output columns are renamed: {.field urban_n_t} is now
           {.field human_n_t}, and {.field method_urban*} is now
           {.field method_human*}.",
      i = "The default {.arg population_basis} is now {.val total}."
    ),
    class = c("whep_build_urban_n_deprecated", "lifecycle_warning_deprecated")
  )
  build_human_n(...)
}

# ---- Private helpers --------------------------------------------------

# Require an input frame's `area_code` to BE the numeric WHEP area code, and
# fix its type once, at the input boundary, before it is stringified into the
# transport allocator's `territory` key by .human_source_cells() /
# .human_sink_cells().
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
#    a bridge is meaningful there. build_human_n() manufactures the key
#    itself out of a column its own docs call `area_code`, so a bridge only
#    buys a chance of silently answering with a polity_area_code aggregation
#    bucket that is not the territory's own ("SSD" -> 206, Sudan (former)).
#    The column is refused instead: no bridge, no warn-and-continue, no
#    silent coercion of a label.
#
# The gridded pin build_cell_polity() emits is integer-keyed, so this is the
# identity on real input (asserted over the whole regions_full vocabulary in
# test_n_human.R) and published values do not move.
.human_resolve_area_code <- function(x, input) {
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
      class = .human_area_code_classes()
    )
  }
  .human_check_whole_codes(codes, arg)
  dplyr::mutate(x, area_code = as.integer(codes))
}

# The refusal's class, plus the name it carried before the term was renamed,
# so a handler written against the old name keeps working for one release.
.human_area_code_classes <- function() {
  c("whep_human_n_area_code_unresolved", "whep_urban_area_code_unresolved")
}

# A numeric `area_code` that is not a whole number is a real key error -- a
# share or a fraction landing in the code column -- and as.integer() would
# truncate it into a DIFFERENT territory's code rather than fail.
.human_check_whole_codes <- function(codes, arg) {
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
    class = .human_area_code_classes()
  )
}

.human_filter_years <- function(x, years) {
  if (is.null(years)) {
    return(x)
  }
  dplyr::filter(x, .data$year %in% years)
}

# The population slot a basis reads is `total_population` or
# `urban_population`. Supplying only the OTHER one is refused: falling back
# to the basis's own reader would silently discard the population the caller
# built, and reading it anyway would apply one basis's rate to the other's
# population -- the mismatch this argument exists to prevent.
.human_check_population_slot <- function(data, basis) {
  own <- .human_population_slot(basis)
  other <- .human_population_slot(setdiff(c("urban", "total"), basis))
  if (!is.null(data[[own]]) || is.null(data[[other]])) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.arg population_basis} {.val {basis}} reads {.field data${own}},
       but only {.field data${other}} was supplied.",
      i = "Select the basis that population is on, or supply
           {.field data${own}}: a per-capita rate is only meaningful against
           a population on its own basis."
    ),
    class = "whep_human_n_population_basis_mismatch"
  )
}

.human_population_slot <- function(basis) {
  if (basis == "urban") "urban_population" else "total_population"
}

# Population per polycell-year (`lon`, `lat`, `area_code`, `year`,
# `population`) on the selected basis.
#
# "urban": a per-CELL urban count, split across the polities holding the cell
# by `polity_frac` after joining the crosswalk. Simple one-polity crosswalks
# may omit polity_frac and retain the historical implicit value of 1.
# Population on a cell the crosswalk does not carry is dropped by that join.
#
# "total": already per POLYCELL, because build_total_population_grid() levels
# each country to its own WPP total. Re-splitting it by polity_frac would
# blend two countries' levels on every border cell, so it is taken as it is,
# and a polycell the crosswalk does not carry is refused rather than dropped:
# it means the population and the crosswalk are keyed in different
# vocabularies, and its nitrogen would strand on a territory with no room.
.human_polycell_population <- function(basis, data, polity, years) {
  if (basis == "total") {
    return(.human_total_population(data, polity, years))
  }
  if (!rlang::has_name(polity, "polity_frac")) {
    polity <- dplyr::mutate(polity, polity_frac = 1)
  }
  (data[["urban_population"]] %||%
    read_hyde_population(years = years, variable = "urban")) |>
    .human_filter_years(years) |>
    dplyr::inner_join(polity, by = c("lon", "lat")) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$year,
      population = .data$urban_pop * .data$polity_frac
    )
}

.human_total_population <- function(data, polity, years) {
  supplied <- data[["total_population"]] %||%
    build_total_population_grid(
      years = years,
      data = list(cell_polity = polity)
    )
  .check_columns(
    supplied,
    c("lon", "lat", "area_code", "year", "population"),
    "data$total_population"
  )
  population <- supplied |>
    .human_filter_years(years) |>
    .human_resolve_area_code("total_population") |>
    dplyr::transmute(
      lon = as.numeric(.data$lon),
      lat = as.numeric(.data$lat),
      .data$area_code,
      .data$year,
      .data$population
    )
  .human_check_polycells_known(population, polity)
  population
}

# Every polycell of a supplied total population must exist in the crosswalk.
.human_check_polycells_known <- function(population, polity) {
  # The crosswalk has no year dimension, so this membership test is
  # year-free by construction; it only decides whether to abort.
  orphan <- dplyr::anti_join(
    population,
    polity,
    by = c("lon", "lat", "area_code")
  )
  if (nrow(orphan) == 0L) {
    return(invisible(NULL))
  }
  codes <- utils::head(unique(orphan$area_code), 3)
  cli::cli_abort(
    c(
      "{nrow(orphan)} polycell-year{?s} of {.field data$total_population}
       ha{?s/ve} no row in {.field data$cell_polity}.",
      x = "{signif(sum(orphan$population), 6)} persons; area code{?s}
           include {.val {codes}}.",
      i = "Build it from the same crosswalk:
           {.code build_total_population_grid(data = list(cell_polity =
           <the same table>))}."
    ),
    class = .human_area_code_classes()
  )
}

# Human N generated per polycell-year: population x the per-capita rate on
# the same basis.
.human_n_generated <- function(population, basis) {
  rate <- .human_kgn_cap_series(unique(population$year), basis)
  population |>
    dplyr::inner_join(rate, by = "year") |>
    dplyr::mutate(
      human_n_generated_t = .data$population * .data$human_kgn_cap / 1000
    )
}

# The benchmark table on each basis: kg N per URBAN inhabitant (HYDE's urban
# count, and World Bank urban population for 2018-2022) or per TOTAL
# inhabitant (UN WPP total, from 1950). See data-raw/build_human_kgn_cap.R.
.human_kgn_cap_table <- function(basis) {
  if (basis == "urban") {
    return(whep::human_kgn_cap_reference)
  }
  whep::human_kgn_cap_total_reference
}

# Interpolate the per-capita human-N rate to the requested years:
# fill_linear between the basis's benchmark years, held constant (carried
# forward AND backward, since the series has no data before its first
# benchmark year; see data-raw/build_human_kgn_cap.R for why) outside the
# benchmark range. Under "total" the backward carry is unreachable from
# WHEP's own population, which build_total_population_grid() refuses before
# 1950, the table's first year.
.human_kgn_cap_series <- function(years, basis) {
  table <- .human_kgn_cap_table(basis) |>
    dplyr::select("year", "human_kgn_cap")
  all_years <- sort(unique(c(years, table$year)))
  tibble::tibble(year = all_years) |>
    dplyr::left_join(table, by = "year") |>
    fill_linear(
      human_kgn_cap,
      time_col = year,
      fill_forward = TRUE,
      fill_backward = TRUE
    ) |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::select("year", "human_kgn_cap")
}

# Every human-N-generating cell is a source: 100% of its human N is surplus
# needing placement (population and cropland-N-need do not coincide 1:1).
# No human carbon/VS stream is modelled, so surplus_c and surplus_vs are 0.
.human_source_cells <- function(generated) {
  generated |>
    dplyr::filter(.data$human_n_generated_t > 0) |>
    dplyr::transmute(
      year = .data$year,
      territory = as.character(.data$area_code),
      sub_territory = paste0(.data$lon, "_", .data$lat),
      surplus_n = .data$human_n_generated_t,
      surplus_c = 0,
      surplus_vs = 0
    )
}

# Every cell with cropland area is a possible sink: room_n is the simple
# EU-Nitrates fixed-ceiling proxy (170 kg N/ha, the same
# fixed_ceiling_kg_ha default as allocate_manure_to_land(), since Module C
# has no crop-N-demand table wired in yet).
.human_sink_cells <- function(cropland) {
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
# to the final schema and stamp method_human and the population basis.
.human_finalise <- function(flows, basis) {
  coords <- .parse_cell_id(flows$sub_territory)
  flows |>
    dplyr::mutate(
      lon = coords$lon,
      lat = coords$lat,
      # `territory` is the character key the transport allocator works in. It
      # is `as.character()` of the numeric area_code .human_resolve_area_code()
      # already produced at the input boundary, so recovering it is a plain
      # parse and cannot fold, bridge or fail (#597).
      area_code = as.integer(.data$territory)
    ) |>
    dplyr::summarise(
      human_n_t = sum(.data$applied_n),
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    dplyr::mutate(
      method_human = "spain_hist_rate|room_weighted",
      !!!.human_basis_stamps(basis)
    )
}

# The two provenance stamps a basis writes: which population the load was
# generated from, and the denominator of the per-capita rate applied to it.
# They always travel together, so a table cannot claim one without the other.
.human_basis_stamps <- function(basis) {
  if (basis == "urban") {
    return(list(
      method_human_population = "urban_population",
      method_human_kgn_cap = "kg_n_per_urban_inhabitant"
    ))
  }
  list(
    method_human_population = "total_population",
    method_human_kgn_cap = "kg_n_per_total_inhabitant"
  )
}

# ---- The former "urban" names, accepted for one release -------------------
#
# The term used to be keyed "urban" in build_n_inputs()'s `fert_type`, "Urban"
# in the loss cascade's Title-case vocabulary, and `method_urban_*` in the
# provenance columns. An `n_inputs` table or a driver table built before the
# rename still carries those keys, and a key the renamed code does not know is
# not an error it would raise: the pivot would give the old key its own column
# outside every sum, and the loss filter would skip it, so the nitrogen would
# leave the balance silently. They are translated instead, with a warning.

# `fert_type` with every former key replaced by its new one, warning once
# when any was found.
.human_legacy_fert_type <- function(fert_type) {
  renamed <- c(urban = "human", Urban = "Human")
  legacy <- fert_type %in% names(renamed)
  if (!any(legacy)) {
    return(fert_type)
  }
  found <- unique(fert_type[legacy])
  cli::cli_warn(
    c(
      "{.field fert_type} {.val {found}} is deprecated; the term is now
       {.val {unname(renamed[found])}}.",
      i = "It is read as the renamed term. Rebuild the table to stop this
           warning."
    ),
    class = c("whep_urban_fert_type_deprecated", "lifecycle_warning_deprecated")
  )
  fert_type[legacy] <- unname(renamed[fert_type[legacy]])
  fert_type
}

# An `n_inputs` table with the former `fert_type` and `method_urban_*` names
# renamed. A table that carries both the old and the new stamp is refused:
# which one describes the rows is not recoverable.
.human_upgrade_legacy_inputs <- function(n_inputs) {
  old <- c(
    method_human_population = "method_urban_population",
    method_human_kgn_cap = "method_urban_kgn_cap"
  )
  both <- old[names(old) %in% names(n_inputs) & old %in% names(n_inputs)]
  if (length(both) > 0L) {
    cli::cli_abort(c(
      "{.arg n_inputs} carries both {.field {both}} and
       {.field {names(both)}}.",
      i = "{.field {both}} is the former name of {.field {names(both)}};
           keep one."
    ))
  }
  if (rlang::has_name(n_inputs, "fert_type")) {
    n_inputs$fert_type <- .human_legacy_fert_type(n_inputs$fert_type)
  }
  dplyr::rename(n_inputs, dplyr::any_of(old))
}

# The driver tables build_nitrogen_balance() joins on the Title-case
# `fert_type`, with the former key renamed.
.human_upgrade_legacy_drivers <- function(data) {
  slots <- intersect(
    c("n_balance_drivers", "n_balance_leaching_drivers"),
    names(data)
  )
  data[slots] <- purrr::map(data[slots], .human_upgrade_fert_column)
  data
}

.human_upgrade_fert_column <- function(table) {
  if (is.null(table) || !rlang::has_name(table, "fert_type")) {
    return(table)
  }
  table$fert_type <- .human_legacy_fert_type(table$fert_type)
  table
}

# Toy fixture for a runnable example (one cell, one polity, one year).
.example_human_n <- function(basis = "total") {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~human_n_t, ~method_human,
    -0.25, -0.25, 203L, 2020L, 4.5, "spain_hist_rate|room_weighted"
  ) |>
    dplyr::mutate(!!!.human_basis_stamps(basis)) |>
    .add_reporting_polity_columns()
}
