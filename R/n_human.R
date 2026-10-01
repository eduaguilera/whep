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
# .manure_subnational() (R/build_livestock_nutrient_flows.R). What that
# transport step cannot deliver and hands back to a source cell with no
# cropland is placed by `method_residual` (whep#1171); see
# .human_place_undelivered(). What it hands back to a source cell WITH
# cropland is capped at that cell's own room by `method_local_residual`, and
# the excess joins the undelivered N (whep#1336); see .human_route_residual().

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
#' @param method_residual What happens to human N that the transport step
#'   cannot deliver: the residual on a source cell with **no cropland**, plus,
#'   under the default `method_local_residual = "room_cap"`, the part of a
#'   cropland source cell's residual that exceeds its own room. On the 2010
#'   global grid, under the default `"total"` basis, the first is 13,198 cells
#'   and 47,365 t N, 0.700% of the 6.77 Mt of human N, and the second adds
#'   24,346 t on 1,424 cells; see `method_local_residual` for what `"nearest"`
#'   leaves stranded.
#'   * `"nearest"` (default): run the transport step's own rule again with a
#'     growing radius. Each such cell offers its nitrogen to the same-polity
#'     cropland cells in its nearest ring (Chebyshev distance in 0.5-degree
#'     steps, wrapped at the antimeridian) that still has room, in proportion
#'     to that room; a cell's room is its 170 kg N/ha ceiling minus the human
#'     N already on it, and an over-subscribed cell is filled only to its
#'     room. The radius grows one ring at a time until the nitrogen is placed
#'     or the polity has no room left. Conserves mass and keeps the nitrogen
#'     as close to the people who produced it as the room allows. No distance
#'     cap or transport coefficient is applied.
#'   * `"polity"`: pool it per polity-year and spread it over all of that
#'     polity-year's cropland in proportion to cropland room (area), the rule
#'     [build_n_inputs()] applies under `method_unsupported = "reallocate"`.
#'     Conserves mass, but places the nitrogen anywhere in the polity.
#'   * `"keep"`: leave it on its source cell, as before this argument
#'     existed, flagged in `human_n_stranded_t`. On a cell with no cropland,
#'     [build_n_inputs()]'s `method_unsupported` then decides its fate (by
#'     default, it aborts); an over-room excess is applied on its own cell's
#'     cropland, as `"uncapped"` would.
#'   * `"drop"`: discard it. Loses the mass, biased towards dense,
#'     cropland-free cells.
#'
#'   `"nearest"` and `"polity"` never cross a polity, like the transport step
#'   itself, so a polity-year with population and no cropland anywhere keeps
#'   its nitrogen on the source cell under either, flagged in
#'   `human_n_stranded_t`; `"nearest"` does the same with whatever its polity
#'   has no room left for. Whenever any cell is undelivered, the count, the
#'   tonnes and the share of human N are reported (a warning, class
#'   `whep_human_n_undelivered`, when any nitrogen is dropped or left
#'   stranded; otherwise a message of the same class), and the per-year
#'   figures are attached as `attr(x, "human_n_undelivered")`. Recorded in
#'   the `method_human_residual` output column.
#' @param method_local_residual What happens to the residual the transport
#'   step hands back to a source cell that **has** cropland. The transport step
#'   offers a cell's load to its ring-1 neighbours only, so a dense cell with
#'   little cropland gets back most of its own load, on that little cropland.
#'   * `"room_cap"` (default): the cell keeps only what fits in its own room,
#'     170 kg N/ha times its cropland minus the human N the transport step
#'     already landed on it -- the room the transport step and `"nearest"`
#'     respect everywhere else. The excess is undelivered N, placed by
#'     `method_residual` like the residual of a cell with no cropland. On the
#'     2010 global grid (`"total"` basis) that is 24,346 t on 1,424 of the
#'     2,180 cropland source cells left with a residual. Under `"nearest"`,
#'     17,083 t of it is moved and 7,263 t stays stranded on its own cell, in
#'     the three polities with no room left anywhere (Hong Kong 6,701 t,
#'     Kuwait 363 t, the Bahamas 200 t), alongside the 2,145 t on cells with
#'     no cropland (Qatar, Iceland, Samoa).
#'   * `"uncapped"`: the cell keeps its whole residual, as before this
#'     argument existed, whatever its cropland area. On the same grid 1,417
#'     cropland cells then end above 170 kg N/ha, holding 24,346 t above it,
#'     and the largest load is booked on 3.6e-7 ha.
#'
#'   No minimum-cropland threshold is offered: the room cap already moves a
#'   sliver's whole residual, and a threshold would be a new, unsourced number
#'   that misses the excess on larger cells (15,202 t of the 24,346 t sits on
#'   cells with at least 1 ha). Recorded in the `method_human_local_residual`
#'   output column; the part of `undelivered_t` it adds is the summary's
#'   `over_room_t`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year`, `human_n_t`,
#'   `human_n_relocated_t` (the part of `human_n_t` placed on the cell by
#'   `method_residual`), `human_n_stranded_t` (the undelivered part no rule
#'   could place: on a cell with no cropland, which no downstream cropland
#'   allocation can place, or above the room of the cell's own cropland, which
#'   [build_n_inputs()] then applies there, over the ceiling),
#'   `method_human`, `method_human_population` (`"total_population"` or
#'   `"urban_population"`), `method_human_kgn_cap`
#'   (`"kg_n_per_total_inhabitant"` or `"kg_n_per_urban_inhabitant"`) and
#'   `method_human_residual` and `method_human_local_residual`, plus the
#'   polity columns below, plus
#'   `reporting_polity_out_of_span` when `polity_validity = "flag"`. The
#'   attribute `"human_n_undelivered"` is a tibble with one row per year:
#'   `year`, `human_n_t`, `n_cells` (undelivered source cells),
#'   `undelivered_t`, `over_room_t` (the part of `undelivered_t` that exceeded
#'   the room of its own cell's cropland), `relocated_t`, `stranded_t`,
#'   `dropped_t`, `undelivered_share` (of `human_n_t`),
#'   `method_human_residual` and `method_human_local_residual`.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_human_n(example = TRUE)
build_human_n <- function(
  years = NULL,
  population_basis = c("total", "urban"),
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE,
  method_residual = c("nearest", "polity", "keep", "drop"),
  method_local_residual = c("room_cap", "uncapped")
) {
  population_basis <- rlang::arg_match(population_basis)
  polity_validity <- rlang::arg_match(polity_validity)
  methods <- list(
    residual = rlang::arg_match(method_residual),
    local_residual = rlang::arg_match(method_local_residual)
  )
  if (isTRUE(example)) {
    return(.resolve_polity_validity(
      .example_human_n(population_basis, methods),
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
  flows <- allocate_manure_transport(source_cells, sink_cells) |>
    .human_route_residual(sink_cells, methods$local_residual)
  placed <- .human_place_undelivered(flows, sink_cells, methods$residual)
  out <- .human_finalise(placed, population_basis, methods) |>
    .resolve_polity_validity(polity_validity)
  attr(out, "human_n_undelivered") <- .human_undelivered_summary(
    flows,
    placed,
    methods
  )
  out
}

#' @rdname build_human_n
#' @param ... For `build_urban_n()`, arguments passed on to `build_human_n()`.
#'   `population_basis` defaults to `"urban"` here if omitted, matching this
#'   function's historical behaviour, unlike `build_human_n()`'s own
#'   `"total"` default.
#' @export
build_urban_n <- function(...) {
  cli::cli_warn(
    c(
      "{.fn build_urban_n} is deprecated; use {.fn build_human_n}.",
      i = "The term is nitrogen from the whole human population. Its
           output columns are renamed: {.field urban_n_t} is now
           {.field human_n_t}, and {.field method_urban*} is now
           {.field method_human*}.",
      i = "{.fn build_human_n}'s own default {.arg population_basis} is
           {.val total}; this alias keeps {.val urban}, its historical
           behaviour, unless you pass {.arg population_basis} explicitly."
    ),
    class = c("whep_build_urban_n_deprecated", "lifecycle_warning_deprecated")
  )
  args <- list(...)
  if (!"population_basis" %in% names(args)) {
    args$population_basis <- "urban"
  }
  rlang::exec(build_human_n, !!!args)
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
  # `population` is still the split (mass-conserving) headcount consumers of
  # this table rely on. `urban_pop` and `polity_frac` also travel unmultiplied
  # alongside it, so .human_n_generated() can apply the rate in main's
  # original grouping -- urban_pop * rate * polity_frac, not
  # (urban_pop * polity_frac) * rate -- and reproduce main's floating-point
  # result bit for bit rather than a mathematically equal but
  # differently-rounded one.
  (data[["urban_population"]] %||%
    read_hyde_population(years = years, variable = "urban")) |>
    .human_filter_years(years) |>
    dplyr::inner_join(polity, by = c("lon", "lat")) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$year,
      population = .data$urban_pop * .data$polity_frac,
      urban_pop = .data$urban_pop,
      polity_frac = .data$polity_frac
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
# the same basis. The urban basis reproduces main's exact multiplication
# grouping, urban_pop * rate * polity_frac / 1000 -- the unsplit count times
# the rate times this row's own polity_frac -- not (population * rate), which
# would multiply the ALREADY-SPLIT `population` (urban_pop * polity_frac) by
# the rate: mathematically the same product, but grouped differently, so it
# rounds to a different last bit. The total basis has no main equivalent to
# match and never involves polity_frac at all (.human_total_population()
# takes the polycell total as it is, see its comment), so it keeps the
# simpler population * rate / 1000.
.human_n_generated <- function(population, basis) {
  rate <- .human_kgn_cap_series(unique(population$year), basis)
  joined <- dplyr::inner_join(population, rate, by = "year")
  if (basis == "urban") {
    return(dplyr::mutate(
      joined,
      human_n_generated_t = .data$urban_pop *
        .data$human_kgn_cap *
        .data$polity_frac /
        1000
    ))
  }
  dplyr::mutate(
    joined,
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

# ---- Undelivered human N (whep#1171) ------------------------------------
#
# allocate_manure_transport() hands back, at the SOURCE cell, whatever a source
# could not send to its ring neighbours. On a source cell that has cropland the
# residual is simply applied there. On a source cell with NO cropland there is
# nothing to apply it to: build_n_inputs() spreads non-item nitrogen over its
# own cell's cropland, so such a row joins nothing and the balance aborts. On
# the 2010 global grid (default "total" basis) that is 13,198 cells and
# 47,365 t N, 0.700% of the 6.77 Mt of human N (urban basis: 1,985 cells,
# 38,425 t, 0.955%). Until #1171 the output carried it with nothing to tell
# it apart from nitrogen that had been placed.

# Tag every transport row with where it ended up: "transported" (delivered to a
# neighbour), "residual_local" (handed back to a source cell that has cropland,
# and fits there) or "undelivered" (handed back to a source cell with no
# cropland, or, under "room_cap", the part of a cropland cell's residual beyond
# its own room). `over_room` marks the second kind of undelivered row.
.human_route_residual <- function(flows, sink_cells, method_local) {
  has_cropland <- sink_cells |>
    dplyr::distinct(.data$year, .data$territory, .data$sub_territory) |>
    dplyr::mutate(.has_cropland = TRUE)
  routed <- flows |>
    dplyr::left_join(
      has_cropland,
      by = c("year", "territory", "sub_territory")
    ) |>
    dplyr::mutate(
      route = dplyr::case_when(
        .data$kind == "transported" ~ "transported",
        dplyr::coalesce(.data$.has_cropland, FALSE) ~ "residual_local",
        .default = "undelivered"
      ),
      over_room = FALSE
    ) |>
    dplyr::select(-".has_cropland")
  if (method_local == "uncapped") {
    return(routed)
  }
  .human_cap_local_residual(routed, sink_cells)
}

# "room_cap": a cropland source cell keeps the part of its residual that fits
# in the room the transport step left it -- room_n minus the N transported onto
# it, the same room "nearest" sees -- and the rest becomes an "undelivered"
# row on the same cell. The transport step never fills a sink past room_n, so
# the room left is never negative but for rounding, which pmax() absorbs. An
# excess under 1e-12 of the residual is that rounding, not nitrogen, and is
# left where it is.
.human_cap_local_residual <- function(routed, sink_cells) {
  keys <- c("year", "territory", "sub_territory")
  inflow <- routed |>
    dplyr::filter(.data$route == "transported") |>
    dplyr::summarise(inflow_n = sum(.data$applied_n), .by = dplyr::all_of(keys))
  room <- dplyr::summarise(
    sink_cells,
    room_n = sum(.data$room_n),
    .by = dplyr::all_of(keys)
  )
  local <- routed |>
    dplyr::filter(.data$route == "residual_local") |>
    dplyr::inner_join(room, by = keys) |>
    dplyr::left_join(inflow, by = keys) |>
    dplyr::mutate(
      room_left = pmax(.data$room_n - dplyr::coalesce(.data$inflow_n, 0), 0),
      excess_n = pmax(.data$applied_n - .data$room_left, 0),
      excess_n = dplyr::if_else(
        .data$excess_n > .human_live_tol() * .data$applied_n,
        .data$excess_n,
        0
      )
    )
  excess <- local |>
    dplyr::filter(.data$excess_n > 0) |>
    dplyr::mutate(
      applied_n = .data$excess_n,
      route = "undelivered",
      over_room = TRUE
    )
  local <- dplyr::mutate(local, applied_n = .data$applied_n - .data$excess_n)
  dplyr::bind_rows(
    dplyr::filter(routed, .data$route != "residual_local"),
    dplyr::filter(local, .data$applied_n > 0),
    excess
  ) |>
    dplyr::select(dplyr::all_of(names(routed)))
}

# Apply `method_residual` to the "undelivered" rows. Every rule except "drop"
# conserves mass. "nearest" and "polity" move nitrogen only inside its own
# polity-year, as the transport step does, so an undelivered row in a
# polity-year with no cropland anywhere stays where it is, re-tagged
# "stranded" -- 70 cells and 2,145 t N at 2010 (total basis; Qatar, Iceland
# and Samoa). "nearest" also strands what a polity has no room left for; an
# over-room excess stranded that way stays on its own cropland cell (#1336).
.human_place_undelivered <- function(flows, sink_cells, method) {
  undelivered <- dplyr::filter(flows, .data$route == "undelivered")
  kept <- dplyr::filter(flows, .data$route != "undelivered")
  if (nrow(undelivered) == 0L || method == "drop") {
    return(kept)
  }
  if (method == "keep") {
    return(dplyr::bind_rows(kept, .human_mark_stranded(undelivered)))
  }
  by <- c("year", "territory")
  reachable <- dplyr::semi_join(undelivered, sink_cells, by = by)
  stranded <- dplyr::anti_join(undelivered, sink_cells, by = by)
  relocated <- if (method == "nearest") {
    .human_relocate_nearest(reachable, sink_cells, kept)
  } else {
    .human_relocate_polity(reachable, sink_cells)
  }
  dplyr::bind_rows(kept, relocated, .human_mark_stranded(stranded))
}

.human_mark_stranded <- function(rows) {
  dplyr::mutate(rows, route = "stranded")
}

# "nearest": the transport step's own rule, widened. Each undelivered cell
# offers its nitrogen to the same-polity cropland cells in its nearest ring
# that still has room, in proportion to that room, and an over-subscribed sink
# is filled only to its room -- exactly allocate_manure_transport()'s rule,
# with the ring radius growing one step at a time instead of stopping at 1.
# A sink's room is its room_n minus the human N already on it (transported
# there, or its own residual), so a sink the ring-1 pass filled takes nothing
# more. No distance cap and no transport coefficient is introduced: the
# radius grows until the nitrogen is placed or the polity has no room left.
# What is left then stays on its source cell, "stranded".
#
# Why room matters: without it, the whole load of a city lands on the one
# nearest cropland cell. On the 2010 urban basis that put 3,459 t N on 1,219 ha
# next to Jeddah (3,985 kg N/ha) and lifted the 99th percentile of human N per
# cropland hectare from 182 to 4,141 kg N/ha (measured in whep#1224).
.human_relocate_nearest <- function(undelivered, sink_cells, kept) {
  sinks <- .human_sink_room(sink_cells, kept)
  pairs <- .human_nearest_pairs(undelivered, sinks)
  remaining <- undelivered |>
    dplyr::transmute(
      .source = dplyr::row_number(),
      rem_n = .data$applied_n,
      rem_n0 = .data$applied_n
    )
  room <- dplyr::transmute(
    sinks,
    .data$.sink,
    .data$room_left,
    room_left0 = .data$room_left
  )
  # Seeded with a zero-row frame so a polity with cropland but no room left
  # anywhere, where no pass runs, relocates nothing instead of failing.
  placed <- list(tibble::tibble(.sink = integer(), applied_n = numeric()))
  # Sequential by construction: each ring sees the room the previous one left.
  # Every pass either places a source's whole load or fills every sink in its
  # nearest ring with room, so the loop ends within the number of rings.
  while (nrow(pairs) > 0L) {
    step <- .human_ring_step(pairs, remaining, room)
    placed[[length(placed) + 1L]] <- step$flows
    remaining <- step$remaining
    room <- step$room
    pairs <- .human_prune_pairs(pairs, remaining, room)
  }
  relocated <- dplyr::bind_rows(placed) |>
    dplyr::inner_join(dplyr::select(sinks, -"room_left"), by = ".sink") |>
    .human_relocated_rows()
  left <- undelivered |>
    dplyr::mutate(.source = dplyr::row_number()) |>
    dplyr::inner_join(remaining, by = ".source") |>
    dplyr::filter(.data$rem_n > .human_live_tol() * .data$rem_n0) |>
    dplyr::mutate(applied_n = .data$rem_n) |>
    dplyr::select(-".source", -"rem_n", -"rem_n0")
  dplyr::bind_rows(relocated, .human_mark_stranded(left))
}

# Room left on each cropland cell once the transport step has run.
.human_sink_room <- function(sink_cells, kept) {
  landed <- dplyr::summarise(
    kept,
    landed_n = sum(.data$applied_n),
    .by = c("year", "territory", "sub_territory")
  )
  xy <- .parse_cell_id(sink_cells$sub_territory)
  sink_cells |>
    dplyr::mutate(lon = xy$lon, lat = xy$lat) |>
    dplyr::left_join(landed, by = c("year", "territory", "sub_territory")) |>
    dplyr::transmute(
      .sink = dplyr::row_number(),
      year = .data$year,
      territory = .data$territory,
      sub_territory = .data$sub_territory,
      lon = .data$lon,
      lat = .data$lat,
      room_left = pmax(.data$room_n - dplyr::coalesce(.data$landed_n, 0), 0)
    )
}

# Every (undelivered source, same-polity sink with room) pair, with its ring.
.human_nearest_pairs <- function(undelivered, sinks) {
  xy <- .parse_cell_id(undelivered$sub_territory)
  undelivered |>
    dplyr::transmute(
      .source = dplyr::row_number(),
      year = .data$year,
      territory = .data$territory,
      slon = xy$lon,
      slat = xy$lat
    ) |>
    dplyr::inner_join(
      dplyr::filter(sinks, .data$room_left > 0),
      by = c("year", "territory"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      .data$.source,
      .data$.sink,
      ring = .human_ring_distance(.data$slon, .data$slat, .data$lon, .data$lat)
    )
}

# One pass of the transport rule: each source's nearest ring that still has
# room, room-weighted offers, over-subscribed sinks scaled to their room.
.human_ring_step <- function(pairs, remaining, room) {
  flows <- pairs |>
    dplyr::filter(.data$ring == min(.data$ring), .by = ".source") |>
    dplyr::inner_join(
      dplyr::select(remaining, ".source", "rem_n"),
      by = ".source"
    ) |>
    dplyr::inner_join(
      dplyr::select(room, ".sink", "room_left"),
      by = ".sink"
    ) |>
    dplyr::mutate(
      offer = .data$rem_n * .data$room_left / sum(.data$room_left),
      .by = ".source"
    ) |>
    dplyr::mutate(
      applied_n = .data$offer *
        pmin(1, .data$room_left / sum(.data$offer)),
      .by = ".sink"
    )
  sent <- dplyr::summarise(flows, sent = sum(.data$applied_n), .by = ".source")
  took <- dplyr::summarise(flows, took = sum(.data$applied_n), .by = ".sink")
  list(
    flows = dplyr::select(flows, ".sink", "applied_n"),
    remaining = .human_subtract(remaining, sent, ".source", "rem_n", "sent"),
    room = .human_subtract(room, took, ".sink", "room_left", "took")
  )
}

.human_subtract <- function(x, delta, key, value, by_value) {
  x |>
    dplyr::left_join(delta, by = key) |>
    dplyr::mutate(
      !!value := pmax(
        .data[[value]] - dplyr::coalesce(.data[[by_value]], 0),
        0
      )
    ) |>
    dplyr::select(-dplyr::all_of(by_value))
}

# Drop the pairs whose source is placed or whose sink is full. "Placed" and
# "full" are relative to the starting amount, so the rounding residue of the
# room scaling (~1e-16 of the load) cannot keep the loop alive.
.human_prune_pairs <- function(pairs, remaining, room) {
  tol <- .human_live_tol()
  live_sources <- remaining$.source[remaining$rem_n > tol * remaining$rem_n0]
  live_sinks <- room$.sink[room$room_left > tol * room$room_left0]
  dplyr::filter(
    pairs,
    .data$.source %in% live_sources,
    .data$.sink %in% live_sinks
  )
}

.human_live_tol <- function() {
  1e-12
}

# Chebyshev distance in 0.5-degree grid steps, with longitude wrapped across
# the antimeridian: Chukotka's cells at 179.75 and -179.75 are one step apart,
# not 719.
.human_ring_distance <- function(lon1, lat1, lon2, lat2) {
  dlon <- abs(lon1 - lon2) %% 360
  dlon <- pmin(dlon, 360 - dlon)
  round(pmax(dlon, abs(lat1 - lat2)) / 0.5)
}

# "polity": the undelivered nitrogen of each polity-year is pooled and spread
# over all of that polity-year's cropland cells by room_n, i.e. by cropland
# area -- the rule build_n_inputs(method_unsupported = "reallocate") applies to
# the same rows further down the chain.
.human_relocate_polity <- function(undelivered, sink_cells) {
  sinks <- dplyr::select(
    sink_cells,
    "year",
    "territory",
    "sub_territory",
    "room_n"
  )
  undelivered |>
    dplyr::summarise(
      applied_n = sum(.data$applied_n),
      .by = c("year", "territory")
    ) |>
    dplyr::inner_join(sinks, by = c("year", "territory")) |>
    dplyr::mutate(
      applied_n = .data$applied_n * .data$room_n / sum(.data$room_n),
      .by = c("year", "territory")
    ) |>
    .human_relocated_rows()
}

.human_relocated_rows <- function(x) {
  x |>
    dplyr::summarise(
      applied_n = sum(.data$applied_n),
      .by = c("year", "territory", "sub_territory")
    ) |>
    dplyr::mutate(route = "relocated")
}

# One row per year: how much human N the transport step could not deliver to
# any cropland, and what `method_residual` did with it. Attached to the output
# as the "human_n_undelivered" attribute, and reported.
.human_undelivered_summary <- function(flows, placed, methods) {
  totals <- dplyr::summarise(
    flows,
    human_n_t = sum(.data$applied_n),
    n_cells = sum(.data$route == "undelivered"),
    undelivered_t = sum(.data$applied_n[.data$route == "undelivered"]),
    over_room_t = sum(
      .data$applied_n[.data$route == "undelivered" & .data$over_room]
    ),
    .by = "year"
  )
  outcome <- dplyr::summarise(
    placed,
    relocated_t = sum(.data$applied_n[.data$route == "relocated"]),
    stranded_t = sum(.data$applied_n[.data$route == "stranded"]),
    .by = "year"
  )
  summary <- totals |>
    dplyr::left_join(outcome, by = "year") |>
    dplyr::mutate(
      relocated_t = dplyr::coalesce(.data$relocated_t, 0),
      stranded_t = dplyr::coalesce(.data$stranded_t, 0),
      dropped_t = pmax(
        .data$undelivered_t - .data$relocated_t - .data$stranded_t,
        0
      ),
      undelivered_share = .data$undelivered_t / .data$human_n_t,
      method_human_residual = methods$residual,
      method_human_local_residual = methods$local_residual
    ) |>
    dplyr::arrange(.data$year)
  .human_report_undelivered(summary, methods$residual)
  summary
}

# A message when every undelivered tonne was relocated; a warning when any of
# it was dropped or is left stranded: on a cell with no cropland the nitrogen
# balance cannot place it (build_n_inputs()'s `method_unsupported` decides
# what happens next), and on a cell with cropland it lands above the ceiling.
.human_report_undelivered <- function(summary, method) {
  n_cells <- sum(summary$n_cells)
  if (n_cells == 0L) {
    return(invisible(NULL))
  }
  undelivered <- signif(sum(summary$undelivered_t), 6)
  share <- signif(100 * undelivered / sum(summary$human_n_t), 3)
  over_room <- signif(sum(summary$over_room_t), 6)
  msg <- c(
    "{cli::qty(n_cells)}{n_cells} human-N source cell-year{?s} could not
     place {undelivered} t N on cropland room ({share}% of human N).",
    i = "{.arg method_residual} = {.val {method}}: relocated
         {signif(sum(summary$relocated_t), 6)} t, left stranded on its own
         cell {signif(sum(summary$stranded_t), 6)} t, dropped
         {signif(sum(summary$dropped_t), 6)} t.",
    i = "Per-year totals: {.code attr(x, \"human_n_undelivered\")}."
  )
  if (over_room > 0) {
    msg <- append(
      msg,
      c(
        i = "{over_room} t of it is the excess of a cell's own residual over
             the room of its cropland; the rest sits on cells with no
             cropland."
      ),
      after = 1L
    )
  }
  if (sum(summary$dropped_t) + sum(summary$stranded_t) > 0) {
    cli::cli_warn(msg, class = "whep_human_n_undelivered")
  } else {
    cli::cli_inform(msg, class = "whep_human_n_undelivered")
  }
  invisible(NULL)
}


# Parse sub_territory back to lon/lat, aggregate transported, residual and
# relocated flows to the final schema, and stamp the methods. The two
# component columns say how much of a cell's `human_n_t` reached it through
# the residual rule (`human_n_relocated_t`) and how much is undelivered N no
# rule could place (`human_n_stranded_t`): on a cell with no cropland, or above
# the room of the cell's own cropland.
.human_finalise <- function(flows, basis, methods) {
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
      human_n_relocated_t = sum(.data$applied_n[.data$route == "relocated"]),
      human_n_stranded_t = sum(.data$applied_n[.data$route == "stranded"]),
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    dplyr::mutate(
      method_human = "calibration_rate|room_weighted",
      !!!.human_basis_stamps(basis),
      method_human_residual = methods$residual,
      method_human_local_residual = methods$local_residual
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
  n_inputs |>
    dplyr::rename(dplyr::any_of(old)) |>
    .human_fill_legacy_stamps()
}

# main never recorded a population-basis stamp at all -- it had only one
# basis, so `method_human_population` / `method_human_kgn_cap` (and their
# former `method_urban_*` spelling) do not exist in a table it built. Add the
# two columns when the table lacks them entirely, and fill them on every
# "human" (formerly "urban") row that is still unstamped -- missing column or
# NA -- with `urban_population` / `kg_n_per_urban_inhabitant`, the only basis
# main ever produced. A row that already carries a stamp (a table built after
# this branch) is left untouched.
.human_fill_legacy_stamps <- function(n_inputs) {
  if (!rlang::has_name(n_inputs, "fert_type")) {
    return(n_inputs)
  }
  if (!rlang::has_name(n_inputs, "method_human_population")) {
    n_inputs$method_human_population <- NA_character_
  }
  if (!rlang::has_name(n_inputs, "method_human_kgn_cap")) {
    n_inputs$method_human_kgn_cap <- NA_character_
  }
  human <- !is.na(n_inputs$fert_type) & n_inputs$fert_type == "human"
  dplyr::mutate(
    n_inputs,
    method_human_population = dplyr::if_else(
      human & is.na(.data$method_human_population),
      "urban_population",
      .data$method_human_population
    ),
    method_human_kgn_cap = dplyr::if_else(
      human & is.na(.data$method_human_kgn_cap),
      "kg_n_per_urban_inhabitant",
      .data$method_human_kgn_cap
    )
  )
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
.example_human_n <- function(
  basis = "total",
  methods = list(residual = "nearest", local_residual = "room_cap")
) {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~human_n_t, ~method_human,
    -0.25, -0.25, 203L, 2020L, 4.5, "calibration_rate|room_weighted"
  ) |>
    dplyr::mutate(
      human_n_relocated_t = 0,
      human_n_stranded_t = 0,
      .after = "human_n_t"
    ) |>
    dplyr::mutate(
      !!!.human_basis_stamps(basis),
      method_human_residual = methods$residual,
      method_human_local_residual = methods$local_residual
    ) |>
    .add_reporting_polity_columns()
}
