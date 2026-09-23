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
#' @param method_residual What happens to urban N that the transport step
#'   cannot deliver and that sits on a source cell with **no cropland** (the
#'   residual on a source cell that has cropland is applied there under every
#'   method). On the 2010 global grid this is 1,985 cells and 38,425 t N,
#'   0.955% of the 4.02 Mt of urban N, most of it in Russia, Algeria, the
#'   United States, Saudi Arabia and Australia.
#'   * `"nearest"` (default): move it to the same-polity cropland cell(s) at
#'     the smallest grid distance (Chebyshev, in 0.5-degree steps -- the
#'     transport step's own ring metric, widened until a ring holds
#'     cropland), split between tied cells by cropland room. Conserves mass
#'     and keeps the nitrogen as close to the people who produced it as the
#'     grid allows. No distance cap is applied.
#'   * `"polity"`: pool it per polity-year and spread it over all of that
#'     polity-year's cropland in proportion to cropland room (area), the rule
#'     [build_n_inputs()] applies under `method_unsupported = "reallocate"`.
#'     Conserves mass, but places the nitrogen anywhere in the polity.
#'   * `"keep"`: leave it on its source cell, as before this argument
#'     existed, flagged in `urban_n_stranded_t`. [build_n_inputs()]'s
#'     `method_unsupported` then decides its fate (by default, it aborts).
#'   * `"drop"`: discard it. Loses the mass, biased towards dense,
#'     cropland-free cells.
#'
#'   `"nearest"` and `"polity"` never cross a polity, like the transport step
#'   itself, so a polity-year with population and no cropland anywhere keeps
#'   its nitrogen on the source cell under either (51 cells, 834 t N at
#'   2010), flagged in `urban_n_stranded_t`. Whenever any cell is
#'   undelivered, the count, the tonnes and the share of urban N are reported
#'   (a warning, class `whep_urban_n_undelivered`, when any nitrogen is
#'   dropped or left stranded; otherwise a message of the same class), and the
#'   per-year figures are attached as `attr(x, "urban_n_undelivered")`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year`, `urban_n_t`,
#'   `urban_n_relocated_t` (the part of `urban_n_t` placed on the cell by
#'   `method_residual`), `urban_n_stranded_t` (the part sitting on a cell with
#'   no cropland, which no downstream cropland allocation can place),
#'   `method_urban` and `method_urban_residual`, plus the polity columns below,
#'   plus `reporting_polity_out_of_span` when `polity_validity = "flag"`. The
#'   attribute `"urban_n_undelivered"` is a tibble with one row per year:
#'   `year`, `urban_n_t`, `n_cells` (undelivered source cells), `undelivered_t`,
#'   `relocated_t`, `stranded_t`, `dropped_t`, `undelivered_share` (of
#'   `urban_n_t`) and `method_urban_residual`.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_urban_n(example = TRUE)
build_urban_n <- function(
  years = NULL,
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE,
  method_residual = c("nearest", "polity", "keep", "drop")
) {
  polity_validity <- rlang::arg_match(polity_validity)
  method_residual <- rlang::arg_match(method_residual)
  if (isTRUE(example)) {
    return(
      .example_urban_n() |>
        dplyr::mutate(method_urban_residual = method_residual) |>
        .resolve_polity_validity(polity_validity)
    )
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
  flows <- allocate_manure_transport(source_cells, sink_cells) |>
    .urban_route_residual(sink_cells)
  placed <- .urban_place_undelivered(flows, sink_cells, method_residual)
  out <- .urban_finalise(placed, method_residual) |>
    .resolve_polity_validity(polity_validity)
  attr(out, "urban_n_undelivered") <- .urban_undelivered_summary(
    flows,
    placed,
    method_residual
  )
  out
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

# ---- Undelivered urban N (whep#1171) ------------------------------------
#
# allocate_manure_transport() hands back, at the SOURCE cell, whatever a source
# could not send to its ring neighbours. On a source cell that has cropland the
# residual is simply applied there. On a source cell with NO cropland there is
# nothing to apply it to: build_n_inputs() spreads non-item nitrogen over its
# own cell's cropland, so such a row joins nothing and the balance aborts. On
# the 2010 global grid that is 1,985 cells and 38,425 t N, 0.955% of the
# 4.02 Mt of urban N (Russia alone 1,056 cells and 7.0 kt). Until #1171 the
# output carried it with nothing to tell it apart from nitrogen that had been
# placed.

# Tag every transport row with where it ended up: "transported" (delivered to a
# neighbour), "residual_local" (handed back to a source cell that has cropland)
# or "undelivered" (handed back to a source cell with no cropland).
.urban_route_residual <- function(flows, sink_cells) {
  has_cropland <- sink_cells |>
    dplyr::distinct(.data$year, .data$territory, .data$sub_territory) |>
    dplyr::mutate(.has_cropland = TRUE)
  flows |>
    dplyr::left_join(
      has_cropland,
      by = c("year", "territory", "sub_territory")
    ) |>
    dplyr::mutate(
      route = dplyr::case_when(
        .data$kind == "transported" ~ "transported",
        dplyr::coalesce(.data$.has_cropland, FALSE) ~ "residual_local",
        .default = "undelivered"
      )
    ) |>
    dplyr::select(-".has_cropland")
}

# Apply `method_residual` to the "undelivered" rows. Every rule except "drop"
# conserves mass. "nearest" and "polity" move nitrogen only inside its own
# polity-year, as the transport step does, so an undelivered row in a
# polity-year with no cropland anywhere stays where it is, re-tagged
# "stranded" -- 51 cells and 834 t N at 2010.
.urban_place_undelivered <- function(flows, sink_cells, method) {
  undelivered <- dplyr::filter(flows, .data$route == "undelivered")
  kept <- dplyr::filter(flows, .data$route != "undelivered")
  if (nrow(undelivered) == 0L || method == "drop") {
    return(kept)
  }
  if (method == "keep") {
    return(dplyr::bind_rows(kept, .urban_mark_stranded(undelivered)))
  }
  by <- c("year", "territory")
  reachable <- dplyr::semi_join(undelivered, sink_cells, by = by)
  stranded <- dplyr::anti_join(undelivered, sink_cells, by = by)
  relocated <- if (method == "nearest") {
    .urban_relocate_nearest(reachable, sink_cells)
  } else {
    .urban_relocate_polity(reachable, sink_cells)
  }
  dplyr::bind_rows(kept, relocated, .urban_mark_stranded(stranded))
}

.urban_mark_stranded <- function(rows) {
  dplyr::mutate(rows, route = "stranded")
}

# "nearest": each undelivered cell's nitrogen goes to the same-polity cropland
# cell(s) at the smallest Chebyshev distance on the grid. That is the transport
# step's own ring metric, widened until a ring holds cropland, so no distance
# cap or transport coefficient is introduced. Ties are split by room_n, the
# weight the transport step itself uses. The metric counts grid steps, not km:
# a step of longitude shortens towards the poles.
.urban_relocate_nearest <- function(undelivered, sink_cells) {
  src_xy <- .parse_cell_id(undelivered$sub_territory)
  snk_xy <- .parse_cell_id(sink_cells$sub_territory)
  sources <- undelivered |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      .source = dplyr::row_number(),
      slon = src_xy$lon,
      slat = src_xy$lat,
      applied_n = .data$applied_n
    )
  sinks <- sink_cells |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      sub_territory = .data$sub_territory,
      lon = snk_xy$lon,
      lat = snk_xy$lat,
      room_n = .data$room_n
    )
  sources |>
    dplyr::inner_join(
      sinks,
      by = c("year", "territory"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      ring = round(
        pmax(abs(.data$lon - .data$slon), abs(.data$lat - .data$slat)) / 0.5
      )
    ) |>
    dplyr::filter(.data$ring == min(.data$ring), .by = ".source") |>
    dplyr::mutate(
      applied_n = .data$applied_n * .data$room_n / sum(.data$room_n),
      .by = ".source"
    ) |>
    .urban_relocated_rows()
}

# "polity": the undelivered nitrogen of each polity-year is pooled and spread
# over all of that polity-year's cropland cells by room_n, i.e. by cropland
# area -- the rule build_n_inputs(method_unsupported = "reallocate") applies to
# the same rows further down the chain.
.urban_relocate_polity <- function(undelivered, sink_cells) {
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
    .urban_relocated_rows()
}

.urban_relocated_rows <- function(x) {
  x |>
    dplyr::summarise(
      applied_n = sum(.data$applied_n),
      .by = c("year", "territory", "sub_territory")
    ) |>
    dplyr::mutate(route = "relocated")
}

# One row per year: how much urban N the transport step could not deliver to
# any cropland, and what `method_residual` did with it. Attached to the output
# as the "urban_n_undelivered" attribute, and reported.
.urban_undelivered_summary <- function(flows, placed, method) {
  totals <- dplyr::summarise(
    flows,
    urban_n_t = sum(.data$applied_n),
    n_cells = sum(.data$route == "undelivered"),
    undelivered_t = sum(.data$applied_n[.data$route == "undelivered"]),
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
      undelivered_share = .data$undelivered_t / .data$urban_n_t,
      method_urban_residual = method
    ) |>
    dplyr::arrange(.data$year)
  .urban_report_undelivered(summary, method)
  summary
}

# A message when every undelivered tonne was relocated; a warning when any of
# it was dropped or is left on a cell with no cropland, because then the
# nitrogen balance cannot place it (build_n_inputs()'s `method_unsupported`
# decides what happens next).
.urban_report_undelivered <- function(summary, method) {
  n_cells <- sum(summary$n_cells)
  if (n_cells == 0L) {
    return(invisible(NULL))
  }
  undelivered <- signif(sum(summary$undelivered_t), 6)
  share <- signif(100 * undelivered / sum(summary$urban_n_t), 3)
  msg <- c(
    "{cli::qty(n_cells)}{n_cells} urban-N source cell-year{?s} with no
     cropland could not deliver {undelivered} t N ({share}% of urban N).",
    i = "{.arg method_residual} = {.val {method}}: relocated
         {signif(sum(summary$relocated_t), 6)} t, left on cells with no
         cropland {signif(sum(summary$stranded_t), 6)} t, dropped
         {signif(sum(summary$dropped_t), 6)} t.",
    i = "Per-year totals: {.code attr(x, \"urban_n_undelivered\")}."
  )
  if (sum(summary$dropped_t) + sum(summary$stranded_t) > 0) {
    cli::cli_warn(msg, class = "whep_urban_n_undelivered")
  } else {
    cli::cli_inform(msg, class = "whep_urban_n_undelivered")
  }
  invisible(NULL)
}

# Parse sub_territory back to lon/lat, aggregate transported, residual and
# relocated flows to the final schema, and stamp the methods. The two
# component columns say how much of a cell's `urban_n_t` reached it through
# the residual rule (`urban_n_relocated_t`) and how much sits on a cell with no
# cropland (`urban_n_stranded_t`).
.urban_finalise <- function(flows, method_residual) {
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
      urban_n_relocated_t = sum(.data$applied_n[.data$route == "relocated"]),
      urban_n_stranded_t = sum(.data$applied_n[.data$route == "stranded"]),
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    dplyr::mutate(
      method_urban = "spain_hist_rate|room_weighted",
      method_urban_residual = method_residual
    )
}

# Toy fixture for a runnable example (one cell, one polity, one year).
.example_urban_n <- function() {
  tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~year, ~urban_n_t,
    -0.25, -0.25, 203L,       2020L, 4.5
  ) |>
    dplyr::mutate(
      urban_n_relocated_t = 0,
      urban_n_stranded_t = 0,
      method_urban = "spain_hist_rate|room_weighted",
      method_urban_residual = "nearest"
    ) |>
    .add_reporting_polity_columns()
}
