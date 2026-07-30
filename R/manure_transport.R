#' Spill surplus manure to neighbouring cells with spare capacity.
#'
#' @description
#' Moves the manure nitrogen that a cell could not place locally (its surplus
#' above the local cropland and grassland caps) to nearby same-polity cells that
#' still have room, on the 0.5-degree grid. Each source offers its surplus to its
#' single-ring (king-move) neighbours in proportion to their remaining room; a
#' sink that is over-subscribed by several sources is filled only to its room and
#' the rejected manure returns to the sources as residual. This is the
#' resolution-robust, mass-conserving analogue of Spain's room-weighted first-ring
#' redistribution (`Calc_OA_redistribution`); cross-polity transport is not
#' allowed. Carbon and volatile solids ride along with each nitrogen flow at the
#' source cell's bundle ratio.
#'
#' @param source_cells A tibble of cells exporting manure, keyed by `year`,
#'   `territory` and `sub_territory` (a `"lon_lat"` cell id), with `surplus_n`,
#'   `surplus_c` and `surplus_vs` (t).
#' @param sink_cells A tibble of cells with spare capacity, keyed by `year`,
#'   `territory` and `sub_territory`, with `room_n` (remaining cropland plus
#'   grassland N capacity, t).
#' @param options A named list. `n_rings` (default 1) is the neighbourhood radius
#'   in grid steps; only the single-ring kernel is shipped.
#'
#' @return A tibble with one row per landing site: `year`, `territory`,
#'   `sub_territory` (the sink cell for transported manure, the source cell for
#'   residual), `applied_n`, `applied_c`, `applied_vs`, `kind`
#'   (`"transported"` or `"residual"`) and `method_transport`. The `"residual"`
#'   rows are the un-transportable surplus handed back for local disposal.
#' @export
#' @examples
#' source_cells <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~surplus_n, ~surplus_c, ~surplus_vs,
#'   2020L, "ESP", "1.5_40", 10, 90, 6
#' )
#' sink_cells <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~room_n,
#'   2020L, "ESP", "1_40", 4,
#'   2020L, "ESP", "2_40", 6
#' )
#' allocate_manure_transport(source_cells, sink_cells)
allocate_manure_transport <- function(
  source_cells,
  sink_cells,
  options = list()
) {
  opt <- .transport_options(options)
  .check_transport_cols(source_cells, sink_cells)
  sources <- .transport_sources(source_cells)
  sinks <- .transport_sinks(sink_cells)
  flows <- .transport_flows(sources, sinks, opt)
  .assemble_transport(sources, flows)
}

# Private helpers ----

.transport_options <- function(options) {
  opt <- utils::modifyList(list(n_rings = 1), options)
  if (
    !rlang::is_scalar_integerish(opt$n_rings) ||
      opt$n_rings < 1
  ) {
    cli::cli_abort("{.arg n_rings} must be a positive integer.")
  }
  opt
}

.check_transport_cols <- function(source_cells, sink_cells) {
  src_req <- c(
    "year",
    "territory",
    "sub_territory",
    "surplus_n",
    "surplus_c",
    "surplus_vs"
  )
  snk_req <- c("year", "territory", "sub_territory", "room_n")
  src_miss <- src_req[
    !purrr::map_lgl(src_req, ~ rlang::has_name(source_cells, .x))
  ]
  snk_miss <- snk_req[
    !purrr::map_lgl(snk_req, ~ rlang::has_name(sink_cells, .x))
  ]
  if (length(src_miss) > 0) {
    cli::cli_abort(
      "{.arg source_cells} is missing column{?s}: {.val {src_miss}}."
    )
  }
  if (length(snk_miss) > 0) {
    cli::cli_abort(
      "{.arg sink_cells} is missing column{?s}: {.val {snk_miss}}."
    )
  }
  invisible(NULL)
}

# Sum each cell's mass by key first, so repeated rows for a cell are the cell's
# total surplus (never a partial double-count that would leak mass downstream).
.transport_sources <- function(source_cells) {
  src <- source_cells |>
    tibble::as_tibble() |>
    dplyr::summarise(
      surplus_n = sum(.data$surplus_n),
      surplus_c = sum(.data$surplus_c),
      surplus_vs = sum(.data$surplus_vs),
      .by = c("year", "territory", "sub_territory")
    ) |>
    dplyr::filter(.data$surplus_n > 1e-12)
  coords <- .parse_cell_id(src$sub_territory)
  src |>
    dplyr::mutate(
      slon = round(coords$lon, 2),
      slat = round(coords$lat, 2),
      c_per_n = .data$surplus_c / .data$surplus_n,
      vs_per_n = .data$surplus_vs / .data$surplus_n
    ) |>
    dplyr::select(
      "year",
      "territory",
      source_cell = "sub_territory",
      "slon",
      "slat",
      "surplus_n",
      "c_per_n",
      "vs_per_n"
    )
}

.transport_sinks <- function(sink_cells) {
  snk <- sink_cells |>
    tibble::as_tibble() |>
    dplyr::summarise(
      room_n = sum(.data$room_n),
      .by = c("year", "territory", "sub_territory")
    ) |>
    dplyr::filter(.data$room_n > 1e-12)
  coords <- .parse_cell_id(snk$sub_territory)
  snk |>
    dplyr::mutate(lon = round(coords$lon, 2), lat = round(coords$lat, 2)) |>
    dplyr::select(
      "year",
      "territory",
      sink_cell = "sub_territory",
      "lon",
      "lat",
      "room_n"
    )
}

# Propose room-weighted flows from each source to its same-polity ring sinks,
# then scale flows into any over-subscribed sink down to its room. One pass,
# mass-conserving: a sink never receives more than its room; the unmet surplus
# stays with the source.
.transport_flows <- function(sources, sinks, opt) {
  sources |>
    tidyr::crossing(.ring_offsets(opt$n_rings)) |>
    dplyr::mutate(
      lon = round(.data$slon + .data$dlon, 2),
      lat = round(.data$slat + .data$dlat, 2)
    ) |>
    dplyr::inner_join(sinks, by = c("year", "territory", "lon", "lat")) |>
    dplyr::mutate(
      nbr_room = sum(.data$room_n),
      .by = c("year", "territory", "source_cell")
    ) |>
    dplyr::mutate(flow0 = .data$surplus_n * .data$room_n / .data$nbr_room) |>
    dplyr::mutate(
      sink_demand = sum(.data$flow0),
      .by = c("year", "territory", "sink_cell")
    ) |>
    dplyr::mutate(
      flow_n = .data$flow0 * pmin(1, .data$room_n / .data$sink_demand)
    )
}

.assemble_transport <- function(sources, flows) {
  transported <- flows |>
    dplyr::mutate(
      flow_c = .data$flow_n * .data$c_per_n,
      flow_vs = .data$flow_n * .data$vs_per_n
    ) |>
    dplyr::summarise(
      applied_n = sum(.data$flow_n),
      applied_c = sum(.data$flow_c),
      applied_vs = sum(.data$flow_vs),
      .by = c("year", "territory", "sink_cell")
    ) |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      sub_territory = .data$sink_cell,
      applied_n = .data$applied_n,
      applied_c = .data$applied_c,
      applied_vs = .data$applied_vs,
      kind = "transported"
    )
  residual <- .transport_residual(sources, flows)
  dplyr::bind_rows(transported, residual) |>
    dplyr::mutate(method_transport = "room_weighted")
}

# Surplus each source could not deliver = surplus - sent; carried back at the
# source bundle ratio for the driver to dispose locally.
.transport_residual <- function(sources, flows) {
  sent <- dplyr::summarise(
    flows,
    sent_n = sum(.data$flow_n),
    .by = c("year", "territory", "source_cell")
  )
  sources |>
    dplyr::left_join(sent, by = c("year", "territory", "source_cell")) |>
    dplyr::mutate(
      sent_n = dplyr::coalesce(.data$sent_n, 0),
      residual_n = .data$surplus_n - .data$sent_n
    ) |>
    dplyr::filter(.data$residual_n > 1e-12) |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      sub_territory = .data$source_cell,
      applied_n = .data$residual_n,
      applied_c = .data$residual_n * .data$c_per_n,
      applied_vs = .data$residual_n * .data$vs_per_n,
      kind = "residual"
    )
}

# Filled Chebyshev disk of radius `n_rings` on the 0.5-degree grid, excluding the
# centre. n_rings = 1 reproduces the eight king-move offsets, so the existing
# grass-border stencil is unchanged; this is a separate generalised helper that
# does not touch `.king_move_offsets`.
.ring_offsets <- function(n_rings = 1) {
  vals <- seq(-n_rings, n_rings) * 0.5
  g <- tidyr::crossing(dlon = vals, dlat = vals)
  g[!(g$dlon == 0 & g$dlat == 0), , drop = FALSE]
}
