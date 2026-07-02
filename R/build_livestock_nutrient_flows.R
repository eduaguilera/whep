#' Build livestock nutrient flows from realised feed intake.
#'
#' @description
#' Top-level driver that traces the nitrogen, carbon and volatile solids in
#' realised feed intake (the [redistribute_feed()] result) through livestock
#' excretion, manure management and application to soil by land use and crop,
#' plus the management-loss side-streams. It chains
#' [estimate_n_excretion()], [split_manure_management()],
#' [apply_management_losses()] and [allocate_manure_to_land()]; at the
#' `"subnational"` resolution it additionally spills each cell's un-placeable
#' surplus to neighbouring cells with [allocate_manure_transport()] before local
#' disposal. Every method choice is recorded in a `method_*` provenance column and
#' the nitrogen balance (excreted = applied + management losses) is conserved.
#'
#' @param intake A tibble of realised feed intake (the [redistribute_feed()]
#'   result); see [estimate_n_excretion()] for the required columns. At
#'   `"subnational"` resolution `sub_territory` is the `"lon_lat"` cell id.
#' @param resolution One of `"global"`, `"national"` (default) or
#'   `"subnational"`. Transport between cells runs only at `"subnational"`.
#' @param methods A named list of per-stage option lists, any of `excretion`,
#'   `split`, `losses`, `allocation` and `transport`, each forwarded to the
#'   matching pipeline function's `options`.
#' @param gridded The land-surface layer (`crops` and optional `grass` tibbles)
#'   passed to [allocate_manure_to_land()]; required for the default
#'   `"potential_uptake"` cap. `NULL` is treated as an empty list.
#'
#' @return A named list with `applied` (manure applied per
#'   `land_use x crop (x cell)` with `manure_type` (`"Excreta"`/`"Solid"`/
#'   `"Liquid"`) and all `method_*` provenance columns), `losses`
#'   (management-loss side-streams per polity) and `excretion` (the
#'   per-category excretion totals).
#' @export
#' @examples
#' intake <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~livestock_category,
#'   ~item_cbs_code, ~feed_quality, ~intake_dm_t,
#'   2020L, "ESP", NA, "Cattle_milk", 2513L, "high_quality", 200,
#'   2020L, "ESP", NA, "Cattle_milk", NA, "grass", 600
#' )
#' gridded <- list(
#'   crops = tibble::tribble(
#'     ~year, ~territory, ~sub_territory, ~crop, ~manure_n_receptivity, ~crop_n_cap,
#'     2020L, "ESP", NA, "barley", 6, 200,
#'     2020L, "ESP", NA, "wheat", 4, 200
#'   )
#' )
#' build_livestock_nutrient_flows(intake, gridded = gridded)
build_livestock_nutrient_flows <- function(
  intake,
  resolution = "national",
  methods = list(),
  gridded = NULL
) {
  .check_resolution(resolution)
  m <- .manure_methods(methods)
  grid <- gridded %||% list()
  alloc_opt <- .allocate_options(m$allocation)

  excretion <- estimate_n_excretion(intake, m$excretion)
  split <- split_manure_management(excretion, m$split)
  losses <- apply_management_losses(split, m$losses)

  applied <- if (resolution == "subnational") {
    .manure_subnational(losses, grid, m)
  } else {
    allocate_manure_to_land(losses, grid, m$allocation)
  }

  list(
    applied = .tag_provenance(
      applied,
      excretion,
      split,
      losses,
      resolution,
      alloc_opt
    ),
    losses = .summarise_losses(losses),
    excretion = excretion
  )
}

# Private helpers ----

.check_resolution <- function(resolution) {
  valid <- c("global", "national", "subnational")
  if (!rlang::is_string(resolution) || !resolution %in% valid) {
    cli::cli_abort("{.arg resolution} must be one of {.val {valid}}.")
  }
  invisible(NULL)
}

.manure_methods <- function(methods) {
  stages <- c("excretion", "split", "losses", "allocation", "transport")
  bad <- setdiff(names(methods), stages)
  if (length(bad) > 0) {
    cli::cli_abort(
      "Unknown {.arg methods} stage(s): {.val {bad}}. Use {.val {stages}}."
    )
  }
  stats::setNames(lapply(stages, function(s) methods[[s]] %||% list()), stages)
}

.summarise_losses <- function(losses) {
  cols <- c(
    "n_volatilized",
    "n_leached",
    "n2o_direct_n",
    "n2_n",
    "n2o_indirect_n",
    "c_lost",
    "vs_destroyed"
  )
  losses |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols), sum),
      .by = c("year", "territory", "sub_territory")
    )
}

# Stamp every row with the caller's actual stage choices. The allocation
# provenance is set from the resolved options (not read off the rows) so that
# transported/disposed rows and the internal "retain_unallocated" pass used by
# the subnational composition do not leak NA or the internal label.
.tag_provenance <- function(
  applied,
  excretion,
  split,
  losses,
  resolution,
  alloc_opt
) {
  m_transport <- if (identical(resolution, "subnational")) {
    "room_weighted"
  } else {
    NA_character_
  }
  applied |>
    dplyr::mutate(
      resolution = resolution,
      method_n_excretion = excretion$method_n_excretion[1],
      method_vs = excretion$method_vs[1],
      method_mms = split$method_mms[1],
      method_losses = losses$method_losses[1],
      method_allocation = alloc_opt$method,
      method_cap = alloc_opt$cap_method,
      disposal_method = alloc_opt$disposal_method,
      method_transport = m_transport
    )
}

# Subnational: allocate per cell while RETAINING the un-placeable surplus, spill
# that surplus to neighbouring cells' remaining room, then dispose what is still
# left per the caller's disposal method. Mass is conserved at each step.
.manure_subnational <- function(losses, gridded, m) {
  local_opt <- utils::modifyList(
    m$allocation,
    list(disposal_method = "retain_unallocated")
  )
  # Retained surplus here is not disposed but routed to transport, so the local
  # over-cap warning would mislead; the real disposal happens after the spill.
  local <- suppressWarnings(
    allocate_manure_to_land(losses, gridded, local_opt)
  )
  surplus <- .cell_surplus(local)
  room <- .cell_room(local, gridded, m$allocation)
  flows <- allocate_manure_transport(surplus, room, m$transport)
  by_type <- .transport_manure_type_split(local, surplus, room, m$transport)
  .fold_transport(local, flows, by_type, m$allocation)
}

# allocate_manure_transport()'s public contract is pooled across manure_type
# (a cell's spare cropland-N-capacity does not care which manure_type fills
# it, and that function is also reused by build_urban_n(); its signature is
# out of scope here). To reattach manure_type to the transported/residual
# rows, this recomputes the same source-to-sink pairwise flows with the
# package's own transport helpers, defined in the manure transport module, so
# each sink's transported total can be split by the manure_type mix each
# contributing source cell actually sent, and each source's residual by its
# own local manure_type mix -- the same proportional-share pattern the
# manure allocation module's own reattachment helper uses.
.transport_manure_type_split <- function(local, surplus, room, transport_m) {
  opt <- .transport_options(transport_m)
  sources <- .transport_sources(surplus)
  sinks <- .transport_sinks(room)
  pair_flows <- .transport_flows(sources, sinks, opt)
  type_mix <- .surplus_type_mix(local)
  list(
    transported = .split_transported(pair_flows, type_mix),
    residual = .split_residual(sources, pair_flows, type_mix)
  )
}

# Each source cell's manure_type share of its own retained surplus (the
# "Unallocated" rows of the local, pre-transport allocation).
.surplus_type_mix <- function(local) {
  local |>
    dplyr::filter(.data$land_use == "Unallocated") |>
    dplyr::mutate(
      cell_total = sum(.data$applied_n),
      .by = c("year", "territory", "sub_territory")
    ) |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      source_cell = .data$sub_territory,
      manure_type = .data$manure_type,
      share = dplyr::if_else(
        .data$cell_total > 0,
        .data$applied_n / .data$cell_total,
        0
      )
    )
}

# Split each sink's transported N by the manure_type mix of the N it actually
# received, summed over every contributing source cell's own mix.
.split_transported <- function(pair_flows, type_mix) {
  pair_flows |>
    dplyr::select("year", "territory", "source_cell", "sink_cell", "flow_n") |>
    dplyr::left_join(
      type_mix,
      by = c("year", "territory", "source_cell"),
      relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      applied_n = sum(.data$flow_n * .data$share),
      .by = c("year", "territory", "sink_cell", "manure_type")
    ) |>
    dplyr::rename(sub_territory = "sink_cell")
}

# Split each source's residual N (surplus not sent anywhere) by its own local
# manure_type mix.
.split_residual <- function(sources, pair_flows, type_mix) {
  sent <- dplyr::summarise(
    pair_flows,
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
    dplyr::left_join(
      type_mix,
      by = c("year", "territory", "source_cell"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      sub_territory = .data$source_cell,
      manure_type = .data$manure_type,
      applied_n = .data$residual_n * .data$share
    )
}

# Split a landed (transported or residual) tibble's pooled applied_n/c/vs
# across manure_type, in proportion to the matching by_type$applied_n share
# for that cell -- the bundle C:N/VS:N ratio is unchanged by the split, so
# applied_c/applied_vs scale with the same share as applied_n.
.apply_type_split <- function(pooled, by_type) {
  shares <- by_type |>
    dplyr::mutate(
      cell_total = sum(.data$applied_n),
      .by = c("year", "territory", "sub_territory")
    ) |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      sub_territory = .data$sub_territory,
      manure_type = .data$manure_type,
      share = dplyr::if_else(
        .data$cell_total > 0,
        .data$applied_n / .data$cell_total,
        0
      )
    )
  joined <- pooled |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::left_join(
      shares,
      by = c("year", "territory", "sub_territory"),
      relationship = "many-to-many"
    )
  unmatched <- joined |>
    dplyr::filter(is.na(.data$manure_type)) |>
    dplyr::mutate(manure_type = "Excreta", share = 1)
  joined |>
    dplyr::filter(!is.na(.data$manure_type)) |>
    dplyr::bind_rows(unmatched) |>
    dplyr::mutate(
      applied_n = .data$applied_n * .data$share,
      applied_c = .data$applied_c * .data$share,
      applied_vs = .data$applied_vs * .data$share
    ) |>
    dplyr::arrange(.data$row_id) |>
    dplyr::select(-"row_id", -"share")
}

.cell_surplus <- function(local) {
  local |>
    dplyr::filter(.data$land_use == "Unallocated") |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      sub_territory = .data$sub_territory,
      surplus_n = .data$applied_n,
      surplus_c = .data$applied_c,
      surplus_vs = .data$applied_vs
    )
}

# Remaining room per cell = resolved cropland + grassland cap minus the collected
# manure already placed locally (grazing deposition is in situ, not charged to
# the cap, matching allocate_manure_to_land).
.cell_room <- function(local, gridded, alloc_methods) {
  opt <- .allocate_options(alloc_methods)
  crop_cap <- .prepare_crop_layer(gridded[["crops"]], opt) |>
    dplyr::summarise(
      cap = sum(.data$cap_n),
      .by = c("year", "territory", "sub_territory")
    )
  grass_cap <- .prepare_grass_cap(gridded[["grass"]], opt)
  placed <- local |>
    dplyr::filter(
      .data$source_stream == "collected" &
        .data$land_use %in% c("Cropland", "Grassland")
    ) |>
    dplyr::summarise(
      placed = sum(.data$applied_n),
      .by = c("year", "territory", "sub_territory")
    )
  crop_cap |>
    .join_cell_grass_cap(grass_cap) |>
    dplyr::left_join(placed, by = c("year", "territory", "sub_territory")) |>
    dplyr::mutate(
      room_n = pmax(
        0,
        .data$cap + .data$grass_cap_n - dplyr::coalesce(.data$placed, 0)
      )
    ) |>
    dplyr::select("year", "territory", "sub_territory", "room_n")
}

.join_cell_grass_cap <- function(crop_cap, grass_cap) {
  if (is.null(grass_cap)) {
    return(dplyr::mutate(crop_cap, grass_cap_n = 0))
  }
  crop_cap |>
    dplyr::left_join(
      dplyr::rename(grass_cap, grass_cap_n = "grass_n_cap"),
      by = c("year", "territory", "sub_territory")
    ) |>
    dplyr::mutate(grass_cap_n = dplyr::coalesce(.data$grass_cap_n, 0))
}

# Replace each source cell's retained surplus with the transport outcome:
# manure delivered to neighbours, plus the un-transportable remainder disposed
# locally per the caller's disposal method.
.fold_transport <- function(local, flows, by_type, alloc_methods) {
  opt <- .allocate_options(alloc_methods)
  kept <- dplyr::filter(local, .data$land_use != "Unallocated")
  transported <- flows |>
    dplyr::filter(.data$kind == "transported") |>
    .transport_landing("transported", FALSE, by_type$transported)
  residual <- flows |>
    dplyr::filter(.data$kind == "residual") |>
    .transport_landing(
      .disposal_land_use(opt$disposal_method),
      .disposal_over_cap(opt$disposal_method),
      by_type$residual
    )
  dplyr::bind_rows(kept, transported, residual)
}

.transport_landing <- function(flows, land_use, over_cap, by_type) {
  dplyr::transmute(
    flows,
    year = .data$year,
    territory = .data$territory,
    sub_territory = .data$sub_territory,
    land_use = land_use,
    crop = NA_character_,
    source_stream = "transported",
    applied_n = .data$applied_n,
    applied_c = .data$applied_c,
    applied_vs = .data$applied_vs,
    over_cap = over_cap
  ) |>
    .apply_type_split(by_type)
}

# Disposal fate mapping, the twin of allocate_manure_to_land()'s `.disposal_rows`.
.disposal_land_use <- function(disposal_method) {
  switch(
    disposal_method,
    over_apply_local = "Cropland",
    unmanaged_disposal = "Disposal",
    retain_unallocated = "Unallocated"
  )
}

.disposal_over_cap <- function(disposal_method) {
  identical(disposal_method, "over_apply_local")
}
