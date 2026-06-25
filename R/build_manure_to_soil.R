#' Build the livestock manure-to-soil dataset from realised feed intake.
#'
#' @description
#' Top-level driver that turns the realised feed intake from [redistribute_feed()]
#' into manure nitrogen, carbon and volatile solids applied to soil by land use
#' and crop, plus the management-loss side-streams. It chains
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
#'   `land_use x crop (x cell)` with all `method_*` provenance columns),
#'   `losses` (management-loss side-streams per polity) and `excretion` (the
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
#' build_manure_to_soil(intake, gridded = gridded)
build_manure_to_soil <- function(
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
  .fold_transport(local, flows, m$allocation)
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
.fold_transport <- function(local, flows, alloc_methods) {
  opt <- .allocate_options(alloc_methods)
  kept <- dplyr::filter(local, .data$land_use != "Unallocated")
  transported <- flows |>
    dplyr::filter(.data$kind == "transported") |>
    .transport_landing("transported", FALSE)
  residual <- flows |>
    dplyr::filter(.data$kind == "residual") |>
    .transport_landing(
      .disposal_land_use(opt$disposal_method),
      .disposal_over_cap(opt$disposal_method)
    )
  dplyr::bind_rows(kept, transported, residual)
}

.transport_landing <- function(flows, land_use, over_cap) {
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
  )
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
