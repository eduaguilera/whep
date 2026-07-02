#' Allocate field-available manure to cropland and grassland by crop.
#'
#' @description
#' Distributes the manure nitrogen, carbon and volatile solids that survive
#' management (the output of [apply_management_losses()]) across cropland crops
#' and grassland, with a local agronomic cap and an explicit overflow path. The
#' collected/housed manure fills cropland up to each crop's cap (weighted by
#' West-2014 receptivity by default), the surplus spills onto grassland up to the
#' grassland cap, and any residual beyond all reachable sinks follows the
#' `disposal_method`. The in-situ grazing stream is deposited on grassland where
#' it falls, uncapped. Carbon and volatile solids ride along with each stream at
#' its post-storage bundle ratio, so mass is conserved by construction.
#'
#' @param applied A tibble from [apply_management_losses()] with at least `year`,
#'   `territory`, `sub_territory`, `stream`, `applied_n`, `applied_c` and
#'   `applied_vs`.
#' @param gridded A named list describing the land surface for each polity:
#'   * `crops`: a tibble keyed by `year`, `territory`, `sub_territory`, `crop`
#'     with the allocation weight (`manure_n_receptivity` for
#'     `"area_x_receptivity"`, `crop_n_demand` for `"crop_n_demand"`) and the cap
#'     basis (`crop_n_cap`, t N, for `"potential_uptake"`/`"realised_removal"`;
#'     `crop_area_ha` for `"fixed_ceiling"`).
#'   * `grass` (optional): a tibble keyed by `year`, `territory`, `sub_territory`
#'     with `grass_n_cap` (t N) or `grass_area_ha`. The grassland cap is scaled by
#'     `f_n_tolerance` on the same footing as the uptake-based crop caps (not for
#'     `"fixed_ceiling"`). Absent grassland means no grassland sink (cap zero).
#' @param options A named list of method options: `method`
#'   (`"area_x_receptivity"` default, or `"crop_n_demand"`), `cap_method`
#'   (`"potential_uptake"` default, `"realised_removal"`, or `"fixed_ceiling"`),
#'   `f_n_tolerance` (default 1.2, applied to the uptake-based caps),
#'   `fixed_ceiling_kg_ha` (default 170, EU Nitrates) and `disposal_method`
#'   (`"over_apply_local"` default, `"unmanaged_disposal"`, or
#'   `"retain_unallocated"`).
#'
#' @return A tibble with one row per allocation target: `year`, `territory`,
#'   `sub_territory`, `land_use` (`"Cropland"`/`"Grassland"`/`"Disposal"`/
#'   `"Unallocated"`), `crop`, `source_stream` (`"collected"`/`"grazing"`),
#'   `manure_type` (`"Excreta"`/`"Solid"`/`"Liquid"`, carried from `applied`
#'   when present; each allocated row's N/C/VS is split across the
#'   `manure_type`s in proportion to their share of the cell's collected N,
#'   so the capacity-filling math itself stays pooled), `applied_n`,
#'   `applied_c`, `applied_vs`, `over_cap` and the `method_allocation`,
#'   `method_cap` and `disposal_method` provenance columns.
#' @export
#' @examples
#' applied <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~stream, ~manure_type,
#'   ~applied_n, ~applied_c, ~applied_vs,
#'   2020L, "ESP", NA, "collected", "Solid", 80, 800, 40,
#'   2020L, "ESP", NA, "grazing", "Excreta", 20, 380, 12
#' )
#' crops <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~crop, ~manure_n_receptivity, ~crop_n_cap,
#'   2020L, "ESP", NA, "barley", 6, 50,
#'   2020L, "ESP", NA, "wheat", 4, 40
#' )
#' allocate_manure_to_land(applied, list(crops = crops))
allocate_manure_to_land <- function(
  applied,
  gridded = list(),
  options = list()
) {
  opt <- .allocate_options(options)
  .check_applied_cols(applied)
  .check_streams(applied)
  streams <- .aggregate_applied_streams(applied)
  type_shares <- .manure_type_shares(applied)
  crops <- .prepare_crop_layer(gridded[["crops"]], opt)
  grass_cap <- .prepare_grass_cap(gridded[["grass"]], opt)
  cropland <- .fill_cropland(streams, crops)
  .assemble_allocation(streams, cropland, grass_cap, opt) |>
    .reattach_manure_type(type_shares)
}

# Private helpers ----

.allocate_options <- function(options) {
  opt <- utils::modifyList(
    list(
      method = "area_x_receptivity",
      cap_method = "potential_uptake",
      f_n_tolerance = 1.2,
      fixed_ceiling_kg_ha = 170,
      disposal_method = "over_apply_local"
    ),
    options
  )
  methods <- c("area_x_receptivity", "crop_n_demand")
  caps <- c("potential_uptake", "realised_removal", "fixed_ceiling")
  disp <- c("over_apply_local", "unmanaged_disposal", "retain_unallocated")
  if (!opt$method %in% methods) {
    cli::cli_abort(
      "Unknown {.arg method} {.val {opt$method}}. Use {.val {methods}}."
    )
  }
  if (!opt$cap_method %in% caps) {
    cli::cli_abort(
      "Unknown {.arg cap_method} {.val {opt$cap_method}}. Use {.val {caps}}."
    )
  }
  if (!opt$disposal_method %in% disp) {
    cli::cli_abort(
      "Unknown {.arg disposal_method} {.val {opt$disposal_method}}. Use {.val {disp}}."
    )
  }
  opt
}

.check_applied_cols <- function(applied) {
  req <- c(
    "year",
    "territory",
    "sub_territory",
    "stream",
    "applied_n",
    "applied_c",
    "applied_vs"
  )
  miss <- req[!purrr::map_lgl(req, ~ rlang::has_name(applied, .x))]
  if (length(miss) > 0) {
    cli::cli_abort("{.arg applied} is missing column{?s}: {.val {miss}}.")
  }
  invisible(NULL)
}

# The stream label is a branch key: only "collected" and "grazing" are summed,
# so an unexpected value would be dropped (silent mass loss). Abort instead.
.check_streams <- function(applied) {
  bad <- setdiff(unique(applied$stream), c("collected", "grazing"))
  if (length(bad) > 0) {
    valid <- c("collected", "grazing")
    cli::cli_abort(
      "Unexpected {.field stream} value(s): {.val {bad}}. Expected {.val {valid}}."
    )
  }
  invisible(NULL)
}

# Collapse the per-system manure to per-polity collected and grazing totals.
# The capacity-filling math (crop caps, grassland spillover, disposal) stays
# pooled across manure_type here; the per-manure_type split is reattached
# downstream from .manure_type_shares(), the same proportional-share pattern
# .stream_ratios() already uses for applied_c/applied_vs.
.aggregate_applied_streams <- function(applied) {
  applied |>
    tibble::as_tibble() |>
    dplyr::summarise(
      coll_n = sum(.data$applied_n[.data$stream == "collected"]),
      coll_c = sum(.data$applied_c[.data$stream == "collected"]),
      coll_vs = sum(.data$applied_vs[.data$stream == "collected"]),
      graze_n = sum(.data$applied_n[.data$stream == "grazing"]),
      graze_c = sum(.data$applied_c[.data$stream == "grazing"]),
      graze_vs = sum(.data$applied_vs[.data$stream == "grazing"]),
      .by = c("year", "territory", "sub_territory")
    )
}

# Per-(year, territory, sub_territory, stream, manure_type) share of that
# stream's pooled N. A missing manure_type column (e.g. a caller that has not
# adopted the field yet) is treated as a single implicit "Excreta" bucket, so
# the reattachment step is a no-op share of 1 and every existing aggregate
# total is untouched.
.manure_type_shares <- function(applied) {
  applied <- tibble::as_tibble(applied)
  if (!rlang::has_name(applied, "manure_type")) {
    applied <- dplyr::mutate(applied, manure_type = "Excreta")
  }
  applied |>
    dplyr::summarise(
      type_n = sum(.data$applied_n),
      .by = c("year", "territory", "sub_territory", "stream", "manure_type")
    ) |>
    dplyr::mutate(
      stream_n = sum(.data$type_n),
      share = dplyr::if_else(
        .data$stream_n > 0,
        .data$type_n / .data$stream_n,
        0
      ),
      .by = c("year", "territory", "sub_territory", "stream")
    ) |>
    dplyr::select(
      "year",
      "territory",
      "sub_territory",
      "stream",
      "manure_type",
      "share"
    )
}

# Split each pooled allocation row's N/C/VS across manure_type in proportion
# to that manure_type's share of the row's source_stream (collected/grazing)
# pooled N for the same cell; a cell/stream with zero total N (share table has
# no matching rows, e.g. a "Disposal"/"Unallocated" cell whose stream never
# had positive pooled N) keeps a single "Excreta"-labelled row so mass is
# never silently dropped.
.reattach_manure_type <- function(allocation, type_shares) {
  joined <- allocation |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::left_join(
      type_shares,
      by = c(
        "year",
        "territory",
        "sub_territory",
        "source_stream" = "stream"
      ),
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
    dplyr::select(-"row_id", -"share") |>
    dplyr::relocate("manure_type", .after = "source_stream")
}

.prepare_crop_layer <- function(crops, opt) {
  if (is.null(crops)) {
    cli::cli_abort("{.arg gridded} must include a {.field crops} layer.")
  }
  crops <- tibble::as_tibble(crops)
  weight_col <- if (opt$method == "crop_n_demand") {
    "crop_n_demand"
  } else {
    "manure_n_receptivity"
  }
  if (!rlang::has_name(crops, weight_col)) {
    cli::cli_abort(
      "{.field crops} needs {.field {weight_col}} for {.arg method} {.val {opt$method}}."
    )
  }
  crops |>
    dplyr::mutate(weight = .data[[weight_col]]) |>
    .resolve_crop_caps(opt)
}

# Caps: fixed_ceiling = rate x area (the legal ceiling, no tolerance); the
# uptake-based caps are the supplied potential/realised N x f_n_tolerance.
.resolve_crop_caps <- function(crops, opt) {
  if (opt$cap_method == "fixed_ceiling") {
    if (!rlang::has_name(crops, "crop_area_ha")) {
      cli::cli_abort(
        "{.val fixed_ceiling} needs {.field crop_area_ha} in {.field crops}."
      )
    }
    return(dplyr::mutate(
      crops,
      cap_n = opt$fixed_ceiling_kg_ha / 1000 * .data$crop_area_ha
    ))
  }
  if (!rlang::has_name(crops, "crop_n_cap")) {
    cli::cli_abort(
      "{.val {opt$cap_method}} needs a precomputed {.field crop_n_cap} in {.field crops}."
    )
  }
  dplyr::mutate(crops, cap_n = .data$crop_n_cap * opt$f_n_tolerance)
}

.prepare_grass_cap <- function(grass, opt) {
  if (is.null(grass)) {
    return(NULL)
  }
  grass <- tibble::as_tibble(grass)
  if (opt$cap_method == "fixed_ceiling") {
    if (!rlang::has_name(grass, "grass_area_ha")) {
      cli::cli_abort(
        "{.val fixed_ceiling} needs {.field grass_area_ha} in {.field grass}."
      )
    }
    grass <- dplyr::mutate(
      grass,
      grass_n_cap = opt$fixed_ceiling_kg_ha / 1000 * .data$grass_area_ha
    )
  } else {
    if (!rlang::has_name(grass, "grass_n_cap")) {
      cli::cli_abort(
        "{.val {opt$cap_method}} needs a precomputed {.field grass_n_cap} in {.field grass}."
      )
    }
    grass <- dplyr::mutate(
      grass,
      grass_n_cap = .data$grass_n_cap * opt$f_n_tolerance
    )
  }
  dplyr::select(grass, "year", "territory", "sub_territory", "grass_n_cap")
}

# Single room-weighted pass (Spain Calc_OA_redistribution analogue): proportional
# fill clamped to cap, then the clamped excess redistributed to remaining room.
.fill_cropland <- function(streams, crops) {
  coll <- dplyr::select(
    streams,
    "year",
    "territory",
    "sub_territory",
    "coll_n"
  )
  crops |>
    dplyr::left_join(coll, by = c("year", "territory", "sub_territory")) |>
    dplyr::filter(!is.na(.data$coll_n) & .data$coll_n > 0) |>
    dplyr::mutate(
      sum_w = sum(.data$weight),
      share = dplyr::if_else(.data$sum_w > 0, .data$weight / .data$sum_w, 0),
      a0 = pmin(.data$cap_n, .data$coll_n * .data$share),
      excess0 = .data$coll_n - sum(.data$a0),
      room = .data$cap_n - .data$a0,
      sum_room = sum(.data$room),
      crop_alloc_n = .data$a0 +
        dplyr::if_else(
          .data$sum_room > 0,
          pmin(.data$room, .data$excess0 * .data$room / .data$sum_room),
          0
        ),
      .by = c("year", "territory", "sub_territory")
    )
}

.assemble_allocation <- function(streams, cropland, grass_cap, opt) {
  ratios <- .stream_ratios(streams)
  crop_rows <- dplyr::transmute(
    cropland,
    year = .data$year,
    territory = .data$territory,
    sub_territory = .data$sub_territory,
    land_use = "Cropland",
    crop = .data$crop,
    source_stream = "collected",
    applied_n = .data$crop_alloc_n,
    over_cap = FALSE
  )
  leftover <- .grass_leftover(streams, cropland, grass_cap)
  .warn_disposal(leftover)
  grass_spill <- leftover |>
    dplyr::filter(.data$grass_alloc > 1e-12) |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      sub_territory = .data$sub_territory,
      land_use = "Grassland",
      crop = NA_character_,
      source_stream = "collected",
      applied_n = .data$grass_alloc,
      over_cap = FALSE
    )
  collected_rows <- dplyr::bind_rows(
    crop_rows,
    grass_spill,
    .disposal_rows(leftover, opt)
  ) |>
    dplyr::left_join(ratios, by = c("year", "territory", "sub_territory")) |>
    dplyr::mutate(
      applied_c = .data$applied_n * .data$coll_c_per_n,
      applied_vs = .data$applied_n * .data$coll_vs_per_n
    ) |>
    dplyr::select(-"coll_c_per_n", -"coll_vs_per_n")
  dplyr::bind_rows(collected_rows, .grazing_rows(streams)) |>
    dplyr::mutate(
      method_allocation = opt$method,
      method_cap = opt$cap_method,
      disposal_method = opt$disposal_method
    ) |>
    dplyr::select(
      "year",
      "territory",
      "sub_territory",
      "land_use",
      "crop",
      "source_stream",
      "applied_n",
      "applied_c",
      "applied_vs",
      "over_cap",
      "method_allocation",
      "method_cap",
      "disposal_method"
    )
}

# Per-polity collected C:N and VS:N (the post-storage bundle each allocated tonne
# of collected N carries along).
.stream_ratios <- function(streams) {
  dplyr::transmute(
    streams,
    year = .data$year,
    territory = .data$territory,
    sub_territory = .data$sub_territory,
    coll_c_per_n = dplyr::if_else(
      .data$coll_n > 0,
      .data$coll_c / .data$coll_n,
      0
    ),
    coll_vs_per_n = dplyr::if_else(
      .data$coll_n > 0,
      .data$coll_vs / .data$coll_n,
      0
    )
  )
}

# Collected N not placed on cropland spills to grassland up to its cap; the rest
# is residual handed to the disposal path.
.grass_leftover <- function(streams, cropland, grass_cap) {
  placed <- dplyr::summarise(
    cropland,
    placed = sum(.data$crop_alloc_n),
    .by = c("year", "territory", "sub_territory")
  )
  streams |>
    dplyr::select("year", "territory", "sub_territory", "coll_n") |>
    dplyr::left_join(placed, by = c("year", "territory", "sub_territory")) |>
    dplyr::mutate(
      placed = dplyr::coalesce(.data$placed, 0),
      leftover = .data$coll_n - .data$placed
    ) |>
    .join_grass_cap(grass_cap) |>
    dplyr::mutate(
      grass_alloc = pmin(.data$leftover, .data$grass_cap_n),
      residual = .data$leftover - .data$grass_alloc
    )
}

.join_grass_cap <- function(leftover, grass_cap) {
  if (is.null(grass_cap)) {
    return(dplyr::mutate(leftover, grass_cap_n = 0))
  }
  leftover |>
    dplyr::left_join(
      dplyr::rename(grass_cap, grass_cap_n = "grass_n_cap"),
      by = c("year", "territory", "sub_territory")
    ) |>
    dplyr::mutate(grass_cap_n = dplyr::coalesce(.data$grass_cap_n, 0))
}

.disposal_rows <- function(leftover, opt) {
  fate <- switch(
    opt$disposal_method,
    over_apply_local = list(land_use = "Cropland", over_cap = TRUE),
    unmanaged_disposal = list(land_use = "Disposal", over_cap = FALSE),
    retain_unallocated = list(land_use = "Unallocated", over_cap = FALSE)
  )
  leftover |>
    dplyr::filter(.data$residual > 1e-12) |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      sub_territory = .data$sub_territory,
      land_use = fate$land_use,
      crop = NA_character_,
      source_stream = "collected",
      applied_n = .data$residual,
      over_cap = fate$over_cap
    )
}

.grazing_rows <- function(streams) {
  streams |>
    dplyr::filter(.data$graze_n > 1e-12) |>
    dplyr::transmute(
      year = .data$year,
      territory = .data$territory,
      sub_territory = .data$sub_territory,
      land_use = "Grassland",
      crop = NA_character_,
      source_stream = "grazing",
      applied_n = .data$graze_n,
      applied_c = .data$graze_c,
      applied_vs = .data$graze_vs,
      over_cap = FALSE
    )
}

.warn_disposal <- function(leftover) {
  bad <- dplyr::filter(
    leftover,
    .data$coll_n > 0 & .data$residual / .data$coll_n > 0.05
  )
  if (nrow(bad) > 0) {
    cli::cli_warn(
      "Manure disposal exceeds 5% of collected N in {nrow(bad)} polit{?y/ies}: {.val {unique(bad$territory)}}."
    )
  }
  invisible(NULL)
}
