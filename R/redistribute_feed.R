#' Redistribute available feed supply among livestock demand.
#'
#' @description
#' Matches livestock feed demand to available feed items through a
#' hierarchical allocation that follows the remaining-share principle to
#' avoid exceeding availability. The redistribution path adapts to the
#' `fixed_demand` column in the demand table.
#'
#' @param feed_demand A tibble of feed demand with columns `year`,
#'   `territory`, `sub_territory`, `livestock_category`, `item_cbs_code`,
#'   `feed_group`, `feed_quality`, `demand_dm_t`, and a logical
#'   `fixed_demand`.
#' @param feed_avail A tibble of feed availability with columns `year`,
#'   `sub_territory`, `item_cbs_code`, `feed_group`, `feed_quality`,
#'   `avail_dm_t`, and `feed_scale`.
#' @param options A named list of allocation options. See
#'   `.redistribute_feed_options()` for the available entries and their
#'   defaults.
#'
#' @return A tibble of realised intake per demand row.
#'
#' @export
redistribute_feed <- function(feed_demand, feed_avail, options = list()) {
  options <- .redistribute_feed_options(options)
  .validate_feed_inputs(feed_demand, feed_avail)

  demand <- .prep_feed_demand(feed_demand)
  avail <- .prep_feed_avail(feed_avail)
  if (nrow(demand) == 0) {
    return(.empty_redistribute_result())
  }

  mode <- .allocation_mode(demand)
  state <- .init_state(demand, avail)
  state <- .run_allocation_levels(state, demand, avail, mode)
  .assemble_result(state, demand)
}

.redistribute_feed_options <- function(options = list()) {
  defaults <- list(
    zoot_fixed_max_multiplier = 3,
    prioritize_monogastric = TRUE,
    territory_col = "territory",
    sub_territory_col = "sub_territory",
    monogastric = NULL,
    max_intake_share = NULL,
    verbose = FALSE
  )
  utils::modifyList(defaults, options)
}

.validate_feed_inputs <- function(feed_demand, feed_avail) {
  required_demand <- c(
    "year",
    "territory",
    "sub_territory",
    "livestock_category",
    "item_cbs_code",
    "feed_group",
    "feed_quality",
    "demand_dm_t",
    "fixed_demand"
  )
  required_avail <- c(
    "year",
    "sub_territory",
    "item_cbs_code",
    "feed_group",
    "feed_quality",
    "avail_dm_t",
    "feed_scale"
  )
  .check_required_cols(feed_demand, required_demand, "feed_demand")
  .check_required_cols(feed_avail, required_avail, "feed_avail")
  .check_fixed_demand(feed_demand)
  invisible(NULL)
}

.check_required_cols <- function(data, required, name) {
  missing <- required[!purrr::map_lgl(required, ~ rlang::has_name(data, .x))]
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg {name}} is missing column{?s}: {.val {missing}}."
    )
  }
  invisible(NULL)
}

.check_fixed_demand <- function(feed_demand) {
  fixed <- feed_demand[["fixed_demand"]]
  if (!is.logical(fixed)) {
    cli::cli_abort(
      "{.field fixed_demand} must be logical (TRUE/FALSE) for every row."
    )
  }
  if (any(is.na(fixed))) {
    cli::cli_abort("{.field fixed_demand} cannot contain NA values.")
  }
  invisible(NULL)
}

# ---- Preprocessing ----------------------------------------------------------

.prep_feed_demand <- function(feed_demand) {
  tibble::as_tibble(feed_demand) |>
    dplyr::mutate(
      territory = .blank_to_default(territory, "All_territories"),
      sub_territory = .blank_to_na(sub_territory),
      demand_id = dplyr::row_number(),
      requested_item = item_cbs_code,
      source_compartment = sub_territory,
      remaining = demand_dm_t
    )
}

.prep_feed_avail <- function(feed_avail) {
  avail <- tibble::as_tibble(feed_avail)
  if (!rlang::has_name(avail, "territory")) {
    avail <- dplyr::mutate(avail, territory = "All_territories")
  }
  avail |>
    dplyr::mutate(
      territory = .blank_to_default(territory, "All_territories"),
      feed_scale = .normalise_feed_scale(feed_scale, sub_territory),
      sub_territory = dplyr::if_else(
        feed_scale == "national",
        NA_character_,
        .blank_to_na(sub_territory)
      )
    ) |>
    dplyr::summarize(
      .by = c(
        year,
        territory,
        sub_territory,
        feed_scale,
        item_cbs_code,
        feed_group,
        feed_quality
      ),
      avail_dm_t = sum(avail_dm_t, na.rm = TRUE)
    ) |>
    dplyr::mutate(avail_id = dplyr::row_number())
}

.blank_to_default <- function(x, default) {
  x <- as.character(x)
  dplyr::if_else(is.na(x) | x == "" | x == "NA", default, x)
}

.blank_to_na <- function(x) {
  x <- as.character(x)
  dplyr::if_else(is.na(x) | x == "" | x == "NA", NA_character_, x)
}

.normalise_feed_scale <- function(feed_scale, sub_territory) {
  scale <- tolower(as.character(feed_scale))
  dplyr::case_when(
    scale %in% c("national", "nacional", "import") ~ "national",
    scale %in% c("provincial", "province", "local") ~ "provincial",
    is.na(sub_territory) ~ "national",
    TRUE ~ "provincial"
  )
}

.allocation_mode <- function(demand) {
  has_fixed <- any(demand$fixed_demand)
  has_variable <- any(!demand$fixed_demand)
  dplyr::case_when(
    has_fixed && has_variable ~ "mixed",
    has_fixed ~ "fixed",
    TRUE ~ "variable"
  )
}

# ---- State object -----------------------------------------------------------

.init_state <- function(demand, avail) {
  cap_ledger <- demand |>
    dplyr::filter(feed_quality != "zoot_fixed") |>
    dplyr::summarize(
      .by = c(year, territory, sub_territory, livestock_category),
      total_demand = sum(demand_dm_t, na.rm = TRUE)
    ) |>
    dplyr::mutate(remaining_cap = total_demand)
  list(
    demand_remaining = stats::setNames(demand$remaining, demand$demand_id),
    avail_remaining = stats::setNames(avail$avail_dm_t, avail$avail_id),
    demand_cap_ledger = cap_ledger,
    allocations = list()
  )
}

.add_alloc <- function(state, rows, respect_cap = TRUE) {
  rows <- .filter_positive_intake(rows)
  if (nrow(rows) == 0) {
    return(state)
  }
  if (respect_cap) {
    capped <- .apply_cap_ledger(state$demand_cap_ledger, rows)
    state$demand_cap_ledger <- capped$ledger
    rows <- capped$rows
    rows <- .filter_positive_intake(rows)
    if (nrow(rows) == 0) {
      return(state)
    }
  }
  state$demand_remaining <- .decrement_remaining(
    state$demand_remaining,
    rows$demand_id,
    rows$intake_dm_t
  )
  state$avail_remaining <- .decrement_avail(state$avail_remaining, rows)
  state$allocations[[length(state$allocations) + 1L]] <- rows
  state
}

.filter_positive_intake <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    return(.empty_alloc())
  }
  rows[!is.na(rows$intake_dm_t) & rows$intake_dm_t > 1e-9, , drop = FALSE]
}

.apply_cap_ledger <- function(ledger, rows) {
  zoot <- rows$feed_quality == "zoot_fixed"
  uncapped <- rows[zoot, , drop = FALSE]
  capped <- rows[!zoot, , drop = FALSE]
  if (nrow(capped) == 0) {
    return(list(ledger = ledger, rows = uncapped))
  }
  capped <- .order_cap_rows(capped)
  key <- .cap_key(capped)
  idx <- match(key, .cap_key(ledger))
  cap_left <- ledger$remaining_cap[idx]
  prev <- stats::ave(capped$intake_dm_t, key, FUN = cumsum) - capped$intake_dm_t
  capped$intake_dm_t <- dplyr::if_else(
    is.na(cap_left),
    capped$intake_dm_t,
    pmax(0, pmin(capped$intake_dm_t, cap_left - prev))
  )
  capped <- capped[capped$intake_dm_t > 1e-9, , drop = FALSE]
  ledger <- .update_cap_ledger(ledger, capped)
  list(ledger = ledger, rows = dplyr::bind_rows(uncapped, capped))
}

.order_cap_rows <- function(capped) {
  rank <- .feed_quality_rank(capped$feed_quality)
  avail_tie <- if ("avail_id" %in% names(capped)) {
    dplyr::coalesce(as.numeric(capped$avail_id), Inf)
  } else {
    rep(Inf, nrow(capped))
  }
  ord <- order(
    capped$year,
    capped$territory,
    capped$sub_territory,
    capped$livestock_category,
    rank,
    -capped$intake_dm_t,
    capped$demand_id,
    avail_tie
  )
  capped[ord, , drop = FALSE]
}

.update_cap_ledger <- function(ledger, capped) {
  if (nrow(capped) == 0) {
    return(ledger)
  }
  key <- .cap_key(capped)
  alloc <- rowsum(capped$intake_dm_t, group = key, reorder = FALSE)
  idx <- match(rownames(alloc), .cap_key(ledger))
  ledger$remaining_cap[idx] <- pmax(0, ledger$remaining_cap[idx] - alloc[, 1])
  ledger
}

.cap_key <- function(df) {
  paste(
    df$year,
    df$territory,
    df$sub_territory,
    df$livestock_category,
    sep = "\001"
  )
}

.decrement_remaining <- function(remaining, demand_id, intake) {
  alloc <- rowsum(intake, group = demand_id, reorder = FALSE)
  ids <- rownames(alloc)
  remaining[ids] <- pmax(0, remaining[ids] - alloc[, 1])
  remaining
}

.decrement_avail <- function(avail_remaining, rows) {
  if (!"avail_id" %in% names(rows)) {
    return(avail_remaining)
  }
  rows <- rows[!is.na(rows$avail_id), , drop = FALSE]
  if (nrow(rows) == 0) {
    return(avail_remaining)
  }
  alloc <- rowsum(rows$intake_dm_t, group = rows$avail_id, reorder = FALSE)
  ids <- rownames(alloc)
  avail_remaining[ids] <- pmax(0, avail_remaining[ids] - alloc[, 1])
  avail_remaining
}

.feed_quality_rank <- function(feed_quality) {
  ranks <- c(
    lactation = 1,
    high_quality = 1,
    low_quality = 2,
    residues = 3,
    grass = 4
  )
  out <- unname(ranks[feed_quality])
  dplyr::coalesce(out, 999)
}

# ---- Cartesian allocator (provincial, proportional share) -------------------

.allocate_cartesian <- function(state, demand, avail, group_cols, level) {
  ds <- .live_demand(state, demand)
  as_ <- .live_avail(state, avail)
  if (nrow(ds) == 0 || nrow(as_) == 0) {
    return(.empty_alloc())
  }
  matched <- .cartesian_match_groups(ds, as_, group_cols)
  if (nrow(matched) == 0) {
    return(.empty_alloc())
  }
  shares <- .cartesian_compute_shares(ds, as_, matched, group_cols)
  .cartesian_assemble(shares$res, group_cols, level)
}

.cartesian_match_groups <- function(ds, as_, group_cols) {
  demand_dt <- data.table::as.data.table(ds)
  avail_dt <- data.table::as.data.table(as_)
  demand_groups <- demand_dt[,
    list(demand_group = sum(remaining, na.rm = TRUE)),
    by = group_cols
  ]
  avail_groups <- avail_dt[,
    list(avail_group = sum(avail_remaining, na.rm = TRUE)),
    by = group_cols
  ]
  matched <- demand_groups[avail_groups, on = group_cols, nomatch = NULL]
  tibble::as_tibble(matched[demand_group > 1e-9 & avail_group > 1e-9])
}

.cartesian_compute_shares <- function(ds, as_, matched, group_cols) {
  demand_dt <- data.table::as.data.table(ds)
  avail_dt <- data.table::as.data.table(as_)
  matched_dt <- data.table::as.data.table(matched)
  dw <- demand_dt[
    matched_dt,
    on = group_cols,
    allow.cartesian = TRUE,
    nomatch = NULL
  ]
  dw[, demand_share := remaining / demand_group]
  aw <- avail_dt[
    matched_dt,
    on = group_cols,
    allow.cartesian = TRUE,
    nomatch = NULL
  ]
  aw[, `:=`(
    avail_share = avail_remaining / avail_group,
    source_prov = sub_territory
  )]
  aw_small <- .rename_avail_side(aw, group_cols)
  if ("source_compartment" %in% names(dw)) {
    dw[, source_compartment := NULL]
  }
  res <- dw[aw_small, on = group_cols, allow.cartesian = TRUE, nomatch = NULL]
  res[,
    intake_dm_t := pmin(demand_group, avail_group) * demand_share * avail_share
  ]
  list(res = tibble::as_tibble(res[intake_dm_t > 1e-9]))
}

.rename_avail_side <- function(aw, group_cols) {
  item_cols <- c("item_cbs_code", "feed_group", "feed_quality")
  for (nm in setdiff(item_cols, group_cols)) {
    data.table::setnames(aw, nm, paste0(nm, "_sub"))
  }
  extra <- intersect(paste0(item_cols, "_sub"), names(aw))
  keep <- unique(c(group_cols, "avail_id", extra, "avail_share", "source_prov"))
  aw[, keep, with = FALSE]
}

.cartesian_assemble <- function(res, group_cols, level) {
  if (nrow(res) == 0) {
    return(.empty_alloc())
  }
  tibble::tibble(
    demand_id = res$demand_id,
    year = res$year,
    territory = res$territory,
    sub_territory = res$sub_territory,
    livestock_category = res$livestock_category,
    item_cbs_code = .pick_sub(res, "item_cbs_code"),
    feed_group = .pick_sub(res, "feed_group"),
    feed_quality = .pick_sub(res, "feed_quality"),
    intake_dm_t = res$intake_dm_t,
    hierarchy_level = level,
    requested_item = res$requested_item,
    source_compartment = res$source_prov,
    avail_id = res$avail_id
  )
}

.pick_sub <- function(res, col) {
  sub <- paste0(col, "_sub")
  if (sub %in% names(res)) {
    dplyr::coalesce(res[[sub]], res[[col]])
  } else {
    res[[col]]
  }
}

# ---- Grouped allocator (national scale) -------------------------------------

.allocate_grouped <- function(state, demand, avail, group_cols, level) {
  ds <- .live_demand(state, demand)
  as_ <- .live_avail(state, avail)
  if (nrow(ds) == 0 || nrow(as_) == 0) {
    return(.empty_alloc())
  }
  matched <- .cartesian_match_groups(ds, as_, group_cols)
  if (nrow(matched) == 0) {
    return(.empty_alloc())
  }
  res <- .grouped_compute(ds, as_, matched, group_cols)
  .cartesian_assemble(res, group_cols, level)
}

.grouped_compute <- function(ds, as_, matched, group_cols) {
  demand_dt <- data.table::as.data.table(ds)
  avail_dt <- data.table::as.data.table(as_)
  matched_dt <- data.table::as.data.table(matched)
  dw <- demand_dt[
    matched_dt,
    on = group_cols,
    allow.cartesian = TRUE,
    nomatch = NULL
  ]
  dw[, scale_factor := pmin(1, avail_group / demand_group)]
  dw[, intake_dm_t := remaining * scale_factor]
  aw <- avail_dt[
    matched_dt,
    on = group_cols,
    allow.cartesian = TRUE,
    nomatch = NULL
  ]
  aw[, `:=`(share = avail_remaining / avail_group, source_prov = sub_territory)]
  aw_small <- .rename_avail_side_grouped(aw, group_cols)
  if ("source_compartment" %in% names(dw)) {
    dw[, source_compartment := NULL]
  }
  res <- dw[aw_small, on = group_cols, allow.cartesian = TRUE, nomatch = NULL]
  res[, intake_dm_t := intake_dm_t * share]
  tibble::as_tibble(res[intake_dm_t > 1e-9])
}

.rename_avail_side_grouped <- function(aw, group_cols) {
  item_cols <- c("item_cbs_code", "feed_group", "feed_quality")
  for (nm in setdiff(item_cols, group_cols)) {
    data.table::setnames(aw, nm, paste0(nm, "_sub"))
  }
  extra <- intersect(paste0(item_cols, "_sub"), names(aw))
  keep <- unique(c(group_cols, "avail_id", extra, "share", "source_prov"))
  aw[, keep, with = FALSE]
}

# ---- Live views (apply remaining from state) --------------------------------

.live_demand <- function(state, demand) {
  demand$remaining <- unname(state$demand_remaining[as.character(
    demand$demand_id
  )])
  demand[
    !is.na(demand$remaining) &
      demand$remaining > 1e-9 &
      demand$feed_quality != "zoot_fixed",
    ,
    drop = FALSE
  ]
}

.live_avail <- function(state, avail) {
  avail$avail_remaining <- unname(state$avail_remaining[as.character(
    avail$avail_id
  )])
  avail[
    !is.na(avail$avail_remaining) & avail$avail_remaining > 1e-9,
    ,
    drop = FALSE
  ]
}

.empty_alloc <- function() {
  tibble::tibble(
    demand_id = integer(),
    year = integer(),
    territory = character(),
    sub_territory = character(),
    livestock_category = character(),
    item_cbs_code = integer(),
    feed_group = character(),
    feed_quality = character(),
    intake_dm_t = numeric(),
    hierarchy_level = character(),
    requested_item = integer(),
    source_compartment = character(),
    avail_id = integer()
  )
}

# ---- Level driver -----------------------------------------------------------

.run_allocation_levels <- function(state, demand, avail, mode) {
  state <- .run_primary_levels(state, demand, avail)
  if (mode %in% c("fixed", "mixed")) {
    state <- .allocate_grassland_sink(
      state,
      demand,
      only_fixed = mode == "mixed"
    )
  }
  state
}

.run_primary_levels <- function(state, demand, avail) {
  prov <- avail[avail$feed_scale == "provincial", , drop = FALSE]
  nat <- avail[avail$feed_scale == "national", , drop = FALSE]
  state <- .primary_pair(
    state,
    demand,
    prov,
    nat,
    "item_cbs_code",
    "1_item_exact"
  )
  state <- .primary_pair(
    state,
    demand,
    prov,
    nat,
    "feed_group",
    "2_feed_group_sub"
  )
  .primary_pair(state, demand, prov, nat, "feed_quality", "3_feed_quality_sub")
}

.primary_pair <- function(state, demand, prov, nat, key, level) {
  prov_cols <- c("year", "territory", "sub_territory", key)
  nat_cols <- c("year", "territory", key)
  alloc <- .allocate_cartesian(state, demand, prov, prov_cols, level)
  state <- .add_alloc(state, alloc)
  alloc <- .allocate_grouped(state, demand, nat, nat_cols, level)
  .add_alloc(state, alloc)
}

.allocate_grassland_sink <- function(state, demand, only_fixed = FALSE) {
  demand$remaining <- unname(state$demand_remaining[as.character(
    demand$demand_id
  )])
  target <- demand[
    !is.na(demand$remaining) &
      demand$remaining > 1e-9 &
      demand$feed_quality != "zoot_fixed",
    ,
    drop = FALSE
  ]
  if (only_fixed) {
    target <- target[target$fixed_demand, , drop = FALSE]
  }
  if (nrow(target) == 0) {
    return(state)
  }
  rows <- tibble::tibble(
    demand_id = target$demand_id,
    year = target$year,
    territory = target$territory,
    sub_territory = target$sub_territory,
    livestock_category = target$livestock_category,
    item_cbs_code = NA_integer_,
    feed_group = "grass",
    feed_quality = "grass",
    intake_dm_t = target$remaining,
    hierarchy_level = "6_grassland_unlimited",
    requested_item = target$requested_item,
    source_compartment = target$sub_territory,
    avail_id = NA_integer_
  )
  .add_alloc(state, rows)
}

# ---- Assembly ---------------------------------------------------------------

.assemble_result <- function(state, demand) {
  if (length(state$allocations) == 0) {
    return(.empty_redistribute_result())
  }
  result <- dplyr::bind_rows(state$allocations)
  result <- .add_zero_rows(result, demand)
  result$demand_dm_t <- demand$demand_dm_t[result$demand_id]
  result$fixed_demand <- demand$fixed_demand[result$demand_id]
  result$scaling_factor <- .compute_scaling(result, demand)
  result |>
    dplyr::select(
      year,
      territory,
      sub_territory,
      livestock_category,
      item_cbs_code,
      feed_group,
      feed_quality,
      demand_dm_t,
      intake_dm_t,
      scaling_factor,
      hierarchy_level,
      requested_item,
      source_compartment,
      fixed_demand
    ) |>
    dplyr::arrange(
      year,
      territory,
      sub_territory,
      livestock_category,
      requested_item,
      hierarchy_level,
      item_cbs_code,
      source_compartment
    )
}

.add_zero_rows <- function(result, demand) {
  missing_ids <- setdiff(demand$demand_id, result$demand_id)
  if (length(missing_ids) == 0) {
    return(result)
  }
  zero <- demand[demand$demand_id %in% missing_ids, , drop = FALSE] |>
    dplyr::transmute(
      demand_id,
      year,
      territory,
      sub_territory,
      livestock_category,
      item_cbs_code,
      feed_group,
      feed_quality,
      intake_dm_t = 0,
      hierarchy_level = "1_item_exact",
      requested_item,
      source_compartment
    )
  dplyr::bind_rows(result, zero)
}

.compute_scaling <- function(result, demand) {
  intake <- rowsum(
    result$intake_dm_t,
    group = result$demand_id,
    reorder = FALSE
  )
  ids <- as.integer(rownames(intake))
  tot <- intake[, 1]
  dm <- demand$demand_dm_t[ids]
  sf <- numeric(nrow(demand))
  sf[ids] <- dplyr::if_else(
    dm > 0,
    tot / dm,
    dplyr::if_else(tot > 0, NA_real_, 0)
  )
  sf[result$demand_id]
}

.empty_redistribute_result <- function() {
  tibble::tibble(
    year = integer(),
    territory = character(),
    sub_territory = character(),
    livestock_category = character(),
    item_cbs_code = integer(),
    feed_group = character(),
    feed_quality = character(),
    demand_dm_t = numeric(),
    intake_dm_t = numeric(),
    scaling_factor = numeric(),
    hierarchy_level = character(),
    requested_item = integer(),
    source_compartment = character(),
    fixed_demand = logical()
  )
}
