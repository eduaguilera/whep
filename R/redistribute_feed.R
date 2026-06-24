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
#'   defaults. Supply `grass_availability` (a tibble with `year`, `territory`
#'   or `area_code`, and `grass_avail_dm_t`) to bound the otherwise-unlimited
#'   pasture grass at that supply per polity-year. The grass deficit then
#'   cascades: pasture grass is capped at the ceiling, the deficit is
#'   redistributed to leftover non-grass availability in the polity (added as
#'   `7_grass_deficit_substitute` intake, limited by that leftover), and the
#'   residual stays as biologically-feasible underfeeding (`scaling_factor < 1`).
#'   Supply `maintenance_share` (a scalar fraction or a tibble with
#'   `livestock_category` and `maintenance_share`) to also diagnose polities
#'   pushed below maintenance; the over-stocked demand rows are attached to the
#'   result as the `grass_deficit_diagnosis` attribute.
#'
#' @return A tibble of realised intake per demand row. When `maintenance_share`
#'   is supplied alongside `grass_availability`, a `grass_deficit_diagnosis`
#'   attribute lists demand rows underfed below maintenance.
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
  state <- .apply_zoot_fixed(state, demand, avail, options)
  state <- .run_allocation_levels(state, demand, avail, mode)
  caps <- .resolve_max_intake_caps(options$max_intake_share)
  .assemble_result(
    state,
    demand,
    avail,
    mode,
    caps,
    options$grass_availability,
    options$maintenance_share
  )
}

.redistribute_feed_options <- function(options = list()) {
  defaults <- list(
    zoot_fixed_max_multiplier = 3,
    prioritize_monogastric = TRUE,
    territory_col = "territory",
    sub_territory_col = "sub_territory",
    monogastric = NULL,
    max_intake_share = NULL,
    grass_availability = NULL,
    maintenance_share = NULL,
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
  cap <- demand |>
    dplyr::filter(feed_quality != "zoot_fixed") |>
    dplyr::summarize(
      .by = c(year, territory, sub_territory, livestock_category),
      total_demand = sum(demand_dm_t, na.rm = TRUE)
    )
  list(
    # Plain (unnamed) vectors indexed by demand_id / avail_id, which are
    # dplyr::row_number()s and so equal their array position. Integer indexing
    # then costs O(rows-in-call); a named-vector lookup would match() over all
    # names (O(n)) on every call, which is O(n^2) across a run of many years.
    demand_remaining = demand$remaining,
    avail_remaining = avail$avail_dm_t,
    cap_remaining = cap$total_demand,
    # demand_id -> position in cap_remaining (one cap bucket per demand row), so
    # cap lookups/updates also index by integer position rather than by key.
    cap_pos_by_demand = match(.cap_key(demand), .cap_key(cap)),
    allocations = list()
  )
}

.add_alloc <- function(state, rows, respect_cap = TRUE) {
  rows <- .filter_positive_intake(rows)
  if (nrow(rows) == 0) {
    return(state)
  }
  if (respect_cap) {
    capped <- .apply_cap_ledger(
      state$cap_remaining,
      state$cap_pos_by_demand,
      rows
    )
    state$cap_remaining <- capped$cap_remaining
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

.apply_cap_ledger <- function(cap_remaining, cap_pos_by_demand, rows) {
  zoot <- rows$feed_quality == "zoot_fixed"
  uncapped <- rows[zoot, , drop = FALSE]
  capped <- rows[!zoot, , drop = FALSE]
  if (nrow(capped) == 0) {
    return(list(cap_remaining = cap_remaining, rows = uncapped))
  }
  capped <- .order_cap_rows(capped)
  pos <- cap_pos_by_demand[capped$demand_id]
  cap_left <- cap_remaining[pos]
  prev <- stats::ave(capped$intake_dm_t, pos, FUN = cumsum) - capped$intake_dm_t
  capped$intake_dm_t <- dplyr::if_else(
    is.na(cap_left),
    capped$intake_dm_t,
    pmax(0, pmin(capped$intake_dm_t, cap_left - prev))
  )
  capped <- capped[capped$intake_dm_t > 1e-9, , drop = FALSE]
  cap_remaining <- .update_cap_ledger(cap_remaining, cap_pos_by_demand, capped)
  list(cap_remaining = cap_remaining, rows = dplyr::bind_rows(uncapped, capped))
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

.update_cap_ledger <- function(cap_remaining, cap_pos_by_demand, capped) {
  if (nrow(capped) == 0) {
    return(cap_remaining)
  }
  pos <- cap_pos_by_demand[capped$demand_id]
  alloc <- rowsum(capped$intake_dm_t, group = pos, reorder = FALSE)
  ids <- as.integer(rownames(alloc))
  # Rows whose demand has no cap bucket (pos NA) are uncapped; skip them so the
  # update only touches real ledger positions (mirrors the prior match()-NA).
  known <- !is.na(ids)
  cap_remaining[ids[known]] <- pmax(
    0,
    cap_remaining[ids[known]] - alloc[known, 1]
  )
  cap_remaining
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
  ids <- as.integer(rownames(alloc))
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
  ids <- as.integer(rownames(alloc))
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

# ---- Zoot-fixed processing --------------------------------------------------

# Zoot-fixed items keep their requested intake (intake == demand) instead of
# being reallocated, but each row is capped at
# `zoot_fixed_max_multiplier * availability` to prevent extreme violations.
# When availability is zero or NA, no cap is applied. After allocation the
# zoot demand is fully consumed, so its `remaining` is zeroed in the state.
.apply_zoot_fixed <- function(state, demand, avail, options) {
  zoot <- demand[demand$feed_quality == "zoot_fixed", , drop = FALSE]
  if (nrow(zoot) == 0) {
    return(state)
  }
  zoot <- .zoot_match_avail(zoot, avail)
  rows <- .zoot_intake(zoot, options$zoot_fixed_max_multiplier)
  state <- .add_alloc(state, rows, respect_cap = FALSE)
  state$demand_remaining[zoot$demand_id] <- 0
  state
}

.zoot_match_avail <- function(zoot, avail) {
  prov <- avail[
    avail$feed_quality == "zoot_fixed" & avail$feed_scale == "provincial",
    c(
      "year",
      "territory",
      "sub_territory",
      "item_cbs_code",
      "avail_dm_t",
      "avail_id"
    ),
    drop = FALSE
  ]
  names(prov)[names(prov) %in% c("avail_dm_t", "avail_id")] <-
    c("avail_match", "avail_id")
  zoot |>
    dplyr::left_join(
      prov,
      by = c("year", "territory", "sub_territory", "item_cbs_code")
    )
}

.zoot_intake <- function(zoot, multiplier) {
  cap <- dplyr::if_else(
    is.na(zoot$avail_match) | zoot$avail_match <= 0,
    NA_real_,
    multiplier * zoot$avail_match
  )
  intake <- dplyr::if_else(
    is.na(cap) | zoot$demand_dm_t <= 0,
    zoot$demand_dm_t,
    pmin(zoot$demand_dm_t, cap)
  )
  tibble::tibble(
    demand_id = zoot$demand_id,
    year = zoot$year,
    territory = zoot$territory,
    sub_territory = zoot$sub_territory,
    livestock_category = zoot$livestock_category,
    item_cbs_code = zoot$item_cbs_code,
    feed_group = zoot$feed_group,
    feed_quality = zoot$feed_quality,
    intake_dm_t = intake,
    hierarchy_level = "1_item_exact",
    requested_item = zoot$requested_item,
    source_compartment = zoot$source_compartment,
    avail_id = zoot$avail_id
  )
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

# ---- Group splitting --------------------------------------------------------

# Allocation is independent per (year, territory). Splitting a table by that
# key once (O(n)) and indexing each group is what keeps the per-group loops
# linear; the previous `df[df$year == y & df$territory == t, ]` rescanned the
# whole multi-year table on every iteration (O(n^2) over a run of many years).
.group_key <- function(df) {
  paste(df$year, df$territory, sep = "\r")
}

.split_by_group <- function(df) {
  split(df, .group_key(df))
}

# ---- Live views (apply remaining from state) --------------------------------

.live_demand <- function(state, demand) {
  demand$remaining <- state$demand_remaining[demand$demand_id]
  demand[
    !is.na(demand$remaining) &
      demand$remaining > 1e-9 &
      demand$feed_quality != "zoot_fixed",
    ,
    drop = FALSE
  ]
}

.live_avail <- function(state, avail) {
  avail$avail_remaining <- state$avail_remaining[avail$avail_id]
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
  state <- .run_secondary_levels(state, demand, avail, mode)
  if (mode %in% c("fixed", "mixed")) {
    state <- .allocate_grassland_sink(
      state,
      demand,
      only_fixed = mode == "mixed"
    )
  }
  if (mode %in% c("variable", "mixed")) {
    state <- .distribute_surplus(
      state,
      demand,
      avail,
      only_variable = mode == "mixed"
    )
  }
  state
}

.run_secondary_levels <- function(state, demand, avail, mode) {
  pool_level <- if (mode == "variable") {
    "4_all_substitute"
  } else {
    "5_all_substitute"
  }
  if (mode %in% c("fixed", "mixed")) {
    state <- .allocate_trade(state, demand, avail)
  }
  state <- .allocate_priority_pool(
    state,
    demand,
    avail,
    pool_level,
    list(scope = "non_grass")
  )
  state <- .allocate_priority_pool(
    state,
    demand,
    avail,
    pool_level,
    list(scope = "grass")
  )
  .run_release_pass(state, demand, avail, pool_level)
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

# ---- Level 4: inter-provincial trade ----------------------------------------

.allocate_trade <- function(state, demand, avail) {
  trade_avail <- .trade_avail(state, avail)
  ds <- .live_demand(state, demand)
  ds <- ds[!is.na(ds$sub_territory), , drop = FALSE]
  if (nrow(trade_avail) == 0 || nrow(ds) == 0) {
    return(state)
  }
  group_cols <- c("year", "territory", "item_cbs_code")
  alloc <- .allocate_cartesian(
    state,
    ds,
    trade_avail,
    group_cols,
    "4_inter_prov_trade"
  )
  alloc <- .finalise_trade_alloc(alloc, trade_avail)
  .add_alloc(state, alloc)
}

.trade_avail <- function(state, avail) {
  live <- .live_avail(state, avail)
  live <- live[
    live$feed_scale == "provincial" &
      live$feed_quality != "grass" &
      !is.na(live$sub_territory),
    ,
    drop = FALSE
  ]
  if (nrow(live) == 0) {
    return(live)
  }
  live$trade_origin <- live$sub_territory
  live$sub_territory <- NA_character_
  live
}

.finalise_trade_alloc <- function(alloc, trade_avail) {
  if (nrow(alloc) == 0) {
    return(alloc)
  }
  origin <- stats::setNames(trade_avail$trade_origin, trade_avail$avail_id)
  alloc$source_compartment <- unname(origin[as.character(alloc$avail_id)])
  alloc[
    !is.na(alloc$source_compartment) &
      (is.na(alloc$sub_territory) |
        alloc$source_compartment != alloc$sub_territory),
    ,
    drop = FALSE
  ]
}

# ---- Level 5: priority pool (order-preserving cumulative allocation) ---------

.allocate_priority_pool <- function(
  state,
  demand,
  avail,
  label,
  options = list()
) {
  opts <- utils::modifyList(
    list(scope = "all", allow_trade = FALSE),
    options
  )
  pool <- .pool_avail(state, avail, opts)
  if (nrow(pool) == 0) {
    return(state)
  }
  pool_groups <- .split_by_group(pool)
  demand_groups <- .split_by_group(demand)
  for (key in names(pool_groups)) {
    supply <- pool_groups[[key]]
    ctx <- list(
      yr = supply$year[1L],
      terr = supply$territory[1L],
      label = label,
      opts = opts
    )
    state <- .priority_pool_group(state, demand_groups[[key]], supply, ctx)
  }
  state
}

.pool_avail <- function(state, avail, opts) {
  live <- .live_avail(state, avail)
  live <- live[live$feed_quality != "zoot_fixed", , drop = FALSE]
  if (opts$allow_trade) {
    live <- live[live$feed_group != "grass", , drop = FALSE]
  }
  if (opts$scope == "non_grass") {
    live <- live[live$feed_quality != "grass", , drop = FALSE]
  } else if (opts$scope == "grass") {
    live <- live[live$feed_quality == "grass", , drop = FALSE]
  }
  live
}

# `demand_grp` is this group's pre-split demand slice (or NULL when the group
# has no demand rows); `.live_demand` then works on the slice, not the full
# table.
.priority_pool_group <- function(state, demand_grp, supply, ctx) {
  if (is.null(demand_grp) || nrow(supply) == 0) {
    return(state)
  }
  ds <- .live_demand(state, demand_grp)
  if (nrow(ds) == 0) {
    return(state)
  }
  prov <- supply[supply$feed_scale == "provincial", , drop = FALSE]
  nat <- supply[supply$feed_scale == "national", , drop = FALSE]
  if (!ctx$opts$allow_trade && nrow(prov) > 0) {
    state <- .priority_pool_scope(state, ds, prov, ctx, "provincial")
    ds <- .live_demand(state, demand_grp)
  } else {
    nat <- supply
  }
  if (nrow(nat) > 0 && nrow(ds) > 0) {
    state <- .priority_pool_scope(state, ds, nat, ctx, "territory")
  }
  state
}

.priority_pool_scope <- function(state, ds, supply, ctx, scope) {
  if (scope == "provincial") {
    out <- purrr::map(
      unique(supply$sub_territory),
      function(p) {
        .pool_match_one(
          ds[!is.na(ds$sub_territory) & ds$sub_territory == p, , drop = FALSE],
          supply[supply$sub_territory == p, , drop = FALSE],
          ctx$label,
          p
        )
      }
    )
    alloc <- dplyr::bind_rows(out)
  } else {
    alloc <- .pool_match_one(ds, supply, ctx$label, NA_character_)
  }
  .add_alloc(state, .filter_positive_intake(alloc), respect_cap = TRUE)
}

# Order-preserving cumulative allocator. Availability is swept in priority
# order (feed_rank asc, then descending supply); demand is filled by priority
# buckets (rank 1 first). A non-equi interval overlap on the cumulative supply
# and cumulative demand-bucket axes assigns each availability slice to the
# bucket it covers; the per-bucket fill is then split across rows in proportion
# to remaining demand. Replaces the sequential row-loop in the afsetools origin
# while preserving its result (cumsum within priority order via `:=`).
.pool_match_one <- function(prov_demand, supply, label, source_prov) {
  if (nrow(prov_demand) == 0 || nrow(supply) == 0) {
    return(.empty_alloc())
  }
  supply_dt <- .pool_supply_axis(supply)
  bucket_dt <- .pool_bucket_axis(prov_demand)
  total_supply <- sum(supply$avail_remaining, na.rm = TRUE)
  bucket_dt <- bucket_dt[
    bucket_dt$bucket_lo < total_supply - 1e-12,
    ,
    drop = FALSE
  ]
  if (nrow(bucket_dt) == 0) {
    return(.empty_alloc())
  }
  bucket_dt$bucket_hi <- pmin(bucket_dt$bucket_hi, total_supply)
  contrib <- .pool_overlap(supply_dt, bucket_dt)
  if (nrow(contrib) == 0) {
    return(.empty_alloc())
  }
  .pool_expand_rows(contrib, prov_demand, label, source_prov)
}

.pool_supply_axis <- function(supply) {
  rank <- .feed_quality_rank(supply$feed_quality)
  ord <- order(rank, -supply$avail_remaining)
  s <- supply[ord, , drop = FALSE]
  hi <- cumsum(s$avail_remaining)
  tibble::tibble(
    avail_id = s$avail_id,
    item_cbs_code = s$item_cbs_code,
    feed_group = s$feed_group,
    feed_quality = s$feed_quality,
    supply_lo = hi - s$avail_remaining,
    supply_hi = hi
  )
}

.pool_bucket_axis <- function(prov_demand) {
  rank <- .feed_quality_rank(prov_demand$feed_quality)
  bucket <- tibble::tibble(
    priority = rank,
    remaining = prov_demand$remaining
  ) |>
    dplyr::summarise(
      bucket_total = sum(remaining, na.rm = TRUE),
      .by = priority
    ) |>
    dplyr::arrange(priority)
  hi <- cumsum(bucket$bucket_total)
  bucket$bucket_lo <- hi - bucket$bucket_total
  bucket$bucket_hi <- hi
  bucket
}

.pool_overlap <- function(supply_dt, bucket_dt) {
  s <- data.table::as.data.table(supply_dt)
  b <- data.table::as.data.table(bucket_dt)
  data.table::setkey(s, supply_lo, supply_hi)
  joined <- data.table::foverlaps(
    b,
    s,
    by.x = c("bucket_lo", "bucket_hi"),
    by.y = c("supply_lo", "supply_hi"),
    type = "any",
    nomatch = NULL
  )
  joined[,
    contribution := pmin(supply_hi, bucket_hi) - pmax(supply_lo, bucket_lo)
  ]
  joined <- joined[contribution > 1e-9]
  tibble::as_tibble(joined)
}

.pool_expand_rows <- function(contrib, prov_demand, label, source_prov) {
  prov_demand$priority <- .feed_quality_rank(prov_demand$feed_quality)
  bucket_total <- prov_demand |>
    dplyr::summarise(
      bucket_total = sum(remaining, na.rm = TRUE),
      .by = priority
    )
  rows <- prov_demand |>
    dplyr::left_join(bucket_total, by = "priority") |>
    dplyr::inner_join(
      contrib |>
        dplyr::select(
          priority,
          avail_id,
          sub_item = item_cbs_code,
          sub_group = feed_group,
          sub_quality = feed_quality,
          contribution
        ),
      by = "priority",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      intake_dm_t = contribution * remaining / bucket_total
    )
  .pool_assemble(rows, label, source_prov)
}

.pool_assemble <- function(rows, label, source_prov) {
  rows <- rows[rows$intake_dm_t > 1e-9, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(.empty_alloc())
  }
  tibble::tibble(
    demand_id = rows$demand_id,
    year = rows$year,
    territory = rows$territory,
    sub_territory = rows$sub_territory,
    livestock_category = rows$livestock_category,
    item_cbs_code = rows$sub_item,
    feed_group = rows$sub_group,
    feed_quality = rows$sub_quality,
    intake_dm_t = rows$intake_dm_t,
    hierarchy_level = label,
    requested_item = rows$requested_item,
    source_compartment = source_prov,
    avail_id = rows$avail_id
  )
}

# ---- Release pass: trade leftover non-grass supply across provinces ----------

.run_release_pass <- function(state, demand, avail, label) {
  ds <- .live_demand(state, demand)
  releasable <- .live_avail(state, avail)
  releasable <- releasable[
    releasable$feed_quality != "grass" &
      releasable$feed_quality != "zoot_fixed",
    ,
    drop = FALSE
  ]
  if (nrow(ds) == 0 || nrow(releasable) == 0) {
    return(state)
  }
  .allocate_priority_pool(
    state,
    demand,
    avail,
    label,
    list(scope = "all", allow_trade = TRUE)
  )
}

# ---- Surplus distribution (variable mode): intake may exceed demand ----------

.distribute_surplus <- function(state, demand, avail, only_variable = FALSE) {
  surplus <- .live_avail(state, avail)
  surplus <- surplus[surplus$feed_quality != "zoot_fixed", , drop = FALSE]
  if (nrow(surplus) == 0) {
    return(state)
  }
  surplus_groups <- .split_by_group(surplus)
  demand_groups <- .split_by_group(demand)
  for (key in names(surplus_groups)) {
    state <- .surplus_group(
      state,
      demand_groups[[key]],
      surplus_groups[[key]],
      only_variable
    )
  }
  state
}

# `demand_grp`/`items` are this group's pre-split demand and surplus slices.
.surplus_group <- function(state, demand_grp, items, only_variable) {
  if (is.null(demand_grp) || nrow(items) == 0) {
    return(state)
  }
  ds <- demand_grp[
    demand_grp$demand_dm_t > 0 & demand_grp$feed_quality != "zoot_fixed",
    ,
    drop = FALSE
  ]
  if (only_variable) {
    ds <- ds[!ds$fixed_demand, , drop = FALSE]
  }
  if (nrow(ds) == 0) {
    return(state)
  }
  alloc <- .surplus_alloc(items, ds)
  .add_alloc(state, .filter_positive_intake(alloc), respect_cap = FALSE)
}

.surplus_alloc <- function(items, ds) {
  out <- purrr::map(
    seq_len(nrow(items)),
    function(i) .surplus_one_item(items[i, , drop = FALSE], ds)
  )
  dplyr::bind_rows(out)
}

.surplus_one_item <- function(item, ds) {
  eligible <- if (item$feed_scale == "provincial") {
    ds[
      !is.na(ds$sub_territory) & ds$sub_territory == item$sub_territory,
      ,
      drop = FALSE
    ]
  } else {
    ds
  }
  total_weight <- sum(eligible$demand_dm_t, na.rm = TRUE)
  if (nrow(eligible) == 0 || total_weight <= 1e-9) {
    return(.empty_alloc())
  }
  tibble::tibble(
    demand_id = eligible$demand_id,
    year = eligible$year,
    territory = eligible$territory,
    sub_territory = eligible$sub_territory,
    livestock_category = eligible$livestock_category,
    item_cbs_code = item$item_cbs_code,
    feed_group = item$feed_group,
    feed_quality = item$feed_quality,
    intake_dm_t = item$avail_remaining * eligible$demand_dm_t / total_weight,
    hierarchy_level = "5_surplus_distribution",
    requested_item = eligible$requested_item,
    source_compartment = item$sub_territory,
    avail_id = item$avail_id
  )
}

.allocate_grassland_sink <- function(state, demand, only_fixed = FALSE) {
  demand$remaining <- state$demand_remaining[demand$demand_id]
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

.assemble_result <- function(
  state,
  demand,
  avail,
  mode,
  caps,
  grass_availability = NULL,
  maintenance_share = NULL
) {
  if (length(state$allocations) == 0) {
    return(.empty_redistribute_result())
  }
  result <- dplyr::bind_rows(state$allocations)
  result <- .add_zero_rows(result, demand)
  result <- .reroute_excess_to_grass(result, avail, mode)
  live_avail <- .avail_with_remaining(state, avail)
  if (!is.null(grass_availability)) {
    result <- .grass_deficit_cascade(result, grass_availability, live_avail)
  }
  result <- .apply_max_intake_caps(result, live_avail, caps)
  result$demand_dm_t <- demand$demand_dm_t[result$demand_id]
  result$fixed_demand <- demand$fixed_demand[result$demand_id]
  result$scaling_factor <- .compute_scaling(result, demand)
  diagnosis <- NULL
  if (!is.null(grass_availability) && !is.null(maintenance_share)) {
    diagnosis <- .grass_deficit_diagnosis(result, demand, maintenance_share)
  }
  out <- result |>
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
  if (!is.null(diagnosis)) {
    attr(out, "grass_deficit_diagnosis") <- diagnosis
  }
  out
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

# ---- Reroute grass intake exceeding availability to the unlimited sink ------

# In fixed/mixed modes the primary levels can over-allocate a finite grass
# item (the priority pool fills demand buckets and may exceed an item's
# availability). Any grass-item intake above its availability is moved to the
# unlimited grassland sink (item_cbs_code = NA), preserving conservation.
.reroute_excess_to_grass <- function(result, avail, mode) {
  if (mode == "variable") {
    return(result)
  }
  limits <- .grass_avail_limits(avail)
  if (nrow(limits) == 0) {
    return(result)
  }
  viol <- .grass_violations(result, limits)
  if (nrow(viol) == 0) {
    return(result)
  }
  .split_grass_excess(result, viol)
}

.grass_avail_limits <- function(avail) {
  grass <- avail[
    avail$feed_quality == "grass" & !is.na(avail$item_cbs_code),
    ,
    drop = FALSE
  ]
  if (nrow(grass) == 0) {
    return(grass[, c("year", "territory", "item_cbs_code"), drop = FALSE])
  }
  grass |>
    dplyr::summarise(
      avail_limit = sum(avail_dm_t, na.rm = TRUE),
      .by = c(year, territory, item_cbs_code)
    )
}

.grass_violations <- function(result, limits) {
  intake <- result[
    result$feed_quality == "grass" & !is.na(result$item_cbs_code),
    ,
    drop = FALSE
  ] |>
    dplyr::summarise(
      total_intake = sum(intake_dm_t, na.rm = TRUE),
      .by = c(year, territory, item_cbs_code)
    )
  intake |>
    dplyr::left_join(limits, by = c("year", "territory", "item_cbs_code")) |>
    dplyr::mutate(
      avail_limit = dplyr::coalesce(avail_limit, 0),
      excess = total_intake - avail_limit
    ) |>
    dplyr::filter(excess > 1e-6)
}

.split_grass_excess <- function(result, viol) {
  result$row_id <- seq_len(nrow(result))
  grass_rows <- result[
    result$feed_quality == "grass" & !is.na(result$item_cbs_code),
    ,
    drop = FALSE
  ] |>
    dplyr::inner_join(
      viol[, c("year", "territory", "item_cbs_code", "total_intake", "excess")],
      by = c("year", "territory", "item_cbs_code")
    ) |>
    dplyr::mutate(
      reduction = pmax(
        0,
        pmin(
          intake_dm_t,
          intake_dm_t / total_intake * pmin(excess, total_intake)
        )
      )
    )
  result$intake_dm_t[grass_rows$row_id] <-
    pmax(0, result$intake_dm_t[grass_rows$row_id] - grass_rows$reduction)
  additions <- .grass_sink_rows(
    result[grass_rows$row_id, ],
    grass_rows$reduction
  )
  result$row_id <- NULL
  dplyr::bind_rows(result, additions)
}

.grass_sink_rows <- function(rows, reduction) {
  rows$intake_dm_t <- reduction
  rows$item_cbs_code <- NA_integer_
  rows$feed_group <- "grass"
  rows$feed_quality <- "grass"
  rows$hierarchy_level <- "6_grassland_unlimited"
  rows$source_compartment <- rows$sub_territory
  rows$row_id <- NULL
  if ("avail_id" %in% names(rows)) {
    rows$avail_id <- NA_integer_
  }
  rows[rows$intake_dm_t > 1e-9, , drop = FALSE]
}

# ---- Bounded grass deficit cascade -----------------------------------------

# With options$grass_availability supplied, the otherwise-unlimited pasture
# grass (item_cbs_code = NA, "6_grassland_unlimited") is bounded by the LPJmL
# grass supply per (territory, year). The grass deficit then cascades:
#   1. cap pasture grass at the ceiling (excess removed pro-rata),
#   2. redistribute the deficit to leftover non-grass availability in the same
#      polity (limited by it), added as substitution intake,
#   3. the residual is biologically-feasible underfeeding (intake < demand, so
#      scaling_factor < 1).
# A separate maintenance diagnosis (.grass_deficit_diagnosis) flags polities
# pushed below maintenance.
.grass_deficit_cascade <- function(result, grass_availability, live_avail) {
  capped <- .cap_grass_to_availability(
    result,
    grass_availability,
    return_deficit = TRUE
  )
  result <- capped$result
  deficit <- capped$deficit
  if (is.null(deficit) || nrow(deficit) == 0) {
    return(result)
  }
  .redistribute_grass_deficit(result, deficit, live_avail)
}

# Cap the pasture grass at the polity ceiling, removing the excess pro-rata.
# With return_deficit = TRUE, also return the per-grazer reduction (the grass
# deficit) used by the redistribution step.
.cap_grass_to_availability <- function(
  result,
  grass_availability,
  return_deficit = FALSE
) {
  is_pasture <- result$feed_quality == "grass" & is.na(result$item_cbs_code)
  if (!any(is_pasture)) {
    return(.cap_return(result, NULL, return_deficit))
  }
  keys <- .grass_cap_keys(grass_availability)
  result$row_id <- seq_len(nrow(result))
  pasture <- result[is_pasture, , drop = FALSE]
  totals <- pasture |>
    dplyr::summarise(
      total = sum(intake_dm_t, na.rm = TRUE),
      .by = dplyr::all_of(keys)
    ) |>
    dplyr::inner_join(
      .grass_avail_keyed(grass_availability, keys),
      by = keys
    ) |>
    dplyr::mutate(excess = pmax(0, total - grass_avail_dm_t)) |>
    dplyr::filter(excess > 1e-6)
  if (nrow(totals) == 0) {
    result$row_id <- NULL
    return(.cap_return(result, NULL, return_deficit))
  }
  cut <- pasture |>
    dplyr::inner_join(
      totals[, c(keys, "total", "excess")],
      by = keys
    ) |>
    dplyr::mutate(reduction = intake_dm_t / total * excess)
  result$intake_dm_t[cut$row_id] <- pmax(
    0,
    result$intake_dm_t[cut$row_id] - cut$reduction
  )
  result$row_id <- NULL
  deficit <- NULL
  if (return_deficit) {
    deficit <- dplyr::select(
      cut,
      demand_id,
      year,
      territory,
      sub_territory,
      livestock_category,
      reduction
    )
  }
  .cap_return(result, deficit, return_deficit)
}

.cap_return <- function(result, deficit, return_deficit) {
  if (return_deficit) {
    list(result = result, deficit = deficit)
  } else {
    result
  }
}

# Redistribute the grass deficit to leftover non-grass availability in the same
# polity (year, territory), pro-rata across the deficit grazers and capped at
# that leftover. The filled portion is added as substitution intake; the rest
# stays as underfeeding.
.redistribute_grass_deficit <- function(result, deficit, live_avail) {
  leftover <- .leftover_nongrass(live_avail)
  if (nrow(leftover) == 0) {
    return(result)
  }
  fill <- deficit |>
    dplyr::left_join(leftover, by = c("year", "territory")) |>
    dplyr::mutate(leftover = dplyr::coalesce(leftover, 0)) |>
    dplyr::mutate(
      terr_deficit = sum(reduction, na.rm = TRUE),
      .by = c(year, territory)
    ) |>
    dplyr::mutate(
      substitute = dplyr::if_else(
        terr_deficit > 0,
        reduction / terr_deficit * pmin(leftover, terr_deficit),
        0
      )
    ) |>
    dplyr::filter(substitute > 1e-9)
  if (nrow(fill) == 0) {
    return(result)
  }
  dplyr::bind_rows(result, .grass_substitute_rows(fill))
}

# Leftover (unallocated) non-grass availability per polity (year, territory).
# The prepared availability already carries `territory`, so it is grouped
# directly (no sub_territory -> territory remapping needed).
.leftover_nongrass <- function(live_avail) {
  empty <- tibble::tibble(
    year = integer(),
    territory = character(),
    leftover = numeric()
  )
  if (is.null(live_avail) || nrow(live_avail) == 0) {
    return(empty)
  }
  live_avail |>
    dplyr::filter(feed_quality != "grass", avail_remaining > 1e-9) |>
    dplyr::summarise(
      leftover = sum(avail_remaining, na.rm = TRUE),
      .by = c(year, territory)
    )
}

# Build the substitution intake rows that fill part of the grass deficit.
.grass_substitute_rows <- function(fill) {
  tibble::tibble(
    demand_id = fill$demand_id,
    year = fill$year,
    territory = fill$territory,
    sub_territory = fill$sub_territory,
    livestock_category = fill$livestock_category,
    item_cbs_code = NA_integer_,
    feed_group = "substitute",
    feed_quality = "substitute",
    intake_dm_t = fill$substitute,
    hierarchy_level = "7_grass_deficit_substitute",
    requested_item = NA_integer_,
    source_compartment = fill$sub_territory,
    avail_id = NA_integer_
  )
}

# Diagnose polities pushed below maintenance: total intake < demand x
# maintenance_share. `maintenance_share` is a scalar fraction or a tibble
# (livestock_category, maintenance_share). Returns the over-stocked demand rows.
.grass_deficit_diagnosis <- function(result, demand, maintenance_share) {
  share <- .maintenance_share_lookup(demand, maintenance_share)
  result |>
    dplyr::summarise(
      intake_dm_t = sum(intake_dm_t, na.rm = TRUE),
      .by = demand_id
    ) |>
    dplyr::inner_join(share, by = "demand_id") |>
    dplyr::mutate(maintenance_dm_t = demand_dm_t * maintenance_share) |>
    dplyr::filter(intake_dm_t < maintenance_dm_t - 1e-9) |>
    dplyr::select(
      demand_id,
      livestock_category,
      demand_dm_t,
      maintenance_dm_t,
      intake_dm_t
    )
}

# Per-demand maintenance share (scalar broadcast or per-category join).
.maintenance_share_lookup <- function(demand, maintenance_share) {
  base <- tibble::tibble(
    demand_id = demand$demand_id,
    livestock_category = demand$livestock_category,
    demand_dm_t = demand$demand_dm_t
  )
  if (is.numeric(maintenance_share) && length(maintenance_share) == 1L) {
    base$maintenance_share <- maintenance_share
    return(base)
  }
  ms <- tibble::as_tibble(maintenance_share)
  base |>
    dplyr::left_join(ms, by = "livestock_category") |>
    dplyr::mutate(maintenance_share = dplyr::coalesce(maintenance_share, 0))
}

# Grass binds per cell when supplied per sub_territory (local grain), else
# per polity (national grain). The cap key follows the grain of the supplied
# grass availability. When a sub_territory is present the polity is kept in the
# key too (year, territory, sub_territory) so a 0.5-degree border cell shared by
# two polities is not conflated against a single ceiling; if no polity column is
# supplied it falls back to (year, sub_territory).
.grass_cap_keys <- function(grass_availability) {
  ga <- tibble::as_tibble(grass_availability)
  has_sub <- rlang::has_name(ga, "sub_territory") &&
    !all(is.na(ga$sub_territory))
  if (!has_sub) {
    return(c("year", "territory"))
  }
  has_terr <- rlang::has_name(ga, "territory") ||
    rlang::has_name(ga, "area_code")
  if (has_terr) {
    c("year", "territory", "sub_territory")
  } else {
    c("year", "sub_territory")
  }
}

# Normalise supplied grass availability to (keys, grass_avail_dm_t), where keys
# is the cap grain from `.grass_cap_keys` (per territory or per sub_territory).
.grass_avail_keyed <- function(grass_availability, keys) {
  ga <- tibble::as_tibble(grass_availability)
  if ("area_code" %in% names(ga) && !("territory" %in% names(ga))) {
    ga$territory <- ga$area_code
  }
  if ("territory" %in% names(ga)) {
    ga$territory <- as.character(ga$territory)
  }
  if ("sub_territory" %in% names(ga)) {
    ga$sub_territory <- as.character(ga$sub_territory)
  }
  dplyr::summarise(
    ga,
    grass_avail_dm_t = sum(grass_avail_dm_t, na.rm = TRUE),
    .by = dplyr::all_of(keys)
  )
}

# ---- Max-intake caps (per item_cbs_code and per feed_quality) ---------------

.resolve_max_intake_caps <- function(max_intake_share) {
  if (is.null(max_intake_share)) {
    return(.empty_caps())
  }
  caps <- tibble::as_tibble(max_intake_share)
  if (nrow(caps) == 0) {
    return(.empty_caps())
  }
  caps |>
    dplyr::transmute(
      livestock_category = as.character(livestock_category),
      var = as.character(var),
      var_value = as.character(var_value),
      max_intake_share = pmin(pmax(max_intake_share, 0), 1)
    ) |>
    dplyr::filter(is.finite(max_intake_share)) |>
    dplyr::summarise(
      max_intake_share = max(max_intake_share, na.rm = TRUE),
      .by = c(livestock_category, var, var_value)
    )
}

.empty_caps <- function() {
  tibble::tibble(
    livestock_category = character(),
    var = character(),
    var_value = character(),
    max_intake_share = numeric()
  )
}

# Reduce any (livestock_category, item or feed_quality) intake that exceeds
# its allowed share of the livestock's diet, then reroute the freed DM to the
# unlimited grassland sink so conservation per demand row is preserved. The
# strict-share limit keeps the post-reduction share at or below the cap.
.apply_max_intake_caps <- function(result, live_avail, caps) {
  if (nrow(caps) == 0) {
    return(result)
  }
  totals <- result |>
    dplyr::summarise(
      total_dm = sum(intake_dm_t, na.rm = TRUE),
      .by = c(year, territory, sub_territory, livestock_category)
    )
  item_viol <- .item_cap_violations(result, caps, totals)
  qual_viol <- .quality_cap_violations(result, caps, totals)
  if (nrow(item_viol) == 0 && nrow(qual_viol) == 0) {
    return(result)
  }
  result <- .reduce_item_caps(result, item_viol)
  result <- .reduce_quality_caps(result, qual_viol)
  freed <- .freed_excess(item_viol, qual_viol)
  result <- .reroute_freed_excess(result, freed, live_avail, caps)
  result[result$intake_dm_t > 1e-9, , drop = FALSE]
}

.avail_with_remaining <- function(state, avail) {
  avail$avail_remaining <- state$avail_remaining[avail$avail_id]
  avail
}

# Freed DM goes to the unlimited grassland sink unless grass itself is capped
# for the livestock; in that case it draws from remaining non-grass
# availability (priority order) and any leftover is dropped (the strict cap
# wins over conservation, matching the afsetools Phase-3 behaviour).
.reroute_freed_excess <- function(result, freed, live_avail, caps) {
  if (nrow(freed) == 0) {
    return(result)
  }
  grass_capped_lc <- caps$livestock_category[
    caps$var == "feed_quality" & caps$var_value == "grass"
  ]
  to_grass <- freed[
    !freed$livestock_category %in% grass_capped_lc,
    ,
    drop = FALSE
  ]
  to_other <- freed[
    freed$livestock_category %in% grass_capped_lc,
    ,
    drop = FALSE
  ]
  result <- dplyr::bind_rows(result, .cap_grass_rows(result, to_grass))
  dplyr::bind_rows(result, .cap_nongrass_rows(result, to_other, live_avail))
}

.strict_limit <- function(other_dm, max_share) {
  dplyr::if_else(
    max_share >= 1 - 1e-9,
    Inf,
    other_dm * max_share / pmax(1 - max_share, 1e-9)
  )
}

.item_cap_violations <- function(result, caps, totals) {
  item_caps <- caps[caps$var == "item_cbs_code", , drop = FALSE]
  if (nrow(item_caps) == 0) {
    return(.empty_viol("item_cbs_code"))
  }
  item_caps$item_cbs_code <- suppressWarnings(as.integer(item_caps$var_value))
  result |>
    dplyr::summarise(
      group_dm = sum(intake_dm_t, na.rm = TRUE),
      .by = c(year, territory, sub_territory, livestock_category, item_cbs_code)
    ) |>
    dplyr::inner_join(
      item_caps[, c("livestock_category", "item_cbs_code", "max_intake_share")],
      by = c("livestock_category", "item_cbs_code")
    ) |>
    .finalise_violation(totals)
}

.quality_cap_violations <- function(result, caps, totals) {
  qual_caps <- caps[caps$var == "feed_quality", , drop = FALSE]
  if (nrow(qual_caps) == 0) {
    return(.empty_viol("feed_quality"))
  }
  qual_caps$feed_quality <- qual_caps$var_value
  result |>
    dplyr::summarise(
      group_dm = sum(intake_dm_t, na.rm = TRUE),
      .by = c(year, territory, sub_territory, livestock_category, feed_quality)
    ) |>
    dplyr::inner_join(
      qual_caps[, c("livestock_category", "feed_quality", "max_intake_share")],
      by = c("livestock_category", "feed_quality")
    ) |>
    .finalise_violation(totals)
}

.finalise_violation <- function(grouped, totals) {
  grouped |>
    dplyr::left_join(
      totals,
      by = c("year", "territory", "sub_territory", "livestock_category")
    ) |>
    dplyr::mutate(
      total_dm = dplyr::coalesce(total_dm, 0),
      other_dm = total_dm - group_dm,
      limit_dm = .strict_limit(other_dm, max_intake_share),
      excess = group_dm - limit_dm
    ) |>
    dplyr::filter(is.finite(excess), excess > 1e-6)
}

.empty_viol <- function(key) {
  base <- tibble::tibble(
    year = integer(),
    territory = character(),
    sub_territory = character(),
    livestock_category = character(),
    group_dm = numeric(),
    max_intake_share = numeric(),
    total_dm = numeric(),
    other_dm = numeric(),
    limit_dm = numeric(),
    excess = numeric()
  )
  base[[key]] <- if (key == "item_cbs_code") integer() else character()
  base
}

.reduce_item_caps <- function(result, viol) {
  if (nrow(viol) == 0) {
    return(result)
  }
  .reduce_by_key(
    result,
    viol,
    c(
      "year",
      "territory",
      "sub_territory",
      "livestock_category",
      "item_cbs_code"
    )
  )
}

.reduce_quality_caps <- function(result, viol) {
  if (nrow(viol) == 0) {
    return(result)
  }
  .reduce_by_key(
    result,
    viol,
    c(
      "year",
      "territory",
      "sub_territory",
      "livestock_category",
      "feed_quality"
    )
  )
}

.reduce_by_key <- function(result, viol, keys) {
  result$row_id <- seq_len(nrow(result))
  matches <- result |>
    dplyr::inner_join(
      viol[, c(keys, "group_dm", "excess")],
      by = keys
    ) |>
    dplyr::mutate(
      factor = pmax(0, (group_dm - excess) / group_dm),
      new_intake = intake_dm_t * factor
    )
  result$intake_dm_t[matches$row_id] <- matches$new_intake
  result$row_id <- NULL
  result
}

.cap_excess_rows <- function(v) {
  if (nrow(v) == 0) {
    return(NULL)
  }
  v |>
    dplyr::transmute(
      year,
      territory,
      sub_territory,
      livestock_category,
      capped_excess = pmin(excess, group_dm)
    )
}

.freed_excess <- function(item_viol, qual_viol) {
  dplyr::bind_rows(
    .cap_excess_rows(item_viol),
    .cap_excess_rows(qual_viol)
  ) |>
    dplyr::summarise(
      total_excess = sum(capped_excess, na.rm = TRUE),
      .by = c(year, territory, sub_territory, livestock_category)
    ) |>
    dplyr::filter(total_excess > 1e-9)
}

.cap_grass_rows <- function(result, freed) {
  if (nrow(freed) == 0) {
    return(.empty_alloc())
  }
  donors <- result |>
    dplyr::slice_head(
      n = 1L,
      by = c(
        year,
        territory,
        sub_territory,
        livestock_category
      )
    )
  donors |>
    dplyr::inner_join(
      freed,
      by = c("year", "territory", "sub_territory", "livestock_category")
    ) |>
    dplyr::transmute(
      demand_id,
      year,
      territory,
      sub_territory,
      livestock_category,
      item_cbs_code = NA_integer_,
      feed_group = "grass",
      feed_quality = "grass",
      intake_dm_t = total_excess,
      hierarchy_level = "6_grassland_unlimited",
      requested_item,
      source_compartment = sub_territory,
      avail_id = NA_integer_
    )
}

# Route freed DM to remaining non-grass availability within the same
# compartment, priority order, capped by what is left; leftover is dropped.
.cap_nongrass_rows <- function(result, freed, live_avail) {
  if (nrow(freed) == 0) {
    return(.empty_alloc())
  }
  supply <- live_avail[
    live_avail$feed_quality != "grass" &
      live_avail$feed_quality != "zoot_fixed" &
      !is.na(live_avail$avail_remaining) &
      live_avail$avail_remaining > 1e-9,
    ,
    drop = FALSE
  ]
  if (nrow(supply) == 0) {
    return(.empty_alloc())
  }
  donors <- result |>
    dplyr::slice_head(
      n = 1L,
      by = c(
        year,
        territory,
        sub_territory,
        livestock_category
      )
    )
  matched <- .match_nongrass_supply(freed, supply)
  .nongrass_alloc_rows(matched, donors)
}

.match_nongrass_supply <- function(freed, supply) {
  supply$rank <- .feed_quality_rank(supply$feed_quality)
  freed |>
    dplyr::inner_join(
      supply[, c(
        "year",
        "territory",
        "avail_id",
        "item_cbs_code",
        "feed_group",
        "feed_quality",
        "avail_remaining",
        "rank"
      )],
      by = c("year", "territory"),
      relationship = "many-to-many"
    ) |>
    dplyr::arrange(
      year,
      territory,
      sub_territory,
      livestock_category,
      rank,
      avail_id
    ) |>
    dplyr::mutate(
      prev = cumsum(avail_remaining) - avail_remaining,
      alloc = pmax(0, pmin(avail_remaining, total_excess - prev)),
      .by = c(year, territory, sub_territory, livestock_category)
    ) |>
    dplyr::filter(alloc > 1e-9)
}

.nongrass_alloc_rows <- function(matched, donors) {
  if (nrow(matched) == 0) {
    return(.empty_alloc())
  }
  matched |>
    dplyr::inner_join(
      donors[, c(
        "year",
        "territory",
        "sub_territory",
        "livestock_category",
        "demand_id",
        "requested_item"
      )],
      by = c("year", "territory", "sub_territory", "livestock_category")
    ) |>
    dplyr::transmute(
      demand_id,
      year,
      territory,
      sub_territory,
      livestock_category,
      item_cbs_code,
      feed_group,
      feed_quality,
      intake_dm_t = alloc,
      hierarchy_level = "6_grassland_unlimited",
      requested_item,
      source_compartment = sub_territory,
      avail_id = NA_integer_
    )
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
