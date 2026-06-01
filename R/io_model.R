#' Build multi-regional input-output model.
#'
#' @description
#' Construct a multi-regional input-output (MRIO) model from
#' supply-use tables, bilateral trade, and commodity balance
#' sheets. Uses the industry technology assumption to derive
#' symmetric product-by-product tables.
#'
#' The resulting matrices follow the FABIO methodology
#' (Bruckner et al., 2019). Rows and columns of `Z` represent
#' (country, item) pairs. Each entry `Z[i,j]` gives the
#' intermediate flow from sector `i` to sector `j`.
#'
#' @param supply_use Tibble from [build_supply_use()]. By default,
#'   this function calls [build_supply_use()] internally. Must have
#'   columns: `year`, `area_code`, `proc_group`, `proc_cbs_code`,
#'   `item_cbs_code`, `type`, `value`.
#' @param bilateral_trade Tibble from [get_bilateral_trade()]. By
#'   default, this function calls [get_bilateral_trade()]
#'   internally. Must have columns: `year`, `item_cbs_code`,
#'   `bilateral_trade` (list-column of matrices).
#' @param cbs Tibble from [get_wide_cbs()]. By default, this
#'   function calls [get_wide_cbs()] internally. Must have
#'   columns: `year`, `area_code`, `item_cbs_code`, `production`,
#'   `import`, `export`, `stock_withdrawal`, `stock_addition`,
#'   plus final demand columns (`food`, `other_uses`).
#' @param years Numeric vector of years to compute, or NULL.
#'   If NULL, computes all years in the intersection of
#'   available data across inputs. If specified, must be
#'   a subset of available years.
#' @param endogenize_losses Logical. If `TRUE` and `cbs`
#'   contains a `losses` column, losses are moved from final
#'   demand to the diagonal of `Z` (self-use), following the
#'   FABIO convention. The `losses` column is removed from Y
#'   and `fd_labels`. Defaults to `FALSE`.
#'
#' @return A tibble with one row per year and list-columns:
#'   - `Z`: Inter-industry flow matrix (product-by-product).
#'   - `Y`: Final demand matrix.
#'   - `X`: Total output vector.
#'   - `labels`: Tibble mapping row/column indices to
#'     `area_code` and `item_cbs_code`.
#'   - `fd_labels`: Tibble mapping each Y column to its
#'     `area_code` (consuming country) and `fd_col` (demand
#'     category, e.g. `"food"`) . Pass to
#'     [compute_footprint()] as `fd_labels` to get a
#'     `target_fd` column in the footprint output.
#'
#' @export
#'
#' @examples
#' su <- build_supply_use(example = TRUE)
#' btd <- get_bilateral_trade(example = TRUE)
#' cbs <- get_wide_cbs(example = TRUE)
#' build_io_model(su, btd, cbs)
build_io_model <- function(
  supply_use = NULL,
  bilateral_trade = NULL,
  cbs = NULL,
  years = NULL,
  endogenize_losses = FALSE
) {
  # Build shared pipeline once when using defaults.
  # Intermediate results are session-cached (see ?whep_clear_cache).
  if (is.null(cbs) || is.null(supply_use)) {
    primary_prod <- .cache_get("primary_prod", build_primary_production())

    cbs_built <- .cache_get("cbs_built", {
      cli::cli_h1("Building commodity balance sheets")
      build_commodity_balances(primary_prod)
    })

    if (is.null(cbs)) {
      cbs <- .cache_get("cbs_wide", {
        cli::cli_progress_step("Adding livestock CBS rows")
        wide <- .pivot_cbs_wide(cbs_built)
        livestock_cbs <- get_livestock_cbs(primary_prod)
        dplyr::bind_rows(wide, livestock_cbs)
      })
    }

    if (is.null(supply_use)) {
      coeffs <- .cache_get("proc_coefs", {
        cli::cli_h1("Building processing coefficients")
        build_processing_coefs(cbs_built)
      })

      supply_use <- .cache_get("supply_use", {
        cli::cli_h1("Building supply-use tables")
        cli::cli_progress_step("Reading crop residues")
        crop_residues <- get_primary_residues()
        cli::cli_progress_step("Reading feed intake")
        feed_intake <- get_feed_intake()

        cli::cli_progress_step("Assembling supply-use tables")
        .build_supply_use_from_inputs(
          items_prod = whep::items_prod,
          items_cbs = whep::items_cbs,
          coeffs = coeffs,
          cbs = cbs,
          crop_residues = crop_residues,
          primary_prod = primary_prod,
          feed_intake = feed_intake
        )
      })
    }
  }

  if (is.null(bilateral_trade)) {
    bilateral_trade <- .cache_get("bilateral_trade", {
      cli::cli_h1("Building bilateral trade matrices")
      get_bilateral_trade(cbs = cbs)
    })
  }
  .validate_io_inputs(supply_use, bilateral_trade, cbs)
  common_years <- .get_common_years(
    supply_use,
    bilateral_trade,
    cbs
  )
  if (is.null(years)) {
    years <- common_years
  } else {
    .validate_years(years, common_years)
  }
  fd_cols <- .detect_fd_columns(cbs, endogenize_losses)
  n_years <- length(years)

  cli::cli_inform(c(
    "i" = "Building IO model for {n_years} year{?s}.",
    " " = "Year range: {min(years)}-{max(years)}.",
    " " = "Final demand columns: {.field {fd_cols}}.",
    if (endogenize_losses) " " else NULL,
    if (endogenize_losses) "Losses will be endogenized into Z." else NULL
  ))

  results <- purrr::imap(years, function(yr, i) {
    cli::cli_inform(c(
      ">" = "Year {yr} ({i}/{n_years})..."
    ))
    .build_io_year(
      su = dplyr::filter(supply_use, year == yr),
      btd = dplyr::filter(bilateral_trade, year == yr),
      cbs_yr = dplyr::filter(cbs, year == yr),
      fd_cols = fd_cols,
      endogenize_losses = endogenize_losses
    )
  })

  cli::cli_alert_success("IO model complete.")
  .io_results_to_tibble(results, years)
}

# --- Main per-year builder ---

.build_io_year <- function(
  su,
  btd,
  cbs_yr,
  fd_cols,
  endogenize_losses = FALSE
) {
  dims <- .get_io_dims(su, cbs_yr)
  n_sectors <- dims$n_areas * dims$n_items
  cli::cli_inform(c(
    " " = "  {dims$n_areas} areas, {dims$n_items} items,",
    " " = "  {dims$n_procs} processes -> {n_sectors} sectors."
  ))

  cbs_yr <- .adjust_food_for_leftovers(cbs_yr, su)

  cli::cli_inform("  Computing trade shares...")
  trade_shares <- .build_trade_shares(btd, cbs_yr, dims)
  shares_mat <- .build_shares_matrix(
    trade_shares,
    dims$items,
    dims$n_areas,
    dims$n_items
  )

  cli::cli_inform("  Building supply matrix...")
  mr_supply <- .build_mr_supply(su, dims)
  trans <- .row_normalize(mr_supply)

  cli::cli_inform(
    "  Building Z matrix ({n_sectors}x{n_sectors})..."
  )
  z_mat <- .build_z_matrix(su, dims, shares_mat, trans)

  z_nnz <- Matrix::nnzero(z_mat)
  z_pct <- round(
    100 * z_nnz / (n_sectors * n_sectors),
    1
  )
  cli::cli_inform(
    "  Z sparsity: {z_nnz} non-zeros ({z_pct}% dense)."
  )

  cli::cli_inform("  Building final demand matrix...")
  y_mat <- .build_mr_final_demand(
    cbs_yr,
    shares_mat,
    dims,
    fd_cols
  )

  cli::cli_inform("  Merging stock withdrawal into Y...")
  y_mat <- .merge_stock_into_fd(
    y_mat,
    cbs_yr,
    shares_mat,
    dims,
    fd_cols
  )

  if (endogenize_losses) {
    cli::cli_inform("  Endogenizing losses into Z diagonal...")
    endo <- .endogenize_losses(z_mat, y_mat, dims, fd_cols)
    z_mat <- endo$Z
    y_mat <- endo$Y
    fd_cols <- endo$fd_cols
  }

  cli::cli_inform("  Rebalancing diagonal...")
  rebal <- .rebalance_diagonal(z_mat, y_mat, dims, fd_cols)
  z_mat <- rebal$Z
  y_mat <- rebal$Y

  cli::cli_inform("  Fixing negative outputs...")
  fixed <- .fix_negative_output(z_mat, y_mat)
  n_neg <- sum(fixed$X < 0)
  if (n_neg > 0) {
    cli::cli_warn(
      "  {n_neg} sector{?s} still have negative output."
    )
  }

  labels <- .build_io_labels(dims)
  fd_labels <- .build_fd_labels(dims, fd_cols)
  list(
    Z = z_mat,
    Y = fixed$Y,
    X = fixed$X,
    labels = labels,
    fd_labels = fd_labels
  )
}

# --- Input validation ---

.validate_io_inputs <- function(su, btd, cbs) {
  required_su <- c(
    "year",
    "area_code",
    "proc_group",
    "proc_cbs_code",
    "item_cbs_code",
    "type",
    "value"
  )
  required_btd <- c(
    "year",
    "item_cbs_code",
    "bilateral_trade"
  )
  required_cbs <- c(
    "year",
    "area_code",
    "item_cbs_code",
    "production",
    "export",
    "stock_withdrawal",
    "stock_addition"
  )
  .check_required_cols(su, required_su, "supply_use")
  .check_required_cols(btd, required_btd, "bilateral_trade")
  .check_required_cols(cbs, required_cbs, "cbs")
}

.check_required_cols <- function(data, required, name) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg {name}} is missing columns: {.field {missing}}."
    )
  }
}

# --- Dimension helpers ---

.get_io_dims <- function(su, cbs_yr) {
  areas <- sort(unique(c(
    su$area_code,
    cbs_yr$area_code
  )))
  items <- sort(unique(c(
    su$item_cbs_code,
    cbs_yr$item_cbs_code
  )))
  procs <- su |>
    dplyr::distinct(proc_group, proc_cbs_code) |>
    dplyr::arrange(proc_group, proc_cbs_code)

  list(
    areas = areas,
    items = items,
    procs = procs,
    n_areas = length(areas),
    n_items = length(items),
    n_procs = nrow(procs)
  )
}

.get_common_years <- function(su, btd, cbs) {
  sort(Reduce(
    intersect,
    list(unique(su$year), unique(btd$year), unique(cbs$year))
  ))
}

.detect_fd_columns <- function(cbs, endogenize_losses = FALSE) {
  possible <- c("food", "other_uses", "stock_addition")
  if (endogenize_losses && "losses" %in% names(cbs)) {
    possible <- c(possible, "losses")
  }
  intersect(possible, names(cbs))
}

.validate_years <- function(years, common_years) {
  if (!is.numeric(years)) {
    cli::cli_abort(
      "{.arg years} must be numeric or NULL."
    )
  }
  missing <- setdiff(years, common_years)
  if (length(missing) > 0) {
    cli::cli_abort(
      "Years {.field {missing}} not available in data. \
      Available: {.field {sort(common_years)}}."
    )
  }
}

# --- Supply matrix (block-diagonal) ---

.build_mr_supply <- function(su, dims) {
  su |>
    dplyr::filter(type == "supply") |>
    .build_block_diag_matrix(dims)
}

# --- Z matrix construction ---

.build_z_matrix <- function(su, dims, shares_mat, trans) {
  use_flat <- .build_use_flat(su, dims)
  mr_use <- .expand_with_shares(
    use_flat,
    shares_mat,
    dims$n_areas,
    dims$n_procs
  )
  mr_use %*% trans
}

.build_use_flat <- function(su, dims) {
  use_data <- dplyr::filter(su, type == "use")
  blocks <- purrr::map(dims$areas, function(area) {
    .tidy_to_matrix(
      dplyr::filter(use_data, area_code == area),
      proc_rows = dims$procs,
      item_cols = dims$items
    ) |>
      Matrix::t()
  })
  do.call(cbind, blocks)
}

.expand_with_shares <- function(
  flat_mat,
  shares_mat,
  n_areas,
  n_cols_per_area
) {
  n_items <- nrow(flat_mat)
  x_exp <- flat_mat[
    rep(seq_len(n_items), n_areas),
    ,
    drop = FALSE
  ]
  y_exp <- shares_mat[,
    rep(seq_len(n_areas), each = n_cols_per_area),
    drop = FALSE
  ]
  methods::as(y_exp * x_exp, "CsparseMatrix")
}

# --- Trade shares ---

.build_trade_shares <- function(btd, cbs_yr, dims) {
  purrr::map(dims$items, function(item) {
    .item_trade_shares(btd, cbs_yr, item, dims$areas)
  }) |>
    purrr::set_names(as.character(dims$items))
}

.item_trade_shares <- function(btd, cbs_yr, item, areas) {
  n <- length(areas)
  btd_row <- dplyr::filter(btd, item_cbs_code == item)

  if (nrow(btd_row) == 0) {
    return(diag(n))
  }

  trade_mat <- .extract_trade_matrix(
    btd_row$bilateral_trade[[1]],
    areas
  )
  diag(trade_mat) <- .domestic_own_use(
    cbs_yr,
    item,
    areas
  )
  .col_normalize(trade_mat)
}

.extract_trade_matrix <- function(mat_or_val, areas) {
  n <- length(areas)
  if (!is.matrix(mat_or_val)) {
    mat_or_val <- matrix(
      mat_or_val,
      nrow = 1,
      ncol = 1
    )
  }
  if (nrow(mat_or_val) == n && ncol(mat_or_val) == n) {
    return(mat_or_val)
  }
  areas_chr <- as.character(areas)
  available <- intersect(areas_chr, rownames(mat_or_val))
  result <- matrix(0, nrow = n, ncol = n)
  if (length(available) > 0) {
    idx <- match(available, areas_chr)
    result[idx, idx] <- mat_or_val[
      available,
      available,
      drop = FALSE
    ]
  }
  result
}

.domestic_own_use <- function(cbs_yr, item, areas) {
  cbs_item <- dplyr::filter(cbs_yr, item_cbs_code == item)
  result <- rep(0, length(areas))
  if (nrow(cbs_item) == 0) {
    return(result)
  }

  idx <- match(cbs_item$area_code, areas)
  valid <- !is.na(idx)
  own <- cbs_item$production[valid] +
    cbs_item$stock_withdrawal[valid] -
    cbs_item$export[valid]
  result[idx[valid]] <- pmax(own, 0)
  result
}

# --- Shares matrix ---

.build_shares_matrix <- function(
  trade_shares,
  items,
  n_areas,
  n_items
) {
  big <- purrr::map(
    as.character(items),
    ~ trade_shares[[.x]]
  ) |>
    do.call(rbind, args = _)
  row_order <- .interleave_index(n_areas, n_items)
  methods::as(big[row_order, , drop = FALSE], "CsparseMatrix")
}

.interleave_index <- function(n_areas, n_items) {
  as.vector(t(matrix(
    seq_len(n_items * n_areas),
    nrow = n_areas,
    ncol = n_items
  )))
}

# --- Final demand ---

.build_mr_final_demand <- function(
  cbs_yr,
  shares_mat,
  dims,
  fd_cols
) {
  fd_flat <- .build_fd_flat(cbs_yr, dims, fd_cols)
  .expand_with_shares(
    fd_flat,
    shares_mat,
    dims$n_areas,
    length(fd_cols)
  )
}

.build_fd_flat <- function(cbs_yr, dims, fd_cols) {
  template <- tidyr::expand_grid(
    area_code = dims$areas,
    item_cbs_code = dims$items
  )
  fd_data <- cbs_yr |>
    dplyr::select(
      area_code,
      item_cbs_code,
      dplyr::all_of(fd_cols)
    )
  merged <- template |>
    dplyr::left_join(
      fd_data,
      by = c("area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(fd_cols),
      ~ tidyr::replace_na(.x, 0)
    ))
  blocks <- purrr::map(dims$areas, function(area) {
    merged |>
      dplyr::filter(area_code == area) |>
      dplyr::arrange(
        match(item_cbs_code, dims$items)
      ) |>
      dplyr::select(dplyr::all_of(fd_cols)) |>
      as.matrix() |>
      methods::as("CsparseMatrix")
  })
  do.call(cbind, blocks)
}

# --- Stock and leftover adjustments ---

.adjust_food_for_leftovers <- function(cbs_yr, su) {
  has_processing <- rlang::has_name(cbs_yr, "processing")
  has_seed <- rlang::has_name(cbs_yr, "seed")
  if (!has_processing && !has_seed) {
    return(cbs_yr)
  }
  su_used <- .compute_su_used(su, has_processing, has_seed)
  cbs_yr |>
    dplyr::left_join(su_used, by = c("area_code", "item_cbs_code")) |>
    .apply_leftovers(has_processing, has_seed)
}

.compute_su_used <- function(su, has_processing, has_seed) {
  parts <- list()
  if (has_processing) {
    parts$proc <- su |>
      dplyr::filter(
        type == "use",
        proc_group == "processing"
      ) |>
      dplyr::summarise(
        processing_used = sum(value),
        .by = c(area_code, item_cbs_code)
      )
  }
  if (has_seed) {
    parts$seed <- su |>
      dplyr::filter(
        type == "use",
        proc_group == "crop_production"
      ) |>
      dplyr::summarise(
        seed_used = sum(value),
        .by = c(area_code, item_cbs_code)
      )
  }
  if (length(parts) == 1L) {
    return(parts[[1L]])
  }
  dplyr::full_join(
    parts$proc,
    parts$seed,
    by = c("area_code", "item_cbs_code")
  )
}

.apply_leftovers <- function(data, has_processing, has_seed) {
  if (has_processing) {
    data <- data |>
      dplyr::mutate(
        processing_used = tidyr::replace_na(
          processing_used,
          0
        ),
        food = food + pmax(processing - processing_used, 0)
      ) |>
      dplyr::select(-processing_used)
  }
  if (has_seed) {
    data <- data |>
      dplyr::mutate(
        seed_used = tidyr::replace_na(seed_used, 0),
        food = food + pmax(seed - seed_used, 0)
      ) |>
      dplyr::select(-seed_used)
  }
  data
}

.merge_stock_into_fd <- function(
  y_mat,
  cbs_yr,
  shares_mat,
  dims,
  fd_cols
) {
  sa_idx <- match("stock_addition", fd_cols)
  has_sw <- rlang::has_name(cbs_yr, "stock_withdrawal")
  if (is.na(sa_idx) || !has_sw) {
    return(y_mat)
  }
  # Stock withdrawal is domestic-only: a country can only withdraw
  # from its own stocks. Build a domestic-block vector (no trade
  # share expansion) and subtract from the stock_addition column.
  sw_domestic <- .build_sw_domestic(cbs_yr, dims)
  n_fd <- length(fd_cols)
  for (a in seq_len(dims$n_areas)) {
    y_col <- (a - 1L) * n_fd + sa_idx
    rows <- ((a - 1L) * dims$n_items + 1L):(a * dims$n_items)
    y_mat[rows, y_col] <- y_mat[rows, y_col] -
      sw_domestic[
        ((a - 1L) * dims$n_items + 1L):(a * dims$n_items)
      ]
  }
  y_mat
}

.build_sw_domestic <- function(cbs_yr, dims) {
  template <- tidyr::expand_grid(
    area_code = dims$areas,
    item_cbs_code = dims$items
  )
  merged <- template |>
    dplyr::left_join(
      cbs_yr |>
        dplyr::select(area_code, item_cbs_code, stock_withdrawal),
      by = c("area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(
      stock_withdrawal = tidyr::replace_na(stock_withdrawal, 0)
    )
  merged$stock_withdrawal
}

# --- Losses endogenization ---
# Move losses from final demand (Y) to the diagonal of Z,
# treating them as self-use within each country's sub-block.
# This follows the FABIO convention where losses are considered
# an input to the sector itself rather than a final demand.

.endogenize_losses <- function(z_mat, y_mat, dims, fd_cols) {
  losses_idx <- match("losses", fd_cols)
  if (is.na(losses_idx)) {
    return(list(Z = z_mat, Y = y_mat, fd_cols = fd_cols))
  }

  n_fd <- length(fd_cols)
  n_items <- dims$n_items

  # Extract losses columns from Y, aggregate by product within
  # each country, and place on the domestic sub-block diagonal
  for (a in seq_len(dims$n_areas)) {
    y_col <- (a - 1L) * n_fd + losses_idx
    rows <- ((a - 1L) * n_items + 1L):(a * n_items)
    losses_vec <- as.numeric(y_mat[rows, y_col])

    # Add losses to the diagonal of the country's sub-block
    for (k in seq_along(losses_vec)) {
      if (losses_vec[k] != 0) {
        global_row <- (a - 1L) * n_items + k
        z_mat[global_row, global_row] <-
          z_mat[global_row, global_row] + losses_vec[k]
      }
    }
  }

  # Remove losses columns from Y
  losses_col_indices <- seq(losses_idx, ncol(y_mat), by = n_fd)
  y_mat <- y_mat[, -losses_col_indices, drop = FALSE]
  fd_cols <- fd_cols[-losses_idx]

  list(Z = z_mat, Y = y_mat, fd_cols = fd_cols)
}

# --- Diagonal rebalancing ---
# When diag(Z)[i] >= X[i], the diagonal accounts for all output.
# This is mainly due to reporting errors in FAOSTAT (e.g. seed =
# production). Following the original FABIO methodology, we move
# 80% of the diagonal value to final demand (spread proportionally
# across fd categories for the sector's own country) and keep 20%
# on the diagonal.

.rebalance_diagonal <- function(z_mat, y_mat, dims, fd_cols) {
  x_vec <- Matrix::rowSums(z_mat) + Matrix::rowSums(y_mat)
  diag_z <- Matrix::diag(z_mat)
  valid <- (x_vec != 0) & (diag_z >= x_vec)
  n_fix <- sum(valid)
  if (n_fix == 0) {
    return(list(Z = z_mat, Y = y_mat))
  }

  cli::cli_inform(
    "  Rebalancing {n_fix} sector{?s} with diag(Z) >= X."
  )

  # Pre-compute global Y totals per (item, fd_category) by
  # summing across all countries for each commodity.
  n_fd <- length(fd_cols)
  n_items <- dims$n_items
  n_areas <- dims$n_areas
  y_global <- matrix(0, nrow = n_items, ncol = n_fd)
  for (a in seq_len(n_areas)) {
    y_cols_a <- ((a - 1L) * n_fd + 1L):(a * n_fd)
    rows_a <- ((a - 1L) * n_items + 1L):(a * n_items)
    block <- as.matrix(
      y_mat[rows_a, y_cols_a, drop = FALSE]
    )
    y_global <- y_global + block
  }

  for (i in which(valid)) {
    # Find which country and item this sector belongs to
    area_idx <- ((i - 1) %/% n_items) + 1L
    item_idx <- ((i - 1) %% n_items) + 1L
    # Columns of Y belonging to this country
    y_cols <- ((area_idx - 1L) * n_fd + 1L):(area_idx * n_fd)
    y_row <- as.numeric(y_mat[i, y_cols])

    # If this sector has no positive domestic final demand, use
    # global average across all countries for this commodity
    if (sum(y_row) <= 0) {
      y_row <- y_global[item_idx, ]
    }

    if (sum(y_row) > 0) {
      bal <- diag_z[i] * 0.8
      share <- y_row / sum(y_row)
      y_mat[i, y_cols] <- y_mat[i, y_cols] + bal * share
      z_mat[i, i] <- diag_z[i] * 0.2
    }
  }

  list(Z = z_mat, Y = y_mat)
}

# --- Output fixing ---

.fix_negative_output <- function(z_mat, y_mat) {
  x_vec <- Matrix::rowSums(z_mat) + Matrix::rowSums(y_mat)
  neg <- which(x_vec < 0)
  if (length(neg) == 0) {
    return(list(X = as.numeric(x_vec), Y = y_mat))
  }

  for (i in neg) {
    deficit <- -x_vec[i]
    y_row <- y_mat[i, ]
    y_neg <- which(y_row < 0)
    if (length(y_neg) > 0) {
      y_mat[i, y_neg[1]] <- y_mat[i, y_neg[1]] + deficit
    }
  }
  list(
    X = as.numeric(
      Matrix::rowSums(z_mat) + Matrix::rowSums(y_mat)
    ),
    Y = y_mat
  )
}

# --- Matrix utilities ---

.tidy_to_matrix <- function(data, proc_rows, item_cols) {
  n_procs <- nrow(proc_rows)
  n_items <- length(item_cols)
  if (nrow(data) == 0) {
    return(Matrix::sparseMatrix(
      i = integer(0),
      j = integer(0),
      x = numeric(0),
      dims = c(n_procs, n_items)
    ))
  }

  agg <- data |>
    dplyr::inner_join(
      proc_rows |>
        dplyr::mutate(proc_idx = dplyr::row_number()),
      by = c("proc_group", "proc_cbs_code")
    ) |>
    dplyr::mutate(
      item_idx = match(item_cbs_code, item_cols)
    ) |>
    dplyr::filter(!is.na(item_idx)) |>
    dplyr::summarise(
      value = sum(value),
      .by = c(proc_idx, item_idx)
    )
  Matrix::sparseMatrix(
    i = agg$proc_idx,
    j = agg$item_idx,
    x = agg$value,
    dims = c(n_procs, n_items)
  )
}

.build_block_diag_matrix <- function(data, dims) {
  blocks <- purrr::map(dims$areas, function(area) {
    .tidy_to_matrix(
      dplyr::filter(data, area_code == area),
      proc_rows = dims$procs,
      item_cols = dims$items
    )
  })
  .block_diag(blocks)
}

.block_diag <- function(matrices) {
  Matrix::bdiag(matrices)
}

.row_normalize <- function(mat) {
  rs <- Matrix::rowSums(mat)
  rs[rs == 0] <- 1
  Matrix::Diagonal(x = 1 / rs) %*% mat
}

.col_normalize <- function(mat) {
  cs <- Matrix::colSums(mat)
  cs[cs == 0] <- 1
  mat %*% Matrix::Diagonal(x = 1 / cs)
}

# --- Labels and output formatting ---

.build_io_labels <- function(dims) {
  tidyr::expand_grid(
    area_code = dims$areas,
    item_cbs_code = dims$items
  ) |>
    dplyr::mutate(index = dplyr::row_number())
}

.build_fd_labels <- function(dims, fd_cols) {
  tibble::tibble(
    area_code = rep(
      dims$areas,
      each = length(fd_cols)
    ),
    fd_col = rep(fd_cols, times = dims$n_areas)
  )
}

.io_results_to_tibble <- function(results, years) {
  tibble::tibble(
    year = years,
    Z = purrr::map(results, "Z"),
    Y = purrr::map(results, "Y"),
    X = purrr::map(results, "X"),
    labels = purrr::map(results, "labels"),
    fd_labels = purrr::map(results, "fd_labels")
  )
}
