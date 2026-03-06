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
#' @param supply_use Tibble from [build_supply_use()]. Must have
#'   columns: `year`, `area_code`, `proc_group`, `proc_cbs_code`,
#'   `item_cbs_code`, `type`, `value`.
#' @param bilateral_trade Tibble from [get_bilateral_trade()].
#'   Must have columns: `year`, `item_cbs_code`,
#'   `bilateral_trade` (list-column of matrices).
#' @param cbs Tibble from [get_wide_cbs()]. Must have columns:
#'   `year`, `area_code`, `item_cbs_code`, `production`,
#'   `import`, `export`, `stock_retrieval`, plus final demand
#'   columns (`food`, `other_uses`, etc.).
#'
#' @return A tibble with one row per year and list-columns:
#'   - `Z`: Inter-industry flow matrix (product-by-product).
#'   - `Y`: Final demand matrix.
#'   - `X`: Total output vector.
#'   - `labels`: Tibble mapping row/column indices to
#'     `area_code` and `item_cbs_code`.
#'
#' @export
#'
#' @examples
#' su <- build_supply_use(example = TRUE)
#' btd <- get_bilateral_trade(example = TRUE)
#' cbs <- get_wide_cbs(example = TRUE)
#' # build_io_model(su, btd, cbs)
build_io_model <- function(supply_use, bilateral_trade, cbs) {
  .validate_io_inputs(supply_use, bilateral_trade, cbs)
  years <- .get_common_years(supply_use, bilateral_trade, cbs)
  fd_cols <- .detect_fd_columns(cbs)
  n_years <- length(years)

  cli::cli_inform(c(
    "i" = "Building IO model for {n_years} year{?s}.",
    " " = "Year range: {min(years)}-{max(years)}.",
    " " = "Final demand columns: {.field {fd_cols}}."
  ))

  results <- purrr::imap(years, function(yr, i) {
    cli::cli_inform(c(
      ">" = "Year {yr} ({i}/{n_years})..."
    ))
    .build_io_year(
      su = dplyr::filter(supply_use, year == yr),
      btd = dplyr::filter(bilateral_trade, year == yr),
      cbs_yr = dplyr::filter(cbs, year == yr),
      fd_cols = fd_cols
    )
  })

  cli::cli_alert_success("IO model complete.")
  .io_results_to_tibble(results, years)
}

# --- Main per-year builder ---

.build_io_year <- function(su, btd, cbs_yr, fd_cols) {
  dims <- .get_io_dims(su, cbs_yr)
  n_sectors <- dims$n_areas * dims$n_items
  cli::cli_inform(c(
    " " = "  {dims$n_areas} areas, {dims$n_items} items,",
    " " = "  {dims$n_procs} processes -> {n_sectors} sectors."
  ))

  cli::cli_inform("  Computing trade shares...")
  trade_shares <- .build_trade_shares(btd, cbs_yr, dims)
  shares_mat <- .build_shares_matrix(
    trade_shares, dims$items, dims$n_areas, dims$n_items
  )

  cli::cli_inform("  Building supply matrix...")
  mr_supply <- .build_mr_supply(su, dims)
  trans <- .row_normalize(mr_supply)

  cli::cli_inform(
    "  Building Z matrix ({n_sectors}x{n_sectors})..."
  )
  z_mat <- .build_z_matrix(su, dims, shares_mat, trans)

  cli::cli_inform("  Building final demand matrix...")
  y_mat <- .build_mr_final_demand(
    cbs_yr, shares_mat, dims, fd_cols
  )

  cli::cli_inform("  Fixing negative outputs...")
  fixed <- .fix_negative_output(z_mat, y_mat)
  n_neg <- sum(fixed$X < 0)
  if (n_neg > 0) {
    cli::cli_warn(
      "  {n_neg} sector{?s} still have negative output."
    )
  }

  labels <- .build_io_labels(dims)
  list(
    Z = z_mat, Y = fixed$Y, X = fixed$X,
    labels = labels
  )
}

# --- Input validation ---

.validate_io_inputs <- function(su, btd, cbs) {
  required_su <- c(
    "year", "area_code", "proc_group",
    "proc_cbs_code", "item_cbs_code", "type", "value"
  )
  required_btd <- c(
    "year", "item_cbs_code", "bilateral_trade"
  )
  required_cbs <- c(
    "year", "area_code", "item_cbs_code",
    "production", "export", "stock_retrieval"
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
    su$area_code, cbs_yr$area_code
  )))
  items <- sort(unique(c(
    su$item_cbs_code, cbs_yr$item_cbs_code
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

.detect_fd_columns <- function(cbs) {
  possible <- c("food", "other_uses", "feed")
  intersect(possible, names(cbs))
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
    use_flat, shares_mat, dims$n_areas, dims$n_procs
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
      t()
  })
  do.call(cbind, blocks)
}

.expand_with_shares <- function(
  flat_mat, shares_mat, n_areas, n_cols_per_area
) {
  n_items <- nrow(flat_mat)
  x_exp <- flat_mat[
    rep(seq_len(n_items), n_areas), , drop = FALSE
  ]
  y_exp <- shares_mat[
    , rep(seq_len(n_areas), each = n_cols_per_area),
    drop = FALSE
  ]
  y_exp * x_exp
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

  if (nrow(btd_row) == 0) return(diag(n))

  trade_mat <- .extract_trade_matrix(
    btd_row$bilateral_trade[[1]], areas
  )
  diag(trade_mat) <- .domestic_own_use(
    cbs_yr, item, areas
  )
  .col_normalize(trade_mat)
}

.extract_trade_matrix <- function(mat_or_val, areas) {
  n <- length(areas)
  if (!is.matrix(mat_or_val)) {
    mat_or_val <- matrix(
      mat_or_val, nrow = 1, ncol = 1
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
      available, available, drop = FALSE
    ]
  }
  result
}

.domestic_own_use <- function(cbs_yr, item, areas) {
  cbs_item <- dplyr::filter(cbs_yr, item_cbs_code == item)
  result <- rep(0, length(areas))
  if (nrow(cbs_item) == 0) return(result)

  idx <- match(cbs_item$area_code, areas)
  valid <- !is.na(idx)
  own <- cbs_item$production[valid] +
    cbs_item$stock_retrieval[valid] -
    cbs_item$export[valid]
  result[idx[valid]] <- pmax(own, 0)
  result
}

# --- Shares matrix ---

.build_shares_matrix <- function(
  trade_shares, items, n_areas, n_items
) {
  big <- purrr::map(
    as.character(items), ~ trade_shares[[.x]]
  ) |>
    do.call(rbind, args = _)
  row_order <- .interleave_index(n_areas, n_items)
  big[row_order, , drop = FALSE]
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
  cbs_yr, shares_mat, dims, fd_cols
) {
  fd_flat <- .build_fd_flat(cbs_yr, dims, fd_cols)
  .expand_with_shares(
    fd_flat, shares_mat, dims$n_areas, length(fd_cols)
  )
}

.build_fd_flat <- function(cbs_yr, dims, fd_cols) {
  template <- tidyr::expand_grid(
    area_code = dims$areas,
    item_cbs_code = dims$items
  )
  fd_data <- cbs_yr |>
    dplyr::select(
      area_code, item_cbs_code, dplyr::all_of(fd_cols)
    )
  merged <- template |>
    dplyr::left_join(
      fd_data, by = c("area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(fd_cols), ~ tidyr::replace_na(.x, 0)
    ))
  purrr::map(dims$areas, function(area) {
    merged |>
      dplyr::filter(area_code == area) |>
      dplyr::arrange(
        match(item_cbs_code, dims$items)
      ) |>
      dplyr::select(dplyr::all_of(fd_cols)) |>
      as.matrix()
  }) |>
    do.call(cbind, args = _)
}

# --- Output fixing ---

.fix_negative_output <- function(z_mat, y_mat) {
  x_vec <- rowSums(z_mat) + rowSums(y_mat)
  neg <- which(x_vec < 0)
  if (length(neg) == 0) {
    return(list(X = x_vec, Y = y_mat))
  }

  for (i in neg) {
    deficit <- -x_vec[i]
    y_neg <- which(y_mat[i, ] < 0)
    if (length(y_neg) > 0) {
      y_mat[i, y_neg[1]] <- y_mat[i, y_neg[1]] + deficit
    }
  }
  list(
    X = rowSums(z_mat) + rowSums(y_mat),
    Y = y_mat
  )
}

# --- Matrix utilities ---

.tidy_to_matrix <- function(data, proc_rows, item_cols) {
  n_procs <- nrow(proc_rows)
  n_items <- length(item_cols)
  m <- matrix(0, nrow = n_procs, ncol = n_items)
  if (nrow(data) == 0) return(m)

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
      value = sum(value), .by = c(proc_idx, item_idx)
    )
  m[cbind(agg$proc_idx, agg$item_idx)] <- agg$value
  m
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
  row_sizes <- vapply(matrices, nrow, integer(1))
  col_sizes <- vapply(matrices, ncol, integer(1))
  result <- matrix(
    0, sum(row_sizes), sum(col_sizes)
  )
  r_off <- 0L
  c_off <- 0L
  for (m in matrices) {
    nr <- nrow(m)
    nc <- ncol(m)
    result[
      r_off + seq_len(nr), c_off + seq_len(nc)
    ] <- m
    r_off <- r_off + nr
    c_off <- c_off + nc
  }
  result
}

.row_normalize <- function(mat) {
  rs <- rowSums(mat)
  rs[rs == 0] <- 1
  mat / rs
}

.col_normalize <- function(mat) {
  cs <- colSums(mat)
  cs[cs == 0] <- 1
  t(t(mat) / cs)
}

# --- Labels and output formatting ---

.build_io_labels <- function(dims) {
  tidyr::expand_grid(
    area_code = dims$areas,
    item_cbs_code = dims$items
  ) |>
    dplyr::mutate(index = dplyr::row_number())
}

.io_results_to_tibble <- function(results, years) {
  tibble::tibble(
    year = years,
    Z = purrr::map(results, "Z"),
    Y = purrr::map(results, "Y"),
    X = purrr::map(results, "X"),
    labels = purrr::map(results, "labels")
  )
}
