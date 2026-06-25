#' Compute land footprints by physical trade balance.
#'
#' @description
#' Estimate consumption-based land footprints by propagating direct
#' land use through the bilateral physical trade network, an
#' approach independent of the Leontief inverse used by
#' [compute_footprint()]. Each country's supply pool (production
#' plus imports) carries an embodied-land intensity `s`; solving
#' the balance \eqn{(D - M) s = L} per item gives `s`, where `D` is
#' diagonal supply throughput, `M` routes import intensities, and
#' `L` is direct land. The footprint of consumption (supply minus
#' exports) is then `consumption * s`.
#'
#' Unlike the multi-regional input-output method, this tracer
#' handles only the trade dimension, not inter-product processing
#' transformations. Comparing the two with
#' [compare_footprint_methods()] isolates the effect of that
#' assumption -- an apples-to-apples stress test on shared FAOSTAT
#' data.
#'
#' @param production Tibble with `area_code`, `item_cbs_code` and
#'   `value` (quantity produced).
#' @param trade Tibble with `from_code`, `to_code`,
#'   `item_cbs_code` and `value` (quantity exported from `from_code`
#'   to `to_code`).
#' @param extension Tibble with `area_code`, `item_cbs_code` and
#'   `value` (direct land use of domestic production).
#'
#' @return A tibble with `area_code` (consuming country),
#'   `item_cbs_code`, `value` (embodied land in consumption) and
#'   `method` (`"land_balance"`).
#'
#' @export
#'
#' @examples
#' production <- tibble::tibble(
#'   area_code = c(1L, 2L),
#'   item_cbs_code = c(10L, 10L),
#'   value = c(100, 0)
#' )
#' trade <- tibble::tibble(
#'   from_code = 1L, to_code = 2L, item_cbs_code = 10L, value = 40
#' )
#' extension <- tibble::tibble(
#'   area_code = c(1L, 2L),
#'   item_cbs_code = c(10L, 10L),
#'   value = c(50, 0)
#' )
#' compute_footprint_balance(production, trade, extension)
compute_footprint_balance <- function(production, trade, extension) {
  .require_cols(
    production,
    c("area_code", "item_cbs_code", "value"),
    "production"
  )
  .require_cols(
    trade,
    c("from_code", "to_code", "item_cbs_code", "value"),
    "trade"
  )
  .require_cols(
    extension,
    c("area_code", "item_cbs_code", "value"),
    "extension"
  )

  items <- sort(unique(c(production$item_cbs_code, trade$item_cbs_code)))
  purrr::map(items, \(it) {
    .balance_one_item(
      production[production$item_cbs_code == it, ],
      trade[trade$item_cbs_code == it, ],
      extension[extension$item_cbs_code == it, ],
      it
    )
  }) |>
    purrr::list_rbind()
}

#' Compare two footprint estimates.
#'
#' @description
#' Align two consumption-based footprint estimates (for example the
#' Leontief and land-balance methods) by consuming country and item
#' and report their difference. Disagreement is diagnostic: it
#' localises where a method's assumptions matter most.
#'
#' @param method_a,method_b Tibbles with `area_code`,
#'   `item_cbs_code` and `value` (consumption footprint).
#'
#' @return A tibble with `area_code`, `item_cbs_code`, `value_a`,
#'   `value_b`, `abs_diff` and `rel_diff` (relative to the larger
#'   of the two), ordered by descending `abs_diff`.
#'
#' @export
#'
#' @examples
#' a <- tibble::tibble(area_code = 1L, item_cbs_code = 10L, value = 30)
#' b <- tibble::tibble(area_code = 1L, item_cbs_code = 10L, value = 25)
#' compare_footprint_methods(a, b)
compare_footprint_methods <- function(method_a, method_b) {
  .require_cols(method_a, c("area_code", "item_cbs_code", "value"), "method_a")
  .require_cols(method_b, c("area_code", "item_cbs_code", "value"), "method_b")

  a <- dplyr::summarise(
    method_a,
    value_a = sum(value),
    .by = c(area_code, item_cbs_code)
  )
  b <- dplyr::summarise(
    method_b,
    value_b = sum(value),
    .by = c(area_code, item_cbs_code)
  )
  dplyr::full_join(a, b, by = c("area_code", "item_cbs_code")) |>
    dplyr::mutate(
      value_a = tidyr::replace_na(value_a, 0),
      value_b = tidyr::replace_na(value_b, 0),
      abs_diff = abs(value_a - value_b),
      rel_diff = .safe_rel(abs_diff, pmax(value_a, value_b))
    ) |>
    dplyr::arrange(dplyr::desc(abs_diff))
}

#' Melt a bilateral trade matrix to long format.
#'
#' @description
#' Convert the `bilateral_trade` list-column of [get_bilateral_trade()]
#' (one square origin-by-destination matrix per year and item) into
#' the tidy `from_code`/`to_code` long form consumed by
#' [compute_footprint_balance()]. Self-trade (diagonal) entries are
#' dropped.
#'
#' @param bilateral_trade Tibble from [get_bilateral_trade()], with
#'   `year`, `item_cbs_code` and a `bilateral_trade` matrix
#'   list-column.
#'
#' @return A tibble with `year`, `from_code`, `to_code`,
#'   `item_cbs_code` and `value`.
#'
#' @export
#'
#' @examples
#' m <- matrix(
#'   c(0, 40, 0, 0),
#'   nrow = 2,
#'   dimnames = list(c("1", "2"), c("1", "2"))
#' )
#' bt <- tibble::tibble(
#'   year = 2010L, item_cbs_code = 10L, bilateral_trade = list(m)
#' )
#' melt_bilateral_trade(bt)
melt_bilateral_trade <- function(bilateral_trade) {
  .require_cols(
    bilateral_trade,
    c("year", "item_cbs_code", "bilateral_trade"),
    "bilateral_trade"
  )
  purrr::pmap(
    list(
      bilateral_trade$year,
      bilateral_trade$item_cbs_code,
      bilateral_trade$bilateral_trade
    ),
    .melt_one_trade_matrix
  ) |>
    purrr::list_rbind()
}

#' Build a consumption land footprint by physical trade balance.
#'
#' @description
#' End-to-end land-balance footprint for one year on real WHEP data:
#' assemble production (primary crop production plus grass dry-matter
#' derived from grassland area times yield), the bilateral trade
#' network, and the crop-plus-grassland direct-land extension, then
#' trace land to consumers with [compute_footprint_balance()]. This
#' is the independent, non-Leontief estimator for stress-testing the
#' multi-regional input-output footprint via
#' [compare_footprint_methods()].
#'
#' Grass items (`item_cbs_code` 3000 and 3002) are barely traded, so
#' their land stays with the producing country: the balance, unlike
#' the input-output model, does not route grass through the
#' grass-to-livestock chain. Any `production`, `trade` or `extension`
#' supplied explicitly is used as-is instead of being built, which is
#' useful for reusing cached inputs or for testing.
#'
#' @param year Year to build the footprint for.
#' @param production Optional production tibble
#'   (`area_code`, `item_cbs_code`, `value`); built when `NULL`.
#' @param trade Optional trade tibble (`from_code`, `to_code`,
#'   `item_cbs_code`, `value`); built when `NULL`.
#' @param extension Optional direct-land tibble (`area_code`,
#'   `item_cbs_code`, `value`); built when `NULL`.
#' @param example If `TRUE`, return a small example output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble as returned by [compute_footprint_balance()].
#'
#' @export
#'
#' @examples
#' build_land_balance_footprint(example = TRUE)
build_land_balance_footprint <- function(
  year,
  production = NULL,
  trade = NULL,
  extension = NULL,
  example = FALSE
) {
  if (example) {
    return(.example_build_land_balance_footprint())
  }
  .validate_year(year)
  extension <- extension %||% .land_balance_extension(year)
  production <- production %||% .land_balance_production(year, extension)
  trade <- trade %||% .land_balance_trade(year)
  compute_footprint_balance(production, trade, extension)
}

# --- Helpers ---

.balance_one_item <- function(prod_i, trade_i, ext_i, item) {
  areas <- sort(unique(c(prod_i$area_code, trade_i$from_code, trade_i$to_code)))
  if (length(areas) == 0) {
    return(.empty_balance())
  }

  p <- .vec_by_area(prod_i$area_code, prod_i$value, areas)
  l <- .vec_by_area(ext_i$area_code, ext_i$value, areas)
  tmat <- .trade_matrix(trade_i, areas)

  exports <- rowSums(tmat)
  imports <- colSums(tmat)
  throughput <- p + imports
  s <- .solve_balance_intensity(throughput, tmat, l)
  value <- pmax(throughput - exports, 0) * s

  tibble::tibble(
    area_code = as.integer(areas),
    item_cbs_code = as.integer(item),
    value = value,
    method = "land_balance"
  ) |>
    dplyr::filter(value > 0)
}

# Solve (D - M) s = L for the embodied-land intensity s, where
# M[i, j] = tmat[j, i] routes exporter j's intensity into importer
# i. Sectors with no supply throughput carry zero intensity.
.solve_balance_intensity <- function(throughput, tmat, l) {
  s <- rep(0, length(throughput))
  active <- throughput > 0
  if (!any(active)) {
    return(s)
  }
  m <- t(tmat)
  a_sys <- (diag(throughput, length(throughput)) - m)[
    active,
    active,
    drop = FALSE
  ]
  l_act <- l[active]
  s[active] <- tryCatch(
    as.numeric(solve(a_sys, l_act)),
    error = function(e) {
      diag(a_sys) <- diag(a_sys) + 1e-9 * max(diag(a_sys))
      as.numeric(solve(a_sys, l_act))
    }
  )
  s
}

.trade_matrix <- function(trade_i, areas) {
  n <- length(areas)
  tmat <- matrix(0, n, n)
  if (nrow(trade_i) == 0) {
    return(tmat)
  }
  agg <- dplyr::summarise(
    trade_i,
    value = sum(value),
    .by = c(from_code, to_code)
  )
  ii <- match(agg$from_code, areas)
  jj <- match(agg$to_code, areas)
  tmat[cbind(ii, jj)] <- agg$value
  tmat
}

.vec_by_area <- function(area, value, areas) {
  if (length(area) == 0) {
    return(rep(0, length(areas)))
  }
  agg <- tapply(value, factor(area, levels = areas), sum)
  ifelse(is.na(agg), 0, agg) |> as.numeric()
}

.empty_balance <- function() {
  tibble::tibble(
    area_code = integer(),
    item_cbs_code = integer(),
    value = numeric(),
    method = character()
  )
}

.melt_one_trade_matrix <- function(year, item, m) {
  m <- as.matrix(m)
  idx <- which(m != 0 & !is.na(m), arr.ind = TRUE)
  idx <- idx[idx[, 1] != idx[, 2], , drop = FALSE]
  if (nrow(idx) == 0) {
    return(NULL)
  }
  tibble::tibble(
    year = as.integer(year),
    from_code = as.integer(rownames(m)[idx[, 1]]),
    to_code = as.integer(colnames(m)[idx[, 2]]),
    item_cbs_code = as.integer(item),
    value = m[idx]
  )
}

.grass_item_codes <- function() {
  c(3000L, 3002L)
}

.land_balance_extension <- function(year) {
  crop <- build_cropgrids_land_extension(source = "cropgrids_fallow") |>
    dplyr::filter(year == .env$year) |>
    dplyr::select(area_code, item_cbs_code, value = impact_u)
  grass <- build_grassland_land_extension(grassland_metric = "occupation") |>
    dplyr::filter(year == .env$year) |>
    dplyr::select(area_code, item_cbs_code, value = impact_u)
  dplyr::bind_rows(crop, grass)
}

# Grass dry-matter tonnage is derived from grassland area times yield
# so the grass-land intensity is internally consistent; without it the
# balance would have zero grass throughput and silently drop all
# grassland land.
.land_balance_production <- function(year, extension, grass_yield = 2.06) {
  grass_codes <- .grass_item_codes()
  grass_prod <- extension |>
    dplyr::filter(item_cbs_code %in% grass_codes) |>
    dplyr::mutate(value = value * grass_yield)
  crop_prod <- get_primary_production() |>
    dplyr::filter(
      year == .env$year,
      unit == "tonnes",
      !is.na(item_cbs_code),
      !item_cbs_code %in% grass_codes,
      value > 0
    ) |>
    dplyr::summarise(value = sum(value), .by = c(area_code, item_cbs_code))
  dplyr::bind_rows(crop_prod, grass_prod)
}

.land_balance_trade <- function(year) {
  get_bilateral_trade() |>
    melt_bilateral_trade() |>
    dplyr::filter(year == .env$year) |>
    dplyr::select(from_code, to_code, item_cbs_code, value)
}

.validate_year <- function(year) {
  if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
    cli::cli_abort("{.arg year} must be a single number.")
  }
}
