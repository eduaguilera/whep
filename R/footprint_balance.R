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
