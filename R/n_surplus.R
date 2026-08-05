# Gridded soil-surface nitrogen surplus (SJOS-N Module 2, Task 2.1). The
# "safe"-side state variable the critical-nitrogen boundary is compared
# against: net nitrogen inputs minus the nitrogen exported off the field in
# harvest (crop product, used or exported residue, grazed forage). This is the
# standard OECD / IMAGE-GNM soil-surface nitrogen balance and the basis of the
# Schulte-Uebbing et al. (2022) critical surplus (locked plan decision 4).
#
# The default "harvest_removal" surplus deliberately does NOT subtract
# burnt-residue nitrogen (field-burnt residue volatilises in place, so it is a
# loss pathway inside the surplus, like ammonia, not an export) and does not
# subtract recycled residue (returned to the same field, internal). The
# alternative "full_balance" method returns build_nitrogen_balance()'s
# post-loss n_balance_t and is a sensitivity only. Per-crop item_cbs_code and
# the full grid key are preserved so the downstream per-crop boundary
# exceedance (Task 2.2) and footprint (Module 4) can trace each crop.

#' Calculate the gridded soil-surface nitrogen surplus.
#'
#' @description
#' Derives the nitrogen surplus from a [build_nitrogen_balance()] output. The
#' default `"harvest_removal"` method is the standard soil-surface nitrogen
#' balance: net inputs (`n_input_std_t`) minus the nitrogen exported in harvest
#' (crop product, used or exported residue and grazed forage), matching the
#' basis of the Schulte-Uebbing et al. (2022) critical nitrogen surplus.
#' Field-burnt residue nitrogen is not subtracted (it volatilises in place, a
#' loss inside the surplus, not an export), nor is recycled residue (returned
#' to the field, internal). The `"full_balance"` method instead returns the
#' balance's post-loss `n_balance_t`, a sensitivity alternative. The surplus
#' may be negative (a nitrogen deficit) and is not clamped here, as clamping is
#' a boundary or leaching concern. The full grid key (`lon`, `lat`,
#' `area_code`, `item_cbs_code`, `year`) and every balance column are
#' preserved.
#'
#' @param balance A [build_nitrogen_balance()] output tibble. For
#'   `"harvest_removal"` it must carry `n_input_std_t`, `prod_n_t`,
#'   `used_residue_n_t` and `grazed_weeds_n_t`; for `"full_balance"` it must
#'   carry `n_balance_t`. When an `area_ha` column (each crop's harvested
#'   hectares in the cell) is present, the per-hectare surplus `surplus_kgn_ha`
#'   is also emitted.
#' @param method Surplus definition: `"harvest_removal"` (default,
#'   `n_input_std_t - (prod_n_t + used_residue_n_t + grazed_weeds_n_t)`) or
#'   `"full_balance"` (the balance's `n_balance_t`, a sensitivity).
#' @param example If `TRUE`, return a small fixture instead of computing from
#'   `balance`. Defaults to `FALSE`.
#' @return The `balance` tibble with `surplus_n_t` (tonnes N, may be negative),
#'   `method_surplus` and, when `area_ha` is present, `surplus_kgn_ha`
#'   (kg N per hectare). An area-keyed `balance` also gains the polity columns
#'   below; one without an `area_code` is returned without them.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' calculate_n_surplus(example = TRUE)
calculate_n_surplus <- function(
  balance,
  method = c("harvest_removal", "full_balance"),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_n_surplus())
  }
  method <- rlang::arg_match(method)
  .check_columns(balance, .n_surplus_required(method), "balance")
  balance |>
    .n_surplus_add_value(method) |>
    .n_surplus_add_production() |>
    .n_surplus_add_intensity() |>
    .add_polity_columns_if_keyed()
}

# ---- Private helpers -------------------------------------------------------

# Columns each method needs on the balance input.
.n_surplus_required <- function(method) {
  if (method == "full_balance") {
    return("n_balance_t")
  }
  c("n_input_std_t", "prod_n_t", "used_residue_n_t", "grazed_weeds_n_t")
}

# Add surplus_n_t and the method stamp. harvest_removal = net inputs minus
# harvested-nitrogen exports (product + used residue + grazed); full_balance =
# the balance's own post-loss residual.
.n_surplus_add_value <- function(balance, method) {
  if (method == "full_balance") {
    return(dplyr::mutate(
      balance,
      surplus_n_t = .data$n_balance_t,
      method_surplus = "full_balance"
    ))
  }
  dplyr::mutate(
    balance,
    surplus_n_t = .data$n_input_std_t -
      (.data$prod_n_t + .data$used_residue_n_t + .data$grazed_weeds_n_t),
    method_surplus = "harvest_removal"
  )
}

# Carry the nitrogen embodied in agricultural production separately from the
# soil-surface surplus. The footprint's `production` category must trace this
# harvest output, not relabel total surplus as production.
.n_surplus_add_production <- function(x) {
  terms <- c("prod_n_t", "used_residue_n_t", "grazed_weeds_n_t")
  if (!all(rlang::has_name(x, terms))) {
    return(dplyr::mutate(x, production_n_t = NA_real_))
  }
  dplyr::mutate(
    x,
    production_n_t = .data$prod_n_t +
      .data$used_residue_n_t +
      .data$grazed_weeds_n_t
  )
}

# Emit the per-hectare surplus when the harvested-area column is available
# (needed by the per-hectare boundary comparison, Task 2.2); a zero or missing
# area yields NA rather than a non-finite rate.
.n_surplus_add_intensity <- function(x) {
  if (!rlang::has_name(x, "area_ha")) {
    return(x)
  }
  dplyr::mutate(
    x,
    surplus_kgn_ha = dplyr::if_else(
      .data$area_ha > 0,
      .data$surplus_n_t * 1000 / .data$area_ha,
      NA_real_
    )
  )
}
