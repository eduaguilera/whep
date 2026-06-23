#' Estimate the destinies of crop residues.
#'
#' Splits crop residue dry matter into three destinies that sum to the total
#' residue: fed to livestock, burned / removed for fuel, and left on the field
#' for soil incorporation.
#'
#' @param x A tibble with `item_prod_code` and `residue_dm_t`. The
#'   `krausmann_regional` method also needs `region_krausmann` (for the recovery
#'   rate) and `region_hanpp` (for the feed-use fraction). The `shares` method
#'   needs `year`.
#' @param method Destiny method: `"krausmann_regional"` (default, Krausmann
#'   recovery x HANPP-regional feed-use fraction) or `"shares"` (the
#'   Spain-specific per-crop-year use/burn shares, flagged `to_be_revised`).
#' @return The input tibble with `residue_feed_dm_t`, `residue_burn_dm_t`,
#'   `residue_soil_dm_t` and `method_residue_destiny`.
#' @export
#' @examples
#' calculate_residue_destinies(
#'   tibble::tibble(
#'     item_prod_code = "15", residue_dm_t = 100,
#'     region_krausmann = "West Europe", region_hanpp = "Western Europe"
#'   )
#' )
calculate_residue_destinies <- function(
  x,
  method = c("krausmann_regional", "shares")
) {
  method <- rlang::arg_match(method)
  .crop_npp_validate(
    x,
    c("item_prod_code", "residue_dm_t"),
    "calculate_residue_destinies"
  )
  out <- switch(
    method,
    krausmann_regional = .residue_destiny_krausmann(x),
    shares = .residue_destiny_shares(x)
  )
  dplyr::mutate(out, method_residue_destiny = method)
}

#' Build residue feed availability for feed allocation.
#'
#' Turns the feed destiny of crop residues into the `feed_avail` contract
#' consumed by [redistribute_feed()]: maps each crop to its residue commodity
#' item, applies a feed-availability loss, and aggregates to year / territory /
#' residue item.
#'
#' @param x A tibble with `item_prod_code`, `year`, `sub_territory` and
#'   `residue_feed_dm_t` (from [calculate_residue_destinies()]).
#' @param loss_fraction Fraction of the feed residue lost before intake
#'   (default 0.15).
#' @param feed_scale Value for the `feed_scale` column (default `"national"`).
#' @return A tibble with the `redistribute_feed()` `feed_avail` columns: `year`,
#'   `sub_territory`, `item_cbs_code`, `feed_group`, `feed_quality`
#'   (`"residues"`), `avail_dm_t` and `feed_scale`.
#' @export
#' @examples
#' tibble::tibble(
#'   item_prod_code = "15", year = 2000, sub_territory = "ESP",
#'   residue_feed_dm_t = 50
#' ) |>
#'   build_residue_feed_avail()
build_residue_feed_avail <- function(
  x,
  loss_fraction = 0.15,
  feed_scale = "national"
) {
  .crop_npp_validate(
    x,
    c("item_prod_code", "year", "sub_territory", "residue_feed_dm_t"),
    "build_residue_feed_avail"
  )
  item_map <- whep::whep_coef_table("crop_residue_item_map") |>
    dplyr::transmute(
      item_prod_code = as.character(item_prod_code),
      item_cbs_code = residue_item_cbs_code
    )
  x |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::left_join(item_map, by = "item_prod_code") |>
    dplyr::filter(!is.na(item_cbs_code)) |>
    dplyr::summarise(
      avail_dm_t = sum(residue_feed_dm_t * (1 - loss_fraction), na.rm = TRUE),
      .by = c(year, sub_territory, item_cbs_code)
    ) |>
    dplyr::mutate(
      feed_group = "residues",
      feed_quality = "residues",
      feed_scale = feed_scale
    )
}

# ---- Private helpers --------------------------------------------------

.residue_destiny_krausmann <- function(x) {
  if (!all(c("region_krausmann", "region_hanpp") %in% names(x))) {
    cli::cli_abort(
      "method {.val krausmann_regional} needs {.field region_krausmann} \\
       and {.field region_hanpp}."
    )
  }
  cat_map <- whep::items_prod_full |>
    dplyr::transmute(
      item_prod_code = as.character(item_prod_code),
      cat_krausmann = Cat_Krausmann
    )
  recovery <- whep::whep_coef_table("residue_krausmann") |>
    dplyr::select(cat_krausmann, region_krausmann, recovery_rates)
  feed <- whep::whep_coef_table("residue_feed_fraction") |>
    dplyr::select(region_hanpp, feed_use_fraction)
  global_feed <- feed$feed_use_fraction[feed$region_hanpp == "Global"]
  x |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::left_join(cat_map, by = "item_prod_code") |>
    dplyr::left_join(recovery, by = c("cat_krausmann", "region_krausmann")) |>
    dplyr::left_join(feed, by = "region_hanpp") |>
    dplyr::mutate(
      recovery_rates = tidyr::replace_na(recovery_rates, 0),
      feed_use_fraction = tidyr::replace_na(feed_use_fraction, global_feed),
      residue_feed_dm_t = residue_dm_t * recovery_rates * feed_use_fraction,
      residue_burn_dm_t = residue_dm_t *
        recovery_rates *
        (1 - feed_use_fraction),
      residue_soil_dm_t = residue_dm_t * (1 - recovery_rates)
    ) |>
    dplyr::select(-cat_krausmann, -recovery_rates, -feed_use_fraction)
}

.residue_destiny_shares <- function(x) {
  if (!rlang::has_name(x, "year")) {
    cli::cli_abort("method {.val shares} needs a {.field year} column.")
  }
  cli::cli_warn(
    c(
      "Residue destinies use the Spain-specific {.field residue_shares} table.",
      i = "Flagged {.field to_be_revised}: not validated for global scope."
    ),
    .frequency = "once",
    .frequency_id = "residue_shares_provisional"
  )
  shares <- whep::whep_coef_table("residue_shares") |>
    dplyr::select(item_prod_code, year, use_share, burn_share)
  x |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::left_join(shares, by = c("item_prod_code", "year")) |>
    dplyr::mutate(
      use_share = tidyr::replace_na(use_share, 0),
      burn_share = tidyr::replace_na(burn_share, 0),
      residue_feed_dm_t = residue_dm_t * use_share,
      residue_burn_dm_t = residue_dm_t * burn_share,
      residue_soil_dm_t = residue_dm_t * pmax(1 - use_share - burn_share, 0),
      residue_destiny_to_be_revised = TRUE
    ) |>
    dplyr::select(-use_share, -burn_share)
}
