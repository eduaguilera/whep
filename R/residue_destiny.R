#' Estimate the destinies of crop residues.
#'
#' Splits crop residue dry matter into three destinies that sum to the total
#' residue: fed to livestock, burned / removed for fuel, and left on the field
#' for soil incorporation.
#'
#' @param x A tibble with `item_prod_code` and `residue_dm_t`. The
#'   `krausmann_regional` method also needs `region_krausmann` (for the recovery
#'   rate) and `region_hanpp` (for the feed-use fraction). `region_krausmann`
#'   can use the recovery-table labels or the matching `regions_full` labels.
#'   The `shares` method needs `year`.
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
#'     region_krausmann = "Western Europe", region_hanpp = "Western Europe"
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
  joined <- x |>
    dplyr::mutate(
      item_prod_code = as.character(item_prod_code),
      region_krausmann = .residue_recovery_region(.data$region_krausmann)
    ) |>
    dplyr::left_join(cat_map, by = "item_prod_code") |>
    dplyr::left_join(recovery, by = c("cat_krausmann", "region_krausmann")) |>
    dplyr::left_join(feed, by = "region_hanpp")

  .warn_coef_fallbacks(joined, global_feed)

  joined |>
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

# Say when a regional coefficient did not match, instead of filling it in silence.
#
# Both fills below are `replace_na()`, so an unmatched region is indistinguishable from a
# matched one afterwards, and neither is an error anywhere. That is how whep#405 survived: the
# `region_hanpp` column of `residue_feed_fraction.csv` holds **UN M49 sub-regions** while it is
# joined against `regions_full$region_HANPP`, the two vocabularies share ZERO values, so all 17
# of the file's regional coefficients are dead and every row takes the Global 0.20, discarding
# a published range of 0.05 to 0.45.
#
# `recovery_rates` matters more than it looks, because its fallback is not a central estimate
# but **0**: an unmatched region sends 100% of that row's residue to soil and none to feed or
# burning. A wrong region there does not nudge a number, it removes a flow.
#
# Deliberately a warning and not an abort, and it changes no output: the fills still happen
# exactly as before. Fixing #405 moves published coefficients for every area, which is a
# maintainer decision; making the silence audible is not.
.warn_coef_fallbacks <- function(joined, global_feed) {
  n <- nrow(joined)
  if (n == 0L) {
    return(invisible(NULL))
  }

  report <- function(col, what, consequence) {
    missing <- is.na(joined[[col]])
    if (!any(missing)) {
      return(invisible(NULL))
    }
    regions <- sort(unique(as.character(joined[[what]][missing])))
    cli::cli_warn(c(
      "!" = "{sum(missing)} of {n} residue row{?s} matched no {.field {col}}
         ({round(100 * sum(missing) / n, 1)}%), so {consequence}",
      "i" = "unmatched {.field {what}} value{?s}:
         {.val {utils::head(regions, 8)}}",
      "i" = "A vocabulary mismatch between the coefficient file and
         {.field regions_full} looks exactly like this - see whep#405."
    ))
  }

  report(
    "feed_use_fraction",
    "region_hanpp",
    paste0("each fell back to the global ", global_feed, ".")
  )
  report(
    "recovery_rates",
    "region_krausmann",
    "each fell back to 0, sending all of that residue to soil."
  )

  invisible(NULL)
}

.residue_recovery_region <- function(region) {
  region <- as.character(region)
  lookup <- whep::regions_full |>
    dplyr::transmute(
      input_region = .data$region_krausmann,
      recovery_region = .data$region_HANPP
    ) |>
    dplyr::filter(
      !is.na(.data$input_region),
      !is.na(.data$recovery_region)
    ) |>
    dplyr::distinct(.data$input_region, .data$recovery_region)
  .assert_unique_region_map(lookup)
  mapped <- lookup$recovery_region[match(region, lookup$input_region)]
  dplyr::coalesce(mapped, region)
}

# Guard the krausmann -> HANPP region map against silent fan-out: the earlier
# distinct(input_region, .keep_all = TRUE) kept the first HANPP region whenever a
# Krausmann label spanned several, hiding the ambiguity. The map is 1:1 today, so
# abort loudly if a future regions_full change breaks that (relates to #170).
.assert_unique_region_map <- function(lookup) {
  dupes <- unique(lookup$input_region[duplicated(lookup$input_region)])
  if (length(dupes) > 0L) {
    cli::cli_abort(
      c(
        "Each {.field region_krausmann} must map to one {.field region_HANPP}.",
        i = "Ambiguous label{?s}: {.val {dupes}}."
      )
    )
  }
  invisible(lookup)
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
      # Cap the removed (use + burn) fraction at 1, preserving the feed:burn
      # ratio, so the three destinies always sum to residue_dm_t and soil stays
      # non-negative even on out-of-range share data.
      removed_scale = dplyr::if_else(
        use_share + burn_share > 1,
        1 / (use_share + burn_share),
        1
      ),
      use_share = use_share * removed_scale,
      burn_share = burn_share * removed_scale,
      residue_feed_dm_t = residue_dm_t * use_share,
      residue_burn_dm_t = residue_dm_t * burn_share,
      residue_soil_dm_t = residue_dm_t * (1 - use_share - burn_share),
      residue_destiny_to_be_revised = TRUE
    ) |>
    dplyr::select(-use_share, -burn_share, -removed_scale)
}
