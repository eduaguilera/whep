#' Estimate the destinies of crop residues.
#'
#' Splits crop residue dry matter into four destinies that sum to the total
#' residue: fed to livestock, used as livestock bedding, burned / removed for
#' fuel, and left on the field for soil incorporation.
#'
#' @section Bedding:
#' Bedding straw leaves the field with the rest of the recovered residue and
#' comes back to the soil later, through the yard, as part of the managed
#' manure. It is therefore carved out of the recovered **non-feed** residue --
#' the mass the commodity balance books as `other_uses` -- and **never** out of
#' `residue_soil_dm_t`, which is the residue that stays on the field. That is
#' the split IPCC 2019 Refinement Vol. 4 Ch. 10 p. 10.95 asks for when it tells
#' inventory compilers to cross-check bedding nitrogen "relative to the amount
#' of agricultural residues that is removed for other purposes (i.e. bedding)
#' other than the amount of agricultural residues returned to soils or burnt",
#' so as "to eliminate the possibility of double counting".
#'
#' `bedding_fraction` defaults to **0**, and that default is *unset, not
#' measured*: no global bedding-only fraction of crop residue could be sourced
#' (whep#1005). FAO GLEAM's `FracRemove` and IPCC 2019 Eq. 11.6's `FracRemove`
#' both merge bedding with feed and construction into one term. Three partial
#' anchors exist and none is on this function's denominator, so each needs
#' converting before it can be used here:
#'
#' * Wirsenius (2000), PhD thesis, Chalmers University of Technology, Table
#'   3.21 p. 126 -- litter is 14% of *distributed* cereal straw and stover and
#'   11% of distributed crop by-products. The author grades these "very rough",
#'   and the South & Central Asia cattle entry is 0 because the data were
#'   absent, which must not be inherited as an estimate.
#' * Statistics Denmark HALM/HALM1/HALM2 -- the only official statistic with a
#'   bedding-only column: 16-21% of straw *production*, about 30% of *removed*
#'   straw.
#' * Bentsen, Felby & Thorsen (2014), Prog. Energy Combust. Sci. 40:59-73,
#'   Table 5 -- Denmark, barley 16% and wheat 11% of *production*.
#'
#' @param x A tibble with `item_prod_code` and `residue_dm_t`. The
#'   `krausmann_regional` method also needs `region_krausmann` (for the recovery
#'   rate) and `region_un_sub` (for the feed-use fraction, the UN M49
#'   sub-regions of `regions_full$region_UN_sub`). `region_krausmann` can use
#'   the recovery-table labels or the matching `regions_full` labels. The
#'   `shares` method needs `year`.
#' @param method Destiny method: `"krausmann_regional"` (default, Krausmann
#'   recovery x UN-sub-regional feed-use fraction) or `"shares"` (the
#'   Spain-specific per-crop-year use/burn shares, flagged `to_be_revised`).
#' @param bedding_fraction Fraction of the recovered **non-feed** residue used
#'   as livestock bedding, one number in `[0, 1]`. Default `0`, which is unset
#'   rather than measured; see the Bedding section for why, and what a caller
#'   setting it must convert from.
#' @return The input tibble with `residue_feed_dm_t`, `residue_bedding_dm_t`,
#'   `residue_burn_dm_t`, `residue_soil_dm_t`, `residue_bedding_fraction` and
#'   `method_residue_destiny`.
#' @export
#' @examples
#' calculate_residue_destinies(
#'   tibble::tibble(
#'     item_prod_code = "15", residue_dm_t = 100,
#'     region_krausmann = "Western Europe", region_un_sub = "Western Europe"
#'   )
#' )
calculate_residue_destinies <- function(
  x,
  method = c("krausmann_regional", "shares"),
  bedding_fraction = 0
) {
  method <- rlang::arg_match(method)
  .check_bedding_fraction(bedding_fraction)
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
  out |>
    .residue_carve_bedding(bedding_fraction) |>
    dplyr::mutate(method_residue_destiny = method)
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

.check_bedding_fraction <- function(bedding_fraction) {
  ok <- rlang::is_bare_numeric(bedding_fraction, n = 1) &&
    !is.na(bedding_fraction) &&
    bedding_fraction >= 0 &&
    bedding_fraction <= 1
  if (!ok) {
    cli::cli_abort(
      "{.arg bedding_fraction} must be one number between 0 and 1, not
       {.val {bedding_fraction}}."
    )
  }
  invisible(NULL)
}

# Bedding comes out of the recovered NON-FEED residue, never out of
# residue_soil_dm_t: the soil share never left the field, so routing it through
# a manure heap would move carbon and nitrogen that is already booked as a crop
# input (the double count IPCC 2019 Vol. 4 Ch. 10 p. 10.95 warns about). The
# recovered total -- what the commodity balance carries as residue `production`
# -- is unchanged by the carve, which is what keeps `production = feed +
# other_uses` closed with a fourth destiny in play.
#
# The fraction is recorded on the rows rather than only in a method label,
# because it is a magnitude and a downstream reader has to be able to tell a
# build with bedding switched on from one without it.
.residue_carve_bedding <- function(out, bedding_fraction) {
  dplyr::mutate(
    out,
    residue_bedding_dm_t = .data$residue_burn_dm_t * bedding_fraction,
    residue_burn_dm_t = .data$residue_burn_dm_t - .data$residue_bedding_dm_t,
    residue_bedding_fraction = bedding_fraction
  )
}

# The feed-use fraction is keyed by UN M49 sub-region, not by HANPP region: the
# coefficient table's 17 named values are M49 sub-regions (Sub-Saharan Africa,
# Western Asia, Melanesia, ...), which is what its sources report by. It was
# joined against region_hanpp until #405, and since none of the 8 HANPP labels
# is an M49 sub-region label the join matched nothing and replace_na() below
# gave every polity on earth the "Global" default of 0.2 -- a coefficient table
# spanning 0.05 to 0.45, dead in full and silently. The Krausmann recovery
# vocabulary IS bridged through regions_full (see .residue_recovery_region), but
# that trick cannot be reused here: region_UN_sub -> region_HANPP is not 1:1
# (M49 puts Sudan in Northern Africa, HANPP with Sub-saharan Africa; Greenland
# is M49 Northern America, HANPP West Europe), so the caller must supply the M49
# sub-region itself rather than have one derived from a HANPP label.
.residue_destiny_krausmann <- function(x) {
  if (!all(c("region_krausmann", "region_un_sub") %in% names(x))) {
    cli::cli_abort(
      "method {.val krausmann_regional} needs {.field region_krausmann} \\
       and {.field region_un_sub}."
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
    dplyr::select(region_un_sub, feed_use_fraction)
  global_feed <- feed$feed_use_fraction[feed$region_un_sub == "Global"]
  x |>
    dplyr::mutate(
      item_prod_code = as.character(item_prod_code),
      region_krausmann = .residue_recovery_region(.data$region_krausmann)
    ) |>
    dplyr::left_join(cat_map, by = "item_prod_code") |>
    dplyr::left_join(recovery, by = c("cat_krausmann", "region_krausmann")) |>
    dplyr::left_join(feed, by = "region_un_sub") |>
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
