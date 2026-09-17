#' Estimate the destinies of crop residues.
#'
#' Splits crop residue dry matter into three destinies that sum to the total
#' residue: fed to livestock, burned / removed for fuel, and left on the field
#' for soil incorporation.
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
#' @param unmatched_recovery What the `krausmann_regional` method does with a
#'   row that reaches no recovery rate at all, because its crop carries no
#'   Krausmann category or its region label reaches no recovery region:
#'   `"report"` (default) keeps the historical all-to-soil treatment and warns
#'   with the row count and tonnage, `"abort"` refuses to continue. Ignored by
#'   the `"shares"` method.
#' @return The input tibble with `residue_feed_dm_t`, `residue_burn_dm_t`,
#'   `residue_soil_dm_t` and `method_residue_destiny`. The
#'   `"krausmann_regional"` method also returns `residue_recovery_matched`,
#'   `FALSE` where no recovery rate was found, which is what separates a rate
#'   the table gives as zero from a zero standing in for a failed lookup.
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
  unmatched_recovery = c("report", "abort")
) {
  method <- rlang::arg_match(method)
  unmatched_recovery <- rlang::arg_match(unmatched_recovery)
  .crop_npp_validate(
    x,
    c("item_prod_code", "residue_dm_t"),
    "calculate_residue_destinies"
  )
  out <- switch(
    method,
    krausmann_regional = .residue_destiny_krausmann(x, unmatched_recovery),
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
.residue_destiny_krausmann <- function(x, unmatched_recovery = "report") {
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
  joined <- x |>
    dplyr::mutate(
      item_prod_code = as.character(item_prod_code),
      region_krausmann = .residue_recovery_region(.data$region_krausmann)
    ) |>
    dplyr::left_join(cat_map, by = "item_prod_code") |>
    dplyr::left_join(recovery, by = c("cat_krausmann", "region_krausmann")) |>
    dplyr::left_join(feed, by = "region_un_sub") |>
    dplyr::mutate(
      # A rate the table GIVES as zero -- 18 of its 160 rows, e.g. fodder crops
      # in West Europe -- and a zero standing in for a rate the join never found
      # are the same number with opposite meanings, and `replace_na()` erased
      # the difference. The row is then booked entirely to soil, no total moves,
      # and a mass balance reconciles exactly, so nothing downstream could see
      # it (whep#1175). Record which it was, per row, BEFORE the substitution
      # makes the two indistinguishable.
      residue_recovery_matched = !is.na(recovery_rates),
      recovery_rates = tidyr::replace_na(recovery_rates, 0),
      feed_use_fraction = tidyr::replace_na(feed_use_fraction, global_feed),
      residue_feed_dm_t = residue_dm_t * recovery_rates * feed_use_fraction,
      residue_burn_dm_t = residue_dm_t *
        recovery_rates *
        (1 - feed_use_fraction),
      residue_soil_dm_t = residue_dm_t * (1 - recovery_rates)
    )
  .check_unmatched_recovery(joined, unmatched_recovery)
  dplyr::select(joined, -cat_krausmann, -recovery_rates, -feed_use_fraction)
}

# Say -- or refuse -- when a residue row reached no recovery rate.
#
# Both axes of the lookup can miss: the crop may carry no `Cat_Krausmann`, and
# the row's region label may reach no recovery region. The second is what
# whep#1175 measured at 16.65 Gt, and the guard whep#1162 adds cannot see it
# because it checks that every category carries all eight regions, not that
# every row's region is one of them.
#
# `"report"` keeps the historical behaviour exactly -- the rate is zero and the
# residue stays on the field -- so the default moves no published value. What
# it stops being is silent.
.check_unmatched_recovery <- function(joined, action) {
  unmatched <- !joined$residue_recovery_matched
  if (!any(unmatched)) {
    return(invisible(NULL))
  }
  # No cli pluralisation markers: `{?s}` beside a bare numeric vector aborts
  # inside its own message. Plain wording cannot fail.
  n_rows <- sum(unmatched)
  mass_mt <- round(sum(joined$residue_dm_t[unmatched], na.rm = TRUE) / 1e6)
  regions <- sort(unique(dplyr::coalesce(
    as.character(joined$region_krausmann[unmatched]),
    "<no region>"
  )))
  msg <- c(
    "!" = "{n_rows} residue rows reached no recovery rate, so all of their
       residue is booked to soil: {mass_mt} Mt of dry matter.",
    "i" = "Regions with no rate: {.val {regions}}. A missing rate is not a rate
       of zero; {.field residue_recovery_matched} separates the two."
  )
  if (identical(action, "abort")) {
    cli::cli_abort(msg, class = "whep_unmatched_recovery")
  }
  cli::cli_warn(msg, class = "whep_unmatched_recovery")
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
