#' Estimate the destinies of crop residues.
#'
#' Splits crop residue dry matter into four destinies that sum to the total
#' residue: fed to livestock, used as animal bedding, otherwise removed
#' (burned, fuel, construction, industry, export), and left on the field for
#' soil incorporation.
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
#' @param bedding_fraction Share of the **removed non-feed** residue used as
#'   animal bedding, in `[0, 1]`. Default `0`, which leaves every published
#'   value where it was: the bedding destiny is reported as zero and
#'   `residue_burn_dm_t` keeps the whole non-feed removal, exactly as before
#'   this argument existed.
#'
#'   **There is deliberately no sourced default**, because no settled global
#'   bedding-only fraction exists. Every global framework merges bedding with
#'   feed: FAO GLEAM 2.0/3.0 `FracRemove` (default 0.45) covers "feed, bedding
#'   and construction" together, and the IPCC 2019 Refinement Vol.4 Ch.11
#'   Eq. 11.6 `FracRemove` has the same scope with no default at all ("if data
#'   are not available, assume no removal"). `residue_feed_fraction`, the
#'   feed-use table this function uses, is drawn from that same literature and
#'   likewise does not isolate bedding. Three anchors exist if a value is
#'   wanted, none of them a settled coefficient:
#'   * Wirsenius (2000, *Human Use of Land and Organic Materials*, PhD thesis,
#'     Chalmers/Göteborg, Table 3.21 p.126): 270 Tg DM/yr of litter, i.e. 14%
#'     of *distributed* cereal straw and stover and 11% of all distributed crop
#'     by-products, with region x species rates in Table 3.13 p.86. The author
#'     grades these as "very rough figures", and South and Central Asia cattle
#'     is entered as 0 because the data were absent — an admitted gap that must
#'     not be inherited as an estimate.
#'   * Statistics Denmark tables HALM / HALM1 / HALM2, "Straw yield and use",
#'     1997-2025, the only official statistic carrying a bedding-only column:
#'     16-21% of straw production and about 30% of the removed straw.
#'   * Bentsen, Felby and Thorsen (2014), *Prog. Energy Combust. Sci.*
#'     40:59-73, Table 5 (Denmark, 2006-2008, from the same source): barley
#'     16%, wheat 11% of production. The same paper's section 4.7 is worth
#'     reading before generalising any of this: "very little information exists
#'     on how residues are actually used".
#'
#'   Note also that Smil (1999) carries no bedding fraction, only a
#'   straw-per-manure ratio (about 250 kg straw per tonne of excrement), so it
#'   cannot be cited for a share of production. Picking a number here is the
#'   caller's decision and must be justified where it is set. See whep#1005.
#'
#'   Bedding is taken out of the removed non-feed share, never out of
#'   `residue_soil_dm_t`, which is what the IPCC 2019 Refinement Vol.4 Ch.10
#'   (p. 10.96) requires: bedding coming from crop residues has to be
#'   accounted for in `FracRemove` so it is not also counted as residue
#'   returned to the soil.
#' @return The input tibble with `residue_feed_dm_t`, `residue_bedding_dm_t`,
#'   `residue_burn_dm_t`, `residue_soil_dm_t`, `method_residue_destiny` and
#'   `bedding_fraction`.
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
    .split_residue_bedding(bedding_fraction) |>
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

#' Build the bedding manure input from the residue bedding destiny.
#'
#' @description
#' Turns the bedding destiny of crop residues into the `bedding` contract
#' [split_manure_management()] consumes: bedding dry matter with its carbon and
#' nitrogen, aggregated to `year x territory x sub_territory`.
#'
#' Carbon and nitrogen come from the same `bio_coefs` residue columns the crop
#' NPP and nitrogen-balance paths already use for residue composition
#' (`residue_c_kgdm`, `residue_n_kgdm`, joined on `item_prod_code`), so the
#' straw that arrives in the manure heap carries the composition the rest of
#' the package gives it, and nothing new is assumed here. For the cereals that
#' supply most bedding that table holds 0.40-0.49 kg C and 0.005-0.007 kg N per
#' kg DM, a C:N of roughly 57-98 -- far wider than the excreta it is mixed
#' with, which is why bedding raises the C:N of stored farmyard manure.
#'
#' Those nitrogen values sit at the high end of what is published. Verified
#' per-mass straw nitrogen spans 0.0022 kg N per kg DM (Andersson et al. 2024,
#' *Front. Sustain. Food Syst.* 8:1393674, Table 1, wheat straw used as
#' bedding) through 0.0045-0.0058 in the two European inventory sources
#' (EMEP/EEA Guidebook 2023 Ch. 3.B Table 3-7 footnote a, 4 g N per kg fresh;
#' Rösemann et al., Thünen Report 84, Table 3.6) to the IPCC 2019 Refinement
#' Vol.4 Ch.11 Table 11.1a above-ground residue defaults of 0.006 (wheat,
#' maize) and 0.007 (barley, rice). Since the bedding nitrogen is what moves
#' the applied carbon under the default storage rule, that threefold spread
#' propagates directly; carbon is much tighter (0.398-0.495 kg C per kg DM
#' across the same sources).
#'
#' @param x A tibble with `item_prod_code`, `year`, `territory` and
#'   `residue_bedding_dm_t` (from [calculate_residue_destinies()] with a
#'   non-zero `bedding_fraction`). `sub_territory` is optional and defaults to
#'   `NA`, the national grain.
#' @return A tibble with `year`, `territory`, `sub_territory`, `bedding_dm_t`,
#'   `bedding_c_t` and `bedding_n_t`.
#' @export
#' @examples
#' tibble::tibble(
#'   item_prod_code = "15", year = 2020L, territory = "203",
#'   residue_bedding_dm_t = 1000
#' ) |>
#'   build_residue_bedding_supply()
build_residue_bedding_supply <- function(x) {
  .crop_npp_validate(
    x,
    c("item_prod_code", "year", "territory", "residue_bedding_dm_t"),
    "build_residue_bedding_supply"
  )
  composition <- whep::whep_coef_table("bio_coefs") |>
    dplyr::transmute(
      item_prod_code = as.character(.data$item_prod_code),
      residue_c_kgdm = .data$residue_c_kgdm,
      residue_n_kgdm = .data$residue_n_kgdm
    ) |>
    dplyr::distinct(.data$item_prod_code, .keep_all = TRUE)
  x |>
    tibble::as_tibble() |>
    ensure_columns(
      tibble::tibble(sub_territory = character()),
      extra = "keep"
    ) |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code)) |>
    dplyr::left_join(composition, by = "item_prod_code") |>
    .warn_bedding_no_composition() |>
    dplyr::summarise(
      bedding_dm_t = sum(.data$residue_bedding_dm_t, na.rm = TRUE),
      bedding_c_t = sum(
        .data$residue_bedding_dm_t * .data$residue_c_kgdm,
        na.rm = TRUE
      ),
      bedding_n_t = sum(
        .data$residue_bedding_dm_t * .data$residue_n_kgdm,
        na.rm = TRUE
      ),
      .by = c("year", "territory", "sub_territory")
    )
}

# ---- Private helpers --------------------------------------------------

# Bedding is carved out of the removed non-feed share, never out of the feed or
# the field-left share: a tonne of straw cannot be both bedded and burned, and
# the residue that never leaves the field is not a commodity at all. The four
# destinies therefore still sum to `residue_dm_t` (whep#1005 point 4).
.split_residue_bedding <- function(out, bedding_fraction) {
  dplyr::mutate(
    out,
    residue_bedding_dm_t = .data$residue_burn_dm_t * bedding_fraction,
    residue_burn_dm_t = .data$residue_burn_dm_t * (1 - bedding_fraction),
    bedding_fraction = bedding_fraction
  )
}

.check_bedding_fraction <- function(bedding_fraction) {
  ok <- rlang::is_scalar_double(bedding_fraction) ||
    rlang::is_scalar_integer(bedding_fraction)
  if (!ok || is.na(bedding_fraction)) {
    cli::cli_abort(
      "{.arg bedding_fraction} must be a single non-missing number.",
      class = "whep_error_bedding_fraction"
    )
  }
  if (bedding_fraction < 0 || bedding_fraction > 1) {
    cli::cli_abort(
      "{.arg bedding_fraction} must lie in {.val {c(0, 1)}}, not
       {.val {bedding_fraction}}.",
      class = "whep_error_bedding_fraction"
    )
  }
  invisible(NULL)
}

# Say when bedding dry matter has no residue composition to convert it with,
# instead of summing it away to zero carbon and zero nitrogen. `bio_coefs`
# carries no residue row for some `item_prod_code`s, and na.rm in the sums
# below would turn that into a silent loss of the carbon and nitrogen the whole
# point of this function is to trace.
.warn_bedding_no_composition <- function(joined) {
  gap <- joined[
    is.na(joined$residue_c_kgdm) | is.na(joined$residue_n_kgdm),
    ,
    drop = FALSE
  ]
  gap <- gap[gap$residue_bedding_dm_t > 0, , drop = FALSE]
  if (nrow(gap) == 0L) {
    return(joined)
  }
  items <- unique(gap$item_prod_code)
  cli::cli_warn(c(
    "{length(items)} crop{?s} bedding dry matter has no {.field bio_coefs}
     residue composition: {.val {items}}.",
    i = "{round(sum(gap$residue_bedding_dm_t))} t of bedding dry matter
      contributes no carbon or nitrogen to the manure streams."
  ))
  joined
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
