#' Estimate the destinies of crop residues.
#'
#' Splits crop residue dry matter into three destinies that sum to the total
#' residue: fed to livestock, burned / removed for fuel, and left on the field
#' for soil incorporation.
#'
#' @param x A tibble with `item_prod_code` and `residue_dm_t`. The
#'   `recovery_regional` method also needs `region_krausmann` (for the recovery
#'   rate) and `region_un_sub` (for the feed-use fraction, the UN M49
#'   sub-regions of `regions_full$region_UN_sub`). `region_krausmann` can use
#'   the recovery-table labels or the matching `regions_full` labels. The
#'   `shares` method needs `year`.
#' @param method Destiny method: `"recovery_regional"` (default, the regional
#'   recovery rate times the UN-sub-regional feed-use fraction) or `"shares"`
#'   (the Spain-specific per-crop-year use/burn shares, flagged
#'   `to_be_revised`).
#' @return The input tibble with `residue_feed_dm_t`, `residue_burn_dm_t`,
#'   `residue_soil_dm_t` and `method_residue_destiny`.
#' @section Where the recovery rates come from:
#' The `recovery_regional` recovery rates live in
#' `inst/extdata/coefs/residue_recovery.csv`, whose per-row `source` column
#' records provenance. They are **Wirsenius (2000)**, *Human Use of Land and
#' Organic Materials*, PhD thesis, Chalmers University of Technology --
#' Table 3.17 (recovery rates, p. 94) and Table 3.16 (harvest index, p. 92,
#' from which `residue_dm_product_dm` is the residue:product ratio rounded to
#' one decimal). The file was named after Krausmann and its two key columns
#' still are, but no coefficient in it is Krausmann's. Only the crop category
#' (`items_prod_full$Cat_Krausmann`) really is his: `region_krausmann` here
#' holds `regions_full$region_HANPP` labels, whose eight values are
#' Wirsenius's eight regions (whep#1132).
#'
#' Nine of the twenty crop categories match Table 3.17 cell for cell. The
#' rest do not, and the `source` column says which is which:
#' `Groundnuts in Shell` uses the cereals pattern where Table 3.17 gives 0.90
#' everywhere, and ten categories -- roots, tubers, cassava, sugar beet, sugar
#' crops nes, dry beans, pulses, oil palm, castor beans, permanent and fodder
#' crops -- appear in neither table, so their recovery rates are assumed and
#' unverified. Wirsenius states that recovery rates for the crop flows Table
#' 3.17 omits "were assumed to be close to 100 percent"; this table instead
#' assumes between 0 and 0.9 for them, which is why fodder crops recover
#' nothing at all here.
#' @section The feed-use fraction is a different, unpaired source:
#' Wirsenius coordinates Table 3.17 directly with the feed assignments of his
#' Table 3.20 (p. 102) -- "assumptions on recovery rates were directly
#' coordinated with those on assignment for use as feed". WHEP does not use
#' Table 3.20: the feed half comes from `residue_feed_fraction.csv` (Smil
#' 1999, Lal 2005, Krausmann 2008, Erenstein 2014, McIntire 1992), whose named
#' values span 0.05 to 0.45 around a 0.20 global default. That is well under
#' the "some 33 percent of the amount generated" Wirsenius reports for cereals
#' straw and stover fed to animals (p. 177), and under the livestock share of
#' Smerald, Rahimi & Scheer (2023), *Scientific Data* **10**:685,
#' \doi{10.1038/s41597-023-02587-0}. Re-anchoring it is **not** done here on
#' purpose: the gross residue base it multiplies is itself too high, so the
#' two errors partly cancel and fixing one alone would land further from the
#' truth (whep#1132, whep#1041).
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
  method = c("recovery_regional", "shares")
) {
  method <- rlang::arg_match(method)
  .crop_npp_validate(
    x,
    c("item_prod_code", "residue_dm_t"),
    "calculate_residue_destinies"
  )
  out <- switch(
    method,
    recovery_regional = .residue_destiny_recovery(x),
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
# spanning 0.05 to 0.45, dead in full and silently. The recovery vocabulary IS
# bridged through regions_full (see .residue_recovery_region), but
# that trick cannot be reused here: region_UN_sub -> region_HANPP is not 1:1
# (M49 puts Sudan in Northern Africa, HANPP with Sub-saharan Africa; Greenland
# is M49 Northern America, HANPP West Europe), so the caller must supply the M49
# sub-region itself rather than have one derived from a HANPP label.
#
# The two halves are NOT a matched pair. The recovery rates are Wirsenius
# (2000) Table 3.17, which that thesis coordinates with its own Table 3.20
# feed shares; the feed fraction here is a different and smaller set. See the
# "Where the recovery rates come from" section above and whep#1132.
.residue_destiny_recovery <- function(x) {
  if (!all(c("region_krausmann", "region_un_sub") %in% names(x))) {
    cli::cli_abort(
      "method {.val recovery_regional} needs {.field region_krausmann} \\
       and {.field region_un_sub}."
    )
  }
  cat_map <- whep::items_prod_full |>
    dplyr::transmute(
      item_prod_code = as.character(item_prod_code),
      cat_krausmann = Cat_Krausmann
    )
  recovery <- whep::whep_coef_table("residue_recovery") |>
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
