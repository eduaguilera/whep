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
#'   `recovery_regional` method also needs `region_krausmann` (for the recovery
#'   rate) and `region_un_sub` (for the feed-use fraction, the UN M49
#'   sub-regions of `regions_full$region_UN_sub`). `region_krausmann` can use
#'   the recovery-table labels or the matching `regions_full` labels. The
#'   `shares` method needs `year`.
#' @param method Destiny method: `"recovery_regional"` (default, the regional
#'   recovery rate times the UN-sub-regional feed-use fraction) or `"shares"`
#'   (the Spain-specific per-crop-year use/burn shares, flagged
#'   `to_be_revised`).
#' @param bedding_fraction Fraction of the recovered **non-feed** residue used
#'   as livestock bedding, one number in `[0, 1]`. Default `0`, which is unset
#'   rather than measured; see the Bedding section for why, and what a caller
#'   setting it must convert from.
#' @param unmatched_recovery What the `recovery_regional` method does with a
#'   row that reaches no recovery rate at all, because its crop carries no
#'   Krausmann category or its region label reaches no recovery region:
#'   `"report"` (default) keeps the historical all-to-soil treatment and warns
#'   with the row count and tonnage, `"abort"` refuses to continue. Ignored by
#'   the `"shares"` method.
#' @param recovery Which recovery-rate variant the `recovery_regional`
#'   method reads: `"wirsenius"` (default, every rate Wirsenius 2000 states,
#'   at the value it states) or `"legacy"` (the table as shipped before
#'   whep#1163). See the Two recovery variants section. Ignored by the
#'   `"shares"` method.
#' @return The input tibble with `residue_feed_dm_t`, `residue_bedding_dm_t`,
#'   `residue_burn_dm_t`, `residue_soil_dm_t`, `residue_bedding_fraction` and
#'   `method_residue_destiny`, and `method_residue_recovery` (the `recovery`
#'   variant, `NA` for the `"shares"` method). The `"recovery_regional"`
#'   method also returns
#'   `residue_recovery_matched`, `FALSE` where no recovery rate was found,
#'   which is what separates a rate the table gives as zero from a zero
#'   standing in for a failed lookup.
#' @section Where the recovery rates come from:
#' The `recovery_regional` recovery rates live in
#' `inst/extdata/coefs/residue_recovery.csv`. Its numeric columns are
#' sourced separately and carry a provenance column each (`source_ratio`,
#' `source_recovery` and `source_recovery_wirsenius`), because they agree
#' with the source to different degrees. All are **Wirsenius (2000)**, *Human Use of Land and
#' Organic Materials*, PhD thesis, Chalmers University of Technology --
#' Table 3.17 (recovery rates, p. 94) and Table 3.16 (harvest index, p. 92,
#' from which `residue_dm_product_dm` is the residue:product ratio rounded to
#' one decimal). The file was named after Krausmann and its two key columns
#' still are, but no coefficient in it is Krausmann's. Only the crop category
#' (`items_prod_full$Cat_Krausmann`) really is his: `region_krausmann` here
#' holds `regions_full$region_HANPP` labels, whose eight values are
#' Wirsenius's eight regions (whep#1132).
#'
#' Table 3.17 states its rates per crop **category**, not per crop: one
#' "Cereals straw & stover" row governs every cereal, and one "Sugar crops
#' tops & leaves" row governs both crops Table 3.16 files under sugar crops,
#' cane and beet. Read that way it governs twelve of the twenty categories
#' here, and `source_recovery` splits them (whep#1150):
#'
#' * nine match it cell for cell;
#' * three sit **below** it -- `Groundnuts in Shell`, `Sugar Beets` and
#'   `Sugar Crops nes`, each against 0.90 in every region;
#' * three -- roots and tubers, cassava and oil palm -- are residues the
#'   thesis does model (Table 2.6, pp. 48-49: cassava leaves and tops,
#'   potato tops, oil palm leaves and trunks) but Table 3.17 does not list,
#'   and for those Wirsenius states that recovery rates "were assumed to be
#'   close to 100 percent" (p. 94);
#' * five -- dry beans, pulses, castor beans, permanent crops and fodder
#'   crops -- have **no residue flow in the thesis at all**: Table 2.6 gives
#'   pulses, fruits, tree nuts, vegetables and stimulants "no representation
#'   of by-products" (p. 47), models forage crops whole, and has no castor.
#'   The p. 94 default does not reach them, so the source is silent on them
#'   (whep#1163).
#'
#' @section Two recovery variants:
#' `recovery =` selects the rate column, and `method_residue_recovery`
#' records which one was used:
#'
#' * `"wirsenius"` (default) reads `recovery_rates_wirsenius`: every rate the
#'   thesis states, at the value it states. The three below-source
#'   categories take 0.90, the three p. 94 categories take 1.00 -- "close to
#'   100 percent" read as 1.00, which is a reading of the text and not a
#'   number it prints -- and the five categories the source is silent on
#'   keep the legacy assumed rate. `source_recovery_wirsenius` labels each.
#' * `"legacy"` reads `recovery_rates`, the table as shipped before
#'   whep#1163, whose departures from the source are all downward.
#'
#' `"wirsenius"` is the default because it is the only variant in which
#' every rate is traceable to the cited source; the legacy values it replaces
#' have no source of their own. On the `crop_residues` pin as read by
#' [get_primary_residues()], 1961--2021, it raises recovered residue from
#' 269.9 to 276.4 Gt DM (+2.43%), the feed destiny from 76.5 to 78.4 Gt
#' (+2.51%) and the burned/other-use destiny from 193.4 to 198.0 Gt
#' (+2.40%), and lowers the soil destiny from 57.8 to 51.3 Gt (-11.3%).
#' Roots and tubers (+3.52 Gt recovered), cassava (+1.69), sugar beet
#' (+1.20) and groundnut (+0.14) are the only categories that move.
#'
#' The gross residue base these rates multiply is itself thought to be too
#' high (whep#1132), so raising the rate before that base is corrected moves
#' recovered residue further from the literature, not closer. Pass
#' `recovery = "legacy"` to reproduce the pre-whep#1163 numbers.
#'
#' Three categories move no mass at all today, which is why the fodder rate
#' of 0 is not the live problem it looks like:
#' `Sugar Crops nes` and `Oil Palm Fruit` are in the table and in no
#' production item, and no `Fodder crops` item reaches the residue pin.
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
  method = c("recovery_regional", "shares"),
  bedding_fraction = 0,
  unmatched_recovery = c("report", "abort"),
  recovery = c("wirsenius", "legacy")
) {
  method <- rlang::arg_match(method)
  unmatched_recovery <- rlang::arg_match(unmatched_recovery)
  recovery <- rlang::arg_match(recovery)
  .check_bedding_fraction(bedding_fraction)
  .crop_npp_validate(
    x,
    c("item_prod_code", "residue_dm_t"),
    "calculate_residue_destinies"
  )
  out <- switch(
    method,
    recovery_regional = .residue_destiny_recovery(
      x,
      unmatched_recovery,
      recovery
    ),
    shares = .residue_destiny_shares(x)
  )
  out |>
    .residue_carve_bedding(bedding_fraction) |>
    dplyr::mutate(
      method_residue_destiny = method,
      # The recovery table is read by the recovery_regional method only.
      method_residue_recovery = if (method == "shares") {
        NA_character_
      } else {
        recovery
      }
    )
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
.residue_destiny_recovery <- function(
  x,
  unmatched_recovery = "report",
  recovery = "wirsenius"
) {
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
  recovery <- .residue_recovery_rates(recovery)
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
      # in West Europe -- and a zero standing in for a rate the join never
      # found are the same number with opposite meanings, and `replace_na()`
      # erased the difference. The row is then booked entirely to soil, no
      # total moves, and a mass balance reconciles exactly, so nothing
      # downstream could see it (whep#1175). Record which it was, per row,
      # BEFORE the substitution makes the two indistinguishable.
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
# whep#1175 measured at 16.65 Gt, and whep#1162's guard cannot see it because
# it checks that every category carries all eight regions, not that every row's
# region is one of them.
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

# The recovery-rate column a `recovery =` variant reads (whep#1163). Both live
# side by side in residue_recovery.csv, so the two variants can only ever
# differ in the rate, never in the key set.
.residue_recovery_rates <- function(recovery) {
  rate_col <- switch(
    recovery,
    wirsenius = "recovery_rates_wirsenius",
    legacy = "recovery_rates"
  )
  whep::whep_coef_table("residue_recovery") |>
    dplyr::transmute(
      cat_krausmann,
      region_krausmann,
      recovery_rates = .data[[rate_col]]
    )
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
