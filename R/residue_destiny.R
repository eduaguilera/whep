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
#' @section Where the bedding share comes from:
#' `bedding = "wirsenius"` (default) reads the one bedding figure that comes
#' from the same accounting as the recovery rates: **Wirsenius (2000)**, *Human
#' Use of Land and Organic Materials*, PhD thesis, Chalmers University of
#' Technology, Table 3.21 (p. 126). Of 2700 Tg DM/yr of cereals straw & stover
#' generated, 530 Tg is "not recovered", so 2170 Tg is recovered, and 270 Tg is
#' "used as litter for animal bedding" -- 14% of the 1940 Tg *distributed*.
#' This function's denominator is the *recovered* residue, so the share is
#' taken against the table's own recovered total, `270 / 2170 = 0.124`. That
#' is the same distributed -> recovered conversion whep#1431 makes for the
#' Table 3.20 feed shares; here the world table carries both totals, so no loss factor
#' has to be assumed (the 10% loss route, `0.14 * 0.90`, gives 0.126).
#'
#' Wirsenius models cereals straw and stover as the *only* bedding material
#' (p. 85, after Section 2.3.1), so the share applies to the five cereal
#' categories (`Wheat, other cereals`, `Rice, Paddy`, `Maize`, `Sorghum`,
#' `Millet`) and
#' every other crop beds nothing. It is one **world** share applied in every
#' region: the thesis prints no regional litter total, and its regional
#' litter-use rates (Table 3.13, p. 86, kg straw per kg housed manure) are
#' "very rough figures" (p. 85), with South & Central Asia cattle set to zero
#' because "no reasonable values on litter use could be identified" -- not an
#' estimate to inherit. The bedding is carved out of the recovered non-feed
#' residue and capped at it; where the cap binds a warning says how much.
#'
#' `bedding = "fraction"` keeps the caller-set rule: `bedding_fraction` of the
#' recovered non-feed residue of every crop, default `0` (no bedding). Two
#' other anchors are on that denominator family, neither of them global:
#'
#' * Statistics Denmark HALM/HALM1/HALM2 -- the only official statistic with a
#'   bedding-only column: 16-21% of straw *production*, about 30% of *removed*
#'   straw.
#' * Bentsen, Felby & Thorsen (2014), Prog. Energy Combust. Sci. 40:59-73,
#'   Table 5 -- Denmark, barley 16% and wheat 11% of *production*.
#'
#' FAO GLEAM's `FracRemove` and IPCC 2019 Eq. 11.6's `FracRemove` both merge
#' bedding with feed and construction into one term, so neither is usable.
#' The rule used is recorded in `method_residue_bedding`.
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
#'   as livestock bedding under `bedding = "fraction"`, one number in `[0, 1]`.
#'   Default `0`. Must stay `0` under `bedding = "wirsenius"`, which sets its
#'   own share.
#' @param unmatched_recovery What the `recovery_regional` method does with a
#'   row that reaches no recovery rate at all, because its crop carries no
#'   Krausmann category or its region label reaches no recovery region:
#'   `"report"` (default) keeps the historical all-to-soil treatment and warns
#'   with the row count and tonnage, `"abort"` refuses to continue. Ignored by
#'   the `"shares"` method.
#' @param recovery Which recovery-rate variant the `recovery_regional`
#'   method reads: `"wirsenius"` (default since whep#1330, every rate
#'   Wirsenius 2000 states, at the value it states) or `"legacy"` (the table
#'   as shipped before whep#1163). See the Two recovery variants section.
#'   Ignored by the `"shares"` method.
#' @param feed_share Which feed-use fraction the `recovery_regional` method
#'   reads: `"wirsenius"` (default since whep#1398, Wirsenius 2000 Table 3.20,
#'   per crop by-product and Wirsenius region, the feed half of the recovery
#'   rates) or `"legacy"` (`residue_feed_fraction.csv`, one fraction per UN
#'   M49 sub-region for every crop). See the Two feed-share variants section.
#'   Ignored by the `"shares"` method.
#' @param bedding Bedding rule: `"wirsenius"` (default, 12.4% of the recovered
#'   cereal straw, Wirsenius 2000 Table 3.21) or `"fraction"` (`bedding_fraction`
#'   of the recovered non-feed residue of every crop). See the Where the bedding
#'   share comes from section.
#' @return The input tibble with `residue_feed_dm_t`, `residue_bedding_dm_t`,
#'   `residue_burn_dm_t`, `residue_soil_dm_t`, `residue_bedding_fraction` (the
#'   share applied: of the recovered residue under `"wirsenius"`, zero for a
#'   non-cereal; of the recovered non-feed residue under `"fraction"`),
#'   `method_residue_bedding`, `method_residue_destiny`, and
#'   `method_residue_recovery` (the `recovery` variant, `NA` for the `"shares"`
#'   method) and `method_residue_feed` (per row, the table that priced its feed
#'   share: `"wirsenius"` or `"legacy"`, `NA` for the `"shares"` method). The
#'   `"recovery_regional"`
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
#' Wirsenius's eight region names (whep#1132), though not his country
#' membership (see the Two feed-share variants section).
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
#'   number it prints -- and the five categories the source is silent on keep
#'   the legacy assumed rate. `source_recovery_wirsenius` labels each.
#' * `"legacy"` reads `recovery_rates`, the table as shipped before
#'   whep#1163, whose departures from the source are all downward.
#'
#' `"wirsenius"` is the only variant in which every rate is traceable to the
#' cited source, and it is the default since whep#1330. Its switch waited on
#' the gross residue base these rates multiply, once thought to be about 36%
#' too high (step 2 of whep#1132). whep#1330 found that excess gone in dry
#' matter, but it measured the `crop_residues` pin as it was then read, and
#' that pin holds residue already multiplied by the `"legacy"` rate
#' (whep#1195). Read as the residue produced, world cereal residue in dry
#' matter was 16.1% above the three-method mean of Smerald, Rahimi & Scheer
#' (2023), \doi{10.1038/s41597-023-02587-0}, over 1997--2021. Since whep#1448
#' [get_primary_residues()] estimates cereal residue from yield, and the
#' 1997--2021 mean is 0.8% above theirs, inside their band in every year
#' (`validation/residue_base_dm.R`). The rates the switch moves are all
#' non-cereal, and no published global total for the non-cereal residue base
#' was found to check it against.
#'
#' Both rate columns are keyed on Wirsenius's eight regions, so a row takes
#' his own region membership (Table 3.1, p. 58), not the HANPP label's: from
#' its HANPP region and UN M49 sub-region, through `residue_feed_regions.csv`.
#' The HANPP label files Southeast Asia, Russia, Belarus and the Caucasus
#' under South & Central Asia, whose rates are the highest (whep#1430). On
#' the commodity balance that lowers residue production by 2.4% in 2010 and
#' 2.7% in 2020.
#'
#' Measured on [get_primary_residues()]'s fresh `value` (the commodity
#' balance's basis), `"wirsenius"` against `"legacy"` in 2010 raises
#' recovered residue from 6939 to 7148 Mt fresh matter (+3.0%), the feed
#' destiny from 1964 to 2025 Mt (+3.1%) and the burned/other-use destiny from
#' 4976 to 5123 Mt (+3.0%), and lowers the soil destiny from 1789 to 1580 Mt
#' (-11.7%); over 1961--1965 the same moves are +6.3%, +5.4%, +6.6% and
#' -20.4%. Roots and tubers, cassava, sugar beet and groundnut are the only
#' categories that move.
#'
#' Three categories move no mass at all today, which is why the fodder rate
#' of 0 is not the live problem it looks like:
#' `Sugar Crops nes` and `Oil Palm Fruit` are in the table and in no
#' production item, and no `Fodder crops` item reaches the residue pin.
#' @section Two feed-share variants:
#' `feed_share =` selects the fraction of the **recovered** residue booked as
#' feed (the rest is the burned / other-use destiny), and
#' `method_residue_feed` records, per row, which table priced it:
#'
#' * `"wirsenius"` (default) reads `residue_feed_assignment.csv`: Wirsenius (2000)
#'   Table 3.20 (p. 102), the "Assignm." rows for crop by-products, which the
#'   thesis coordinates directly with the Table 3.17 recovery rates --
#'   "assumptions on recovery rates were directly coordinated with those on
#'   assignment for use as feed" (p. 95). One row covers rice straw, one the
#'   other cereals (read for millet too, as Table 3.17 is), and one each
#'   cassava leaves, potato tops (read for all roots and tubers), sugar cane
#'   tops, sugar beet tops, groundnut stalks and the other oil crops. The seven
#'   categories it has no row for keep the legacy fraction, and their rows say
#'   `"legacy"`.
#' * `"legacy"` reads `residue_feed_fraction.csv` (Smil 1999, Lal 2005,
#'   Krausmann 2008, Erenstein 2014, McIntire 1992): one fraction per UN M49
#'   sub-region, 0.05 to 0.45 around a 0.20 global default, the same for every
#'   crop. It is not paired with the recovery rates, and was the only feed
#'   share before whep#1398.
#'
#' Table 3.20 is a share of the amount **distributed**, which is not the
#' amount recovered: Wirsenius sets the distribution and storage losses of
#' all crop by-products to 10% (p. 98), and in North America & Oceania leaves
#' 5% of the recovered cereal and soybean straw in the field (p. 95, note
#' 117). So the share of recovered residue fed is the printed value times
#' 0.90 (0.855 for those North American straws): wheat straw in South & Central
#' Asia is 0.80 of the distributed straw and 0.72 of the recovered. Read
#' without the conversion it would book more feed than Wirsenius does.
#'
#' Like the recovery rates, the table is keyed on Wirsenius's own region
#' membership (Table 3.1, p. 58), through `residue_feed_regions.csv`. It
#' matters more here: the HANPP label files Southeast Asia, Russia and
#' Belarus, and the Caucasus under South and Central Asia, whose cereal feed
#' share is 0.80, where Wirsenius has them at 0.30, 0.10 and 0.60.
#'
#' Measured on [get_primary_residues()] in dry matter, the cereals' feed
#' share of their generated residue is 0.224 (2010) and 0.225 (2020) under
#' `"legacy"`, 0.305 and 0.309 under `"wirsenius"`. Wirsenius reports "some
#' 33 percent of the amount generated" for the early 1990s (p. 177), and
#' Smerald, Rahimi & Scheer (2023), *Scientific Data* **10**:685,
#' \doi{10.1038/s41597-023-02587-0}, a share of 0.331 of the residue produced
#' going to livestock. That figure is read from the authors' Zenodo notebook
#' (whep#1132), not printed in the article, and their livestock category is
#' "animal feed or bedding", so it is an upper bound for feed alone. The
#' residue the commodity balance books as feed (Straw and Other crop
#' residues) rises from 1089 to 1550 Tg of dry matter in 2010 (+42%) and
#' from 1293 to 1856 Tg in 2020 (+44%); recovered and soil-returned residue
#' do not move. In the 2010 national feed allocation ([get_feed_intake()]),
#' residue intake rises from 1578 to 1985 Tg of dry matter and grass intake
#' falls from 2703 to 2367 Tg (-12.4%), the residue displacing grazing.
#' These are measured on the yield-based cereal residue base of whep#1448.
#'
#' The default is `"wirsenius"` because it is the half of a matched pair whose
#' other half, the recovery rates, is already the default, every value it
#' reads is traceable to the source, and it lands within 8% of both
#' published anchors above, where the legacy table falls about a third short
#' of them.
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
  recovery = c("wirsenius", "legacy"),
  feed_share = c("wirsenius", "legacy"),
  bedding = c("wirsenius", "fraction")
) {
  method <- rlang::arg_match(method)
  unmatched_recovery <- rlang::arg_match(unmatched_recovery)
  recovery <- rlang::arg_match(recovery)
  feed_share <- rlang::arg_match(feed_share)
  bedding <- rlang::arg_match(bedding)
  .check_bedding_fraction(bedding_fraction, bedding)
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
      recovery,
      feed_share
    ),
    shares = dplyr::mutate(
      .residue_destiny_shares(x),
      # The shares table carries its own use share; neither feed table is read.
      method_residue_feed = NA_character_
    )
  )
  out |>
    .residue_carve_bedding(bedding, bedding_fraction) |>
    dplyr::mutate(
      method_residue_bedding = bedding,
      method_residue_destiny = method,
      # The recovery table is read by the recovery_regional method only.
      method_residue_recovery = if (method == "shares") {
        NA_character_
      } else {
        recovery
      }
    )
}

# ---- Private helpers --------------------------------------------------

.check_bedding_fraction <- function(bedding_fraction, bedding) {
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
  # A fraction passed alongside the sourced rule would be silently ignored, so
  # the two are refused together rather than one quietly winning.
  if (bedding == "wirsenius" && bedding_fraction != 0) {
    cli::cli_abort(
      c(
        "{.arg bedding_fraction} is only read under
         {.code bedding = \"fraction\"}.",
        i = "{.code bedding = \"wirsenius\"} sets its own share; pass
             {.code bedding = \"fraction\"} to use {.val {bedding_fraction}}."
      ),
      class = "whep_bedding_rule_conflict"
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
# The share is recorded on the rows rather than only in a method label,
# because it is a magnitude and a downstream reader has to be able to tell a
# build with bedding switched on from one without it.
.residue_carve_bedding <- function(out, bedding, bedding_fraction) {
  if (bedding == "fraction") {
    return(dplyr::mutate(
      out,
      residue_bedding_dm_t = .data$residue_burn_dm_t * bedding_fraction,
      residue_burn_dm_t = .data$residue_burn_dm_t - .data$residue_bedding_dm_t,
      residue_bedding_fraction = bedding_fraction
    ))
  }
  share <- dplyr::if_else(
    .residue_beds_straw(out$item_prod_code),
    .wirsenius_bedding_share(),
    0
  )
  wanted <- (out$residue_feed_dm_t + out$residue_burn_dm_t) * share
  .warn_bedding_capped(wanted, out$residue_burn_dm_t)
  dplyr::mutate(
    out,
    residue_bedding_dm_t = pmin(wanted, .data$residue_burn_dm_t),
    residue_burn_dm_t = .data$residue_burn_dm_t - .data$residue_bedding_dm_t,
    residue_bedding_fraction = share
  )
}

# Wirsenius (2000), PhD thesis, Chalmers University of Technology, Table 3.21
# (p. 126), world totals for cereals straw & stover in Tg DM/yr: 2700
# generated, 530 not recovered, 270 used as litter for animal bedding. The
# share is litter over RECOVERED straw (2700 - 530), the denominator
# calculate_residue_destinies() multiplies; see its roxygen section.
.wirsenius_bedding_share <- function() {
  270 / (2700 - 530)
}

# Cereals straw and stover is the only bedding material Wirsenius models
# (p. 85, after Section 2.3.1), so only the five cereal recovery categories
# bed.
.residue_beds_straw <- function(item_prod_code) {
  cereals <- c(
    "Wheat, other cereals",
    "Rice, Paddy",
    "Maize",
    "Sorghum",
    "Millet"
  )
  cats <- whep::items_prod_full |>
    dplyr::filter(.data$Cat_Krausmann %in% cereals) |>
    dplyr::pull("item_prod_code") |>
    as.character()
  as.character(item_prod_code) %in% cats
}

# The sourced share is of the recovered residue, but bedding can only come out
# of what is not fed. Where the feed share leaves less than that, the carve is
# capped at the non-feed residue, and the bedding lost to the cap is said out
# loud rather than vanishing.
.warn_bedding_capped <- function(wanted, burn) {
  short <- wanted - burn
  capped <- !is.na(short) & short > 1e-9 * pmax(wanted, 1)
  if (!any(capped)) {
    return(invisible(NULL))
  }
  n_rows <- sum(capped)
  lost <- signif(sum(short[capped]), 3)
  cli::cli_warn(
    c(
      "!" = "Bedding capped at the non-feed residue on {n_rows} row(s).",
      "i" = "Bedding not carved because the feed share leaves too little:
             {lost} t of dry matter."
    ),
    class = "whep_bedding_capped"
  )
  invisible(NULL)
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
# Under `feed_share = "legacy"` the two halves are NOT a matched pair: the
# recovery rates are Wirsenius (2000) Table 3.17, which that thesis
# coordinates with its own Table 3.20 feed shares, and the legacy fraction is
# a different and smaller set (whep#1132). `feed_share = "wirsenius"` reads
# Table 3.20 itself (whep#1398); see .residue_feed_use_fraction().
.residue_destiny_recovery <- function(
  x,
  unmatched_recovery = "report",
  recovery = "wirsenius",
  feed_share = "wirsenius"
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
  joined <- x |>
    dplyr::mutate(
      item_prod_code = as.character(item_prod_code),
      # The rates are Wirsenius's, keyed on his eight regions, so the row
      # takes his region membership, not the HANPP label's (whep#1430).
      region_krausmann = .residue_wirsenius_region(
        .residue_recovery_region(.data$region_krausmann),
        .data$region_un_sub
      )
    ) |>
    dplyr::left_join(cat_map, by = "item_prod_code") |>
    dplyr::left_join(recovery, by = c("cat_krausmann", "region_krausmann")) |>
    .residue_feed_use_fraction(feed_share) |>
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

# The feed-use fraction a `feed_share =` variant reads, attached per row as
# `feed_use_fraction` (share of the RECOVERED residue fed) with the variant
# that priced the row in `method_residue_feed` (whep#1398).
#
# `"wirsenius"` reads Wirsenius (2000) Table 3.20 on the recovery table's own
# key, (crop category, Wirsenius region): by this point `region_krausmann`
# holds the row's Wirsenius region, from `.residue_wirsenius_region()`
# (whep#1430). The categories that table has no row for
# keep the legacy UN-sub-region fraction, and their rows say `"legacy"`, so a
# coefficient the source does not give is never passed off as one it does.
.residue_feed_use_fraction <- function(joined, feed_share) {
  legacy <- whep::whep_coef_table("residue_feed_fraction") |>
    dplyr::select(region_un_sub, legacy_fraction = feed_use_fraction)
  global_feed <- legacy$legacy_fraction[legacy$region_un_sub == "Global"]
  out <- joined |>
    dplyr::left_join(legacy, by = "region_un_sub") |>
    dplyr::mutate(
      legacy_fraction = tidyr::replace_na(.data$legacy_fraction, global_feed)
    )
  if (feed_share == "legacy") {
    return(
      out |>
        dplyr::mutate(
          feed_use_fraction = .data$legacy_fraction,
          method_residue_feed = "legacy"
        ) |>
        dplyr::select(-"legacy_fraction")
    )
  }
  assignment <- whep::whep_coef_table("residue_feed_assignment") |>
    dplyr::select(cat_krausmann, region_wirsenius, feed_use_fraction)
  out |>
    dplyr::left_join(
      assignment,
      by = c("cat_krausmann", region_krausmann = "region_wirsenius")
    ) |>
    dplyr::mutate(
      method_residue_feed = dplyr::if_else(
        is.na(.data$feed_use_fraction),
        "legacy",
        "wirsenius"
      ),
      feed_use_fraction = dplyr::coalesce(
        .data$feed_use_fraction,
        .data$legacy_fraction
      )
    ) |>
    dplyr::select(-"legacy_fraction")
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
