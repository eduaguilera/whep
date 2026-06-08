#' Bouwman feed conversion ratios.
#'
#' Feed conversion (kilograms dry matter feed per kilogram product) and per
#' feed type composition for the main production species, from Bouwman et al.
#' (2005). Migrated from the afsetools `Codes_coefs.xlsx` workbook.
#'
#' @format A tibble with one row per species, feed type, region and anchor year:
#' \describe{
#'   \item{item_bouwman}{Bouwman species label (Beef cattle, Dairy cattle, Pigs,
#'     Poultry, Sheep and goats).}
#'   \item{feed_type}{Feed type: animals, crops, grass, residues or scavenging.}
#'   \item{year}{Anchor year (1970, 1995 or 2030).}
#'   \item{region_bouwman}{Bouwman seventeen region label.}
#'   \item{conversion}{Per feed type conversion factor (kg DM feed per kg
#'     product). Sum across feed types is the total feed conversion ratio;
#'     normalised across feed types it gives the default feed composition shares.}
#' }
#' @source Bouwman et al. (2005), via afsetools `Codes_coefs.xlsx`.
"conv_bouwman"

#' Krausmann per head feed intake.
#'
#' Annual feed intake (kilograms dry matter per head per year) for draft and
#' non productive species that lack a product based feed conversion ratio, from
#' Krausmann et al. (2013). Migrated from the afsetools `Codes_coefs.xlsx`
#' workbook.
#'
#' @format A tibble with one row per species:
#' \describe{
#'   \item{item_cbs_code}{FAOSTAT commodity balance item code.}
#'   \item{species}{Species name (Horses, Asses, Mules, Camels and so on).}
#'   \item{conversion}{Feed intake (kg DM per head per year).}
#' }
#' @source Krausmann et al. (2013), via afsetools `Codes_coefs.xlsx`.
"conv_krausmann"

#' Feed taxonomy.
#'
#' Maps each feed item to its feed group, feed quality class, per consumer feed
#' type labels and an allocation priority. Granivores get a restricted feed type
#' set, so only grazers take fibrous roughage. Migrated from the afsetools
#' `Codes_coefs.xlsx` workbook.
#'
#' @format A tibble with one row per feed item:
#' \describe{
#'   \item{item_cbs_code}{FAOSTAT commodity balance item code.}
#'   \item{item_cbs}{Item name.}
#'   \item{feed_group}{Feed group (crop or material class).}
#'   \item{feed_quality}{Feed quality class: lactation, high_quality,
#'     low_quality, residues, grass, zoot_fixed or non_feed.}
#'   \item{feed_quality_rank}{Allocation priority rank (lower is allocated
#'     first): lactation and high_quality 1, low_quality 2, residues 3, grass 4.}
#'   \item{granivore_feedtype}{Feed type the item provides to granivores.}
#'   \item{grazer_feedtype}{Feed type the item provides to grazers.}
#'   \item{zoot_fixed}{Whether intake equals demand regardless of supply
#'     (compound feed ingredients that are not substitutable).}
#' }
#' @source afsetools `Codes_coefs.xlsx`.
"feed_taxonomy"

#' Maximum intake shares.
#'
#' Per livestock category diet share caps that limit how much of a diet a feed
#' item or feed class may make up. Migrated from the afsetools
#' `Livestock_coefs.xlsx` workbook.
#'
#' @format A tibble with one row per cap:
#' \describe{
#'   \item{livestock_category}{Livestock category the cap applies to.}
#'   \item{var}{Cap key type: item_cbs (per item) or Cat_feed (per feed class).}
#'   \item{var_value}{Item or feed class the cap applies to.}
#'   \item{max_intake_share}{Maximum share of the diet (fraction).}
#' }
#' @source afsetools `Livestock_coefs.xlsx`.
"max_intake_share"
