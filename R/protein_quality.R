# Protein quality for the SJOS-N nourishment band.
#
# TRS 935 issues its 0.83 g/kg per day safe level "for proteins with a protein
# digestibility-corrected amino acid score value of 1.0" (s14.2, printed p.242,
# repeated in Table 46 footnote b). A PDCAAS of 1.0 requires a digestibility of
# 1.0, which no real diet has. So under the report's own construction every real
# diet pays at least 1/D, and a band built on the uncorrected safe level is too
# low by that factor -- one-sided, for every country.
#
# THE FIDELITY LADDER, and where this sits on it:
#
#   0   none               shipped until now; band 11-36% too low
#   1b  digestibility_share  Diet digestibility from the animal/plant protein
#                            split. Needs no data WHEP does not have.
#   1a  trs935_item          THIS, and the default. Measured per-item true
#                            digestibility where TRS 935 Table 5 has a row,
#                            falling back to the 1b class rate where it does
#                            not -- the report prints no fruit, vegetable,
#                            root, tuber or sugar row at all.
#   2   full aggregate PDCAAS    + ~88 x 4 amino acid coefficients from an
#                                external composition table; this is the
#                                standard, and the target
#   3   true ileal DIAAS         FAO says the data do not exist
#
# 1b IS A PROVABLE LOWER BOUND ON 2, because PDCAAS = min(1, AAS) x D <= D
# always. State that correctly: it is conservative about the SIZE OF THE
# CORRECTION, not conservative about nourishment adequacy. It under-corrects,
# so it classifies FEWER countries as deficient than the full rule would. Same
# convention, and the same warning, as the Gustavsson half-of-minimum wedge.
#
# NEVER average per-item scores. FAO forbids it in words twice -- TRS 935 p.99
# "the amino acid score for food mixtures should be calculated from the weighted
# average digestible amino acid content", FNP 92 p.17 the same -- and FNP 51
# p.37 gives the reason: "the score of a mixture cannot always be calculated
# with certainty from a knowledge of the individual scores of the components.
# Because of the complementary potential between proteins". Digestibility IS a
# protein-weighted mean and is computed as one here; the amino acid score is
# not, which is why tier 2 cannot be assembled from item scores when it lands.

#' Build the protein-quality correction for the nourishment band.
#'
#' @description
#' Returns the diet's protein quality per country and year, the factor the
#' SJOS-N band divides by. WHO/FAO/UNU TRS 935 issues its safe level of protein
#' intake "for proteins with a protein digestibility-corrected amino acid score
#' value of 1.0" (section 14.2), and no real diet reaches that, so an
#' uncorrected band is too low for every country.
#'
#' `method = "trs935_item"` (default) is **tier 1a**: it uses the measured true
#' digestibility TRS 935 Table 5 publishes for each commodity, and falls back to
#' the tier 1b class rate for the items the report does not measure. Table 5 has
#' 35 rows and prints **no fruit, vegetable, root, tuber or sugar** entry at
#' all, so the fallback is not a corner case — on the 2010 world basket the
#' measured share is **84.5%** of food protein and the rest takes the class
#' rate. `protein_measured_share` reports it per row.
#'
#' `"digestibility_share"` is **tier 1b**: the protein-weighted mean of **0.95
#' for animal protein and 0.80 for plant protein**, which is how TRS 935 Table
#' 43 footnote b computes it. The animal/plant split follows FAO's own Food
#' Balance Sheet grouping — Animal Products (item 2941) against Vegetal Products
#' (2903) — so it reconciles against FAOSTAT's published aggregates rather than
#' being WHEP's opinion. `"none"` returns a quality of 1 and leaves the band on
#' crude protein.
#'
#' **Both are a provable lower bound on the full correction**, because PDCAAS is
#' `min(1, AAS) x D`, which never exceeds `D`. They are conservative about the
#' **size of the correction**, not about nourishment adequacy — they
#' under-correct, and so classify fewer countries as deficient than the full
#' amino acid score would. [build_protein_score()] is that full score, tier 2;
#' it is code-complete and validated but needs a composition table WHEP does not
#' have, and it arrives as a new method rather than silently changing this one.
#'
#' `variant` brackets the one judgement tier 1a makes. Table 5 prints several
#' forms of the same commodity and CBS cannot say which was eaten: wheat whole
#' 0.86, cereal 0.77, flour white 0.96; maize 0.85, corn whole 0.87, corn cereal
#' 0.70; rice polished 0.88, cereal 0.75. **The processing direction is not
#' uniform** — refining raises wheat by removing bran and lowers maize, rice and
#' oats through extrusion and Maillard damage — so there is no single axis to
#' sweep and the bracket is carried per item. `"default"` takes the
#' least-processed form, which is the consistent partner for WHEP's own
#' whole-commodity agronomic nitrogen; `"low"` and `"high"` give the span.
#'
#' @param data Named list of injected inputs. `protein_supply` (`year`,
#'   `area_code`, `item_cbs_code`, `protein_t`) is required;
#'   `protein_digestibility`, `protein_digestibility_items` and
#'   `protein_digestibility_trs935` override the packaged tables.
#' @param method `"trs935_item"` (default), `"digestibility_share"` or
#'   `"none"`.
#' @param variant Which Table 5 row each item takes, for `"trs935_item"`:
#'   `"default"` (the least-processed form the report names for the commodity),
#'   or `"low"` / `"high"`, the plausible bracket. Ignored by the other methods.
#' @return A tibble keyed by `year`, `area_code` with `quality`,
#'   `animal_protein_share`, `protein_classified_share`,
#'   `protein_measured_share` (the share carrying a measured Table 5 value
#'   rather than the class rate) and `method_quality`, plus the polity columns
#'   below.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_protein_quality(
#'   data = list(
#'     protein_supply = tibble::tribble(
#'       ~year, ~area_code, ~item_cbs_code, ~protein_t,
#'       2010L, 10L,        2731L,          40,
#'       2010L, 10L,        2511L,          60
#'     )
#'   )
#' )
build_protein_quality <- function(
  data = list(),
  method = c("trs935_item", "digestibility_share", "none"),
  variant = c("default", "low", "high")
) {
  method <- rlang::arg_match(method)
  variant <- rlang::arg_match(variant)
  supply <- data$protein_supply
  .check_columns(
    supply,
    c("year", "area_code", "item_cbs_code", "protein_t"),
    "data$protein_supply"
  )
  classes <- data$protein_digestibility %||%
    whep::whep_coef_table("protein_digestibility")
  .check_columns(
    classes,
    c("item_cbs_code", "protein_class"),
    "data$protein_digestibility"
  )

  items <- data$protein_digestibility_items %||%
    whep::whep_coef_table("protein_digestibility_items")
  measured <- data$protein_digestibility_trs935 %||%
    whep::whep_coef_table("protein_digestibility_trs935")

  supply |>
    .pq_diet_digestibility(
      classes,
      method,
      .pq_item_rates(method, variant, items, measured)
    ) |>
    dplyr::mutate(
      method_quality = if (method == "trs935_item") {
        paste("trs935_item", variant, sep = "_")
      } else {
        method
      }
    ) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers -------------------------------------------------------

# TRS 935 Table 43 footnote b: diet digestibility as "the weighted mean of 95%
# and 80% for animal and plant protein sources respectively". Two numbers, one
# footnote, cited here rather than in a coefficient table because that is where
# they are read.
.pq_animal_digestibility <- function() 0.95

.pq_plant_digestibility <- function() 0.80

# Per-item true digestibility from TRS 935 Table 5, for the items the report
# actually measures. Only the least-processed form is the default, and the
# choice is per item rather than uniform because the processing direction is
# not: refining RAISES wheat (whole 0.86 -> flour white 0.96, bran removed) and
# LOWERS maize, rice and oats (0.85 -> 0.70, 0.88 -> 0.75, 0.86 -> 0.72,
# extrusion and Maillard damage). There is no single "processed" axis to sweep,
# which is why the bracket is carried per item as `source_low` / `source_high`
# rather than derived.
#
# The default pairs with WHEP's own nitrogen basis: `biomass_coefs` carries an
# agronomic whole-commodity nitrogen, so the whole-grain digestibility is the
# consistent partner. Using the refined rows instead would raise quality and so
# LOWER the band.
.pq_item_rates <- function(method, variant, items, measured) {
  if (method != "trs935_item") {
    return(tibble::tibble(
      item_cbs_code = integer(0),
      item_rate = numeric(0)
    ))
  }
  column <- paste0("source_", variant)
  .check_columns(
    items,
    c("item_cbs_code", column),
    "protein_digestibility_items"
  )
  .check_columns(
    measured,
    c("source_name", "true_digestibility"),
    "protein_digestibility_trs935"
  )
  items |>
    dplyr::transmute(
      item_cbs_code = .data$item_cbs_code,
      source_name = .data[[column]]
    ) |>
    dplyr::inner_join(
      dplyr::select(measured, "source_name", "true_digestibility"),
      by = "source_name"
    ) |>
    dplyr::transmute(
      item_cbs_code = .data$item_cbs_code,
      item_rate = .data$true_digestibility
    )
}

# Digestibility IS additive over protein -- TRS 935 Table 6 computes it as "sum
# of digestible protein/total protein", and standardized ileal AA digestibility
# is additive in mixed diets because it is independent of basal endogenous
# losses (Fanelli et al. 2021). Apparent digestibility is NOT additive; this
# uses true faecal values, which are.
#
# Items outside FAO's own animal/vegetal grouping carry no class and leave the
# weighting rather than defaulting to either rate. Their share is reported so
# the omission stays visible.
.pq_diet_digestibility <- function(supply, classes, method, item_rates) {
  rates <- tibble::tibble(
    protein_class = c("animal", "plant"),
    rate = c(.pq_animal_digestibility(), .pq_plant_digestibility())
  )
  keyed <- supply |>
    dplyr::left_join(
      dplyr::select(classes, "item_cbs_code", "protein_class"),
      by = "item_cbs_code"
    ) |>
    dplyr::left_join(rates, by = "protein_class") |>
    # A measured per-item value wins over the class rate where TRS 935 Table 5
    # has one; where it does not, the class rate carries the item rather than
    # dropping it. The measured share is reported either way.
    dplyr::left_join(item_rates, by = "item_cbs_code") |>
    dplyr::mutate(
      measured = !is.na(.data$item_rate),
      rate = dplyr::coalesce(.data$item_rate, .data$rate)
    )
  out <- keyed |>
    dplyr::summarise(
      total_t = sum(.data$protein_t, na.rm = TRUE),
      classified_t = sum(
        .data$protein_t[!is.na(.data$rate)],
        na.rm = TRUE
      ),
      animal_t = sum(
        .data$protein_t[.data$protein_class %in% "animal"],
        na.rm = TRUE
      ),
      digestible_t = sum(
        (.data$protein_t * .data$rate)[!is.na(.data$rate)],
        na.rm = TRUE
      ),
      measured_t = sum(.data$protein_t[.data$measured], na.rm = TRUE),
      .by = c("year", "area_code")
    ) |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$area_code,
      animal_protein_share = dplyr::if_else(
        .data$classified_t > 0,
        .data$animal_t / .data$classified_t,
        NA_real_
      ),
      protein_classified_share = dplyr::if_else(
        .data$total_t > 0,
        .data$classified_t / .data$total_t,
        NA_real_
      ),
      protein_measured_share = dplyr::if_else(
        .data$total_t > 0,
        .data$measured_t / .data$total_t,
        NA_real_
      ),
      quality = dplyr::if_else(
        .data$classified_t > 0,
        .data$digestible_t / .data$classified_t,
        NA_real_
      )
    )
  if (method == "none") {
    out <- dplyr::mutate(out, quality = 1)
  }
  out
}
