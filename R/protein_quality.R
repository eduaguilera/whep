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
#   1b  digestibility_share  THIS. Diet digestibility from the animal/plant
#                            protein split. Needs no data WHEP does not have.
#   1a  per-item digestibility   ~88 coefficients; TRS 935 Table 5 has 35 rows
#                                and none for fruit, vegetables, roots, tubers,
#                                sugar or seafood
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
#' `method = "digestibility_share"` (default) takes the diet's digestibility as
#' the protein-weighted mean of **0.95 for animal protein and 0.80 for plant
#' protein**, which is how TRS 935 Table 43 footnote b computes it. The
#' animal/plant split follows FAO's own Food Balance Sheet grouping — Animal
#' Products (item 2941) against Vegetal Products (2903) — so it reconciles
#' against FAOSTAT's published aggregates rather than being WHEP's opinion.
#'
#' This is **tier 1b of four**, and it is a *provable lower bound* on the full
#' correction: PDCAAS is `min(1, AAS) x D`, which never exceeds `D`. It is
#' therefore conservative about the **size of the correction**, not about
#' nourishment adequacy — it under-corrects, and so classifies fewer countries
#' as deficient than the full amino acid score would. The full score needs a
#' per-item amino acid composition table WHEP does not have; when it arrives it
#' becomes a new method rather than silently changing this one.
#'
#' `"none"` returns a quality of 1 and leaves the band on crude protein, which
#' is the behaviour before this function existed.
#'
#' On the 2010 world basket the animal protein share is 40.3%, giving a
#' digestibility of 0.860 and raising the band by 16.2%. Across countries the
#' correction runs roughly +11% to +23%, largest where the diet is most
#' plant-based.
#'
#' @param data Named list of injected inputs. `protein_supply` (`year`,
#'   `area_code`, `item_cbs_code`, `protein_t`) is required;
#'   `protein_digestibility` overrides the packaged classification.
#' @param method `"digestibility_share"` (default) or `"none"`.
#' @return A tibble keyed by `year`, `area_code` with `quality`,
#'   `animal_protein_share`, `protein_classified_share` and `method_quality`,
#'   plus the polity columns below.
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
  method = c("digestibility_share", "none")
) {
  method <- rlang::arg_match(method)
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

  supply |>
    .pq_diet_digestibility(classes, method) |>
    dplyr::mutate(method_quality = method) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers -------------------------------------------------------

# TRS 935 Table 43 footnote b: diet digestibility as "the weighted mean of 95%
# and 80% for animal and plant protein sources respectively". Two numbers, one
# footnote, cited here rather than in a coefficient table because that is where
# they are read.
.pq_animal_digestibility <- function() 0.95

.pq_plant_digestibility <- function() 0.80

# Digestibility IS additive over protein -- TRS 935 Table 6 computes it as "sum
# of digestible protein/total protein", and standardized ileal AA digestibility
# is additive in mixed diets because it is independent of basal endogenous
# losses (Fanelli et al. 2021). Apparent digestibility is NOT additive; this
# uses true faecal values, which are.
#
# Items outside FAO's own animal/vegetal grouping carry no class and leave the
# weighting rather than defaulting to either rate. Their share is reported so
# the omission stays visible.
.pq_diet_digestibility <- function(supply, classes, method) {
  rates <- tibble::tibble(
    protein_class = c("animal", "plant"),
    rate = c(.pq_animal_digestibility(), .pq_plant_digestibility())
  )
  keyed <- supply |>
    dplyr::left_join(
      dplyr::select(classes, "item_cbs_code", "protein_class"),
      by = "item_cbs_code"
    ) |>
    dplyr::left_join(rates, by = "protein_class")
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
