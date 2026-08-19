# Aggregate amino acid scoring for the SJOS-N nourishment band -- tier 2 of the
# protein-quality ladder.
#
# THE AGGREGATION IS FAO'S, AND IT IS NOT THE OBVIOUS ONE. Averaging per-item
# PDCAAS values is forbidden in words, twice: TRS 935 p.99 "amino acid score is
# calculated for the diet from the overall amino acid profile of the dietary
# amino acid mixture without identifying the score of component proteins ...
# the amino acid score for food mixtures should be calculated from the weighted
# average digestible amino acid content", and FNP 92 p.17 repeats it. FNP 51
# p.37 gives the reason: "the score of a mixture cannot always be calculated
# with certainty from a knowledge of the individual scores of the components.
# Because of the complementary potential between proteins".
#
# The asymmetry that makes this work, and that makes the shortcut wrong:
#
#   - Digestibility IS a protein-weighted mean (TRS 935 Table 6 computes it as
#     "sum of digestible protein/total protein").
#   - The per-amino-acid ratios A_i/ref_i ARE additive over digestible protein.
#   - The min() is NOT. Because min is concave, a weighted mean of per-item
#     scores is a rigorous LOWER bound on diet quality, so it is a rigorous
#     UPPER bound on the floor -- it would understate deficiency.
#
# The weights are DIGESTIBLE protein, not crude: "on the principle that
# digestibility is first limiting, the composition and amino acid score of the
# absorbed available amino acids will reflect the relative digestibility of the
# individual food protein constituents" (TRS 935 p.99). The FNP 92 form weights
# by crude protein and divides by total rather than digestible protein; the
# intermediate profiles differ (44.34 against 44.14 mg/g on Table 6) and the
# final score does not. This implements the TRS 935 form.
#
# TRUNCATION IS MANDATORY FOR A MIXED DIET and the two standards truncate
# DIFFERENT quantities:
#
#   TRS 935  truncate the amino acid SCORE at 1, then multiply by digestibility
#            -> ceiling is the diet's digestibility, 0.80-0.96
#   FNP 92   truncate the DIAAS itself at 100%
#            -> ceiling is 1.0
#
# For a diet at AAS 1.4 and D 0.85 the first gives 0.85 and the second 1.00, an
# 18% difference in the floor, and it bites on exactly the animal-rich diets
# that truncate. WHEP uses the TRS 935 convention because that is the anchor:
# the 0.83 g/kg safe level is issued "for proteins with a protein
# digestibility-corrected amino acid score value of 1.0" (s14.2), and a PDCAAS
# of 1.0 requires D = 1.0, so under the report's own construction every real
# diet pays at least 1/D. Never mix the two ceilings within one series.

#' Score a diet's protein against the age-weighted requirement pattern.
#'
#' @description
#' Computes the aggregate protein digestibility-corrected amino acid score
#' (PDCAAS) of a diet, per country and year, following the worked example in
#' WHO/FAO/UNU TRS 935 Table 6 (printed p.100). This is **tier 2** of the
#' protein-quality ladder and the standard the ladder targets;
#' [build_protein_quality()] is the tier 1b lower bound that ships when no amino
#' acid composition is available.
#'
#' Given per-item protein supply `P_j`, digestibility `d_j` and amino acid
#' content `aa_ij` in mg per g protein:
#'
#' \deqn{P^d_j = P_j d_j \qquad D = \sum_j P^d_j / \sum_j P_j}
#' \deqn{A_i = \sum_j P^d_j \, aa_{ij} / \sum_j P^d_j \qquad
#'   \mathrm{AAS} = \min_i (A_i / \mathrm{ref}_i)}
#' \deqn{q = \min(1, \mathrm{AAS}) \cdot D}
#'
#' The amino acid profile is weighted by **digestible** protein, not crude, and
#' the score is truncated **before** multiplying by digestibility, which is the
#' TRS 935 convention rather than FNP 92's. Both choices are load-bearing and
#' are explained at the top of the source file.
#'
#' **Averaging per-item scores is not an approximation of this, it is a
#' different and biased quantity.** FAO forbids it twice in words, and because
#' `min()` is concave the average of item scores is a rigorous lower bound on
#' diet quality and so a rigorous upper bound on the floor.
#'
#' Four amino acids are enough for most diets — TRS 935 p.99: "in calculating
#' scores it is usually only necessary to use a pattern based on these four
#' amino acids" (lysine, sulfur amino acids, threonine, tryptophan) — and are
#' what both FAO worked examples use. Supplying more is supported: every amino
#' acid present in both `amino_acids` and `pattern` is scored.
#'
#' @param data Named list of injected inputs. `protein_supply` (`year`,
#'   `area_code`, `item_cbs_code`, `protein_t`), `amino_acids`
#'   (`item_cbs_code`, `amino_acid`, `mg_per_g_protein`) and `digestibility`
#'   (`item_cbs_code`, `digestibility`) are required; `pattern` (`amino_acid`,
#'   `mg_per_g_protein`) overrides the packaged adult reference pattern.
#' @return A tibble keyed by `year`, `area_code` with `quality`,
#'   `amino_acid_score`, `digestibility`, `limiting_amino_acid`,
#'   `protein_scored_share` and `method_quality`, plus the polity columns below.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' # TRS 935 Table 6: wheat, chickpea and milk powder against the adult pattern.
#' build_protein_score(
#'   data = list(
#'     protein_supply = tibble::tribble(
#'       ~year, ~area_code, ~item_cbs_code, ~protein_t,
#'       2010L, 10L,        1L,             52.0,
#'       2010L, 10L,        2L,             22.0,
#'       2010L, 10L,        3L,             11.9
#'     ),
#'     digestibility = tibble::tribble(
#'       ~item_cbs_code, ~digestibility,
#'       1L,             0.85,
#'       2L,             0.80,
#'       3L,             0.95
#'     ),
#'     amino_acids = tibble::tribble(
#'       ~item_cbs_code, ~amino_acid, ~mg_per_g_protein,
#'       1L,             "lysine",    25,
#'       2L,             "lysine",    70,
#'       3L,             "lysine",    80
#'     ),
#'     pattern = tibble::tribble(
#'       ~amino_acid, ~mg_per_g_protein,
#'       "lysine",    45
#'     )
#'   )
#' )
build_protein_score <- function(data = list()) {
  supply <- data$protein_supply
  .check_columns(
    supply,
    c("year", "area_code", "item_cbs_code", "protein_t"),
    "data$protein_supply"
  )
  digestibility <- data$digestibility
  .check_columns(
    digestibility,
    c("item_cbs_code", "digestibility"),
    "data$digestibility"
  )
  amino_acids <- data$amino_acids
  .check_columns(
    amino_acids,
    c("item_cbs_code", "amino_acid", "mg_per_g_protein"),
    "data$amino_acids"
  )
  pattern <- .ps_pattern_long(data$pattern)

  digestible <- .ps_digestible(supply, digestibility)
  digestible |>
    .ps_profile(amino_acids) |>
    .ps_score(pattern) |>
    dplyr::inner_join(
      .ps_digestibility(digestible),
      by = c("year", "area_code")
    ) |>
    dplyr::mutate(
      quality = pmin(1, .data$amino_acid_score) * .data$digestibility,
      method_quality = "amino_acid_score"
    ) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers -------------------------------------------------------

# The reference pattern in either shape. WHEP carries it wide -- one column per
# amino acid -- because that is how TRS 935 Table 50 prints it and how
# build_protein_requirement() emits the age-weighted version. Scoring wants it
# long. Accepting both means a build_protein_requirement() output passes
# straight through without the caller reshaping.
#
# The default is the ADULT row, and using it on a young population understates
# the correction: the infant pattern demands 57 mg lysine per g protein against
# the adult's 45, so the same diet scores 0.67 against 0.84 on TRS 935's own
# example. Pass the age-weighted pattern for a population.
.ps_pattern_long <- function(pattern) {
  if (is.null(pattern)) {
    pattern <- whep::whep_coef_table("protein_scoring_pattern") |>
      dplyr::filter(.data$age_class == "19+")
  }
  if (all(rlang::has_name(pattern, c("amino_acid", "mg_per_g_protein")))) {
    return(dplyr::select(pattern, "amino_acid", "mg_per_g_protein"))
  }
  wide <- c(
    lysine = "lysine_mg_g",
    saa = "saa_mg_g",
    threonine = "threonine_mg_g",
    tryptophan = "tryptophan_mg_g"
  )
  if (!all(rlang::has_name(pattern, unname(wide)))) {
    cli::cli_abort(c(
      "{.field data$pattern} must carry the reference pattern.",
      i = "Either {.field amino_acid}/{.field mg_per_g_protein}, or the wide
           columns {.field {unname(wide)}}."
    ))
  }
  if (nrow(pattern) != 1L) {
    cli::cli_abort(
      "A wide {.field data$pattern} must be one row, not {nrow(pattern)}."
    )
  }
  tibble::tibble(
    amino_acid = names(wide),
    mg_per_g_protein = as.numeric(pattern[1L, unname(wide)])
  )
}

# Digestible protein per item, equation (1). Items with no digestibility are
# excluded from BOTH the profile and the digestibility, rather than defaulting
# to either, and their share is reported.
.ps_digestible <- function(supply, digestibility) {
  supply |>
    dplyr::left_join(
      dplyr::select(digestibility, "item_cbs_code", "digestibility"),
      by = "item_cbs_code"
    ) |>
    dplyr::mutate(
      digestible_t = .data$protein_t * .data$digestibility
    )
}

# Equation (2): diet digestibility, the sum of digestible protein over the sum
# of total protein. A genuine protein-weighted mean, which is why it is computed
# as one.
.ps_digestibility <- function(digestible) {
  digestible |>
    dplyr::summarise(
      total_t = sum(.data$protein_t, na.rm = TRUE),
      scored_t = sum(
        .data$protein_t[!is.na(.data$digestibility)],
        na.rm = TRUE
      ),
      digestible_sum = sum(.data$digestible_t, na.rm = TRUE),
      .by = c("year", "area_code")
    ) |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$area_code,
      digestibility = dplyr::if_else(
        .data$scored_t > 0,
        .data$digestible_sum / .data$scored_t,
        NA_real_
      ),
      protein_scored_share = dplyr::if_else(
        .data$total_t > 0,
        .data$scored_t / .data$total_t,
        NA_real_
      )
    )
}

# Equation (3): the aggregate DIGESTIBLE amino acid profile, mg per g of
# digestible protein. Weighting by digestible rather than crude protein is the
# TRS 935 form, and is the correction the 1991 report's Table 10 got wrong.
.ps_profile <- function(digestible, amino_acids) {
  digestible |>
    dplyr::inner_join(
      dplyr::select(
        amino_acids,
        "item_cbs_code",
        "amino_acid",
        "mg_per_g_protein"
      ),
      by = "item_cbs_code",
      relationship = "many-to-many"
    ) |>
    # Distinct output names: reusing `digestible_t` here would redefine it
    # before the next expression reads it, and the amino acid total would be
    # computed from the country-year scalar instead of the item column. That
    # silently truncates every score to 1 and hands back the digestibility.
    dplyr::summarise(
      digestible_sum = sum(.data$digestible_t, na.rm = TRUE),
      amino_acid_mg = sum(
        .data$digestible_t * .data$mg_per_g_protein,
        na.rm = TRUE
      ),
      .by = c("year", "area_code", "amino_acid")
    ) |>
    dplyr::mutate(
      profile_mg_per_g = .data$amino_acid_mg / .data$digestible_sum
    )
}

# Equations (4) and (5): the score is the MINIMUM ratio across amino acids, and
# the limiting acid is named because which one binds is the diagnostic a reader
# wants. Truncation happens in the caller, after this and before digestibility.
.ps_score <- function(profile, pattern) {
  profile |>
    dplyr::inner_join(
      dplyr::select(pattern, "amino_acid", ref_mg_per_g = "mg_per_g_protein"),
      by = "amino_acid"
    ) |>
    dplyr::mutate(ratio = .data$profile_mg_per_g / .data$ref_mg_per_g) |>
    dplyr::slice_min(
      .data$ratio,
      n = 1L,
      by = c("year", "area_code"),
      with_ties = FALSE
    ) |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$area_code,
      amino_acid_score = .data$ratio,
      limiting_amino_acid = .data$amino_acid
    )
}
