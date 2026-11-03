# Residue and root of each rainfed/irrigated part of a cell-crop, re-evaluated
# from the package's own residue and root model (issue #1233). Shared by the
# carbon chain (`.ci_part_factors()`) and the nitrogen chain
# (`.nb_item_part_factors()`).
#
# A crop's residue and root dry matter come from calculate_crop_residues() and
# calculate_crop_roots() at its mean yield. Its rainfed and irrigated parts
# yield differently (the bounded ratio of split_regime_yield()), so each
# part's residue and root are those two functions evaluated again, on one
# hectare, at the part's own fresh yield and in the part's water regime. The
# regime switches the model's irrigation terms on for the irrigated part: the
# residue terms times `residue_ratio_factor` scaled by the crop group's
# `irr_residue_sensitivity`, and the root:shoot ratio times `rs_ratio_factor`
# for every crop but paddy rice (`irrigation_adj`, `irr_residue_crop_adj`;
# whep#1536). Each part then carries its
# crop's residue (root) per hectare times
#   model(part yield, part regime) / model(mean yield, no regime),
# the denominator being the evaluation the crop's own residue and root come
# from. Where the crop's per-hectare residue and root are the model's at its
# mean yield -- both default chains, which compute them from the national
# production and harvested area -- the part's are the model's at its own
# yield; a caller-supplied density is scaled by the same model response.
#
# The residue is affine in yield (the IPCC slope-intercept term, for a
# non-cereal averaged with the residue:product ratio term) and so is the
# root (root:shoot times the above-ground biomass, averaged with a reference
# below-ground biomass per hectare). With the irrigation terms neutral and
# the root cap not binding, the parts therefore sum to the crop: A_r f(Y_r) + A_i f(Y_i) = A f(Y)
# because A_r Y_r + A_i Y_i = A Y. The positive intercepts give an
# above-mean hectare less than its yield share. With the irrigation terms on,
# residue and root fall on irrigated land, so a crop's total is not kept.
#
# Carbon and nitrogen are fixed fractions of residue and root dry matter
# (calculate_npp_carbon_nitrogen(), `bio_coefs`), and the residue destinies
# are yield-independent fractions of the residue (calculate_residue_
# destinies()), so the soil-returned, fed, bedding and burnt residue and the
# root, in carbon and in nitrogen, all scale by the part's two factors.

# The residue and root factors of each part. `parts` has `item_prod_code`,
# `area_code`, `year`, `yield_mean` and `yield_part` (fresh t/ha, in the units
# of the production the crop's residue was computed from) and `water_regime`
# (`"rainfed"` or `"irrigated"`). Returns `residue_factor` and `root_factor`,
# one row per row of `parts`, in its order. A factor is 1 where the model
# gives the crop no residue (root) at its mean yield: there is nothing to
# scale, and the crop's own density is then zero on the default chains.
.rr_part_factors <- function(parts) {
  .rr_check_model_labels()
  base <- .rr_model_rows(parts)
  mean <- .rr_model_at(base, "yield_mean", with_regime = FALSE)
  part <- .rr_model_at(base, "yield_part", with_regime = TRUE)
  tibble::tibble(
    residue_factor = .rr_ratio(part$residue_dm_t, mean$residue_dm_t),
    root_factor = .rr_ratio(part$root_dm_t, mean$root_dm_t)
  )
}

# The model's covariates for each part: the item as the coefficient tables
# key it, the year and the polity's HANPP region for the modern-variety term
# (the crop-NPP chain attaches the same region, .sci_crop_regions()), and
# the part's regime in the model's vocabulary.
.rr_model_rows <- function(parts) {
  # The model books a missing production as zero, which would hand a part
  # the residue intercept of a zero yield: a part reaches here only with
  # both yields known.
  if (anyNA(parts$yield_mean) || anyNA(parts$yield_part)) {
    cli::cli_abort(
      "A regime part reached the residue model without a yield.",
      class = "whep_regime_part_yield"
    )
  }
  regions <- dplyr::select(.sci_crop_regions(), "area_code", "region_hanpp")
  parts |>
    dplyr::transmute(
      item_prod_code = as.character(.data$item_prod_code),
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year),
      yield_mean = as.numeric(.data$yield_mean),
      yield_part = as.numeric(.data$yield_part),
      water_regime = .rr_model_regime(.data$water_regime)
    ) |>
    dplyr::left_join(regions, by = "area_code", relationship = "many-to-one")
}

# Residue and root dry matter on one hectare at the yield in `yield_col`,
# with the part's regime or with none (the crop's own evaluation). Each
# distinct row is evaluated once and joined back, through the soil chain's
# own `.sci_crop_npp()` (cereals on the CBS's residue method, rice on a paddy
# basis; whep#1519), so the denominator is the evaluation the crop's own
# residue and root come from.
.rr_model_at <- function(base, yield_col, with_regime) {
  covariates <- c("item_prod_code", "area_code", "year", "region_hanpp")
  if (with_regime) {
    covariates <- c(covariates, "water_regime")
  }
  rows <- base |>
    dplyr::select(
      dplyr::all_of(covariates),
      yield_t_ha = dplyr::all_of(yield_col)
    )
  evaluated <- rows |>
    dplyr::distinct() |>
    dplyr::mutate(production_t = .data$yield_t_ha, area_ha = 1) |>
    .sci_crop_npp() |>
    dplyr::select(
      dplyr::all_of(c(covariates, "yield_t_ha")),
      "residue_dm_t",
      "root_dm_t"
    )
  dplyr::left_join(
    rows,
    evaluated,
    by = c(covariates, "yield_t_ha"),
    relationship = "many-to-one"
  )
}

.rr_ratio <- function(part, mean) {
  dplyr::if_else(mean > 0 & !is.na(mean), part / mean, 1)
}

# The split labels its parts "rainfed" and "irrigated"; the model's
# irrigation table keys `water_regime` on "Rainfed" and "Irrigated". Mapped
# explicitly, so any other label aborts rather than joining nothing.
.rr_model_regime <- function(water_regime) {
  labels <- c(rainfed = "Rainfed", irrigated = "Irrigated")
  out <- unname(labels[water_regime])
  unknown <- unique(water_regime[is.na(out)])
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "Unknown regime part label{?s} {.val {unknown}}.",
        i = "A part is {.val rainfed} or {.val irrigated}."
      ),
      class = "whep_regime_label"
    )
  }
  out
}

# The irrigation table must carry both labels the parts map to. A label it
# does not carry joins nothing, calculate_crop_residues() and
# calculate_crop_roots() then take a factor of 1, and the irrigated part
# silently loses its irrigation terms.
.rr_check_model_labels <- function() {
  check_labels_supplied(
    whep::whep_coef_table("irrigation_adj"),
    "water_regime",
    c("Rainfed", "Irrigated"),
    details = c(
      i = "The regime split re-evaluates each part's residue and root with
           its water regime; {.code whep_coef_table(\"irrigation_adj\")} is
           where the irrigated part's residue and root:shoot factors come
           from."
    )
  )
}

# Each part's yields: split_regime_yield()'s production-keeping split,
# Y_r = Y / (1 + s (R - 1)) and Y_i = R Y_r, written on the crop's own mean
# yield Y, irrigated share s and bounded ratio R so that R = 1 returns the
# mean yield exactly.
.rr_part_yields <- function(yield_mean, irrigated_share, ratio) {
  rainfed <- yield_mean / (1 + irrigated_share * (ratio - 1))
  list(rainfed = rainfed, irrigated = ratio * rainfed)
}

# How a part's residue and root were set, in the vocabulary of
# `method_residue_regime`: re-evaluated at the part's yield, with the
# irrigation terms on an irrigated part, or the crop's own density.
.rr_residue_stamp <- function(reevaluated, irrigated) {
  dplyr::case_when(
    !reevaluated ~ "crop_density",
    irrigated ~ "part_yield_irrigation_terms",
    .default = "part_yield"
  )
}

# The split rules under which a part's yield is known: the ratio split, and
# a crop wholly in one regime (whose part is at the mean yield).
.rr_reevaluated_rules <- function() {
  c("yield_ratio", "trivial_rainfed_only", "trivial_irrigated_only")
}
