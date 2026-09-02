#' Decompose cropland N surplus into size, intensity, and inefficiency
#' drivers
#'
#' @description
#' Decomposes the year-on-year change in Spain's cropland nitrogen (N)
#' surplus into three multiplicative drivers, following an additive LMDI
#' (logarithmic mean Divisia index) shift-share decomposition computed at
#' the national level:
#' - **Size**: national cropland area.
#' - **Intensity**: N input per hectare of cropland.
#' - **Inefficiency**: surplus fraction of inputs (1 - nitrogen use
#'   efficiency).
#'
#' Contributions are additive and residual-free: they sum exactly to the
#' observed change in cropland N surplus for every year-on-year transition.
#'
#' This is a simplified, national-only view (no provincial or destiny
#' breakdown); [decompose_specialization_cov()] still uses the
#' full province x destiny detail for its `cropland_province` and
#' `cropland_destiny` series.
#'
#' @param n_prov_destiny Nitrogen flows tibble from
#'   [create_n_prov_destiny()]. If `NULL`, loaded automatically.
#' @param npp_ygpit Land use and area tibble from
#'   `whep_read_file("npp_ygpit")`. If `NULL`, loaded automatically.
#' @param codes_coefs Item and biomass coefficients tibble from
#'   `whep_read_file("codes_coefs")`. If `NULL`, loaded automatically.
#' @param by_period If `TRUE`, compares each reference period (each
#'   averaged across its ten years) against the immediately preceding
#'   one — 1860-1870 -> 1920-1930 -> 1960-1970 -> 2010-2020 — plus one
#'   extra transition spanning the full analysis window, 1860-1870
#'   straight to 2010-2020 (the total change) — instead of chaining year
#'   on year.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble from [calculate_lmdi()] with columns `period`,
#'   `period_years`, `factor_label`, `component_type`, `additive`,
#'   `multiplicative`, and `multiplicative_log`.
#' @export
#'
#' @examples
#' decompose_cropland_surplus(example = TRUE)
decompose_cropland_surplus <- function(
  n_prov_destiny = NULL,
  npp_ygpit = NULL,
  codes_coefs = NULL,
  by_period = FALSE,
  example = FALSE
) {
  if (example) {
    return(.example_decomp_cropland())
  }
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }
  if (is.null(npp_ygpit)) {
    npp_ygpit <- whep_read_file("npp_ygpit")
  }
  if (is.null(codes_coefs)) {
    codes_coefs <- whep_read_file("codes_coefs")
  }

  panel <- .build_cropland_destiny_panel(
    n_prov_destiny,
    npp_ygpit,
    codes_coefs
  ) |>
    .national_area_panel()
  .warn_if_sign_change(panel, surplus, character(0), "Cropland surplus")
  if (by_period) {
    panel <- .period_average_panel(panel, character(0))
  }

  identity <- .simple_area_identity("Cropland N surplus")
  calculate_lmdi(
    panel,
    identity = identity$formula,
    identity_labels = identity$labels,
    time_var = year,
    periods = if (by_period) .reference_period_pairs() else NULL,
    verbose = FALSE
  )
}


#' Decompose semi-natural agroecosystem N surplus into size, intensity,
#' and inefficiency drivers
#'
#' @description
#' Decomposes the year-on-year change in Spain's semi-natural agroecosystem
#' (grazing land, dehesa, and non-cropland vegetation) nitrogen (N) surplus
#' into three multiplicative drivers, following an additive LMDI
#' shift-share decomposition computed at the national level:
#' - **Size**: national semi-natural area.
#' - **Intensity**: N input per hectare of semi-natural land.
#' - **Inefficiency**: surplus fraction of inputs (1 - nitrogen use
#'   efficiency).
#'
#' No destiny factor is used because grazed and cut vegetation is assumed
#' to be overwhelmingly a single destiny (livestock feed).
#'
#' The land-use categories included (`Dehesa`, `Forest_high`, `Forest_low`,
#' `Other`, `Pasture_Shrubland`) are all of `npp_ygpit`'s non-cropland
#' categories, matching the existing `semi_natural_agroecosystems` box used
#' elsewhere in the package. Some of that land (e.g. `Forest_high`/
#' `Forest_low`) may not actually be grazed and can produce non-feed output
#' (firewood), which would call for its own destiny factor (as in
#' [decompose_cropland_surplus()]) rather than the single-destiny
#' assumption above; that refinement is not implemented here.
#'
#' This is a simplified, national-only view (no provincial breakdown).
#' Semi-natural surplus can turn negative (soil N mining) in some years.
#' LMDI relies on logarithms and cannot handle a series that changes sign
#' between two compared years; this function warns when that occurs
#' instead of silently returning `NA`, but does not implement the
#' Shapley/Sun alternative required for those cases.
#'
#' @param n_prov_destiny Nitrogen flows tibble from
#'   [create_n_prov_destiny()]. If `NULL`, loaded automatically.
#' @param npp_ygpit Land use and area tibble from
#'   `whep_read_file("npp_ygpit")`. If `NULL`, loaded automatically.
#' @param by_period If `TRUE`, compares each reference period (each
#'   averaged across its ten years) against the immediately preceding
#'   one — 1860-1870 -> 1920-1930 -> 1960-1970 -> 2010-2020 — plus one
#'   extra transition spanning the full analysis window, 1860-1870
#'   straight to 2010-2020 (the total change) — instead of chaining year
#'   on year.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble from [calculate_lmdi()] with columns `period`,
#'   `period_years`, `factor_label`, `component_type`, `additive`,
#'   `multiplicative`, and `multiplicative_log`.
#' @export
#'
#' @examples
#' decompose_semi_natural_surplus(example = TRUE)
decompose_semi_natural_surplus <- function(
  n_prov_destiny = NULL,
  npp_ygpit = NULL,
  by_period = FALSE,
  example = FALSE
) {
  if (example) {
    return(.example_decomp_seminat())
  }
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }
  if (is.null(npp_ygpit)) {
    npp_ygpit <- whep_read_file("npp_ygpit")
  }

  panel <- .build_semi_natural_panel(n_prov_destiny, npp_ygpit) |>
    .national_area_panel()
  .warn_if_sign_change(panel, surplus, character(0), "Semi-natural surplus")
  if (by_period) {
    panel <- .period_average_panel(panel, character(0))
  }

  identity <- .simple_area_identity("Semi-natural N surplus")
  calculate_lmdi(
    panel,
    identity = identity$formula,
    identity_labels = identity$labels,
    time_var = year,
    periods = if (by_period) .reference_period_pairs() else NULL,
    verbose = FALSE
  )
}


#' Decompose livestock manure management losses into herd, feed,
#' excretion, and management-loss drivers
#'
#' @description
#' Decomposes the year-on-year change in nitrogen (N) lost from livestock
#' housing and manure storage (before any manure reaches land) into four
#' multiplicative drivers, computed at the national level: herd size,
#' feed N intake per livestock unit, excreted fraction of feed N
#' (1 - feed nitrogen use efficiency), and the management-loss fraction of
#' excreted N.
#'
#' This is a simplified, national-only view with no species breakdown
#' (no species-mix factor); [decompose_specialization_cov()] still
#' uses the full per-species detail for its `livestock_species` series.
#' Only livestock categories with a livestock-unit (LU) coefficient in
#' `livestock_units` are included in the underlying herd/feed/excretion
#' totals (currently Cattle_milk, Cattle_meat, Sheep, Goats, Horses,
#' Donkeys_mules, Pigs, Poultry, Rabbits); categories present in
#' `intake_ygiac`/`n_excretion_ygs` but absent from `livestock_units`
#' (e.g. "Fur animals", "Other", "Other_birds") are dropped.
#'
#' `n_prov_destiny` records manure already applied to land without
#' retaining which species it came from, so the management-loss fraction
#' has always been computed nationally, not per species.
#'
#' @param n_prov_destiny Nitrogen flows tibble from
#'   [create_n_prov_destiny()]. If `NULL`, loaded automatically.
#' @param intake_ygiac Feed intake tibble from
#'   `whep_read_file("intake_ygiac")`. If `NULL`, loaded automatically.
#' @param n_excretion_ygs Livestock excretion tibble from
#'   `whep_read_file("n_excretion_ygs")`. If `NULL`, loaded automatically.
#' @param stock_prod_ygps Livestock stock tibble from
#'   `whep_read_file("stock_prod_ygps")`. If `NULL`, loaded automatically.
#' @param livestock_units Livestock unit coefficients tibble from
#'   `whep_read_file("livestock_units")`. If `NULL`, loaded automatically.
#' @param by_period If `TRUE`, compares each reference period (each
#'   averaged across its ten years) against the immediately preceding
#'   one — 1860-1870 -> 1920-1930 -> 1960-1970 -> 2010-2020 — plus one
#'   extra transition spanning the full analysis window, 1860-1870
#'   straight to 2010-2020 (the total change) — instead of chaining year
#'   on year.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble from [calculate_lmdi()] with columns `period`,
#'   `period_years`, `factor_label`, `component_type`, `additive`,
#'   `multiplicative`, and `multiplicative_log`.
#' @export
#'
#' @examples
#' decompose_manure_losses(example = TRUE)
decompose_manure_losses <- function(
  n_prov_destiny = NULL,
  intake_ygiac = NULL,
  n_excretion_ygs = NULL,
  stock_prod_ygps = NULL,
  livestock_units = NULL,
  by_period = FALSE,
  example = FALSE
) {
  if (example) {
    return(.example_decomp_manure())
  }
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }
  raw <- .default_raw_inputs(
    list(
      intake_ygiac = intake_ygiac,
      n_excretion_ygs = n_excretion_ygs,
      stock_prod_ygps = stock_prod_ygps,
      livestock_units = livestock_units
    ),
    keys = c(
      "intake_ygiac",
      "n_excretion_ygs",
      "stock_prod_ygps",
      "livestock_units"
    )
  )

  panel <- .build_manure_panel(
    n_prov_destiny,
    raw$intake_ygiac,
    raw$n_excretion_ygs,
    raw$stock_prod_ygps,
    raw$livestock_units
  ) |>
    .national_manure_panel()
  .warn_if_sign_change(panel, loss, character(0), "Manure losses")
  if (by_period) {
    panel <- .period_average_panel(panel, character(0))
  }

  identity <- .manure_identity()
  calculate_lmdi(
    panel,
    identity = identity$formula,
    identity_labels = identity$labels,
    time_var = year,
    periods = if (by_period) .reference_period_pairs() else NULL,
    verbose = FALSE
  )
}


#' Decompose urban nitrogen losses into population, per-capita, and
#' recycling drivers
#'
#' @description
#' Decomposes the year-on-year change in non-recycled human excreta
#' nitrogen (N) into three multiplicative drivers, computed at the
#' national level: population, per-capita excreted N (approximated by
#' per-capita food N consumption, since intake is approximately equal to
#' excretion), and the non-recycled fraction of excreted N.
#'
#' Only the `population_food` destiny is used as the excretion proxy.
#' `population_other_uses` (non-food industrial use, e.g. cotton/tobacco)
#' is deliberately excluded even though `.create_wastewater_surplus_df()`
#' in `grafs_plot_df.R` includes it: that material is not ingested, so it
#' cannot be assumed to leave the body as excreta, breaking the
#' intake-approximates-excretion logic this compartment relies on.
#'
#' Food waste is not included: the commodity-balance sheets underlying
#' `n_prov_destiny` carry Food/Feed/Seed/Other-uses/Export/Import
#' destinies but no separate consumer food-waste line, so this loss is
#' excreta-only. Whether the `urban`/`People` recycling flows already
#' folded in some food waste from the original source is unconfirmed.
#'
#' @param n_prov_destiny Nitrogen flows tibble from
#'   [create_n_prov_destiny()]. If `NULL`, loaded automatically.
#' @param population_yg Population tibble from
#'   `whep_read_file("population_yg")`. If `NULL`, loaded automatically.
#' @param by_period If `TRUE`, compares each reference period (each
#'   averaged across its ten years) against the immediately preceding
#'   one — 1860-1870 -> 1920-1930 -> 1960-1970 -> 2010-2020 — plus one
#'   extra transition spanning the full analysis window, 1860-1870
#'   straight to 2010-2020 (the total change) — instead of chaining year
#'   on year.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble from [calculate_lmdi()] with columns `period`,
#'   `period_years`, `factor_label`, `component_type`, `additive`,
#'   `multiplicative`, and `multiplicative_log`.
#' @export
#'
#' @examples
#' decompose_urban_losses(example = TRUE)
decompose_urban_losses <- function(
  n_prov_destiny = NULL,
  population_yg = NULL,
  by_period = FALSE,
  example = FALSE
) {
  if (example) {
    return(.example_decomp_urban())
  }
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }
  if (is.null(population_yg)) {
    population_yg <- whep_read_file("population_yg") |>
      .forwardfill_population(max(
        as.numeric(n_prov_destiny$year),
        na.rm = TRUE
      ))
  }

  panel <- .build_urban_panel(n_prov_destiny, population_yg)
  .warn_if_sign_change(panel, loss, character(0), "Urban losses")
  if (by_period) {
    panel <- .period_average_panel(panel, character(0))
  }

  identity <- .urban_identity()
  calculate_lmdi(
    panel,
    identity = identity$formula,
    identity_labels = identity$labels,
    time_var = year,
    periods = if (by_period) .reference_period_pairs() else NULL,
    verbose = FALSE
  )
}


#' Decompose total territorial N losses into compartments and mechanisms
#'
#' @description
#' Runs the cropland ([decompose_cropland_surplus()]), semi-natural
#' ([decompose_semi_natural_surplus()]), manure ([decompose_manure_losses()]),
#' and urban ([decompose_urban_losses()]) decompositions, then combines
#' them into two cumulative, year-on-year contribution series:
#' - `by_compartment`: change in total territorial N losses attributed to
#'   each of the four compartments.
#' - `by_mechanism`: the same total change regrouped across compartments
#'   into scale, intensification, and efficiency (population and
#'   per-capita excretion are grouped under scale, since together they
#'   represent total human N throughput). Cropland, semi-natural, and
#'   manure no longer carry a spatial/destiny/species-mix factor (see
#'   their own simplified decompositions), so no factor currently maps to
#'   a "specialization" mechanism — that signal now lives only in
#'   [decompose_specialization_cov()] and
#'   [decompose_crop_livestock_conn()].
#'
#' @param n_prov_destiny Nitrogen flows tibble from
#'   [create_n_prov_destiny()], shared across all four compartments. If
#'   `NULL`, loaded automatically.
#' @param raw Named list overriding any of the raw inputs shared across
#'   compartments (`npp_ygpit`, `codes_coefs`, `intake_ygiac`,
#'   `n_excretion_ygs`, `stock_prod_ygps`, `livestock_units`,
#'   `population_yg`). Missing elements are loaded automatically.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A named list with tibbles `detail` (per-compartment LMDI
#'   output, with `compartment` and `mechanism` columns added),
#'   `by_compartment`, and `by_mechanism` (each with `t0`,
#'   `contribution_mgn`, and `cumulative_mgn`).
#' @export
#'
#' @examples
#' decompose_terr_losses(example = TRUE)
decompose_terr_losses <- function(
  n_prov_destiny = NULL,
  raw = NULL,
  example = FALSE
) {
  if (example) {
    return(.example_decomp_terr())
  }
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }
  if (is.null(raw)) {
    raw <- list()
  }
  raw <- .default_raw_inputs(raw, keys = names(.raw_input_loaders()))

  compartments <- list(
    cropland = decompose_cropland_surplus(
      n_prov_destiny,
      raw$npp_ygpit,
      raw$codes_coefs
    ),
    semi_natural = decompose_semi_natural_surplus(
      n_prov_destiny,
      raw$npp_ygpit
    ),
    manure = decompose_manure_losses(
      n_prov_destiny,
      raw$intake_ygiac,
      raw$n_excretion_ygs,
      raw$stock_prod_ygps,
      raw$livestock_units
    ),
    urban = decompose_urban_losses(n_prov_destiny, raw$population_yg)
  )

  detail <- purrr::imap(compartments, ~ dplyr::mutate(.x, compartment = .y)) |>
    purrr::list_rbind() |>
    .tag_mechanism() |>
    .add_period_start()

  list(
    detail = detail,
    by_compartment = .cumulate_series(
      detail,
      "compartment",
      target_only = TRUE
    ),
    by_mechanism = .cumulate_series(detail, "mechanism", target_only = FALSE)
  )
}


#' Decompose territorial N losses by reference period (chained)
#'
#' @description
#' Runs the same four compartments as [decompose_terr_losses()],
#' but comparing each reference period (each averaged across its ten
#' years) against the immediately preceding one — 1860-1870 ->
#' 1920-1930 -> 1960-1970 -> 2010-2020 — plus one extra transition
#' spanning the full analysis window, 1860-1870 straight to 2010-2020
#' (the total change), instead of chaining year on year. This is the
#' periodised table recommended alongside the main chained figure in the
#' decomposition proposal (section 12), summarizing the four historical
#' phases rather than following the full 160-year trajectory.
#'
#' @param n_prov_destiny Nitrogen flows tibble from
#'   [create_n_prov_destiny()], shared across all four compartments. If
#'   `NULL`, loaded automatically.
#' @param raw Named list overriding any of the raw inputs shared across
#'   compartments (`npp_ygpit`, `codes_coefs`, `intake_ygiac`,
#'   `n_excretion_ygs`, `stock_prod_ygps`, `livestock_units`,
#'   `population_yg`). Missing elements are loaded automatically.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A named list with tibbles `detail` (per-compartment LMDI
#'   output, with `compartment` and `mechanism` columns added),
#'   `by_compartment`, and `by_mechanism` (each with `period` — one of
#'   "1865-1925", "1925-1965", "1965-2015", "Total (1865-2015)", the mean
#'   year of each reference window (1865 = mean of 1860-1870, and so on)
#'   — plus `contribution_mgn`, `period_years`, and
#'   `contribution_per_yr_mgn`, the per-year-normalized value used for
#'   plotting).
#' @export
#'
#' @examples
#' decompose_terr_losses_periods(example = TRUE)
decompose_terr_losses_periods <- function(
  n_prov_destiny = NULL,
  raw = NULL,
  example = FALSE
) {
  if (example) {
    return(.example_decomp_terr_periods())
  }
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }
  if (is.null(raw)) {
    raw <- list()
  }
  raw <- .default_raw_inputs(raw, keys = names(.raw_input_loaders()))

  compartments <- list(
    cropland = decompose_cropland_surplus(
      n_prov_destiny,
      raw$npp_ygpit,
      raw$codes_coefs,
      by_period = TRUE
    ),
    semi_natural = decompose_semi_natural_surplus(
      n_prov_destiny,
      raw$npp_ygpit,
      by_period = TRUE
    ),
    manure = decompose_manure_losses(
      n_prov_destiny,
      raw$intake_ygiac,
      raw$n_excretion_ygs,
      raw$stock_prod_ygps,
      raw$livestock_units,
      by_period = TRUE
    ),
    urban = decompose_urban_losses(
      n_prov_destiny,
      raw$population_yg,
      by_period = TRUE
    )
  )

  detail <- purrr::imap(compartments, ~ dplyr::mutate(.x, compartment = .y)) |>
    purrr::list_rbind() |>
    .tag_mechanism()

  list(
    detail = detail,
    by_compartment = .aggregate_period_series(
      detail,
      "compartment",
      target_only = TRUE
    ),
    by_mechanism = .aggregate_period_series(
      detail,
      "mechanism",
      target_only = FALSE
    )
  )
}


#' Decompose specialization from diversification via the Olley-Pakes
#' allocation covariance
#'
#' @description
#' [decompose_cropland_surplus()], [decompose_semi_natural_surplus()], and
#' [decompose_manure_losses()] are all simplified to national-only views
#' with no spatial, destiny, or species-mix factor, so the LMDI
#' "Specialization" mechanism (in [decompose_terr_losses()]) is
#' currently empty. This function recovers the provincial and species
#' allocation signal independently, straight from the underlying panels:
#' it shows whether the allocation of area or herd across units
#' (provinces, destinies, species) concentrated into high-surplus units
#' (genuine specialization) or spread towards low-surplus ones
#' (diversification) — a distinction the mix alone cannot make. This
#' function adds that signal, following the Olley-Pakes
#' allocation identity used in the decomposition proposal
#' (`sum(w_i * s_i) = mean(s) + covariance(w_i, s_i)`): for a set of units
#' with area/herd share `w_i` and per-unit surplus `s_i`, the covariance
#' between the two is positive and growing when the allocation
#' concentrates into high-surplus units (specialization raising surplus),
#' and shrinks towards zero or turns negative under diversification.
#'
#' Unlike the additive LMDI contributions (in Mg N), the covariance is
#' expressed in per-unit-area or per-unit-herd surplus terms (Mg N per ha,
#' or Mg N per livestock unit) — it is not directly comparable in
#' magnitude to the "Specialization" mechanism total from
#' [decompose_terr_losses()], only in sign and trend.
#'
#' @param n_prov_destiny Nitrogen flows tibble from
#'   [create_n_prov_destiny()]. If `NULL`, loaded automatically.
#' @param raw Named list overriding any of the raw inputs (`npp_ygpit`,
#'   `codes_coefs`, `intake_ygiac`, `n_excretion_ygs`, `stock_prod_ygps`,
#'   `livestock_units`). Missing elements are loaded automatically.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A named list with tibbles `cropland_province`,
#'   `cropland_destiny`, and `livestock_species`, each with columns `year`
#'   and `covariance`.
#' @export
#'
#' @examples
#' decompose_specialization_cov(example = TRUE)
decompose_specialization_cov <- function(
  n_prov_destiny = NULL,
  raw = NULL,
  example = FALSE
) {
  if (example) {
    return(.example_decomp_spec_cov())
  }
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }
  if (is.null(raw)) {
    raw <- list()
  }
  raw <- .default_raw_inputs(
    raw,
    keys = c(
      "npp_ygpit",
      "codes_coefs",
      "intake_ygiac",
      "n_excretion_ygs",
      "stock_prod_ygps",
      "livestock_units"
    )
  )

  cropland_panel <- .build_cropland_destiny_panel(
    n_prov_destiny,
    raw$npp_ygpit,
    raw$codes_coefs
  )

  manure_panel <- .build_manure_panel(
    n_prov_destiny,
    raw$intake_ygiac,
    raw$n_excretion_ygs,
    raw$stock_prod_ygps,
    raw$livestock_units
  )

  list(
    cropland_province = .cropland_area_surplus_units(
      cropland_panel,
      "province_name"
    ) |>
      .olley_pakes_covariance(),
    cropland_destiny = .cropland_area_surplus_units(
      cropland_panel,
      "destiny_grp"
    ) |>
      .olley_pakes_covariance(),
    livestock_species = .manure_species_units(manure_panel) |>
      .olley_pakes_covariance()
  )
}


#' Compute the crop-livestock connectivity index per province
#'
#' @description
#' Computes, per province and year, two indicators of local crop-livestock
#' integration described in the decomposition proposal (section 7c) as the
#' "specialization of greatest interest" — regional crop-livestock
#' disconnection:
#' - **Local feed self-sufficiency**: the share of feed consumed by
#'   livestock in a province that was itself grown in that same province
#'   (rather than sourced from anywhere else, whether another Spanish
#'   province or abroad — `n_prov_destiny` does not distinguish
#'   inter-provincial trade from international imports, so both count as
#'   "not self-sufficient" here).
#' - **Manure-recycling ratio**: the share of a province's total cropland
#'   and semi-natural N inputs that comes from its own livestock manure,
#'   rather than synthetic fertilizer, deposition, fixation, or urban
#'   waste.
#'
#' A well-connected (mixed) province has high values on both; a
#' disconnected (specialized crop-only or livestock-only) province has low
#' values on both, since its livestock has nowhere local to send manure
#' and/or its cropland has no local manure to draw on.
#'
#' @param n_prov_destiny Nitrogen flows tibble from
#'   [create_n_prov_destiny()]. If `NULL`, loaded automatically.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A named list with tibbles `by_province` (columns `year`,
#'   `province_name`, `self_sufficiency`, `recycling_ratio`) and `national`
#'   (the unweighted across-province average of both indicators, by
#'   `year`).
#' @export
#'
#' @examples
#' decompose_crop_livestock_conn(example = TRUE)
decompose_crop_livestock_conn <- function(
  n_prov_destiny = NULL,
  example = FALSE
) {
  if (example) {
    return(.example_decomp_cl_conn())
  }
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }

  by_province <- .crop_livestock_conn_panel(n_prov_destiny)

  national <- by_province |>
    dplyr::summarise(
      self_sufficiency = mean(self_sufficiency, na.rm = TRUE),
      recycling_ratio = mean(recycling_ratio, na.rm = TRUE),
      .by = year
    )

  list(by_province = by_province, national = national)
}


#' Compute the national cropland destiny mix over time
#'
#' @description
#' Computes, per year, the share of Spain's cropland output (by mass N)
#' going to each destiny — domestic food, feed, exported food, non-food —
#' from [create_n_nat_destiny()]'s national commodity balance sheets.
#' Deliberately *not* [create_n_prov_destiny()]'s provincial data summed
#' up: the provincial `"export"` destiny does not distinguish
#' inter-provincial trade from true international export (per
#' [decompose_crop_livestock_conn()]'s same caveat on `"Outside"`
#' imports), which would overstate the export share here.
#' [create_n_nat_destiny()] instead recomputes export/import directly
#' from the national production-vs-consumption balance per item, so
#' trade between two Spanish provinces nets out rather than counting as
#' export. This is the supplementary diagnostic recommended in the
#' decomposition proposal (section 14, "attach to existing figures"), to
#' show the local-food -> feed + export transition directly, since
#' [decompose_cropland_surplus()] no longer carries a destiny factor.
#'
#' @param n_nat_destiny National nitrogen flows tibble from
#'   [create_n_nat_destiny()]. If `NULL`, computed automatically (slow).
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble with columns `year`, `destiny_grp` (one of
#'   `"domestic_food"`, `"feed"`, `"exported"`, `"non_food"`),
#'   `output_mg`, and `share` (that destiny's share of the year's total
#'   cropland output).
#' @export
#'
#' @examples
#' decompose_destiny_mix(example = TRUE)
decompose_destiny_mix <- function(n_nat_destiny = NULL, example = FALSE) {
  if (example) {
    return(.example_decomp_destiny())
  }
  if (is.null(n_nat_destiny)) {
    n_nat_destiny <- create_n_nat_destiny()
  }

  .crop_output_by_destiny(n_nat_destiny) |>
    dplyr::summarise(
      output_mg = sum(output_mg, na.rm = TRUE),
      .by = c(year, destiny_grp)
    ) |>
    dplyr::mutate(
      share = output_mg / sum(output_mg, na.rm = TRUE),
      .by = year
    )
}


#' Plot cumulative drivers of the change in territorial N losses
#'
#' @description
#' Plots two stacked-area charts of the cumulative, year-on-year
#' contribution to the change in Spain's total territorial nitrogen (N)
#' losses since the start of the reconstruction: one broken down by
#' compartment (cropland, semi-natural, manure, urban), one regrouped by
#' transformation mechanism (scale, specialization, intensification,
#' efficiency), as computed by [decompose_terr_losses()].
#'
#' @param decomp A named list from [decompose_terr_losses()].
#'   If `NULL`, computed automatically (slow).
#'
#' @return A named list with ggplot objects `by_compartment` and
#'   `by_mechanism`.
#' @export
#'
#' @examples
#' decomp <- list(
#'   by_compartment = tibble::tribble(
#'     ~t0, ~compartment, ~contribution_mgn, ~cumulative_mgn,
#'     1861, "cropland", 12000, 12000,
#'     1861, "manure", 4000, 4000,
#'     1862, "cropland", 8000, 20000,
#'     1862, "manure", -1000, 3000
#'   ),
#'   by_mechanism = tibble::tribble(
#'     ~t0, ~mechanism, ~contribution_mgn, ~cumulative_mgn,
#'     1861, "Size", 9000, 9000,
#'     1861, "Inefficiency", 7000, 7000,
#'     1862, "Size", 5000, 14000,
#'     1862, "Inefficiency", 2000, 9000
#'   )
#' )
#' plots <- plot_loss_decomp(decomp)
plot_loss_decomp <- function(decomp = NULL) {
  if (is.null(decomp)) {
    decomp <- decompose_terr_losses()
  }

  list(
    by_compartment = .plot_cumulative_stack(
      decomp$by_compartment,
      compartment,
      "Drivers of Spain's territorial N losses, by compartment",
      colors = .compartment_typology_colors(),
      labels = .compartment_display_labels()
    ),
    by_mechanism = .plot_cumulative_stack(
      decomp$by_mechanism,
      mechanism,
      "Drivers of Spain's territorial N losses, by mechanism",
      colors = .mechanism_colors()
    )
  )
}


#' Plot year-on-year (non-cumulative) drivers of the change in
#' territorial N losses
#'
#' @description
#' Uses the same data as [plot_loss_decomp()], but plots each
#' year's own additive contribution directly, without accumulating it
#' over time: each bar shows how much a compartment or mechanism
#' contributed to the change in territorial N losses in that one
#' year-on-year transition, not the running total since 1860.
#'
#' @param decomp A named list from [decompose_terr_losses()].
#'   If `NULL`, computed automatically (slow).
#'
#' @return A named list with ggplot objects `by_compartment` and
#'   `by_mechanism`.
#' @export
#'
#' @examples
#' decomp <- list(
#'   by_compartment = tibble::tribble(
#'     ~t0, ~compartment, ~contribution_mgn, ~cumulative_mgn,
#'     1861, "cropland", 12000, 12000,
#'     1862, "cropland", 8000, 20000,
#'     1861, "urban", 500, 500,
#'     1862, "urban", 700, 1200
#'   ),
#'   by_mechanism = tibble::tribble(
#'     ~t0, ~mechanism, ~contribution_mgn, ~cumulative_mgn,
#'     1861, "Size", 9000, 9000,
#'     1862, "Size", 5000, 14000,
#'     1861, "Intensification", 3500, 3500,
#'     1862, "Intensification", 3700, 7200
#'   )
#' )
#' plots <- plot_loss_decomp_yearly(decomp)
plot_loss_decomp_yearly <- function(decomp = NULL) {
  if (is.null(decomp)) {
    decomp <- decompose_terr_losses()
  }

  list(
    by_compartment = .plot_yearly_stack(
      decomp$by_compartment,
      compartment,
      "Year-on-year drivers of Spain's territorial N losses, by compartment",
      colors = .compartment_typology_colors(),
      labels = .compartment_display_labels()
    ),
    by_mechanism = .plot_yearly_stack(
      decomp$by_mechanism,
      mechanism,
      "Year-on-year drivers of Spain's territorial N losses, by mechanism",
      colors = .mechanism_colors()
    )
  )
}


#' Plot rolling-mean year-on-year drivers of the change in territorial N
#' losses
#'
#' @description
#' Same data as [plot_loss_decomp_yearly()], but smooths each
#' year's own additive contribution with a centered rolling mean
#' (`window` years wide, `NA`-padded at the edges) before plotting, to
#' make sustained multi-year trends (e.g. a period of continuously
#' improving efficiency) easier to see than in the raw, noisy
#' year-on-year series.
#'
#' @param decomp A named list from [decompose_terr_losses()].
#'   If `NULL`, computed automatically (slow).
#' @param window Width of the centered rolling-mean window, in years.
#'   Default `10`.
#'
#' @return A named list with ggplot objects `by_compartment` and
#'   `by_mechanism`.
#' @export
#'
#' @examples
#' # `window` must not exceed the number of years available per group.
#' decomp <- list(
#'   by_compartment = tibble::tribble(
#'     ~t0, ~compartment, ~contribution_mgn, ~cumulative_mgn,
#'     1861, "cropland", 12000, 12000,
#'     1862, "cropland", 8000, 20000,
#'     1863, "cropland", 9000, 29000
#'   ),
#'   by_mechanism = tibble::tribble(
#'     ~t0, ~mechanism, ~contribution_mgn, ~cumulative_mgn,
#'     1861, "Size", 9000, 9000,
#'     1862, "Size", 5000, 14000,
#'     1863, "Size", 6000, 20000
#'   )
#' )
#' plots <- plot_loss_decomp_rolling(decomp, window = 3)
plot_loss_decomp_rolling <- function(decomp = NULL, window = 10) {
  if (is.null(decomp)) {
    decomp <- decompose_terr_losses()
  }

  compartment_title <- paste0(
    window,
    "-year rolling mean drivers of Spain's territorial N losses, ",
    "by compartment"
  )
  mechanism_title <- paste0(
    window,
    "-year rolling mean drivers of Spain's territorial N losses, ",
    "by mechanism"
  )

  caption <- .rolling_caption(window)

  list(
    by_compartment = decomp$by_compartment |>
      .add_rolling_mean("compartment", window) |>
      .plot_rolling_stack(
        compartment,
        compartment_title,
        colors = .compartment_typology_colors(),
        labels = .compartment_display_labels(),
        caption = caption
      ),
    by_mechanism = decomp$by_mechanism |>
      .add_rolling_mean("mechanism", window) |>
      .plot_rolling_stack(
        mechanism,
        mechanism_title,
        colors = .mechanism_colors(),
        caption = caption
      )
  )
}


#' Plot rolling-mean year-on-year drivers of territorial N losses, as
#' one combined panel plot
#'
#' @description
#' Combines the two views from [plot_loss_decomp_rolling()]
#' (by compartment and by mechanism) side by side into a single
#' patchwork plot, matching [plot_loss_decomp_periods_panel()]'s
#' style: one shared y-axis label, each panel keeping its own legend
#' (compartment and mechanism are different fill scales, so the legends
#' aren't collected into one).
#'
#' @param decomp A named list from [decompose_terr_losses()].
#'   If `NULL`, computed automatically (slow).
#' @param window Width of the centered rolling-mean window, in years.
#'   Default `10`.
#'
#' @return A patchwork ggplot object with two panels ("By compartment",
#'   "By mechanism").
#' @export
#'
#' @examples
#' if (
#'   requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)
#' ) {
#'   decomp <- list(
#'     by_compartment = tibble::tribble(
#'       ~t0, ~compartment, ~contribution_mgn, ~cumulative_mgn,
#'       1861, "cropland", 12000, 12000,
#'       1862, "cropland", 8000, 20000,
#'       1863, "cropland", 9000, 29000
#'     ),
#'     by_mechanism = tibble::tribble(
#'       ~t0, ~mechanism, ~contribution_mgn, ~cumulative_mgn,
#'       1861, "Size", 9000, 9000,
#'       1862, "Size", 5000, 14000,
#'       1863, "Size", 6000, 20000
#'     )
#'   )
#'   panel <- plot_loss_decomp_rolling_panel(decomp, window = 3)
#' }
plot_loss_decomp_rolling_panel <- function(
  decomp = NULL,
  window = 10
) {
  rlang::check_installed(
    c("ggplot2", "patchwork"),
    "to draw the decomposition panel."
  )
  if (is.null(decomp)) {
    decomp <- decompose_terr_losses()
  }

  p_compartment <- decomp$by_compartment |>
    .add_rolling_mean("compartment", window) |>
    .plot_rolling_stack(
      compartment,
      "By compartment",
      colors = .compartment_typology_colors(),
      labels = .compartment_display_labels()
    )
  p_mechanism <- decomp$by_mechanism |>
    .add_rolling_mean("mechanism", window) |>
    .plot_rolling_stack(
      mechanism,
      "By mechanism",
      colors = .mechanism_colors(),
      y_label = NULL
    )

  .n_losses_two_panel(
    p_compartment,
    p_mechanism,
    title = paste0(
      window,
      "-year rolling mean drivers of Spain's territorial N losses"
    )
  )
}


#' Plot period-based drivers of the change in territorial N losses
#'
#' @description
#' Plots two stacked bar charts from
#' [decompose_terr_losses_periods()]: one bar per reference
#' period comparison, labeled by mean year (1865-1925, 1925-1965,
#' 1965-2015), each compared against the immediately preceding reference
#' period (chained), plus one extra bar for the full analysis window
#' (Total (1865-2015)), broken down by compartment in one chart and by
#' mechanism in the other. Contributions are normalized to Gg N/yr (see
#' [decompose_terr_losses_periods()]), since the chained
#' transitions and the Total span very different numbers of years.
#'
#' @param decomp A named list from
#'   [decompose_terr_losses_periods()]. If `NULL`, computed
#'   automatically (slow).
#'
#' @return A named list with ggplot objects `by_compartment` and
#'   `by_mechanism`.
#' @export
#'
#' @examples
#' decomp <- list(
#'   by_compartment = tibble::tribble(
#'     ~period, ~compartment, ~contribution_per_yr_mgn,
#'     "1865-1925", "cropland", 120,
#'     "1865-1925", "manure", 40,
#'     "1925-1965", "cropland", 260,
#'     "1925-1965", "manure", 90
#'   ),
#'   by_mechanism = tibble::tribble(
#'     ~period, ~mechanism, ~contribution_per_yr_mgn,
#'     "1865-1925", "Size", 90,
#'     "1865-1925", "Inefficiency", 70,
#'     "1925-1965", "Size", 150,
#'     "1925-1965", "Inefficiency", 200
#'   )
#' )
#' plots <- plot_loss_decomp_periods(decomp)
plot_loss_decomp_periods <- function(decomp = NULL) {
  if (is.null(decomp)) {
    decomp <- decompose_terr_losses_periods()
  }

  list(
    by_compartment = .plot_period_bars(
      decomp$by_compartment,
      compartment,
      "Drivers of Spain's territorial N losses by period and compartment",
      colors = .compartment_typology_colors(),
      labels = .compartment_display_labels()
    ),
    by_mechanism = .plot_period_bars(
      decomp$by_mechanism,
      mechanism,
      "Drivers of Spain's territorial N losses by period and mechanism",
      colors = .mechanism_colors()
    )
  )
}


#' Plot period-based drivers of territorial N losses, as one combined
#' panel plot
#'
#' @description
#' Combines the two views from [plot_loss_decomp_periods()]
#' (by compartment and by mechanism) side by side into a single
#' patchwork plot with one shared y-axis label, since both break down
#' contributions to the same total territorial N losses. Each panel
#' keeps its own legend (compartment and mechanism are different fill
#' scales, so the legends aren't collected into one).
#'
#' @param decomp A named list from
#'   [decompose_terr_losses_periods()]. If `NULL`, computed
#'   automatically (slow).
#'
#' @return A patchwork ggplot object with two panels ("By compartment",
#'   "By mechanism").
#' @export
#'
#' @examples
#' if (
#'   requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)
#' ) {
#'   decomp <- list(
#'     by_compartment = tibble::tribble(
#'       ~period, ~compartment, ~contribution_per_yr_mgn,
#'       "1865-1925", "cropland", 120,
#'       "1925-1965", "cropland", 260,
#'       "1865-1925", "urban", 15,
#'       "1925-1965", "urban", 35
#'     ),
#'     by_mechanism = tibble::tribble(
#'       ~period, ~mechanism, ~contribution_per_yr_mgn,
#'       "1865-1925", "Size", 90,
#'       "1925-1965", "Size", 150,
#'       "1865-1925", "Intensification", 45,
#'       "1925-1965", "Intensification", 145
#'     )
#'   )
#'   panel <- plot_loss_decomp_periods_panel(decomp)
#' }
plot_loss_decomp_periods_panel <- function(decomp = NULL) {
  rlang::check_installed(
    c("ggplot2", "patchwork"),
    "to draw the decomposition panel."
  )
  if (is.null(decomp)) {
    decomp <- decompose_terr_losses_periods()
  }

  p_compartment <- .plot_period_bars(
    decomp$by_compartment,
    compartment,
    "By compartment",
    colors = .compartment_typology_colors(),
    labels = .compartment_display_labels(),
    caption = NULL
  )
  p_mechanism <- .plot_period_bars(
    decomp$by_mechanism,
    mechanism,
    "By mechanism",
    colors = .mechanism_colors(),
    y_label = NULL,
    caption = NULL
  )

  .n_losses_two_panel(
    p_compartment,
    p_mechanism,
    title = "Drivers of Spain's territorial N losses by period",
    caption = .period_caption()
  )
}


#' Plot the specialization-vs-diversification allocation covariance
#'
#' @description
#' Plots the Olley-Pakes allocation covariance from
#' [decompose_specialization_cov()] as a line chart, one line per
#' dimension (cropland province, cropland destiny, livestock species), with
#' a zero reference line. Positive and rising values indicate genuine
#' specialization (allocation concentrating into high-surplus units);
#' values near zero or falling indicate diversification. This is meant as
#' a supplementary diagnostic reported alongside (not inside) the main
#' additive decomposition, per the decomposition proposal.
#'
#' @param covariance A named list from
#'   [decompose_specialization_cov()]. If `NULL`, computed
#'   automatically (slow).
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' covariance <- list(
#'   cropland_province = tibble::tribble(
#'     ~year, ~covariance,
#'     1960, -0.02,
#'     1980, 0.05,
#'     2000, 0.11
#'   ),
#'   livestock_species = tibble::tribble(
#'     ~year, ~covariance,
#'     1960, 0.01,
#'     1980, 0.08,
#'     2000, 0.17
#'   )
#' )
#' p <- plot_specialization_cov(covariance)
plot_specialization_cov <- function(covariance = NULL) {
  if (is.null(covariance)) {
    covariance <- decompose_specialization_cov()
  }

  combined <- purrr::imap(covariance, ~ dplyr::mutate(.x, dimension = .y)) |>
    purrr::list_rbind()

  ggplot2::ggplot(
    combined,
    ggplot2::aes(x = year, y = covariance, color = dimension)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
    ggplot2::scale_color_manual(values = .specialization_colors()) +
    ggplot2::labs(
      x = NULL,
      y = "Allocation covariance (specialization > 0, diversification < 0)",
      color = NULL,
      title = "Specialization vs. diversification (Olley-Pakes covariance)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}


#' Plot the crop-livestock connectivity index
#'
#' @description
#' Plots the national, unweighted average of local feed self-sufficiency
#' and the manure-recycling ratio from
#' [decompose_crop_livestock_conn()] as a line chart over time.
#' Falling lines indicate growing crop-livestock disconnection. This is
#' meant as a supplementary diagnostic reported alongside (not inside) the
#' main additive decomposition, per the decomposition proposal.
#'
#' @param connectivity A named list from
#'   [decompose_crop_livestock_conn()]. If `NULL`, computed
#'   automatically (slow).
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' connectivity <- list(
#'   national = tibble::tribble(
#'     ~year, ~self_sufficiency, ~recycling_ratio,
#'     1960, 0.95, 0.62,
#'     1980, 0.71, 0.48,
#'     2000, 0.54, 0.39
#'   )
#' )
#' p <- plot_crop_livestock_conn(connectivity)
plot_crop_livestock_conn <- function(connectivity = NULL) {
  if (is.null(connectivity)) {
    connectivity <- decompose_crop_livestock_conn()
  }

  national_long <- connectivity$national |>
    tidyr::pivot_longer(
      cols = c(self_sufficiency, recycling_ratio),
      names_to = "indicator",
      values_to = "value"
    )

  ggplot2::ggplot(
    national_long,
    ggplot2::aes(x = year, y = value, color = indicator)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(
      x = NULL,
      y = "Share",
      color = NULL,
      title = "Crop-livestock connectivity: feed self-sufficiency and manure recycling"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}


#' Plot the cropland destiny mix over time
#'
#' @description
#' Plots the national cropland destiny mix (domestic food / feed /
#' exported food / non-food shares) from [decompose_destiny_mix()] as a
#' line chart over time, showing the local-food -> feed + export
#' transition described in the decomposition proposal (sections 3 and
#' 14). This is meant as a supplementary diagnostic reported alongside
#' (not inside) the main additive decomposition, since
#' [decompose_cropland_surplus()] no longer carries a destiny factor.
#'
#' @param destiny_mix A tibble from [decompose_destiny_mix()]. If `NULL`,
#'   computed automatically (slow).
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' destiny_mix <- tibble::tribble(
#'   ~year, ~destiny_grp, ~share,
#'   1960, "domestic_food", 0.55,
#'   1960, "feed", 0.30,
#'   1960, "exported", 0.10,
#'   1960, "non_food", 0.05,
#'   2000, "domestic_food", 0.35,
#'   2000, "feed", 0.45,
#'   2000, "exported", 0.15,
#'   2000, "non_food", 0.05
#' )
#' p <- plot_destiny_mix(destiny_mix)
plot_destiny_mix <- function(destiny_mix = NULL) {
  if (is.null(destiny_mix)) {
    destiny_mix <- decompose_destiny_mix()
  }

  ggplot2::ggplot(
    destiny_mix,
    ggplot2::aes(x = year, y = share, color = destiny_grp)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(
      values = .destiny_mix_colors(),
      labels = .destiny_mix_labels()
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(
      x = NULL,
      y = "Share of cropland output",
      color = NULL,
      title = "Cropland destiny mix: domestic food, feed, export, non-food"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}


#' Plot each compartment's own factor breakdown
#'
#' @description
#' Plots panels 3-6 of the composite decomposition figure described in the
#' decomposition proposal (section 14): the cumulative, year-on-year
#' contribution of each factor *within* one compartment's own
#' decomposition (e.g. Cropland's Size/Intensity/Inefficiency), as
#' opposed to the AFS-wide panels 1-2 from
#' [plot_loss_decomp()], which only show each compartment's
#' total contribution.
#'
#' @param cropland A tibble from [decompose_cropland_surplus()]. If
#'   `NULL`, computed automatically (slow).
#' @param semi_natural A tibble from [decompose_semi_natural_surplus()].
#'   If `NULL`, computed automatically (slow).
#' @param manure A tibble from [decompose_manure_losses()]. If `NULL`,
#'   computed automatically (slow).
#' @param urban A tibble from [decompose_urban_losses()]. If `NULL`,
#'   computed automatically (slow).
#'
#' @return A named list with ggplot objects `cropland`, `semi_natural`,
#'   `manure`, and `urban`.
#' @export
#'
#' @examples
#' # Each argument is a calculate_lmdi() table; the same shape is reused for
#' # all four compartments here.
#' lmdi <- tibble::tribble(
#'   ~period, ~factor_label, ~component_type, ~additive,
#'   "1861-1862", "N surplus", "target", 20000,
#'   "1861-1862", "Size", "factor", 9000,
#'   "1861-1862", "Intensity", "factor", 7000,
#'   "1861-1862", "Inefficiency", "factor", 4000,
#'   "1862-1863", "N surplus", "target", 10000,
#'   "1862-1863", "Size", "factor", 5000,
#'   "1862-1863", "Intensity", "factor", 3000,
#'   "1862-1863", "Inefficiency", "factor", 2000
#' )
#' plots <- plot_compart_factor(lmdi, lmdi, lmdi, lmdi)
plot_compart_factor <- function(
  cropland = NULL,
  semi_natural = NULL,
  manure = NULL,
  urban = NULL
) {
  if (is.null(cropland)) {
    cropland <- decompose_cropland_surplus()
  }
  if (is.null(semi_natural)) {
    semi_natural <- decompose_semi_natural_surplus()
  }
  if (is.null(manure)) {
    manure <- decompose_manure_losses()
  }
  if (is.null(urban)) {
    urban <- decompose_urban_losses()
  }

  list(
    cropland = .plot_compartment_factor_panel(cropland),
    semi_natural = .plot_compartment_factor_panel(semi_natural),
    manure = .plot_compartment_factor_panel(manure),
    urban = .plot_compartment_factor_panel(urban)
  )
}


#' Plot each compartment's own factor breakdown, year-on-year
#' (non-cumulative)
#'
#' @description
#' Same as [plot_compart_factor()], but plots each year's own
#' additive contribution directly, without accumulating it over time —
#' matching [plot_loss_decomp_yearly()]'s year-on-year view,
#' one panel per compartment.
#'
#' @param cropland A tibble from [decompose_cropland_surplus()]. If
#'   `NULL`, computed automatically (slow).
#' @param semi_natural A tibble from [decompose_semi_natural_surplus()].
#'   If `NULL`, computed automatically (slow).
#' @param manure A tibble from [decompose_manure_losses()]. If `NULL`,
#'   computed automatically (slow).
#' @param urban A tibble from [decompose_urban_losses()]. If `NULL`,
#'   computed automatically (slow).
#'
#' @return A named list with ggplot objects `cropland`, `semi_natural`,
#'   `manure`, and `urban`.
#' @export
#'
#' @examples
#' lmdi <- tibble::tribble(
#'   ~period, ~factor_label, ~component_type, ~additive,
#'   "1861-1862", "N surplus", "target", 20000,
#'   "1861-1862", "Size", "factor", 9000,
#'   "1861-1862", "Intensity", "factor", 7000,
#'   "1862-1863", "N surplus", "target", 10000,
#'   "1862-1863", "Size", "factor", 5000,
#'   "1862-1863", "Intensity", "factor", 3000
#' )
#' plots <- plot_compart_factor_yearly(lmdi, lmdi, lmdi, lmdi)
plot_compart_factor_yearly <- function(
  cropland = NULL,
  semi_natural = NULL,
  manure = NULL,
  urban = NULL
) {
  if (is.null(cropland)) {
    cropland <- decompose_cropland_surplus()
  }
  if (is.null(semi_natural)) {
    semi_natural <- decompose_semi_natural_surplus()
  }
  if (is.null(manure)) {
    manure <- decompose_manure_losses()
  }
  if (is.null(urban)) {
    urban <- decompose_urban_losses()
  }

  list(
    cropland = .plot_compart_factor_yearly(cropland),
    semi_natural = .plot_compart_factor_yearly(semi_natural),
    manure = .plot_compart_factor_yearly(manure),
    urban = .plot_compart_factor_yearly(urban)
  )
}


#' Plot each compartment's own factor breakdown, rolling mean
#'
#' @description
#' Same as [plot_compart_factor_yearly()], but smooths each
#' year's own additive contribution with a centered rolling mean
#' (`window` years wide, `NA`-padded at the edges) before plotting, one
#' panel per compartment.
#'
#' @param cropland A tibble from [decompose_cropland_surplus()]. If
#'   `NULL`, computed automatically (slow).
#' @param semi_natural A tibble from [decompose_semi_natural_surplus()].
#'   If `NULL`, computed automatically (slow).
#' @param manure A tibble from [decompose_manure_losses()]. If `NULL`,
#'   computed automatically (slow).
#' @param urban A tibble from [decompose_urban_losses()]. If `NULL`,
#'   computed automatically (slow).
#' @param window Width of the centered rolling-mean window, in years.
#'   Default `10`.
#'
#' @return A named list with ggplot objects `cropland`, `semi_natural`,
#'   `manure`, and `urban`.
#' @export
#'
#' @examples
#' # `window` must not exceed the number of periods available per factor.
#' lmdi <- tibble::tribble(
#'   ~period, ~factor_label, ~component_type, ~additive,
#'   "1861-1862", "N surplus", "target", 20000,
#'   "1861-1862", "Size", "factor", 9000,
#'   "1862-1863", "N surplus", "target", 10000,
#'   "1862-1863", "Size", "factor", 5000,
#'   "1863-1864", "N surplus", "target", 12000,
#'   "1863-1864", "Size", "factor", 6000
#' )
#' plots <- plot_compart_factor_roll(lmdi, lmdi, lmdi, lmdi, window = 3)
plot_compart_factor_roll <- function(
  cropland = NULL,
  semi_natural = NULL,
  manure = NULL,
  urban = NULL,
  window = 10
) {
  if (is.null(cropland)) {
    cropland <- decompose_cropland_surplus()
  }
  if (is.null(semi_natural)) {
    semi_natural <- decompose_semi_natural_surplus()
  }
  if (is.null(manure)) {
    manure <- decompose_manure_losses()
  }
  if (is.null(urban)) {
    urban <- decompose_urban_losses()
  }

  list(
    cropland = .plot_compart_factor_roll(cropland, window),
    semi_natural = .plot_compart_factor_roll(
      semi_natural,
      window
    ),
    manure = .plot_compart_factor_roll(manure, window),
    urban = .plot_compart_factor_roll(urban, window)
  )
}


#' Plot each compartment's own factor breakdown, rolling mean, as one
#' combined panel plot
#'
#' @description
#' Same factor-level breakdown as
#' [plot_compart_factor_roll()], combining all four
#' compartments side by side into a single patchwork plot, matching
#' [plot_compart_factor_periods()]'s style: one shared y-axis
#' per axis-sharing pair (Cropland+Semi-natural share "N surpluses",
#' Livestock+Urban share "N losses"), one shared legend, fixed unique
#' colors per factor label.
#'
#' @param cropland A tibble from [decompose_cropland_surplus()]. If
#'   `NULL`, computed automatically (slow).
#' @param semi_natural A tibble from [decompose_semi_natural_surplus()].
#'   If `NULL`, computed automatically (slow).
#' @param manure A tibble from [decompose_manure_losses()]. If `NULL`,
#'   computed automatically (slow).
#' @param urban A tibble from [decompose_urban_losses()]. If `NULL`,
#'   computed automatically (slow).
#' @param window Width of the centered rolling-mean window, in years.
#'   Default `10`.
#'
#' @return A patchwork ggplot object: one panel per compartment, side by
#'   side, with a single shared legend.
#' @export
#'
#' @examples
#' if (
#'   requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)
#' ) {
#'   lmdi <- tibble::tribble(
#'     ~period, ~factor_label, ~component_type, ~additive,
#'     "1861-1862", "N surplus", "target", 20000,
#'     "1861-1862", "Size", "factor", 9000,
#'     "1862-1863", "N surplus", "target", 10000,
#'     "1862-1863", "Size", "factor", 5000,
#'     "1863-1864", "N surplus", "target", 12000,
#'     "1863-1864", "Size", "factor", 6000
#'   )
#'   panel <- plot_compart_factor_roll_panel(
#'     lmdi,
#'     lmdi,
#'     lmdi,
#'     lmdi,
#'     window = 3
#'   )
#' }
plot_compart_factor_roll_panel <- function(
  cropland = NULL,
  semi_natural = NULL,
  manure = NULL,
  urban = NULL,
  window = 10
) {
  rlang::check_installed(
    c("ggplot2", "patchwork"),
    "to draw the decomposition panel."
  )
  if (is.null(cropland)) {
    cropland <- decompose_cropland_surplus()
  }
  if (is.null(semi_natural)) {
    semi_natural <- decompose_semi_natural_surplus()
  }
  if (is.null(manure)) {
    manure <- decompose_manure_losses()
  }
  if (is.null(urban)) {
    urban <- decompose_urban_losses()
  }

  titles <- .compartment_panel_titles()
  plot_data <- list(
    cropland = .compart_roll_factor_data(cropland, window),
    semi_natural = .compart_roll_factor_data(semi_natural, window),
    manure = .compart_roll_factor_data(manure, window),
    urban = .compart_roll_factor_data(urban, window)
  )
  surplus_ylim <- .stacked_bar_range(
    list(plot_data$cropland, plot_data$semi_natural),
    value_col = "rolling_mgn",
    x_col = "t0"
  )
  loss_ylim <- .stacked_bar_range(
    list(plot_data$manure, plot_data$urban),
    value_col = "rolling_mgn",
    x_col = "t0"
  )

  panels <- list(
    cropland = .plot_compartment_rolling_bar(
      plot_data$cropland,
      titles[["cropland"]],
      y_label = "Contribution to territorial N surpluses (Gg N/yr)",
      ylim = surplus_ylim
    ),
    semi_natural = .plot_compartment_rolling_bar(
      plot_data$semi_natural,
      titles[["semi_natural"]],
      ylim = surplus_ylim
    ),
    manure = .plot_compartment_rolling_bar(
      plot_data$manure,
      titles[["manure"]],
      y_label = "Contribution to territorial N losses (Gg N/yr)",
      ylim = loss_ylim
    ),
    urban = .plot_compartment_rolling_bar(
      plot_data$urban,
      titles[["urban"]],
      ylim = loss_ylim
    )
  )

  .compartment_panel_row(
    panels$cropland,
    panels$semi_natural,
    panels$manure,
    panels$urban,
    title = paste0(
      window,
      "-year rolling mean drivers of Spain's territorial N losses"
    )
  )
}


#' Plot each compartment's own factor breakdown by period, as one
#' combined panel plot
#'
#' @description
#' Same factor-level breakdown as [plot_compart_factor()], but
#' using the four reference-period bars from
#' [decompose_terr_losses_periods()] (matching
#' [plot_loss_decomp_periods()]) instead of the year-on-year
#' cumulative view, and combining all four compartments side by side into
#' a single patchwork plot sharing one legend, instead of four separate
#' ggplot objects. Each factor label has a fixed, unique color across the
#' whole plot, so no two factors share a color.
#'
#' @param cropland A tibble from [decompose_cropland_surplus()] with
#'   `by_period = TRUE`. If `NULL`, computed automatically (slow).
#' @param semi_natural A tibble from [decompose_semi_natural_surplus()]
#'   with `by_period = TRUE`. If `NULL`, computed automatically (slow).
#' @param manure A tibble from [decompose_manure_losses()] with
#'   `by_period = TRUE`. If `NULL`, computed automatically (slow).
#' @param urban A tibble from [decompose_urban_losses()] with
#'   `by_period = TRUE`. If `NULL`, computed automatically (slow).
#'
#' @return A patchwork ggplot object: one panel per compartment, side by
#'   side, with a single shared legend.
#' @export
#'
#' @examples
#' if (
#'   requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)
#' ) {
#'   # Each argument is a by-period calculate_lmdi() table, as returned by
#'   # decompose_cropland_surplus(by_period = TRUE).
#'   lmdi <- tibble::tribble(
#'     ~period, ~period_years, ~factor_label, ~component_type, ~additive,
#'     "1865-1925", 60, "Size", "factor", 5400,
#'     "1865-1925", 60, "Intensity", "factor", 3600,
#'     "1865-1925", 60, "Inefficiency", "factor", -1200,
#'     "1925-1965", 40, "Size", "factor", 4000,
#'     "1925-1965", 40, "Intensity", "factor", 9200,
#'     "1925-1965", 40, "Inefficiency", "factor", 2800
#'   )
#'   panel <- plot_compart_factor_periods(lmdi, lmdi, lmdi, lmdi)
#' }
plot_compart_factor_periods <- function(
  cropland = NULL,
  semi_natural = NULL,
  manure = NULL,
  urban = NULL
) {
  rlang::check_installed(
    c("ggplot2", "patchwork"),
    "to draw the decomposition panel."
  )
  if (is.null(cropland)) {
    cropland <- decompose_cropland_surplus(by_period = TRUE)
  }
  if (is.null(semi_natural)) {
    semi_natural <- decompose_semi_natural_surplus(by_period = TRUE)
  }
  if (is.null(manure)) {
    manure <- decompose_manure_losses(by_period = TRUE)
  }
  if (is.null(urban)) {
    urban <- decompose_urban_losses(by_period = TRUE)
  }

  titles <- .compartment_panel_titles()
  plot_data <- list(
    cropland = .compart_period_factor_data(cropland),
    semi_natural = .compart_period_factor_data(semi_natural),
    manure = .compart_period_factor_data(manure),
    urban = .compart_period_factor_data(urban)
  )
  surplus_ylim <- .stacked_bar_range(
    list(plot_data$cropland, plot_data$semi_natural),
    value_col = "contribution_per_yr_mgn",
    x_col = "period"
  )
  loss_ylim <- .stacked_bar_range(
    list(plot_data$manure, plot_data$urban),
    value_col = "contribution_per_yr_mgn",
    x_col = "period"
  )

  panels <- list(
    cropland = .plot_compartment_period_bar(
      plot_data$cropland,
      titles[["cropland"]],
      y_label = "Contribution to territorial N surpluses (Gg N/yr)",
      ylim = surplus_ylim
    ),
    semi_natural = .plot_compartment_period_bar(
      plot_data$semi_natural,
      titles[["semi_natural"]],
      ylim = surplus_ylim
    ),
    manure = .plot_compartment_period_bar(
      plot_data$manure,
      titles[["manure"]],
      y_label = "Contribution to territorial N losses (Gg N/yr)",
      ylim = loss_ylim
    ),
    urban = .plot_compartment_period_bar(
      plot_data$urban,
      titles[["urban"]],
      ylim = loss_ylim
    )
  )

  .compartment_panel_row(
    panels$cropland,
    panels$semi_natural,
    panels$manure,
    panels$urban,
    caption = .period_caption()
  )
}


# --- Private helpers: cropland panel -----------------------------------------

.destiny_group <- function(destiny) {
  dplyr::case_when(
    # population_food_inedible is the remainder .split_food_inedible_loss()
    # (n_prov_destiny.R) split out of population_food -- it left the field
    # the same way, so it stays grouped as domestic_food here.
    destiny %in% c("population_food", "population_food_inedible") ~
      "domestic_food",
    destiny == "population_other_uses" ~ "non_food",
    destiny %in% c("livestock_rum", "livestock_mono") ~ "feed",
    destiny == "export" ~ "exported"
  )
}

.crop_output_by_destiny <- function(n_prov_destiny) {
  n_prov_destiny |>
    dplyr::filter(
      origin == "Cropland",
      destiny %in%
        c(
          "population_food",
          "population_food_inedible",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::mutate(
      year = as.numeric(year),
      destiny_grp = .destiny_group(destiny)
    ) |>
    dplyr::summarise(
      output_mg = sum(mg_n, na.rm = TRUE),
      .by = c(year, province_name, item, destiny_grp)
    )
}

.crop_item_destiny_shares <- function(output_by_destiny) {
  output_by_destiny |>
    dplyr::mutate(
      item_total = sum(output_mg, na.rm = TRUE),
      .by = c(year, province_name, item)
    ) |>
    dplyr::mutate(
      share = dplyr::if_else(item_total > 0, output_mg / item_total, 0)
    ) |>
    dplyr::select(year, province_name, item, destiny_grp, share, output_mg)
}

.crop_item_inputs <- function(n_prov_destiny) {
  n_prov_destiny |>
    dplyr::filter(
      origin %in%
        c("Deposition", "Fixation", "Synthetic", "Livestock", "People"),
      destiny == "Cropland"
    ) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::summarise(
      input_mg = sum(mg_n, na.rm = TRUE),
      .by = c(year, province_name, item)
    )
}

.crop_item_area <- function(npp_ygpit, codes_coefs) {
  .merge_items_biomass(npp_ygpit, codes_coefs) |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::summarise(
      area_ha = sum(Area_ygpit_ha, na.rm = TRUE),
      .by = c(Year, Province_name, Item)
    ) |>
    dplyr::rename(year = Year, province_name = Province_name, item = Item) |>
    dplyr::mutate(year = as.numeric(year))
}

.allocate_by_destiny_share <- function(item_values, shares, value_col) {
  shares |>
    dplyr::full_join(item_values, by = c("year", "province_name", "item")) |>
    dplyr::mutate(
      # Items with no tracked-destiny output keep their full mass here,
      # tagged instead of silently dropped or misattributed.
      destiny_grp = dplyr::coalesce(destiny_grp, "no_tracked_output"),
      share = dplyr::coalesce(share, 1)
    ) |>
    dplyr::mutate(allocated = dplyr::coalesce(.data[[value_col]], 0) * share) |>
    dplyr::summarise(
      allocated = sum(allocated, na.rm = TRUE),
      .by = c(year, province_name, destiny_grp)
    )
}

.build_cropland_destiny_panel <- function(
  n_prov_destiny,
  npp_ygpit,
  codes_coefs
) {
  shares <- .crop_output_by_destiny(n_prov_destiny) |>
    .crop_item_destiny_shares()

  inputs_pu <- .allocate_by_destiny_share(
    .crop_item_inputs(n_prov_destiny),
    shares,
    "input_mg"
  ) |>
    dplyr::rename(inputs = allocated)

  area_pu <- .allocate_by_destiny_share(
    .crop_item_area(npp_ygpit, codes_coefs),
    shares,
    "area_ha"
  ) |>
    dplyr::rename(area = allocated)

  outputs_pu <- shares |>
    dplyr::summarise(
      outputs = sum(output_mg, na.rm = TRUE),
      .by = c(year, province_name, destiny_grp)
    )

  .assemble_cropland_panel(inputs_pu, area_pu, outputs_pu)
}

.assemble_cropland_panel <- function(inputs_pu, area_pu, outputs_pu) {
  inputs_pu |>
    dplyr::full_join(area_pu, by = c("year", "province_name", "destiny_grp")) |>
    dplyr::full_join(
      outputs_pu,
      by = c("year", "province_name", "destiny_grp")
    ) |>
    dplyr::mutate(
      dplyr::across(c(inputs, area, outputs), ~ dplyr::coalesce(.x, 0))
    ) |>
    tidyr::complete(
      tidyr::nesting(year, province_name),
      destiny_grp = c(
        "domestic_food",
        "feed",
        "exported",
        "non_food",
        "no_tracked_output"
      ),
      fill = list(inputs = 0, area = 0, outputs = 0)
    ) |>
    dplyr::mutate(surplus = inputs - outputs) |>
    dplyr::mutate(total_area = sum(area, na.rm = TRUE), .by = year)
}


# --- Private helpers: Olley-Pakes specialization covariance -------------------

.cropland_area_surplus_units <- function(panel, unit_col) {
  panel |>
    dplyr::summarise(
      area = sum(area, na.rm = TRUE),
      surplus = sum(surplus, na.rm = TRUE),
      total_area = dplyr::first(total_area),
      .by = dplyr::all_of(c("year", unit_col))
    ) |>
    dplyr::mutate(
      w = dplyr::if_else(total_area > 0, area / total_area, 0),
      s = dplyr::if_else(area > 0, surplus / area, 0)
    )
}

.manure_species_units <- function(panel) {
  panel |>
    dplyr::mutate(
      w = dplyr::if_else(herd_total > 0, herd_lu / herd_total, 0),
      s = dplyr::if_else(herd_lu > 0, loss / herd_lu, 0)
    )
}

# Olley-Pakes allocation covariance: sum_i w_i*s_i = mean(s) + covariance,
# where the covariance is computed unweighted across units i within a year.
.olley_pakes_covariance <- function(units) {
  units |>
    dplyr::mutate(
      w_mean = mean(w, na.rm = TRUE),
      s_mean = mean(s, na.rm = TRUE),
      .by = year
    ) |>
    dplyr::summarise(
      covariance = sum((w - w_mean) * (s - s_mean), na.rm = TRUE),
      .by = year
    ) |>
    dplyr::arrange(year)
}


# --- Private helpers: crop-livestock connectivity -----------------------------

.local_feed_self_sufficiency <- function(n_prov_destiny) {
  feed <- n_prov_destiny |>
    dplyr::filter(destiny %in% c("livestock_rum", "livestock_mono")) |>
    dplyr::mutate(year = as.numeric(year))

  local_feed <- feed |>
    dplyr::filter(origin != "Outside") |>
    dplyr::summarise(
      local_feed = sum(mg_n, na.rm = TRUE),
      .by = c(year, province_name)
    )

  total_feed <- feed |>
    dplyr::summarise(
      total_feed = sum(mg_n, na.rm = TRUE),
      .by = c(year, province_name)
    )

  dplyr::full_join(total_feed, local_feed, by = c("year", "province_name")) |>
    dplyr::mutate(
      local_feed = dplyr::coalesce(local_feed, 0),
      self_sufficiency = dplyr::if_else(
        total_feed > 0,
        local_feed / total_feed,
        0
      )
    )
}

.manure_recycling_ratio <- function(n_prov_destiny) {
  land_destinies <- c("Cropland", "semi_natural_agroecosystems")

  manure_n <- n_prov_destiny |>
    dplyr::filter(origin == "Livestock", destiny %in% land_destinies) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::summarise(
      manure_n = sum(mg_n, na.rm = TRUE),
      .by = c(year, province_name)
    )

  total_n <- n_prov_destiny |>
    dplyr::filter(
      origin %in%
        c("Deposition", "Fixation", "Synthetic", "Livestock", "People"),
      destiny %in% land_destinies
    ) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::summarise(
      total_n = sum(mg_n, na.rm = TRUE),
      .by = c(year, province_name)
    )

  dplyr::full_join(total_n, manure_n, by = c("year", "province_name")) |>
    dplyr::mutate(
      manure_n = dplyr::coalesce(manure_n, 0),
      recycling_ratio = dplyr::if_else(total_n > 0, manure_n / total_n, 0)
    )
}

.crop_livestock_conn_panel <- function(n_prov_destiny) {
  self_sufficiency <- .local_feed_self_sufficiency(n_prov_destiny) |>
    dplyr::select(year, province_name, self_sufficiency)

  recycling_ratio <- .manure_recycling_ratio(n_prov_destiny) |>
    dplyr::select(year, province_name, recycling_ratio)

  dplyr::full_join(
    self_sufficiency,
    recycling_ratio,
    by = c("year", "province_name")
  )
}


# --- Private helpers: semi-natural panel ---------------------------------------

.semi_natural_landuse_cats <- function() {
  c("Dehesa", "Forest_high", "Forest_low", "Other", "Pasture_Shrubland")
}

.build_semi_natural_panel <- function(n_prov_destiny, npp_ygpit) {
  inputs_p <- n_prov_destiny |>
    dplyr::filter(
      origin %in%
        c("Deposition", "Fixation", "Synthetic", "Livestock", "People"),
      destiny == "semi_natural_agroecosystems"
    ) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::summarise(
      inputs = sum(mg_n, na.rm = TRUE),
      .by = c(year, province_name)
    )

  outputs_p <- n_prov_destiny |>
    dplyr::filter(
      origin == "semi_natural_agroecosystems",
      destiny %in%
        c(
          "population_food",
          "population_food_inedible",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::summarise(
      outputs = sum(mg_n, na.rm = TRUE),
      .by = c(year, province_name)
    )

  area_p <- npp_ygpit |>
    dplyr::rename_with(tolower) |>
    dplyr::filter(landuse %in% .semi_natural_landuse_cats()) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::summarise(
      area = sum(area_ygpit_ha, na.rm = TRUE),
      .by = c(year, province_name)
    )

  .assemble_semi_natural_panel(inputs_p, outputs_p, area_p)
}

.assemble_semi_natural_panel <- function(inputs_p, outputs_p, area_p) {
  inputs_p |>
    dplyr::full_join(outputs_p, by = c("year", "province_name")) |>
    dplyr::full_join(area_p, by = c("year", "province_name")) |>
    dplyr::mutate(
      dplyr::across(c(inputs, outputs, area), ~ dplyr::coalesce(.x, 0))
    ) |>
    dplyr::mutate(surplus = inputs - outputs) |>
    dplyr::mutate(total_area = sum(area, na.rm = TRUE), .by = year)
}


# --- Private helpers: manure panel --------------------------------------------

.manure_identity <- function() {
  list(
    formula = paste0(
      "loss:herd_total*",
      "(feed_total/herd_total)*",
      "(excr_total/feed_total)*",
      "loss_frac"
    ),
    labels = c(
      "Manure management loss",
      "Herd size",
      "Feed intensity",
      "Excreted feed fraction",
      "Management loss fraction"
    )
  )
}

# Collapses the per-species manure panel down to the national level (no
# species-mix factor): herd_total/excr_total/loss_frac are already
# national broadcast columns in .build_manure_panel()'s output, only
# feed_total needs summing across species.
.national_manure_panel <- function(panel) {
  panel |>
    dplyr::mutate(feed_total = sum(feed_n, na.rm = TRUE), .by = year) |>
    dplyr::distinct(year, herd_total, feed_total, excr_total, loss_frac) |>
    dplyr::mutate(loss = excr_total * loss_frac)
}

.national_livestock_lu <- function(stock_prod_ygps, livestock_units) {
  stock_prod_ygps |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::select(year, province_name, livestock_cat, stock_number) |>
    dplyr::distinct() |>
    dplyr::summarise(
      stock_number = sum(stock_number, na.rm = TRUE),
      .by = c(year, livestock_cat)
    ) |>
    dplyr::left_join(
      livestock_units |> dplyr::rename_with(tolower),
      by = "livestock_cat"
    ) |>
    dplyr::filter(!is.na(lu_head), livestock_cat != "Pets") |>
    dplyr::mutate(herd_lu = stock_number * lu_head) |>
    dplyr::select(year, livestock_cat, herd_lu)
}

.national_feed_n <- function(intake_ygiac) {
  intake_ygiac |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::filter(livestock_cat != "Pets") |>
    dplyr::summarise(
      feed_n = sum(intake_mgn, na.rm = TRUE),
      .by = c(year, livestock_cat)
    )
}

.national_excretion_n <- function(n_excretion_ygs) {
  n_excretion_ygs |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::filter(livestock_cat != "Pets") |>
    dplyr::summarise(
      excr_n = sum(n_excr_mgn, na.rm = TRUE),
      .by = c(year, livestock_cat)
    )
}

.national_manure_applied <- function(n_prov_destiny) {
  n_prov_destiny |>
    dplyr::filter(
      origin == "Livestock",
      destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::summarise(applied = sum(mg_n, na.rm = TRUE), .by = year)
}

.build_manure_panel <- function(
  n_prov_destiny,
  intake_ygiac,
  n_excretion_ygs,
  stock_prod_ygps,
  livestock_units
) {
  # left_join (not full_join): the row set is anchored to species with a
  # valid LU coefficient (.national_livestock_lu()'s output). intake_ygiac /
  # n_excretion_ygs can carry categories absent from livestock_units (e.g.
  # "Fur animals", "Other", "Other_birds"); a full_join would add those with
  # herd_lu = 0 after completion, sending feed_n/herd_lu to infinity.
  panel <- .national_livestock_lu(stock_prod_ygps, livestock_units) |>
    dplyr::left_join(
      .national_feed_n(intake_ygiac),
      by = c("year", "livestock_cat")
    ) |>
    dplyr::left_join(
      .national_excretion_n(n_excretion_ygs),
      by = c("year", "livestock_cat")
    ) |>
    tidyr::complete(
      year,
      livestock_cat,
      fill = list(herd_lu = 0, feed_n = 0, excr_n = 0)
    )

  # excr_total must cover all species like `applied` does, not just the
  # LU-covered ones in `panel`, or loss_frac compares mismatched scopes.
  excr_total <- .national_excretion_n(n_excretion_ygs) |>
    dplyr::summarise(excr_total = sum(excr_n, na.rm = TRUE), .by = year)

  .finalize_manure_panel(
    panel,
    excr_total,
    .national_manure_applied(n_prov_destiny)
  )
}

.finalize_manure_panel <- function(panel, excr_total, applied) {
  panel |>
    dplyr::left_join(excr_total, by = "year") |>
    dplyr::left_join(applied, by = "year") |>
    dplyr::mutate(
      loss_frac = dplyr::if_else(
        excr_total > 0,
        (excr_total - dplyr::coalesce(applied, 0)) / excr_total,
        0
      ),
      loss = excr_n * loss_frac
    ) |>
    dplyr::mutate(herd_total = sum(herd_lu, na.rm = TRUE), .by = year)
}


# --- Private helpers: urban panel ----------------------------------------------

.urban_identity <- function() {
  list(
    formula = "loss:population*excr_pc*loss_frac",
    labels = c(
      "Urban N loss",
      "Population",
      "Per-capita excretion",
      "Non-recycled fraction"
    )
  )
}

.build_urban_panel <- function(n_prov_destiny, population_yg) {
  excr_h <- n_prov_destiny |>
    dplyr::filter(destiny == "population_food") |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::summarise(excr_h = sum(mg_n, na.rm = TRUE), .by = year)

  recycled <- n_prov_destiny |>
    dplyr::filter(
      origin == "People",
      destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::summarise(recycled = sum(mg_n, na.rm = TRUE), .by = year)

  pop <- population_yg |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::summarise(population = sum(pop_mpeop_yg, na.rm = TRUE), .by = year)

  .assemble_urban_panel(excr_h, recycled, pop)
}

.assemble_urban_panel <- function(excr_h, recycled, pop) {
  panel <- excr_h |>
    dplyr::full_join(recycled, by = "year") |>
    dplyr::full_join(pop, by = "year") |>
    dplyr::mutate(
      dplyr::across(c(excr_h, recycled, population), ~ dplyr::coalesce(.x, 0))
    ) |>
    dplyr::mutate(
      excr_pc = dplyr::if_else(population > 0, excr_h / population, 0),
      loss_frac = dplyr::if_else(excr_h > 0, (excr_h - recycled) / excr_h, 0),
      loss = excr_h - recycled
    )

  # excr_h = 0 forces the identity to 0 even when loss (excr_h - recycled)
  # isn't; warn instead of letting that mismatch pass silently.
  n_broken <- sum(panel$excr_h == 0 & panel$recycled != 0)
  if (n_broken > 0) {
    cli::cli_warn(c(
      "Urban losses: {n_broken} year{?s} with excr_h = 0 but recycled > 0.",
      "i" = "excr_pc and loss_frac both collapse to 0 there, so the",
      "i" = "additive identity no longer equals the observed loss."
    ))
  }

  panel
}


# --- Private helpers: shared across compartments ------------------------------

# Collapses an area/inputs/surplus panel (at any finer grain, e.g. province
# x destiny) down to the national level by summing, for the simplified
# Size/Intensity/Inefficiency decomposition used by cropland and
# semi-natural.
.national_area_panel <- function(panel) {
  panel |>
    dplyr::summarise(
      total_area = sum(area, na.rm = TRUE),
      inputs = sum(inputs, na.rm = TRUE),
      surplus = sum(surplus, na.rm = TRUE),
      .by = year
    )
}

.simple_area_identity <- function(target_label) {
  list(
    formula = "surplus:total_area*(inputs/total_area)*(surplus/inputs)",
    labels = c(target_label, "Size", "Intensity", "Inefficiency")
  )
}

# Reuses .assign_period_label() (circularity_index.R) for the same four
# reference periods (1860-1870, 1920-1930, 1960-1970, 2010-2020), then
# averages every numeric column within each period (plus any extra
# grouping columns), replacing `year` with the period's start year.
.period_average_panel <- function(panel, group_cols) {
  panel |>
    .assign_period_label() |>
    dplyr::mutate(
      year = as.numeric(stringr::str_extract(period_label, "^[0-9]+"))
    ) |>
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .by = dplyr::all_of(c("year", group_cols))
    )
}

# Each reference period vs. the immediately preceding one (chained, not
# vs. a fixed 1860-1870 baseline), plus one extra transition spanning the
# full analysis window (1860-1870 straight to 2010-2020), for
# calculate_lmdi()'s `periods` argument: 1860-1870 -> 1920-1930 ->
# 1960-1970 -> 2010-2020, and 1860-1870 -> 2010-2020 (total change).
.reference_period_pairs <- function() {
  tibble::tibble(
    t0 = c(1860, 1920, 1960, 1860),
    t_t = c(1920, 1960, 2010, 2010)
  )
}

.raw_input_loaders <- function() {
  list(
    npp_ygpit = function() whep_read_file("npp_ygpit"),
    codes_coefs = function() whep_read_file("codes_coefs"),
    intake_ygiac = function() whep_read_file("intake_ygiac"),
    n_excretion_ygs = function() whep_read_file("n_excretion_ygs"),
    stock_prod_ygps = function() whep_read_file("stock_prod_ygps"),
    livestock_units = function() whep_read_file("livestock_units"),
    population_yg = function() whep_read_file("population_yg")
  )
}

# Only loads (via whep_read_file(), which may hit the network) the `keys`
# not already supplied as a non-NULL element of `raw`, so callers that only
# need a subset of the raw inputs never pay for the rest.
.default_raw_inputs <- function(raw, keys) {
  supplied <- names(raw)[!vapply(raw, is.null, logical(1))]
  missing <- setdiff(keys, supplied)
  raw[missing] <- purrr::map(.raw_input_loaders()[missing], ~ .x())
  raw[keys]
}

.warn_if_sign_change <- function(panel, value_col, group_cols, label) {
  ordered <- panel |> dplyr::arrange(year)
  flagged <- if (length(group_cols) > 0) {
    ordered |>
      dplyr::mutate(
        prev_sign = dplyr::lag(sign({{ value_col }})),
        .by = dplyr::all_of(group_cols)
      )
  } else {
    ordered |> dplyr::mutate(prev_sign = dplyr::lag(sign({{ value_col }})))
  }

  n_changes <- flagged |>
    dplyr::filter(!is.na(prev_sign), sign({{ value_col }}) != prev_sign) |>
    nrow()

  if (n_changes > 0) {
    cli::cli_warn(c(
      "{label}: sign changes detected in {n_changes} year cell{?s}.",
      "i" = "LMDI cannot handle sign-changing series; consider Shapley/Sun."
    ))
  }
}

.decomp_mechanism_lookup <- function() {
  tibble::tribble(
    ~factor_label, ~mechanism,
    "Size", "Size",
    "Intensity", "Intensification",
    "Inefficiency", "Inefficiency",
    "Herd size", "Size",
    "Feed intensity", "Intensification",
    "Excreted feed fraction", "Inefficiency",
    "Management loss fraction", "Inefficiency",
    "Population", "Size",
    "Per-capita excretion", "Size",
    "Non-recycled fraction", "Inefficiency"
  )
}

.tag_mechanism <- function(decomp_df) {
  decomp_df |>
    dplyr::left_join(.decomp_mechanism_lookup(), by = "factor_label") |>
    dplyr::mutate(
      mechanism = dplyr::if_else(component_type == "target", "Total", mechanism)
    )
}

.add_period_start <- function(decomp_df) {
  decomp_df |>
    dplyr::mutate(t0 = as.numeric(stringr::str_extract(period, "^[0-9]+")))
}

.cumulate_series <- function(detail, group_col, target_only) {
  component <- if (target_only) "target" else "factor"

  detail |>
    dplyr::filter(component_type == component) |>
    dplyr::summarise(
      contribution_mgn = sum(additive, na.rm = TRUE),
      .by = dplyr::all_of(c("t0", group_col))
    ) |>
    dplyr::arrange(t0) |>
    dplyr::mutate(
      cumulative_mgn = cumsum(contribution_mgn),
      .by = dplyr::all_of(group_col)
    )
}

# Smooths the noisy year-on-year contribution with a centered rolling
# mean (zoo::rollmean(), NA-padded at the edges, dropping the first/last
# ~window/2 years), to make sustained multi-year trends (e.g. a period
# of continuously improving efficiency) easier to see than in the raw
# series.
.add_rolling_mean <- function(df, group_col, window) {
  df |>
    dplyr::arrange(t0) |>
    dplyr::mutate(
      rolling_mgn = zoo::rollmean(
        contribution_mgn,
        k = window,
        fill = NA,
        align = "center"
      ),
      .by = dplyr::all_of(group_col)
    )
}

# Unlike .cumulate_series(), each transition already compares against the
# immediately preceding reference period (via .reference_period_pairs()),
# so contributions are just summed per transition, not accumulated
# further over time. Also normalizes to a per-year rate
# (contribution_per_yr_mgn = contribution_mgn / period_years), since the
# chained transitions and the Total transition span very different
# numbers of years and are otherwise not directly comparable.
.aggregate_period_series <- function(detail, group_col, target_only) {
  component <- if (target_only) "target" else "factor"

  detail |>
    dplyr::filter(component_type == component) |>
    dplyr::summarise(
      contribution_mgn = sum(additive, na.rm = TRUE),
      period_years = dplyr::first(period_years),
      .by = dplyr::all_of(c("period", group_col))
    ) |>
    dplyr::mutate(contribution_per_yr_mgn = contribution_mgn / period_years) |>
    .relabel_period_transitions()
}

# calculate_lmdi() labels a transition "1860-1920" (t0-t_final); relabel
# it with the short mean-year-vs-mean-year label shown under each bar
# (e.g. "1865-1925"), and the 1860-2010 transition as the full-period
# total. Levels are set explicitly (not left to alphabetical sort) so
# the total bar always plots last. See .period_caption() for the label
# key (which mean year stands for which reference window).
.relabel_period_transitions <- function(df) {
  window_labels <- c(
    `1860-1920` = "1865-1925",
    `1920-1960` = "1925-1965",
    `1960-2010` = "1965-2015",
    `1860-2010` = "Total (1865-2015)"
  )
  period_levels <- c(
    "1865-1925",
    "1925-1965",
    "1965-2015",
    "Total (1865-2015)"
  )
  relabeled <- unname(window_labels[df$period])
  dplyr::mutate(df, period = factor(relabeled, levels = period_levels))
}

# Explains the shorthand mean-year period labels (e.g. "1865-1925") used
# throughout the period plots and panels.
.period_caption <- function() {
  paste0(
    "Period labels show the mean year of each reference window: ",
    "1865 = mean of 1860-1870, 1925 = mean of 1920-1930, ",
    "1965 = mean of 1960-1970, 2015 = mean of 2010-2020."
  )
}

# Explains the rolling-mean window size, so the smoothed panels are
# never mistaken for the raw yearly ones.
.rolling_caption <- function(window) {
  paste0(
    "Contributions smoothed with a ",
    window,
    "-year centered rolling mean (edges dropped where undefined)."
  )
}


# --- Private helpers: plots ---------------------------------------------------

.plot_cumulative_stack <- function(
  df,
  fill_var,
  title,
  colors = NULL,
  labels = ggplot2::waiver()
) {
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = t0, y = cumulative_mgn / 1000, fill = {{ fill_var }})
  ) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
    ggplot2::labs(
      x = NULL,
      y = "Cumulative contribution to territorial N losses (Gg N)",
      fill = NULL,
      title = title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  if (is.null(colors)) {
    plot
  } else {
    plot + ggplot2::scale_fill_manual(values = colors, labels = labels)
  }
}

# Same data as .plot_cumulative_stack(), but plots the un-accumulated
# year-on-year contribution (contribution_mgn) instead of the running
# total (cumulative_mgn).
.plot_yearly_stack <- function(
  df,
  fill_var,
  title,
  colors = NULL,
  labels = ggplot2::waiver()
) {
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = t0, y = contribution_mgn / 1000, fill = {{ fill_var }})
  ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
    ggplot2::labs(
      x = NULL,
      y = "Year-on-year contribution to territorial N losses (Gg N)",
      fill = NULL,
      title = title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  if (is.null(colors)) {
    plot
  } else {
    plot + ggplot2::scale_fill_manual(values = colors, labels = labels)
  }
}

# Same data as .plot_yearly_stack(), but plots the smoothed rolling_mgn
# (see .add_rolling_mean()) instead of the raw, noisy year-on-year
# contribution.
.plot_rolling_stack <- function(
  df,
  fill_var,
  title,
  colors = NULL,
  labels = ggplot2::waiver(),
  y_label = "Contribution to territorial N losses (Gg N/yr)",
  caption = NULL
) {
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = t0, y = rolling_mgn / 1000, fill = {{ fill_var }})
  ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
    ggplot2::labs(
      x = NULL,
      y = y_label,
      fill = NULL,
      title = title,
      caption = caption
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  if (is.null(colors)) {
    plot
  } else {
    plot + ggplot2::scale_fill_manual(values = colors, labels = labels)
  }
}

.plot_period_bars <- function(
  df,
  fill_var,
  title,
  colors,
  labels = ggplot2::waiver(),
  y_label = "Contribution to territorial N losses (Gg N/yr)",
  caption = .period_caption()
) {
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = period,
      y = contribution_per_yr_mgn / 1000,
      fill = {{ fill_var }}
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
    ggplot2::scale_fill_manual(values = colors, labels = labels) +
    ggplot2::labs(
      x = NULL,
      y = y_label,
      fill = NULL,
      title = title,
      caption = caption
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

# One compartment's own factors (not its total contribution to the AFS-wide
# view): cumulates each factor_label over time and stacks them, auto-titled
# from the compartment's own target label (e.g. "Cropland N surplus").
.plot_compartment_factor_panel <- function(decomp) {
  title <- unique(decomp$factor_label[decomp$component_type == "target"])[1]

  decomp |>
    .add_period_start() |>
    .cumulate_series("factor_label", target_only = FALSE) |>
    .plot_cumulative_stack(factor_label, title)
}

.plot_compart_factor_yearly <- function(decomp) {
  title <- unique(decomp$factor_label[decomp$component_type == "target"])[1]

  decomp |>
    .add_period_start() |>
    .cumulate_series("factor_label", target_only = FALSE) |>
    .plot_yearly_stack(factor_label, title)
}

.plot_compart_factor_roll <- function(decomp, window) {
  title <- unique(decomp$factor_label[decomp$component_type == "target"])[1]

  .compart_roll_factor_data(decomp, window) |>
    .plot_rolling_stack(factor_label, title)
}

# One compartment's own factors, rolling mean (not cumulated), keeping
# factor_label instead of collapsing it — the rolling counterpart of
# .compart_period_factor_data().
.compart_roll_factor_data <- function(decomp, window) {
  decomp |>
    .add_period_start() |>
    .cumulate_series("factor_label", target_only = FALSE) |>
    .add_rolling_mean("factor_label", window)
}

# Rolling counterpart of .plot_compartment_period_bar(): same
# axis-sharing (`y_label`/`ylim`) support, but one bar per year (the
# rolling-mean-smoothed value) instead of one bar per reference period.
.plot_compartment_rolling_bar <- function(
  plot_data,
  title,
  y_label = NULL,
  ylim = NULL
) {
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = t0, y = rolling_mgn / 1000, fill = factor_label)
  ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
    ggplot2::scale_fill_manual(values = .factor_label_colors()) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(
      x = NULL,
      y = y_label,
      fill = NULL,
      title = title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

# Aggregates one compartment's own factors per reference-period
# transition (not cumulated, matching .aggregate_period_series(), but
# keeping factor_label instead of collapsing it), and normalizes to a
# per-year rate the same way .aggregate_period_series() does.
.compart_period_factor_data <- function(decomp) {
  decomp |>
    dplyr::filter(component_type == "factor") |>
    dplyr::summarise(
      contribution_mgn = sum(additive, na.rm = TRUE),
      period_years = dplyr::first(period_years),
      .by = dplyr::all_of(c("period", "factor_label"))
    ) |>
    dplyr::mutate(contribution_per_yr_mgn = contribution_mgn / period_years) |>
    .relabel_period_transitions()
}

# Stacked geom_col() bars sum positive and negative factor contributions
# separately per period; the range must be computed the same way (not
# just min/max of the raw values) for each data frame in `plot_data_list`
# *separately* (each is one compartment's own bars), then combined by
# taking the overall min/max across them — summing across compartments
# first would inflate the range, since they're never actually stacked
# together in the same bar.
.stacked_bar_range <- function(plot_data_list, value_col, x_col) {
  stack_totals <- function(df) {
    df |>
      dplyr::mutate(
        stack_side = dplyr::if_else(.data[[value_col]] >= 0, "pos", "neg")
      ) |>
      dplyr::summarise(
        stack_total = sum(.data[[value_col]], na.rm = TRUE) / 1000,
        .by = dplyr::all_of(c(x_col, "stack_side"))
      )
  }
  totals <- purrr::list_rbind(purrr::map(plot_data_list, stack_totals))
  c(min(c(totals$stack_total, 0)), max(c(totals$stack_total, 0)))
}

# Titled with the compartment's panel title so it reads like a facet
# strip once combined by .compartment_panel_row(). `y_label` is left
# `NULL` for panels that share the same y-axis meaning as their neighbor
# (Semi-natural reuses Cropland's "N surpluses" axis, Urban reuses
# Livestock's "N losses" axis), so the title is only printed once per
# pair. `ylim` fixes both axis-sharing panels to the same numeric range
# (see .stacked_bar_range()), for a direct visual comparison.
.plot_compartment_period_bar <- function(
  plot_data,
  title,
  y_label = NULL,
  ylim = NULL
) {
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = period,
      y = contribution_per_yr_mgn / 1000,
      fill = factor_label
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
    ggplot2::scale_fill_manual(values = .factor_label_colors()) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(
      x = NULL,
      y = y_label,
      fill = NULL,
      title = title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(
        angle = 30,
        hjust = 1,
        margin = ggplot2::margin(t = 6)
      ),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 10, l = 15)
    )
}

# Combines the four compartment panels side by side, collecting all
# four fill legends into a single shared one at the bottom.
.compartment_panel_row <- function(
  p_cropland,
  p_semi_natural,
  p_manure,
  p_urban,
  title = NULL,
  caption = NULL
) {
  patchwork::wrap_plots(
    p_cropland,
    p_semi_natural,
    p_manure,
    p_urban,
    nrow = 1
  ) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(title = title, caption = caption) &
    ggplot2::theme(
      legend.position = "bottom",
      legend.box.spacing = ggplot2::unit(0.5, "cm"),
      legend.text = ggplot2::element_text(size = 11)
    )
}

# Combines the by-compartment and by-mechanism period panels side by
# side, under one shared overall title (each panel keeps its own short
# title and legend, since the two use different fill scales).
.n_losses_two_panel <- function(
  p_compartment,
  p_mechanism,
  title,
  caption = NULL
) {
  patchwork::wrap_plots(p_compartment, p_mechanism, nrow = 1) +
    patchwork::plot_annotation(title = title, caption = caption)
}

# One fixed, unique color per factor_label across all four compartments
# (Cropland and Semi-natural share the Size/Intensity/Inefficiency
# labels, so they get the same color for the same concept). Chosen to
# also stay reasonably distinct from .compartment_typology_colors() and
# .mechanism_colors(), though those two aren't shown on this plot.
.factor_label_colors <- function() {
  c(
    "Size" = "#E41A1C",
    "Intensity" = "#377EB8",
    "Inefficiency" = "#4DAF4A",
    "Herd size" = "#984EA3",
    "Feed intensity" = "#F781BF",
    "Excreted feed fraction" = "#999999",
    "Management loss fraction" = "#66C2A5",
    "Population" = "#FC8D62",
    "Per-capita excretion" = "#8DA0CB",
    "Non-recycled fraction" = "#A6D854"
  )
}

# Matches the typology colors used in typologies_spain.R /
# typologies_spain_plot.R, so compartment colors read consistently with
# the typology figures elsewhere in the package.
.compartment_typology_colors <- function() {
  c(
    cropland = "#F7DD5A",
    semi_natural = "#66a61e",
    manure = "#b3001b",
    urban = "#6A5ACD"
  )
}

# Cropland dimensions reuse the cropland/livestock typology colors
# (yellow/red) for visual consistency; cropland_destiny gets blue since
# there's no third typology color to reuse.
.specialization_colors <- function() {
  c(
    cropland_province = "#F7DD5A",
    cropland_destiny = "#377EB8",
    livestock_species = "#b3001b"
  )
}

.destiny_mix_colors <- function() {
  c(
    domestic_food = "#1B9E77",
    feed = "#D95F02",
    exported = "#7570B3",
    non_food = "#999999"
  )
}

.destiny_mix_labels <- function() {
  c(
    domestic_food = "Domestic food",
    feed = "Feed",
    exported = "Export",
    non_food = "Non-food"
  )
}

.compartment_display_labels <- function() {
  c(
    cropland = "Cropland",
    semi_natural = "Semi-natural",
    manure = "Livestock",
    urban = "Urban"
  )
}

# Spells out "Semi-natural agroecosystems" in full, unlike the shorter
# .compartment_display_labels() used for legends elsewhere — this is the
# only place each compartment name appears as a standalone panel title.
.compartment_panel_titles <- function() {
  c(
    cropland = "Cropland",
    semi_natural = "Semi-natural agroecosystems",
    manure = "Livestock",
    urban = "Urban"
  )
}

# Deliberately outside the yellow/green/dark-red/slate-blue-purple hues
# used by .compartment_typology_colors(), so the two plots never share a
# color for a different meaning.
.mechanism_colors <- function() {
  c(
    Size = "#4477AA",
    Specialization = "#E67E00",
    Intensification = "#22A6B3",
    Inefficiency = "#795548"
  )
}
