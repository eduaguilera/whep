# Nitrogen loss cascade: ammonia volatilisation, direct soil N2O, nitrate
# leaching + denitrification, and indirect N2O (Module C, Task C5), ported
# from Spain_Hist n_fun.r.
#
# These four functions are PURE, TESTABLE row-wise transforms: they take a
# tibble x (one row per N-input record) plus explicit driver columns already
# present on x (or, for calculate_n_leaching(), a separate drainage_mm
# argument) and append new columns. None of them read any gridded raster
# themselves; wiring real gridded drivers into x is a later task's job.

#' Estimate ammonia-N volatilisation from applied nitrogen.
#'
#' @description
#' Three independent methods for the fraction of applied nitrogen
#' volatilised as ammonia. `"ipcc"` (IPCC 2019 Tier 1, `n_fun.r:914-930`)
#' needs only `fert_type` and applies a single global fraction. `"manner"`
#' (the default) dispatches each row through the process-based
#' [calculate_manner_nh3()] MANNER model (Task C4), which requires far more
#' driver detail (see Details); this asymmetry in input requirements is
#' intentional, not an oversight. `"manner_default"` dispatches each row
#' through [calculate_manner_nh3_default()] instead, the same process-based
#' organic-manure model but with `technique`/`incorporation_delay_h` filled
#' in from a documented gross-assumption blend rather than required as
#' driver columns.
#'
#' @details
#' `method = "manner"` requires `x` to already carry a `manner_fertiliser`
#' column holding the exact [calculate_manner_nh3()] `fertiliser` key
#' (`"Urea"`, `"AN"`, `"CAN"`, `"AS"`, `"cattle_slurry"`, `"pig_slurry"`,
#' `"FYM"`, `"poultry_manure"` or `"urban"`) plus every driver column that
#' key's path needs: `soil_ph`, `rate_kg_ha`, `rainfall_mm`, `irrigated`,
#' `temp_c`, `temp_c_annual_mean` for the synthetic path; `rainfall_mm`,
#' `irrigated`, `windspeed_ms`, `technique`, `system`, `temp_c`,
#' `incorporation_delay_h`, `species` (unless `manner_fertiliser ==
#' "urban"`) for the organic path. This function does not infer
#' `manner_fertiliser` from `fert_type` (e.g. which synthetic sub-type
#' `"Synthetic"` maps to is not determined by `fert_type` alone) and does
#' not silently fall back to `"ipcc"` or invent driver values: a missing
#' required column aborts naming exactly which column is absent. Each row is
#' dispatched to [calculate_manner_nh3()] individually (MANNER's dispatch is
#' inherently per-row categorical, not vectorizable across the coefficient
#' joins); this row-iteration is isolated to a small private helper.
#'
#' `method = "manner_default"` requires the same `manner_fertiliser` column
#' (restricted to the organic-manure keys, since the gross default only
#' covers that path) plus `rainfall_mm`, `irrigated`, `windspeed_ms`,
#' `system`, `temp_c` and `species` (unless `manner_fertiliser == "urban"`).
#' It does NOT require `technique` or `incorporation_delay_h`: those are
#' filled in from [manner_default_technique_mix] (see
#' [calculate_manner_nh3_default()]'s Details for the gross-assumption
#' reasoning), never invented per-row.
#'
#' @param x A tibble with `n_input_t` (numeric, tonnes N) and `fert_type`.
#'   `method = "manner"` additionally requires `manner_fertiliser` and the
#'   driver columns listed in Details. `method = "manner_default"`
#'   additionally requires `manner_fertiliser` and the driver columns listed
#'   in Details, but NOT `technique`/`incorporation_delay_h`.
#' @param method `"manner"` (default, process-based, per-row), `"ipcc"`
#'   (Tier 1, global fraction) or `"manner_default"` (process-based organic
#'   path with a gross-assumption technique/incorporation-delay blend, no
#'   `technique`/`incorporation_delay_h` columns required).
#' @param example If `TRUE`, return a small fixture instead of computing
#'   from `x`. Defaults to `FALSE`.
#' @return `x` with `nh3_n_t` and `method_nh3` appended.
#' @export
#' @examples
#' calculate_nh3(example = TRUE)
calculate_nh3 <- function(x, method = "manner", example = FALSE) {
  method <- rlang::arg_match(method, c("manner", "ipcc", "manner_default"))
  if (isTRUE(example)) {
    return(.example_nh3())
  }
  if (method == "ipcc") {
    .nh3_ipcc(x)
  } else if (method == "manner_default") {
    .nh3_manner_default(x)
  } else {
    .nh3_manner(x)
  }
}

#' Estimate direct soil N2O emissions from applied nitrogen.
#'
#' @description
#' Three emission-factor regimes for direct nitrous oxide from
#' nitrogen applied to soil. `"ipcc2019"` (the default) is the IPCC 2019
#' Tier 1 climate-disaggregated `EF1`, needing only `climate`: it reuses the
#' [n2o_efs_disaggregated] table's two climate-level rows (`irrig_type ==
#' "Tier_1"` for ATL, `irrig_type == "Med_average"` for MED, 0.010 wet /
#' 0.005 dry) with no `mf` multiplier; the ATL value (0.010) is the same
#' value documented as `EF1` in [build_crop_soil_n2o_extension()], pulled
#' from one shared source of truth rather than hardcoded a second time. It is
#' the default because it is the internationally standard, globally
#' applicable Tier 1 method. `"aguilera"` (`n_fun.r:906-912`) is a finer
#' Mediterranean-calibrated disaggregation (Cayuela et al. 2017), selectable
#' where its `irrig_type`/`fert_type` granularity is available and its
#' regional emission factors apply: `n2o_direct_n_t = n_input_t * ef * mf`,
#' `ef` from [n2o_efs_disaggregated] on `(irrig_type, climate)`, `mf` from
#' [fertiliser_n2o_modifiers] on `(fert_type, climate)`. `"ipcc2006"` uses
#' the [n2o_efs_ipcc2006] table (IPCC 2006 Tier 1 defaults, flat 0.010
#' except flooded rice 0.003), keyed like `"aguilera"` on
#' `(irrig_type, climate)` with no `mf` multiplier.
#'
#' @param x A tibble with `n_input_t` and `climate`. `method = "aguilera"`
#'   or `"ipcc2006"` additionally require `irrig_type` and (aguilera only)
#'   `fert_type`.
#' @param method `"ipcc2019"` (default, IPCC 2019 Tier 1, climate-only),
#'   `"aguilera"` (Mediterranean-calibrated, needs `irrig_type`/`fert_type`)
#'   or `"ipcc2006"` (IPCC 2006 Tier 1, needs `irrig_type`).
#' @param example If `TRUE`, return a small fixture instead of computing
#'   from `x`. Defaults to `FALSE`.
#' @return `x` with `n2o_direct_n_t` and `method_soil_n2o` appended.
#' @export
#' @examples
#' calculate_soil_n2o(example = TRUE)
calculate_soil_n2o <- function(
  x,
  method = c("ipcc2019", "aguilera", "ipcc2006"),
  example = FALSE
) {
  method <- match.arg(method)
  if (isTRUE(example)) {
    return(.example_soil_n2o())
  }
  if (method == "aguilera") {
    .soil_n2o_aguilera(x)
  } else if (method == "ipcc2006") {
    .soil_n2o_ipcc2006(x)
  } else {
    .soil_n2o_ipcc2019(x)
  }
}

#' Estimate nitrate leaching, topsoil denitrification and indirect N2O.
#'
#' @description
#' Two methods for partitioning a nitrogen surplus into leached nitrate and
#' topsoil-denitrified nitrogen. `"meisinger_drainage"` (the default,
#' `n_fun.r:932-988`) is the full Spain_Hist cascade: bins annual drainage
#' and soil organic matter share, looks up a topsoil denitrification share
#' from [meisinger_denitrification], applies subsoil NO3 reduction
#' ([subsoil_no3_reduction]) and a carbon-to-nitrogen leaching attenuation,
#' then re-derives `denitrification_n_t` as the residual of `n_surplus_t`
#' minus the computed `no3_n_t` (the raw denitrification share is only an
#' intermediate; see Details). `"ipcc_fracleach"` is a much simpler global
#' fallback using the flat `FracLEACH = 0.24` constant already documented
#' in [build_crop_soil_n2o_extension()].
#'
#' @details
#' For `method = "meisinger_drainage"`, `denitrification_n_t` is computed
#' twice: first as `n_surplus_t * denit_share` (the raw Meisinger share) to
#' derive `no3_n_t`, then overwritten as `n_surplus_t - no3_n_t` (verified
#' `n_fun.r:983`). The RETURNED `denitrification_n_t` is this second,
#' residual value, not the raw share product; this is a deliberate two-step
#' sequence in the source, not a redundant computation to simplify away.
#' Drainage and soil organic matter bins are matched with the source's
#' strictly-open `s_min < s < s_max` filter (`n_fun.r:939,942`): a value
#' exactly on a shared bin edge, or outside the covered range, matches no
#' bin and aborts via the unmatched-row check (the source drops it),
#' rather than being pulled into an adjacent or ceiling bin.
#'
#' Manure/organic rows (`fert_cat == "Manure"`, i.e. every `fert_type`
#' other than `"Synthetic"`) always join the Meisinger table's
#' `tillage == "Not_specified"` row regardless of `x$tillage`. Synthetic
#' rows join on `x$tillage`, but only `"Tillage"` exists in the source's
#' synthetic block: a synthetic row with `tillage == "No_tillage"` aborts.
#'
#' @param x A tibble with `n_surplus_t`, `fert_type`, `climate`,
#'   `irrig_cat`, `land_use`, `cn_input` (may be `NA`), `tillage` (checked
#'   only for synthetic rows) and `som_share`.
#' @param drainage_mm A numeric vector aligned to `x`'s rows giving annual
#'   drainage (mm), or a single string naming a column of `x` to use
#'   instead. Kept as a separate argument (rather than a static `x` column)
#'   because in the full pipeline it flows in from Module A's gridded water
#'   balance.
#' @param method `"meisinger_drainage"` (default) or `"ipcc_fracleach"`.
#' @param example If `TRUE`, return a small fixture instead of computing
#'   from `x`. Defaults to `FALSE`.
#' @return `x` with `no3_n_t`, `denitrification_n_t`,
#'   `n2o_indirect_no3_n_t` and `method_leaching` appended.
#' @export
#' @examples
#' calculate_n_leaching(example = TRUE)
calculate_n_leaching <- function(
  x,
  drainage_mm = NULL,
  method = c("meisinger_drainage", "ipcc_fracleach"),
  example = FALSE
) {
  method <- match.arg(method)
  if (isTRUE(example)) {
    return(.example_n_leaching())
  }
  if (method == "ipcc_fracleach") {
    .leaching_ipcc_fracleach(x)
  } else {
    .leaching_meisinger(x, .leaching_drainage_values(x, drainage_mm))
  }
}

#' Estimate indirect N2O from volatilised ammonia.
#'
#' @description
#' Converts the ammonia-N already volatilised ([calculate_nh3()]'s
#' `nh3_n_t`) into indirect nitrous oxide (`n_fun.r:955-957`). Atlantic rows
#' use the flat IPCC EF4 factor (`ef4_nh3_to_n2o_atl`, 0.016) and touch no
#' emission-factor lookup; Mediterranean rows use the disaggregated
#' [n2o_efs_disaggregated] `ef` on `(irrig_type, climate)` alone (`NH3_MgN *
#' N2O_EF`), WITHOUT the [fertiliser_n2o_modifiers] `mf` that
#' [calculate_soil_n2o()]'s `method = "aguilera"` applies to direct N2O.
#'
#' @param x A tibble with `nh3_n_t`, `climate` and (for MED rows) the
#'   `irrig_type` column [n2o_efs_disaggregated] is keyed on.
#' @param example If `TRUE`, return a small fixture instead of computing
#'   from `x`. Defaults to `FALSE`.
#' @return `x` with `n2o_indirect_nh3_n_t` appended.
#' @export
#' @examples
#' calculate_indirect_n2o_nh3(example = TRUE)
calculate_indirect_n2o_nh3 <- function(x, example = FALSE) {
  if (isTRUE(example)) {
    return(.example_indirect_n2o_nh3())
  }
  .n_check_climate(x$climate)
  med_rows <- which(x$climate == "MED")
  ef_med <- rep(NA_real_, nrow(x))
  if (length(med_rows) > 0L) {
    ef_med[med_rows] <- .indirect_nh3_ef_med(x[med_rows, , drop = FALSE])
  }
  ef4_atl <- .n_constant("ef4_nh3_to_n2o_atl")
  x |>
    dplyr::mutate(
      n2o_indirect_nh3_n_t = dplyr::if_else(
        .data$climate == "ATL",
        .data$nh3_n_t * ef4_atl,
        .data$nh3_n_t * .env$ef_med
      )
    )
}

# ---- Private helpers: calculate_nh3 ------------------------------------

# IPCC Tier 1: a single global fraction per fert_type, from
# n_attenuation_constants (n_fun.r:914-930).
.nh3_ipcc <- function(x) {
  frac_synth <- .n_constant("nh3_frac_synthetic")
  frac_org <- .n_constant("nh3_frac_organic")
  x |>
    dplyr::mutate(
      nh3_n_t = data.table::fcase(
        .data$fert_type == "Synthetic",
        .data$n_input_t * frac_synth,
        .data$fert_type %in% c("Recycling", "SOM"),
        0,
        default = .data$n_input_t * frac_org
      ),
      method_nh3 = "ipcc"
    )
}

# MANNER dispatch: requires manner_fertiliser plus every driver column its
# fertiliser path needs, aborting with the exact missing column rather than
# guessing a mapping or falling back silently. Read nh3_n_t directly from the
# model result: for organic fertilisers it includes the manure-specific
# inorganic-N fraction and therefore is not simply ef * n_input_t.
.nh3_manner <- function(x) {
  .nh3_manner_require_columns(x)
  nh3_n_t <- purrr::pmap_dbl(x, .nh3_manner_row_nh3)
  x |>
    dplyr::mutate(
      nh3_n_t = .env$nh3_n_t,
      method_nh3 = "manner"
    )
}

.nh3_manner_synthetic_cols <- c(
  "soil_ph",
  "rate_kg_ha",
  "rainfall_mm",
  "irrigated",
  "temp_c",
  "temp_c_annual_mean"
)
.nh3_manner_organic_cols <- c(
  "rainfall_mm",
  "irrigated",
  "windspeed_ms",
  "technique",
  "system",
  "temp_c",
  "incorporation_delay_h"
)

.nh3_manner_require_columns <- function(x) {
  if (!rlang::has_name(x, "manner_fertiliser")) {
    cli::cli_abort(c(
      "{.arg x} is missing required column {.field manner_fertiliser}.",
      i = paste0(
        "calculate_nh3(method = \"manner\") requires the exact ",
        "calculate_manner_nh3() fertiliser key on every row."
      )
    ))
  }
  synthetic <- c("Urea", "AN", "CAN", "AS")
  needs_species <- !all(x$manner_fertiliser %in% c(synthetic, "urban"))
  required <- c(
    if (any(x$manner_fertiliser %in% synthetic)) {
      .nh3_manner_synthetic_cols
    },
    if (any(!x$manner_fertiliser %in% synthetic)) .nh3_manner_organic_cols,
    if (needs_species) "species"
  )
  missing <- required[!purrr::map_lgl(required, \(col) rlang::has_name(x, col))]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg x} is missing required MANNER driver column{?s} {.field {missing}}.",
      i = "calculate_nh3(method = \"manner\") never invents driver values."
    ))
  }
}

# One MANNER call per row; dispatch stays isolated here rather than leaking
# a row-by-row style into the rest of this file.
.nh3_manner_row_nh3 <- function(...) {
  row <- list(...)
  synthetic <- c("Urea", "AN", "CAN", "AS")
  drivers <- if (row$manner_fertiliser %in% synthetic) {
    row[.nh3_manner_synthetic_cols]
  } else if (row$manner_fertiliser == "urban") {
    row[.nh3_manner_organic_cols]
  } else {
    c(row[.nh3_manner_organic_cols], list(species = row$species))
  }
  calculate_manner_nh3(
    n_applied_t = row$n_input_t,
    fertiliser = row$manner_fertiliser,
    drivers = drivers
  )$nh3_n_t
}

# manner_default dispatch: same organic-path driver contract as .nh3_manner
# MINUS technique/incorporation_delay_h, which calculate_manner_nh3_default()
# fills in from the gross-assumption blend instead. Reads nh3_n_t directly
# off each row's calculate_manner_nh3_default() call rather than
# re-deriving via ef * n_input_t: the blended ef already excludes the
# inorganic_n_fraction scaling (matching calculate_manner_nh3()'s organic
# path, where ef and nh3_n_t are not related by a plain n_input_t product).
.nh3_manner_default <- function(x) {
  .nh3_manner_default_req_cols(x)
  nh3_n_t <- purrr::pmap_dbl(x, .nh3_manner_default_row_nh3)
  x |>
    dplyr::mutate(
      nh3_n_t = .env$nh3_n_t,
      method_nh3 = "manner_default"
    )
}

.nh3_manner_default_cols <- c(
  "rainfall_mm",
  "irrigated",
  "windspeed_ms",
  "system",
  "temp_c"
)

.nh3_manner_default_req_cols <- function(x) {
  if (!rlang::has_name(x, "manner_fertiliser")) {
    cli::cli_abort(c(
      "{.arg x} is missing required column {.field manner_fertiliser}.",
      i = paste0(
        "calculate_nh3(method = \"manner_default\") requires the exact ",
        "calculate_manner_nh3_default() fertiliser key on every row."
      )
    ))
  }
  needs_species <- !all(x$manner_fertiliser == "urban")
  required <- c(
    .nh3_manner_default_cols,
    if (needs_species) "species"
  )
  missing <- required[!purrr::map_lgl(required, \(col) rlang::has_name(x, col))]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg x} is missing required MANNER driver column{?s} {.field {missing}}.",
      i = "calculate_nh3(method = \"manner_default\") never invents driver values."
    ))
  }
}

# One calculate_manner_nh3_default() call per row.
.nh3_manner_default_row_nh3 <- function(...) {
  row <- list(...)
  drivers <- if (row$manner_fertiliser == "urban") {
    row[.nh3_manner_default_cols]
  } else {
    c(row[.nh3_manner_default_cols], list(species = row$species))
  }
  calculate_manner_nh3_default(
    n_applied_t = row$n_input_t,
    fertiliser = row$manner_fertiliser,
    drivers = drivers
  )$nh3_n_t
}

# ---- Private helpers: calculate_soil_n2o -------------------------------

# aguilera: full disaggregated ef * mf, exposed separately so
# calculate_indirect_n2o_nh3() can reuse the raw ef*mf product without a
# second EF join.
.soil_n2o_aguilera <- function(x) {
  x |>
    dplyr::mutate(
      n2o_direct_n_t = .data$n_input_t * .soil_n2o_ef_mf_aguilera(x)
    ) |>
    dplyr::mutate(method_soil_n2o = "aguilera")
}

.soil_n2o_ef_mf_aguilera <- function(x) {
  ef <- .soil_n2o_ef_disaggregated(x)
  .soil_n2o_check_ef(ef)
  mf <- x |>
    dplyr::select("fert_type", "climate") |>
    dplyr::left_join(
      whep::fertiliser_n2o_modifiers,
      by = c("fert_type", "climate")
    ) |>
    dplyr::pull("mf")
  .soil_n2o_check_mf(x, mf)
  ef * mf
}

# The disaggregated (irrig_type, climate) ef, joined without any NA guard so
# both the aguilera direct-N2O path (which adds mf) and the MED indirect-NH3
# path (which uses ef alone) can share one join.
.soil_n2o_ef_disaggregated <- function(x) {
  x |>
    dplyr::select("irrig_type", "climate") |>
    dplyr::left_join(
      whep::n2o_efs_disaggregated,
      by = c("irrig_type", "climate")
    ) |>
    dplyr::pull("ef")
}

# MED indirect NH3-N2O uses the disaggregated ef alone (no mf), so the ATL
# branch never touches an emission factor; ATL rows keep their NA ef here and
# it is discarded by calculate_indirect_n2o_nh3()'s ATL if_else branch.
.indirect_nh3_ef_med <- function(x) {
  if (!rlang::has_name(x, "irrig_type")) {
    cli::cli_abort(c(
      "Mediterranean rows require {.field irrig_type}.",
      i = paste0(
        "Atlantic rows use the flat EF4 and do not require this column; ",
        "Mediterranean rows are keyed by irrig_type and climate."
      )
    ))
  }
  ef <- .soil_n2o_ef_disaggregated(x)
  if (anyNA(ef)) {
    cli::cli_abort(c(
      "{.field n2o_efs_disaggregated} has no factor for a Mediterranean row.",
      i = "Check irrig_type/climate against whep::n2o_efs_disaggregated."
    ))
  }
  ef
}

# Any missing factor in the direct-N2O path is unsupported. The source table's
# ATL non-Tier_1/Flooded rows deliberately carry NA, while every supported MED
# irrigation key has a finite factor; neither case may silently propagate NA.
# The separate indirect-NH3 path still bypasses this lookup entirely for ATL.
.soil_n2o_check_ef <- function(ef) {
  if (anyNA(ef)) {
    cli::cli_abort(c(
      "{.field n2o_efs_disaggregated} has no direct N2O factor for a row.",
      i = "Check irrig_type/climate against whep::n2o_efs_disaggregated."
    ))
  }
}

# An NA mf must abort, not multiply the ef into a silent NA that a downstream
# na.rm sum would drop: a missing modifier is a real data gap
# (fertiliser_n2o_modifiers) that has to be visible.
.soil_n2o_check_mf <- function(x, mf) {
  if (anyNA(mf)) {
    bad <- unique(x$fert_type[is.na(mf)])
    cli::cli_abort(c(
      "{.field fertiliser_n2o_modifiers} has no modifier for \\
       fert_type{?s} {.val {bad}}.",
      i = paste0(
        "calculate_soil_n2o(method = \"aguilera\") never treats a missing ",
        "modifier as zero; add the {.field mf} value to ",
        "{.code whep::fertiliser_n2o_modifiers}."
      )
    ))
  }
}

# ipcc2019: reuse n2o_efs_disaggregated's two climate-level rows (Tier_1 for
# ATL, Med_average for MED) as the single source of truth for the flat
# 0.010/0.005 factors -- the same 0.010 documented as EF1 in
# build_crop_soil_n2o_extension(), not re-hardcoded here.
.soil_n2o_ipcc2019 <- function(x) {
  .n_check_climate(x$climate)
  ef_atl <- .n2o_disaggregated_row("Tier_1", "ATL")
  ef_med <- .n2o_disaggregated_row("Med_average", "MED")
  x |>
    dplyr::mutate(
      n2o_direct_n_t = .data$n_input_t *
        dplyr::if_else(.data$climate == "ATL", ef_atl, ef_med),
      method_soil_n2o = "ipcc2019"
    )
}

.n2o_disaggregated_row <- function(.irrig_type, .climate) {
  whep::n2o_efs_disaggregated |>
    dplyr::filter(.data$irrig_type == .irrig_type, .data$climate == .climate) |>
    dplyr::pull("ef")
}

# ipcc2006: flat IPCC 2006 Tier 1 defaults, keyed like aguilera but with no
# mf multiplier.
.soil_n2o_ipcc2006 <- function(x) {
  ef <- x |>
    dplyr::select("irrig_type", "climate") |>
    dplyr::left_join(whep::n2o_efs_ipcc2006, by = c("irrig_type", "climate")) |>
    dplyr::pull("ef")
  if (anyNA(ef)) {
    cli::cli_abort(c(
      "{.field n2o_efs_ipcc2006} has no direct N2O factor for a row.",
      i = "Check irrig_type/climate against whep::n2o_efs_ipcc2006."
    ))
  }
  x |>
    dplyr::mutate(
      n2o_direct_n_t = .data$n_input_t * .env$ef,
      method_soil_n2o = "ipcc2006"
    )
}

# ---- Private helpers: calculate_n_leaching -----------------------------

.leaching_drainage_values <- function(x, drainage_mm) {
  if (is.character(drainage_mm) && length(drainage_mm) == 1) {
    x[[drainage_mm]]
  } else {
    drainage_mm
  }
}

# ipcc_fracleach: the flat FracLEACH constant from
# build_crop_soil_n2o_extension(), reused rather than re-derived.
.leaching_ipcc_fracleach <- function(x) {
  frac_leach <- 0.24
  ef5 <- .n_constant("ef5_no3_to_n2o")
  x |>
    dplyr::mutate(
      no3_n_t = .data$n_surplus_t * frac_leach,
      denitrification_n_t = .data$n_surplus_t - .data$no3_n_t,
      n2o_indirect_no3_n_t = .data$no3_n_t * ef5,
      method_leaching = "ipcc_fracleach"
    )
}

# meisinger_drainage: bin drainage + SOM, look up the topsoil
# denitrification share, apply subsoil NO3 reduction and the C:N
# attenuation, then re-derive denitrification_n_t as the residual
# (n_fun.r:932-988).
.leaching_meisinger <- function(x, drainage_mm) {
  fert_cat <- dplyr::if_else(x$fert_type == "Synthetic", "Synthetic", "Manure")
  .leaching_check_tillage(x, fert_cat)
  denit_share <- .leaching_denit_share(x, fert_cat, drainage_mm)
  a_cn <- .leaching_a_cn(x)
  no3_red <- .leaching_no3_red(x)
  raw_denit <- x$n_surplus_t * denit_share
  no3_n_t <- (x$n_surplus_t - raw_denit) * (1 - no3_red) * (1 - a_cn)
  ef5 <- .n_constant("ef5_no3_to_n2o")
  x |>
    dplyr::mutate(
      no3_n_t = .env$no3_n_t,
      denitrification_n_t = .data$n_surplus_t - .data$no3_n_t,
      n2o_indirect_no3_n_t = .data$no3_n_t * ef5,
      method_leaching = "meisinger_drainage"
    )
}

# Synthetic rows must supply tillage == "Tillage" (the Meisinger table's
# synthetic block has no No_tillage rows); manure rows always join
# Not_specified regardless of x$tillage, so their tillage value is never
# checked.
.leaching_check_tillage <- function(x, fert_cat) {
  bad <- fert_cat == "Synthetic" & x$tillage != "Tillage"
  if (any(bad)) {
    cli::cli_abort(c(
      "Synthetic-fertiliser rows require {.code tillage == \"Tillage\"}.",
      i = paste0(
        "meisinger_denitrification has no {.val No_tillage} rows for ",
        "{.val Synthetic}; {.val No_tillage} is unsupported here."
      )
    ))
  }
}

.leaching_denit_share <- function(x, fert_cat, drainage_mm) {
  tillage_join <- dplyr::if_else(
    fert_cat == "Synthetic",
    x$tillage,
    "Not_specified"
  )
  som_content <- .bin_range(
    x$som_share,
    whep::som_ranges,
    "som_content",
    "som_min",
    "som_max"
  )
  drainage_rate <- .bin_range(
    drainage_mm,
    whep::drainage_ranges,
    "drainage_rate",
    "s_min",
    "s_max"
  )
  tibble::tibble(
    fert_cat = fert_cat,
    tillage = tillage_join,
    som_content = som_content,
    drainage_rate = drainage_rate,
    climate = x$climate
  ) |>
    dplyr::left_join(
      whep::meisinger_denitrification,
      by = c(
        "fert_cat",
        "tillage",
        "som_content",
        "drainage_rate",
        "climate"
      )
    ) |>
    dplyr::pull("denit_share") |>
    .leaching_check_na(
      "meisinger_denitrification",
      "fert_cat/tillage/som_content/drainage_rate/climate"
    )
}

# meisinger_denitrification and subsoil_no3_reduction are lookup tables with
# real coverage gaps (e.g. fert_type "Recycling" is absent from
# subsoil_no3_reduction); an unmatched row must abort, not silently
# propagate NA through no3_n_t/denitrification_n_t/n2o_indirect_no3_n_t.
.leaching_check_na <- function(values, table_name, join_desc) {
  if (anyNA(values)) {
    cli::cli_abort(c(
      "{.field {table_name}} has no matching row for {sum(is.na(values))} \\
       row{?s} of {.arg x}.",
      i = "Check the {join_desc} combination against {.code whep::{table_name}}."
    ))
  }
  values
}

# Bin a numeric vector into a labelled class via the source's strictly-open
# min < v < max filter (n_fun.r:939,942): a value on a shared bin edge or
# outside the covered range matches no bin and returns NA (the source drops
# such rows), rather than being pulled into an adjacent or ceiling bin.
.bin_range <- function(values, ranges, label_col, min_col, max_col) {
  purrr::map_chr(values, function(v) {
    hit <- ranges[
      v > ranges[[min_col]] & v < ranges[[max_col]],
      ,
      drop = FALSE
    ]
    hit[[label_col]][1]
  })
}

.leaching_a_cn <- function(x) {
  min_cn <- .n_constant("a_cn_min_cn")
  max_a_cn <- .n_constant("a_cn_max")
  span <- dplyr::if_else(
    x$land_use == "Other",
    .n_constant("a_cn_span_other"),
    .n_constant("a_cn_span")
  )
  cn <- dplyr::coalesce(x$cn_input, min_cn)
  pmin(max_a_cn, pmax(0, (cn - min_cn) / span))
}

.leaching_no3_red <- function(x) {
  x |>
    dplyr::select("fert_type", "climate", "irrig_cat") |>
    dplyr::left_join(
      whep::subsoil_no3_reduction,
      by = c("fert_type", "climate", "irrig_cat")
    ) |>
    dplyr::pull("no3_red") |>
    .leaching_check_na("subsoil_no3_reduction", "fert_type/climate/irrig_cat")
}

# ---- Private helpers: shared ---------------------------------------------

.n_constant <- function(name) {
  whep::n_attenuation_constants |>
    dplyr::filter(.data$constant == name) |>
    dplyr::pull("value")
}

.n_check_climate <- function(climate) {
  valid <- c("ATL", "MED")
  bad <- unique(climate[is.na(climate) | !climate %in% valid])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Unexpected or missing {.field climate} value{?s}: {.val {bad}}.",
      i = "Expected {.val ATL} or {.val MED}."
    ))
  }
  invisible(NULL)
}

# ---- Private helpers: examples -------------------------------------------

.example_nh3 <- function() {
  tibble::tribble(
    ~n_input_t, ~fert_type, ~nh3_n_t, ~method_nh3,
    10, "Synthetic", 1.1, "ipcc"
  )
}

.example_soil_n2o <- function() {
  tibble::tribble(
    ~n_input_t, ~climate, ~irrig_type, ~n2o_direct_n_t, ~method_soil_n2o,
    10, "MED", "Med_average", 0.05, "ipcc2019"
  )
}

.example_n_leaching <- function() {
  tibble::tribble(
    ~n_surplus_t,
    ~no3_n_t,
    ~denitrification_n_t,
    ~n2o_indirect_no3_n_t,
    ~method_leaching,
    100,
    24,
    76,
    0.264,
    "ipcc_fracleach"
  )
}

.example_indirect_n2o_nh3 <- function() {
  tibble::tribble(
    ~nh3_n_t, ~climate, ~n2o_indirect_nh3_n_t,
    1.1, "ATL", 0.0176
  )
}
