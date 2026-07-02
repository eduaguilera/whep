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
#' Two independent methods for the fraction of applied nitrogen volatilised
#' as ammonia. `"ipcc"` (IPCC 2019 Tier 1, `n_fun.r:914-930`) needs only
#' `fert_type` and applies a single global fraction. `"manner"` (the
#' default) dispatches each row through the process-based
#' [calculate_manner_nh3()] MANNER model (Task C4), which requires far more
#' driver detail (see Details); this asymmetry in input requirements is
#' intentional, not an oversight.
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
#' @param x A tibble with `n_input_t` (numeric, tonnes N) and `fert_type`.
#'   `method = "manner"` additionally requires `manner_fertiliser` and the
#'   driver columns listed in Details.
#' @param method `"manner"` (default, process-based, per-row) or `"ipcc"`
#'   (Tier 1, global fraction).
#' @param example If `TRUE`, return a small fixture instead of computing
#'   from `x`. Defaults to `FALSE`.
#' @return `x` with `nh3_n_t` and `method_nh3` appended.
#' @export
#' @examples
#' calculate_nh3(example = TRUE)
calculate_nh3 <- function(x, method = c("manner", "ipcc"), example = FALSE) {
  method <- match.arg(method)
  if (isTRUE(example)) {
    return(.example_nh3())
  }
  if (method == "ipcc") {
    .nh3_ipcc(x)
  } else {
    .nh3_manner(x)
  }
}

#' Estimate direct soil N2O emissions from applied nitrogen.
#'
#' @description
#' Three emission-factor regimes for direct nitrous oxide from
#' nitrogen applied to soil. `"aguilera"` (`n_fun.r:906-912`) is the full
#' Spain_Hist disaggregated method, needing the finer `irrig_type`/
#' `fert_type` granularity: `n2o_direct_n_t = n_input_t * ef * mf`, `ef`
#' from [n2o_efs_disaggregated] on `(irrig_type, climate)`, `mf` from
#' [fertiliser_n2o_modifiers] on `(fert_type, climate)`.
#' `"ipcc2019"` (the default) is a coarser global fallback needing only
#' `climate`, reusing the SAME [n2o_efs_disaggregated] table's two
#' climate-level rows (`irrig_type == "Tier_1"` for ATL, `irrig_type ==
#' "Med_average"` for MED) with no `mf` multiplier; the ATL value (0.010)
#' is the same value documented as `EF1` in
#' [build_crop_soil_n2o_extension()], pulled from one shared source of
#' truth rather than hardcoded a second time. `"ipcc2006"` uses the
#' [n2o_efs_ipcc2006] table (IPCC 2006 Tier 1 defaults, flat 0.010 except
#' flooded rice 0.003), keyed like `"aguilera"` on `(irrig_type, climate)`
#' with no `mf` multiplier.
#'
#' @param x A tibble with `n_input_t` and `climate`. `method = "aguilera"`
#'   or `"ipcc2006"` additionally require `irrig_type` and (aguilera only)
#'   `fert_type`.
#' @param method `"ipcc2019"` (default, climate-only), `"aguilera"`
#'   (Cayuela-disaggregated, needs `irrig_type`/`fert_type`) or `"ipcc2006"`
#'   (IPCC 2006 Tier 1, needs `irrig_type`).
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
#' Drainage and soil organic matter bins are matched `s_min < s <= s_max`
#' (the source's exact boundary operator at the shared edge between
#' adjacent bins is not fully pinned down by the porting spec; this
#' half-open convention makes the bins exhaustive and non-overlapping).
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
#' use the flat IPCC EF4 factor (`ef4_nh3_to_n2o_atl`, 0.016); Mediterranean
#' rows reuse the SAME Cayuela-style `ef * mf` product
#' [calculate_soil_n2o()]'s `method = "aguilera"` computes for direct N2O,
#' rather than re-deriving that join a second time.
#'
#' @param x A tibble with `nh3_n_t`, `climate` and (for MED rows) the
#'   `irrig_type`/`fert_type` columns [calculate_soil_n2o()]'s
#'   `method = "aguilera"` needs.
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
  ef_mf <- .soil_n2o_ef_mf_aguilera(x)
  ef4_atl <- .n_constant("ef4_nh3_to_n2o_atl")
  x |>
    dplyr::mutate(
      n2o_indirect_nh3_n_t = dplyr::if_else(
        .data$climate == "ATL",
        .data$nh3_n_t * ef4_atl,
        .data$nh3_n_t * ef_mf
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
        .data$fert_type == "Synthetic"             , .data$n_input_t * frac_synth ,
        .data$fert_type %in% c("Recycling", "SOM") ,                            0 ,
        default = .data$n_input_t * frac_org
      ),
      method_nh3 = "ipcc"
    )
}

# MANNER dispatch: requires manner_fertiliser plus every driver column its
# fertiliser path needs, aborting with the exact missing column rather than
# guessing a mapping or falling back silently.
.nh3_manner <- function(x) {
  .nh3_manner_require_columns(x)
  ef <- purrr::pmap_dbl(x, .nh3_manner_row_ef)
  x |>
    dplyr::mutate(
      nh3_n_t = ef * .data$n_input_t,
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
.nh3_manner_row_ef <- function(...) {
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
  )$ef
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
  ef <- x |>
    dplyr::select("irrig_type", "climate") |>
    dplyr::left_join(
      whep::n2o_efs_disaggregated,
      by = c("irrig_type", "climate")
    ) |>
    dplyr::pull("ef")
  .soil_n2o_check_ef(x, ef)
  mf <- x |>
    dplyr::select("fert_type", "climate") |>
    dplyr::left_join(
      whep::fertiliser_n2o_modifiers,
      by = c("fert_type", "climate")
    ) |>
    dplyr::pull("mf")
  ef * mf
}

# The source table's ATL non-Tier_1/Flooded rows carry a real data gap (NA
# ef), not a value calculate_soil_n2o() should silently propagate.
.soil_n2o_check_ef <- function(x, ef) {
  bad <- is.na(ef) & !(x$climate == "MED")
  if (any(bad)) {
    cli::cli_abort(c(
      "Unsupported {.field irrig_type} for an Atlantic row.",
      i = paste0(
        "n2o_efs_disaggregated has no direct N2O factor for Atlantic ",
        "irrigation strata other than {.val Tier_1} and {.val Flooded}."
      )
    ))
  }
}

# ipcc2019: reuse n2o_efs_disaggregated's two climate-level rows (Tier_1 for
# ATL, Med_average for MED) as the single source of truth for the flat
# 0.010/0.005 factors -- the same 0.010 documented as EF1 in
# build_crop_soil_n2o_extension(), not re-hardcoded here.
.soil_n2o_ipcc2019 <- function(x) {
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
  x |>
    dplyr::mutate(
      n2o_direct_n_t = .data$n_input_t * ef,
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
      no3_n_t = no3_n_t,
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
  climate_cat <- dplyr::if_else(x$climate == "MED", "Semiarid", "Humid")
  tibble::tibble(
    fert_cat = fert_cat,
    tillage = tillage_join,
    som_content = som_content,
    climate_cat = climate_cat,
    drainage_rate = drainage_rate
  ) |>
    dplyr::left_join(
      whep::meisinger_denitrification,
      by = c(
        "fert_cat",
        "tillage",
        "som_content",
        "climate_cat",
        "drainage_rate"
      )
    ) |>
    dplyr::pull("denit_share") |>
    .leaching_check_na(
      "meisinger_denitrification",
      "fert_cat/tillage/som_content/climate_cat/drainage_rate"
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

# Bin a numeric vector into a labelled class via a half-open s_min < v <=
# s_max lookup table; the top class's finite s_max is treated as an open
# ceiling (any value above its s_min matches), per the tables' own
# open-topped design (see whep::som_ranges' documentation).
.bin_range <- function(values, ranges, label_col, min_col, max_col) {
  top <- dplyr::slice_max(ranges, .data[[max_col]], n = 1)
  purrr::map_chr(values, function(v) {
    hit <- ranges[
      v > ranges[[min_col]] & v <= ranges[[max_col]],
      ,
      drop = FALSE
    ]
    if (nrow(hit) == 0 && v > top[[min_col]]) {
      hit <- top
    }
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
