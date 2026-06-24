#' Split livestock excretion across manure-management systems.
#'
#' @description
#' Splits the excreted nitrogen, carbon and volatile solids from
#' [estimate_n_excretion()] across manure-management systems (MMS), separating
#' the in-situ grazing stream (pasture/range/paddock, deposited where it falls)
#' from the collected/housed streams routed to storage. The split conserves mass:
#' the per-species MMS shares sum to one.
#'
#' @param excretion A tibble from [estimate_n_excretion()] with `year`,
#'   `territory`, `sub_territory`, `livestock_category`, `n_excretion`,
#'   `c_excretion` and `vs_excretion`.
#' @param options A named list. `mms_source` selects the MMS-share table
#'   (`"regional_default"`, the global IPCC/GLEAM default in
#'   `regional_mms_distribution`).
#'
#' @return A tibble with one row per
#'   `year x territory x sub_territory x livestock_category x mms_type`, plus
#'   `species_gen`, `loss_category`, `stream` (`"grazing"` or `"collected"`),
#'   `n_stream`, `c_stream`, `vs_stream` and `method_mms`.
#' @export
#' @examples
#' excretion <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~livestock_category,
#'   ~n_excretion, ~c_excretion, ~vs_excretion,
#'   2020L, "ES", NA, "Cattle_milk", 100, 1900, 60,
#'   2020L, "ES", NA, "Pigs", 30, 270, 20
#' )
#' split_manure_management(excretion)
split_manure_management <- function(excretion, options = list()) {
  opt <- utils::modifyList(list(mms_source = "regional_default"), options)
  .check_excretion_cols(excretion)
  bridge <- dplyr::select(
    .species_taxonomy_bridge(),
    "livestock_category",
    "species_gen",
    "loss_category"
  )

  joined <- excretion |>
    tibble::as_tibble() |>
    dplyr::left_join(bridge, by = "livestock_category") |>
    dplyr::left_join(
      .mms_shares(opt$mms_source),
      by = c("species_gen" = "species"),
      relationship = "many-to-many"
    )
  if (anyNA(joined$mms_type)) {
    bad <- unique(joined$species_gen[is.na(joined$mms_type)])
    cli::cli_abort("No MMS distribution for species {.val {bad}}.")
  }

  joined |>
    dplyr::mutate(
      stream = dplyr::if_else(
        .data$mms_type == "Pasture/Range/Paddock",
        "grazing",
        "collected"
      ),
      n_stream = .data$n_excretion * .data$fraction,
      c_stream = .data$c_excretion * .data$fraction,
      vs_stream = .data$vs_excretion * .data$fraction,
      method_mms = opt$mms_source
    ) |>
    dplyr::select(
      "year",
      "territory",
      "sub_territory",
      "livestock_category",
      "species_gen",
      "loss_category",
      "mms_type",
      "stream",
      "n_stream",
      "c_stream",
      "vs_stream",
      "method_mms"
    )
}

# Private helpers ----

.check_excretion_cols <- function(excretion) {
  req <- c(
    "year",
    "territory",
    "sub_territory",
    "livestock_category",
    "n_excretion",
    "c_excretion",
    "vs_excretion"
  )
  miss <- req[!purrr::map_lgl(req, ~ rlang::has_name(excretion, .x))]
  if (length(miss) > 0) {
    cli::cli_abort("{.arg excretion} is missing column{?s}: {.val {miss}}.")
  }
  invisible(NULL)
}

.mms_shares <- function(source) {
  if (!identical(source, "regional_default")) {
    cli::cli_abort("Unknown {.arg mms_source} {.val {source}}.")
  }
  whep::regional_mms_distribution |>
    dplyr::filter(.data$region == "Global") |>
    dplyr::mutate(
      fraction = .data$fraction / sum(.data$fraction),
      .by = "species"
    ) |>
    dplyr::select("species", "mms_type", "fraction")
}

#' Apply IPCC manure-management losses to the collected manure streams.
#'
#' @description
#' Nets the nitrogen surviving manure management onto the field, applying the
#' IPCC 2019 management-loss fractions to the collected/housed streams from
#' [split_manure_management()]: `applied_n = n_stream * (1 - FracLossMS)` where
#' `FracLossMS = FracGasMS + FracLeachMS + EF3 + FracN2MS`. The grazing
#' (pasture/range/paddock) stream is deposited in situ and keeps its full
#' nitrogen (its in-situ soil losses belong to the soil stage). Indirect N2O is
#' reported as a labelled sub-flux of the already-removed volatilized and leached
#' nitrogen (the same N is not removed twice). Carbon and volatile-solids storage
#' losses are not yet applied (`applied_c`/`applied_vs` pass through).
#'
#' @param split A tibble from [split_manure_management()].
#' @param options A named list. `method` selects the loss method
#'   (`"ipcc_2019_tier2"`).
#'
#' @return The input rows with `applied_n`, `applied_c`, `applied_vs`,
#'   `n_volatilized`, `n_leached`, `n2o_direct_n`, `n2_n`, `n2o_indirect_n` and
#'   `method_losses`.
#' @export
#' @examples
#' excretion <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~livestock_category,
#'   ~n_excretion, ~c_excretion, ~vs_excretion,
#'   2020L, "ES", NA, "Cattle_milk", 100, 1900, 60
#' )
#' apply_management_losses(split_manure_management(excretion))
apply_management_losses <- function(split, options = list()) {
  opt <- utils::modifyList(list(method = "ipcc_2019_tier2"), options)
  if (!identical(opt$method, "ipcc_2019_tier2")) {
    cli::cli_abort("Unknown {.arg method} {.val {opt$method}}.")
  }
  .check_split_cols(split)

  ind <- whep::indirect_n2o_ef
  ef4 <- ind$value[ind$parameter == "ef4_volatilization"]
  ef5 <- ind$value[ind$parameter == "ef5_leaching"]
  n2_ratio <- .n2_to_n2o_ratio()

  out <- split |>
    tibble::as_tibble() |>
    dplyr::left_join(
      .manure_loss_fractions(),
      by = c("mms_type", "loss_category" = "animal_category")
    ) |>
    dplyr::left_join(.manure_ef3(), by = "mms_type")
  if (anyNA(out$frac_gas_ms) || anyNA(out$ef3)) {
    cli::cli_abort("Missing loss fraction or EF3 for some MMS.")
  }

  out |>
    dplyr::mutate(
      n_volatilized = dplyr::if_else(
        .data$stream == "grazing",
        0,
        .data$n_stream * .data$frac_gas_ms
      ),
      n_leached = dplyr::if_else(
        .data$stream == "grazing",
        0,
        .data$n_stream * .data$frac_leach_ms
      ),
      n2o_direct_n = dplyr::if_else(
        .data$stream == "grazing",
        0,
        .data$n_stream * .data$ef3
      ),
      n2_n = .data$n2o_direct_n * n2_ratio,
      n2o_indirect_n = .data$n_volatilized * ef4 + .data$n_leached * ef5,
      applied_n = dplyr::if_else(
        .data$stream == "grazing",
        .data$n_stream,
        pmax(
          0,
          .data$n_stream -
            .data$n_volatilized -
            .data$n_leached -
            .data$n2o_direct_n -
            .data$n2_n
        )
      ),
      applied_c = .data$c_stream,
      applied_vs = .data$vs_stream,
      method_losses = opt$method
    ) |>
    dplyr::select(
      "year",
      "territory",
      "sub_territory",
      "livestock_category",
      "species_gen",
      "mms_type",
      "stream",
      "applied_n",
      "applied_c",
      "applied_vs",
      "n_volatilized",
      "n_leached",
      "n2o_direct_n",
      "n2_n",
      "n2o_indirect_n",
      "method_losses"
    )
}

.check_split_cols <- function(split) {
  req <- c(
    "mms_type",
    "loss_category",
    "stream",
    "n_stream",
    "c_stream",
    "vs_stream"
  )
  miss <- req[!purrr::map_lgl(req, ~ rlang::has_name(split, .x))]
  if (length(miss) > 0) {
    cli::cli_abort("{.arg split} is missing column{?s}: {.val {miss}}.")
  }
  invisible(NULL)
}

# EF3 (direct-N2O from management) per engine MMS type, reusing
# ipcc_2019_n2o_ef_direct with a name crosswalk (its rows use finer system
# labels than the six MMS this engine carries).
.manure_ef3 <- function() {
  ef <- whep::ipcc_2019_n2o_ef_direct
  pick <- function(sys) ef$ef_kg_n2o_n_per_kg_n[ef$system == sys]
  tibble::tibble(
    mms_type = c(
      "Pasture/Range/Paddock",
      "Daily Spread",
      "Solid Storage",
      "Liquid/Slurry",
      "Anaerobic Lagoon",
      "Poultry Manure"
    ),
    ef3 = c(
      pick("Pasture/Range/Paddock"),
      pick("Daily Spread"),
      pick("Solid Storage"),
      pick("Liquid/Slurry"),
      pick("Uncovered Anaerobic Lagoon"),
      pick("Poultry Manure - Deep Litter")
    )
  )
}
