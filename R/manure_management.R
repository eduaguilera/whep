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
