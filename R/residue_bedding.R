#' Build the bedding straw supply reaching the managed manure chain.
#'
#' @description
#' Converts the bedding destiny of crop residues
#' ([calculate_residue_destinies()]'s `residue_bedding_dm_t`) into the dry
#' matter, carbon and nitrogen that [add_manure_bedding()] adds to the housed
#' manure streams. Composition comes from the same `bio_coefs` residue
#' coefficients [calculate_npp_carbon_nitrogen()] uses for the residue that
#' stays on the field (`residue_c_kgdm`, `residue_n_kgdm`, joined on
#' `item_prod_code`), so the straw that goes through the yard and the straw
#' that does not are described by one table.
#'
#' The supply is aggregated over crops, because bedding is not traceable to the
#' crop it came from once it is in the heap: the manure chain keys on
#' `year x territory` (and `sub_territory` when the input carries one).
#'
#' @param x A tibble from [calculate_residue_destinies()] with
#'   `item_prod_code`, `residue_bedding_dm_t`, `year` and `territory`, and
#'   optionally `sub_territory`.
#' @return A tibble with one row per `year x territory` (x `sub_territory`) and
#'   columns `bedding_dm_t`, `bedding_c_t` and `bedding_n_t`.
#' @export
#' @examples
#' tibble::tibble(
#'   year = 2020L, territory = "203", item_prod_code = "15",
#'   residue_bedding_dm_t = 1000
#' ) |>
#'   build_residue_bedding_supply()
build_residue_bedding_supply <- function(x) {
  req <- c("item_prod_code", "residue_bedding_dm_t", "year", "territory")
  miss <- req[!purrr::map_lgl(req, ~ rlang::has_name(x, .x))]
  if (length(miss) > 0) {
    cli::cli_abort("{.arg x} is missing column{?s}: {.val {miss}}.")
  }
  # A bedding supply built from an identically-zero bedding column is
  # indistinguishable downstream from a build with no bedding at all, and it is
  # exactly what a caller gets who forgot to set `bedding_fraction`. So the
  # absence is refused here, where it is still visible, rather than travelling
  # on as a zero (see "Absent inputs must not become zeros" in CLAUDE.md).
  check_inputs_supplied(
    x,
    "residue_bedding_dm_t",
    details = c(
      i = "Set {.arg bedding_fraction} in {.fun calculate_residue_destinies};
           it defaults to zero because no bedding share of crop residue could
           be sourced (whep#1005)."
    )
  )
  keys <- intersect(c("year", "territory", "sub_territory"), names(x))
  x |>
    .bedding_attach_composition() |>
    dplyr::summarise(
      bedding_dm_t = sum(.data$residue_bedding_dm_t, na.rm = TRUE),
      bedding_c_t = sum(
        .data$residue_bedding_dm_t * .data$residue_c_kgdm,
        na.rm = TRUE
      ),
      bedding_n_t = sum(
        .data$residue_bedding_dm_t * .data$residue_n_kgdm,
        na.rm = TRUE
      ),
      .by = dplyr::all_of(keys)
    )
}

#' Add bedding carbon and nitrogen to the housed manure streams.
#'
#' @description
#' Places the bedding supply from [build_residue_bedding_supply()] onto the
#' litter-using, non-grazing streams of a [split_manure_management()] result, in
#' proportion to the nitrogen each of those streams already carries. Grazing
#' deposition (`Pasture/Range/Paddock`) never receives bedding, which is what
#' keeps its manure the fresh excreta it is.
#'
#' The result is the input rows plus `dm_bedding`, `n_bedding` and `c_bedding`,
#' which [apply_management_losses()] then adds to the manure applied to land.
#'
#' @section Which systems use litter:
#' IPCC 2019 Refinement Vol. 4 Ch. 10 p. 10.94 defines `NbeddingMS` as the
#' bedding nitrogen "to be applied for solid storage and deep bedding MMS if
#' known organic bedding usage". WHEP's `"Poultry Manure"` is the deep-litter
#' system (it takes its EF3 from the IPCC `"Poultry Manure - Deep Litter"`
#' row), so the IPCC pair is `"Solid Storage"` and `"Poultry Manure"` and that
#' is the default. `"with_daily_spread"` additionally beds `"Daily Spread"`,
#' the manure scraped from a barn and spread the same day, which is bedded in
#' practice but which IPCC does not name; it is selectable rather than default
#' for that reason. Liquid and lagoon systems are excluded under both, since
#' straw cannot be pumped.
#'
#' Moving bedding onto `"Daily Spread"` as well spreads the same mass over more
#' nitrogen, so it lowers the bedding placed per tonne of solid-storage manure
#' and raises the applied C:N of the daily-spread stream instead; it moves no
#' total.
#'
#' @param split A tibble from [split_manure_management()].
#' @param bedding A tibble from [build_residue_bedding_supply()], keyed on the
#'   `year x territory` (x `sub_territory`) columns it shares with `split`.
#' @param options A named list. `mms_bedding` selects the litter-using systems:
#'   `"ipcc_2019"` (default) or `"with_daily_spread"`; see the section above.
#' @return The `split` rows with `dm_bedding`, `n_bedding`, `c_bedding` and
#'   `method_bedding_mms`.
#' @export
#' @examples
#' excretion <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~livestock_category,
#'   ~n_excretion, ~c_excretion, ~vs_excretion,
#'   2020L, "203", NA, "Cattle_milk", 100, 1000, 210
#' )
#' bedding <- tibble::tibble(
#'   year = 2020L, territory = "203",
#'   bedding_dm_t = 50, bedding_c_t = 22, bedding_n_t = 0.3
#' )
#' add_manure_bedding(split_manure_management(excretion), bedding)
add_manure_bedding <- function(split, bedding, options = list()) {
  opt <- utils::modifyList(list(mms_bedding = "ipcc_2019"), options)
  mms_bedding <- opt$mms_bedding
  opt$mms_bedding <- rlang::arg_match(
    mms_bedding,
    c("ipcc_2019", "with_daily_spread")
  )
  .check_bedding_cols(split, bedding)
  keys <- intersect(names(bedding), c("year", "territory", "sub_territory"))
  litter <- .bedding_litter_mms(opt$mms_bedding)

  weighted <- split |>
    tibble::as_tibble() |>
    dplyr::mutate(
      .bedding_weight = dplyr::if_else(
        .data$mms_type %in% litter,
        .data$n_stream,
        0
      )
    ) |>
    dplyr::mutate(
      .bedding_total = sum(.data$.bedding_weight),
      .by = dplyr::all_of(keys)
    )
  .warn_stranded_bedding(weighted, bedding, keys)

  weighted |>
    dplyr::left_join(bedding, by = keys) |>
    dplyr::mutate(
      .bedding_share = dplyr::if_else(
        .data$.bedding_total > 0,
        .data$.bedding_weight / .data$.bedding_total,
        0
      ),
      dm_bedding = .data$.bedding_share *
        tidyr::replace_na(.data$bedding_dm_t, 0),
      c_bedding = .data$.bedding_share *
        tidyr::replace_na(.data$bedding_c_t, 0),
      n_bedding = .data$.bedding_share *
        tidyr::replace_na(.data$bedding_n_t, 0),
      method_bedding_mms = opt$mms_bedding
    ) |>
    dplyr::select(
      -dplyr::any_of(c("bedding_dm_t", "bedding_c_t", "bedding_n_t")),
      -".bedding_weight",
      -".bedding_total",
      -".bedding_share"
    )
}

# Private helpers ----

.bedding_litter_mms <- function(mms_bedding) {
  ipcc <- c("Solid Storage", "Poultry Manure")
  switch(
    mms_bedding,
    ipcc_2019 = ipcc,
    with_daily_spread = c(ipcc, "Daily Spread")
  )
}

# The bedding composition coefficients, from the same bio_coefs residue columns
# the on-field residue uses. A crop the table does not cover has no known straw
# carbon or nitrogen; booking it as zero would understate the supply silently,
# so it is dropped WITH its dry matter named, the way unrecovered residue is
# reported in build_cbs.R.
.bedding_attach_composition <- function(x) {
  coefs <- whep::whep_coef_table("bio_coefs") |>
    dplyr::transmute(
      item_prod_code = as.character(.data$item_prod_code),
      residue_c_kgdm = .data$residue_c_kgdm,
      residue_n_kgdm = .data$residue_n_kgdm
    ) |>
    dplyr::filter(!is.na(.data$residue_c_kgdm), !is.na(.data$residue_n_kgdm))
  joined <- x |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code)) |>
    dplyr::left_join(coefs, by = "item_prod_code")
  .warn_bedding_no_composition(joined)
  dplyr::filter(joined, !is.na(.data$residue_c_kgdm))
}

.warn_bedding_no_composition <- function(joined) {
  dropped <- is.na(joined$residue_c_kgdm) &
    !is.na(joined$residue_bedding_dm_t) &
    joined$residue_bedding_dm_t > 0
  if (!any(dropped)) {
    return(invisible(NULL))
  }
  codes <- sort(unique(as.character(joined$item_prod_code[dropped])))
  mass <- sum(joined$residue_bedding_dm_t[dropped])
  cli::cli_warn(c(
    "No {.field bio_coefs} residue composition for production
     item{?s} {.val {codes}}.",
    i = "Bedding dry matter dropped: {.val {mass}} t, rather than booked with
         an invented carbon and nitrogen content."
  ))
  invisible(NULL)
}

# Bedding whose territory has no litter-using stream at all cannot be placed.
# That is mass leaving the model, so it is said out loud instead of quietly
# evaporating in the join.
.warn_stranded_bedding <- function(weighted, bedding, keys) {
  placeable <- weighted |>
    dplyr::filter(.data$.bedding_total > 0) |>
    dplyr::select(dplyr::all_of(keys)) |>
    dplyr::distinct()
  stranded <- dplyr::anti_join(bedding, placeable, by = keys)
  if (nrow(stranded) == 0) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "Bedding that reaches no litter-using manure stream is dropped.",
    i = "Bedding rows dropped: {.val {nrow(stranded)}}.",
    i = "Bedding dry matter dropped: {.val {sum(stranded$bedding_dm_t)}} t.",
    i = "Those keys carry no housed excretion in a litter-using system."
  ))
  invisible(NULL)
}

.check_bedding_cols <- function(split, bedding) {
  .check_split_cols(split)
  req <- c("year", "territory", "bedding_dm_t", "bedding_c_t", "bedding_n_t")
  miss <- req[!purrr::map_lgl(req, ~ rlang::has_name(bedding, .x))]
  if (length(miss) > 0) {
    cli::cli_abort("{.arg bedding} is missing column{?s}: {.val {miss}}.")
  }
  keys <- intersect(names(bedding), c("year", "territory", "sub_territory"))
  gone <- keys[!purrr::map_lgl(keys, ~ rlang::has_name(split, .x))]
  if (length(gone) > 0) {
    cli::cli_abort("{.arg split} is missing key column{?s}: {.val {gone}}.")
  }
  invisible(NULL)
}
