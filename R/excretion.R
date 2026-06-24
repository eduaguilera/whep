#' Estimate livestock nitrogen, carbon and volatile-solids excretion.
#'
#' @description
#' Converts realised feed intake (the output of [redistribute_feed()]) into
#' excreted nitrogen, carbon and volatile solids per
#' `year x territory x sub_territory x livestock_category`. All excretion methods
#' share one canonical nitrogen intake,
#' `n_intake = sum(intake_dm_t * feed_n_content)`, so the methods are directly
#' comparable.
#'
#' @param intake A tibble of realised feed intake with at least `year`,
#'   `territory`, `sub_territory`, `livestock_category`, `item_cbs_code`,
#'   `feed_quality` and `intake_dm_t` (the [redistribute_feed()] result).
#' @param options A named list of method options:
#'   * `method`: `"intake_minus_retention"` (default,
#'     `n_intake * (1 - n_retention_frac)`) or `"intake_minus_product_n"`
#'     (`n_intake - product_n`).
#'   * `method_vs`: `"intake_digestibility"` (default,
#'     `intake_dm_t * (1 - digestibility) * (1 - ash)`).
#'   * `product_n`: a tibble (`year`, `territory`, `sub_territory`,
#'     `livestock_category`, `product_n`) required by `"intake_minus_product_n"`.
#'
#' @return A tibble with one row per
#'   `year x territory x sub_territory x livestock_category` and columns
#'   `n_intake`, `n_excretion`, `c_excretion`, `vs_excretion`,
#'   `method_n_excretion` and `method_vs`.
#' @export
#' @examples
#' intake <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~livestock_category,
#'   ~item_cbs_code, ~feed_quality, ~intake_dm_t,
#'   2020L, "ES", NA, "Cattle_milk", 2513L, "high_quality", 100,
#'   2020L, "ES", NA, "Cattle_milk", NA, "grass", 500
#' )
#' estimate_n_excretion(intake)
estimate_n_excretion <- function(intake, options = list()) {
  opt <- .excretion_options(options)
  .check_intake_cols(intake)

  rows <- intake |>
    .join_excretion_bridge(.species_taxonomy_bridge()) |>
    .attach_feed_n() |>
    .attach_vs_components()

  rows |>
    dplyr::summarise(
      n_intake = sum(.data$intake_dm_t * .data$feed_n_content, na.rm = TRUE),
      vs_excretion = sum(.data$vs_dm, na.rm = TRUE),
      .by = c(
        "year",
        "territory",
        "sub_territory",
        "livestock_category",
        "bo_category",
        "cn_species"
      )
    ) |>
    .calc_excretion_n(opt) |>
    .calc_excretion_c() |>
    dplyr::mutate(
      method_n_excretion = opt$method,
      method_vs = opt$method_vs
    ) |>
    dplyr::select(
      "year",
      "territory",
      "sub_territory",
      "livestock_category",
      "n_intake",
      "n_excretion",
      "c_excretion",
      "vs_excretion",
      "method_n_excretion",
      "method_vs"
    )
}

# Private helpers ----

.excretion_options <- function(options) {
  opt <- utils::modifyList(
    list(
      method = "intake_minus_retention",
      method_vs = "intake_digestibility",
      product_n = NULL
    ),
    options
  )
  methods <- c("intake_minus_retention", "intake_minus_product_n")
  if (!opt$method %in% methods) {
    cli::cli_abort(
      "Unknown {.arg method} {.val {opt$method}}. Use one of {.val {methods}}."
    )
  }
  if (!opt$method_vs %in% "intake_digestibility") {
    cli::cli_abort("Unknown {.arg method_vs} {.val {opt$method_vs}}.")
  }
  if (opt$method == "intake_minus_product_n" && is.null(opt$product_n)) {
    cli::cli_abort(
      "{.val intake_minus_product_n} needs {.arg product_n} in {.arg options}."
    )
  }
  opt
}

.check_intake_cols <- function(intake) {
  req <- c(
    "year",
    "territory",
    "sub_territory",
    "livestock_category",
    "item_cbs_code",
    "feed_quality",
    "intake_dm_t"
  )
  miss <- req[!purrr::map_lgl(req, ~ rlang::has_name(intake, .x))]
  if (length(miss) > 0) {
    cli::cli_abort("{.arg intake} is missing column{?s}: {.val {miss}}.")
  }
  invisible(NULL)
}

.join_excretion_bridge <- function(intake, bridge) {
  out <- dplyr::left_join(
    tibble::as_tibble(intake),
    dplyr::select(
      bridge,
      "livestock_category",
      "species_gen",
      "bo_category",
      "cn_species"
    ),
    by = "livestock_category"
  )
  unmatched <- unique(out$livestock_category[is.na(out$bo_category)])
  if (length(unmatched) > 0) {
    cli::cli_abort(
      "No taxonomy bridge row for livestock_category {.val {unmatched}}."
    )
  }
  out
}

.attach_feed_n <- function(rows) {
  forage <- .forage_n_kgn_kgdm()
  rows |>
    dplyr::left_join(
      dplyr::select(
        .feed_n_content_lookup(),
        "item_cbs_code",
        "feed_n_kgn_kgdm"
      ),
      by = "item_cbs_code"
    ) |>
    dplyr::mutate(
      feed_n_content = dplyr::if_else(
        is.na(.data$item_cbs_code),
        forage,
        dplyr::coalesce(.data$feed_n_kgn_kgdm, forage)
      )
    )
}

# Volatile solids on the intake path: VS dry matter = intake DM that is neither
# digested (1 - digestibility) nor mineral ash. Digestibility is a CALIBRATE
# per-feed-quality scaffold anchored on GLEAM (grass ~58-67, straw ~45-46);
# ash is the IPCC Tier-2 per-species value.
.attach_vs_components <- function(rows) {
  out <- rows |>
    dplyr::left_join(.feed_quality_digestibility(), by = "feed_quality") |>
    dplyr::left_join(
      dplyr::select(
        whep::ipcc_tier2_manure_ash,
        "species_gen" = "category",
        "ash_percent"
      ),
      by = "species_gen"
    )
  if (anyNA(out$digestibility)) {
    bad <- unique(out$feed_quality[is.na(out$digestibility)])
    cli::cli_abort("No digestibility for feed_quality {.val {bad}}.")
  }
  if (anyNA(out$ash_percent)) {
    cli::cli_abort("No ash content for some species.")
  }
  dplyr::mutate(
    out,
    vs_dm = .data$intake_dm_t *
      (1 - .data$digestibility) *
      (1 - .data$ash_percent / 100)
  )
}

.feed_quality_digestibility <- function() {
  tibble::tribble(
    ~feed_quality,
    ~digestibility,
    "lactation",
    0.72,
    "high_quality",
    0.72,
    "low_quality",
    0.62,
    "residues",
    0.47,
    "grass",
    0.62,
    "substitute",
    0.62,
    "zoot_fixed",
    0.68
  )
}

.calc_excretion_n <- function(agg, opt) {
  out <- dplyr::left_join(
    agg,
    dplyr::select(
      whep::ipcc_tier2_n_retention,
      "bo_category" = "category",
      "n_retention_frac"
    ),
    by = "bo_category"
  )
  if (anyNA(out$n_retention_frac)) {
    cli::cli_abort("Missing N retention fraction for some bo_category.")
  }
  if (opt$method == "intake_minus_retention") {
    return(dplyr::mutate(
      out,
      n_excretion = .data$n_intake * (1 - .data$n_retention_frac)
    ))
  }
  out |>
    dplyr::left_join(
      tibble::as_tibble(opt$product_n),
      by = c("year", "territory", "sub_territory", "livestock_category")
    ) |>
    dplyr::mutate(
      n_excretion = pmax(
        0,
        .data$n_intake - dplyr::coalesce(.data$product_n, 0)
      )
    )
}

.calc_excretion_c <- function(out) {
  cn <- .manure_cn_coefs() |>
    dplyr::filter(.data$manure_type == "Excreta") |>
    dplyr::select("cn_species" = "species", "cn_ratio")
  out <- dplyr::left_join(out, cn, by = "cn_species")
  if (anyNA(out$cn_ratio)) {
    cli::cli_abort("Missing Excreta C:N for some species.")
  }
  dplyr::mutate(out, c_excretion = .data$n_excretion * .data$cn_ratio)
}
