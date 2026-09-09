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
#'   `territory` is a stringified `area_code` (`as.character(area_code)`, what
#'   [redistribute_feed()] emits and what the whole manure chain carries
#'   through to the nitrogen inputs); an `iso3c` literal is still resolved
#'   there but is deprecated, since it can only answer with an aggregation
#'   bucket and so loses the territory for 62 of the 257 codes it knows.
#' @param options A named list of method options:
#'   * `method`: `"intake_minus_retention"` (default,
#'     `n_intake * (1 - n_retention_frac)`) or `"intake_minus_product_n"`
#'     (`n_intake - product_n`).
#'   * `method_vs`: `"intake_digestibility"` (default,
#'     `intake_dm_t * (1 - digestibility) * (1 - ash)`).
#'   * `method_c`: `"volatile_solids"` (default,
#'     `vs_excretion * carbon_per_volatile_solids`) or `"excreta_cn"`
#'     (`n_excretion *` the `bio_coefs` `Excreta` carbon-to-nitrogen ratio,
#'     the behaviour before whep#1006). See the carbon section below.
#'   * `carbon_per_volatile_solids`: kilograms of carbon per kilogram of
#'     volatile solids for `"volatile_solids"`. Default `0.47`.
#'   * `product_n`: a tibble (`year`, `territory`, `sub_territory`,
#'     `livestock_category`, `product_n`) required by `"intake_minus_product_n"`.
#'
#' @section Excreted carbon:
#' Carbon is taken from the volatile solids, so it inherits the same intake
#' and digestibility mass balance as the feed it came from. The default
#' `0.47` kg C per kg volatile solids is the carbon content of microbial
#' organic matter in Dijkstra et al. (2018), Front. Sustain. Food Syst. 2:63,
#' Table 1 (fibre 0.44, microbial organic matter 0.47, starch 0.45, protein
#' 0.52, lipid 0.75 g C/g DM), and their Table 5 puts dairy faeces at
#' 0.46 g C per g faecal organic matter (13.4 g C/g N over 154 g N and 4469 g
#' organic matter per day). Measured manures bracket it: 0.52 for fresh bedded
#' dairy manure (Choi et al. 2022, PeerJ 10:e14134, Table 1, 43.3% C and 83.3%
#' volatile solids of dry matter) and 0.39-0.46 for stored cattle and pig
#' manure (Baek et al. 2020, Int. J. Environ. Res. Public Health 17:4737,
#' Table 1, carbon on a volatile-solids basis). IPCC 2019 Vol.4 Ch.10 gives no
#' carbon fraction of volatile solids; none is cited to it here.
#'
#' `"excreta_cn"` reproduces the retired route. Its `bio_coefs` `Excreta`
#' carbon-to-nitrogen ratio is 19.065 for cattle, which is a faeces
#' composition (2.313% N of dry matter) applied to whole-excreta nitrogen, of
#' which roughly 60% is urinary and carries almost no carbon (urine
#' carbon-to-nitrogen 0.9 in Dijkstra et al. 2018, Table 5). Against ASAE
#' D384.1 FEB03 Table 1 as-voided faeces plus urine, whole-excreta
#' carbon-to-nitrogen at 0.47 kg C per kg volatile solids is 10.4 for dairy
#' and 10.0 for beef, so that route runs cattle about 1.9 times high; it is
#' kept selectable for comparison only. Urine carbon is not in the volatile
#' solids and is deliberately not added: at carbon-to-nitrogen 0.9 it is under
#' a tenth of a dairy cow's excreted carbon and is respired within days of
#' deposition.
#'
#' @return A tibble with one row per
#'   `year x territory x sub_territory x livestock_category` and columns
#'   `n_intake`, `n_excretion`, `c_excretion`, `vs_excretion`,
#'   `method_n_excretion`, `method_vs` and `method_c_excretion`.
#' @export
#' @examples
#' intake <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~livestock_category,
#'   ~item_cbs_code, ~feed_quality, ~intake_dm_t,
#'   2020L, "203", NA, "Cattle_milk", 2513L, "high_quality", 100,
#'   2020L, "203", NA, "Cattle_milk", NA, "grass", 500
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
    .calc_excretion_c(opt) |>
    dplyr::mutate(
      method_n_excretion = opt$method,
      method_vs = opt$method_vs,
      method_c_excretion = opt$method_c
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
      "method_vs",
      "method_c_excretion"
    )
}

# Private helpers ----

.excretion_options <- function(options) {
  opt <- utils::modifyList(
    list(
      method = "intake_minus_retention",
      method_vs = "intake_digestibility",
      method_c = "volatile_solids",
      carbon_per_volatile_solids = .carbon_per_volatile_solids(),
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
  method_c <- opt$method_c
  opt$method_c <- rlang::arg_match(
    method_c,
    c("volatile_solids", "excreta_cn")
  )
  .check_carbon_per_vs(opt$carbon_per_volatile_solids)
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
    0.68,
    "scavenging",
    0.55
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

# Carbon content of excreted organic matter, kg C per kg volatile solids.
# 0.47 is the carbon content of microbial organic matter in Dijkstra et al.
# (2018), Front. Sustain. Food Syst. 2:63, doi:10.3389/fsufs.2018.00063,
# Table 1; their Table 5 gives dairy faeces 0.46 g C per g faecal organic
# matter. Measured manures bracket it, 0.52 fresh bedded dairy (Choi et al.
# 2022, PeerJ 10:e14134, doi:10.7717/peerj.14134, Table 1) to 0.39-0.46 stored
# (Baek et al. 2020, IJERPH 17:4737, doi:10.3390/ijerph17134737, Table 1).
.carbon_per_volatile_solids <- function() {
  0.47
}

.check_carbon_per_vs <- function(value) {
  ok <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    value > 0 &&
    value <= 1
  if (!ok) {
    cli::cli_abort(
      "{.arg carbon_per_volatile_solids} must be one number in (0, 1]."
    )
  }
  invisible(NULL)
}

# Excreted carbon. The volatile-solids route keeps carbon on the same intake
# and digestibility mass balance as the nitrogen and the volatile solids, so
# the two organic-matter estimates cannot disagree. The Excreta C:N route it
# replaced could, and did per species: measured on the real 2020 national
# chain it put excreted carbon at 0.547 kg C per kg volatile solids for beef
# cattle (0.504-0.568 across all four demand-tier x feed-mode runs), 0.521
# for dairy and 0.646 for the All_species bucket, against a highest measured
# manure value of 0.52, while leaving pigs at 0.289 and poultry at 0.328. The
# global mean, 0.483, hid all of it (whep#1006).
.calc_excretion_c <- function(out, opt) {
  if (identical(opt$method_c, "volatile_solids")) {
    return(dplyr::mutate(
      out,
      c_excretion = .data$vs_excretion * opt$carbon_per_volatile_solids
    ))
  }
  cn <- .manure_cn_coefs() |>
    dplyr::filter(.data$manure_type == "Excreta") |>
    dplyr::select("cn_species" = "species", "cn_ratio")
  out <- dplyr::left_join(out, cn, by = "cn_species")
  if (anyNA(out$cn_ratio)) {
    cli::cli_abort("Missing Excreta C:N for some species.")
  }
  dplyr::mutate(out, c_excretion = .data$n_excretion * .data$cn_ratio)
}
