# Classify source products without converting their mass (whep#1616/#1617).
# FAO Yearbook 2021 (2024), A.8, printed p.204 gives the species groups:
# https://doi.org/10.4060/cc9523en . Product forms are the ISSCFC codes in
# FI_Trade_2025.1.0, ISSCFC.pdf (Rev.1,2024) and its commodity codelist:
# https://www.fao.org/fishery/static/Data/FI_Trade_2025.1.0.zip .
# A cod liver oil retains ISSCAAP32, so a species-only join is insufficient.
# Meals keep their species target AND a separate form: FAOSTAT standardizes
# meals into species aggregates, whereas FIAS uses a different treatment.
# This is classification of net product weight, never a live-weight conversion.
# https://sws-methodology.github.io/faoswsFisheriesSUAFBSdocumentation/
# Mammals, nonfood materials, offal meals and ambiguous oils remain visible
# but held. A.8 includes coral/sponges whereas the food-scope prose excludes
# miscellaneous products; neither is treated as fish oil or silently dropped.
.fishstat_classify <- function(commodities, bridge) {
  .check_fishstat_class_keys(commodities, bridge)
  groups <- tibble::as_tibble(bridge) |>
    ensure_columns(tibble::tibble(classification_status = character())) |>
    dplyr::transmute(
      isscaap_group = as.integer(.data$isscaap_group),
      species_item_cbs_code = as.integer(.data$item_cbs_code),
      classification_status = dplyr::coalesce(
        .data$classification_status,
        "classified"
      )
    )
  rows <- tibble::as_tibble(commodities) |>
    dplyr::transmute(
      fao_code = .data$Code,
      isscaap_group = as.integer(dplyr::na_if(as.character(.data$ISSCAAP), "")),
      product_form = .fishstat_product_form(.data$fao_code)
    ) |>
    dplyr::left_join(groups, by = "isscaap_group", relationship = "many-to-one")
  .fishstat_class_targets(rows)
}

.check_fishstat_class_keys <- function(commodities, bridge) {
  if (
    !all(rlang::has_name(commodities, c("Code", "ISSCAAP"))) ||
      !all(rlang::has_name(bridge, c("isscaap_group", "item_cbs_code")))
  ) {
    cli::cli_abort(
      "FishStat classification needs commodity and species code columns.",
      class = "whep_fishstat_classification"
    )
  }
  if (
    rlang::has_name(bridge, "classification_status") &&
      any(
        is.na(bridge$classification_status) |
          !bridge$classification_status %in% c("classified", "held_scope")
      )
  ) {
    cli::cli_abort(
      "FishStat bridge has a missing or unknown classification status.",
      class = "whep_fishstat_classification"
    )
  }
  codes <- commodities$Code
  groups <- as.character(commodities$ISSCAAP)
  if (
    !is.character(codes) ||
      anyNA(codes) ||
      anyDuplicated(codes) ||
      any(!stringr::str_detect(codes, "^[0-9]{2,3}(\\.[0-9]+)*$")) ||
      any(
        !is.na(groups) & groups != "" & !stringr::str_detect(groups, "^[0-9]+$")
      ) ||
      anyNA(bridge$isscaap_group) ||
      anyDuplicated(bridge$isscaap_group) ||
      any(
        !stringr::str_detect(
          as.character(bridge$isscaap_group),
          "^[0-9]+$"
        )
      )
  ) {
    cli::cli_abort(
      "FishStat classification requires unique, nonmissing commodity keys.",
      class = "whep_fishstat_classification"
    )
  }
  invisible(NULL)
}

.fishstat_product_form <- function(code) {
  dplyr::case_when(
    code == "081.42.1.5.9.99" ~ "offal_meal",
    stringr::str_detect(code, "^411\\.11(\\.|$)") ~ "liver_oil",
    stringr::str_detect(code, "^411\\.12(\\.|$)") ~ "body_oil",
    stringr::str_detect(code, "^411(\\.|$)") ~ "oil_other",
    stringr::str_detect(code, "^081\\.42(\\.|$)") ~ "meal",
    stringr::str_detect(code, "^081\\.99(\\.|$)") ~ "feed_other",
    stringr::str_detect(code, "^291(\\.|$)") ~ "nonfood_material",
    stringr::str_detect(code, "^292(\\.|$)") ~ "plant_product",
    stringr::str_detect(code, "^03[4-7](\\.|$)") ~ "aquatic_product",
    .default = "unmapped_form"
  )
}

.fishstat_class_targets <- function(rows) {
  rows |>
    dplyr::mutate(
      classification_status = dplyr::case_when(
        .data$classification_status == "held_scope" ~ "held_scope",
        is.na(.data$species_item_cbs_code) ~ "unmapped_group",
        .data$product_form == "unmapped_form" ~ "unmapped_form",
        .data$product_form %in%
          c(
            "offal_meal",
            "feed_other",
            "nonfood_material",
            "oil_other"
          ) ~ "held_scope",
        # This code explicitly denotes algae unfit for human consumption.
        .data$fao_code == "292.9.1.79" ~ "held_scope",
        .default = "classified"
      ),
      item_cbs_code = dplyr::case_when(
        .data$classification_status != "classified" ~ NA_integer_,
        .data$product_form == "liver_oil" ~ 2782L,
        .data$product_form == "body_oil" ~ 2781L,
        .default = .data$species_item_cbs_code
      )
    )
}

# Preserve the source observations before the territorial resolution stage.
# Reexports and other measures are separate source accounts, not conversions.
.fishstat_candidate_items <- function(trade, commodities, bridge) {
  required <- c(
    "TRADE_FLOW.ALPHA_CODE",
    "COUNTRY_REPORTER.UN_CODE",
    "COMMODITY.FAO_CODE",
    "MEASURE",
    "PERIOD",
    "STATUS",
    "VALUE"
  )
  if (!all(rlang::has_name(trade, required))) {
    cli::cli_abort(
      "FishStat raw candidate requires source codes, measure, flag and value.",
      class = "whep_fishstat_source_schema"
    )
  }
  groups <- .fishstat_classify(commodities, bridge)
  tibble::as_tibble(trade) |>
    dplyr::filter(
      .data$MEASURE == "Q_tpw",
      .data$TRADE_FLOW.ALPHA_CODE %in% c("I", "E")
    ) |>
    dplyr::transmute(
      reporter_code = sprintf(
        "%03d",
        as.integer(.data$COUNTRY_REPORTER.UN_CODE)
      ),
      fao_code = .data$COMMODITY.FAO_CODE,
      year = as.integer(.data$PERIOD),
      flow = .data$TRADE_FLOW.ALPHA_CODE,
      value = .data$VALUE,
      source_measure = .data$MEASURE,
      source_flag = .data$STATUS
    ) |>
    dplyr::left_join(groups, by = "fao_code", relationship = "many-to-one") |>
    dplyr::mutate(
      classification_status = dplyr::coalesce(
        .data$classification_status,
        "unmapped_commodity"
      )
    )
}
