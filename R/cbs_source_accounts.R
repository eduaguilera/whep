# Retain the raw account observations (stock change, losses, residuals and
# tourist use) before item rebasing, unit conversion, sign normalisation,
# aggregation or reconciliation: the source's own code, unit, value, flag,
# pin version and dated reporting polity. These records describe a source, not
# validated physical truth: FAOSTAT flag I, for example, denotes an
# agency-imputed observation.
.cbs_input_account_records <- function(dt, pin_alias) {
  elements <- c(
    "Stock Variation",
    "Losses",
    "Residuals",
    "Tourist consumption"
  )
  records <- tibble::as_tibble(dt) |>
    dplyr::filter(element %in% elements) |>
    ensure_columns(tibble::tibble(fao_flag = character())) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      element,
      unit,
      value,
      fao_flag
    ) |>
    .add_polity_columns_dt(
      include_unmapped = TRUE,
      backcast_anchor = 1850L
    ) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      year,
      source_area_code = area_code,
      source_item_code = item_cbs_code,
      source_element = element,
      source_unit = as.character(unit),
      source_value = value,
      fao_flag,
      reporting_polity_code = polity_code,
      source_alias = pin_alias,
      source_stock_convention = dplyr::if_else(
        source_element == "Stock Variation",
        .fao_stock_convention(pin_alias),
        NA_character_
      )
    )
  versions <- whep::whep_inputs |>
    dplyr::select(source_alias = alias, source_version = version)
  records |>
    dplyr::left_join(
      versions,
      by = "source_alias",
      relationship = "many-to-one"
    ) |>
    dplyr::arrange(
      year,
      source_area_code,
      source_item_code,
      source_element,
      source_unit
    )
}

# Statistical residuals retain their source meaning. The model discrepancy
# below is a different quantity: the mismatch left after calibrated uses and
# source accounts are combined. Neither quantity represents a physical stock.
#
# `domestic_supply` stays total physical domestic use, as documented for
# `build_commodity_balances()`: the calibrated destinies plus losses and
# tourist use. FAOSTAT's own domestic supply also nets in its residual; that
# value stays in the source ledger. FABIO's balance formula also leaves
# residuals out of `domestic_use` (fineprint-global/fabio@66a9298,
# R/03_1a_build_cbs.R), though its later rebalancing (R/03_1b_balance_cbs.R)
# reallocates them into the uses; that would hide the statistical quantity,
# so it is not done here. Production plus import, less export and stock
# variation, equals domestic supply plus residuals plus the balance
# discrepancy, with an unobserved stock change (NA) counted as none in the
# identity only, as FABIO's `na_sum()` does, so its size is visible in the
# discrepancy.
.cbs_restore_accounts <- function(
  model,
  observations,
  method = c("source", "balance_residual")
) {
  method <- rlang::arg_match(method)
  if (method == "balance_residual") {
    return(dplyr::mutate(model, method_cbs_account = method))
  }
  keys <- c("year", "reporting_polity_code", "item_cbs_code")
  .cbs_check_account_identity(model, observations, keys)
  template <- model |>
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE) |>
    dplyr::select(-element, -value, -source, -dplyr::any_of("fao_flag"))
  # Only the built years are in scope; the extracts can span more.
  observed <- observations |>
    dplyr::filter(
      element %in% .cbs_account_elements(),
      year %in% unique(model$year)
    ) |>
    dplyr::select(
      dplyr::all_of(c(keys, "element", "value", "source")),
      dplyr::any_of("fao_flag")
    )
  unmatched <- dplyr::anti_join(observed, template, by = keys)
  observed <- dplyr::inner_join(
    observed,
    template,
    by = keys,
    relationship = "many-to-one"
  )
  stocks <- template |>
    dplyr::anti_join(
      dplyr::filter(observed, element == "stock_variation"),
      by = keys
    ) |>
    dplyr::mutate(
      element = "stock_variation",
      value = NA_real_,
      source = "unobserved_stock",
      fao_flag = NA_character_
    )
  restored <- model |>
    dplyr::filter(
      !element %in%
        c(.cbs_account_elements(), "domestic_supply", "balance_discrepancy")
    ) |>
    dplyr::bind_rows(observed, stocks)
  derived <- .cbs_account_totals(restored, keys) |>
    tidyr::pivot_longer(
      c(domestic_supply, balance_discrepancy),
      names_to = "element",
      values_to = "value"
    ) |>
    dplyr::left_join(template, by = keys, relationship = "many-to-one") |>
    dplyr::mutate(
      source = "WHEP_account_reconciliation",
      fao_flag = NA_character_
    )
  out <- restored |>
    dplyr::bind_rows(derived) |>
    dplyr::mutate(method_cbs_account = method)
  attr(out, "unmatched_source_accounts") <- .cbs_report_unmatched(unmatched)
  out
}

# The selected account observations keyed on the dated reporting polity. A
# build whose sources carried none (a mocked or custom input) has an empty
# ledger: every stock change is then unobserved, never a silent zero.
.cbs_account_ledger <- function(observations) {
  if (is.null(observations) || nrow(observations) == 0L) {
    return(tibble::tibble(
      year = integer(),
      reporting_polity_code = character(),
      item_cbs_code = double(),
      element = character(),
      value = double(),
      source = character(),
      fao_flag = character()
    ))
  }
  if (rlang::has_name(observations, "reporting_polity_code")) {
    return(tibble::as_tibble(observations))
  }
  tibble::as_tibble(.add_reporting_polity_columns(observations))
}

# The account rows of a supplied balance, or NULL.
.cbs_account_rows <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }
  dplyr::filter(tibble::as_tibble(data), element %in% .cbs_account_elements())
}

# The four account elements a source reports beside the calibrated uses.
.cbs_account_elements <- function() {
  c("stock_variation", .cbs_account_only_elements())
}

# Elements the calibration never sees: they are parked before source
# selection and restored by .cbs_restore_accounts().
.cbs_account_only_elements <- function() {
  c("losses", "residuals", "tourist_consumption")
}

.cbs_check_account_identity <- function(model, observations, keys) {
  required <- c(keys, "element", "value", "source")
  if (
    !all(rlang::has_name(model, required)) ||
      !all(rlang::has_name(observations, required))
  ) {
    cli::cli_abort(
      "CBS source accounts require dated polity and commodity identities.",
      class = "whep_cbs_account_identity"
    )
  }
  observed <- dplyr::filter(observations, element %in% .cbs_account_elements())
  duplicate <- observed |>
    dplyr::count(dplyr::across(dplyr::all_of(c(keys, "element")))) |>
    dplyr::filter(n > 1L)
  if (nrow(duplicate) > 0L || anyNA(observed$reporting_polity_code)) {
    cli::cli_abort(
      "Source account identities must be resolved and unique per element.",
      class = "whep_cbs_account_identity"
    )
  }
  invisible(NULL)
}

# Source observations on a commodity, polity and year the calibrated balance
# does not carry. They stay in the `source_accounts` ledger; the balance does
# not grow rows around them, so they are reported rather than restored.
.cbs_report_unmatched <- function(unmatched) {
  if (nrow(unmatched) > 0L) {
    total <- sum(abs(unmatched$value), na.rm = TRUE)
    cli::cli_inform(c(
      "i" = "{nrow(unmatched)} source account observation{?s} fall outside \
        the calibrated balance keys ({format(total, big.mark = ',')} t in \
        absolute value).",
      " " = "They stay in the {.val source_accounts} ledger and are \
        returned as the {.val unmatched_source_accounts} attribute."
    ))
  }
  unmatched
}

.cbs_account_totals <- function(restored, keys) {
  quantities <- c(
    "production",
    "import",
    "export",
    "stock_variation",
    "food",
    "feed",
    "seed",
    "processing",
    "processing_primary",
    "other_uses",
    "losses",
    "residuals",
    "tourist_consumption"
  )
  restored |>
    dplyr::select(dplyr::all_of(keys), element, value) |>
    dplyr::filter(element %in% quantities) |>
    tidyr::pivot_wider(
      names_from = element,
      values_from = value,
      values_fill = 0
    ) |>
    ensure_columns(
      tibble::as_tibble(rlang::rep_named(quantities, list(double()))),
      defaults = rlang::rep_named(quantities, list(0))
    ) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      domestic_supply = food +
        feed +
        seed +
        processing +
        processing_primary +
        other_uses +
        losses +
        tourist_consumption,
      balance_discrepancy = production +
        import -
        export -
        dplyr::coalesce(stock_variation, 0) -
        domestic_supply -
        residuals
    )
}

# Follow the existing source-vintage precedence, but do not apply the
# production-level ratio that .select_best_source() applies to old FBS values.
# A stock observation is retained in its own normalized mass basis.
.cbs_account_observations <- function(df) {
  df |>
    tibble::as_tibble() |>
    dplyr::filter(element %in% .cbs_account_elements()) |>
    .add_reporting_polity_columns() |>
    tibble::as_tibble() |>
    ensure_columns(tibble::tibble(fao_flag = character())) |>
    dplyr::summarise(
      value = .sum_if_any_cbs(value),
      fao_flag = .fold_fao_flag(fao_flag),
      .by = c(
        year,
        reporting_polity_code,
        area_code,
        item_cbs_code,
        element,
        source
      )
    ) |>
    dplyr::mutate(
      source_rank = dplyr::coalesce(
        match(source, c("FAOSTAT_FBS_New", "FAOSTAT_FBS_Old", "FAOSTAT_CBS")),
        4L
      )
    ) |>
    dplyr::arrange(source_rank, source) |>
    dplyr::slice_head(
      n = 1L,
      by = c(year, reporting_polity_code, item_cbs_code, element)
    ) |>
    dplyr::select(-source_rank)
}

# FAO's older FBS/CBS domains use positive withdrawals (Handbook 2001,
# II.2: https://www.fao.org/4/x9892e/X9892e02.htm). The current FBS uses
# positive additions. Domain-specific raw supply identities were checked;
# preserve the original source sign in source_accounts before normalizing.
.normalise_fao_stock_sign <- function(dt, pin_alias) {
  old <- c("faostat-fbs-old", "faostat-cbs-old-crops", "faostat-cbs-old-animal")
  known <- c(old, "faostat-fbs-new", "faostat-cbs-new")
  if (any(dt$element == "stock_variation") && !pin_alias %in% known) {
    cli::cli_abort(
      "The stock convention of source {.val {pin_alias}} is not registered.",
      class = "whep_cbs_stock_convention"
    )
  }
  if (pin_alias %in% old) {
    dt[element == "stock_variation", value := -value]
  }
  dt
}

.fao_stock_convention <- function(pin_alias) {
  if (
    pin_alias %in%
      c(
        "faostat-fbs-old",
        "faostat-cbs-old-crops",
        "faostat-cbs-old-animal"
      )
  ) {
    return("withdrawal_positive")
  }
  if (pin_alias %in% c("faostat-fbs-new", "faostat-cbs-new")) {
    return("addition_positive")
  }
  "unregistered"
}
