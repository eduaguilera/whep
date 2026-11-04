#' Account for cohort-specific direct livestock water.
#'
#' @description
#' Multiply explicitly supplied cohort rates by animal-day exposure, retaining
#' species, cohort, production system and source identity. FAO (2019),
#' *Water use of livestock production systems and supply chains*, sections
#' 4.5.4–4.5.5, distinguishes water supplied, returned and evaporated:
#' ISBN 978-92-5-131713-6.
#' Drinking intake or water requirements are not automatically withdrawals or
#' net consumptive blue water. No demographic or consumption fraction is
#' inferred. Supplied provenance is an attestation, not verified evidence.
#' Existing generated output columns and conflicting join columns abort
#' rather than silently replacing prior quantities or provenance.
#'
#' @param exposure Tibble with `item_cbs_code`, `cohort`, `production_system`,
#'   `animal_days`, `rate_id`, `assessment_period`, `exposure_source`, and
#'   `exposure_basis`. Animal-days must cover the described assessment interval;
#'   a snapshot head count is insufficient. Optional `year`, canonical polity,
#'   and other context columns are retained unchanged, including unresolved
#'   values. No territorial aggregation or identity validation is performed.
#'   An annual extension adapter must separately require and validate year and
#'   canonical polity, and reject unresolved multi-year study records.
#' @param rates Tibble unique by `rate_id`, with matching `item_cbs_code`,
#'   `cohort`, `production_system`, rates `drinking_l_day`, `service_l_day`,
#'   and nonempty `rate_source`, `rate_basis`, `quantity_basis`.
#'   `rate_basis` is `"measured"`, `"modelled"`, or `"assumed"`;
#'   `quantity_basis` is `"withdrawal"`, `"requirement"`, or
#'   `"free_drinking_intake"`. The first two require both component rates on
#'   that same stated basis. Free intake requires `service_l_day = NA_real_`:
#'   service, total direct water and consumption remain unknown, never zero.
#'   This explicitly retains a drinking estimate without mixing it with a
#'   differently defined service quantity. Use [estimate_dairy_drinking()] to
#'   derive an empirical rate; its examples show the composition path.
#' @param consumption Optional tibble unique by `rate_id`, with
#'   `drinking_fraction`, `service_fraction` between zero and one, and nonempty
#'   `consumption_source`, `consumption_basis`. Only explicit withdrawal rates
#'   can be converted. These fractions must match the stated system boundary;
#'   they are not universally applicable livestock constants.
#'
#' @return Exposure rows with rates, provenance, `drinking_m3`, `service_m3`,
#'   `direct_water_m3`, and `consumption_m3` (unknown when not supplied).
#'   `coverage_direct_water` is `"service_unknown"` for intake-only rows and
#'   `"complete"` for rows with both rates. This describes supplied component
#'   coverage, not completeness of an external herd inventory.
#' @export
#' @examples
#' x <- tibble::tibble(
#'   year = 2000L, reporting_polity_code = "synthetic-example",
#'   item_cbs_code = 961L, cohort = "adult", production_system = "example",
#'   animal_days = 10, rate_id = "synthetic",
#'   assessment_period = "synthetic annual interval",
#'   exposure_source = "synthetic example", exposure_basis = "assumed"
#' )
#' rates <- tibble::tibble(
#'   rate_id = "synthetic", item_cbs_code = 961L, cohort = "adult",
#'   production_system = "example", drinking_l_day = 30, service_l_day = 8,
#'   rate_source = "synthetic example", rate_basis = "assumed",
#'   quantity_basis = "requirement"
#' )
#' calculate_livestock_water(x, rates)
calculate_livestock_water <- function(exposure, rates, consumption = NULL) {
  outputs <- c(
    "drinking_m3",
    "service_m3",
    "direct_water_m3",
    "consumption_m3",
    "coverage_direct_water"
  )
  .livestock_water_no_output(exposure, outputs)
  .livestock_water_no_output(rates, outputs)
  .livestock_water_no_output(consumption, outputs)
  keys <- c("rate_id", "item_cbs_code", "cohort", "production_system")
  .livestock_water_schema(
    exposure,
    c(
      keys,
      "assessment_period",
      "animal_days",
      "exposure_source",
      "exposure_basis"
    )
  )
  .livestock_water_schema(
    rates,
    c(
      keys,
      "drinking_l_day",
      "service_l_day",
      "rate_source",
      "rate_basis",
      "quantity_basis"
    ),
    allow_na = "service_l_day"
  )
  if (length(intersect(setdiff(names(rates), keys), names(exposure))) > 0L) {
    cli::cli_abort(
      "Exposure and rates must not have conflicting columns.",
      class = "whep_livestock_water_input"
    )
  }
  .livestock_water_numbers(exposure$animal_days)
  .livestock_water_numbers(rates$drinking_l_day)
  .livestock_water_numbers(rates$service_l_day[!is.na(rates$service_l_day)])
  .livestock_water_service(rates)
  if (
    anyDuplicated(rates$rate_id) ||
      !all(rates$rate_basis %in% c("measured", "modelled", "assumed")) ||
      !all(
        rates$quantity_basis %in%
          c("withdrawal", "requirement", "free_drinking_intake")
      )
  ) {
    cli::cli_abort(
      "Water rates require unique IDs and explicit valid bases.",
      class = "whep_livestock_water_input"
    )
  }
  if (nrow(dplyr::anti_join(exposure, rates, by = keys)) > 0L) {
    cli::cli_abort(
      "Every exposure requires a matching species/cohort/system rate.",
      class = "whep_livestock_water_coverage"
    )
  }
  out <- dplyr::left_join(
    exposure,
    rates,
    by = keys,
    relationship = "many-to-one"
  ) |>
    dplyr::mutate(
      drinking_m3 = .data$animal_days * .data$drinking_l_day / 1000,
      service_m3 = .data$animal_days * .data$service_l_day / 1000,
      direct_water_m3 = .data$drinking_m3 + .data$service_m3,
      coverage_direct_water = dplyr::if_else(
        .data$quantity_basis == "free_drinking_intake",
        "service_unknown",
        "complete"
      )
    )
  .livestock_water_consumption(out, consumption)
}

#' Estimate free drinking intake of lactating dairy cows.
#'
#' @description
#' Apply the empirical lactating-cow model of Cardot, Le Roux and Jurjanz
#' (2008), doi:10.3168/jds.2007-0204. `"cardot_table5"` uses the body equation
#' and Table 5 coefficients (DMI 1.54; minimum temperature 0.58).
#' `"cardot_abstract"` explicitly selects the differing abstract coefficients
#' (1.53 and 0.57). Other coefficients are identical. The fit used 1,837
#' cow-days from 41 mid-lactation Holstein cows during three winters in a
#' free-stall, corn-silage/concentrate TMR setting (summer grazing excluded).
#' R-squared was 0.446. Checking marginal predictor ranges does not
#' establish validity for other breeds, diets, climates or joint conditions.
#'
#' `"legacy_adult"` selects the 55 L/day mixed-system dairy drinking demand
#' in Chapagain and Hoekstra (2003), *Value of Water Research Report 13*,
#' Table 3.8 (<https://waterfootprint.org/resources/Report13.pdf>): mean of
#' industrial 70 and grazing 40 L/day. Its adult lactation
#' assumption is a sensitivity, not an observed population coefficient.
#' Neither method includes service water, leakage or net consumption.
#' Existing generated rate, quantity and method columns are rejected rather
#' than overwritten; supply the original predictor/exposure columns.
#'
#' @param data Tibble with `animal_days`, `item_cbs_code` (960), `cohort`
#'   (`"lactating"`), and `production_system`. Cardot methods require
#'   `production_system = "free_stall_tmr"`, `dry_matter_intake_kg_day`,
#'   `milk_kg_day`, `diet_dry_matter_percent`, `minimum_temperature_c`, and
#'   `rainfall_mm_day`. Additional identity/source columns are retained.
#' @param method Select `"cardot_table5"` (default), `"cardot_abstract"`, or
#'   `"legacy_adult"`. There is no fallback between methods.
#' @param extrapolation `"abort"` (default) rejects predictors outside the
#'   marginal ranges in Table 4. `"allow"` warns and marks such rows.
#'   Impossible negative predictions always abort, never clamp to zero.
#'
#' @return Input rows with `drinking_l_day`, `drinking_m3`,
#'   `method_drinking_water`, `quantity_basis`, `water_extrapolated`, and
#'   `drinking_rate_source` / `drinking_rate_basis` provenance.
#' @export
#' @examples
#' # Published rounded means/exposure, not raw individual cow-day records.
#' x <- tibble::tibble(
#'   animal_days = 1837, item_cbs_code = 960L, cohort = "lactating",
#'   production_system = "free_stall_tmr", dry_matter_intake_kg_day = 20.6,
#'   milk_kg_day = 26.5, diet_dry_matter_percent = 48.1,
#'   minimum_temperature_c = 0.1, rainfall_mm_day = 1.3,
#'   rate_id = "study_means", assessment_period = "2003–2005 winter trials",
#'   exposure_source = "Cardot 2008 Tables 1 and 3",
#'   exposure_basis = "observed cow-days"
#' )
#' intake <- estimate_dairy_drinking(x)
#' rates <- intake |>
#'   dplyr::transmute(
#'     rate_id, item_cbs_code, cohort, production_system, drinking_l_day,
#'     service_l_day = NA_real_, quantity_basis,
#'     rate_source = drinking_rate_source, rate_basis = drinking_rate_basis
#'   )
#' calculate_livestock_water(x, rates)
estimate_dairy_drinking <- function(
  data,
  method = c("cardot_table5", "cardot_abstract", "legacy_adult"),
  extrapolation = c("abort", "allow")
) {
  method <- rlang::arg_match(method)
  extrapolation <- rlang::arg_match(extrapolation)
  .livestock_water_no_output(
    data,
    c(
      "drinking_l_day",
      "drinking_m3",
      "water_extrapolated",
      "method_drinking_water",
      "drinking_rate_source",
      "drinking_rate_basis",
      "quantity_basis"
    )
  )
  .livestock_water_schema(
    data,
    c(
      "animal_days",
      "item_cbs_code",
      "cohort",
      "production_system"
    )
  )
  .livestock_water_numbers(data$animal_days)
  if (any(data$item_cbs_code != 960L | data$cohort != "lactating")) {
    cli::cli_abort(
      "This drinking model requires lactating dairy-cow exposure.",
      class = "whep_livestock_water_scope"
    )
  }
  estimated <- if (method == "legacy_adult") {
    dplyr::mutate(
      data,
      drinking_l_day = (70 + 40) / 2,
      water_extrapolated = FALSE
    )
  } else {
    .cardot_drinking(data, method, extrapolation)
  }
  estimated |>
    dplyr::mutate(
      drinking_m3 = .data$animal_days * .data$drinking_l_day / 1000,
      method_drinking_water = method,
      drinking_rate_source = if (method == "legacy_adult") {
        "Chapagain and Hoekstra (2003), Report 13 Table 3.8, mixed adult dairy"
      } else {
        paste0("Cardot et al. (2008), doi:10.3168/jds.2007-0204, ", method)
      },
      drinking_rate_basis = if (method == "legacy_adult") {
        "assumed"
      } else {
        "modelled"
      },
      quantity_basis = if (method == "legacy_adult") {
        "requirement"
      } else {
        "free_drinking_intake"
      }
    )
}

.livestock_water_no_output <- function(data, columns) {
  conflicting <- intersect(names(data), columns)
  if (length(conflicting)) {
    cli::cli_abort(
      "Livestock-water inputs must not overwrite columns: {.field {conflicting}}.",
      class = "whep_livestock_water_input"
    )
  }
}

.livestock_water_schema <- function(data, columns, allow_na = character()) {
  if (!all(rlang::has_name(data, columns))) {
    cli::cli_abort(
      "Missing livestock-water input columns.",
      class = "whep_livestock_water_input"
    )
  }
  selected <- dplyr::select(data, dplyr::all_of(setdiff(columns, allow_na)))
  if (
    anyNA(selected) ||
      any(vapply(
        selected,
        function(x) {
          (is.character(x) || is.factor(x)) &&
            any(!nzchar(trimws(as.character(x))))
        },
        logical(1)
      ))
  ) {
    cli::cli_abort(
      "Livestock-water input values and provenance must be supplied.",
      class = "whep_livestock_water_input"
    )
  }
}

.livestock_water_numbers <- function(x) {
  if (!is.numeric(x) || any(!is.finite(x) | x < 0)) {
    cli::cli_abort(
      "Water quantities and exposures must be finite and nonnegative.",
      class = "whep_livestock_water_input"
    )
  }
}

.livestock_water_service <- function(rates) {
  intake <- rates$quantity_basis == "free_drinking_intake"
  if (
    any(is.nan(rates$service_l_day)) ||
      any(intake & !is.na(rates$service_l_day)) ||
      any(!intake & is.na(rates$service_l_day))
  ) {
    cli::cli_abort(
      "Intake rows require unknown service; other bases require service.",
      class = "whep_livestock_water_input"
    )
  }
}

.livestock_water_consumption <- function(out, consumption) {
  if (is.null(consumption)) {
    return(dplyr::mutate(out, consumption_m3 = NA_real_))
  }
  .livestock_water_no_output(consumption, setdiff(names(out), "rate_id"))
  .livestock_water_schema(
    consumption,
    c(
      "rate_id",
      "drinking_fraction",
      "service_fraction",
      "consumption_source",
      "consumption_basis"
    )
  )
  .livestock_water_numbers(consumption$drinking_fraction)
  .livestock_water_numbers(consumption$service_fraction)
  if (
    anyDuplicated(consumption$rate_id) ||
      any(
        consumption$drinking_fraction > 1 | consumption$service_fraction > 1
      ) ||
      any(out$quantity_basis != "withdrawal") ||
      nrow(dplyr::anti_join(out, consumption, by = "rate_id")) > 0L
  ) {
    cli::cli_abort(
      "Consumption requires covered withdrawals and valid fractions.",
      class = "whep_livestock_water_consumption"
    )
  }
  dplyr::left_join(
    out,
    consumption,
    by = "rate_id",
    relationship = "many-to-one"
  ) |>
    dplyr::mutate(
      consumption_m3 = .data$drinking_m3 *
        .data$drinking_fraction +
        .data$service_m3 * .data$service_fraction
    )
}

.cardot_drinking <- function(data, method, extrapolation) {
  cols <- c(
    "dry_matter_intake_kg_day",
    "milk_kg_day",
    "diet_dry_matter_percent",
    "minimum_temperature_c",
    "rainfall_mm_day"
  )
  .livestock_water_schema(data, cols)
  values <- as.matrix(dplyr::select(data, dplyr::all_of(cols)))
  if (
    !is.numeric(values) ||
      any(!is.finite(values)) ||
      any(data$production_system != "free_stall_tmr")
  ) {
    cli::cli_abort(
      "Cardot requires finite predictors and free-stall TMR cows.",
      class = "whep_livestock_water_scope"
    )
  }
  if (
    any(values[, c(1, 2, 3, 5), drop = FALSE] < 0) ||
      any(data$diet_dry_matter_percent > 100)
  ) {
    cli::cli_abort(
      "Cardot predictors must retain physical units and bounds.",
      class = "whep_livestock_water_input"
    )
  }
  # Published marginal bounds, Table 4; not a joint-domain validation.
  lower <- c(5.4, 7.3, 36.3, -11.1, 0)
  upper <- c(33.9, 46.3, 57, 9.1, 33.2)
  outside <- rowSums(
    sweep(values, 2, lower, `<`) |
      sweep(values, 2, upper, `>`)
  ) >
    0
  if (any(outside)) {
    if (extrapolation == "abort") {
      cli::cli_abort(
        "Cardot predictors exceed published marginal ranges.",
        class = "whep_livestock_water_extrapolation"
      )
    }
    cli::cli_warn(
      "Cardot extrapolation is explicitly requested, not validated.",
      class = "whep_livestock_water_extrapolation"
    )
  }
  coefs <- if (method == "cardot_table5") {
    c(1.54, 1.33, 0.89, 0.58, -0.30)
  } else {
    c(1.53, 1.33, 0.89, 0.57, -0.30)
  }
  estimate <- as.numeric(values %*% coefs) - 25.65
  .livestock_water_numbers(estimate)
  dplyr::mutate(data, drinking_l_day = estimate, water_extrapolated = outside)
}
