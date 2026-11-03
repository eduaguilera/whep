# Source crop targets and spatial support are different contracts. A CBS group
# with one supported crop does not prove that all its components are supported
# (#599/#1002). Keep the supplied source/flag columns in the coverage report;
# an imputed area must not be relabelled as an observed one.
.crop_component_audit <- function(allocation, primary, mapping, groups = NULL) {
  required <- c("year", "item_prod_code", "unit", "value")
  if (is.null(primary) || !all(rlang::has_name(primary, required))) {
    return(tibble::tibble())
  }
  targets <- .crop_component_targets(primary, mapping)
  if (!is.null(groups)) {
    targets <- .crop_component_domain(targets, groups)
    if (nrow(targets) == 0L) {
      return(tibble::tibble())
    }
    scoped <- .crop_alloc_pair(targets, groups)
    geo_year <- c(.crop_alloc_key(scoped$table), "year")
    requested <- scoped$reference |>
      dplyr::distinct(dplyr::across(dplyr::all_of(c(
        geo_year,
        "item_cbs_code"
      )))) |>
      dplyr::mutate(component_requested = TRUE)
    targets <- scoped$table |>
      dplyr::semi_join(scoped$reference, by = geo_year) |>
      dplyr::left_join(requested, by = c(geo_year, "item_cbs_code")) |>
      dplyr::filter(is.na(.data$item_cbs_code) | .data$component_requested) |>
      dplyr::select(-"component_requested")
  }
  if (nrow(targets) == 0L) {
    return(tibble::tibble())
  }
  weights <- .crop_component_domain(allocation$weights, targets)
  paired <- .crop_alloc_pair(targets, weights)
  targets <- paired$table
  geo <- .crop_alloc_key(targets)
  keys <- c(geo, "year", "item_prod_code")
  support <- paired$reference |>
    dplyr::filter(.data$area_weight > 0) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::mutate(component_present = TRUE)
  targets |>
    dplyr::left_join(support, by = keys) |>
    dplyr::mutate(
      component_present = dplyr::coalesce(.data$component_present, FALSE),
      coverage_status = dplyr::case_when(
        .data$mapping_status != "mapped" ~ .data$mapping_status,
        is.na(.data$target_area_ha) ~ "missing_target",
        !is.finite(.data$target_area_ha) | .data$target_area_ha < 0 ~
          "invalid_target",
        .data$component_present ~ "placed",
        any(.data$component_present) ~ "missing_component",
        TRUE ~ "whole_cbs_fallback"
      ),
      .by = dplyr::all_of(c(geo, "year", "item_cbs_code"))
    )
}

.crop_component_check <- function(audit, policy) {
  policy <- rlang::arg_match(policy, c("error", "legacy_pool"))
  if (nrow(audit) == 0) {
    return("targets_unavailable")
  }
  missing <- dplyr::filter(
    audit,
    .data$coverage_status %in%
      c(
        "missing_component",
        "missing_target",
        "invalid_target",
        "missing_mapping",
        "conflicting_mapping"
      )
  )
  if (nrow(missing) > 0) {
    text <- c(
      "{nrow(missing)} crop component{?s} lack a consistent mapping, target or
       spatial support in the requested CBS inputs.",
      i = "Inspect {.field missing_components}; source flags and imputed
           targets are preserved. A conserved CBS total is not complete
           crop coverage.",
      i = "Resolve the crop support or explicitly request
           {.code data$component_policy = 'legacy_pool'} to retain the
           legacy mixture with this diagnostic."
    )
    if (policy == "error") {
      cli::cli_abort(
        text,
        class = "whep_crop_component_coverage",
        missing_components = missing,
        component_coverage = audit
      )
    }
    cli::cli_warn(
      text,
      class = "whep_crop_component_coverage",
      missing_components = missing,
      component_coverage = audit
    )
    return("legacy_pool")
  }
  if (any(audit$coverage_status == "whole_cbs_fallback")) {
    return("checked_with_fallback")
  }
  "supplied_targets_covered"
}


.crop_component_targets <- function(primary, mapping) {
  .crop_alloc_table_years(primary)
  primary |>
    ensure_columns(tibble::tibble(item_cbs_code = integer())) |>
    dplyr::filter(.data$unit == "ha", is.na(.data$value) | .data$value != 0) |>
    dplyr::rename(source_item_cbs_code = "item_cbs_code") |>
    dplyr::mutate(item_prod_code = as.integer(.data$item_prod_code)) |>
    dplyr::left_join(mapping, by = "item_prod_code") |>
    dplyr::mutate(
      mapping_status = dplyr::case_when(
        is.na(.data$item_cbs_code) ~ "missing_mapping",
        !is.na(.data$source_item_cbs_code) &
          .data$item_cbs_code != .data$source_item_cbs_code ~
          "conflicting_mapping",
        TRUE ~ "mapped"
      ),
      # The supplied CBS key scopes the diagnostic when its production key
      # is unknown. It does not invent a production-item mapping.
      item_cbs_code = dplyr::coalesce(
        .data$source_item_cbs_code,
        .data$item_cbs_code
      )
    ) |>
    dplyr::rename(target_area_ha = "value")
}

# Narrow source-adapter candidates before strict identity resolution. Native
# inputs are matched on polity identity, never on their numeric annotations.
# Missing identities within this candidate domain remain errors.
.crop_component_domain <- function(table, reference) {
  .crop_alloc_table_years(table)
  .crop_alloc_table_years(reference)
  keys <- "year"
  native <- .crop_alloc_has_identity(table) &&
    .crop_alloc_has_identity(reference)
  if (
    !native &&
      rlang::has_name(table, "area_code") &&
      rlang::has_name(reference, "area_code")
  ) {
    table <- dplyr::mutate(
      table,
      .k_area = as.character(.data$area_code),
      .k_year = as.integer(.data$year)
    )
    reference <- dplyr::mutate(
      reference,
      .k_area = as.character(.data$area_code),
      .k_year = as.integer(.data$year)
    )
    return(
      dplyr::semi_join(table, reference, by = c(".k_area", ".k_year")) |>
        dplyr::select(-".k_area", -".k_year")
    )
  }
  dplyr::semi_join(table, reference, by = keys)
}
