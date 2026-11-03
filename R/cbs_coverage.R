# Require comparable territorial populations before CBS source selection.
# FAO (2017), Guidelines for the Compilation of Food Balance Sheets,
# sections 3.4.2.1-2, pp. 53-54, recommends historical-series imputation when
# observations are unavailable and publishing the source of estimates:
# UNSIAP's 140-page edition, independently checked for whep#1481.
#
# The complete-territory source policy is provisional (whep#1481), not a FAO
# prescription. It reuses WHEP's existing interpolation of the bucket's own
# shares and world-share estimator, rather than assigning a missing member
# the covered member's pattern implicitly. Balancing each member separately
# remains the more detailed solution in the territorial migration (whep#680).
# Assumed, unverified: prefer a whole-territory temporal estimate over a
# partial-member pattern when constructing an aggregate balance.
.cbs_coverage_choices <- function() {
  c("complete_sources", "world_shares")
}

.cbs_coverage_inputs <- function(inputs, method) {
  method <- rlang::arg_match(method, .cbs_coverage_choices())
  sources <- intersect(
    c("fbs_new", "fbs_old", "cbs_crops", "cbs_animals", "cbs_new"),
    names(inputs)
  )
  records <- purrr::map(inputs[sources], attr, which = "bucket_members")
  years <- sort(unique(purrr::list_rbind(records)$year))
  if (length(years) == 0L) {
    return(inputs)
  }
  .check_cbs_member_records(inputs[sources], records)
  expected <- .resolve_all_area_years(years) |>
    .in_span_members() |>
    dplyr::distinct(.data$year, .data$polity_area_code, .data$area_code)
  log <- purrr::imap(records, \(reported, source) {
    .cbs_partial_years(expected, reported) |>
      dplyr::mutate(source = source, method = method)
  }) |>
    purrr::list_rbind()
  inputs[sources] <- purrr::imap(inputs[sources], \(data, source) {
    bad <- dplyr::filter(log, .data$source == .env$source) |>
      dplyr::select("year", area_code = "polity_area_code")
    dplyr::anti_join(
      tibble::as_tibble(data),
      bad,
      by = c("year", "area_code")
    )
  })
  attr(inputs, "coverage_log") <- log
  inputs
}

# An empty supplied record can legitimately describe zero-only observations.
# An absent record cannot certify a nonempty source using another source's
# metadata. Keep these cases distinct before attaching coverage labels.
.check_cbs_member_records <- function(inputs, records) {
  missing <- names(inputs)[purrr::map_lgl(names(inputs), \(source) {
    nrow(inputs[[source]]) > 0L && is.null(records[[source]])
  })]
  if (length(missing) > 0L) {
    cli::cli_abort(
      "CBS sources {.val {missing}} lack supplied membership records.",
      class = "whep_cbs_coverage_unavailable",
      sources = missing
    )
  }
  invisible(NULL)
}

.cbs_partial_years <- function(expected, reported) {
  empty <- tibble::tibble(
    year = integer(),
    polity_area_code = integer(),
    covered = character(),
    missing = character()
  )
  if (is.null(reported) || nrow(reported) == 0L) {
    return(empty)
  }
  reported <- tibble::as_tibble(reported)
  expected |>
    dplyr::semi_join(reported, by = c("year", "polity_area_code")) |>
    dplyr::anti_join(
      reported,
      by = c("year", "polity_area_code", "area_code")
    ) |>
    dplyr::summarise(
      missing = paste(sort(unique(.data$area_code)), collapse = ", "),
      .by = c("year", "polity_area_code")
    ) |>
    dplyr::left_join(
      dplyr::summarise(
        reported,
        covered = paste(sort(unique(.data$area_code)), collapse = ", "),
        .by = c("year", "polity_area_code")
      ),
      by = c("year", "polity_area_code")
    )
}

# Stamp the decision after the balancing reductions, which drop extra columns.
# The audit keeps the original partial source and both member sets. Its key is
# bucket-year, so the stamp describes the policy used, not a claim that every
# item was imputed or that missing members were directly observed.
.cbs_stamp_coverage <- function(data, log) {
  if (is.null(log) || nrow(log) == 0L) {
    data$method_cbs_coverage <- if (is.null(log)) {
      "not_checked"
    } else {
      "reported_members"
    }
  } else {
    keys <- log |>
      dplyr::distinct(
        .data$year,
        area_code = .data$polity_area_code,
        method_cbs_coverage = .data$method
      )
    data <- dplyr::left_join(data, keys, by = c("year", "area_code")) |>
      dplyr::mutate(
        method_cbs_coverage = dplyr::coalesce(
          .data$method_cbs_coverage,
          "reported_members"
        )
      )
  }
  # A world-share estimate is not a FAO observation for this bucket.
  # Keep the direct primary-production flags.
  if (all(rlang::has_name(data, c("fao_flag", "element")))) {
    data <- dplyr::mutate(
      data,
      fao_flag = dplyr::if_else(
        .data$method_cbs_coverage == "world_shares" &
          .data$element != "production",
        NA_character_,
        .data$fao_flag
      )
    )
  }
  attr(data, "cbs_coverage_log") <- log
  data
}


# The coarser alternative uses complete same-year/item donor profiles. Like
# .add_global_destiny_shares(), normalize observed uses over their total. All
# affected bucket-years are excluded from the donors. Build every donor
# destiny, not only those the target ever reported, so the shares sum to one.
.cbs_world_coverage <- function(destiny, balance, log) {
  if (is.null(log) || !any(log$method == "world_shares")) {
    return(destiny)
  }
  keys <- log |>
    dplyr::filter(.data$method == "world_shares") |>
    dplyr::distinct(.data$year, area_code = .data$polity_area_code)
  targets <- .cbs_world_targets(balance, keys)
  donors <- .cbs_world_profiles(destiny, keys)
  missing <- dplyr::anti_join(targets, donors, by = c("year", "item_cbs_code"))
  if (nrow(missing) > 0L) {
    cli::cli_abort(
      "No positive world use profile for {nrow(missing)} bucket-item-years.",
      class = "whep_cbs_world_profile_absent",
      missing_profiles = missing
    )
  }
  labels <- .cbs_area_labels(data.table::as.data.table(balance))
  estimates <- dplyr::inner_join(
    targets,
    donors,
    by = c("year", "item_cbs_code"),
    relationship = "many-to-many"
  ) |>
    dplyr::mutate(
      elem_cat = "destiny",
      value = NA_real_,
      source = "world_share_estimate"
    ) |>
    .attach_cbs_area_label(labels) |>
    tibble::as_tibble() |>
    dplyr::left_join(
      tibble::as_tibble(.items_cbs_bridge()),
      by = "item_cbs_code"
    )
  changed <- dplyr::distinct(
    targets,
    .data$year,
    .data$area_code,
    .data$item_cbs_code
  )
  tibble::as_tibble(destiny) |>
    dplyr::anti_join(changed, by = c("year", "area_code", "item_cbs_code")) |>
    dplyr::bind_rows(estimates)
}

.cbs_world_profiles <- function(destiny, keys) {
  tibble::as_tibble(destiny) |>
    dplyr::anti_join(keys, by = c("year", "area_code")) |>
    dplyr::filter(!is.na(.data$value), .data$value > 0) |>
    dplyr::summarise(
      value = sum(.data$value),
      .by = c("year", "item_cbs_code", "element")
    ) |>
    dplyr::mutate(
      dest_share = .data$value / sum(.data$value),
      .by = c("year", "item_cbs_code")
    ) |>
    dplyr::select(-"value")
}


.cbs_world_targets <- function(balance, keys) {
  # Structural allocation, not an estimated profile: processing residues go
  # entirely to feed in .compute_destiny_shares(), even at zero observed use.
  structural <- whep::items_full |>
    dplyr::filter(.data$comm_group == "Other processing residues") |>
    dplyr::pull("item_cbs_code")
  tibble::as_tibble(balance) |>
    dplyr::filter(
      .data$element == "domestic_supply",
      .data$value > 0,
      !.data$item_cbs_code %in% structural
    ) |>
    dplyr::semi_join(keys, by = c("year", "area_code")) |>
    dplyr::distinct(.data$year, .data$area_code, .data$item_cbs_code)
}


# Old serialized production objects may carry extracts whose member record was
# stripped. Refresh them through the normal reader instead of silently serving
# uncheckable source populations from the cache.
.cbs_reuse_extracts <- function(extracts) {
  sources <- c("fbs_new", "fbs_old", "cbs_crops", "cbs_animals")
  !is.null(extracts) &&
    all(rlang::has_name(extracts, sources)) &&
    purrr::every(extracts[sources], \(data) {
      !is.null(attr(data, "bucket_members")) &&
        !is.null(attr(data, "source_accounts")) &&
        identical(attr(data, "stock_convention"), "addition_positive")
    })
}
