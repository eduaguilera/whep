#' Build source-specific reporting support on a complete physical domain
#'
#' @description
#' Preserves canonical polity footprints while selecting an explicit source/year
#' inventory. Selected footprints must not overlap. The physical denominator is
#' measured independently from the supplied domain, including territory with no
#' selected reporting row. Neither source codes nor polity types set priority.
#' Water supplied as whole-cell fractions is allocated uniformly over ice-free
#' territory. This is an explicit within-cell spatial assumption, not an observed
#' water-polygon distribution. Domain water beyond available ice-free territory
#' remains recorded as excess. After geometric checks, a numerical excess within
#' the existing polycell resolution (1e-6 ha per cell) is reduced proportionally.
#' `measured_land_area_ha`, `area_adjustment_ha` and `area_adjustment_method` retain
#' that operation; the independent domain is unchanged. Larger excesses abort,
#' and genuinely unrepresented territory is never redistributed. These checks
#' establish computational consistency, not historical boundary truth.
#' Refs #1310, #1483 and #458.
#'
#' @param inventory Tibble with `source`, numeric whole `year`, and
#'   `reporting_polity_code`. Optional `use` is `reporting` or `alternative`;
#'   alternatives require a nonempty `basis` and remain in the returned inventory.
#' @param domain An `sf` polygon table covering the complete physical domain,
#'   with a nonempty `domain_source` provenance column and `year` matching the
#'   inventory. Overlapping domain polygons
#'   are unioned, not summed. Completeness is a supplied contract, not inferred
#'   from reporting rows; the returned object preserves the domain for review.
#' @param geometries Canonical polity geometries; defaults to
#'   [get_polity_geometries()].
#' @param layers Named list with optional `water` and `ice` inputs as accepted by
#'   [build_polycell_support()]. Absent inputs remain stamped as absent.
#' @return A reporting-support bundle with `support`, `footprints`,
#'   `canonical_geometries`, `physical_domain`, `domain_support`,
#'   `inventory`, `domain`, and per-cell `audit`. Use [read_reporting_support()]
#'   to retrieve support retaining the independent domain denominator.
#' @export
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   g <- polycell_example_geometries()
#'   d <- sf::st_sf(domain_source = "example", year = 2010L,
#'                   geometry = sf::st_union(g))
#'   i <- tibble::tibble(source = "example", year = 2010L,
#'                      reporting_polity_code = g$polity_code)
#'   suppressWarnings(build_reporting_support(i, d, g))
#' }
build_reporting_support <- function(
  inventory,
  domain,
  geometries = NULL,
  layers = list()
) {
  rlang::check_installed("sf")
  old_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(old_s2)))
  suppressMessages(sf::sf_use_s2(TRUE))
  inventory <- .rs_inventory(inventory)
  geometries <- geometries %||% get_polity_geometries()
  canonical <- .rs_canonical(geometries, inventory)
  selected <- canonical[
    canonical$polity_code %in%
      inventory$reporting_polity_code[inventory$use == "reporting"],
  ]
  physical <- .rs_domain(domain, inventory$year[[1]])
  attr(physical, "geometry_source") <- paste(
    unique(domain$domain_source),
    collapse = ";"
  )
  containment_check <- .rs_check_domain(canonical, physical)
  .rs_check_footprints(selected)
  denominator <- build_polycell_support(
    geometries = physical,
    water = layers$water,
    ice = layers$ice
  )
  footprints <- .rs_measure(canonical, denominator, layers)
  footprints$geometry_source <- .pcs_geometry_source(geometries)
  footprints$source <- inventory$source[[1]]
  footprints$water_spatial_method <- if (is.null(layers$water)) {
    "not_supplied"
  } else {
    "uniform_ice_free_area"
  }
  footprints$partition_method <- "explicit_reporting_inventory"
  footprints$domain_source <- paste(
    unique(domain$domain_source),
    collapse = ";"
  )
  footprints$measured_land_area_ha <- footprints$land_area_ha
  footprints$area_adjustment_ha <- 0
  footprints$area_adjustment_method <- "none"
  support <- .rs_close_support(
    footprints[footprints$polity_code %in% selected$polity_code, ]
  )
  footprints$support_role <- "overlap"
  structure(
    list(
      schema_version = 1L,
      support = support,
      footprints = footprints,
      canonical_geometries = geometries,
      inventory = inventory,
      domain = domain,
      physical_domain = physical,
      domain_support = denominator,
      geometry_checks = list(containment = containment_check),
      audit = .rs_audit(support, denominator)
    ),
    class = "whep_reporting_support"
  )
}

#' Read native reporting support without renormalizing its physical domain
#'
#' @param bundle A bundle returned by [build_reporting_support()], or the path
#'   to a complete bundle saved with `saveRDS()`. The canonical geometries,
#'   inventory and domain remain together in that artifact.
#' @param role Select the disjoint reporting `partition` or all inventory
#'   `footprints`, including explicitly excluded alternatives. Footprints are
#'   marked as overlap and must not be summed as a physical partition.
#' @param require_layers Require supplied water and ice, as in
#'   [read_polycell_support()]. Set `FALSE` explicitly for geometry-only study.
#' @return A tibble carrying `domain_land_area_ha` alongside polity areas.
#' @export
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   g <- polycell_example_geometries()
#'   d <- sf::st_sf(domain_source = "example", year = 2010L,
#'                   geometry = sf::st_union(g))
#'   i <- tibble::tibble(source = "example", year = 2010L,
#'                      reporting_polity_code = g$polity_code)
#'   b <- suppressWarnings(build_reporting_support(i, d, g))
#'   read_reporting_support(b, require_layers = FALSE)
#' }
read_reporting_support <- function(
  bundle,
  role = c("partition", "footprints"),
  require_layers = TRUE
) {
  role <- rlang::arg_match(role)
  if (is.character(bundle) && length(bundle) == 1L && !is.na(bundle)) {
    if (!file.exists(bundle)) {
      cli::cli_abort(
        "Reporting-support artifact does not exist: {.file {bundle}}.",
        class = "whep_reporting_bundle"
      )
    }
    bundle <- readRDS(bundle)
  }
  required <- c(
    "support",
    "footprints",
    "physical_domain",
    "domain_support",
    "canonical_geometries",
    "inventory",
    "domain",
    "audit"
  )
  if (
    !inherits(bundle, "whep_reporting_support") ||
      !identical(bundle$schema_version, 1L) ||
      !all(rlang::has_name(bundle, required))
  ) {
    cli::cli_abort(
      "Expected a native reporting-support bundle.",
      class = "whep_reporting_bundle"
    )
  }
  tryCatch(.rs_check_bundle(bundle), error = function(e) {
    cli::cli_abort(
      "Reporting bundle contents violate its inventory or domain.",
      class = "whep_reporting_bundle",
      parent = e
    )
  })
  out <- if (role == "footprints") bundle$footprints else bundle$support
  .check_polycell_layers(out, require_layers, "native reporting-support bundle")
  out
}

.rs_inventory <- function(x) {
  .check_columns(x, c("source", "year", "reporting_polity_code"), "inventory")
  if (
    !.valid_native_years(x$year) ||
      any(x$year >= .Machine$integer.max) ||
      nrow(x) == 0L ||
      dplyr::n_distinct(x$year) != 1L ||
      dplyr::n_distinct(x$source) != 1L ||
      anyNA(x$source) ||
      any(!nzchar(trimws(x$source))) ||
      !is.character(x$reporting_polity_code) ||
      anyNA(x$reporting_polity_code) ||
      any(!nzchar(trimws(x$reporting_polity_code))) ||
      anyDuplicated(x$reporting_polity_code)
  ) {
    cli::cli_abort(
      "Inventory needs one source/year and unique polity identities.",
      class = "whep_reporting_inventory"
    )
  }
  x$use <- x[["use"]] %||% rep("reporting", nrow(x))
  x$basis <- x[["basis"]] %||% rep(NA_character_, nrow(x))
  alternative <- x$use == "alternative"
  if (
    anyNA(x$use) ||
      any(!x$use %in% c("reporting", "alternative")) ||
      !any(x$use == "reporting") ||
      any(alternative & (is.na(x$basis) | !nzchar(trimws(x$basis))))
  ) {
    cli::cli_abort(
      "Every alternative needs an explicit selection basis.",
      class = "whep_reporting_policy"
    )
  }
  tibble::as_tibble(x)
}

.rs_canonical <- function(geometries, inventory) {
  .check_columns(
    geometries,
    c("polity_code", "start_year", "end_year"),
    "geometries"
  )
  if (!inherits(geometries, "sf")) {
    cli::cli_abort(
      "Canonical footprints must be an sf geometry table.",
      class = "whep_reporting_geometry"
    )
  }
  x <- geometries[
    geometries$polity_code %in%
      inventory$reporting_polity_code,
  ]
  if (
    !.valid_native_years(x$start_year) ||
      !.valid_native_years(x$end_year)
  ) {
    cli::cli_abort(
      "Canonical footprint intervals need finite whole years.",
      class = "whep_reporting_geometry"
    )
  }
  # Source codes are optional provenance, never a prerequisite for native
  # geometry. Preserve a supplied annotation and do not perform a FAOSTAT join.
  x$area_code <- .pcs_col(sf::st_drop_geometry(x), "area_code", NA_integer_)
  x <- .pcs_prepare_polities(x, aggregates = "overlap_layer")
  year <- inventory$year[[1]]
  x <- x[
    x$polity_code %in%
      inventory$reporting_polity_code &
      x$start_year <= year &
      x$end_year > year,
  ]
  if (
    anyDuplicated(x$polity_code) ||
      !setequal(x$polity_code, inventory$reporting_polity_code) ||
      any(!x$coverage_status %in% c("has_geometry", "s2_repaired"))
  ) {
    cli::cli_abort(
      "Every inventory identity needs one usable dated footprint.",
      class = "whep_reporting_geometry"
    )
  }
  x$start_year <- year
  x$end_year <- year + 1L
  x$support_role <- "partition"
  x
}

.rs_check_domain_input <- function(domain, year) {
  if (
    !inherits(domain, "sf") ||
      !all(rlang::has_name(domain, c("domain_source", "year"))) ||
      !.valid_native_years(domain$year) ||
      any(domain$year != year) ||
      nrow(domain) == 0L ||
      anyNA(domain$domain_source) ||
      any(!nzchar(trimws(domain$domain_source))) ||
      any(sf::st_is_empty(domain))
  ) {
    cli::cli_abort(
      "Supply a complete physical domain with explicit provenance.",
      class = "whep_reporting_domain"
    )
  }
  invisible(domain)
}

.rs_domain <- function(domain, year) {
  .rs_check_domain_input(domain, year)
  input <- .rs_valid_geometry(sf::st_geometry(sf::st_transform(domain, 4326)))
  united <- .rs_valid_geometry(sf::st_union(input$geom))
  sf::st_sf(
    polity_code = "__physical_domain__",
    area_code = NA_integer_,
    domain_geometry_status = paste(
      unique(c(input$status, united$status)),
      collapse = ";"
    ),
    start_year = year,
    end_year = year + 1L,
    geometry = united$geom
  )
}

.rs_valid_geometry <- function(geom) {
  fixed <- .s2_repair(geom)
  if (any(fixed$status == "invalid")) {
    cli::cli_abort(
      "A derived reporting geometry remains invalid under s2.",
      class = "whep_reporting_geometry"
    )
  }
  fixed
}

.rs_check_domain <- function(x, domain) {
  # Match the existing polycell support's declared 1e-6 ha clipping floor,
  # rather than introducing a larger sliver allowance or changing geometry.
  outside <- suppressWarnings(sf::st_difference(
    sf::st_geometry(x),
    sf::st_geometry(domain)
  ))
  measured <- .pcs_measure_pieces(sf::st_sf(geometry = outside))
  if (any(measured$polity_area_ha > .pcs_area_floor_ha())) {
    cli::cli_abort(
      "Reporting footprints extend outside the physical domain.",
      class = "whep_reporting_outside",
      outside = sf::st_drop_geometry(measured)
    )
  }
  sf::st_drop_geometry(measured)
}

.rs_check_footprints <- function(x) {
  overlaps <- .rs_overlap_pairs(x)
  if (nrow(overlaps) > 0L) {
    cli::cli_abort(
      c(
        "Selected source footprints overlap geometrically.",
        i = "Declare source alternatives or resolve the reporting-footprint policy.",
        i = "Affected pairs and hectares are recorded in the condition's overlap table."
      ),
      class = "whep_reporting_overlap",
      overlap = overlaps
    )
  }
}

.rs_overlap_pairs <- function(x) {
  candidates <- sf::st_intersects(x)
  purrr::map(seq_len(nrow(x)), function(i) {
    j <- candidates[[i]][candidates[[i]] > i]
    purrr::map(j, function(k) {
      pieces <- suppressWarnings(sf::st_intersection(
        sf::st_geometry(x)[i],
        sf::st_geometry(x)[k]
      ))
      measured <- .pcs_measure_pieces(sf::st_sf(geometry = pieces))
      tibble::tibble(
        left = x$polity_code[i],
        right = x$polity_code[k],
        overlap_ha = sum(measured$polity_area_ha),
        area_engine = paste(unique(measured$area_engine), collapse = ";")
      )
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows() |>
    ensure_columns(tibble::tibble(
      left = character(),
      right = character(),
      overlap_ha = double(),
      area_engine = character()
    )) |>
    dplyr::filter(.data$overlap_ha > .pcs_area_floor_ha())
}

.rs_measure <- function(selected, denominator, layers) {
  ice <- .pcs_prepare_ice(layers$ice)
  pieces <- selected |>
    .pcs_intersect_grid() |>
    .pcs_add_ice(ice) |>
    .pcs_split_intervals()
  domain <- denominator |>
    dplyr::transmute(
      cell_id = .data$cell_id,
      domain_area_ha = .data$polity_area_ha,
      domain_land_area_ha = .data$land_area_ha,
      domain_water_ha = .data$inland_water_ha,
      domain_ice_free_ha = pmax(.data$polity_area_ha - .data$ice_area_ha, 0),
      domain_water_excess_ha = .data$water_excess_ha
    )
  pieces |>
    dplyr::left_join(domain, by = "cell_id") |>
    dplyr::mutate(
      # Water and ice occupy disjoint physical compartments. Allocate the
      # domain's water over ice-free territory, so a member's ice cannot strand
      # water through independent post-allocation caps. This is distinct from
      # the legacy builder's territory-share-then-cap placement rule.
      water_fraction_ice_free = dplyr::if_else(
        .data$domain_ice_free_ha > 0,
        .data$domain_water_ha / .data$domain_ice_free_ha,
        0
      ),
      inland_water_ha = pmax(.data$polity_area_ha - .data$ice_area_ha, 0) *
        .data$water_fraction_ice_free,
      water_excess_ha = dplyr::if_else(
        .data$domain_ice_free_ha > 0,
        .data$domain_water_excess_ha *
          pmax(.data$polity_area_ha - .data$ice_area_ha, 0) /
          .data$domain_ice_free_ha,
        0
      )
    ) |>
    .pcs_finalize(
      "canonical_reporting_footprints",
      list(),
      .pcs_layers_supplied(layers$water, ice)
    ) |>
    dplyr::mutate(
      split_method = dplyr::if_else(
        .data$inland_water_ha > 0,
        "polygon_intersection+water_ice_free_pro_rata",
        "polygon_intersection"
      )
    )
}

.rs_audit <- function(support, denominator) {
  claimed <- support |>
    dplyr::summarise(
      reporting_land_ha = sum(.data$land_area_ha),
      measured_reporting_land_ha = sum(.data$measured_land_area_ha),
      area_adjustment_ha = sum(.data$area_adjustment_ha),
      .by = "cell_id"
    )
  denominator |>
    dplyr::select(
      "cell_id",
      domain_land_area_ha = "land_area_ha",
      domain_water_excess_ha = "water_excess_ha"
    ) |>
    dplyr::left_join(claimed, by = "cell_id") |>
    dplyr::mutate(
      reporting_land_ha = dplyr::coalesce(.data$reporting_land_ha, 0),
      measured_reporting_land_ha = dplyr::coalesce(
        .data$measured_reporting_land_ha,
        0
      ),
      area_adjustment_ha = dplyr::coalesce(.data$area_adjustment_ha, 0),
      unrepresented_land_ha = .data$domain_land_area_ha -
        .data$reporting_land_ha
    )
}


.rs_check_bundle <- function(bundle) {
  inventory <- .rs_inventory(bundle$inventory)
  year <- inventory$year[[1]]
  .rs_check_domain_input(bundle$domain, year)
  .rs_check_physical(bundle$domain_support, year)
  canonical <- bundle$canonical_geometries
  .check_columns(
    canonical,
    c("polity_code", "start_year", "end_year"),
    "canonical_geometries"
  )
  if (
    !inherits(canonical, "sf") ||
      !inherits(bundle$domain, "sf") ||
      !inherits(bundle$physical_domain, "sf")
  ) {
    cli::cli_abort("Bundle geometries are missing or malformed.")
  }
  rows <- canonical$polity_code %in% inventory$reporting_polity_code
  if (
    !.valid_native_years(canonical$start_year[rows]) ||
      !.valid_native_years(canonical$end_year[rows]) ||
      any(
        canonical$start_year[rows] > year | canonical$end_year[rows] <= year
      ) ||
      anyDuplicated(canonical$polity_code[rows]) ||
      !setequal(canonical$polity_code[rows], inventory$reporting_polity_code) ||
      !.valid_native_years(bundle$domain$year) ||
      any(bundle$domain$year != year)
  ) {
    cli::cli_abort("Bundle geometry identities or annual scope disagree.")
  }
  codes <- inventory$reporting_polity_code[inventory$use == "reporting"]
  .rs_check_rows(bundle$support, inventory, codes, "partition")
  .rs_check_rows(
    bundle$footprints,
    inventory,
    inventory$reporting_polity_code,
    "overlap"
  )
  .rs_check_domain_links(bundle)
  expected <- bundle$footprints[bundle$footprints$polity_code %in% codes, ]
  expected$support_role <- "partition"
  expected <- .rs_close_support(expected)
  if (
    !identical(expected, bundle$support) ||
      !identical(.rs_audit(bundle$support, bundle$domain_support), bundle$audit)
  ) {
    cli::cli_abort("Bundle selected footprints or domain audit are incomplete.")
  }
  invisible(bundle)
}

.rs_check_rows <- function(x, inventory, codes, role) {
  fields <- c(
    "cell_id",
    "polity_code",
    "source",
    "start_year",
    "end_year",
    "land_area_ha",
    "polity_area_ha",
    "domain_land_area_ha",
    "domain_area_ha",
    "support_role",
    "partition_method"
  )
  .check_columns(x, fields, "support")
  key <- c("cell_id", "polity_code", "start_year", "end_year")
  if (
    nrow(x) == 0L ||
      !.valid_native_years(x$start_year) ||
      !.valid_native_years(x$end_year) ||
      any(
        x$start_year != inventory$year[[1]] |
          x$end_year != inventory$year[[1]] + 1L
      ) ||
      anyNA(x$polity_code) ||
      !setequal(x$polity_code, codes) ||
      anyNA(x$source) ||
      any(x$source != inventory$source[[1]]) ||
      anyNA(x$support_role) ||
      any(x$support_role != role) ||
      anyNA(x$partition_method) ||
      any(x$partition_method != "explicit_reporting_inventory") ||
      anyDuplicated(x[key])
  ) {
    cli::cli_abort("Support identities, roles or annual scope disagree.")
  }
  areas <- x[c(
    "land_area_ha",
    "polity_area_ha",
    "domain_land_area_ha",
    "domain_area_ha"
  )]
  valid <- vapply(
    areas,
    function(a) {
      is.numeric(a) &&
        all(is.finite(a)) &&
        all(a >= 0)
    },
    logical(1)
  )
  if (!all(valid)) {
    cli::cli_abort("Support contains malformed areas.")
  }
  invisible(x)
}


.rs_check_physical <- function(x, year) {
  fields <- c(
    "cell_id",
    "start_year",
    "end_year",
    "polity_area_ha",
    "land_area_ha",
    "inland_water_ha",
    "ice_area_ha",
    "water_excess_ha",
    "layers_supplied"
  )
  .check_columns(x, fields, "domain_support")
  if (
    nrow(x) == 0L ||
      anyNA(x$cell_id) ||
      anyDuplicated(x$cell_id) ||
      !.valid_native_years(x$start_year) ||
      !.valid_native_years(x$end_year) ||
      any(x$start_year != year | x$end_year != year + 1L) ||
      anyNA(x$layers_supplied) ||
      any(!nzchar(x$layers_supplied))
  ) {
    cli::cli_abort(
      "Physical-domain cells need explicit provenance and annual keys."
    )
  }
  areas <- c(
    "polity_area_ha",
    "land_area_ha",
    "inland_water_ha",
    "ice_area_ha",
    "water_excess_ha"
  )
  valid <- vapply(
    x[areas],
    function(a) {
      is.numeric(a) &&
        all(is.finite(a)) &&
        all(a >= 0)
    },
    logical(1)
  )
  if (
    !all(valid) ||
      any(
        x$land_area_ha !=
          pmax(x$polity_area_ha - x$inland_water_ha - x$ice_area_ha, 0)
      )
  ) {
    cli::cli_abort(
      "Physical-domain components disagree with the supplied land."
    )
  }
  invisible(x)
}


# Invoked only after exact footprint overlap and domain-containment checks.
# The existing polycell resolution is a representation bound, not permission to
# repair geographic overlap. Keep measured areas and the numerical correction.
.rs_close_support <- function(x) {
  x |>
    dplyr::mutate(
      measured_land_area_ha = .data$land_area_ha,
      land_area_ha = .rs_close_cell(
        .data$land_area_ha,
        .data$domain_land_area_ha
      ),
      .by = "cell_id"
    ) |>
    dplyr::mutate(
      area_adjustment_ha = .data$measured_land_area_ha - .data$land_area_ha,
      area_adjustment_method = dplyr::if_else(
        .data$area_adjustment_ha > 0,
        "proportional_resolution_closure",
        "none"
      )
    )
}

.rs_close_cell <- function(area, domain) {
  capacity <- unique(domain)
  total <- sum(area)
  if (
    length(capacity) != 1L ||
      !is.finite(capacity) ||
      capacity < 0 ||
      any(!is.finite(area) | area < 0) ||
      total - capacity > .pcs_area_floor_ha()
  ) {
    cli::cli_abort(
      "Reporting land exceeds its independently measured domain.",
      class = "whep_reporting_capacity"
    )
  }
  if (total <= capacity) {
    return(area)
  }
  closed <- area * (capacity / total)
  if (sum(closed) > capacity) {
    # A common floating-point step preserves relative shares and avoids choosing
    # one polity to absorb the last arithmetic ULP. No spatial tolerance changes.
    closed <- closed * (1 - .Machine$double.eps)
  }
  if (
    sum(closed) > capacity ||
      sum(area - closed) > .pcs_area_floor_ha()
  ) {
    cli::cli_abort(
      "Numerical closure exceeds the existing support resolution.",
      class = "whep_reporting_capacity"
    )
  }
  closed
}

.rs_check_domain_links <- function(bundle) {
  domain <- bundle$domain_support
  fields <- c(
    "polity_area_ha",
    "land_area_ha",
    "inland_water_ha",
    "ice_area_ha",
    "water_excess_ha",
    "layers_supplied"
  )
  .check_columns(domain, c("cell_id", fields), "domain_support")
  expected <- tibble::tibble(
    domain_area_ha = domain$polity_area_ha,
    domain_land_area_ha = domain$land_area_ha,
    domain_water_ha = domain$inland_water_ha,
    domain_ice_free_ha = pmax(domain$polity_area_ha - domain$ice_area_ha, 0),
    domain_water_excess_ha = domain$water_excess_ha,
    layers_supplied = domain$layers_supplied
  )
  purrr::walk(c("support", "footprints"), function(member) {
    x <- bundle[[member]]
    .check_columns(x, c("cell_id", names(expected)), member)
    .rs_check_row_components(x)
    index <- match(x$cell_id, domain$cell_id)
    if (
      anyNA(index) ||
        anyNA(x$cell_id) ||
        !identical(as.list(x[names(expected)]), as.list(expected[index, ]))
    ) {
      cli::cli_abort(
        "Reporting cells disagree with independent domain components."
      )
    }
  })
  invisible(bundle)
}

.rs_check_row_components <- function(x) {
  fields <- c(
    "polity_area_ha",
    "measured_land_area_ha",
    "inland_water_ha",
    "ice_area_ha",
    "water_excess_ha"
  )
  .check_columns(x, fields, "reporting components")
  valid <- vapply(
    x[fields],
    function(a) {
      is.numeric(a) && all(is.finite(a)) && all(a >= 0)
    },
    logical(1)
  )
  if (
    !all(valid) ||
      any(
        x$measured_land_area_ha !=
          pmax(x$polity_area_ha - x$inland_water_ha - x$ice_area_ha, 0)
      )
  ) {
    cli::cli_abort("Reporting components disagree with measured land.")
  }
  invisible(x)
}
