# Production driver behind inst/scripts/run_sjos_nitrogen.R. It runs
# build_sjos_nitrogen() one year at a time over the year-partitioned gridded
# nitrogen balance (`whep_n_balance_grid/year=<Y>/part.parquet` and its run
# manifest `whep_n_balance_run_manifest.json`) and writes the surplus-mode
# boundary, the country-year table, the classification and the nourishment
# axis as year partitions, with one run manifest per option set.
#
# The logic lives here rather than in the script, as R/nbd_stage.R does for
# run_nitrogen_balance.R, so that it can be tested on fixtures: the script only
# parses its arguments and calls .sjr_run().
#
# What the driver adds to build_sjos_nitrogen() is bookkeeping that a long
# unattended run needs and that must fail closed:
# * every year is reconciled before anything of it is written: the country
#   sums plus the exceedance left unallocated on residual records equal the
#   summed cell exceedance, and the grid, country, country-table and
#   classification tables agree with one another;
# * the input balance is tied to its manifest (row counts per year and the
#   SHA-256 of the manifest's bytes), so outputs cannot be read against a
#   different balance than the one they were built from;
# * one output root holds one option set: a second option set writes under its
#   own arm directory, and outputs written under other options, another WHEP
#   commit or another balance manifest are refused rather than mixed;
# * the share of the grid's nitrogen input that the balance spread uniformly
#   over a polity's cropland because its crop had no crop-pattern cell (issue
#   #533) is reported per year from the balance run's own captured warnings.
#
# Pathway mode is not run: the per-cell loss drivers it needs are tracked in
# issue #359, so pathway mode aborts.

# ---- Layout ----------------------------------------------------------------

# Year-partitioned products, `<root>/<product>/year=<Y>/part.parquet`.
.sjr_products <- function() {
  c(
    country_crop = "whep_sjos_n",
    grid = "whep_sjos_n_grid",
    country = "whep_sjos_n_country",
    class = "whep_sjos_n_class",
    nourishment = "whep_sjos_n_nourishment",
    diagnostics = "whep_sjos_n_diag"
  )
}

# What each product holds, written into the manifest for its readers.
.sjr_product_grain <- function() {
  c(
    country_crop = paste(
      "build_n_boundary_exceedance() at resolution = 'country': one row per",
      "country, crop and year"
    ),
    grid = paste(
      "build_n_boundary_exceedance() at resolution = 'grid': one row per",
      "cell, polity, crop (and residual record) and year, with",
      "binding_threshold and binding_matches_mi"
    ),
    country = paste(
      "build_n_boundary_country() country table: one row per country and",
      "year"
    ),
    class = "classify_sjos_n(): one row per country, crop and year",
    nourishment = "normalize_nourishment() axis: one row per country and year",
    diagnostics = paste(
      "build_n_boundary_country() diagnostics plus the driver's",
      "reconciliation and uniform-spread diagnostics: one row per year"
    )
  )
}

# The 2010 binding-threshold table of the run's land-use scope, the same for
# every year, written once: `<root>/whep_sjos_n_critical_binding/part.parquet`.
.sjr_binding_product <- function() "whep_sjos_n_critical_binding"

.sjr_manifest_name <- function() "whep_sjos_n_run_manifest.json"

.sjr_march_manifest_name <- function() "whep_n_balance_run_manifest.json"

.sjr_march_product <- function() "whep_n_balance_grid"

.sjr_arms_dir <- function() "whep_sjos_n_arms"

.sjr_schema_version <- function() 1L

.sjr_partition_rel <- function(product, year) {
  file.path(product, sprintf("year=%d", as.integer(year)), "part.parquet")
}

.sjr_partition_path <- function(root, product, year) {
  file.path(root, .sjr_partition_rel(product, year))
}

# ---- Options ---------------------------------------------------------------

#' Validate the options of one SJOS-N production run.
#'
#' @param land_use Land-use scope of the critical-surplus comparison, `"all"`
#'   (default) or `"ara"`.
#' @param nourishment_thresholds Nourishment band, `"composed"` (default) or
#'   `"flat"`.
#' @param negative_critical Treatment of negative critical surpluses,
#'   `"clamp"` (default) or `"keep"`.
#' @param grassland_split Grassland treatment under `land_use = "all"`,
#'   `"image_density"` (default) or `"none"`.
#' @param boundary_mode `"surplus"`; `"pathway"` aborts (issue #359).
#' @param beyond_share_cut Country boundary-side cut in `[0, 1)`.
#' @return A named list of run options.
#' @noRd
.sjr_options <- function(
  land_use = "all",
  nourishment_thresholds = "composed",
  negative_critical = "clamp",
  grassland_split = "image_density",
  boundary_mode = "surplus",
  beyond_share_cut = 0.5
) {
  if (identical(boundary_mode, "pathway")) {
    cli::cli_abort(
      c(
        "Pathway mode is not supported by this driver yet.",
        i = "It needs per-cell nitrogen-loss drivers (issue #359); run
             {.code boundary_mode = \"surplus\"}."
      ),
      class = "whep_sjr_pathway_unsupported"
    )
  }
  .nbc_check_cut(beyond_share_cut)
  list(
    boundary_mode = rlang::arg_match0(boundary_mode, "surplus"),
    surplus_method = "harvest_removal",
    critical_var = "critical_n_surplus",
    critical_threshold = "mi",
    critical_reference_year = 2010L,
    land_use = rlang::arg_match0(land_use, c("all", "ara")),
    grassland_split = rlang::arg_match0(
      grassland_split,
      c("image_density", "none")
    ),
    negative_critical = rlang::arg_match0(
      negative_critical,
      c("clamp", "keep")
    ),
    nourishment_thresholds = rlang::arg_match0(
      nourishment_thresholds,
      c("composed", "flat")
    ),
    nourishment_band = list(),
    beyond_share_cut = beyond_share_cut
  )
}

# The primary option set writes at the output root itself; every other set
# writes under its own arm directory, so a sensitivity run can never overwrite
# or mix with the primary outputs.
.sjr_is_primary <- function(options) {
  primary <- .sjr_options()
  keys <- c(
    "land_use",
    "grassland_split",
    "negative_critical",
    "nourishment_thresholds",
    "beyond_share_cut"
  )
  identical(options[keys], primary[keys])
}

.sjr_arm_id <- function(options) {
  paste0(
    "land_use-",
    options$land_use,
    "_grassland_split-",
    options$grassland_split,
    "_negative_critical-",
    options$negative_critical,
    "_nourishment-",
    options$nourishment_thresholds,
    "_cut-",
    format(options$beyond_share_cut)
  )
}

.sjr_arm_root <- function(out_root, options) {
  if (.sjr_is_primary(options)) {
    return(out_root)
  }
  file.path(out_root, .sjr_arms_dir(), .sjr_arm_id(options))
}

# The defaults build_sjos_nitrogen() leaves to each builder, read from the
# builders themselves so the manifest names what actually ran.
.sjr_resolved_defaults <- function(options) {
  common <- list(
    food_supply_method = "whep_native",
    surplus_method = options$surplus_method
  )
  if (options$nourishment_thresholds == "flat") {
    return(c(common, list(nourishment_band = "flat pair, no band terms")))
  }
  c(
    common,
    list(
      loss_wedge_method = .sjr_default(build_loss_wedge, "method"),
      loss_wedge_coverage = .sjr_default(build_loss_wedge, "coverage"),
      protein_quality_method = .sjr_default(build_protein_quality, "method"),
      protein_quality_variant = .sjr_default(build_protein_quality, "variant"),
      band_shortfall = .sjr_default(build_nourishment_band, "shortfall"),
      band_ceiling = .sjr_default(build_nourishment_band, "ceiling"),
      band_requirement_sd = .sjr_default(
        build_nourishment_band,
        "requirement_sd"
      )
    )
  )
}

# A function's default for `arg`: the first of a choice vector, else the value.
.sjr_default <- function(fn, arg) {
  value <- eval(formals(fn)[[arg]])
  if (is.character(value)) value[[1L]] else value
}

# ---- Command line ----------------------------------------------------------

# `--name=value` arguments of inst/scripts/run_sjos_nitrogen.R. An unknown
# argument aborts rather than being ignored, so a mistyped option cannot run
# the default and be reported as an arm.
.sjr_parse_args <- function(args) {
  out <- list(
    years = NULL,
    march_root = NULL,
    out_root = NULL,
    force = FALSE,
    options = list()
  )
  out <- purrr::reduce(args, .sjr_parse_one, .init = out)
  out$options <- rlang::exec(.sjr_options, !!!out$options)
  out
}

.sjr_cli_options <- function() {
  c(
    "land-use" = "land_use",
    "nourishment" = "nourishment_thresholds",
    "negative-critical" = "negative_critical",
    "grassland-split" = "grassland_split",
    "boundary-mode" = "boundary_mode",
    "beyond-share-cut" = "beyond_share_cut"
  )
}

.sjr_parse_one <- function(out, arg) {
  if (identical(arg, "--force")) {
    out$force <- TRUE
    return(out)
  }
  parts <- regmatches(arg, regexec("^--([a-z-]+)=(.*)$", arg))[[1L]]
  name <- if (length(parts) == 3L) parts[[2L]] else ""
  value <- if (length(parts) == 3L) parts[[3L]] else ""
  options <- .sjr_cli_options()
  if (name == "years") {
    out$years <- .sjr_parse_years(value)
  } else if (name %in% c("march-root", "out-root")) {
    out[[sub("-", "_", name, fixed = TRUE)]] <- value
  } else if (name == "beyond-share-cut") {
    out$options$beyond_share_cut <- as.numeric(value)
  } else if (name %in% names(options)) {
    out$options[[options[[name]]]] <- value
  } else {
    cli::cli_abort(
      "Unknown argument {.val {arg}}.",
      class = "whep_sjr_bad_argument"
    )
  }
  out
}

# "1961:2023", "2010", or a comma list of either.
.sjr_parse_years <- function(value) {
  bits <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  years <- unlist(purrr::map(bits, \(bit) {
    ends <- suppressWarnings(as.integer(strsplit(bit, ":", fixed = TRUE)[[1L]]))
    if (length(ends) == 2L && !anyNA(ends) && ends[[1L]] <= ends[[2L]]) {
      seq.int(ends[[1L]], ends[[2L]])
    } else if (length(ends) == 1L && !anyNA(ends)) {
      ends
    } else {
      NA_integer_
    }
  }))
  if (length(years) == 0L || anyNA(years)) {
    cli::cli_abort(
      "{.arg --years} must be years or ranges like 1961:2023, not
       {.val {value}}.",
      class = "whep_sjr_bad_argument"
    )
  }
  sort(unique(years))
}

# ---- Input balance ---------------------------------------------------------

# The balance run's manifest: its SHA-256 (over the file's bytes), the grid
# partitions it vouches for and their row counts, and the per-year driver
# report whose captured warnings carry the uniform-spread mass.
.sjr_read_march <- function(march_root) {
  rlang::check_installed("jsonlite", "to read and write run manifests")
  path <- file.path(march_root, .sjr_march_manifest_name())
  if (!file.exists(path)) {
    cli::cli_abort(
      c(
        "No gridded nitrogen-balance run manifest at {.file {path}}.",
        i = "Point {.arg march_root} at the root holding
             {.file {(.sjr_march_product())}/} and its manifest."
      ),
      class = "whep_sjr_no_march"
    )
  }
  manifest <- jsonlite::read_json(path, simplifyVector = FALSE)
  grid <- purrr::keep(
    manifest$partitions %||% list(),
    \(part) identical(part$resolution, "grid")
  )
  rows <- tibble::tibble(
    year = purrr::map_int(grid, \(part) as.integer(part$year)),
    rows = purrr::map_int(grid, \(part) as.integer(part$rows))
  )
  if (nrow(rows) == 0L) {
    cli::cli_abort(
      "The balance manifest {.file {path}} lists no grid partition.",
      class = "whep_sjr_no_march"
    )
  }
  if (anyDuplicated(rows$year) > 0L) {
    cli::cli_abort(
      "The balance manifest {.file {path}} lists a grid year twice.",
      class = "whep_sjr_no_march"
    )
  }
  list(
    path = normalizePath(path, winslash = "/"),
    sha256 = unname(tools::sha256sum(path)),
    whep_commit = manifest$whep_commit %||% NA_character_,
    schema_version = manifest$schema_version %||% NA_integer_,
    grid = rows,
    driver_report = manifest$driver_report %||% list(),
    subnational = .sjr_subnational(manifest)
  )
}

# Whatever the balance manifest records about the subnational refinement of
# crop area (granted per country, off by default): every field whose name
# mentions "subnational", copied with its path. The driver applies nothing
# itself; it only carries the record into its own manifest.
.sjr_subnational <- function(manifest) {
  hits <- .sjr_find_named(manifest, "subnational")
  if (length(hits) == 0L) {
    return("not recorded in the balance manifest")
  }
  hits
}

.sjr_find_named <- function(x, pattern, path = character()) {
  if (!is.list(x)) {
    return(list())
  }
  keys <- names(x) %||% rep("", length(x))
  keys[keys == ""] <- as.character(which(keys == ""))
  hits <- purrr::map2(unname(x), keys, \(value, key) {
    here <- c(path, key)
    if (grepl(pattern, key, ignore.case = TRUE)) {
      return(stats::setNames(list(value), paste(here, collapse = ".")))
    }
    .sjr_find_named(value, pattern, here)
  })
  purrr::list_flatten(hits)
}

# Default: 1961 to the last grid year the balance manifest lists. A requested
# year the manifest does not list aborts, naming the years.
.sjr_years <- function(years, march) {
  available <- march$grid$year
  years <- sort(unique(as.integer(years %||% seq.int(1961L, max(available)))))
  missing <- setdiff(years, available)
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "{length(missing)} requested year{?s} {?has/have} no grid balance
         partition in the manifest: {.val {missing}}.",
        i = "The manifest lists {length(available)} grid year{?s}."
      ),
      class = "whep_sjr_missing_years"
    )
  }
  years
}

.sjr_read_balance <- function(march_root, year, expected_rows) {
  path <- .sjr_partition_path(march_root, .sjr_march_product(), year)
  if (!file.exists(path)) {
    cli::cli_abort(
      "The grid balance partition {.file {path}} is missing.",
      class = "whep_sjr_balance_mismatch"
    )
  }
  balance <- tibble::as_tibble(arrow::read_parquet(path))
  if (nrow(balance) != expected_rows || !all(balance$year == year)) {
    cli::cli_abort(
      c(
        "The grid balance partition {.file {path}} is not the one its
         manifest lists.",
        x = "{nrow(balance)} row{?s} read, {expected_rows} listed; years
             {.val {unique(balance$year)}}."
      ),
      class = "whep_sjr_balance_mismatch"
    )
  }
  balance
}

# Food tonnes per country and crop from the commodity balances, the
# `data$cbs_food` input of build_food_supply(). Live-animal rows are counted
# in heads and must carry no food: tonnes and heads are never added.
.sjr_cbs_food <- function(year, cbs = get_wide_cbs(years = year)) {
  .check_columns(
    cbs,
    c("year", "area_code", "item_cbs_code", "unit", "food"),
    "cbs"
  )
  cbs <- dplyr::filter(cbs, .data$year == .env$year)
  heads_food <- dplyr::filter(
    cbs,
    .data$unit != "tonnes",
    dplyr::coalesce(.data$food, 0) != 0
  )
  if (nrow(heads_food) > 0L) {
    cli::cli_abort(
      "{nrow(heads_food)} commodity-balance row{?s} not in tonnes carr{?ies/y}
       food; food in other units cannot be added to tonnes.",
      class = "whep_sjr_food_units"
    )
  }
  cbs |>
    dplyr::filter(.data$unit == "tonnes") |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$area_code,
      item_cbs_code = .data$item_cbs_code,
      food_t = .data$food
    )
}

# ---- Diagnostics -----------------------------------------------------------

# Issue #533: nitrogen of a polity-crop with no crop-pattern cell is spread
# uniformly over the polity's cropland, and the balance says so in a warning
# (.n_warn_unmatched(), R/n_balance_spatialize.R) that the balance run captures
# per stage into its manifest's `driver_report.<year>`. The mass those warnings
# report is summed and set against the grid's standard nitrogen input. A
# warning of that kind whose numbers cannot be read aborts rather than counting
# as zero; a year the manifest has no report for is "not_recorded", never zero.
.sjr_uniform_spread <- function(driver_report, year, grid_input_std_n_t) {
  messages <- .sjr_grid_messages(driver_report[[as.character(year)]])
  if (is.null(messages)) {
    return(tibble::tibble(
      year = as.integer(year),
      uniform_spread_status = "not_recorded",
      uniform_spread_n_t = NA_real_,
      uniform_spread_polity_crops = NA_integer_,
      uniform_spread_warnings = NA_integer_,
      grid_input_std_n_t = grid_input_std_n_t,
      uniform_spread_share_of_input = NA_real_
    ))
  }
  hits <- messages[grepl(
    "had no crop-pattern grid cells",
    messages,
    fixed = TRUE
  )]
  pattern <- paste0(
    "([0-9]+) polity-crop totals? \\(([-+0-9.eE]+) t N\\) had no",
    " crop-pattern grid cells;\\s+reallocating\\s+uniformly"
  )
  parsed <- regmatches(hits, regexec(pattern, hits))
  n_t <- suppressWarnings(as.numeric(purrr::map_chr(parsed, \(m) {
    m[3L] %||% NA
  })))
  crops <- suppressWarnings(as.integer(purrr::map_chr(parsed, \(m) {
    m[2L] %||% NA
  })))
  if (anyNA(n_t) || anyNA(crops)) {
    cli::cli_abort(
      c(
        "A uniform-spread warning of the {year} balance run cannot be read.",
        i = "Expected {.val {pattern}}."
      ),
      class = "whep_sjr_uniform_unparsed"
    )
  }
  spread <- sum(n_t)
  tibble::tibble(
    year = as.integer(year),
    uniform_spread_status = "recorded",
    uniform_spread_n_t = spread,
    uniform_spread_polity_crops = sum(crops),
    uniform_spread_warnings = length(hits),
    grid_input_std_n_t = grid_input_std_n_t,
    uniform_spread_share_of_input = dplyr::if_else(
      grid_input_std_n_t > 0,
      spread / grid_input_std_n_t,
      NA_real_
    )
  )
}

# The messages the grid balance raised in one year's report: its own stages
# when the balance run's primary resolution was the grid, else the conditions
# it captured while building the grid as its second resolution. NULL when the
# year has no report.
.sjr_grid_messages <- function(report) {
  if (is.null(report)) {
    return(NULL)
  }
  conditions <- if (identical(report$resolution, "grid")) {
    if (is.null(report$stages)) {
      return(NULL)
    }
    purrr::list_flatten(purrr::map(report$stages, \(s) {
      s$conditions %||% list()
    }))
  } else {
    grid <- report$second_resolution_conditions$grid
    if (is.null(grid)) {
      return(NULL)
    }
    grid
  }
  messages <- purrr::map_chr(
    conditions,
    \(cnd) as.character(cnd$message %||% NA_character_)
  )
  cli::ansi_strip(messages[!is.na(messages)])
}

# Every identity the year's tables must satisfy before any of them is written.
# The country sums plus the exceedance left on residual records equal the
# summed cell exceedance (counted once per cell); the country-resolution
# boundary, the country table and the classification carry the same total as
# the grid's crop rows; and the country table's own diagnostics agree. A gap
# beyond `tolerance` relative to the cell total, or a missing value, aborts.
.sjr_reconcile <- function(out, year, tolerance = 1e-8) {
  grid <- out$boundary_surplus$grid
  valid <- dplyr::filter(grid, .data$coverage_state == "valid")
  crop <- dplyr::filter(
    valid,
    .data$attribution_record_type == "crop_allocation"
  )
  cell_total <- valid |>
    dplyr::distinct(.data$cell_id, .keep_all = TRUE) |>
    dplyr::pull("cell_positive_overshoot_n_t") |>
    sum()
  grid_crop <- sum(crop$exceedance_n_t)
  grid_unallocated <- sum(valid$unallocated_positive_overshoot_n_t)
  diagnostics <- out$country_table$diagnostics
  if (nrow(diagnostics) != 1L) {
    cli::cli_abort(
      "The {year} country table has {nrow(diagnostics)} diagnostic rows, not
       one.",
      class = "whep_sjr_unreconciled"
    )
  }
  gaps <- c(
    cells = grid_crop + grid_unallocated - cell_total,
    country_resolution = sum(
      out$boundary_surplus$country$exceedance_n_t,
      na.rm = TRUE
    ) -
      grid_crop,
    country_table = sum(out$country_table$country$exceedance_n_t) - grid_crop,
    classification = sum(out$sjos_class$exceedance_n_t, na.rm = TRUE) -
      grid_crop,
    diagnostics_cells = diagnostics$cell_exceedance_n_t - cell_total,
    diagnostics_unallocated = diagnostics$unallocated_exceedance_n_t -
      grid_unallocated,
    diagnostics_gap = diagnostics$exceedance_gap_n_t
  )
  bad <- is.na(gaps) | abs(gaps) > tolerance * max(1, abs(cell_total))
  if (any(bad)) {
    cli::cli_abort(
      c(
        "The {year} SJOS-N tables do not reconcile; nothing of {year} was
         written.",
        x = "{names(gaps)[bad]}: gap {.val {unname(gaps[bad])}} t N."
      ),
      class = "whep_sjr_unreconciled"
    )
  }
  tibble::tibble(
    year = as.integer(year),
    reconciliation_status = "pass",
    reconciliation_tolerance = tolerance,
    reconciliation_cell_exceedance_n_t = cell_total,
    reconciliation_crop_exceedance_n_t = grid_crop,
    reconciliation_unallocated_n_t = grid_unallocated,
    reconciliation_max_abs_gap_n_t = max(abs(gaps))
  )
}

# ---- Writing ---------------------------------------------------------------

# The product tables of one year. The nourishment band is stamped on the three
# tables that depend on it; the boundary tables already carry their own
# negative_critical, land_use and grassland_split stamps.
.sjr_tables <- function(out, diagnostics, options) {
  band <- options$nourishment_thresholds
  tables <- list(
    country_crop = out$boundary_surplus$country,
    grid = out$boundary_surplus$grid,
    country = out$country_table$country |>
      .sjr_add_population(out$nourishment) |>
      .sjr_stamp_band(band),
    class = .sjr_stamp_band(out$sjos_class, band),
    nourishment = .sjr_stamp_band(out$nourishment, band),
    diagnostics = diagnostics
  )
  empty <- names(tables)[purrr::map_int(tables, nrow) == 0L]
  if (length(empty) > 0L) {
    cli::cli_abort(
      "Empty SJOS-N table{?s} {.val {empty}}; nothing was written.",
      class = "whep_sjr_empty_output"
    )
  }
  tables
}

# Population per country-year from the nourishment table: the denominator
# build_sjos_nitrogen() divided the food supply by (read_population() at its
# own default composition unless data$population was supplied, named in
# `method_population`). A country-year without a nourishment row keeps NA.
.sjr_add_population <- function(country, nourishment) {
  population <- dplyr::distinct(
    nourishment,
    .data$year,
    .data$area_code,
    .data$population,
    .data$method_population
  )
  dplyr::left_join(
    country,
    population,
    by = c("year", "area_code"),
    relationship = "many-to-one"
  )
}

.sjr_stamp_band <- function(x, band) {
  dplyr::mutate(x, nourishment_thresholds = .env$band)
}

.sjr_write_year <- function(tables, root, year) {
  products <- .sjr_products()
  purrr::imap(tables, \(table, key) {
    rel <- .sjr_partition_rel(products[[key]], year)
    written <- write_table_checked(
      table,
      file.path(root, rel),
      format = "parquet",
      overwrite = TRUE
    )
    list(
      year = as.integer(year),
      product = products[[key]],
      path = rel,
      rows = as.integer(written$n_rows),
      md5 = unname(written$md5)
    )
  }) |>
    unname()
}

.sjr_write_binding <- function(binding, root) {
  rel <- file.path(.sjr_binding_product(), "part.parquet")
  written <- write_table_checked(
    binding,
    file.path(root, rel),
    format = "parquet",
    overwrite = TRUE
  )
  list(
    product = .sjr_binding_product(),
    path = rel,
    rows = as.integer(written$n_rows),
    md5 = unname(written$md5)
  )
}

# ---- Manifest --------------------------------------------------------------

# The commit of the WHEP checkout the run loaded, and whether its tree was
# clean. A run that cannot name its commit is refused.
.sjr_whep_state <- function(repo = ".") {
  sha <- .sjr_git(repo, c("rev-parse", "HEAD"))
  status <- .sjr_git(repo, c("status", "--porcelain"))
  if (
    !is.null(attr(sha, "status")) ||
      length(sha) != 1L ||
      !grepl("^[0-9a-f]{40}$", sha)
  ) {
    cli::cli_abort(
      "Cannot read the WHEP commit of {.file {repo}}; run from a git
       checkout.",
      class = "whep_sjr_no_sha"
    )
  }
  list(
    sha = sha,
    clean = is.null(attr(status, "status")) && length(status) == 0L
  )
}

.sjr_git <- function(repo, args) {
  suppressWarnings(system2(
    "git",
    c("-C", .sjr_shell_quote(repo), args),
    stdout = TRUE,
    stderr = FALSE
  ))
}

.sjr_shell_quote <- function(x) {
  shQuote(x, type = if (.Platform$OS.type == "windows") "cmd" else "sh")
}

# The fields a later run must share to add years to the same outputs.
.sjr_identity <- function(manifest) {
  list(
    schema_version = manifest$schema_version,
    whep_sha = manifest$whep_sha,
    input_march_manifest_hash = manifest$input_march_manifest_hash,
    options = manifest$options
  )
}

.sjr_manifest_header <- function(whep, march, options) {
  list(
    schema_version = .sjr_schema_version(),
    generator = "inst/scripts/run_sjos_nitrogen.R",
    whep_sha = whep$sha,
    whep_tree_clean = whep$clean,
    whep_version = as.character(utils::packageVersion("whep")),
    r_version = R.version.string,
    input_march_manifest_hash = march$sha256,
    input_march_manifest = list(
      path = march$path,
      hash = "sha256 of the manifest file's bytes",
      whep_commit = march$whep_commit,
      schema_version = march$schema_version,
      subnational = march$subnational
    ),
    options = options,
    resolved_defaults = .sjr_resolved_defaults(options),
    population = list(
      column = "population in whep_sjos_n_country and whep_sjos_n_nourishment",
      source = paste(
        "the nourishment denominator of build_sjos_nitrogen(): read_population()",
        "at its default composition (method_population = read_population)"
      ),
      population_source = .sjr_default(read_population, "population_source"),
      territory_overlap = .sjr_default(read_population, "territory_overlap")
    ),
    boundary_modes = list(
      surplus = "run",
      pathway = "not supported: waits on issue #359"
    ),
    declared_departures = if (options$negative_critical == "clamp") {
      list(
        negative_critical = paste(
          "negative critical surpluses clamped to zero before the",
          "exceedance, a departure from Schulte-Uebbing et al. (2022), whose",
          "critical-surplus layer keeps negative values"
        )
      )
    } else {
      list()
    },
    arm = list(
      primary = .sjr_is_primary(options),
      id = .sjr_arm_id(options)
    ),
    products = purrr::imap(.sjr_products(), \(dir, key) {
      list(
        dir = dir,
        partition = "year=<YYYY>/part.parquet",
        grain = .sjr_product_grain()[[key]]
      )
    }) |>
      unname()
  )
}

# The manifest already at `root`, checked against this run. A manifest written
# under other options, another commit or another balance aborts, and so do
# product partitions with no manifest at all: adding years to either would mix
# two constructions under one set of directories.
.sjr_previous_manifest <- function(root, header) {
  path <- file.path(root, .sjr_manifest_name())
  if (!file.exists(path)) {
    stray <- unlist(purrr::map(
      file.path(root, .sjr_products()),
      \(dir) Sys.glob(file.path(dir, "year=*", "part.parquet"))
    ))
    if (length(stray) > 0L) {
      cli::cli_abort(
        c(
          "{.file {root}} holds {length(stray)} SJOS-N partition{?s} with no
           run manifest.",
          i = "Remove them or choose another output root."
        ),
        class = "whep_sjr_unmanaged_output"
      )
    }
    return(NULL)
  }
  previous <- jsonlite::read_json(path, simplifyVector = FALSE)
  mine <- jsonlite::toJSON(
    .sjr_identity(header),
    auto_unbox = TRUE,
    digits = NA
  )
  theirs <- jsonlite::toJSON(
    .sjr_identity(previous),
    auto_unbox = TRUE,
    digits = NA
  )
  if (!identical(mine, theirs)) {
    cli::cli_abort(
      c(
        "{.file {path}} was written by a different run (options, WHEP commit
         or balance manifest).",
        i = "Choose another output root, or remove the old outputs first."
      ),
      class = "whep_sjr_incompatible_output"
    )
  }
  previous
}

.sjr_year_done <- function(state, root, year) {
  recorded <- purrr::keep(state$partitions, \(p) identical(p$year, year))
  done <- unlist(purrr::map(recorded, "product"))
  all(.sjr_products() %in% done) &&
    all(file.exists(file.path(root, unlist(purrr::map(recorded, "path")))))
}

# This run's year replaces whatever the state held for it.
.sjr_merge_year <- function(state, year_record) {
  year <- year_record$year
  kept <- purrr::discard(state$partitions, \(p) identical(p$year, year))
  state$partitions <- c(kept, year_record$partitions)
  state$partitions <- state$partitions[order(
    purrr::map_int(state$partitions, "year"),
    purrr::map_chr(state$partitions, "product")
  )]
  state$diagnostics[[as.character(year)]] <- year_record$diagnostics
  state$diagnostics <- state$diagnostics[order(names(state$diagnostics))]
  state$years <- as.integer(names(state$diagnostics))
  state
}

.sjr_write_manifest <- function(state, root) {
  state$built_at <- format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
  path <- file.path(root, .sjr_manifest_name())
  tmp <- paste0(path, ".tmp")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    state,
    tmp,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA,
    null = "null",
    na = "null"
  )
  if (file.exists(path)) {
    unlink(path)
  }
  if (!file.rename(tmp, path)) {
    cli::cli_abort("Could not install the run manifest {.file {path}}.")
  }
  invisible(path)
}

# ---- Run -------------------------------------------------------------------

# The readers of the run's inputs beyond the balance. `extra(year)` returns
# further `build_sjos_nitrogen()` data entries for that year; the production
# readers add none, so population, the band terms and the agricultural land
# are read by build_sjos_nitrogen() itself.
.sjr_readers <- function() {
  list(
    critical = \(options) {
      read_critical_n(
        var = options$critical_var,
        threshold = options$critical_threshold,
        land_use = options$land_use
      )
    },
    binding = \(options) build_critical_n_binding(land_use = options$land_use),
    cbs_food = \(year) .sjr_cbs_food(year),
    extra = \(year) list()
  )
}

#' Run the SJOS-N driver over a year-partitioned gridded nitrogen balance.
#'
#' @param years Integer years, or `NULL` for 1961 to the last grid year of the
#'   balance manifest.
#' @param march_root Directory holding `whep_n_balance_grid/` and
#'   `whep_n_balance_run_manifest.json`.
#' @param out_root Output root. The primary option set writes here, any other
#'   under `whep_sjos_n_arms/<arm id>/`.
#' @param options A `.sjr_options()` list.
#' @param force Rebuild years already written by a compatible run.
#' @param context A list with `readers` (a `.sjr_readers()` list) and `whep`
#'   (a `.sjr_whep_state()` list, or `NULL` to read it).
#' @return Invisibly, the path of the run manifest.
#' @noRd
.sjr_run <- function(
  years = NULL,
  march_root,
  out_root = march_root,
  options = .sjr_options(),
  force = FALSE,
  context = list(readers = .sjr_readers(), whep = NULL)
) {
  march <- .sjr_read_march(march_root)
  years <- .sjr_years(years, march)
  root <- .sjr_arm_root(out_root, options)
  whep <- context$whep %||% .sjr_whep_state()
  header <- .sjr_manifest_header(whep, march, options)
  state <- .sjr_previous_manifest(root, header) %||%
    c(header, list(partitions = list(), diagnostics = list(), years = list()))
  readers <- context$readers
  binding <- readers$binding(options)
  state$static <- list(critical_binding = .sjr_write_binding(binding, root))
  .sjr_write_manifest(state, root)
  run <- list(
    march = march,
    march_root = march_root,
    root = root,
    critical = readers$critical(options),
    binding = binding,
    options = options,
    readers = readers,
    force = force
  )
  purrr::reduce(
    years,
    \(state, year) .sjr_step(state, year, run),
    .init = state
  )
  invisible(file.path(root, .sjr_manifest_name()))
}

# One year of the run: skipped when this run's options already wrote it, else
# built, reconciled, written and recorded in the manifest before the next year
# starts, so an interrupted run keeps every finished year.
.sjr_step <- function(state, year, run) {
  if (!run$force && .sjr_year_done(state, run$root, year)) {
    cli::cli_inform("skip {year}: already written with these options")
    return(state)
  }
  record <- .sjr_run_year(year, run)
  state <- .sjr_merge_year(state, record)
  .sjr_write_manifest(state, run$root)
  cli::cli_inform("PASS {year}: {length(record$partitions)} partitions")
  state
}

.sjr_run_year <- function(year, run) {
  expected <- run$march$grid$rows[run$march$grid$year == year]
  balance <- .sjr_read_balance(run$march_root, year, expected)
  data <- c(
    list(
      balance = balance,
      critical = run$critical,
      critical_binding = run$binding,
      cbs_food = run$readers$cbs_food(year)
    ),
    run$readers$extra(year)
  )
  options <- run$options
  out <- build_sjos_nitrogen(
    data = data,
    surplus_method = options$surplus_method,
    boundary_land_use = options$land_use,
    grassland_split = options$grassland_split,
    nourishment_thresholds = options$nourishment_thresholds,
    nourishment_band = options$nourishment_band,
    negative_critical = options$negative_critical,
    country_table = TRUE,
    beyond_share_cut = options$beyond_share_cut,
    include = character()
  )
  diagnostics <- .sjr_diagnostics(out, year, run, balance)
  tables <- .sjr_tables(out, diagnostics, options)
  list(
    year = as.integer(year),
    partitions = .sjr_write_year(tables, run$root, year),
    diagnostics = as.list(diagnostics)
  )
}

# The country table's world diagnostics with the run's reconciliation (which
# aborts on a breach, before anything is written), the uniform-spread share
# and the option stamps, one row per year.
.sjr_diagnostics <- function(out, year, run, balance) {
  reconciliation <- .sjr_reconcile(out, year)
  uniform <- .sjr_uniform_spread(
    run$march$driver_report,
    year,
    sum(balance$n_input_std_t)
  )
  out$country_table$diagnostics |>
    dplyr::left_join(
      reconciliation,
      by = "year",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(uniform, by = "year", relationship = "one-to-one") |>
    dplyr::mutate(
      negative_critical = run$options$negative_critical,
      land_use = run$options$land_use,
      grassland_split = run$options$grassland_split,
      nourishment_thresholds = run$options$nourishment_thresholds
    )
}
