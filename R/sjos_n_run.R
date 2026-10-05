# Production driver behind inst/scripts/run_sjos_nitrogen.R. It runs
# build_sjos_nitrogen() one year at a time over the year-partitioned gridded
# nitrogen balance (`whep_n_balance_grid/year=<Y>/part.parquet` and its run
# manifest `whep_n_balance_run_manifest.json`) and writes the surplus-mode
# boundary, the country-year table, the classification, the nourishment axis
# and the nourishment band with its headcounts as year partitions, with one run
# manifest per option set.
#
# The logic lives here rather than in the script, as R/nbd_stage.R does for
# run_nitrogen_balance.R, so that it can be tested on fixtures: the script only
# parses its arguments and calls .sjr_run().
#
# What the driver adds to build_sjos_nitrogen() is bookkeeping that a long
# unattended run needs and that must fail closed:
# * every year is reconciled before anything of it is written: the country
#   sums plus the exceedance left unallocated on residual records equal the
#   summed cell exceedance, the grid, country, country-table and
#   classification tables agree with one another, and the band's headcounts
#   are its prevalences times the country table's population and never exceed
#   it;
# * the input balance is tied to its manifest (row counts per year and the
#   SHA-256 of the manifest's bytes) and each grid partition read is recorded
#   by its own SHA-256, so outputs cannot be read against, or extended from, a
#   different balance than the one they were built from;
# * one output root holds one option set: a second option set writes under its
#   own arm directory, and outputs written under other options, another WHEP
#   commit, another balance manifest or changed balance partitions are refused
#   rather than mixed;
# * a WHEP tree with uncommitted changes to R/ or inst/scripts/ is refused
#   unless the run is explicitly a development run, which then writes only to
#   a root of its own;
# * the share of the grid's nitrogen input that the balance spread uniformly
#   over a polity's cropland because its crop had no crop-pattern cell (issue
#   #533) is reported per year from the balance run's own captured warnings,
#   and only when those warnings were captured completely.
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
    diagnostics = "whep_sjos_n_diag",
    band = "whep_sjos_n_band"
  )
}

# The products one option set writes. The band table exists only under the
# composed band: the flat pair has no band terms and no headcounts.
.sjr_run_products <- function(options) {
  products <- .sjr_products()
  if (options$nourishment_thresholds == "flat") {
    return(products[names(products) != "band"])
  }
  products
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
    ),
    band = paste(
      "build_nourishment_band() as build_sjos_nitrogen() composed it (floor,",
      "ceiling, prevalences and headcounts), with the country table's",
      "population: one row per country and year; composed band only"
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

.sjr_schema_version <- function() 2L

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
  .sjr_check_cut_label(beyond_share_cut)
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
    .sjr_cut_label(options$beyond_share_cut)
  )
}

# The cut as it appears in an arm id: fixed-point at six decimals, so the id
# never depends on print width or significant digits.
.sjr_cut_label <- function(cut) {
  sprintf("%.6f", as.double(cut))
}

# Two different cuts must never share an arm directory. A cut that its label
# does not reproduce exactly (0.30000000001 labels as 0.300000, the label of
# 0.3) is refused, so every accepted cut has a label of its own.
.sjr_check_cut_label <- function(cut) {
  label <- .sjr_cut_label(cut)
  if (as.double(label) != cut) {
    cli::cli_abort(
      c(
        "{.arg beyond_share_cut} {format(cut, digits = 17)} is not exact at six
         decimals; its arm id would collide with that of {label}.",
        i = "Give the cut with at most six decimals."
      ),
      class = "whep_sjr_arm_collision"
    )
  }
  invisible(cut)
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
    allow_dirty = FALSE,
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
  if (identical(arg, "--allow-dirty")) {
    out$allow_dirty <- TRUE
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
    driver_report = .sjr_check_driver_report(manifest$driver_report, path),
    allocation = .sjr_allocation(manifest)
  )
}

# The driver report is an object keyed by year. Any other shape (a bare list,
# say) would leave every year without a report and report the uniform spread
# as unrecorded for the whole run, so it aborts instead.
.sjr_check_driver_report <- function(report, path) {
  report <- report %||% list()
  keys <- names(report)
  if (length(report) > 0L && (is.null(keys) || any(!nzchar(keys)))) {
    cli::cli_abort(
      c(
        "The driver report of {.file {path}} is not keyed by year.",
        i = "Expected {.code driver_report.<year>} records."
      ),
      class = "whep_sjr_report_layout"
    )
  }
  report
}

# The production run uses the national crop-area allocation: no country is
# granted the subnational refinement (whep#1033), which is off by default and
# granted per country. The driver copies whatever the balance manifest records
# about it (every field whose name mentions an allocation, "subnational" or a
# grant, with its path) and aborts, rather than warning, when any record says
# otherwise: a granted balance is a different construction, and its outputs
# must not reach the national series. A manifest that records nothing is taken
# as the default, national.
.sjr_allocation <- function(manifest) {
  hits <- .sjr_find_named(manifest, .sjr_allocation_pattern())
  violations <- .sjr_allocation_violations(manifest)
  if (length(violations) > 0L) {
    cli::cli_abort(
      c(
        "The balance run records a subnational allocation; this driver runs
         the national allocation only.",
        x = "{.field {violations}} in the balance manifest."
      ),
      class = "whep_sjr_subnational_granted"
    )
  }
  list(
    expected = "national (no subnational grant)",
    recorded = if (length(hits) == 0L) {
      "not recorded in the balance manifest"
    } else {
      hits
    }
  )
}

.sjr_allocation_pattern <- function() "allocation|subnational|grant"

# Every field of the manifest that records a subnational allocation, by path:
# a grant field (any key mentioning "grant", at any depth) that names anything,
# and a level field (`level`, `*_level`) of 1 or more anywhere under an
# allocation-like key. The walk descends through every field, matched or not,
# so a grant nested under a matched key is still seen. A level that is not a
# number cannot be shown to be national and counts as a violation.
.sjr_allocation_violations <- function(x, path = character(), inside = FALSE) {
  if (!is.list(x)) {
    return(character())
  }
  keys <- .sjr_keys(x)
  found <- purrr::map2(unname(x), keys, \(value, key) {
    here <- c(path, key)
    within <- inside ||
      grepl(.sjr_allocation_pattern(), key, ignore.case = TRUE)
    grant <- grepl("grant", key, ignore.case = TRUE) && .sjr_names_any(value)
    level <- within &&
      grepl("(^|_)levels?$", key, ignore.case = TRUE) &&
      .sjr_subnational_level(value)
    c(
      if (grant || level) paste(here, collapse = "."),
      .sjr_allocation_violations(value, here, within)
    )
  })
  unlist(found, use.names = FALSE) %||% character()
}

# Whether a recorded grant names anything: a non-empty value other than an
# unset marker, compared without regard to case.
.sjr_names_any <- function(value) {
  values <- unlist(value, use.names = FALSE)
  values <- values[!is.na(values)]
  unset <- c("", "<unset>", "none", "null", "na", "false", "0")
  any(!tolower(trimws(as.character(values))) %in% unset)
}

# Whether a recorded allocation level is subnational: any value of 1 or more,
# or any value that is not a number.
.sjr_subnational_level <- function(value) {
  values <- unlist(value, use.names = FALSE)
  values <- values[!is.na(values)]
  levels <- suppressWarnings(as.numeric(values))
  any(is.na(levels) | levels >= 1)
}

# The outermost fields whose name matches `pattern`, by path, for the record.
.sjr_find_named <- function(x, pattern, path = character()) {
  if (!is.list(x)) {
    return(list())
  }
  hits <- purrr::map2(unname(x), .sjr_keys(x), \(value, key) {
    here <- c(path, key)
    if (grepl(pattern, key, ignore.case = TRUE)) {
      return(stats::setNames(list(value), paste(here, collapse = ".")))
    }
    .sjr_find_named(value, pattern, here)
  })
  purrr::list_flatten(hits)
}

# A list's names, with an element's position standing in for a missing name.
.sjr_keys <- function(x) {
  keys <- names(x) %||% rep("", length(x))
  keys[keys == ""] <- as.character(which(keys == ""))
  keys
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

# The grid balance partition of `year`, checked against the row count its
# manifest lists, with the SHA-256 of its bytes for the run manifest.
.sjr_read_balance <- function(march_root, year, expected_rows) {
  path <- .sjr_partition_path(march_root, .sjr_march_product(), year)
  if (!file.exists(path)) {
    cli::cli_abort(
      "The grid balance partition {.file {path}} is missing.",
      class = "whep_sjr_balance_mismatch"
    )
  }
  sha256 <- unname(tools::sha256sum(path))
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
  list(
    balance = balance,
    input = list(
      path = .sjr_partition_rel(.sjr_march_product(), year),
      rows = nrow(balance),
      sha256 = sha256
    )
  )
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
# report is summed and set against the grid's standard nitrogen input.
#
# The sum is reported ("recorded") only when the year's capture is complete:
# the grid's own stages are in the report and each stage captured exactly as
# many warning-class conditions as it counted. Otherwise -- no report, no
# stages, warnings counted but not captured, or a grid built as the balance
# run's second resolution, whose conditions carry no count -- the year is
# "not_recorded", never zero. A condition that names the uniform spread but
# cannot be read as one aborts, whatever the capture.
.sjr_uniform_spread <- function(driver_report, year, grid_input_std_n_t) {
  grid <- .sjr_grid_conditions(driver_report[[as.character(year)]], year)
  spread <- .sjr_parse_uniform(grid, year)
  if (!grid$complete) {
    return(tibble::tibble(
      year = as.integer(year),
      uniform_spread_status = "not_recorded",
      uniform_spread_note = grid$note,
      uniform_spread_n_t = NA_real_,
      uniform_spread_polity_crops = NA_integer_,
      uniform_spread_warnings = NA_integer_,
      grid_input_std_n_t = grid_input_std_n_t,
      uniform_spread_share_of_input = NA_real_
    ))
  }
  total <- sum(spread$n_t)
  tibble::tibble(
    year = as.integer(year),
    uniform_spread_status = "recorded",
    uniform_spread_note = NA_character_,
    uniform_spread_n_t = total,
    uniform_spread_polity_crops = sum(spread$crops),
    uniform_spread_warnings = nrow(spread),
    grid_input_std_n_t = grid_input_std_n_t,
    uniform_spread_share_of_input = dplyr::if_else(
      grid_input_std_n_t > 0,
      total / grid_input_std_n_t,
      NA_real_
    )
  )
}

# The words every uniform-spread warning carries. Other conditions that
# mention the crop pattern (the cropland support's, the soil carbon's, a pin
# fetch) do not.
.sjr_uniform_marker <- function() "polity-crop total"

.sjr_uniform_pattern <- function() {
  paste0(
    "([0-9]+) polity-crop totals? \\(([-+0-9.eE]+) t N\\) had no",
    " crop-pattern grid cells; reallocating uniformly"
  )
}

# One row per uniform-spread warning, with its polity-crop count and tonnes.
# Whitespace runs are collapsed first, since the console width decides where
# the captured text was wrapped.
.sjr_parse_uniform <- function(grid, year) {
  texts <- gsub("\\s+", " ", cli::ansi_strip(grid$messages))
  marked <- grepl(.sjr_uniform_marker(), texts, fixed = TRUE)
  parsed <- regmatches(
    texts[marked],
    regexec(.sjr_uniform_pattern(), texts[marked])
  )
  field <- \(i) {
    purrr::map_chr(parsed, \(m) if (length(m) == 3L) m[[i]] else NA)
  }
  n_t <- suppressWarnings(as.numeric(field(3L)))
  crops <- suppressWarnings(as.integer(field(2L)))
  classes <- grid$classes[marked]
  unread <- is.na(n_t) | is.na(crops) | is.na(classes) | classes != "warning"
  if (any(unread)) {
    cli::cli_abort(
      c(
        "{sum(unread)} uniform-spread condition{?s} of the {year} balance run
         cannot be read.",
        x = "{.val {texts[marked][unread][[1]]}}",
        i = "Expected a warning matching {.val {(.sjr_uniform_pattern())}}."
      ),
      class = "whep_sjr_uniform_unparsed"
    )
  }
  tibble::tibble(n_t = n_t, crops = crops)
}

# The conditions the grid balance raised in one year's report, with whether
# their capture is complete and, when not, why. The report is the record the
# balance run writes per year: `resolution`, `stages` (one record per stage,
# each with its `warnings` count and captured `conditions`) and
# `second_resolution_conditions`.
.sjr_grid_conditions <- function(report, year) {
  incomplete <- \(note, conditions = list()) {
    c(.sjr_condition_text(conditions), list(complete = FALSE, note = note))
  }
  if (is.null(report)) {
    return(incomplete("no driver report for the year"))
  }
  .sjr_check_report_layout(report, year)
  if (!identical(report$resolution, "grid")) {
    conditions <- report$second_resolution_conditions$grid
    if (is.null(conditions)) {
      return(incomplete("the driver report holds no grid conditions"))
    }
    return(incomplete(
      "grid built as the second resolution; its warnings are not counted",
      conditions
    ))
  }
  stages <- report$stages
  if (length(stages) == 0L) {
    return(incomplete("the driver report records no stages"))
  }
  conditions <- purrr::list_flatten(purrr::map(stages, \(s) {
    s$conditions %||% list()
  }))
  counted <- purrr::map_lgl(stages, .sjr_stage_complete)
  if (!all(counted)) {
    uncounted <- purrr::map_chr(stages[!counted], \(s) {
      as.character(s$input %||% "?")
    })
    return(incomplete(
      paste(
        "warnings counted but not captured in stage",
        paste(uncounted, collapse = ", ")
      ),
      conditions
    ))
  }
  c(
    .sjr_condition_text(conditions),
    list(complete = TRUE, note = NA_character_)
  )
}

# A stage's capture is complete when its `warnings` count equals the number of
# warning-class conditions it captured.
.sjr_stage_complete <- function(stage) {
  count <- stage$warnings
  classes <- .sjr_condition_text(stage$conditions %||% list())$classes
  captured <- sum(classes == "warning", na.rm = TRUE)
  is.numeric(count) && length(count) == 1L && !is.na(count) && count == captured
}

# The message and class of each captured condition; a field that is missing
# or not one string is NA (an empty message).
.sjr_condition_text <- function(conditions) {
  text <- \(cnd, field) {
    value <- if (is.list(cnd)) cnd[[field]] else NULL
    if (rlang::is_string(value)) value else NA_character_
  }
  list(
    messages = dplyr::coalesce(
      purrr::map_chr(conditions, \(cnd) text(cnd, "message")),
      ""
    ),
    classes = purrr::map_chr(conditions, \(cnd) text(cnd, "class"))
  )
}

# The layout the balance run writes per year. Anything else -- a bare list of
# stage records among them -- aborts rather than reading as "no report".
.sjr_check_report_layout <- function(report, year) {
  keys <- c("resolution", "stages", "second_resolution_conditions")
  record <- \(x) is.list(x) && length(x) > 0L && !is.null(names(x))
  same_year <- is.null(report$year) ||
    identical(as.integer(report$year), as.integer(year))
  ok <- record(report) &&
    all(keys %in% names(report)) &&
    rlang::is_string(report$resolution) &&
    is.list(report$stages) &&
    is.null(names(report$stages)) &&
    all(purrr::map_lgl(report$stages, record)) &&
    is.list(report$second_resolution_conditions) &&
    same_year
  if (!ok) {
    cli::cli_abort(
      c(
        "The {year} driver report of the balance run has an unknown layout.",
        i = "Expected a record with {.field {keys}}, its stages a list of
             records."
      ),
      class = "whep_sjr_report_layout"
    )
  }
  invisible(report)
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

# The band's headcounts against the country table, before anything of the year
# is written. Each headcount must be its prevalence times the band table's
# population (so the band divided by the same population the country table
# carries), that population must equal the country table's for every
# country-year both hold, and the two tails together must not exceed it. A
# headcount next to a missing population is a breach; a band row whose
# headcounts are missing (a band term or the supply absent) is counted, not
# failed. Under the flat pair there is no band and nothing to check.
.sjr_reconcile_band <- function(band, country, year, tolerance = 1e-8) {
  if (is.null(band)) {
    return(tibble::tibble(
      year = as.integer(year),
      band_reconciliation_status = "not_applicable",
      band_max_headcount_excess = NA_real_,
      band_rows_without_headcount = NA_integer_
    ))
  }
  paired <- dplyr::inner_join(
    dplyr::select(band, "year", "area_code", band = "population"),
    dplyr::select(country, "year", "area_code", country = "population"),
    by = c("year", "area_code"),
    relationship = "one-to-one"
  )
  population_differs <- !(is.na(paired$band) & is.na(paired$country)) &
    !dplyr::coalesce(paired$band == paired$country, FALSE)
  scale <- tolerance * pmax(1, band$population)
  off <- \(people, prevalence) {
    !is.na(people) &
      !dplyr::coalesce(
        abs(people - prevalence * band$population) <= scale,
        FALSE
      )
  }
  excess <- band$people_under + band$people_over - band$population
  breaches <- c(
    population_differs = sum(population_differs),
    people_under_off = sum(off(
      band$people_under,
      band$prevalence_protein_deficit
    )),
    people_over_off = sum(off(
      band$people_over,
      band$prevalence_protein_excess
    )),
    tails_exceed_population = sum(excess > scale, na.rm = TRUE)
  )
  if (any(breaches > 0L)) {
    bad <- breaches[breaches > 0L]
    cli::cli_abort(
      c(
        "The {year} nourishment band does not reconcile with the country
         table; nothing of {year} was written.",
        x = "Country-years in breach: {paste0(names(bad), ' ', bad)}."
      ),
      class = "whep_sjr_unreconciled"
    )
  }
  tibble::tibble(
    year = as.integer(year),
    band_reconciliation_status = "pass",
    band_max_headcount_excess = if (all(is.na(excess))) {
      NA_real_
    } else {
      max(excess, na.rm = TRUE)
    },
    band_rows_without_headcount = sum(
      is.na(band$people_under) | is.na(band$people_over)
    )
  )
}

# ---- Writing ---------------------------------------------------------------

# The product tables of one year, in the option set's product list. The
# nourishment band is stamped on the tables that depend on it; the boundary
# tables already carry their own negative_critical, land_use and
# grassland_split stamps.
.sjr_tables <- function(out, country, band, diagnostics, options) {
  stamp <- options$nourishment_thresholds
  tables <- list(
    country_crop = out$boundary_surplus$country,
    grid = out$boundary_surplus$grid,
    country = .sjr_stamp_band(country, stamp),
    class = .sjr_stamp_band(out$sjos_class, stamp),
    nourishment = .sjr_stamp_band(out$nourishment, stamp),
    diagnostics = diagnostics,
    band = band
  )
  tables <- tables[names(.sjr_run_products(options))]
  empty <- names(tables)[purrr::map_int(tables, \(x) nrow(x) %||% 0L) == 0L]
  if (length(empty) > 0L) {
    cli::cli_abort(
      "Empty SJOS-N table{?s} {.val {empty}}; nothing was written.",
      class = "whep_sjr_empty_output"
    )
  }
  tables
}

# The band table: build_nourishment_band() as build_sjos_nitrogen() composed it
# for this year, reduced to the floor, the ceiling, the two prevalences and
# their headcounts with the band's own polity and method stamps, plus the
# population and method_population of the nourishment table (the denominator
# the country table carries; the band drops its own copy). NULL under the flat
# pair, which has no band.
.sjr_band_table <- function(out, options) {
  band <- out$nourishment_band
  if (is.null(band)) {
    return(NULL)
  }
  population <- dplyr::distinct(
    out$nourishment,
    .data$year,
    .data$area_code,
    .data$population,
    .data$method_population
  )
  band |>
    dplyr::select(
      "year",
      "area_code",
      dplyr::any_of(c(
        .reporting_polity_cols(),
        .polity_status_cols("reporting_")
      )),
      dplyr::all_of(.sjr_band_columns()),
      dplyr::starts_with("method_")
    ) |>
    dplyr::left_join(
      population,
      by = c("year", "area_code"),
      relationship = "one-to-one"
    ) |>
    dplyr::relocate("population", .after = "people_over") |>
    .sjr_stamp_band(options$nourishment_thresholds)
}

.sjr_band_columns <- function() {
  c(
    "floor_g_cap_day",
    "ceiling_g_cap_day",
    "prevalence_protein_deficit",
    "prevalence_protein_excess",
    "people_under",
    "people_over"
  )
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

# The commit of the WHEP checkout the run loaded and whether the code it ran
# is that commit's: the tree counts as clean when no tracked file under R/ or
# inst/scripts/ (what pkgload::load_all() and the script execute) differs from
# HEAD. A dirty tree is described by the paths that differ and the SHA-256 of
# their diff against HEAD. A run that cannot name its commit, or read its
# tree, is refused.
.sjr_whep_state <- function(repo = ".") {
  sha <- .sjr_git(repo, c("rev-parse", "HEAD"))
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
  scope <- .sjr_code_paths()
  status <- .sjr_git(
    repo,
    c("status", "--porcelain", "--untracked-files=no", "--", scope)
  )
  diff <- .sjr_git(repo, c("diff", "HEAD", "--", scope))
  if (!is.null(attr(status, "status")) || !is.null(attr(diff, "status"))) {
    cli::cli_abort(
      "Cannot read the state of the WHEP tree at {.file {repo}}.",
      class = "whep_sjr_no_sha"
    )
  }
  clean <- length(status) == 0L
  list(
    sha = sha,
    clean = clean,
    dirty_paths = substring(status, 4L),
    diff_sha256 = if (clean) NA_character_ else .sjr_text_sha256(diff)
  )
}

# The code a run executes from the WHEP tree.
.sjr_code_paths <- function() c("R", "inst/scripts")

.sjr_text_sha256 <- function(lines) {
  path <- tempfile("sjr_diff_")
  on.exit(unlink(path), add = TRUE)
  writeLines(lines, path, useBytes = TRUE)
  unname(tools::sha256sum(path))
}

# A dirty tree runs only when asked for (--allow-dirty), and then only as a
# development run: never into the balance root, and never into a root, or an
# arm under it, that holds outputs of a clean tree. A clean run likewise
# refuses a root holding a development run's outputs, so the two never share
# directories.
.sjr_check_tree <- function(whep, allow_dirty, out_root, march_root) {
  if (!whep$clean && !isTRUE(allow_dirty)) {
    cli::cli_abort(
      c(
        "The WHEP tree has uncommitted changes to tracked files under
         {.path R/} or {.path inst/scripts/}.",
        x = "{.file {whep$dirty_paths %||% character()}}",
        i = "Commit them, or pass {.code --allow-dirty} to write a development
             run to an output root of its own."
      ),
      class = "whep_sjr_dirty_tree"
    )
  }
  if (!whep$clean && .sjr_same_path(out_root, march_root)) {
    cli::cli_abort(
      c(
        "A development run from an uncommitted tree cannot write into the
         balance root {.file {march_root}}.",
        i = "Pass {.code --out-root} with a root of its own."
      ),
      class = "whep_sjr_dirty_root"
    )
  }
  manifests <- c(
    file.path(out_root, .sjr_manifest_name()),
    Sys.glob(file.path(out_root, .sjr_arms_dir(), "*", .sjr_manifest_name()))
  )
  manifests <- manifests[file.exists(manifests)]
  other <- purrr::keep(manifests, \(path) {
    clean <- jsonlite::read_json(path, simplifyVector = FALSE)$whep_tree_clean
    !identical(isTRUE(clean), isTRUE(whep$clean))
  })
  if (length(other) > 0L) {
    held <- if (whep$clean) {
      "a development run from an uncommitted tree"
    } else {
      "a run from a clean tree"
    }
    cli::cli_abort(
      c(
        "{.file {out_root}} holds outputs of {held}.",
        x = "{.file {other}}",
        i = "Clean and development outputs never share a root; choose another
             output root."
      ),
      class = "whep_sjr_dirty_root"
    )
  }
  invisible(whep)
}

.sjr_same_path <- function(a, b) {
  identical(
    normalizePath(a, winslash = "/", mustWork = FALSE),
    normalizePath(b, winslash = "/", mustWork = FALSE)
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
    whep_tree_clean = manifest$whep_tree_clean,
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
    whep_tree_scope = paste(
      "tracked files under",
      paste0(.sjr_code_paths(), "/", collapse = " and "),
      "against HEAD; recorded per year in the diagnostics"
    ),
    whep_version = as.character(utils::packageVersion("whep")),
    r_version = R.version.string,
    input_march_manifest_hash = march$sha256,
    input_march_manifest = list(
      path = march$path,
      hash = "sha256 of the manifest file's bytes",
      partitions = paste(
        "input_balance.<year>: the grid partition each year read, with its",
        "row count and the sha256 of its bytes"
      ),
      whep_commit = march$whep_commit,
      schema_version = march$schema_version,
      allocation = march$allocation
    ),
    options = options,
    resolved_defaults = .sjr_resolved_defaults(options),
    population = list(
      column = paste(
        "population in whep_sjos_n_country, whep_sjos_n_nourishment and",
        "whep_sjos_n_band"
      ),
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
    products = purrr::imap(.sjr_run_products(options), \(dir, key) {
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
# under other options, another commit, a tree of the other cleanliness or
# another balance manifest aborts; so does one whose recorded balance
# partitions no longer hash to what they were read as, and so do product
# partitions with no manifest at all: adding years to any of them would mix two
# constructions under one set of directories.
.sjr_previous_manifest <- function(root, header, march_root) {
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
  .sjr_check_inputs(previous$input_balance, march_root, path)
  previous
}

# Every balance partition an earlier run of this root read must still be the
# same bytes: the balance manifest vouches for row counts only, so a partition
# rewritten in place under an unchanged manifest is caught here.
.sjr_check_inputs <- function(inputs, march_root, path) {
  changed <- purrr::keep(inputs %||% list(), \(input) {
    file <- file.path(march_root, input$path)
    !file.exists(file) ||
      !identical(unname(tools::sha256sum(file)), input$sha256)
  })
  if (length(changed) > 0L) {
    cli::cli_abort(
      c(
        "{.file {path}} was built from balance partitions that have changed
         or gone since.",
        x = "{.file {purrr::map_chr(changed, 'path')}}",
        i = "Choose another output root, or remove the old outputs first."
      ),
      class = "whep_sjr_incompatible_output"
    )
  }
  invisible(inputs)
}

.sjr_year_done <- function(state, root, year, products) {
  recorded <- purrr::keep(state$partitions, \(p) identical(p$year, year))
  done <- unlist(purrr::map(recorded, "product"))
  all(products %in% done) &&
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
  state$input_balance[[as.character(year)]] <- year_record$input
  state$input_balance <- state$input_balance[order(names(state$input_balance))]
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
#' @param allow_dirty Run from a WHEP tree with uncommitted changes under `R/`
#'   or `inst/scripts/`, as a development run into a root of its own.
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
  allow_dirty = FALSE,
  context = list(readers = .sjr_readers(), whep = NULL)
) {
  march <- .sjr_read_march(march_root)
  years <- .sjr_years(years, march)
  root <- .sjr_arm_root(out_root, options)
  whep <- context$whep %||% .sjr_whep_state()
  .sjr_check_tree(whep, allow_dirty, out_root, march_root)
  header <- .sjr_manifest_header(whep, march, options)
  state <- .sjr_previous_manifest(root, header, march_root) %||%
    c(
      header,
      list(
        partitions = list(),
        diagnostics = list(),
        input_balance = list(),
        years = list()
      )
    )
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
    products = .sjr_run_products(options),
    whep = whep,
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
  if (!run$force && .sjr_year_done(state, run$root, year, run$products)) {
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
  input <- .sjr_read_balance(run$march_root, year, expected)
  data <- c(
    list(
      balance = input$balance,
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
    include = if ("band" %in% names(run$products)) "band" else character()
  )
  country <- .sjr_add_population(out$country_table$country, out$nourishment)
  band <- .sjr_band_table(out, options)
  band_check <- .sjr_reconcile_band(band, country, year)
  diagnostics <- .sjr_diagnostics(out, year, run, input$balance, band_check)
  tables <- .sjr_tables(out, country, band, diagnostics, options)
  list(
    year = as.integer(year),
    partitions = .sjr_write_year(tables, run$root, year),
    diagnostics = as.list(diagnostics),
    input = input$input
  )
}

# The country table's world diagnostics with the run's reconciliations (which
# abort on a breach, before anything is written), the uniform-spread share, the
# state of the WHEP tree the year ran from and the option stamps, one row per
# year.
.sjr_diagnostics <- function(out, year, run, balance, band_check) {
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
    dplyr::left_join(band_check, by = "year", relationship = "one-to-one") |>
    dplyr::left_join(uniform, by = "year", relationship = "one-to-one") |>
    dplyr::mutate(
      whep_sha = run$whep$sha,
      whep_tree_clean = run$whep$clean,
      whep_dirty_diff_sha256 = run$whep$diff_sha256 %||% NA_character_,
      negative_critical = run$options$negative_critical,
      land_use = run$options$land_use,
      grassland_split = run$options$grassland_split,
      nourishment_thresholds = run$options$nourishment_thresholds
    )
}
