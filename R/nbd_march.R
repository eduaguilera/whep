# The year-partitioned gridded nitrogen balance that
# inst/scripts/run_nitrogen_balance.R writes when WHEP_NBD_MARCH_ROOT is set,
# and that the SJOS-N production driver (R/sjos_n_run.R,
# inst/scripts/run_sjos_nitrogen.R) reads (whep#1411):
#
#   <root>/whep_n_balance_grid/year=<Y>/part.parquet   one partition per year
#   <root>/whep_n_balance_run_manifest.json            the run manifest
#
# The manifest holds `partitions` (one `{resolution, year, rows, path,
# sha256}` record per grid year), the `whep_commit` and tree state of the run,
# the `schema_version`, the run `options`, and `driver_report.<year>`: that
# year's stages, each `{input, status, seconds, rows, detail, warnings,
# messages, conditions}` with every captured condition as `{class, message}`.
# `warnings` is counted by the condition handler itself
# (.nbd_capture_conditions(), R/nbd_stage.R), not from the captured rows, so a
# lossy capture shows up as a stage whose count and conditions disagree, and
# the SJOS-N driver then reports the #533 uniform spread as not recorded.
#
# The paths and file names are the SJOS-N driver's own (.sjr_march_product(),
# .sjr_march_manifest_name(), .sjr_partition_rel()), so writer and reader
# cannot drift apart.
#
# One year at a time: a year's partition is written first and the manifest is
# then replaced whole, each through a temporary file and a rename, so an
# interrupted span keeps every year it finished and a manifest never lists a
# partition that is not there. One root holds one construction: a year
# written under another commit, tree state or option set is refused, as the
# SJOS-N driver refuses to mix its own outputs.
#
# The balance run builds one resolution, so `second_resolution_conditions` is
# always empty. It records nothing about a subnational allocation: the run
# reads its crop-pattern surface from WHEP_CROP_PATTERNS_PATH and cannot see
# how that surface was built, and the SJOS-N driver takes a manifest that
# records nothing as the national default.

.nbd_march_schema_version <- function() 1L

# Whether `year` is already written under `root` by a compatible run. Aborts
# when the root holds a different construction, so a long span fails before
# the build rather than after it.
#
# @param root The balance root.
# @param year The year to build.
# @param identity A `.nbd_march_identity()` list for this run.
# @return `TRUE` when the manifest lists the year and its partition exists.
.nbd_march_has_year <- function(root, year, identity) {
  manifest <- .nbd_march_read(root, identity)
  if (is.null(manifest)) {
    return(FALSE)
  }
  listed <- purrr::keep(manifest$partitions, \(part) {
    identical(as.integer(part$year), as.integer(year))
  })
  length(listed) > 0L &&
    file.exists(.sjr_partition_path(root, .sjr_march_product(), year))
}

# What every year of one root must share: the schema, the WHEP tree it was
# built from and the run options.
#
# @param whep A `.sjr_whep_state()` list.
# @param options A named list of the run's options (resolution, loss methods,
#   environment switches).
.nbd_march_identity <- function(whep, options) {
  if (!identical(options$resolution, "grid")) {
    cli::cli_abort(
      "The balance root holds the grid balance only, not resolution
       {.val {options$resolution}}.",
      class = "whep_nbd_march_resolution"
    )
  }
  list(
    schema_version = .nbd_march_schema_version(),
    whep_commit = whep$sha,
    whep_tree_clean = whep$clean,
    whep_dirty_diff_sha256 = whep$diff_sha256 %||% NA_character_,
    options = options
  )
}

# Write one year of the gridded balance and record it in the manifest.
#
# @param root The balance root.
# @param year The year built.
# @param balance The `build_nitrogen_balance(resolution = "grid")` result.
# @param report The run's stage table: `dplyr::bind_rows()` of
#   `.nbd_stage_row()` rows.
# @param identity A `.nbd_march_identity()` list for this run.
# @return Invisibly, the path of the manifest.
.nbd_write_march_year <- function(root, year, balance, report, identity) {
  year <- as.integer(year)
  .nbd_march_check_balance(balance, year, identity)
  manifest <- .nbd_march_read(root, identity) %||%
    c(
      identity,
      list(
        generator = "inst/scripts/run_nitrogen_balance.R",
        partitions = list(),
        driver_report = stats::setNames(list(), character())
      )
    )
  partition <- .nbd_march_write_partition(root, year, balance)
  manifest$partitions <- c(
    purrr::discard(manifest$partitions, \(part) {
      identical(as.integer(part$year), year)
    }),
    list(partition)
  )
  manifest$partitions <- manifest$partitions[order(
    purrr::map_int(manifest$partitions, \(part) as.integer(part$year))
  )]
  manifest$driver_report[[as.character(year)]] <- .nbd_driver_report(
    report,
    year,
    identity$options$resolution
  )
  manifest$driver_report <- manifest$driver_report[
    order(names(manifest$driver_report))
  ]
  manifest$years <- as.integer(names(manifest$driver_report))
  .nbd_march_write_manifest(manifest, root)
}

# The balance a year writes: grid resolution, a non-empty table of that year
# only. A zero-row balance is not written as a year with nothing in it.
.nbd_march_check_balance <- function(balance, year, identity) {
  if (!identical(identity$options$resolution, "grid")) {
    cli::cli_abort(
      "The balance root holds the grid balance only, not resolution
       {.val {identity$options$resolution}}.",
      class = "whep_nbd_march_resolution"
    )
  }
  if (
    !is.data.frame(balance) ||
      nrow(balance) == 0L ||
      !rlang::has_name(balance, "year") ||
      !isTRUE(all(balance$year == year))
  ) {
    cli::cli_abort(
      c(
        "No {year} grid balance to write.",
        i = "The balance must be a non-empty table of year {year} only."
      ),
      class = "whep_nbd_march_no_balance"
    )
  }
  invisible(balance)
}

# The manifest at `root`, or `NULL` when there is none, checked against this
# run's identity.
.nbd_march_read <- function(root, identity) {
  rlang::check_installed("jsonlite", "to read and write run manifests")
  path <- file.path(root, .sjr_march_manifest_name())
  if (!file.exists(path)) {
    return(NULL)
  }
  manifest <- jsonlite::read_json(path, simplifyVector = FALSE)
  mine <- .nbd_march_json(identity)
  theirs <- .nbd_march_json(manifest[names(identity)])
  if (!identical(mine, theirs)) {
    cli::cli_abort(
      c(
        "{.file {path}} was written by a different balance run (WHEP commit,
         tree state or options).",
        i = "Choose another balance root, or remove the old one first."
      ),
      class = "whep_nbd_march_incompatible"
    )
  }
  manifest$partitions <- manifest$partitions %||% list()
  manifest$driver_report <- manifest$driver_report %||%
    stats::setNames(list(), character())
  manifest
}

# A list as its manifest JSON, so a freshly built identity and one read back
# from disk compare equal.
.nbd_march_json <- function(x) {
  jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    digits = NA,
    null = "null",
    na = "null"
  )
}

# Write the year's partition through a temporary file in the same directory,
# so a partition is either the whole table or absent.
.nbd_march_write_partition <- function(root, year, balance) {
  rel <- .sjr_partition_rel(.sjr_march_product(), year)
  path <- file.path(root, rel)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  arrow::write_parquet(tibble::as_tibble(balance), tmp)
  .nbd_march_install(tmp, path)
  list(
    resolution = "grid",
    year = year,
    rows = nrow(balance),
    path = rel,
    sha256 = unname(tools::sha256sum(path))
  )
}

.nbd_march_write_manifest <- function(manifest, root) {
  manifest$built_at <- format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
  path <- file.path(root, .sjr_march_manifest_name())
  tmp <- paste0(path, ".tmp")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    manifest,
    tmp,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA,
    null = "null",
    na = "null"
  )
  .nbd_march_install(tmp, path)
  invisible(path)
}

# Replace `path` by `tmp`. The unlink is for Windows, where a rename does not
# overwrite.
.nbd_march_install <- function(tmp, path) {
  if (file.exists(path)) {
    unlink(path)
  }
  if (!file.rename(tmp, path)) {
    cli::cli_abort("Could not install {.file {path}}.")
  }
  invisible(path)
}

# One year's driver report: the resolution built, one record per stage and the
# (always empty) conditions of a second resolution.
.nbd_driver_report <- function(report, year, resolution) {
  .check_columns(
    report,
    c(
      "input",
      "status",
      "seconds",
      "rows",
      "detail",
      "warnings",
      "messages",
      "conditions"
    ),
    "report"
  )
  list(
    year = as.integer(year),
    resolution = resolution,
    stages = purrr::pmap(report, .nbd_stage_record),
    second_resolution_conditions = stats::setNames(list(), character())
  )
}

.nbd_stage_record <- function(
  input,
  status,
  seconds,
  rows,
  detail,
  warnings,
  messages,
  conditions,
  ...
) {
  list(
    input = input,
    status = status,
    seconds = seconds,
    rows = rows,
    detail = detail,
    warnings = warnings,
    messages = messages,
    conditions = purrr::pmap(
      dplyr::select(conditions, "class", "message"),
      \(class, message) list(class = class, message = message)
    )
  )
}
