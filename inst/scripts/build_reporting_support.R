# Build a local, validated source/year reporting-support artifact. No upload.
# Source this script, then call .rsa_build(config, output_dir). config contains
# inventory (CSV), domain (RDS sf), water (RDS tibble), ice (RDS sf), and a
# provenance list: cohort_id, domain_basis, inventory_basis, and source_files
# (named paths). The caller loads the intended WHEP revision and cohort first.
# Refs #1013, #1310, #1483. This does not replace the legacy world pin.

.rsa_check_water <- function(domain, water) {
  required <- c("lon", "lat", "water_frac")
  if (!all(rlang::has_name(water, required))) {
    cli::cli_abort(
      "Water needs explicit cell fractions.",
      class = "whep_artifact_water"
    )
  }
  if (
    !all(vapply(water[required], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(water[required])))
  ) {
    cli::cli_abort(
      "Water coordinates/fractions must be finite numeric values.",
      class = "whep_artifact_water"
    )
  }
  keys <- water[c("lon", "lat")]
  absent <- dplyr::anti_join(domain, keys, by = c("lon", "lat"))
  if (
    nrow(absent) ||
      anyDuplicated(keys) ||
      any(!is.finite(water$water_frac)) ||
      any(water$water_frac < 0 | water$water_frac > 1)
  ) {
    cli::cli_abort(
      "Water must cover every domain cell exactly once with finite fractions.",
      class = "whep_artifact_water"
    )
  }
  invisible(NULL)
}

.rsa_text <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

.rsa_sources <- function(config) {
  required_paths <- c("inventory", "domain", "water", "ice")
  required <- c("cohort_id", "domain_basis", "inventory_basis")
  if (
    !is.list(config) ||
      !all(rlang::has_name(config, c(required_paths, "provenance")))
  ) {
    cli::cli_abort(
      "Provide inventory, domain, water, ice and provenance.",
      class = "whep_artifact_config"
    )
  }
  p <- config$provenance
  if (
    !is.list(p) ||
      !all(rlang::has_name(p, c(required, "source_files"))) ||
      !all(vapply(p[required], .rsa_text, logical(1))) ||
      !is.list(p$source_files) ||
      !length(p$source_files)
  ) {
    cli::cli_abort(
      "Provide scalar cohort/domain/inventory attestations and source files.",
      class = "whep_artifact_config"
    )
  }
  files <- c(config[required_paths], p$source_files)
  labels <- names(files)
  if (
    is.null(labels) ||
      anyNA(labels) ||
      any(!nzchar(trimws(labels))) ||
      anyDuplicated(labels) ||
      !all(vapply(files, .rsa_text, logical(1)))
  ) {
    cli::cli_abort(
      "Input paths need unique labels and single nonempty strings.",
      class = "whep_artifact_config"
    )
  }
  paths <- unlist(files, use.names = FALSE)
  if (any(!file.exists(paths) | dir.exists(paths))) {
    cli::cli_abort(
      "Every input/source path must identify an existing file.",
      class = "whep_artifact_config"
    )
  }
  rlang::check_installed("digest")
  purrr::imap_dfr(files, \(path, name) {
    tibble::tibble(
      input = name,
      path = normalizePath(path),
      sha256 = digest::digest(file = path, algo = "sha256")
    )
  })
}

.rsa_build <- function(config, output_dir) {
  # Refuse a previous run directory: a failed rerun must not leave an older
  # successful artifact looking like the current result.
  if (!.rsa_text(output_dir)) {
    cli::cli_abort(
      "Output directory must be a single nonempty path.",
      class = "whep_artifact_config"
    )
  }
  if (file.exists(output_dir)) {
    cli::cli_abort(
      "Output path already exists.",
      class = "whep_artifact_output"
    )
  }
  sources <- .rsa_sources(config)
  inventory <- readr::read_csv(config$inventory, show_col_types = FALSE)
  domain <- readRDS(config$domain)
  water <- readRDS(config$water)
  ice <- readRDS(config$ice)
  # The bundle preserves the full canonical table, including alternatives;
  # never apply a source-code eligibility filter here.
  bundle <- whep::build_reporting_support(
    inventory,
    domain,
    layers = list(water = water, ice = ice)
  )
  .rsa_check_water(bundle$domain_support, water)
  partition <- whep::read_reporting_support(bundle)
  footprints <- whep::read_reporting_support(bundle, role = "footprints")
  expected <- inventory$reporting_polity_code
  if (!setequal(footprints$polity_code, expected)) {
    cli::cli_abort("Artifact lost inventory identities.")
  }
  if (!dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    cli::cli_abort(
      "Could not reserve a new output directory.",
      class = "whep_artifact_output"
    )
  }
  path <- file.path(output_dir, "reporting-support.rds")
  saveRDS(bundle, path)
  readback <- whep::read_reporting_support(path)
  if (!identical(partition, readback)) {
    cli::cli_abort("Readback changed support.")
  }
  manifest <- list(
    artifact_kind = "source_year_reporting_support",
    global_pin_restored = FALSE,
    published = FALSE,
    source = unique(inventory$source),
    year = unique(inventory$year),
    provenance = config$provenance,
    provenance_status = "caller_attested_not_independently_verified",
    canonical_geometry_sha256 = digest::digest(
      sf::st_as_binary(sf::st_geometry(bundle$canonical_geometries)),
      algo = "sha256"
    ),
    inputs = sources,
    inventory = inventory,
    footprint_identities = unique(footprints$polity_code),
    partition_rows = nrow(partition),
    footprint_rows = nrow(footprints),
    physical_domain_cells = nrow(bundle$domain_support),
    land_ha = sum(partition$land_area_ha),
    water_ha = sum(partition$inland_water_ha),
    ice_ha = sum(partition$ice_area_ha),
    domain_land_ha = sum(bundle$domain_support$land_area_ha),
    unrepresented_land_ha = sum(bundle$audit$unrepresented_land_ha),
    artifact_sha256 = digest::digest(file = path, algo = "sha256"),
    readback_identical = TRUE,
    runtime = list(R = R.version.string, sf = sf::sf_extSoftVersion())
  )
  jsonlite::write_json(
    manifest,
    file.path(output_dir, "manifest.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null",
    digits = 15
  )
  invisible(manifest)
}
