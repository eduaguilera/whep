# Prepare an explicitly unconverted FishStat raw candidate (whep#1616/#1617).
# From the repository root:
# Rscript inst/scripts/prepare_fishstat_trade.R SOURCE_DIR NEW_OUTPUT_DIR
# Obtain SOURCE_DIR with download/download_fishstat.R. The verified official
# release is fixed here; other releases need an explicit provenance review.
# Output is NOT a replacement fishstat-trade pin. No upload instructions or
# pin metadata are produced. Product-to-account standardization remains open.

prepare_fishstat_candidate <- function(source_dir, output_dir) {
  if (file.exists(output_dir)) {
    cli::cli_abort("Refusing to overwrite {.path {output_dir}}.")
  }
  inputs <- .fishstat_verified_inputs(source_dir)
  bridge_path <- system.file(
    "extdata",
    "harmonization",
    "fishstat_cbs_bridge.csv",
    package = "whep"
  )
  candidate <- whep:::.prepare_fishstat_candidate(
    data.table::fread(inputs[["TRADE_QUANTITY.csv"]]),
    data.table::fread(inputs[["CL_FI_COMMODITY_ISSCFC.csv"]]),
    data.table::fread(inputs[["CL_FI_COUNTRY_GROUPS.csv"]]),
    data.table::fread(bridge_path)
  )
  dir.create(output_dir, recursive = TRUE)
  path <- file.path(output_dir, "fishstat-classified-raw.parquet")
  nanoparquet::write_parquet(candidate, path)
  readback <- tibble::as_tibble(nanoparquet::read_parquet(path))
  if (!identical(candidate, readback)) {
    cli::cli_abort("FishStat candidate readback differs from prepared rows.")
  }
  .write_fishstat_candidate_meta(candidate, inputs, bridge_path, path)
  cli::cli_alert_success("Saved {nrow(candidate)} unconverted source rows.")
  cli::cli_alert_info("This candidate is rejected by the CBS reader (#1617).")
  invisible(candidate)
}

.fishstat_verified_inputs <- function(source_dir) {
  # Audited-byte fingerprints, not publisher assertions of scientific fitness.
  checksums <- c(
    "TRADE_QUANTITY.csv" = "a3d8372dea26b43fb58497ada8e970eb",
    "CL_FI_COMMODITY_ISSCFC.csv" = "5042e35942afa61d447f7484acfb9057",
    "CL_FI_COUNTRY_GROUPS.csv" = "24b5d600ded98a484e0534493117ad24",
    "FSJ_UNIT.csv" = "382cf17cfb77e5dabd693aa1c1c01bb5",
    "ReadMe.txt" = "3d238a2099e55f9d3c1e9a9996704ee0",
    "ISSCAAP.pdf" = "f1519e6d116505a3b23533884b84b642",
    "ISSCFC.pdf" = "e47ac1cef39cf92cd26e69b93d5867fb",
    "Trade_countries_notes.pdf" = "0f4448776591723a7870693299df312e",
    "Trade_E.pdf" = "ae16cda1e99ee196bf77496fd0948ecd"
  )
  files <- stats::setNames(
    file.path(source_dir, names(checksums)),
    names(checksums)
  )
  if (!all(file.exists(files))) {
    cli::cli_abort("Missing FishStat source data or provenance files.")
  }
  if (!identical(unname(tools::md5sum(files)), unname(checksums))) {
    cli::cli_abort("FishStat inputs differ from the verified 2025.1.0 release.")
  }
  files
}

.write_fishstat_candidate_meta <- function(
  candidate,
  inputs,
  bridge_path,
  path
) {
  polity_keys <- c(
    "WHEP_POLITIES_GPKG",
    "WHEP_POLITIES_FAOSTAT_MAP",
    "WHEP_POLITIES_LABEL_ALIAS_MAP",
    "WHEP_POLITY_CONTAINMENT_CSV",
    "WHEP_POLITIES_LABEL_ITEM_CORRECTIONS",
    "WHEP_POLITIES_MANIFEST"
  )
  polity_paths <- Sys.getenv(polity_keys)
  polity_paths <- polity_paths[nzchar(polity_paths) & file.exists(polity_paths)]
  metadata <- list(
    artifact_type = "fishstat_classified_raw_v1",
    source_release = "FI_Trade_2025.1.0",
    source_url = "https://www.fao.org/fishery/static/Data/FI_Trade_2025.1.0.zip",
    weight_basis = "net_product_weight",
    standardization = "none; not a CBS replacement pin",
    scope = "I/E Q_tpw observations, including held and unmapped rows",
    source_md5 = as.list(stats::setNames(tools::md5sum(inputs), names(inputs))),
    bridge_md5 = unname(tools::md5sum(bridge_path)),
    ambient_build_input_md5 = as.list(tools::md5sum(polity_paths)),
    ambient_build_input_role = "references only; not consumed by the resolver",
    runtime_provenance = whep:::.fishstat_runtime_provenance(),
    runtime_hash_algorithm = "rlang::hash of named columns and code signatures",
    rlang_version = as.character(utils::packageVersion("rlang")),
    r_version = R.version.string,
    preparer_code_hash = rlang::hash(lapply(
      list(
        prepare_fishstat_candidate,
        .fishstat_verified_inputs,
        .write_fishstat_candidate_meta
      ),
      whep:::.fishstat_code_signature
    )),
    whep_version = as.character(utils::packageVersion("whep")),
    rows = nrow(candidate),
    missing_values = sum(is.na(candidate$Value)),
    product_tonnes = sum(candidate$Value, na.rm = TRUE),
    output_md5 = unname(tools::md5sum(path)),
    classification = candidate |>
      dplyr::count(.data$`Classification Status`),
    coverage = candidate |> dplyr::count(.data$`Coverage Status`)
  )
  jsonlite::write_json(
    metadata,
    file.path(dirname(path), "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L) {
    cli::cli_abort("Usage: prepare_fishstat_trade.R SOURCE_DIR NEW_OUTPUT_DIR")
  }
  devtools::load_all(".")
  data.table::setDTthreads(2L)
  prepare_fishstat_candidate(args[[1]], args[[2]])
}
