# Obtain the exact official release used by the classified-raw preparer.
# Usage: source("inst/scripts/download/download_fishstat.R")
#        download_fishstat("/path/to/raw-data")
# Returns /path/to/raw-data/FI_Trade_2025.1.0, the directory the reader needs.
# This checksum fingerprints the independently archived official bytes; it is
# not a publisher-supplied checksum or evidence of scientific applicability.
# SHA256: 69afcd65bde6c860603808bf2c550541403049d0ea934e8b0161dd81b6ec72fb.
download_fishstat <- function(dest_dir) {
  output_dir <- file.path(dest_dir, "FI_Trade_2025.1.0")
  if (file.exists(output_dir)) {
    cli::cli_abort("Refusing to overwrite {.path {output_dir}}.")
  }
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  archive <- tempfile("fishstat-", tmpdir = dest_dir, fileext = ".zip")
  on.exit(unlink(archive), add = TRUE)
  utils::download.file(
    "https://www.fao.org/fishery/static/Data/FI_Trade_2025.1.0.zip",
    archive,
    mode = "wb"
  )
  .extract_fishstat_archive(
    archive,
    output_dir,
    "7f3076f771504bff8f1c49047bce7096"
  )
  invisible(output_dir)
}

.extract_fishstat_archive <- function(archive, output_dir, expected_md5) {
  if (file.exists(output_dir)) {
    cli::cli_abort("Refusing to overwrite {.path {output_dir}}.")
  }
  if (!identical(unname(tools::md5sum(archive)), expected_md5)) {
    cli::cli_abort(
      "FishStat archive checksum differs from the verified release."
    )
  }
  files <- c(
    "TRADE_QUANTITY.csv",
    "CL_FI_COMMODITY_ISSCFC.csv",
    "CL_FI_COUNTRY_GROUPS.csv",
    "FSJ_UNIT.csv",
    "ReadMe.txt",
    "ISSCAAP.pdf",
    "ISSCFC.pdf",
    "Trade_countries_notes.pdf",
    "Trade_E.pdf"
  )
  if (!all(files %in% utils::unzip(archive, list = TRUE)$Name)) {
    cli::cli_abort("FishStat archive is missing required data or provenance.")
  }
  dir.create(output_dir, recursive = TRUE)
  utils::unzip(archive, files = files, exdir = output_dir)
  invisible(output_dir)
}
