# -----------------------------------------------------------------------
# download_hyde.R
#
# Downloads the HYDE 3.2.1 baseline archive, which carries the gridded
# urban population count read_hyde_population() reads for the urban/human
# excreta nitrogen stream (build_urban_n()).
#
# HYDE is distributed as one 5 GB baseline archive containing per-year ZIPs
# ("2010AD_pop.zip"), which is the granularity read_hyde_population() wants:
# it opens one year's archive at a time rather than holding the series. So
# this extracts the outer archive and leaves the per-year ZIPs alone.
#
# Point WHEP_HYDE_DIR at the directory holding the per-year ZIPs, reported
# at the end of the run.
#
# Not to be confused with download_population.R, which fetches the ISIMIP3a
# gridded TOTAL population for LPJmL's popdens forcing. HYDE supplies the
# urban/rural split that ISIMIP's total does not carry, so the two are
# different quantities for different consumers, not duplicates.
#
# Source: DANS Data Station Archaeology, doi:10.17026/DANS-25G-GEZ3
#   (Dataverse; file ids resolved from the dataset's own metadata).
# Licence: CC BY 3.0.
#
# References:
#   Klein Goldewijk, K., Beusen, A., Doelman, J. & Stehfest, E. (2017).
#   Anthropogenic land use estimates for the Holocene - HYDE 3.2.
#   Earth System Science Data 9, 927-953. doi:10.5194/essd-9-927-2017
# -----------------------------------------------------------------------

HYDE_DOI <- "doi:10.17026/DANS-25G-GEZ3"
HYDE_BASE <- "https://archaeology.datastations.nl/api/access/datafile"

# Dataverse file ids for HYDE 3.2.1, from the dataset metadata endpoint
# (api/datasets/export?exporter=dataverse_json). Ids are stable per file
# version; .hyde_check_ids() re-resolves them so a silent republication
# surfaces as an error rather than a wrong download.
HYDE_FILES <- list(
  baseline = list(id = 5490328L, name = "HYDE3_2_1-baseline.zip"),
  readme = list(id = 5396388L, name = "readme_release_HYDE3.2.1.txt")
)

download_hyde <- function(dest_dir, timeout = 7200) {
  hyde_dir <- file.path(dest_dir, "HYDE")
  dir.create(hyde_dir, recursive = TRUE, showWarnings = FALSE)

  .hyde_check_ids()
  for (entry in HYDE_FILES) {
    .hyde_download_file(entry, hyde_dir, timeout)
  }
  zip_dir <- .hyde_extract_baseline(hyde_dir)

  cli::cli_alert_success("HYDE 3.2.1 ready.")
  cli::cli_inform(c(i = "Set {.envvar WHEP_HYDE_DIR} to {.file {zip_dir}}."))
  invisible(zip_dir)
}

# -- Private helpers ----------------------------------------------------

# Confirm the recorded file ids still name the files we expect. A Dataverse
# id points at one file version, so if the archive is republished the id
# either 404s or serves something else; either way we want to know before
# spending an hour on a 5 GB download.
.hyde_check_ids <- function() {
  url <- paste0(
    "https://archaeology.datastations.nl/api/datasets/export",
    "?exporter=dataverse_json&persistentId=",
    HYDE_DOI
  )
  meta <- try(jsonlite::fromJSON(url, simplifyVector = FALSE), silent = TRUE)
  if (inherits(meta, "try-error")) {
    cli::cli_warn(
      "Could not reach DANS to verify file ids; downloading anyway."
    )
    return(invisible(NULL))
  }
  published <- .hyde_published_names(meta)
  for (entry in HYDE_FILES) {
    name <- published[[as.character(entry$id)]]
    if (is.null(name)) {
      cli::cli_abort(c(
        "HYDE file id {entry$id} is no longer in {.val {HYDE_DOI}}.",
        i = "The archive was probably republished; re-resolve the ids from
             the dataset metadata and update {.code HYDE_FILES}."
      ))
    }
    if (!identical(name, entry$name)) {
      cli::cli_abort(
        "HYDE file id {entry$id} now names {.file {name}}, not
         {.file {entry$name}}."
      )
    }
  }
  invisible(NULL)
}

.hyde_published_names <- function(meta) {
  files <- meta$datasetVersion$files
  ids <- vapply(files, \(f) as.character(f$dataFile$id), character(1))
  names <- vapply(files, \(f) f$dataFile$filename, character(1))
  stats::setNames(as.list(names), ids)
}

.hyde_download_file <- function(entry, hyde_dir, timeout) {
  out_path <- file.path(hyde_dir, entry$name)
  if (file.exists(out_path)) {
    cli::cli_alert_info("HYDE {entry$name}: already exists")
    return(invisible(out_path))
  }
  cli::cli_alert("Downloading {entry$name}...")
  old <- options(timeout = timeout)
  on.exit(options(old), add = TRUE)
  utils::download.file(
    paste0(HYDE_BASE, "/", entry$id),
    out_path,
    mode = "wb",
    quiet = TRUE
  )
  cli::cli_alert_success("HYDE {entry$name}: saved")
  invisible(out_path)
}

# Unpack the outer archive and return the directory holding the per-year
# population ZIPs, which is what WHEP_HYDE_DIR must point at. The archive's
# internal layout is baseline/zip/, but locate it rather than assume it, so
# a repackaged release fails loudly instead of yielding an empty directory.
.hyde_extract_baseline <- function(hyde_dir) {
  archive <- file.path(hyde_dir, HYDE_FILES$baseline$name)
  if (length(.hyde_year_zips(hyde_dir)) == 0L) {
    cli::cli_alert("Extracting {HYDE_FILES$baseline$name} (~5 GB)...")
    utils::unzip(archive, exdir = hyde_dir)
  }
  found <- .hyde_year_zips(hyde_dir)
  if (length(found) == 0L) {
    cli::cli_abort(c(
      "No {.file <year>AD_pop.zip} found under {.file {hyde_dir}}.",
      i = "The HYDE archive layout has changed; {.fn read_hyde_population}
           reads one per-year archive at a time."
    ))
  }
  unique(dirname(found))[[1L]]
}

.hyde_year_zips <- function(hyde_dir) {
  list.files(
    hyde_dir,
    pattern = "^[0-9]+AD_pop[.]zip$",
    recursive = TRUE,
    full.names = TRUE
  )
}
