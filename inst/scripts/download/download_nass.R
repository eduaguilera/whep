# -----------------------------------------------------------------------
# download_nass.R
#
# Downloads the USDA NASS Quick Stats bulk dumps that read_admin_stats_nass()
# reads: the crops dump (~1.05 GB gzipped, ~8 GB flat) and the animals and
# products dump (~442 MB gzipped). No key, no registration, licence CC0 1.0
# Universal (catalog.data.gov entry for the Quick Stats agricultural
# database).
#
# NASS republishes both files every night under a new name carrying that
# day's date (qs.crops_20260902.txt.gz), and keeps only the current one, so
# there is no stable URL to hardcode: the newest names are discovered from
# the listing page. The date stamp in the name is the dump's vintage, and
# read_admin_stats_nass() reads it back off the file name as source_version,
# which is why the files are stored under their published names.
#
# The dumps land in <dest_dir>/NASS/, which is what WHEP_NASS_DIR points at.
# They are left gzipped: the reader streams them without extracting.
#
# Verified against the live listing on 2026-09-02.

download_nass <- function(dest_dir) {
  target_dir <- file.path(dest_dir, "NASS")
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  listing <- .nass_listing_names()
  for (domain in c("crops", "animals_products")) {
    .nass_download_domain(domain, listing, target_dir)
  }

  cli::cli_alert_success("NASS: dumps in {.file {target_dir}}")
  invisible()
}

.nass_datasets_url <- function() {
  "https://www.nass.usda.gov/datasets/"
}

# Every dump is linked as <a href="/datasets/qs.<domain>_YYYYMMDD.txt.gz">,
# so the file names are read straight out of the listing HTML. Only the two
# dated dumps are wanted; the census extracts (qs.census2022.txt.gz) carry
# no date stamp and are not matched.
.nass_listing_names <- function() {
  response <- httr::GET(.nass_datasets_url())
  if (httr::http_error(response)) {
    cli::cli_abort(
      "Failed to reach the NASS dataset listing
       ({httr::status_code(response)})."
    )
  }
  html <- httr::content(response, as = "text", encoding = "UTF-8")
  names <- unlist(regmatches(
    html,
    gregexpr("qs\\.[a-z_]+_[0-9]{8}\\.txt\\.gz", html)
  ))
  if (length(names) == 0) {
    # The URL is resolved to a local first: cli >= 3.4.0 reads a `{}`
    # expression starting with a dot as a style name, so
    # `{.url {.nass_datasets_url()}}` aborts with "Invalid cli literal"
    # and the whole message is lost (whep#618, the same trap fixed in
    # `.download_natural_earth()`).
    listing_url <- .nass_datasets_url()
    cli::cli_abort(c(
      "The NASS dataset listing named no dated dump.",
      i = "Its markup may have changed; check {.url {listing_url}}."
    ))
  }
  unique(names)
}

.nass_download_domain <- function(domain, listing, target_dir) {
  pattern <- paste0("^qs\\.", domain, "_[0-9]{8}\\.txt\\.gz$")
  candidates <- listing[grepl(pattern, listing)]
  if (length(candidates) == 0) {
    cli::cli_abort("The NASS listing offers no {.val {domain}} dump.")
  }
  # Sorting the names sorts the YYYYMMDD stamps, the newest last.
  newest <- sort(candidates)[[length(candidates)]]
  out_path <- file.path(target_dir, newest)

  if (file.exists(out_path)) {
    cli::cli_alert_info("NASS {newest}: already downloaded")
    return(invisible())
  }
  .nass_warn_superseded(domain, newest, target_dir)

  cli::cli_alert("Downloading {newest}...")
  utils::download.file(
    paste0(.nass_datasets_url(), newest),
    out_path,
    mode = "wb"
  )
  cli::cli_alert_success("NASS {newest}: saved")
  invisible()
}

# An older dump of the same domain is left on disk rather than deleted --
# a run reproducing an earlier result may need it -- but it is named,
# because read_admin_stats_nass() silently prefers the newest stamp.
.nass_warn_superseded <- function(domain, newest, target_dir) {
  pattern <- paste0("^qs\\.", domain, "_[0-9]{8}\\.txt(\\.gz)?$")
  existing <- list.files(target_dir, pattern = pattern)
  if (length(existing) > 0) {
    cli::cli_alert_warning(
      "NASS: {.file {existing}} {?is/are} superseded by {newest};
       the reader will use the newest stamp."
    )
  }
  invisible()
}
