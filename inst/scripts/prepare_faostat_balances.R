# -----------------------------------------------------------------------
# prepare_faostat_balances.R
#
# Refreshes the four FAOSTAT balance pins from the official bulk
# downloads, so a pin version can be traced back to a dated FAO release
# instead of to whoever last downloaded a file by hand.
#
# The pins store the bulk CSV verbatim -- the column set that
# .extract_fao() renames in R/read_raw_inputs.R -- so there is no
# reshaping step here. Only the domains that actually changed need
# re-uploading; FBSH and CBH have not been revised since 2023-03-10 and
# 2021-12-03 respectively.
#
# Usage:
#   source("inst/scripts/prepare_faostat_balances.R")
#   balances <- download_faostat_balances(tempdir())
#   # then hand each path to prepare_upload.R's prepare_for_upload()
#
# Source: https://bulks-faostat.fao.org/production/datasets_E.json
# -----------------------------------------------------------------------

FAOSTAT_BULK_ROOT <- "https://bulks-faostat.fao.org/production/"

# alias: the whep_inputs.csv alias each domain feeds.
FAOSTAT_BALANCE_DOMAINS <- tibble::tibble(
  alias = c(
    "faostat-fbs-new",
    "faostat-fbs-old",
    "faostat-cbs-new",
    "faostat-cbs-old-crops"
  ),
  domain = c("FBS", "FBSH", "CB", "CBH"),
  stem = c(
    "FoodBalanceSheets",
    "FoodBalanceSheetsHistoric",
    "CommodityBalances_(non-food)_(2010-)",
    "CommodityBalances_(non-food)_(-2013_old_methodology)"
  ),
  archive = paste0(stem, "_E_All_Data_(Normalized).zip")
)

#' Download and unpack one FAOSTAT balance domain.
#'
#' Returns the path of the extracted "All Data (Normalized)" CSV, which is
#' exactly what the pin stores.
.download_faostat_domain <- function(archive, dest_dir, timeout = 3600) {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(dest_dir, archive)
  url <- paste0(FAOSTAT_BULK_ROOT, utils::URLencode(archive))

  if (!file.exists(zip_path)) {
    cli::cli_alert("Downloading {.val {archive}}...")
    # download.file() signals rather than returning non-zero on most
    # failures, and a truncated file would pass the skip check next run.
    ok <- tryCatch(
      identical(
        as.integer(
          utils::download.file(url, zip_path, mode = "wb", quiet = TRUE)
        ),
        0L
      ),
      error = function(e) {
        cli::cli_warn("{archive}: {conditionMessage(e)}")
        FALSE
      }
    )
    if (!ok || !file.exists(zip_path)) {
      unlink(zip_path)
      cli::cli_abort("Download failed for {.val {archive}}.")
    }
  }

  files <- utils::unzip(zip_path, list = TRUE)$Name
  data_csv <- grep("All_Data.*[.]csv$", files, value = TRUE)
  if (length(data_csv) != 1L) {
    cli::cli_abort(
      "Expected one All Data CSV in {.val {archive}}, found {length(data_csv)}."
    )
  }
  utils::unzip(zip_path, files = data_csv, exdir = dest_dir, overwrite = TRUE)
  file.path(dest_dir, data_csv)
}

#' Download every FAOSTAT balance domain WHEP pins.
#'
#' @param dest_dir Directory to download and unpack into.
#' @param aliases Character vector of whep_inputs.csv aliases to fetch.
#'   Defaults to the two domains FAO still revises.
#' @return A tibble of alias, domain, path and the year range found.
download_faostat_balances <- function(
  dest_dir,
  aliases = c("faostat-fbs-new", "faostat-cbs-new")
) {
  wanted <- FAOSTAT_BALANCE_DOMAINS |>
    dplyr::filter(.data$alias %in% aliases)
  if (nrow(wanted) != length(aliases)) {
    missing <- setdiff(aliases, wanted$alias)
    cli::cli_abort("Unknown alias{?es}: {.val {missing}}")
  }

  wanted |>
    dplyr::mutate(
      path = purrr::map_chr(
        .data$archive,
        ~ .download_faostat_domain(.x, dest_dir)
      ),
      year_range = purrr::map_chr(.data$path, .faostat_year_range)
    ) |>
    dplyr::select("alias", "domain", "path", "year_range")
}

# Report the span actually present, so a refresh that silently fetched a
# stale mirror is visible before anything is uploaded.
.faostat_year_range <- function(path) {
  years <- data.table::fread(path, select = "Year", showProgress = FALSE)$Year
  paste(range(years, na.rm = TRUE), collapse = "-")
}
