# -----------------------------------------------------------------------
# prepare_faostat_bulk.R
#
# Refreshes the FAOSTAT-sourced pins from the official bulk downloads, so
# a pin version can be traced back to a dated FAO release instead of to
# whoever last downloaded a file by hand.
#
# The pins store the bulk CSV verbatim -- the column set that
# .extract_fao() renames in R/read_raw_inputs.R -- so there is no
# reshaping step here. Only the domains that actually changed need
# re-uploading; FBSH and CBH have not been revised since 2023-03-10 and
# 2021-12-03 respectively.
#
# Was prepare_faostat_balances.R until whep#1098 added the livestock
# emissions domain, which is not a balance. Nothing outside this file
# referenced the old names.
#
# Usage:
#   source("inst/scripts/prepare_faostat_bulk.R")
#   domains <- download_faostat_bulk(tempdir())
#   # then hand each path to prepare_upload.R's prepare_for_upload(),
#   # ALWAYS with the column spec -- see faostat_bulk_col_types():
#   prepare_for_upload(
#     domains$path[[1]],
#     domains$alias[[1]],
#     col_types = faostat_bulk_col_types()
#   )
#
# Source: https://bulks-faostat.fao.org/production/datasets_E.json
# -----------------------------------------------------------------------

FAOSTAT_BULK_ROOT <- "https://bulks-faostat.fao.org/production/"

#' Column types for a FAOSTAT "All Data (Normalized)" bulk CSV.
#'
#' Every label column is read as character and nothing is guessed. readr
#' guesses per column from the values present, and it parses "t" -- FAOSTAT's
#' tonnes label -- as the logical TRUE. So a domain reporting a single unit
#' loses its unit label entirely: the CB (non-food, 2010-) domain is all
#' tonnes, and the faostat-cbs-new pin holds TRUE in the Unit column of all
#' 127,558 rows of its 2026-06-15 release (whep#1025). The same guess turns an
#' all-empty Note column into a logical, which is why the faostat-fbs-new and
#' faostat-landuse pins carry a boolean Note until they are next regenerated
#' through this spec (whep#1178). Both pinned columns are all NA, Note is
#' empty in every row of the FAO bulk CSVs checked (the RL file the landuse
#' pin was cut from, and an FBS release from January 2025), and no reader in
#' R/ selects it -- so nothing is known to be lost. It is still read as
#' character here, so a release that does populate Note keeps its text.
#'
#' `.default` covers the columns that differ between domains -- Note, and the
#' `Item Code (CPC)` / `Item Code (FBS)` / `Area Code (M49)` code columns,
#' which FAO writes with a leading apostrophe and which are labels, not
#' numbers -- and stops a newly added column from being guessed either.
#'
#' The numeric columns are col_double(), not col_integer(), because that is
#' what the guesser produced for the pins already published: keeping the types
#' identical means a refreshed pin differs from its predecessor only in the
#' Unit and Note columns, and no downstream join changes type.
faostat_bulk_col_types <- function() {
  readr::cols(
    .default = readr::col_character(),
    `Area Code` = readr::col_double(),
    `Item Code` = readr::col_double(),
    `Element Code` = readr::col_double(),
    `Year Code` = readr::col_double(),
    Year = readr::col_double(),
    Value = readr::col_double()
  )
}

# alias: the whep_inputs.csv alias each domain feeds.
#
# `faostat-emissions-livestock` (GLE) is the one that is not a balance. It is
# here because it had no builder at all: the registered pin was a hand-made
# extract that carried only the N-content Elements, so the three emission ones
# -- "Enteric fermentation (Emissions CH4)", "Manure management (Emissions
# CH4)" and "Manure management (Emissions N2O)" -- were simply absent, and
# every consumer read the gap as a literal zero (whep#1016, whep#1098). The
# bulk archive carries all of them, plus the `Source` column that separates
# FAO TIER 1 from UNFCCC, so fetching it is what makes the pin reproducible
# rather than a copy of someone's Downloads folder.
#
# `faostat-landuse` (RL) is here for the same reason: it is a verbatim bulk
# CSV, but it had no producer, so nothing applied faostat_bulk_col_types() to
# it and its registered pin carries a logical Note (whep#1178). It is not in
# download_faostat_bulk()'s default set, because re-cutting it is a data
# update, not a label repair: the pinned 2026-06-24 cut has 413,211 rows,
# while FAO's catalogue lists 421,859 for its 2026-07-17 RL release.
FAOSTAT_BULK_DOMAINS <- tibble::tibble(
  alias = c(
    "faostat-fbs-new",
    "faostat-fbs-old",
    "faostat-cbs-new",
    "faostat-cbs-old-crops",
    "faostat-emissions-livestock",
    "faostat-landuse"
  ),
  domain = c("FBS", "FBSH", "CB", "CBH", "GLE", "RL"),
  stem = c(
    "FoodBalanceSheets",
    "FoodBalanceSheetsHistoric",
    "CommodityBalances_(non-food)_(2010-)",
    "CommodityBalances_(non-food)_(-2013_old_methodology)",
    "Emissions_livestock",
    "Inputs_LandUse"
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

#' Download every FAOSTAT bulk domain WHEP pins.
#'
#' @param dest_dir Directory to download and unpack into.
#' @param aliases Character vector of whep_inputs.csv aliases to fetch.
#'   Defaults to the domains FAO still revises.
#' @return A tibble of alias, domain, path and the year range found.
download_faostat_bulk <- function(
  dest_dir,
  aliases = c(
    "faostat-fbs-new",
    "faostat-cbs-new",
    "faostat-emissions-livestock"
  )
) {
  wanted <- FAOSTAT_BULK_DOMAINS |>
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
