#' Download and cache files
#'
#' @description
#' If the requested file doesn't exist locally, it is downloaded from a public
#' Google Drive link and stored in a cache directory obtained using
#' `tools::R_user_dir`.
#'
#' @param file_alias Alias of the requested file. For now the possible
#'    values are:
#'    - `"commodity_balance_sheet"`: Intended for `get_wide_cbs()`.
#'    - `"bilateral_trade"`: Intended for `get_bilateral_trade()`.
#'    - `"processing_coefs"`: Intended for `get_processing_coefs()`.
#' @param force_download If `TRUE`, does not try to cache the file and
#' redownloads it anyway.
#'
#' @returns A character vector with the path where the requested file is located
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_file_path("processing_coefs")
#' get_file_path("commodity_balance_sheet", force_download = TRUE)
#' }
get_file_path <- function(file_alias, force_download = FALSE) {
  message(stringr::str_glue("Fetching file {file_alias}..."))
  .download(file_alias, force_download)
}

.download <- function(file_alias, force_download) {
  file_info <- .fetch_file_info(file_alias)

  destdir <- .get_destdir()
  destfile <- .get_destfile(destdir, file_info$alias, file_info$extension)

  if (file.exists(destfile) && !force_download) {
    message("Using already existing local file")
    return(destfile)
  }
  if (!dir.exists(destdir)) {
    dir.create(destdir, recursive = TRUE)
  }
  if (file.exists(destfile) && force_download) {
    message("Local file existed, but forcing redownload as requested.")
  }

  tryCatch(
    {
      message(stringr::str_glue("Downloading {file_info$alias}..."))
      .download_from_drive(file_info$drive_file_id, destfile)
    },
    error = function(cond) {
      if (file.exists(destfile)) {
        file.remove(destfile)
      }
      stop("File was not downloaded correctly. Try again.")
    }
  )
  destfile
}

.get_destdir <- function() {
  tools::R_user_dir(utils::packageName(), which = "cache")
}

.get_destfile <- function(destdir, alias, extension) {
  file.path(
    destdir,
    stringr::str_glue("{alias}.{extension}")
  )
}

.download_from_drive <- function(drive_file_id, destfile) {
  googledrive::drive_deauth()
  googledrive::drive_download(
    googledrive::as_id(drive_file_id),
    path = destfile,
    overwrite = TRUE
  )
}

.fetch_file_info <- function(file_alias) {
  file_info <- dplyr::filter(.get_input_files_data(), alias == file_alias)

  if (nrow(file_info) == 0) {
    stop(stringr::str_glue("There is no file entry with alias {file_alias}"))
  }
  if (nrow(file_info) > 1) {
    stop(
      stringr::str_glue(
        paste0(
          "There are {nrow(file_info)} file entries with alias {file_alias} ",
          "and there should be only one. Double check the content of file ",
          '"inst/extdata/input/raw/input_files.csv"'
        )
      )
    )
  }

  c(file_info)
}

.get_input_files_data <- function() {
  "extdata" |>
    system.file("input/raw/input_files.csv", package = utils::packageName()) |>
    readr::read_csv(show_col_types = FALSE)
}
