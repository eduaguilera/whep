#' Download, cache and read files
#'
#' @description
#' Used to fetch input files that are needed for the package's functions
#' and that were built in external sources and are too large to include
#' directly. This is a public function for transparency purposes, so that
#' users can inspect the original inputs of this package that were not
#' directly processed here.
#'
#' If the requested file doesn't exist locally, it is downloaded from a public
#' link and cached before reading it. This is all implemented using the
#' [`pins`](https://pins.rstudio.com/index.html) package. It supports multiple
#' file formats and file versioning.
#'
#' @param file_alias Internal name of the requested file. You can find the
#'   possible values in the `alias` column of the [`whep_inputs`] dataset.
#' @param type The extension of the file that must be read. Possible values:
#'   - `parquet`: This is the default value for code efficiency reasons.
#'   - `csv`: Mainly available for those who want a more human-readable option.
#'     If the `parquet` version is available, this is useless because this
#'     function already returns the dataset in an `R` object, so the origin is
#'     irrelevant, and `parquet` is read faster.
#'
#'   Saving each file in both formats is for transparency and accessibility
#'   purposes, e.g., having to share the data with non-programmers who can
#'   easily import a CSV into a spreadsheet. You will most likely never have
#'   to set this option manually unless for some reason a file could not be
#'   supplied in e.g. `parquet` format but was in another one.
#' @param version The version of the file that must be read. Possible values:
#'   - `NULL`: This is the default value. A frozen version is chosen to make
#'     the code reproducible. Each release will have its own frozen versions.
#'     The version is the string that can be found in [`whep_inputs`] in the
#'     `version` column.
#'   - `"latest"`: This overrides the frozen version and instead fetches the
#'     latest one that is available. This might or might not match the frozen
#'     version.
#'   - Other: A specific version can also be used. For more details read the
#'     `version` column information from [`whep_inputs`].
#'
#' @returns A tibble with the dataset. Some information about each dataset can
#'   be found in the code where it's used as input for further processing.
#'
#' @export
#'
#' @examples
#' whep_read_file("read_example")
#' whep_read_file("read_example", type = "parquet", version = "latest")
#' whep_read_file(
#'   "read_example",
#'   type = "csv",
#'   version = "20250721T152646Z-ce61b"
#' )
whep_read_file <- function(file_alias, type = "parquet", version = NULL) {
  cli::cli_alert_info("Fetching files for {file_alias}...")

  file_info <- .fetch_file_info(file_alias, whep::whep_inputs)
  version <- .choose_version(file_info$version, version)

  file_info |>
    purrr::pluck("board_url") |> 
    .build_board_with_progress () |>
    pins::pin_download(file_alias, version = version) |>
    .read_file(type)
}

#' Input file versions
#'
#' @description
#' Lists all existing versions of an input file from [`whep_inputs`].
#'
#' @param file_alias Internal name of the requested file. You can find the
#'   possible values in the [`whep_inputs`] dataset.
#'
#' @returns A tibble where each row is a version. For details about its format,
#'   see `pins::pin_versions()`.
#'
#' @export
#'
#' @examples
#' whep_list_file_versions("read_example")
whep_list_file_versions <- function(file_alias) {
  file_alias |>
    .fetch_file_info(whep::whep_inputs) |>
    purrr::pluck("board_url") |>
    pins::board_url() |>
    pins::pin_versions(file_alias)
}

.read_file <- function(paths, extension) {
  path <- purrr::detect(paths, ~ stringr::str_ends(.x, extension))

  if (extension == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else if (extension == "parquet") {
    path |>
      nanoparquet::read_parquet() |>
      # Make sure it has `tbl_df` subclass, not present by default
      tibble::as_tibble()
  } else {
    extensions <- purrr::map(paths, fs::path_ext)
    cli::cli_abort(
      "Unknown file type {extension}. Available for this file: {extensions}"
    )
  }
}

.choose_version <- function(frozen_version, user_version) {
  if (is.null(user_version)) {
    frozen_version
  } else if (user_version == "latest") {
    NULL
  } else {
    user_version
  }
}

.fetch_file_info <- function(file_alias, input_files) {
  file_info <- input_files |>
    dplyr::filter(alias == file_alias)

  if (nrow(file_info) == 0) {
    cli::cli_abort("There is no file entry with alias {file_alias}")
  }
  if (nrow(file_info) > 1) {
    cli::cli_abort(
      paste0(
        "There are {nrow(file_info)} file entries with alias {file_alias} ",
        "and there should be only one. Double check the content of ",
        "'whep_inputs' dataset."
      )
    )
  }

  c(file_info)
}

.build_board_with_progress <- function(board_url) {
  board <- pins::board_url(board_url)
  # Make our own pin_fetch method to include progress bar
  # https://github.com/rstudio/pins-r/issues/873
  class(board) <- c("pins_with_progress", class(board))

  board
}

#' @importFrom pins pin_fetch
#' @method pin_fetch pins_with_progress
#' @export
#' @noRd
pin_fetch.pins_with_progress <- function(
    board,
    name,
    version = NULL,
    ...) {
  meta <- pins::pin_meta(board, name, version = version)
  .pins_cache_touch(board, meta)

  purrr::pmap_chr(
    list(
      meta$local$file_url,
      meta$file,
      meta$file_size
    ),
    function(url, file, size) {
      .pins_http_download(
        url = url,
        path_dir = meta$local$dir,
        path_file = file,
        use_cache_on_failure = board$use_cache_on_failure,
        headers = board$headers,
        .pins_http_utils_progress(size = size)
      )
    }
  )

  meta
}
