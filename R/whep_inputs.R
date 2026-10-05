#' External inputs
#'
#' The information needed for accessing external datasets used as inputs
#' in our modeling.
#'
#' @format
#' A tibble where each row corresponds to one external input dataset.
#' It contains the following columns:
#' - `alias`: An internal name used to refer to this dataset, which is the
#'   expected name when trying to get the dataset with `whep_read_file()`.
#' - `board_url`: The public static URL where the data is found, following
#'   the concept of a _board_ from the
#'   [`pins`](https://pins.rstudio.com/index.html) package, which is what we
#'   use for storing these input datasets. For a restricted row it is a path
#'   relative to the restricted board, `restricted:<folder>/_pins.yaml`, so
#'   no personal address enters the registry.
#' - `version`: The specific version of the dataset, as defined by the `pins`
#'   package. The version is a string similar to `"20250714T123343Z-114b5"`.
#'   This version is the one used by default if no `version` is specified when
#'   calling `whep_read_file()`. If you want to use a different one, you can
#'   find the available versions of a file by using `whep_list_file_versions()`.
#' - `access`: `"public"`, or `"restricted"` for licensed or confidential
#'   data read from a private board with per-user credentials. See
#'   [whep_data_access_status()].
#' - `public_alternative`: for a restricted row, the public alias suggested
#'   instead when restricted access is unavailable, or `NA`. It is advice in
#'   error messages only; nothing is ever read in its place.
#'
#' @inheritSection whep_read_file Frozen predecessor-pipeline references
#' @inheritSection whep_read_file The two batch pins on the build path
#'
#' @source Created by the package authors.
"whep_inputs"
