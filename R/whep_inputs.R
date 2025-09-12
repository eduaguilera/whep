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
#'   use for storing these input datasets.
#' - `version`: The specific version of the dataset, as defined by the `pins`
#'   package. The version is a string similar to `"20250714T123343Z-114b5"`.
#'   This version is the one used by default if no `version` is specified when
#'   calling `whep_read_file()`. If you want to use a different one, you can
#'   find the available versions of a file by using `whep_list_file_versions()`.
#'
#' @source Created by the package authors.
"whep_inputs"
