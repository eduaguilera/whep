# Location of the GRAFS Spain input files.
#
# The Josette and Julia typologies read a set of large CSV/Excel files that
# are not redistributed with the package and are not available as pins. They
# live in a synced project folder (for example the WHEP Nextcloud share, under
# "Model inputs/GRAFS_spain"). The directory is therefore configurable rather
# than hardcoded to one machine.

.grafs_input_files <- function() {
  c(
    "Biomass_coefs.xlsx",
    "Codes_coefs.xlsx",
    "GRAFS_Prod_Destiny_git.csv",
    "Livestock_Prod_ygps.csv",
    "N_Inputs_combined.csv",
    "NPP_ygpit.csv.gz",
    "PIE_FullDestinies_FM.csv"
  )
}

# Resolve the GRAFS Spain input directory.
#
# Resolution order: `path`, then `getOption("whep.grafs_inputs_dir")`. There is
# no download fallback because the files are not public.
.grafs_inputs_dir <- function(path = NULL) {
  path <- path %||% getOption("whep.grafs_inputs_dir")

  if (is.null(path)) {
    cli::cli_abort(c(
      "No GRAFS Spain input directory configured.",
      i = "Pass {.arg inputs_dir}, or set
           {.code options(whep.grafs_inputs_dir = \"<path>\")}.",
      i = "It must contain: {.file {.grafs_input_files()}}."
    ))
  }

  if (!dir.exists(path)) {
    cli::cli_abort("Input directory {.path {path}} does not exist.")
  }

  missing <- .grafs_input_files() |>
    purrr::discard(~ file.exists(file.path(path, .x)))

  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.path {path}} is missing {length(missing)} required file{?s}.",
      x = "Missing: {.file {missing}}.",
      i = "File names are case-sensitive on Linux and macOS."
    ))
  }

  path
}
