# Proof of concept: read whep inputs from a GitHub Releases file store
# instead of the Nextcloud pins board. See issue #136.
#
# Assets live in a data-only repo (default `eduaguilera/whep_inputs`) as flat
# files named `<alias>.parquet`, attached to a release tag. This module
# resolves an alias to its release-asset URL, downloads it into a local cache,
# and returns the path -- matching the shape `.download_pin_paths()` expects.
#
# Enable with `options(whep.input_source = "github")` or the environment
# variable `WHEP_INPUT_SOURCE=github`.

.use_github_inputs <- function() {
  source <- getOption(
    "whep.input_source",
    Sys.getenv("WHEP_INPUT_SOURCE", "pins")
  )
  identical(source, "github")
}

.github_inputs_repo <- function() {
  getOption(
    "whep.github_inputs_repo",
    Sys.getenv("WHEP_GITHUB_INPUTS_REPO", "eduaguilera/whep_inputs")
  )
}

.github_inputs_tag <- function() {
  getOption(
    "whep.github_inputs_tag",
    Sys.getenv("WHEP_GITHUB_INPUTS_TAG", "poc")
  )
}

.github_asset_url <- function(file_alias, repo, tag) {
  paste0(
    "https://github.com/",
    repo,
    "/releases/download/",
    tag,
    "/",
    file_alias,
    ".parquet"
  )
}

.github_cache_dir <- function(tag) {
  dir <- fs::path(tools::R_user_dir("whep", "cache"), "github", tag)
  fs::dir_create(dir)
  dir
}

# Download `<alias>.parquet` from the release (cached) and return its path.
.get_github_release_paths <- function(file_alias) {
  repo <- .github_inputs_repo()
  tag <- .github_inputs_tag()
  dest <- fs::path(.github_cache_dir(tag), paste0(file_alias, ".parquet"))

  if (fs::file_exists(dest)) {
    return(as.character(dest))
  }

  url <- .github_asset_url(file_alias, repo, tag)
  cli::cli_alert_info(
    "Fetching {.val {file_alias}} from GitHub release {.val {tag}}..."
  )
  status <- utils::download.file(
    url,
    destfile = dest,
    mode = "wb",
    quiet = TRUE
  )

  if (!identical(status, 0L) || !fs::file_exists(dest)) {
    cli::cli_abort(c(
      "Could not download {.val {file_alias}} from GitHub release.",
      "i" = "Tried {.url {url}}."
    ))
  }

  as.character(dest)
}
