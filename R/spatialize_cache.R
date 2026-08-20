# Cache-validity helpers for the spatialize input preparation script.
#
# `.load_or_cache_production()` in `inst/scripts/prepare_spatialize_all.R`
# caches a multi-hour `build_primary_production()` result on disk. It used to
# invalidate on the requested year span only, so a cache written under an
# older area model was silently reused and every pin derived from it inherited
# the stale vocabulary: the deployed `.prod_cache.parquet` predated the polity
# restructure (whep#628, published areas 195 -> 216) yet kept being reused
# because its year span still covered the request (whep#657).
#
# The fingerprint below hashes *every* package dataset, discovered at run time
# rather than enumerated, so a harmonization table that becomes an input later
# cannot be missed: any change under `data/` moves the hash and the cache
# rebuilds loudly. These helpers live in `R/` (not in the script, which is
# outside `R CMD check` and `lintr`) so the invalidation logic is testable.

.whep_dataset_names <- function() {
  found <- utils::data(package = "whep")$results
  if (length(found) == 0) {
    cli::cli_abort("No {.pkg whep} datasets found; cannot fingerprint inputs.")
  }
  # `data()` reports aliases as "name (alias)"; keep the object name only.
  sort(unique(stringr::str_remove(found[, "Item"], " .*$")))
}

.whep_data_digests <- function(names = .whep_dataset_names()) {
  env <- new.env(parent = emptyenv())
  # Some `.rda` files hold several objects (`livestock_coefs.rda` holds 40+),
  # so hash what was loaded, not the file names.
  utils::data(list = names, package = "whep", envir = env)
  objs <- ls(env, sorted = TRUE)
  vapply(objs, \(nm) rlang::hash(get(nm, envir = env)), character(1))
}

.prod_cache_fingerprint <- function(digests = .whep_data_digests()) {
  rlang::hash(digests[order(names(digests))])
}

# Column set plus the sorted `area_code` domain: what a stale area model
# actually changes in the cached table.
.prod_cache_table_digest <- function(prod) {
  if (!rlang::has_name(prod, "area_code")) {
    cli::cli_abort("Cached production table has no {.field area_code} column.")
  }
  rlang::hash(list(
    columns = sort(names(prod)),
    area_codes = sort(unique(as.integer(prod$area_code)))
  ))
}

.prod_cache_meta <- function(prod, fingerprint) {
  years <- as.integer(prod$year)
  list(
    whep_version = as.character(utils::packageVersion("whep")),
    written_at = Sys.time(),
    min_year = min(years),
    max_year = max(years),
    n_rows = nrow(prod),
    columns = sort(names(prod)),
    data_fingerprint = fingerprint,
    table_digest = .prod_cache_table_digest(prod)
  )
}

# Returns `NULL` when the cache may be reused, otherwise the reason it is
# stale, for the caller to report before rebuilding.
.prod_cache_stale_reason <- function(meta, year_range, fingerprint, cached) {
  required <- c("min_year", "max_year", "data_fingerprint", "table_digest")
  if (!is.list(meta) || !all(required %in% names(meta))) {
    return("no fingerprint sidecar (cache predates whep#657)")
  }
  requested <- range(as.integer(year_range))
  if (requested[1] < meta$min_year || requested[2] > meta$max_year) {
    return(paste0(
      "covers ",
      meta$min_year,
      "-",
      meta$max_year,
      ", request is ",
      requested[1],
      "-",
      requested[2]
    ))
  }
  if (!identical(meta$data_fingerprint, fingerprint)) {
    return("package data changed since the cache was written")
  }
  if (!identical(.prod_cache_table_digest(cached), meta$table_digest)) {
    return("cached table no longer matches its recorded schema and areas")
  }
  NULL
}
