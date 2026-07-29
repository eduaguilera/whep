# This code is only for generating the internal data in case it's needed again
# The actual internal data (which can be used throughout the package) is stored
# in `R/sysdata.rda` and is directly available to developers when using
# `devtools::load_all()`. If you want to add more constants, rerun this file.

k_tonnes_per_livestock_unit <- 0.65

# Which FAOSTAT area codes resolve to no polity ON PURPOSE, taken from the upstream manifest rather
# than decided here.
#
# whep-polities publishes `faostat_unmapped_areas` because its own matcher acts on these two values
# and this package was re-deriving them: the 5000 threshold was measured against real production
# (34 of 34 unmapped codes are >= 5000, arriving at a number upstream already knew), and "deliberate"
# was inferred from crosswalk membership. Inference cannot distinguish a decision from an absence,
# which is how a warning here came to call FAOSTAT 351 "China" an area code nobody knows.
#
# Embedded rather than read at runtime because the manifest is an upstream file, not a package
# resource. test_faostat_unmapped_contract.R compares the embedded copy against the manifest whenever
# one is reachable, so a stale sysdata.rda is caught rather than trusted.
manifest_path <- Sys.getenv(
  "WHEP_POLITIES_MANIFEST",
  unset = path.expand("~/whep-polities/data/final/polities_manifest.json")
)
if (!file.exists(manifest_path)) {
  cli::cli_abort(c(
    "The upstream polities manifest is missing.",
    x = "Looked for {.path {manifest_path}}.",
    i = paste(
      "It publishes {.field faostat_unmapped_areas}, which these constants embed.",
      "Point {.envvar WHEP_POLITIES_MANIFEST} at it if checked out elsewhere."
    )
  ))
}
manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = TRUE)
unmapped <- manifest$faostat_unmapped_areas
if (
  is.null(unmapped$group_code_min) || is.null(unmapped$deliberate_area_codes)
) {
  cli::cli_abort(
    "The manifest has no {.field faostat_unmapped_areas}; regenerate it upstream."
  )
}
faostat_group_code_min <- as.integer(unmapped$group_code_min)
faostat_deliberate_area_codes <- sort(as.integer(
  unmapped$deliberate_area_codes
))

usethis::use_data(
  k_tonnes_per_livestock_unit,
  faostat_group_code_min,
  faostat_deliberate_area_codes,
  internal = TRUE,
  overwrite = TRUE
)
