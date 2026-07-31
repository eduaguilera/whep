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
# Regional groups whose code sits BELOW the threshold, so `code >= group_code_min`
# misses them. Kept separate from the deliberate non-mappings because they are a
# different fact: 351 China is a decision not to route an aggregate that would
# double-count its components, while 420 Sub-Saharan Africa is simply a group with a
# low code. Read from the manifest rather than hardcoded, for the same reason the other
# two are.
faostat_subthreshold_groups <- sort(as.integer(
  unmapped$subthreshold_group_codes
))

# Record WHICH upstream state these datasets were built from, so drift is detectable on a
# runner that cannot see the upstream repository at all.
#
# The contract tests compare the embedded copies against the real upstream files, and skip
# when those are absent -- which is every CI run, because whep-polities is private. So on CI
# nothing links `data/*.rda` to a known upstream state, and a rebuild against a DIFFERENT
# upstream than these constants came from passes everything. That is not hypothetical: this
# branch twice shipped two representations of one decision with only one of them rebuilt.
#
# The stamp is written here rather than anywhere else because this is the one data-raw script
# that already requires the manifest, so it cannot record a version it did not read.
upstream_stamp <- list(
  identity_sha256 = manifest$identity_sha256,
  counts = manifest$counts,
  alias_map_sha256 = manifest$label_alias_map$sha256,
  faostat_area_map_sha256 = manifest$faostat_area_map$sha256,
  source = manifest$source
)
jsonlite::write_json(
  upstream_stamp,
  file.path("inst", "extdata", "upstream_stamp.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

usethis::use_data(
  k_tonnes_per_livestock_unit,
  faostat_group_code_min,
  faostat_deliberate_area_codes,
  faostat_subthreshold_groups,
  internal = TRUE,
  overwrite = TRUE
)
