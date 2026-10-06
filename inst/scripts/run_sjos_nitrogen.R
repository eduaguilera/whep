# Production driver for the Safe and Just Operating Space for nitrogen
# (SJOS-N) over the year-partitioned gridded nitrogen balance.
#
# For each year it reads one grid balance partition
# (`<march-root>/whep_n_balance_grid/year=<Y>/part.parquet`, listed in
# `<march-root>/whep_n_balance_run_manifest.json`), runs build_sjos_nitrogen()
# in surplus mode against the Schulte-Uebbing et al. (2022) critical surplus
# (threshold "mi", the per-cell minimum over deposition, groundwater and
# surface water, 2010 reference), with the country-year table, and writes:
#
#   <root>/whep_sjos_n/year=<Y>/part.parquet          country x crop boundary
#   <root>/whep_sjos_n_grid/year=<Y>/part.parquet     grid boundary, with the
#                                                     binding threshold
#   <root>/whep_sjos_n_country/year=<Y>/part.parquet  country-year table
#   <root>/whep_sjos_n_class/year=<Y>/part.parquet    per-crop classification
#   <root>/whep_sjos_n_nourishment/year=<Y>/part.parquet
#   <root>/whep_sjos_n_diag/year=<Y>/part.parquet     one row per year
#   <root>/whep_sjos_n_band/year=<Y>/part.parquet     nourishment band with
#                                                     its headcounts (composed
#                                                     band only)
#   <root>/whep_sjos_n_critical_binding/part.parquet  2010 per-threshold
#                                                     critical surpluses
#   <root>/whep_sjos_n_run_manifest.json
#
# `<root>` is the output root for the primary option set (all agricultural
# land with the grassland split, negative critical surpluses clamped to zero,
# composed nourishment band, cut 0.5) and
# `<out-root>/whep_sjos_n_arms/<arm id>/` for any other. Every year is
# reconciled before it is written and the run aborts on a breach. The
# manifest records the WHEP commit, the SHA-256 of the balance manifest and of
# each balance partition read, every option and, per year, the reconciliation,
# the state of the WHEP tree and the share of the grid's input nitrogen spread
# uniformly for want of a crop-pattern cell (issue #533). A WHEP tree with
# uncommitted changes under R/ or inst/scripts/ is refused unless
# --allow-dirty is given.
# The logic is in R/sjos_n_run.R.
#
# Usage (from the repository root):
#   Rscript --no-init-file inst/scripts/run_sjos_nitrogen.R [options]
#
#   --years=1961:2023        years or ranges, comma separated (default 1961
#                            to the last grid year of the balance manifest)
#   --march-root=<dir>       balance root (default $XL_FILES/whep/output)
#   --out-root=<dir>         output root (default the balance root)
#   --land-use=all|ara       critical-surplus land-use scope (default all)
#   --grassland-split=image_density|none   (default image_density)
#   --negative-critical=clamp|keep         (default clamp)
#   --nourishment=composed|flat            (default composed)
#   --beyond-share-cut=0.5   country boundary-side cut (default 0.5)
#   --boundary-mode=surplus  pathway mode is not supported yet (issue #359)
#   --force                  rebuild years this run's options already wrote
#   --allow-dirty            run from a tree with uncommitted changes under R/
#                            or inst/scripts/, as a development run: it needs
#                            an --out-root other than the balance root and
#                            holding no clean run's outputs
#
# A year takes the commodity balances, the critical-N archive and the land
# and population readers; the whole span is a long, memory-heavy job.

suppressMessages(pkgload::load_all(".", quiet = TRUE))

args <- whep:::.sjr_parse_args(commandArgs(trailingOnly = TRUE))
if (is.null(args$march_root) && !nzchar(Sys.getenv("XL_FILES"))) {
  cli::cli_abort("Set XL_FILES or pass {.arg --march-root}.")
}
march_root <- args$march_root %||%
  file.path(Sys.getenv("XL_FILES"), "whep", "output")
manifest <- whep:::.sjr_run(
  years = args$years,
  march_root = march_root,
  out_root = args$out_root %||% march_root,
  options = args$options,
  force = args$force,
  allow_dirty = args$allow_dirty
)
cli::cli_alert_success("Run manifest: {.file {manifest}}")
