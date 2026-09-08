# Regenerate the two pinned derived-HWSD grids.
#
#   hwsd-clay-grid           lon, lat, clay_pct
#   hwsd-texture-class-grid  lon, lat, n_<usda_texture_class> x 12
#
# Both are global 0.5-degree grids aggregated from the HWSD raster, and both
# are pinned so that no user has to aggregate an 11 GB archive (about an hour
# per pass) and -- the reason that matters more -- so that every user reads ONE
# vintage. HWSD ships in two incompatible versions: v1.2's topsoil is 0-30 cm,
# HWSD2's D1 is 0-20 cm, and `build_carbon_balance()` reports 0-30 cm
# (whep#851). Which one a user happens to hold should not silently decide their
# soil texture.
#
# WHY COUNTS AND NOT THE HYDRAULIC VALUES. `t_field`, `t_wilt` and `porosity`
# are not HWSD quantities. Each map unit resolves to a dominant USDA texture
# class and then to that class's CONSTANTS from
# `whep::soil_hydraulic_by_texture`. Publishing the values would freeze that
# table, and `whep::hwsd_texture_usda` with it, inside a data artifact, so a
# later revision of either would move locally-deriving users and leave pinned
# users behind with no error and no symptom -- the very thing the LPJmL pins
# are designed to avoid by holding only LPJmL-derived quantities. Counts are
# purely HWSD-derived and `read_soil_hydraulic()` multiplies them by whichever
# coefficient table the installed package carries.
#
# Nothing is lost: a pixel's value depends on the pixel only through its class
# and the aggregation is a plain mean, so
# mean = sum_k(n_k * v_k) / sum_k(n_k) is an identity. Measured agreement with
# the previous route is 3.952e-14 (worst of the three columns) over a real
# window, and `test_soil_ph.R` pins it on a fixture that mixes two dominant
# textures inside every cell.
#
# DO NOT take the aggregation from `inst/scripts/prepare_spatialize_all.R`.
# Its texture block aggregates with `fun = "modal"` -- the dominant class of
# the coarse cell -- where the package reader means the per-pixel values with
# `fun = "mean"`. Producing the pin from that would move every cell, and both
# results would still satisfy porosity > t_field > t_wilt. Call the package
# functions below, which are the same code the local path runs.
#
# Usage:
#   Rscript inst/scripts/prepare_hwsd_grids.R [output_dir]
# then hand the two parquet files to inst/scripts/prepare_upload.R, add the
# aliases to inst/extdata/whep_inputs.csv, and rebuild data/whep_inputs.rda
# with `Rscript data-raw/whep_inputs.R`.

devtools::load_all(here::here(), quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) > 0) args[[1]] else tempdir()
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

hwsd_dir <- whep:::.resolve_hwsd_dir(NULL)
cli::cli_alert_info("Reading HWSD from {.path {hwsd_dir}}.")

# Provenance travels with the artifact rather than in a commit message: the
# extract records which HWSD version it came from and at what topsoil depth.
attrs <- whep:::.read_hwsd_attributes_local(
  hwsd_dir,
  required = whep:::.hwsd_texture_columns()
)
stamp <- attrs |>
  dplyr::summarise(
    source = paste(sort(unique(.data$source)), collapse = "/"),
    topsoil_depth_cm = paste(
      sort(unique(.data$topsoil_depth_cm)),
      collapse = "/"
    )
  )
cli::cli_alert_info(
  "HWSD source {.val {stamp$source}}, topsoil
   {.val {stamp$topsoil_depth_cm}} cm."
)
if (!identical(stamp$source, "hwsd_v1.2")) {
  cli::cli_abort(c(
    "Refusing to publish a grid derived from {.val {stamp$source}}.",
    i = "The carbon balance reports 0-30 cm, which is HWSD v1.2's topsoil.
         HWSD2 layers its topsoil as 0-20 cm and is not interchangeable
         (whep#851)."
  ))
}

write_grid <- function(grid, alias) {
  path <- file.path(out_dir, paste0(alias, ".parquet"))
  nanoparquet::write_parquet(grid, path)
  cli::cli_alert_success(
    "{.val {alias}}: {nrow(grid)} cells -> {.path {path}}
     ({round(file.size(path) / 1e6, 1)} MB)"
  )
  path
}

# `target_grid = NULL` means the whole raster extent, i.e. a GLOBAL grid. That
# is safe to serve to any caller because the aggregation is extent-invariant:
# a window gives the same values as the same cells taken from a larger window,
# since a 0.5-degree-centred target pads onto 0.5-degree boundaries. Readers
# crop with `.hwsd_crop_to_target()`.
cli::cli_h2("Texture class counts")
counts <- whep:::.derive_hwsd_texture_counts(hwsd_dir, target_grid = NULL)
write_grid(counts, "hwsd-texture-class-grid")

cli::cli_h2("Clay")
clay <- whep:::.derive_hwsd_clay(hwsd_dir, target_grid = NULL)
write_grid(clay, "hwsd-clay-grid")

cli::cli_alert_success("Done. Publish with inst/scripts/prepare_upload.R.")
