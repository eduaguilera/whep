# -----------------------------------------------------------------------
# run_pipeline.R
#
# Full WHEP pipeline: download raw data → prepare inputs + spatialize
# → run LPJmL. Each step can be skipped by setting the corresponding
# flag to FALSE.
#
# Usage:
#   source("inst/scripts/run_pipeline.R")
#
#   # Full run (1851–2021, 24 cores)
#   run_pipeline(
#     l_files_dir  = "/path/to/LPJmL_inputs",
#     model_path   = "/path/to/lpjml",
#     climate_dir  = "/path/to/lpjml/inputs/climate",
#     use_cores    = 24
#   )
#
#   # Quick test run (2 years, skip download if data already present)
#   run_pipeline(
#     l_files_dir  = "/path/to/LPJmL_inputs",
#     model_path   = "/path/to/lpjml",
#     year_range   = 2000:2001,
#     run_download = FALSE
#   )
#
#   # Redo only the LPJmL step with a different spinup length
#   run_pipeline(
#     l_files_dir  = "/path/to/LPJmL_inputs",
#     model_path   = "/path/to/lpjml",
#     nspinup      = 500,
#     run_download = FALSE,
#     run_prepare  = FALSE
#   )
# -----------------------------------------------------------------------

source("inst/scripts/download_all.R")
source("inst/scripts/prepare_spatialize_all.R")
source("inst/scripts/run_lpjml.R")

run_pipeline <- function(
  l_files_dir,
  model_path,
  datasets = NULL,
  year_range = 1851:2021,
  target_res = 0.5,
  climate_dir = NULL,
  sim_path = file.path(model_path, "simulation"),
  export_start = min(year_range),
  export_end = max(year_range),
  dep_start = export_start,
  dep_end = export_end,
  simulation_start_year = 1901,
  simulation_end_year = 2009,
  nspinup = 200,
  use_cores = 24,
  run_download = TRUE,
  run_prepare = TRUE,
  run_lpjml = TRUE
) {
  if (run_download) {
    cli::cli_h1("Step 1 / 3 — Download raw data")
    download_all(l_files_dir, datasets)
  }

  if (run_prepare) {
    cli::cli_h1("Step 2 / 3 — Prepare inputs and spatialize")
    prepare_spatialize_all(
      l_files_dir = l_files_dir,
      year_range = year_range,
      target_res = target_res,
      climate_dir = climate_dir
    )
  }

  if (run_lpjml) {
    cli::cli_h1("Step 3 / 3 — Run LPJmL")
    run_lpjml(
      model_path = model_path,
      l_files_dir = l_files_dir,
      sim_path = sim_path,
      export_start = export_start,
      export_end = export_end,
      dep_start = dep_start,
      dep_end = dep_end,
      simulation_start_year = simulation_start_year,
      simulation_end_year = simulation_end_year,
      nspinup = nspinup,
      use_cores = use_cores
    )
  }

  invisible(NULL)
}

if (sys.nframe() == 0L) {
  stop(
    "Call run_pipeline() with required arguments, e.g.:\n",
    "  run_pipeline(\n",
    "    l_files_dir = \"/data/L_files\",\n",
    "    model_path  = \"/path/to/LPJmL\"\n",
    "  )"
  )
}
