# -----------------------------------------------------------------------
# run_lpjml.R
#
# Generates LPJmL config and runs the simulation using WHEP-generated
# inputs. Year-dependent input file names are constructed from the
# export_start/export_end configuration below, so input.cjson never
# needs manual edits between runs.
#
# Requires: lpjmlkit, stringr, tibble
# Run after: prepare_spatialize_all.R (which writes all LPJmL inputs)
# -----------------------------------------------------------------------

library(tibble)

# ---- Configuration ----------------------------------------------------

main <- function() {
  # Set this to your L_files directory path (absolute or relative to this
  # script; normalizePath converts it to absolute so LPJmL can find files)
  l_files_dir <- normalizePath("LPJmL_inputs", mustWork = TRUE)

  model_path <- "/home/usuario/LPJmL"
  sim_path <- "/home/usuario/LPJmL/simulation"

  input_path <- file.path(l_files_dir, "whep", "lpjml_inputs")

  # Must match the year_range used in prepare_spatialize_all.R
  export_start <- 2000
  export_end   <- 2001

  # HaNi deposition starts at 1851; these may differ from export_start/export_end
  dep_start <- 2000
  dep_end   <- 2001

  simulation_start_year <- 2000
  simulation_end_year   <- 2001
  nspinup               <- 2

  use_cores <- 24

  # ---- Verify inputs --------------------------------------------------

  lu_name     <- .input_name("landuse/cft_default_cft_aggregation_30min_%d-%d.nc",
                              export_start, export_end)
  fert_name   <- .input_name("landuse/fert_N_default_cft_aggregation_30min_%d-%d.nc",
                              export_start, export_end)
  manure_name <- .input_name("landuse/manure_N_default_cft_aggregation_30min_%d-%d.nc",
                              export_start, export_end)
  nhx_name    <- .input_name("nitrogen/ndep_nhx_whep_annual_%d_%d.nc4",
                              dep_start, dep_end)
  noy_name    <- .input_name("nitrogen/ndep_noy_whep_annual_%d_%d.nc4",
                              dep_start, dep_end)
  lakes_name  <- "lakes_rivers/glwd_lakes_and_rivers_30arcmin.nc"

  .check_inputs(input_path,
                lu_name, fert_name, manure_name, nhx_name, noy_name, lakes_name)

  # ---- Build config params tibble -------------------------------------

  simulation_params <- tibble(
    sim_name  = "scenario_1",
    inpath    = input_path,
    firstyear = simulation_start_year,
    lastyear  = simulation_end_year,
    nspinup   = nspinup,
    river_routing = TRUE,
    landuse   = "yes",

    # Year-dependent WHEP inputs — override input.cjson names
    `input.landuse.name`       = lu_name,
    `input.fertilizer_nr.name` = fert_name,
    `input.manure_nr.name`     = manure_name,
    `input.nh4deposition.name` = nhx_name,
    `input.no3deposition.name` = noy_name,

    # Lakes — WHEP writes NC; input.cjson updated to cdf/var="lakes"
    `input.lakes.name` = lakes_name,
    `input.lakes.fmt`  = "cdf"
  )

  # ---- Write config and run LPJmL ------------------------------------

  cfg <- lpjmlkit::write_config(
    x          = simulation_params,
    model_path = model_path,
    sim_path   = sim_path,
    debug      = TRUE
  )

  lpjmlkit::run_lpjml(
    cfg,
    model_path,
    sim_path,
    run_cmd = stringr::str_glue("mpirun -np {use_cores} ")
  )

  invisible(NULL)
}

# ---- Private helpers --------------------------------------------------

.input_name <- function(template, start, end) {
  sprintf(template, start, end)
}

.check_inputs <- function(input_path, ...) {
  names <- c(...)
  missing <- names[!file.exists(file.path(input_path, names))]
  if (length(missing) > 0L) {
    stop(
      "Missing WHEP input files — re-run prepare_spatialize_all.R:\n",
      paste0("  ", missing, collapse = "\n")
    )
  }
}


# ---- Entry point ------------------------------------------------------

if (sys.nframe() == 0L) main()
