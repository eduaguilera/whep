# -----------------------------------------------------------------------
# run_lpjml.R
#
# Generates LPJmL config and runs the simulation using WHEP-generated
# inputs. Year-dependent input file names are constructed from the
# export_start/export_end configuration below, so input.cjson never
# needs manual edits between runs.
#
# Requires: lpjmlkit, stringr, tibble
# Run after: export_lpjml_inputs_netcdf.R (and optionally
#            prepare_neighbour_irrig.R for a fresh grid)
# -----------------------------------------------------------------------

library(tibble)

# ---- Configuration ----------------------------------------------------

model_path <- "/home/usuario/LPJmL"
sim_path   <- "/home/usuario/LPJmL/simulation"

l_files_dir <- Sys.getenv("WHEP_L_FILES_DIR")
if (!nzchar(l_files_dir)) stop("WHEP_L_FILES_DIR is not set.")
input_path <- file.path(l_files_dir, "whep", "lpjml_inputs")

# Must match the export_years used in export_lpjml_inputs_netcdf.R
export_start <- 2000
export_end   <- 2001

simulation_start_year <- export_start
simulation_end_year   <- export_end
nspinup               <- 2

use_cores <- 24

# ---- Derive year-dependent input paths --------------------------------

lu_name     <- sprintf(
  "landuse/cft_default_cft_aggregation_30min_%d-%d.nc",
  export_start, export_end
)
fert_name   <- sprintf(
  "landuse/fert_N_default_cft_aggregation_30min_%d-%d.nc",
  export_start, export_end
)
manure_name <- sprintf(
  "landuse/manure_N_default_cft_aggregation_30min_%d-%d.nc",
  export_start, export_end
)
nhx_name    <- sprintf(
  "nitrogen/ndep_nhx_whep_annual_%d_%d.nc4",
  export_start, export_end
)
noy_name    <- sprintf(
  "nitrogen/ndep_noy_whep_annual_%d_%d.nc4",
  export_start, export_end
)

# Verify all required WHEP-generated files exist before writing config
year_dep_files <- c(lu_name, fert_name, manure_name, nhx_name, noy_name)
missing <- year_dep_files[
  !file.exists(file.path(input_path, year_dep_files))
]
if (length(missing) > 0) {
  stop(
    "Missing WHEP input files — re-run export_lpjml_inputs_netcdf.R:\n",
    paste0("  ", missing, collapse = "\n")
  )
}

# ---- Build config params tibble ---------------------------------------

simulation_params <- tibble(
  sim_name  = "scenario_1",
  inpath    = input_path,
  firstyear = simulation_start_year,
  lastyear  = simulation_end_year,
  nspinup   = nspinup,
  river_routing = TRUE,
  landuse   = "yes",

  # Year-dependent inputs — override input.cjson names
  `input.landuse.name`       = lu_name,
  `input.fertilizer_nr.name` = fert_name,
  `input.manure_nr.name`     = manure_name,
  `input.nh4deposition.name` = nhx_name,
  `input.no3deposition.name` = noy_name
)

# ---- Write config and run LPJmL ---------------------------------------

simulation_config_details <- lpjmlkit::write_config(
  x          = simulation_params,
  model_path = model_path,
  sim_path   = sim_path,
  debug      = TRUE
)

simulation_run_details <- lpjmlkit::run_lpjml(
  simulation_config_details,
  model_path,
  sim_path,
  run_cmd = stringr::str_glue("mpirun -np {use_cores} ")
)
