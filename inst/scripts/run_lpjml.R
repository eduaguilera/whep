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

run_lpjml <- function(
  model_path,
  l_files_dir = "LPJmL_inputs",
  sim_path = file.path(model_path, "simulation"),
  export_start = 1851,
  export_end = 2021,
  dep_start = export_start,
  dep_end = export_end,
  simulation_start_year = 1901,
  simulation_end_year = 2018,
  nspinup = 200,
  use_cores = 24,
  input_set = c("whep", "stock")
) {
  # input_set "whep" runs LPJmL on whep-generated inputs; "stock" runs it on the
  # model's own standard inputs, so whep results can be validated against LPJmL's
  # published ones.
  input_set <- match.arg(input_set)
  l_files_dir <- normalizePath(l_files_dir, mustWork = TRUE)
  input_path <- file.path(l_files_dir, "whep", "lpjml_inputs")

  # ---- Verify inputs --------------------------------------------------

  lu_name <- .input_name(
    "landuse/cft_default_cft_aggregation_30min_%d-%d.nc",
    export_start,
    export_end
  )
  fert_name <- .input_name(
    "landuse/fert_N_default_cft_aggregation_30min_%d-%d.nc",
    export_start,
    export_end
  )
  manure_name <- .input_name(
    "landuse/manure_N_default_cft_aggregation_30min_%d-%d.nc",
    export_start,
    export_end
  )
  nhx_name <- .input_name(
    "nitrogen/ndep_nhx_whep_annual_%d_%d.nc4",
    dep_start,
    dep_end
  )
  noy_name <- .input_name(
    "nitrogen/ndep_noy_whep_annual_%d_%d.nc4",
    dep_start,
    dep_end
  )
  lakes_name <- "lakes_rivers/glwd_lakes_and_rivers_30arcmin.nc"
  lsuha_name <- .input_name(
    "landuse/grassland_lsuha_%d-%d.nc",
    export_start,
    export_end
  )

  # The whep set must carry the whep-generated grassland landuse band + lsuha;
  # the stock set uses the model's own grassland inputs (no whep-file check).
  if (input_set == "whep") {
    .check_inputs(
      input_path,
      lu_name,
      fert_name,
      manure_name,
      nhx_name,
      noy_name,
      lakes_name,
      lsuha_name
    )
  }

  # ---- Build config params tibble -------------------------------------

  simulation_params <- tibble(
    sim_name = "scenario_1",
    inpath = input_path,
    firstyear = simulation_start_year,
    lastyear = simulation_end_year,
    nspinup = nspinup,
    river_routing = TRUE,
    landuse = "yes",
    # Activate the managed-grassland livestock grazing module (Heinke/Herzfeld);
    # without it the grassland stand writes no *_mgrass grazing balance.
    grazing = "livestock",
    # Drive grazing density from the grassland_lsuha input file. With
    # prescribe_lsuha = FALSE (the model default) LPJmL ignores the file and
    # falls back to the scalar param.lsuha (default 0): the grassland stand
    # establishes but is never grazed and every *_mgrass output stays zero.
    prescribe_lsuha = TRUE,

    # Use spatially explicit WHEP fertilizer/manure inputs instead of
    # global constant rates (fix_fertilization=true ignores the NC files)
    fix_fertilization = FALSE
  )

  if (input_set == "whep") {
    # Year-dependent WHEP inputs override input.cjson names, including the
    # whep-generated grassland landuse band and grazing density (lsuha).
    simulation_params <- tibble::add_column(
      simulation_params,
      `input.landuse.name` = lu_name,
      `input.fertilizer_nr.name` = fert_name,
      `input.manure_nr.name` = manure_name,
      `input.nh4deposition.name` = nhx_name,
      `input.no3deposition.name` = noy_name,
      `input.grassland_lsuha.name` = lsuha_name,
      `input.grassland_lsuha.fmt` = "cdf",
      # Lakes — WHEP writes NC; input.cjson updated to cdf/var="lakes"
      `input.lakes.name` = lakes_name,
      `input.lakes.fmt` = "cdf"
    )
  }

  # NOTE: the grazing outputs (pft_npp, cftfrac, and the *_mgrass C+N balance:
  # uptakec/yieldc/yieldn/fecesc/fecesn/urinec/urinen/respc/methanec_mgrass) must
  # be present in the model output config for the availability + validation step.

  # ---- Write config and run LPJmL ------------------------------------

  cfg <- lpjmlkit::write_config(
    x = simulation_params,
    model_path = model_path,
    sim_path = sim_path,
    debug = TRUE
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

if (sys.nframe() == 0L) {
  stop(
    "Call run_lpjml() with required arguments, e.g.:\n",
    "  run_lpjml(model_path = \"/path/to/LPJmL\")"
  )
}
