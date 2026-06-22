# -----------------------------------------------------------------------
# run_lpjml.R
#
# Generates LPJmL config and runs the simulation using WHEP-generated
# inputs. Year-dependent input file names are constructed from the
# export_start/export_end configuration below, so input.cjson never
# needs manual edits between runs.
#
# Returns invisibly a list with the simulation path, generated config, and
# canonical LPJmL stdout/stderr log at simulation/output/lpjml_run.log.
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
  simulation_end_year = 2009,
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
    "nitrogen/ndep_nhx_whep_monthly_%d_%d.nc4",
    dep_start,
    dep_end
  )
  noy_name <- .input_name(
    "nitrogen/ndep_noy_whep_monthly_%d_%d.nc4",
    dep_start,
    dep_end
  )
  lakes_name <- "lakes_rivers/glwd_lakes_and_rivers_30arcmin.nc"
  soil_name <- "soil/soil_30arcmin_13_types.nc"
  temp_name <- "climate/cru_ts_3_10.1901.2009.tmp.dat.nc"
  prec_name <- "climate/cru_ts_3_10_01.1901.2009.pre.dat.nc"
  cloud_name <- "climate/cru_ts_3_10.1901.2009.cld.dat.nc"
  wind_name <- "climate/wind_gswp3-w5e5_1901_2016_monthly.nc"
  co2_name <- "climate/historical_CO2_annual_1765_2018.txt"
  wetdays_name <- "climate/cru_ts3.20.1901.2011.wet.dat.nc"
  coord_nc_name <- "gadm/grid_gadm_30arcmin.nc"
  coord_name <- "gadm/grid_gadm_30arcmin.bin"
  lsuha_name <- .input_name(
    "landuse/grassland_lsuha_%d-%d.nc",
    export_start,
    export_end
  )

  # The whep set must carry the whep-generated grassland landuse band + lsuha;
  # the stock set uses the model's own grassland inputs (no whep-file check).
  if (input_set == "whep") {
    .ensure_coord_bin(model_path, input_path, coord_nc_name, coord_name)
    .check_inputs(
      input_path,
      coord_nc_name,
      coord_name,
      lu_name,
      fert_name,
      manure_name,
      nhx_name,
      noy_name,
      lakes_name,
      soil_name,
      temp_name,
      prec_name,
      cloud_name,
      wind_name,
      co2_name,
      wetdays_name,
      lsuha_name
    )
    .check_climate_coverage(
      input_path,
      simulation_start_year = simulation_start_year,
      simulation_end_year = simulation_end_year,
      soil = c(soil_name, "soil_type"),
      temp = c(temp_name, "tmp"),
      prec = c(prec_name, "pre"),
      cloud = c(cloud_name, "cld")
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
      `input.coord.name` = coord_name,
      `input.coord.fmt` = "clm",
      `input.temp.name` = temp_name,
      `input.temp.fmt` = "cdf",
      `input.temp.var` = "tmp",
      `input.temp.unit` = "celsius",
      `input.prec.name` = prec_name,
      `input.prec.fmt` = "cdf",
      `input.prec.var` = "pre",
      # The CRU precipitation file stores monthly totals as "mm"; LPJmL asks
      # NetCDF precipitation for kg/m2/day and would otherwise inflate it by
      # roughly a month when udunits conversion is enabled.
      `input.prec.unit` = "kg/m2/month",
      `input.cloud.name` = cloud_name,
      `input.cloud.fmt` = "cdf",
      `input.cloud.var` = "cld",
      `input.cloud.unit` = "%",
      `input.wind.name` = wind_name,
      `input.wind.fmt` = "cdf",
      `input.wind.var` = "wind",
      `input.wind.unit` = "m/s",
      `input.wetdays.name` = wetdays_name,
      `input.wetdays.fmt` = "cdf",
      `input.wetdays.var` = "wet",
      `input.wetdays.unit` = "day",
      `input.co2.name` = co2_name,
      `input.co2.fmt` = "txt",
      `input.grassland_lsuha.name` = lsuha_name,
      `input.grassland_lsuha.fmt` = "cdf",
      `input.grassland_lsuha.var` = "grassland_lsuha",
      # LPJmL's udunits path does not understand LSU/ha; the value is already
      # in the model's expected livestock-density scale, so skip conversion.
      `input.grassland_lsuha.unit` = "-",
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

  log_file <- file.path(sim_path, "output", "lpjml_run.log")
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)

  tryCatch(
    lpjmlkit::run_lpjml(
      cfg,
      model_path,
      sim_path,
      run_cmd = stringr::str_glue("mpirun -np {use_cores} "),
      write_stdout = TRUE
    ),
    error = function(err) {
      .write_lpjml_log(sim_path, simulation_params$sim_name[[1L]], log_file)
      stop(err)
    }
  )
  .write_lpjml_log(sim_path, simulation_params$sim_name[[1L]], log_file)

  invisible(list(sim_path = sim_path, log_file = log_file, config = cfg))
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

.ensure_coord_bin <- function(
  model_path,
  input_path,
  coord_nc_name,
  coord_name
) {
  coord_nc <- file.path(input_path, coord_nc_name)
  coord_bin <- file.path(input_path, coord_name)
  if (!file.exists(coord_nc)) {
    stop("Missing WHEP coordinate NetCDF: ", coord_nc)
  }

  cdf2grid <- file.path(model_path, "bin", "cdf2grid")
  if (!file.exists(cdf2grid)) {
    stop("Missing LPJmL cdf2grid utility: ", cdf2grid)
  }

  if (
    file.exists(coord_bin) &&
      file.info(coord_bin)$mtime >= file.info(coord_nc)$mtime
  ) {
    return(invisible(coord_bin))
  }

  dir.create(dirname(coord_bin), recursive = TRUE, showWarnings = FALSE)
  result <- system2(
    cdf2grid,
    c("-var", "coord", coord_nc, coord_bin),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      "Failed to generate WHEP coordinate CLM from ",
      coord_nc,
      ":\n",
      paste(result, collapse = "\n")
    )
  }

  invisible(coord_bin)
}

.check_climate_coverage <- function(
  input_path,
  simulation_start_year,
  simulation_end_year,
  soil,
  temp,
  prec,
  cloud
) {
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop(
      "Package ncdf4 is required to check WHEP climate coverage before ",
      "running LPJmL."
    )
  }

  soil_grid <- .read_nc_grid_mask(
    file.path(input_path, soil[[1L]]),
    soil[[2L]],
    land_only = TRUE
  )
  climate_files <- list(temp = temp, prec = prec, cloud = cloud)
  climate_grids <- lapply(
    climate_files,
    function(x) {
      .read_nc_grid_mask(
        file.path(input_path, x[[1L]]),
        x[[2L]],
        first_year = simulation_start_year,
        last_year = simulation_end_year
      )
    }
  )
  climate_on_soil <- lapply(
    climate_grids,
    .match_grid_to_soil,
    soil = soil_grid
  )

  .abort_missing_climate("precipitation", climate_on_soil$prec)
  prec_valid <- climate_on_soil$prec$valid
  .abort_missing_climate("temperature", climate_on_soil$temp, prec_valid)
  .abort_missing_climate("cloud", climate_on_soil$cloud, prec_valid)

  invisible(TRUE)
}

.read_nc_grid_mask <- function(
  path,
  var,
  land_only = FALSE,
  first_year = NULL,
  last_year = NULL
) {
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  if (!var %in% names(nc$var)) {
    stop(sprintf("Variable '%s' not found in %s.", var, path))
  }

  var_info <- nc$var[[var]]
  dim_names <- vapply(var_info$dim, `[[`, character(1L), "name")
  lon_dim <- .match_nc_dim(dim_names, c("longitude", "lon", "x"), path, var)
  lat_dim <- .match_nc_dim(dim_names, c("latitude", "lat", "y"), path, var)
  time_dim <- match("time", dim_names)
  dim_lengths <- vapply(var_info$dim, `[[`, integer(1L), "len")

  lon <- .nc_dim_values(nc, var_info$dim[[lon_dim]])
  lat <- .nc_dim_values(nc, var_info$dim[[lat_dim]])
  time_index <- .nc_time_index(
    nc,
    var_info,
    time_dim,
    first_year,
    last_year,
    path,
    var
  )

  valid <- NULL
  for (time_step in time_index) {
    start <- rep(1L, length(dim_names))
    count <- dim_lengths
    if (!is.na(time_dim)) {
      start[[time_dim]] <- time_step
      count[[time_dim]] <- 1L
    }
    values <- ncdf4::ncvar_get(nc, var, start = start, count = count)
    values <- .nc_orient_lon_lat(values, dim_names, lon_dim, lat_dim, time_dim)
    step_valid <- is.finite(values)
    if (land_only) {
      step_valid <- step_valid & values > 0
    }
    valid <- if (is.null(valid)) step_valid else valid & step_valid
  }

  list(path = path, var = var, lon = lon, lat = lat, valid = valid)
}

.match_nc_dim <- function(dim_names, candidates, path, var) {
  matched <- match(candidates, dim_names, nomatch = 0L)
  matched <- matched[matched > 0L]
  if (length(matched) == 0L) {
    stop(
      sprintf(
        "Cannot identify longitude/latitude dimensions for '%s' in %s.",
        var,
        path
      )
    )
  }
  matched[[1L]]
}

.nc_dim_values <- function(nc, dim_info) {
  vals <- dim_info$vals
  if (!is.null(vals) && length(vals) > 0L) {
    return(as.numeric(vals))
  }
  if (dim_info$name %in% names(nc$dim)) {
    vals <- nc$dim[[dim_info$name]]$vals
    if (!is.null(vals) && length(vals) > 0L) {
      return(as.numeric(vals))
    }
  }
  if (dim_info$name %in% names(nc$var)) {
    return(as.numeric(ncdf4::ncvar_get(nc, dim_info$name)))
  }
  stop(sprintf("Cannot read NetCDF dimension '%s'.", dim_info$name))
}

.nc_time_index <- function(
  nc,
  var_info,
  time_dim,
  first_year,
  last_year,
  path,
  var
) {
  if (is.na(time_dim)) {
    return(NA_integer_)
  }
  if (is.null(first_year) || is.null(last_year)) {
    return(1L)
  }

  time_info <- var_info$dim[[time_dim]]
  time_vals <- .nc_dim_values(nc, time_info)
  units <- ncdf4::ncatt_get(nc, time_info$name, "units")$value
  years <- .nc_time_years(time_vals, units)
  available <- range(years, na.rm = TRUE)
  if (first_year < available[[1L]] || last_year > available[[2L]]) {
    stop(
      sprintf(
        "%s:%s covers %d-%d, but the requested LPJmL run is %d-%d.",
        path,
        var,
        available[[1L]],
        available[[2L]],
        first_year,
        last_year
      )
    )
  }

  first <- which(years == first_year)[[1L]]
  last <- min(
    first + 11L,
    which(years == last_year)[[length(which(years == last_year))]]
  )
  seq.int(first, last)
}

.nc_time_years <- function(time_vals, units) {
  if (is.null(units) || is.na(units)) {
    return(seq_along(time_vals))
  }
  origin <- sub(
    ".*since[[:space:]]+([0-9]{4}-[0-9]{1,2}-[0-9]{1,2}).*",
    "\\1",
    units
  )
  if (identical(origin, units)) {
    origin_year <- as.integer(sub(
      ".*since[[:space:]]+([0-9]{4}).*",
      "\\1",
      units
    ))
  } else {
    origin_year <- as.integer(format(as.Date(origin), "%Y"))
  }

  if (grepl("^days since", units)) {
    dates <- as.Date(origin) + round(time_vals)
    as.integer(format(dates, "%Y"))
  } else if (grepl("^months since", units)) {
    origin_year + floor(time_vals / 12)
  } else if (grepl("^years since", units)) {
    origin_year + floor(time_vals)
  } else {
    seq_along(time_vals)
  }
}

.nc_orient_lon_lat <- function(values, dim_names, lon_dim, lat_dim, time_dim) {
  if (!is.na(time_dim)) {
    dim_names <- dim_names[-time_dim]
  }
  values <- drop(values)
  lon_pos <- match(
    dim_names[[lon_dim - (!is.na(time_dim) && lon_dim > time_dim)]],
    dim_names
  )
  lat_pos <- match(
    dim_names[[lat_dim - (!is.na(time_dim) && lat_dim > time_dim)]],
    dim_names
  )
  if (length(dim(values)) != length(dim_names)) {
    stop("Unexpected NetCDF array shape after dropping time dimension.")
  }
  if (length(dim_names) != 2L) {
    stop("Expected a 2D lon/lat NetCDF variable after selecting time.")
  }
  aperm(values, c(lon_pos, lat_pos))
}

.match_grid_to_soil <- function(climate, soil) {
  soil_cells <- which(soil$valid, arr.ind = TRUE)
  lon_match <- match(round(soil$lon, 6L), round(climate$lon, 6L))
  lat_match <- match(round(soil$lat, 6L), round(climate$lat, 6L))
  lon_idx <- lon_match[soil_cells[, 1L]]
  lat_idx <- lat_match[soil_cells[, 2L]]
  missing_coords <- is.na(lon_idx) | is.na(lat_idx)
  valid <- rep(FALSE, nrow(soil_cells))
  valid[!missing_coords] <- climate$valid[
    cbind(lon_idx[!missing_coords], lat_idx[!missing_coords])
  ]
  list(
    path = climate$path,
    var = climate$var,
    valid = valid,
    n_soil = nrow(soil_cells),
    n_missing_coords = sum(missing_coords)
  )
}

.abort_missing_climate <- function(label, coverage, reference = NULL) {
  check_cells <- if (is.null(reference)) {
    rep(TRUE, length(coverage$valid))
  } else {
    reference
  }
  missing <- check_cells & !coverage$valid
  if (any(missing)) {
    stop(
      sprintf(
        "%s climate coverage is missing %d of %d WHEP soil cells in %s:%s.",
        label,
        sum(missing),
        coverage$n_soil,
        coverage$path,
        coverage$var
      )
    )
  }
  if (coverage$n_missing_coords > 0L) {
    stop(
      sprintf(
        "%s climate grid is missing coordinates for %d WHEP soil cells in %s:%s.",
        label,
        coverage$n_missing_coords,
        coverage$path,
        coverage$var
      )
    )
  }
  invisible(TRUE)
}

.write_lpjml_log <- function(sim_path, sim_name, log_file) {
  run_output_dir <- file.path(sim_path, "output", sim_name)
  stdout_file <- .newest_file(run_output_dir, "^outfile_.*\\.out$")
  stderr_file <- .newest_file(run_output_dir, "^errfile_.*\\.err$")
  log_parts <- c(
    "LPJmL run log",
    sprintf("sim_path: %s", sim_path),
    sprintf("sim_name: %s", sim_name),
    sprintf("created: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  )
  log_parts <- c(log_parts, .read_log_part("stdout", stdout_file))
  log_parts <- c(log_parts, .read_log_part("stderr", stderr_file))
  writeLines(log_parts, log_file)
  invisible(log_file)
}

.newest_file <- function(path, pattern) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) {
    return(NA_character_)
  }
  files[which.max(file.info(files)$mtime)]
}

.read_log_part <- function(label, path) {
  header <- c("", sprintf("## %s", label))
  if (is.na(path) || !file.exists(path)) {
    return(c(header, sprintf("<missing %s log>", label)))
  }
  c(header, sprintf("# source: %s", path), readLines(path, warn = FALSE))
}


# ---- Entry point ------------------------------------------------------

if (sys.nframe() == 0L) {
  stop(
    "Call run_lpjml() with required arguments, e.g.:\n",
    "  run_lpjml(model_path = \"/path/to/LPJmL\")"
  )
}
