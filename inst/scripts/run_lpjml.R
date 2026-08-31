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
  # LPJmL generation to configure for. 6.x needs a different radiation driver
  # and four extra static inputs; see .radiation_params() and
  # .lpjml6_static_params(). Default stays 5.9 so existing calls are unchanged.
  lpjml_version = "5.9",
  sim_path = file.path(model_path, "simulation"),
  export_start = 1851,
  export_end = 2023,
  dep_start = export_start,
  # N deposition is only exported to 2021; it lags the other WHEP inputs
  # until prepare_spatialize_all.R is re-run for 2022-2023.
  dep_end = 2021,
  simulation_start_year = 1901,
  # `simulation_end_year` must not exceed the end year of any *hard* forcing.
  # LPJmL treats its inputs in two ways (verified in the LPJmL source):
  #
  #   HARD -- read per year by readclimate(); a year outside the file's range
  #   aborts with ERROR130/ERROR131 and no fallback. Configuring past the end
  #   is what produced the all-NA 2010+ and corrupt seepage of the
  #   global_1901-2018 run (issue #340):
  #     temp / prec / cloud / wetdays  CRU TS 4.09   1901-2024
  #     co2                            NOAA-extended 1765-2025
  #     wind  gswp3-w5e5 + ERA5 tail                 1901-2023  <- binding
  #
  #   CLAMPED -- routed through landuse.c::checkyear() or the equivalent, which
  #   emits "WARNING024: ... data from last year used" and holds the last year
  #   constant. Ending early costs realism, not validity:
  #     popdens (ISIMIP3a) 2021     residue_on_field  2015
  #     with_tillage       2010     crop_phu (phusum) 2019
  #     nh4/no3deposition  2021     landuse/fert/manure/lsuha (WHEP) 2023
  #   tillage and residues are not stale copies: those are the last years the
  #   source datasets cover at all (PIK ships the identical files).
  #
  #   NOT READ in this configuration: tamp (needs fire = SPITFIRE; this runs
  #   fire = "fire"), wateruse (wateruse = "no"), burntarea (prescribed-
  #   burntarea fire only).
  #
  # 2023 is now reachable, as asked for in #340. Wind was the last hard forcing
  # short of it: the ISIMIP obsclim products stop at 2019 (W5E5 v2.0 ends
  # there), so 2020-2023 comes from ERA5, bias-corrected per cell and per
  # calendar month against the 2017-2019 overlap (extend_lpjml_wind()).
  #
  # Caveat to carry into any write-up: the clamped inputs above are frozen for
  # the late years -- tillage after 2010, residues after 2015, popdens and
  # ndep after 2021 -- so a 2023 run is native climate with management held
  # constant, not native throughout.
  simulation_end_year = 2023,
  # CRU TS release used for temp/prec/cloud/wetdays, as written by
  # prepare_spatialize_all.R (Section 9d).
  cru_tag = "cru_ts4.09.1901.2024",
  # Spinup length. NULL selects by version: 200 for 5.x, 300 for 6.x.
  #
  # 6.x needs the longer spinup for nitrogen. `nlosses/ninflux` must approach 1
  # at N steady state; 5.9.7 reaches 1.07 by year 120, while 6.x is still at
  # 1.68 at year 248 with soil pools draining (SoilNH4 -1.91%/yr). Measured on
  # 500 tropical cells; the decay is not a single exponential, so it cannot be
  # extrapolated from a short run. Carbon and water need far less -- 6.x's
  # slowest carbon pool (LitC) equilibrates by year 189 -- so this is only about
  # N-derived output. Re-measured after the nitrogen fixes on the fork and the
  # trajectory is unchanged, i.e. the requirement is structural rather than a bug.
  nspinup = NULL,
  # MPI ranks. Open MPI counts *physical cores* as slots, so on a machine with
  # 24 physical cores and 32 logical CPUs the default ceiling is 24 and asking
  # for more aborts with "not enough slots". `--use-hwthread-cpus` lifts it, and
  # measured on 6000 cells / 60-yr spinup it is worth having: 30 ranks ran in
  # 680 s against 748/769 s at 24, about 10% faster. The flag is added
  # automatically when use_cores exceeds the physical core count.
  use_cores = 24,
  input_set = c("whep", "stock")
) {
  # input_set "whep" runs LPJmL on whep-generated inputs; "stock" runs it on the
  # model's own standard inputs, so whep results can be validated against LPJmL's
  # published ones.
  input_set <- match.arg(input_set)
  nspinup <- nspinup %||% .default_nspinup(lpjml_version)
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
  # CRU TS supplies temp/prec/cloud/wetdays from one release; the files are
  # written by prepare_spatialize_all.R (Section 9d), which strips stn/mae/maea
  # diagnostics and normalises the `units` attribute to the strings declared
  # below. The superseded 3.10/3.20 forcing ended in 2009/2011 and is what
  # capped every historical run at 2009 (issue #340).
  temp_name <- sprintf("climate/%s.tmp.dat.nc", cru_tag)
  prec_name <- sprintf("climate/%s.pre.dat.nc", cru_tag)
  cloud_name <- sprintf("climate/%s.cld.dat.nc", cru_tag)
  wetdays_name <- sprintf("climate/%s.wet.dat.nc", cru_tag)
  # Wind, in three pieces: ISIMIP2a gswp3-w5e5 (1901-2016), ISIMIP3a monthly
  # means for 2017-2019, and ERA5 for 2020-2023. The two ISIMIP rounds agree
  # at the 2016 overlap (unweighted global mean 6.3289 vs 6.32887 m/s), and
  # the ERA5 tail is bias-corrected per cell and calendar month against
  # 2017-2019, so neither joint introduces a step. The two ISIMIP segments are
  # rebuilt by fetch_isimip_wind.sh, the ERA5 tail by fetch_era5_wind.py, and
  # the splice is extend_lpjml_wind() in prepare_spatialize_all.R.
  wind_name <- "climate/wind_gswp3-w5e5_era5_1901_2023_monthly.nc"
  co2_name <- "climate/historical_CO2_annual_1765_2025.txt"
  # LPJmL 6.x removed the `cloudiness` radiation option and the `cloud` input,
  # so CRU cld is unusable there and the model must be driven by downwelling
  # shortwave and longwave instead. ISIMIP3a supplies those only to 2019
  # (W5E5 ends there), so the tail is ERA5, spliced and bias-corrected the same
  # way as wind. See fetch_era5_radiation.py.
  swdown_name <- "climate/rsds_gswp3-w5e5_era5_1901_2023_monthly.nc"
  lwdown_name <- "climate/rlds_gswp3-w5e5_era5_1901_2023_monthly.nc"
  # 6.x opens these unconditionally and aborts without them (celldata.c:131
  # for kbf, :175 for slope; all three slope statistics are required even
  # though filesexist.c names only slope_mean). Written by
  # prepare_spatialize_all.R Section 10b.
  kbf_name <- "soil/kbf_30arcmin.nc"
  slope_names <- c(
    slope_mean = "soil/slope_mean_30arcmin.nc",
    slope_min = "soil/slope_min_30arcmin.nc",
    slope_max = "soil/slope_max_30arcmin.nc"
  )
  hydrotopes_name <- "soil/hydrotopes_cti_30arcmin.nc"
  # ISIMIP3a population converted to people/km2 by prepare_spatialize_all.R
  # (Section 9c). Replaces the stock HYDE3 .clm, which stops at 2011.
  popdens_name <- "socioeconomic/popdens_isimip3a_1901_2021.nc"
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
      lsuha_name,
      popdens_name
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
      # Radiation is version-dependent and is bound on after this block; see
      # .radiation_params() and .lpjml6_static_params().
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
      `input.popdens.name` = popdens_name,
      `input.popdens.fmt` = "cdf",
      `input.popdens.var` = "popdens",
      # Matches both the file attribute and what src/spitfire/popdens.c asks
      # for, so no udunits conversion is applied.
      `input.popdens.unit` = "km-2",
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

  # Radiation differs between LPJmL generations, and 6.x needs static terrain
  # and groundwater fields that 5.x has no concept of. Both are bound on here
  # so the shared block above stays version-agnostic.
  simulation_params <- dplyr::bind_cols(
    simulation_params,
    .radiation_params(lpjml_version, cloud_name, swdown_name, lwdown_name)
  )
  if (.is_lpjml6(lpjml_version)) {
    simulation_params <- dplyr::bind_cols(
      simulation_params,
      .lpjml6_static_params(kbf_name, slope_names, hydrotopes_name)
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
      run_cmd = .mpirun_cmd(use_cores),
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


# Spinup length when the caller does not set one. 6.x needs the longer run for
# nitrogen; see the `nspinup` argument comment for the measurements.
.default_nspinup <- function(lpjml_version) {
  if (.is_lpjml6(lpjml_version)) 300L else 200L
}

# Physical cores, which is what Open MPI counts as slots. `nproc` reports
# logical CPUs, so it overcounts on any machine with hyperthreading.
.physical_cores <- function() {
  out <- suppressWarnings(
    tryCatch(
      system2("lscpu", stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
  )
  sockets <- .lscpu_int(out, "^Socket\\(s\\):")
  per_socket <- .lscpu_int(out, "^Core\\(s\\) per socket:")
  if (is.na(sockets) || is.na(per_socket)) {
    return(NA_integer_)
  }
  sockets * per_socket
}

.lscpu_int <- function(lines, pattern) {
  hit <- grep(pattern, lines, value = TRUE)
  if (!length(hit)) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(stringr::str_trim(sub("^[^:]*:", "", hit[[1L]]))))
}

# The mpirun invocation lpjmlkit::run_lpjml() prefixes the binary with.
#
# Open MPI aborts with "There are not enough slots available in the system" when
# asked for more ranks than there are physical cores. `--use-hwthread-cpus`
# makes it count hardware threads instead, which is what allows going past that
# ceiling. Added only when needed, so a normal run's command line is unchanged.
.mpirun_cmd <- function(use_cores) {
  physical <- .physical_cores()
  if (!is.na(physical) && use_cores > physical) {
    cli::cli_alert_info(
      "Requesting {use_cores} ranks on {physical} physical core{?s}; adding
       {.code --use-hwthread-cpus} so Open MPI counts hardware threads."
    )
    return(stringr::str_glue("mpirun -np {use_cores} --use-hwthread-cpus "))
  }
  stringr::str_glue("mpirun -np {use_cores} ")
}

# LPJmL 6.x is anything with a major version of 6 or above.
.is_lpjml6 <- function(lpjml_version) {
  as.integer(sub("^([0-9]+).*$", "\\1", as.character(lpjml_version))) >= 6L
}

# Radiation config, which is the hard break between the two generations.
#
# 5.x drives radiation from CRU cloud fraction (`cloudiness`). 6.x removed both
# that option and the `cloud` input, and must instead read downwelling
# shortwave and longwave under `radiation_lwdown` -- the mode that reads a
# `lwdown` key (fscanconfig.c:953). ISIMIP publishes downwelling longwave
# (rlds); net longwave is not published, so `lwnet` cannot be substituted.
.radiation_params <- function(
  lpjml_version,
  cloud_name,
  swdown_name,
  lwdown_name
) {
  if (!.is_lpjml6(lpjml_version)) {
    return(tibble(
      radiation = "cloudiness",
      `input.cloud.name` = cloud_name,
      `input.cloud.fmt` = "cdf",
      `input.cloud.var` = "cld",
      `input.cloud.unit` = "%"
    ))
  }
  tibble(
    radiation = "radiation_lwdown",
    `input.swdown.name` = swdown_name,
    `input.swdown.fmt` = "cdf",
    `input.swdown.var` = "rsds",
    `input.swdown.unit` = "W/m2",
    `input.lwdown.name` = lwdown_name,
    `input.lwdown.fmt` = "cdf",
    `input.lwdown.var` = "rlds",
    `input.lwdown.unit` = "W/m2"
  )
}

# Static fields 6.x opens unconditionally, plus the methane switch.
#
# `with_methane` defaults to TRUE upstream, which makes littersom.c subdaily and
# adds an oxygen pool, a groundwater pool, Sphagnum and two flood-tolerant PFTs.
# Measured on identical cells it costs ~2x runtime and moves every carbon and
# water flux by under 0.5%, so it is switched off deliberately rather than
# inherited. Turn it on only with the atmospheric CH4 input wired, since the
# upstream default pairs it with `methane = "fixed"`.
.lpjml6_static_params <- function(kbf_name, slope_names, hydrotopes_name) {
  tibble(
    with_methane = FALSE,
    `input.kbf.name` = kbf_name,
    `input.kbf.fmt` = "cdf",
    `input.kbf.var` = "kbf",
    `input.slope_mean.name` = unname(slope_names[["slope_mean"]]),
    `input.slope_mean.fmt` = "cdf",
    `input.slope_mean.var` = "slope",
    `input.slope_min.name` = unname(slope_names[["slope_min"]]),
    `input.slope_min.fmt` = "cdf",
    `input.slope_min.var` = "slope",
    `input.slope_max.name` = unname(slope_names[["slope_max"]]),
    `input.slope_max.fmt` = "cdf",
    `input.slope_max.var` = "slope",
    `input.hydrotopes.name` = hydrotopes_name,
    `input.hydrotopes.fmt` = "cdf",
    `input.hydrotopes.var` = "cti"
  )
}

# ---- Entry point ------------------------------------------------------

if (sys.nframe() == 0L) {
  stop(
    "Call run_lpjml() with required arguments, e.g.:\n",
    "  run_lpjml(model_path = \"/path/to/LPJmL\")"
  )
}
