# ---------------------------------------------------------------------
# export_lpjml_inputs_netcdf.R
#
# Build a LPJmL-like input folder tree under WHEP outputs and export
# available WHEP parquet inputs to NetCDF.
#
# Output root:
#   <lfiles_dir>/whep/lpjml_inputs/
#
# Notes:
# - This script mirrors LPJmL directory structure (climate, gadm,
#   lakes_rivers, landuse, nitrogen, river_routing, soil).
# - Climate forcing is not generated here (reported as missing).
# - Deposition in WHEP is currently total annual N (country-level), so
#   NHx/NOy monthly fields are approximated and clearly flagged.
#
# Usage (interactive):
#   source("inst/scripts/export_lpjml_inputs_netcdf.R")
#   main(
#     lfiles_dir   = "/path/to/L_files",
#     export_years = 1850:2020,      # NULL = all available years
#     climate_dir  = "~/LPJmL/inputs/climate"  # NULL = skip
#   )
# ---------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(ncdf4)
  library(nanoparquet)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) y else x
}

.msg <- function(...) cat(sprintf(...), "\n")

# ---- Grid helpers -------------------------------------------------------

make_target_grid <- function() {
  lon <- seq(-179.75, 179.75, by = 0.5)
  lat <- seq(83.75, -59.25, by = -0.5)
  list(
    lon = lon, lat = lat,
    lon_key = sprintf("%.2f", lon),
    lat_key = sprintf("%.2f", lat),
    nlon = length(lon), nlat = length(lat)
  )
}

cell_area_ha_by_lat <- function(lat_vals) {
  r <- 6371000
  dlon <- 0.5 * pi / 180
  lat_n <- (lat_vals + 0.25) * pi / 180
  lat_s <- (lat_vals - 0.25) * pi / 180
  r * r * dlon * (sin(lat_n) - sin(lat_s)) / 10000
}

coord_to_rowcol <- function(dt, grid) {
  nlat <- length(grid$lat); nlon <- length(grid$lon)
  dt[, row := round((83.75 - lat)  / 0.5) + 1L]
  dt[, col := round((lon  + 179.75) / 0.5) + 1L]
  dt[row >= 1L & row <= nlat & col >= 1L & col <= nlon]
}

new_slice <- function(nlon, nlat, fill = 0) matrix(fill, nrow = nlon, ncol = nlat)

lat_idx_of <- function(lat, nlon_g) as.integer(round((83.75 - lat) / 0.5))
lon_idx_of <- function(lon, nlon_g) as.integer(round((lon + 179.75) / 0.5))
flat_of    <- function(lon, lat, nlon_g) lat_idx_of(lat) * nlon_g + lon_idx_of(lon)

haver_m <- function(lo1, la1, lo2, la2) {
  r <- 6371000
  p1 <- la1 * pi / 180; p2 <- la2 * pi / 180
  dp <- (la2 - la1) * pi / 180; dl <- (lo2 - lo1) * pi / 180
  a <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  2 * r * asin(pmin(sqrt(a), 1))
}

# ---- Status tracker -----------------------------------------------------

.new_status <- function() {
  data.table(
    input_name = character(), source = character(),
    output_file = character(), status = character(), note = character()
  )
}

.add_status <- function(status, input_name, source, output_file, status_txt, note = "") {
  rbind(
    status,
    data.table(
      input_name = input_name, source = source,
      output_file = output_file, status = status_txt, note = note
    ),
    fill = TRUE
  )
}

# ---- NetCDF writers -----------------------------------------------------

write_nc_2d <- function(
  path, var_name, long_name, units, values_lon_lat, lon, lat,
  prec = "float", missval = -1.175494e+38
) {
  dlon <- ncdim_def("longitude", "degrees_east", vals = lon)
  dlat <- ncdim_def("latitude", "degrees_north", vals = lat)
  v <- ncvar_def(
    name = var_name, units = units, dim = list(dlon, dlat),
    missval = missval, longname = long_name, prec = prec, compression = 4
  )
  nc <- nc_create(path, vars = list(v), force_v4 = TRUE)
  on.exit(nc_close(nc), add = TRUE)
  ncvar_put(nc, v, values_lon_lat, start = c(1, 1), count = c(length(lon), length(lat)))
  ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncatt_put(nc, 0, "created_by", "WHEP export_lpjml_inputs_netcdf.R")
  ncatt_put(nc, 0, "created_date", as.character(Sys.time()))
}

write_nc_4d_pft_time <- function(
  path, var_name, long_name, units, lon, lat, pft_vals, time_vals, data_dt,
  fill_value = 0, missval = -1.175494e+38, units_time = "years since 0000-1-1 0:0:0",
  chunk_size = 100L
) {
  data_dt <- as.data.table(data_dt)
  required_cols <- c("pft", "row", "col", "value")
  missing_cols <- setdiff(required_cols, names(data_dt))
  if (length(missing_cols) > 0) {
    stop("write_nc_4d_pft_time(): missing columns: ", paste(missing_cols, collapse = ", "))
  }
  time_col <- if ("time" %in% names(data_dt)) "time" else if ("year" %in% names(data_dt)) {
    "year"
  } else {
    stop("write_nc_4d_pft_time(): data_dt must contain 'time' or 'year'.")
  }
  nlon <- length(lon); nlat <- length(lat); npft <- length(pft_vals)
  dlon  <- ncdim_def("longitude", "degrees_east",  vals = lon)
  dlat  <- ncdim_def("latitude",  "degrees_north", vals = lat)
  dpft  <- ncdim_def("pft",  "plant functional types", vals = pft_vals)
  dtime <- ncdim_def("time", units_time, vals = time_vals)
  v <- ncvar_def(
    name = var_name, units = units, dim = list(dlon, dlat, dpft, dtime),
    missval = missval, longname = long_name, prec = "float", compression = 4
  )
  nc <- nc_create(path, vars = list(v), force_v4 = TRUE)
  on.exit(nc_close(nc), add = TRUE)
  setkeyv(data_dt, time_col)
  ntime  <- length(time_vals)
  chunks <- split(seq_len(ntime), ceiling(seq_len(ntime) / chunk_size))
  for (chunk in chunks) {
    ys  <- time_vals[chunk]
    dty <- data_dt[J(ys), nomatch = 0L]
    m4  <- array(fill_value, dim = c(nlon, nlat, npft, length(chunk)))
    if (nrow(dty) > 0) {
      ti_pos <- match(dty[[time_col]], ys)
      pi     <- match(dty$pft, pft_vals)
      flat   <- (ti_pos - 1L) * nlon * nlat * npft +
                (pi     - 1L) * nlon * nlat +
                (dty$row - 1L) * nlon + dty$col
      m4[flat] <- dty$value
    }
    ncvar_put(nc, v, m4, start = c(1L, 1L, 1L, chunk[1L]),
              count = c(nlon, nlat, npft, length(chunk)))
    .msg("  wrote times %s-%s (%d/%d)", min(ys), max(ys), max(chunk), ntime)
  }
  ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncatt_put(nc, 0, "created_by", "WHEP export_lpjml_inputs_netcdf.R")
  ncatt_put(nc, 0, "created_date", as.character(Sys.time()))
}

.write_dep_monthly <- function(out_path, var_name, value_col, grid, dep_monthly, time_vals) {
  ntime <- length(time_vals)
  nlon  <- grid$nlon; nlat <- grid$nlat
  dlon  <- ncdim_def("longitude", "degrees_east",  vals = grid$lon)
  dlat  <- ncdim_def("latitude",  "degrees_north", vals = grid$lat)
  dtime <- ncdim_def("time", "months since 0000-1-1 0:0:0", vals = time_vals, unlim = FALSE)
  v <- ncvar_def(
    name = var_name, units = "g/m2/day", dim = list(dlon, dlat, dtime),
    missval = -1.175494e+38,
    longname = sprintf("%s deposition (WHEP/HaNi)", var_name),
    prec = "float", compression = 4
  )
  .msg("  building %s array (%d months)...", var_name, ntime)
  m3 <- array(0, dim = c(nlon, nlat, ntime))
  ti_idx <- match(dep_monthly$time_idx, time_vals)
  valid  <- !is.na(ti_idx)
  if (any(valid)) {
    sub  <- dep_monthly[valid]
    flat <- (ti_idx[valid] - 1L) * nlon * nlat + (sub$row - 1L) * nlon + sub$col
    m3[flat] <- sub[[value_col]]
  }
  .msg("  writing %s NetCDF...", var_name)
  nc <- nc_create(out_path, vars = list(v), force_v4 = TRUE)
  on.exit(nc_close(nc), add = TRUE)
  ncvar_put(nc, v, m3)
  ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncatt_put(nc, 0, "created_by", "WHEP export_lpjml_inputs_netcdf.R")
  ncatt_put(nc, 0, "created_date", as.character(Sys.time()))
}

# ---- CFT / PFT mappings -------------------------------------------------

pft16 <- c(
  "temperate cereals", "rice", "maize", "tropical cereals", "pulses",
  "temperate roots", "tropical roots", "oil crops sunflower",
  "oil crops soybean", "oil crops groundnut", "oil crops rapeseed",
  "sugarcane", "others", "grassland", "biomass grass", "biomass tree"
)

cft_to_pft <- c(
  temperate_cereals = 1L, rice = 2L, maize = 3L, tropical_cereals = 4L,
  pulses = 5L, temperate_roots = 6L, tropical_roots = 7L,
  oil_crops_sunflower = 8L, oil_crops_soybean = 9L,
  oil_crops_groundnut = 10L, oil_crops_rapeseed = 11L, sugarcane = 12L,
  others_annual = 13L, others_perennial = 13L, oil_crops_other = 13L
)

# ---- Private helpers ----------------------------------------------------

.filter_years <- function(dt, export_years) {
  if (!is.null(export_years) && "year" %in% names(dt)) {
    dt <- dt[year %in% export_years]
  }
  dt
}

.downstream_vec <- function(start, nextcell, max_steps = 60L) {
  out <- integer(max_steps)
  n <- 0L
  cur <- nextcell[start]
  while (cur > 0L && n < max_steps) {
    n <- n + 1L
    out[n] <- cur
    cur <- nextcell[cur]
  }
  out[seq_len(n)]
}

.upstream_vec <- function(start, parents_list, max_steps = 60L) {
  out <- integer(max_steps)
  n <- 0L
  q <- parents_list[[start]]
  while (length(q) > 0L && n < max_steps) {
    cur <- q[1L]
    q <- q[-1L]
    n <- n + 1L
    out[n] <- cur
    q <- c(q, parents_list[[cur]])
  }
  out[seq_len(n)]
}

# ---- Main ---------------------------------------------------------------

main <- function(lfiles_dir, export_years = NULL, climate_dir = NULL) {
  if (!dir.exists(lfiles_dir)) {
    stop("lfiles_dir not found: ", lfiles_dir)
  }
  lfiles_dir <- normalizePath(lfiles_dir, mustWork = TRUE)
  whep_dir   <- file.path(lfiles_dir, "whep")
  src_inputs <- file.path(whep_dir, "inputs")
  out_root   <- file.path(whep_dir, "lpjml_inputs")

  .msg("Source WHEP inputs: %s", src_inputs)
  .msg("Target LPJmL-like root: %s", out_root)

  lpjml_dirs <- c("climate", "gadm", "lakes_rivers", "landuse", "nitrogen",
                  "river_routing", "soil")
  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
  for (d in lpjml_dirs) {
    dir.create(file.path(out_root, d), recursive = TRUE, showWarnings = FALSE)
  }

  grid        <- make_target_grid()
  row_area_ha <- cell_area_ha_by_lat(grid$lat)
  nlon_g      <- grid$nlon
  nlat_g      <- grid$nlat

  ddm_dlon <- c(0, 0.5, 0.5, 0, -0.5, -0.5, -0.5, 0, 0.5)
  ddm_dlat <- c(0, 0, -0.5, -0.5, -0.5, 0, 0.5, 0.5, 0.5)
  search_radius_m <- 75000

  status <- .new_status()

  # ---- 1) GADM-like static grids ----------------------------------------
  country_grid_file <- file.path(src_inputs, "country_grid.parquet")
  if (file.exists(country_grid_file)) {
    cg <- as.data.table(read_parquet(country_grid_file))
    cg <- coord_to_rowcol(cg, grid)

    cow_lpjml <- as.data.table(read.csv(
      system.file("extdata", "cow_to_lpjml.csv", package = "whep")
    ))
    cg <- cow_lpjml[cg, on = "area_code"]
    cg_unmatched <- cg[is.na(lpjml_code)]
    if (nrow(cg_unmatched) > 0L) {
      warning(sprintf(
        "%d cells with area_code not in cow_to_lpjml.csv (set to 0): %s",
        nrow(cg_unmatched), paste(sort(unique(cg_unmatched$area_code)), collapse = ", ")
      ))
    }
    cg_matched <- cg[!is.na(lpjml_code)]

    m_country <- new_slice(grid$nlon, grid$nlat, fill = 0)
    m_country[cbind(cg_matched$col, cg_matched$row)] <- cg_matched$lpjml_code
    out_country <- file.path(out_root, "gadm", "cow_gadm_30arcmin.nc")
    write_nc_2d(out_country, "countrycode",
      "LPJmL country index (per managepar.h) mapped to grid", "index",
      m_country, grid$lon, grid$lat, prec = "integer", missval = -9999)
    status <- .add_status(status, "countrycode", country_grid_file, out_country, "generated")

    m_coord <- new_slice(grid$nlon, grid$nlat, fill = 0)
    ord <- order(cg$row, cg$col)
    m_coord[cbind(cg$col[ord], cg$row[ord])] <- seq_len(nrow(cg))
    out_coord <- file.path(out_root, "gadm", "grid_gadm_30arcmin.nc")
    write_nc_2d(out_coord, "coord", "Land-cell index on LPJmL grid", "index",
      m_coord, grid$lon, grid$lat, prec = "integer", missval = -9999)
    status <- .add_status(status, "coord", country_grid_file, out_coord, "generated")

    m_landfrac <- new_slice(grid$nlon, grid$nlat, fill = 0)
    m_landfrac[cbind(cg$col, cg$row)] <- 1
    out_landfrac <- file.path(out_root, "gadm", "landfrac_gadm_30arcmin.nc")
    write_nc_2d(out_landfrac, "landfrac", "Land fraction (binary from country grid)", "1",
      m_landfrac, grid$lon, grid$lat, prec = "float")
    status <- .add_status(status, "landfrac", country_grid_file, out_landfrac, "generated")
  } else {
    status <- .add_status(status, "countrycode", country_grid_file, "", "missing_source")
    status <- .add_status(status, "coord",       country_grid_file, "", "missing_source")
    status <- .add_status(status, "landfrac",    country_grid_file, "", "missing_source")
  }

  # ---- 2) Soil -----------------------------------------------------------
  soil_file <- file.path(src_inputs, "soil.parquet")
  if (file.exists(soil_file)) {
    s <- as.data.table(read_parquet(soil_file))
    s <- coord_to_rowcol(s, grid)

    m_soil_type <- new_slice(grid$nlon, grid$nlat, fill = NA_real_)
    m_soil_type[cbind(s$col, s$row)] <- as.numeric(s$soil_texture_code)
    out_soil_type <- file.path(out_root, "soil", "soil_30arcmin_13_types.nc")
    write_nc_2d(out_soil_type, "soil_type", "USDA soil texture class (1-13)", "index",
      m_soil_type, grid$lon, grid$lat, prec = "float")
    status <- .add_status(status, "soil_type", soil_file, out_soil_type, "generated")

    m_soil_ph <- new_slice(grid$nlon, grid$nlat, fill = NA_real_)
    m_soil_ph[cbind(s$col, s$row)] <- as.numeric(s$soil_ph)
    out_soil_ph <- file.path(out_root, "soil", "soil_pH_30arcmin.nc")
    write_nc_2d(out_soil_ph, "soil_pH", "Top-soil pH", "pH",
      m_soil_ph, grid$lon, grid$lat, prec = "float")
    status <- .add_status(status, "soil_pH", soil_file, out_soil_ph, "generated")
  } else {
    status <- .add_status(status, "soil_type", soil_file, "", "missing_source")
    status <- .add_status(status, "soil_pH",   soil_file, "", "missing_source")
  }

  # ---- 3) Drainage routing -----------------------------------------------
  drainage_file <- file.path(src_inputs, "drainage.parquet")
  if (file.exists(drainage_file)) {
    dr <- as.data.table(read_parquet(drainage_file))
    dr[, flat_idx := flat_of(lon, lat, nlon_g)]
    land_flat <- dr$flat_idx
    valid <- !is.na(dr$flow_direction) & dr$flow_direction > 0L
    dr[, nx_lon := lon]
    dr[, nx_lat := lat]
    dr[valid, nx_lon := round((lon + ddm_dlon[flow_direction + 1L]) * 2) / 2]
    dr[valid, nx_lat := round((lat + ddm_dlat[flow_direction + 1L]) * 2) / 2]
    dr[, nx_flat := -1L]
    dr[valid, nx_flat := flat_of(nx_lon, nx_lat, nlon_g)]
    dr[valid & !(nx_flat %in% land_flat), nx_flat := -1L]
    dr[, riverlen := 0]
    dr[nx_flat != -1L, riverlen := haver_m(lon, lat, nx_lon, nx_lat)]

    m_index    <- matrix(-9999L, nrow = nlon_g, ncol = nlat_g)
    m_riverlen <- matrix(0,      nrow = nlon_g, ncol = nlat_g)
    dr_rc <- coord_to_rowcol(dr, grid)
    m_index[cbind(dr_rc$col,    dr_rc$row)] <- dr_rc$nx_flat
    m_riverlen[cbind(dr_rc$col, dr_rc$row)] <- dr_rc$riverlen

    out_drain <- file.path(out_root, "river_routing", "river_routing.nc")
    dlon_dim <- ncdim_def("longitude", "degrees_east",  grid$lon)
    dlat_dim <- ncdim_def("latitude",  "degrees_north", grid$lat)
    v_index <- ncvar_def("index", "1", list(dlon_dim, dlat_dim), missval = -9999L,
      longname = "Flat 2D index of downstream cell", prec = "integer", compression = 4)
    v_riverlen <- ncvar_def("riverlen", "m", list(dlon_dim, dlat_dim), missval = -9999,
      longname = "River length to downstream cell (m)", prec = "float", compression = 4)
    nc <- nc_create(out_drain, vars = list(v_index, v_riverlen), force_v4 = TRUE)
    ncvar_put(nc, v_index,    m_index)
    ncvar_put(nc, v_riverlen, m_riverlen)
    ncatt_put(nc, 0, "Conventions", "CF-1.8")
    ncatt_put(nc, 0, "created_by", "WHEP export_lpjml_inputs_netcdf.R")
    ncatt_put(nc, 0, "created_date", as.character(Sys.time()))
    nc_close(nc)

    .msg("  river_routing.nc: %d routed cells, %d sinks/ocean",
      sum(dr$nx_flat != -1L), sum(dr$nx_flat == -1L))
    status <- .add_status(status, "drainage", drainage_file, out_drain, "generated")
  } else {
    status <- .add_status(status, "drainage", drainage_file, "", "missing_source")
  }

  # ---- 4) Irrigation neighbour index -------------------------------------
  neighbour_file <- file.path(src_inputs, "country_grid.parquet")
  if (file.exists(drainage_file) && file.exists(neighbour_file)) {
    dr_n <- as.data.table(read_parquet(drainage_file))
    cgi  <- as.data.table(read_parquet(neighbour_file))
    cgi  <- dr_n[cgi, on = c("lon", "lat")]
    cgi[, cell := .I]
    N <- nrow(cgi)

    cgi[, flat_idx := flat_of(lon, lat, nlon_g)]
    lon_all  <- cgi$lon; lat_all <- cgi$lat; flat_all <- cgi$flat_idx

    grid_lookup <- matrix(-1L, nrow = nlon_g, ncol = nlat_g)
    grid_lookup[cbind(lon_idx_of(lon_all) + 1L, lat_idx_of(lat_all) + 1L)] <- cgi$cell

    nextcell <- integer(N)
    ddir <- cgi[!is.na(flow_direction) & flow_direction > 0L]
    nx_lon <- round((ddir$lon + ddm_dlon[ddir$flow_direction + 1L]) * 2) / 2
    nx_lat <- round((ddir$lat + ddm_dlat[ddir$flow_direction + 1L]) * 2) / 2
    nx_loi <- lon_idx_of(nx_lon) + 1L
    nx_li  <- lat_idx_of(nx_lat) + 1L
    in_bounds <- nx_loi >= 1L & nx_loi <= nlon_g & nx_li >= 1L & nx_li <= nlat_g
    nx_cell <- integer(nrow(ddir))
    nx_cell[in_bounds] <- grid_lookup[cbind(nx_loi[in_bounds], nx_li[in_bounds])]
    nextcell[ddir$cell] <- pmax(nx_cell, 0L)

    in_deg <- tabulate(nextcell[nextcell > 0L], nbins = N)
    upstream_count <- rep(1L, N)
    queue <- which(in_deg == 0L)
    qi <- 1L
    while (qi <= length(queue)) {
      ci <- queue[qi]; qi <- qi + 1L
      parent <- nextcell[ci]
      if (parent > 0L) {
        upstream_count[parent] <- upstream_count[parent] + upstream_count[ci]
        in_deg[parent] <- in_deg[parent] - 1L
        if (in_deg[parent] == 0L) queue <- c(queue, parent)
      }
    }

    parents_list <- vector("list", N)
    valid_nc <- which(nextcell > 0L)
    tmp <- split(valid_nc, nextcell[valid_nc])
    parents_list[as.integer(names(tmp))] <- tmp

    search_cells <- ceiling(search_radius_m / (111111 * 0.5)) + 1L
    ncores <- parallel::detectCores(logical = FALSE) %||% 1L
    .msg("  neighbour_irrig: searching %d cells on %d cores...", N, ncores)
    neighbour_flat <- unlist(parallel::mclapply(seq_len(N), function(i) {
      loi0  <- lon_idx_of(lon_all[i]) + 1L
      li0   <- lat_idx_of(lat_all[i]) + 1L
      loi_r <- seq(max(1L, loi0 - search_cells), min(nlon_g, loi0 + search_cells))
      li_r  <- seq(max(1L, li0  - search_cells), min(nlat_g, li0  + search_cells))
      cands <- as.integer(grid_lookup[loi_r, li_r])
      cands <- cands[cands > 0L & cands != i]
      if (length(cands) == 0L) return(flat_all[i])
      dist_m <- haver_m(lon_all[i], lat_all[i], lon_all[cands], lat_all[cands])
      keep   <- dist_m <= search_radius_m
      cands  <- cands[keep]; dist_m <- dist_m[keep]
      if (length(cands) == 0L) return(flat_all[i])
      excl  <- c(.downstream_vec(i, nextcell), .upstream_vec(i, parents_list))
      cands <- cands[!cands %in% excl]
      if (length(cands) == 0L) return(flat_all[i])
      dist_m <- haver_m(lon_all[i], lat_all[i], lon_all[cands], lat_all[cands])
      best   <- cands[which.max(upstream_count[cands] / dist_m^2)]
      flat_all[best]
    }, mc.cores = ncores), use.names = FALSE)

    m_neighbour <- matrix(-9999L, nrow = nlon_g, ncol = nlat_g)
    m_neighbour[cbind(lon_idx_of(lon_all) + 1L, lat_idx_of(lat_all) + 1L)] <- neighbour_flat
    out_neighbour <- file.path(out_root, "river_routing",
      "neighbour_irrig_30arcmin_75000m_radius_exclude_downstream_exclude_upstream_idw.nc")
    v_neigh <- ncvar_def("neighbour", "index", list(dlon_dim, dlat_dim), missval = -9999L,
      longname = "Flat 2D index of irrigation neighbour cell", prec = "integer", compression = 4)
    nc_neigh <- nc_create(out_neighbour, vars = list(v_neigh), force_v4 = TRUE)
    ncvar_put(nc_neigh, v_neigh, m_neighbour)
    ncatt_put(nc_neigh, 0, "Conventions", "CF-1.8")
    ncatt_put(nc_neigh, 0, "search_radius_m", search_radius_m)
    ncatt_put(nc_neigh, 0, "created_by", "WHEP export_lpjml_inputs_netcdf.R")
    ncatt_put(nc_neigh, 0, "created_date", as.character(Sys.time()))
    nc_close(nc_neigh)
    .msg("  neighbour_irrig.nc: %d land cells, %d self-assigned",
      sum(neighbour_flat != flat_all), sum(neighbour_flat == flat_all))
    status <- .add_status(status, "neighbour_irrig", neighbour_file, out_neighbour, "generated")
  } else {
    status <- .add_status(status, "neighbour_irrig", neighbour_file, "", "missing_source")
  }

  # ---- 5) Lakes and rivers -----------------------------------------------
  lakes_file <- file.path(src_inputs, "lakes_rivers.parquet")
  if (file.exists(lakes_file)) {
    lr <- as.data.table(read_parquet(lakes_file))
    lr <- coord_to_rowcol(lr, grid)
    m_lakes <- new_slice(grid$nlon, grid$nlat, fill = 0)
    m_lakes[cbind(lr$col, lr$row)] <- pmin(1, pmax(0, lr$lake_fraction + lr$river_fraction))
    out_lakes <- file.path(out_root, "lakes_rivers", "glwd_lakes_and_rivers_30arcmin.nc")
    write_nc_2d(out_lakes, "lakes", "Lake and river fraction", "1",
      m_lakes, grid$lon, grid$lat, prec = "float")
    status <- .add_status(status, "lakes_rivers", lakes_file, out_lakes, "generated")
  } else {
    status <- .add_status(status, "lakes_rivers", lakes_file, "", "missing_source",
      "Optional hydrology source not found (prepare_hydrology_inputs.R).")
  }

  # ---- 6) Land-use fractions ---------------------------------------------
  landuse_file <- file.path(whep_dir, "gridded_landuse.parquet")
  if (file.exists(landuse_file)) {
    lu <- .filter_years(as.data.table(read_parquet(landuse_file)), export_years)
    lu[, base_pft := as.integer(cft_to_pft[cft_name])]
    lu <- lu[!is.na(base_pft)]
    lu <- coord_to_rowcol(lu, grid)
    lu[, cell_area_ha := row_area_ha[row]]
    lu[, rainfed_frac   := pmin(1, pmax(0, rainfed_ha   / cell_area_ha))]
    lu[, irrigated_frac := pmin(1, pmax(0, irrigated_ha / cell_area_ha))]
    rf <- lu[, .(value = sum(rainfed_frac,   na.rm = TRUE)),
             by = .(year, pft = base_pft,        row, col)]
    ir <- lu[, .(value = sum(irrigated_frac, na.rm = TRUE)),
             by = .(year, pft = base_pft + 16L,  row, col)]
    lu_out <- rbind(rf, ir)
    lu_out[, value := pmin(1, pmax(0, value))]
    years_lu <- sort(unique(lu_out$year))
    out_lu <- file.path(out_root, "landuse", sprintf(
      "cft_default_cft_aggregation_30min_%d-%d.nc", min(years_lu), max(years_lu)
    ))
    .msg("Writing landuse NetCDF: %s", out_lu)
    write_nc_4d_pft_time(out_lu, "landuse", "land use fraction", "1",
      grid$lon, grid$lat, 1:32, years_lu, lu_out, fill_value = 0)
    status <- .add_status(status, "landuse", landuse_file, out_lu, "generated",
      "Mapped 14 WHEP CFTs into LPJmL 16-CFT order; grassland/biomass set to 0.")
  } else {
    status <- .add_status(status, "landuse", landuse_file, "", "missing_source")
  }

  # ---- 7) Fertilizer / manure by CFT -------------------------------------
  nitro_grid_file <- file.path(whep_dir, "gridded_nitrogen_cft.parquet")
  if (file.exists(nitro_grid_file)) {
    ng <- .filter_years(as.data.table(read_parquet(nitro_grid_file)), export_years)
    ng[, base_pft := as.integer(cft_to_pft[cft_name])]
    ng <- ng[!is.na(base_pft)]
    ng <- coord_to_rowcol(ng, grid)
    years_n <- sort(unique(ng$year))

    syn <- ng[fert_type == "Synthetic",
              .(value = mean(kg_n_ha, na.rm = TRUE)), by = .(year, pft = base_pft, row, col)]
    out_syn <- file.path(out_root, "landuse", sprintf(
      "fert_N_default_cft_aggregation_30min_%d-%d.nc", min(years_n), max(years_n)
    ))
    .msg("Writing synthetic fertilizer NetCDF: %s", out_syn)
    write_nc_4d_pft_time(out_syn, "fertilizer_nr", "Synthetic fertilizer nitrogen rate",
      "kgN ha-1 yr-1", grid$lon, grid$lat, 1:16, years_n, syn, fill_value = 0)
    status <- .add_status(status, "fertilizer_nr", nitro_grid_file, out_syn, "generated",
      "Using fert_type == Synthetic only; P/K kept separate in source parquet.")

    man <- ng[fert_type == "Manure",
              .(value = mean(kg_n_ha, na.rm = TRUE)), by = .(year, pft = base_pft, row, col)]
    out_man <- file.path(out_root, "landuse", sprintf(
      "manure_N_default_cft_aggregation_30min_%d-%d.nc", min(years_n), max(years_n)
    ))
    .msg("Writing manure NetCDF: %s", out_man)
    write_nc_4d_pft_time(out_man, "manure_nr", "Manure nitrogen rate",
      "kgN ha-1 yr-1", grid$lon, grid$lat, 1:16, years_n, man, fill_value = 0)
    status <- .add_status(status, "manure_nr", nitro_grid_file, out_man, "generated",
      "Using fert_type == Manure only; NSBNF/Deposition are not merged here.")
  } else {
    status <- .add_status(status, "fertilizer_nr", nitro_grid_file, "", "missing_source")
    status <- .add_status(status, "manure_nr",     nitro_grid_file, "", "missing_source")
  }

  # ---- 8) NHx / NOy deposition -------------------------------------------
  dep_file <- file.path(src_inputs, "n_deposition.parquet")
  if (file.exists(dep_file)) {
    dep <- .filter_years(as.data.table(read_parquet(dep_file)), export_years)
    if (!"lon" %in% names(dep)) {
      cg_dep <- as.data.table(read_parquet(country_grid_file))
      cg_dep <- coord_to_rowcol(cg_dep, grid)
      dep_cells <- merge(dep, cg_dep[, .(area_code, row, col)],
                         by = "area_code", allow.cartesian = TRUE)
    } else {
      dep_cells <- coord_to_rowcol(dep, grid)
    }
    has_split <- all(c("nhx", "noy") %in% names(dep_cells))
    if (has_split) {
      dep_cells[, value_nhx := nhx * 0.1 / 365]
      dep_cells[, value_noy := noy * 0.1 / 365]
      split_note <- "True NHx/NOy split from HaNi via n_deposition.parquet."
    } else {
      dep_cells[, value_nhx := deposit_kg_n_ha * 0.1 / 365 * 0.5]
      dep_cells[, value_noy := deposit_kg_n_ha * 0.1 / 365 * 0.5]
      split_note <- "nhx/noy columns absent; fell back to 50/50 of total deposit_kg_n_ha."
      .msg("WARNING: nhx/noy columns not found in parquet – using 50/50 fallback.")
    }
    years_d <- sort(unique(dep_cells$year))
    month_grid  <- CJ(year = years_d, month = 1L:12L)
    dep_monthly <- merge(month_grid, dep_cells, by = "year", allow.cartesian = TRUE)
    dep_monthly[, time_idx := (year * 12L) + (month - 1L)]
    time_vals <- sort(unique(dep_monthly$time_idx))

    out_nhx <- file.path(out_root, "nitrogen", sprintf(
      "ndep_nhx_whep_annual_%d_%d.nc4", min(years_d), max(years_d)
    ))
    out_noy <- file.path(out_root, "nitrogen", sprintf(
      "ndep_noy_whep_annual_%d_%d.nc4", min(years_d), max(years_d)
    ))
    .msg("Writing NHx deposition NetCDF (%d months): %s", length(time_vals), out_nhx)
    .write_dep_monthly(out_nhx, "nhx", "value_nhx", grid, dep_monthly, time_vals)
    .msg("Writing NOy deposition NetCDF (%d months): %s", length(time_vals), out_noy)
    .write_dep_monthly(out_noy, "noy", "value_noy", grid, dep_monthly, time_vals)

    status_txt <- if (has_split) "generated" else "generated_with_assumption"
    status <- .add_status(status, "nh4deposition", dep_file, out_nhx, status_txt, split_note)
    status <- .add_status(status, "no3deposition", dep_file, out_noy, status_txt, split_note)
  } else {
    status <- .add_status(status, "nh4deposition", dep_file, "", "missing_source")
    status <- .add_status(status, "no3deposition", dep_file, "", "missing_source")
  }

  # ---- 9) Climate (symlinks) ---------------------------------------------
  climate_mapping <- list(
    temp    = "cru_ts_3_10.1901.2009.tmp.dat.nc",
    prec    = "cru_ts_3_10_01.1901.2009.pre.dat.nc",
    cloud   = "cru_ts_3_10.1901.2009.cld.dat.nc",
    wind    = "wind_gswp3-w5e5_1901_2016_monthly.nc",
    co2     = "historical_CO2_annual_1765_2018.txt",
    wetdays = "cru_ts3.20.1901.2011.wet.dat.nc"
  )
  if (!is.null(climate_dir) && dir.exists(climate_dir)) {
    for (nm in names(climate_mapping)) {
      src_path <- file.path(climate_dir, climate_mapping[[nm]])
      dst_path <- file.path(out_root, "climate", climate_mapping[[nm]])
      if (file.exists(src_path)) {
        if (!file.exists(dst_path)) file.symlink(src_path, dst_path)
        status <- .add_status(status, nm, src_path, dst_path, "symlinked",
          "Symlinked from external LPJmL repository.")
      } else {
        status <- .add_status(status, nm, "external climate forcing", "", "missing_source",
          sprintf("Expected %s not found in %s", climate_mapping[[nm]], climate_dir))
      }
    }
  } else {
    for (nm in names(climate_mapping)) {
      status <- .add_status(status, nm, "external climate forcing", "", "not_generated_here",
        "Use LPJmL climate inputs (CRU/ISIMIP) from dedicated data pipeline.")
    }
  }

  # ---- Report ------------------------------------------------------------
  status_csv <- file.path(out_root, "comparison_status.csv")
  fwrite(status, status_csv)

  report_md <- file.path(out_root, "comparison_report.md")
  con <- file(report_md, open = "wt")
  writeLines("# WHEP -> LPJmL Input Comparison", con)
  writeLines("", con)
  writeLines(sprintf("- Generated on: %s", as.character(Sys.time())), con)
  writeLines(sprintf("- Source WHEP dir: %s", whep_dir), con)
  writeLines(sprintf("- Target LPJmL-like dir: %s", out_root), con)
  writeLines("", con)
  writeLines("## Status Legend", con)
  writeLines("", con)
  writeLines("- `generated`: direct export from WHEP outputs", con)
  writeLines("- `generated_with_assumption`: exported using documented approximation", con)
  writeLines("- `missing_source`: required source parquet not found", con)
  writeLines("- `not_generated_here`: not part of current WHEP generation pipeline", con)
  writeLines("", con)
  writeLines("## Inputs", con)
  writeLines("", con)
  for (i in seq_len(nrow(status))) {
    r <- status[i]
    line <- sprintf("- **%s**: %s%s", r$input_name, r$status,
      if (nzchar(r$output_file)) sprintf(" -> `%s`", r$output_file) else "")
    writeLines(line, con)
    if (nzchar(r$note))   writeLines(sprintf("  - note: %s",   r$note),   con)
    if (nzchar(r$source)) writeLines(sprintf("  - source: %s", r$source), con)
  }
  close(con)

  .msg("Done.")
  .msg("Wrote: %s", status_csv)
  .msg("Wrote: %s", report_md)
  invisible(status)
}

# Run if executed directly (not just sourced)
if (sys.nframe() == 0L) {
  stop(
    "Call main() with required paths, e.g.:\n",
    "  main(\n",
    "    lfiles_dir   = \"/path/to/L_files\",\n",
    "    export_years = 1850:2020,\n",
    "    climate_dir  = \"~/LPJmL/inputs/climate\"\n",
    "  )"
  )
}
