################################################################################
# Generate neighbour_irrig NetCDF for LPJmL irrigation network
#
# For each land cell, find the best irrigation neighbour within 75 km:
#   - Exclude upstream and downstream cells (river network)
#   - Score remaining candidates by upstream_area / dist^2 (IDW)
#   - Default to self if no valid neighbour found
#
# Output format: 2D lon/lat NetCDF storing the flat grid index
# (lat_idx_from_north * nlon + lon_idx) of the chosen neighbour cell,
# which is what LPJmL initdrain.c expects for CDF format.
#
# Requires: country_grid.parquet, drainage.parquet
################################################################################

library(data.table)
library(nanoparquet)
library(ncdf4)
library(cli)

# ---- Configuration --------------------------------------------------------
search_radius_m <- 75000
res             <- 0.5
l_files_dir <- Sys.getenv("WHEP_LFILES_DIR")
input_dir   <- file.path(l_files_dir, "whep", "inputs")
output_dir  <- file.path(l_files_dir, "whep", "lpjml_inputs", "river_routing")
out_file <- file.path(
  output_dir,
  "neighbour_irrig_30arcmin_75000m_radius_exclude_downstream_exclude_upstream_idw.nc"
)

# ---- Load data ------------------------------------------------------------
cli_h1("Loading inputs")
cg <- as.data.table(read_parquet(file.path(input_dir, "country_grid.parquet")))
dr <- as.data.table(read_parquet(file.path(input_dir, "drainage.parquet")))
cg <- dr[cg, on = c("lon", "lat")]
cg[, cell := .I]  # 1-based throughout
N <- nrow(cg)
cli_alert_info("Land cells: {N}")

# ---- NetCDF grid ----------------------------------------------------------
lon_vec <- seq(-179.75, 179.75, by =  0.5)   # 720, west to east
lat_vec <- seq( 83.75, -59.25, by = -0.5)   # 287, north to south
nlon <- length(lon_vec)
nlat <- length(lat_vec)

# Flat index matching LPJmL getindexinput_netcdf():
# lat stored north-to-south so lat_idx=0 is northernmost row
lat_idx_of <- function(lat) as.integer(round((83.75 - lat) / 0.5))
lon_idx_of <- function(lon) as.integer(round((lon + 179.75) / 0.5))
flat_of    <- function(lon, lat) lat_idx_of(lat) * nlon + lon_idx_of(lon)

cg[, flat_idx := flat_of(lon, lat)]
lon_all  <- cg$lon
lat_all  <- cg$lat
flat_all <- cg$flat_idx

# ---- Grid lookup matrix [lon_idx+1, lat_idx+1] -> cell (1-based) ----------
grid_lookup <- matrix(-1L, nrow = nlon, ncol = nlat)
grid_lookup[cbind(lon_idx_of(lon_all) + 1L, lat_idx_of(lat_all) + 1L)] <- cg$cell

# ---- Flow network ---------------------------------------------------------
# DDM30 convention: 0=sink, 1=E, 2=SE, 3=S, 4=SW, 5=W, 6=NW, 7=N, 8=NE
ddm_dlon <- c(0, 0.5, 0.5, 0, -0.5, -0.5, -0.5, 0, 0.5)
ddm_dlat <- c(0, 0, -0.5, -0.5, -0.5, 0, 0.5, 0.5, 0.5)

cli_h1("Building flow network")
nextcell <- integer(N)   # nextcell[i] = downstream cell (1-based); 0 = sink
vdr      <- cg[!is.na(flow_direction) & flow_direction > 0L]
nx_lon   <- round((vdr$lon + ddm_dlon[vdr$flow_direction + 1L]) * 2) / 2
nx_lat   <- round((vdr$lat + ddm_dlat[vdr$flow_direction + 1L]) * 2) / 2
nx_loi   <- lon_idx_of(nx_lon) + 1L
nx_li    <- lat_idx_of(nx_lat) + 1L
in_bounds <- nx_loi >= 1L & nx_loi <= nlon & nx_li >= 1L & nx_li <= nlat
nx_cell   <- integer(nrow(vdr))
nx_cell[in_bounds]  <- grid_lookup[cbind(nx_loi[in_bounds], nx_li[in_bounds])]
nextcell[vdr$cell] <- pmax(nx_cell, 0L)

# ---- Upstream area (topological sort) ------------------------------------
cli_h1("Computing upstream areas")
in_deg         <- tabulate(nextcell[nextcell > 0L], nbins = N)
upstream_count <- rep(1L, N)
queue          <- which(in_deg == 0L)
qi             <- 1L
while (qi <= length(queue)) {
  ci     <- queue[qi]; qi <- qi + 1L
  parent <- nextcell[ci]
  if (parent > 0L) {
    upstream_count[parent] <- upstream_count[parent] + upstream_count[ci]
    in_deg[parent]         <- in_deg[parent] - 1L
    if (in_deg[parent] == 0L) queue <- c(queue, parent)
  }
}
ua_all <- upstream_count

# ---- Reverse adjacency (for upstream exclusion) --------------------------
cli_h1("Building reverse adjacency")
valid_nc     <- which(nextcell > 0L)
parents_list <- vector("list", N)
tmp          <- split(valid_nc, nextcell[valid_nc])  # fast: named list by parent
for (nm in names(tmp))
  parents_list[[as.integer(nm)]] <- tmp[[nm]]
rm(tmp)

# ---- Trace helpers (limited depth to bound cost) -------------------------
downstream_vec <- function(start, max_steps = 60L) {
  out <- integer(max_steps); n <- 0L; cur <- nextcell[start]
  while (cur > 0L && n < max_steps) { n <- n + 1L; out[n] <- cur; cur <- nextcell[cur] }
  out[seq_len(n)]
}

upstream_vec <- function(start, max_steps = 60L) {
  out <- integer(max_steps); n <- 0L; q <- parents_list[[start]]
  while (length(q) > 0L && n < max_steps) {
    cur <- q[1L]; q <- q[-1L]
    n <- n + 1L; out[n] <- cur
    q <- c(q, parents_list[[cur]])
  }
  out[seq_len(n)]
}

# ---- Haversine (vectorised, metres) --------------------------------------
haver_m <- function(lon1, lat1, lon2, lat2) {
  r  <- 6371000
  p1 <- lat1 * pi / 180; p2 <- lat2 * pi / 180
  dp <- (lat2 - lat1) * pi / 180; dl <- (lon2 - lon1) * pi / 180
  a  <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  2 * r * asin(pmin(sqrt(a), 1))
}

# ---- Main neighbour search -----------------------------------------------
cli_h1("Finding irrigation neighbours")
search_cells  <- ceiling(search_radius_m / (111111 * res)) + 1L
neighbour_flat <- flat_all  # default: self

for (i in seq_len(N)) {
  if (i %% 5000L == 0L) cli_alert_info("  {i}/{N}")

  loi0 <- lon_idx_of(lon_all[i]) + 1L
  li0  <- lat_idx_of(lat_all[i]) + 1L

  loi_r <- seq(max(1L, loi0 - search_cells), min(nlon, loi0 + search_cells))
  li_r  <- seq(max(1L, li0  - search_cells), min(nlat, li0  + search_cells))

  cands <- as.integer(grid_lookup[loi_r, li_r])
  cands <- cands[cands > 0L & cands != i]
  if (length(cands) == 0L) next

  dist_m <- haver_m(lon_all[i], lat_all[i], lon_all[cands], lat_all[cands])
  keep   <- dist_m <= search_radius_m
  cands  <- cands[keep]; dist_m <- dist_m[keep]
  if (length(cands) == 0L) next

  excl  <- c(downstream_vec(i), upstream_vec(i))
  cands <- cands[!cands %in% excl]
  if (length(cands) == 0L) next

  dist_m <- haver_m(lon_all[i], lat_all[i], lon_all[cands], lat_all[cands])
  best   <- cands[which.max(ua_all[cands] / dist_m^2)]
  neighbour_flat[i] <- flat_all[best]
}

# ---- Write NetCDF ---------------------------------------------------------
cli_h1("Writing NetCDF")
m_out <- matrix(-9999L, nrow = nlon, ncol = nlat)
m_out[cbind(lon_idx_of(lon_all) + 1L, lat_idx_of(lat_all) + 1L)] <- neighbour_flat

dlon <- ncdim_def("longitude", "degrees_east",  lon_vec)
dlat <- ncdim_def("latitude",  "degrees_north", lat_vec)
vdef <- ncvar_def(
  "neighbour", "index", list(dlon, dlat),
  missval     = -9999L,
  longname    = "Flat 2D index (lat_idx*720+lon_idx) of irrigation neighbour cell",
  prec        = "integer",
  compression = 4
)
nc <- nc_create(out_file, vdef, force_v4 = TRUE)
ncvar_put(nc, vdef, m_out)
ncatt_put(nc, 0, "Conventions",        "CF-1.8")
ncatt_put(nc, 0, "search_radius_m",    search_radius_m)
ncatt_put(nc, 0, "exclude_upstream",   1L)
ncatt_put(nc, 0, "exclude_downstream", 1L)
ncatt_put(nc, 0, "weighting",          "IDW of upstream area")
ncatt_put(nc, 0, "created_by",         "WHEP prepare_neighbour_irrig.R")
ncatt_put(nc, 0, "created_date",       as.character(Sys.time()))
nc_close(nc)
cli_alert_success("Written: {out_file}")
