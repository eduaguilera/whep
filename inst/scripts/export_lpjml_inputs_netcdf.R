# ---------------------------------------------------------------------
# export_lpjml_inputs_netcdf.R
#
# Build a LPJmL-like input folder tree under WHEP outputs and export
# available WHEP parquet inputs to NetCDF.
#
# Output root:
#   <WHEP_LFILES_DIR>/whep/lpjml_inputs/
#
# Notes:
# - This script mirrors LPJmL directory structure (climate, gadm,
#   lakes_rivers, landuse, nitrogen, river_routing, soil).
# - Climate forcing is not generated here (reported as missing).
# - Deposition in WHEP is currently total annual N (country-level), so
#   NHx/NOy monthly fields are approximated and clearly flagged.
# ---------------------------------------------------------------------

suppressPackageStartupMessages({
    library(data.table)
    library(ncdf4)
    library(nanoparquet)
})

`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) y else x
}

export_years <- 2000:2001 # e.g., 2000:2001 to subset, or NULL for all available years
external_climate_dir <- path.expand("~/LPJmL/inputs/climate")

filter_years <- function(dt) {
    if (!is.null(export_years) && "year" %in% names(dt)) {
        dt <- dt[year %in% export_years]
    }
    dt
}

.msg <- function(...) {
    cat(sprintf(...), "\n")
}

find_lfiles_dir <- function() {
    candidates <- c(
        Sys.getenv("WHEP_LFILES_DIR", unset = ""),
        "/home/usuario/OneDrive/L_files"
    )
    candidates <- candidates[nzchar(candidates)]
    for (p in candidates) {
        if (dir.exists(p)) {
            return(p)
        }
    }
    stop("Could not resolve WHEP_LFILES_DIR. Set env var WHEP_LFILES_DIR.")
}

# LPJmL-like 0.5-degree grid used by existing LPJmL NetCDF in this setup.
# Matches: lon = 720, lat = 287, geotransform -180,0.5,0,84,0,-0.5
make_target_grid <- function() {
    lon <- seq(-179.75, 179.75, by = 0.5)
    lat <- seq(83.75, -59.25, by = -0.5)

    list(
        lon = lon,
        lat = lat,
        lon_key = sprintf("%.2f", lon),
        lat_key = sprintf("%.2f", lat),
        nlon = length(lon),
        nlat = length(lat)
    )
}

# Approximate 0.5-degree cell area (ha) by latitude row.
cell_area_ha_by_lat <- function(lat_vals) {
    # Spherical Earth approximation
    r <- 6371000
    dlon <- 0.5 * pi / 180
    lat_n <- (lat_vals + 0.25) * pi / 180
    lat_s <- (lat_vals - 0.25) * pi / 180
    area_m2 <- r * r * dlon * (sin(lat_n) - sin(lat_s))
    area_m2 / 10000
}

coord_to_rowcol <- function(dt, grid) {
    dt[, row := match(sprintf("%.2f", lat), grid$lat_key)]
    dt[, col := match(sprintf("%.2f", lon), grid$lon_key)]
    dt[!is.na(row) & !is.na(col)]
}

new_slice <- function(nlon, nlat, fill = 0) {
    matrix(fill, nrow = nlon, ncol = nlat)
}

write_nc_2d <- function(
  path,
  var_name,
  long_name,
  units,
  values_lon_lat,
  lon,
  lat,
  prec = "float",
  missval = -1.175494e+38
) {
    dlon <- ncdim_def("longitude", "degrees_east", vals = lon)
    dlat <- ncdim_def("latitude", "degrees_north", vals = lat)

    v <- ncvar_def(
        name = var_name,
        units = units,
        dim = list(dlon, dlat),
        missval = missval,
        longname = long_name,
        prec = prec,
        compression = 4
    )

    nc <- nc_create(path, vars = list(v), force_v4 = TRUE)
    on.exit(nc_close(nc), add = TRUE)

    ncvar_put(
        nc,
        v,
        values_lon_lat,
        start = c(1, 1),
        count = c(length(lon), length(lat))
    )
    ncatt_put(nc, 0, "Conventions", "CF-1.8")
    ncatt_put(nc, 0, "created_by", "WHEP export_lpjml_inputs_netcdf.R")
    ncatt_put(nc, 0, "created_date", as.character(Sys.time()))
}

write_nc_4d_pft_time <- function(
  path,
  var_name,
  long_name,
  units,
  lon,
  lat,
  pft_vals,
  time_vals,
  data_dt,
  fill_value = 0,
  missval = -1.175494e+38,
  units_time = "years since 0000-1-1 0:0:0"
) {
    data_dt <- as.data.table(data_dt)

    required_cols <- c("pft", "row", "col", "value")
    missing_cols <- setdiff(required_cols, names(data_dt))
    if (length(missing_cols) > 0) {
        stop(
            "write_nc_4d_pft_time(): missing required columns: ",
            paste(missing_cols, collapse = ", ")
        )
    }

    time_col <- if ("time" %in% names(data_dt)) {
        "time"
    } else if ("year" %in% names(data_dt)) {
        "year"
    } else {
        stop(
            "write_nc_4d_pft_time(): data_dt must contain either 'time' or 'year'."
        )
    }

    nlon <- length(lon)
    nlat <- length(lat)

    dlon <- ncdim_def("longitude", "degrees_east", vals = lon)
    dlat <- ncdim_def("latitude", "degrees_north", vals = lat)
    dpft <- ncdim_def("pft", "plant functional types", vals = pft_vals)
    dtime <- ncdim_def("time", units_time, vals = time_vals)

    v <- ncvar_def(
        name = var_name,
        units = units,
        dim = list(dlon, dlat, dpft, dtime),
        missval = missval,
        longname = long_name,
        prec = "float",
        compression = 4
    )

    nc <- nc_create(path, vars = list(v), force_v4 = TRUE)
    on.exit(nc_close(nc), add = TRUE)

    # Write slice by (time, pft) to keep memory bounded.
    for (ti in seq_along(time_vals)) {
        y <- time_vals[ti]
        dty <- data_dt[get(time_col) == y]

        for (pi in seq_along(pft_vals)) {
            p <- pft_vals[pi]
            dtp <- dty[get("pft") == p]

            m <- new_slice(nlon, nlat, fill = fill_value)
            if (nrow(dtp) > 0) {
                # row/col unique by construction (already aggregated)
                m[cbind(dtp$col, dtp$row)] <- dtp$value
            }

            ncvar_put(
                nc,
                v,
                m,
                start = c(1, 1, pi, ti),
                count = c(nlon, nlat, 1, 1)
            )
        }

        .msg("  wrote time %s (%d/%d)", y, ti, length(time_vals))
    }

    ncatt_put(nc, 0, "Conventions", "CF-1.8")
    ncatt_put(nc, 0, "created_by", "WHEP export_lpjml_inputs_netcdf.R")
    ncatt_put(nc, 0, "created_date", as.character(Sys.time()))
}

# LPJmL base order for 16 rainfed CFTs, then +16 irrigated for landuse.
pft16 <- c(
    "temperate cereals",
    "rice",
    "maize",
    "tropical cereals",
    "pulses",
    "temperate roots",
    "tropical roots",
    "oil crops sunflower",
    "oil crops soybean",
    "oil crops groundnut",
    "oil crops rapeseed",
    "sugarcane",
    "others",
    "grassland",
    "biomass grass",
    "biomass tree"
)

# WHEP cft_name -> LPJmL pft index (1..16)
cft_to_pft <- c(
    temperate_cereals = 1L,
    rice = 2L,
    maize = 3L,
    tropical_cereals = 4L,
    pulses = 5L,
    temperate_roots = 6L,
    tropical_roots = 7L,
    oil_crops_sunflower = 8L,
    oil_crops_soybean = 9L,
    oil_crops_groundnut = 10L,
    oil_crops_rapeseed = 11L,
    sugarcane = 12L,
    others_annual = 13L,
    others_perennial = 13L,
    oil_crops_other = 13L
)

lfiles_dir <- find_lfiles_dir()
whep_dir <- file.path(lfiles_dir, "whep")
src_inputs <- file.path(whep_dir, "inputs")
out_root <- file.path(whep_dir, "lpjml_inputs")

lpjml_dirs <- c(
    "climate",
    "gadm",
    "lakes_rivers",
    "landuse",
    "nitrogen",
    "river_routing",
    "soil"
)

dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
for (d in lpjml_dirs) {
    dir.create(file.path(out_root, d), recursive = TRUE, showWarnings = FALSE)
}

.msg("Source WHEP inputs: %s", src_inputs)
.msg("Target LPJmL-like root: %s", out_root)

grid <- make_target_grid()
row_area_ha <- cell_area_ha_by_lat(grid$lat)

status <- data.table(
    input_name = character(),
    source = character(),
    output_file = character(),
    status = character(),
    note = character()
)

add_status <- function(input_name, source, output_file, status_txt, note = "") {
    status <<- rbind(
        status,
        data.table(
            input_name = input_name,
            source = source,
            output_file = output_file,
            status = status_txt,
            note = note
        ),
        fill = TRUE
    )
}

# ------------------------------------------------------------------
# 1) GADM-like static grids from country_grid.parquet
# ------------------------------------------------------------------
country_grid_file <- file.path(src_inputs, "country_grid.parquet")
if (file.exists(country_grid_file)) {
    cg <- as.data.table(read_parquet(country_grid_file))
    cg <- coord_to_rowcol(cg, grid)

    # countrycode: map COW area_code → LPJmL country index per managepar.h
    cow_lpjml <- as.data.table(read.csv(
        system.file("extdata", "cow_to_lpjml.csv", package = "whep")
    ))
    cg <- cow_lpjml[cg, on = "area_code"]
    cg_unmatched <- cg[is.na(lpjml_code)]
    if (nrow(cg_unmatched) > 0L) {
        missing_codes <- sort(unique(cg_unmatched$area_code))
        warning(sprintf(
            "%d cells with area_code not in cow_to_lpjml.csv (set to 0): %s",
            nrow(cg_unmatched), paste(missing_codes, collapse = ", ")
        ))
    }
    cg_matched <- cg[!is.na(lpjml_code)]
    m_country <- new_slice(grid$nlon, grid$nlat, fill = 0)
    m_country[cbind(cg_matched$col, cg_matched$row)] <- cg_matched$lpjml_code

    out_country <- file.path(out_root, "gadm", "cow_gadm_30arcmin.nc")
    write_nc_2d(
        path = out_country,
        var_name = "countrycode",
        long_name = "LPJmL country index (per managepar.h) mapped to grid",
        units = "index",
        values_lon_lat = m_country,
        lon = grid$lon,
        lat = grid$lat,
        prec = "integer",
        missval = -9999
    )
    add_status("countrycode", country_grid_file, out_country, "generated")

    # coord/index grid
    m_coord <- new_slice(grid$nlon, grid$nlat, fill = 0)
    ord <- order(cg$row, cg$col)
    m_coord[cbind(cg$col[ord], cg$row[ord])] <- seq_len(nrow(cg))

    out_coord <- file.path(out_root, "gadm", "grid_gadm_30arcmin.nc")
    write_nc_2d(
        path = out_coord,
        var_name = "coord",
        long_name = "Land-cell index on LPJmL grid",
        units = "index",
        values_lon_lat = m_coord,
        lon = grid$lon,
        lat = grid$lat,
        prec = "integer",
        missval = -9999
    )
    add_status("coord", country_grid_file, out_coord, "generated")

    # land fraction (1 for land cell, 0 otherwise)
    m_landfrac <- new_slice(grid$nlon, grid$nlat, fill = 0)
    m_landfrac[cbind(cg$col, cg$row)] <- 1

    out_landfrac <- file.path(out_root, "gadm", "landfrac_gadm_30arcmin.nc")
    write_nc_2d(
        path = out_landfrac,
        var_name = "landfrac",
        long_name = "Land fraction (binary from country grid)",
        units = "1",
        values_lon_lat = m_landfrac,
        lon = grid$lon,
        lat = grid$lat,
        prec = "float"
    )
    add_status("landfrac", country_grid_file, out_landfrac, "generated")
} else {
    add_status("countrycode", country_grid_file, "", "missing_source")
    add_status("coord", country_grid_file, "", "missing_source")
    add_status("landfrac", country_grid_file, "", "missing_source")
}

# ------------------------------------------------------------------
# 2) Soil type and pH from soil.parquet
# ------------------------------------------------------------------
soil_file <- file.path(src_inputs, "soil.parquet")
if (file.exists(soil_file)) {
    s <- as.data.table(read_parquet(soil_file))
    s <- coord_to_rowcol(s, grid)

    m_soil_type <- new_slice(grid$nlon, grid$nlat, fill = NA_real_)
    m_soil_type[cbind(s$col, s$row)] <- as.numeric(s$soil_texture_code)

    out_soil_type <- file.path(out_root, "soil", "soil_30arcmin_13_types.nc")
    write_nc_2d(
        path = out_soil_type,
        var_name = "soil_type",
        long_name = "USDA soil texture class (1-13)",
        units = "index",
        values_lon_lat = m_soil_type,
        lon = grid$lon,
        lat = grid$lat,
        prec = "float"
    )
    add_status("soil_type", soil_file, out_soil_type, "generated")

    m_soil_ph <- new_slice(grid$nlon, grid$nlat, fill = NA_real_)
    m_soil_ph[cbind(s$col, s$row)] <- as.numeric(s$soil_ph)

    out_soil_ph <- file.path(out_root, "soil", "soil_pH_30arcmin.nc")
    write_nc_2d(
        path = out_soil_ph,
        var_name = "soil_pH",
        long_name = "Top-soil pH",
        units = "pH",
        values_lon_lat = m_soil_ph,
        lon = grid$lon,
        lat = grid$lat,
        prec = "float"
    )
    add_status("soil_pH", soil_file, out_soil_ph, "generated")
} else {
    add_status("soil_type", soil_file, "", "missing_source")
    add_status("soil_pH", soil_file, "", "missing_source")
}

# ------------------------------------------------------------------
# 3) Drainage routing from drainage.parquet
# ------------------------------------------------------------------
drainage_file <- file.path(src_inputs, "drainage.parquet")
if (file.exists(drainage_file)) {
    dr <- as.data.table(read_parquet(drainage_file))
    dr <- coord_to_rowcol(dr, grid)

    m_drain <- new_slice(grid$nlon, grid$nlat, fill = 0)
    m_drain[cbind(dr$col, dr$row)] <- as.numeric(dr$flow_direction)

    out_drain <- file.path(out_root, "river_routing", "river_routing.nc")
    write_nc_2d(
        path = out_drain,
        var_name = "drainage",
        long_name = "Flow direction (DDM30-like code)",
        units = "index",
        values_lon_lat = m_drain,
        lon = grid$lon,
        lat = grid$lat,
        prec = "integer",
        missval = -9999
    )
    add_status("drainage", drainage_file, out_drain, "generated")
} else {
    add_status("drainage", drainage_file, "", "missing_source")
}

# ------------------------------------------------------------------
# 4) Lakes and rivers fraction from optional lakes_rivers.parquet
# ------------------------------------------------------------------
lakes_file <- file.path(src_inputs, "lakes_rivers.parquet")
if (file.exists(lakes_file)) {
    lr <- as.data.table(read_parquet(lakes_file))
    lr <- coord_to_rowcol(lr, grid)

    m_lakes <- new_slice(grid$nlon, grid$nlat, fill = 0)
    frac <- pmin(1, pmax(0, lr$lake_fraction + lr$river_fraction))
    m_lakes[cbind(lr$col, lr$row)] <- frac

    out_lakes <- file.path(
        out_root,
        "lakes_rivers",
        "glwd_lakes_and_rivers_30arcmin.nc"
    )
    write_nc_2d(
        path = out_lakes,
        var_name = "lakes",
        long_name = "Lake and river fraction",
        units = "1",
        values_lon_lat = m_lakes,
        lon = grid$lon,
        lat = grid$lat,
        prec = "float"
    )
    add_status("lakes_rivers", lakes_file, out_lakes, "generated")
} else {
    add_status(
        "lakes_rivers",
        lakes_file,
        "",
        "missing_source",
        "Optional hydrology source not found (prepare_hydrology_inputs.R)."
    )
}

# ------------------------------------------------------------------
# 5) Land-use fractions (time x pft x lat x lon)
# ------------------------------------------------------------------
landuse_file <- file.path(whep_dir, "gridded_landuse.parquet")
if (file.exists(landuse_file)) {
    lu <- filter_years(as.data.table(read_parquet(landuse_file)))
    lu[, base_pft := as.integer(cft_to_pft[cft_name])]
    lu <- lu[!is.na(base_pft)]
    lu <- coord_to_rowcol(lu, grid)

    # Convert area (ha) to fraction of 0.5-degree cell area by latitude row.
    lu[, cell_area_ha := row_area_ha[row]]
    lu[, rainfed_frac := pmin(1, pmax(0, rainfed_ha / cell_area_ha))]
    lu[, irrigated_frac := pmin(1, pmax(0, irrigated_ha / cell_area_ha))]

    rf <- lu[,
        .(value = sum(rainfed_frac, na.rm = TRUE)),
        by = .(year, pft = base_pft, row, col)
    ]
    ir <- lu[,
        .(value = sum(irrigated_frac, na.rm = TRUE)),
        by = .(year, pft = base_pft + 16L, row, col)
    ]

    lu_out <- rbind(rf, ir)
    lu_out[, value := pmin(1, pmax(0, value))]

    years <- sort(unique(lu_out$year))
    pft_vals <- 1:32

    out_lu <- file.path(
        out_root,
        "landuse",
        sprintf(
            "cft_default_cft_aggregation_30min_%d-%d.nc",
            min(years),
            max(years)
        )
    )

    .msg("Writing landuse NetCDF: %s", out_lu)
    write_nc_4d_pft_time(
        path = out_lu,
        var_name = "landuse",
        long_name = "land use fraction",
        units = "1",
        lon = grid$lon,
        lat = grid$lat,
        pft_vals = pft_vals,
        time_vals = years,
        data_dt = lu_out,
        fill_value = 0
    )

    add_status(
        "landuse",
        landuse_file,
        out_lu,
        "generated",
        "Mapped 14 WHEP CFTs into LPJmL 16-CFT order; grassland/biomass set to 0."
    )
} else {
    add_status("landuse", landuse_file, "", "missing_source")
}

# ------------------------------------------------------------------
# 6) Fertilizer/manure by CFT (time x pft x lat x lon)
# ------------------------------------------------------------------
nitro_grid_file <- file.path(whep_dir, "gridded_nitrogen_cft.parquet")
if (file.exists(nitro_grid_file)) {
    ng <- filter_years(as.data.table(read_parquet(nitro_grid_file)))
    ng[, base_pft := as.integer(cft_to_pft[cft_name])]
    ng <- ng[!is.na(base_pft)]
    ng <- coord_to_rowcol(ng, grid)

    years_n <- sort(unique(ng$year))
    pft16_vals <- 1:16

    # Synthetic N only
    syn <- ng[
        fert_type == "Synthetic",
        .(value = mean(kg_n_ha, na.rm = TRUE)),
        by = .(year, pft = base_pft, row, col)
    ]

    out_syn <- file.path(
        out_root,
        "landuse",
        sprintf(
            "fert_N_default_cft_aggregation_30min_%d-%d.nc",
            min(years_n),
            max(years_n)
        )
    )
    .msg("Writing synthetic fertilizer NetCDF: %s", out_syn)
    write_nc_4d_pft_time(
        path = out_syn,
        var_name = "fertilizer_nr",
        long_name = "Synthetic fertilizer nitrogen rate",
        units = "kgN ha-1 yr-1",
        lon = grid$lon,
        lat = grid$lat,
        pft_vals = pft16_vals,
        time_vals = years_n,
        data_dt = syn,
        fill_value = 0
    )

    add_status(
        "fertilizer_nr",
        nitro_grid_file,
        out_syn,
        "generated",
        "Using fert_type == Synthetic only; P/K kept separate in source parquet."
    )

    # Manure N only
    man <- ng[
        fert_type == "Manure",
        .(value = mean(kg_n_ha, na.rm = TRUE)),
        by = .(year, pft = base_pft, row, col)
    ]

    out_man <- file.path(
        out_root,
        "landuse",
        sprintf(
            "manure_N_default_cft_aggregation_30min_%d-%d.nc",
            min(years_n),
            max(years_n)
        )
    )
    .msg("Writing manure NetCDF: %s", out_man)
    write_nc_4d_pft_time(
        path = out_man,
        var_name = "manure_nr",
        long_name = "Manure nitrogen rate",
        units = "kgN ha-1 yr-1",
        lon = grid$lon,
        lat = grid$lat,
        pft_vals = pft16_vals,
        time_vals = years_n,
        data_dt = man,
        fill_value = 0
    )

    add_status(
        "manure_nr",
        nitro_grid_file,
        out_man,
        "generated",
        "Using fert_type == Manure only; NSBNF/Deposition are not merged here."
    )
} else {
    add_status("fertilizer_nr", nitro_grid_file, "", "missing_source")
    add_status("manure_nr", nitro_grid_file, "", "missing_source")
}

# ------------------------------------------------------------------
# 7) NHx/NOy deposition from n_deposition.parquet (annual, gridded)
# ------------------------------------------------------------------
# Uses true NHx/NOy split from HaNi (stored in parquet columns nhx,
# noy) when available. Falls back to 50/50 of deposit_kg_n_ha if the
# columns are absent (e.g. parquet came from the Global CSV path).
# Time dimension is annual (years since 0000-1-1); LPJmL accepts both
# monthly and annual deposition inputs.
dep_file <- file.path(src_inputs, "n_deposition.parquet")
if (file.exists(dep_file) && file.exists(country_grid_file)) {
    dep <- filter_years(as.data.table(read_parquet(dep_file)))
    cg <- as.data.table(read_parquet(country_grid_file))
    cg <- coord_to_rowcol(cg, grid)

    dep_cells <- merge(
        dep,
        cg[, .(area_code, row, col)],
        by = "area_code",
        allow.cartesian = TRUE
    )

    # Determine NHx/NOy split -----------------------------------------
    has_split <- all(c("nhx", "noy") %in% names(dep_cells))
    if (has_split) {
        # Convert kgN/ha/yr -> gN/m2/yr  (1 kgN/ha = 0.1 gN/m2)
        dep_cells[, value_nhx := nhx * 0.1]
        dep_cells[, value_noy := noy * 0.1]
        split_note <- "True NHx/NOy split from HaNi via n_deposition.parquet."
    } else {
        dep_cells[, value_nhx := deposit_kg_n_ha * 0.1 * 0.5]
        dep_cells[, value_noy := deposit_kg_n_ha * 0.1 * 0.5]
        split_note <- "nhx/noy columns absent; fell back to 50/50 of total deposit_kg_n_ha."
        .msg("WARNING: nhx/noy columns not found in parquet – using 50/50 fallback.")
    }

    # Annual time axis ------------------------------------------------
    years_d <- sort(unique(dep_cells$year))
    ntime <- length(years_d)
    # "years since 0000-1-1 0:0:0" is the standard LPJmL annual axis
    time_vals <- as.numeric(years_d)

    write_dep_annual <- function(out_path, var_name, value_col) {
        dlon <- ncdim_def("longitude", "degrees_east", vals = grid$lon)
        dlat <- ncdim_def("latitude", "degrees_north", vals = grid$lat)
        dtime <- ncdim_def("time", "years since 0000-1-1 0:0:0",
            vals = time_vals, unlim = FALSE
        )

        v <- ncvar_def(
            name = var_name,
            units = "gN m-2 yr-1",
            dim = list(dlon, dlat, dtime),
            missval = -1.175494e+38,
            longname = sprintf("%s deposition (WHEP/HaNi)", var_name),
            prec = "float",
            compression = 4
        )

        nc <- nc_create(out_path, vars = list(v), force_v4 = TRUE)
        on.exit(nc_close(nc), add = TRUE)

        for (ti in seq_len(ntime)) {
            yr <- years_d[ti]
            sub <- dep_cells[year == yr]
            m <- new_slice(grid$nlon, grid$nlat, fill = 0)
            if (nrow(sub) > 0) {
                m[cbind(sub$col, sub$row)] <- sub[[value_col]]
            }
            ncvar_put(nc, v, m,
                start = c(1, 1, ti),
                count = c(grid$nlon, grid$nlat, 1)
            )
        }

        ncatt_put(nc, 0, "Conventions", "CF-1.8")
        ncatt_put(nc, 0, "created_by", "WHEP export_lpjml_inputs_netcdf.R")
        ncatt_put(nc, 0, "created_date", as.character(Sys.time()))
    }

    out_nhx <- file.path(
        out_root, "nitrogen",
        sprintf("ndep_nhx_whep_annual_%d_%d.nc4", min(years_d), max(years_d))
    )
    out_noy <- file.path(
        out_root, "nitrogen",
        sprintf("ndep_noy_whep_annual_%d_%d.nc4", min(years_d), max(years_d))
    )

    .msg("Writing NHx deposition NetCDF (%d years): %s", ntime, out_nhx)
    write_dep_annual(out_nhx, "nhx", "value_nhx")
    .msg("Writing NOy deposition NetCDF (%d years): %s", ntime, out_noy)
    write_dep_annual(out_noy, "noy", "value_noy")

    status_txt <- if (has_split) "generated" else "generated_with_assumption"
    add_status("nh4deposition", dep_file, out_nhx, status_txt, split_note)
    add_status("no3deposition", dep_file, out_noy, status_txt, split_note)
} else {
    add_status("nh4deposition", dep_file, "", "missing_source")
    add_status("no3deposition", dep_file, "", "missing_source")
}

# ------------------------------------------------------------------
# 8) Climate inputs (symlinked from existing LPJmL inputs)
# ------------------------------------------------------------------
climate_mapping <- list(
    temp    = "cru_ts_3_10.1901.2009.tmp.dat.nc",
    prec    = "cru_ts_3_10_01.1901.2009.pre.dat.nc",
    cloud   = "cru_ts_3_10.1901.2009.cld.dat.nc",
    wind    = "wind_gswp3-w5e5_1901_2016_monthly.nc",
    co2     = "historical_CO2_annual_1765_2018.txt",
    wetdays = "cru_ts3.20.1901.2011.wet.dat.nc"
)

if (dir.exists(external_climate_dir)) {
    for (nm in names(climate_mapping)) {
        src_path <- file.path(external_climate_dir, climate_mapping[[nm]])
        dst_path <- file.path(out_root, "climate", climate_mapping[[nm]])

        if (file.exists(src_path)) {
            if (!file.exists(dst_path)) {
                # Create symlink
                file.symlink(src_path, dst_path)
            }
            add_status(
                input_name  = nm,
                source      = src_path,
                output_file = dst_path,
                status_txt  = "symlinked",
                note        = "Symlinked from external LPJmL repository."
            )
        } else {
            add_status(
                input_name  = nm,
                source      = "external climate forcing",
                output_file = "",
                status_txt  = "missing_source",
                note        = sprintf("Expected file %s not found in %s", climate_mapping[[nm]], external_climate_dir)
            )
        }
    }
} else {
    for (nm in names(climate_mapping)) {
        add_status(
            input_name  = nm,
            source      = "external climate forcing",
            output_file = "",
            status_txt  = "not_generated_here",
            note        = "Use LPJmL climate inputs (CRU/ISIMIP) from dedicated data pipeline."
        )
    }
}

# ------------------------------------------------------------------
# Write comparison report and machine-readable status
# ------------------------------------------------------------------
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
writeLines(
    "- `generated_with_assumption`: exported using documented approximation",
    con
)
writeLines("- `missing_source`: required source parquet not found", con)
writeLines(
    "- `not_generated_here`: not part of current WHEP generation pipeline",
    con
)
writeLines("", con)
writeLines("## Inputs", con)
writeLines("", con)

for (i in seq_len(nrow(status))) {
    r <- status[i]
    line <- sprintf(
        "- **%s**: %s%s",
        r$input_name,
        r$status,
        if (nzchar(r$output_file)) sprintf(" -> `%s`", r$output_file) else ""
    )
    writeLines(line, con)
    if (nzchar(r$note)) {
        writeLines(sprintf("  - note: %s", r$note), con)
    }
    if (nzchar(r$source)) {
        writeLines(sprintf("  - source: %s", r$source), con)
    }
}
close(con)

.msg("Done.")
.msg("Wrote: %s", status_csv)
.msg("Wrote: %s", report_md)
