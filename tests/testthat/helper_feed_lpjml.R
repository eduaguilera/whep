.test_lpjml_band_file <- function(
  layout = c("lon", "lat", "pft", "time"),
  n_lat = 2L
) {
  path <- withr::local_tempfile(fileext = ".nc", .local_envir = parent.frame())
  dims <- list(
    lon = ncdf4::ncdim_def("lon", "degrees_east", c(0.25, 0.75)),
    lat = ncdf4::ncdim_def(
      "lat",
      "degrees_north",
      0.25 + (seq_len(n_lat) - 1) * 0.5
    ),
    pft = ncdf4::ncdim_def("pft", "", 1:2),
    time = ncdf4::ncdim_def("time", "days since 1750-1-1 0:0:0", 0),
    x = ncdf4::ncdim_def("x", "degrees_east", c(0.25, 0.75))
  )
  chars <- ncdf4::ncdim_def("len", "", 1:19)
  value <- ncdf4::ncvar_def("depth", "mm/yr", dims[layout], missval = -9999)
  names <- ncdf4::ncvar_def("NamePFT", "", list(chars, dims$pft), prec = "char")
  nc <- ncdf4::nc_create(path, list(value, names))
  on.exit(ncdf4::nc_close(nc))
  sizes <- vapply(dims[layout], `[[`, integer(1), "len")
  ncdf4::ncvar_put(nc, value, array(seq_len(prod(sizes)), dim = sizes))
  ncdf4::ncvar_put(
    nc,
    names,
    c("rainfed grassland", "irrigated grassland")
  )
  path
}
