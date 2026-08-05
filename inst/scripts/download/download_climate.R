# -----------------------------------------------------------------------
# download_climate.R
#
# Downloads the raw climate forcing the LPJmL run needs, into the L_files
# layout that prepare_spatialize_all.R reads. Nothing here is LPJmL-ready:
# Section 9d of prepare_spatialize_all.R derives the model inputs from
# these files, the same way every other input is derived from its source.
#
# Every file is skipped if already present, so this is cheap to re-run --
# CRU alone is ~2.3 GB compressed and expands past 18 GB.
#
# References:
#   Harris et al. (2020) doi:10.1038/s41597-020-0453-3   (CRU TS)
#   Lange (2019) doi:10.5880/pik.2019.023                (GSWP3-W5E5)
#   NOAA GML global annual mean CO2                      (co2_annmean_gl)
# -----------------------------------------------------------------------

# CRU TS release to fetch. `build` is the release's own build tag, which is
# part of the download path and changes with every version, so it cannot be
# derived from the version number.
CRU_TS_RELEASE <- list(
  version = "4.09",
  build = "cruts.2503051245.v4.09",
  first_year = 1901L,
  last_year = 2024L
)

# The four variables LPJmL reads. CRU ships each as a separate file that also
# carries stn/mae/maea diagnostics LPJmL never uses; the prepare step strips
# them, which is where most of the size goes.
CRU_TS_VARIABLES <- c("tmp", "pre", "cld", "wet")

# The two monthly wind artefacts, pinned because neither can be rebuilt from
# this repo: the 1901-2019 base was assembled from ISIMIP2a chunks by a script
# that was never committed, and reproducing the ERA5 means needs ~85 GB of
# streaming. Section 9d assembles the 1901-2023 forcing from these.
WIND_PINS <- c(
  base = "lpjml-wind-isimip-1901-2019",
  era5 = "lpjml-wind-era5-2017-2023"
)

# Downwelling shortwave and longwave, needed only by LPJmL 6.x: 6.x removed the
# `cloudiness` option and the `cloud` input, so CRU cld cannot drive it. Pinned
# for the same reasons as wind -- the ISIMIP obsclim bases end in 2019 (W5E5
# ends there; the v1.1-v1.3 releases are corrections, not extensions), and
# rebuilding the ERA5 means needs tens of GB of streaming. Section 9d assembles
# the 1901-2023 forcing from these.
RADIATION_PINS <- list(
  rsds = c(
    base = "lpjml-rsds-isimip-1901-2019",
    era5 = "lpjml-rsds-era5-2017-2023"
  ),
  rlds = c(
    base = "lpjml-rlds-isimip-1901-2019",
    era5 = "lpjml-rlds-era5-2017-2023"
  )
)

download_climate <- function(dest_dir, timeout = 7200) {
  # download_all() raises the timeout for the whole run, but this is also
  # called on its own, and the default 60 s cannot fetch a ~450 MB CRU file.
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  .download_cru_ts(dest_dir)
  .download_co2(dest_dir)
  .download_wind_pins(dest_dir)
  .download_radiation_pins(dest_dir)
  invisible()
}

# Fetches the pinned wind artefacts into the L_files tree, so the prepare step
# finds them like any other downloaded input rather than asking the user to
# call whep_read_file() by hand.
.download_wind_pins <- function(dest_dir) {
  .download_climate_pins(
    dest_dir,
    subdir = "wind",
    label = "Wind",
    pins = WIND_PINS,
    patterns = c(base = "^wind_gswp3.*\\.nc$", era5 = "^era5_wind.*\\.nc$")
  )
}

# Fetches the pinned radiation artefacts, one variable at a time. Absent pins
# are a warning, not an error: 5.x does not read these at all, so a 5.x-only
# setup must still complete.
.download_radiation_pins <- function(dest_dir) {
  for (variable in names(RADIATION_PINS)) {
    .download_climate_pins(
      dest_dir,
      subdir = "radiation",
      label = variable,
      pins = RADIATION_PINS[[variable]],
      patterns = c(
        base = sprintf("^%s_gswp3.*\\.nc$", variable),
        era5 = sprintf("^era5_%s.*\\.nc$", variable)
      )
    )
  }
  invisible()
}

# Shared pin fetcher for the climate artefacts that cannot be rebuilt from this
# repo. Copies out of the pins cache into the L_files tree, so the prepare step
# finds them like any other downloaded input rather than asking the user to call
# whep_read_file() by hand.
.download_climate_pins <- function(dest_dir, subdir, label, pins, patterns) {
  out_dir <- file.path(dest_dir, subdir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  for (nm in names(pins)) {
    alias <- pins[[nm]]
    existing <- list.files(out_dir, pattern = patterns[[nm]])
    if (length(existing) > 0L) {
      cli::cli_alert_info("{label} {nm}: already exists ({existing[[1L]]})")
      next
    }
    cli::cli_alert("Fetching pin {alias}...")
    src <- tryCatch(
      whep::whep_read_file(alias, type = "nc"),
      error = function(e) {
        cli::cli_warn("{label} {nm}: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(src) || !file.exists(src[[1L]])) {
      next
    }
    # The pins cache is not a stable location to point a pipeline at.
    if (!file.copy(src[[1L]], file.path(out_dir, basename(src[[1L]])))) {
      cli::cli_warn("{label} {nm}: could not copy into {.path {out_dir}}")
      next
    }
    cli::cli_alert_success("{label} {nm}: {basename(src[[1L]])}")
  }

  invisible()
}

# download.file() signals an error rather than returning non-zero on most
# failures, so every call here goes through this: one slow mirror should
# leave the rest of the pipeline runnable instead of aborting the run.
.try_download <- function(url, out_path, label) {
  status <- tryCatch(
    utils::download.file(url, out_path, mode = "wb", quiet = TRUE),
    error = function(e) {
      cli::cli_warn("{label}: {conditionMessage(e)}")
      1L
    },
    warning = function(w) {
      cli::cli_warn("{label}: {conditionMessage(w)}")
      1L
    }
  )
  ok <- identical(as.integer(status), 0L) && file.exists(out_path)
  if (!ok) {
    # Never leave a truncated file behind: it would be indistinguishable
    # from a complete one on the next run's skip check.
    unlink(out_path)
  }

  ok
}

# ---- CRU TS ------------------------------------------------------------

.download_cru_ts <- function(dest_dir) {
  cru_dir <- file.path(dest_dir, "CRU")
  dir.create(cru_dir, recursive = TRUE, showWarnings = FALSE)

  release <- CRU_TS_RELEASE
  tag <- sprintf(
    "cru_ts%s.%d.%d",
    release$version,
    release$first_year,
    release$last_year
  )
  base_url <- paste0(
    "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_",
    release$version,
    "/",
    release$build
  )

  for (variable in CRU_TS_VARIABLES) {
    fname <- paste0(tag, ".", variable, ".dat.nc")
    out_path <- file.path(cru_dir, fname)
    if (file.exists(out_path)) {
      cli::cli_alert_info("CRU {variable}: already exists")
      next
    }
    .download_and_gunzip(
      url = paste0(base_url, "/", variable, "/", fname, ".gz"),
      out_path = out_path,
      label = paste("CRU", variable)
    )
  }

  invisible()
}

# CRU serves gzipped NetCDF. Unpacking here rather than in the prepare step
# keeps the prepare step reading plain files like every other section.
.download_and_gunzip <- function(url, out_path, label) {
  gz_path <- paste0(out_path, ".gz")
  on.exit(unlink(gz_path), add = TRUE)
  cli::cli_alert("Downloading {label} (this one is large)...")
  if (!.try_download(url, gz_path, label)) {
    return(invisible(FALSE))
  }

  cli::cli_alert("Unpacking {label}...")
  if (!.gunzip(gz_path, out_path)) {
    cli::cli_warn("{label}: could not unpack {.path {gz_path}}")
    unlink(out_path)
    return(invisible(FALSE))
  }
  cli::cli_alert_success("{label}: saved")

  invisible(TRUE)
}

.gunzip <- function(gz_path, out_path) {
  con_in <- gzfile(gz_path, "rb")
  on.exit(close(con_in), add = TRUE)
  con_out <- file(out_path, "wb")
  on.exit(close(con_out), add = TRUE)

  repeat {
    chunk <- readBin(con_in, "raw", n = 16L * 1024L^2)
    if (length(chunk) == 0L) {
      break
    }
    writeBin(chunk, con_out)
  }

  file.exists(out_path) && file.size(out_path) > 0L
}

# ---- CO2 ---------------------------------------------------------------

# NOAA's global annual means, used to extend LPJmL's historical CO2 series
# past its 2018 end. A few kB, so it is always refetched.
.download_co2 <- function(dest_dir) {
  co2_dir <- file.path(dest_dir, "CO2")
  dir.create(co2_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(co2_dir, "co2_annmean_gl.txt")

  url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_gl.txt"
  if (!.try_download(url, out_path, "CO2")) {
    return(invisible(FALSE))
  }
  cli::cli_alert_success("CO2: NOAA global annual means saved")

  invisible(TRUE)
}

# ---- Wind --------------------------------------------------------------

# The raw ISIMIP3a daily sfcwind chunks are no longer downloaded here: the
# monthly means derived from them are pinned instead (WIND_PINS), which
# avoids a ~2.5 GB download plus an aggregation step per chunk. Fetching
# those pins is .download_wind_pins() above.
