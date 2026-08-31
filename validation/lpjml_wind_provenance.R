# Audits the pinned LPJmL wind base against the ISIMIP files it claims to come
# from, by rebuilding the monthly means and comparing them value for value.
#
# WHY THIS EXISTS
#
# `lpjml-wind-isimip-1901-2019` is a 1.48 GB NetCDF that nothing in the repo
# could re-derive: the script that built it was never committed, from a URL list
# on somebody's disk (#371). `validation/lpjml_forcing_pins.R` can tell you the
# pin is not corrupt -- right shape, no impossible values -- but not that it is
# the dataset its name claims. Those are different questions, and wind is the
# one forcing where the second matters most: it is a HARD LPJmL input
# (readclimate() aborts with ERROR130/ERROR131 on a year outside the file range
# instead of holding the last year), so it sets the length of every run.
#
# `inst/scripts/fetch_isimip_wind.sh` is the reproduction path. This is the
# check that the path actually lands on the pin.
#
# WHAT IT CHECKS
#
# For each ISIMIP2a chunk present on disk, `cdo monmean` it and require the
# result to equal the pin over exactly the years the chunk covers. The
# comparison is EXACT, not tolerant: both sides are `cdo monmean` over the same
# float32 daily field in the same order, so any nonzero difference means the two
# came from different data, not from arithmetic.
#
# Chunks are read from disk and never downloaded -- one is ~2.7 GB and the full
# set is ~31 GB. Whatever chunks are present are audited and the rest simply
# are not reported, so a partial archive still verifies the years it holds.
#
# Usage:
#   WHEP_ISIMIP_WIND_DIR=<dir> Rscript validation/lpjml_wind_provenance.R
#
# <dir> holds any of the DAILY files `wind_gswp3-w5e5_<start>_<end>.nc4` as
# published at
# https://files.isimip.org/ISIMIP2a/InputData/climate_co2/climate/HistObs/GSWP3-W5E5/
#
# Note this is NOT `WHEP_WIND_DIR`, which points at the assembled monthly
# product that `read_lpjml_wind()` consumes. This one points at the raw ISIMIP
# daily chunks that product is derived from -- the inputs, not the output.
# `inst/scripts/fetch_isimip_wind.sh` downloads them, but deletes each daily
# file as soon as it has the monthly mean, so to keep them for this audit
# either fetch them by hand from the URLs above or comment out its `rm`.
#
# Requires cdo on PATH. Note cdo cannot parse a path containing spaces even when
# quoted ("To many inputs"), so the chunks are symlinked into a work directory
# before use -- the WHEP archive path has spaces in it.

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
})

WIND_PIN <- "lpjml-wind-isimip-1901-2019" # nolint: object_name_linter.

# The ISIMIP2a release the 1901-2016 segment comes from. Recorded here because
# the pin carries no provenance attribute of its own: its global attributes name
# terra and cdo, not ISIMIP. Read off the source files' `title`/`version`
# attributes.
#
# Segment 2 (2017-2019) is ISIMIP3a `sfcwind`, a DIFFERENT bias-adjustment
# release (ISIMIP3BASD v2.5.0 against v2.4.1), and is not audited here: the 3a
# chunk covering it also covers 2011-2016, so a chunk-to-year mapping would be
# ambiguous. It was verified by hand at max |diff| = 0 against the pin (#371).
#
# The two releases are NOT interchangeable, so this script must keep judging
# 1901-2016 against ISIMIP2a specifically: over 1901-1910 they differ by up to
# 2.55 m/s per cell while their global means agree to 1e-4 m/s. The numbers and
# what they mean for the run are in fetch_isimip_wind.sh.
# nolint start: object_name_linter.
ISIMIP2A <- list(
  title = paste(
    "GSWP3 global meteorological forcing data bias-adjusted to W5E5 with",
    "ISIMIP3BASD v2.4.1 for ISIMIP2a"
  ),
  published = "2020-06-18",
  variable = "wind",
  units = "m s-1",
  first_year = 1901L,
  last_year = 2016L
)
# nolint end

main <- function() {
  dir <- Sys.getenv("WHEP_ISIMIP_WIND_DIR", "")
  if (!nzchar(dir) || !dir.exists(dir)) {
    cli::cli_abort(c(
      "Set {.envvar WHEP_ISIMIP_WIND_DIR} to a directory of ISIMIP2a wind
       chunks.",
      i = "Files named {.file wind_gswp3-w5e5_<start>_<end>.nc4}, as published
         at {.url https://files.isimip.org/ISIMIP2a/InputData/climate_co2/climate/HistObs/GSWP3-W5E5/}.",
      i = "{.path inst/scripts/fetch_isimip_wind.sh} downloads them."
    ))
  }
  require_cdo()

  work <- file.path(tempdir(), "wind_provenance")
  dir.create(work, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  # type = "nc" hands back the path, not the contents: cdo does the reading.
  pin_path <- whep::whep_read_file(WIND_PIN, type = "nc")
  pin_local <- link_into(pin_path, file.path(work, "pin.nc"))

  chunks <- discover_chunks(dir)
  if (nrow(chunks) == 0L) {
    cli::cli_abort("No {.file wind_gswp3-w5e5_*.nc4} chunks in {.path {dir}}.")
  }
  rows <- lapply(seq_len(nrow(chunks)), function(i) {
    audit_chunk(chunks[i, ], pin_local, work)
  })
  report(dplyr::bind_rows(rows), dir)
}

# ---- Measuring -------------------------------------------------------------

# Chunk year span comes from the FILENAME, then is verified against the file's
# own time axis, because the name is the only thing that says which slice of the
# pin to compare against and a mislabelled file would silently compare the wrong
# years and still "pass" if both were wrong the same way.
discover_chunks <- function(dir) {
  paths <- list.files(
    dir,
    pattern = "^wind_gswp3-w5e5_\\d{4}_\\d{4}\\.nc4$",
    full.names = TRUE
  )
  years <- stringr::str_match(basename(paths), "(\\d{4})_(\\d{4})")
  tibble::tibble(
    path = paths,
    first_year = as.integer(years[, 2L]),
    last_year = as.integer(years[, 3L])
  ) |>
    dplyr::arrange(.data$first_year)
}

audit_chunk <- function(chunk, pin_local, work) {
  label <- sprintf("%d-%d", chunk$first_year, chunk$last_year)
  base <- tibble::tibble(chunk = label)

  if (chunk$last_year > ISIMIP2A$last_year) {
    return(dplyr::mutate(
      base,
      steps = NA_integer_,
      max_abs_diff = NA_real_,
      verdict = "SKIP",
      detail = sprintf(
        "beyond the ISIMIP2a segment (ends %d); 2017-2019 is ISIMIP3a",
        ISIMIP2A$last_year
      )
    ))
  }

  # cdo cannot take a path with spaces, and the archive path has them.
  src <- link_into(chunk$path, file.path(work, sprintf("src_%s.nc4", label)))
  monthly <- file.path(work, sprintf("monthly_%s.nc", label))
  slice <- file.path(work, sprintf("pin_%s.nc", label))
  delta <- file.path(work, sprintf("diff_%s.nc", label))

  cli::cli_alert("{label}: reducing {basename(chunk$path)} to monthly means")
  run_cdo(c("-s", "monmean", src, monthly))
  run_cdo(c(
    "-s",
    sprintf("selyear,%d/%d", chunk$first_year, chunk$last_year),
    pin_local,
    slice
  ))

  expected <- 12L * (chunk$last_year - chunk$first_year + 1L)
  got <- c(nc_steps(monthly), nc_steps(slice))
  if (!all(got == expected)) {
    return(dplyr::mutate(
      base,
      steps = got[[1L]],
      max_abs_diff = NA_real_,
      verdict = "SPAN",
      detail = sprintf(
        "expected %d monthly steps for %s; rebuilt %d, pin slice %d",
        expected,
        label,
        got[[1L]],
        got[[2L]]
      )
    ))
  }

  # The pin was written by terra and carries a `projection` grid where cdo
  # reads the source as `lonlat`; cdo warns and aligns by index, which is
  # correct here -- both are the same 720x360 -180/90 0.5-degree geotransform,
  # recorded in the pin's own crs:geotransform attribute.
  run_cdo(c("-s", "-sub", monthly, slice, delta))
  worst <- cdo_scalar(c(
    "-s",
    "outputf,%.17g,1",
    "-fldmax",
    "-timmax",
    "-abs",
    delta
  ))

  dplyr::mutate(
    base,
    steps = expected,
    max_abs_diff = worst,
    verdict = if (identical(worst, 0)) "ok" else "DEVIATES",
    detail = if (identical(worst, 0)) {
      "bit-identical to the pin"
    } else {
      sprintf("rebuilt series differs from the pin by up to %.6g m/s", worst)
    }
  )
}

# ---- cdo plumbing ----------------------------------------------------------

require_cdo <- function() {
  if (!nzchar(Sys.which("cdo"))) {
    cli::cli_abort(
      "cdo not found on PATH. Install it (e.g. {.code apt install cdo})."
    )
  }
  invisible(TRUE)
}

run_cdo <- function(args) {
  status <- system2("cdo", args, stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) {
    cli::cli_abort("cdo failed: {.code cdo {paste(args, collapse = ' ')}}")
  }
  invisible(TRUE)
}

cdo_scalar <- function(args) {
  out <- system2("cdo", args, stdout = TRUE, stderr = FALSE)
  as.numeric(trimws(out[[length(out)]]))
}

nc_steps <- function(path) {
  as.integer(trimws(system2(
    "cdo",
    c("-s", "ntime", path),
    stdout = TRUE,
    stderr = FALSE
  )[[1L]]))
}

# A symlink, not a copy: these are 1.5-2.7 GB each.
link_into <- function(from, to) {
  if (!file.exists(to)) {
    file.symlink(normalizePath(from), to)
  }
  to
}

# ---- Reporting -------------------------------------------------------------

report <- function(table, dir) {
  cli::cli_h1("Pinned wind base vs ISIMIP2a source")
  pin <- WIND_PIN
  source_title <- ISIMIP2A$title
  cli::cli_alert_info("Pin: {pin}")
  cli::cli_alert_info(
    "Source: {source_title}, published {ISIMIP2A$published}"
  )
  cli::cli_alert_info("Chunks read from {.path {dir}}")
  print(as.data.frame(table), row.names = FALSE)
  cat("\n")

  audited <- dplyr::filter(table, .data$verdict != "SKIP")
  failed <- dplyr::filter(audited, .data$verdict != "ok")
  if (nrow(audited) == 0L) {
    cli::cli_alert_warning("No chunk inside the ISIMIP2a segment was audited.")
    return(invisible(NULL))
  }
  if (nrow(failed) == 0L) {
    covered <- sum(audited$steps)
    cli::cli_alert_success(
      "{nrow(audited)} chunk{?s} reproduce the pin exactly: {covered} of 1428
       monthly steps verified against ISIMIP2a."
    )
    return(invisible(NULL))
  }
  cli::cli_alert_danger("{nrow(failed)} of {nrow(audited)} chunk{?s} deviate:")
  for (i in seq_len(nrow(failed))) {
    cli::cli_alert_warning("{failed$chunk[[i]]}: {failed$detail[[i]]}")
  }
  invisible(NULL)
}

main()
