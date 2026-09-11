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
# The two releases are not interchangeable before 1979, so this script must
# keep judging 1901-2016 against ISIMIP2a specifically. What separates them,
# re-measured for #929: only the bias-adjusted era differs at all (1981-1990
# agrees at max 1.55e-04 m/s), and there the difference is a reshaped seasonal
# cycle inside each cell, not a spatial one -- a single cell-month moves by up
# to 2.55 m/s, yet every cell keeps its own decadal mean to within 0.081 m/s
# and the global mean to 2e-05 m/s. So this audit's exact-equality test is the
# right instrument: nothing coarser than a cell-month would see the swap.
# The full distribution, the downstream consequence and the open decision are
# in fetch_isimip_wind.sh and issue #929.
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
  dir <- .existing_dir("WHEP_ISIMIP_WIND_DIR")
  dir_3a <- .existing_dir("WHEP_ISIMIP3A_WIND_DIR")
  if (is.null(dir) && is.null(dir_3a)) {
    cli::cli_abort(c(
      "Set {.envvar WHEP_ISIMIP_WIND_DIR} to a directory of ISIMIP2a wind
       chunks, or {.envvar WHEP_ISIMIP3A_WIND_DIR} to ISIMIP3a ones.",
      i = "ISIMIP2a: {.file wind_gswp3-w5e5_<start>_<end>.nc4}, as published
         at {.url https://files.isimip.org/ISIMIP2a/InputData/climate_co2/climate/HistObs/GSWP3-W5E5/}.",
      i = "ISIMIP3a: {.file gswp3-w5e5_obsclim_sfcwind_global_daily_<start>_<end>.nc},
         or files already reduced to {.file sfcwind_monthly_<start>_<end>.nc}.",
      i = "{.path inst/scripts/fetch_isimip_wind.sh} downloads the ISIMIP2a
         set; its {.var BASE_3A} is where the ISIMIP3a ones live."
    ))
  }
  require_cdo()

  work <- file.path(tempdir(), "wind_provenance")
  dir.create(work, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  # type = "nc" hands back the path, not the contents: cdo does the reading.
  pin_path <- whep::whep_read_file(WIND_PIN, type = "nc")
  pin_local <- link_into(pin_path, file.path(work, "pin.nc"))

  if (!is.null(dir)) {
    audit_against_isimip2a(dir, pin_local, work)
  }
  if (!is.null(dir_3a)) {
    measure_release_difference(dir_3a, pin_local, work)
  }
  invisible(NULL)
}

.existing_dir <- function(var) {
  value <- Sys.getenv(var, "")
  if (nzchar(value) && dir.exists(value)) value else NULL
}

audit_against_isimip2a <- function(dir, pin_local, work) {
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

# ---- The other release: what consolidating on ISIMIP3a would change --------
#
# Second question, same pin. `audit_against_isimip2a()` asks "is the pin the
# dataset its name claims". This asks "what would the defensible alternative
# base change", because ISIMIP3a publishes `sfcwind` for the full 1901-2019 and
# taking the whole base from it -- consistent with the pure-ISIMIP3a rsds/rlds
# pins -- is a standing open question (#929).
#
# It is measured at TWO grains on purpose, and the pair is the answer:
#
#   cell-month   how far a single monthly forcing value moves
#   per-cell annual mean   how far the cell's own climatology moves
#
# They differ by a factor of 10 to 15, which is what makes this a question
# about the seasonal cycle rather than about spatial redistribution. See the
# block in fetch_isimip_wind.sh for the distribution behind these two numbers.
measure_release_difference <- function(dir, pin_local, work) {
  chunks <- discover_3a_chunks(dir)
  if (nrow(chunks) == 0L) {
    cli::cli_abort(
      "No ISIMIP3a {.file *sfcwind*_<start>_<end>.nc} chunks in {.path {dir}}."
    )
  }
  rows <- lapply(seq_len(nrow(chunks)), function(i) {
    release_diff(chunks[i, ], pin_local, work)
  })
  report_release_difference(dplyr::bind_rows(rows), dir)
}

# Accepts either the published daily chunk or one already reduced to monthly
# means, because the reduction is the expensive half (~2.8 GB in, minutes out)
# and is worth keeping between runs.
discover_3a_chunks <- function(dir) {
  patterns <- c(
    daily = "^gswp3-w5e5_obsclim_sfcwind_global_daily_\\d{4}_\\d{4}\\.nc$",
    monthly = "^sfcwind_monthly_\\d{4}_\\d{4}\\.nc$"
  )
  found <- lapply(names(patterns), function(kind) {
    paths <- list.files(dir, pattern = patterns[[kind]], full.names = TRUE)
    years <- stringr::str_match(basename(paths), "(\\d{4})_(\\d{4})")
    tibble::tibble(
      path = paths,
      kind = kind,
      first_year = as.integer(years[, 2L]),
      last_year = as.integer(years[, 3L])
    )
  })

  # A pre-reduced file wins over the daily one covering the same years --
  # spelled out rather than left to the alphabet, which happens to rank them
  # the wrong way round.
  dplyr::bind_rows(found) |>
    dplyr::mutate(
      preference = dplyr::if_else(.data$kind == "monthly", 1L, 2L)
    ) |>
    dplyr::slice_min(.data$preference, n = 1L, by = "first_year") |>
    dplyr::arrange(.data$first_year) |>
    dplyr::select(-"preference")
}

release_diff <- function(chunk, pin_local, work) {
  label <- sprintf("%d-%d", chunk$first_year, chunk$last_year)
  if (chunk$last_year > ISIMIP2A$last_year) {
    return(tibble::tibble(
      chunk = label,
      steps = NA_integer_,
      mean_abs_diff = NA_real_,
      max_abs_diff = NA_real_,
      annual_max_abs = NA_real_,
      mean_bias = NA_real_,
      note = "outside the ISIMIP2a segment; the pin is already ISIMIP3a here"
    ))
  }

  monthly <- monthly_3a(chunk, work, label)
  slice <- file.path(work, sprintf("pin3a_%s.nc", label))
  delta <- file.path(work, sprintf("rel_%s.nc", label))
  run_cdo(c(
    "-s",
    sprintf("selyear,%d/%d", chunk$first_year, chunk$last_year),
    pin_local,
    slice
  ))
  # `-sub` takes its output grid from the FIRST operand, which is why the
  # ISIMIP3a file goes first: the pin was written by terra as gridtype
  # "projection", and cdo's fldmean cannot area-weight that, so a statistic
  # taken on the pin's own grid would silently be unweighted. Both are the
  # same 720x360 -180/90 half-degree geotransform, so index alignment is
  # correct; only the weighting differs.
  run_cdo(c("-s", "-sub", monthly, slice, delta))

  tibble::tibble(
    chunk = label,
    steps = nc_steps(monthly),
    mean_abs_diff = cdo_scalar(c(
      "-s",
      "outputf,%.17g,1",
      "-timmean",
      "-fldmean",
      "-abs",
      delta
    )),
    max_abs_diff = cdo_scalar(c(
      "-s",
      "outputf,%.17g,1",
      "-fldmax",
      "-timmax",
      "-abs",
      delta
    )),
    annual_max_abs = cdo_scalar(c(
      "-s",
      "outputf,%.17g,1",
      "-fldmax",
      "-abs",
      "-timmean",
      delta
    )),
    mean_bias = cdo_scalar(c(
      "-s",
      "outputf,%.17g,1",
      "-timmean",
      "-fldmean",
      delta
    )),
    note = NA_character_
  )
}

# `chname` because the two releases spell the variable differently (`sfcwind`
# against `wind`) and cdo matches by name when both files carry one variable.
monthly_3a <- function(chunk, work, label) {
  if (identical(chunk$kind, "monthly")) {
    return(link_into(chunk$path, file.path(work, sprintf("m3a_%s.nc", label))))
  }
  src <- link_into(chunk$path, file.path(work, sprintf("d3a_%s.nc", label)))
  out <- file.path(work, sprintf("m3a_%s.nc", label))
  cli::cli_alert("{label}: reducing {basename(chunk$path)} to monthly means")
  run_cdo(c("-s", "-chname,sfcwind,wind", "-monmean", src, out))
  out
}

report_release_difference <- function(table, dir) {
  cli::cli_h1("Pinned wind base (ISIMIP2a) vs the ISIMIP3a alternative")
  cli::cli_alert_info("ISIMIP3a chunks read from {.path {dir}}")
  cli::cli_alert_info(
    "All figures m/s, area-weighted where a mean is taken."
  )
  print(as.data.frame(table), row.names = FALSE)
  cat("\n")

  measured <- dplyr::filter(table, !is.na(.data$max_abs_diff))
  if (nrow(measured) == 0L) {
    cli::cli_alert_warning("No chunk inside the ISIMIP2a segment was measured.")
    return(invisible(NULL))
  }
  worst <- measured[which.max(measured$max_abs_diff), ]
  ratio <- worst$max_abs_diff / worst$annual_max_abs
  cli::cli_alert_info(
    "{worst$chunk}: the worst single cell-month moves
     {round(worst$max_abs_diff, 3)}, the worst cell ANNUAL mean
     {round(worst$annual_max_abs, 3)} -- a factor of {round(ratio, 1)}."
  )
  cli::cli_alert_info(
    "A ratio near 1 would mean a spatial redistribution; a large one means the
     releases differ in the seasonal cycle within each cell. See
     {.path inst/scripts/fetch_isimip_wind.sh} and issue #929."
  )
  invisible(NULL)
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
