# Driver for the gridded nitrogen balance (#446).
#
# build_nitrogen_balance() takes a `data` list of a dozen upstream inputs, and
# nothing in the repository assembled it. That is why the chain from the balance
# through the surplus, the boundary classification and the footprint had never
# run on real data, and why every test on that path is fixture-driven.
#
# This script is that assembly. It is deliberately STAGED and self-reporting
# rather than one call: each input is built independently, timed and recorded,
# so a machine missing one local surface gets a named blocker instead of an
# error thrown from somewhere inside the balance. Read the coverage table, then
# the blockers.
#
# Usage:
#   Rscript --vanilla inst/scripts/run_nitrogen_balance.R [year] [resolution]
#
#   WHEP_NBD_SKIP_HEAVY=1   skip the two multi-minute stages (the SOC carbon
#                           balance and the feed redistribution) to get a fast
#                           coverage report of everything else.
#
# Requires the local surfaces (CLAUDE.md, "New data sources"):
#   WHEP_TYPE_CROPLAND_PATH   WHEP_CROP_PATTERNS_PATH  WHEP_GRIDDED_PASTURE_PATH
#   WHEP_POLITY_FRACTION_PATH WHEP_HANI_DIR            WHEP_HYDE_DIR
# plus cached pins for production, fertiliser and the commodity balances.

suppressMessages(pkgload::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
year <- as.integer(if (length(args) >= 1L) args[[1L]] else "2010")
resolution <- if (length(args) >= 2L) args[[2L]] else "grid"
skip_heavy <- nzchar(Sys.getenv("WHEP_NBD_SKIP_HEAVY"))

# ---- staging ----------------------------------------------------------------

.nbd_log <- new.env(parent = emptyenv())
.nbd_log$rows <- list()

# Build one input, recording the outcome instead of aborting: a missing input is
# a fact to report, not a reason to lose the other twelve.
nbd_stage <- function(label, expr, heavy = FALSE) {
  if (heavy && skip_heavy) {
    .nbd_record(label, "skip", 0, NA_integer_, "WHEP_NBD_SKIP_HEAVY set")
    cli::cli_inform("skip {label}")
    return(NULL)
  }
  started <- proc.time()
  value <- tryCatch(
    suppressMessages(suppressWarnings(force(expr))),
    error = function(e) e
  )
  elapsed <- round((proc.time() - started)[["elapsed"]], 1)
  if (inherits(value, "error")) {
    .nbd_record(label, "FAIL", elapsed, NA_integer_, conditionMessage(value))
    cli::cli_inform("{cli::col_red('FAIL')} {label} ({elapsed}s)")
    return(NULL)
  }
  .nbd_record(label, "ok", elapsed, .nbd_size(value), NA_character_)
  cli::cli_inform("{cli::col_green('ok')}   {label} ({elapsed}s)")
  value
}

.nbd_record <- function(label, status, seconds, rows, detail) {
  .nbd_log$rows[[length(.nbd_log$rows) + 1L]] <- tibble::tibble(
    input = label,
    status = status,
    seconds = seconds,
    rows = rows,
    detail = if (is.na(detail)) {
      NA_character_
    } else {
      substr(gsub("\\s+", " ", detail), 1, 200)
    }
  )
}

.nbd_size <- function(x) {
  if (is.data.frame(x)) {
    nrow(x)
  } else if (is.list(x)) {
    length(x)
  } else {
    NA_integer_
  }
}

# The crops table the whole NPP -> BNF -> residue chain descends from.
# .sci_crop_prod_wide() (R/soil_carbon_inputs.R) already reshapes primary
# production into exactly the columns calculate_crop_npp() wants, plus the
# Krausmann/HANPP regions, so it is reused rather than reimplemented.
.nbd_crops_table <- function(primary_prod, year) {
  whep:::.sci_crop_prod_wide(primary_prod) |>
    dplyr::filter(.data$year == !!year) |>
    dplyr::mutate(sub_territory = as.character(.data$area_code))
}

# ---- 1. spatial and land surfaces -------------------------------------------

cli::cli_h1("Nitrogen balance driver: {year}, resolution = {resolution}")
cli::cli_h2("1. Spatial and land surfaces")

cell_polity <- nbd_stage("cell_polity", build_cell_polity())
ag_land_support <- nbd_stage(
  "ag_land_support",
  build_ag_land_support(years = year, data = list(cell_polity = cell_polity))
)
cropland_ha <- nbd_stage(
  "cropland_ha",
  ag_land_support |>
    dplyr::filter(.data$land_use == "cropland") |>
    dplyr::summarise(
      cropland_ha = sum(.data$area_ha),
      .by = c("lon", "lat", "area_code", "year")
    )
)

# ---- 2. country statistics --------------------------------------------------

cli::cli_h2("2. Country statistics")

primary_prod <- nbd_stage("primary_prod", get_primary_production())
fertilizer <- nbd_stage(
  "fertilizer",
  whep_read_file("faostat-fertilizer-nutrients")
)
manure_pin <- nbd_stage("manure", whep_read_file("faostat-emissions-livestock"))
primary_residues <- nbd_stage("primary_residues", get_primary_residues())

# ---- 3. the crop NPP chain ---------------------------------------------------

cli::cli_h2("3. Crop NPP -> carbon/nitrogen -> BNF -> residue destinies")

crops <- nbd_stage("crops", .nbd_crops_table(primary_prod, year))
npp <- nbd_stage(
  "npp_n_input",
  crops |> calculate_crop_npp() |> calculate_npp_carbon_nitrogen()
)

# ---- 4. upstream models ------------------------------------------------------

cli::cli_h2("4. Upstream models")

urban_population <- nbd_stage(
  "urban_population",
  read_hyde_population(years = year)
)
nhx <- nbd_stage("nhx", read_n_deposition("nhx", years = year))
noy <- nbd_stage("noy", read_n_deposition("noy", years = year))
carbon_balance <- nbd_stage(
  "carbon_balance",
  build_carbon_balance(resolution = "grid"),
  heavy = TRUE
)
livestock_intake <- nbd_stage(
  "livestock_intake",
  redistribute_feed(),
  heavy = TRUE
)

# ---- 5. coverage and blockers -------------------------------------------------

cli::cli_h2("5. Coverage")
report <- dplyr::bind_rows(.nbd_log$rows)
print(as.data.frame(report[, c("input", "status", "seconds", "rows")]))

# The NPP chain must be GRIDDED: .n_inputs_bnf() and .n_inputs_recycling()
# (R/n_balance_inputs.R) both read lon/lat off it. A crops table built from
# country production has neither, so the balance cannot consume it however
# complete the rest of the list is. This is checked explicitly because it is
# the blocker most easily mistaken for a missing input.
gridded_npp <- !is.null(npp) && all(c("lon", "lat") %in% names(npp))
if (!is.null(npp) && !gridded_npp) {
  .nbd_record(
    "npp_n_input (gridded)",
    "FAIL",
    0,
    NA_integer_,
    paste(
      "NPP built from country production has no lon/lat.",
      ".n_inputs_bnf() and .n_inputs_recycling() require them, so gridded",
      "per-crop production is needed first (run_spatialize() /",
      "build_gridded_landuse())."
    )
  )
}

report <- dplyr::bind_rows(.nbd_log$rows)
blockers <- dplyr::filter(report, .data$status == "FAIL")
if (nrow(blockers) > 0L) {
  cli::cli_h2("Blockers")
  for (i in seq_len(nrow(blockers))) {
    cli::cli_alert_danger("{blockers$input[i]}")
    cli::cli_bullets(c(" " = blockers$detail[i]))
  }
  cli::cli_alert_info(
    "{nrow(blockers)} blocker{?s}; the balance is not attempted. See #446."
  )
  invisible(report)
} else {
  cli::cli_h2("6. Nitrogen balance")
  balance <- nbd_stage(
    "nitrogen_balance",
    build_nitrogen_balance(
      resolution = resolution,
      data = list(
        cell_polity = cell_polity,
        ag_land_support = ag_land_support,
        cropland_ha = cropland_ha,
        primary_prod = primary_prod,
        fertilizer = fertilizer,
        manure = manure_pin,
        primary_residues = primary_residues,
        npp_n_input = npp,
        bnf_input = npp,
        residue_destiny_input = npp,
        carbon_balance = carbon_balance,
        livestock_intake = livestock_intake,
        urban_population = urban_population,
        nhx = nhx,
        noy = noy
      )
    )
  )
  if (!is.null(balance)) {
    surplus_tg <- sum(balance$n_input_std_t, na.rm = TRUE) / 1e6
    cli::cli_h2("7. Plausibility")
    cli::cli_inform("standard N input: {round(surplus_tg, 1)} Tg N/yr")
    if (surplus_tg < 50 || surplus_tg > 300) {
      cli::cli_warn(
        "Outside the 50-300 Tg N/yr range a global agricultural nitrogen
         input should sit in. Treat the chain as wrong before treating the
         world as surprising (#446 step 3)."
      )
    }
  }
  invisible(list(report = report, balance = balance))
}
