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
      substr(gsub("\\s+", " ", detail), 1, 1200)
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

# Spread the national per-crop NPP chain onto cells.
#
# .n_inputs_bnf() and .n_inputs_recycling() read lon/lat off this table, and a
# crops table built from country production has neither. Rather than introduce
# a second spatialization, reuse the weights build_soil_carbon_inputs() already
# uses to put per-crop carbon on cells: harvested-area shares per polity-crop
# from the crop_patterns surface, which sum to 1 per (area_code,
# item_prod_code). Every extensive column is scaled by that share, so cell
# values sum back to the national ones.
#
# METHOD NOTE: distributing NPP by harvested-area share is the same assumption
# the soil-carbon inputs already make for the same crops, not a new one, but it
# is an assumption and belongs to the scientific sign-off for #446, not to this
# driver. What the driver owes is the arithmetic and the loss report below.
#
# The residual is polity-crops that have production but no positive cropland
# cell, so nothing to spread onto; they drop out here exactly as they do in the
# carbon inputs (cf. #599). It is reported rather than silently absorbed.
.nbd_grid_npp <- function(npp) {
  weights <- whep:::.sci_grid_weights(
    whep:::.sci_read_country_grid(),
    whep:::.sci_read_crop_patterns()
  )
  keys <- c("year", "area_code", "item_prod_code")
  extensive <- setdiff(names(npp)[vapply(npp, is.numeric, TRUE)], keys)
  # .n_inputs_recycling() reads item_cbs_code straight off this table, while
  # .n_inputs_bnf() derives it from item_prod_code. Apply the package's own
  # mapping once here so all three consumers of this table see the same codes.
  gridded <- npp |>
    dplyr::mutate(
      item_cbs_code = whep:::.ni_item_cbs_from_prod(.data$item_prod_code)
    ) |>
    dplyr::inner_join(
      weights,
      by = c("area_code", "item_prod_code"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(extensive), \(v) v * .data$area_weight)
    )
  gridded
}

# What the gridding could not place, as a share of the national total. A silent
# 0.1% is fine; a silent 30% would not be, and neither is distinguishable from
# an arithmetic error once it reaches the surplus.
.nbd_report_unspatialized <- function(npp, gridded) {
  for (column in intersect(c("crop_npp_n_t", "product_n_t"), names(npp))) {
    national <- sum(npp[[column]], na.rm = TRUE)
    placed <- sum(gridded[[column]], na.rm = TRUE)
    if (!is.finite(national) || national == 0) {
      next
    }
    lost <- (national - placed) / national
    cli::cli_inform(c(
      i = "{column}: {round(100 * lost, 3)}% not spatialized
           ({signif(national - placed, 3)} of {signif(national, 4)} t)."
    ))
  }
  invisible(NULL)
}

# The BNF input needs three columns the NPP chain does not produce, and which
# nothing in the package supplies: land_use, legumes_seeded and
# seeded_cover_crop_share. calculate_weed_bnf() aborts without them.
#
# land_use is not a choice here -- these rows are crop NPP, so "cropland".
#
# ASSUMPTION, for #446's scientific sign-off to replace: no seeded cover crops
# (legumes_seeded = 0, seeded_cover_crop_share = 0). That leaves weed BNF coming
# only from spontaneous weeds via calculate_weed_bnf()'s own land-use default,
# and it is deliberately the LOW end -- a wrong non-zero cover-crop share would
# inflate fixation and therefore the surplus, whereas zero can only understate
# it. It is not a claim that cover cropping is absent globally.
.nbd_bnf_input <- function(npp) {
  if (is.null(npp)) {
    return(NULL)
  }
  cli::cli_inform(c(
    "!" = "bnf_input: assuming no seeded cover crops (legumes_seeded = 0,
           seeded_cover_crop_share = 0). Weed BNF is a lower bound (#446)."
  ))
  npp |>
    dplyr::mutate(
      land_use = "cropland",
      legumes_seeded = 0,
      seeded_cover_crop_share = 0
    )
}

# Drop fertiliser for polities that have no cropland to spread it on.
#
# spatialize_country_n_to_crops() aborts rather than lose that nitrogen
# silently, which is right -- but for 2010 the offenders are Greenland, Palau,
# French Guiana, Martinique, Reunion and the residual Rest-of-World bucket:
# territories reporting a little fertiliser with no crop production in WHEP's
# data. Together 1367 t, 0.0013% of the global 101.33 Mt. Aborting a global run
# on that is disproportionate; losing it unremarked is what the guard exists to
# prevent. So the driver makes the call, in the open, and prints what it cost.
#
# This is a data-coverage gap, not a fix: either those polities should carry
# crop area, or their fertiliser should not be attributed to them (#446).
.nbd_drop_unsupported_fertilizer <- function(
  fertilizer,
  primary_prod,
  year,
  cropland_ha = NULL
) {
  if (is.null(fertilizer) || is.null(primary_prod)) {
    return(fertilizer)
  }
  totals <- whep:::.synthetic_n_country(fertilizer)
  shares <- whep:::.n_synthetic_crop_shares(primary_prod, "coello", NULL)
  supported <- dplyr::distinct(shares, .data$year, .data$area_code)
  # Crop shares are not enough: a polity can have crop production and still no
  # positive cropland CELL at 0.5 degrees, which is where the grid step then
  # aborts. cropland_ha is the driver's own gridded cropland, so require both.
  if (!is.null(cropland_ha)) {
    supported <- dplyr::semi_join(
      supported,
      dplyr::distinct(cropland_ha, .data$year, .data$area_code),
      by = c("year", "area_code")
    )
  }
  unsupported <- totals |>
    dplyr::filter(.data$synthetic_n_t > 0) |>
    dplyr::anti_join(supported, by = c("year", "area_code"))
  if (nrow(unsupported) == 0L) {
    return(fertilizer)
  }
  cli::cli_inform(c(
    "!" = "Dropping {nrow(unsupported)} polit{?y/ies} with fertiliser but no
           cropland: {signif(sum(unsupported$synthetic_n_t), 4)} t,
           {signif(100 * sum(unsupported$synthetic_n_t) / sum(totals$synthetic_n_t), 3)}%
           of {year} synthetic N. Codes: {unsupported$area_code}."
  ))
  # The SAME crosswalk .synthetic_n_country() re-keys with, or the reverse
  # mapping disagrees with the forward one: that helper reads the static
  # whep::polity_area_crosswalk, so members that fold into 999 are invisible to
  # .polity_crosswalk() and their raw rows survive the drop, leaving the folded
  # bucket still carrying nitrogen it cannot place.
  bridge <- tibble::as_tibble(whep::polity_area_crosswalk) |>
    dplyr::transmute(
      raw = as.integer(.data$area_code),
      polity = as.integer(.data$polity_area_code)
    )
  drop_raw <- bridge$raw[bridge$polity %in% unsupported$area_code]
  dplyr::filter(fertilizer, !as.integer(.data[["Area Code"]]) %in% drop_raw)
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

# Scoped to the driven year, like every other input. build_nitrogen_balance()
# has no `years` argument -- it covers whatever span its inputs do -- so an
# unscoped production table made it compare 2010 crop shares against country
# totals for 2002-2023 and abort on every year that did not line up.
primary_prod <- nbd_stage("primary_prod", get_primary_production(years = year))
# Same reason: the raw pin carries every year, and the synthetic-N country
# totals derived from it must cover the same span as the crop shares.
fertilizer <- nbd_stage(
  "fertilizer",
  whep_read_file("faostat-fertilizer-nutrients") |>
    dplyr::filter(as.integer(.data$Year) == year)
)
manure_pin <- nbd_stage("manure", whep_read_file("faostat-emissions-livestock"))
primary_residues <- nbd_stage("primary_residues", get_primary_residues())
fertilizer <- .nbd_drop_unsupported_fertilizer(
  fertilizer,
  primary_prod,
  year,
  cropland_ha
)

# ---- 3. the crop NPP chain ---------------------------------------------------

cli::cli_h2("3. Crop NPP -> carbon/nitrogen -> BNF -> residue destinies")

crops <- nbd_stage("crops", .nbd_crops_table(primary_prod, year))
npp_national <- nbd_stage(
  "npp_n_input (national)",
  crops |> calculate_crop_npp() |> calculate_npp_carbon_nitrogen()
)
npp <- nbd_stage("npp_n_input", .nbd_grid_npp(npp_national))
# Reported here, not inside the stage: nbd_stage() suppresses messages so a
# noisy input cannot drown the coverage table, which would also hide this.
if (!is.null(npp) && !is.null(npp_national)) {
  .nbd_report_unspatialized(npp_national, npp)
}

# ---- 4. upstream models ------------------------------------------------------

cli::cli_h2("4. Upstream models")

urban_population <- nbd_stage(
  "urban_population",
  read_hyde_population(years = year)
)
nhx <- nbd_stage("nhx", read_n_deposition("nhx", years = year))
noy <- nbd_stage("noy", read_n_deposition("noy", years = year))
# Marched, not single-year. A one-year balance initialises every cell at
# equilibrium and takes one step, so that step absorbs the whole gap between
# equilibrium and reality -- and .n_inputs_som() reads it as an annual nitrogen
# input. Measured at 2010, varying only this span:
#
#   2010 only     median son_change 1136 kg N/ha   SOM N 1403 Tg
#   2000-2010      479                              586 Tg
#   1980-2010      274                              312 Tg
#   1901-2010      280                              340 Tg
#
# It converges by roughly thirty years, so NBD_SPINUP_YEARS defaults to that:
# long enough that the initialisation transient is spent, short enough that a
# diagnostic run stays affordable. Longer buys nothing measurable.
#
# The converged value is still ~3-5x the physical range, which is NOT this
# span's doing and is not fixed by lengthening it (#792) -- it is the carbon
# balance's own calibration. The driver's job is to stop manufacturing the 4x
# that was its own.
NBD_SPINUP_YEARS <- 30L
carbon_balance <- nbd_stage(
  "carbon_balance",
  build_carbon_balance(
    resolution = "grid",
    years = (year - NBD_SPINUP_YEARS):year
  ),
  heavy = TRUE
)
# redistribute_feed() takes two already-assembled tables (feed demand and feed
# availability); .run_redistribute_national() is the wrapper that builds both
# from production and the commodity balances, and is what the manure path in
# build_soil_carbon_inputs() already uses. Calling redistribute_feed() bare, as
# this driver did, can only fail on a missing argument.
livestock_intake <- nbd_stage(
  "livestock_intake",
  whep:::.run_redistribute_national(
    production = primary_prod,
    cbs = get_wide_cbs(years = year),
    demand_tier = "ipcc",
    options = list(distribute_surplus = FALSE)
  ),
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
        bnf_input = .nbd_bnf_input(npp),
        residue_destiny_input = npp,
        carbon_balance = carbon_balance,
        livestock_intake = livestock_intake,
        # build_livestock_nutrient_flows() needs the land surface its manure is
        # spread over as well as the intake; .sci_manure_crop_layer() is the
        # same crops layer build_soil_carbon_inputs() gives it, so the manure
        # reaching the nitrogen balance sits on the same hectares as the manure
        # reaching the carbon balance.
        gridded = list(crops = whep:::.sci_manure_crop_layer(primary_prod)),
        # The default allocation cap, "potential_uptake", needs a precomputed
        # crop_n_cap that this crops layer does not carry. build_soil_carbon_
        # inputs() hits the same wall and answers it with "fixed_ceiling", so
        # use that here too: the manure entering the nitrogen balance is then
        # allocated exactly as the manure entering the carbon balance, rather
        # than the two disagreeing about the same animals. A method choice, and
        # one for #446's sign-off to confirm.
        methods = list(allocation = list(cap_method = "fixed_ceiling")),
        urban_population = urban_population,
        nhx = nhx,
        noy = noy
      )
    )
  )
  # The whole point of the driver is this stage, so when it fails say why here
  # rather than leaving the reason in a column of the coverage table nobody
  # prints. Section 5's blocker list runs before this stage exists.
  if (is.null(balance)) {
    failure <- dplyr::last(dplyr::bind_rows(.nbd_log$rows))
    cli::cli_h2("6b. Why the balance did not run")
    cli::cli_inform(c(x = "{failure$detail}"))
  }
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
