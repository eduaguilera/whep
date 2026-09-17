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
#   Rscript --no-init-file inst/scripts/run_nitrogen_balance.R [year] [resolution]
#
#   NOT `--vanilla`: that implies `--no-environ`, so R never reads
#   `~/.Renviron` -- which is exactly where CLAUDE.md says every `WHEP_*` path
#   belongs, and the only place a working checkout may keep them (a `.Renviron`
#   at the repo root would hide the home one, whep#456). Run this way, the very
#   first stage that needs a raster died with "No WHEP_TYPE_CROPLAND_PATH input
#   available" on a machine where the variable was set. `--no-init-file` skips
#   the repo `.Rprofile`'s `devtools::load_all()` (this script loads the
#   package itself) while still reading the environment file.
#
#   WHEP_NBD_SKIP_HEAVY=1   skip the two multi-minute stages (the SOC carbon
#                           balance and the feed redistribution) to get a fast
#                           coverage report of everything else.
#   WHEP_NBD_OUT=<file.rds> save the coverage report, balance, surplus and
#                           boundary exceedance, so
#                           `validation/n_balance_gridded.R` can check them
#                           without re-running the assembly.
#
# Requires the local surfaces (CLAUDE.md, "New data sources"):
#   WHEP_TYPE_CROPLAND_PATH   WHEP_CROP_PATTERNS_PATH  WHEP_GRIDDED_PASTURE_PATH
#   WHEP_HANI_DIR             WHEP_HYDE_DIR
# plus cached pins for production, fertiliser and the commodity balances.
# `WHEP_POLITY_FRACTION_PATH` is only an override: the cell-polity crosswalk is
# WHEP-built and comes from the `spatialize-cell-polity-fraction` pin (whep#694).

suppressMessages(pkgload::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
year <- as.integer(if (length(args) >= 1L) args[[1L]] else "2010")
resolution <- if (length(args) >= 2L) args[[2L]] else "grid"
skip_heavy <- nzchar(Sys.getenv("WHEP_NBD_SKIP_HEAVY"))
out_path <- Sys.getenv("WHEP_NBD_OUT")

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
  # mapping disagrees with the forward one -- and it is .polity_crosswalk(),
  # NOT the static whep::polity_area_crosswalk this read from. The two differ by
  # .unfold_rest_of_world(): since whep#628 the Rest-of-World bucket is unfolded,
  # so a former member keeps its own code as its polity code, and that code
  # appears nowhere in the static table's polity_area_code column. Measured at
  # 2010: area codes 22, 69, 85, 135, 180 and 182 were all matched by the
  # forward re-keying, all unmatched by the static reverse lookup, so their raw
  # fertiliser rows survived the drop and .n_polity_crop_totals() still aborted
  # on exactly those six -- "Cannot allocate 6 polity N totals".
  bridge <- whep:::.polity_crosswalk() |>
    as.data.frame() |>
    tibble::as_tibble() |>
    dplyr::transmute(
      raw = as.integer(.data$area_code),
      polity = as.integer(.data$polity_area_code)
    )
  drop_raw <- bridge$raw[bridge$polity %in% unsupported$area_code]
  dplyr::filter(fertilizer, !as.integer(.data[["Area Code"]]) %in% drop_raw)
}

# The loss cascade's method set, and the one driver column it still needs.
#
# build_nitrogen_balance()'s defaults are MANNER ammonia and the Meisinger
# drainage cascade. Both were ported from Spain_Hist and both need per-cell
# driver columns that nothing in the package produces globally (#359): MANNER
# wants `manner_fertiliser` plus soil pH, application rate, rainfall,
# irrigation, wind, technique, system, temperature and incorporation delay;
# Meisinger wants `drainage_mm` plus tillage, irrigation category, SOM share
# and C:N. The IPCC Tier 1 alternatives are already selectable, are globally
# applicable by construction, and are stamped into `method_nh3` /
# `method_leaching`, so a balance built this way says which it used.
#
# With those chosen, exactly ONE driver column is left: `climate` (`"ATL"` or
# `"MED"`), read by calculate_soil_n2o() and calculate_indirect_n2o_nh3().
# There is no global classifier for it -- that is #359's first suggested
# action -- so this driver supplies a CONSTANT placeholder. It is result-
# affecting for the loss columns and for nothing else: `n_input_std_t`,
# `prod_n_t`, `used_residue_n_t` and `grazed_weeds_n_t` are all computed by
# .nb_inputs()/.nb_outputs() before .nb_losses() runs, and those four are the
# whole of calculate_n_surplus(method = "harvest_removal"). The script checks
# that identity below rather than asserting it.
NBD_LOSS_METHODS <- list(
  nh3 = "ipcc",
  n2o = "ipcc2019",
  leaching = "ipcc_fracleach"
)
NBD_PLACEHOLDER_CLIMATE <- "ATL"

# One driver row per (balance key, fert_type), matching the grain .nb_loss_rows()
# summarises to, so the balance's many-to-one join is satisfied exactly.
.nbd_climate_drivers <- function(n_inputs, resolution, climate) {
  if (is.null(n_inputs)) {
    return(NULL)
  }
  key <- whep:::.nb_key(resolution)
  n2o_fert_types <- c(
    "excreta",
    "manure_liquid",
    "manure_solid",
    "som_mineralization",
    "synthetic",
    "urban",
    "recycling"
  )
  n_inputs |>
    dplyr::filter(.data$fert_type %in% n2o_fert_types) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(key, "fert_type")))) |>
    dplyr::mutate(
      fert_type = whep:::.nb_loss_fert_type(.data$fert_type),
      climate = climate
    ) |>
    dplyr::distinct()
}

# The harvest-removal surplus re-derived from the balance's own pre-loss
# columns. If this matches calculate_n_surplus() to floating point, the
# surplus this run reports is untouched by the placeholder climate -- and by
# every other loss method choice -- which is the only reason a number from a
# run with a placeholder driver is quotable at all.
.nbd_check_surplus_is_lossfree <- function(surplus) {
  recomputed <- surplus$n_input_std_t -
    (surplus$prod_n_t + surplus$used_residue_n_t + surplus$grazed_weeds_n_t)
  gap <- max(abs(recomputed - surplus$surplus_n_t), na.rm = TRUE)
  cli::cli_inform(c(
    i = "Surplus recomputed from the pre-loss columns differs by
         {signif(gap, 3)} t N at worst, so no loss method (and therefore no
         placeholder climate) reaches it."
  ))
  invisible(gap)
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
#
# Marched over the spin-up, then SLICED to the driven year, like every other
# input. build_nitrogen_balance() has no `years` argument, so a whole marched
# balance handed to it makes .n_inputs_som() emit thirty-one years of soil
# mineralization against a land support built for one -- thirty years of it
# then has no support row at all, is dropped by the allocation join, and takes
# the mass check down with it. That would have hidden the very factor this
# spin-up removes: the source mass reaching the guard would be the whole span's
# nitrogen, not the driven year's.
#
# method_grazing: "whep" is build_carbon_balance()'s default and needs BOTH
# data$livestock_intake and data$excreta, which it refuses to run without
# (whep#1120). Neither is reachable while the commodity balances are: the
# livestock chain starts at get_wide_cbs(), and that aborts on the
# faostat-cbs-new pin's logical unit column (whep#1025). "lpjml" is the
# package's own selectable alternative -- LPJmL's livestock module supplies the
# grassland offtake instead -- and needs neither input. It is recorded in the
# output's method_grazing column, so a balance built this way says so.
carbon_balance <- nbd_stage(
  "carbon_balance",
  build_carbon_balance(
    resolution = "grid",
    years = (year - NBD_SPINUP_YEARS):year,
    method_grazing = "lpjml"
  ) |>
    dplyr::filter(.data$year == !!year),
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
# build_n_inputs() answers a NULL carbon balance or livestock intake with an
# EMPTY term, not an error, so a run without them is a partial balance rather
# than a failed one -- and both are currently unreachable: the livestock chain
# starts at get_wide_cbs(), which aborts on the faostat-cbs-new pin's logical
# unit column (#1025), and the carbon balance's own default grazing method
# needs that same chain (#1120). Report them as gaps and keep going, so the
# terms that ARE available still get measured; a genuine blocker still stops.
NBD_TOLERATED <- c("carbon_balance", "livestock_intake")
gaps <- dplyr::filter(
  report,
  .data$status != "ok",
  .data$input %in% NBD_TOLERATED
)
if (nrow(gaps) > 0L) {
  cli::cli_h2("5b. Terms this run does NOT carry")
  for (i in seq_len(nrow(gaps))) {
    cli::cli_alert_warning("{gaps$input[i]} ({gaps$status[i]})")
    if (!is.na(gaps$detail[i])) {
      cli::cli_bullets(c(" " = gaps$detail[i]))
    }
  }
  cli::cli_alert_info(
    "The balance runs without them; its som_mineralization, manure and
     grazed-weeds terms are then zero, and every total below excludes them."
  )
}
blockers <- dplyr::filter(
  report,
  .data$status == "FAIL",
  !.data$input %in% NBD_TOLERATED
)
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
  nbd_data <- list(
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
    # Non-item nitrogen whose own cell carries no cropland. build_urban_n()
    # returns the urban nitrogen its transport step could not deliver AT THE
    # SOURCE CELL, and on the 2010 global grid 1985 of those cells hold no
    # cropland: 38,425 t of 4.02 Mt urban N, enough to abort the whole
    # assembly under the default "abort" rule. "reallocate_drop" keeps the mass
    # on the polity's other cropland cells -- the same rule the synthetic path
    # already applies to a crop with no pattern cell -- and discards only what
    # no polity can carry at all: 51 rows, 834 t N, 0.021% of urban N, in
    # polities with population and no cropland anywhere in the year. Refusing a
    # global build over that is disproportionate; losing it unremarked is what
    # the guard exists to prevent, so the rule is named and the cost printed.
    # Recorded in method_unsupported.
    method_unsupported = "reallocate_drop",
    urban_population = urban_population,
    nhx = nhx,
    noy = noy
  )

  # Built here rather than inside build_nitrogen_balance() because the climate
  # driver table has to be keyed on the rows the inputs actually produced.
  n_inputs <- nbd_stage(
    "n_inputs",
    build_n_inputs(resolution = resolution, data = nbd_data)
  )
  if (!is.null(n_inputs)) {
    by_stream <- n_inputs |>
      dplyr::summarise(
        tg_n = sum(.data$n_input_t, na.rm = TRUE) / 1e6,
        .by = "fert_type"
      ) |>
      dplyr::arrange(dplyr::desc(.data$tg_n))
    print(as.data.frame(by_stream))
  }
  # The balance would rebuild the inputs itself from the same `data`, so a
  # failure here is a failure there: say so once instead of paying for it twice.
  if (is.null(n_inputs)) {
    failure <- dplyr::last(dplyr::bind_rows(.nbd_log$rows))
    cli::cli_h2("6b. Why the balance did not run")
    cli::cli_inform(c(x = "{failure$detail}"))
  }
  cli::cli_inform(c(
    "!" = "climate driver: every loss row is given
           {.val {NBD_PLACEHOLDER_CLIMATE}} because no global classifier
           exists (#359). The loss columns of this run are NOT results."
  ))
  balance <- if (is.null(n_inputs)) {
    NULL
  } else {
    nbd_stage(
      "nitrogen_balance",
      build_nitrogen_balance(
        methods = NBD_LOSS_METHODS,
        resolution = resolution,
        data = c(
          nbd_data,
          list(
            n_inputs = n_inputs,
            n_balance_drivers = .nbd_climate_drivers(
              n_inputs,
              resolution,
              NBD_PLACEHOLDER_CLIMATE
            )
          )
        )
      )
    )
  }
  # The whole point of the driver is this stage, so when it fails say why here
  # rather than leaving the reason in a column of the coverage table nobody
  # prints. Section 5's blocker list runs before this stage exists.
  if (is.null(balance) && !is.null(n_inputs)) {
    failure <- dplyr::last(dplyr::bind_rows(.nbd_log$rows))
    cli::cli_h2("6b. Why the balance did not run")
    cli::cli_inform(c(x = "{failure$detail}"))
  }
  surplus <- NULL
  exceedance <- NULL
  if (!is.null(balance)) {
    cli::cli_h2("7. Surplus and plausibility")
    surplus <- nbd_stage("n_surplus", calculate_n_surplus(balance))
  }
  if (!is.null(surplus)) {
    .nbd_check_surplus_is_lossfree(surplus)
    input_tg <- sum(balance$n_input_std_t, na.rm = TRUE) / 1e6
    surplus_tg <- sum(surplus$surplus_n_t, na.rm = TRUE) / 1e6
    cli::cli_inform("standard N input:  {round(input_tg, 1)} Tg N/yr")
    cli::cli_inform("harvest-removal surplus: {round(surplus_tg, 1)} Tg N/yr")
    if (input_tg < 50 || input_tg > 300) {
      cli::cli_warn(
        "Outside the 50-300 Tg N/yr range a global agricultural nitrogen
         input should sit in. Treat the chain as wrong before treating the
         world as surprising (#446 step 3)."
      )
    }
    exceedance <- nbd_stage(
      "n_boundary_exceedance",
      build_n_boundary_exceedance(
        surplus = surplus,
        resolution = "grid",
        cell_polity = cell_polity,
        actual_year = year
      )
    )
  }
  if (!is.null(exceedance)) {
    cli::cli_h2("8. Boundary exceedance")
    cli::cli_inform(
      "exceedance: {round(sum(exceedance$exceedance_n_t, na.rm = TRUE) / 1e6, 1)}
       Tg N/yr over
       {round(sum(exceedance$within_boundary_n_t, na.rm = TRUE) / 1e6, 1)}
       Tg N/yr within the critical surplus."
    )
  }
  # classify_sjos_n() needs a nourishment axis and build_sjos_n_footprint()
  # needs an IO model; both descend from the commodity balances, so both are
  # unreachable while get_wide_cbs() aborts (#1025). Say so rather than
  # leaving the last two steps of #446 silently unattempted.
  cli::cli_h2("9. Not attempted")
  cli::cli_alert_warning(
    "classify_sjos_n() and build_sjos_n_footprint() need the commodity
     balances (nourishment axis, IO model); get_wide_cbs() aborts on the
     faostat-cbs-new pin's logical unit column (#1025)."
  )
  result <- list(
    year = year,
    resolution = resolution,
    report = dplyr::bind_rows(.nbd_log$rows),
    balance = balance,
    surplus = surplus,
    exceedance = exceedance
  )
  if (nzchar(out_path)) {
    saveRDS(result, out_path)
    cli::cli_alert_success("Saved to {.file {out_path}}.")
  }
  invisible(result)
}
