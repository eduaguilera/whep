# redistribute_feed-based feed-intake path (migration in progress).
#
# Plan: plans/2026-06-19-wire-redistribute-feed-migration. Three engines:
#   Engine 1 DEMAND   = a 3-method hybrid: IPCC Tier-2 energy for the 4 ruminant
#                       species it covers (Cattle, Buffalo, Sheep, Goats),
#                       Bouwman FCR for pigs/poultry (no Tier-2 energy model
#                       exists for monogastrics), Krausmann per-head for draft /
#                       other species.
#   Engine 2 MIX      = Bouwman dm_share (within-species feed_type split).
#   Engine 3 ALLOCATE = redistribute_feed() at 0.5-degree cell grain
#                       (territory = country, sub_territory = cell), with the
#                       grass ceiling/deficit cascade + the distance-decay
#                       feed-access buffer on the roughage trade.
#
# The national grain is wired (Engines 1-3 plus the Phase-6 reshape). The
# provincial grain (sub_territory = 0.5-degree cell) is still in progress and
# errors; the package default (grain = "national", demand_tier = "fcr") keeps
# routing to the legacy allocator.

.build_redistribute_intake <- function(grain, demand_tier) {
  if (grain == "provincial") {
    cli::cli_abort(c(
      "Run the {.val provincial} grain with {.fn build_feed_intake_provincial}.",
      i = "It chunks the per-cell allocation by year (the full run is too large
        for one in-memory result): pass {.arg out_dir} to write per-year output
        to disk, or a small {.arg years} subset to return it in memory."
    ))
  }
  engine <- .national_redistribute(
    get_primary_production(),
    get_wide_cbs(),
    demand_tier,
    .feed_demand_data()
  )
  .reshape_redistribute_intake(engine$result, engine$code_shares)
}

#' Build provincial (per-cell) feed intake, chunked by year.
#'
#' @description
#' Runs the `redistribute_feed` provincial path (0.5-degree cell grain) one year
#' at a time, so the per-cell allocation stays within memory and the full
#' multi-year run is restartable. Sources the LPJmL grass run and gridded
#' livestock inputs from the `WHEP_LPJML_RUN_DIR` and `WHEP_SPATIAL_INPUT_DIR`
#' environment variables.
#'
#' @param years Integer vector of years to build. Default `NULL` builds every
#'   year present in the production data.
#' @param out_dir Directory to write per-year `feed_intake_provincial_<year>`
#'   parquet files to. If `NULL`, the bound result is returned in memory (only
#'   practical for a few years).
#' @param demand_tier Demand-estimation tier, `"ipcc"` (default) or `"fcr"`.
#' @param overwrite Re-run years whose output file already exists. Default
#'   `FALSE` skips them so the batch is restartable.
#' @param example If `TRUE`, return a small example output without sourcing the
#'   remote and gridded data. Default is `FALSE`.
#'
#' @returns
#' When `out_dir` is `NULL`, a tibble in the `get_feed_intake()` contract plus a
#' `sub_territory` (0.5-degree cell) column. Otherwise, invisibly, the written
#' file paths.
#'
#' @export
#'
#' @examples
#' build_feed_intake_provincial(example = TRUE)
build_feed_intake_provincial <- function(
  years = NULL,
  out_dir = NULL,
  demand_tier = c("ipcc", "fcr"),
  overwrite = FALSE,
  example = FALSE
) {
  if (example) {
    return(.example_provincial_intake())
  }
  demand_tier <- rlang::arg_match(demand_tier)
  ctx <- .provincial_run_context(demand_tier)
  years <- .resolve_provincial_years(years, ctx$production)
  if (is.null(out_dir)) {
    return(.bind_provincial_years(years, ctx))
  }
  .write_provincial_years(years, ctx, out_dir, overwrite)
}

# Shared per-run context (configured paths + the once-fetched, normalised
# production / CBS / coefficient data), grouped so the per-year helpers take few
# arguments.
.provincial_run_context <- function(demand_tier) {
  list(
    paths = .provincial_paths(),
    production = .normalise_feed_primary(get_primary_production()),
    cbs = .normalise_feed_cbs(get_wide_cbs()),
    data = .feed_demand_data(),
    demand_tier = demand_tier
  )
}

# Years to build: every production year, or the requested subset intersected
# with the production years.
.resolve_provincial_years <- function(years, production) {
  all_years <- sort(unique(as.integer(
    .normalise_feed_primary(production)$year
  )))
  if (is.null(years)) {
    all_years
  } else {
    intersect(as.integer(years), all_years)
  }
}

# One year's provincial intake: per-cell spatial inputs -> engine -> contract.
.provincial_year_intake <- function(yr, ctx) {
  spatial <- .provincial_spatial_inputs(yr, ctx$paths)
  prod_y <- dplyr::filter(ctx$production, as.integer(.data$year) == yr)
  cbs_y <- dplyr::filter(ctx$cbs, as.integer(.data$year) == yr)
  engine <- .run_redistribute_provincial(
    prod_y,
    cbs_y,
    ctx$demand_tier,
    spatial,
    ctx$data
  )
  .reshape_redistribute_intake(
    engine$result,
    engine$code_shares,
    provincial = TRUE
  )
}

# Write per-year output to disk, skipping years already written (restartable)
# and releasing each year's memory before the next.
.write_provincial_years <- function(years, ctx, out_dir, overwrite) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  written <- purrr::map_chr(years, function(yr) {
    f <- file.path(out_dir, sprintf("feed_intake_provincial_%d.parquet", yr))
    if (file.exists(f) && !overwrite) {
      cli::cli_alert_info("Year {yr}: output exists, skipping.")
      return(f)
    }
    out <- .provincial_year_intake(yr, ctx)
    nanoparquet::write_parquet(out, f)
    cli::cli_alert_success("Year {yr}: {nrow(out)} rows written.")
    gc()
    f
  })
  invisible(written)
}

# Bind years in memory (practical only for a few years).
.bind_provincial_years <- function(years, ctx) {
  if (length(years) > 5) {
    cli::cli_warn(c(
      "Binding {length(years)} years of provincial intake in memory.",
      i = "Pass {.arg out_dir} to write per-year output to disk for large runs."
    ))
  }
  dplyr::bind_rows(purrr::map(years, function(yr) {
    .provincial_year_intake(yr, ctx)
  }))
}

# Resolve the configured provincial input directories from the environment,
# aborting with guidance if either is unset.
.provincial_paths <- function() {
  run_dir <- Sys.getenv("WHEP_LPJML_RUN_DIR")
  input_dir <- Sys.getenv("WHEP_SPATIAL_INPUT_DIR")
  if (!nzchar(run_dir) || !nzchar(input_dir)) {
    cli::cli_abort(c(
      "The {.val provincial} grain needs the spatial input directories.",
      i = "Set {.envvar WHEP_LPJML_RUN_DIR} (the LPJmL run scenario output
        directory with {.file pft_npp.nc} and {.file cftfrac.nc}) and
        {.envvar WHEP_SPATIAL_INPUT_DIR} (the gridded-livestock input directory)."
    ))
  }
  list(run_dir = run_dir, input_dir = input_dir)
}

# Source the provincial spatial inputs (per-cell livestock head shares + per-cell
# grass availability) from the configured directories. A heavy global
# computation: gridded livestock plus LPJmL grass for every model year. Years
# outside the LPJmL run's coverage get unbounded grass.
.provincial_spatial_inputs <- function(years, paths) {
  ls_inputs <- .load_livestock_inputs(paths$input_dir)
  gridded_heads <- build_gridded_livestock(
    livestock_data = ls_inputs$livestock_data,
    gridded_pasture = ls_inputs$gridded_pasture,
    gridded_cropland = ls_inputs$gridded_cropland,
    country_grid = ls_inputs$country_grid,
    species_proxy = ls_inputs$species_proxy,
    manure_pattern = ls_inputs$manure_pattern,
    years = years
  )
  grass <- build_grass_availability(
    method = "lpjml",
    run_dir = paths$run_dir,
    years = years
  )
  list(
    cell_shares = .heads_to_cell_shares(gridded_heads),
    grass_avail = .grass_to_cells(grass, ls_inputs$country_grid)
  )
}

# Curated live_anim_code -> livestock_category crosswalk: maps each live animal
# (animals_codes.csv) to its feed livestock_category (max_intake_share.csv) and
# marks which demand method it uses (`demand_source`: "ipcc" Tier-2 energy for
# the 4 ruminant species, "fcr" Bouwman for pigs/poultry, "krausmann" per-head
# for draft / other). `energy_species` is INFORMATIONAL ONLY: the energy model
# resolves the species itself from animals_codes via .get_general_species, so
# this column documents (it does not drive) which energy species each IPCC row
# uses. Built + adversarially verified against the three source code systems.
.livestock_crosswalk <- function() {
  system.file(
    "extdata",
    "feed",
    "livestock_category_crosswalk.csv",
    package = "whep"
  ) |>
    data.table::fread(na.strings = "") |>
    tibble::as_tibble()
}

# ---- Engine 1: DEMAND -------------------------------------------------------

# Total dry-matter feed demand per (year, area_code, livestock_category). Each
# live animal's demand uses the method the crosswalk assigns it; methods are
# alternatives, never silent fallbacks, and the chosen one is recorded in
# `method_demand`:
#   * demand_tier = "ipcc": IPCC Tier-2 energy (gross_energy -> DM) for the
#     ruminant species (the only ones IPCC's energy model covers); Bouwman FCR
#     for pigs and poultry; Krausmann per-head for draft / other species.
#   * demand_tier = "fcr": the legacy Bouwman / Krausmann magnitude for every
#     species (the regression baseline that reproduces today's output).
.build_feed_demand_total <- function(
  production,
  demand_tier,
  data = .feed_demand_data()
) {
  .build_feed_demand_codes(production, demand_tier, data) |>
    .aggregate_demand_to_category(data$crosswalk)
}

# Per-(year, area_code, live_anim_code) dry-matter demand, each row tagged with
# the method that produced it. This pre-aggregation breakdown is the reverse-
# split weight the reshape uses to attribute a livestock_category's allocated
# intake back to its individual live animals (feed follows the demand it
# generated).
.build_feed_demand_codes <- function(
  production,
  demand_tier,
  data = .feed_demand_data()
) {
  prod <- .normalise_feed_primary(production)
  if (nrow(prod) == 0) {
    return(.empty_feed_demand_codes())
  }
  fcr <- .build_bouwman_fcr(data$conv_bouwman, sort(unique(prod$year)))
  totals <- .build_feed_demand(
    prod,
    data$items_prod_full,
    data$animals_codes,
    data$conv_krausmann,
    data$polities_cats,
    fcr
  ) |>
    dplyr::summarise(
      demand_dm_t = sum(demand_aft, na.rm = TRUE),
      .by = c(year, area_code, live_anim_code)
    ) |>
    .tag_legacy_method(data$crosswalk)

  if (demand_tier == "ipcc") {
    totals <- .overlay_energy_demand(totals, production, data$crosswalk)
  }
  totals
}

# Tag every legacy (`.build_feed_demand`) row with the method that actually
# produced it: that path is always the Bouwman / Krausmann magnitude, so the
# label is the "fcr"-tier label (`bouwman_fcr` or `krausmann_per_head`). The
# ipcc tier later overwrites the covered ruminant rows with energy demand and
# re-tags them; any uncovered ruminant keeps this FCR-fallback label, so the
# recorded method always reflects the number actually used.
.tag_legacy_method <- function(totals, crosswalk) {
  src <- crosswalk |>
    dplyr::transmute(
      live_anim_code = as.integer(live_anim_code),
      demand_source
    )
  totals |>
    dplyr::mutate(live_anim_code = as.integer(live_anim_code)) |>
    dplyr::inner_join(src, by = "live_anim_code") |>
    dplyr::transmute(
      year,
      area_code,
      live_anim_code,
      demand_dm_t,
      method_demand = .demand_method_label(demand_source, "fcr")
    )
}

# Package datasets the demand engine needs, grouped so the signature stays
# small and tests can inject fixtures.
.feed_demand_data <- function() {
  list(
    items_prod_full = whep::items_prod_full,
    animals_codes = whep::animals_codes,
    conv_krausmann = whep::conv_krausmann,
    conv_bouwman = whep::conv_bouwman,
    polities_cats = whep::polities_cats,
    crosswalk = .livestock_crosswalk()
  )
}

# Replace the ruminant codes' legacy FCR totals with IPCC Tier-2 energy totals,
# re-tagging those rows as energy-derived. Ruminant codes the energy model could
# not cover (e.g. a polity with ruminant product but no heads row) keep their
# legacy FCR total and its `bouwman_fcr` label, so their demand is never
# silently dropped and the recorded method stays honest.
.overlay_energy_demand <- function(totals, production, crosswalk) {
  ruminant_codes <- crosswalk$live_anim_code[crosswalk$demand_source == "ipcc"]
  energy <- .build_demand_energy(production, ruminant_codes) |>
    dplyr::mutate(method_demand = "ipcc_tier2_energy")
  covered <- intersect(ruminant_codes, unique(energy$live_anim_code))
  uncovered <- setdiff(
    intersect(ruminant_codes, totals$live_anim_code),
    covered
  )
  if (length(uncovered) > 0) {
    n <- length(uncovered)
    cli::cli_warn(c(
      "IPCC energy demand could not be computed for {n} ruminant
       live_anim_code{?s}: {.val {uncovered}}.",
      i = "Keeping the Bouwman FCR magnitude for {cli::qty(n)}{?it/them}."
    ))
  }
  totals |>
    dplyr::filter(!live_anim_code %in% covered) |>
    dplyr::bind_rows(
      dplyr::select(
        energy,
        year,
        area_code,
        live_anim_code,
        demand_dm_t,
        method_demand
      )
    )
}

# IPCC Tier-2 energy demand for ruminant species: gross_energy (MJ/day/head)
# converted to dry-matter tonnes/year via the diet gross-energy content and a
# 365-day year, summed over the GLEAM cohorts of each animal.
.build_demand_energy <- function(production, ruminant_codes) {
  # The emissions bridge joins the yield (t_head) rows on a CHARACTER
  # live_anim_code (the animal code); real production carries it as a double, so
  # cast it (do NOT drop it: the milk-yield join needs it, or lactation energy
  # silently drops to zero). Keep item_cbs_code integer for the code filter.
  production <- production |>
    dplyr::mutate(
      item_cbs_code = as.integer(item_cbs_code),
      dplyr::across(dplyr::any_of("live_anim_code"), as.character)
    )
  heads <- production |>
    dplyr::filter(unit == "heads", item_cbs_code %in% ruminant_codes)
  if (nrow(heads) == 0) {
    return(.empty_demand_codes())
  }
  production |>
    dplyr::filter(
      (unit == "heads" & item_cbs_code %in% ruminant_codes) | unit == "t_head"
    ) |>
    .ensure_diet_quality() |>
    prepare_livestock_emissions(expand_cohorts = TRUE) |>
    estimate_energy_demand() |>
    .energy_to_dm()
}

# Demand assumes a default IPCC "Medium" diet (DE 65%) where diet quality is not
# supplied; this documented assumption feeds both DE% and the gross-energy
# density used in the dry-matter conversion.
.ensure_diet_quality <- function(data) {
  if (!rlang::has_name(data, "diet_quality")) {
    data$diet_quality <- "Medium"
  } else {
    data$diet_quality <- dplyr::coalesce(data$diet_quality, "Medium")
  }
  data
}

# gross_energy (MJ/day/head) -> dry-matter tonnes/year, summed over cohorts.
.energy_to_dm <- function(energy) {
  ge_content <- feed_characteristics |>
    dplyr::select(diet_quality, ge_content_mj_kg_dm) |>
    dplyr::distinct()
  days <- livestock_constants$days_in_year
  out <- energy |>
    dplyr::left_join(ge_content, by = "diet_quality") |>
    dplyr::mutate(
      dm_t = cohort_heads * (gross_energy / ge_content_mj_kg_dm) * days / 1000
    )
  if (any(is.na(out$dm_t) & out$cohort_heads > 0)) {
    cli::cli_warn(
      "Some ruminant cohorts have undefined dry-matter demand and are excluded:
       missing gross energy (check GLEAM cohort weights) or an unrecognised
       diet_quality with no gross-energy density."
    )
  }
  out |>
    dplyr::summarise(
      demand_dm_t = sum(dm_t, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::transmute(
      year,
      area_code = as.integer(area_code),
      live_anim_code = as.integer(item_cbs_code),
      demand_dm_t
    )
}

# Aggregate per-animal totals (already carrying the method that produced each
# row) to livestock_category, recording the method(s) the category's demand was
# actually built from.
.aggregate_demand_to_category <- function(totals, crosswalk) {
  cat_map <- crosswalk |>
    dplyr::transmute(
      live_anim_code = as.integer(live_anim_code),
      livestock_category
    )
  totals |>
    dplyr::mutate(live_anim_code = as.integer(live_anim_code)) |>
    dplyr::inner_join(cat_map, by = "live_anim_code") |>
    dplyr::summarise(
      demand_dm_t = sum(demand_dm_t, na.rm = TRUE),
      method_demand = .combine_demand_methods(method_demand),
      .by = c(year, area_code, livestock_category)
    )
}

# Method label per (demand_source, demand_tier): energy only for ruminants under
# the ipcc tier; Krausmann for draft; Bouwman FCR otherwise.
.demand_method_label <- function(demand_source, demand_tier) {
  dplyr::case_when(
    demand_source == "ipcc" & demand_tier == "ipcc" ~ "ipcc_tier2_energy",
    demand_source == "krausmann" ~ "krausmann_per_head",
    TRUE ~ "bouwman_fcr"
  )
}

# The method(s) a category's demand was built from. A single method for pure
# categories; a "+"-joined set for a mixed category (e.g. Cattle_meat when one
# ruminant came from energy and another fell back to FCR), so the provenance is
# never hidden.
.combine_demand_methods <- function(x) {
  paste(sort(unique(x)), collapse = "+")
}

# Reverse-split weights: within each (year, area_code, livestock_category), the
# share of the category's demand that each live animal generated. The reshape
# multiplies a category's allocated intake by these shares to attribute it to the
# individual live_anim_codes.
.demand_code_shares <- function(codes, crosswalk) {
  cat_map <- crosswalk |>
    dplyr::transmute(
      live_anim_code = as.integer(live_anim_code),
      livestock_category
    )
  codes |>
    dplyr::mutate(live_anim_code = as.integer(live_anim_code)) |>
    dplyr::inner_join(cat_map, by = "live_anim_code") |>
    dplyr::mutate(
      code_share = .safe_share(demand_dm_t),
      .by = c(year, area_code, livestock_category)
    ) |>
    dplyr::select(
      year,
      area_code,
      livestock_category,
      live_anim_code,
      code_share
    )
}

# Share of x within a group. NA and negative entries are treated as 0 (a missing
# or invalid cell/code contributes nothing and can neither leak nor inflate the
# total via NA propagation), falling back to an equal split when every entry is 0
# so the group's demand is never dropped.
.safe_share <- function(x) {
  x <- pmax(dplyr::coalesce(x, 0), 0)
  total <- sum(x)
  if (total > 0) {
    x / total
  } else {
    rep(1 / length(x), length(x))
  }
}

.empty_feed_demand_codes <- function() {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    live_anim_code = integer(),
    demand_dm_t = numeric(),
    method_demand = character()
  )
}

.empty_feed_demand_total <- function() {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    livestock_category = character(),
    demand_dm_t = numeric(),
    method_demand = character()
  )
}

.empty_demand_codes <- function() {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    live_anim_code = integer(),
    demand_dm_t = numeric()
  )
}

# ---- Engine 2: MIX ----------------------------------------------------------

# Split Engine-1's per-category DM total across Bouwman feed types and emit the
# `redistribute_feed()` `feed_demand` schema. Ports the Spain_Hist `add_feedtypes`
# mix (feed shares from Bouwman, per livestock_category with a grazer-average
# fallback for the draft species IPCC/Bouwman does not cover), globalised: the
# Bouwman region comes from each polity (not a hardcoded "OECD Europe") and the
# Spain-specific reallocations are dropped. Demand is emitted at feed-type grain
# (item_cbs_code/feed_group NA); `redistribute_feed` resolves items via its
# feed_quality matching level.
.build_feed_mix <- function(demand_total, data = .feed_demand_data()) {
  if (nrow(demand_total) == 0) {
    return(.empty_feed_demand())
  }
  years <- sort(unique(as.integer(demand_total$year)))
  shares <- .bouwman_feedtype_shares(data$conv_bouwman, years)
  grazer_shares <- .grazer_feedtype_shares(shares)
  region <- .feed_region_lookup(data$polities_cats)
  # One bridge row per category: a category maps to a single Bouwman class (NA
  # for draft species). graniv_grazers is NOT a key here (a category can span
  # several graniv_grazers values, e.g. Other) and keeping it would fan a
  # category's demand into duplicates.
  bridge <- dplyr::distinct(data$crosswalk, livestock_category, item_bouwman)

  keyed <- demand_total |>
    dplyr::mutate(year = as.integer(year), area_code = as.integer(area_code)) |>
    dplyr::left_join(bridge, by = "livestock_category") |>
    dplyr::left_join(region, by = "area_code")
  .warn_dropped_mix(keyed, data$crosswalk)

  keyed |>
    .join_feedtype_shares(shares, grazer_shares) |>
    .emit_feed_demand()
}

# Surface demand that the mix cannot place (so it is never silently lost):
# categories absent from the crosswalk, and areas with no Bouwman region (whose
# demand the share join drops to NA and the emit step filters out).
.warn_dropped_mix <- function(keyed, crosswalk) {
  unknown <- setdiff(
    unique(keyed$livestock_category),
    crosswalk$livestock_category
  )
  if (length(unknown) > 0) {
    n <- length(unknown)
    cli::cli_warn(c(
      "Feed mix received {n} livestock categor{?y/ies} not in the crosswalk:
       {.val {unknown}}.",
      i = "Treated as draft grazers (no Bouwman class)."
    ))
  }
  no_region <- keyed[is.na(keyed$region_bouwman), , drop = FALSE]
  if (nrow(no_region) > 0) {
    areas <- unique(no_region$area_code)
    dropped <- round(sum(no_region$demand_dm_t, na.rm = TRUE))
    cli::cli_warn(c(
      "No Bouwman region for {length(areas)} area{?s} ({.val {areas}}):
       {dropped} t of feed demand is dropped from the mix.",
      i = "Map the {cli::qty(length(areas))}area{?s} to a Bouwman region in
        {.field polities_cats}."
    ))
  }
  invisible(NULL)
}

# Bouwman feed-type shares: the share of each feed type in a livestock product's
# feed, normalised per (item_bouwman, region, year) from `conv_bouwman` and
# interpolated to every model year. This is the Spain_Hist `feed_share1` step,
# without the Spain-specific Pigs->grass reallocation.
.bouwman_feedtype_shares <- function(conv_bouwman, years) {
  years <- sort(unique(as.integer(years)))
  conv <- tibble::as_tibble(conv_bouwman) |>
    dplyr::rename(feed_type = dplyr::any_of("feedtype")) |>
    dplyr::rename(region_bouwman = dplyr::any_of("region")) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::mutate(
      dm_share = conversion / sum(conversion, na.rm = TRUE),
      .by = c(item_bouwman, region_bouwman, year)
    )
  all_years <- seq(
    min(c(years, conv$year), na.rm = TRUE),
    max(c(years, conv$year), na.rm = TRUE)
  )
  conv |>
    dplyr::select(year, region_bouwman, item_bouwman, feed_type, dm_share) |>
    tidyr::complete(
      year = all_years,
      tidyr::nesting(region_bouwman, item_bouwman, feed_type)
    ) |>
    dplyr::arrange(year) |>
    fill_linear(
      dm_share,
      time_col = year,
      .by = c("region_bouwman", "item_bouwman", "feed_type")
    ) |>
    dplyr::filter(year %in% years)
}

# Grazer-average feed-type shares: the mean Bouwman share across the grazing
# products (the draft / other species borrow this, as in Spain_Hist).
.grazer_feedtype_shares <- function(shares) {
  shares |>
    dplyr::filter(
      item_bouwman %in% c("Beef cattle", "Dairy cattle", "Sheep and goats")
    ) |>
    dplyr::summarise(
      dm_share = mean(dm_share, na.rm = TRUE),
      .by = c(year, region_bouwman, feed_type)
    )
}

# Attach the per-feed-type share to each demand row: the product-specific share
# where the category has a Bouwman class, the grazer-average otherwise.
.join_feedtype_shares <- function(demand, shares, grazer_shares) {
  with_bouwman <- demand |>
    dplyr::filter(!is.na(item_bouwman)) |>
    dplyr::left_join(
      shares,
      by = c("year", "region_bouwman", "item_bouwman"),
      relationship = "many-to-many"
    )
  borrow_grazer <- demand |>
    dplyr::filter(is.na(item_bouwman)) |>
    dplyr::left_join(
      grazer_shares,
      by = c("year", "region_bouwman"),
      relationship = "many-to-many"
    )
  dplyr::bind_rows(with_bouwman, borrow_grazer)
}

# Final feed_demand schema for redistribute_feed. Demand stays at feed-type grain
# (item_cbs_code / feed_group NA); each feed type maps to a representative
# feed_quality and a fixed_demand flag.
.emit_feed_demand <- function(mix) {
  mix |>
    dplyr::mutate(demand_ft = demand_dm_t * dm_share) |>
    dplyr::filter(!is.na(feed_type), .data$demand_ft > 0) |>
    dplyr::left_join(.feedtype_feed_quality(), by = "feed_type") |>
    dplyr::summarise(
      demand_dm_t = sum(demand_ft, na.rm = TRUE),
      .by = c(year, area_code, livestock_category, feed_quality, fixed_demand)
    ) |>
    dplyr::transmute(
      year,
      territory = as.character(area_code),
      sub_territory = NA_character_,
      livestock_category,
      item_cbs_code = NA_integer_,
      feed_group = NA_character_,
      feed_quality,
      demand_dm_t,
      fixed_demand
    )
}

# Bouwman feed type -> representative redistribute_feed feed_quality + whether
# demand is fixed (guaranteed met). Each feed type carries items of more than one
# feed_quality (e.g. crops are mostly high_quality with some low_quality); the
# representative quality is a documented simplification to refine later. Only
# grass is fixed (it is met from the bounded grassland sink); other feed types
# may be underfed when supply is short.
.feedtype_feed_quality <- function() {
  tibble::tribble(
    ~feed_type,
    ~feed_quality,
    ~fixed_demand,
    "grass",
    "grass",
    TRUE,
    "crops",
    "high_quality",
    FALSE,
    "residues",
    "residues",
    FALSE,
    "animals",
    "high_quality",
    FALSE,
    "scavenging",
    "scavenging",
    FALSE
  )
}

.empty_feed_demand <- function() {
  tibble::tibble(
    year = integer(),
    territory = character(),
    sub_territory = character(),
    livestock_category = character(),
    item_cbs_code = integer(),
    feed_group = character(),
    feed_quality = character(),
    demand_dm_t = numeric(),
    fixed_demand = logical()
  )
}

# ---- Engine 3: ALLOCATION (national grain) ----------------------------------

# Run the full national-grain feed allocation: Engine-1 demand -> Engine-2 mix ->
# national CBS availability -> redistribute_feed. Returns the raw redistribute
# result (reshaped to the get_feed_intake contract by a later phase). Grass is
# the unlimited grassland sink unless `grass_availability` bounds it.
.run_redistribute_national <- function(
  production,
  cbs,
  demand_tier,
  data = .feed_demand_data(),
  options = list()
) {
  .national_redistribute(production, cbs, demand_tier, data, options)$result
}

# Run the national-grain engine, returning both the raw redistribute result and
# the per-animal reverse-split weights from the same demand pass (so the reshape
# never recomputes demand).
.national_redistribute <- function(
  production,
  cbs,
  demand_tier,
  data = .feed_demand_data(),
  options = list()
) {
  codes <- .build_feed_demand_codes(production, demand_tier, data)
  demand_total <- .aggregate_demand_to_category(codes, data$crosswalk)
  feed_demand <- .build_feed_mix(demand_total, data)
  feed_avail <- .build_feed_avail_national(cbs) |>
    .add_scavenging_avail(feed_demand)
  list(
    result = redistribute_feed(feed_demand, feed_avail, options = options),
    code_shares = .demand_code_shares(codes, data$crosswalk)
  )
}

# Scavenging (household waste, free-range foraging for monogastrics) is a non-CBS
# free source. Give it availability exactly equal to its own demand per
# (year, territory): enough to meet scavenging in full (scaling 1), with zero
# leftover that surplus distribution could spill onto other feed types. The
# canonical Scavenging item (3500) carries the scavenging feed_type label and the
# dry-matter density the reshape needs.
.add_scavenging_avail <- function(feed_avail, feed_demand) {
  scav <- feed_demand |>
    dplyr::filter(.data$feed_quality == "scavenging", .data$demand_dm_t > 0) |>
    dplyr::summarise(
      avail_dm_t = sum(demand_dm_t, na.rm = TRUE),
      .by = c(year, territory)
    )
  if (nrow(scav) == 0) {
    return(feed_avail)
  }
  scav_avail <- scav |>
    dplyr::transmute(
      year,
      territory,
      sub_territory = NA_character_,
      item_cbs_code = 3500L,
      feed_group = "scavenging",
      feed_quality = "scavenging",
      avail_dm_t,
      feed_scale = "national"
    )
  dplyr::bind_rows(feed_avail, scav_avail)
}

# National feed availability from the Commodity Balance Sheet `feed` element:
# per-item dry-matter supply (with the 0.9 feed-loss factor), tagged with its
# feed_group + feed_quality from `feed_taxonomy` and feed_scale = "national"
# (served to every territory by the national-scale allocator). Grass is not a
# CBS item; it enters redistribute_feed as the grassland sink, not here.
.build_feed_avail_national <- function(
  cbs,
  items_full = whep::items_full,
  biomass_coefs = whep::biomass_coefs,
  feed_taxonomy = whep::feed_taxonomy
) {
  cbs <- .normalise_feed_cbs(cbs)
  items <- .feed_items_lookup(items_full)
  biomass <- .feed_biomass_lookup(biomass_coefs)
  tax <- tibble::as_tibble(feed_taxonomy) |>
    dplyr::select(item_cbs_code, feed_group, feed_quality) |>
    dplyr::distinct(item_cbs_code, .keep_all = TRUE)

  cbs |>
    dplyr::filter(!is.na(.data$feed), .data$feed != 0) |>
    dplyr::left_join(items, by = "item_cbs_code") |>
    dplyr::left_join(biomass, by = "Name_biomass") |>
    dplyr::left_join(tax, by = "item_cbs_code") |>
    dplyr::transmute(
      year,
      territory = as.character(area_code),
      sub_territory = NA_character_,
      item_cbs_code,
      feed_group,
      feed_quality,
      avail_dm_t = .data$feed * 0.9 * product_kgdm_kgfm,
      feed_scale = "national"
    ) |>
    dplyr::filter(
      !is.na(avail_dm_t),
      avail_dm_t > 0,
      !is.na(feed_quality),
      feed_quality != "non_feed"
    )
}

# ---- Provincial grain (sub_territory = cell) --------------------------------

# Run the provincial-grain allocation: national category demand + Bouwman mix,
# distributed to cells by each cell's share of the category's livestock, then
# allocated by redistribute_feed with national CBS feed served to every cell
# (feed_scale "national") and per-cell grass bounding the pasture sink
# (options$grass_availability, keyed by sub_territory). Returns the raw result
# and the per-animal reverse-split weights. `spatial` carries the per-cell inputs
# (cell_shares = per-cell head shares, grass_avail = per-cell grass ceiling).
.run_redistribute_provincial <- function(
  production,
  cbs,
  demand_tier,
  spatial,
  data = .feed_demand_data()
) {
  codes <- .build_feed_demand_codes(production, demand_tier, data)
  demand_total <- .aggregate_demand_to_category(codes, data$crosswalk)
  feed_demand <- .build_feed_mix(demand_total, data) |>
    .distribute_demand_to_cells(spatial$cell_shares)
  feed_avail <- .build_feed_avail_national(cbs) |>
    .add_scavenging_avail(feed_demand)
  list(
    result = redistribute_feed(
      feed_demand,
      feed_avail,
      options = list(grass_availability = spatial$grass_avail)
    ),
    code_shares = .demand_code_shares(codes, data$crosswalk)
  )
}

# Distribute national feed demand to cells: each (year, territory, category)
# demand row is split across the territory's cells by `cell_share` (the cell's
# share of the category's livestock heads), which sums to 1 per category so the
# total is conserved. The mix shares are polity-level, so distributing the mixed
# demand is equivalent to mixing per cell.
.distribute_demand_to_cells <- function(feed_demand, cell_shares) {
  if (nrow(feed_demand) == 0) {
    return(feed_demand)
  }
  cell_shares <- .normalise_cell_shares(cell_shares)
  .warn_uncelled_demand(feed_demand, cell_shares)
  feed_demand |>
    dplyr::select(-sub_territory) |>
    dplyr::inner_join(
      cell_shares,
      by = c("year", "territory", "livestock_category"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      year,
      territory,
      sub_territory,
      livestock_category,
      item_cbs_code,
      feed_group,
      feed_quality,
      demand_dm_t = demand_dm_t * cell_share,
      fixed_demand
    )
}

# Renormalise cell shares to sum to 1 per (year, territory, livestock_category)
# so per-cell heads that do not sum exactly to 1 (rounding, partial coverage,
# border cells split across polities) neither drop nor inflate the distributed
# demand. Reuses the demand-share helper (equal split if a group sums to 0).
.normalise_cell_shares <- function(cell_shares) {
  cell_shares |>
    dplyr::mutate(
      cell_share = .safe_share(cell_share),
      .by = c(year, territory, livestock_category)
    )
}

# Stable per-cell id (sub_territory) from the 0.5-degree cell centre, shared by
# the grass and livestock cell bridges so their cells align.
.cell_id <- function(lon, lat) {
  sprintf("%.2f_%.2f", lon, lat)
}

# Map gridded grass availability to the provincial grass_availability schema:
# each 0.5-degree cell becomes a sub_territory under its polity (territory =
# area_code). Pass `country_grid` (majority assignment, one polity per cell) to
# match how gridded livestock heads are assigned; a `cell_polity` carrying
# `polity_frac` instead splits a border cell's grass across its polities. The
# result is the per-cell forage ceiling redistribute_feed binds the pasture sink
# to.
.grass_to_cells <- function(grass, cell_polity) {
  cp <- dplyr::mutate(cell_polity, lon = round(lon, 2), lat = round(lat, 2))
  if (!rlang::has_name(cp, "polity_frac")) {
    cp$polity_frac <- 1
  }
  grass |>
    dplyr::mutate(lon = round(lon, 2), lat = round(lat, 2)) |>
    dplyr::inner_join(
      cp,
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      year = as.integer(year),
      territory = as.character(as.integer(area_code)),
      sub_territory = .cell_id(lon, lat),
      grass_avail_dm_t = grass_avail_dm_t * polity_frac
    )
}

# Per-cell livestock head shares for the demand distribution: map each gridded
# species_group to its feed livestock_category, sum heads per (year, territory,
# category, cell), and normalise to the cell's share of that category's heads in
# the polity. Gridded heads already carry their (majority) area_code, so no
# cell->polity join is needed.
.heads_to_cell_shares <- function(
  gridded_heads,
  mapping = .species_group_to_category()
) {
  gridded_heads |>
    dplyr::inner_join(
      mapping,
      by = "species_group",
      relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      heads = sum(heads, na.rm = TRUE),
      .by = c(year, area_code, livestock_category, lon, lat)
    ) |>
    dplyr::mutate(
      year = as.integer(year),
      territory = as.character(as.integer(area_code)),
      sub_territory = .cell_id(lon, lat)
    ) |>
    dplyr::mutate(
      cell_share = .safe_share(heads),
      .by = c(year, territory, livestock_category)
    ) |>
    dplyr::select(
      year,
      territory,
      livestock_category,
      sub_territory,
      cell_share
    )
}

# Gridded livestock species_group -> feed livestock_category. Many-to-many where
# the gridded data is coarser than the feed categories: sheep_goats and equines
# each share their spatial pattern across two categories (the grids do not
# separate them); cattle_non_dairy + buffalo both inform Cattle_meat; the three
# poultry grids (broilers, layers, other poultry) all inform Poultry; camels +
# other inform Other, and Rabbits borrow the `other` pattern (no rabbit grid).
# An MVP mapping to refine if finer gridded layers become available.
.species_group_to_category <- function() {
  tibble::tribble(
    ~species_group,
    ~livestock_category,
    "cattle_dairy",
    "Cattle_milk",
    "cattle_non_dairy",
    "Cattle_meat",
    "buffalo",
    "Cattle_meat",
    "sheep_goats",
    "Sheep",
    "sheep_goats",
    "Goats",
    "pigs",
    "Pigs",
    "poultry",
    "Poultry",
    "chickens_broilers",
    "Poultry",
    "chickens_layers",
    "Poultry",
    "equines",
    "Horses",
    "equines",
    "Donkeys_mules",
    "camels",
    "Other",
    "other",
    "Other",
    "other",
    "Rabbits"
  )
}

# Surface demand the cell distribution would silently drop: a (year, territory,
# category) with no per-cell share (no gridded livestock for that category).
.warn_uncelled_demand <- function(feed_demand, cell_shares) {
  keys <- dplyr::distinct(cell_shares, year, territory, livestock_category)
  unmatched <- feed_demand |>
    dplyr::filter(.data$demand_dm_t > 0) |>
    dplyr::anti_join(keys, by = c("year", "territory", "livestock_category"))
  if (nrow(unmatched) == 0) {
    return(invisible(NULL))
  }
  dropped <- round(sum(unmatched$demand_dm_t, na.rm = TRUE))
  cats <- unique(unmatched$livestock_category)
  cli::cli_warn(c(
    "No per-cell share for {length(cats)} category-territory
     combination{?s}: {dropped} t DM of demand is dropped from the provincial
     allocation.",
    i = "Provide {.field cell_shares} (per-cell livestock heads) for the
      {cli::qty(length(cats))}missing categor{?y/ies}."
  ))
  invisible(NULL)
}

# ---- Phase 6: reshape to the get_feed_intake contract -----------------------

# Reshape the raw redistribute_feed result to the get_feed_intake contract:
# reverse-split each livestock_category's intake to its live animals (by the
# Engine-1 demand share), label each allocated item with its Bouwman feed type,
# and convert dry matter to fresh matter. The allocator is demand-pull, so every
# allocated tonne is eaten: supply = intake and loss = 0 (underfeeding is carried
# by redistribute's scaling_factor, not by a loss term).
.reshape_redistribute_intake <- function(
  result,
  code_shares,
  data = .reshape_data(),
  provincial = FALSE
) {
  if (nrow(result) == 0) {
    return(.empty_feed_intake(provincial))
  }
  result <- .assign_grass_sink_item(result)
  .warn_unsplit_intake(result, code_shares)
  result |>
    .split_intake_to_animals(code_shares) |>
    .label_feed_type(data$item_feedtype) |>
    .intake_to_fresh_matter(data$item_kgdm) |>
    .summarise_feed_intake(provincial)
}

# Surface intake the reverse-split would silently drop: rows whose area_code did
# not parse from territory, or whose (year, area_code, livestock_category) has no
# Engine-1 demand share. On the national path neither occurs; the guard turns a
# future upstream desync into a visible warning rather than corrupted totals.
.warn_unsplit_intake <- function(result, code_shares) {
  keys <- dplyr::distinct(
    code_shares,
    year,
    area_code,
    livestock_category
  )
  unmatched <- result |>
    dplyr::filter(.data$intake_dm_t > 0) |>
    dplyr::anti_join(keys, by = c("year", "area_code", "livestock_category"))
  if (nrow(unmatched) == 0) {
    return(invisible(NULL))
  }
  dropped <- round(sum(unmatched$intake_dm_t, na.rm = TRUE))
  cats <- unique(unmatched$livestock_category)
  cli::cli_warn(c(
    "Reverse-split has no demand share for {length(cats)} category-area
     combination{?s}: {dropped} t DM of intake is dropped.",
    i = "Cause: an area_code that did not parse from territory, or a category
      absent from the Engine-1 demand. Totals will under-count."
  ))
  invisible(NULL)
}

# The unlimited grassland sink emits item_cbs_code = NA; label it with the
# canonical Grassland item (3000) so it carries a real feeding-item code and its
# grass biomass density for the fresh-matter conversion. Grass-deficit substitute
# rows (feed_group "substitute") are left item NA here and handled as leftover
# roughage downstream (NOT folded into grass: their feed is non-grass, so the
# grass density would corrupt fresh matter). Also recover the integer area_code.
.assign_grass_sink_item <- function(result) {
  result |>
    tibble::as_tibble() |>
    dplyr::mutate(
      area_code = as.integer(territory),
      item_cbs_code = dplyr::if_else(
        is.na(item_cbs_code) & feed_group == "grass",
        3000L,
        as.integer(item_cbs_code)
      )
    )
}

# Reverse-split each category row to its live animals, scaling intake by the
# per-animal demand share (so the per-animal intakes sum back to the category's).
.split_intake_to_animals <- function(result, code_shares) {
  result |>
    dplyr::inner_join(
      code_shares,
      by = c("year", "area_code", "livestock_category"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(intake_dm_t = intake_dm_t * code_share)
}

# Label every allocated item with its Bouwman feed type. Grass-deficit substitute
# rows (leftover non-grass roughage filling a bounded-grass deficit) are labelled
# residues; unmatched items (none expected nationally) default to crops.
.label_feed_type <- function(result, item_feedtype) {
  result |>
    dplyr::left_join(item_feedtype, by = "item_cbs_code") |>
    dplyr::mutate(
      feed_type = dplyr::case_when(
        feed_group %in% "substitute" ~ "residues",
        !is.na(feed_type) ~ feed_type,
        TRUE ~ "crops"
      )
    )
}

# Convert dry-matter intake to fresh matter via each item's biomass density.
# Substitute rows have no item; report them at a dry-roughage density (0.9) so
# their fresh matter is realistic, not the 5x over-statement a grass density
# would give (a documented MVP value, until the substitute's items are tracked).
.intake_to_fresh_matter <- function(result, item_kgdm) {
  result |>
    dplyr::left_join(item_kgdm, by = "item_cbs_code") |>
    dplyr::mutate(
      product_kgdm_kgfm = dplyr::if_else(
        feed_group %in% "substitute",
        0.9,
        product_kgdm_kgfm
      ),
      intake_dry_matter = intake_dm_t,
      intake = intake_dm_t / product_kgdm_kgfm
    )
}

# Collapse to the contract grain and emit its columns. Demand-pull semantics:
# supply = intake, loss = 0, loss_share = 0. The provincial grain keeps
# sub_territory (cell); the national grain (sub_territory all NA) drops it.
.summarise_feed_intake <- function(result, provincial = FALSE) {
  result |>
    dplyr::filter(
      !is.na(item_cbs_code) | feed_group %in% "substitute",
      intake_dm_t > 0
    ) |>
    dplyr::summarise(
      intake = sum(intake, na.rm = TRUE),
      intake_dry_matter = sum(intake_dry_matter, na.rm = TRUE),
      .by = c(
        year,
        area_code,
        sub_territory,
        live_anim_code,
        item_cbs_code,
        feed_type
      )
    ) |>
    dplyr::mutate(
      year = as.integer(year),
      area_code = as.integer(area_code),
      live_anim_code = as.integer(live_anim_code),
      item_cbs_code = as.integer(item_cbs_code),
      supply = intake,
      loss = 0,
      loss_share = 0
    ) |>
    .select_feed_intake_cols(provincial)
}

# Emit the contract columns, inserting sub_territory (cell) for the provincial
# grain, and order by the key columns.
.select_feed_intake_cols <- function(agg, provincial) {
  cols <- c(
    "year",
    "area_code",
    "live_anim_code",
    "item_cbs_code",
    "feed_type",
    "supply",
    "intake",
    "intake_dry_matter",
    "loss",
    "loss_share"
  )
  if (provincial) {
    cols <- append(cols, "sub_territory", after = 2)
  }
  key_cols <- intersect(
    c(
      "year",
      "area_code",
      "sub_territory",
      "live_anim_code",
      "item_cbs_code",
      "feed_type"
    ),
    cols
  )
  agg |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key_cols)))
}

# Datasets the reshape needs (feed-type labels and DM->fresh densities), grouped
# so the reshape signature stays small and tests can inject fixtures.
.reshape_data <- function() {
  list(
    item_feedtype = .item_feedtype_lookup(),
    item_kgdm = .item_kgdm_lookup()
  )
}

# item_cbs_code -> Bouwman feed type. grazer_feedtype is the complete per-item
# labeling (granivore_feedtype is NA only for fibrous items granivores never
# request); additives (compound-feed ingredients) fold into crops.
.item_feedtype_lookup <- function(feed_taxonomy = whep::feed_taxonomy) {
  tibble::as_tibble(feed_taxonomy) |>
    dplyr::transmute(
      item_cbs_code = as.integer(item_cbs_code),
      feed_type = dplyr::coalesce(grazer_feedtype, granivore_feedtype)
    ) |>
    dplyr::mutate(
      feed_type = dplyr::if_else(feed_type == "additives", "crops", feed_type)
    ) |>
    dplyr::filter(!is.na(item_cbs_code), !is.na(feed_type)) |>
    dplyr::distinct(item_cbs_code, .keep_all = TRUE)
}

# item_cbs_code -> dry-matter fraction (kg DM / kg fresh) via the biomass coefs.
.item_kgdm_lookup <- function(
  items_full = whep::items_full,
  biomass_coefs = whep::biomass_coefs
) {
  .feed_items_lookup(items_full) |>
    dplyr::left_join(
      .feed_biomass_lookup(biomass_coefs),
      by = "Name_biomass"
    ) |>
    dplyr::transmute(
      item_cbs_code = as.integer(item_cbs_code),
      product_kgdm_kgfm
    ) |>
    dplyr::filter(
      !is.na(item_cbs_code),
      !is.na(product_kgdm_kgfm),
      product_kgdm_kgfm > 0
    ) |>
    dplyr::distinct(item_cbs_code, .keep_all = TRUE)
}
