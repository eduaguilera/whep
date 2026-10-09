# Real-data check of the gross crop-residue base, in dry matter (#1330).
#
# The `crop_residues` pin is fresh matter (#1215, PR #1255). Issue #1132
# compared it, unconverted, with dry-matter literature and found the base
# "~36% too high": 7.209 Pg of Straw + Other crop residues at 2020 against
# ~5.3 Pg, the ~3.90 Pg of cereal residue Smerald et al. give for 2020
# divided by their 73% cereal share. That comparison mixed fresh and dry
# weight. This script makes it in dry matter, cereals against cereals, which
# is the only part of the base with a published, checkable global total.
#
# Ground truth is `gt_residue_base_dm.json`: Smerald, Rahimi & Scheer (2023),
# *Sci. Data* 10:685, doi:10.1038/s41597-023-02587-0, transcribed from the
# authors' own code and outputs (Zenodo doi:10.5281/zenodo.7843730). Their
# residue is grain x residue:product ratio x dry-matter content, by three
# methods (a constant ratio, and two yield-dependent ones); the spread of the
# three is the band WHEP is judged against.
#
# What is checked:
#   1. World cereal residue dry matter lies inside the three-method band,
#      widened by `tolerance`, in every year 1997-2021. This is the gate.
#   2. The pin is still fresh matter (straw fresh/dry > 1), so a later change
#      to the unit basis is noticed rather than silently double-converted.
# What is only reported:
#   - per-crop 2010 and 2020 against the band (the crops offset each other);
#   - the country-year totals Smerald validate against;
#   - #1132's all-crop yardstick, whose 73% cereal share (Shinde et al. 2022,
#     Ind. Crops Prod. 181:114772) could not be opened: assumed, unverified.
#   - the non-cereal base (#1399), crop by crop, at 2010 and 2020: soybean,
#     groundnut and potato against IPCC (2006) Table 11.2 and its +-2 s.d.;
#     sugar cane trash per tonne of cane (Hassuani et al. 2005); oil palm
#     Firewood between a fronds-only lower bound (Heuze et al. 2015) and
#     Malaysia's all-solid-biomass upper bound (National Biomass Strategy
#     2020); and Lal's (2005) cereal share. A Malaysian oil palm residue above
#     that upper bound warns but does not fail: changing a residue ratio is a
#     science decision, not something this check should force.
#
# History. The pin holds residue the predecessor had already multiplied by
# its legacy recovery rate; #1330 compared that recovered residue with
# Smerald's gross residue and found it inside the band. #1195 undid the
# recovery, and the residue produced was above the band in 16 of the 25
# years, its 1997-2021 mean 16.1% above Smerald's (3899 against 3357 Tg DM),
# a steady 25% above their constant-ratio method with the same grain
# production. It was the residue:product ratio: Wirsenius's early-1990s
# regional ratios, the region membership of #1430, and scaling a
# non-West-Europe `biomass_coefs` ratio by region over West Europe (#1448).
#
# Since #1448 `get_primary_residues()` estimates cereal residue from the
# pin's production and harvested area with `calculate_crop_residues()`. The
# default, `cereal_residue = "ipcc"`, passes: 1997-2021 mean 3385 Tg DM, 0.8%
# above Smerald's, inside the band in every year. Pass another method as the
# first argument to check it: `Rscript ... validation/residue_base_dm.R
# ensemble` (5 years below the band), `ratio` or `wirsenius` (both fail).
#
# Not part of the test suite: it reads the `crop_residues` pin.
#
# Run:  Rscript --no-init-file validation/residue_base_dm.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))

# Outside the band by more than this, relative to the nearest edge, fails.
# 5% is a fraction of the band's own width (RPR to Fischer spans 3-28%).
tolerance <- 0.05
gt <- jsonlite::read_json(
  "validation/gt_residue_base_dm.json",
  simplifyVector = TRUE
)

cli::cli_h1("Gross crop-residue base, dry matter (#1330)")

cereal_codes <- whep::items_full |>
  dplyr::filter(.data$comm_group == "Cereals") |>
  dplyr::pull("item_cbs_code")
method <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(method)) {
  method <- "ipcc"
}
cli::cli_inform("Cereal residue method: {.val {method}}.")
residues <- get_primary_residues(cereal_residue = method)
cereals <- dplyr::filter(residues, .data$item_cbs_code_crop %in% cereal_codes)

# 2. Unit basis -------------------------------------------------------------
straw <- dplyr::filter(residues, .data$item_cbs_code_residue == 2105)
fresh_over_dry <- sum(straw$value) / sum(straw$value_dm)
cli::cli_inform(
  "Straw fresh/dry mass ratio over all years: {round(fresh_over_dry, 3)}."
)
if (!(fresh_over_dry > 1)) {
  cli::cli_abort(c(
    "The residue pin no longer reads as fresh matter.",
    "i" = "{.fn get_primary_residues} documents {.field value} as fresh and
           {.field value_dm} as dry (#1215); a ratio of 1 means one of them
           changed basis."
  ))
}

# 1. World cereal residue against the band ----------------------------------
world <- cereals |>
  dplyr::summarise(
    whep_fresh_tg = sum(.data$value) / 1e6,
    whep_dm_tg = sum(.data$value_dm) / 1e6,
    .by = "year"
  ) |>
  dplyr::inner_join(tibble::as_tibble(gt$world_tg_dm), by = "year") |>
  dplyr::mutate(
    band_low = pmin(.data$RPR, .data$bentsen, .data$fischer),
    band_high = pmax(.data$RPR, .data$bentsen, .data$fischer),
    band_mean = (.data$RPR + .data$bentsen + .data$fischer) / 3,
    ratio_to_mean = .data$whep_dm_tg / .data$band_mean,
    outside = .data$whep_dm_tg < .data$band_low * (1 - tolerance) |
      .data$whep_dm_tg > .data$band_high * (1 + tolerance)
  ) |>
  dplyr::arrange(.data$year)

print(
  dplyr::select(
    world,
    "year",
    "whep_fresh_tg",
    "whep_dm_tg",
    "band_low",
    "band_high",
    "ratio_to_mean"
  ),
  n = Inf
)

mean_dm <- mean(world$whep_dm_tg)
cli::cli_inform(c(
  "*" = "1997-2021 mean: WHEP {round(mean_dm)} Tg DM against Smerald
         {gt$world_mean_1997_2021_tg_dm} Tg DM
         ({round(100 * (mean_dm / gt$world_mean_1997_2021_tg_dm - 1), 1)}%).",
  "*" = "Same years in fresh matter: {round(mean(world$whep_fresh_tg))} Tg."
))

# Per crop, reported only ----------------------------------------------------
crop_map <- tibble::tribble(
  ~item_cbs_code_crop , ~crop           ,
                 2511 , "Wheat"         ,
                 2807 , "Rice"          ,
                 2514 , "Maize"         ,
                 2513 , "Barley"        ,
                 2517 , "Millet"        ,
                 2518 , "Sorghum"       ,
                 2515 , "Other cereals" ,
                 2516 , "Other cereals" ,
                 2520 , "Other cereals"
)
per_crop <- cereals |>
  dplyr::filter(.data$year %in% c(2010, 2020)) |>
  dplyr::inner_join(crop_map, by = "item_cbs_code_crop") |>
  dplyr::summarise(
    whep_dm_tg = sum(.data$value_dm) / 1e6,
    .by = c("year", "crop")
  ) |>
  dplyr::inner_join(
    tibble::as_tibble(gt$per_crop_tg_dm),
    by = c("year", "crop")
  ) |>
  dplyr::mutate(
    ratio_to_mean = .data$whep_dm_tg /
      ((.data$RPR + .data$bentsen + .data$fischer) / 3)
  ) |>
  dplyr::arrange(.data$year, dplyr::desc(.data$whep_dm_tg))
print(per_crop)

# Country-years, reported only -----------------------------------------------
countries <- cereals |>
  dplyr::mutate(
    iso3 = stringr::str_sub(.data$reporting_polity_code, 1, 3)
  ) |>
  dplyr::summarise(
    whep_dm_tg = sum(.data$value_dm) / 1e6,
    .by = c("iso3", "year")
  ) |>
  dplyr::inner_join(
    tibble::as_tibble(gt$countries),
    by = c("iso3", "year")
  ) |>
  dplyr::mutate(
    ratio_to_smerald = .data$whep_dm_tg / .data$smerald_tg_dm,
    ratio_to_literature = .data$whep_dm_tg / .data$literature_tg_dm
  )
print(
  dplyr::select(
    countries,
    "iso3",
    "year",
    "author",
    "whep_dm_tg",
    "smerald_tg_dm",
    "literature_tg_dm",
    "ratio_to_smerald",
    "ratio_to_literature"
  ),
  n = Inf,
  width = Inf
)

# #1132's all-crop yardstick, reported only ---------------------------------
all_2020 <- dplyr::filter(residues, .data$year == 2020)
feedable_2020 <- dplyr::filter(
  all_2020,
  .data$item_cbs_code_residue %in% c(2105, 2106)
)
yardstick <- gt$world_tg_dm$fischer[gt$world_tg_dm$year == 2020] / 0.73
cli::cli_inform(c(
  "*" = "2020 Straw + Other crop residues: {round(sum(feedable_2020$value) /
         1e6)} Tg fresh, {round(sum(feedable_2020$value_dm) / 1e6)} Tg DM.",
  "*" = "2020 all residue incl. Firewood: {round(sum(all_2020$value_dm) /
         1e6)} Tg DM.",
  "*" = "Smerald 2020 cereals / 0.73 (share assumed, unverified):
         {round(yardstick)} Tg DM at the top of their band."
))

# 3. Non-cereal residue against per-crop literature (#1399), reported only ---
# No openable global non-cereal series exists, so each large non-cereal crop
# is checked against the best source that could be read; see `non_cereal` in
# the ground truth for the quotes. These are reported, not gated: moving a
# residue ratio is a science decision (#1399), and the cereal gate below must
# keep working until one is taken.
nc <- gt$non_cereal
check_years <- c(2010, 2020)

# Harvested area and fresh production of each crop, from the same pin's
# `Product` rows, so every benchmark is applied to WHEP's own crop base.
crop_base <- whep_read_file("crop_residues") |>
  dplyr::rename_with(tolower) |>
  dplyr::filter(
    .data$product_residue == "Product",
    .data$year %in% check_years
  ) |>
  add_item_cbs_code(
    name_column = "item_cbs_crop",
    code_column = "item_cbs_code_crop"
  ) |>
  dplyr::summarise(
    product_fm_t = sum(.data$prod_ygpit_mg, na.rm = TRUE),
    area_ha = sum(.data$area_ygpit_ha, na.rm = TRUE),
    .by = c("year", "item_cbs_code_crop")
  )
crop_residue <- residues |>
  dplyr::filter(.data$year %in% check_years) |>
  dplyr::summarise(
    whep_dm_tg = sum(.data$value_dm) / 1e6,
    .by = c("year", "item_cbs_code_crop")
  )

# IPCC (2006) Table 11.2 above-ground residue, with the band its own +-2 s.d.
# on slope and intercept give (both at their low or both at their high end).
ipcc <- tibble::as_tibble(nc$ipcc_2006_table_11_2$crops) |>
  dplyr::inner_join(crop_base, by = "item_cbs_code_crop") |>
  dplyr::inner_join(crop_residue, by = c("year", "item_cbs_code_crop")) |>
  dplyr::mutate(
    product_dm_t = .data$product_fm_t * .data$dry,
    ipcc_tg = (.data$product_dm_t *
      .data$slope +
      .data$area_ha * .data$intercept) /
      1e6,
    ipcc_low_tg = (.data$product_dm_t *
      .data$slope *
      (1 - .data$slope_2sd_pct / 100) +
      .data$area_ha * .data$intercept * (1 - .data$intercept_2sd_pct / 100)) /
      1e6,
    ipcc_high_tg = (.data$product_dm_t *
      .data$slope *
      (1 + .data$slope_2sd_pct / 100) +
      .data$area_ha * .data$intercept * (1 + .data$intercept_2sd_pct / 100)) /
      1e6,
    ratio_to_ipcc = .data$whep_dm_tg / .data$ipcc_tg,
    inside_2sd = .data$whep_dm_tg >= .data$ipcc_low_tg &
      .data$whep_dm_tg <= .data$ipcc_high_tg
  )
cli::cli_h2("Non-cereal residue against IPCC (2006) Table 11.2")
print(
  dplyr::select(
    ipcc,
    "year",
    "crop",
    "whep_dm_tg",
    "ipcc_low_tg",
    "ipcc_tg",
    "ipcc_high_tg",
    "ratio_to_ipcc",
    "inside_2sd"
  ),
  width = Inf
)

# Sugar cane trash per tonne of cane stalk (Hassuani et al. 2005).
cane <- crop_base |>
  dplyr::filter(.data$item_cbs_code_crop == 2536) |>
  dplyr::inner_join(crop_residue, by = c("year", "item_cbs_code_crop")) |>
  dplyr::mutate(
    whep_t_dm_per_t_cane = .data$whep_dm_tg * 1e6 / .data$product_fm_t,
    ratio_to_hassuani = .data$whep_t_dm_per_t_cane /
      nc$sugarcane_trash$t_dm_per_t_cane
  )
cli::cli_h2("Sugar cane trash against Hassuani et al. (2005)")
print(
  dplyr::select(
    cane,
    "year",
    "whep_dm_tg",
    "whep_t_dm_per_t_cane",
    "ratio_to_hassuani"
  )
)

# Oil palm: fronds alone (Heuze et al. 2015) are a lower bound on the field
# residue; Malaysia's all-solid-biomass total, which also holds the mill
# residues, is an upper bound on it.
palm <- crop_base |>
  dplyr::filter(.data$item_cbs_code_crop == 254) |>
  dplyr::inner_join(crop_residue, by = c("year", "item_cbs_code_crop")) |>
  dplyr::mutate(
    whep_t_dm_per_ha = .data$whep_dm_tg * 1e6 / .data$area_ha,
    fronds_tg = .data$area_ha * nc$oil_palm_fronds$t_dm_per_ha_yr / 1e6,
    ratio_to_fronds = .data$whep_dm_tg / .data$fronds_tg
  )
palm_mys <- residues |>
  dplyr::filter(
    .data$item_cbs_code_crop == 254,
    stringr::str_starts(.data$reporting_polity_code, "MYS")
  ) |>
  dplyr::summarise(whep_dm_tg = sum(.data$value_dm) / 1e6, .by = "year") |>
  dplyr::inner_join(
    tibble::as_tibble(nc$malaysia_oil_palm_solid_biomass$values),
    by = "year"
  ) |>
  dplyr::mutate(
    ratio_to_high = .data$whep_dm_tg / .data$high_mt_dm,
    above_upper_bound = .data$whep_dm_tg > .data$high_mt_dm
  )
cli::cli_h2("Oil palm Firewood against fronds and Malaysia's biomass total")
print(
  dplyr::select(
    palm,
    "year",
    "area_ha",
    "whep_dm_tg",
    "whep_t_dm_per_ha",
    "fronds_tg",
    "ratio_to_fronds"
  )
)
print(
  dplyr::select(
    palm_mys,
    "year",
    "whep_dm_tg",
    "low_mt_dm",
    "high_mt_dm",
    "ratio_to_high",
    "above_upper_bound",
    "kind"
  ),
  width = Inf
)
if (any(palm_mys$above_upper_bound)) {
  cli::cli_warn(c(
    "Malaysian oil palm residue exceeds the country's whole solid oil palm
     biomass, mill residues included, in {cli::qty(sum(
     palm_mys$above_upper_bound))}year{?s}
     {palm_mys$year[palm_mys$above_upper_bound]}.",
    "i" = "Upper bound: Agensi Inovasi Malaysia (2013), National Biomass
           Strategy 2020, v2.0. See #1399."
  ))
}

# Lal (2005) cereal share, reported only: year and mass basis unverified.
lal_share <- nc$lal_2005$cereals_mt / nc$lal_2005$food_crops_27_mt
share_2020 <- all_2020 |>
  dplyr::summarise(
    all = sum(.data$value_dm),
    no_firewood = sum(.data$value_dm[.data$item_cbs_code_residue != 2107]),
    cereal = sum(.data$value_dm[.data$item_cbs_code_crop %in% cereal_codes]),
    cereal_no_firewood = sum(
      .data$value_dm[
        .data$item_cbs_code_crop %in%
          cereal_codes &
          .data$item_cbs_code_residue != 2107
      ]
    )
  )
cli::cli_inform(c(
  "*" = "2020 non-cereal residue: {round((share_2020$all -
         share_2020$cereal) / 1e6)} Tg DM, of which Firewood
         {round((share_2020$all - share_2020$no_firewood) / 1e6)} Tg DM.",
  "*" = "2020 cereal share: {round(100 * share_2020$cereal / share_2020$all,
         1)}% of all residue, {round(100 * share_2020$cereal_no_firewood /
         share_2020$no_firewood, 1)}% excluding Firewood; Lal (2005), 27 food
         crops: {round(100 * lal_share, 1)}% (year and basis unverified)."
))

if (any(world$outside)) {
  bad <- world$year[world$outside]
  cli::cli_abort(c(
    "World cereal residue leaves the published band in
     {cli::qty(length(bad))}year{?s} {bad}.",
    "i" = "Band: Smerald et al. (2023), three methods, widened by
           {tolerance * 100}%."
  ))
}

cli::cli_alert_success(
  "World cereal residue is inside the published dry-matter band in every
   year {min(world$year)}-{max(world$year)}."
)
invisible(list(
  world = world,
  per_crop = per_crop,
  countries = countries,
  ipcc = ipcc,
  cane = cane,
  palm = palm,
  palm_mys = palm_mys
))
